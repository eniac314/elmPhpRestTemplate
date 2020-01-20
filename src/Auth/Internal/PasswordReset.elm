module Auth.Internal.PasswordReset exposing (..)

import Auth.Common exposing (..)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Element.Region as Region
import Http exposing (..)
import Internal.Helpers exposing (PluginResult(..), Status(..), httpErrorToString)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline exposing (..)
import Json.Encode as Encode
import Style.Helpers exposing (buttonStyle, textInputStyle)
import Validate exposing (..)


type alias PasswordResetModel =
    { email : String
    , password : String
    , confirmPassword : String
    , encryptedSelectorAndToken : Decode.Value
    , passwordResetStatus : InternalStatus
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


initPasswordResetModel : PasswordResetModel
initPasswordResetModel =
    { email = ""
    , password = ""
    , confirmPassword = ""
    , encryptedSelectorAndToken = Encode.string ""
    , passwordResetStatus = InitiatingPasswordResetRequest Initial
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


type InternalStatus
    = InitiatingPasswordResetRequest Status
    | UpdatingPasswordRequest Status



-------------------------------------------------------------------------------


initiatePasswordReset : Valid { a | email : String } -> (Result Http.Error InitiatePasswordResetResult -> msg) -> Cmd msg
initiatePasswordReset validData handler =
    let
        model =
            fromValid validData

        body =
            Encode.object
                [ ( "email"
                  , Encode.string model.email
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/api/initiatePasswordReset"
        , body = body
        , expect = Http.expectJson handler decodeInitiatePasswordResetResult
        }


type InitiatePasswordResetResult
    = InitiatePasswordResetSuccess
    | InitiatePasswordResetInvalidEmail
    | InitiatePasswordResetEmailNotVerified
    | InitiatePasswordResetResetDisabled
    | InitiatePasswordResetTooManyRequests


decodeInitiatePasswordResetResult =
    Decode.oneOf
        [ Decode.field "message" decodeInitiatePasswordResetSuccess
        , Decode.field "serverError" decodeInitiatePasswordResetInvalidEmail
        , Decode.field "serverError" decodeInitiatePasswordResetEmailNotVerified
        , Decode.field "serverError" decodeInitiatePasswordResetResetDisabled
        , Decode.field "serverError" decodeInitiatePasswordResetTooManyRequests
        ]


decodeInitiatePasswordResetSuccess =
    decodeConstant "INITIATE PASSWORD RESET SUCCESS" InitiatePasswordResetSuccess


decodeInitiatePasswordResetInvalidEmail =
    decodeConstant "INVALID EMAIL ADDRESS" InitiatePasswordResetInvalidEmail


decodeInitiatePasswordResetEmailNotVerified =
    decodeConstant "EMAIL NOT VERIFIED" InitiatePasswordResetEmailNotVerified


decodeInitiatePasswordResetResetDisabled =
    decodeConstant "PASSWORD RESET IS DISABLED" InitiatePasswordResetResetDisabled


decodeInitiatePasswordResetTooManyRequests =
    decodeConstant "TOO MANY REQUESTS" InitiatePasswordResetTooManyRequests



-------------------------------------------------------------------------------


updatePassword : Valid { a | password : String, encryptedSelectorAndToken : Decode.Value } -> (Result Http.Error UpdatePasswordResult -> msg) -> Cmd msg
updatePassword validData handler =
    let
        model =
            fromValid validData

        body =
            Encode.object
                [ ( "password"
                  , Encode.string model.password
                  )
                , ( "payload", model.encryptedSelectorAndToken )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/api/updatePassword"
        , body = body
        , expect = Http.expectJson handler decodeUpdatePasswordResult
        }


type UpdatePasswordResult
    = UpdatePasswordSuccess
    | UpdatePasswordInvalidSelectorTokenPair
    | UpdatePasswordTokenExpired
    | UpdateInitiatePasswordResetDisabled
    | UpdatePasswordInvalidPassword
    | UpdatePasswordTooManyRequests


decodeUpdatePasswordResult =
    Decode.oneOf
        [ Decode.field "message" decodeUpdatePasswordSuccess
        , Decode.field "serverError" decodeUpdatePasswordInvalidSelectorTokenPair
        , Decode.field "serverError" decodeUpdatePasswordTokenExpired
        , Decode.field "serverError" decodeUpdateInitiatePasswordResetDisabled
        , Decode.field "serverError" decodeUpdatePasswordInvalidPassword
        , Decode.field "serverError" decodeUpdatePasswordTooManyRequests
        ]


decodeUpdatePasswordSuccess =
    decodeConstant "PASSWORD UPDATE SUCCESS" UpdatePasswordSuccess


decodeUpdatePasswordInvalidSelectorTokenPair =
    decodeConstant "INVALID TOKEN" UpdatePasswordInvalidSelectorTokenPair


decodeUpdatePasswordTokenExpired =
    decodeConstant "TOKEN EXPIRED" UpdatePasswordTokenExpired


decodeUpdateInitiatePasswordResetDisabled =
    decodeConstant "PASSWORD RESET IS DISABLED" UpdateInitiatePasswordResetDisabled


decodeUpdatePasswordInvalidPassword =
    decodeConstant "INVALID PASSWORD" UpdatePasswordInvalidPassword


decodeUpdatePasswordTooManyRequests =
    decodeConstant "TOO MANY REQUESTS" UpdatePasswordTooManyRequests


type alias Handlers msg =
    { setEmail : String -> msg
    , setPassword : String -> msg
    , setConfirmPassword : String -> msg
    , initiatePasswordResetRequest : msg
    , updatePasswordRequest : msg
    , toLogin : msg
    }


passwordResetView : Handlers msg -> PasswordResetModel -> Element msg
passwordResetView handlers model =
    case model.passwordResetStatus of
        InitiatingPasswordResetRequest Initial ->
            let
                model_ =
                    validateModelIfShowError (validateErrorDict validateEmail) model
            in
            column
                [ spacing 15 ]
                [ customEmailInput
                    { label = "Email:"
                    , value = model_.email
                    , tag = "email"
                    , handler = handlers.setEmail
                    }
                    model_
                , row
                    [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just handlers.initiatePasswordResetRequest
                        , label = text "Send"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| handlers.toLogin
                        , label = text "Back"
                        }
                    ]
                ]

        InitiatingPasswordResetRequest Waiting ->
            waitingView

        InitiatingPasswordResetRequest Success ->
            Element.none

        InitiatingPasswordResetRequest Failure ->
            column
                [ spacing 15 ]
                [ Input.button (buttonStyle True)
                    { onPress = Just <| handlers.toLogin
                    , label = text "Back"
                    }
                ]

        UpdatingPasswordRequest Initial ->
            let
                model_ =
                    validateModelIfShowError
                        (validateErrorDict <|
                            Validate.all
                                [ validatePassword
                                , validateConfirmPasword
                                ]
                        )
                        model
            in
            column
                [ spacing 15 ]
                [ customCurrentPasswordInput
                    { label = "Password: "
                    , value = model_.password
                    , tag = "password"
                    , handler = handlers.setPassword
                    }
                    model_
                , customCurrentPasswordInput
                    { label = "Confirm password: "
                    , value = model_.confirmPassword
                    , tag = "confirmPassword"
                    , handler = handlers.setConfirmPassword
                    }
                    model_
                , row
                    [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just handlers.updatePasswordRequest
                        , label = text "Send"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| handlers.toLogin
                        , label = text "Back"
                        }
                    ]
                ]

        UpdatingPasswordRequest Waiting ->
            waitingView

        UpdatingPasswordRequest Success ->
            column
                [ spacing 15 ]
                [ text "password updated succesfully"
                , Input.button (buttonStyle True)
                    { onPress = Just <| handlers.toLogin
                    , label = text "Back to login"
                    }
                ]

        UpdatingPasswordRequest Failure ->
            column
                [ spacing 15 ]
                [ Input.button (buttonStyle True)
                    { onPress = Just <| handlers.toLogin
                    , label = text "Back"
                    }
                ]


waitingView =
    column
        [ spacing 15 ]
        [ text "Processing request, please wait" ]
