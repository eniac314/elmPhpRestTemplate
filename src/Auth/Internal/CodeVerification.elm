module Auth.Internal.CodeVerification exposing (..)

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
import Json.Encode as Encode
import Style.Helpers exposing (buttonStyle, textInputStyle)
import Validate exposing (..)


type alias CodeVerificationModel =
    { code : String
    , email : String
    , askForEmail : Bool
    , requestStatus : Status
    , resendRequestStatus : Status
    , verificationNotice : String
    , verificationEndpoint : String
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


initCodeVerificationModel : CodeVerificationModel
initCodeVerificationModel =
    { code = ""
    , email = ""
    , askForEmail = False
    , requestStatus = Initial
    , resendRequestStatus = Initial
    , verificationNotice = ""
    , verificationEndpoint = ""
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


validateCodeVerification =
    validateErrorDict
        (Validate.all
            [ ifFalse
                (\m ->
                    case String.toInt m.code of
                        Just n ->
                            n >= 0 && n <= 999999

                        Nothing ->
                            False
                )
                ( "code", "The code is invalid" )
            , ifTrue
                (\m ->
                    m.requestStatus
                        == Waiting
                        || m.requestStatus
                        == Success
                )
                ( "admin", "Can't verify code now" )
            , validateEmail
            ]
        )


verifyCode :
    Valid { a | code : String, verificationEndpoint : String, email : String }
    -> (Result Http.Error CodeVerificationResult -> msg)
    -> Cmd msg
verifyCode validData handler =
    let
        model =
            fromValid validData

        body =
            Encode.object
                [ ( "code"
                  , Encode.int
                        (String.toInt model.code
                            |> Maybe.withDefault 0
                        )
                  )
                , ( "email", Encode.string model.email )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = model.verificationEndpoint
        , body = body
        , expect = Http.expectJson handler decodeCodeVerificationResult
        }


type CodeVerificationResult
    = CodeVerificationSuccess Decode.Value
    | CodeVerificationFailure
    | CodeVerificationTooManyAttempts


decodeCodeVerificationResult : Decode.Decoder CodeVerificationResult
decodeCodeVerificationResult =
    Decode.oneOf
        [ Decode.field "message" decodeCodeVerificationWithPayloadSuccess
        , Decode.field "serverError" decodeCodeVerificationFailure
        , Decode.field "serverError" decodeCodeVerificationTooManyAttempts
        ]


decodeCodeVerificationWithPayloadSuccess =
    Decode.field "codeVerificationPayload" Decode.value
        |> Decode.map CodeVerificationSuccess


decodeCodeVerificationFailure =
    decodeConstant "CODE VERIFICATION FAILURE" CodeVerificationFailure


decodeCodeVerificationTooManyAttempts =
    decodeConstant "CODE VERIFICATION TOO MANY ATTEMPTS" CodeVerificationTooManyAttempts


resendConfirmation :
    Valid { a | email : String }
    -> (Result Http.Error ResendConfirmationResult -> msg)
    -> Cmd msg
resendConfirmation validData handler =
    let
        model =
            fromValid validData

        body =
            Encode.object
                [ ( "email", Encode.string model.email )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "api/resendConfirmation"
        , body = body
        , expect = Http.expectJson handler decodeResendConfirmationResult
        }


type ResendConfirmationResult
    = ResendConfirmationSuccess
    | ResendConfirmationFailure


decodeResendConfirmationResult =
    Decode.oneOf
        [ Decode.field "message" decodeResendConfirmationSuccess
        ]


decodeResendConfirmationSuccess =
    decodeConstant "RESEND CODE CONFIRMATION SUCCESS" ResendConfirmationSuccess


type alias Handlers msg =
    { setVerificationCode : String -> msg
    , setEmail : String -> msg
    , codeVerificationRequest : msg
    , toCodeVerification : msg
    }


codeVerificationView : Handlers msg -> CodeVerificationModel -> Element msg
codeVerificationView handlers model =
    let
        model_ =
            validateModelIfShowError validateCodeVerification model

        status =
            model_.requestStatus

        initialView =
            column
                [ spacing 15 ]
                [ customInput
                    { label = "Input verification code: "
                    , value = model_.code
                    , tag = "code"
                    , handler = handlers.setVerificationCode
                    }
                    model_
                , if model.askForEmail then
                    customEmailInput
                        { label = "Email:"
                        , value = model_.email
                        , tag = "email"
                        , handler = handlers.setEmail
                        }
                        model_

                  else
                    Element.none
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just handlers.codeVerificationRequest
                        , label = text "Send"
                        }
                    ]
                ]

        waitingView =
            column
                [ spacing 15 ]
                [ text "Processing request, please wait" ]

        successView =
            column
                [ spacing 15 ]
                [ text "Code verification success"
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Could not verify code"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| handlers.toCodeVerification
                        , label = text "Try again"
                        }
                    , Input.button (buttonStyle True)
                        { onPress =
                            Nothing
                        , label = text "Back"
                        }
                    ]
                ]
    in
    column
        [ padding 15
        , spacing 15
        , Font.size 16
        , alignTop
        ]
        [ text model_.verificationNotice
        , case status of
            Initial ->
                initialView

            Waiting ->
                waitingView

            Success ->
                successView

            Failure ->
                failureView
        ]
