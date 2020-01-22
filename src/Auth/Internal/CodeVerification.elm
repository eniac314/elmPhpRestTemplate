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
    , canResendCode : Bool
    , verificationNotice : String
    , verificationEndpoint : String
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    , internalStatus : InternalStatus
    }


type InternalStatus
    = VerifyingCode Status
    | RequestingNewCode Status


toogleInternalStatus model =
    case model.internalStatus of
        VerifyingCode _ ->
            { model | internalStatus = RequestingNewCode Initial }

        _ ->
            { model | internalStatus = VerifyingCode Initial }


initCodeVerificationModel : CodeVerificationModel
initCodeVerificationModel =
    { code = ""
    , email = ""
    , askForEmail = False
    , canResendCode = False
    , verificationNotice = ""
    , verificationEndpoint = ""
    , showValidationErrors = False
    , validationErrors = Dict.empty
    , internalStatus = VerifyingCode Initial
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

            --, ifTrue
            --    (\m ->
            --        m.requestStatus
            --            == Waiting
            --            || m.requestStatus
            --            == Success
            --    )
            --    ( "admin", "Can't verify code now" )
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
    | CodeVerificationInvalidCode
    | CodeVerificationInvalidSelectorTokenPairException
    | CodeVerificationTokenExpiredException
    | CodeVerificationUserAlreadyExistsException
    | CodeVerificationTooManyAttempts
    | CodeVerificationGenericError


decodeCodeVerificationResult : Decode.Decoder CodeVerificationResult
decodeCodeVerificationResult =
    Decode.oneOf
        [ Decode.field "message" decodeCodeVerificationWithPayloadSuccess
        , Decode.field "serverError" decodeCodeVerificationInvalidCode
        , Decode.field "serverError" decodeCodeVerificationInvalidSelectorTokenPairException
        , Decode.field "serverError" decodeCodeVerificationTokenExpiredException
        , Decode.field "serverError" decodeCodeVerificationUserAlreadyExistsException
        , Decode.field "serverError" decodeCodeVerificationTooManyAttempts
        , decodeGenericError CodeVerificationGenericError
        ]


decodeCodeVerificationWithPayloadSuccess =
    Decode.field "codeVerificationPayload" Decode.value
        |> Decode.map CodeVerificationSuccess


decodeCodeVerificationInvalidCode =
    decodeConstant "INVALID CODE" CodeVerificationInvalidCode


decodeCodeVerificationInvalidSelectorTokenPairException =
    decodeConstant "INVALID SELECTOR TOKEN PAIR EXCEPTION" CodeVerificationInvalidSelectorTokenPairException


decodeCodeVerificationTokenExpiredException =
    decodeConstant "TOKEN EXPIRED EXCEPTION" CodeVerificationTokenExpiredException


decodeCodeVerificationUserAlreadyExistsException =
    decodeConstant "USER ALREADY EXISTS EXCEPTION" CodeVerificationUserAlreadyExistsException


decodeCodeVerificationTooManyAttempts =
    decodeConstant "CODE VERIFICATION TOO MANY ATTEMPTS" CodeVerificationTooManyAttempts


newCode :
    Valid { a | email : String }
    -> (Result Http.Error NewCodeResult -> msg)
    -> Cmd msg
newCode validData handler =
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
        { url = "api/newCode"
        , body = body
        , expect = Http.expectJson handler decodeNewCodeResult
        }


type NewCodeResult
    = NewCodeSuccess
    | NewCodeTooManyAttemps
    | NewCodeNoPreviousAttempt


decodeNewCodeResult =
    Decode.oneOf
        [ Decode.field "message" decodeNewCodeSuccess
        , Decode.field "serverError" decodeNewCodeTooManyAttemps
        , Decode.field "serverError" decodeNewCodeNoPreviousAttempt
        ]


decodeNewCodeSuccess =
    decodeConstant "RESEND CODE VERIFICATION SUCCESS" NewCodeSuccess


decodeNewCodeTooManyAttemps =
    decodeConstant "RESEND CODE TOO MANY ATTEMPS" NewCodeTooManyAttemps


decodeNewCodeNoPreviousAttempt =
    decodeConstant "RESEND CODE NO PREVIOUS ATTEMPT" NewCodeNoPreviousAttempt


type alias Handlers msg =
    { setVerificationCode : String -> msg
    , setEmail : String -> msg
    , codeVerificationRequest : msg
    , toogleInternalStatus : msg
    , newCodeRequest : msg
    , toCodeVerification : CodeVerificationModel -> msg
    }


codeVerificationView : Handlers msg -> CodeVerificationModel -> Element msg
codeVerificationView handlers model =
    case model.internalStatus of
        VerifyingCode Initial ->
            let
                model_ =
                    validateModelIfShowError validateCodeVerification model
            in
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
                        , label = text "Confirm code"
                        }
                    ]
                , if model.canResendCode then
                    el
                        [ Events.onClick handlers.toogleInternalStatus
                        , pointer
                        , Font.color (rgb 0 0 1)
                        , Font.underline
                        ]
                        (text "I did not get a code")

                  else
                    Element.none
                ]

        VerifyingCode Waiting ->
            waitingView

        VerifyingCode Success ->
            column
                [ spacing 15 ]
                [ text "Code verification success"
                ]

        VerifyingCode Failure ->
            let
                model_ =
                    validateModelIfShowError validateCodeVerification model
            in
            column
                [ spacing 15 ]
                [ text "Could not verify code"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <|
                                handlers.toCodeVerification
                                    { model_ | internalStatus = VerifyingCode Initial }
                        , label = text "Try again"
                        }
                    , Input.button (buttonStyle True)
                        { onPress =
                            Nothing
                        , label = text "Back"
                        }
                    ]
                ]

        RequestingNewCode Initial ->
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
                    [ Input.button
                        (buttonStyle True)
                        { onPress = Just handlers.newCodeRequest
                        , label = text "Get a new code"
                        }
                    , Input.button
                        (buttonStyle True)
                        { onPress = Just handlers.toogleInternalStatus
                        , label = text "Back"
                        }
                    ]
                ]

        RequestingNewCode Waiting ->
            Element.none

        RequestingNewCode Success ->
            Element.none

        RequestingNewCode Failure ->
            Element.none


waitingView =
    column
        [ spacing 15 ]
        [ text "Processing request, please wait" ]
