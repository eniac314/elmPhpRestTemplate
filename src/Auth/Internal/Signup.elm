module Auth.Internal.Signup exposing (..)

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


type alias SignupModel =
    { username : String
    , email : String
    , password : String
    , confirmPassword : String
    , requestStatus : Status
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


initSignupModel : SignupModel
initSignupModel =
    { username = ""
    , email = ""
    , password = ""
    , confirmPassword = ""
    , requestStatus = Initial
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


validateSignup =
    validateErrorDict
        (Validate.all
            [ validateUsername
            , validatePassword
            , validateConfirmPasword
            , ifFalse (\m -> m.password == m.confirmPassword)
                ( "confirmPassword", "Passwords are not matching" )
            , validateEmail
            ]
        )


signup :
    Valid { a | username : String, password : String, email : String }
    -> (Result Http.Error SignupResult -> msg)
    -> Cmd msg
signup validData handler =
    let
        model =
            fromValid validData

        body =
            Encode.object
                [ ( "username", Encode.string model.username )
                , ( "password", Encode.string model.password )
                , ( "email", Encode.string model.email )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/api/signup"
        , body = body
        , expect = Http.expectJson handler decodeSignupResult
        }


type SignupResult
    = SignupSuccess
    | SignupInvalidEmail
    | SignupUserAlreadyExists
    | SignupTooManyRequests
    | SignupInvalidPassword


decodeSignupResult =
    Decode.oneOf
        [ Decode.field "message" decodeSignupSuccess
        , Decode.field "serverError" decodeSignupInvalidEmail
        , Decode.field "serverError" decodeSignupUserAlreadyExists
        , Decode.field "serverError" decodeSignupTooManyRequests
        , Decode.field "serverError" decodeSignupInvalidPassword
        ]


decodeSignupSuccess =
    decodeConstant "SIGNUP SUCCESSFUL" SignupSuccess


decodeSignupInvalidEmail =
    decodeConstant "INVALID EMAIL ADDRESS" SignupInvalidEmail


decodeSignupUserAlreadyExists =
    decodeConstant "USER ALREADY EXISTS" SignupUserAlreadyExists


decodeSignupTooManyRequests =
    decodeConstant "TOO MANY REQUESTS" SignupTooManyRequests


decodeSignupInvalidPassword =
    decodeConstant "INVALID PASSWORD" SignupInvalidPassword


type alias Handlers msg =
    { setUsername : String -> msg
    , setEmail : String -> msg
    , setPassword : String -> msg
    , setConfirmPassword : String -> msg
    , signupRequest : msg
    , toLogin : msg
    , toSignup : msg
    }


signupView : Handlers msg -> SignupModel -> Element msg
signupView handlers model =
    let
        model_ =
            validateModelIfShowError validateSignup model

        status =
            model_.requestStatus

        initialView =
            column
                [ spacing 15 ]
                [ customInput
                    { label = "Username: "
                    , value = model_.username
                    , tag = "username"
                    , handler = handlers.setUsername
                    }
                    model_
                , customEmailInput
                    { label = "Email: "
                    , value = model_.email
                    , tag = "email"
                    , handler = handlers.setEmail
                    }
                    model_
                , customCurrentPasswordInput
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
                        { onPress = Just handlers.signupRequest
                        , label = text "Send"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| handlers.toLogin --ToLogin initLoginModel False
                        , label = text "Back"
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
                [ text "Signup successful!" ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Signup failure!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| handlers.toSignup

                        --ToSignup { model_ | requestStatus = Initial }
                        , label = text "Réessayer"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just handlers.toLogin --ToLogin initLoginModel False
                        , label = text "Retour"
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
        [ text "New user signup:"
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
