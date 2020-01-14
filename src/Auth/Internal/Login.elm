module Auth.Internal.Login exposing (..)

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


type alias LoginModel =
    { username : String
    , password : String
    , requestStatus : Status
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


initLogin : LoginModel
initLogin =
    { username = ""
    , password = ""
    , requestStatus = Initial
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


validateLogin =
    validateErrorDict
        (Validate.all
            [ validateUsername
            , validatePassword
            ]
        )


login : Valid { a | username : String, password : String } -> (Result Http.Error LoginResult -> msg) -> Cmd msg
login validData handler =
    let
        model =
            fromValid validData

        body =
            Encode.object
                [ ( "username"
                  , Encode.string model.username
                  )
                , ( "password"
                  , Encode.string model.password
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/api/login"
        , body = body
        , expect = Http.expectJson handler decodeLoginResult
        }


type LoginResult
    = LoginSuccess UserProfile
    | LoginWrongCredentials
    | LoginNeedEmailConfirmation
    | LoginTooManyRequests


decodeLoginResult : Decode.Decoder LoginResult
decodeLoginResult =
    Decode.oneOf
        [ Decode.field "message" decodeUserProfile
            |> Decode.map LoginSuccess
        , Decode.field "serverError" decodeLoginWrongCredentials
        , Decode.field "serverError" decodeLoginNeedEmailConfirmation
        , Decode.field "serverError" decodeLoginTooManyRequests
        ]


decodeLoginWrongCredentials =
    decodeConstant "WRONG CREDENTIALS" LoginWrongCredentials


decodeLoginNeedEmailConfirmation =
    decodeConstant "NEED EMAIL CONFIRMATION" LoginNeedEmailConfirmation


decodeLoginTooManyRequests =
    decodeConstant "TOO MANY REQUESTS" LoginTooManyRequests


decodeUserProfile : Decode.Decoder UserProfile
decodeUserProfile =
    Decode.succeed UserProfile
        |> Pipeline.required "username" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "roles" decodeRole


decodeRole : Decode.Decoder Role
decodeRole =
    Decode.succeed Admin


type alias Handlers msg =
    { setUsername : String -> msg
    , setPassword : String -> msg
    , loginRequest : msg
    , toLogin : msg
    , toSignup : msg
    }


loginView : Handlers msg -> LoginModel -> Element msg
loginView handlers model =
    let
        model_ =
            validateModelIfShowError validateLogin model

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
                , customCurrentPasswordInput
                    { label = "Password: "
                    , value = model_.password
                    , tag = "password"
                    , handler = handlers.setPassword
                    }
                    model_
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just handlers.loginRequest
                        , label = text "Log in"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| handlers.toSignup --ToSignup Signup.initSignupModel
                        , label = text "New user signup"
                        }
                    ]
                ]

        waitingView =
            column
                [ spacing 15 ]
                [ text "Traitement en cours, veuillez patienter" ]

        successView =
            column
                [ spacing 15 ]
                [ text "Connexion réussie!"
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Echec Connexion!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| handlers.toLogin

                        --ToLogin { model_ | requestStatus = Initial } False
                        , label = text "Réessayer"
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
        [ text "Connexion: "
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
