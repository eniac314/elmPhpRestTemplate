module Auth.AuthPlugin exposing
    ( LogInfo(..)
    , Model
    , Msg
    , cmdIfLogged
    , getLogInfo
    , init
    , isLogged
    , newLogIfLogged
    , status
    , subscriptions
    , update
    , view
    )

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
import Internal.Logger exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode exposing (..)
import Style.Helpers exposing (buttonStyle, textInputStyle)
import Task exposing (..)
import Time exposing (..)


cmdIfLogged : LogInfo -> (String -> Cmd msg) -> Cmd msg
cmdIfLogged logInfo cmd =
    case logInfo of
        LoggedIn { sessionId } ->
            cmd sessionId

        _ ->
            Cmd.none


isLogged : LogInfo -> Bool
isLogged logInfo =
    case logInfo of
        LoggedIn _ ->
            True

        _ ->
            False


newLogIfLogged : LogInfo -> (Log -> msg) -> String -> Maybe String -> Bool -> Bool -> Cmd msg
newLogIfLogged logInfo addLogMsg logMsg details isError isImportant =
    if isLogged logInfo then
        newLog addLogMsg logMsg details isError isImportant

    else
        newLog
            addLogMsg
            "Opération impossible: utilisateur déconnecté"
            Nothing
            True
            True


type LogInfo
    = LoggedIn
        { username : String
        , sessionId : String
        }
    | LoggedOut


type alias Model msg =
    { username : String
    , password : String
    , confirmPassword : String
    , logInfo : LogInfo
    , pluginMode : PluginMode
    , externalMsg : Msg -> msg
    }


init externalMsg =
    ( { username = ""
      , password = ""
      , confirmPassword = ""
      , logInfo = LoggedOut
      , pluginMode = LoginMode Waiting
      , externalMsg = externalMsg
      }
    , Cmd.map externalMsg <| checkLogin
    )


status model =
    case model.pluginMode of
        SignUpMode s ->
            s

        LoginMode s ->
            s

        LogoutMode s ->
            s


subscriptions model =
    Sub.map model.externalMsg <|
        case model.pluginMode of
            LoginMode _ ->
                Time.every (30 * 1000) (\_ -> Ping)

            _ ->
                Sub.none


reset model =
    ( { model
        | username = ""
        , password = ""
        , confirmPassword = ""
        , pluginMode = LoginMode Initial
      }
    , login model
    )


getLogInfo model =
    model.logInfo


type PluginMode
    = SignUpMode Status
    | LoginMode Status
    | LogoutMode Status


type Msg
    = SetUsername String
    | SetPassword String
    | SetConfirmPassword String
    | Login
    | LoginChecked (Result Http.Error LogInfo)
    | ConfirmLogin (Result Http.Error LogInfo)
    | SignUp
    | ConfirmSignUp (Result Http.Error Bool)
    | Logout
    | ConfirmLogout (Result Http.Error Bool)
    | ChangePluginMode PluginMode
    | Ping
    | PingResult (Result Http.Error Bool)
    | Quit
    | NoOp


update config msg model =
    case msg of
        SetUsername s ->
            ( { model | username = s }
            , Cmd.none
            , Nothing
            )

        SetPassword s ->
            ( { model | password = s }
            , Cmd.none
            , Nothing
            )

        SetConfirmPassword s ->
            ( { model | confirmPassword = s }
            , Cmd.none
            , Nothing
            )

        Login ->
            ( { model
                | pluginMode = LoginMode Waiting
              }
            , login model
                |> Cmd.map model.externalMsg
            , Nothing
            )

        LoginChecked res ->
            case res of
                Err e ->
                    ( { model
                        | logInfo = LoggedOut
                        , pluginMode = LoginMode Initial
                      }
                    , Cmd.none
                    , Nothing
                    )

                Ok logInfo ->
                    ( { model
                        | logInfo = logInfo
                        , pluginMode = LoginMode Success
                      }
                    , Cmd.none
                    , Just PluginQuit
                    )

        ConfirmLogin res ->
            case res of
                Err e ->
                    ( { model
                        | logInfo = LoggedOut
                        , pluginMode = LoginMode Failure
                      }
                    , newLog
                        config.addLog
                        "Echec connexion"
                        (Just <| httpErrorToString e)
                        True
                        True
                    , Nothing
                    )

                Ok logInfo ->
                    ( { model
                        | logInfo = logInfo
                        , pluginMode = LoginMode Success
                      }
                    , Cmd.none
                    , Nothing
                    )

        SignUp ->
            ( { model
                | pluginMode = SignUpMode Waiting
              }
            , signUp model
                |> Cmd.map model.externalMsg
            , Nothing
            )

        ConfirmSignUp res ->
            case res of
                Err e ->
                    ( { model | pluginMode = SignUpMode Failure }
                    , newLog
                        config.addLog
                        "Echec création compte"
                        (Just <| httpErrorToString e)
                        True
                        True
                    , Nothing
                    )

                Ok _ ->
                    ( { model | pluginMode = SignUpMode Success }
                    , Cmd.none
                    , Nothing
                    )

        Logout ->
            ( { model
                | pluginMode = LogoutMode Waiting
              }
            , logout
                |> Cmd.map model.externalMsg
            , Nothing
            )

        ConfirmLogout res ->
            case res of
                Err e ->
                    ( { model | pluginMode = LogoutMode Failure }
                    , newLog
                        config.addLog
                        "Echec déconnexion"
                        (Just <| httpErrorToString e)
                        True
                        True
                    , Nothing
                    )

                Ok _ ->
                    ( { model
                        | pluginMode = LogoutMode Success
                        , logInfo = LoggedOut
                      }
                    , Cmd.none
                    , Nothing
                    )

        ChangePluginMode mode ->
            ( { model
                | pluginMode = mode
              }
            , Cmd.none
            , Nothing
            )

        Ping ->
            case model.logInfo of
                LoggedIn li ->
                    ( { model | pluginMode = LoginMode Waiting }
                    , ping li.sessionId
                        |> Cmd.map model.externalMsg
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        PingResult res ->
            case res of
                Ok True ->
                    ( { model | pluginMode = LoginMode Success }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( { model
                        | logInfo = LoggedOut
                        , pluginMode = LoginMode Failure
                      }
                    , Cmd.none
                    , Nothing
                    )

        Quit ->
            ( model, Cmd.none, Just PluginQuit )

        NoOp ->
            ( model, Cmd.none, Nothing )


login : Model msg -> Cmd Msg
login model =
    let
        body =
            Encode.object
                [ ( "username"
                  , Encode.string (.username model)
                  )
                , ( "password"
                  , Encode.string (.password model)
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "login.php"
        , body = body
        , expect = Http.expectJson ConfirmLogin decodeLoginResult
        }


checkLogin : Cmd Msg
checkLogin =
    Http.get
        { url = "login.php"
        , expect = Http.expectJson LoginChecked decodeLoginResult
        }


decodeLoginResult : Decode.Decoder LogInfo
decodeLoginResult =
    Decode.map2 (\a b -> LoggedIn { username = a, sessionId = b })
        (Decode.field "username" Decode.string)
        (Decode.field "sessionId" Decode.string)


signUp : Model msg -> Cmd Msg
signUp model =
    let
        body =
            Encode.object
                [ ( "username"
                  , Encode.string (.username model)
                  )
                , ( "password"
                  , Encode.string (.password model)
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "signup.php"
        , body = body
        , expect = Http.expectJson ConfirmSignUp decodeSignupResult
        }


decodeSignupResult =
    Decode.field "signUpComplete" Decode.bool


ping : String -> Cmd Msg
ping sessionId =
    let
        body =
            Encode.object
                [ ( "sessionId"
                  , Encode.string sessionId
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "ping.php"
        , body = body
        , expect = Http.expectJson PingResult decodePing
        }


decodePing =
    Decode.field "message" Decode.string
        |> Decode.map (\s -> s == "success!")


logout : Cmd Msg
logout =
    Http.get
        { url = "logout.php"
        , expect = Http.expectJson ConfirmLogout decodeLogoutResult
        }


decodeLogoutResult =
    Decode.field "notLoggedIn" Decode.bool


view config model =
    Element.map model.externalMsg <|
        case model.pluginMode of
            SignUpMode status_ ->
                signUpView config status_ model

            LoginMode status_ ->
                loginView config status_ model

            LogoutMode status_ ->
                logoutView config status_ model


signUpView config status_ model =
    let
        initialView =
            column
                [ spacing 15 ]
                [ Input.text textInputStyle
                    { onChange =
                        SetUsername
                    , text =
                        model.username
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Nom utilisateur: "))
                    }
                , Input.newPassword textInputStyle
                    { onChange =
                        SetPassword
                    , text =
                        model.password
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Mot de passe: "))
                    , show = False
                    }
                , Input.newPassword textInputStyle
                    { onChange =
                        SetConfirmPassword
                    , text =
                        model.confirmPassword
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Confirmation: "))
                    , show = False
                    }
                , row
                    [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just SignUp
                        , label = text "Envoyer"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Retour"
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
                [ text "Inscription réussie!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Connexion"
                        }
                    ]
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Echec inscription!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| ChangePluginMode (SignUpMode Initial)
                        , label = text "Réessayer"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| ChangePluginMode (LoginMode Initial)
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
        [ text "Nouvel utilisateur: "
        , case status_ of
            Initial ->
                initialView

            Waiting ->
                waitingView

            Success ->
                successView

            Failure ->
                failureView
        ]


loginView config status_ model =
    let
        initialView =
            column
                [ spacing 15 ]
                [ Input.text textInputStyle
                    { onChange =
                        SetUsername
                    , text =
                        model.username
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Nom utilisateur: "))
                    }
                , Input.currentPassword textInputStyle
                    { onChange =
                        SetPassword
                    , text =
                        model.password
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Mot de passe: "))
                    , show = False
                    }
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just Login
                        , label = text "Connexion"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| ChangePluginMode (SignUpMode Initial)
                        , label = text "Nouvel utilisateur"
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
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just <| ChangePluginMode (LogoutMode Initial)
                        , label = text "Deconnexion"
                        }
                    ]
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Echec Connexion!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| ChangePluginMode (LoginMode Initial)
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
        , case status_ of
            Initial ->
                initialView

            Waiting ->
                waitingView

            Success ->
                successView

            Failure ->
                failureView
        ]


logoutView config status_ model =
    let
        initialView =
            column
                [ spacing 15 ]
                [ Input.button (buttonStyle True)
                    { onPress = Just Logout
                    , label = text "Se déconnecter"
                    }
                ]

        waitingView =
            column
                [ spacing 15 ]
                [ text "Traitement en cours, veuillez patienter" ]

        successView =
            column
                [ spacing 15 ]
                [ text "Déconnexion réussie!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Connexion"
                        }
                    ]
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Echec déconnexion!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| ChangePluginMode (LogoutMode Initial)
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
        [ text "Déconnexion: "
        , case status_ of
            Initial ->
                initialView

            Waiting ->
                waitingView

            Success ->
                successView

            Failure ->
                failureView
        ]
