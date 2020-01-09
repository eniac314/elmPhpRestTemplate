module Auth.AuthPlugin exposing
    ( LogInfo(..)
    , Model
    , Msg
    , getLogInfo
    , init
    , isLogged
    , newLogIfLogged
    , status
    , subscriptions
    , update
    , view
    )

import Dict exposing (..)
import Dict.Extra exposing (fromListDedupe)
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
import StateMachine exposing (Allowed, State(..))
import Style.Helpers exposing (buttonStyle, textInputStyle)
import Task exposing (..)
import Time exposing (..)
import Validate exposing (..)


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

        --, role : Role
        }
    | LoggedOut



--type Role =
--    SuperAdmin
--    |
--type Auth
--    = SignUpForm
--        (State { emailConfirmation : Allowed }
--            { username : String
--            , email : String
--            , password : String
--            , confirmPassword : String
--            }
--        )
--    | EmailConfirmationForm
--        (State {signup})


status model =
    case model.pluginMode of
        SignUpMode s ->
            s

        LoginMode s ->
            s

        LogoutMode s ->
            s

        EmailConfirmation s ->
            s


type alias Model msg =
    { username : String
    , email : String
    , password : String
    , confirmPassword : String
    , confirmationCode : String
    , logInfo : LogInfo
    , pluginMode : PluginMode
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    , externalMsg : Msg -> msg
    }


init externalMsg =
    ( { username = ""
      , password = ""
      , confirmPassword = ""
      , confirmationCode = ""
      , email = ""
      , logInfo = LoggedOut
      , pluginMode = LoginMode Waiting
      , showValidationErrors = False
      , validationErrors = Dict.empty
      , externalMsg = externalMsg
      }
    , Cmd.map externalMsg <| checkLoginStatus
    )


subscriptions model =
    Sub.map model.externalMsg <|
        case model.pluginMode of
            LoginMode _ ->
                Time.every (30 * 1000) (\_ -> Refresh)

            _ ->
                Sub.none


reset model =
    ( { model
        | username = Nothing
        , password = Nothing
        , confirmPassword = Nothing
        , email = Nothing
        , pluginMode = LoginMode Initial
      }
    , Cmd.none
      --login model
    )


getLogInfo model =
    model.logInfo


type PluginMode
    = SignUpMode Status
    | EmailConfirmation Status
    | LoginMode Status
    | LogoutMode Status


type Msg
    = SetUsername String
    | SetEmail String
    | SetPassword String
    | SetConfirmPassword String
    | SetConfirmCode String
    | Login
    | LoginStatusResult (Result Http.Error LogInfo)
    | LoginResult (Result Http.Error LogInfo)
    | SignUp
    | SignUpResult (Result Http.Error Bool)
    | ConfirmEmail
    | EmailConfirmationResult (Result Http.Error Bool)
    | Logout
    | LogoutResult (Result Http.Error Bool)
    | ChangePluginMode PluginMode
    | Refresh
    | RefreshResult (Result Http.Error Bool)
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

        SetEmail s ->
            ( { model | email = s }
            , Cmd.none
            , Nothing
            )

        SetConfirmCode s ->
            ( { model | confirmationCode = s }
            , Cmd.none
            , Nothing
            )

        Login ->
            case validateLogin model of
                Ok validModel ->
                    ( { model
                        | pluginMode = LoginMode Waiting
                      }
                    , login validModel
                        |> Cmd.map model.externalMsg
                    , Nothing
                    )

                Err errors ->
                    ( { model | showValidationErrors = True, validationErrors = errors }
                    , Cmd.none
                    , Nothing
                    )

        LoginStatusResult res ->
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

        LoginResult res ->
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
              --, signUp model
              --    |> Cmd.map model.externalMsg
            , Cmd.none
            , Nothing
            )

        SignUpResult res ->
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

        ConfirmEmail ->
            ( model, Cmd.none, Nothing )

        EmailConfirmationResult _ ->
            ( model, Cmd.none, Nothing )

        Logout ->
            ( { model
                | pluginMode = LogoutMode Waiting
              }
            , logout
                |> Cmd.map model.externalMsg
            , Nothing
            )

        LogoutResult res ->
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

        Refresh ->
            case model.logInfo of
                LoggedIn li ->
                    ( { model | pluginMode = LoginMode Waiting }
                    , refresh
                        |> Cmd.map model.externalMsg
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        RefreshResult res ->
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



-------------------------------------------------------------------------------
--# **************************************** #--
--# ***** Http requests and validation ***** #--


type alias FieldId =
    String


type alias ValidationErrors =
    Dict FieldId (List String)


compileErrors : List ( FieldId, String ) -> ValidationErrors
compileErrors xs =
    List.map (\( k, a ) -> ( k, [ a ] )) xs
        |> Dict.Extra.fromListDedupe (++)


validateErrorDict validator model =
    validate validator model
        |> Result.mapError compileErrors



-------------------------------------------------------------------------------


type alias LoginData a =
    { a | username : String, password : String }


validateLogin : LoginData a -> Result ValidationErrors (Valid (LoginData a))
validateLogin =
    validateErrorDict <|
        Validate.all
            [ ifBlank .username ( "username", "Please enter a username." )
            , ifBlank .password ( "password", "Please enter a password" )
            ]


login : Valid (LoginData a) -> Cmd Msg
login validModel =
    let
        model =
            fromValid validModel

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
        , expect = Http.expectJson LoginResult decodeLoginResult
        }


checkLoginStatus : Cmd Msg
checkLoginStatus =
    Http.get
        { url = "/api/loginStatus"
        , expect = Http.expectJson LoginStatusResult decodeLoginResult
        }


decodeLoginResult : Decode.Decoder LogInfo
decodeLoginResult =
    Decode.map (\a -> LoggedIn { username = a })
        (Decode.field "username" Decode.string)



-------------------------------------------------------------------------------


type alias SignUpData a =
    { a
        | username : String
        , email : String
        , password : String
        , confirmPassword : String
    }


validateSignUp : SignUpData a -> Result ValidationErrors (Valid (SignUpData a))
validateSignUp =
    validateErrorDict <|
        Validate.all
            [ ifBlank .username ( "username", "Please enter a username." )
            , ifBlank .password ( "password", "Please enter a password" )
            , ifBlank .confirmPassword ( "confirmPassword", "Please confirm your password" )
            , ifFalse (\m -> m.password == m.confirmPassword)
                ( "confirmPassword", "Passwords are not matching" )
            , ifBlank .email ( "email", "Please enter an email" )
            , ifInvalidEmail .email (\e -> ( "email", "Invalid email: " ++ e ))
            ]


signUp : Valid (SignUpData a) -> Cmd Msg
signUp validModel =
    let
        model =
            fromValid validModel

        body =
            Encode.object
                [ ( "username"
                  , Encode.string (.username model)
                  )
                , ( "password"
                  , Encode.string (.password model)
                  )
                , ( "email"
                  , Encode.string "florian.gillard@tutanota.com"
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/api/signup"
        , body = body
        , expect = Http.expectJson SignUpResult decodeSignupResult
        }


decodeSignupResult =
    Decode.field "signUpComplete" Decode.bool



-------------------------------------------------------------------------------


type alias ConfirmEmailData a =
    { a
        | email : String
        , confirmationCode : String
    }


validateConfirmEmail : ConfirmEmailData a -> Result ValidationErrors (Valid (ConfirmEmailData a))
validateConfirmEmail =
    validateErrorDict <|
        Validate.all
            [ ifBlank .email ( "email", "Please enter an email" )
            , ifInvalidEmail .email (\e -> ( "email", "Invalid email: " ++ e ))
            , ifBlank .confirmationCode ( "confirmationCode", "Please enter a confirmation code" )
            , ifFalse
                (\m ->
                    case String.toInt m.confirmationCode of
                        Just n ->
                            n >= 0 && n <= 999999

                        Nothing ->
                            False
                )
                ( "confirmationCode", "The code is invalid" )
            ]


confirmEmail : Valid (ConfirmEmailData a) -> Cmd Msg
confirmEmail validModel =
    let
        model =
            fromValid validModel
    in
    Http.post
        { url = "api/confirmMail"
        , body =
            Encode.object
                [ ( "confirmationCode"
                  , String.toInt model.confirmationCode
                        |> Maybe.withDefault 0
                        |> Encode.int
                  )
                , ( "email"
                  , Encode.string "florian.gillard@tutanota.com"
                  )
                ]
                |> Http.jsonBody
        , expect = Http.expectJson EmailConfirmationResult Decode.bool
        }


refresh : Cmd Msg
refresh =
    Http.get
        { url = "/api/refresh"
        , expect = Http.expectJson RefreshResult decodeRefresh
        }


decodeRefresh =
    Decode.field "message" Decode.string
        |> Decode.map (\s -> s == "success!")



-------------------------------------------------------------------------------


logout : Cmd Msg
logout =
    Http.get
        { url = "logout.php"
        , expect = Http.expectJson LogoutResult decodeLogoutResult
        }


decodeLogoutResult =
    Decode.field "notLoggedIn" Decode.bool



--# ***** Http requests and validation ***** #--
--# **************************************** #--
-------------------------------------------------------------------------------
--# ************************** #--
--# ***** View functions ***** #--


view config model =
    Element.map model.externalMsg <|
        case model.pluginMode of
            SignUpMode status_ ->
                signUpView config status_ model

            LoginMode status_ ->
                loginView config status_ model

            LogoutMode status_ ->
                logoutView config status_ model

            EmailConfirmation status_ ->
                Element.none


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
