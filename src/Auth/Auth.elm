module Auth.Auth exposing
    ( Auth
    , LogInfo(..)
    , Msg
    , getLogInfo
    , init
    , isLogged
    , subscriptions
    , update
    , view
    )

import Auth.Common exposing (..)
import Auth.Internal.AdminControlPanel exposing (..)
import Auth.Internal.CodeVerification exposing (..)
import Auth.Internal.Login exposing (..)
import Auth.Internal.Logout exposing (..)
import Auth.Internal.PasswordReset exposing (..)
import Auth.Internal.Signup exposing (..)
import Auth.Internal.UserControlPanel exposing (..)
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
import Http exposing (Error)
import Internal.Helpers exposing (PluginResult(..), Status(..), httpErrorToString)
import Internal.Logger exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline exposing (..)
import Json.Encode as Encode
import StateMachine exposing (Allowed, State(..), untag)
import Style.Helpers exposing (buttonStyle, textInputStyle)
import Time exposing (every)
import Validate exposing (..)


type Auth
    = Login
        (State
            { signup : Allowed
            , codeVerification : Allowed
            , passwordReset : Allowed
            , userControlPanel : Allowed
            , adminControlPanel : Allowed
            , login : Allowed
            }
            LoginModel
        )
    | PasswordReset
        (State
            { login : Allowed
            , codeVerification : Allowed
            }
            PasswordResetModel
        )
    | CodeVerification
        (Decode.Value -> Msg)
        (State
            { codeVerification : Allowed
            , login : Allowed
            , passwordReset : Allowed
            , userControlPanel : Allowed
            , adminControlPanel : Allowed
            }
            CodeVerificationModel
        )
    | UserControlPanel
        (State
            { logout : Allowed
            , codeVerification : Allowed
            }
            UserControlPanelModel
        )
    | AdminControlPanel
        (State
            { logout : Allowed
            , codeVerification : Allowed
            }
            AdminControlPanelModel
        )
    | Signup
        (State
            { signup : Allowed
            , login : Allowed
            , codeVerification : Allowed
            }
            SignupModel
        )
    | Logout
        (State
            { logout : Allowed
            , login : Allowed
            }
            LogoutModel
        )


type LogInfo
    = LoggedIn UserProfile
    | LoggedOut


getLogInfo : Auth -> LogInfo
getLogInfo auth =
    case auth of
        UserControlPanel (State { userProfile }) ->
            LoggedIn userProfile

        AdminControlPanel (State { userProfile }) ->
            LoggedIn userProfile

        _ ->
            LoggedOut


isLogged : LogInfo -> Bool
isLogged logInfo =
    case logInfo of
        LoggedIn _ ->
            True

        _ ->
            False


type Msg
    = SetUsername String
    | SetPassword String
    | SetConfirmPassword String
    | SetEmail String
    | LoginRequest
    | LoginRequestResult (Result Http.Error LoginResult)
    | RequestPasswordReset
    | SetVerificationCode String
    | CodeVerificationRequest
    | CodeVerificationRequestResult (Result Http.Error CodeVerificationResult)
    | SignupRequest
    | SignupRequestResult (Result Http.Error SignupResult)
    | EmailConfirmationResult (Result Http.Error LogInfo)
    | LogoutRequest
    | Refresh
    | RefreshResult (Result Http.Error Bool)
    | ToLogin LoginModel Bool
    | ToCodeVerification CodeVerificationModel
    | ToSignup SignupModel
    | ToLogout LogoutModel
    | ToPasswordReset PasswordResetModel
    | ResendConfirmationRequest
    | ResendConfirmationRequestResult (Result Http.Error ResendConfirmationResult)
    | NoOp


type alias UpdateConfig msg =
    { outMsg : Msg -> msg
    , addLogMsg : Log -> msg
    }


update : UpdateConfig msg -> Msg -> Auth -> ( Auth, Cmd msg, Maybe (PluginResult LogInfo) )
update config msg auth =
    case ( auth, msg ) of
        ( _, SetUsername name ) ->
            ( setUsername auth name
            , Cmd.none
            , Nothing
            )

        ( _, SetPassword password ) ->
            ( setPassword auth password
            , Cmd.none
            , Nothing
            )

        ( _, SetConfirmPassword password ) ->
            ( setConfirmPassword auth password
            , Cmd.none
            , Nothing
            )

        ( _, SetEmail email ) ->
            ( setEmail auth email
            , Cmd.none
            , Nothing
            )

        ( Login state, ToPasswordReset passwordResetModel ) ->
            ( toPasswordReset state passwordResetModel
            , Cmd.none
            , Nothing
            )

        ( Login state, LoginRequest ) ->
            let
                ( newAuth, cmd, result ) =
                    validateThenLogin (untag state)
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( Login state, LoginRequestResult res ) ->
            case res of
                Ok (LoginSuccess userProfile) ->
                    ( toLoggedState state userProfile
                    , Cmd.none
                    , Nothing
                    )

                Ok LoginWrongCredentials ->
                    ( Login (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Login error: wrong credentials"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok LoginTooManyRequests ->
                    ( Login (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Login error: too many requests"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok LoginNeedEmailConfirmation ->
                    ( toCodeVerification
                        state
                        (always <|
                            ToLogin
                                { initLogin
                                    | username = (untag state).username
                                    , password = (untag state).password
                                }
                                True
                        )
                        { initCodeVerificationModel
                            | verificationNotice = "You need to verify your email address"
                            , verificationEndpoint = "/api/confirmEmail"
                            , askForEmail = True

                            --, onVerified =
                        }
                    , Cmd.none
                    , Nothing
                    )

                Err httpError ->
                    ( Login (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Login error: network or system error"
                        , details = Just <| httpErrorToString httpError
                        , isError = True
                        , isImportant = False
                        }
                    , Nothing
                    )

        ( Login state, ToLogin loginModel autoLogin ) ->
            let
                ( newAuth, cmd, result ) =
                    toLogin state loginModel autoLogin
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( Login state, ToSignup signupModel ) ->
            ( toSignup state signupModel
            , Cmd.none
            , Nothing
            )

        ( CodeVerification onVerified state, SetVerificationCode code ) ->
            ( StateMachine.map (\m -> { m | code = code }) state
                |> CodeVerification onVerified
            , Cmd.none
            , Nothing
            )

        ( CodeVerification onVerified state, CodeVerificationRequest ) ->
            let
                ( newAuth, cmd, result ) =
                    validateThenVerifyCode onVerified state
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( CodeVerification onVerified state, CodeVerificationRequestResult res ) ->
            case res of
                Ok (CodeVerificationSuccess payload) ->
                    let
                        ( newAuth, cmd, result ) =
                            update config (onVerified payload) auth
                    in
                    ( newAuth
                    , cmd
                    , result
                    )

                Ok CodeVerificationFailure ->
                    ( CodeVerification onVerified (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Code verification failure"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok CodeVerificationTooManyAttempts ->
                    ( CodeVerification onVerified (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Code verification failure: too many attempts"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Err httpError ->
                    ( CodeVerification onVerified (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Code verification error: network or system error"
                        , details = Just <| httpErrorToString httpError
                        , isError = True
                        , isImportant = False
                        }
                    , Nothing
                    )

        ( CodeVerification onVerified state, ToLogin loginModel autoLogin ) ->
            let
                ( newAuth, cmd, result ) =
                    toLogin state loginModel autoLogin
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( CodeVerification onVerified state, ToCodeVerification codeVerificationModel ) ->
            ( toCodeVerification state onVerified codeVerificationModel
            , Cmd.none
            , Nothing
            )

        ( Signup state, ToSignup signupModel ) ->
            ( toSignup state signupModel
            , Cmd.none
            , Nothing
            )

        ( Signup state, ToLogin loginModel autoLogin ) ->
            let
                ( newAuth, cmd, result ) =
                    toLogin state loginModel autoLogin
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( Signup state, SignupRequest ) ->
            let
                ( newAuth, cmd, result ) =
                    validateThenSignup state
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( Signup state, SignupRequestResult res ) ->
            case res of
                Ok SignupSuccess ->
                    ( toCodeVerification
                        (setStateRequestStatus state Success)
                        (always <|
                            ToLogin
                                { initLogin
                                    | username = (untag state).username
                                    , password = (untag state).password
                                }
                                True
                        )
                        { initCodeVerificationModel
                            | verificationNotice = "You need to verify your email address"
                            , verificationEndpoint = "/api/confirmEmail"
                            , email = (untag state).email

                            --, onVerified =
                        }
                    , Cmd.none
                    , Nothing
                    )

                Ok SignupInvalidEmail ->
                    ( Signup <| setStateRequestStatus state Failure
                    , newLogR config
                        { logMsg = "Signup error: invalid email"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok SignupUserAlreadyExists ->
                    ( Signup <| setStateRequestStatus state Failure
                    , newLogR config
                        { logMsg = "Signup error: user already exists"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok SignupTooManyRequests ->
                    ( Signup <| setStateRequestStatus state Failure
                    , newLogR config
                        { logMsg = "Signup error: too many requests"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok SignupInvalidPassword ->
                    ( Signup <| setStateRequestStatus state Failure
                    , newLogR config
                        { logMsg = "Signup error: invalid password"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Err httpError ->
                    ( Signup <| setStateRequestStatus state Failure
                    , newLogR config
                        { logMsg = "Signup error: network or system error"
                        , details = Just <| httpErrorToString httpError
                        , isError = True
                        , isImportant = False
                        }
                    , Nothing
                    )

        ( _, Refresh ) ->
            case getLogInfo auth of
                LoggedIn _ ->
                    ( auth
                    , Cmd.map config.outMsg refresh
                    , Nothing
                    )

                LoggedOut ->
                    ( initAuth "" ""
                    , Cmd.none
                    , Nothing
                    )

        ( _, NoOp ) ->
            ( auth
            , Cmd.none
            , Nothing
            )

        ignoredMsg ->
            ( auth
            , newLogR config
                { logMsg = "Ignored message:"
                , details = Just <| Debug.toString ignoredMsg
                , isError = True
                , isImportant = True
                }
            , Nothing
            )


subscriptions : (Msg -> msg) -> Auth -> Sub msg
subscriptions outMsg auth =
    let
        refreshSub =
            Time.every (30 * 1000) (\_ -> Refresh)
    in
    Sub.map outMsg <|
        case auth of
            AdminControlPanel _ ->
                Time.every (30 * 1000) (\_ -> Refresh)

            UserControlPanel _ ->
                Time.every (30 * 1000) (\_ -> Refresh)

            _ ->
                Sub.none



-------------------------------------------------------------------------------
--# ***************************** #--
--# ***** State transitions ***** #--


init : ( Auth, Cmd msg )
init =
    ( initAuth "" ""
    , Cmd.none
    )


initAuth : String -> String -> Auth
initAuth username password =
    { initLogin
        | username = username
        , password = password
    }
        |> State
        |> Login


toLogin :
    State { a | login : Allowed } m
    -> LoginModel
    -> Bool
    -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
toLogin _ state autoLogin =
    let
        newAuth =
            Login (State state)

        result =
            if autoLogin then
                validateThenLogin state

            else
                ( newAuth, Cmd.none, Nothing )
    in
    result


toLoggedState :
    State
        { a
            | userControlPanel : Allowed
            , adminControlPanel : Allowed
        }
        m
    -> UserProfile
    -> Auth
toLoggedState state userProfile =
    case userProfile.role of
        User ->
            initUserControlPanelModel userProfile
                |> State
                |> UserControlPanel

        Admin ->
            initAdminControlPanelModel userProfile
                |> State
                |> AdminControlPanel


toCodeVerification :
    State { a | codeVerification : Allowed } m
    -> (Decode.Value -> Msg)
    -> CodeVerificationModel
    -> Auth
toCodeVerification state onVerified codeVerificationModel =
    codeVerificationModel
        |> State
        |> CodeVerification onVerified


initPasswordResetModel : PasswordResetModel
initPasswordResetModel =
    { email = ""
    , password = ""
    , confirmPassword = ""
    , encryptedSelectorAndToken = ""
    , passwordResetStatus = Initial
    , newPasswordRegistrationStatus = Initial
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


toPasswordReset :
    State { a | passwordReset : Allowed } m
    -> PasswordResetModel
    -> Auth
toPasswordReset state passwordResetModel =
    passwordResetModel
        |> State
        |> PasswordReset


toSignup :
    State { a | signup : Allowed } m
    -> SignupModel
    -> Auth
toSignup state signupModel =
    signupModel
        |> State
        |> Signup


toLogout :
    State { a | logout : Allowed } m
    -> LogoutModel
    -> Auth
toLogout state logoutModel =
    logoutModel
        |> State
        |> Logout



--# ***** State transitions ***** #--
--# ***************************** #--
-------------------------------------------------------------------------------
--# **************************************** #--
--# ***** Http requests and validation ***** #--


validateThenLogin : LoginModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenLogin model =
    case
        validateLogin model
    of
        Ok validData ->
            ( State
                { model
                    | username = model.username
                    , password = model.password
                    , requestStatus = Waiting
                }
                |> Login
            , login validData LoginRequestResult
            , Nothing
            )

        Err errors ->
            ( State { model | showValidationErrors = True }
                |> Login
            , Cmd.none
            , Nothing
            )


validateThenResendConfirmation : (Decode.Value -> Msg) -> State t CodeVerificationModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenResendConfirmation onVerified (State model) =
    case
        validateErrorDict validateEmail model
    of
        Ok validData ->
            ( State { model | resendRequestStatus = Waiting }
                |> CodeVerification onVerified
            , resendConfirmation validData ResendConfirmationRequestResult
            , Nothing
            )

        Err errors ->
            ( State { model | showValidationErrors = True }
                |> CodeVerification onVerified
            , Cmd.none
            , Nothing
            )


validateThenVerifyCode : (Decode.Value -> Msg) -> State t CodeVerificationModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenVerifyCode onVerified (State model) =
    case
        validateCodeVerification model
    of
        Ok validData ->
            ( State { model | requestStatus = Waiting }
                |> CodeVerification onVerified
            , verifyCode validData CodeVerificationRequestResult
            , Nothing
            )

        Err errors ->
            ( State { model | showValidationErrors = True }
                |> CodeVerification onVerified
            , Cmd.none
            , Nothing
            )


validateThenSignup : State t SignupModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenSignup (State data) =
    case validateSignup data of
        Ok validData ->
            ( State { data | requestStatus = Waiting }
                |> Signup
            , signup validData SignupRequestResult
            , Nothing
            )

        Err errors ->
            ( State { data | showValidationErrors = True }
                |> Signup
            , Cmd.none
            , Nothing
            )


refresh : Cmd Msg
refresh =
    Http.get
        { url = "/api/refresh"
        , expect = Http.expectJson RefreshResult decodeRefresh
        }


decodeRefresh =
    Decode.field "message" Decode.string
        |> Decode.map (\s -> s == "success!")



--# ***** Http requests and validation ***** #--
--# **************************************** #--
-------------------------------------------------------------------------------
--# ************************* #--
--# ***** View function ***** #--


type alias ViewConfig msg =
    { outMsg : Msg -> msg }


view : ViewConfig msg -> Auth -> Element msg
view config auth =
    Element.map config.outMsg <|
        case auth of
            Login state ->
                let
                    model =
                        untag state
                in
                loginView
                    { setUsername = SetUsername
                    , setPassword = SetPassword
                    , loginRequest = LoginRequest
                    , toLogin = ToLogin { model | requestStatus = Initial } False
                    , toSignup = ToSignup initSignupModel
                    }
                    model

            PasswordReset state ->
                let
                    model =
                        untag state
                in
                passwordResetView {} model

            CodeVerification onVerified state ->
                let
                    model =
                        untag state
                in
                codeVerificationView
                    { setVerificationCode = SetVerificationCode
                    , setEmail = SetEmail
                    , codeVerificationRequest = CodeVerificationRequest
                    , toCodeVerification = ToCodeVerification initCodeVerificationModel
                    }
                    model

            UserControlPanel state ->
                let
                    model =
                        untag state
                in
                userControlView {} model

            AdminControlPanel state ->
                let
                    model =
                        untag state
                in
                adminControlView {} model

            Signup state ->
                let
                    model =
                        untag state
                in
                signupView
                    { setUsername = SetUsername
                    , setEmail = SetEmail
                    , setPassword = SetPassword
                    , setConfirmPassword = SetConfirmPassword
                    , signupRequest = SignupRequest
                    , toLogin = ToLogin initLogin False
                    , toSignup = ToSignup { model | requestStatus = Initial }
                    }
                    model

            Logout state ->
                let
                    model =
                        untag state
                in
                logoutView
                    { logoutRequest = LogoutRequest
                    , toLogin = ToLogin initLogin False
                    , toLogout = ToLogout initLogoutModel
                    }
                    model



--# ***** View function ***** #--
--# ************************* #--
-------------------------------------------------------------------------------
--# ************************** #--
--# ***** Common setters ***** #--


setUsername : Auth -> String -> Auth
setUsername auth name =
    let
        setStateUsername : State t { a | username : String } -> String -> State t { a | username : String }
        setStateUsername state s =
            StateMachine.map (\m -> { m | username = s }) state
    in
    case auth of
        Login state ->
            setStateUsername state name
                |> Login

        Signup state ->
            setStateUsername state name
                |> Signup

        _ ->
            auth


setPassword : Auth -> String -> Auth
setPassword auth name =
    let
        setStatePassword state s =
            StateMachine.map (\m -> { m | password = s }) state
    in
    case auth of
        Login state ->
            setStatePassword state name
                |> Login

        Signup state ->
            setStatePassword state name
                |> Signup

        _ ->
            auth


setConfirmPassword : Auth -> String -> Auth
setConfirmPassword auth name =
    let
        setStateConfirmPassword state s =
            StateMachine.map (\m -> { m | confirmPassword = s }) state
    in
    case auth of
        Signup state ->
            setStateConfirmPassword state name
                |> Signup

        PasswordReset state ->
            setStateConfirmPassword state name
                |> PasswordReset

        _ ->
            auth


setEmail : Auth -> String -> Auth
setEmail auth email =
    let
        setStateEmail state s =
            StateMachine.map (\m -> { m | email = s }) state
    in
    case auth of
        Signup state ->
            setStateEmail state email
                |> Signup

        PasswordReset state ->
            setStateEmail state email
                |> PasswordReset

        CodeVerification onVerified state ->
            setStateEmail state email
                |> CodeVerification onVerified

        _ ->
            auth


setStateRequestStatus :
    State t { a | requestStatus : Status }
    -> Status
    -> State t { a | requestStatus : Status }
setStateRequestStatus state status =
    StateMachine.map (\m -> { m | requestStatus = status }) state



--# ***** Common setters ***** #--
--# ************************** #--
