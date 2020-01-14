module Auth.Auth exposing
    ( Auth
    , LogInfo(..)
    , Msg
    , getLogInfo
    , init
    , isLogged
    , subscriptions
    , update
    , validateModelIfShowError
    , view
    )

import Auth.Common exposing (..)
import Auth.Internal.Signup as Signup
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


type alias LoginModel =
    { username : String
    , password : String
    , requestStatus : Status
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


type alias PasswordResetModel =
    { email : String
    , password : String
    , confirmPassword : String
    , encryptedSelectorAndToken : String
    , passwordResetStatus : Status
    , newPasswordRegistrationStatus : Status
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


type alias CodeVerificationModel =
    { code : String
    , email : String
    , askForEmail : Bool
    , requestStatus : Status
    , resendRequestStatus : Status
    , verificationNotice : String
    , verificationEndpoint : String
    , onVerified : Decode.Value -> Msg
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


type alias UserControlPanelModel =
    { newEmail : String
    , password : String
    , confirmPassword : String
    , userProfile : UserProfile
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


type alias AdminControlPanelModel =
    { newEmail : String
    , password : String
    , confirmPassword : String
    , userProfile : UserProfile
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


type alias SignupModel =
    Signup.SignupModel



--{ username : String
--, email : String
--, password : String
--, confirmPassword : String
--, requestStatus : Status
--, showValidationErrors : Bool
--, validationErrors : ValidationErrors
--}


type alias LogoutModel =
    { requestStatus : Status }



--type alias FieldId =
--    String
--type alias ValidationErrors =
--    Dict FieldId (List String)


compileErrors : List ( FieldId, String ) -> ValidationErrors
compileErrors xs =
    List.map (\( k, a ) -> ( k, [ a ] )) xs
        |> Dict.Extra.fromListDedupe (++)


validateErrorDict validator model =
    validate validator model
        |> Result.mapError compileErrors


type LogInfo
    = LoggedIn UserProfile
    | LoggedOut


type alias UserProfile =
    { username : String
    , email : String
    , role : Role
    }


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


type Role
    = Admin
    | User


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
                        { initCodeVerificationModel
                            | verificationNotice = "You need to verify your email address"
                            , verificationEndpoint = "/api/confirmEmail"
                            , askForEmail = True
                            , onVerified =
                                always <|
                                    ToLogin
                                        { initLoginModel
                                            | username = (untag state).username
                                            , password = (untag state).password
                                        }
                                        True
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

        ( CodeVerification state, SetVerificationCode code ) ->
            ( StateMachine.map (\m -> { m | code = code }) state
                |> CodeVerification
            , Cmd.none
            , Nothing
            )

        ( CodeVerification state, CodeVerificationRequest ) ->
            let
                ( newAuth, cmd, result ) =
                    validateThenVerifyCode state
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( CodeVerification state, CodeVerificationRequestResult res ) ->
            case res of
                Ok (CodeVerificationSuccess payload) ->
                    let
                        ( newAuth, cmd, result ) =
                            update config ((untag state).onVerified payload) auth
                    in
                    ( newAuth
                    , cmd
                    , result
                    )

                Ok CodeVerificationFailure ->
                    ( CodeVerification (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Code verification failure"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok CodeVerificationTooManyAttempts ->
                    ( CodeVerification (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Code verification failure: too many attempts"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Err httpError ->
                    ( CodeVerification (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Code verification error: network or system error"
                        , details = Just <| httpErrorToString httpError
                        , isError = True
                        , isImportant = False
                        }
                    , Nothing
                    )

        ( CodeVerification state, ToLogin loginModel autoLogin ) ->
            let
                ( newAuth, cmd, result ) =
                    toLogin state loginModel autoLogin
            in
            ( newAuth
            , Cmd.map config.outMsg cmd
            , result
            )

        ( CodeVerification state, ToCodeVerification codeVerificationModel ) ->
            ( toCodeVerification state codeVerificationModel
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
                        { initCodeVerificationModel
                            | verificationNotice = "You need to verify your email address"
                            , verificationEndpoint = "/api/confirmEmail"
                            , email = (untag state).email
                            , onVerified =
                                always <|
                                    ToLogin
                                        { initLoginModel
                                            | username = (untag state).username
                                            , password = (untag state).password
                                        }
                                        True
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



--_ ->
--    ( auth
--    , Cmd.none
--    , Nothing
--    )
--validateIfShowError (State state) =
--    if state.showValidationErrors then


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

        CodeVerification state ->
            setStateEmail state email
                |> CodeVerification

        _ ->
            auth


setStateRequestStatus :
    State t { a | requestStatus : Status }
    -> Status
    -> State t { a | requestStatus : Status }
setStateRequestStatus state status =
    StateMachine.map (\m -> { m | requestStatus = status }) state


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



--setAdminError : Auth -> String -> Auth
--setAdminError auth error =
--    let setStateAdminError : String -> State t { a | validationErrors : ValidationErrors }
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
    { initLoginModel
        | username = username
        , password = password
    }
        |> State
        |> Login


initLoginModel : LoginModel
initLoginModel =
    { username = ""
    , password = ""
    , requestStatus = Initial
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


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


initUserControlPanelModel : UserProfile -> UserControlPanelModel
initUserControlPanelModel userProfile =
    { newEmail = ""
    , password = ""
    , confirmPassword = ""
    , userProfile = userProfile
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


initAdminControlPanelModel : UserProfile -> AdminControlPanelModel
initAdminControlPanelModel userProfile =
    { newEmail = ""
    , password = ""
    , confirmPassword = ""
    , userProfile = userProfile
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


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


initCodeVerificationModel : CodeVerificationModel
initCodeVerificationModel =
    { code = ""
    , email = ""
    , askForEmail = False
    , requestStatus = Initial
    , resendRequestStatus = Initial
    , verificationNotice = ""
    , verificationEndpoint = ""
    , onVerified = always NoOp
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


toCodeVerification :
    State { a | codeVerification : Allowed } m
    -> CodeVerificationModel
    -> Auth
toCodeVerification state codeVerificationModel =
    codeVerificationModel
        |> State
        |> CodeVerification


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



--initSignupModel : SignupModel
--initSignupModel =
--    { username = ""
--    , email = ""
--    , password = ""
--    , confirmPassword = ""
--    , requestStatus = Initial
--    , showValidationErrors = False
--    , validationErrors = Dict.empty
--    }


toSignup :
    State { a | signup : Allowed } m
    -> SignupModel
    -> Auth
toSignup state signupModel =
    signupModel
        |> State
        |> Signup


initLogoutModel : LogoutModel
initLogoutModel =
    { requestStatus = Initial }


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
-- # *********** # --
-- # ** Login ** # --


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
            , login validData
            , Nothing
            )

        Err errors ->
            ( State { model | showValidationErrors = True }
                |> Login
            , Cmd.none
            , Nothing
            )


login : Valid { a | username : String, password : String } -> Cmd Msg
login validData =
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
        , expect = Http.expectJson LoginRequestResult decodeLoginResult
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



--Decode.string
--    |> Decode.andThen
--        (\s ->
--            case s of
--                "ADMIN" ->
--                    Decode.succeed Admin
--                "USER" ->
--                    Decode.succeed User
--                otherwise ->
--                    Decode.fail (otherwise ++ " is not a valid role")
--        )
-- # ** Login ** # --
-- # *********** # --
-- # *********************** # --
-- # ** Code verification ** # --


validateThenResendConfirmation : State t CodeVerificationModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenResendConfirmation (State model) =
    case
        validateErrorDict validateEmail model
    of
        Ok validData ->
            ( State { model | resendRequestStatus = Waiting }
                |> CodeVerification
            , resendConfirmation validData
            , Nothing
            )

        Err errors ->
            ( State { model | showValidationErrors = True }
                |> CodeVerification
            , Cmd.none
            , Nothing
            )


validateThenVerifyCode : State t CodeVerificationModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenVerifyCode (State model) =
    case
        validateCodeVerification model
    of
        Ok validData ->
            ( State { model | requestStatus = Waiting }
                |> CodeVerification
            , verifyCode validData
            , Nothing
            )

        Err errors ->
            ( State { model | showValidationErrors = True }
                |> CodeVerification
            , Cmd.none
            , Nothing
            )


verifyCode : Valid { a | code : String, verificationEndpoint : String, email : String } -> Cmd Msg
verifyCode validData =
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
        , expect = Http.expectJson CodeVerificationRequestResult decodeCodeVerificationResult
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


resendConfirmation : Valid { a | email : String } -> Cmd Msg
resendConfirmation validData =
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
        , expect = Http.expectJson ResendConfirmationRequestResult decodeResendConfirmationResult
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



-- # ** Code verification ** # --
-- # *********************** # --
-- # ************ # --
-- # ** Signup ** # --


validateThenSignup : State t SignupModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenSignup (State data) =
    case validateSignup data of
        Ok validData ->
            ( State { data | requestStatus = Waiting }
                |> Signup
            , signup validData
            , Nothing
            )

        Err errors ->
            ( State { data | showValidationErrors = True }
                |> Signup
            , Cmd.none
            , Nothing
            )


signup : Valid { a | username : String, password : String, email : String } -> Cmd Msg
signup validData =
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
        , expect = Http.expectJson SignupRequestResult decodeSignupResult
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



-- # ** Signup ** # --
-- # ************ # --


refresh : Cmd Msg
refresh =
    Http.get
        { url = "/api/refresh"
        , expect = Http.expectJson RefreshResult decodeRefresh
        }


decodeRefresh =
    Decode.field "message" Decode.string
        |> Decode.map (\s -> s == "success!")


decodeConstant c v =
    Decode.string
        |> Decode.andThen
            (\s ->
                if s == c then
                    Decode.succeed v

                else
                    Decode.fail "wrong constant"
            )



-- # **************** # --
-- # ** Validators ** # --


validateUsername =
    ifBlank .username ( "username", "Please enter a username." )


validatePassword =
    ifBlank .password ( "password", "Please enter a password" )


validateConfirmPasword =
    ifBlank .confirmPassword ( "confirmPassword", "Please confirm your password" )


validateEmail =
    Validate.all
        [ ifBlank .email ( "email", "Please enter an email" )
        , ifInvalidEmail .email (\e -> ( "email", "Invalid email: " ++ e ))
        ]


validateLogin =
    validateErrorDict
        (Validate.all
            [ validateUsername
            , validatePassword
            ]
        )


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



-- # ** Validators ** # --
-- # **************** # --
--# ***** Http requests and validation ***** #--
--# **************************************** #--
-------------------------------------------------------------------------------
--# ************************** #--
--# ***** View functions ***** #--


type alias ViewConfig msg =
    { outMsg : Msg -> msg }


view : ViewConfig msg -> Auth -> Element msg
view config auth =
    Element.map config.outMsg <|
        case auth of
            Login state ->
                loginView config (untag state)

            PasswordReset state ->
                passwordResetView config (untag state)

            CodeVerification state ->
                codeVerificationView config (untag state)

            UserControlPanel state ->
                userControlView config (untag state)

            AdminControlPanel state ->
                adminControlView config (untag state)

            Signup state ->
                signupView config (untag state)

            Logout state ->
                logoutView config (untag state)


loginView : ViewConfig msg -> LoginModel -> Element Msg
loginView config model =
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
                    , handler = SetUsername
                    }
                    model_
                , customCurrentPasswordInput
                    { label = "Password: "
                    , value = model_.password
                    , tag = "password"
                    , handler = SetPassword
                    }
                    model_
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just LoginRequest
                        , label = text "Log in"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| ToSignup Signup.initSignupModel
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
                            Just <| ToLogin { model_ | requestStatus = Initial } False
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


passwordResetView : ViewConfig msg -> PasswordResetModel -> Element Msg
passwordResetView config model =
    Element.none


codeVerificationView : ViewConfig msg -> CodeVerificationModel -> Element Msg
codeVerificationView config model =
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
                    , handler = SetVerificationCode
                    }
                    model_
                , if model.askForEmail then
                    customEmailInput
                        { label = "Email:"
                        , value = model_.email
                        , tag = "email"
                        , handler = SetEmail
                        }
                        model_

                  else
                    Element.none
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just CodeVerificationRequest
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
                            Just <| ToCodeVerification model_
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


userControlView : ViewConfig msg -> UserControlPanelModel -> Element Msg
userControlView config model =
    Element.none


adminControlView : ViewConfig msg -> AdminControlPanelModel -> Element Msg
adminControlView config model =
    Element.none


signupView : ViewConfig msg -> SignupModel -> Element Msg
signupView config model =
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
                    , handler = SetUsername
                    }
                    model_
                , customEmailInput
                    { label = "Email: "
                    , value = model_.email
                    , tag = "email"
                    , handler = SetEmail
                    }
                    model_
                , customCurrentPasswordInput
                    { label = "Password: "
                    , value = model_.password
                    , tag = "password"
                    , handler = SetPassword
                    }
                    model_
                , customCurrentPasswordInput
                    { label = "Confirm password: "
                    , value = model_.confirmPassword
                    , tag = "confirmPassword"
                    , handler = SetConfirmPassword
                    }
                    model_
                , row
                    [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just SignupRequest
                        , label = text "Send"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| ToLogin initLoginModel False
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
                            Just <| ToSignup { model_ | requestStatus = Initial }
                        , label = text "Réessayer"
                        }
                    , Input.button (buttonStyle True)
                        { onPress = Just <| ToLogin initLoginModel False
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


logoutView : ViewConfig msg -> LogoutModel -> Element Msg
logoutView config model =
    let
        status =
            model.requestStatus

        initialView =
            column
                [ spacing 15 ]
                [ Input.button (buttonStyle True)
                    { onPress = Just LogoutRequest
                    , label = text "Log out"
                    }
                ]

        waitingView =
            column
                [ spacing 15 ]
                [ text "Processing request, please wait" ]

        successView =
            column
                [ spacing 15 ]
                [ text "Logout successful!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress = Just <| ToLogin initLoginModel False
                        , label = text "Log in"
                        }
                    ]
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Logout failure!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle True)
                        { onPress =
                            Just <| ToLogout initLogoutModel
                        , label = text "Try again"
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
        [ text "Logout: "
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



-- Misc


type alias CustomInputConfig =
    { label : String
    , value : String
    , tag : String
    , handler : String -> Msg
    }


customInput :
    CustomInputConfig
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element Msg
customInput config model =
    column
        [ spacing 15 ]
        [ Input.text textInputStyle
            { onChange =
                config.handler
            , text =
                config.value
            , placeholder = Nothing
            , label =
                Input.labelAbove [ centerY ]
                    (el [ width (px 110) ] (text <| config.label))
            }
        , errorView config model
        ]


customCurrentPasswordInput :
    CustomInputConfig
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element Msg
customCurrentPasswordInput config model =
    column
        [ spacing 15 ]
        [ Input.currentPassword textInputStyle
            { onChange =
                config.handler
            , text =
                config.value
            , placeholder = Nothing
            , label =
                Input.labelAbove [ centerY ]
                    (el [ width (px 110) ] (text <| config.label))
            , show = False
            }
        , errorView config model
        ]


customEmailInput :
    CustomInputConfig
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element Msg
customEmailInput config model =
    column
        [ spacing 15 ]
        [ Input.email textInputStyle
            { onChange =
                config.handler
            , text =
                config.value
            , placeholder = Nothing
            , label =
                Input.labelAbove [ centerY ]
                    (el [ width (px 110) ] (text <| config.label))
            }
        , errorView config model
        ]



--validateModelIfShowError : Validator b c -> { a | showValidationErrors : Bool, validationErrors : ValidationErrors } -> { a | showValidationErrors : Bool, validationErrors : ValidationErrors }


validateModelIfShowError validator model =
    case ( model.showValidationErrors, validator model ) of
        ( True, Err validationErrors ) ->
            { model | validationErrors = validationErrors }

        ( True, Ok _ ) ->
            { model | validationErrors = Dict.empty }

        _ ->
            model


errorView :
    CustomInputConfig
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element Msg
errorView config model =
    case
        ( model.showValidationErrors
        , Dict.get config.tag model.validationErrors
        )
    of
        ( True, Just errors ) ->
            column
                [ Font.color (Element.rgb 1 0 0)
                , Font.size 12
                , spacing 10
                ]
                (List.map text errors)

        _ ->
            Element.none



--# ***** View functions ***** #--
--# ************************** #--
-------------------------------------------------------------------------------
