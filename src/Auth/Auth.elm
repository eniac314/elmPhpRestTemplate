module Auth.Auth exposing (..)

import Dict exposing (..)
import Dict.Extra exposing (fromListDedupe)
import Http exposing (Error)
import Internal.Helpers exposing (PluginResult(..), Status(..), httpErrorToString)
import Internal.Logger exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline exposing (..)
import Json.Encode as Encode
import StateMachine exposing (Allowed, State(..))
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
            { login : Allowed
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
            { login : Allowed
            , codeVerification : Allowed
            }
            SignupModel
        )


type alias LoginModel =
    { username : String
    , password : String
    , loginRequestStatus : Status
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


type alias PasswordResetModel =
    { password : String
    , confirmPassword : String
    , passwordReset : Bool
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


type alias CodeVerificationModel =
    { code : String
    , codeVerificationRequestStatus : Status
    , verificationNotice : String
    , verificationEndpoint : String
    , onVerified : ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
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
    { username : String
    , email : String
    , password : String
    , confirmPassword : String
    , signupRequestStatus : Status
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


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


type Role
    = Admin
    | User


type Msg
    = SetUserName String
    | LoginRequest
    | LoginRequestResult (Result Http.Error LoginResult)
    | CodeVerificationRequest
    | CodeVerificationRequestResult (Result Http.Error CodeVerificationResult)
    | SignupRequest
    | SignupRequestResult (Result Http.Error SignupResult)
    | EmailConfirmationResult (Result Http.Error LogInfo)
    | Refresh
    | RefreshResult (Result Http.Error Bool)
    | NoOp


type alias UpdateConfig msg =
    { outMsg : Msg -> msg
    , addLogMsg : Log -> msg
    }


update : UpdateConfig msg -> Msg -> Auth -> ( Auth, Cmd msg, Maybe (PluginResult LogInfo) )
update config msg auth =
    case ( auth, msg ) of
        ( _, SetUserName name ) ->
            ( setUserName auth name
            , Cmd.none
            , Nothing
            )

        ( Login state, LoginRequest ) ->
            let
                ( newAuth, cmd, result ) =
                    validateThenLogin state
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

                Ok WrongCredentials ->
                    ( auth
                    , newLogR config
                        { logMsg = "Login error: wrong credentials"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok NeedEmailConfirmation ->
                    ( toCodeVerification
                        state
                        "You need to verify your email address"
                        "/api/confirmEmail"
                        (validateThenLogin state)
                    , Cmd.none
                    , Nothing
                    )

                Err httpError ->
                    ( auth
                    , newLogR config
                        { logMsg = "Login error: network or system error"
                        , details = Just <| httpErrorToString httpError
                        , isError = True
                        , isImportant = False
                        }
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

        ( _, Refresh ) ->
            case getLogInfo auth of
                LoggedIn _ ->
                    ( auth
                    , Cmd.map config.outMsg refresh
                    , Nothing
                    )

                LoggedOut ->
                    ( initState
                    , Cmd.none
                    , Nothing
                    )

        ( _, NoOp ) ->
            ( auth
            , Cmd.none
            , Nothing
            )

        _ ->
            ( auth
            , Cmd.none
            , Nothing
            )


setUserName : Auth -> String -> Auth
setUserName auth name =
    let
        setStateUserName : String -> State t { a | username : String } -> State t { a | username : String }
        setStateUserName s state =
            StateMachine.map (\m -> { m | username = s }) state
    in
    case auth of
        Login state ->
            setStateUserName name state
                |> Login

        --Signup state ->
        --    setUserName name state
        --        |> SignUpForm
        _ ->
            auth



--setAdminError : Auth -> String -> Auth
--setAdminError auth error =
--    let setStateAdminError : String -> State t { a | validationErrors : ValidationErrors }
-------------------------------------------------------------------------------
--# ***************************** #--
--# ***** State transitions ***** #--


init =
    ( initState
    , Cmd.none
    , Nothing
    )


initState =
    { username = ""
    , password = ""
    , loginRequestStatus = Initial
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }
        |> State
        |> Login


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
            { newEmail = ""
            , password = ""
            , confirmPassword = ""
            , userProfile = userProfile
            , showValidationErrors = False
            , validationErrors = Dict.empty
            }
                |> State
                |> UserControlPanel

        Admin ->
            { newEmail = ""
            , password = ""
            , confirmPassword = ""
            , userProfile = userProfile
            , showValidationErrors = False
            , validationErrors = Dict.empty
            }
                |> State
                |> AdminControlPanel


toCodeVerification :
    State
        { a
            | userControlPanel : Allowed
            , adminControlPanel : Allowed
        }
        m
    -> String
    -> String
    -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
    -> Auth
toCodeVerification state notice verificationEndpoint onVerified =
    { code = ""
    , codeVerificationRequestStatus = Initial
    , verificationNotice = notice
    , verificationEndpoint = verificationEndpoint
    , onVerified = onVerified
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }
        |> State
        |> CodeVerification



--# ***** State transitions ***** #--
--# ***************************** #--
-------------------------------------------------------------------------------
--# **************************************** #--
--# ***** Http requests and validation ***** #--
-- # *********** # --
-- # ** Login ** # --


validateThenLogin : State t LoginModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenLogin (State data) =
    case
        validateErrorDict
            (Validate.all
                [ ifBlank .username ( "username", "Please enter a username." )
                , ifBlank .password ( "password", "Please enter a password" )
                ]
            )
            data
    of
        Ok validData ->
            ( State
                { data
                    | username = data.username
                    , password = data.password
                    , loginRequestStatus = Waiting
                }
                |> Login
            , login validData
            , Nothing
            )

        Err errors ->
            ( State { data | showValidationErrors = True, validationErrors = errors }
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
    | WrongCredentials
    | NeedEmailConfirmation


decodeLoginResult : Decode.Decoder LoginResult
decodeLoginResult =
    Decode.oneOf
        [ Decode.field "message" decodeUserProfile
            |> Decode.map LoginSuccess
        , Decode.field "message" decodeWrongCredentials
        , Decode.field "message" decodeNeedEmailConfirmation
        ]


decodeWrongCredentials =
    decodeConstant "WRONG CREDENTIALS" WrongCredentials ""


decodeNeedEmailConfirmation =
    decodeConstant "NEED EMAIL CONFIRMATION" NeedEmailConfirmation ""


decodeUserProfile : Decode.Decoder UserProfile
decodeUserProfile =
    Decode.succeed UserProfile
        |> Pipeline.required "username" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "role" decodeRole


decodeRole : Decode.Decoder Role
decodeRole =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "ADMIN" ->
                        Decode.succeed Admin

                    "USER" ->
                        Decode.succeed User

                    otherwise ->
                        Decode.fail (otherwise ++ " is not a valid role")
            )



-- # ** Login ** # --
-- # *********** # --
-- # *********************** # --
-- # ** Code verification ** # --


validateThenVerifyCode : State t CodeVerificationModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenVerifyCode (State data) =
    case
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
                    ( "confirmationCode", "The code is invalid" )
                , ifTrue
                    (\m ->
                        m.codeVerificationRequestStatus
                            == Waiting
                            || m.codeVerificationRequestStatus
                            == Success
                    )
                    ( "admin", "Can't verify code now" )
                ]
            )
            data
    of
        Ok validData ->
            ( State { data | codeVerificationRequestStatus = Waiting }
                |> CodeVerification
            , verifyCode validData
            , Nothing
            )

        Err errors ->
            ( State { data | showValidationErrors = True, validationErrors = errors }
                |> CodeVerification
            , Cmd.none
            , Nothing
            )


verifyCode : Valid { a | code : String, verificationEndpoint : String } -> Cmd Msg
verifyCode validData =
    let
        model =
            fromValid validData

        body =
            Encode.object
                [ ( "confirmationCode"
                  , Encode.int
                        (String.toInt model.code
                            |> Maybe.withDefault 0
                        )
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = model.verificationEndpoint
        , body = body
        , expect = Http.expectJson CodeVerificationRequestResult decodeCodeVerificationResult
        }


type CodeVerificationResult
    = CodeVerificationSuccess
    | CodeVerificationFailure
    | CodeVerificationTooManyAttemps


decodeCodeVerificationResult : Decode.Decoder CodeVerificationResult
decodeCodeVerificationResult =
    Decode.oneOf
        [ Decode.field "message" decodeCodeVerificationSuccess
        , Decode.field "serverError" decodeCodeVerificationFailure
        , Decode.field "serverError" decodeCodeVerificationTooManyAttemps
        ]


decodeCodeVerificationSuccess =
    decodeConstant "CODE VERIFICATION SUCCESSFUL" CodeVerificationSuccess ""


decodeCodeVerificationFailure =
    decodeConstant "CODE VERIFICATION FAILURE" CodeVerificationFailure ""


decodeCodeVerificationTooManyAttemps =
    decodeConstant "CODE VERIFICATION TOO MANY ATTEMPS" CodeVerificationTooManyAttemps ""



-- # ** Code verification ** # --
-- # *********************** # --
-- # ************ # --
-- # ** Signup ** # --


validateThenSignup : State t SignupModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenSignup (State data) =
    case
        validateErrorDict
            (Validate.all
                [ ifBlank .username ( "username", "Please enter a username." )
                , ifBlank .password ( "password", "Please enter a password" )
                , ifBlank .confirmPassword ( "confirmPassword", "Please confirm your password" )
                , ifFalse (\m -> m.password == m.confirmPassword)
                    ( "confirmPassword", "Passwords are not matching" )
                , ifBlank .email ( "email", "Please enter an email" )
                , ifInvalidEmail .email (\e -> ( "email", "Invalid email: " ++ e ))
                ]
            )
            data
    of
        Ok validData ->
            ( State { data | signupRequestStatus = Waiting }
                |> Signup
            , Cmd.none
            , Nothing
            )

        Err errors ->
            ( State { data | showValidationErrors = True, validationErrors = errors }
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


decodeSignupResult =
    Decode.oneOf
        [ Decode.field "message" decodeSignupSuccess ]


decodeSignupSuccess =
    decodeConstant "SIGNUP SUCCESSFUL" SignupSuccess ""



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


decodeConstant c v e =
    Decode.string
        |> Decode.andThen
            (\s ->
                if s == c then
                    Decode.succeed v

                else
                    Decode.fail e
            )
