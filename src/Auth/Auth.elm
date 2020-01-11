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
            { signup : Allowed
            , login : Allowed
            , codeVerification : Allowed
            }
            SignupModel
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
    , requestStatus : Status
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
    { username : String
    , email : String
    , password : String
    , confirmPassword : String
    , requestStatus : Status
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
    | LoginRequest
    | ToLogin LoginModel Bool
    | ToSignup SignupModel
    | LoginRequestResult (Result Http.Error LoginResult)
    | ToPasswordReset
    | RequestPasswordReset
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
        ( _, SetUsername name ) ->
            ( setUsername auth name
            , Cmd.none
            , Nothing
            )

        ( Login state, ToPasswordReset ) ->
            ( toPasswordReset state initPasswordResetModel
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

                Ok WrongCredentials ->
                    ( Login (setStateRequestStatus state Failure)
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
                        (always <|
                            ToLogin
                                { initLoginModel
                                    | username = (untag state).username
                                    , password = (untag state).password
                                }
                                True
                        )
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

                Ok CodeVerificationTooManyAttemps ->
                    ( CodeVerification (setStateRequestStatus state Failure)
                    , newLogR config
                        { logMsg = "Code verification failure: too many attemps"
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

        ( Signup state, ToSignup signupModel ) ->
            ( toSignup state signupModel
            , Cmd.none
            , Nothing
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
                        state
                        "You need to verify your email address"
                        "/api/confirmEmail"
                        (always <|
                            ToLogin
                                { initLoginModel
                                    | username = (untag state).username
                                    , password = (untag state).password
                                }
                                True
                        )
                    , Cmd.none
                    , Nothing
                    )

                Ok SignupInvalidEmail ->
                    ( auth
                    , newLogR config
                        { logMsg = "Signup error: invalid email"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok SignupUserAlreadyExists ->
                    ( auth
                    , newLogR config
                        { logMsg = "Signup error: username already taken"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Ok SignupTooManyRequests ->
                    ( auth
                    , newLogR config
                        { logMsg = "Signup error: too many requests"
                        , details = Nothing
                        , isError = True
                        , isImportant = True
                        }
                    , Nothing
                    )

                Err httpError ->
                    ( auth
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

        _ ->
            ( auth
            , Cmd.none
            , Nothing
            )


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


toCodeVerification :
    State { a | codeVerification : Allowed } m
    -> String
    -> String
    -> (Decode.Value -> Msg)
    -> Auth
toCodeVerification state notice verificationEndpoint onVerified =
    { code = ""
    , requestStatus = Initial
    , verificationNotice = notice
    , verificationEndpoint = verificationEndpoint
    , onVerified = onVerified
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }
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


toSignup :
    State { a | signup : Allowed } m
    -> SignupModel
    -> Auth
toSignup state signupModel =
    signupModel
        |> State
        |> Signup



--# ***** State transitions ***** #--
--# ***************************** #--
-------------------------------------------------------------------------------
--# **************************************** #--
--# ***** Http requests and validation ***** #--
-- # *********** # --
-- # ** Login ** # --


validateThenLogin : LoginModel -> ( Auth, Cmd Msg, Maybe (PluginResult LogInfo) )
validateThenLogin data =
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
                    , requestStatus = Waiting
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
    decodeConstant "WRONG CREDENTIALS" WrongCredentials


decodeNeedEmailConfirmation =
    decodeConstant "NEED EMAIL CONFIRMATION" NeedEmailConfirmation


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
                        m.requestStatus
                            == Waiting
                            || m.requestStatus
                            == Success
                    )
                    ( "admin", "Can't verify code now" )
                ]
            )
            data
    of
        Ok validData ->
            ( State { data | requestStatus = Waiting }
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
    = CodeVerificationSuccess Decode.Value
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
    Decode.field "codeVerificationPayload" Decode.value
        |> Decode.map CodeVerificationSuccess


decodeCodeVerificationFailure =
    decodeConstant "CODE VERIFICATION FAILURE" CodeVerificationFailure


decodeCodeVerificationTooManyAttemps =
    decodeConstant "CODE VERIFICATION TOO MANY ATTEMPS" CodeVerificationTooManyAttemps



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
            ( State { data | requestStatus = Waiting }
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
    | SignupInvalidEmail
    | SignupUserAlreadyExists
    | SignupTooManyRequests


decodeSignupResult =
    Decode.oneOf
        [ Decode.field "message" decodeSignupSuccess
        , Decode.field "serverError" decodeSignupInvalidEmail
        , Decode.field "serverError" decodeSignupUserAlreadyExists
        , Decode.field "serverError" decodeSignupTooManyRequests
        ]


decodeSignupSuccess =
    decodeConstant "SIGNUP SUCCESSFUL" SignupSuccess


decodeSignupInvalidEmail =
    decodeConstant "INVALID EMAIL ADDRESS" SignupInvalidEmail


decodeSignupUserAlreadyExists =
    decodeConstant "USER ALREADY EXISTS" SignupUserAlreadyExists


decodeSignupTooManyRequests =
    decodeConstant "TOO MANY REQUESTS" SignupTooManyRequests



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


loginView : ViewConfig msg -> LoginModel -> Element Msg
loginView config state =
    Element.none


passwordResetView : ViewConfig msg -> PasswordResetModel -> Element Msg
passwordResetView config state =
    Element.none


codeVerificationView : ViewConfig msg -> CodeVerificationModel -> Element Msg
codeVerificationView config state =
    Element.none


userControlView : ViewConfig msg -> UserControlPanelModel -> Element Msg
userControlView config state =
    Element.none


adminControlView : ViewConfig msg -> AdminControlPanelModel -> Element Msg
adminControlView config state =
    Element.none


signupView : ViewConfig msg -> SignupModel -> Element Msg
signupView config state =
    let
        status =
            state.requestStatus

        initialView =
            column
                [ spacing 15 ]
                [ Input.text textInputStyle
                    { onChange =
                        SetUsername
                    , text =
                        state.username
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Username: "))
                    }
                , Input.newPassword textInputStyle
                    { onChange =
                        SetPassword
                    , text =
                        state.password
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Password: "))
                    , show = False
                    }
                , Input.newPassword textInputStyle
                    { onChange =
                        SetConfirmPassword
                    , text =
                        state.confirmPassword
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Confirm password: "))
                    , show = False
                    }
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
                            Just <| ToSignup initSignupModel
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



--# ***** View functions ***** #--
--# ************************** #--
-------------------------------------------------------------------------------
