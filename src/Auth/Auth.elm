module Auth.Auth exposing (..)

import Dict exposing (..)
import Dict.Extra exposing (fromListDedupe)
import Http exposing (Error)
import Internal.Helpers exposing (PluginResult(..), Status(..), httpErrorToString)
import Json.Decode as Decode
import Json.Encode as Encode
import StateMachine exposing (Allowed, State(..))
import Validate exposing (..)


type Auth
    = LoginForm
        (State
            { requestValidLogin : Allowed
            }
            { username : String
            , password : String
            , showValidationErrors : Bool
            , validationErrors : ValidationErrors
            }
        )
    | RequestValidLogin
        (State
            { loginForm : Allowed
            }
            { username : String
            , password : String
            , loginRequestStatus : Status
            , logInfo : LogInfo
            }
        )
    | SignUpForm
        (State
            { loginForm : Allowed
            , requestValidSignUp : Allowed
            }
            { username : String
            , email : String
            , password : String
            , confirmPassword : String
            , showValidationErrors : Bool
            , validationErrors : ValidationErrors
            }
        )
    | RequestValidSignUp
        (State
            { loginForm : Allowed
            , emailConfirmationForm : Allowed
            }
            { username : String
            , email : String
            , password : String
            , signupRequestStatus : Status
            }
        )
    | EmailConfirmationForm
        (State
            { loginForm : Allowed
            , requestEmailConfirmation : Allowed
            }
            { email : String
            , confirmationCode : String
            }
        )
    | RequestEmailConfirmation
        (State
            { loginForm : Allowed
            , emailConfirmationForm : Allowed
            }
            { email : String
            , confirmationCode : String
            }
        )


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
    = LoggedIn
        { username : String

        --, role : Role
        }
    | LoggedOut


initialState =
    State
        { username = ""
        , password = ""
        , showValidationErrors = False
        , validationErrors = Dict.empty
        }
        |> LoginForm


setUserNameCommon : String -> Auth -> Auth
setUserNameCommon name auth =
    let
        setUserName : String -> State t { a | username : String } -> State t { a | username : String }
        setUserName s state =
            StateMachine.map (\m -> { m | username = s }) state
    in
    case auth of
        LoginForm state ->
            setUserName name state
                |> LoginForm

        SignUpForm state ->
            setUserName name state
                |> SignUpForm

        _ ->
            auth


validateThenRequestLogin :
    State { requestValidLogin : Allowed }
        { username : String
        , password : String
        , showValidationErrors : Bool
        , validationErrors : ValidationErrors
        }
    -> ( Auth, Cmd Msg )
validateThenRequestLogin (State data) =
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
                { username = data.username
                , password = data.password
                , loginRequestStatus = Initial
                , logInfo = LoggedOut
                }
                |> RequestValidLogin
            , login validData
            )

        Err errors ->
            ( State { data | showValidationErrors = True, validationErrors = errors }
                |> LoginForm
            , Cmd.none
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
        , expect = Http.expectJson LoginResult decodeLoginResult
        }


decodeLoginResult : Decode.Decoder LogInfo
decodeLoginResult =
    Decode.map (\a -> LoggedIn { username = a })
        (Decode.field "username" Decode.string)


type Msg
    = LoginResult (Result Http.Error LogInfo)
