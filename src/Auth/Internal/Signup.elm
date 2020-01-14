module Auth.Internal.Signup exposing (..)

import Auth.Common exposing (..)
import Dict exposing (..)
import Internal.Helpers exposing (PluginResult(..), Status(..), httpErrorToString)


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
