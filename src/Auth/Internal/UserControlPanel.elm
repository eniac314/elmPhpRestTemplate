module Auth.Internal.UserControlPanel exposing (..)

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


type alias UserControlPanelModel =
    { newEmail : String
    , password : String
    , confirmPassword : String
    , userProfile : UserProfile
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
    }


initUserControlPanelModel : UserProfile -> UserControlPanelModel
initUserControlPanelModel userProfile =
    { newEmail = ""
    , password = ""
    , confirmPassword = ""
    , userProfile = userProfile
    , showValidationErrors = False
    , validationErrors = Dict.empty
    }


type alias Handlers =
    {}


userControlView : Handlers -> UserControlPanelModel -> Element msg
userControlView handlers model =
    Element.none
