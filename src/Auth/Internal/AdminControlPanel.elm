module Auth.Internal.AdminControlPanel exposing (..)

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


type alias AdminControlPanelModel =
    { newEmail : String
    , password : String
    , confirmPassword : String
    , userProfile : UserProfile
    , showValidationErrors : Bool
    , validationErrors : ValidationErrors
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


type alias Handlers =
    {}


adminControlView : Handlers -> AdminControlPanelModel -> Element msg
adminControlView handlers model =
    Element.none
