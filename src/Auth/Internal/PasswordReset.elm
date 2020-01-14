module Auth.Internal.PasswordReset exposing (..)

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


type alias Handlers =
    {}


passwordResetView : Handlers -> PasswordResetModel -> Element msg
passwordResetView handlers model =
    Element.none
