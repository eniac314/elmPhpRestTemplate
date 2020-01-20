module Auth.Internal.Logout exposing (..)

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


type alias LogoutModel =
    { requestStatus : Status
    , autoLogout : Bool
    }


initLogoutModel : LogoutModel
initLogoutModel =
    { requestStatus = Initial
    , autoLogout = False
    }


logout : (Result Http.Error LogoutResult -> msg) -> Cmd msg
logout handler =
    Http.get
        { url = "/api/logout"
        , expect = Http.expectJson handler decodeLogoutMessage
        }


type LogoutResult
    = LogoutSuccess
    | LogoutNotLoggedIn


decodeLogoutMessage =
    Decode.oneOf
        [ Decode.field "message" decodeLogoutSuccess
        , Decode.field "serverError" decodeNotLoggedIn
        ]


decodeLogoutSuccess =
    decodeConstant "LOGOUT SUCCESS" LogoutSuccess


decodeNotLoggedIn =
    decodeConstant "NOT LOGGED IN" LogoutNotLoggedIn


type alias Handlers msg =
    { logoutRequest : msg
    , toLogin : msg
    , toLogout : msg
    }


logoutView : Handlers msg -> LogoutModel -> Element msg
logoutView handlers model =
    let
        status =
            model.requestStatus

        initialView =
            column
                [ spacing 15 ]
                [ Input.button (buttonStyle True)
                    { onPress = Just handlers.logoutRequest
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
                        { onPress = Just <| handlers.toLogin
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
                            Just <| handlers.toLogout
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
