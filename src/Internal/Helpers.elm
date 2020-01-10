module Internal.Helpers exposing (..)

import Http exposing (Error(..))


type Status
    = Initial
    | Waiting
    | Success
    | Failure


type PluginResult a
    = PluginQuit
    | PluginData a


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        BadUrl s ->
            "Bad Url: " ++ s

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus statusCode ->
            "Bad status: "
                ++ String.fromInt statusCode

        BadBody details ->
            "Unexpected Json: " ++ details
