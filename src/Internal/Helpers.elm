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
            "Url invalide: " ++ s

        Timeout ->
            "Délai d'attente dépassé"

        NetworkError ->
            "Erreur de réseau"

        BadStatus statusCode ->
            "Erreur serveur: "
                ++ String.fromInt statusCode

        BadBody details ->
            "Erreur décodage: " ++ details
