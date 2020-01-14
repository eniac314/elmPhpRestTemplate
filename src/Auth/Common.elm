module Auth.Common exposing (..)

import Dict exposing (..)


type alias FieldId =
    String


type alias ValidationErrors =
    Dict FieldId (List String)
