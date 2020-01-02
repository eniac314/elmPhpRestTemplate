module Internal.Logger exposing (..)

import Dict exposing (Dict(..), insert)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Murmur3 exposing (hashString)
import Random exposing (Seed, int, step)
import Style.Helpers exposing (noAttr, sides)
import Task exposing (perform)
import Time exposing (..)



-------------------------------------------------------------------------------
------------
-- Logger --
------------


type alias Log =
    { message : String
    , mbDetails : Maybe String
    , isError : Bool
    , isImportant : Bool
    , timeStamp : Posix
    }


hashLog : Seed -> Log -> ( Int, Seed )
hashLog seed log =
    let
        ( hashSeed, newSeed ) =
            Random.step (Random.int 0 10000) seed

        hash =
            (log.message
                ++ (log.mbDetails
                        |> Maybe.withDefault ""
                   )
                ++ (if log.isError then
                        "isError"

                    else
                        "isNotError"
                   )
                ++ (if log.isImportant then
                        "isImportant"

                    else
                        "isNotImportant"
                   )
                ++ (posixToMillis log.timeStamp
                        |> String.fromInt
                   )
            )
                |> hashString hashSeed
    in
    ( hash, newSeed )


newLog : (Log -> msg) -> String -> Maybe String -> Bool -> Bool -> Cmd msg
newLog addLogMsg logMsg details isError isImportant =
    Task.perform addLogMsg <|
        (Time.now
            |> Task.andThen
                (\t ->
                    Task.succeed <|
                        Log logMsg
                            details
                            isError
                            isImportant
                            t
                )
        )


formatTime =
    String.fromInt
        >> String.padLeft 2 '0'


logTitleView l zone =
    row [ spacing 15 ]
        [ el [ Font.color (rgb 0.7 0.7 0.7) ]
            (text <|
                formatTime (Time.toHour zone l.timeStamp)
                    ++ ":"
                    ++ formatTime (Time.toMinute zone l.timeStamp)
            )
        , el
            [ if l.isError then
                Font.color (rgb 1 0 0)

              else
                noAttr
            ]
            (text l.message)
        ]


logsView : List Log -> Time.Zone -> Element msg
logsView logs zone =
    let
        logView ({ message, mbDetails, isError, timeStamp } as log) =
            column
                [ spacing 5
                , width (maximum 500 fill)
                ]
                [ logTitleView log zone
                , case mbDetails of
                    Nothing ->
                        Element.none

                    Just details ->
                        paragraph
                            [ paddingEach { sides | left = 20 }
                            , Font.size 12
                            ]
                            [ text details ]
                ]
    in
    column [ spacing 15 ]
        (List.map logView logs)


logsDictView : List ( Int, ( Log, Bool ) ) -> Time.Zone -> (Int -> msg) -> List (Element msg)
logsDictView logs zone toogleLog =
    let
        logView i ( k, ( { message, mbDetails, isError, timeStamp } as log, isOpen ) ) =
            column
                [ spacing 5
                , width fill
                , Events.onClick (toogleLog k)
                ]
                [ el
                    [ if modBy 2 i /= 0 then
                        Background.color (rgb 1 1 1)

                      else
                        Background.color (rgb 0.5 0.5 0.5)
                    , width fill
                    , paddingXY 10 7
                    ]
                    (logTitleView log zone)
                , case mbDetails of
                    Nothing ->
                        Element.none

                    Just details ->
                        if isOpen then
                            paragraph
                                [ paddingEach { sides | left = 20 }
                                , Font.size 12
                                ]
                                [ text details ]

                        else
                            Element.none
                ]
    in
    List.indexedMap logView logs


safeInsert :
    (comparable -> comparable)
    -> comparable
    -> b
    -> Dict comparable b
    -> Dict comparable b
safeInsert f k v d =
    if Dict.member k d then
        safeInsert f (f k) v d

    else
        Dict.insert k v d



-------------------------------------------------------------------------------
