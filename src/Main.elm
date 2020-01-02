module Main exposing (..)

import Auth.AuthPlugin as Auth
import Browser exposing (..)
import Browser.Events exposing (Visibility(..), onResize, onVisibilityChange)
import Browser.Navigation as Nav
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Html as Html
import Html.Attributes as HtmlAttr
import Internal.Logger exposing (..)
import Json.Decode as Decode
import Random exposing (Seed, initialSeed)
import String.Extra exposing (unsurround)
import Style.Palette exposing (..)
import Time exposing (Zone, utc)
import Url as Url
import Url.Builder as UrlBuilder exposing (Root(..), absolute)
import Url.Parser as UrlParser exposing (..)



-- to be use if the app is not located in the web document folder
-- ex: ["myproject", "public"] -> /myproject/public
-- it should be [] otherwise


docRoot =
    [ "public" ]


subscriptions model =
    Sub.batch
        [ onResize WinResize
        , onVisibilityChange VisibilityChange
        , Auth.subscriptions model.authPlugin
        ]


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangeUrl
        }


type alias Model =
    { device : Device
    , width : Int
    , height : Int
    , visibility : Visibility
    , key : Nav.Key
    , url : Url.Url
    , currentPosition : { path : Path, anchor : Maybe String }
    , zone : Time.Zone
    , seed : Random.Seed
    , logs : Dict.Dict Int ( Log, Bool )
    , authPlugin : Auth.Model Msg
    }


type alias Flags =
    { width : Int
    , height : Int
    , currentTime : Int
    }


type Msg
    = ChangeUrl Url.Url
    | ClickedLink UrlRequest
    | WinResize Int Int
    | VisibilityChange Visibility
    | AddLog Log
    | AuthMsg Auth.Msg
    | NoOp


type alias Path =
    String


type alias Anchor =
    String


type alias Label =
    String


type alias ExternalUrl =
    String


type Link
    = Internal Path Label (Maybe Anchor)
    | External ExternalUrl Label


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( authPlugin, authCmd ) =
            Auth.init AuthMsg

        path =
            if unsurround "/" url.path == UrlBuilder.relative docRoot [] then
                UrlBuilder.absolute (docRoot ++ [ "home" ]) []

            else
                url.path
    in
    ( { device = classifyDevice { width = flags.width, height = flags.height }
      , width = flags.width
      , height = flags.height
      , visibility = Visible
      , key = key
      , url = url
      , currentPosition = { path = path, anchor = Nothing }
      , zone = Time.utc
      , seed = initialSeed flags.currentTime
      , logs = Dict.empty
      , authPlugin = authPlugin
      }
    , Cmd.batch
        [ authCmd ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ChangeUrl url ->
            case UrlParser.parse pathParser url of
                Just ( path, mbAnchor ) ->
                    ( { model
                        | currentPosition =
                            { path = path
                            , anchor = mbAnchor
                            }
                        , url = url
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        WinResize width height ->
            ( { model
                | width = width
                , height = height
                , device = classifyDevice { width = width, height = height }
              }
            , Cmd.none
            )

        VisibilityChange visibility ->
            ( { model | visibility = visibility }, Cmd.none )

        AddLog log ->
            let
                ( logHash, newSeed ) =
                    hashLog model.seed log

                newLogs =
                    safeInsert (\k -> k + 1) logHash ( log, False ) model.logs
            in
            ( { model
                | logs = newLogs
                , seed = newSeed
              }
            , Cmd.none
            )

        AuthMsg authPluginMsg ->
            let
                logInfo =
                    Auth.getLogInfo newAuthPlugin

                ( newAuthPlugin, authToolCmds, mbPluginResult ) =
                    Auth.update { addLog = AddLog } authPluginMsg model.authPlugin
            in
            ( { model
                | authPlugin = newAuthPlugin
              }
            , Cmd.batch <|
                [ authToolCmds
                ]
            )

        NoOp ->
            ( model, Cmd.none )


pathParser =
    let
        rootParser =
            if docRoot == [] then
                UrlParser.top

            else
                List.foldl (\seg acc -> acc </> UrlParser.s seg) UrlParser.top docRoot
    in
    UrlParser.oneOf
        [ UrlParser.map
            (\anchor -> ( UrlBuilder.absolute (docRoot ++ [ "home" ]) [], anchor ))
            (rootParser
                </> UrlParser.fragment identity
            )
        , UrlParser.map
            (\anchor -> ( UrlBuilder.absolute (docRoot ++ [ "auth" ]) [], anchor ))
            (rootParser
                </> UrlParser.s "auth"
                </> UrlParser.fragment identity
            )
        ]



-------------------------------------------------------------------------------
-- View functions --


view model =
    { title = "Basic template"
    , body =
        [ Element.layout
            [ width fill
            , height (px model.height)
            , Font.size 16
            ]
            (column
                []
                [ row [ spacing 15 ]
                    [ link
                        []
                        { url = UrlBuilder.absolute docRoot []
                        , label = el [] (text "home")
                        }
                    , link
                        []
                        { url = UrlBuilder.absolute (docRoot ++ [ "auth" ]) []
                        , label = el [] (text "auth")
                        }
                    ]
                , Dict.get
                    (String.dropLeft (String.length <| UrlBuilder.absolute docRoot []) model.currentPosition.path)
                    (content model)
                    |> Maybe.withDefault Element.none
                ]
            )
        ]
    }



-------------------------------------------------------------------------------
-- Content


content : Model -> Dict String (Element Msg)
content model =
    Dict.fromList
        [ ( "/home"
          , column
                []
                [ text "this is home" ]
          )
        , ( "/auth"
          , column
                [ spacing 15
                , padding 15
                , centerX
                , centerY
                , Background.color white
                ]
                [ Auth.view { zone = model.zone } model.authPlugin ]
          )
        ]


testurl =
    { fragment = Nothing
    , host = "localhost"
    , path = "/public/auth"
    , port_ = Nothing
    , protocol = Url.Http
    , query = Nothing
    }
