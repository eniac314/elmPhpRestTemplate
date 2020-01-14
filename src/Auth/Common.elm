module Auth.Common exposing (..)

import Dict exposing (..)
import Dict.Extra exposing (fromListDedupe)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Json.Decode as Decode
import Style.Helpers exposing (buttonStyle, textInputStyle)
import Validate exposing (..)


type alias FieldId =
    String


type alias ValidationErrors =
    Dict FieldId (List String)


decodeConstant c v =
    Decode.string
        |> Decode.andThen
            (\s ->
                if s == c then
                    Decode.succeed v

                else
                    Decode.fail "wrong constant"
            )


compileErrors : List ( FieldId, String ) -> ValidationErrors
compileErrors xs =
    List.map (\( k, a ) -> ( k, [ a ] )) xs
        |> Dict.Extra.fromListDedupe (++)


validateErrorDict validator model =
    validate validator model
        |> Result.mapError compileErrors


validateModelIfShowError validator model =
    case ( model.showValidationErrors, validator model ) of
        ( True, Err validationErrors ) ->
            { model | validationErrors = validationErrors }

        ( True, Ok _ ) ->
            { model | validationErrors = Dict.empty }

        _ ->
            model


validateUsername =
    ifBlank .username ( "username", "Please enter a username." )


validatePassword =
    ifBlank .password ( "password", "Please enter a password" )


validateConfirmPasword =
    ifBlank .confirmPassword ( "confirmPassword", "Please confirm your password" )


validateEmail =
    Validate.all
        [ ifBlank .email ( "email", "Please enter an email" )
        , ifInvalidEmail .email (\e -> ( "email", "Invalid email: " ++ e ))
        ]


type alias CustomInputConfig msg =
    { label : String
    , value : String
    , tag : String
    , handler : String -> msg
    }


customInput :
    CustomInputConfig msg
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element msg
customInput config model =
    column
        [ spacing 15 ]
        [ Input.text textInputStyle
            { onChange =
                config.handler
            , text =
                config.value
            , placeholder = Nothing
            , label =
                Input.labelAbove [ centerY ]
                    (el [ width (px 110) ] (text <| config.label))
            }
        , errorView config model
        ]


customCurrentPasswordInput :
    CustomInputConfig msg
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element msg
customCurrentPasswordInput config model =
    column
        [ spacing 15 ]
        [ Input.currentPassword textInputStyle
            { onChange =
                config.handler
            , text =
                config.value
            , placeholder = Nothing
            , label =
                Input.labelAbove [ centerY ]
                    (el [ width (px 110) ] (text <| config.label))
            , show = False
            }
        , errorView config model
        ]


customEmailInput :
    CustomInputConfig msg
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element msg
customEmailInput config model =
    column
        [ spacing 15 ]
        [ Input.email textInputStyle
            { onChange =
                config.handler
            , text =
                config.value
            , placeholder = Nothing
            , label =
                Input.labelAbove [ centerY ]
                    (el [ width (px 110) ] (text <| config.label))
            }
        , errorView config model
        ]


errorView :
    CustomInputConfig msg
    -> { a | validationErrors : ValidationErrors, showValidationErrors : Bool }
    -> Element msg
errorView config model =
    case
        ( model.showValidationErrors
        , Dict.get config.tag model.validationErrors
        )
    of
        ( True, Just errors ) ->
            column
                [ Font.color (Element.rgb 1 0 0)
                , Font.size 12
                , spacing 10
                ]
                (List.map text errors)

        _ ->
            Element.none


type Role
    = Admin
    | User


type alias UserProfile =
    { username : String
    , email : String
    , role : Role
    }
