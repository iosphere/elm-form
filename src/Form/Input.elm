module Form.Input exposing (baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput)

{-|
@docs Input

@docs baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput


-}

import Maybe exposing (andThen)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Json.Decode as Json
import Form exposing (Form, Msg, FieldState, Msg(Input, Focus, Blur), InputType(..))
import Form.Field as Field exposing (Field, FieldValue(..))


{-| Untyped input, first param is `type` attribute.
-}
baseInput : String -> (String -> FieldValue) -> InputType -> FieldState comparable e a -> List (Attribute Msg) -> Html Msg
baseInput t toFieldValue inputType state attrs =
    let
        formAttrs =
            [ type_ t
            , defaultValue (state.value |> Maybe.withDefault "")
            , onInput (toFieldValue >> (Input state.path inputType))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
        input (formAttrs ++ attrs) []


{-| Text input.
-}
textInput : FieldState comparable e a -> List (Attribute Msg) -> Html Msg
textInput =
    baseInput "text" String Text


{-| Password input.
-}
passwordInput : FieldState comparable e a -> List (Attribute Msg) -> Html Msg
passwordInput =
    baseInput "password" String Text


{-| Textarea.
-}
textArea : FieldState comparable e a -> List (Attribute Msg) -> Html Msg
textArea state attrs =
    let
        formAttrs =
            [ defaultValue (state.value |> Maybe.withDefault "")
            , onInput (String >> (Input state.path Textarea))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
        Html.textarea (formAttrs ++ attrs) []


{-| Select input.
-}
selectInput : List ( String, String ) -> FieldState comparable e a -> List (Attribute Msg) -> Html Msg
selectInput options state attrs =
    let
        formAttrs =
            [ on
                "change"
                (targetValue |> Json.map (String >> (Input state.path Select)))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]

        buildOption ( k, v ) =
            option [ value k, selected (state.value == Just k) ] [ text v ]
    in
        select (formAttrs ++ attrs) (List.map buildOption options)


{-| Checkbox input.
-}
checkboxInput : FieldState comparable e a -> List (Attribute Msg) -> Html Msg
checkboxInput state attrs =
    let
        formAttrs =
            [ type_ "checkbox"
            , checked (state.value |> Maybe.withDefault False)
            , onCheck (Bool >> (Input state.path Checkbox))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
        input (formAttrs ++ attrs) []


{-| Radio input.
-}
radioInput : String -> FieldState comparable e a -> List (Attribute Msg) -> Html Msg
radioInput value state attrs =
    let
        formAttrs =
            [ type_ "radio"
            , name state.path
            , HtmlAttr.value value
            , checked (state.value == Just value)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , on
                "change"
                (targetValue |> Json.map (String >> (Input state.path Radio)))
            ]
    in
        input (formAttrs ++ attrs) []
