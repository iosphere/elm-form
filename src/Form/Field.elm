module Form.Field exposing (Field, FieldValue(..), value, string, bool, group, list, asString, asBool)

{-| Read and write field values.

# Constructors
@docs Field, FieldValue, value, string, bool, group, list


# Value readers
@docs  asString, asBool
-}

import Form.Tree as Tree exposing (..)


{-| A field is a tree node.
-}
type alias Field key =
    Tree key FieldValue


{-| Form field. Can either be a group of named fields, or a final field.
-}
type FieldValue
    = String String
    | Bool Bool
    | EmptyField


{-| Build a field from its value.
-}
value : FieldValue -> Field comparable
value =
    Tree.Value


{-| Build a string field, for text inputs, selects, etc.
-}
string : String -> Field comparable
string =
    String >> Tree.Value


{-| Build a boolean field, for checkboxes.
-}
bool : Bool -> Field comparable
bool =
    Bool >> Tree.Value


{-| Gather named fields as a group field.
-}
group : List ( comparable, Field comparable ) -> Field comparable
group =
    Tree.group


{-| Gather fields as a list field.
-}
list : List (Field comparable) -> Field comparable
list =
    Tree.List


{-| Get field value as boolean.
-}
asBool : Field comparable -> Maybe Bool
asBool field =
    case field of
        Tree.Value (Bool b) ->
            Just b

        _ ->
            Nothing


{-| Get field value as string.
-}
asString : Field comparable -> Maybe String
asString field =
    case field of
        Tree.Value (String s) ->
            Just s

        _ ->
            Nothing
