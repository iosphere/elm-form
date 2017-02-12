module Form.Validate exposing (Validation, field, map, andThen, andMap, customError, defaultValue, mapError, withCustomError, map2, map3, map4, map5, map6, map7, map8, list, string, int, float, bool, date, maybe, email, url, emptyString, minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, format, includedIn, fail, succeed, customValidation, oneOf, sequence)

{-| Form validation.

# Combinators
@docs Validation, field, map, succeed, andThen, andMap, customError, defaultValue, mapError, withCustomError, sequence

# Fixed-size forms
@docs map2, map3, map4, map5, map6, map7, map8

# Type extractors
@docs list, string, int, float, bool, date, maybe, email, url, emptyString

# Common filters
@docs minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, format, includedIn

# Custom validations
@docs fail, succeed, customValidation, oneOf
-}

import Result
import Date exposing (Date)
import Dict exposing (Dict)
import String
import Regex exposing (Regex)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Tree as Tree


{-| A validation is a function that takes a form field and returns a result
being either a Validation comparable error or the expected object.
-}
type alias Validation comparable customError output =
    Field comparable -> Result (Error comparable customError) output


{-| Map over the result of the validation.

    field "myfield" (string |> map String.trim)
-}
map : (a -> b) -> Validation comparable e a -> Validation comparable e b
map f validation field =
    Result.map f (validation field)


{-| Apply a new validation to the result of the validation.

    field "myfield" (int |> andThen (minInt 10))
-}
andThen : (a -> Validation comparable e b) -> Validation comparable e a -> Validation comparable e b
andThen callback validation field =
    validation field |> Result.andThen (\next -> (callback next) field)


{-| Incremental form validation for records with more that 8 fields.

    Form.succeed SomeRecord
      |> andMap (field "foo" string)
      |> andMap (field "bar" string)
-}
andMap : Validation comparable e a -> Validation comparable e (a -> b) -> Validation comparable e b
andMap aValidation partialValidation field =
    case ( partialValidation field, aValidation field ) of
        ( Ok partial, Ok a ) ->
            Ok (partial a)

        ( partialResult, aResult ) ->
            Err (mergeMany [ errMaybe partialResult, errMaybe aResult ])


{-| Rescue a failed validation with the supplied value.
-}
defaultValue : a -> Validation comparable e a -> Validation comparable e a
defaultValue a validation field =
    Ok (Result.withDefault a (validation field))


{-| Call Result.mapError on validation result.
-}
mapError : (Error comparable e1 -> Error comparable e2) -> Validation comparable e1 a -> Validation comparable e2 a
mapError f validation =
    \field -> Result.mapError f (validation field)


{-| Arrange that if a validation fails, it has the given custom error.

    field "customerId" (V.int
          |> andThen (minInt 1)
          |> andThen (maxInt 9999)
          |> withCustomError InvalidIdentity)
-}
withCustomError : customErr -> Validation comparable e a -> Validation comparable customErr a
withCustomError =
    mapError << always << customError


{-| Helper to create a CustomError.
-}
customError : e -> Error comparable e
customError =
    CustomError >> Error.value


{-| Access the given field in the group.

    field "name" string
-}
field : comparable -> Validation comparable e a -> Validation comparable e a
field key validation field =
    Tree.getAtName key field
        |> Maybe.withDefault (Tree.Value EmptyField)
        |> validation
        |> Result.mapError
            (\e -> Tree.group [ ( key, e ) ])


{-| Validation a form with two fields.
-}
map2 : (a -> b -> m) -> Validation comparable e a -> Validation comparable e b -> Validation comparable e m
map2 func v1 v2 =
    map func v1
        |> andMap v2



-- apply (form1 func v1) v2


{-| Validation a form with three fields.
-}
map3 : (a -> b -> c -> m) -> Validation comparable e a -> Validation comparable e b -> Validation comparable e c -> Validation comparable e m
map3 func v1 v2 v3 =
    map2 func v1 v2
        |> andMap v3


{-| Validation a form with four fields.
-}
map4 : (a -> b -> c -> d -> m) -> Validation comparable e a -> Validation comparable e b -> Validation comparable e c -> Validation comparable e d -> Validation comparable e m
map4 func v1 v2 v3 v4 =
    map3 func v1 v2 v3
        |> andMap v4


{-| Validation a form with five fields.
-}
map5 : (a -> b -> c -> d -> e -> m) -> Validation comparable err a -> Validation comparable err b -> Validation comparable err c -> Validation comparable err d -> Validation comparable err e -> Validation comparable err m
map5 func v1 v2 v3 v4 v5 =
    map4 func v1 v2 v3 v4
        |> andMap v5


{-| Validation a form with six fields.
-}
map6 : (a -> b -> c -> d -> e -> f -> m) -> Validation comparable err a -> Validation comparable err b -> Validation comparable err c -> Validation comparable err d -> Validation comparable err e -> Validation comparable err f -> Validation comparable err m
map6 func v1 v2 v3 v4 v5 v6 =
    map5 func v1 v2 v3 v4 v5
        |> andMap v6


{-| Validation a form with seven fields.
-}
map7 : (a -> b -> c -> d -> e -> f -> g -> m) -> Validation comparable err a -> Validation comparable err b -> Validation comparable err c -> Validation comparable err d -> Validation comparable err e -> Validation comparable err f -> Validation comparable err g -> Validation comparable err m
map7 func v1 v2 v3 v4 v5 v6 v7 =
    map6 func v1 v2 v3 v4 v5 v6
        |> andMap v7


{-| Validation a form with eight fields.
-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> m) -> Validation comparable err a -> Validation comparable err b -> Validation comparable err c -> Validation comparable err d -> Validation comparable err e -> Validation comparable err f -> Validation comparable err g -> Validation comparable err h -> Validation comparable err m
map8 func v1 v2 v3 v4 v5 v6 v7 v8 =
    map7 func v1 v2 v3 v4 v5 v6 v7
        |> andMap v8


{-| Private
-}
mergeMany : List (Maybe (Error comparable e)) -> Error comparable e
mergeMany errors =
    errors
        |> List.filterMap identity
        |> List.foldl groupErrorsUnion (Tree.group [])


{-| Private
-}
groupErrorsUnion : Error comparable e -> Error comparable e -> Error comparable e
groupErrorsUnion e1 e2 =
    case ( e1, e2 ) of
        ( Tree.Group g1, Tree.Group g2 ) ->
            Tree.Group (Dict.union g1 g2)

        _ ->
            e2


{-| Private
-}
errMaybe : Result e a -> Maybe e
errMaybe res =
    case res of
        Ok _ ->
            Nothing

        Err e ->
            Just e


{-| Validation an integer using `String.toInt`.
-}
int : Validation comparable e Int
int v =
    case Field.asString v of
        Just s ->
            String.toInt s
                |> Result.mapError (\_ -> Error.value InvalidInt)

        Nothing ->
            Err (Error.value InvalidInt)


{-| Validation a float using `String.toFloat`.
-}
float : Validation comparable e Float
float v =
    case Field.asString v of
        Just s ->
            String.toFloat s
                |> Result.mapError (\_ -> Error.value InvalidFloat)

        Nothing ->
            Err (Error.value InvalidFloat)


{-| Validation a String.
-}
string : Validation comparable e String
string v =
    case Field.asString v of
        Just s ->
            if String.isEmpty s then
                Err (Error.value Empty)
            else
                Ok s

        Nothing ->
            Err (Error.value InvalidString)


{-| Validate an empty string, otherwise failing with InvalidString.
Useful with `oneOf` for optional fields with format validation.
-}
emptyString : Validation comparable e String
emptyString v =
    case Field.asString v of
        Just s ->
            if String.isEmpty s then
                Ok s
            else
                Err (Error.value InvalidString)

        Nothing ->
            Ok ""


{-| Validation a Bool.
-}
bool : Validation comparable e Bool
bool v =
    case Field.asBool v of
        Just b ->
            Ok b

        Nothing ->
            Ok False


{-| Validation a Date using `Date.fromString`.
-}
date : Validation comparable e Date
date v =
    case Field.asString v of
        Just s ->
            Date.fromString s
                |> Result.mapError (\_ -> Error.value InvalidDate)

        Nothing ->
            Err (Error.value InvalidDate)


{-| Transform validation result to `Maybe`, using `Result.toMaybe`.
-}
maybe : Validation comparable e a -> Validation comparable e (Maybe a)
maybe validation field =
    Ok (Result.toMaybe (validation field))


{-| Fails if `String.isEmpty`.
-}
nonEmpty : String -> Validation comparable e String
nonEmpty s field =
    if String.isEmpty s then
        Err (Error.value Empty)
    else
        Ok s


{-| Min length for String.
-}
minLength : Int -> String -> Validation comparable e String
minLength min s field =
    if String.length s >= min then
        Ok s
    else
        Err (Error.value (ShorterStringThan min))


{-| Max length for String.
-}
maxLength : Int -> String -> Validation comparable e String
maxLength max s field =
    if String.length s <= max then
        Ok s
    else
        Err (Error.value (LongerStringThan max))


{-| Min value for Int.
-}
minInt : Int -> Int -> Validation comparable e Int
minInt min i field =
    if i >= min then
        Ok i
    else
        Err (Error.value (SmallerIntThan min))


{-| Max value for Int.
-}
maxInt : Int -> Int -> Validation comparable e Int
maxInt max i field =
    if i <= max then
        Ok i
    else
        Err (Error.value (GreaterIntThan max))


{-| Min value for Float.
-}
minFloat : Float -> Float -> Validation comparable e Float
minFloat min i field =
    if i >= min then
        Ok i
    else
        Err (Error.value (SmallerFloatThan min))


{-| Max value for Float.
-}
maxFloat : Float -> Float -> Validation comparable e Float
maxFloat max i field =
    if i <= max then
        Ok i
    else
        Err (Error.value (GreaterFloatThan max))


{-| Validates format of the string.
-}
format : Regex -> String -> Validation comparable e String
format regex s field =
    if Regex.contains regex s then
        Ok s
    else
        Err (Error.value InvalidFormat)


{-| Stolen to elm-validate.
-}
validEmailPattern : Regex
validEmailPattern =
    Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.caseInsensitive


{-| Check if the string is a valid email address.
-}
email : Validation comparable e String
email =
    string
        |> andThen
            (\s ->
                format validEmailPattern s
                    |> mapError (\_ -> Error.value InvalidEmail)
            )


validUrlPattern : Regex
validUrlPattern =
    Regex.regex "^(https?://)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\w \\.-]*)*/?$"
        |> Regex.caseInsensitive


{-| Check if the string is a valid URL.
-}
url : Validation comparable e String
url =
    string
        |> andThen
            (\s ->
                format validUrlPattern s
                    |> mapError (\_ -> Error.value InvalidUrl)
            )


{-| Check if the string is included in the given list.
-}
includedIn : List String -> String -> Validation comparable e String
includedIn items s field =
    if List.member s items then
        Ok s
    else
        Err (Error.value NotIncludedIn)


{-| A validation that always fails. Useful for contextual validation.
-}
fail : Error comparable e -> Validation comparable e a
fail error field =
    Err error


{-| A validation that always succeeds. Useful for contextual validation.
-}
succeed : a -> Validation comparable e a
succeed a field =
    Ok a


{-| Custom validation for your special cases.
-}
customValidation : Validation comparable e a -> (a -> Result (Error comparable e) b) -> Validation comparable e b
customValidation validation callback field =
    validation field |> Result.andThen callback


{-| First successful validation wins, from left to right.
-}
oneOf : List (Validation comparable e a) -> Validation comparable e a
oneOf validations field =
    let
        results =
            List.map (\v -> v field) validations

        walkResults result combined =
            case ( combined, result ) of
                ( Ok _, _ ) ->
                    combined

                _ ->
                    result
    in
        List.foldl walkResults (Err (Error.value Empty)) results


{-| Combine a list of validations into a validation producing a list of all
results.
-}
sequence : List (Validation comparable e a) -> Validation comparable e (List a)
sequence validations =
    List.foldr (map2 (::)) (succeed []) validations


{-| Validate a list of fields.
-}
list : Validation comparable e a -> Validation comparable e (List a)
list validation field =
    case field of
        Tree.List items ->
            let
                results =
                    List.map validation items

                errors =
                    List.filterMap errMaybe results
            in
                if List.isEmpty errors then
                    Ok (List.filterMap Result.toMaybe results)
                else
                    Err (Tree.List errors)

        _ ->
            Ok []
