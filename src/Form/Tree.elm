module Form.Tree exposing (Tree(..), getAtPath, getAtName, group, asList, asValue, setAtPath)

{-| Data structures

# Tree structure and builders
@docs Tree, group

# Readers
@docs getAtPath, getAtName,  asList, asValue

# Writers
@docs setAtPath
-}

import Dict exposing (Dict)


{-| Field values and errors are stored as trees.
-}
type Tree comparable value
    = Group (Dict comparable (Tree comparable value))
    | List (List (Tree comparable value))
    | Value value


{-| Get node at given path
-}
getAtPath : List comparable -> Tree comparable value -> Maybe (Tree comparable value)
getAtPath path tree =
    let
        walkPath fragment maybeField =
            maybeField |> Maybe.andThen (getAtName fragment)
    in
        List.foldl walkPath (Just tree) path


{-| Get node at name, if group
-}
getAtName : comparable -> Tree comparable value -> Maybe (Tree comparable value)
getAtName name value =
    case value of
        Group group ->
            Dict.get name group

        _ ->
            Nothing


{-| Extract value, if possible.
-}
asValue : Tree comparable value -> Maybe value
asValue node =
    case node of
        Value value ->
            Just value

        _ ->
            Nothing


{-| Get field as a list of fields
-}
asList : Tree comparable value -> List (Tree comparable value)
asList value =
    case value of
        List items ->
            items

        _ ->
            []


{-| Helper to create a group value.
-}
group : List ( comparable, Tree comparable value ) -> Tree comparable value
group items =
    items
        |> Dict.fromList
        |> Group


{-| Set node in tree at given path.
-}
setAtPath : List comparable -> Tree comparable value -> Tree comparable value -> Tree comparable value
setAtPath path node tree =
    case path of
        head :: rest ->
            let
                target =
                    getAtName head tree |> Maybe.withDefault (Group Dict.empty)

                childNode =
                    setAtPath rest node target
            in
                merge (Group (Dict.fromList [ ( head, childNode ) ])) tree

        [] ->
            node


updateListAtIndex : Int -> (a -> a) -> List a -> List a
updateListAtIndex index updater =
    List.indexedMap
        (\i f ->
            if i == index then
                updater f
            else
                f
        )


merge : Tree comparable value -> Tree comparable value -> Tree comparable value
merge t1 t2 =
    case ( t1, t2 ) of
        ( Group g1, Group g2 ) ->
            Group (Dict.union g1 g2)

        _ ->
            t1
