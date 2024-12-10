module Day09 exposing (..)

-- https://adventofcode.com/2024/day/9
-- Disk Fragmenter
-- Part 1

import List.Extra exposing (dropWhile, takeWhile)


type alias Map =
    List Int


type alias Group =
    ( Maybe Int, Int )


type alias Layout =
    List (Maybe Int)


parse : String -> Map
parse s =
    s |> String.trim |> String.toList |> List.map (\c -> c |> String.fromChar |> String.toInt |> Maybe.withDefault 0)


buildLayout : Map -> Layout
buildLayout m =
    let
        idthing i =
            case modBy 2 i of
                0 ->
                    Just (i // 2)

                _ ->
                    Nothing
    in
    m |> List.indexedMap (\i x -> ( idthing i, x )) |> List.map (\( a, b ) -> List.repeat b a) |> List.concat


showLayout : Layout -> String
showLayout l =
    let
        f : Maybe Int -> String
        f x =
            case x of
                Just c ->
                    String.fromInt c

                Nothing ->
                    "."
    in
    l |> List.map f |> String.join ""


hasValue maybeValue =
    case maybeValue of
        Just _ ->
            True

        Nothing ->
            False


defragment : Layout -> Layout
defragment l =
    let
        isDefragmented : Layout -> Bool
        isDefragmented ll =
            (ll |> takeWhile hasValue |> List.length) == (ll |> List.filter hasValue |> List.length)
    in
    case isDefragmented l of
        True ->
            l

        False ->
            let
                nextVal =
                    l |> List.reverse |> dropWhile (\x -> x == Nothing) |> List.head |> Maybe.andThen identity

                emptyLen =
                    l |> List.reverse |> takeWhile (\x -> x == Nothing) |> List.length |> (+) 1

                pre =
                    l |> takeWhile hasValue

                post =
                    l |> dropWhile hasValue |> List.drop 1 |> List.reverse |> List.drop emptyLen |> List.append (List.repeat emptyLen Nothing) |> List.reverse
            in
            defragment (pre ++ [ nextVal ] ++ post)


checkSum : Layout -> Int
checkSum l =
    let
        cs : Int -> Maybe Int -> Int
        cs i x =
            case x of
                Just c ->
                    i * c

                Nothing ->
                    0
    in
    l |> List.indexedMap (\i x -> cs i x) |> List.sum


part1 : Map -> Int
part1 m =
    m |> buildLayout |> defragment |> checkSum


type alias Layout2 =
    List Group


buildLayout2 : Map -> Layout2
buildLayout2 m =
    let
        idthing i =
            case modBy 2 i of
                0 ->
                    Just (i // 2)

                _ ->
                    Nothing
    in
    m |> List.indexedMap (\i x -> ( idthing i, x ))


showLayout2 : Layout2 -> String
showLayout2 l =
    let
        f : Group -> String
        f x =
            case x of
                ( Just c, i ) ->
                    String.fromInt c |> List.repeat i |> String.join ""

                ( Nothing, i ) ->
                    List.repeat i "." |> String.join ""
    in
    l |> List.map f |> String.join ""


insert : Group -> Layout2 -> Maybe Layout2
insert ( c, i ) l =
    let
        splitz : Layout2 -> Maybe ( Layout2, Layout2 )
        splitz ll =
            ll |> List.Extra.splitWhen (\( nn, ii ) -> nn == Nothing && ii >= i)

        pad : Group -> Group -> List Group
        pad ( c1, i1 ) ( _, i2 ) =
            if i1 == i2 then
                [ ( c1, i1 ) ]

            else
                [ ( c1, i1 ), ( Nothing, i2 - i1 ) ]
    in
    case splitz l of
        Just ( pre, firstEmpty :: post ) ->
            Just (pre ++ pad ( c, i ) firstEmpty ++ post)

        _ ->
            Nothing


moveN : Int -> Layout2 -> Layout2
moveN n l =
    let
        ( pre, post ) =
            l |> List.Extra.splitWhen (\( nn, _ ) -> nn == Just n) |> Maybe.withDefault ( [], [] )

        ( ourGuy, rest ) =
            case post of
                [] ->
                    ( ( Nothing, -1 ), [] )

                x :: xs ->
                    ( x, xs )
    in
    case n of
        0 ->
            l

        _ ->
            case insert ourGuy pre of
                Just ll ->
                    moveN (n - 1) (ll ++ [ ( Nothing, Tuple.second ourGuy ) ] ++ rest)

                Nothing ->
                    moveN (n - 1) l


maxId : Layout2 -> Int
maxId l =
    l |> List.map (\( x, _ ) -> x) |> List.filterMap identity |> List.maximum |> Maybe.withDefault 0


toLayout1 : Layout2 -> Layout
toLayout1 l =
    l |> List.map (\( x, i ) -> List.repeat i x) |> List.concat


part2 : Map -> Int
part2 m =
    let
        layout =
            buildLayout2 m

        id =
            maxId layout
    in
    layout |> moveN id |> toLayout1 |> checkSum
