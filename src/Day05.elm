module Day05 exposing (..)

-- https://adventofcode.com/2024/day/5
--
-- Part 1
--

import Dict exposing (Dict)


type alias Page =
    Int


type alias Update =
    List Page


type alias Ordering =
    ( Int, Int )



-- Parse Pages Input


parsePages : String -> List Update
parsePages s =
    s |> String.lines |> List.map (\x -> x |> String.trim |> String.split "," |> List.map String.toInt |> List.map (Maybe.withDefault 0))



-- Parse Orderings Input


parseOrdering : String -> List Ordering
parseOrdering s =
    let
        listToTuple : List (Maybe Int) -> Ordering
        listToTuple l =
            case l of
                (Just x) :: (Just y) :: [] ->
                    ( x, y )

                _ ->
                    ( 0, 0 )
    in
    s |> String.lines |> List.map (\x -> x |> String.trim |> String.split "|" |> List.map String.toInt) |> List.map listToTuple



-- Is the Update conforming to the Ordering?


correctOrdering : List Ordering -> Update -> Bool
correctOrdering o u =
    let
        -- Does the given update obey the given ordering?
        obeys : Update -> Ordering -> Bool
        obeys uu oo =
            let
                updateRank =
                    uu |> List.indexedMap Tuple.pair |> List.map (\( x, y ) -> ( y, x )) |> Dict.fromList
            in
            case ( Dict.get (Tuple.first oo) updateRank, Dict.get (Tuple.second oo) updateRank ) of
                ( Just x, Just y ) ->
                    x < y

                _ ->
                    True
    in
    o |> List.map (obeys u) |> List.all identity



-- Given an update, return the midpoint


midpoint : Update -> Int
midpoint s =
    s |> List.drop (List.length s // 2) |> List.head |> Maybe.withDefault 0



-- Find the correct ordered Updates, find the midpoint, and sum them


part1 : List Update -> List Ordering -> Int
part1 u o =
    u |> List.filter (correctOrdering o) |> List.map midpoint |> List.sum



-- Sort and update by setting the comparison to correctOrdering


reorder : List Ordering -> Update -> Update
reorder o u =
    let
        mySort : Page -> Page -> Order
        mySort x y =
            if correctOrdering o [ x, y ] then
                LT

            else
                GT
    in
    u |> List.sortWith mySort



-- Pick the incorrect ordered Updates, correct the ordering, and sum the midpoints


part2 : List Update -> List Ordering -> Int
part2 u o =
    u |> List.filter (\x -> not (correctOrdering o x)) |> List.map (reorder o) |> List.map midpoint |> List.sum
