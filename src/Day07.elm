module Day07 exposing (..)

-- https://adventofcode.com/2024/day/7
-- Bridge Repair
-- Part 1
-- Find all the possible solutions


type alias Equation =
    ( Int, List Int )


type alias Operator =
    Int -> Int -> Int


parse : String -> List Equation
parse s =
    s |> String.lines |> List.map parseLine


parseLine : String -> Equation
parseLine s =
    let
        v =
            s |> String.trim |> String.split ":"
    in
    case v of
        tv :: ints :: [] ->
            ( tv |> String.toInt |> Maybe.withDefault 0, ints |> String.split " " |> List.filterMap String.toInt )

        _ ->
            ( 0, [] )



-- Given a list of operators and a list of integers, return a list of all possible results


eval : List Operator -> List Int -> List Int
eval operators xs =
    case xs of
        x :: y :: rest ->
            operators |> List.map (\f -> f x y) |> List.concatMap (\v -> [ v :: rest |> eval operators ]) |> List.concat

        _ ->
            xs



-- Parse and evaluate the equations, summing the total possible correct values


part1 : List Equation -> Int
part1 l =
    let
        operators =
            [ (+), (*) ]
    in
    l |> List.map (\( x, v ) -> ( x, eval operators v )) |> List.filter (\( x, v ) -> v |> List.member x) |> List.map Tuple.first |> List.sum



-- Part 2
-- A new operator
-- Implement a new operator


concat : Int -> Int -> Int
concat x y =
    case String.concat [ String.fromInt x, String.fromInt y ] |> String.toInt of
        Just v ->
            v

        Nothing ->
            0



-- Parse and evaluate the equations, summing the total possible correct values with the new operator included


part2 : List Equation -> Int
part2 l =
    let
        operators =
            [ (+), (*), concat ]
    in
    l |> List.map (\( x, v ) -> ( x, eval operators v )) |> List.filter (\( x, v ) -> v |> List.member x) |> List.map Tuple.first |> List.sum
