module Day03 exposing (..)

import Regex exposing (..)



-- https://adventofcode.com/2024/day/3
-- Mull it over
-- Part 1
-- Extract all the valid mul instructions and multiply the two numbers together


parse : String -> String
parse s =
    s |> String.lines |> String.concat


part1 : String -> Int
part1 s =
    let
        r =
            Maybe.withDefault Regex.never <| Regex.fromString "mul\\((\\d+),(\\d+)\\)"
    in
    s |> Regex.find r |> List.map (\x -> x.submatches |> List.map (Maybe.withDefault "0") |> List.map (\y -> String.toInt y |> Maybe.withDefault 0) |> List.foldl (*) 1) |> List.sum



-- Part 2
-- Enable and disable the mul instructions based on the do and don't instructions


type Instruction
    = Mult Int
    | Do
    | Dont


part2 : String -> Int
part2 s =
    let
        r =
            Maybe.withDefault Regex.never <| Regex.fromString "(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don\\'t\\(\\))"

        xtract x =
            case x of
                Mult y ->
                    y

                _ ->
                    0
    in
    s |> Regex.find r |> List.map mapMatch |> takeUntil |> List.map xtract |> List.sum



-- Helper function to convert a match to an instruction


mapMatch : Match -> Instruction
mapMatch m =
    case m.match of
        "do()" ->
            Do

        "don't()" ->
            Dont

        _ ->
            m.submatches |> List.tail |> Maybe.withDefault [] |> List.map (Maybe.withDefault "0") |> List.map (\y -> String.toInt y |> Maybe.withDefault 0) |> List.foldl (*) 1 |> Mult



-- Take a list of instructions until a don't instruction is found


takeUntil : List Instruction -> List Instruction
takeUntil xs =
    case xs of
        [] ->
            []

        x :: rest ->
            case x of
                Dont ->
                    dropUntil rest

                _ ->
                    x :: takeUntil rest



-- Drop a list of instructions until a do instruction is found


dropUntil : List Instruction -> List Instruction
dropUntil xs =
    case xs of
        [] ->
            []

        x :: rest ->
            case x of
                Do ->
                    takeUntil rest

                _ ->
                    dropUntil rest
