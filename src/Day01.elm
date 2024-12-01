module Day01 exposing (..)

-- https://adventofcode.com/2024/day/1
-- Historian Hysteria
-- Part 1 Compare sorted list absolute differences


part1 : List ( Int, Int ) -> Int
part1 xs =
    let
        ( a, b ) =
            List.unzip xs
    in
    List.map2 (\aa bb -> Basics.abs (aa - bb)) (List.sort a) (List.sort b) |> List.sum



-- Part 2 Similarity Score (count of a in b) * a


part2 : List ( Int, Int ) -> Int
part2 xs =
    let
        ( a, b ) =
            List.unzip xs
    in
    a |> List.map (\aa -> (b |> List.filter (\bb -> aa == bb) |> List.length) * aa) |> List.sum
