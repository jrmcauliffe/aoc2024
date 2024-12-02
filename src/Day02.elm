module Day02 exposing (..)

-- https://adventofcode.com/2024/day/2
-- Historian Hysteria
-- Part 1
-- Create a list of differences between the elements of a list


differences : List Int -> List Int
differences xs =
    case xs of
        [] ->
            []

        _ :: [] ->
            []

        _ ->
            List.map2 (\a b -> b - a) xs (Maybe.withDefault [] (List.tail xs))



-- Check if a report is safe by running tests on differences (No change is captured by having to be either increasing or decreasing)


isSafe : List Int -> Bool
isSafe report =
    let
        diff =
            differences report

        isSmallDiff =
            List.all (\x -> Basics.abs x <= 3) diff

        isIncreasing =
            List.all (\x -> x > 0) diff

        isDecreasing =
            List.all (\x -> x < 0) diff
    in
    isSmallDiff && (isIncreasing || isDecreasing)



-- Test both the original report and with each version of the report with one element removed, if any of them are safe, the report is safe


isSafePD : List Int -> Bool
isSafePD report =
    report :: allButOne report |> List.any isSafe



-- Helper function to generate all versions of a list with one element removed


allButOne : List a -> List (List a)
allButOne xs =
    let
        removeNth : Int -> List a -> List a
        removeNth n l =
            List.take (n - 1) l ++ List.drop n l
    in
    List.range 1 (List.length xs) |> List.map (\i -> removeNth i xs)


part1 : List (List Int) -> Int
part1 reports =
    reports |> List.filter isSafe |> List.length


part2 : List (List Int) -> Int
part2 reports =
    reports |> List.filter isSafePD |> List.length
