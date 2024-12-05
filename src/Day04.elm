module Day04 exposing (..)

import Dict exposing (Dict)



-- https://adventofcode.com/2024/day/4
-- Ceres Search
-- Part 1
-- Word Search


type alias Position =
    ( Int, Int )


type alias Direction =
    ( Int, Int )


type alias Value =
    Char


type alias Point =
    { pos : Position
    , val : Value
    }



-- Parse the input


parse : String -> List Point
parse s =
    s |> String.lines |> List.indexedMap (\y v -> parseLine y v) |> List.concat


parseLine : Int -> String -> List Point
parseLine y s =
    s |> String.trim |> String.toList |> List.indexedMap (\x v -> { pos = ( x, y ), val = v })



-- Move a position in a direction


move : Position -> Direction -> Position
move ( x, y ) ( dx, dy ) =
    ( x + dx, y + dy )



-- Expand a position 4 places in a direction


expand : Position -> Direction -> List Position
expand pos dir =
    pos :: move pos dir :: move (move pos dir) dir :: move (move (move pos dir) dir) dir :: []



-- Check if a list of positions is the word "XMAS"


isWord : Dict Position Value -> List Position -> Bool
isWord d l =
    (l |> List.map (\y -> d |> Dict.get y)) == [ Just 'X', Just 'M', Just 'A', Just 'S' ]



-- Give a position, count the number of valid words in any direction


wordCount : Position -> Dict Position Value -> Int
wordCount pos dict =
    let
        dirs =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, 1 ), ( 1, -1 ), ( -1, 1 ), ( -1, -1 ) ]
    in
    dirs |> List.map (\x -> expand pos x |> isWord dict) |> List.filter identity |> List.length



-- Parse the input


part1 : List Point -> Int
part1 s =
    let
        dict =
            s |> List.map (\x -> ( x.pos, x.val )) |> Dict.fromList
    in
    s |> List.map (\x -> x.pos) |> List.map (\p -> wordCount p dict) |> List.sum



-- Part 2
-- Cross Search
-- Is the position the center of an X?


hasX : Position -> Dict Position Value -> Bool
hasX pos dict =
    let
        isSide : List (Maybe Value) -> Bool
        isSide l =
            l == [ Just 'M', Just 'S' ] || l == [ Just 'S', Just 'M' ]

        sideA =
            [ move pos ( 1, 1 ), move pos ( -1, -1 ) ] |> List.map (\x -> dict |> Dict.get x)

        sideB =
            [ move pos ( -1, 1 ), move pos ( 1, -1 ) ] |> List.map (\x -> dict |> Dict.get x)
    in
    isSide sideA && isSide sideB


part2 : List Point -> Int
part2 s =
    let
        dict =
            s |> List.map (\x -> ( x.pos, x.val )) |> Dict.fromList

        xpoints =
            s |> List.filter (\x -> x.val == 'A')
    in
    xpoints |> List.map (\x -> x.pos) |> List.map (\p -> hasX p dict) |> List.filter (\x -> x == True) |> List.length
