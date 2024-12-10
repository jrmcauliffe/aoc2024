module Day08 exposing (..)

-- https://adventofcode.com/2024/day/8
-- Resonant Collinearity
-- Part 1

import Dict exposing (Dict)
import List.Extra exposing (..)


type alias Position =
    ( Int, Int )


type alias Frequency =
    Char


type alias Station =
    { pos : Position
    , val : Frequency
    }


type alias AntiNode =
    { pos : Position
    , vals : List Frequency
    }


type alias Map =
    Dict Frequency (List Position)


mapDim : String -> ( Int, Int )
mapDim s =
    let
        lines =
            s |> String.lines

        firstLine =
            List.head lines |> Maybe.withDefault ""
    in
    ( String.length firstLine - 1, List.length lines - 1 )


parse : String -> List Station
parse s =
    s |> String.lines |> List.indexedMap (\y v -> parseLine y v) |> List.concat |> List.filter (\x -> x.val /= '.')


buildMap : List Station -> Map
buildMap stations =
    let
        updateMap : Position -> Frequency -> Map -> Map
        updateMap p f m =
            case Dict.get f m of
                Just positions ->
                    Dict.insert f (p :: positions) m

                Nothing ->
                    Dict.insert f [ p ] m
    in
    stations |> List.foldl (\v d -> updateMap v.pos v.val d) Dict.empty


parseLine : Int -> String -> List Station
parseLine y s =
    s |> String.trim |> String.toList |> List.indexedMap (\x v -> { pos = ( x, y ), val = v })


allPairs : List Position -> List ( Position, Position )
allPairs list =
    let
        tupleSort : ( Position, Position ) -> Maybe ( Position, Position )
        tupleSort ( a, b ) =
            if a < b then
                Just ( a, b )

            else if a == b then
                Nothing

            else
                Just ( b, a )
    in
    List.concatMap (\x -> List.map (\y -> ( x, y )) list) list |> List.map tupleSort |> List.Extra.unique |> List.filterMap identity


antiNodes : ( Position, Position ) -> List Position
antiNodes ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        dx =
            x2 - x1

        dy =
            y2 - y1
    in
    [ ( x1 - dx, y1 - dy ), ( x2 + dx, y2 + dy ) ]


part1 : String -> Int
part1 s =
    let
        stations =
            s |> parse |> buildMap

        ( max_x, max_y ) =
            s |> mapDim

        onMap ( x, y ) =
            x <= max_x && y <= max_y && x >= 0 && y >= 0
    in
    stations |> Dict.toList |> List.map (\( _, l ) -> l |> allPairs) |> List.concat |> List.map antiNodes |> List.concat |> List.filter onMap |> List.Extra.unique |> List.length


cast : ( Int, Int ) -> ( Int, Int ) -> Position -> List Position
cast ( dx, dy ) ( max_x, max_y ) ( x, y ) =
    let
        xx =
            x + dx

        yy =
            y + dy
    in
    if xx > max_x || yy > max_y || xx < 0 || yy < 0 then
        []

    else
        ( xx, yy ) :: cast ( dx, dy ) ( max_x, max_y ) ( xx, yy )


antiNodes2 : ( Int, Int ) -> ( Position, Position ) -> List Position
antiNodes2 ( max_x, max_y ) ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        dx =
            x2 - x1

        dy =
            y2 - y1
    in
    ( x1, y1 ) :: ( x2, y2 ) :: ([ ( x1, y1 ) |> cast ( -dx, -dy ) ( max_x, max_y ), ( x2, y2 ) |> cast ( dx, dy ) ( max_x, max_y ) ] |> List.concat)


part2 : String -> Int
part2 s =
    let
        stations =
            s |> parse |> buildMap

        ( max_x, max_y ) =
            s |> mapDim

        onMap ( x, y ) =
            x <= max_x && y <= max_y && x >= 0 && y >= 0
    in
    stations |> Dict.toList |> List.map (\( _, l ) -> l |> allPairs) |> List.concat |> List.map (antiNodes2 ( max_x, max_y )) |> List.concat |> List.Extra.unique |> List.length
