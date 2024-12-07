module Day06 exposing (..)

-- https://adventofcode.com/2024/day/6
--
-- Part 1
--

import Dict exposing (Dict)


type alias Position =
    ( Int, Int )


type Direction
    = N
    | E
    | S
    | W


type Value
    = Visited (List Direction)
    | Unvisited
    | Obstacle
    | Dir Direction


type alias Point =
    { pos : Position
    , val : Value
    }


type alias State =
    Dict Position Value


parse : String -> State
parse s =
    s |> String.lines |> List.indexedMap (\y v -> parseLine y v) |> List.concat |> List.map (\x -> ( x.pos, x.val )) |> Dict.fromList


parseLine : Int -> String -> List Point
parseLine y s =
    s |> String.trim |> String.toList |> List.indexedMap (\x v -> { pos = ( x, y ), val = parseVal v })


parseVal : Char -> Value
parseVal c =
    case c of
        '.' ->
            Unvisited

        '#' ->
            Obstacle

        '^' ->
            Dir N

        '>' ->
            Dir E

        '<' ->
            Dir W

        'v' ->
            Dir S

        _ ->
            Unvisited


rotate : Direction -> Direction
rotate d =
    case d of
        N ->
            E

        E ->
            S

        S ->
            W

        W ->
            N


move : Position -> Direction -> Position
move ( x, y ) d =
    case d of
        N ->
            ( x, y - 1 )

        E ->
            ( x + 1, y )

        S ->
            ( x, y + 1 )

        W ->
            ( x - 1, y )


whereIsGuard : State -> ( Position, Direction )
whereIsGuard s =
    let
        isDir ( p, v ) =
            case v of
                Dir _ ->
                    True

                _ ->
                    False

        point =
            s |> Dict.toList |> List.filter isDir |> List.head
    in
    case point of
        Just ( p, Dir d ) ->
            ( p, d )

        _ ->
            ( ( 0, 0 ), N )


advance : State -> State
advance s =
    let
        ( p, d ) =
            whereIsGuard s

        newPos =
            move p d
    in
    case Dict.get newPos s of
        Just Obstacle ->
            advance (Dict.insert p (Dir (rotate d)) s)

        Just _ ->
            advance (Dict.insert newPos (Dir d) (logDir p d s))

        Nothing ->
            logDir p d s


logDir : Position -> Direction -> State -> State
logDir p d s =
    case Dict.get p s of
        Just (Visited dd) ->
            Dict.insert p (Visited (d :: dd)) s

        _ ->
            Dict.insert p (Visited [ d ]) s


isVisited : ( Position, Value ) -> Bool
isVisited ( _, v ) =
    case v of
        Visited _ ->
            True

        _ ->
            False


part1 : State -> Int
part1 s =
    s |> advance |> Dict.toList |> List.filter isVisited |> List.length



-- Does the guard re-vist the same point while facing the same direction?


getsStuck : ( Position, Direction ) -> State -> Bool
getsStuck ( p, d ) s =
    let
        newPos =
            move p d
    in
    case Dict.get newPos s of
        Just Obstacle ->
            getsStuck ( p, rotate d ) (logDir p d s)

        Just (Visited dd) ->
            if List.member d dd then
                True

            else
                getsStuck ( newPos, d ) (logDir p d s)

        Just _ ->
            getsStuck ( newPos, d ) (logDir p d s)

        Nothing ->
            False


part2 : State -> Int
part2 s =
    let
        ( pos, d ) =
            whereIsGuard s

        -- Get a list of all visited points and create a new initial state with an obstacle in each of them (without including the starting point)
        allStateVersion =
            s |> advance |> Dict.remove pos |> Dict.toList |> List.filter isVisited |> List.map (\( p, _ ) -> s |> Dict.insert p Obstacle)
    in
    allStateVersion |> List.filter (getsStuck ( pos, d )) |> List.length
