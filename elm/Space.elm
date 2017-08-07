module Space exposing (..)

import List exposing (..)


type Coord
    = Coord Int Int


type PathCommand
    = Move Coord
    | Cubic Coord Coord Coord
    | Quad Coord Coord
    | Line Coord
    | End


type Bounds
    = Bounds Coord Coord


type alias WhaleCoords =
    { start : Coord
    , topBodyCP1 : Coord
    , topBodyCP2 : Coord
    , topBodyEnd : Coord
    , topTailCP : Coord
    , topTailEnd : Coord
    , tailMiddle : Coord
    , lowTailStart : Coord
    , lowTailCP : Coord
    , lowTailEnd : Coord
    , lowBodyCP1 : Coord
    , lowBodyCP2 : Coord
    , lowBodyEnd : Coord
    , eye : Coord
    }


getX : Coord -> Int
getX (Coord x y) =
    x


getY : Coord -> Int
getY (Coord x y) =
    y


cStr : Coord -> String
cStr (Coord x y) =
    (toString x) ++ " " ++ (toString y)


dStr : PathCommand -> String
dStr pc =
    case pc of
        Move c ->
            "M " ++ (cStr c)

        Cubic c1 c2 end ->
            "C " ++ (cStr c1) ++ " " ++ (cStr c2) ++ " " ++ (cStr end)

        Quad c end ->
            "Q " ++ (cStr c) ++ " " ++ (cStr end)

        Line end ->
            "L " ++ (cStr end)

        End ->
            "Z"


pathDefStr : List PathCommand -> String
pathDefStr commands =
    let
        cmdStrings =
            List.map (dStr) commands

        stringList =
            intersperse " " cmdStrings
    in
        foldr (++) "" stringList
