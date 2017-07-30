module Obstacles exposing (..)

import Space exposing (..)


type alias Obstacles =
    List Obstacle


type Obstacle
    = Obstacle Int Shapes


type alias Shapes =
    List Shape


type Shape
    = Circle Coord Int
    | Rect Coord Int Int


leftGameObstacles : Obstacles
leftGameObstacles =
    [ Obstacle 10 [ Rect (Coord 0 30) 100 500 ]
    , Obstacle 420 [ Rect (Coord 0 600) 150 280 ]
    ]


rightGameObstacles : Obstacles
rightGameObstacles =
    [ Obstacle 10 [ Rect (Coord 0 30) 100 500 ]
    , Obstacle 420 [ Rect (Coord 0 600) 150 280 ]
    ]
