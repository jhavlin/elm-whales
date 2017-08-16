module Obstacles exposing (..)

import Space exposing (..)


type alias Obstacles =
    List Obstacle


type Obstacle
    = CompoundObstacle Int Shapes
    | ImageObstacle String Int Int Int Int String -- x y w h color


type alias Shapes =
    List Shape


type Shape
    = Circle Coord Int
    | Rect Coord Int Int


leftGameObstacles : Obstacles
leftGameObstacles =
    [ ImageObstacle "img/hoblovacka.png" 0 100 (174 * 2) (167 * 2) "blue"
    , CompoundObstacle 420 [ Rect (Coord 0 700) 160 210 ]
    , ImageObstacle "img/pivo.png" 730 200 (716) (404) "blue"
    , ImageObstacle "img/ux.png" 1900 90 (281) (504) "blue"
    , CompoundObstacle 2550
        [ Rect (Coord 0 90) 50 50
        , Rect (Coord 20 260) 50 45
        , Rect (Coord 40 495) 50 40
        , Rect (Coord 60 680) 50 50
        , Rect (Coord 60 850) 50 40
        ]
    , ImageObstacle "img/penize.png" 3100 0 (679) (723) "blue"
    ]


rightGameObstacles : Obstacles
rightGameObstacles =
    []
