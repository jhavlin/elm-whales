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
    , CompoundObstacle 1900
          [ Rect (Coord 0 90) 50 50
          , Rect (Coord 20 260) 50 40
          , Rect (Coord 40 495) 50 40
          , Rect (Coord 60 680) 50 50
          , Rect (Coord 60 850) 50 40
          ]
    , ImageObstacle "img/ux.png" 2400 90 (281) (504) "blue"
    , ImageObstacle "img/penize.png" 3150 150 (679) (723) "blue"
    , CompoundObstacle 4300
          [ Rect (Coord 0 0) 50 300
          , Rect (Coord 0 600) 50 300
          ]

    ]


rightGameObstacles : Obstacles
rightGameObstacles =
    []
