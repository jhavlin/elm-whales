module Obstacles exposing (Obstacle(..), Obstacles, Shape(..), Shapes, leftGameObstacles, rightGameObstacles)

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
    [ ImageObstacle "img/game/coral1.png" 0 100 (174 * 2) (167 * 2) "blue"
    , CompoundObstacle 420 [ Rect (Coord 0 700) 160 210 ]
    , ImageObstacle "img/game/coral2.png" 730 200 716 404 "blue"
    , CompoundObstacle 1900
        [ Rect (Coord 0 90) 50 50
        , Rect (Coord 20 260) 50 40
        , Rect (Coord 40 495) 50 40
        , Rect (Coord 60 680) 50 50
        , Rect (Coord 60 850) 50 40
        ]
    , ImageObstacle "img/game/coral3.png" 2400 90 281 504 "blue"
    , ImageObstacle "img/game/coral4.png" 3150 150 679 723 "blue"
    , CompoundObstacle 4300
        [ Rect (Coord 0 0) 50 300
        , Rect (Coord 0 600) 50 300
        ]
    ]


rightGameObstacles : Obstacles
rightGameObstacles =
    [ ImageObstacle "img/game/fish1.png" 0 100 487 306 "blue"
    , ImageObstacle "img/game/fish2.png" 900 280 733 600 "blue"
    , CompoundObstacle 2200
        [ Rect (Coord 0 110) 250 50
        , Rect (Coord 0 250) 250 45
        , Rect (Coord 0 400) 250 40
        , Rect (Coord 0 550) 250 35
        , Rect (Coord 0 700) 250 30
        , Rect (Coord 0 870) 250 20
        ]
    , ImageObstacle "img/game/fish3.png" 2935 100 501 535 "blue"
    , ImageObstacle "img/game/fish4.png" 3866 0 733 300 "blue"
    , ImageObstacle "img/game/fish5.png" 3867 599 731 301 "blue"
    ]
