module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (tabindex)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame exposing (times)
import Time exposing (Time, inSeconds, inMilliseconds, every, millisecond)
import Space exposing (..)
import Obstacles exposing (..)
import GamePorts
import Json.Decode


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Whale =
    { phase : Int
    , posX : Int
    , posY : Int
    }


type alias Game =
    { whale : Whale
    , direction : Int
    , obstacles : Obstacles
    }


type alias Model =
    { leftGame : Game
    , rightGame : Game
    , time : Time
    , phase : Int
    , dist : Int
    , gameState : GameState
    }


type GameState
    = Play
    | Ended


init : ( Model, Cmd Msg )
init =
    let
        whale =
            { phase = 0
            , posX = 310
            , posY = 400 -- 170
            }

        leftGame =
            { whale = whale
            , direction = -1
            , obstacles = leftGameObstacles
            }

        rightGame =
            { whale = whale
            , direction = 1
            , obstacles = rightGameObstacles
            }
    in
        ( { leftGame = leftGame
          , rightGame = rightGame
          , time = 0.0
          , phase = 0
          , dist = -800
          , gameState = Play
          }
        , GamePorts.startLoop ""
        )



-- UPDATE


type Msg
    = Render GamePorts.RoundData
    | Retry Int


winDist : Int
winDist =
    4400


finalDist : Int
finalDist =
    4400 + 320


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Render { wPressed, sPressed, oPressed, kPressed } ->
            ( { model
                | time = model.time + 15
                , phase = timeToPhase model.time
                , dist = model.dist + 1
                , gameState =
                    if (collides model.leftGame model) || (collides model.rightGame model) then
                        Ended
                    else
                        model.gameState
                , leftGame =
                    if model.dist > winDist then
                        if model.dist < finalDist then
                            moveToWin model.leftGame
                        else
                            model.leftGame
                    else if wPressed then
                        moveInGame model.leftGame -1
                    else if sPressed then
                        moveInGame model.leftGame 1
                    else
                        model.leftGame
                , rightGame =
                    if model.dist > winDist then
                        if model.dist < finalDist then
                            moveToWin model.rightGame
                        else
                            model.rightGame
                    else if oPressed then
                        moveInGame model.rightGame -1
                    else if kPressed then
                        moveInGame model.rightGame 1
                    else
                        model.rightGame
              }
            , Cmd.none
            )

        Retry _ ->
            if model.gameState == Ended then
                let
                    initState =
                        init

                    initialModel ( model, cmd ) =
                        model
                in
                    ( initialModel initState, Cmd.none )
            else
                ( model, Cmd.none )


handleKey : Model -> Int -> Model
handleKey model code =
    case code of
        87 ->
            { model | leftGame = moveInGame model.leftGame -1 }

        -- w
        83 ->
            { model | leftGame = moveInGame model.leftGame 1 }

        -- s
        79 ->
            { model | rightGame = moveInGame model.rightGame -1 }

        -- o
        75 ->
            { model | rightGame = moveInGame model.rightGame 1 }

        -- k
        _ ->
            model


moveInGame : Game -> Int -> Game
moveInGame game direction =
    { game | whale = moveWhale game.whale direction }


moveWhale : Whale -> Int -> Whale
moveWhale whale direction =
    let
        rawPos =
            whale.posY + (direction * 5)

        pos =
            if rawPos < 0 then
                0
            else if rawPos > 900 then
                900
            else
                rawPos
    in
        { whale | posY = pos }


moveToWin : Game -> Game
moveToWin game =
    let
        moveWhaleForward whale =
            { whale | posX = whale.posX + 3 }
    in
        { game | whale = moveWhaleForward game.whale }


collides : Game -> Model -> Bool
collides { whale, direction, obstacles } model =
    let
        ws =
            whaleShapes whale model.phase

        whaleCollidesWithObstacle obstacle =
            List.any (\s -> shapeCollidesWithObstacle s obstacle model.dist) ws
    in
        List.any (whaleCollidesWithObstacle) obstacles


shapeCollidesWithObstacle : Shape -> Obstacle -> Int -> Bool
shapeCollidesWithObstacle shape obstacle dist =
    case shape of
        -- shape in the whale
        Circle (Coord x y) r ->
            let
                obstacleShapeCollides base shape =
                    case shape of
                        -- shape in the obstacle
                        Circle (Coord ox oy) or_ ->
                            let
                                rx =
                                    base + ox - dist

                                distanceOfCenters =
                                    (toFloat >> sqrt) ((abs (x - rx)) ^ 2 + (abs (y - oy)) ^ 2)
                            in
                                distanceOfCenters < toFloat (r + or_)

                        Rect (Coord ox oy) ow oh ->
                            let
                                -- fix coordinates - enlarge rect by radius
                                fx1 =
                                    base + ox - dist - r

                                fy1 =
                                    oy - r

                                fx2 =
                                    base + ox - dist + ow + r

                                fy2 =
                                    oy + oh + r
                            in
                                (x > fx1) && (x < fx2) && (y > fy1) && (y < fy2)
            in
                case obstacle of
                    CompoundObstacle base shapes ->
                        List.any (obstacleShapeCollides base) shapes

                    ImageObstacle src ix iy iw ih clr ->
                        obstacleShapeCollides 0 (Rect (Coord ix iy) iw ih)

        _ ->
            True



-- TODO, no rectangles in whale shapes


timeToPhase : Time -> Int
timeToPhase time =
    let
        range =
            200

        modulo =
            (round (inMilliseconds time / 10)) % range

        absVal =
            if modulo < (range // 2) then
                modulo
            else
                range - modulo
    in
        absVal - 50



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameState == Play then
        GamePorts.roundToPlay Render
    else
        GamePorts.retryGame Retry



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ let
            lWidth =
                2600

            -- logical width
            lHeight =
                900

            -- logical height
            leftGameBounds =
                Bounds (Coord 10 10) (Coord ((lWidth // 2) - 10) (lHeight - 10))

            rightGameBounds =
                Bounds (Coord ((lWidth // 2) + 10) 10) (Coord (lWidth - 10) (lHeight - 10))
          in
            div []
                [ svg
                    [ version "1.1"
                    , width "100%"

                    --, height "600"
                    , baseProfile "full"
                    , viewBox ("0 0 " ++ (toString lWidth) ++ " " ++ (toString 270))
                    ]
                    [ Svg.image [ x "100", y "0", xlinkHref "img/honza_controls.png", width "400", height "250" ] []
                    , Svg.image [ x "2100", y "0", xlinkHref "img/janina_controls.png", width "400", height "250" ] []
                    ]
                , svg
                    [ version "1.1"
                    , width "100%"

                    --, height "600"
                    , baseProfile "full"
                    , viewBox ("0 0 " ++ (toString lWidth) ++ " " ++ (toString lHeight))
                    ]
                    [ Svg.defs []
                        [ boundsToClipPath leftGameBounds "leftGameClip"
                        , boundsToClipPath rightGameBounds "rightGameClip"
                        ]
                    , Svg.rect
                        [ x "2"
                        , y "2"
                        , width (toString (lWidth - 2))
                        , height (toString (lHeight - 2))
                        , fill "lightblue"
                        , stroke "gray"
                        ]
                        []
                    , game model.leftGame leftGameBounds model.phase model.dist
                    , game model.rightGame rightGameBounds model.phase model.dist
                    , gameControls model
                    , victory model
                    ]
                ]
        ]


boundsToClipPath : Bounds -> String -> Html Msg
boundsToClipPath (Bounds (Coord x1 y1) (Coord x2 y2)) clipId =
    let
        w =
            x2 - x1

        h =
            y2 - y1
    in
        Svg.clipPath [ id clipId ]
            [ Svg.rect
                [ x (toString x1)
                , y (toString y1)
                , width (toString w)
                , height (toString h)
                ]
                []
            ]


game : Game -> Bounds -> Int -> Int -> Html Msg
game { whale, direction, obstacles } bounds globalPhase dist =
    Svg.g
        [ Svg.Attributes.clipPath
            (if direction == -1 then
                "url(#leftGameClip)"
             else
                "url(#rightGameClip)"
            )
        ]
        [ case bounds of
            Bounds (Coord x1 y1) (Coord x2 y2) ->
                Svg.rect
                    [ x (toString x1)
                    , y (toString y1)
                    , width (toString (x2 - x1))
                    , height (toString (y2 - y1))
                    , fill "none"
                    , stroke "white"
                    , strokeWidth "3"
                    ]
                    []
        , whaleBody whale bounds direction globalPhase
        , obstaclesView obstacles bounds direction dist
        ]


whaleCoords : Whale -> Int -> WhaleCoords
whaleCoords { phase, posX, posY } globalPhase =
    let
        lPhase =
            globalPhase + phase
    in
        { start = Coord (posX - 0) (posY - 0)
        , topBodyCP1 = Coord (posX - 80) (posY - 180)
        , topBodyCP2 = Coord (posX - 100) (posY - 30)
        , topBodyEnd = Coord (posX - 250) (posY + (0 + (lPhase // 1)))
        , topTailCP = Coord (posX - 275) (posY + (-100 + (lPhase // 1)))
        , topTailEnd = Coord (posX - 300) (posY + (-50 + (lPhase // 1)))
        , tailMiddle = Coord (posX - 275) (posY + (0 + (lPhase // 1)))
        , lowTailStart = Coord (posX - 300) (posY + (40 + (lPhase // 1)))
        , lowTailCP = Coord (posX - 275) (posY + (80 + (lPhase // 1)))
        , lowTailEnd = Coord (posX - 250) (posY + (10 + (lPhase // 1)))
        , lowBodyCP1 = Coord (posX - 100) (posY + 100)
        , lowBodyCP2 = Coord (posX - 30) (posY + 90)
        , lowBodyEnd = Coord (posX - 0) (posY + 0)
        , eye = Coord (posX - 35) (posY - 10)
        }


whaleBody : Whale -> Bounds -> Int -> Int -> Html Msg
whaleBody whale (Bounds (Coord x1 y1) (Coord x2 y2)) direction globalPhase =
    let
        rel (Coord x y) =
            Coord
                (if direction == 1 then
                    x2 - x
                 else
                    x1 + x
                )
                (y)

        coords =
            whaleCoords whale globalPhase

        body =
            [ Move (rel coords.start), Cubic (rel coords.topBodyCP1) (rel coords.topBodyCP2) (rel coords.topBodyEnd) ]

        tail =
            [ Quad (rel coords.topTailCP) (rel coords.topTailEnd), Line (rel coords.tailMiddle) ]

        tailLow =
            [ Line (rel coords.lowTailStart), Quad (rel coords.lowTailCP) (rel coords.lowTailEnd) ]

        bodyLow =
            [ Cubic (rel coords.lowBodyCP1) (rel coords.lowBodyCP2) (rel coords.lowBodyEnd), End ]

        absShapes =
            whaleShapes whale globalPhase

        relShape shape =
            case shape of
                Circle coord r ->
                    Circle (rel coord) r

                Rect coord w h ->
                    Rect (rel coord) w h

        relShapes =
            List.map relShape absShapes

        shapeToSvg shape =
            case shape of
                Circle (Coord x y) cr ->
                    Svg.circle [ cx (toString x), cy (toString y), r (toString cr), fill "yellow" ] []

                Rect (Coord cx cy) cw ch ->
                    Svg.rect [] []
    in
        Svg.g []
            [ Svg.path
                [ d ((List.concat >> pathDefStr) [ body, tail, tailLow, bodyLow ])
                , fill "black"
                , strokeWidth "1"
                , stroke "black"
                ]
                []
            , Svg.circle
                [ cx (toString (getX (rel (coords.eye))))
                , cy (toString (getY (rel (coords.eye))))
                , r "5"
                , fill "white"
                ]
                []

            -- , Svg.g [] (List.map shapeToSvg relShapes)
            ]


whaleShapes : Whale -> Int -> List Shape
whaleShapes whale globalPhase =
    let
        coords =
            whaleCoords whale globalPhase

        shift (Coord x y) dx dy =
            Coord (x + dx) (y + dy)
    in
        [ Circle (shift coords.start -10 0) 10
        , Circle (shift coords.topTailEnd 16 -6) 10

        -- , Circle coords.lowTailEnd 10
        , Circle (shift coords.lowTailStart 18 4) 10
        , Circle (shift coords.start -71 (-54 + (globalPhase // 20))) 30
        , Circle (shift coords.start -72 (26 + (globalPhase // 20))) 40
        , Circle (shift coords.start -110 (27 + (globalPhase // 4))) 40
        ]


obstaclesView : Obstacles -> Bounds -> Int -> Int -> Html Msg
obstaclesView obstacles (Bounds (Coord bx1 by1) (Coord bx2 by2)) direction dist =
    let
        oStart obstacle =
            case obstacle of
                CompoundObstacle start shapes ->
                    start

                ImageObstacle src ix iy iw ih clr ->
                    ix

        isActive obstacle =
            (oStart obstacle) < dist + (bx2 - bx1) && (oStart obstacle) + 1025 > dist

        active =
            List.filter (isActive) obstacles

        renderShape start shape =
            case shape of
                Circle coord r ->
                    renderCircle start coord r

                Rect coord w h ->
                    renderRect start coord w h

        renderCircle start (Coord x y) r =
            Svg.circle [] []

        -- TODO
        renderRect start (Coord x1 y1) w h =
            let
                fx1 =
                    if direction == -1 then
                        bx1 + start + x1 - dist
                    else
                        bx2 - start - w - x1 + dist

                fy1 =
                    y1
            in
                Svg.rect [ x (toString fx1), y (toString fy1), width (toString w), height (toString h), fill "red" ] []

        renderImageObstacle src ix iy iw ih clr =
            let
                fx =
                    if direction == -1 then
                        bx1 + ix - dist
                    else
                        bx2 - iw - ix + dist

                fy1 =
                    y1
            in
                Svg.image [ x (toString fx), y (toString iy), width (toString iw), height (toString ih), xlinkHref src ] []

        --Svg.g []
        --  [ Svg.rect [x (toString fx), y (toString iy), width (toString iw), height (toString ih), stroke clr, strokeWidth "2", fill "none"] []
        --  ,
        --  ]
        render obstacle =
            case obstacle of
                CompoundObstacle start shapes ->
                    Svg.g [] (List.map (\s -> renderShape start s) shapes)

                ImageObstacle src ix iy iw ih clr ->
                    (renderImageObstacle src ix iy iw ih clr)
    in
        Svg.g [] (List.map render active)


victory : Model -> Html Msg
victory model =
    if model.dist >= finalDist then
        Svg.g []
            [ Svg.rect
                [ x "800"
                , y "250"
                , width "1000"
                , height "400"
                , fill "#EEEEFF"
                , stroke "gray"
                , strokeWidth "3"
                , rx "10"
                , ry "10"
                ]
                []
            , Svg.text_ [ x "1000", y "600", fontSize "50" ] [ Svg.text "Kód je: Math.floor(π*1000)" ]
            , Svg.image [ xlinkHref "img/vyhra.png", width "450", height "223", x "1075", y "290" ] []
            ]
    else
        Svg.g [] []


gameControls : Model -> Html Msg
gameControls model =
    if model.gameState == Ended then
        Svg.g []
            [ Svg.rect
                [ x "800"
                , y "250"
                , width "1000"
                , height "400"
                , fill "#EEEEFF"
                , stroke "gray"
                , strokeWidth "3"
                , rx "10"
                , ry "10"
                , fillOpacity "0.85"
                ]
                []
            , Svg.text_ [ x "900", y "480", fontSize "80" ] [ Svg.text "Bum. Zkuste to znovu." ]
            ]
    else
        Svg.g [] []
