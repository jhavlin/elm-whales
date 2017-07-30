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
    , obstacles: Obstacles
    }


type alias Model =
    { leftGame : Game
    , rightGame : Game
    , time : Time
    , phase : Int
    , dist : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        whale =
            { phase = 0
            , posX = 310
            , posY = 170
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
          , dist = -1300
          }
        , Cmd.none
        )



-- UPDATE


type Msg
    = Render Time
    | KeyPressed Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Render time ->
            if (time > model.time + 10) then
                ( { model | time = time, phase = timeToPhase time, dist = model.dist + 1 }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyPressed code ->
            ( handleKey model code, Cmd.none )


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
      rawPos = whale.posY + (direction * 5)
      pos = if rawPos < 0 then 0 else if rawPos > 900 then 900 else rawPos
    in
      { whale | posY = pos }


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
    Time.every (25 * millisecond) Render



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ onKeyDown KeyPressed
        , tabindex -1
        ]
        [ h1 [] [ Html.text ("Whales " ++ (toString model.time) ++ " phase " ++ (toString model.phase)) ]
        , let
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
            svg
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
                ]
        ]

boundsToClipPath : Bounds -> String -> Html Msg
boundsToClipPath (Bounds (Coord x1 y1) (Coord x2 y2)) clipId =
  let
    w = x2 - x1
    h = y2 - y1
  in
    Svg.clipPath [id clipId]
      [ Svg.rect
          [ x (toString x1)
          , y (toString y1)
          , width (toString w)
          , height (toString h)
          ] []
      ]

onKeyDown : (Int -> Msg) -> Html.Attribute Msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger keyCode)


game : Game -> Bounds -> Int -> Int-> Html Msg
game { whale, direction, obstacles } bounds globalPhase dist =
    Svg.g [Svg.Attributes.clipPath (if direction == -1 then "url(#leftGameClip)" else "url(#rightGameClip)")]
        [ case bounds of
            Bounds (Coord x1 y1) (Coord x2 y2) ->
                Svg.rect
                    [ x (toString x1)
                    , y (toString y1)
                    , width (toString (x2 - x1))
                    , height (toString (y2 - y1))
                    , fill "none"
                    , stroke "white"
                    ]
                    []
        , whaleBody whale bounds direction globalPhase
        , obstaclesView obstacles bounds direction dist
        ]


whaleBody : Whale -> Bounds -> Int -> Int -> Html Msg
whaleBody { phase, posX, posY } (Bounds (Coord x1 y1) (Coord x2 y2)) direction globalPhase =
    let
        lPhase =
            globalPhase + phase

        compX x =
            if direction == 1 then
                x2 - posX + x
            else
                x1 + posX - x

        compY y =
            posY + y

        start =
            Coord (compX 0) (compY 0)

        topBodyCP1 =
            Coord (compX 80) (compY -180)

        topBodyCP2 =
            Coord (compX 100) (compY -30)

        topBodyEnd =
            Coord (compX 250) (compY (0 + (lPhase // 1)))

        topTailCP =
            Coord (compX 275) (compY (-100 + (lPhase // 1)))

        topTailEnd =
            Coord (compX 300) (compY (-50 + (lPhase // 1)))

        tailMiddle =
            Coord (compX 275) (compY (0 + (lPhase // 1)))

        lowTailStart =
            Coord (compX 300) (compY (40 + (lPhase // 1)))

        lowTailCP =
            Coord (compX 275) (compY (80 + (lPhase // 1)))

        lowTailEnd =
            Coord (compX 250) (compY (10 + (lPhase // 1)))

        lowBodyCP1 =
            Coord (compX 100) (compY 100)

        lowBodyCP2 =
            Coord (compX 30) (compY 90)

        lowBodyEnd =
            Coord (compX 0) (compY 0)

        body =
            [ Move start, Cubic topBodyCP1 topBodyCP2 topBodyEnd ]

        tail =
            [ Quad topTailCP topTailEnd, Line tailMiddle ]

        tailLow =
            [ Line lowTailStart, Quad lowTailCP lowTailEnd ]

        bodyLow =
            [ Cubic lowBodyCP1 lowBodyCP2 lowBodyEnd, End ]
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
                [ cx (toString (compX 35))
                , cy (toString (compY -10))
                , r "5"
                , fill "white"
                ]
                []
            ]

obstaclesView : Obstacles -> Bounds -> Int -> Int -> Html Msg
obstaclesView obstacles (Bounds (Coord bx1 by1) (Coord bx2 by2)) direction dist =
    let
      oStart (Obstacle start shapes) = start
      isActive obstacle = (oStart obstacle) < dist + (bx2 - bx1) && (oStart obstacle) + 300 > dist
      active = List.filter (isActive) obstacles
      renderShape start shape = case shape of
        Circle coord r  -> renderCircle start coord r
        Rect coord w h -> renderRect start coord w h
      renderCircle start (Coord x y) r =
        Svg.circle [] []
      renderRect start (Coord x1 y1) w h =
        let
          fx1 = if direction == -1 then bx1 + start + x1 - dist else bx2 - start - w - x1 + dist
          fy1 = y1
        in
          Svg.rect [x (toString fx1), y (toString fy1), width (toString w), height (toString h), fill "red"] []
      render (Obstacle start shapes ) =
        Svg.g [] (List.map (\s -> renderShape start s) shapes)
    in
      Svg.g [] (List.map render active)
