module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame exposing (times)
import Time exposing (Time, inSeconds, inMilliseconds)
import Space exposing (..)


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
    }


type alias Model =
    { whale : Whale
    , time : Time
    , phase : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { whale = { phase = 0 }, time = 0.0, phase = 0 }, Cmd.none )



-- UPDATE


type Msg
    = Render Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Render time ->
            if
              (time > model.time + 10)
            then
              ( { model | time = time, phase = timeToPhase time }, Cmd.none )
            else
              ( model, Cmd.none )


timeToPhase : Time -> Int
timeToPhase time =
  let
    modulo = (round (inMilliseconds time / 10)) % 100
  in
    if modulo < 50 then modulo else 100 - modulo


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Render



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text ("Whales " ++ (toString model.time) ++ " phase " ++ (toString model.phase)) ]
        , svg
            [ version "1.1"
            , width "300"
            , height "300"
            , baseProfile "full"
            ]
            [ ( whaleBody model.phase )
            , Svg.circle [cx "35", cy "140", r "5", fill "white"] []
            ]
        ]

whaleBody : Int -> Html Msg
whaleBody phase =
  let

      start = Coord 0 150
      topBodyCP1 = Coord 80 -30
      topBodyCP2 = Coord 100 120
      topBodyEnd = Coord 250 150
      topTailCP = Coord 275 50
      topTailEnd = Coord 300 100
      tailMiddle = Coord 275 150
      lowTailStart = Coord 300 190
      lowTailCP = Coord 275 230
      lowTailEnd = Coord 250 160
      lowBodyCP1 = Coord 100 250
      lowBodyCP2 = Coord 30 240
      lowBodyEnd = Coord 0 150

      body =
        [Move start, Cubic topBodyCP1 topBodyCP2 topBodyEnd]
      tail =
        [Quad topTailCP topTailEnd, Line tailMiddle]
      tailLow =
        [Line lowTailStart, Quad lowTailCP lowTailEnd]
      bodyLow =
        [Cubic lowBodyCP1 lowBodyCP2 lowBodyEnd, End]
    in
      Svg.path
          [ d ((List.concat >> pathDefStr) [body, tail, tailLow, bodyLow])
          , fill "black"
          , strokeWidth "1"
          , stroke "black"
          ]
          []
