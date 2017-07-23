module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
    }


init : ( Model, Cmd Msg )
init =
    ( { whale = { phase = 0 } }, Cmd.none )



-- UPDATE


type Msg
    = Unit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Unit ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text ("Whales") ]
        , svg
            [ version "1.1"
            , width "300"
            , height "300"
            , baseProfile "full"
            ]
            [
                let
                  body = "M 0 150 C 20 100, 130 50, 250 150"
                  tail = "Q 275 50 300 100 L 275 150"
                  tailLow = "L 300 190 Q 275 230 250 160"
                  bodyLow = "C 100 250, 30 240, 0 150"
                in
                  Svg.path [d (body ++ tail ++ tailLow ++ bodyLow)
                  , fill "black"
                  , strokeWidth "1"
                  , stroke "black"
                  ] []
            ]
        ]
