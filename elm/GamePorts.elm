port module GamePorts exposing (RoundData, retryGame, roundToPlay, startLoop)


type alias RoundData =
    { wPressed : Bool
    , sPressed : Bool
    , oPressed : Bool
    , kPressed : Bool
    }


port startLoop : String -> Cmd msg


port roundToPlay : (RoundData -> msg) -> Sub msg


port retryGame : (Int -> msg) -> Sub msg
