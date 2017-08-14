port module GamePorts exposing (..)

type alias RoundData =
  { wPressed: Bool
  , sPressed: Bool
  , oPressed: Bool
  , kPressed: Bool
  }

port startLoop : String -> Cmd msg

port roundToPlay : (RoundData -> msg) -> Sub msg
