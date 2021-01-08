port module Ports exposing (..)

import Json.Decode as Json


port fetchedVersion : (Json.Value -> msg) -> Sub msg


port fetchedIndexDat : (Json.Value -> msg) -> Sub msg


port fetchDat : String -> Cmd msg


port fetchedDat : (Json.Value -> msg) -> Sub msg
