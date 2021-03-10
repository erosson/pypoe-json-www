port module Ports exposing (..)

import Json.Decode as Json


port fetchedVersion : (Json.Value -> msg) -> Sub msg


port fetchedPypoeIndex : (Json.Value -> msg) -> Sub msg


port fetchedDatIndex : (Json.Value -> msg) -> Sub msg


port fetchDat : { file : String, lang : Maybe String } -> Cmd msg


port fetchedDat : (Json.Value -> msg) -> Sub msg


port fetchedLangs : (Json.Value -> msg) -> Sub msg
