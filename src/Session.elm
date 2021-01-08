module Session exposing (IndexEntry, Session, indexDecoder, init)

import Browser.Navigation as Nav
import Json.Decode as D
import RemoteData exposing (RemoteData)


type alias Session =
    -- Nav.Key cannot be unit tested; Maybe Nav.Key is a workaround.
    -- See https://github.com/elm-explorations/test/issues/24
    { version : RemoteData String String
    , index : RemoteData String (List (Result String IndexEntry))
    , nav : Maybe Nav.Key
    }


type alias IndexEntry =
    { filename : String, numHeaders : Int, numItems : Int, size : Int }


init : Maybe Nav.Key -> Session
init =
    Session RemoteData.Loading RemoteData.Loading


indexDecoder : D.Decoder (List (Result String IndexEntry))
indexDecoder =
    D.oneOf
        [ D.map4 IndexEntry
            (D.field "filename" D.string)
            (D.field "numHeaders" D.int)
            (D.field "numItems" D.int)
            (D.field "size" D.int)
            |> D.map Ok
        , D.map2 Tuple.pair
            (D.field "filename" D.string)
            (D.field "missing" D.bool)
            |> D.map (Tuple.first >> Err)
        ]
        |> D.list
