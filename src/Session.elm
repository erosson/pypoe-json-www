module Session exposing (Flags, IndexEntry, Session, fileLangPath, indexDecoder, init)

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as D
import Maybe.Extra
import RemoteData exposing (RemoteData)
import Set exposing (Set)
import Util exposing (Lang)


type alias Session =
    -- Nav.Key cannot be unit tested; Maybe Nav.Key is a workaround.
    -- See https://github.com/elm-explorations/test/issues/24
    { version : RemoteData String String
    , index : RemoteData String Index
    , langs : RemoteData String (List String)
    , dataUrl : String
    , nav : Maybe Nav.Key
    }


type alias Index =
    { list : List IndexEntry, byFilename : Dict String IndexEntry }


type alias IndexEntry =
    -- This used to have more fields; it was simplified very quickly. Artifacts like `Index.byFilename` are still around.
    { filename : String }


type alias Flags =
    { dataUrl : String }


init : Flags -> Maybe Nav.Key -> Session
init flags =
    Session RemoteData.Loading RemoteData.Loading RemoteData.Loading flags.dataUrl


indexDecoder : D.Decoder Index
indexDecoder =
    D.map IndexEntry
        D.string
        |> D.list
        |> D.map (List.filter (.filename >> String.endsWith ".min.json"))
        |> D.map
            (\list ->
                list
                    |> Dict.Extra.fromListBy .filename
                    |> Index list
            )


fileLangPath : Lang -> String -> Session -> String
fileLangPath mlang file session =
    let
        lang : String
        lang =
            Maybe.withDefault "default" mlang
    in
    [ "/pypoe/v1/tree"
    , RemoteData.withDefault "???" session.version
    , lang
    , file
    ]
        |> String.join "/"
