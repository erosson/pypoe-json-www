module Session exposing (CombinedIndexEntry, Flags, PypoeEntry, Session, datIndexDecoder, fileLangPathDat, fileLangPathJson, init, pypoeIndexDecoder, updateDatIndex, updatePypoeIndex)

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
    , pypoeIndex : RemoteData String PypoeIndex
    , datIndex : RemoteData String DatIndex
    , index : RemoteData String (List CombinedIndexEntry)
    , langs : RemoteData String (List String)
    , dataUrl : String
    , nav : Maybe Nav.Key
    }


type alias CombinedIndexEntry =
    { name : String, pypoe : Maybe PypoeEntry }


type alias PypoeIndex =
    { list : List PypoeEntry, byFilename : Dict String PypoeEntry }


type alias DatIndex =
    List String


type alias PypoeEntry =
    { filename : String, numHeaders : Int, numItems : Int, size : Int }


type alias Flags =
    { dataUrl : String }


init : Flags -> Maybe Nav.Key -> Session
init flags =
    Session RemoteData.Loading RemoteData.Loading RemoteData.Loading RemoteData.Loading RemoteData.Loading flags.dataUrl


updatePypoeIndex : RemoteData String PypoeIndex -> Session -> Session
updatePypoeIndex index session =
    { session | pypoeIndex = index } |> combineIndexes


updateDatIndex : RemoteData String DatIndex -> Session -> Session
updateDatIndex index session =
    { session | datIndex = index } |> combineIndexes


combineIndexes session =
    { session
        | index =
            RemoteData.map2 (\pypoe -> List.map (\name -> CombinedIndexEntry name <| Dict.get name pypoe.byFilename))
                session.pypoeIndex
                session.datIndex
    }


pypoeIndexDecoder : D.Decoder PypoeIndex
pypoeIndexDecoder =
    -- example: https://poedat.erosson.org/pypoe/v1/tree/3.13.1e/pypoe.json
    D.map4 PypoeEntry
        (D.field "filename" D.string)
        (D.field "numHeaders" D.int)
        (D.field "numItems" D.int)
        (D.field "size" D.int)
        |> D.list
        |> D.map
            (\list ->
                list
                    |> Dict.Extra.fromListBy .filename
                    |> PypoeIndex list
            )


datIndexDecoder : D.Decoder DatIndex
datIndexDecoder =
    -- example: https://poedat.erosson.org/dat/v1/tree/3.13.1e/Data/index.json
    D.list D.string
        |> D.map (List.filter (String.endsWith ".dat"))


fileLangPathJson : Lang -> String -> Session -> String
fileLangPathJson mlang file session =
    let
        lang : String
        lang =
            Maybe.withDefault "default" mlang
    in
    [ "/pypoe/v1/tree"
    , RemoteData.withDefault "???" session.version
    , lang
    , file ++ ".min.json"
    ]
        |> String.join "/"


fileLangPathDat : Lang -> String -> Session -> String
fileLangPathDat mlang file session =
    [ Just "/dat/v1/tree"
    , Just <| RemoteData.withDefault "???" session.version
    , Just "Data"
    , mlang
    , Just file
    ]
        |> List.filterMap identity
        |> String.join "/"
