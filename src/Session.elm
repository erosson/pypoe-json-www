module Session exposing (Flags, IndexEntry, Session, fileLangPath, hasFileLanguage, indexDecoder, init)

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
    , githubUrl : String
    , nav : Maybe Nav.Key
    }


type alias Index =
    { list : List (Result String IndexEntry), byFilename : Dict String IndexEntry }


type alias IndexEntry =
    { filename : String, numHeaders : Int, numItems : Int, size : Int, langs : Set String }


type alias Flags =
    { dataUrl : String, githubUrl : String }


init : Flags -> Maybe Nav.Key -> Session
init flags =
    Session RemoteData.Loading RemoteData.Loading RemoteData.Loading flags.dataUrl flags.githubUrl


indexDecoder : D.Decoder Index
indexDecoder =
    D.oneOf
        [ D.map5 IndexEntry
            (D.field "filename" D.string)
            (D.field "numHeaders" D.int)
            (D.field "numItems" D.int)
            (D.field "size" D.int)
            (D.field "langs" <| D.map Set.fromList <| D.list D.string)
            |> D.map Ok
        , D.map2 Tuple.pair
            (D.field "filename" D.string)
            (D.field "missing" D.bool)
            |> D.map (Tuple.first >> Err)
        ]
        |> D.list
        |> D.map
            (\list ->
                list
                    |> List.filterMap Result.toMaybe
                    |> Dict.Extra.fromListBy .filename
                    |> Index list
            )


hasFileLanguage : String -> String -> Session -> Bool
hasFileLanguage lang file =
    .index >> RemoteData.unwrap False (.byFilename >> Dict.get file >> Maybe.Extra.unwrap False (.langs >> Set.member lang))


fileLangPath : Lang -> String -> Session -> String
fileLangPath mlang file session =
    case mlang of
        Nothing ->
            "/dat/" ++ file ++ ".json"

        Just lang ->
            if hasFileLanguage lang file session then
                "/lang/" ++ lang ++ "/" ++ file ++ ".json"

            else
                "/dat/" ++ file ++ ".json"
