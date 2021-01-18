module Dat exposing
    ( Dat
    , Entry
    , Header
    , Value(..)
    , decoder
    , entryEncoder
    , objEncoder
    )

import Audio
import Json.Decode as D
import Json.Encode as E


type alias Dat =
    { filename : String, headers : List Header, data : List Entry }


type alias Header =
    { name : String, key : Maybe String }


type alias Entry =
    { vals : List Value }


type Value
    = StringVal String
    | ImgVal String
    | AudioVal String String
    | BoolVal Bool
    | IntVal Int
    | FloatVal Float
    | NullVal
    | ListVal (List Value)
    | UnknownVal D.Value


decoder : D.Decoder Dat
decoder =
    D.field "data" <|
        D.map3 Dat
            (D.field "filename" D.string)
            (D.field "header" <| D.list <| D.map2 Header (D.field "name" D.string) (D.field "key" <| D.maybe D.string))
            (D.field "data" <| D.list entryDecoder)


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map Entry
        (D.list valDecoder)


valDecoder : D.Decoder Value
valDecoder =
    D.oneOf
        [ D.string
            |> D.map
                (\s ->
                    if String.startsWith "Art/2D" s || String.startsWith "Art/Texture" s then
                        ImgVal s

                    else
                        case Audio.url s of
                            Nothing ->
                                StringVal s

                            Just url ->
                                AudioVal url s
                )
        , D.bool |> D.map BoolVal
        , D.int |> D.map IntVal
        , D.float |> D.map FloatVal
        , D.null NullVal
        , D.list (D.lazy (\_ -> valDecoder)) |> D.map ListVal
        , D.value |> D.map UnknownVal
        ]


entryEncoder : Entry -> E.Value
entryEncoder =
    .vals >> List.map valEncoder >> E.list identity


objEncoder : List Header -> Entry -> E.Value
objEncoder headers =
    .vals >> List.map2 (\h v -> ( h.name, valEncoder v )) headers >> E.object


valEncoder val =
    case val of
        StringVal s ->
            E.string s

        ImgVal s ->
            E.string s

        AudioVal _ s ->
            E.string s

        BoolVal b ->
            E.bool b

        IntVal i ->
            E.int i

        FloatVal s ->
            E.float s

        NullVal ->
            E.null

        ListVal vs ->
            vs |> List.map valEncoder |> E.list identity

        UnknownVal json ->
            json
