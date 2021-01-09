module Pages.Dat exposing (Dat, DatValue(..), Header, Model, Msg(..), decoder, init, subscriptions, toSession, update, updateSession, view, viewKeyVal, viewVal)

import Audio
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as D
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Session)
import Util


type alias Dat =
    { filename : String, headers : List Header, data : List (List DatValue) }


type alias Header =
    { name : String, key : Maybe String }


type DatValue
    = DatString String
    | DatImg String
    | DatAudio String String
    | DatBool Bool
    | DatInt Int
    | DatFloat Float
    | DatNull
    | DatList (List DatValue)
    | DatUnknown D.Value


type alias Model =
    { session : Session
    , file : String
    , content : RemoteData String Dat

    -- page number, OR show all
    , page : Maybe Int
    }


type Msg
    = FetchedDat D.Value
    | PageAll
    | PageNext
    | PagePrev
    | PageFirst
    | PageLast


init : String -> Session -> ( Model, Cmd Msg )
init file session =
    ( { session = session
      , file = file
      , content = RemoteData.Loading
      , page = Just 0
      }
    , Ports.fetchDat file
    )


toSession : Model -> Session
toSession m =
    m.session


updateSession : Session -> Model -> Model
updateSession s m =
    { m | session = s }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedDat json ->
            case D.decodeValue decoder json of
                Ok content ->
                    ( { model | content = RemoteData.Success content }, Cmd.none )

                Err err ->
                    ( { model | content = RemoteData.Failure <| D.errorToString err }, Cmd.none )

        PageAll ->
            ( { model | page = Nothing }, Cmd.none )

        PageNext ->
            ( model |> pageTo (\_ n -> n + 1), Cmd.none )

        PagePrev ->
            ( model |> pageTo (\_ n -> n - 1), Cmd.none )

        PageFirst ->
            ( model |> pageTo (\_ _ -> 0), Cmd.none )

        PageLast ->
            -- crude, but effective
            ( model |> pageTo (\top _ -> top), Cmd.none )


pageTo : (Int -> Int -> Int) -> Model -> Model
pageTo pageFn model =
    case ( model.page, model.content ) of
        ( Just page, RemoteData.Success dat ) ->
            let
                top =
                    List.length dat.data // pageSize
            in
            { model | page = pageFn top page |> clamp 0 top |> Just }

        _ ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fetchedDat FetchedDat
        ]


view : Model -> List (Html Msg)
view model =
    [ h1 []
        [ a [ Route.href Route.Home ] [ text "pypoe-json" ]
        , text " > "
        , text model.file
        ]
    , h4 []
        [ text <| model.file ++ ".json: "
        , a [ target "_blank", href <| "https://github.com/erosson/pypoe-json/tree/master/dist/dat/" ++ model.file ++ ".json" ] [ text "github" ]
        , text ", "
        , a [ target "_blank", href <| "https://erosson.github.io/pypoe-json/dist/dat/" ++ model.file ++ ".json" ] [ text "raw" ]
        , span [] <|
            case model.content of
                RemoteData.Success dat ->
                    [ text <| " (" ++ (Util.formatInt <| List.length dat.data) ++ " rows)" ]

                _ ->
                    []
        ]
    , div [] <|
        case model.content of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success dat ->
                [ viewPaginator model dat
                , table []
                    [ thead []
                        [ tr []
                            (th [] [ text "#" ]
                                :: List.map
                                    (\h ->
                                        th []
                                            [ case h.key of
                                                Nothing ->
                                                    text h.name

                                                Just key ->
                                                    a [ Route.href <| Route.Dat key ] [ text h.name ]
                                            ]
                                    )
                                    dat.headers
                            )
                        ]
                    , tbody []
                        (dat.data
                            |> (case model.page of
                                    Nothing ->
                                        identity

                                    Just n ->
                                        List.drop (pageSize * n) >> List.take pageSize
                               )
                            |> List.indexedMap
                                (\i row ->
                                    tr []
                                        (td [] [ a [ Route.href <| Route.DatId model.file i ] [ code [] [ text "#", text <| String.fromInt <| pageSize * Maybe.withDefault 0 model.page + i ] ] ]
                                            :: List.map2
                                                (\h val ->
                                                    td []
                                                        [ case ( h.key, val ) of
                                                            ( Just key, DatInt n ) ->
                                                                a [ Route.href <| Route.DatId key n ] [ viewKeyVal h val ]

                                                            _ ->
                                                                viewKeyVal h val
                                                        ]
                                                )
                                                dat.headers
                                                row
                                        )
                                )
                        )
                    ]
                , viewPaginator model dat
                ]

            _ ->
                [ code [] [ text "loading..." ] ]
    ]


pageSize =
    250


viewPaginator : Model -> Dat -> Html Msg
viewPaginator model dat =
    case model.page of
        Nothing ->
            div [] []

        Just page ->
            let
                lo =
                    pageSize * page

                hi =
                    Basics.min top <| pageSize * (page + 1)

                top =
                    List.length dat.data
            in
            if top <= pageSize then
                div [] []

            else
                div [ class "paginator" ]
                    [ button [ onClick PageFirst ] [ text "<< First" ]
                    , button [ onClick PagePrev ] [ text "< Prev" ]
                    , div []
                        [ div []
                            [ text <| Util.formatInt lo
                            , text " - "
                            , text <| Util.formatInt hi
                            , text " of "
                            , text <| Util.formatInt top
                            ]
                        , div []
                            [ button [ onClick PageAll ]
                                [ text "Show all "
                                , text <| Util.formatInt top
                                , text " items (slow)"
                                ]
                            ]
                        ]
                    , button [ onClick PageNext ] [ text "Next >" ]
                    , button [ onClick PageLast ] [ text "Last >>" ]
                    ]


viewKeyVal : Header -> DatValue -> Html msg
viewKeyVal h val =
    case ( h.key, val ) of
        ( Just key, DatInt n ) ->
            a [ Route.href <| Route.DatId key n ] [ viewVal val ]

        ( Just key, DatList vs ) ->
            span [] [ text "[", vs |> List.map (viewKeyVal h) |> List.intersperse (text ", ") |> span [], text "]" ]

        _ ->
            viewVal val


viewVal : DatValue -> Html msg
viewVal val =
    case val of
        DatString s ->
            text s

        DatImg s ->
            let
                url =
                    "https://web.poecdn.com/image/" ++ String.replace ".dds" "" s ++ ".png?scale=1"
            in
            a [ target "_blank", href url ] [ img [ style "max-height" "2em", src url, alt s ] [] ]

        DatAudio url s ->
            a [ target "_blank", href url ] [ text s ]

        DatBool b ->
            i []
                [ text <|
                    if b then
                        "true"

                    else
                        "false"
                ]

        DatInt i ->
            code [] [ text <| String.fromInt i ]

        DatFloat f ->
            code [] [ text <| String.fromFloat f ]

        DatNull ->
            i [] [ text "null" ]

        DatList vs ->
            span [] [ text "[", vs |> List.map viewVal |> List.intersperse (text ", ") |> span [], text "]" ]

        DatUnknown v ->
            i [] [ text "???" ]


decoder : D.Decoder Dat
decoder =
    D.field "data" <|
        D.map3 Dat
            (D.field "filename" D.string)
            (D.field "header" <| D.list <| D.map2 Header (D.field "name" D.string) (D.field "key" <| D.maybe D.string))
            (D.field "data" <| D.list <| D.list valDecoder)


valDecoder : D.Decoder DatValue
valDecoder =
    D.oneOf
        [ D.string
            |> D.map
                (\s ->
                    if String.startsWith "Art/2D" s then
                        DatImg s

                    else
                        case Audio.url s of
                            Nothing ->
                                DatString s

                            Just url ->
                                DatAudio url s
                )
        , D.bool |> D.map DatBool
        , D.int |> D.map DatInt
        , D.float |> D.map DatFloat
        , D.null DatNull
        , D.list (D.lazy (\_ -> valDecoder)) |> D.map DatList
        , D.value |> D.map DatUnknown
        ]
