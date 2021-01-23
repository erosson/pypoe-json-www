module Pages.Dat exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , toSession
    , update
    , updateSession
    , view
    , viewKeyVal
    , viewVal
    )

import Browser
import Dat exposing (Dat, Entry, Header)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as D
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Session)
import Util exposing (Lang)


type alias Model =
    { session : Session
    , lang : Lang
    , file : String
    , content : RemoteData String Dat

    -- page number, OR show all
    , page : Maybe Int
    , search : String
    }


type Msg
    = FetchedDat D.Value
    | PageAll
    | PageNext
    | PagePrev
    | PageFirst
    | PageLast
    | SearchInput String


init : Lang -> String -> Session -> ( Model, Cmd Msg )
init lang file session =
    ( { session = session
      , lang = lang
      , file = file
      , content = RemoteData.Loading
      , page = Just 0
      , search = ""
      }
    , Ports.fetchDat { lang = lang, file = file }
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
            case D.decodeValue Dat.decoder json of
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

        SearchInput str ->
            ( { model | search = str }, Cmd.none )


applySearch : String -> Dat -> List Entry
applySearch search { headers, data } =
    if search == "" then
        data

    else
        data |> List.filter (Dat.entrySearchText headers >> String.contains search)


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


view : Model -> Browser.Document Msg
view model =
    { title = "pypoe-json > " ++ model.file
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ h1 []
        [ a [ Route.href Route.home ] [ text "pypoe-json" ]
        , text " > "
        , text model.file
        ]
    , h4 []
        [ text <| model.file ++ ".json: "
        , a [ target "_blank", href <| model.session.githubUrl ++ Session.fileLangPath model.lang model.file model.session ] [ text "github" ]
        , text ", "
        , a [ target "_blank", href <| model.session.dataUrl ++ Session.fileLangPath model.lang model.file model.session ] [ text "raw" ]
        , span [] <|
            case model.content of
                RemoteData.Success dat ->
                    [ text <| " (" ++ (Util.formatInt <| List.length dat.data) ++ " rows)" ]

                _ ->
                    []
        ]
    , p [] <|
        case model.lang of
            Just lang ->
                [ text <| "File contains " ++ lang ++ " text: "
                , b []
                    [ text <|
                        if Session.hasFileLanguage lang model.file model.session then
                            "yes"

                        else
                            "no"
                    ]
                ]

            Nothing ->
                []
    , div [] <|
        case model.content of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success dat ->
                let
                    entries =
                        applySearch model.search dat
                in
                [ viewPaginator model entries
                , viewSearch model
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
                                                    a [ Route.href <| Route.Dat model.lang key ] [ text h.name ]
                                            ]
                                    )
                                    dat.headers
                            )
                        ]
                    , tbody []
                        (entries
                            |> (case model.page of
                                    Nothing ->
                                        identity

                                    Just n ->
                                        List.drop (pageSize * n) >> List.take pageSize
                               )
                            |> List.indexedMap
                                (\i row ->
                                    tr []
                                        (td [] [ a [ Route.href <| Route.DatId model.lang model.file i ] [ code [] [ text "#", text <| String.fromInt <| pageSize * Maybe.withDefault 0 model.page + i ] ] ]
                                            :: List.map2
                                                (\h val ->
                                                    td []
                                                        [ case ( h.key, val ) of
                                                            ( Just key, Dat.IntVal n ) ->
                                                                a [ Route.href <| Route.DatId model.lang key n ] [ viewKeyVal model.lang h val ]

                                                            _ ->
                                                                viewKeyVal model.lang h val
                                                        ]
                                                )
                                                dat.headers
                                                row.vals
                                        )
                                )
                        )
                    ]
                , viewPaginator model entries
                , viewSearch model
                ]

            _ ->
                [ code [] [ text "loading..." ] ]
    ]


pageSize =
    250


viewSearch : Model -> Html Msg
viewSearch model =
    div []
        [ input
            [ placeholder "Search"
            , value model.search
            , onInput SearchInput
            ]
            []
        ]


viewPaginator : Model -> List Entry -> Html Msg
viewPaginator model data =
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
                    List.length data
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


viewKeyVal : Lang -> Header -> Dat.Value -> Html msg
viewKeyVal lang h val =
    case ( h.key, val ) of
        ( Just key, Dat.IntVal n ) ->
            a [ Route.href <| Route.DatId lang key n ] [ viewVal val ]

        ( Just key, Dat.ListVal vs ) ->
            span [] [ text "[", vs |> List.map (viewKeyVal lang h) |> List.intersperse (text ", ") |> span [], text "]" ]

        _ ->
            viewVal val


viewVal : Dat.Value -> Html msg
viewVal val =
    case val of
        Dat.StringVal s ->
            text s

        Dat.ImgVal s ->
            let
                url =
                    "https://web.poecdn.com/image/" ++ String.replace ".dds" "" s ++ ".png?scale=1"
            in
            div []
                [ div [] [ a [ target "_blank", href url ] [ img [ style "max-height" "2em", src url, title s, alt s ] [] ] ]
                , div [] [ a [ target "_blank", href url ] [ text s ] ]
                ]

        Dat.AudioVal url s ->
            a [ target "_blank", href url ] [ text s ]

        Dat.BoolVal b ->
            i []
                [ text <|
                    if b then
                        "true"

                    else
                        "false"
                ]

        Dat.IntVal i ->
            code [] [ text <| String.fromInt i ]

        Dat.FloatVal f ->
            code [] [ text <| String.fromFloat f ]

        Dat.NullVal ->
            i [] [ text "null" ]

        Dat.ListVal vs ->
            span [] [ text "[", vs |> List.map viewVal |> List.intersperse (text ", ") |> span [], text "]" ]

        Dat.UnknownVal v ->
            i [] [ text "???" ]
