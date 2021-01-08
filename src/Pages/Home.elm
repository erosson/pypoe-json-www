module Pages.Home exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , toSession
    , update
    , updateSession
    , view
    )

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Session)
import Util


type alias Model =
    { session : Session, sortCol : Col, sortAsc : Bool }


type Col
    = Name
    | NumHeaders
    | NumItems
    | Size


type Msg
    = SortClicked Col


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, sortCol = Name, sortAsc = False }, Cmd.none )


toSession : Model -> Session
toSession m =
    m.session


updateSession : Session -> Model -> Model
updateSession s m =
    { m | session = s }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SortClicked col ->
            if model.sortCol == col then
                ( { model | sortAsc = not model.sortAsc }, Cmd.none )

            else
                ( { model | sortCol = col, sortAsc = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "pypoe-json" ]
    , p []
        [ text "Browse and download "
        , a [ target "_blank", href "https://github.com/erosson/pypoe-json" ] [ text "Path of Exile JSON data" ]
        , text " with ease."
        ]
    , p []
        [ text "Data exported from "
        , a [ target "_blank", href "https://www.pathofexile.com" ] [ text "Path of Exile" ]
        , text " using "
        , a [ target "_blank", href "https://github.com/OmegaK2/PyPoE" ] [ text "PyPoE" ]
        , text "."
        ]
    , div [] <|
        case model.session.version of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success version ->
                [ text version
                , text " ("
                , a [ target "_blank", href <| "https://github.com/erosson/pypoe-json/tree/master/dist/version.json" ] [ text "github" ]
                , text ", "
                , a [ target "_blank", href <| "https://erosson.github.io/pypoe-json/dist/version.json" ] [ text "raw" ]
                , text ")"
                ]

            _ ->
                [ text "version loading..." ]
    , div [] <|
        case model.session.index of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success index ->
                [ table []
                    [ thead []
                        [ th [] [ button [ onClick <| SortClicked Name ] [ text "Filename" ] ]
                        , th [] [ button [ onClick <| SortClicked NumHeaders ] [ text "Cols" ] ]
                        , th [] [ button [ onClick <| SortClicked NumItems ] [ text "Rows" ] ]
                        , th [] [ button [ onClick <| SortClicked Size ] [ text "Size" ] ]
                        ]
                    , tbody []
                        (index
                            |> sortIndex model.sortCol model.sortAsc
                            |> List.map
                                (\mentry ->
                                    tr [] <|
                                        case mentry of
                                            Ok entry ->
                                                [ td [] [ a [ Route.href <| Route.Dat entry.filename ] [ text entry.filename ] ]
                                                , td [] [ text <| Util.formatInt entry.numHeaders ]
                                                , td [] [ text <| Util.formatInt entry.numItems ]
                                                , td [] [ text <| formatBytes entry.size ]
                                                ]

                                            Err name ->
                                                [ td [] [ text name ]
                                                , td [ colspan 3 ] [ code [] [ text "error in PyPoE" ] ]
                                                ]
                                )
                        )
                    ]
                ]

            _ ->
                [ text ".dat list loading..." ]
    ]


sortIndex : Col -> Bool -> List (Result String Session.IndexEntry) -> List (Result String Session.IndexEntry)
sortIndex col asc =
    let
        sortFn fn default =
            List.sortBy (Result.map fn >> Result.withDefault default)

        sort : List (Result String Session.IndexEntry) -> List (Result String Session.IndexEntry)
        sort =
            case col of
                Name ->
                    List.sortBy
                        (\e ->
                            case e of
                                Ok entry ->
                                    entry.filename

                                Err name ->
                                    name
                        )

                NumHeaders ->
                    sortFn (.numHeaders >> (*) -1) 1

                NumItems ->
                    sortFn (.numItems >> (*) -1) 1

                Size ->
                    sortFn (.size >> (*) -1) 1

        ord =
            if asc then
                List.reverse

            else
                identity
    in
    sort >> ord


formatBytes : Int -> String
formatBytes b =
    let
        kb =
            toFloat b / 1024

        mb =
            kb / 1024

        gb =
            mb / 1024
    in
    if mb < 1 then
        Util.formatFloat kb ++ "k"

    else if gb < 1 then
        Util.formatFloat mb ++ "MB"

    else
        Util.formatFloat gb ++ "GB"
