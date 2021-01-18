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
import Set exposing (Set)
import Util exposing (Lang)


type alias Model =
    { session : Session, lang : Lang, sortCol : Col, sortAsc : Bool }


type Col
    = Name
    | NumHeaders
    | NumItems
    | Size
    | Lang


type Msg
    = SortClicked Col


init : Lang -> Session -> ( Model, Cmd Msg )
init lang session =
    ( { session = session, lang = lang, sortCol = NumHeaders, sortAsc = False }, Cmd.none )


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
        , text ". "
        , case model.session.index of
            RemoteData.Success index ->
                let
                    successdex =
                        index.list |> List.filterMap Result.toMaybe
                in
                span []
                    [ text "Parsed "
                    , text <| Util.formatInt <| List.length successdex
                    , text " of "
                    , text <| Util.formatInt <| List.length index.list
                    , text " .dat files."
                    ]

            _ ->
                span [] []
        ]
    , div [] <|
        case model.session.version of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success version ->
                [ text "Path of Exile version: "
                , text version
                , jsonLinks model "version.json"
                ]

            _ ->
                [ text "version loading..." ]
    , div [] <|
        case model.session.langs of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success langs ->
                langs
                    |> List.map (\lang -> viewLangLink model.lang (Just lang) lang)
                    |> (::) (viewLangLink model.lang Nothing "English")
                    |> List.intersperse (text ", ")
                    |> (::) (text "Available languages: ")

            _ ->
                []
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
                        , th [] [ button [ onClick <| SortClicked Lang ] [ text "Languages" ] ]
                        ]
                    , tbody []
                        (index.list
                            |> sortIndex model.lang model.sortCol model.sortAsc
                            |> List.map
                                (\mentry ->
                                    tr [] <|
                                        case mentry of
                                            Ok entry ->
                                                [ td [] [ a [ Route.href <| Route.Dat model.lang entry.filename ] [ text entry.filename ] ]
                                                , td [] [ text <| Util.formatInt entry.numHeaders ]
                                                , td [] [ text <| Util.formatInt entry.numItems ]
                                                , td [] [ text <| formatBytes entry.size ]
                                                , td [] <|
                                                    case model.lang of
                                                        Nothing ->
                                                            if Set.isEmpty entry.langs then
                                                                []

                                                            else
                                                                [ text <| Util.formatInt <| Set.size entry.langs, text " languages" ]

                                                        Just lang ->
                                                            if Set.member lang entry.langs then
                                                                [ text lang ]

                                                            else
                                                                []
                                                ]

                                            Err name ->
                                                [ td [] [ text name ]
                                                , td [ colspan 4 ] [ code [] [ text "error in PyPoE" ] ]
                                                ]
                                )
                        )
                    ]
                ]

            _ ->
                [ text ".dat list loading..." ]
    , div []
        [ p [] [ text "Other JSON data pypoe-json's constructed:" ]
        , ul []
            [ li [] [ text "index.json: .dat file metadata", jsonLinks model "index.json" ]
            , li [] [ text "list.json: .dat file list", jsonLinks model "list.json" ]
            , li [] [ text "lang.json: Path of Exile's supported languages", jsonLinks model "lang.json" ]
            , li [] [ text "version.json: Path of Exile version number", jsonLinks model "version.json" ]
            , li [] [ text "passive-skill-tree.json", jsonLinks model "passive-skill-tree.json" ]
            ]
        ]
    ]


jsonLinks : Model -> String -> Html msg
jsonLinks model file =
    span []
        [ text " ("
        , a [ target "_blank", href <| model.session.githubUrl ++ "/" ++ file ] [ text "github" ]
        , text ", "
        , a [ target "_blank", href <| model.session.dataUrl ++ "/" ++ file ] [ text "raw" ]
        , text ")"
        ]


viewLangLink : Lang -> Lang -> String -> Html msg
viewLangLink expected lang label =
    if expected == lang then
        b [] [ text label ]

    else
        a [ Route.href <| Route.Home lang ] [ text label ]


sortIndex : Lang -> Col -> Bool -> List (Result String Session.IndexEntry) -> List (Result String Session.IndexEntry)
sortIndex lang col asc =
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
                    sortFn .numHeaders -1 >> List.reverse

                NumItems ->
                    sortFn .numItems -1 >> List.reverse

                Size ->
                    sortFn .size -1 >> List.reverse

                Lang ->
                    case lang of
                        Nothing ->
                            sortFn (.langs >> Set.size) -1 >> List.reverse

                        Just l ->
                            List.sortBy
                                (\e ->
                                    case e of
                                        -- does this file differ in this language? then, alphabetically
                                        Ok entry ->
                                            ( Set.member l entry.langs |> sortableBool |> (*) -1, entry.filename )

                                        Err name ->
                                            ( 1, name )
                                )

        ord =
            if asc then
                List.reverse

            else
                identity
    in
    sort >> ord


sortableBool b =
    if b then
        1

    else
        0


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
