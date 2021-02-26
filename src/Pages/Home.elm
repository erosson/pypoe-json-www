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
        , a [ target "_blank", href <| model.session.dataUrl ++ "/pypoe/v1/tree/" ] [ text "Path of Exile JSON data" ]
        , text " with ease."
        ]
    , p []
        [ text "Data exported from "
        , a [ target "_blank", href "https://www.pathofexile.com" ] [ text "Path of Exile" ]
        , text " using "
        , a [ target "_blank", href "https://github.com/OmegaK2/PyPoE" ] [ text "PyPoE" ]
        , text " and "
        , a [ target "_blank", href "https://github.com/erosson/poedat" ] [ text "poedat" ]
        , text ". "
        ]
    , div [] <|
        case model.session.version of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success version ->
                [ text "Path of Exile version: "
                , text version
                , text " ("
                , jsonLinks model "/pypoe/v1/latest.json"
                , text ")"
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
                        ]
                    , tbody []
                        (index.list
                            |> sortIndex model.lang model.sortCol model.sortAsc
                            |> List.map
                                (\entry ->
                                    tr []
                                        [ td [] [ a [ Route.href <| Route.Dat model.lang entry.filename ] [ text entry.filename ] ]
                                        , td [] [ text <| Util.formatInt entry.numHeaders ]
                                        , td [] [ text <| Util.formatInt entry.numItems ]
                                        , td [] [ text <| formatBytes entry.size ]
                                        ]
                                )
                        )
                    ]
                ]

            _ ->
                [ text ".dat list loading..." ]
    , div []
        [ p [] [ text "Other JSON data poedat's constructed:" ]
        , ul []
            ([ li [] [ text "Path of Exile version number: ", jsonLinks model "/pypoe/v1/latest.json" ]
             , li [] [ text "Passive skill tree: ", jsonLinks model "/web/v1/passive-skill-tree.json" ]
             ]
                ++ (case model |> toSession |> .version of
                        RemoteData.Success version ->
                            [ li [] [ text "Path of Exile's supported languages: ", jsonLinks model <| "/pypoe/v1/tree/" ++ version ++ "/lang.json" ]
                            , li [] [ text ".dat file metadata: ", jsonLinks model <| "/pypoe/v1/tree/" ++ version ++ "/pypoe.json" ]
                            , li []
                                [ text "poedat directory index: "
                                , jsonLinks model <| "/pypoe/v1/tree/" ++ version ++ "/index.html"
                                , text ", "
                                , jsonLinks model <| "/pypoe/v1/tree/" ++ version ++ "/index.json"
                                ]
                            ]

                        _ ->
                            []
                   )
            )
        ]
    ]


jsonLinks : Model -> String -> Html msg
jsonLinks model file =
    let
        basename =
            file
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault file
    in
    a [ target "_blank", href <| model.session.dataUrl ++ file ] [ text basename ]


viewLangLink : Lang -> Lang -> String -> Html msg
viewLangLink expected lang label =
    if expected == lang then
        b [] [ text label ]

    else
        a [ Route.href <| Route.Home lang ] [ text label ]


sortIndex : Lang -> Col -> Bool -> List Session.IndexEntry -> List Session.IndexEntry
sortIndex lang col asc =
    let
        sortFn fn default =
            List.sortBy (Result.map fn >> Result.withDefault default)

        sort : List Session.IndexEntry -> List Session.IndexEntry
        sort =
            case col of
                Name ->
                    List.sortBy .filename

                NumHeaders ->
                    List.sortBy .numHeaders >> List.reverse

                NumItems ->
                    List.sortBy .numItems >> List.reverse

                Size ->
                    List.sortBy .size >> List.reverse

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
