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


type alias Model =
    { session : Session }


type Msg
    = Noop


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


toSession : Model -> Session
toSession m =
    m.session


updateSession : Session -> Model -> Model
updateSession s m =
    { m | session = s }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


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
        case model.session.indexDat of
            RemoteData.Failure err ->
                [ code [] [ text err ] ]

            RemoteData.Success indexDat ->
                [ indexDat
                    |> List.map (\dat -> li [] [ a [ Route.href <| Route.Dat dat ] [ text dat ] ])
                    |> ul []
                ]

            _ ->
                [ text ".dat list loading..." ]
    ]
