module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as D
import Pages.Dat
import Pages.DatId
import Pages.Debug
import Pages.Home
import Pages.NotFound
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



---- MODEL ----


type Model
    = Home Pages.Home.Model
    | Dat Pages.Dat.Model
    | DatId Pages.DatId.Model
    | Debug Pages.Debug.Model
    | NotFound Session


type alias Flags =
    {}


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url nav =
    let
        session =
            Session.init (Just nav)
    in
    routeTo (Route.parse url) (NotFound session)


toSession : Model -> Session
toSession model =
    case model of
        NotFound session ->
            session

        Home pgmodel ->
            Pages.Home.toSession pgmodel

        Dat pgmodel ->
            Pages.Dat.toSession pgmodel

        DatId pgmodel ->
            Pages.DatId.toSession pgmodel

        Debug pgmodel ->
            Pages.Debug.toSession pgmodel


updateSession : Session -> Model -> Model
updateSession session model =
    case model of
        NotFound _ ->
            NotFound session

        Home pgmodel ->
            Pages.Home.updateSession session pgmodel |> Home

        Dat pgmodel ->
            Pages.Dat.updateSession session pgmodel |> Dat

        DatId pgmodel ->
            Pages.DatId.updateSession session pgmodel |> DatId

        Debug pgmodel ->
            Pages.Debug.updateSession session pgmodel |> Debug


routeTo : Maybe Route -> Model -> ( Model, Cmd Msg )
routeTo mroute =
    toSession
        >> (\session ->
                case mroute of
                    Nothing ->
                        ( NotFound session, Cmd.none )

                    Just Route.Home ->
                        Pages.Home.init session |> Tuple.mapBoth Home (Cmd.map HomeMsg)

                    Just (Route.Dat f) ->
                        Pages.Dat.init f session |> Tuple.mapBoth Dat (Cmd.map DatMsg)

                    Just (Route.DatId f id) ->
                        Pages.DatId.init f id session |> Tuple.mapBoth DatId (Cmd.map DatIdMsg)

                    Just Route.Debug ->
                        Pages.Debug.init session |> Tuple.mapBoth Debug (Cmd.map DebugMsg)
           )



---- UPDATE ----


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | FetchedVersion D.Value
    | FetchedIndexDat D.Value
    | HomeMsg Pages.Home.Msg
    | DatMsg Pages.Dat.Msg
    | DatIdMsg Pages.DatId.Msg
    | DebugMsg Pages.Debug.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange url ->
            routeTo (Route.parse url) model

        OnUrlRequest (Browser.Internal url) ->
            case (toSession model).nav of
                Nothing ->
                    -- This should only happen in unit tests! See the note about Nav.Key in Session.init
                    ( model, Cmd.none )

                Just nav ->
                    ( model, url |> Url.toString |> Nav.pushUrl nav )

        OnUrlRequest (Browser.External urlstr) ->
            ( model, Nav.load urlstr )

        FetchedVersion json ->
            case D.decodeValue (D.field "data" <| D.field "version" D.string) json of
                Ok version ->
                    ( updateSession (model |> toSession |> (\s -> { s | version = RemoteData.Success version })) model, Cmd.none )

                Err err ->
                    ( updateSession (model |> toSession |> (\s -> { s | version = RemoteData.Failure <| D.errorToString err })) model, Cmd.none )

        FetchedIndexDat json ->
            case D.decodeValue (D.field "data" <| D.list D.string) json of
                Ok indexDat ->
                    ( updateSession (model |> toSession |> (\s -> { s | indexDat = RemoteData.Success indexDat })) model, Cmd.none )

                Err err ->
                    ( updateSession (model |> toSession |> (\s -> { s | version = RemoteData.Failure <| D.errorToString err })) model, Cmd.none )

        HomeMsg pgmsg ->
            case model of
                Home pgmodel ->
                    Pages.Home.update pgmsg pgmodel |> Tuple.mapBoth Home (Cmd.map HomeMsg)

                _ ->
                    ( model, Cmd.none )

        DatMsg pgmsg ->
            case model of
                Dat pgmodel ->
                    Pages.Dat.update pgmsg pgmodel |> Tuple.mapBoth Dat (Cmd.map DatMsg)

                _ ->
                    ( model, Cmd.none )

        DatIdMsg pgmsg ->
            case model of
                DatId pgmodel ->
                    Pages.DatId.update pgmsg pgmodel |> Tuple.mapBoth DatId (Cmd.map DatIdMsg)

                _ ->
                    ( model, Cmd.none )

        DebugMsg pgmsg ->
            case model of
                Debug pgmodel ->
                    Pages.Debug.update pgmsg pgmodel |> Tuple.mapBoth Debug (Cmd.map DebugMsg)

                _ ->
                    ( model, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fetchedVersion FetchedVersion
        , Ports.fetchedIndexDat FetchedIndexDat
        , case model of
            NotFound session ->
                Sub.none

            Home pgmodel ->
                Pages.Home.subscriptions pgmodel |> Sub.map HomeMsg

            Dat pgmodel ->
                Pages.Dat.subscriptions pgmodel |> Sub.map DatMsg

            DatId pgmodel ->
                Pages.DatId.subscriptions pgmodel |> Sub.map DatIdMsg

            Debug pgmodel ->
                Pages.Debug.subscriptions pgmodel |> Sub.map DebugMsg
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "pypoe-json"
    , body =
        case model of
            NotFound session ->
                Pages.NotFound.view session

            Home pgmodel ->
                Pages.Home.view pgmodel |> List.map (H.map HomeMsg)

            Dat pgmodel ->
                Pages.Dat.view pgmodel |> List.map (H.map DatMsg)

            DatId pgmodel ->
                Pages.DatId.view pgmodel |> List.map (H.map DatIdMsg)

            Debug pgmodel ->
                Pages.Debug.view pgmodel |> List.map (H.map DebugMsg)
    }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
