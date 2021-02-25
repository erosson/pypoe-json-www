module Pages.DatId exposing (Model, Msg(..), init, subscriptions, toSession, update, updateSession, view)

import Browser
import Dat exposing (Dat, Header)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as D
import Json.Encode as JE
import Pages.Dat exposing (viewKeyVal, viewVal)
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Session)
import Util exposing (Lang)


type alias Model =
    { session : Session, lang : Lang, file : String, id : Int, content : RemoteData String Dat, row : Maybe Dat.Entry }


type Msg
    = FetchedDat D.Value


init : Lang -> String -> Int -> Session -> ( Model, Cmd Msg )
init lang file id session =
    ( { session = session
      , lang = lang
      , file = file
      , id = id
      , content = RemoteData.Loading
      , row = Nothing
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
                    ( { model
                        | content = RemoteData.Success content
                        , row = content.data |> List.drop model.id |> List.head
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | content = RemoteData.Failure <| D.errorToString err }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fetchedDat FetchedDat
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "pypoe-json > " ++ model.file ++ " > " ++ String.fromInt model.id
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ h1 []
        [ a [ Route.href Route.home ] [ text "pypoe-json" ]
        , text " > "
        , a [ Route.href <| Route.Dat model.lang model.file ] [ text model.file ]
        , text " > #"
        , text <| String.fromInt model.id
        ]
    , h4 []
        [ a [ target "_blank", href <| model.session.dataUrl ++ Session.fileLangPath model.lang model.file model.session ] [ text model.file ]
        , span [] <|
            case model.content of
                RemoteData.Success dat ->
                    [ text <| " (" ++ (String.fromInt <| List.length dat.data) ++ " rows)" ]

                _ ->
                    []
        ]
    , div [] <|
        case ( model.content, model.row ) of
            ( RemoteData.Failure err, _ ) ->
                [ code [] [ text err ] ]

            ( RemoteData.Success dat, Nothing ) ->
                [ code [] [ text "no such id: #", text <| String.fromInt model.id ] ]

            ( RemoteData.Success dat, Just row ) ->
                [ table []
                    [ tbody []
                        (tr []
                            [ th [] [ text "#" ]
                            , td [] [ text <| String.fromInt model.id ]
                            ]
                            :: List.map2
                                (\h val ->
                                    tr []
                                        [ th []
                                            [ case h.key of
                                                Nothing ->
                                                    text h.name

                                                Just key ->
                                                    a [ Route.href <| Route.Dat model.lang key ] [ text h.name ]
                                            ]
                                        , td [] [ viewKeyVal model.lang h val ]
                                        ]
                                )
                                dat.headers
                                row.vals
                        )
                    ]
                , details [ class "json-source" ]
                    [ summary [] [ text "JSON" ]
                    , div []
                        [ h4 []
                            [ text "List format ("
                            , a [ target "_blank", href <| model.session.dataUrl ++ Session.fileLangPath model.lang model.file model.session ] [ text "source" ]
                            , text ")"
                            ]
                        , pre [] [ text <| JE.encode 2 <| Dat.entryEncoder row ]
                        ]
                    , div []
                        [ h4 [] [ text "Object format" ]
                        , pre [] [ text <| JE.encode 2 <| Dat.objEncoder dat.headers row ]
                        ]
                    ]
                ]

            _ ->
                [ code [] [ text "loading..." ] ]
    ]
