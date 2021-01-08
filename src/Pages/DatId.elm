module Pages.DatId exposing (Model, Msg(..), init, subscriptions, toSession, update, updateSession, view)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as D
import Pages.Dat exposing (Dat, DatValue(..), Header, decoder, viewKeyVal, viewVal)
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Session)


type alias Model =
    { session : Session, file : String, id : Int, content : RemoteData String Dat, row : Maybe (List DatValue) }


type Msg
    = FetchedDat D.Value


init : String -> Int -> Session -> ( Model, Cmd Msg )
init file id session =
    ( { session = session
      , file = file
      , id = id
      , content = RemoteData.Loading
      , row = Nothing
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


view : Model -> List (Html Msg)
view model =
    [ h1 []
        [ a [ Route.href Route.Home ] [ text "pypoe-json" ]
        , text " > "
        , a [ Route.href <| Route.Dat model.file ] [ text model.file ]
        , text " > #"
        , text <| String.fromInt model.id
        ]
    , h4 []
        [ text <| model.file ++ ".json: "
        , a [ target "_blank", href <| "https://github.com/erosson/pypoe-json/tree/master/dist/dat/" ++ model.file ++ ".json" ] [ text "github" ]
        , text ", "
        , a [ target "_blank", href <| "https://erosson.github.io/pypoe-json/dist/dat/" ++ model.file ++ ".json" ] [ text "raw" ]
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
                                                    a [ Route.href <| Route.Dat key ] [ text h.name ]
                                            ]
                                        , td [] [ viewKeyVal h val ]
                                        ]
                                )
                                dat.headers
                                row
                        )
                    ]
                ]

            _ ->
                [ code [] [ text "loading..." ] ]
    ]
