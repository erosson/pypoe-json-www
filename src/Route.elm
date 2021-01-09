module Route exposing
    ( Route(..)
    , home
    , href
    , parse
    , pushUrl
    , replaceUrl
    , toString
    )

import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser)


type Route
    = Home Lang
    | Dat Lang String
    | DatId Lang String Int
    | Debug


home =
    Home Nothing


type alias Lang =
    Maybe String


parse : Url -> Maybe Route
parse =
    P.parse parser


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map (Home Nothing) P.top
        , P.map Home <| langParser
        , P.map (Dat Nothing) <| P.s "dat" </> P.string
        , P.map Dat <| langParser </> P.s "dat" </> P.string
        , P.map (DatId Nothing) <| P.s "dat" </> P.string </> P.s "row" </> P.int
        , P.map DatId <| langParser </> P.s "dat" </> P.string </> P.s "row" </> P.int
        , P.map Debug <| P.s "debug"
        ]


langParser =
    P.s "lang" </> (P.string |> P.map Just)


toString : Route -> String
toString route =
    case route of
        Home Nothing ->
            "/"

        Home (Just lang) ->
            langToString (Just lang)

        Dat lang f ->
            langToString lang ++ "/dat/" ++ f

        DatId lang f id ->
            langToString lang ++ "/dat/" ++ f ++ "/row/" ++ String.fromInt id

        Debug ->
            "/debug"


langToString lang =
    case lang of
        Nothing ->
            ""

        Just l ->
            "/lang/" ++ l


href : Route -> H.Attribute msg
href =
    toString >> A.href


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl nav =
    toString >> Nav.pushUrl nav


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl nav =
    toString >> Nav.replaceUrl nav
