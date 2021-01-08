module Route exposing (Route(..), href, parse, pushUrl, replaceUrl, toString)

import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser)


type Route
    = Home
    | Dat String
    | DatId String Int
    | Debug


parse : Url -> Maybe Route
parse =
    P.parse parser


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map Home P.top
        , P.map Dat <| P.s "dat" </> P.string
        , P.map DatId <| P.s "dat" </> P.string </> P.s "row" </> P.int
        , P.map Debug <| P.s "debug"
        ]


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Dat f ->
            "/dat/" ++ f

        DatId f id ->
            "/dat/" ++ f ++ "/row/" ++ String.fromInt id

        Debug ->
            "/debug"


href : Route -> H.Attribute msg
href =
    toString >> A.href


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl nav =
    toString >> Nav.pushUrl nav


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl nav =
    toString >> Nav.replaceUrl nav
