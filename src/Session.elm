module Session exposing (Session, init)

import Browser.Navigation as Nav
import RemoteData exposing (RemoteData)


type alias Session =
    -- Nav.Key cannot be unit tested; Maybe Nav.Key is a workaround.
    -- See https://github.com/elm-explorations/test/issues/24
    { version : RemoteData String String
    , indexDat : RemoteData String (List String)
    , nav : Maybe Nav.Key
    }


init : Maybe Nav.Key -> Session
init =
    Session RemoteData.Loading RemoteData.Loading
