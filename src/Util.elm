module Util exposing (..)

import FormatNumber
import FormatNumber.Locales


type alias Lang =
    Maybe String


formatInt =
    let
        l =
            FormatNumber.Locales.usLocale
    in
    toFloat >> FormatNumber.format { l | decimals = FormatNumber.Locales.Exact 0 }


formatFloat =
    FormatNumber.format FormatNumber.Locales.usLocale
