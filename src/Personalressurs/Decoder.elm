module Personalressurs.Decoder exposing (..)

import Json.Decode as Decode exposing (at, field)
import Personalressurs.Model exposing (..)


{-| Decode personalressurs
-}
decodePersonalressurs : Decode.Decoder Personalressurs
decodePersonalressurs =
    Decode.map4 Personalressurs
        (at [ "ansattnummer", "identifikatorverdi" ] Decode.string)
        (at [ "brukernavn", "identifikatorverdi" ] Decode.string)
        (at [ "personalressurskategori", "navn" ] Decode.string)
        (field "_links" decodeLinks)


decodeLinks : Decode.Decoder Links
decodeLinks =
    Decode.map2 Links
        (at [ "self", "href" ] Decode.string)
        (at [ "arbeidsforhold", "href" ] Decode.string)
