module Arbeidsforhold.Decoder exposing (..)

import Arbeidsforhold.Model exposing (..)
import Json.Decode as Decode exposing (..)


{-| Decode a single arbeidsforhold
-}
decodeArbeidsforholder : Decode.Decoder Arbeidsforhold
decodeArbeidsforholder =
    let
        decodeStillingskode =
            map3 Stillingskode
                (field "kode" string)
                (field "navn" string)
                (field "ksKode" string)
    in
        Decode.map2 Arbeidsforhold
            (field "stillingsnummer" Decode.string)
            (field "stillingskode" decodeStillingskode)
