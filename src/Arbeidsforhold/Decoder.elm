module Arbeidsforhold.Decoder exposing (..)

import Arbeidsforhold.Model exposing (..)
import Json.Decode as Decode exposing (..)


decodeArbeidsforholder : Decode.Decoder Arbeidsforhold
decodeArbeidsforholder =
    Decode.map2 Arbeidsforhold
        (field "stillingsnummer" Decode.string)
        (field "stillingskode" decodeStillingskode)


decodeStillingskode : Decode.Decoder Stillingskode
decodeStillingskode =
    Decode.map3 Stillingskode
        (field "kode" Decode.string)
        (field "navn" Decode.string)
        (field "ksKode" Decode.string)
