module Module.Personal.Arbeidsforhold exposing (..)

import Json.Decode as Decode exposing (..)


type alias Arbeidsforhold =
    { arbeidsforholdsnummer : String
    , stillingskode : Stillingskode
    }


type alias Stillingskode =
    { kode : String
    , navn : String
    , ksKode : String
    }


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
