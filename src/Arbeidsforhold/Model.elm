module Arbeidsforhold.Model exposing (..)


type alias Arbeidsforhold =
    { arbeidsforholdsnummer : String
    , stillingskode : Stillingskode
    }


type alias Stillingskode =
    { kode : String
    , navn : String
    , ksKode : String
    }
