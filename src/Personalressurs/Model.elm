module Personalressurs.Model exposing (..)


type alias Personalressurs =
    { ansattnummer : String
    , brukernavn : String
    , personalressurskategori : String
    , links : Links
    }


type alias Links =
    { self : String
    , arbeidsforhold : String
    }
