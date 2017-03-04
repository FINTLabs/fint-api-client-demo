module Person.Model exposing (..)


type alias Person =
    { foedselsnummer : String
    , navn : Navn
    , kontatinformasjon : Kontaktinformasjon
    , postadresse : Postadresse
    , links : Links
    }


type alias Navn =
    { fornavn : String
    , etternavn : String
    , mellomnavn : String
    }


type alias Kontaktinformasjon =
    { epostadresse : String
    , telefonnummer : String
    , mobiltelefonnummer : String
    , nettsted : String
    }


type alias Postadresse =
    { adresse : String
    , postnummer : String
    , poststed : String
    , land : String
    }


type alias Links =
    { self : String
    , personalressurs : String
    }
