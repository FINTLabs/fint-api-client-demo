module Module.Personal.Person exposing (..)

import Json.Decode as Decode exposing (Decoder, at, field)


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


{-| Module to decode a list of persons.

Includes an example on how underlying decoders can be organized. Alt. a, b & c.
-}
decodePersoner : Decode.Decoder (List Person)
decodePersoner =
    Decode.at [ "_embedded", "_entries" ] (Decode.list decodePerson)


decodePerson : Decode.Decoder Person
decodePerson =
    let
        -- alt a.
        decodeLinks =
            Decode.map2 Links
                (at [ "self", "href" ] Decode.string)
                (at [ "personalressurs", "href" ] Decode.string)
    in
        Decode.map5 Person
            (at [ "foedselsnummer", "identifikatorverdi" ] Decode.string)
            (field "navn" <|
                -- alt b.
                Decode.map3 Navn
                    (field "fornavn" Decode.string)
                    (field "etternavn" Decode.string)
                    (field "mellomnavn" Decode.string)
            )
            (field "kontaktinformasjon" decodeKontaktinformasjon)
            (field "postadresse" decodePostadresse)
            (at [ "_links" ] decodeLinks)



-- alt c.


decodeKontaktinformasjon : Decode.Decoder Kontaktinformasjon
decodeKontaktinformasjon =
    Decode.map4 Kontaktinformasjon
        (field "epostadresse" Decode.string)
        (field "telefonnummer" Decode.string)
        -- stavefeil i modell, skal være mobiltelefonnummer
        (field "mobiltelefonummer" Decode.string)
        (field "nettsted" Decode.string)


decodePostadresse : Decode.Decoder Postadresse
decodePostadresse =
    Decode.map4 Postadresse
        (field "adresse" Decode.string)
        (field "postnummer" Decode.string)
        (field "poststed" Decode.string)
        (at [ "land", "kode" ] Decode.string)
