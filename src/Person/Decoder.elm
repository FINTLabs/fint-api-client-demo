module Person.Decoder exposing (..)

import Json.Decode as Decode exposing (Decoder, at, field)
import Person.Model exposing (..)


decodePersoner : Decode.Decoder (List Person)
decodePersoner =
    Decode.at [ "_embedded", "personList" ] (Decode.list decodePerson)


decodePerson : Decode.Decoder Person
decodePerson =
    Decode.map5 Person
        (at [ "foedselsnummer", "identifikatorverdi" ] Decode.string)
        (field "navn" <|
            Decode.map3 Navn
                (field "fornavn" Decode.string)
                (field "etternavn" Decode.string)
                (field "mellomnavn" Decode.string)
        )
        (field "kontaktinformasjon" decodeKontaktinformasjon)
        (field "postadresse" decodePostadresse)
        (at [ "_links" ] decodeLinks)


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


decodeLinks : Decode.Decoder Links
decodeLinks =
    Decode.map2 Links
        (at [ "self", "href" ] Decode.string)
        (at [ "personalressurs", "href" ] Decode.string)
