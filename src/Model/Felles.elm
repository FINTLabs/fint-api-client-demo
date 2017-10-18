module Model.Felles exposing (..)

import Date
import Json.Decode as JD exposing (Decoder, at, field, list, map, map2, map3, map4, map5, nullable, string, andThen, succeed, fail)
import Json.Decode.Pipeline as JP exposing (decode, optional, optionalAt, required, requiredAt)


-- BASISKLASSER: Aktør, Begrep, Enhet
-- ISO: Landkode, Kjønn, Språk
-- HOVEDKLASSER: Person


type alias Person =
    { foedselsnummer : Identifikator
    , navn : Personnavn
    , fodselsdato : Date.Date
    , kontatinformasjon : Kontaktinformasjon
    , postadresse : Adresse
    , links : Links
    }



-- KOMPLEKSE DATATYPER: Personnavn, Adresse, Periode, Kontaktinformasjon, Identifikator


type alias Identifikator =
    { identifikatorverdi : String
    , gyldighetsperiode : Maybe Periode
    }


type alias Periode =
    { start : String
    , slutt : String
    }


type alias Personnavn =
    { fornavn : String
    , etternavn : String
    , mellomnavn : String
    }


type alias Adresse =
    { adresse : String
    , postnummer : String
    , poststed : String
    , land : String
    }


type alias Kontaktinformasjon =
    { epostadresse :
        String

    -- telefonnummer?
    , mobiltelefonummer : Maybe String
    , nettsted : String
    }



-- MÅ FLYTTES


type alias Links =
    { self : List Href
    , personalressurs : List Href
    }


type alias Href =
    { href : String
    }


{-| Module to decode a list of persons.
-}
decodePersoner : Decoder (List Person)
decodePersoner =
    at [ "_embedded", "_entries" ] (list decodePerson)


decodePerson : Decoder Person
decodePerson =
    decode Person
        |> required "fodselsnummer" decodeIdentifikator
        |> required "navn" decodePersonnavn
        |> required "fodselsdato" decodeUnixTimestamp
        |> required "kontaktinformasjon" decodeKontaktinformasjon
        |> required "postadresse" decodeAdresse
        |> requiredAt [ "_links" ] decodeLinks


decodeUnixTimestamp : Decoder Date.Date
decodeUnixTimestamp =
    JD.float
        |> andThen
            (\f ->
                succeed <| Date.fromTime f
            )


decodeIdentifikator : Decoder Identifikator
decodeIdentifikator =
    decode Identifikator
        |> required "identifikatorverdi" string
        |> optional "gyldighetsperiode" (nullable decodePeriode) Nothing


decodePeriode : Decoder Periode
decodePeriode =
    decode Periode
        |> optional "start" string ""
        |> optional "slutt" string ""


decodePersonnavn : Decoder Personnavn
decodePersonnavn =
    decode Personnavn
        |> optional "fornavn" string ""
        |> optional "etternavn" string ""
        |> optional "mellomnavn" string ""


decodeAdresse : Decoder Adresse
decodeAdresse =
    decode Adresse
        |> optional "adresse" string ""
        |> optional "postnummer" string ""
        |> optional "poststed" string ""
        |> optionalAt [ "land", "kode" ] string ""


decodeKontaktinformasjon : Decoder Kontaktinformasjon
decodeKontaktinformasjon =
    decode Kontaktinformasjon
        |> optional "epostadresse" string ""
        |> optional "mobiltelefonummer" (nullable string) (Nothing)
        |> optional "nettsted" string ""


decodeLinks : Decoder Links
decodeLinks =
    decode Links
        |> required "self" (list decodeX)
        |> required "personalressurs" (list decodeX)


decodeX =
    decode Href
        |> optional "href" string ""
