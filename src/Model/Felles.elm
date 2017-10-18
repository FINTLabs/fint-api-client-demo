module Model.Felles exposing (..)

import Date
import Hateoas as Hateoas
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
    , links : Hateoas.Links
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
    { adresselinje : List String
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
        |> required "_links" Hateoas.decodeLinks


decodeUnixTimestamp : Decoder Date.Date
decodeUnixTimestamp =
    JD.string
        |> andThen
            (\f ->
                succeed <| (Date.fromString f |> Result.withDefault (Date.fromTime 0))
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
        |> optional "adresselinje" (list string) []
        |> optional "postnummer" string ""
        |> optional "poststed" string ""
        |> optionalAt [ "land", "kode" ] string ""


decodeKontaktinformasjon : Decoder Kontaktinformasjon
decodeKontaktinformasjon =
    decode Kontaktinformasjon
        |> optional "epostadresse" string ""
        |> optional "mobiltelefonummer" (nullable string) (Nothing)
        |> optional "nettsted" string ""
