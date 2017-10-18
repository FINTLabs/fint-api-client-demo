module Model.Administrasjon exposing (..)

import Json.Decode as Decode exposing (Decoder, at, field, string, nullable, list, map, map2, map3, map4, map5)
import Json.Decode.Pipeline exposing (decode, required, optional, requiredAt, optionalAt)


type alias Personalressurs =
    { ansattnummer : String
    , brukernavn : String
    , personalressurskategori : String
    , links : Links
    }


type alias Arbeidsforhold =
    { arbeidsforholdsnummer : String
    , stillingskode : Stillingskode
    }


type alias Stillingskode =
    { kode : String
    , navn : String
    , ksKode : String
    }


type alias Links =
    { self : String
    , arbeidsforhold : String
    }


{-| Decode personalressurs
-}
decodePersonalressurs : Decode.Decoder Personalressurs
decodePersonalressurs =
    Decode.map4 Personalressurs
        (at [ "ansattnummer", "identifikatorverdi" ] Decode.string)
        (at [ "brukernavn", "identifikatorverdi" ] Decode.string)
        (at [ "personalressurskategori", "navn" ] Decode.string)
        (field "_links" decodeLinks)


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


decodeLinks : Decode.Decoder Links
decodeLinks =
    decode Links
        |> requiredAt [ "self", "href" ] string
        |> requiredAt [ "arbeidsforhold", "href" ] string
