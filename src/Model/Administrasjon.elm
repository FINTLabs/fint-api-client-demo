module Model.Administrasjon exposing (..)

import Hateoas as Hateoas
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
    { self : List Hateoas.Href
    , arbeidsforhold : List Hateoas.Href
    }


{-| Decode personalressurs
-}
decodePersonalressurs : Decoder Personalressurs
decodePersonalressurs =
    decode Personalressurs
        |> requiredAt [ "ansattnummer", "identifikatorverdi" ] string
        |> requiredAt [ "brukernavn", "identifikatorverdi" ] string
        |> requiredAt [ "brukernavn", "identifikatorverdi" ] string
        |> required "_links" decodeLinks


decodeLinks : Decoder Links
decodeLinks =
    decode Links
        |> required "self" (list Hateoas.decodeHrefs)
        |> required "arbeidsforhold" (list Hateoas.decodeHrefs)



--|> required "_links" Hateoas.decodeLinks


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
