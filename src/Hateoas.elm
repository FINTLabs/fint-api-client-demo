module Hateoas exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


-- MODEL


type alias Links =
    { self : List Href
    , personalressurs : List Href
    }


type alias Href =
    { href : String
    }



-- HELPERS


headHref : List { href : String } -> String
headHref listOfHref =
    listOfHref
        |> List.head
        |> Maybe.withDefault { href = "" }
        |> .href



-- DECODERS


decodeLinks : Decoder Links
decodeLinks =
    decode Links
        |> required "self" (list decodeHrefs)
        |> required "personalressurs" (list decodeHrefs)


decodeHrefs =
    decode Href
        |> optional "href" string ""
