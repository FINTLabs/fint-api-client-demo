module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (map, s, top)
import Html as Html
import Html.Attributes as Attribute


type Page
    = Index
    | Personal
    | Kodeverk
    | NotFound


parseLocation : Navigation.Location -> Page
parseLocation location =
    case (UrlParser.parseHash routeMatchers location) of
        Nothing ->
            NotFound

        Just route ->
            route


routeMatchers : UrlParser.Parser (Page -> a) a
routeMatchers =
    UrlParser.oneOf
        [ map Index UrlParser.top
        , map Personal (s "personal")
        , map Kodeverk (s "kodeverk")
        ]


getUrl : Page -> String
getUrl page =
    case page of
        Index ->
            "#"

        Personal ->
            "#personal"

        Kodeverk ->
            "#kodeverk"

        NotFound ->
            "#404"


href : Page -> Html.Attribute a
href page =
    Attribute.href <| getUrl page
