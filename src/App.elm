module App exposing (..)

import Html exposing (Html, button, div, img, li, text)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, at, field)
import List exposing (..)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid exposing (..)
import Material.List as Lists
import Material.Options as Options exposing (css)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Person.Decoder as PersonDecoder exposing (decodePerson)
import Person.Model exposing (..)
import RemoteData exposing (RemoteData(Failure), RemoteData(Loading), RemoteData(NotAsked), RemoteData(Success), WebData)


type alias Model =
    { mdl : Material.Model
    , personer : WebData (List Person)
    , personalressurs : WebData Personalressurs
    , selectedPerson : Maybe Person
    , soek : String
    }


type alias Personalressurs =
    { ansattnummer : String
    , brukernavn : String
    , personalressurskategori : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { mdl = Material.model
      , personer = Loading
      , personalressurs = NotAsked
      , selectedPerson = Nothing
      , soek = ""
      }
    , getPersoner
    )


getPersoner : Cmd Msg
getPersoner =
    Http.get "https://api.felleskomponent.no/mocks/administrasjon/personal/person" PersonDecoder.decodePersoner
        |> RemoteData.sendRequest
        |> Cmd.map PersonsResponse


getPersonalressurs : String -> Cmd Msg
getPersonalressurs url =
    Http.get url decodePersonalressurs
        |> RemoteData.sendRequest
        |> Cmd.map PersonalressursResponse


decodePersonalressurs : Decode.Decoder Personalressurs
decodePersonalressurs =
    Decode.map3 Personalressurs
        (at [ "ansattnummer", "identifikatorverdi" ] Decode.string)
        (at [ "brukernavn", "identifikatorverdi" ] Decode.string)
        (at [ "personalressurskategori", "navn" ] Decode.string)


type Msg
    = Mdl (Material.Msg Msg)
    | GetPersonalressurs String
    | PersonsResponse (WebData (List Person))
    | PersonalressursResponse (WebData Personalressurs)
    | VelgPerson Person
    | Upd0 String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl m ->
            Material.update Mdl m model

        GetPersonalressurs s ->
            ( { model | personalressurs = Loading }, getPersonalressurs s )

        PersonsResponse response ->
            ( { model | personer = response }, Cmd.none )

        PersonalressursResponse response ->
            ( { model | personalressurs = response }, Cmd.none )

        VelgPerson p ->
            ( { model | selectedPerson = Just p, personalressurs = NotAsked }, Cmd.none )

        Upd0 t ->
            ( { model | soek = t }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Textfield.render Mdl
            [ 1 ]
            model.mdl
            [ Options.onInput Upd0
            , Textfield.label "Søk på fødselsnummer..."
            , Textfield.floatingLabel
            , Textfield.expandable "id-of-expandable-1"
            , Textfield.expandableIcon "search"
            ]
            []
        , grid []
            [ cell [ size All 6 ] [ viewPersoner model ]
            , cell [ size All 6 ]
                [ visEnPerson model
                , viewPersonalressurs model
                ]
            ]
        ]


visEnPerson : Model -> Html Msg
visEnPerson model =
    case model.selectedPerson of
        Nothing ->
            div [] [ text "Velg en person i lista til venstre..." ]

        Just p ->
            Card.view [ Elevation.e2, Color.background (Color.color Color.Grey Color.S300) ]
                [ Card.title []
                    [ Card.head []
                        [ text <| p.navn.fornavn ++ " " ++ p.navn.mellomnavn ++ " " ++ p.navn.etternavn
                        ]
                    ]
                , Card.text []
                    [ viewPostadresse p.postadresse
                    ]
                , Card.actions [ Card.border ]
                    [ Button.render Mdl
                        [ 1, 0 ]
                        model.mdl
                        [ Button.ripple
                        , Button.accent
                        , Options.attribute <| Html.Events.onClick (GetPersonalressurs p.links.personalressurs)
                        ]
                        [ text "Vis Personalressurs" ]
                    ]
                ]


viewPostadresse : Postadresse -> Html Msg
viewPostadresse postadresse =
    Html.p []
        [ text postadresse.adresse
        , Html.br []
            []
        , text
            (postadresse.postnummer
                ++ " "
                ++ postadresse.poststed
            )
        ]


viewPersoner : Model -> Html Msg
viewPersoner model =
    case model.personer of
        NotAsked ->
            div [] [ text "Ikke spurt etter data..." ]

        Loading ->
            div [] [ text "Henter data...", Loading.indeterminate ]

        Failure err ->
            if ((toString err) == "NetworkError") then
                text ("FINT-API støtter ikke CORS, så du må aktivere dette i nettleseren. Feks ved å installere dette plugin: https://chrome.google.com/webstore/detail/allow-control-allow-origi/nlfbmbojpeacfghkpbjhddihlkkiljbi")
            else
                text ("Error: " ++ toString err)

        Success personer ->
            Lists.ul [] <|
                List.map viewPerson
                    (personer
                        |> List.filter (String.contains model.soek << .foedselsnummer)
                    )


viewPersonalressurs : Model -> Html Msg
viewPersonalressurs model =
    case model.personalressurs of
        NotAsked ->
            text ""

        Loading ->
            div [] [ text "Henter data...", Loading.indeterminate ]

        Failure err ->
            text ("Error: " ++ toString err)

        Success pr ->
            Card.view [ Elevation.e2, Color.background (Color.color Color.Grey Color.S300) ]
                [ Card.title []
                    [ Card.head []
                        [ text <| "Personalressurs"
                        ]
                    ]
                , Card.text []
                    [ Html.ul []
                        [ Html.li [] [ text <| "Ansattnummer: " ++ pr.ansattnummer ]
                        , Html.li [] [ text <| "Brukernavn: " ++ pr.brukernavn ]
                        , Html.li [] [ text <| "Personalressurskategori: " ++ pr.personalressurskategori ]
                        ]
                    ]
                , Card.actions [ Card.border ]
                    [ Button.render Mdl
                        [ 1, 0 ]
                        model.mdl
                        [ Button.ripple
                        , Button.accent
                          --, Options.attribute <| Html.Events.onClick (GetPersonalressurs p.links.personalressurs)
                        ]
                        [ text "Vis arbeidsforhold (todo ;)" ]
                    ]
                ]


viewPerson : Person -> Html Msg
viewPerson person =
    Lists.li [ Lists.withBody ]
        [ Lists.content
            [ css "cursor" "pointer"
            , Options.attribute <| Html.Events.onClick (VelgPerson person)
            ]
            [ Lists.avatarIcon "inbox" []
            , text
                (person.navn.etternavn
                    ++ ", "
                    ++ person.navn.fornavn
                )
            , Lists.body []
                [ text
                    ("Adresse: "
                        ++ person.postadresse.adresse
                        ++ ", "
                        ++ person.postadresse.postnummer
                        ++ " "
                        ++ person.postadresse.poststed
                    )
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
