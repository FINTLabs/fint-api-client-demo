module App exposing (..)

import Arbeidsforhold.Decoder exposing (decodeArbeidsforholder)
import Arbeidsforhold.Model exposing (..)
import Html exposing (Html, button, div, img, li, text)
import Html.Events exposing (..)
import Http exposing (..)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid exposing (..)
import Material.Layout as Layout exposing (selectedTab)
import Material.List as Lists
import Material.Options as Options exposing (css)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Person.Decoder as PersonDecoder exposing (decodePerson)
import Person.Model exposing (..)
import Personalressurs.Decoder exposing (decodePersonalressurs)
import Personalressurs.Model exposing (..)
import RemoteData exposing (RemoteData(Failure), RemoteData(Loading), RemoteData(NotAsked), RemoteData(Success), WebData)


type alias Model =
    { mdl : Material.Model
    , selectedTab : Int
    , personer : WebData (List Person)
    , personalressurs : WebData Personalressurs
    , arbeidsforhold : WebData Arbeidsforhold
    , selectedPerson : Maybe Person
    , soek : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { mdl = Material.model
      , selectedTab = 0
      , personer = Loading
      , personalressurs = NotAsked
      , arbeidsforhold = NotAsked
      , selectedPerson = Nothing
      , soek = ""
      }
    , getPersoner urlPersoner
    )


urlPersoner : String
urlPersoner =
    "https://api.felleskomponent.no/mocks/administrasjon/personal/person"


{-| Url til iso-kodeverk
    - 5218 = Kjønn
    - 31661alpha2 = Land
    - ...
-}
urlKodeverkIso : String -> String
urlKodeverkIso iso =
    "https://api.felleskomponent.no/felles/kodeverk/iso/" ++ iso


getPersoner : String -> Cmd Msg
getPersoner url =
    Http.get url PersonDecoder.decodePersoner
        |> RemoteData.sendRequest
        |> Cmd.map PersonsResponse


getPersonalressurs : String -> Cmd Msg
getPersonalressurs url =
    Http.get url decodePersonalressurs
        |> RemoteData.sendRequest
        |> Cmd.map PersonalressursResponse


getArbeidsforhold : String -> Cmd Msg
getArbeidsforhold url =
    Http.get url decodeArbeidsforholder
        |> RemoteData.sendRequest
        |> Cmd.map ArbeidsforholdResponse


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Int
    | GetPersonalressurs String
    | GetArbeidsforhold String
    | PersonsResponse (WebData (List Person))
    | PersonalressursResponse (WebData Personalressurs)
    | ArbeidsforholdResponse (WebData Arbeidsforhold)
    | VelgPerson Person
    | StartSok String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl m ->
            Material.update Mdl m model

        SelectTab nr ->
            { model | selectedTab = nr } ! []

        GetPersonalressurs s ->
            ( { model | personalressurs = Loading, arbeidsforhold = NotAsked }, getPersonalressurs s )

        GetArbeidsforhold s ->
            ( { model | arbeidsforhold = Loading }, getArbeidsforhold s )

        PersonsResponse response ->
            ( { model | personer = response }, Cmd.none )

        PersonalressursResponse response ->
            ( { model | personalressurs = response }, Cmd.none )

        ArbeidsforholdResponse response ->
            ( { model | arbeidsforhold = response }, Cmd.none )

        VelgPerson p ->
            ( { model | selectedPerson = Just p, personalressurs = NotAsked, arbeidsforhold = NotAsked }, Cmd.none )

        StartSok t ->
            ( { model | soek = t }, Cmd.none )


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.selectedTab model.selectedTab
        , Layout.onSelectTab SelectTab
        ]
        { header = [ Layout.row [] [ Layout.title [] [ text "FINT klienteksempel" ] ] ]
        , drawer = []
        , tabs =
            ( [ text "Personal"
              , text "Kodeverk"
              ]
            , []
            )
        , main =
            [ case model.selectedTab of
                0 ->
                    viewPersonal model

                1 ->
                    text "her kommer uttrekk av iso-kodeverk"

                _ ->
                    text "404"
            ]
        }


viewPersonal : Model -> Html Msg
viewPersonal model =
    div []
        [ Textfield.render Mdl
            [ 1, 0 ]
            model.mdl
            [ Options.onInput StartSok
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
                , viewArbeidsforhold model
                ]
            ]
        , text <| "Tab: " ++ toString model.selectedTab
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
                        [ 1, 1 ]
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
                        , Options.attribute <| Html.Events.onClick (GetArbeidsforhold pr.links.arbeidsforhold)
                        ]
                        [ text "Vis arbeidsforhold" ]
                    ]
                ]


viewArbeidsforhold : Model -> Html Msg
viewArbeidsforhold model =
    case model.arbeidsforhold of
        NotAsked ->
            text ""

        Loading ->
            div [] [ text "Henter data...", Loading.indeterminate ]

        Failure err ->
            text ("Error: " ++ toString err)

        Success a ->
            Card.view [ Elevation.e2, Color.background (Color.color Color.Grey Color.S300) ]
                [ Card.title []
                    [ Card.head []
                        [ text <| "Arbeidsforhold"
                        ]
                    ]
                , Card.text []
                    [ Html.ul []
                        [ Html.li [] [ text <| "Stillingsnummer: " ++ a.arbeidsforholdsnummer ]
                        ]
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
                    ++ " "
                    ++ person.navn.mellomnavn
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
