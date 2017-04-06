module Module.Personal exposing (..)

import Module.Personal.Arbeidsforhold as Arbeidsforhold exposing (Arbeidsforhold)
import Module.Personal.Person as Person exposing (Person, Postadresse)
import Module.Personal.Personalressurs as Personalressurs exposing (Personalressurs)
import Html exposing (Html, button, div, img, li, text)
import Html.Events exposing (..)
import Http exposing (..)
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
import RemoteData exposing (RemoteData(NotAsked, Loading, Failure, Success), WebData)


-- MODEL


type alias Model =
    { mdl : Material.Model
    , personer : WebData (List Person)
    , personalressurs : WebData Personalressurs
    , arbeidsforhold : WebData Arbeidsforhold
    , selectedPerson : Maybe Person
    , soek : String
    }


model : Model
model =
    { mdl = Material.model
    , personer = Loading
    , personalressurs = NotAsked
    , arbeidsforhold = NotAsked
    , selectedPerson = Nothing
    , soek = ""
    }



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | GetPersoner
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

        GetPersoner ->
            ( model, getPersoner urlPersoner )

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



-- VIEW


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
            div []
                [ text ("Error: " ++ toString err)
                , Button.render Mdl
                    [ 1, 1 ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Options.attribute <| Html.Events.onClick GetPersoner
                    ]
                    [ text "Last inn på nytt" ]
                ]

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



-- CMD


urlPersoner : String
urlPersoner =
    "https://api.felleskomponent.no/mocks/administrasjon/personal/person"


getPersoner : String -> Cmd Msg
getPersoner url =
    Http.get url Person.decodePersoner
        |> RemoteData.sendRequest
        |> Cmd.map PersonsResponse


getPersonalressurs : String -> Cmd Msg
getPersonalressurs url =
    Http.get url Personalressurs.decodePersonalressurs
        |> RemoteData.sendRequest
        |> Cmd.map PersonalressursResponse


getArbeidsforhold : String -> Cmd Msg
getArbeidsforhold url =
    Http.get url Arbeidsforhold.decodeArbeidsforholder
        |> RemoteData.sendRequest
        |> Cmd.map ArbeidsforholdResponse
