module Module.Personal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Progress as Progress
import Html exposing (..)
import Html.Attributes as Attr exposing (class, href)
import Html.Events as Events exposing (..)
import Http exposing (..)
import Material
import Material.Card as Card
import Material.Elevation as Elevation
import Module.Personal.Arbeidsforhold as Arbeidsforhold exposing (Arbeidsforhold)
import Module.Personal.Person as Person exposing (Person, Postadresse)
import Module.Personal.Personalressurs as Personalressurs exposing (Personalressurs)
import RemoteData exposing (RemoteData(Failure), RemoteData(Loading), RemoteData(NotAsked, Success), WebData)


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

        PersonsResponse response ->
            ( { model | personer = response }, Cmd.none )

        GetPersonalressurs s ->
            ( { model | personalressurs = Loading, arbeidsforhold = NotAsked }
            , getPersonalressurs s
            )

        PersonalressursResponse response ->
            ( { model | personalressurs = response }, Cmd.none )

        GetArbeidsforhold s ->
            ( { model | arbeidsforhold = Loading }, getArbeidsforhold s )

        ArbeidsforholdResponse response ->
            ( { model | arbeidsforhold = response }, Cmd.none )

        VelgPerson person ->
            ( { model
                | selectedPerson = Just person
                , personalressurs = NotAsked
                , arbeidsforhold = NotAsked
              }
            , Cmd.none
            )

        StartSok t ->
            ( { model | soek = t }, Cmd.none )



-- VIEW


viewPersonal : Model -> Html Msg
viewPersonal model =
    div []
        [ Grid.row []
            [ Grid.col [ Col.lg4 ]
                [ --Options.onInput StartSok
                  Input.search
                    [ Input.onInput StartSok
                    , Input.placeholder "Søk på fødselsnummer..."
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col [] [ viewPersoner model ]
            , Grid.col []
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
            Card.view [ Elevation.e2 ]
                [ Card.title []
                    [ Card.head []
                        [ text <| p.navn.fornavn ++ " " ++ p.navn.mellomnavn ++ " " ++ p.navn.etternavn
                        ]
                    ]
                , Card.text []
                    [ viewPostadresse p.postadresse
                    ]
                , Card.actions [ Card.border ]
                    [ Button.button
                        [ Button.attrs
                            [ onClick (GetPersonalressurs p.links.personalressurs)
                            ]
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
            div []
                [ text "Henter data..."
                , Progress.progress [ Progress.value 100, Progress.animated ]
                ]

        Failure err ->
            div []
                [ text ("Error: " ++ toString err)
                , Button.button
                    [ Button.attrs
                        [ onClick GetPersoner
                        ]
                    ]
                    [ text "Last inn på nytt" ]
                ]

        Success personer ->
            ListGroup.custom <|
                List.map
                    (viewPerson
                        model
                    )
                    (personer
                        |> List.filter (String.contains model.soek << .foedselsnummer)
                    )


viewPersonalressurs : Model -> Html Msg
viewPersonalressurs model =
    case model.personalressurs of
        NotAsked ->
            text ""

        Loading ->
            div []
                [ text "Henter data..."
                , Progress.progress [ Progress.value 100, Progress.animated ]
                ]

        Failure err ->
            text ("Error: " ++ toString err)

        Success pr ->
            Card.view [ Elevation.e2 ]
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
                    [ Button.button
                        [ Button.attrs
                            [ onClick (GetArbeidsforhold pr.links.arbeidsforhold)
                            ]
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
            div []
                [ text "Henter data..."
                , Progress.progress [ Progress.value 100, Progress.animated ]
                ]

        Failure err ->
            text ("Error: " ++ toString err)

        Success a ->
            Card.view [ Elevation.e2 ]
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


viewPerson : Model -> Person -> ListGroup.CustomItem Msg
viewPerson model person =
    ListGroup.anchor
        [ ListGroup.attrs
            [ href "#"
            , class "flex-column align-items-start"
            , onClick <| VelgPerson person
            ]
        ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ text
                    (person.navn.etternavn
                        ++ ", "
                        ++ person.navn.fornavn
                        ++ " "
                        ++ person.navn.mellomnavn
                    )
                ]
            , small [] [ text person.foedselsnummer ]
            ]
        , p [ class "mb-1" ]
            [ text
                ("Adresse: "
                    ++ person.postadresse.adresse
                    ++ ", "
                    ++ person.postadresse.postnummer
                    ++ " "
                    ++ person.postadresse.poststed
                )
            ]
        , small [] [ text person.kontatinformasjon.epostadresse ]
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
