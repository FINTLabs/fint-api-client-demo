module App exposing (..)

import Html exposing (Html, div, img, li, text)
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import List exposing (..)
import RemoteData exposing (RemoteData(Failure), RemoteData(Loading), RemoteData(NotAsked), RemoteData(Success), WebData)


type alias Model =
    { personer : WebData (List Person)
    }


type alias Person =
    { fornavn : String
    , etternavn : String
    , gateadresse : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { personer = Loading }, getPersoner )


getPersoner : Cmd Msg
getPersoner =
    Http.get "https://api.felleskomponent.no/mocks/administrasjon/personal/person" decodePersoner
        |> RemoteData.sendRequest
        |> Cmd.map PersonsResponse


decodePersoner : Decode.Decoder (List Person)
decodePersoner =
    Decode.at [ "_embedded", "personList" ] (Decode.list decodePerson)


decodePerson : Decode.Decoder Person
decodePerson =
    Decode.map3 Person
        (at [ "navn", "fornavn" ] Decode.string)
        (at [ "navn", "etternavn" ] Decode.string)
        (at [ "postadresse", "adresse" ] Decode.string)


type Msg
    = PersonsResponse (WebData (List Person))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersonsResponse response ->
            ( { model | personer = response }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "test" ]
        , div [] [ viewPersoner model ]
        ]


viewPersoner : Model -> Html Msg
viewPersoner model =
    case model.personer of
        NotAsked ->
            text "Starter opp..."

        Loading ->
            text "Henter data..."

        Failure err ->
            text ("Error: " ++ toString err)

        Success personer ->
            viewPersoner2 personer


viewPersoner2 : List Person -> Html Msg
viewPersoner2 personer =
    Html.ul [] (List.map viewPerson personer)


viewPerson : Person -> Html Msg
viewPerson person =
    Html.li []
        [ text (person.etternavn ++ ", " ++ person.fornavn)
        , text " adresse: "
        , text person.gateadresse
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
