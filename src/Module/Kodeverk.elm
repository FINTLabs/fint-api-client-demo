module Module.Kodeverk exposing (..)

import Html exposing (..)
import Http exposing (..)
import Html.Attributes as Attr exposing (..)
import Json.Decode as Decode
import Material as Material
import RemoteData exposing (WebData, RemoteData(NotAsked, Loading, Failure, Success))
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Bootstrap.Progress as Progress


-- MODEL


type alias Model =
    { mdl : Material.Model
    , kodeverk : List ( String, String )
    , kodeliste : WebData (List Begrep)
    }


type alias Begrep =
    { systemId : String
    , kode : String
    , navn : String
    }


model : Model
model =
    { mdl = Material.model
    , kodeverk =
        [ ( "5218", "Kjønn" )
        , ( "31661alpha2", "Land" )
        , ( "6391alpha2", "Språk" )
        ]
    , kodeliste = NotAsked
    }



-- update


type Msg
    = Mdl (Material.Msg Msg)
    | LastKodeverk String
    | KodeverkResponse (WebData (List Begrep))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl m ->
            Material.update Mdl m model

        LastKodeverk iso ->
            ( { model | kodeliste = Loading }, lastKodeverk iso )

        KodeverkResponse response ->
            { model | kodeliste = response } ! []


view : Model -> Html Msg
view model =
    div []
        [ Grid.row []
            [ Grid.col [ Col.attrs [ Attr.align "right" ] ]
                [ a [ href <| urlKodeverkIso "5218" ] [ text "api: 5218" ]
                , text " | "
                , a [ href "https://fintprosjektet.github.io/apidocs/#common-code-list" ] [ text "dokumentasjon" ]
                , text " | "
                , a [ href "https://dokumentasjon.felleskomponent.no/docs/" ] [ text "informasjonsmodell" ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                (model.kodeverk
                    |> List.map
                        (\( a, b ) ->
                            Button.button
                                [ Button.secondary
                                , Button.onClick (LastKodeverk a)
                                , Button.attrs [ class "ml-1" ]
                                ]
                                [ text (b ++ " " ++ a) ]
                        )
                )
            ]
        , case model.kodeliste of
            Success kodeliste ->
                Table.simpleTable
                    ( Table.simpleThead
                        [ Table.th [] [ text "SystemId" ]
                        , Table.th [] [ text "Kode" ]
                        , Table.th [] [ text "Navn" ]
                        ]
                    , Table.tbody []
                        (kodeliste
                            |> List.map
                                (\kode ->
                                    Table.tr []
                                        [ Table.td [] [ text kode.systemId ]
                                        , Table.td [] [ text kode.kode ]
                                        , Table.td [] [ text kode.navn ]
                                        ]
                                )
                        )
                    )

            NotAsked ->
                text "Velg ett kodeverk."

            Loading ->
                div [] [ text "Henter data...", Progress.progress [ Progress.value 100, Progress.animated ] ]

            Failure e ->
                text <| "Feil: " ++ toString e
        ]


{-| Url til iso-kodeverk
    - 5218 = Kjønn
    - 31661alpha2 = Land
    - ...
-}
urlKodeverkIso : String -> String
urlKodeverkIso iso =
    "https://api.felleskomponent.no/felles/kodeverk/iso/" ++ iso


lastKodeverk : String -> Cmd Msg
lastKodeverk iso =
    let
        url =
            urlKodeverkIso iso
    in
        Http.get url decodeEmbeddedEntries
            |> RemoteData.sendRequest
            |> Cmd.map KodeverkResponse


decodeEmbeddedEntries : Decode.Decoder (List Begrep)
decodeEmbeddedEntries =
    Decode.at [ "_embedded", "_entries" ] decodeKodeverk


decodeKodeverk : Decode.Decoder (List Begrep)
decodeKodeverk =
    Decode.list decodeBegrep


decodeBegrep : Decode.Decoder Begrep
decodeBegrep =
    Decode.map3 Begrep
        (Decode.at [ "systemId", "identifikatorverdi" ] Decode.string)
        (Decode.field "kode" Decode.string)
        (Decode.field "navn" Decode.string)
