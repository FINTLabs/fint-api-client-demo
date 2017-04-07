module Module.Kodeverk exposing (..)

import Html exposing (..)
import Html.Events
import Http exposing (..)
import Json.Decode as Decode
import Material as Material
import Material.Table as Table
import Material.Progress as Loading
import RemoteData exposing (RemoteData(NotAsked, Success, Loading, Failure), WebData, sendRequest)


-- MODEL


type alias Model =
    { mdl : Material.Model
    , kodeverk : List ( String, String )
    , kodeliste : WebData (List Begrep)
    }


type alias Begrep =
    { kode : String
    , navn : String
    }


model : Model
model =
    { mdl = Material.model
    , kodeverk =
        [ ( "5218", "Kjønn" )
        , ( "31661alpha2", "Land" )
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
        [ ul []
            (model.kodeverk
                |> List.map
                    (\( a, b ) ->
                        li [ Html.Events.onClick (LastKodeverk a) ] [ text (b ++ " " ++ a) ]
                    )
            )
        , case model.kodeliste of
            Success kodeliste ->
                Table.table []
                    [ Table.thead [] [ Table.tr [] [ Table.th [] [ text "Kode" ], Table.th [] [ text "Navn" ] ] ]
                    , Table.tbody []
                        (kodeliste
                            |> List.map
                                (\kode ->
                                    Table.tr []
                                        [ Table.td [] [ text kode.kode ]
                                        , Table.td [] [ text kode.navn ]
                                        ]
                                )
                        )
                    ]

            NotAsked ->
                text "Velg ett kodeverk i lista."

            Loading ->
                div [] [ text "Henter data...", Loading.indeterminate ]

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
        Http.get url decodeKodeverk
            |> RemoteData.sendRequest
            |> Cmd.map KodeverkResponse


decodeKodeverk : Decode.Decoder (List Begrep)
decodeKodeverk =
    Decode.list decodeBegrep


decodeBegrep : Decode.Decoder Begrep
decodeBegrep =
    Decode.map2 Begrep
        (Decode.field "kode" Decode.string)
        (Decode.field "navn" Decode.string)
