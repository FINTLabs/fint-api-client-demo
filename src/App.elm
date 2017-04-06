module App exposing (..)

import Html exposing (Html, button, div, img, li, text)
import Material as Material
import Material.Helpers as Helpers
import Material.Layout as Layout exposing (selectedTab)
import Module.Personal as Personal


type alias Model =
    { mdl : Material.Model
    , selectedTab : Int
    , personal : Personal.Model
    }


model : Model
model =
    { mdl = Material.model
    , selectedTab = 0
    , personal = Personal.model
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( model
    , Cmd.map PersonalMsg (Personal.getPersoner Personal.urlPersoner)
    )


{-| Url til iso-kodeverk
    - 5218 = Kjønn
    - 31661alpha2 = Land
    - ...
-}
urlKodeverkIso : String -> String
urlKodeverkIso iso =
    "https://api.felleskomponent.no/felles/kodeverk/iso/" ++ iso


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Int
    | PersonalMsg Personal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl m ->
            Material.update Mdl m model

        SelectTab nr ->
            { model | selectedTab = nr } ! []

        PersonalMsg personalMsg ->
            Helpers.lift
                .personal
                (\m x -> { m | personal = x })
                PersonalMsg
                Personal.update
                personalMsg
                model


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
                    Html.map PersonalMsg (Personal.viewPersonal model.personal)

                1 ->
                    text "her kommer uttrekk av iso-kodeverk"

                _ ->
                    text "404"
            ]
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
