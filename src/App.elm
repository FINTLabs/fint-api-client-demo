module App exposing (..)

import Html exposing (Html, button, div, img, li, text)
import Material as Material
import Material.Helpers as Helpers
import Material.Layout as Layout exposing (selectedTab)
import Module.Personal as Personal
import Module.Kodeverk as Kodeverk


type alias Model =
    { mdl : Material.Model
    , selectedTab : Int
    , personal : Personal.Model
    , kodeverk : Kodeverk.Model
    }


model : Model
model =
    { mdl = Material.model
    , selectedTab = 0
    , personal = Personal.model
    , kodeverk = Kodeverk.model
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( model
    , Cmd.map PersonalMsg (Personal.getPersoner Personal.urlPersoner)
    )


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Int
    | PersonalMsg Personal.Msg
    | KodeverkMsg Kodeverk.Msg


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

        KodeverkMsg msg_ ->
            Helpers.lift .kodeverk (\m x -> { m | kodeverk = x }) KodeverkMsg Kodeverk.update msg_ model


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
        , tabs = ( [ text "Personal", text "Kodeverk" ], [] )
        , main =
            [ case model.selectedTab of
                0 ->
                    Html.map PersonalMsg (Personal.viewPersonal model.personal)

                1 ->
                    Html.map KodeverkMsg (Kodeverk.view model.kodeverk)

                _ ->
                    text "404"
            ]
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
