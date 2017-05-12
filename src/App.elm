module App exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Html exposing (..)
import Html.Attributes as Attribute exposing (..)
import Material as Material
import Helpers as Helpers
import Module.Kodeverk as Kodeverk
import Module.Personal as Personal
import Navigation exposing (Location)
import Routing exposing (..)


type alias Model =
    { mdl : Material.Model
    , personal : Personal.Model
    , kodeverk : Kodeverk.Model
    , navbarState : Navbar.State
    , selectedPage : Page
    }


init : Page -> ( Model, Cmd Msg )
init page =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        model =
            { -- mdl kan snart fjernes :)
              mdl = Material.model
            , personal = Personal.model
            , kodeverk = Kodeverk.model
            , navbarState = navbarState
            , selectedPage = page
            }
    in
        ( model
        , Cmd.batch
            [ navbarCmd
            , Cmd.map PersonalMsg (Personal.getPersoner Personal.urlPersoner)
            ]
        )


type Msg
    = Mdl (Material.Msg Msg)
    | PersonalMsg Personal.Msg
    | KodeverkMsg Kodeverk.Msg
    | NavbarMsg Navbar.State
    | OnLocationChange Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl m ->
            Material.update Mdl m model

        PersonalMsg personalMsg ->
            Helpers.lift .personal (\m x -> { m | personal = x }) PersonalMsg Personal.update personalMsg model

        KodeverkMsg kodeverkMsg ->
            Helpers.lift .kodeverk (\m x -> { m | kodeverk = x }) KodeverkMsg Kodeverk.update kodeverkMsg model

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                { model | selectedPage = newRoute } ! []


view : Model -> Html Msg
view model =
    div []
        [ navbarView model
        , contentView model
        ]


navbarView : Model -> Html Msg
navbarView model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ Routing.href Index ]
            [ img
                [ src "src/assets/fint.svg"
                , class "d-inline-block align-top"
                , style [ ( "width", "75px" ), ( "margin-right", "10px" ) ]
                ]
                []
            , text "API-demo"
            ]
        |> Navbar.items
            [ Navbar.itemLink [ Routing.href Personal ] [ text "Personal" ]
            , Navbar.itemLink [ Routing.href Kodeverk ] [ text "Kodeverk" ]
            ]
        |> Navbar.view model.navbarState


contentView : Model -> Html Msg
contentView model =
    Grid.container []
        [ case model.selectedPage of
            Index ->
                Html.map PersonalMsg <| Personal.viewPersonal model.personal

            Personal ->
                Html.map PersonalMsg <| Personal.viewPersonal model.personal

            Kodeverk ->
                Html.map KodeverkMsg <| Kodeverk.view model.kodeverk

            NotFound ->
                text "404"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Navbar.subscriptions model.navbarState NavbarMsg
