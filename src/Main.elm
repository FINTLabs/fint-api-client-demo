module Main exposing (..)

import App exposing (view, update, subscriptions, Model, Msg)
import Routing as Routing
import Navigation exposing (Location)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentPage =
            Routing.parseLocation location
    in
        App.init currentPage


main : Program Never Model Msg
main =
    Navigation.program App.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
