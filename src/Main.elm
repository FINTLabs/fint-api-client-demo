module Main exposing (..)

import App exposing (view, update, subscriptions, Model, Msg)
import Routing as Routing
import Navigation exposing (Location)


init : String -> Location -> ( Model, Cmd Msg )
init logo location =
    let
        currentPage =
            Routing.parseLocation location
    in
        App.init currentPage logo


main : Program String Model Msg
main =
    Navigation.programWithFlags
        App.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
