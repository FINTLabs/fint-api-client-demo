module Main exposing (..)

import App exposing (view, update, subscriptions, Model, Msg)
import Routing as Routing
import Navigation exposing (Location)


type alias Input =
    { logo : String
    , debug : Bool
    }


init : Input -> Location -> ( Model, Cmd Msg )
init input location =
    let
        currentPage =
            Routing.parseLocation location
    in
        App.init currentPage input.logo input.debug


main : Program Input Model Msg
main =
    Navigation.programWithFlags
        App.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
