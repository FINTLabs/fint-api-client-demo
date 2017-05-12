module Helpers exposing (..)

{-| Variant of EA update function type, where effects may be
lifted to a different type.
-}


type alias Update_ model action action_ =
    action -> model -> ( model, Cmd action_ )


{-| Standard EA update function type.
-}
type alias Update model action =
    Update_ model action action


{-| Convenience function for writing update-function boilerplate. Example use:

    case msg of
      ...
      ButtonsMsg msg_ ->
        lift .buttons (\m x->{m|buttons=x}) ButtonsMsg Demo.Buttons.update msg_ model

This is equivalent to the more verbose

    case msg of
      ...
      ButtonsMsg msg_ ->
        let
          (buttons_, cmd) =
            Demo.Buttons.update msg_ model.buttons
        in
          ( { model | buttons = buttons_}
          , Cmd.map ButtonsMsg cmd
          )
-}
lift :
    (model -> submodel)
    -> -- get
       (model -> submodel -> model)
    -> -- set
       (subaction -> action)
    -> -- fwd
       Update submodel subaction
    -> -- update
       subaction
    -> -- action
       model
    -> -- model
       ( model, Cmd action )
lift get set fwd update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, Cmd.map fwd e )
