module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Message exposing (Msg(..))
import Model exposing (Model, init)
import Update exposing (update)
import View exposing (view)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.state == Model.Playing then
            onAnimationFrameDelta Tick

          else
            Sub.none
        , onResize Resize
        , onKeyDown (Decode.map (key True) keyCode)
        , onKeyUp (Decode.map (key False) keyCode)
        ]


key : Bool -> Int -> Msg
key on keycode =
    if on then
        case keycode of
            37 ->
                MoveDirection -1

            39 ->
                MoveDirection 1

            _ ->
                Noop

    else
        case keycode of
            37 ->
                MoveDirection 0

            39 ->
                MoveDirection 0

            _ ->
                Noop


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
