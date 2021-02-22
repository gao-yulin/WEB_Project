module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
    exposing
        ( onAnimationFrameDelta
        , onKeyDown
        , onKeyUp
        )
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Value)
import Message exposing (Msg(..))
import Model exposing (Model)
import Task
import Update exposing (update)
import View exposing (view)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown (Decode.map (key True) keyCode)
        , onKeyUp (Decode.map (key False) keyCode)
        ]


key : Bool -> Int -> Msg
key on keycode =
    if on then
        case keycode of
            27 ->
                BackToMenu
            32 ->
                Pause

            37 ->
                MoveDirection ( -1, 0 )

            38 ->
                MoveDirection ( 0, -1 )

            39 ->
                MoveDirection ( 1, 0 )

            40 ->
                MoveDirection ( 0, 1 )

            89 ->
                Choose "Yes"
            82 ->
                Restart
            83 ->
                Skip

            78 ->
                Choose "No"

            13 ->
                EndCurrentDialogue
            85 ->
                UseRoar

            _ ->
                Noop

    else
        case keycode of
            37 ->
                Static

            38 ->
                Static

            39 ->
                Static

            40 ->
                Static

            89 ->
                Choose "Nothing"

            78 ->
                Choose "Nothing"

            82 ->
                Noop
            83 ->
                Skip

            13 ->
                Static

            _ ->
                Noop


init : Value -> ( Model, Cmd Msg )
init _ =
    ( Model.initial
    , Cmd.batch
        [ Task.perform (\{ viewport } -> Resize (round viewport.width) (round viewport.height)) getViewport
        ]
    )


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
