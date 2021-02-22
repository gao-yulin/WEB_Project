module View exposing (..)

import Color exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Model exposing (..)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, width, x, y)
import Update exposing (..)


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "background-image" "url(https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1591900526633&di=3381bab8beea2010945793bd4bc8ba51&imgtype=0&src=http%3A%2F%2Fimg3.imgtn.bdimg.com%2Fit%2Fu%3D3192206463%2C2486387000%26fm%3D214%26gp%3D0.jpg)"
        ]
        [ div
            [ style "width" (String.fromFloat (0.8 * Tuple.first model.winsize) ++ "px")
            , style "height" (String.fromFloat (0.8 * Tuple.second model.winsize) ++ "px")
            , style "position" "absolute"
            ]
            [ viewtitle "This is a brickbreaker game! Enjoy your time!"
            , renderScore model.score
            , manipulate model
            ]
        ]


renderScore : Int -> Html Msg
renderScore n =
    div
        [ style "color" "#3993d0"
        , style "font-size" "30px"
        , style "line-height" "1"
        , style "margin" "5px 0 0"
        , style "left" "20px"
        ]
        [ text ("Score : " ++ String.fromInt n) ]


manipulate : Model -> Html Msg
manipulate model =
    if model.state == Playing then
        svg
            [ Svg.Attributes.style "outline: thick solid grey"
            , width (String.fromFloat (0.8 * Tuple.first model.winsize))
            , height (String.fromFloat (0.8 * Tuple.second model.winsize))
            , style "position" "absolute"
            , style "left" "160px"
            , style "top" "100px"
            ]
            ([ renderBall model.ball
             , renderPaddle model.paddle
             ]
                ++ (case model.secondflag of
                        0 ->
                            List.map renderBrick model.brick

                        2 ->
                            List.map changeBrick model.brick

                        _ ->
                            List.map renderBrick model.brick
                   )
            )

    else
        renderInfo model.state model


renderPaddle : Object -> Html Msg
renderPaddle paddle =
    Svg.rect
        [ x (String.fromFloat (0.8 * Tuple.first paddle.center_pos))
        , y (String.fromFloat (0.8 * Tuple.second paddle.center_pos))
        , width (String.fromFloat (0.8 * Tuple.first paddle.size))
        , height (String.fromFloat (0.8 * Tuple.second paddle.size))
        , fill (Color.toString (Color.rgb 33 199 28))
        ]
        []


changeBrick : Object -> Html Msg
changeBrick brick =
    case brick.brick_type of
        Just Good ->
            Svg.rect
                [ x (String.fromFloat (0.8 * Tuple.first brick.center_pos))
                , y (String.fromFloat (0.8 * Tuple.second brick.center_pos))
                , height (String.fromFloat (0.8 * Tuple.second brick.size))
                , width (String.fromFloat (0.8 * Tuple.first brick.size))
                , stroke (Color.toString (Color.rgb 230 240 238))
                , fill (Color.toString (Color.rgb 23 24 238))
                ]
                []

        Just Guard ->
            Svg.rect
                [ x (String.fromFloat (0.8 * Tuple.first brick.center_pos))
                , y (String.fromFloat (0.8 * Tuple.second brick.center_pos))
                , height (String.fromFloat (0.8 * Tuple.second brick.size))
                , width (String.fromFloat (0.8 * Tuple.first brick.size))
                , stroke (Color.toString (Color.rgb 230 240 238))
                , fill (Color.toString (Color.rgb 62 238 23))
                ]
                []

        Just Evil ->
            Svg.rect
                [ x (String.fromFloat (0.8 * Tuple.first brick.center_pos))
                , y (String.fromFloat (0.8 * Tuple.second brick.center_pos))
                , height (String.fromFloat (0.8 * Tuple.second brick.size))
                , width (String.fromFloat (0.8 * Tuple.first brick.size))
                , stroke (Color.toString (Color.rgb 230 240 238))
                , fill (Color.toString (Color.rgb 238 23 66))
                ]
                []

        Nothing ->
            Svg.rect
                [ x (String.fromFloat (0.8 * Tuple.first brick.center_pos))
                , y (String.fromFloat (0.8 * Tuple.second brick.center_pos))
                , height (String.fromFloat (0.8 * Tuple.second brick.size))
                , width (String.fromFloat (0.8 * Tuple.first brick.size))
                , stroke (Color.toString (Color.rgb 230 240 238))
                , fill (Color.toString (Color.rgb 2 2 2))
                ]
                []


renderBrick : Object -> Html Msg
renderBrick brick =
    Svg.rect
        [ x (String.fromFloat (0.8 * Tuple.first brick.center_pos))
        , y (String.fromFloat (0.8 * Tuple.second brick.center_pos))
        , height (String.fromFloat (0.8 * Tuple.second brick.size))
        , width (String.fromFloat (0.8 * Tuple.first brick.size))
        , stroke (Color.toString (Color.rgb 230 240 238))
        , fill (Color.toString (Color.rgb 23 24 238))
        ]
        []


renderBall : Object -> Html Msg
renderBall ball =
    Svg.circle
        [ cx (String.fromFloat (0.8 * Tuple.first ball.center_pos))
        , cy (String.fromFloat (0.8 * Tuple.second ball.center_pos))
        , r (String.fromFloat (0.8 * Tuple.first ball.size))
        , fill (Color.toString (Color.rgb 226 77 17))
        ]
        []


renderInfo : Model.State -> Model -> Html Msg
renderInfo state model =
    div
        [ style "color" "#4236ad"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "38px"
        , style "width" (String.fromFloat (0.8 * Tuple.first model.winsize) ++ "px")
        , style "height" (String.fromFloat (0.8 * Tuple.second model.winsize) ++ "px")
        , style "left" "160px"
        , style "line-height" "30"
        , style "text-align" "center"

        --, style "padding" "0 15px"
        , style "position" "absolute"
        , style "top" "100px"
        , style "display"
            (if state == Model.Playing then
                "none"

             else
                "block"
            )
        ]
        [ if state == Stopped then
            button
                [ style "background" "#34495f"
                , style "border" "0"
                , style "bottom" "30px"
                , style "color" "#fff"
                , style "cursor" "pointer"
                , style "display" "block"
                , style "font-family" "Helvetica, Arial, sans-serif"
                , style "font-size" "18px"
                , style "font-weight" "300"
                , style "height" "60px"
                , style "left" "30px"
                , style "line-height" "60px"
                , style "outline" "none"
                , style "padding" "0"
                , style "position" "absolute"
                , style "width" "120px"
                , onClick Start
                ]
                [ text "start game" ]

          else
            div
                [ style "height" "60px"
                ]
                [ if state == Win then
                    text "You win"

                  else
                    text "You lose"
                , button
                    [ style "background" "#34495f"
                    , style "border" "0"
                    , style "bottom" "30px"
                    , style "color" "#fff"
                    , style "cursor" "pointer"
                    , style "display" "block"
                    , style "font-family" "Helvetica, Arial, sans-serif"
                    , style "font-size" "18px"
                    , style "font-weight" "300"
                    , style "height" "60px"
                    , style "left" "30px"
                    , style "line-height" "60px"
                    , style "outline" "none"
                    , style "padding" "0"
                    , style "position" "absolute"
                    , style "width" "120px"
                    , onClick Newgame
                    ]
                    [ text "new game" ]
                ]
        ]


viewtitle : String -> Html Msg
viewtitle txt =
    div
        [ style "color" "#f2f3f5"
        , style "font-size" "40px"
        , style "line-height" "60px"
        , style "margin" "30px 0 0"
        , style "left" "300px"
        , style "text-align" "center"
        ]
        [ text txt ]
