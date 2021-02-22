module View exposing (..)

import Html exposing (Html, audio, button, div)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (onClick)
import Map exposing (..)
import Markdown
import Message exposing (Msg(..))
import Model exposing (..)
import Svg exposing (Svg, image, node, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, xlinkHref, y)
import Update exposing (checkGoRestaurant)


view : Model -> Html Msg
view model =
    case model.runstate of
        StartView ->
            renderBegin model
        Game ->
            renderGame model
        WinGame _ ->
            renderWin model
        Aboutus ->
            renderAbout
        Main ->
            renderMain model

        Paused ->
            renderPause model


renderAbout :  Html Msg
renderAbout =
    div [  style "height" "100%"
         , style "width" "100%"
         , style "left" "0"
         , style "top" "0"
         , style "background" "#ffffff"
         , style "position" "absolute"
        ]
        [  div [  style "height" "100%"
                , style "width" "100%"
                , style "left" "0%"
                , style "top" "0%"
                , style "position" "absolute"
                , style "background" "url(res/aboutus.png) 0% 0% / 100% 100%"
                ]
                []
        ]

renderWin : Model -> Html Msg
renderWin model =
    div [  style "height" "100%"
         , style "width" "100%"
         , style "left" "0"
         , style "top" "0"
         , style "background" "#ffffff"
         , style "position" "absolute"
        ]
        [  div [  style "height" "100%"
                , style "width" "100%"
                , style "left" "0%"
                , style "top" "0%"
                , style "position" "absolute"
                , style "background" "url(res/menu/Dormitory.png) 0% 0% / 100% 100%"
                , style "opacity" (String.fromFloat model.opa)
                ]
                []
        ]


renderBegin : Model -> Html Msg
renderBegin model =
    div [  style "height" "100%"
         , style "width" "100%"
         , style "left" "0"
         , style "top" "0"
         , style "background" "#ffffff"
         , style "position" "absolute"
        ]
        [  div [  style "height" "50%"
                , style "width" "50%"
                , style "left" "25%"
                , style "top" "20%"
                , style "position" "absolute"
                , style "background" "url(res/menu/logo.jpg) 0% 0% / 100% 100%"
                , style "opacity" (String.fromFloat model.opa)
                ]
                []
        ]


--gao: render the page of the pause


renderPause : Model -> Html Msg
renderPause model =
    div
        [ style "height" "100%"
        , style "width" "100%"
        , style "left" "0"
        , style "top" "0"
        , style "background" "url(res/menu/Dormitory.png) 0% 0% / 100% 100%"
        , style "position" "absolute"
        ]
        [ button
            [ style "height" "16%"
            , style "width" "24%"
            , style "top" "15%"
            , style "left" "38%"
            , style "border" "none"
            , style "background" "url(res/menu/resume.png) 0% 0% / 100% 100%"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , onClick Resume
            ]
            []
        , button
            [ style "height" "16%"
            , style "width" "24%"
            , style "top" "40%"
            , style "left" "38%"
            , style "border" "none"
            , style "background" "url(res/menu/newgame.png) 0% 0% / 100% 100%"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , onClick Newgame
            ]
            []
        , button
            [ style "height" "16%"
            , style "width" "24%"
            , style "top" "65%"
            , style "left" "38%"
            , style "border" "none"
            , style "background" "url(res/menu/back.png) 0% 0% / 100% 100%"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , onClick BackToMenu
            ]
            []
        ]


renderMain : Model -> Html Msg
renderMain model =
    div
        [ style "height" "100%"
        , style "width" "100%"
        , style "left" "0"
        , style "top" "0"
        , style "background" "url(res/menu/Dormitory.png) 0% 0% / 100% 100%"
        , style "position" "absolute"
        ]
        [ button
            [ style "height" "16%"
            , style "width" "24%"
            , style "top" "25%"
            , style "left" "38%"
            , style "border" "none"
            , style "background" "url(res/menu/start.png) 0% 0% / 100% 100%"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , onClick Start
            ]
            []
        , button
            [ style "height" "16%"
            , style "width" "24%"
            , style "bottom" "25%"
            , style "left" "38%"
            , style "border" "none"
            , style "background" "url(res/menu/about.png) 0% 0% / 100% 100%"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , onClick About
            ]
            []
        ]


renderGame : Model -> Html Msg
renderGame model =
    let
        color =
            case model.gameState of
                Lisa 1 ->
                    "#e78817"
                Lisa 3 ->
                    "#e78817"
                Lisa 4 ->
                    "#e78817"
                Lisa 2 ->
                    "#2e2c44"
                Felix _ ->
                    "#279cb1"
                _ ->
                   "#442c3b"
    in
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "position" "absolute"
        , style "background-color" color
        ]
        [ renderDormPlot model
        , renderSound model
        , renderlayer model]


--Chen: Play the bgm


renderSound : Model -> Html Msg
renderSound model =
    audio
        [ Html.Attributes.src model.bgm
        , Html.Attributes.autoplay True
        , Html.Attributes.loop True
        , Html.Attributes.preload "True"
        ]
        [ Html.text "You browser does not support audio" ]


renderDormPlot : Model -> Html Msg
renderDormPlot model =
    let
        (x,y) = model.camera
        w = 20 * model.blockSize
        h = 16 * model.blockSize
        str_w = String.fromFloat w
        str_h = String.fromFloat h
        sx = String.fromFloat (x -  w / 2)
        sy = String.fromFloat (y -  h / 2)
        itemsRender = case model.gameState of
            Lisa _ ->
                renderDreamMap model ++ [ renderCat model ] ++ (renderCake model)
            Felix _ ->
                renderDreamMap model ++ [ renderCat model ] ++  renderListFelix model  ++ [ renderBird1 model ] ++ [ renderBird2 model ]++ [ renderBird3 model ] ++ [ renderBird4 model ] ++ [renderFish model]++ (renderCake model)
            _ ->
                renderMap model ++ [ renderCat model ] ++ [ renderLisa model ]
                ++ [ renderFelix model ] ++ [ renderBird1 model ] ++ [ renderBird2 model ]
                ++ [ renderBird3 model ] ++ [ renderBird4 model ] ++ [renderFish model] ++ (renderCake model)
        renderattribute = case model.gameState of
            Lisa _ ->
                --renderdream model
                [ width (String.fromFloat (toFloat (Tuple.first model.block) * model.blockSize * 1.5))
                , height (String.fromFloat (toFloat (Tuple.second model.block) * model.blockSize))
                , Svg.Attributes.viewBox (sx ++ " " ++ sy ++ " " ++ str_w ++ " " ++ str_h)
                , style "position" "absolute"
                , style "left" (String.fromFloat (offsetx model) ++ "px")
                , style "top" (String.fromFloat (offsety model) ++ "px")
                ]
            Felix _ ->
                renderdream model
            _ ->
                renderday model

    in

    svg renderattribute itemsRender

renderListFelix : Model -> List (Svg Msg)
renderListFelix model=
    List.map (renderFeli) model.chaser


renderFeli : Npc -> Svg Msg
renderFeli feli =
    let
        dx = Tuple.first feli.dir
        dy = Tuple.second feli.dir
        choose =
            if dy / dx < 1 && dy / dx > -1 && dx < 0 then "left"
            else if dy / dx < 1 && dy / dx > -1 && dx >= 0 then "right"
            else if dy < 0 then "back"
            else if dy >= 0 then "front"
            else ""
        urll =
            if feli.controlled then "./res/chara/Felix_mad.png"
            else if dx /=0 && dy /= 0 then
            "./res/chara/felix" ++ choose ++ String.fromInt feli.frame ++ ".png"
            else "./res/chara/Felix.png"
    in
    image
        [ x (String.fromFloat (Tuple.first feli.pos))
        , y (String.fromFloat (Tuple.second feli.pos))
        , height (String.fromInt (Tuple.second feli.size))
        , width (String.fromInt (Tuple.first feli.size))
        , xlinkHref urll
        ]
        []


renderdream: Model -> List (Svg.Attribute Msg)
renderdream model =
    [ width (String.fromFloat (toFloat (Tuple.first model.block) * model.blockSize))
    , height (String.fromFloat (toFloat (Tuple.second model.block) * model.blockSize))
    , style "position" "absolute"
    , style "left" ((String.fromFloat (offsetxd model)) ++ "px")
    , style "top" ((String.fromFloat (offsety model)) ++ "px")
    ]

renderday: Model -> List (Svg.Attribute Msg)
renderday model =
    let
        (x,y) = model.camera
        w = 20 * model.blockSize
        h = 16 * model.blockSize
        str_w = String.fromFloat w
        str_h = String.fromFloat h
        sx = String.fromFloat (x -  w / 2)
        sy = String.fromFloat (y -  h / 2)
    in
    [ width (String.fromFloat (toFloat (Tuple.first model.block) * model.blockSize * 1.5 ))
    , height (String.fromFloat (toFloat (Tuple.second model.block) * model.blockSize))
    , Svg.Attributes.viewBox (sx ++ " " ++ sy ++ " " ++ str_w ++ " " ++ str_h)
    , style "position" "absolute"
    , style "left" ((String.fromFloat (offsetx model)) ++ "px")
    , style "top" ((String.fromFloat (offsety model)) ++ "px")
    ]


renderlayer : Model -> Html Msg
renderlayer model =
    div []
        [ renderstatus model
        , renderDialogue model
        ]


renderstatus : Model -> Html Msg
renderstatus model =
    svg
        [ width (String.fromFloat (toFloat (Tuple.first model.block) * model.blockSize + offsetx model))
        , height (String.fromFloat (toFloat (Tuple.second model.block) * model.blockSize))
        , style "position" "absolute"
        ]
        ([ renderbar model ] ++ renderattr model)


renderbar : Model -> Svg Msg
renderbar model =
    let
        wid =
            toFloat (Tuple.first model.block) * model.blockSize
    in
    image
        [ x (String.fromFloat (offsetx model))
        , y (String.fromFloat (offsety model))
        , height (String.fromFloat (wid / 6))
        , width (String.fromFloat (wid / 3))
        , xlinkHref "./res/status_bar.png"
        ]
        []


renderattr : Model -> List (Svg Msg)
renderattr model =
    let
        wid =
            toFloat (Tuple.first model.block) * model.blockSize

        a =
            offsetx model + wid * 13 / 60

        b =
            offsety model + wid / 19

        h =
            String.fromFloat (wid / 48)
    in
    [ rect [ x (String.fromFloat a), y (String.fromFloat b), height h, width (String.fromInt (model.cat.food * 10)), fill "green" ] []
    , rect [ x (String.fromFloat a), y (String.fromFloat (b + wid / 36)), height h, width (String.fromInt (model.cat.knowledge * 10)), fill "blue" ] []
    , rect [ x (String.fromFloat a), y (String.fromFloat (b + wid / 18)), height h, width (String.fromInt (model.cat.sanity * 10)), fill "red" ] []
    ]


offsetx : Model -> Float
offsetx model =
    (toFloat (Tuple.first model.size) - (1.5 * model.blockSize * toFloat (Tuple.first model.block))) / 2


offsetxd : Model -> Float
offsetxd model =
    (toFloat (Tuple.first model.size) - (model.blockSize * toFloat (Tuple.first model.block))) / 2


offsety : Model -> Float
offsety model =
    (toFloat (Tuple.second model.size) - (model.blockSize * toFloat (Tuple.second model.block))) / 2


renderCat : Model -> Svg Msg
renderCat model =
    let
        { cat } =
            model


        siz =
            cat.size
        link = "./res/fun/Layer 1_sprite_" ++ (if cat.dir == (0,0) then "1" else String.fromInt cat.frame)
        linkx = if Tuple.first cat.dir > 0 then (link ++ "_left" ++ ".png") else link ++ ".png"
    in
    image
        [ x (String.fromFloat (Tuple.first cat.pos))
        , y (String.fromFloat (Tuple.second cat.pos))
        , height (String.fromInt (Tuple.second siz))
        , width (String.fromInt (Tuple.first siz))
        , xlinkHref linkx
        ]
        []


renderLisa : Model -> Svg Msg
renderLisa model =
    let
        { lisa } =
            model
        blo = model.blockSize
        dir = lisa.dir
        choose =
            if dir == (-1,0) then "left"
            else if dir == (1,0) then "right"
            else if dir == (0,-1)  then "back"
            else if dir == (0,1) then "front"
            else ""
        url =  "./res/chara/lisa" ++ choose ++ String.fromInt lisa.frame ++ ".png"
    in
    image
        [ x (String.fromFloat (Tuple.first lisa.pos))
        , y (String.fromFloat (Tuple.second lisa.pos))
        , height (String.fromFloat blo)
        , width (String.fromFloat blo)
        , xlinkHref url
        ]
        []


renderFelix : Model -> Svg Msg
renderFelix model =
    let
        { felix } =
            model
        blo = model.blockSize
        dx = Tuple.first felix.dir
        dy = Tuple.second felix.dir
        choose =
            if (dy / dx < 1 && dy / dx > -1 && dx < 0) || (dy == 0 && dx < 0) then "left"
            else if dy / dx < 1 && dy / dx > -1 && dx >= 0 || (dy == 0 && dx > 0)then "right"
            else if dy < 0 then "back"
            else if dy >= 0 then "front"
            else ""
        url =
            if felix.controlled then "./res/chara/Felix_mad.png"
            else if dx == 0 && dy == 0 then
            "./res/chara/Felix.png"
            else "./res/chara/felix" ++ choose ++ String.fromInt felix.frame ++ ".png"
    in
    image
        [ x (String.fromFloat (Tuple.first felix.pos))
        , y (String.fromFloat (Tuple.second felix.pos))
        , height (String.fromFloat blo)
        , width (String.fromFloat blo)
        , xlinkHref url
        ]
        []



-------------


renderBird1 : Model -> Svg Msg
renderBird1 model =
    let
        { bird1 } =
            model
    in
    image
        [ x (String.fromFloat (Tuple.first bird1.pos))
        , y (String.fromFloat (Tuple.second bird1.pos))
        , height (String.fromInt (Tuple.second bird1.size))
        , width (String.fromInt (Tuple.first bird1.size))
        , xlinkHref "./res/chara/bird1.png"
        ]
        []


renderBird2 : Model -> Svg Msg
renderBird2 model =
    let
        { bird2 } =
            model
    in
    image
        [ x (String.fromFloat (Tuple.first bird2.pos))
        , y (String.fromFloat (Tuple.second bird2.pos))
        , height (String.fromInt (Tuple.second bird2.size))
        , width (String.fromInt (Tuple.first bird2.size))
        , xlinkHref "./res/chara/bird2.png"
        ]
        []


renderBird3 : Model -> Svg Msg
renderBird3 model =
    let
        { bird3 } =
            model
    in
    image
        [ x (String.fromFloat (Tuple.first bird3.pos))
        , y (String.fromFloat (Tuple.second bird3.pos))
        , height (String.fromInt (Tuple.second bird3.size))
        , width (String.fromInt (Tuple.first bird3.size))
        , xlinkHref "./res/chara/bird3.png"
        ]
        []


renderBird4 : Model -> Svg Msg
renderBird4 model =
    let
        { bird4 } =
            model
    in
    image
        [ x (String.fromFloat (Tuple.first bird4.pos))
        , y (String.fromFloat (Tuple.second bird4.pos))
        , height (String.fromInt (Tuple.second bird4.size))
        , width (String.fromInt (Tuple.first bird4.size))
        , xlinkHref "./res/chara/bird4.png"
        ]
        []


renderFish : Model -> Svg Msg
renderFish model =
    let
        { fish } =
            model
    in
    image
        [ x (String.fromFloat (Tuple.first fish.pos))
        , y (String.fromFloat (Tuple.second fish.pos))
        , height (String.fromInt (Tuple.second fish.size))
        , width (String.fromInt (Tuple.first fish.size))
        , xlinkHref "res/chara/fish.png"
        ]
        []


renderMap : Model -> List (Svg Msg)
renderMap model =
    List.map (renderBlock model) model.map


renderDreamMap : Model -> List (Svg Msg)
renderDreamMap model =
    List.map (renderBlock model) model.dreamMod.dreamMap


renderBlock : Model -> Block -> Svg Msg
renderBlock model block =
    let
        pixel =
            model.blockSize

        folder =
            "blocks"
    in
    image
        [ x (String.fromFloat (pixel * toFloat (Tuple.first block.pos)))
        , y (String.fromFloat (pixel * toFloat (Tuple.second block.pos)))
        , height (String.fromFloat pixel)
        , width (String.fromFloat pixel)
        , xlinkHref ("./res/" ++ folder ++ "/" ++ block.blockType ++ ".png")
        ]
        []



{-- Some legacies

renderTextBox : Model -> List (Svg Msg)
renderTextBox model =
    let
        pixel =
            model.blockSize
    in
    [ image
        [ x (String.fromFloat (offsetx model + pixel * 10))
        , y (String.fromFloat (offsety model + pixel * 20))
        , height (String.fromFloat (pixel * 5))
        , width (String.fromFloat (pixel * 26))
        , xlinkHref "./res/txtback.png"
        ]
        []
    ]

renderText : Model -> Html Msg
renderText model =
    let
        pixel =
            model.blockSize
        showText =
            if model.gameState == CheckEnterDream then
                "Do you want to end the day and go to sleep? Press y to confirm."
            else if model.gameState == CheckLeaveDream then
                "Dream is over, a new day!"
            else if model.dreamMod.dreamState == Win then
                "You win!!!"
            else
                ""
    in
    if
        model.gameState
            == CheckEnterDream
            || model.gameState
            == CheckLeaveDream
            || model.dreamMod.dreamState
            == Win
    then
        svg
            [ width (String.fromFloat (toFloat (Tuple.first model.block) * model.blockSize + offsetx model))
            , height (String.fromFloat (toFloat (Tuple.second model.block) * model.blockSize))
            , style "position" "absolute"
            ]
            (renderTextBox model
                ++ [ node "text"
                        [ fill "white"
                        , attribute "x" (String.fromFloat (offsetx model + pixel * 10))
                        , attribute "y" (String.fromFloat (offsety model + pixel * 20))
                        , attribute "font-size" "25px"
                        , attribute "font-family" "BlinkMacSystemFont"
                        ]
                        [ Svg.text showText ]
                   ]
            )
    else
        div [] []

renderText_day_night : Model -> Html Msg
renderText_day_night model =
    let
        pixel =
            model.blockSize

        showText =
            if model.gameState == InDream then
                "Dream" ++ String.fromInt model.dream

            else if model.gameState == Daytime || model.gameState == InRestaurant then
                "Day" ++ String.fromInt model.day

            else
                "Bug appears"
    in
    if model.gameState == InDream || model.gameState == Daytime || model.gameState == InRestaurant then
        svg
            [ width (String.fromFloat (toFloat (Tuple.first model.block) * model.blockSize + offsetx model))
            , height (String.fromFloat (toFloat (Tuple.second model.block) * model.blockSize))
            , style "position" "absolute"
            ]
            --renderTextBox model ++
            [ node "text"
                [ fill "white"
                , attribute "x" (String.fromFloat (pixel * 8))
                , attribute "y" (String.fromFloat (pixel * 3))
                , attribute "font-size" "25px"
                , attribute "font-family" "BlinkMacSystemFont"
                ]
                [ Svg.text showText ]
            ]

    else
        div [] []

-}
renderCake: Model -> List (Svg Msg)
renderCake model =
    let
        pixel = model.blockSize
        {cat}=model
    in
    if model.gameState== InRestaurant then
    [image
        [ x (String.fromFloat (pixel * 5))
        , y (String.fromFloat (pixel * 5))
        , height (String.fromFloat (pixel*1.5) )
        , width (String.fromFloat (pixel*3/2) )
        , style "position" "absolute"
        , xlinkHref ("./res/cake.png" )
        ]
        []]
    else
        []

renderDialogue : Model -> Html Msg
renderDialogue model =
    let
        { lisaline } =
            model
        blo = model.blockSize
        height = String.fromFloat (5*3 * blo) ++ "px"
        wid = String.fromFloat (3*24 * blo) ++ "px"
        top = 11*3 * blo

    in
    if model.gameState == InDialog then
        div
            [ style "background" "rgba(191, 178, 145, .851)"
            , style "color" "#442c3b"
            , style "font-family" "Helvetica, Arial, sans-serif"
            , style "font-size" "22px"
            , style "height" height
            , style "line-height" "1.5"
            , style "padding" "0 15px"
            , style "position" "absolute"
            , style "left" ((String.fromFloat (offsetx model)) ++ "px")
            , style "top" ((String.fromFloat (top + offsety model) ) ++ "px")
            , style "width" wid
            ]
            [ Markdown.toHtml [] lisaline.dialog
            ]
    else if model.gameState == Daytime && checkGoRestaurant model then
            div
                [ style "background" "rgba(191, 178, 145, .851)"
                , style "color" "#283f99"
                , style "font-family" "Helvetica, Arial, sans-serif"
                , style "font-size" "18px"
                , style "height" "185px"
                , style "left" "300px"
                , style "line-height" "1.5"
                , style "padding" "0 15px"
                , style "position" "absolute"
                , style "top" "480px"
                , style "width" "800px"
                ]
                [ Markdown.toHtml [] "Do you want to eat? Y/N"
                ]
    else if model.gameState == InRestaurant then
                div
                    [ style "background" "rgba(191, 178, 145, .851)"
                    , style "color" "#283f99"
                    , style "font-family" "Helvetica, Arial, sans-serif"
                    , style "font-size" "18px"
                    , style "height" "185px"
                    , style "left" "300px"
                    , style "line-height" "1.5"
                    , style "padding" "0 15px"
                    , style "position" "absolute"
                    , style "top" "480px"
                    , style "width" "800px"
                    ]
                    [ Markdown.toHtml [] "Press Y to eat"
                    ]
    else
       div [] []
