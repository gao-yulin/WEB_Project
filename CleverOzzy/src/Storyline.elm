module Storyline exposing (..)

--Game storyline by Chen: If the storyline has bugs, fix it in this file.

import Array
import DialogList exposing (..)
import Felix exposing (updateFelix)
import Map exposing (..)
import Model exposing (..)


updateStory : Model -> Model
updateStory model =
    model
        |> updateDialog
        |> updateStoryline


updateDialog : Model -> Model
updateDialog model =
    let
        { lisaline, dialogList, dialogNum, gameState } =
            model

        showText =
            Maybe.withDefault "" (Array.get dialogNum dialogList)

        newlisaline =
            { lisaline | dialog = showText }
    in
    if showText == "null" then
        { model | dialogNum = dialogNum + 1, gameState = Daytime }

    else
        { model | lisaline = newlisaline }


updateStoryline : Model -> Model
updateStoryline model =
    let
        { lisaline } =
            model
    in
    case lisaline.day of
        1 ->
            case lisaline.plot of
                1 ->
                    lisaline11 model

                2 ->
                    lisaline12 model

                3 ->
                    lisaline13 model

                4 ->
                    lisaline14 model

                5 ->
                    lisaline15 model

                6 ->
                    lisaline16 model

                8 ->
                    lisaline18 model

                9 ->
                    lisaline19 model

                _ ->
                    model

        2 ->
            case lisaline.plot of
                1 ->
                    lisaline21 model

                2 ->
                    lisaline22 model

                3 ->
                    lisaline23 model

                4 ->
                    lisaline24 model

                5 ->
                    lisaline25 model

                8 ->
                    lisaline28 model

                _ ->
                    model

        3 ->
            case lisaline.plot of
                1 ->
                    lisaline31 model

                2 ->
                    lisaline32 model

                3 ->
                    lisaline33 model

                8 ->
                    lisaline38 model

                _ ->
                    model

        4 ->
            case lisaline.plot of
                1 ->
                    lisaline41 model

                2 ->
                    lisaline42 model

                3 ->
                    lisaline43 model

                4 ->
                    lisaline44 model

                5 ->
                    lisaline45 model

                8 ->
                    lisaline48 model

                _ ->
                    model

        5 ->
            case lisaline.plot of
                1 ->
                    lisaline51 model

                2 ->
                    lisaline52 model

                3 ->
                    lisaline53 model

                4 ->
                    lisaline54 model

                5 ->
                    lisaline55 model

                9 ->
                    lisaline59 model

                _ ->
                    model

        12 ->
            case lisaline.plot of
                1 ->
                    lisaline121 model

                8 ->
                    lisaline128 model

                9 ->
                    lisaline129 model

                _ ->
                    model

        13 ->
            case lisaline.plot of
                1 ->
                    lisaline131 model

                2 ->
                    lisaline132 model

                8 ->
                    lisaline138 model

                9 ->
                    lisaline139 model

                _ ->
                    model

        14 ->
            case lisaline.plot of
                1 ->
                    lisaline141 model

                2 ->
                    lisaline142 model

                3 ->
                    lisaline143 model

                4 ->
                    lisaline144 model

                5 ->
                    lisaline144 model

                _ ->
                    model

        21 ->
            badend1 model

        22 ->
            lisaend1 model

        23 ->
            felixend model

        24 ->
            lisaend2 model

        25 ->
            lisaend3 model

        _ ->
            model


lisaline11 : Model -> Model
lisaline11 model =
    let
        { lisaline } =
            model
    in
    if collideEUH model then
        { model | lisaline = { lisaline | plot = 2 }, gameState = InDialog }

    else
        model


lisaline12 : Model -> Model
lisaline12 model =
    let
        { lisaline, lisa, cat, felix } =
            model

        blo =
            model.blockSize

        newCat =
            { cat | pos = ( 37 * blo, 40 * blo ) }

        newfelix =
            { felix | pos = ( -100, -100 ) }
    in
    if model.gameState == InDialog then
        model

    else if distanceFelix model < 0.7 then
        { model
            | lisaline = { lisaline | day = 21 }
            , gameState = InDialog
            , dialogList = Array.fromList dialogListBad
            , dialogNum = 0
        }

    else if distanceFelix model > 10 then
        { model | lisaline = { lisaline | plot = 3 }, gameState = InDialog, cat = newCat, felix = newfelix }

    else
        updateFelix model


lisaline13 : Model -> Model
lisaline13 model =
    let
        { lisaline, lisa, cat } =
            model

        blo =
            model.blockSize

        newlisa =
            { lisa | pos = ( 37 * blo, 38.5 * blo ) }
    in
    if distanceLisa model < 0.8 then
        { model | lisaline = { lisaline | plot = 4 }, gameState = InDialog }

    else
        { model | lisa = newlisa }


lisaline14 : Model -> Model
lisaline14 model =
    let
        { lisaline, lisa, cat, felix } =
            model

        ( a, b ) =
            lisa.pos

        blo =
            model.blockSize

        v =
            blo / 8

        newlisa =
            if b > 26 * blo then
              { lisa | pos = ( a, b - v ), dir = ( 0, -1 ) }
            else
              lisa
    in
    if model.gameState == InDialog then
        model

    else if lisa /= newlisa then
        { model | lisa = newlisa }

    else if distanceLisa model > 0.8 && lisa == newlisa then
        { model | lisa = { lisa | dir = ( 0, 0 ) } }

    else
        { model | lisaline = { lisaline | plot = 5 }, gameState = InDialog, felix = { felix | pos = ( -100, -100 ) } }


lisaline15 : Model -> Model
lisaline15 model =
    let
        { lisaline, lisa, cat } =
            model

        ( a, b ) =
            lisa.pos

        blo =
            model.blockSize

        v =
            blo / 8

        newlisa =
            if a > 25 * blo then
                { lisa | pos = ( a - v, b ), dir = ( -1, 0 ) }

            else if b > 6 * blo then
                { lisa | pos = ( a, b - v ), dir = ( 0, -1 ) }

            else if a > 9 * blo then
                { lisa | pos = ( a - v, b ), dir = ( -1, 0 ) }

            else if b > 5 * blo then
                { lisa | pos = ( a, b - v ), dir = ( 0, -1 ) }

            else
                lisa
    in
    if model.gameState == InDialog then
        model

    else if lisa /= newlisa then
        { model | lisa = newlisa }

    else if distanceLisa model > 0.8 then
        { model | lisa = { lisa | dir = ( 0, 0 ) } }

    else
        { model | lisaline = { lisaline | plot = 6 }, gameState = InDialog }


lisaline16 : Model -> Model
lisaline16 model =
    let
        { lisaline, lisa, cat } =
            model

        ( a, b ) =
            lisa.pos

        blo =
            model.blockSize

        v =
            blo / 8

        newlisa =
            if a < 13 * blo then
                { lisa | pos = ( a + v, b ), dir = ( 1, 0 ) }

            else if b < 9 * blo then
                { lisa | pos = ( a, b + v ), dir = ( 0, 1 ) }

            else
                lisa
    in
    if model.gameState == InDialog then
        model

    else if lisa /= newlisa then
        { model | lisa = newlisa }

    else if distanceLisa model > 0.8 then
        { model | lisa = { lisa | dir = ( 0, 0 ) } }

    else
        { model | lisaline = { lisaline | plot = 8 }, gameState = InDialog }


lisaline18 : Model -> Model
lisaline18 model =
    let
        { lisaline, lisa, cat } =
            model

        ( a, b ) =
            lisa.pos

        blo =
            model.blockSize

        v =
            blo / 5

        newlisa =
            { lisa | pos = ( a + v, b ), dir = ( 1, 0 ) }
    in
    if model.gameState == InDialog then
        model

    else if collideHome model then
        { model | lisaline = { lisaline | plot = 9 }, lisa = newlisa, gameState = InDialog }

    else
        { model | lisa = newlisa }


lisaline19 : Model -> Model
lisaline19 model =
    let
        { lisaline, cat } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | gameState = Felix 1, lisaline = { lisaline | plot = 1, day = 2 } }


lisaline21 : Model -> Model
lisaline21 model =
    let
        { lisaline, lisa } =
            model

        newlisa =
            { lisa | pos = ( -100, -100 ) }

        newMap =
            model.map
                ++ [ Block "book2" True False Nothing Nothing ( 38, 43 ) ]
                ++ [ Block "book1" True False Nothing Nothing ( 7, 47 ) ]
    in
    if collideEUH model then
        { model | lisaline = { lisaline | plot = 2 }, gameState = InDialog, lisa = newlisa, map = newMap }

    else
        { model | lisa = newlisa }


lisaline22 : Model -> Model
lisaline22 model =
    let
        { lisaline, felix, cat, lisa } =
            model

        blo =
            model.blockSize

        newlisa =
            { lisa | pos = ( 21 * blo, 9 * blo ) }

        newmap =
            List.filter
                (\block ->
                    block.blockType
                        /= "book2"
                        && block.blockType
                        /= "book1"
                )
                model.map
    in
    if collideBook2 model then
        { model | lisaline = { lisaline | plot = 3 }, gameState = InDialog, lisa = newlisa, map = newmap }

    else if collidePoint model ( 7, 47 ) then
        { model
            | lisaline = { lisaline | day = 12, plot = 1 }
            , gameState = InDialog
            , lisa = newlisa
            , map = newmap
            , dialogList = Array.fromList dialoglistFelix
            , dialogNum = 0
        }

    else
        model


lisaline23 : Model -> Model
lisaline23 model =
    let
        { lisaline, cat } =
            model
    in
    if distanceLisa model < 0.8 then
        { model | lisaline = { lisaline | plot = 4 }, gameState = InDialog }

    else
        model


lisaline24 : Model -> Model
lisaline24 model =
    let
        { lisaline, lisa, cat } =
            model

        ( a, b ) =
            lisa.pos

        blo =
            model.blockSize

        v =
            blo / 8

        newlisa =
            if a < 25 * blo then
                { lisa | pos = ( a + v, b ), dir = ( 1, 0 ) }

            else if b < 26 * blo then
                { lisa | pos = ( a, b + v ), dir = ( 0, 1 ) }

            else if a < 36 * blo then
                { lisa | pos = ( a + v, b ), dir = ( 1, 0 ) }

            else
                lisa
    in
    if model.gameState == InDialog then
        model

    else if lisa /= newlisa then
        { model | lisa = newlisa }

    else if  not (collideLibrary model )then
        {model|lisa = { lisa | dir = ( 0, 0 ) }}

    else
        { model | lisaline = { lisaline | plot = 5 }, gameState = InDialog }


lisaline25 : Model -> Model
lisaline25 model =
    let
        { lisaline, cat, lisa } =
            model

        newlisa =
            { lisa | pos = ( -100, -100 ) }
    in
    if model.gameState == InDialog then
        model

    else
        { model | gameState = Lisa 1, lisaline = { lisaline | plot = 8 }, lisa = newlisa }


lisaline28 : Model -> Model
lisaline28 model =
    let
        { lisaline, lisa, cat } =
            model
    in
    if model.gameState == InDialog then
        model

    else if collideHome model then
        { model | lisaline = { lisaline | plot = 1, day = 3 }, gameState = InDialog }

    else
        model


lisaline31 : Model -> Model
lisaline31 model =
    let
        { lisaline } =
            model
    in
    if collideEUH model then
        { model | lisaline = { lisaline | plot = 2 }, gameState = InDialog }

    else
        model


lisaline32 : Model -> Model
lisaline32 model =
    let
        { lisaline, lisa } =
            model

        newlisa =
            { lisa | pos = ( -100, -100 ) }
    in
    if model.gameState == InDialog then
        model

    else if collideLab model then
        { model | lisaline = { lisaline | plot = 3 }, gameState = InDialog, lisa = newlisa }

    else
        model


lisaline33 : Model -> Model
lisaline33 model =
    let
        { lisaline, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | gameState = Lisa 2, lisaline = { lisaline | plot = 8 } }


lisaline38 : Model -> Model
lisaline38 model =
    let
        { lisaline, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else if collideHome model then
        { model | lisaline = { lisaline | plot = 1, day = 4 }, gameState = InDialog }

    else
        model


lisaline41 : Model -> Model
lisaline41 model =
    let
        { lisaline, lisa } =
            model

        blo =
            model.blockSize

        newlisa =
            { lisa | pos = ( 28 * blo, 39 * blo ) }
    in
    if collideEUH model then
        { model | lisaline = { lisaline | plot = 2 }, gameState = InDialog, lisa = newlisa }

    else
        model


lisaline42 : Model -> Model
lisaline42 model =
    let
        { lisaline } =
            model

        newmap =
            model.map ++ [ Block "professor" True False Nothing Nothing ( 42, 41 ) ]
    in
    if model.gameState == InDialog then
        model

    else if distanceLisa model < 0.8 then
        { model | lisaline = { lisaline | plot = 3 }, gameState = InDialog, map = newmap }

    else
        model


lisaline43 : Model -> Model
lisaline43 model =
    let
        { lisaline, lisa } =
            model

        ( a, b ) =
            lisa.pos

        blo =
            model.blockSize

        v =
            blo / 6

        newlisa =
            if b < 45 * blo && a < 31 * blo then
                { lisa | pos = ( a, b + v ), dir = ( 0, 1 ) }

            else if a < 45 * blo && b > 43 * blo then
                { lisa | pos = ( a + v, b ), dir = ( 1, 0 ) }

            else if b > 41 * blo then
                { lisa | pos = ( a, b - v ), dir = ( 0, -1 ) }

            else if a > 41 * blo then
                { lisa | pos = ( a - v, b ), dir = ( -1, 0 ) }

            else
                lisa

    in
    if model.gameState == InDialog then
        model

    else if  lisa /= newlisa then
        {model|lisa = newlisa}
    else if not (collidePoint model ( 42, 41 ) )then
        { model | lisa = {lisa|dir = (0,0)} }

    else
        { model
            | lisaline = { lisaline | plot = 4 }
            , gameState = InDialog
            , lisa = newlisa
        }

lisaline44 : Model -> Model
lisaline44 model =
    let
        { lisaline, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else if collideLibrary model then
        { model | lisaline = { lisaline | plot = 5 }, gameState = InDialog }

    else
        model


lisaline45 : Model -> Model
lisaline45 model =
    let
        { lisaline, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | gameState = Lisa 4, lisaline = { lisaline | plot = 8 } }


lisaline48 : Model -> Model
lisaline48 model =
    let
        { lisaline, lisa, cat } =
            model

        newlisa =
            { lisa | pos = ( -100, -100 ) }

        newmap =
            List.filter (\blo -> blo.blockType /= "professor") model.map
    in
    if model.gameState == InDialog then
        model

    else if collideHome model then
        { model
            | lisaline = { lisaline | plot = 1, day = 5 }
            , gameState = InDialog
            , lisa = newlisa
            , map = newmap
        }

    else
        model


lisaline51 : Model -> Model
lisaline51 model =
    let
        { lisaline, lisa, cat } =
            model

        blo =
            model.blockSize

        ( x, y ) =
            cat.pos

        newlisa =
            { lisa | pos = ( x, y + blo * 1.5 ) }
    in
    if model.gameState == InDialog then
        model

    else if collideEUH model then
        { model | lisaline = { lisaline | plot = 2 }, gameState = InDialog, lisa = newlisa }

    else
        model


lisaline52 : Model -> Model
lisaline52 model =
    let
        { lisaline, lisa, cat } =
            model

        newmap =
            model.map
                ++ [ Block "orangeCheck" True False Nothing Nothing ( 46, 1 ) ]
                ++ [ Block "orangeCheck" True False Nothing Nothing ( 45, 44 ) ]
                ++ [ Block "orangeCheck" True False Nothing Nothing ( 4, 18 ) ]
                ++ [ Block "orangeCheck" True False Nothing Nothing ( 19, 40 ) ]
    in
    if model.gameState == InDialog then
        model

    else if distanceLisa model < 0.8 then
        { model
            | lisaline = { lisaline | plot = 3 }
            , gameState = InDialog
            , map = newmap
        }

    else
        model


lisaline53 : Model -> Model
lisaline53 model =
    let
        existOrange position block =
            block.blockType == "orangeCheck" && block.pos == position

        noOrange position block =
            not (existOrange position block)

        { lisaline, lisa, cat } =
            model

        --flag means the corresponding block exists
        flag1 =
            List.filter (existOrange ( 46, 1 )) model.map
                |> List.isEmpty
                |> not

        flag2 =
            List.filter (existOrange ( 45, 44 )) model.map
                |> List.isEmpty
                |> not

        flag3 =
            List.filter (existOrange ( 4, 18 )) model.map
                |> List.isEmpty
                |> not

        flag4 =
            List.filter (existOrange ( 19, 40 )) model.map
                |> List.isEmpty
                |> not

        newmap =
            if collidePoint model ( 46, 1 ) && flag1 then
                List.filter (noOrange ( 46, 1 )) model.map
                    |> (++) [ Block "greenCheck" True False Nothing Nothing ( 46, 1 ) ]

            else if collidePoint model ( 45, 44 ) && flag2 then
                List.filter (noOrange ( 45, 44 )) model.map
                    |> (++) [ Block "greenCheck" True False Nothing Nothing ( 45, 44 ) ]

            else if collidePoint model ( 4, 18 ) && flag3 then
                List.filter (noOrange ( 4, 18 )) model.map
                    |> (++) [ Block "greenCheck" True False Nothing Nothing ( 4, 18 ) ]

            else if collidePoint model ( 19, 40 ) && flag4 then
                List.filter (noOrange ( 19, 40 )) model.map
                    |> (++) [ Block "greenCheck" True False Nothing Nothing ( 19, 40 ) ]

            else
                model.map
    in
    if model.gameState == InDialog then
        model

    else if flag1 || flag2 || flag3 || flag4 then
        { model | map = newmap }

    else if distanceLisa model < 0.8 then
        { model
            | lisaline = { lisaline | plot = 4 }
            , gameState = InDialog
            , map = List.filter (\blo -> blo.blockType /= "greenCheck") model.map
        }

    else
        model


lisaline54 : Model -> Model
lisaline54 model =
    let
        { lisaline, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else if collideLibrary model then
        { model | lisaline = { lisaline | plot = 5 }, gameState = InDialog }

    else
        model


lisaline55 : Model -> Model
lisaline55 model =
    let
        { lisaline, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | gameState = Lisa 3, lisaline = { lisaline | plot = 9 } }


lisaline59 : Model -> Model
lisaline59 model =
    let
        { lisaline, lisa, cat } =
            model

        { dreamMod } =
            model

        skip =
            dreamMod.skipTime

        end =
            if skip == 0 then
                22

            else if skip < 3 then
                24

            else
                25

        newlisa =
            { lisa | pos = ( -100, -100 ) }
    in
    if model.gameState == InDialog then
        model

    else if collideHome model then
        if end == 22 then
            { model
                | lisaline = { lisaline | day = end, plot = 1 }
                , gameState = InDialog
                , lisa = newlisa
            }

        else if end == 24 then
            { model
                | lisaline = { lisaline | day = end, plot = 1 }
                , gameState = InDialog
                , lisa = newlisa
                , dialogList = Array.fromList dialogListLisa2
            }

        else
            { model
                | lisaline = { lisaline | day = end, plot = 1 }
                , gameState = InDialog
                , lisa = newlisa
                , dialogList = Array.fromList dialogListLisa3
            }

    else
        model


lisaline121 : Model -> Model
lisaline121 model =
    let
        { lisaline, cat } =
            model
    in
    if model.gameState == InDialog then
        model

    else if collidePoint model ( 25, 47 ) then
        { model | gameState = InDialog, lisaline = { lisaline | plot = 8 } }

    else
        model


lisaline128 : Model -> Model
lisaline128 model =
    let
        { lisaline, felix, cat, lisa } =
            model

        blo =
            model.blockSize

        newlisa =
            { lisa | pos = ( -100, -100 ) }

        newfelix =
            { felix | pos = ( 12 * blo, 10 * blo ) }
    in
    if model.gameState == InDialog then
        model

    else if collideHome model then
        { model
            | lisaline = { lisaline | plot = 9 }
            , gameState = InDialog
            , felix = newfelix
            , lisa = newlisa
        }

    else
        model


lisaline129 : Model -> Model
lisaline129 model =
    let
        { lisaline, cat } =
            model

        newmap =
            model.map ++ [ Block "robot" True False Nothing Nothing ( 4, 29 ) ]
    in
    if model.gameState == InDialog then
        model

    else
        { model | gameState = Felix 2, lisaline = { lisaline | plot = 1, day = 13 }, map = newmap }


lisaline131 : Model -> Model
lisaline131 model =
    let
        { lisaline, felix, cat, lisa } =
            model

        newmap =
            List.filter (\block -> block.blockType /= "robot") model.map
    in
    if model.gameState == InDialog then
        model

    else if collidePoint model ( 4, 29 ) then
        { model
            | lisaline = { lisaline | plot = 2 }
            , gameState = InDialog
            , map = newmap
        }

    else
        model


lisaline132 : Model -> Model
lisaline132 model =
    let
        { lisaline, felix, cat, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else if distanceFelix model < 0.8 then
        { model
            | lisaline = { lisaline | plot = 8 }
            , gameState = InDialog
        }

    else
        model


lisaline138 : Model -> Model
lisaline138 model =
    let
        { lisaline, felix, cat, lisa } =
            model
    in
    if model.gameState == InDialog then
        model

    else if collideHome model then
        { model
            | lisaline = { lisaline | plot = 9 }
            , gameState = InDialog
        }

    else
        model


lisaline139 : Model -> Model
lisaline139 model =
    let
        { lisaline, cat } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | gameState = Felix 3, lisaline = { lisaline | plot = 1, day = 14 } }


lisaline141 : Model -> Model
lisaline141 model =
    let
        { lisaline, cat, felix } =
            model

        blo =
            model.blockSize

        newmap =
            List.filter (\block -> block.blockType /= "boat1" || block.pos /= ( 39, 6 )) model.map

        newfelix =
            { felix | pos = ( blo * 26, blo * 14 ) }
    in
    if model.gameState == InDialog then
        model

    else if collidePoint model ( 39, 7 ) then
        { model
            | gameState = InDialog
            , lisaline = { lisaline | plot = 2 }
            , map = newmap
            , felix = newfelix
        }

    else
        model


lisaline142 : Model -> Model
lisaline142 model =
    let
        { lisaline, cat, felix } =
            model
    in
    if model.gameState == InDialog then
        model

    else if distanceFelix model < 0.8 then
        { model | gameState = InDialog, lisaline = { lisaline | plot = 3 } }

    else
        model


lisaline143 : Model -> Model
lisaline143 model =
    let
        { lisaline, cat, felix } =
            model

        blo =
            model.blockSize

        ( a, b ) =
            felix.pos

        newmap =
            model.map ++ [ Block "boat1" True False Nothing Nothing ( 47, 14 ) ]

        changeMove block =
            if Tuple.first block.pos == 47 then
                { block | movable = True }

            else
                block

        newmap2 =
            List.map changeMove newmap

        newfelix =
            if a < 46 * blo then
                { felix | pos = ( a + v, b ), dir = ( 1, 0 ) }

            else
                { felix | dir = ( 0, 0 ) }

        v =
            blo / 6

        flag =
            List.filter (\bloc -> bloc.blockType == "boat1" && bloc.pos == ( 47, 14 )) model.map
                |> List.isEmpty
    in
    if model.gameState == InDialog then
        model

    else if a > 46 * blo && flag then
        { model | map = newmap }

    else if collidePoint model ( 46, 14 ) then
        { model
            | gameState = InDialog
            , lisaline = { lisaline | plot = 4 }
            , map = newmap2
        }

    else
        { model | felix = newfelix }


lisaline144 : Model -> Model
lisaline144 model =
    let
        { lisaline, cat, felix } =
            model

        newmap =
            List.filter (\blo -> blo.blockType /= "boat1" || blo.pos /= ( 47, 14 )) model.map

        flag =
            List.filter (\bloc -> bloc.blockType == "boat1" && bloc.pos == ( 47, 14 )) model.map
                |> List.isEmpty
                |> not
    in
    if model.gameState == InDialog then
        model

    else if flag then
        { model | map = newmap }

    else if collidePoint model ( 47, 47 ) then
        { model | gameState = InDialog, lisaline = { lisaline | day = 23 } }

    else
        model


badend1 model =
    if model.gameState == InDialog then
        model

    else
        initial


lisaend1 model =
    let
        { dreamMod } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | runstate = WinGame 1 }


lisaend2 model =
    let
        { dreamMod } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | runstate = WinGame 3 }


lisaend3 model =
    let
        { dreamMod } =
            model
    in
    if model.gameState == InDialog then
        model

    else
        { model | runstate = WinGame 4 }


felixend model =
    if model.gameState == InDialog then
        model

    else
        { model | runstate = WinGame 2 }



--------------------------------------------------------------------------------------------
--Functions below are all used for storyline progress


distanceFelix : Model -> Float
distanceFelix model =
    let
        { felix, cat } =
            model

        ( a, b ) =
            cat.pos

        ( c, d ) =
            felix.pos

        blo =
            model.blockSize

        x =
            (a - c) / blo

        y =
            (b - d) / blo
    in
    sqrt (x * x + y * y)


distanceLisa : Model -> Float
distanceLisa model =
    let
        { lisa, cat } =
            model

        ( a, b ) =
            cat.pos

        ( c, d ) =
            lisa.pos

        blo =
            model.blockSize

        x =
            (a - c) / blo

        y =
            (b - d) / blo
    in
    sqrt (x * x + y * y)


collideEUH : Model -> Bool
collideEUH model =
    let
        { cat } =
            model

        blo =
            model.blockSize

        a =
            Tuple.first cat.pos / blo

        b =
            Tuple.second cat.pos / blo
    in
    a > 27.7 && a < 35 && b > 35 && b < 45


collideHome : Model -> Bool
collideHome model =
    let
        { cat } =
            model

        blo =
            model.blockSize

        a =
            Tuple.first cat.pos / blo

        b =
            Tuple.second cat.pos / blo
    in
    a > 12.7 && a < 13.2 && b > 11.7 && b < 12.2


collideLibrary : Model -> Bool
collideLibrary model =
    let
        { cat } =
            model

        blo =
            model.blockSize

        a =
            Tuple.first cat.pos / blo

        b =
            Tuple.second cat.pos / blo
    in
    a > 34 && a < 38 && b > 24 && b < 26.3


collideBook2 : Model -> Bool
collideBook2 model =
    let
        { cat } =
            model

        blo =
            model.blockSize

        a =
            Tuple.first cat.pos / blo

        b =
            Tuple.second cat.pos / blo
    in
    a > 37.5 && a < 38.5 && b > 42.5 && b < 43.5


collideLab : Model -> Bool
collideLab model =
    let
        { cat } =
            model

        blo =
            model.blockSize

        a =
            Tuple.first cat.pos / blo

        b =
            Tuple.second cat.pos / blo
    in
    a > 37.5 && a < 44 && b > 34 && b < 45


collidePoint : Model -> ( Int, Int ) -> Bool
collidePoint model ( x, y ) =
    let
        { cat } =
            model

        blo =
            model.blockSize

        a =
            Tuple.first cat.pos / blo

        b =
            Tuple.second cat.pos / blo
    in
    a
        > toFloat x
        - 0.5
        && a
        < toFloat x
        + 0.5
        && b
        > toFloat y
        - 0.5
        && b
        < toFloat y
        + 0.5
