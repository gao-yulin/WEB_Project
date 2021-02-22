module Lisa1.Update exposing (update)

--Lisa games by Chen: If lisa games have bugs, fix it in this file.

import Map exposing (..)
import Model exposing (..)
import Tools exposing (getPosInt, updateCamera, updateframe)



-----------------Chen: update for Lisa dream game


update : Model -> Float ->  Model
update model time =
     model
        |> initDreamLisa
        |> updateTime
        |> updateframe time
        |> updateWalkability
        |> updateBricks
        |> dreamCatWalk
        |> dream3updateBricks
        |> lisaframe time
        |> updateCamera
        |> checkWin
        |> winBgm
        |> returnToMap



initDreamLisa : Model -> Model
initDreamLisa model =
    let
        { dreamMod, cat } =
            model
        blocksiz = Basics.min (Tuple.first model.size // 20) (Tuple.second model.size // 15)
        newCat =
            case model.gameState of
                Lisa 1 ->
                    { cat | dir = ( 0, 0 ), pos = ( toFloat (5 * blocksiz), toFloat (8 * blocksiz) ), size = (blocksiz , blocksiz ) }

                Lisa 2 ->
                    { cat | dir = ( 0, 0 ), pos = ( toFloat (5 * blocksiz), toFloat (8 * blocksiz) ), size = (blocksiz , blocksiz ) }

                Lisa 3 ->
                    { cat | dir = ( 0, 0 ), pos = ( toFloat (5 * blocksiz), toFloat (8 * blocksiz) ), size = (blocksiz , blocksiz ) }

                _ ->
                    { cat | dir = ( 0, 0 ), pos = ( toFloat (5 * blocksiz), toFloat (8 * blocksiz) ), size = (blocksiz , blocksiz ) }

        newMap =
            case model.gameState of
                Lisa 1 ->
                    mapLisa1

                Lisa 2 ->
                    mapLisa2

                Lisa 3 ->
                    mapLisa3

                Lisa 4 ->
                    mapLisa4

                _ ->
                    mapDorm

        newBgm =
            case model.gameState of
                Lisa 1 ->
                    "./res/bgm_relax.mp3"

                Lisa 2 ->
                    "./res/かえるのピアノ.mp3"

                Lisa 3 ->
                    "./res/Cat_life.mp3"

                Lisa 4 ->
                    "./res/昼下がり気分.mp3"

                _ ->
                    "./res/bgm_day.mp3"

        newDreamMod =
            { dreamMod | dreamState = Playing, dreamTime = 0, dreamMap = newMap }
    in
    if dreamMod.dreamState == InitDream then
        { model | cat = newCat, dreamMod = newDreamMod, bgm = newBgm, block = (20,15), blockSize =toFloat blocksiz}

    else
        model


lisaframe : Float ->Model ->  Model
lisaframe delta model =
    let
        {lisa} = model
        t = lisa.frametime + delta/1000
        frametime = if (t < 0.2) then t else 0
        f = lisa.frame
        nextf = if (t /= frametime ) then f+1 else f
        nextframe = if (remainderBy 4 nextf==0) then 4 else remainderBy 4 nextf
    in
    {model | lisa = { lisa |frametime = frametime, frame = nextframe }}

updateTime : Model -> Model
updateTime model =
    let
        { dreamMod } =
            model

        newTime =
            dreamMod.dreamTime + 1
    in
    { model | dreamMod = { dreamMod | dreamTime = newTime } }


updateWalkability : Model -> Model
updateWalkability model =
    let
        { dreamMod } =
            model
    in
    if dreamMod.dreamTime > 5 then
        { model | dreamMod = { dreamMod | moveFlag = True } }

    else
        model


getDreamBlock : Model -> ( Int, Int ) -> Block
getDreamBlock model pos =
    List.filter (\blo -> blo.pos == pos) model.dreamMod.dreamMap
        |> List.head
        |> Maybe.withDefault nullBlock


nullBlock : Block
nullBlock =
    Block "null" True False Nothing Nothing ( -1, -1 )


dreamCatWalk : Model -> Model
dreamCatWalk model =
    let
        { cat, dreamMod } =
            model

        bloSize =
            model.blockSize

        ( x, y ) =
            cat.pos

        ( a, b ) =
            getPosInt x y bloSize

        ( dx, dy ) =
            cat.dir

        newX =
            (toFloat a + dx) * bloSize

        newY =
            (toFloat b + dy) * bloSize

        nextPos =
            ( a + floor dx, b + floor dy )

        nextBlock =
            getDreamBlock model nextPos

        nextNextBlock =
            getDreamBlock model ( a + 2 * floor dx, b + 2 * floor dy )

        nextNextNextBlock =
            getDreamBlock model ( a + 3 * floor dx, b + 3 * floor dy )

        checkBlo m n string =
            (getDreamBlock model ( m, n )).blockType == string

        catMove =
            if nextBlock.pushable then
                if nextNextBlock.blockType == "null" || nextNextBlock.blockType == "bomb" then
                    True

                else if
                    nextNextBlock.pushable
                        && nextNextNextBlock.blockType
                        == "null"
                        && nextNextBlock.blockType
                        /= "null"
                        && model.gameState
                        /= Lisa 4
                        && model.gameState
                        /= Lisa 3
                then
                    True

                else
                    False

            else if nextBlock.movable then
                True

            else
                False

        newBlock =
            { nextBlock | pos = ( a + 2 * floor dx, b + 2 * floor dy ) }

        newNewBlock =
            { nextNextBlock | pos = ( a + 3 * floor dx, b + 3 * floor dy ) }

        deleteBlocks =
            --only for dream3
            if nextNextBlock.blockType == "bomb" then
                List.filter (\blo -> blo.pos /= nextPos) dreamMod.dreamMap

            else
                dreamMod.dreamMap

        nextPos4 =
            ( a + floor dx + 9, b + floor dy )

        newBlock4 =
            { nextBlock | pos = ( a + 2 * floor dx + 9, b + 2 * floor dy ) }

        movedBlocks =
            if nextBlock.pushable == False then
                deleteBlocks

            else if model.gameState == Lisa 4 && checkBlo 9 8 "equal" then
                List.filter (\blo -> blo.pos /= nextPos && blo.pos /= nextPos4) deleteBlocks
                    ++ [ newBlock, newBlock4 ]

            else if
                nextNextBlock.pushable
                    && nextNextNextBlock.blockType
                    == "null"
                    && nextNextBlock.blockType
                    /= "null"
                    && model.gameState
                    /= Lisa 4
                    && model.gameState
                    /= Lisa 3
            then
                List.filter (\blo -> blo.pos /= nextPos && blo.pos /= ( a + 2 * floor dx, b + 2 * floor dy )) deleteBlocks
                    ++ [ newBlock, newNewBlock ]

            else if nextNextBlock.blockType == "bomb" then
                List.filter (\blo -> blo.pos /= nextPos) dreamMod.dreamMap

            else
                List.filter (\blo -> blo.pos /= nextPos) deleteBlocks
                    ++ [ newBlock ]
    in
    if catMove == False || dreamMod.dreamState == Win then
        model

    else if dreamMod.moveFlag then
        { model
            | cat = { cat | pos = ( newX, newY ), dir = ( 0, 0 ) }
            , dreamMod =
                { dreamMod | moveFlag = False, dreamTime = 0, dreamMap = movedBlocks }
        }

    else
        model


checkWin : Model -> Model
checkWin model =
    case model.gameState of
        Lisa 1 ->
            checkWin1 model

        Lisa 2 ->
            checkWin2 model

        Lisa 3 ->
            checkWin3 model

        Lisa 4 ->
            checkWin4 model

        _ ->
            model


checkWin1 : Model -> Model
checkWin1 model =
    let
        { dreamMod } =
            model

        blockE =
            getDreamBlock model ( 6, 5 )

        blockC =
            getDreamBlock model ( 7, 5 )

        blockT =
            getDreamBlock model ( 8, 5 )

        blockO =
            getDreamBlock model ( 9, 5 )

        winFlag =
            blockE.blockType
                == "E"
                && blockC.blockType
                == "C"
                && blockT.blockType
                == "T"
                && blockO.blockType
                == "O"
    in
    if winFlag then
        { model | dreamMod = { dreamMod | dreamState = Win } }

    else
        model


checkWin2 : Model -> Model
checkWin2 model =
    let
        { dreamMod } =
            model

        winPos =
            [ ( 9, 4 ) ] ++ line 5 4 6 ++ line 10 4 7 ++ line 15 4 9

        winBlocks =
            List.map (getDreamBlock model) winPos

        winFlag =
            List.all (\blo -> blo.blockType == "weigh") winBlocks
    in
    if winFlag then
        { model | dreamMod = { dreamMod | dreamState = Win } }

    else
        model


dream3updateBricks : Model -> Model
dream3updateBricks model =
    let
        { dreamMod } =
            model

        changeBlock blo =
            if blo.blockType == "wall1" then
                { blo | movable = True }

            else
                blo

        changeBlockBack blo =
            if blo.blockType == "wall1" then
                { blo | movable = False }

            else
                blo

        newBlocks =
            List.map changeBlock dreamMod.dreamMap

        newBlocksBack =
            List.map changeBlockBack dreamMod.dreamMap

        checkBlock x y string =
            (getDreamBlock model ( x, y )).blockType == string

        subFlag =
            checkBlock 2 10 "U"
                && checkBlock 3 10 "B"
                && checkBlock 4 10 "S"
                && checkBlock 5 10 "C"
                && checkBlock 6 10 "R"
                && checkBlock 7 10 "I"
                && checkBlock 8 10 "P"
                && checkBlock 9 10 "T"
                && checkBlock 10 10 "I"
                && checkBlock 11 10 "O"
                && checkBlock 12 10 "N"
                && checkBlock 13 10 "S"
    in
    if model.gameState /= Lisa 3 then
        model

    else if subFlag then
        { model | dreamMod = { dreamMod | dreamMap = newBlocks } }

    else
        { model | dreamMod = { dreamMod | dreamMap = newBlocksBack } }


checkWin3 : Model -> Model
checkWin3 model =
    let
        { dreamMod } =
            model

        blos =
            model.dreamMod.dreamMap

        isOutBlos blo =
            Tuple.first blo.pos > 15 || Tuple.second blo.pos > 11

        outBlos =
            List.filter isOutBlos blos

        exceptBomb =
            List.filter (\blo -> blo.blockType /= "bomb") outBlos
    in
    if List.isEmpty exceptBomb then
        { model | dreamMod = { dreamMod | dreamState = Win } }

    else
        model


updateBricks : Model -> Model
updateBricks model =
    let
        { dreamMod } =
            model

        checkBlo x y string =
            (getDreamBlock model ( x, y )).blockType == string

        selectBlos =
            List.filter
                (\blo ->
                    blo.blockType
                        /= "wall1"
                        && blo.blockType
                        /= "answer"
                        && blo.blockType
                        /= "equal"
                )
                model.dreamMod.dreamMap

        addBlos =
            List.map
                (\blo ->
                    { blo
                        | pos =
                            ( Tuple.first blo.pos + 9
                            , Tuple.second blo.pos
                            )
                    }
                )
                selectBlos

        newBlos =
            addBlos ++ model.dreamMod.dreamMap

        newBlosBack =
            List.filter
                (\blo ->
                    blo.blockType
                        == "wall1"
                        || blo.blockType
                        == "answer"
                        || blo.blockType
                        == "equal"
                        || Tuple.first blo.pos
                        < 9
                )
                model.dreamMod.dreamMap
    in
    if model.gameState /= Lisa 4 then
        model

    else if checkBlo 9 8 "equal" && model.dreamMod.dreamState == Playing then
        { model | dreamMod = { dreamMod | dreamMap = newBlos, dreamState = Mirror } }

    else if not (checkBlo 9 8 "equal") && model.dreamMod.dreamState == Mirror then
        { model | dreamMod = { dreamMod | dreamMap = newBlosBack } }

    else
        model


checkWin4 : Model -> Model
checkWin4 model =
    let
        { dreamMod } =
            model

        checkBlo x y string =
            (getDreamBlock model ( x, y )).blockType == string
    in
    if checkBlo 6 8 "E" && checkBlo 7 7 "X" && checkBlo 9 8 "equal" then
        { model | dreamMod = { dreamMod | dreamState = Win } }

    else
        model


returnToMap : Model -> Model
returnToMap model =
    let
        { cat, dreamMod, blockSize} =
            model
        blockSiz = toFloat (Basics.min (Tuple.first model.size // 48) (Tuple.second model.size // 48))
        blockss = (Basics.min (Tuple.first model.size // 48) (Tuple.second model.size // 48))
        newCat =

            if model.lisaline.day == 3 then
                { cat | pos = (  (38 * blockSiz),  (38 * blockSiz) ), size =( blockss, blockss) }

            else
                { cat | pos = ( (36 * blockSiz), (28 * blockSiz) ), size =( blockss, blockss)}

    in
    if dreamMod.dreamState /= Win then
        model

    else if dreamMod.dreamTime < 200 then
        model

    else
        { model
            | gameState = InDialog
            , cat = newCat
            , bgm = "./res/bgm_day.mp3"
            , dreamMod = { dreamMod | dreamState = InitDream}
            , camera = cat.pos
            , block = (48,48)
            , blockSize = blockSiz
        }

winBgm : Model -> Model
winBgm model =
    if model.dreamMod.dreamState == Win && model.bgm /= "./res/victory.mp3" then
        { model | bgm = "./res/victory.mp3" }

    else
        model
