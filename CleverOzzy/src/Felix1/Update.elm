module Felix1.Update exposing (update)

import Map exposing (..)
import Message exposing (Msg(..))
import Model exposing (..)
import Tools exposing (getPosInt, updateCamera, updateframe, getDreamBlock)

update : Model -> Float ->  Model
update model time =
    model
        |> initDreamFelix
        |> catWalk time
        |> updateFelix
        |> releaseBird
        |> roar
        |> updateGameTime time
        |> updateBird
        |> updateRefreshTime time
        |> updateframe time
        |> felixframe time
        |> checkWin
        |> winBgm
        |> returnToMap

roar : Model -> Model
roar model=
    let
        {chaser,cat} = model
    in
    if cat.magic == Roar then
        {model|chaser = List.map (affectFelix model) chaser, cat = {cat|magic = Norm}}
    else
        model

felixframe : Float ->Model ->  Model
felixframe delta model =
    let
        {felix} = model
        t = felix.frametime + delta/1000
        frametime = if (t < 0.2) then t else 0
        f = felix.frame
        nextf = if (t /= frametime ) then f+1 else f
        nextframe = if (remainderBy 4 nextf==0) then 4 else remainderBy 4 nextf
    in
    {model | felix = { felix |frametime = frametime, frame = nextframe }}

affectFelix : Model -> Npc -> Npc
affectFelix model feli=
    let
        (x,y) = model.cat.pos
        (xb,yb) = (x/model.blockSize,y/model.blockSize)
        (m,n) = feli.pos
        (mb,nb) = (m/model.blockSize, n/model.blockSize)
    in
    if (abs (mb - xb) <= 3) && (abs (yb - nb) <= 3) then
        {feli|controlled = True}
    else
        feli

checkWin : Model -> Model
checkWin model =
    let
        {dreamMod} = model
    in
    if model.refreshtime >= 30000 then
        { model | dreamMod = { dreamMod | dreamState = Win } }
    else if List.any (checkCollideWithNPC model) model.chaser then
        { model | dreamMod = { dreamMod | dreamState = Lose } }
    else
        model

updateGameTime : Float -> Model -> Model
updateGameTime delta model =
    --update gametime (for randomdir)
    if model.gametime <= 4000.0 then
        { model | gametime = model.gametime + delta }
    else { model | gametime = 0.0 }

updateRefreshTime : Float -> Model -> Model
updateRefreshTime  delta model =
    --update refreshtime (dream recording)
    { model | refreshtime = model.refreshtime + delta }

releaseBird : Model -> Model
releaseBird model =
    let
        {bird1,bird2,bird3,bird4,felix}= model
        (a,b) = felix.pos
    in
    if model.gameState == Felix 1 && model.refreshtime >= 3000.0 && model.gametime  == 1000 then
        {model | bird1 = {bird1 | pos = (a,b) },bird2 = {bird2 | pos = (a,b) }, bird3 = {bird3 | pos = (a,b) }
        , bird4 = {bird4 | pos =  (a,b) }}
    else model


updateBird : Model -> Model
updateBird model =
    let
        {bird1,bird2,bird3,bird4,felix}= model
        (x1,y1) = bird1.pos
        (a1,b1) = (x1 + 1 * model.blockSize / 40, y1)
        (x2,y2) =  bird2.pos
        (a2,b2) = (x2 - model.blockSize / 40,y2)
        (x3,y3) = bird3.pos
        (a3,b3) = (x3,y3 + model.blockSize / 40)
        (x4,y4) = bird4.pos
        (a4,b4) = (x4,y4 - model.blockSize / 40)
    in
    {model | bird1 = {bird1 | pos = (a1,b1) },bird2 = {bird2 | pos =  (a2,b2) }, bird3 = {bird3 | pos = (a3,b3) }
            , bird4 = {bird4 | pos = (a4,b4) }}

updateFelix : Model -> Model
updateFelix model =
    let
        {chaser} = model
    in
    {model | chaser = List.map (updatesingleFelix model) chaser}

updatesingleFelix : Model -> Npc ->  Npc
updatesingleFelix model feli=
    let
        {cat} = model
        (a,b) = cat.pos
        (c,d) = feli.pos
        distance = sqrt ( (a - c) *(a - c) + (b - d )* (b - d))
        clearsmall num = if (abs num < 0.15)then 0 else num
        dir = Tuple.mapBoth clearsmall clearsmall ((a - c) / distance  , (b - d) / distance)
        dx =  model.blockSize / 60 * Tuple.first dir
        dy =  model.blockSize / 60 * Tuple.second dir
        (x,y) = (dx + c, dy + d)
        (j,k) = (c - dx, d - dy)
        randir =model.felix.randir
        randx =  model.blockSize / 35 * Tuple.first randir
        randy =  model.blockSize / 35 * Tuple.second randir
        (rx,ry) = (randx + c, randy + d)
    in
    case model.gameState of
        Felix _ ->
            if not feli.controlled then
                if detectFelix model feli || eagleEye model then
                    if checkwalk model x y  then
                        { feli | dir = dir, pos = ( x , y), coll = False}
                    else if feli.coll then
                        { feli |  pos = ( c + Tuple.first feli.dir, d + Tuple.second feli.dir) }
                    else if checkwalk model (c+dx) d  then
                        { feli | dir = (dx,0) , pos = ( c + dx , d ), coll = True}
                    else if checkwalk model c (d+dy)  then
                        { feli | dir = (0,dy) , pos = ( c  , d + dy), coll = True}
                    else
                        feli
                else
                    if checkwalk model rx ry then
                        { feli | dir = randir, pos = ( rx , ry), coll = False}
                    else if feli.coll then
                        { feli |  pos = ( c , d ) }
                    else if checkwalk model (c+randx) d  then
                        { feli | dir = (randx,0) , pos = ( c + randx , d ), coll = True}
                    else if checkwalk model c (d+randy) then
                        { feli | dir = (0,randy) , pos = ( c  , d + randy), coll = True}
                    else
                        feli
            else
                if checkwalk model j k then
                    {feli|dir = dir, pos = (j,k), coll = False}
                else
                    {feli|controlled = False}
        _ ->
           feli
eagleEye : Model -> Bool
eagleEye model =
    let
        {bird1,bird2,bird3,bird4,felix,cat}= model
        (a,b)=cat.pos
        (x1,y1) = bird1.pos
        dist1 = sqrt ( (a - x1) *(a - x1) + (b - y1 )* (b - y1))
        (x2,y2) =  bird2.pos
        dist2 = sqrt ( (a - x2) *(a - x2) + (b - y2 )* (b - y2))
        (x3,y3) = bird3.pos
        dist3 = sqrt ( (a - x3) *(a - x3) + (b - y3 )* (b - y3))
        (x4,y4) = bird4.pos
        dist4 = sqrt ( (a - x4) *(a - x4) + (b - y4 )* (b - y4))
    in
    if dist1 <= 100 || dist2 <= 100 || dist3 <= 100 || dist4 <= 100 then
        True
    else False


detectFelix : Model -> Npc -> Bool
detectFelix model feli=
    let
        {cat} = model
        (a,b) = cat.pos
        (c,d) = feli.pos
        dist = sqrt ((a - c)*(a - c)+(b - d)*(b - d))
        (e,f) = feli.dir
        bloSize = model.blockSize
        (g,h) = (( a + bloSize / 2)/ bloSize, (b + bloSize / 2)/ bloSize)
        isInGrass = List.filter (\blo -> blo.pos == (floor(g),floor(h)) ) model.map
                  |> List.any (\blo -> blo.blockType == "grass1" || blo.blockType == "grass2")
    in
    if dist > 300  then
        False
    else if isInGrass then
        False
    else if (e * a + f * b) / ( sqrt (a * a + b * b) * sqrt (e * e + f * f) ) < 0 && (e * a + f * b) / ( sqrt (a * a + b * b) * sqrt (e * e + f * f) ) > 1 then
        False
    else
        True

catWalk : Float -> Model -> Model
catWalk delta model =
    let
            second = delta / 1000
            {cat} = model
            dir = cat.dir
            bloSize =   model.blockSize
            (x,y) = cat.pos
            v =
                if cat.food > 5 then
                1.5
                else if cat.food > 0 && cat.food < 6 then
                1
                else
                0.5
            dx = v * bloSize * second * Tuple.first dir
            dy = v * bloSize * second * Tuple.second dir
        in
        if checkwalk model (x+   dx) ( y+  dy) then
            {model | cat = {cat| pos = (x+  dx,y+  dy)}}
        else
            model

checkCollideWithNPC :Model -> Npc -> Bool
checkCollideWithNPC model npc=
    let
        {cat} = model
    in
        if (abs ((Tuple.first cat.pos) - (Tuple.first npc.pos))) <= toFloat (20)
        &&  (abs ((Tuple.second cat.pos) - (Tuple.second npc.pos))) <= toFloat (20)
        then
            True
        else
      False

checkwalk : Model -> Float -> Float -> Bool
checkwalk model x y=
    let
        bloSize = model.blockSize
        boundOut = x < 0 - bloSize/7  || x > bloSize * 20 - bloSize/1.22
            || y < 0  || y > bloSize * 15 - bloSize/1.22
        movable = collideBlocks model x y
          |> List.all (\blo -> blo.movable )
    in
    movable && (not boundOut)

collideBlocks: Model -> Float -> Float -> List Block
collideBlocks model x y =
    let
        bloSize = model.blockSize
        (a1,b1) = getPosInt (x + bloSize*0.25) (y + bloSize*0.25) bloSize
        (a2,b2) = getPosInt (x + bloSize*0.7) (y + bloSize*0.25) bloSize
        (a3,b3) = getPosInt (x + bloSize*0.25) (y + bloSize*0.6) bloSize
        (a4,b4) = getPosInt (x + bloSize*0.7) (y + bloSize*0.6) bloSize
    in
         List.map (getDreamBlock model) [(a1,b1),(a2,b2),(a3,b3),(a4,b4)]


initDreamFelix : Model -> Model
initDreamFelix model =
    let
        { dreamMod, cat} =
            model
        blocksiz = Basics.min (Tuple.first model.size // 20) (Tuple.second model.size // 15)
        newCat = { cat | dir = ( 0, 0 ), pos = ( toFloat (5 * blocksiz), toFloat (9 * blocksiz) ), size = (blocksiz,blocksiz) }

        newMap = mapDream1

        newBgm = "./res/Cat_life.mp3"

        newDreamMod =
            { dreamMod | dreamState = Playing, dreamTime = 0, dreamMap = newMap }
    in
    if dreamMod.dreamState == InitDream then
        { model | cat = newCat, dreamMod = newDreamMod, bgm = newBgm , chaser = initFelix model.felixDream
        , block = (20,15), blockSize =toFloat blocksiz}

    else
        model

initFelix : Int -> List (Npc)
initFelix num =
    let
        demo = { pos = ( 540, 200 ), size = ( 60, 60 ), dir = ( 0, 0 ), walk = False
                , quality = Just Chaser, coll = False, randir = ( 0, 0 ), controlled = False
                , frame = 1 , frametime = 0}
    in
    case num of
        1 ->
            [
            {demo|pos = (100, 200)}
            , {demo|pos = (300,450)}
            ]
        2 ->
            [
            {demo|pos = (100, 200)}
            , {demo|pos = (300,450)}
            , {demo|pos = (500,450)}
            , {demo|pos = (300,50)}
            ]
        3 ->
            [
            {demo|pos = (100, 200)}
            , {demo|pos = (300,450)}
            , {demo|pos = (500,450)}
            , {demo|pos = (300,50)}
            , {demo|pos = (400, 200)}
            , {demo | pos = (100,400)}
            ]
        4 ->
            [{demo|pos = (100, 200)}
            , {demo|pos = (300,450)}
            , {demo|pos = (500,450)}
            , {demo|pos = (300,50)}
            , {demo|pos = (400, 200)}
            , {demo | pos = (100,400)}
            , {demo|pos = (350,450)}
            , {demo|pos = (550,450)}
            , {demo|pos = (350,50)}
            , {demo|pos = (450, 200)}
            , {demo | pos = (150,400)}
            ]
        _ ->
            []

returnToMap : Model -> Model
returnToMap model =
    let
        { cat, dreamMod, blockSize, felixDream} =
            model
        blockSiz =Basics.min (Tuple.first model.size // 48) (Tuple.second model.size // 48)
        newCat =
            { cat | pos = ( 13 * (toFloat blockSiz), 11 * (toFloat blockSiz) ), size = (blockSiz,blockSiz) }
    in
    if dreamMod.dreamState /= Win && dreamMod.dreamState /= Lose then
        model
    else
        { model
            | gameState = InDialog
            , cat = newCat
            , bgm = "./res/bgm_day.mp3"
            , dreamMod = { dreamMod | dreamState = InitDream}
            , camera = cat.pos
            , block = (48,48)
            , blockSize =toFloat blockSiz
            , gametime = 0
            , refreshtime = 0
            , felixDream = felixDream + 1
        }


winBgm : Model -> Model
winBgm model =
    if model.dreamMod.dreamState == Win && model.bgm /= "./res/victory.mp3" then
        { model | bgm = "./res/victory.mp3" }

    else
        model
