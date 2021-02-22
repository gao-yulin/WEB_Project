module Felix exposing (..)

-- A legacy file

import Map exposing (Block)
import Model exposing (GameState(..), Model)
import Tools exposing (checkwalk)


releaseBird : Model -> Model
releaseBird model =
    let
        {bird1,bird2,bird3,bird4,felix}= model
        (a,b) = felix.pos
    in
    if model.gameState == InDream && model.refreshtime >= 3000.0 && model.gametime  == 1000 then
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

updateFish : Model -> Model
updateFish model =
    let
        {fish,cat}=model
        (a,b) = cat.pos
        (x,y) = fish.pos
        dist = sqrt ( (a - x) *(a - x) + (b - y )* (b - y))
        f =cat.food
    in
    if dist <= 20 then
    {model | fish = {fish | pos = (-900,-900)} ,cat = {cat|food=f+1} }
    else if not(model.gameState == InDream) then
     {model | fish = { fish | pos = (-900,-900)} }
    else model

updateFelix : Model -> Model
updateFelix model =
    let
        {felix,cat} = model
        (a,b) = cat.pos
        (c,d) = felix.pos
        distance = sqrt ( (a - c) *(a - c) + (b - d )* (b - d))
        clearsmall num = if (abs num < 0.15)then 0 else num
        dir = Tuple.mapBoth clearsmall clearsmall ((a - c) / distance  , (b - d) / distance)
        dx =  model.blockSize / 18 * Tuple.first dir
        dy =  model.blockSize / 18 * Tuple.second dir
        (x,y) = (dx + c, dy + d)
        randir =felix.randir
        randx =  model.blockSize / 35 * Tuple.first randir
        randy =  model.blockSize / 35 * Tuple.second randir
        (rx,ry) = (randx + c, randy + d)
    in
    if model.gameState == InDream then
        if detectFelix model || eagleEye model then
            if checkwalk model x y  then
                {model | felix ={ felix | dir = dir, pos = ( x , y), coll = False}}
            else if felix.coll then
                {model | felix ={ felix |  pos = ( c + Tuple.first felix.dir, d + Tuple.second felix.dir) }}
            else if checkwalk model (c+dx) d  then
                {model | felix ={ felix | dir = (dx,0) , pos = ( c + dx , d ), coll = True}}
            else if checkwalk model c (d+dy)  then
                {model | felix ={ felix | dir = (0,dy) , pos = ( c  , d + dy), coll = True}}
            else
                model
        else
            if checkwalk model rx ry then
                {model | felix ={ felix | dir = randir, pos = ( rx , ry), coll = False}}
            else if felix.coll then
                {model | felix ={ felix |  pos = ( c , d ) }}
            else if checkwalk model (c+randx) d  then
                {model | felix ={ felix | dir = (randx,0) , pos = ( c + randx , d ), coll = True}}
            else if checkwalk model c (d+randy) then
                {model | felix ={ felix | dir = (0,randy) , pos = ( c  , d + randy), coll = True}}
            else
                model
    else
       if detectFelix model then
            if checkwalk model x y  then
                {model | felix ={ felix | dir = dir, pos = ( x , y), coll = False}}
            else if felix.coll then
                {model | felix ={ felix |  pos = ( c + Tuple.first felix.dir, d + Tuple.second felix.dir) }}
            else if checkwalk model (c+dx) d  then
                {model | felix ={ felix | dir = (dx,0) , pos = ( c + dx , d ), coll = True}}
            else if checkwalk model c (d+dy)  then
                {model | felix ={ felix | dir = (0,dy) , pos = ( c  , d + dy), coll = True}}
            else
                model
            --else if (dx == 0 || dy == 0) then {model | felix ={ felix | pos = ( c + Tuple.first dir , d+Tuple.second dir)}}
       else model


detectFelix : Model -> Bool
detectFelix model =
    let
        {cat,felix} = model
        (a,b) = cat.pos
        (c,d) = felix.pos
        dist = sqrt ((a - c)*(a - c)+(b - d)*(b - d))
        (e,f) = felix.dir
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


--modify the direction of felix when he collides with an unmovable block
collide_dir :Model ->(Float,Float)->Block->(Float,Float)
collide_dir model dir block=
    let
        {felix,cat}=model
        horizontal_block_distance=(Tuple.first felix.pos) - toFloat (Tuple.first block.pos)
        vertical_block_distance=(Tuple.second felix.pos) - toFloat (Tuple.second block.pos)
        horizontal_cat_distance=(Tuple.first felix.pos) -  (Tuple.first cat.pos)
        vertical_cat_distance=(Tuple.second felix.pos) - (Tuple.second cat.pos)
        sumhori=(Tuple.first felix.pos) + toFloat (Tuple.first block.pos)
        sumvert=(Tuple.second felix.pos) + toFloat (Tuple.second block.pos)
    in
        if horizontal_cat_distance > 0 then
            if horizontal_block_distance>0 && (horizontal_block_distance + Tuple.first dir)<= sumhori then
                if vertical_cat_distance >0 then
                    (0,-1)
                else
                    (0,1)
            else
                dir
        else if horizontal_cat_distance < 0 then
                 if horizontal_block_distance<0 && (horizontal_block_distance + Tuple.first dir)>= -sumhori then
                     if vertical_cat_distance >0 then
                         (0,-1)
                     else
                         (0,1)
                 else
                     dir
        else if vertical_cat_distance > 0 then
                 if vertical_block_distance>0 && (vertical_block_distance + Tuple.second dir)<= sumvert then
                     if horizontal_cat_distance >0 then
                         (-1,0)
                     else
                         (1,0)
                 else
                     dir
        else
                 if vertical_block_distance<0 && (vertical_block_distance + Tuple.second dir)>= -sumvert then
                     if horizontal_cat_distance >0 then
                         (-1,0)
                     else
                         (1,0)
                 else
                     dir