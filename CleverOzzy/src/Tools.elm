module Tools exposing (..)
import Map exposing (Block)
import Model exposing (..)
--gao : continuous map
--update the camera position to make the map continuously translate


updateCamera : Model -> Model
updateCamera model =
    let
        (x,y) = model.cat.pos
        (cx,cy) = model.camera
        display_width = model.blockSize * 9 / 4
        display_height = model.blockSize * 6 / 4
        all_width = toFloat (Tuple.first model.block) * model.blockSize
        all_height = toFloat (Tuple.second model.block) * model.blockSize
        ca_x = if x - cx>display_width then
                    x - display_width
                else if x - cx < -display_width then
                    x + display_width
                else
                    cx
        ca_y = if y - cy>display_height then
                    y - display_height
                else if y - cy < -display_height then
                    y + display_height
                else
                    cy
        camera_x = if ca_x - display_width * 2 < 0 then cx
                   else if ca_x + display_width * 2 > all_width then cx
                   else ca_x
        camera_y = if ca_y - display_height * 2 < 0 then cy
                   else if ca_y + display_height * 2 > all_height then cy
                   else ca_y
    in
       {model|camera = (camera_x, camera_y)}

--Chen:  takes the pixel position and returns the integer coordinate in the map
getPosInt : Float -> Float -> Float -> (Int , Int)
getPosInt x y bloSize =
    (floor( x / bloSize), floor(y / bloSize))



--Chen: give an integer coordinate and return the corresponding block
getBlock :  Model -> (Int , Int) -> Block
getBlock model pos =
    List.filter (\blo -> blo.pos == pos) model.map
    |> List.head
    |> Maybe.withDefault (Block "null" True True Nothing Nothing (-1,-1))
getDreamBlock :  Model -> (Int , Int) -> Block
getDreamBlock model pos =
    List.filter (\blo -> blo.pos == pos) model.dreamMod.dreamMap
    |> List.head
    |> Maybe.withDefault (Block "null" True True Nothing Nothing (-1,-1))
updateframe : Float ->Model ->  Model
updateframe delta model =
    let
        {cat} = model
        t = cat.frametime + delta/1000
        frametime = if (t < 0.2) then t else 0
        f = cat.frame
        nextf = if (t /= frametime ) then f+1 else f
        nextframe = if (remainderBy 4 nextf==0) then 4 else remainderBy 4 nextf
    in
    {model | cat = {cat|frametime = frametime, frame = nextframe }}



--Chen: return the block that collides with the given position
collideBlocks: Model -> Float -> Float -> List Block
collideBlocks model x y =
    let
        bloSize = model.blockSize
        (a1,b1) = getPosInt (x + bloSize*0.3) (y + bloSize*0.25) bloSize
        (a2,b2) = getPosInt (x + bloSize*0.7) (y + bloSize*0.25) bloSize
        (a3,b3) = getPosInt (x + bloSize*0.3) (y + bloSize*0.6) bloSize
        (a4,b4) = getPosInt (x + bloSize*0.7) (y + bloSize*0.6) bloSize
    in
         List.map (getBlock model) [(a1,b1),(a2,b2),(a3,b3),(a4,b4)]


checkwalk : Model -> Float -> Float -> Bool
checkwalk model x y =
    let
        bloSize = model.blockSize
        boundOut = x < 0 - bloSize/7  || x > bloSize * 48 - bloSize / 1.22
            || y < 0  || y > bloSize * 48 - bloSize / 1.22
        movable = collideBlocks model x y
          |> List.all ( \blo -> blo.movable )
    in
    movable && (not boundOut)