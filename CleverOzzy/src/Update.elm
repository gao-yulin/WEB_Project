module Update exposing (..)

--import Felix exposing (directionGenerator, placeGenerator, updateFelix)

import Array
import Browser.Dom exposing (Viewport, getViewport)
import Felix1.Update
import Lisa1.Update
import Map exposing (..)
import Message exposing (Msg(..))
import Model exposing (..)
import Random
import Storyline exposing (..)
import Task
import Tools exposing (checkwalk, collideBlocks, updateCamera, updateframe)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        About ->
            ({model|runstate = Aboutus}, Cmd.none)
        UseRoar ->
            let
                { cat } =
                    model

                c =
                    { cat | magic = Roar }
            in
            ( { model | cat = c }, Cmd.none )

        Resize w h ->
            let
                wPlot =
                    w

                hPlot =
                    h

                { cat, felix, lisa } =
                    model

                blockSize_ =
                    Basics.min (wPlot // 48) (hPlot // 48)

                newCat =
                    { cat | size = ( blockSize_, blockSize_ ), pos = ( 26 * toFloat blockSize_, 34 * toFloat blockSize_ ) }

                newfelix =
                    { felix | pos = ( 28 * toFloat blockSize_, 42 * toFloat blockSize_ ) }

                newlisa =
                    { lisa | pos = ( 30 * toFloat blockSize_, 45 * toFloat blockSize_ ) }
            in
            ( { model
                | size = ( wPlot, hPlot )
                , blockSize = toFloat blockSize_
                , cat = newCat
                , lisa = newlisa
                , felix = newfelix
                , camera = newCat.pos
              }
            , Cmd.none
            )
        BackToMenu ->
            ( { model | runstate = Main }, Cmd.none )

        Newgame ->
            ( { initial | runstate = Game }
            , Cmd.batch
                [ Task.perform (\{ viewport } -> Resize (round viewport.width) (round viewport.height)) getViewport
                , Random.generate Newdirection directionGenerator
                ]
            )

        Start ->
            ( { initial | runstate = Game }
            , Cmd.batch
                [ Task.perform (\{ viewport } -> Resize (round viewport.width) (round viewport.height)) getViewport
                , Random.generate Newdirection directionGenerator
                ]
            )

        Pause ->
            if model.runstate == Game then
                ( { model | runstate = Paused }, Cmd.none )
            else (model, Cmd.none)

        Resume ->
            ( { model | runstate = Game }, Cmd.none )

        MoveDirection ( x, y ) ->
            let
                { cat } =
                    model
            in
            if model.bgm == "none" then
                ( { model
                    | cat = { cat | dir = ( x, y ), walk = True }
                    , bgm = "res/bgm_day.mp3"
                  }
                , Cmd.none
                )

            else
                ( { model | cat = { cat | dir = ( x, y ), walk = True } }, Cmd.none )

        Static ->
            let
                { cat } =
                    model
            in
            if model.gameState == CheckEnterDream || model.gameState == CheckLeaveDream then
                ( model, Cmd.none )

            else
                ( { model | cat = { cat | dir = ( 0, 0 ), walk = False } }, Cmd.none )

        Noop ->
            ( model, Cmd.none )

        Tick time ->
            gameRunning model time
        Roll ->
            ( model
            , Random.generate Newdirection directionGenerator
            )

        Newdirection randir ->
            let
                { felix } =
                    model
            in
            if model.gameState == InDream then
                ( { model | felix = { felix | randir = randir } }, Cmd.none )

            else
                ( model, Cmd.none )

        Refresh ->
            ( model
            , Random.generate Newplace (placeGenerator model)
            )

        Newplace ranpos ->
            let
                { fish } =
                    model
            in
            if model.gameState == InDream then
                ( { model | fish = { fish | pos = ranpos } }, Cmd.none )

            else
                ( model, Cmd.none )

        Choose choice ->
            case choice of
                "Yes" ->
                    ( { model | choice = "Yes" }, Cmd.none )

                "No" ->
                    ( { model | choice = "No" }, Cmd.none )

                _ ->
                    ( { model | choice = "Nothing" }, Cmd.none )

        EndCurrentDialogue ->
            let
                { dialogNum, dialogList } =
                    model

                text =
                    Maybe.withDefault "" (Array.get dialogNum dialogList)
            in
            if model.gameState == InDialog && text /= "null" then
                ( { model | dialogNum = dialogNum + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        Restart ->
            let
                { dreamMod } =
                    model
            in
            case model.gameState of
                Lisa _ ->
                    ( { model | dreamMod = { dreamMod | dreamState = InitDream } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Skip ->
            let
                {dreamMod,cat} = model
                blockSiz = model.blockSize
                newCat =
                    if model.lisaline.day == 3 then
                        { cat | pos = (  (38 * blockSiz),  (38 * blockSiz) ) }
                    else
                        { cat | pos = (  (36 * blockSiz),  (28 * blockSiz) )}
            in
            case model.gameState of
                Lisa _ ->
                    ({ model
                        | gameState = InDialog
                        , cat = newCat
                        , bgm = "./res/bgm_day.mp3"
                        , dreamMod = { dreamMod | dreamState = InitDream,skipTime = dreamMod.skipTime + 1}
                        , camera = cat.pos
                    },Cmd.none)
                _ ->
                    (model,Cmd.none)






gameRunning : Model -> Float -> ( Model, Cmd Msg )
gameRunning model time =
    case model.runstate of
        Game ->
            ( model |> animate time, commandGenerate model )
        StartView ->
            ( model |> changeopa, Cmd.none)
        WinGame _ ->
            (model |> changeopa, Cmd.none)
        _ ->
            ( model, Cmd.none )

changeopa : Model -> Model
changeopa model=
    if model.opa >= 1.3 then
        {model|opa = 0, runstate = Main}
    else
        let
            opa_ = model.opa + 0.004
        in
        {model|opa = opa_}

commandGenerate : Model -> Cmd Msg
commandGenerate model =
    if model.gametime / 500 == 0 then
        Random.generate Newdirection directionGenerator

    else if model.gametime == 50 then
        Random.generate Newplace (placeGenerator model)

    else
        Cmd.none


animate : Float -> Model -> Model
animate time model =
    case model.gameState of
        Felix _ ->
            Felix1.Update.update model time
        Lisa _ ->
            Lisa1.Update.update model time
        _ ->

            model
                |> catWalk time
                |> updateCamera
                |> avoidRepeatCollide
                |> updateWholeTime time
                |> updateframe time
                |> updateRefreshTime time
                |> leaveDream
                |> updateGameTime time
                |> felixframe time
                |> update_cat_status
                |> checkEnterRestaurant
                |> leaveRestaurant
                |> updateStory
                |> updateBgm
                |> lisaframe time



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

{--Chen: some legacies
comeHome : Float -> Model -> Model
comeHome delta model =
    let
        {cat} = model
        (x,y)= cat.pos
        dir = cat.dir
        bloSize =   model.blockSize
        dx = bloSize / 10 * Tuple.second dir
        dy = bloSize / 10 * Tuple.second dir
        level = model.felixDream
    in
    if List.any (\block -> block.blockType == "home") (collideBlocks model (x+ 2 *dx ) (y+ 2 *dy )) then
        checkEnterDream {model|gameState = CheckEnterDream} (Felix level)
    else
        {model|gameState = Daytime}

enterLisaDream : Model -> Model
enterLisaDream model =
    let
        {cat} = model
        blo = model.blockSize
        (x,y)= cat.pos
    in
    if model.gameState == Daytime then
        if y < 6 * blo then
            if x < 10 * blo then
                checkEnterDream model (Lisa 1)
            else
                checkEnterDream model (Lisa 2)
        else
            if x < 10 * blo then
                checkEnterDream model (Lisa 3)
            else
                checkEnterDream model (Lisa 4)
    else
        model

checkEnterDream : Model -> GameState -> Model
checkEnterDream model gameState=
    let
        {lisa,cat}=model
        blocksiz = Basics.min (Tuple.first model.size // 20) (Tuple.second model.size // 15)
        n = model.felixDream
    in
    if model.choice == "Yes" then
        { model |  lisa = {lisa | pos = (-1000,-1000) }
        , gameState = gameState
        , bgm = "./res/bgm_night.mp3"
        , block = (20,15)
        , blockSize =toFloat blocksiz
        , cat = { cat | size = (blocksiz , blocksiz ), pos = (10 * toFloat blocksiz,6 * toFloat blocksiz)}
        --, map = mapDream1
        }
    else
        model
-}

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

leaveDream : Model -> Model
leaveDream model =
    let
        { lisa, cat } =
            model

        blocksiz =
            Basics.min (Tuple.first model.size // 48) (Tuple.second model.size // 48)
    in
    if model.refreshtime == 30000.0 then
        { model
            | map = Map.mapDorm
            , lisa = { lisa | pos = ( 250, 400 ) }
            , gameState = Daytime
            , bgm = "./res/bgm_day.mp3"
            , block = ( 48, 48 )
            , blockSize = toFloat (Basics.min (Tuple.first model.size // 48) (Tuple.second model.size // 48))
            , cat = { cat | size = ( blocksiz, blocksiz ), pos = ( 10 * toFloat blocksiz, 6 * toFloat blocksiz ) }
        }

    else
        model


checkCollideWithNPC : Model -> Npc -> Bool
checkCollideWithNPC model npc =
    let
        { cat } =
            model
    in
    if
        abs (Tuple.first cat.pos - Tuple.first npc.pos)
            <= toFloat 20
            && abs (Tuple.second cat.pos - Tuple.second npc.pos)
            <= toFloat 20
    then
        True

    else
        False


avoidRepeatCollide : Model -> Model
avoidRepeatCollide model =
    let
        { cat, lisa } =
            model
    in
    if model.gameState == JustOutDialogue && not (checkCollideWithNPC model lisa) then
        { model | gameState = Daytime }

    else
        model


updateRefreshTime : Float -> Model -> Model
updateRefreshTime delta model =
    --update refreshtime (dream recording)
    if model.gameState == InDream then
        if model.refreshtime <= 30000.0 then
            { model | refreshtime = model.refreshtime + delta }

        else
            { model | refreshtime = 0.0 }

    else
        { model | refreshtime = 0.0 }


updateGameTime : Float -> Model -> Model
updateGameTime delta model =
    --update gametime (for randomdir)
    if model.gameState == InDream then
        if model.gametime <= 4000.0 then
            { model | gametime = model.gametime + delta }

        else
            { model | gametime = 0.0 }

    else
        { model | gametime = 0.0 }


updateWholeTime : Float -> Model -> Model
updateWholeTime delta model =
    --update wholetime (for decrease of status)
    if model.wholetime <= 2000.0 then
        { model | wholetime = model.wholetime + delta }

    else
        { model | wholetime = 0.0 }


catWalk : Float -> Model -> Model
catWalk delta model =
    let
        second =
            delta / 1000

        { cat } =
            model

        dir =
            cat.dir

        bloSize =
            model.blockSize

        ( x, y ) =
            cat.pos

        v =
            if cat.food > 5 then
                1.5

            else if cat.food > 0 && cat.food < 6 then
                1

            else
                0.5

        dx =

            2 * v * bloSize * second * Tuple.first dir

        dy =
            2 * v * bloSize * second * Tuple.second dir

    in
    if checkwalk model (x + dx) (y + dy) && model.gameState == Daytime then
        { model | cat = { cat | pos = ( x + dx, y + dy ) } }

    else
        model


directionGenerator : Random.Generator ( Float, Float )
directionGenerator =
    Random.uniform ( 1, 0 )
        [ ( 0, 1 )
        , ( -1, 0 )
        , ( 0, 0 )
        , ( 0, -1 )
        ]


placeGenerator : Model -> Random.Generator ( Float, Float )
placeGenerator model =
    Random.uniform ( 2 * model.blockSize, 4 * model.blockSize )
        [ ( 6 * model.blockSize, 14 * model.blockSize )
        , ( 15 * model.blockSize, 11 * model.blockSize )
        ]


pair_toFloat : ( Int, Int ) -> ( Float, Float )
pair_toFloat ( a, b ) =
    let
        c =
            toFloat a

        d =
            toFloat b
    in
    ( c, d )



--eat and automatic decrease


update_cat_status : Model -> Model
update_cat_status model =
    let
        { cat } =
            model

        temp =
            genPosy 0 (List.range 0 4) ++ genPosy 10 (List.range 5 8) ++ genPos 9 (List.range 11 13)

        drinkable_pos =
            List.map pair_toFloat temp

        f =
            cat.food

        s =
            cat.sanity
    in
    if model.wholetime == 1000 then
        if f <= 0 then
            if model.wholetime == 1000 then
                if s > 0 then
                    { model | cat = { cat | sanity = s - 1 } }
                    --in this situation, you lose

                else
                    model

            else
                model

        else if f >= 10 then
            { model | cat = { cat | food = f - 1, sanity = s + 1 } }

        else
            { model | cat = { cat | food = f - 1 } }

    else
        model


checkGoRestaurant : Model -> Bool
checkGoRestaurant model =
    let
        { cat } =
            model

        ( x, y ) =
            cat.pos

        dir =
            cat.dir

        bloSize =
            model.blockSize

        dx =
            bloSize / 10 * Tuple.first dir

        dy =
            bloSize / 10 * Tuple.second dir
    in
    if List.any (\block -> block.pos == ( 6, 5 )) (collideBlocks model (x + 2 * dx) (y + 2 * dy)) then
        True

    else
        False


checkEnterRestaurant : Model -> Model
checkEnterRestaurant model =
    if checkGoRestaurant model then
        if model.gameState == Daytime && model.choice == "Yes" then
            { model | gameState = InRestaurant, choice = "No" }

        else
            model

    else
        model


leaveRestaurant : Model -> Model
leaveRestaurant model =
    let
        { cat } =
            model
    in
    if model.gameState == InRestaurant && model.choice == "Yes" then
        { model | gameState = Daytime, cat = { cat | food = cat.food + 1 }, choice = "No" }

    else
        model


updateBgm : Model -> Model
updateBgm model =
    let
        { bgm, lisaline } =
            model
    in
    if lisaline.plot > 7 && bgm /= "./res/bgm_night.mp3" && model.gameState /= InDialog then
        { model | bgm = "./res/bgm_night.mp3" }

    else if lisaline.plot < 8 && bgm /= "./res/bgm_day.mp3" && model.gameState /= InDialog then
        { model | bgm = "./res/bgm_day.mp3" }

    else
        model
