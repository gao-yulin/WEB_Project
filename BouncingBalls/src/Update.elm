module Update exposing (..)

import Array
import Browser.Dom exposing (Viewport, getViewport)
import Message exposing (Msg(..))
import Model exposing (..)
import Random
import Task



--update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetViewport { viewport } ->
            let
                { ball, paddle } =
                    model
            in
            ( { model
                | winsize =
                    ( viewport.width
                    , viewport.height
                    )
                , brick = generateBricks { model | winsize = ( viewport.width, viewport.height ) } 5 8 model.random
                , ball = { ball | center_pos = ( viewport.width / 2.0, viewport.height - 50.0 ) }
                , paddle = { paddle | center_pos = ( viewport.width / 2.0 - 150, viewport.height - 30.0 ) }
              }
            , Cmd.none
            )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Start ->
            ( { model | state = Playing }, Cmd.none )

        Newgame ->
            ( {initialmodel|state = Playing}, Task.perform GetViewport getViewport )

        Resume ->
            ( { model | state = Playing }
            , Cmd.none
            )

        MoveDirection dir ->
            let
                { paddle } =
                    model
            in
            ( { model | paddle = { paddle | direction = Just (toFloat dir) } }, Cmd.none )

        Resize width height ->
            ( { model | winsize = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        Tick time ->
            ( model
                |> animate time
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )

        Roll ->
            ( model
            , Random.generate Newnum numberGenerator
            )

        Newnum num ->
            ( { model | random = num }
            , Cmd.none
            )

numberGenerator : Random.Generator Int
numberGenerator =
  Random.uniform 9
    [ 10
    , 11
    , 12
    , 13
    , 14
    , 17
    , 18
    , 19
    , 20
    , 21
    , 22
    , 25
    , 26
    , 27
    , 28
    , 29
    , 30
    ]

animate : Float -> Model -> Model
animate time model =
    model |> updatePaddle |> updateBall |> updateEnd |> updateTime time |> infection

--*
updateTime : Float -> Model -> Model
updateTime delta model =
    let
        originaltime = model.secondtime
    in
    if model.secondflag == 2 then
        {model | secondtime = originaltime + delta}
    else
        model



--*


updatePaddle : Model -> Model
updatePaddle model =
    let
        { paddle } =
            model

        max =
            Tuple.first model.winsize - Tuple.first model.paddle.size

        first =
            Tuple.first paddle.center_pos
                + 20
                * Maybe.withDefault 0 paddle.direction
                |> clamp 0 max
    in
    { model | paddle = { paddle | center_pos = ( first, Tuple.second paddle.center_pos ) } }


updateBall : Model -> Model
updateBall model =
    let
        { ball, paddle, brick } =
            model
    in
    model |> collide |> updatemove


updatemove : Model -> Model
updatemove model =
    let
        { ball } =
            model
    in
    { model
        | ball =
            { ball
                | center_pos =
                    ( Tuple.first ball.center_pos + 12 * cos (Maybe.withDefault 0 ball.direction)
                    , Tuple.second ball.center_pos - 12 * sin (Maybe.withDefault 0 ball.direction)
                    )
            }
    }



---------------------------------


collide : Model -> Model
collide model =
    model |> collide_bound |> collide_brick |> collide_paddle |> collide_center


collide_bound : Model -> Model
collide_bound model =
    let
        { ball, winsize } =
            model

        dir =
            Maybe.withDefault 0 ball.direction

        radius =
            Tuple.first ball.size
    in
    if
        (Tuple.first ball.center_pos
            - radius
            < 0
            || Tuple.first ball.center_pos
            + radius
            > Tuple.first winsize
        )
            && Tuple.second ball.center_pos
            - radius
            < 0
    then
        { model | ball = { ball | direction = Just (dir + pi) } }

    else if
        Tuple.first ball.center_pos
            - radius
            < 0
            || Tuple.first ball.center_pos
            + radius
            > Tuple.first winsize
    then
        { model | ball = { ball | direction = Just (-dir + pi) } }

    else if Tuple.second ball.center_pos - radius < 0 then
        { model | ball = { ball | direction = Just -dir } }

    else
        model


collide_brick : Model -> Model
collide_brick model =
    let
        { ball } =
            model

        dir =
            Maybe.withDefault 0 ball.direction

        brickCollideList =
            List.partition (\brick -> collone ball brick /= 0) model.brick |> Tuple.first

        brickNotCollide =
            List.partition (\brick -> collone ball brick /= 0) model.brick |> Tuple.second

        brickCollide =
            Array.fromList brickCollideList
                |> Array.get 1
                |> Maybe.withDefault (Object ( 0, 0 ) Brick ( 0.0, 0.0 ) Nothing Nothing Nothing Nothing)

        minusHpBrick =
            List.map minusHp brickCollideList

        collideMode =
            collone ball brickCollide

        minusHp brick =
            { brick | hp = Maybe.map2 (-) brick.hp (Just 1) }

        isHpNotZero brick =
            if Maybe.withDefault 0 brick.hp <= 0 then
                False

            else
                True

   in
       if List.isEmpty brickCollideList then
           model
       else if collone ball brickCollide == 2 then-----*
           {model | ball = {ball | direction = Just  (pi - dir)}, score=(model.score+List.sum (List.map getpoint brickCollideList))
               ,brick = List.filter isHpNotZero ( minusHpBrick++ brickNotCollide)}
       else----*
           {model | ball = {ball | direction = Just  -dir}, score=model.score+List.sum (List.map getpoint brickCollideList)
                ,brick = List.filter isHpNotZero ( minusHpBrick++ brickNotCollide)}

--enter into the second stage
collide_center : Model -> Model
collide_center model =
    let
        { ball } =
            model

        brickCollideList =
            List.partition (\brick -> collone ball brick /= 0) model.brick |> Tuple.first

        width =
            (Tuple.first model.winsize / toFloat 8)
        height =
            Tuple.second model.winsize / 3.0 / toFloat 5

        randomposition =
             (toFloat (remainderBy 8 model.random) *width,  toFloat (model.random // 8) * height)

        isCenterHit =
            List.member randomposition (List.map .center_pos brickCollideList)
    in
        case isCenterHit of
            True ->
                {model| secondflag = 2}
            False ->
                model

infection : Model -> Model
infection model =
    let
        evil =
            Maybe.withDefault (model.ball) (List.filter judgeEvil model.brick |> List.head)
        nb =
            List.filter (\brick -> neighbourhood brick evil model.secondtime) model.brick
        notnb =
            List.partition (\brick -> neighbourhood brick evil model.secondtime ) model.brick |> Tuple.second
        changetype brick=
            {brick | hp= Just 2, brick_type = Just Guard}
        newnb =
            List.map (\brick -> changetype brick) nb



    in
    case model.secondflag == 2 of
        True ->
            {model | brick = newnb ++ notnb}
        False ->
            model


judgeEvil : Object -> Bool
judgeEvil brick=
    case brick.brick_type of
        Just Good ->
            False
        Just Guard ->
            False
        Just Evil ->
            True
        Nothing ->
            False

neighbourhood : Object -> Object -> Float -> Bool
neighbourhood brick guard time_range=
    let
        (a,b)=
            brick.center_pos
        (c,d)=
            guard.center_pos
        robottype =
            brick.brick_type
    in
    case robottype of
        Just Good ->
            if abs(a - c) < 0.01 * time_range && abs(b - d) < 0.01 * time_range then
                True
            else
                False
        Just Evil ->
            False
        Just Guard ->
            False
        Nothing ->
            False



getpoint: Object->Int
getpoint a =
     (Maybe.withDefault 0 (a.point))

--Change the judement of colloneVertical and collonehorizontal function
--0 for not, 1 for horizontal, 2 for vertical


collone : Object -> Object -> Int
collone ball brick =
    let
        ( a, b ) =
            ball.center_pos

        ( c, d ) =
            brick.center_pos

        r =
            Tuple.first ball.size

        ( x, y ) =
            brick.size
    in
    if abs (c + 0.5 * x - a) > r + 0.5 * x || abs (d + 0.5 * y - b) > 0.5 * y + r then
        0

    else if abs ((d + 0.5 * y - b) / (c + 0.5 * x - a)) < y / x then
        2

    else
        1


collide_paddle : Model -> Model
collide_paddle model =
    let
        { ball } =
            model

        dir =
            collonePaddle model.ball model.paddle
    in
    if Maybe.withDefault 0.0 dir > 0.1 then
        { model | ball = { ball | direction = dir } }

    else
        model


collonePaddle ball paddle =
    let
        ( a, b ) =
            ball.center_pos

        ( c, d ) =
            paddle.center_pos

        r =
            Tuple.first ball.size

        x =
            Tuple.first paddle.size
    in
    if a > c && a < c + x && b + r > d && sin (Maybe.withDefault 0 ball.direction) < 0 then
        Just (-2.1 * a / x + 2.6 + 2.1 * c / x)

    else
        ball.direction



--add a check end function


checkLose model =
    let
            { ball, brick, winsize,score } =
                model
    in
    if score <=0 then
        True

    else if Tuple.second ball.center_pos > Tuple.second winsize then
        True
    else
        False



--*


checkWin model =
    let
           {ball,brick,winsize} = model
    in
        if List.sum (List.map checkBossAlive brick) ==0 then
            True

        else
            False

checkBossAlive: Object -> Int
checkBossAlive brick=
    if brick.brick_type == Just Evil then
        1
    else
        0

updateEnd model =
    --if checkWin model then
      --  { model | state = Win }

    --else if checkLose model then
      --  { model | state = Lose }

    --else
        model



-----------------------------------------------
