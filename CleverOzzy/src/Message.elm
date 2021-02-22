module Message exposing (Msg(..))

type Msg
    = Start
    | Pause
    | Resume
    | BackToMenu
    | Tick Float --Tick the game time flow
    | MoveDirection (Float, Float) --up down right left 1 2 3 4
    | Static --stay where it is
    | Noop --Nothing happens
    | Newgame
    | Resize Int Int
    | Roll
    | Newdirection (Float, Float)
    | Refresh
    | Newplace (Float, Float)
    | Choose String
    | EndCurrentDialogue
    | Restart
    | Skip
    | UseRoar
    | About



