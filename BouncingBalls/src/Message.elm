module Message exposing (Msg(..))
import Browser.Dom exposing (Viewport)
type Msg =
    Start
    | Pause
    | Resume
    | Tick Float
    | MoveDirection Int
    | Resize Int Int
    | Noop
    | GetViewport Viewport
    | Newgame
    | Roll
    | Newnum Int