module Model exposing (..)

import Array exposing (..)
import DialogList exposing (dialoglist)
import Map exposing (..)


type alias Model =
    { cat : Cat
    , gameState : GameState
    , size : ( Int, Int )
    , map : List Block
    , dreamMod : DreamMod
    , lisa : Npc
    , felix : Npc
    , blockSize : Float
    , block : ( Int, Int )
    , wholetime : Float
    , gametime : Float
    , refreshtime : Float
    , choice : String
    , bgm : String
    , bird1 : Item
    , bird2 : Item
    , bird3 : Item
    , bird4 : Item
    , fish : Item
    , camera : ( Float, Float )
    , dialogList : Array String
    , dialogNum : Int
    , runstate : Runstate
    , lisaline : Lisaline
    , felixDream : Int
    , chaser : List Npc
    , opa : Float
    }


type Runstate
    = Paused
    | Main
    | Aboutus
    | Game
    | WinGame Int
-- win 1 lisa good end
-- win 2 felix end
-- win 3 lisa normal end
-- win 4 lisa bad end

    | StartView



type GameState
    = Daytime
    | InDream
    | CheckEnterDream
    | CheckLeaveDream
    | InDialog
    | JustOutDialogue
    | InRestaurant
    | Lisa Int
    | Felix Int



type CatStatus
    = Normal
    | Thirsty
    | Hungry
    | Injured


type alias DreamMod =
    { dreamMap : List Block
    , dreamTime : Float
    , moveFlag : Bool
    , dreamState : DreamState
    , skipTime: Int
    }


type DreamState
    = InitDream
    | Playing
    | Win
    | Lose
    | Mirror


type alias Cat =
    { pos : ( Float, Float )
    , size : ( Int, Int )
    , dir : ( Float, Float )
    , walk : Bool
    , frame : Int
    , frametime : Float
    , sanity : Int
    , knowledge : Int
    , food : Int
    , status : CatStatus
    , magic : Magic
    }

type Magic =
    Roar
    | Norm


type alias Npc =
    { pos : ( Float, Float )
    , size : ( Int, Int )
    , dir : ( Float, Float )
    , walk : Bool
    , quality : Maybe Npctype
    , coll : Bool
    , randir : ( Float, Float )
    , controlled : Bool
    , frame : Int
    , frametime : Float
    }


type alias Item =
    { pos : ( Float, Float )
    , size : ( Int, Int )
    , dir : ( Float, Float )
    }


type alias Lisaline =
    { day : Int
    , plot : Int
    , dialog : String
    }


type Npctype
    = Carer
    | Chaser


initial : Model
initial =
    let
        initialCat =
            { pos = ( 250, 50 )
            , size = ( 0, 0 )
            , dir = ( 0, 0 )
            , walk = False
            , frame = 1
            , frametime = 0
            , sanity = 5
            , knowledge = 0
            , food = 10
            , status = Normal
            , magic = Norm
            }

        initialLisa =
            { pos = ( 320, 400 )
            , size = ( 60, 60 )
            , dir = ( 0, 0 )
            , walk = False
            , quality = Just Carer
            , coll = False
            , randir = ( 0, 0 )
            , controlled = False
            , frame = 1
            , frametime = 0
            }

        initialFelix =
            { pos = ( 540, 200 )
            , size = ( 60, 60 )
            , dir = ( 0, 0 )
            , walk = False
            , quality = Just Chaser
            , coll = False
            , randir = ( 0, 0 )
            , controlled = False
            , frame = 1
            , frametime = 0
            }

        initialBird1 =
            { pos = ( -998, -998 )
            , size = ( 30, 30 )
            , dir = ( 1, 0 )
            }

        initialBird2 =
            { pos = ( -998, -998 )
            , size = ( 30, 30 )
            , dir = ( -1, 0 )
            }

        initialBird3 =
            { pos = ( -998, -998 )
            , size = ( 30, 30 )
            , dir = ( 0, 1 )
            }

        initialBird4 =
            { pos = ( -998, -998 )
            , size = ( 30, 30 )
            , dir = ( 0, -1 )
            }

        initialFish =
            { pos = ( -998, -998 )
            , size = ( 30, 30 )
            , dir = ( 0, 0 )
            }

        initDreamMod =
            { dreamMap = mapLisa1
            , dreamTime = 0
            , moveFlag = False
            , dreamState = InitDream
            , skipTime = 0
            }

        initalLisaline =
            { day = 1
            , plot = 1
            , dialog = ""
            }
    in
    { cat = initialCat
    , lisa = initialLisa
    , felix = initialFelix
    , gameState = InDialog
    , size = ( 0, 0 )
    , map = mapDorm
    , dreamMod = initDreamMod
    , blockSize = 0
    , block = ( 48, 48 )
    , wholetime = 0
    , gametime = 0
    , refreshtime = 0
    , choice = "Nothing"
    , bgm = "none"
    , bird1 = initialBird1
    , bird2 = initialBird2
    , bird3 = initialBird3
    , bird4 = initialBird4
    , fish = initialFish
    , camera = ( 0, 0 )
    , dialogList = Array.fromList dialoglist
    , dialogNum = 0
    , runstate = StartView
    , lisaline = initalLisaline
    , chaser = []
    , felixDream = 1
    , opa = 0
    }
