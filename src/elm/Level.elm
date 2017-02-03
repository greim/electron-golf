
-- module ----------------------------------------------------------------------

module Level exposing
  ( Level
  , allLevels
  , tallyPar
  , tallyScore
  )

-- import ----------------------------------------------------------------------

import Cannon exposing (Cannon)
import Target exposing (Target)
import Phys exposing (horizBarrier, vertBarrier, bubbleBarrier, bouncyBarrier)

-- types -----------------------------------------------------------------------

type alias Level =
  { cannon : Cannon
  , barriers : List Phys.Obj
  , target : Target
  , par : Int
  , score : Int
  }

-- functions -------------------------------------------------------------------

tallyScore : List Level -> Int
tallyScore levels =
  List.foldl accScore 0 levels

accScore : Level -> Int -> Int
accScore level tally =
  tally + level.score

tallyPar : List Level -> Int
tallyPar levels =
  List.foldl accPar 0 levels

accPar : Level -> Int -> Int
accPar level tally =
  tally + level.par

genLevels : Float -> List Level
genLevels n =
  let
    fudge = toFloat (((round n) % 60) - 30)
    mapper = fudgeLevel fudge
  in
    List.map mapper allLevels

fudgeLevel : Float -> Level -> Level
fudgeLevel fudge level =
  let
    cannon = level.cannon
    fudgedCannon = { cannon | angle = cannon.angle + fudge }
  in
    { level | cannon = fudgedCannon }

-- values ----------------------------------------------------------------------

allLevels : List Level
allLevels =
  [ Level
    (Cannon (100, 100) 45 0)
    []
    (Target (200, 200, 150, 150))
    1
    0
  , Level
    (Cannon (100, 900) 335 0)
    []
    (Target (450, 400, 150, 150))
    1
    0
  , Level
    (Cannon (900, 900) 180 0)
    []
    (Target (100, 100, 150, 150))
    1
    0
  , Level
    (Cannon (100, 100) 90 0)
    [ Phys.vertBarrier 500 50 600
    ]
    (Target (750, 100, 150, 150))
    2
    0
  , Level
    (Cannon (880, 450) 202 0)
    [ Phys.horizBarrier 50 500 349
    , Phys.horizBarrier 601 500 349
    , Phys.bubbleBarrier 410 500 10
    , Phys.bubbleBarrier 590 500 10
    , Phys.bubbleBarrier 500 200 100
    ]
    (Target (800, 800, 100, 100))
    2
    0
  , Level
    (Cannon (880, 850) 270 0)
    [ Phys.vertBarrier 333 51 666
    , Phys.vertBarrier 666 332 618
    ]
    (Target (100, 150, 100, 100))
    2
    0
  , Level
    (Cannon (500, 800) 270 0)
    [ Phys.horizBarrier 200 333 600
    , Phys.horizBarrier 200 666 600
    ]
    (Target (450, 140, 100, 100))
    2
    0
  , Level
    (Cannon (900, 900) 270 0)
    [ Phys.vertBarrier 850 230 720
    , Phys.bubbleBarrier 900 100 25
    ]
    (Target (690, 800, 100, 100))
    2
    0
  , Level
    (Cannon (900, 100) 135 0)
    [ Phys.horizBarrier 220 200 580
    , Phys.vertBarrier 800 200 580
    , Phys.bubbleBarrier 198 200 20
    , Phys.bubbleBarrier 800 802 20
    ]
    (Target (600, 300, 100, 100))
    2
    0
  , Level
    (Cannon (100, 500) 0 0)
    [ Phys.horizBarrier 200 199 600
    , Phys.horizBarrier 200 801 600
    , Phys.vertBarrier 200 200 600
    , Phys.vertBarrier 800 200 230
    , Phys.vertBarrier 800 570 230
    ]
    (Target (450, 450, 100, 100))
    3
    0
  , Level
    (Cannon (500, 500) 135 0)
    [ Phys.horizBarrier 200 199 600
    , Phys.horizBarrier 51 801 750
    , Phys.vertBarrier 800 200 600
    , Phys.vertBarrier 200 200 200
    ]
    (Target (75, 825, 100, 100))
    3
    0
  , Level
    (Cannon (500, 100) 90 0)
    [ Phys.horizBarrier 50 250 375
    , Phys.horizBarrier 575 250 375
    , Phys.horizBarrier 150 500 700
    , Phys.horizBarrier 50 750 375
    , Phys.horizBarrier 575 750 375
    ]
    (Target (800, 800, 100, 100))
    3
    0
  , Level
    (Cannon (100, 500) 0 0)
    [ Phys.bubbleBarrier 500 500 210
    , Phys.bubbleBarrier 250 250 100
    , Phys.bubbleBarrier 250 750 100
    , Phys.bubbleBarrier 750 250 100
    , Phys.bubbleBarrier 750 750 100
    ]
    (Target (800, 450, 100, 100))
    3
    0
  , Level
    (Cannon (170, 170) 45 0)
    [ Phys.bubbleBarrier 500 500 400
    ]
    (Target (800, 800, 100, 100))
    3
    0
  , Level
    (Cannon (150, 150) 0 0)
    [ Phys.horizBarrier 500 500 450
    , Phys.horizBarrier 50 300 450
    , Phys.horizBarrier 50 700 450
    , Phys.bubbleBarrier 250 500 125
    , Phys.bubbleBarrier 750 275 125
    , Phys.bubbleBarrier 750 725 125
    ]
    (Target (100, 800, 100, 100))
    3
    0
  , Level
    (Cannon (500, 900) 270 0)
    [ Phys.horizBarrier 51 400 222
    , Phys.bouncyBarrier 387 400 111 0.01
    , Phys.bouncyBarrier 613 400 111 0.01
    , Phys.horizBarrier 725 400 222
    ]
    (Target (450, 100, 100, 100))
    2
    0
  , Level
    (Cannon (100, 100) 45 0)
    [ Phys.bouncyBarrier 500 500 400 0.01
    , Phys.bouncyBarrier 150 850 60 1.0
    , Phys.bouncyBarrier 850 150 60 1.0
    ]
    (Target (800, 800, 100, 100))
    3
    0
  , Level
    (Cannon (100, 900) 315 0)
    [ Phys.bouncyBarrier 150 330 70 0.02
    , Phys.bouncyBarrier 325 330 70 0.02
    , Phys.bouncyBarrier 500 330 70 0.02
    , Phys.bouncyBarrier 675 330 70 0.02
    , Phys.bouncyBarrier 850 330 70 0.02
    , Phys.bouncyBarrier 150 500 70 0.02
    , Phys.bouncyBarrier 325 500 70 0.02
    , Phys.bouncyBarrier 500 500 70 0.02
    , Phys.bouncyBarrier 675 500 70 0.02
    , Phys.bouncyBarrier 850 500 70 0.02
    , Phys.bouncyBarrier 150 670 70 0.02
    , Phys.bouncyBarrier 325 670 70 0.02
    , Phys.bouncyBarrier 500 670 70 0.02
    , Phys.bouncyBarrier 675 670 70 0.02
    , Phys.bouncyBarrier 850 670 70 0.02
    ]
    (Target (800, 100, 100, 100))
    2
    0
  ]
