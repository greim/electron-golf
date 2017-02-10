
-- module ----------------------------------------------------------------------

module Level exposing
  ( Level
  , tallyPar
  , tallyScore
  , XAnchor(..)
  , YAnchor(..)
  , Course
  , Courses
  , courses
  )

-- import ----------------------------------------------------------------------

import Cannon exposing (Cannon)
import Target exposing (Target)
import Phys exposing (horizBarrier, vertBarrier, bubbleBarrier, bouncyBarrier)
import Dict exposing (Dict)

-- types -----------------------------------------------------------------------

type alias Level =
  { cannon : Cannon
  , barriers : List Phys.Obj
  , fields : List Phys.Field
  , target : Target
  , par : Int
  , score : Int
  , message : Maybe Message
  }

type XAnchor = L | R | C

type YAnchor = T | B

type alias Message =
  { text : String
  , pos : (Float, Float)
  , width : Float
  , xAnchor : XAnchor
  , yAnchor : YAnchor
  }

type alias Course =
  List Level

type alias Courses =
  Dict String Course

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

fudgeLevel : Float -> Level -> Level
fudgeLevel fudge level =
  let
    cannon = level.cannon
    fudgedCannon = { cannon | angle = cannon.angle + fudge }
  in
    { level | cannon = fudgedCannon }

-- values ----------------------------------------------------------------------

beginner : Course
beginner =
  [
  Level
    -- Training. Super easy setup.
    (Cannon (200, 300) 34 0)
    []
    []
    (Target (800, 700) 150)
    1
    0
    (Just (Message "Charge the cannon by holding the SPACEBAR, then release to fire. The longer the charge, the faster the launch. Make the electron come to rest on or near the captive proton." (100, 100) 420 R T))
  ,
  Level
    -- Training. Opposite-end target. Cannon not perfectly lined up.
    (Cannon (800, 800) 277 0)
    []
    []
    (Target (225, 225) 150)
    1
    0
    (Just (Message "Aim the cannon using the LEFT or RIGHT arrow keys. While aiming, hold SHIFT or ALT to modify rotation speed." (150, 150) 320 L B))
  ,
  Level
    -- Training. A very introductory level with a big, close target. Helpful
    -- message.
    (Cannon (100, 100) 10 0)
    []
    []
    (Target (275, 275) 150)
    1
    0
    (Just (Message "At closer ranges, hold SHIFT to putt." (100, 100) 600 L B))
  ,
  Level
    -- In which we introduce the first barrier. Simple bank shot.
    (Cannon (100, 100) 50 0)
    [ Phys.vertBarrier 500 50 600
    ]
    []
    (Target (825, 175) 150)
    2
    0
    (Just (Message "Circumvent barriers with bank shots." (100, 100) 250 L B))
  ,
  Level
    -- Introduce a simple force field the player must work around. No other
    -- obstacles.
    (Cannon (500, 100) 90 0)
    []
    [ Phys.Field (370, 500) 50 200 -1
    ]
    (Target (500, 850) 100)
    2
    0
    (Just (Message "Like repels like. Opposites attract. An electrical field will either attract or repel your electron, depending on its charge." (100, 100) 310 R T))
  ,
  Level
    -- Start from the center and try to bounce the ball into a narrow channel.
    (Cannon (800, 200) 104 0)
    [ Phys.bubbleBarrier 350 650 200
    , Phys.bubbleBarrier 750 450 115
    , Phys.bubbleBarrier 530 200 80
    ]
    []
    (Target (650, 800) 120)
    2
    0
    (Just (Message "Some barriers are round. Deflection angles are tricky to estimate." (100, 200) 280 L T))
  ,
  Level
    -- A trick level. Player presumably hasn't seen bouncy barriers yet so
    -- thinks this level is impossible, since no openings to target are visible.
    -- Little do they know that some barriers move.
    (Cannon (500, 900) 270 0)
    [ Phys.horizBarrier 51 400 222
    , Phys.bouncyBarrier 387 400 111 0.01
    , Phys.bouncyBarrier 613 400 111 0.01
    , Phys.horizBarrier 725 400 222
    ]
    []
    (Target (500, 150) 100)
    2
    0
    (Just (Message "Meanwhile, in Japan..." (100, 200) 450 R B))
  ,
  Level
    -- Breeze through a channel of super-lightweight barriers.
    (Cannon (900, 500) 180 0)
    [ Phys.bouncyBarrier (300 + (140 * 0)) (500 + 80) 60 0.03
    , Phys.bouncyBarrier (300 + (140 * 1)) (500 + 80) 60 0.03
    , Phys.bouncyBarrier (300 + (140 * 2)) (500 + 80) 60 0.03
    , Phys.bouncyBarrier (300 + (140 * 3)) (500 + 80) 60 0.03
    , Phys.bouncyBarrier (300 + (140 * 0)) (500 - 80) 60 0.03
    , Phys.bouncyBarrier (300 + (140 * 1)) (500 - 80) 60 0.03
    , Phys.bouncyBarrier (300 + (140 * 2)) (500 - 80) 60 0.03
    , Phys.bouncyBarrier (300 + (140 * 3)) (500 - 80) 60 0.03
    ]
    []
    (Target (150, 500) 100)
    1
    0
    Nothing
  ,
  Level
    -- Try to bounce 90deg off a stationary barrier in order to land the ball on
    -- the target.
    (Cannon (100, 550) 14 0)
    [ Phys.bubbleBarrier 700 700 200
    , Phys.vertBarrier 500 51 450
    ]
    []
    (Target (587.5, 137.5) 75)
    1
    0
    Nothing

  ]

intermediate : Course
intermediate =
  [
  Level
    -- Place the target in the middle of a repellant force field.
    (Cannon (100, 900) 325 0)
    []
    [ Phys.Field (500, 500) 80 240 -1
    ]
    (Target (500.5, 500.5) 75)
    1
    0
    Nothing
  ,
  Level
    -- Shallow-angle aperture. Maybe bounce off nearby sphere?
    (Cannon (870, 420) 150 0)
    [ Phys.horizBarrier 51 500 340
    , Phys.horizBarrier (1000 - (340 + 51)) 500 340
    ]
    []
    (Target (130, 580) 100)
    1
    0
    Nothing
  ,
  Level
    -- Double bank shot.
    (Cannon (880, 850) 244.7 0)
    [ Phys.vertBarrier 333 51 666
    , Phys.vertBarrier 666 332 618
    ]
    []
    (Target (150, 200) 100)
    2
    0
    Nothing
  ,
  Level
    -- An attractive force field. You can skirt the field by banking off the top
    -- boundary.
    (Cannon (900, 150) 170 0)
    []
    [ Phys.Field (500, 500) 200 417 1
    ]
    (Target (150, 150) 100)
    1
    0
    Nothing
  ,
  Level
    -- One big stationary spherical barrier that takes the whole screen. You
    -- must navigate around margins.
    (Cannon (170, 170) 45 0)
    [ Phys.bubbleBarrier 500 500 400
    ]
    []
    (Target (850, 850) 100)
    3
    0
    Nothing
  ,
  Level
    -- PITA 2: Target is behind two 90deg barriers. You must get out of the
    -- channel first.
    (Cannon (900, 100) 135 0)
    [ Phys.horizBarrier 220 200 580
    , Phys.vertBarrier 800 200 580
    , Phys.bubbleBarrier 198 200 20
    , Phys.bubbleBarrier 800 802 20
    ]
    []
    (Target (650, 350) 100)
    2
    0
    Nothing
  ,
  Level
    -- Really annoying level of bouncies that don't cooperate but which is
    -- actually pretty consistently doable in three moves once you figure it out.
    (Cannon (100, 100) 45 0)
    [ Phys.bouncyBarrier 500 500 400 0.01
    , Phys.bouncyBarrier 150 850 60 1.0
    , Phys.bouncyBarrier 850 150 60 1.0
    ]
    []
    (Target (850, 850) 100)
    3
    0
    Nothing
  ,
  Level
    -- Place a bouncy barrier over the target. The player  must try to dislodge
    -- the barrier in order to score.
    (Cannon (900, 100) 135 0)
    [ Phys.bouncyBarrier 500 500 90 0.05
    ]
    []
    (Target (500, 500) 100)
    1
    0
    Nothing
  ,
  Level
    -- Try to thread the needle through a narrow gap between two stationary
    -- spherical barriers. Tricky!
    (Cannon (500, 150) 90 0)
    [ Phys.bubbleBarrier 379.5 500 100
    , Phys.bubbleBarrier 620.5 500 100
    ]
    []
    (Target (500, 720) 80)
    2
    0
    Nothing

  ]

advanced : Course
advanced =
  [
  Level
    -- Another double bank shot.
    (Cannon (500, 800) 270 0)
    [ Phys.horizBarrier 200 333 600
    , Phys.horizBarrier 200 666 600
    ]
    []
    (Target (500, 190) 100)
    2
    0
    Nothing
  ,
  Level
    -- Ridiculous bouncies to the max. Good one to just pull back and fire with
    -- full force.
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
    []
    (Target (850, 150) 100)
    2
    0
    Nothing
  ,
  Level
    -- A fun arrangement of a bubble barrier and an attractive field.
    (Cannon (100, 900) 315 0)
    [ Phys.bubbleBarrier 330 330 200
    ]
    [ Phys.Field (650, 650) 120 240 3
    ]
    (Target (850, 150) 100)
    1
    0
    Nothing
  ,
  Level
    -- Five stationary spheres that take up most of the playing field. Hard
    -- because reflection angles are impossible to predict.
    (Cannon (100, 500) 0 0)
    [ Phys.bubbleBarrier 500 500 210
    , Phys.bubbleBarrier 250 250 100
    , Phys.bubbleBarrier 250 750 100
    , Phys.bubbleBarrier 750 250 100
    , Phys.bubbleBarrier 750 750 100
    ]
    []
    (Target (850, 500) 100)
    3
    0
    Nothing
  ,
  Level
    -- A difficult course consiting of both lines and stationary spheres.
    (Cannon (150, 150) 0 0)
    [ Phys.horizBarrier 500 500 450
    , Phys.horizBarrier 50 300 450
    , Phys.horizBarrier 50 700 450
    , Phys.bubbleBarrier 250 500 125
    , Phys.bubbleBarrier 750 275 125
    , Phys.bubbleBarrier 750 725 125
    ]
    []
    (Target (150, 850) 100)
    2
    0
    Nothing
  ,
  Level
    -- Target is in a box with opening on wrong side.
    (Cannon (100, 500) 0 0)
    [ Phys.horizBarrier 200 199 600
    , Phys.horizBarrier 200 801 600
    , Phys.vertBarrier 200 200 600
    , Phys.vertBarrier 800 200 230
    , Phys.vertBarrier 800 570 230
    ]
    []
    (Target (500, 500) 100)
    3
    0
    Nothing
  ,
  Level
    -- Place the target between four attractive force fields.
    (Cannon (666, 368) 102 0)
    []
    [ Phys.Field (333, 333) 70 163 2
    , Phys.Field (666, 333) 70 163 2
    , Phys.Field (666, 666) 70 163 2
    , Phys.Field (333, 666) 70 163 2
    ]
    (Target (500, 500) 75)
    2
    0
    Nothing
  ,
  Level
    -- PITA 4: Long channel to navigate through.
    (Cannon (500, 500) 119 0)
    [ Phys.horizBarrier 200 199 600
    , Phys.horizBarrier 51 801 750
    , Phys.vertBarrier 800 200 600
    , Phys.vertBarrier 200 200 400
    ]
    []
    (Target (125, 875) 100)
    2
    0
    Nothing
  ,
  Level
    -- PITA 5: Lots of horiz barriers between you and target. You must do a mega
    -- multi-bank shot.
    (Cannon (500, 100) 90 0)
    [ Phys.horizBarrier 50 250 375
    , Phys.horizBarrier 575 250 375
    , Phys.horizBarrier 150 500 700
    , Phys.horizBarrier 50 750 375
    , Phys.horizBarrier 575 750 375
    ]
    []
    (Target (850, 850) 100)
    3
    0
    Nothing
  ]

courses : Courses
courses =
  Dict.fromList
    [ ("Beginner", beginner)
    , ("Intermediate", intermediate)
    , ("Advanced", advanced)
    ]
