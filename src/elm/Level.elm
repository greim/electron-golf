
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

  ----------------------------------------------------------------------------
  -- Training. Super easy setup.
  Level
    (Cannon (200, 300) 34 0)
    []
    []
    (Target ((800 - 75), (700 - 75), 150, 150))
    1
    0
    (Just (Message "Timing is everything. The longer you charge the cannon, the faster the electron travels." (160, 160) 320 L B))

  ----------------------------------------------------------------------------
  -- Training. Opposite-end target. Cannon not perfectly lined up.
  , Level
    (Cannon (800, 800) 234.6 0)
    []
    []
    (Target (150, 150, 150, 150))
    1
    0
    (Just (Message "While aiming, don't forget to press SHIFT or ALT to modify the rotation speed." (150, 150) 320 R T))

  ----------------------------------------------------------------------------
  -- Training. A very introductory level with a big, close target. Helpful
  -- message.
  , Level
    (Cannon (100, 100) 10 0)
    []
    []
    (Target (200, 200, 150, 150))
    1
    0
    (Just (Message "It's a little touchy at close range. Best to do a putt." (100, 100) 400 L B))

  ----------------------------------------------------------------------------
  -- In which we introduce the first barrier. Simple bank shot.
  , Level
    (Cannon (100, 100) 50 0)
    [ Phys.vertBarrier 500 50 600
    ]
    []
    (Target (750, 100, 150, 150))
    2
    0
    (Just (Message "Circumvent barriers by doing bank shots." (100, 100) 250 L B))

  ----------------------------------------------------------------------------
  -- Introduce a simple force field the player must work around. No other
  -- obstacles.
  , Level
    (Cannon (500, 100) 90 0)
    []
    [ Phys.Field (370, 500) 50 200 -1
    ]
    (Target (450, 800, 100, 100))
    2
    0
    (Just (Message "Electrons have a negative charge. Like repels like, while opposites attract. Electrical fields will either attract or repel your electron, depending on their charge." (100, 100) 330 R T))

  ----------------------------------------------------------------------------
  -- A trick level. Player presumably hasn't seen bouncy barriers yet so
  -- thinks this level is impossible, since no openings to target are visible.
  -- Little do they know that some barriers move.
  , Level
    (Cannon (500, 900) 270 0)
    [ Phys.horizBarrier 51 400 222
    , Phys.bouncyBarrier 387 400 111 0.01
    , Phys.bouncyBarrier 613 400 111 0.01
    , Phys.horizBarrier 725 400 222
    ]
    []
    (Target (450, 100, 100, 100))
    3
    0
    (Just (Message "Just go for it." (100, 200) 450 R B))

  ----------------------------------------------------------------------------
  -- Start from the center and try to bounce the ball into a narrow channel.
  , Level
    (Cannon (100, 100) 45 0)
    [ Phys.vertBarrier 775 51 800
    ]
    []
    (Target (825, 100, 75, 75))
    2
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Breeze through a channel of super-lightweight barriers.
  , Level
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
    (Target (100, 450, 100, 100))
    1
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Try to bounce 90deg off a stationary barrier in order to land the ball on
  -- the target.
  , Level
    (Cannon (100, 550) 14 0)
    [ Phys.bubbleBarrier 700 700 200
    , Phys.vertBarrier 500 51 450
    ]
    []
    (Target (550, 100, 75, 75))
    2
    0
    Nothing

  ]

intermediate : Course
intermediate =
  [

  ----------------------------------------------------------------------------
  -- Place the target in the middle of a repellant force field.
  Level
    (Cannon (100, 900) 325 0)
    []
    [ Phys.Field (500, 500) 80 240 -1
    ]
    (Target ((500 - (75 / 2)), (500 - (75 / 2)), 75, 75))
    1
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Shallow-angle aperture. Maybe bounce off nearby sphere?
  , Level
    (Cannon (880, 450) 202 0)
    [ Phys.horizBarrier 50 500 349
    , Phys.horizBarrier 601 500 349
    , Phys.bubbleBarrier 410 500 10
    , Phys.bubbleBarrier 590 500 10
    , Phys.bubbleBarrier 500 200 100
    ]
    []
    (Target (800, 800, 100, 100))
    2
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Double bank shot.
  , Level
    (Cannon (880, 850) 244.7 0)
    [ Phys.vertBarrier 333 51 666
    , Phys.vertBarrier 666 332 618
    ]
    []
    (Target (100, 150, 100, 100))
    2
    0
    Nothing

  ----------------------------------------------------------------------------
  -- An attractive force field. You can skirt the field by banking off the top.
  , Level
    (Cannon (900, 150) 170 0)
    []
    [ Phys.Field (500, 500) 200 417 1
    ]
    (Target (100, 100, 100, 100))
    1
    0
    Nothing

  ----------------------------------------------------------------------------
  -- One big stationary spherical barrier that takes the whole screen. You
  -- must navigate around margins.
  , Level
    (Cannon (170, 170) 45 0)
    [ Phys.bubbleBarrier 500 500 400
    ]
    []
    (Target (800, 800, 100, 100))
    3
    0
    Nothing

  ----------------------------------------------------------------------------
  -- A difficult course consiting of both lines and stationary spheres.
  , Level
    (Cannon (150, 150) 0 0)
    [ Phys.horizBarrier 500 500 450
    , Phys.horizBarrier 50 300 450
    , Phys.horizBarrier 50 700 450
    , Phys.bubbleBarrier 250 500 125
    , Phys.bubbleBarrier 750 275 125
    , Phys.bubbleBarrier 750 725 125
    ]
    []
    (Target (100, 800, 100, 100))
    3
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Really annoying level of bouncies that don't cooperate but which is
  -- actually pretty consistently doable in three moves once you figure it out.
  , Level
    (Cannon (100, 100) 45 0)
    [ Phys.bouncyBarrier 500 500 400 0.01
    , Phys.bouncyBarrier 150 850 60 1.0
    , Phys.bouncyBarrier 850 150 60 1.0
    ]
    []
    (Target (800, 800, 100, 100))
    3
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Place a bouncy barrier over the target. The player  must try to dislodge
  -- the barrier in order to score.
  , Level
    (Cannon (900, 100) 135 0)
    [ Phys.bouncyBarrier 500 500 90 0.05
    ]
    []
    (Target (450, 450, 100, 100))
    1
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Try to thread the needle through a narrow gap between two stationary
  -- spherical barriers. Tricky!
  , Level
    (Cannon (500, 150) 90 0)
    [ Phys.bubbleBarrier 379.5 500 100
    , Phys.bubbleBarrier 620.5 500 100
    ]
    []
    (Target (460, 700, 80, 80))
    2
    0
    Nothing

  ]

advanced : Course
advanced =
  [

  ----------------------------------------------------------------------------
  -- A fun arrangement of a bubble barrier and an attractive field.
  Level
    (Cannon (100, 900) 315 0)
    [ Phys.bubbleBarrier 330 330 200
    ]
    [ Phys.Field (650, 650) 120 240 3
    ]
    (Target (800, 100, 100, 100))
    1
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Ridiculous bouncies to the max. Good one to just pull back and fire with
  -- full force.
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
    []
    (Target (800, 100, 100, 100))
    2
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Another double bank shot.
  , Level
    (Cannon (500, 800) 270 0)
    [ Phys.horizBarrier 200 333 600
    , Phys.horizBarrier 200 666 600
    ]
    []
    (Target (450, 140, 100, 100))
    2
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Five stationary spheres that take up most of the playing field. Hard
  -- because reflection angles are impossible to predict.
  , Level
    (Cannon (100, 500) 0 0)
    [ Phys.bubbleBarrier 500 500 210
    , Phys.bubbleBarrier 250 250 100
    , Phys.bubbleBarrier 250 750 100
    , Phys.bubbleBarrier 750 250 100
    , Phys.bubbleBarrier 750 750 100
    ]
    []
    (Target (800, 450, 100, 100))
    3
    0
    Nothing

  ----------------------------------------------------------------------------
  -- Kind of a PITA: you have to shoot straight up and hopefully deflect out
  -- sideways so that you can take the second shot at the target.
  , Level
    (Cannon (900, 900) 270 0)
    [ Phys.vertBarrier 850 230 720
    , Phys.bubbleBarrier 900 100 25
    ]
    []
    (Target (690, 800, 100, 100))
    2
    0
    Nothing

  ----------------------------------------------------------------------------
  -- PITA 2: Target is behind two 90deg barriers. You must get out of the
  -- channel first.
  , Level
    (Cannon (900, 100) 135 0)
    [ Phys.horizBarrier 220 200 580
    , Phys.vertBarrier 800 200 580
    , Phys.bubbleBarrier 198 200 20
    , Phys.bubbleBarrier 800 802 20
    ]
    []
    (Target (600, 300, 100, 100))
    2
    0
    Nothing

  ----------------------------------------------------------------------------
  -- PITA 3: Target is in a box with opening on wrong side.
  , Level
    (Cannon (100, 500) 0 0)
    [ Phys.horizBarrier 200 199 600
    , Phys.horizBarrier 200 801 600
    , Phys.vertBarrier 200 200 600
    , Phys.vertBarrier 800 200 230
    , Phys.vertBarrier 800 570 230
    ]
    []
    (Target (450, 450, 100, 100))
    3
    0
    Nothing

  ----------------------------------------------------------------------------
  -- PITA 4: Long channel to navigate through.
  , Level
    (Cannon (500, 500) 135 0)
    [ Phys.horizBarrier 200 199 600
    , Phys.horizBarrier 51 801 750
    , Phys.vertBarrier 800 200 600
    , Phys.vertBarrier 200 200 200
    ]
    []
    (Target (75, 825, 100, 100))
    3
    0
    Nothing

  ----------------------------------------------------------------------------
  -- PITA 5: Lots of horiz barriers between you and target. You must do a mega
  -- multi-bank shot.
  , Level
    (Cannon (500, 100) 90 0)
    [ Phys.horizBarrier 50 250 375
    , Phys.horizBarrier 575 250 375
    , Phys.horizBarrier 150 500 700
    , Phys.horizBarrier 50 750 375
    , Phys.horizBarrier 575 750 375
    ]
    []
    (Target (800, 800, 100, 100))
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
