
-- module ----------------------------------------------------------------------

module Transition exposing
  ( Transition
  , Phase(..)
  , step
  , migrateProgress
  , absorbProgress
  , explodeProgress
  , messageProgress
  , moveProgress
  , successful
  , unsuccessful
  )

-- types -----------------------------------------------------------------------

type Phase
  = Migrating Int
  | Absorbing Int
  | Exploding Int
  | Messaging Int
  | Moving Int

type alias Transition =
  { message : String
  , actionPoint : (Float, Float)
  , phase : Phase
  }

-- functions -------------------------------------------------------------------

successful : String -> (Float, Float) -> Transition
successful message (x, y) =
  Transition message (x, y) (Migrating 0)

unsuccessful : String -> (Float, Float) -> Transition
unsuccessful message (x, y) =
  Transition message (x, y) (Messaging -50)

step : Transition -> Maybe Transition
step tr =
  case tr.phase of
    Migrating i ->
      if i < migrateSteps then Just { tr | phase = Migrating (i + 1) }
      else Just { tr | phase = Absorbing 0 }
    Absorbing i ->
      if i < absorbSteps then Just { tr | phase = Absorbing (i + 1) }
      else Just { tr | phase = Exploding 0 }
    Exploding i ->
      if i < explodeSteps then Just { tr | phase = Exploding (i + 1) }
      else Just { tr | phase = Messaging 0 }
    Messaging i ->
      if i < messageSteps then Just { tr | phase = Messaging (i + 1) }
      else Just { tr | phase = Moving 0 }
    Moving i ->
      if i < moveSteps then Just { tr | phase = Moving (i + 1) }
      else Nothing

migrateProgress : Int -> Float
migrateProgress i =
  (toFloat i) / (toFloat migrateSteps)

absorbProgress : Int -> Float
absorbProgress i =
  (toFloat i) / (toFloat absorbSteps)

explodeProgress : Int -> Float
explodeProgress i =
  (toFloat i) / (toFloat explodeSteps)

messageProgress : Int -> Float
messageProgress i =
  (toFloat i) / (toFloat messageSteps)

moveProgress : Int -> Float
moveProgress i =
  (toFloat i) / (toFloat moveSteps)

-- helpers ---------------------------------------------------------------------

migrateSteps : Int
migrateSteps = 10

absorbSteps : Int
absorbSteps = 10

explodeSteps : Int
explodeSteps = 50

messageSteps : Int
messageSteps = 60

moveSteps : Int
moveSteps = 80
