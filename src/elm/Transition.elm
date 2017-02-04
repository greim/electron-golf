
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
  = Migrating Float
  | Absorbing Float
  | Exploding Float
  | Messaging Float
  | Moving Float

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

migrateProgress : Float -> Float
migrateProgress i =
  i / migrateSteps

absorbProgress : Float -> Float
absorbProgress i =
  i / absorbSteps

explodeProgress : Float -> Float
explodeProgress i =
  i / explodeSteps

messageProgress : Float -> Float
messageProgress i =
  i / messageSteps

moveProgress : Float -> Float
moveProgress i =
  i / moveSteps

-- helpers ---------------------------------------------------------------------

migrateSteps : Float
migrateSteps = 7

absorbSteps : Float
absorbSteps = 7

explodeSteps : Float
explodeSteps = 30

messageSteps : Float
messageSteps = 60

moveSteps : Float
moveSteps = 80
