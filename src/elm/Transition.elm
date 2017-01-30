module Transition exposing (Transition, Phase(..), step, getCannonPos, getTargetPos, getExplodingCompleteness)

type Phase
  = Exploding Int
  | Messaging Int
  | Moving Int

type alias Transition a =
  { message : String
  , targetPosOrig : (Float, Float)
  , cannonPosOrig : (Float, Float)
  , targetPosNew : (Float, Float)
  , cannonPosNew : (Float, Float)
  , phase : Phase
  , from : a
  , to : a
  }

step : Transition a -> Maybe (Transition a)
step tr =
  case tr.phase of
    Exploding i ->
      if i < explodeSteps then
        Just { tr | phase = Exploding (i + 1) }
      else
        Just { tr | phase = Messaging 0 }
    Messaging i ->
      if i < messageSteps then
        Just { tr | phase = Messaging (i + 1) }
      else
        Just { tr | phase = Moving 0 }
    Moving i ->
      if i < moveSteps then
        Just { tr | phase = Moving (i + 1) }
      else
        Nothing

getCannonPos : Transition a -> (Float, Float)
getCannonPos tr =
  case tr.phase of
    Moving i ->
      tween i moveSteps tr.cannonPosOrig tr.cannonPosNew
    _ ->
      tr.targetPosOrig

getTargetPos : Transition a -> (Float, Float)
getTargetPos tr =
  case tr.phase of
    Moving i ->
      tween i moveSteps tr.targetPosOrig tr.targetPosNew
    _ ->
      tr.targetPosOrig

getExplodingCompleteness : Transition a -> Float
getExplodingCompleteness tr =
  case tr.phase of
    Exploding step ->
      (toFloat step) / (toFloat explodeSteps)
    _ -> 1.0

tween : Int -> Int -> (Float, Float) -> (Float, Float) -> (Float, Float)
tween idx total (x1, y1) (x2, y2) =
  let
    amount = (toFloat idx) / (toFloat total)
    x = x1 + ((x2 - x1) * amount)
    y = y1 + ((y2 - y1) * amount)
  in
    (x, y)

explodeSteps : Int
explodeSteps = 40

messageSteps : Int
messageSteps = 50

moveSteps : Int
moveSteps = 100
