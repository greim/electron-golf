module Transition exposing (Transition, Phase(..), step, getExplodingCompleteness)

type Phase
  = Exploding Int
  | Messaging Int

type alias Transition a =
  { message : String
  , explosionPoint : (Float, Float)
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
        Nothing

getExplodingCompleteness : Transition a -> Float
getExplodingCompleteness tr =
  case tr.phase of
    Exploding step ->
      (toFloat step) / (toFloat explodeSteps)
    _ -> 1.0

explodeSteps : Int
explodeSteps = 40

messageSteps : Int
messageSteps = 50
