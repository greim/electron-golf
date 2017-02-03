
-- module ----------------------------------------------------------------------

module Cannon exposing (Cannon, thrownCannon, tweenCannon)

-- types -----------------------------------------------------------------------

type alias Cannon =
  { pos : (Float, Float)
  , angle : Float
  , power : Float
  }

-- functions -------------------------------------------------------------------

tweenCannon : Float -> Cannon -> Cannon -> Cannon
tweenCannon prog old new =
  let
    (x1, y1) = old.pos
    (x2, y2) = new.pos
    x = x1 + ((x2 - x1) * prog)
    y = y1 + ((y2 - y1) * prog)
    newPos = (x, y)
    newAngle = old.angle + ((new.angle - old.angle) * prog)
  in
    { old | pos = newPos, angle = newAngle }

-- values ----------------------------------------------------------------------

thrownCannon : Cannon
thrownCannon =
  Cannon (1500, 1500) 720 0
