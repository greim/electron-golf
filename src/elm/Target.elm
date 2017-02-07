
-- module ----------------------------------------------------------------------

module Target exposing (Target, thrownTarget, tweenTarget)

-- types -----------------------------------------------------------------------

type alias Target =
  { pos : (Float, Float)
  , size : Float
  }

-- functions -------------------------------------------------------------------

-- values ----------------------------------------------------------------------

thrownTarget : Target
thrownTarget =
  Target (-1500, -1500) 100

tweenTarget : Float -> Target -> Target -> Target
tweenTarget prog old new =
  let
    (x1, y1) = old.pos
    (x2, y2) = new.pos
    x = x1 + ((x2 - x1) * prog)
    y = y1 + ((y2 - y1) * prog)
    size = old.size + ((new.size - old.size) * prog)
  in
    Target (x, y) size
