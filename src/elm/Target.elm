
-- module ----------------------------------------------------------------------

module Target exposing (Target, center, thrownTarget, tweenTarget)

-- types -----------------------------------------------------------------------

type alias Target =
  { pos : (Float, Float, Float, Float)
  }

-- functions -------------------------------------------------------------------

center : Target -> (Float, Float)
center target =
  let
    (x, y, w, h) = target.pos
    cx = x + w / 2
    cy = y + h / 2
  in
    (cx, cy)

-- values ----------------------------------------------------------------------

thrownTarget : Target
thrownTarget =
  Target (-1500, -1500, 0, 0)

tweenTarget : Float -> Target -> Target -> Target
tweenTarget prog old new =
  let
    (x1, y1, w1, h1) = old.pos
    (x2, y2, w2, h2) = new.pos
    x = x1 + ((x2 - x1) * prog)
    y = y1 + ((y2 - y1) * prog)
    w = w1 + ((w2 - w1) * prog)
    h = h1 + ((h2 - h1) * prog)
  in
    Target (x, y, w, h)
