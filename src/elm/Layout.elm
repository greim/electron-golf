
-- module ----------------------------------------------------------------------

module Layout exposing
  ( Layout
  , from
  )

-- import ----------------------------------------------------------------------

import Window exposing (Size)

-- types -----------------------------------------------------------------------

type alias Layout =
  { left : Float
  , top : Float
  , width : Float
  , height : Float
  , fontSize : Float
  }

-- functions -------------------------------------------------------------------

from : Size -> Layout
from viewport =
  let
    w = toFloat viewport.width
    h = toFloat viewport.height
    ratio = w / h
    isHoriz = ratio > 1.0
    siz = if isHoriz then h else w
    mainLeft = if isHoriz then (w - siz) / 2 else 0
    mainTop = if isHoriz then 0 else (h - siz) / 2
    mainWidth = siz
    mainHeight = siz
    fontSize = siz / 100
  in
    Layout mainLeft mainTop mainWidth mainHeight fontSize

-- values ----------------------------------------------------------------------

-- helpers ---------------------------------------------------------------------
