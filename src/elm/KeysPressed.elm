
-- module ----------------------------------------------------------------------

module KeysPressed exposing (KeysPressed, init)

-- import ----------------------------------------------------------------------

import Time exposing (Time)

-- types -----------------------------------------------------------------------

type alias KeysPressed =
  { shift: Maybe Time
  , alt: Maybe Time
  , meta: Maybe Time
  , space: Maybe Time
  , ctrl: Maybe Time
  , left: Maybe Time
  , right: Maybe Time
  , up: Maybe Time
  , down: Maybe Time
  }

-- values ----------------------------------------------------------------------

init : KeysPressed
init = KeysPressed Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
