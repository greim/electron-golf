
-- module ----------------------------------------------------------------------

module Phase exposing (Phase(..))

-- import ----------------------------------------------------------------------

import Level exposing (Level)
import Transition exposing (Transition)

-- types -----------------------------------------------------------------------

type Phase
  = Starting
  | Playing Level
  | Transitioning Transition
  | Ending
