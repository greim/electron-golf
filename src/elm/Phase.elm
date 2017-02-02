
-- module ----------------------------------------------------------------------

module Phase exposing (Phase(..))

-- import ----------------------------------------------------------------------

import Level exposing (Level)
import Transition exposing (Transition)
import Phys

-- types -----------------------------------------------------------------------

type Phase
  = Starting (List Phys.Obj)
  | Playing Level
  | Transitioning Transition
  | Ending (List Phys.Obj)
