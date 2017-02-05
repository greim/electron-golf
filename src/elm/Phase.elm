
-- module ----------------------------------------------------------------------

module Phase exposing (Phase(..))

-- import ----------------------------------------------------------------------

import Level exposing (Level)
import Transition exposing (Transition)
import Cannon exposing (Cannon)
import Phys

-- types -----------------------------------------------------------------------

type Phase
  = Starting (List Phys.Obj) Cannon
  | Playing Level
  | Transitioning Transition
  | Ending (List Phys.Obj)
