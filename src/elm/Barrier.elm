module Barrier exposing (..)


type alias Point =
    ( Float, Float )


type alias Barrier =
    { endA : Point
    , endB : Point
    , length : Float
    , expansion : Float
    }


intersects 
