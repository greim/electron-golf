module Shapes exposing (..)


type alias Point =
    ( Float, Float )


type alias Angle =
    Float


type alias Segment =
    ( Point, Point )


type alias Arc =
    { center : Point
    , startAngle : Angle
    , endAngle : Angle
    , radius : Float
    }


type alias Trajectory =
    Segment


abc : Segment -> ( Float, Float, Float )
abc seg =
    let
        ( ( x1, y1 ), ( x2, y2 ) ) =
            seg

        a =
            y2 - y1

        b =
            x1 - x2

        c =
            (a * x1) + (b * y1)
    in
        ( a, b, c )


strikesSegment : Segment -> Trajectory -> Maybe Point
strikesSegment seg traj =
    let
        ( a1, b1, c1 ) =
            abc traj

        ( a2, b2, c2 ) =
            abc seg

        det =
            (a1 * b2) - (a2 * b1)
    in
        if det == 0 then
            Nothing
        else
            let
                x =
                    ((b2 * c1) - (b1 * c2)) / det

                y =
                    ((a1 * c2) - (a2 * c1)) / det

                ( ( x1, y1 ), ( x2, y2 ) ) =
                    seg

                maxX =
                    max x1 x2

                minX =
                    min x1 x2

                maxY =
                    max y1 y2

                minY =
                    min y1 y2
            in
                if x < minX || x > maxX || y < minY || y > maxY then
                    Nothing
                else
                    Just ( x, y )



--strikesArc : Arc -> Trajectory -> Maybe Point
--strikesArc arc traj =
