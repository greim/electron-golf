module V exposing (arrow, pendingArrow, unmarkedArrow, markedArrow, node, pendingNode, unmarkedNode, markedNode)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attr
import Debug exposing (log)

--------------------------------------------------------------------------------

pendingArrow : Int -> Int -> Int -> Int -> Svg msg
pendingArrow x1 y1 x2 y2 =
  rawArrow True x1 y1 x2 y2 "pending arrow" []

arrow : Bool -> Int -> Int -> Int -> Int -> List (Attribute msg) -> Svg msg
arrow isStrong x1 y1 x2 y2 attrs =
  rawArrow isStrong x1 y1 x2 y2 "arrow" attrs

unmarkedArrow : Bool -> Int -> Int -> Int -> Int -> List (Attribute msg) -> Svg msg
unmarkedArrow isStrong x1 y1 x2 y2 attrs =
  rawArrow isStrong x1 y1 x2 y2 "unmarked arrow" attrs

markedArrow : Bool -> Int -> Int -> Int -> Int -> List (Attribute msg) -> Svg msg
markedArrow isStrong x1 y1 x2 y2 attrs =
  rawArrow isStrong x1 y1 x2 y2 "marked arrow" attrs

rawArrow : Bool -> Int -> Int -> Int -> Int -> String -> List (Attribute msg) -> Svg msg
rawArrow isStrong x1 y1 x2 y2 cls attrs =
  let
    aHead = arrowHead x1 y1 x2 y2
    aBody = arrowBody x1 y1 x2 y2
    class = Attr.class (if isStrong then cls else cls ++ " weak")
  in
    Svg.g (class :: attrs) [ aHead, aBody ]

arrowBody : Int -> Int -> Int -> Int -> Svg msg
arrowBody = line

line : Int -> Int -> Int -> Int -> Svg msg
line x1 y1 x2 y2 =
  Svg.line
    [ Attr.x1 (toString x1)
    , Attr.y1 (toString y1)
    , Attr.x2 (toString x2)
    , Attr.y2 (toString y2)
    ] []

arrowHead : Int -> Int -> Int -> Int -> Svg msg
arrowHead x1 y1 x2 y2 =
  let
    xDiff = toFloat (x2 - x1)
    yDiff = toFloat (y2 - y1)
    hyp = sqrt (xDiff * xDiff + yDiff * yDiff)
    tooShort = hyp < 50.0
    angleRads = atan2 yDiff xDiff
    tr1 = translate x2 y2
    rot = rotate angleRads
    tr2 = translate -22 0
    tr = tr1 ++ rot ++ tr2
  in
    if tooShort then
      Svg.g [] []
    else
      Svg.g
        [ Attr.class "arrow-head", Attr.transform tr ]
        [ line 0 0 -20 -15, line 0 0 -20 15 ]

translate : Int -> Int -> String
translate x y =
  let
    xs = toString x
    ys = toString y
  in
    "translate(" ++ xs ++ "," ++ ys ++ ")"

rotate : Float -> String
rotate rads =
  let
    degStr = toString (rads * radConvert)
  in
    "rotate(" ++ degStr ++ ")"

radConvert : Float
radConvert =
  360.0 / (2.0 * pi)

--------------------------------------------------------------------------------

pendingNode : Int -> Int -> Svg msg
pendingNode cx cy =
  rawNode False cx cy "pending node" "" []

node : Bool -> Int -> Int -> String -> List (Attribute msg) -> Svg msg
node isRoot cx cy label attrs =
  rawNode isRoot cx cy "node" label attrs

unmarkedNode : Bool -> Int -> Int -> String -> List (Attribute msg) -> Svg msg
unmarkedNode isRoot cx cy label attrs =
  rawNode isRoot cx cy "unmarked node" label attrs

markedNode : Bool -> Int -> Int -> String -> List (Attribute msg) -> Svg msg
markedNode isRoot cx cy label attrs =
  rawNode isRoot cx cy "marked node" label attrs

rawNode : Bool -> Int -> Int -> String -> String -> List (Attribute msg) -> Svg msg
rawNode isRoot x y cls label attrs =
  let
    xs = toString x
    ys = toString y
    xa = Attr.x (toString (x + 30))
    ya = Attr.y ys
    cxa = Attr.cx xs
    cya = Attr.cy ys
    rOuter = Attr.r "20"
    rInner = Attr.r "9"
    class = Attr.class (if isRoot then "root " ++ cls else cls)
    circ = Svg.circle [ cxa, cya, rOuter ] []
    dot = Svg.circle [ cxa, cya, rInner, Attr.style "pointer-events:none", Attr.class "dot" ] []
  in
    if label == "" then
      Svg.g (class :: attrs) [ circ, dot ]
    else
      let text = Svg.text_ [xa, ya] [ Svg.text label ]
      in Svg.g (class :: attrs) [ circ, dot, text ]
