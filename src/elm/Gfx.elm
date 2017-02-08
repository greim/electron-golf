
-- package ---------------------------------------------------------------------

{-
A library of SVG renderers.
-}

module Gfx exposing
  ( cannon
  , target
  , ball
  )

-- imports ---------------------------------------------------------------------

import Debug exposing (log)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attr
import Svg.Lazy exposing (lazy, lazy2, lazy3)

-- types -----------------------------------------------------------------------

type PathCommand
  = M Float Float
  | L Float Float
  | H Float
  | V Float
  | Z

-- functions -------------------------------------------------------------------

cannon : (Float, Float) -> Float -> Float -> Svg msg
cannon (cx, cy) power angle =
  let
    traStr = translateString cx cy
    rotStr = rotateString angle
  in
    Svg.g
      [ Attr.transform (traStr ++ rotStr)
      , Attr.class "cannon"
      ]
      [ basicCannon
      , cannonPower power
      ]

basicCannon : Svg msg
basicCannon =
  Svg.g []
    [ box "barrel" (20, -10) (10, 20)
    , box "barrel" (32, -11) (6, 22)
    , circ "cannon-body" (0, 0) 20
    , line "launch-path" (20, 0) (2100, 0)
    ]

cannonPower : Float -> Svg msg
cannonPower power =
  let
    powerRadius = (power * 2.6) ^ 1.2
    powerOpac = (min 1.0 (1.0 - (powerRadius / 1000))) * 0.2
    opacStyle = "opacity:" ++ (toString powerOpac)
    strokeWidth = (toString (powerRadius / 30))
  in
    case power of
      0 ->
        emptyGroup
      _ ->
        Svg.g
          [ Attr.class "power-gauge"
          ]
          [ circ "cannon-drawback" (-power, 0) 16
          , circExt "power-radius" (0, 0) (powerRadius) [Attr.style opacStyle, Attr.strokeWidth strokeWidth]
          , circ "gauge-dot-bg" (0, 0) 15
          , circ "gauge-dot-bg" (-power, 0) 15
          , lineHoriz "gauge-stretch-bg" (-power, 0) power
          , lineHoriz "gauge-stretch" (-power, 0) power
          , circ "gauge-dot" (0, 0) 8
          , circ "gauge-dot" (-power, 0) 8
          ]

--------------------------------------------------------------------------------

ball : (Float, Float) -> Float -> Svg msg
ball pos r =
  circExt "ball" pos r [ sphereFill ]

--------------------------------------------------------------------------------

target : (Float, Float) -> Float -> Float -> Svg msg
target (tx, ty) protonSize size =
  let
    rFixed = (size / 2) * 0.65
    r = rFixed * protonSize
  in
    Svg.g
      [ Attr.class "target"
      ]
      [ emptyTargetLazy tx ty size
      , circExt "proton" (tx, ty) r [ sphereFill ]
      ]

emptyTargetLazy : Float -> Float -> Float -> Svg msg
emptyTargetLazy = lazy3 emptyTarget

emptyTarget : Float -> Float -> Float -> Svg msg
emptyTarget tx ty size =
  let
    half = size / 2
    x = tx - half
    y = ty - half
    translate = "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
    rFixed = half * 0.65
  in
    Svg.g
      [ Attr.transform translate
      ]
      [ box "target-bound" (0, 0) (size, size)
      , targetCorner 0 0 20 20
      , targetCorner size 0 -20 20
      , targetCorner size size -20 -20
      , targetCorner 0 size 20 -20
      , circ "proton-surround" (half, half) rFixed
      , path "target-corner" [ M half -5, V 10, V -5, Z ]
      , path "target-corner" [ M half (size + 5), V -10, V 5, Z ]
      , path "target-corner" [ M -5 half, H 10, H -5, Z ]
      , path "target-corner" [ M (size + 5) half, H -10, H 5, Z ]
      ]

targetCorner : Float -> Float -> Float -> Float -> Svg msg
targetCorner origX origY extX extY =
  let
    commands =
      [ M origX (origY + extY)
      , V -extY
      , H extX
      , H -extX
      , Z
      ]
  in
    path "target-corner" commands

-- helpers ---------------------------------------------------------------------

sphereFill : Attribute msg
sphereFill = Attr.fill "url(#spherical-gradient)"

line : String -> (Float, Float) -> (Float, Float) -> Svg msg
line cls (x1, y1) (x2, y2) =
  Svg.line
    [ x1_ x1
    , x2_ x2
    , y1_ y1
    , y2_ y2
    , Attr.class cls
    ] []

linePlain : (Float, Float) -> (Float, Float) -> Svg msg
linePlain (x1, y1) (x2, y2) =
  Svg.line
    [ x1_ x1
    , x2_ x2
    , y1_ y1
    , y2_ y2
    ] []

lineVert : String -> (Float, Float) -> Float -> Svg msg
lineVert cls (x, y) height =
  Svg.line
    [ x1_ x
    , x2_ x
    , y1_ y
    , y2_ (y + height)
    , Attr.class cls
    ] []

lineHoriz : String -> (Float, Float) -> Float -> Svg msg
lineHoriz cls (x, y) width =
  Svg.line
    [ x1_ x
    , x2_ (x + width)
    , y1_ y
    , y2_ y
    , Attr.class cls
    ] []

box : String -> (Float, Float) -> (Float, Float) -> Svg msg
box cls (x, y) (width, height) =
  Svg.rect
    [ x_ x
    , y_ y
    , width_ width
    , height_ height
    , Attr.class cls
    ] []

ellipse : String -> (Float, Float) -> (Float, Float) -> Svg msg
ellipse cls (cx, cy) (rx, ry) =
  Svg.ellipse
    [ cx_ cx
    , cy_ cy
    , rx_ rx
    , ry_ ry
    , Attr.class cls
    ] []

circ : String -> (Float, Float) -> number -> Svg msg
circ cls (cx, cy) radius =
  Svg.circle
    [ cx_ cx
    , cy_ cy
    , r_ radius
    , Attr.class cls
    ] []

circPlain : (Float, Float) -> Float -> Svg msg
circPlain (cx, cy) radius =
  Svg.circle
    [ cx_ cx
    , cy_ cy
    , r_ radius
    ] []

circExt : String -> (Float, Float) -> Float -> List (Svg.Attribute msg) -> Svg msg
circExt cls (cx, cy) radius attrs =
  let
    clsa = Attr.class cls
    cxa = cx_ cx
    cya = cy_ cy
    ra = r_ radius
    allAttrs = clsa :: (cxa :: (cya :: (ra :: attrs)))
  in
    Svg.circle allAttrs []

arrow : String -> (Float, Float) -> (Float, Float) -> Svg msg
arrow cls from to =
  group
    [ line cls from to
    , circ cls to 10
    ]

group : List (Svg msg) -> Svg msg
group children =
  Svg.g [] children

emptyGroup : Svg msg
emptyGroup = group []

x1_ : number -> Attribute msg
x1_ x1 = Attr.x1 (toString x1)

x2_ : number -> Attribute msg
x2_ x2 = Attr.x2 (toString x2)

y1_ : number -> Attribute msg
y1_ y1 = Attr.y1 (toString y1)

y2_ : number -> Attribute msg
y2_ y2 = Attr.y2 (toString y2)

x_ : number -> Attribute msg
x_ x = Attr.x(toString  x)

y_ : number -> Attribute msg
y_ y = Attr.y(toString  y)

width_ : number -> Attribute msg
width_ w = Attr.width(toString  w)

height_ : number -> Attribute msg
height_ h = Attr.height(toString  h)

cx_ : number -> Attribute msg
cx_ cx = Attr.cx (toString cx)

cy_ : number -> Attribute msg
cy_ cy = Attr.cy (toString cy)

rx_ : number -> Attribute msg
rx_ rx = Attr.rx (toString rx)

ry_ : number -> Attribute msg
ry_ ry = Attr.ry (toString ry)

r_ : number -> Attribute msg
r_ r = Attr.r(toString  r)

viewBox_ : number -> number -> number -> number -> Attribute msg
viewBox_ x y w h =
  Attr.viewBox ((toString x) ++ " " ++ (toString y) ++ " " ++ (toString w) ++ " " ++ (toString h))

translateString : number -> number -> String
translateString x y =
  "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"

rotateString : number -> String
rotateString angle =
  "rotate(" ++ (toString angle) ++ ")"

path : String -> List PathCommand -> Svg msg
path cls commands =
  Svg.path [Attr.class cls, Attr.d (pathString commands)] []

pathString : List PathCommand -> String
pathString commands =
  case commands of
    command :: rest ->
      case command of
        M x y ->
          " m" ++ (toString x) ++ " " ++ (toString y) ++ (pathString rest)
        L x y ->
          " l" ++ (toString x) ++ " " ++ (toString y) ++ (pathString rest)
        H len ->
          " h" ++ (toString len) ++ (pathString rest)
        V len ->
          " v" ++ (toString len) ++ (pathString rest)
        Z ->
          " z" ++ (pathString rest)
    [] ->
      ""
