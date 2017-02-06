
-- module ----------------------------------------------------------------------

module View exposing (..)

-- import ----------------------------------------------------------------------

--import Html.Events as HEv
import Html exposing (Html)
import Html.Attributes as HAttr
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Phys exposing (areaWidth, areaHeight)
import Layout exposing (Layout)
import Markdown

-- types -----------------------------------------------------------------------

type PathCommand
  = M Float Float
  | L Float Float
  | H Float
  | V Float
  | Z

-- functions -------------------------------------------------------------------

drawPath : List PathCommand -> String
drawPath commands =
  case commands of
    command :: rest ->
      case command of
        M x y ->
          " m" ++ (toString x) ++ " " ++ (toString y) ++ (drawPath rest)
        L x y ->
          " l" ++ (toString x) ++ " " ++ (toString y) ++ (drawPath rest)
        H len ->
          " h" ++ (toString len) ++ (drawPath rest)
        V len ->
          " v" ++ (toString len) ++ (drawPath rest)
        Z ->
          " z" ++ (drawPath rest)
    [] ->
      ""

playingFieldWrapper : Layout -> List (Svg msg) -> Svg msg
playingFieldWrapper playport children =
  Svg.svg
    [ viewBox
    , attrWidth playport.width
    , attrHeight playport.height
    ]
    (defs :: children)

blurb : Html msg
blurb =
  Markdown.toHtml [HAttr.id "blurb"] """
Hi, my name is Greg Reimer. I thought it would be fun to try out the [Elm programming language](http://elm-lang.org/) by creating a video game, and this is the result. If you liked it, follow me on [Twitter](https://twitter.com/) or star the repo on [GitHub](https://github.com/greim/electron-golf).
"""

defs : Svg msg
defs =
  Svg.defs []
    [ Svg.radialGradient
      [ SAttr.id "power-radius-gradient"
      ]
      [ Svg.stop [ SAttr.offset "27%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      , Svg.stop [ SAttr.offset "30%", SAttr.stopColor "hsla(120, 100%, 50%, 0.5)" ] []
      , Svg.stop [ SAttr.offset "33%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []

      , Svg.stop [ SAttr.offset "37%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      , Svg.stop [ SAttr.offset "40%", SAttr.stopColor "hsla(120, 100%, 50%, .80)" ] []
      , Svg.stop [ SAttr.offset "43%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []

      , Svg.stop [ SAttr.offset "47%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      , Svg.stop [ SAttr.offset "50%", SAttr.stopColor "hsla(120, 100%, 50%, 1.0)" ] []
      , Svg.stop [ SAttr.offset "53%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []

      , Svg.stop [ SAttr.offset "57%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      , Svg.stop [ SAttr.offset "60%", SAttr.stopColor "hsla(120, 100%, 50%, .80)" ] []
      , Svg.stop [ SAttr.offset "63%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []

      , Svg.stop [ SAttr.offset "67%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      , Svg.stop [ SAttr.offset "70%", SAttr.stopColor "hsla(120, 100%, 50%, 0.5)" ] []
      , Svg.stop [ SAttr.offset "73%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      ]
    , Svg.radialGradient
      [ SAttr.id "explosion-gradient"
      ]
      [ Svg.stop [ SAttr.offset "20%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      , Svg.stop [ SAttr.offset "40%", SAttr.stopColor "hsla(120, 100%, 50%, 1.0)" ] []
      , Svg.stop [ SAttr.offset "60%", SAttr.stopColor "hsla(120, 100%, 50%, 1.0)" ] []
      , Svg.stop [ SAttr.offset "100%", SAttr.stopColor "hsla(120, 100%, 50%, 0.0)" ] []
      ]
    , Svg.radialGradient
      [ SAttr.id "spherical-gradient"
      , SAttr.cx "35%"
      , SAttr.cy "35%"
      , SAttr.r "70%"
      ]
      [ Svg.stop [ SAttr.offset "0%", SAttr.stopColor "#888" ] []
      , Svg.stop [ SAttr.offset "10%", SAttr.stopColor "#888" ] []
      , Svg.stop [ SAttr.offset "15%", SAttr.stopColor "#666" ] []
      , Svg.stop [ SAttr.offset "100%", SAttr.stopColor "#222" ] []
      ]
    , Svg.linearGradient
      [ SAttr.id "field-center-gradient"
      , SAttr.x1 "10%"
      , SAttr.x2 "20%"
      , SAttr.y1 "30%"
      , SAttr.y2 "40%"
      , SAttr.spreadMethod "repeat"
      ]
      [ Svg.stop [ SAttr.offset "0%", SAttr.stopColor "rgba(0,0,0,0.1)" ] []
      , Svg.stop [ SAttr.offset "50%", SAttr.stopColor "rgba(0,0,0,0.1)" ] []
      , Svg.stop [ SAttr.offset "50%", SAttr.stopColor "rgba(255,255,255,0.1)" ] []
      , Svg.stop [ SAttr.offset "100%", SAttr.stopColor "rgba(255,255,255,0.1)" ] []
      ]
    ]

val : a -> Html msg
val a =
  Html.strong [ HAttr.class "value" ] [ Html.text (toString a) ]

drawLine : String -> (Float, Float) -> (Float, Float) -> Svg msg
drawLine cls (x1, y1) (x2, y2) =
  Svg.line
    [ attrX1 x1
    , attrX2 x2
    , attrY1 y1
    , attrY2 y2
    , attrClass cls
    ] []

drawLinePlain : (Float, Float) -> (Float, Float) -> Svg msg
drawLinePlain (x1, y1) (x2, y2) =
  Svg.line
    [ attrX1 x1
    , attrX2 x2
    , attrY1 y1
    , attrY2 y2
    ] []

drawLineVert : String -> (Float, Float) -> Float -> Svg msg
drawLineVert cls (x, y) height =
  Svg.line
    [ attrX1 x
    , attrX2 x
    , attrY1 y
    , attrY2 (y + height)
    , attrClass cls
    ] []

drawLineHoriz : String -> (Float, Float) -> Float -> Svg msg
drawLineHoriz cls (x, y) width =
  Svg.line
    [ attrX1 x
    , attrX2 (x + width)
    , attrY1 y
    , attrY2 y
    , attrClass cls
    ] []

drawBox : String -> (Float, Float) -> (Float, Float) -> Svg msg
drawBox cls (x, y) (width, height) =
  Svg.rect
    [ attrX x
    , attrY y
    , attrWidth width
    , attrHeight height
    , attrClass cls
    ] []

drawEllipse : String -> (Float, Float) -> (Float, Float) -> Svg msg
drawEllipse cls (cx, cy) (rx, ry) =
  Svg.ellipse
    [ attrCX cx
    , attrCY cy
    , attrRX rx
    , attrRY ry
    , attrClass cls
    ] []

drawCirc : String -> (Float, Float) -> number -> Svg msg
drawCirc cls (cx, cy) radius =
  Svg.circle
    [ attrCX cx
    , attrCY cy
    , attrR radius
    , attrClass cls
    ] []

drawCircPlain : (Float, Float) -> Float -> Svg msg
drawCircPlain (cx, cy) radius =
  Svg.circle
    [ attrCX cx
    , attrCY cy
    , attrR radius
    ] []

drawCircExt : String -> (Float, Float) -> Float -> List (Svg.Attribute msg) -> Svg msg
drawCircExt cls (cx, cy) radius attrs =
  let
    clsa = attrClass cls
    cxa = attrCX cx
    cya = attrCY cy
    ra = attrR radius
    allAttrs = clsa :: (cxa :: (cya :: (ra :: attrs)))
  in
    Svg.circle allAttrs []

drawArrow : String -> (Float, Float) -> (Float, Float) -> Svg msg
drawArrow cls from to =
  group
    [ drawLine cls from to
    , drawCirc cls to 10
    ]

group : List (Svg msg) -> Svg msg
group children =
  Svg.g [] children

emptyGroup : Svg msg
emptyGroup = group []

labeledVal : String -> String -> Html msg
labeledVal label val =
  Html.span [ HAttr.class "section" ]
    [ Html.span [ HAttr.class "label" ] [ Html.text (label ++ ": ") ]
    , Html.span [ HAttr.class "value" ] [ Html.text val ]
    ]

labeledSlashVal : String -> String -> String -> Html msg
labeledSlashVal label val1 val2 =
  Html.span [ HAttr.class "section" ]
    [ Html.span [ HAttr.class "label" ] [ Html.text (label ++ ": ") ]
    , Html.span [ HAttr.class "value" ]
      [ Html.text val1
      , Html.span [ HAttr.class "slash" ] [ Html.text "/" ]
      , Html.text val2
      ]
    ]

-- helpers ---------------------------------------------------------------------

viewBox : Svg.Attribute msg
viewBox = attrViewBox 0 0 areaWidth areaHeight

attrX : number -> Svg.Attribute msg
attrX x = SAttr.x (toString x)

attrY : number -> Svg.Attribute msg
attrY y = SAttr.y (toString y)

attrWidth : number -> Svg.Attribute msg
attrWidth width = SAttr.width (toString width)

attrHeight : number -> Svg.Attribute msg
attrHeight height = SAttr.height (toString height)

attrCX : number -> Svg.Attribute msg
attrCX cx = SAttr.cx (toString cx)

attrCY : number -> Svg.Attribute msg
attrCY cy = SAttr.cy (toString cy)

attrX1 : number -> Svg.Attribute msg
attrX1 x1 = SAttr.x1 (toString x1)

attrY1 : number -> Svg.Attribute msg
attrY1 y1 = SAttr.y1 (toString y1)

attrX2 : number -> Svg.Attribute msg
attrX2 x2 = SAttr.x2 (toString x2)

attrY2 : number -> Svg.Attribute msg
attrY2 y2 = SAttr.y2 (toString y2)

attrR : number -> Svg.Attribute msg
attrR r = SAttr.r (toString r)

attrRX : number -> Svg.Attribute msg
attrRX rx = SAttr.rx (toString rx)

attrRY : number -> Svg.Attribute msg
attrRY ry = SAttr.ry (toString ry)

attrClass : String -> Svg.Attribute msg
attrClass cls = SAttr.class cls

attrViewBox : number -> number -> number -> number -> Svg.Attribute msg
attrViewBox x y w h =
  SAttr.viewBox ((toString x) ++ " " ++ (toString y) ++ " " ++ (toString w) ++ " " ++ (toString h))
