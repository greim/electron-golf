
-- module ----------------------------------------------------------------------

module View exposing (..)

-- import ----------------------------------------------------------------------

--import Html exposing (Html)
--import Html.Attributes as HAttr
--import Html.Events as HEv
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Phys exposing (areaWidth, areaHeight)

-- functions -------------------------------------------------------------------

playingFieldWrapper : number -> number -> List (Svg msg) -> Svg msg
playingFieldWrapper width height children =
  Svg.svg [ viewBox, attrWidth width, attrHeight height ] children

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

attrClass : String -> Svg.Attribute msg
attrClass cls = SAttr.class cls

attrViewBox : number -> number -> number -> number -> Svg.Attribute msg
attrViewBox x y w h =
  SAttr.viewBox ((toString x) ++ " " ++ (toString y) ++ " " ++ (toString w) ++ " " ++ (toString h))
