-- module ----------------------------------------------------------------------

module Phys exposing
  ( Obj
  , Role(..)
  , ball
  , vertBarrier
  , horizBarrier
  , boxBarrier
  , bouncyBarrier
  , bubbleBarrier
  , bounds
  , isAtRest
  , step
  )

-- imports ---------------------------------------------------------------------

import BoxesAndBubbles.Bodies exposing (Body)
import BoxesAndBubbles exposing (step, box, bubble)

-- types -----------------------------------------------------------------------

type Role = Ball | Barrier | Bound

type alias Obj =
  Body Role

-- values ----------------------------------------------------------------------

areaWidth : Float
areaWidth = 1000

areaHeight : Float
areaHeight = 1000

gutter : Float
gutter = 50

boundVals : (Float, Float, Float, Float)
boundVals = (gutter, gutter, areaWidth - gutter, areaHeight - gutter)

bounds : List Obj
bounds =
  let
    (xLo, yLo, xHi, yHi) = boundVals
    playableWidth = areaWidth - (gutter * 2)
    playableHeight = areaHeight - (gutter * 2)
  in
    List.map (\b -> { b | meta = Bound })
      [ boxBarrier 0 0 (gutter - 1) (gutter - 1)
      , boxBarrier (xHi + 1) 0 (gutter - 1) (gutter - 1)
      , boxBarrier (xHi + 1) (yHi + 1) (gutter - 1) (gutter - 1)
      , boxBarrier 0 (yHi + 1) (gutter - 1) (gutter - 1)
      , boxBarrier xLo 0 playableWidth gutter
      , boxBarrier xHi yLo gutter playableHeight
      , boxBarrier xLo yHi playableWidth gutter
      , boxBarrier 0 yLo gutter playableHeight
      ]

-- functions -------------------------------------------------------------------

ball : (Float, Float) -> Float -> Float -> Obj
ball pos degreeAngle power =
  let
    velX = (cos (degrees degreeAngle)) * power
    velY = (sin (degrees degreeAngle)) * power
    radius = 22
    density = 1.0
    restitution = 1.0
  in
    bubble radius density normalRestitution pos (velX, velY) Ball

vertBarrier : Float -> Float -> Float -> Obj
vertBarrier x y height =
  let width = 1
  in boxBarrier x y width height

horizBarrier : Float -> Float -> Float -> Obj
horizBarrier x y width =
  let height = 1
  in boxBarrier x y width height

boxBarrier : Float -> Float -> Float -> Float -> Obj
boxBarrier x y width height =
  let
    centerX = x + (width / 2)
    centerY = y + (height / 2)
  in
    box (width, height) infiniteDensity normalRestitution (centerX, centerY) zeroVelocity Barrier

bouncyBarrier : Float -> Float -> Float -> Float -> Obj
bouncyBarrier cx cy radius density =
  bubble radius density normalRestitution (cx, cy) zeroVelocity Barrier

bubbleBarrier : Float -> Float -> Float -> Obj
bubbleBarrier cx cy radius =
  bubble radius infiniteDensity normalRestitution (cx, cy) zeroVelocity Barrier

step : Obj -> List Obj -> (Bool, Bool, Obj, List Obj)
step ball barriers =
  let
    bodies = ball :: (barriers ++ bounds)
    newBodies = BoxesAndBubbles.step (0, 0) (0, 0) bodies
    slowerBodies = List.map reduceBodyVelocity newBodies
    newBall = findFirst isBall ball slowerBodies
    newBarriers = List.filter isBarrier slowerBodies
    (x, y) = newBall.pos
    (xLo, yLo, xHi, yHi) = boundVals
    isOOB = x <= xLo || x >= xHi || y <= yLo || y >= yHi
    atRest = isAtRest ball barriers
  in
    (atRest, isOOB, newBall, newBarriers)

isAtRest : Obj -> List Obj -> Bool
isAtRest ball barriers =
  let bodies = ball :: barriers
  in everyItem isZeroVelocity bodies

--------------------------------------------------------------------------------
-- helpers

infiniteDensity : Float
infiniteDensity = 1.0 / 0.0

zeroVelocity : (Float, Float)
zeroVelocity = (0, 0)

normalRestitution : Float
normalRestitution = 1.0

reduceBodyVelocity : Body meta -> Body meta
reduceBodyVelocity  bod =
  let
    newVelocity = reduceVelocity bod.velocity
    newBod = { bod | velocity = newVelocity }
  in
    newBod

reduceVelocity : (Float, Float) -> (Float, Float)
reduceVelocity (xVel, yVel) =
  let
    speed = sqrt (xVel * xVel + yVel * yVel)
    newSpeed = max 0.0 ((speed * 0.99) - 0.0075)
    slowDown = newSpeed / speed
    newXVel = xVel * slowDown
    newYVel = yVel * slowDown
  in
    (if isNaN newXVel then 0 else newXVel, if isNaN newYVel then 0 else newYVel)

findFirst : (a -> Bool) -> a -> List a -> a
findFirst fn default list =
  case list of
    a :: rest ->
      if fn a then
        a
      else
        findFirst fn default rest
    [] ->
      default

isBall : Obj -> Bool
isBall body =
  case body.meta of
    Ball -> True
    _ -> False

isBarrier : Obj -> Bool
isBarrier body =
  case body.meta of
    Barrier -> True
    _ -> False

isZeroVelocity : Obj -> Bool
isZeroVelocity obj =
  obj.velocity == zeroVelocity

everyItem : (a -> Bool) -> List a -> Bool
everyItem fn things =
  case things of
    thing :: rest ->
      if fn thing then
        everyItem fn rest
      else
        False
    [] ->
      True
