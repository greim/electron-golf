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
  , isAtRest
  , step
  , areaWidth
  , areaHeight
  , playableWidth
  , playableHeight
  , gutter
  , splashBouncers
  , stepSplashBouncers
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

playableWidth : Float
playableWidth = areaWidth - (gutter * 2)

playableHeight : Float
playableHeight = areaHeight - (gutter * 2)

bounds : List Obj
bounds =
  let
    (xLo, yLo, xHi, yHi) = boundVals
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

splashBouncers : List Obj
splashBouncers =
  [ ball (100, 100) (45 + 10) 10
  --, ball (900, 100) (45 + 20) (8 / 2)
  --, ball (900, 900) (45 + 30) (9 / 2)
  --, ball (100, 900) (45 + 40) (5 / 2)
  --, ball (800, 200) (45 + 25) (8 / 2)
  --, ball (800, 700) (45 + 35) (9 / 2)
  --, ball (500, 500) (45 + 45) (5 / 2)
  ]

stepSplashBouncers : List Obj -> List Obj
stepSplashBouncers balls =
  let
    bodies = balls ++ bounds
    newBodies = BoxesAndBubbles.step (0, 0) (0, 0) bodies
    newBalls = List.filter isBall newBodies
  in
    newBalls

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
    {-
     - Two opposing goals:
     -
     - 1. The ball should appear to glide effortlessly in a low-friction
     -    environment.
     - 2. The ball should stop soon in order to speed along game play.
     -
     - To achieve #1, we multiply the current speed by a ratio which is
     - slightly below 1.0. The closer to 1.0, the longer the ball glides.
     -
     - To achieve #2, we subtract a very small constant from the speed, which
     - is slightly above 0.0. At high speeds, the effect of this subtraction is
     - negligible. As speed decreases, however, it begins to have a more
     - obvious effect.
     -
     - We apply both calculations. The result is that when the ball launches,
     - it appears to glide along frictionlessly, but as it slows down, it
     - steadily increases braking pressure, coming to a smooth and rapid stop.
     -
     - #1 could be applied by simply multiplying the ratio with each leg of the
     - velocity tuple, since it's a ratio, however applying #2 in that manner
     - would cause the ball to curve, since it isn't a ratio. So instead we
     - need to calculate the hypotenuse, then apply steps #1 and #2 to that to
     - find the slowdown ratio, then multiply *that* by each leg of the
     - velocity tuple.
     -}

    -- hypotenuse
    speed = sqrt (xVel * xVel + yVel * yVel)

    -- constants
    slowDownConstant = 0.99
    brakingConstant = 0.0075

    -- find the new speed, however this is still a hypotenuse
    newSpeed = max 0.0 ((speed * slowDownConstant) - brakingConstant)

    -- here's our final slowdown ratio
    slowDown = newSpeed / speed

    -- apply to each leg
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
