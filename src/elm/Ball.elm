module Ball exposing (Ball, next, new, intersectsWithAny)

import Barrier exposing (..)

type alias Ball =
  { x: Float
  , y: Float
  , radius: Float
  , velX: Float
  , velY: Float
  }

friction : Float
friction =
  0.975

new : Float -> Float -> Float -> Float -> Float -> Ball
new x y radius velX velY =
  Ball x y radius velX velY

next : Ball -> Ball
next ball =
  let
    velX2 = ball.velX * ball.velX
    velY2 = ball.velY * ball.velY
    absVelocity = sqrt (velX2 + velY2)
  in
    if absVelocity > 0.25 then
      let
        newX = ball.x + ball.velX
        newY = ball.y + ball.velY
        newVelX = ball.velX * friction
        newVelY = ball.velY * friction
      in
        { ball | x = newX, y = newY, velX = newVelX, velY = newVelY }
    else
      { ball | velX = 0, velY = 0 }

intersectsWithAny : List Barrier -> Ball -> Maybe Barrier
intersectsWithAny barriers ball =
  let
    filtered = List.filter (\barrier -> intersectsWith barrier ball) barriers
  in
    List.head filtered

intersectsWith : Barrier -> Ball -> Bool
intersectsWith barrier ball =
  (intersectsWithInteriorRect barrier ball) || (intersectsWithEnd1 barrier ball) || (intersectsWithEnd2 barrier ball)

intersectsWithInteriorRect : Barrier -> Ball -> Bool
intersectsWithInteriorRect barrier ball =
  let
    (x1, x2, y1, y2) = getInteriorRect barrier ball
  in
    not (ball.x < x1 || ball.x > x2 || ball.y < y1 || ball.y > y2)

intersectsWithEnd1 : Barrier -> Ball -> Bool
intersectsWithEnd1 barrier ball =
  let
    (x, y) = Barrier.endpoint1 barrier
    dist = hypotenuse ball.x ball.y x y
  in
    dist <= ball.radius

intersectsWithEnd2 : Barrier -> Ball -> Bool
intersectsWithEnd2 barrier ball =
  let
    (x, y) = Barrier.endpoint2 barrier
    hyp = hypotenuse ball.x ball.y x y
  in
    hyp <= ball.radius

hypotenuse : Float -> Float -> Float -> Float -> Float
hypotenuse x1 y1 x2 y2 =
  let
    xDiff = x2 - x1
    yDiff = y2 - y1
    xSq = xDiff * xDiff
    ySq = yDiff * yDiff
  in
    sqrt (xSq + ySq)

getInteriorRect : Barrier -> Ball -> (Float, Float, Float, Float)
getInteriorRect barrier ball =
  case barrier of
    Vertical x y len ->
      let
        x1 = x - ball.radius
        x2 = x + ball.radius
        y1 = y
        y2 = y + len
      in
        (x1, x2, y1, y2)
    Horizontal x y len ->
      let
        y1 = y - ball.radius
        y2 = y + ball.radius
        x1 = x
        x2 = x + len
      in
        (x1, x2, y1, y2)
