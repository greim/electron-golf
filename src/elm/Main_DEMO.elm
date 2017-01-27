module Main exposing (..)

{-| # Overview
A basic example of using BoxesAndBubbles.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.

# Running

@docs main

-}

import Html exposing (program)
import BoxesAndBubbles.Bodies exposing (..)
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Text exposing (fromString)
import AnimationFrame
import Time exposing (Time)


inf =
    1 / 0



-- infinity, hell yeah


e0 =
    0.99



-- default restitution coefficient
-- box: (w,h) pos velocity density restitution
-- bubble: radius pos velocity density restitution


type alias Model meta =
    List (Body meta)


defaultLabel =
    ""


someBodies =
    [ bubble 30 1 e0 ( -80, 0 ) ( 13.5, 10 ) defaultLabel
      --, bubble 70 inf 0 ( 80, 0 ) ( 0, 0 ) defaultLabel
      --, bubble 40 1 e0 ( 0, 200 ) ( 0.4, -3.0 ) defaultLabel
      --, bubble 80 0.1 e0 ( 300, -280 ) ( -2, 1 ) defaultLabel
      --, bubble 15 5 0.4 ( 300, 300 ) ( -4, -3 ) defaultLabel
      --, bubble 40 1 e0 ( 200, 200 ) ( -5, -1 ) defaultLabel
      --, box ( 100, 100 ) 1 e0 ( 300, 0 ) ( 0, 0 ) defaultLabel
      --, box ( 20, 20 ) 1 e0 ( -200, 0 ) ( 3, 0 ) defaultLabel
      --, box ( 20, 40 ) 1 e0 ( 200, -200 ) ( -1, -1 ) defaultLabel
    ]
        ++ bounds ( 750, 750 ) 100 e0 ( 0, 0 ) defaultLabel



-- we'll just compute the label from the data in the body


bodyLabel restitution inverseMass =
    [ "e = ", toString restitution, "\nm = ", toString (round (1 / inverseMass)) ] |> String.concat


type alias Labeled =
    { label : String }


type alias LabeledBody =
    Body Labeled



--attachlabel label body =
--  let labelRecord = { label = label }
--  in { body }
-- and attach it to all the bodies


labeledBodies : Model String
labeledBodies =
    List.map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies



-- why yes, it draws a body with label. Or creates the Element, rather


drawBody { pos, velocity, inverseMass, restitution, shape, meta } =
    let
        veloLine =
            segment ( 0, 0 ) (mul2 velocity 5) |> traced (solid red)

        info =
            meta |> fromString |> centered |> toForm

        ready =
            case shape of
                Bubble radius ->
                    group
                        [ circle radius |> outlined (solid black)
                        , info |> move ( 0, radius + 16 )
                        , veloLine
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                        group
                            [ rect (w * 2) (h * 2) |> outlined (solid black)
                            , info |> move ( 0, h + 16 )
                            , veloLine
                            ]
    in
        move pos ready


scene : Model String -> Element
scene bodies =
    collage 800 800 <| List.map drawBody bodies



-- different force functions to experiment with


constgravity : a -> ( ( number, Float ), ( number1, number2 ) )
constgravity t =
    --( ( 0, -0.2 ), ( 0, 0 ) )
    ( ( 0, 0.0 ), ( 0, 0 ) )



-- constant downward gravity


sinforce : Float -> ( Float, number )
sinforce t =
    ( (sin <| radians (t / 1000)) * 50, 0 )



-- sinusoidal sideways force


counterforces : Float -> ( ( number, Float ), ( number1, Float ) )
counterforces t =
    ( ( 0, -0.01 ), ( 0, t / 1000 ) )



-- small gravity, slowly accellerating upward drift


type Msg
    = Tick Time


subs : Sub Msg
subs =
    AnimationFrame.diffs Tick


update : Msg -> Model meta -> Model meta
update (Tick dt) bodies =
  let
    newBodies = List.map slowDown bodies
  in
    uncurry step (constgravity dt) newBodies


slowDown : Body meta -> Body meta
slowDown body =
  let
    (a, b) = body.velocity
    newVelocity = (a * 0.995, b * 0.995)
  in
    { body | velocity = newVelocity }


{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}
main : Program Never (Model String) Msg
main =
    program
        { init = ( labeledBodies, Cmd.none )
        , update = (\msg bodies -> ( update msg bodies, Cmd.none ))
        , subscriptions = always subs
        , view = scene >> Element.toHtml
        }
