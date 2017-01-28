import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Window
import Task
import Process
import Time exposing (Time)
import Keyboard
import AnimationFrame
import BoxesAndBubbles.Bodies as Bodies
import BoxesAndBubbles as BnB
--import Html.Attributes as HAttr
--import Html.Events as HEv
--import Json.Decode as Json
--import Set exposing (Set)
--import V
--import History exposing (History)
--import Dom
import Debug exposing (log)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL ############################################################################

type alias Model =
  { viewport : Window.Size
  , cannon : Cannon
  , keysPressed : KeysPressed
  , ball : Maybe Ball
  , bounds : List Barrier
  , barriers : List Barrier
  , time : Time
  }

type alias Ball =
  Bodies.Body ()

type alias Barrier =
  Bodies.Body ()

type alias Cannon =
  { pos : (Float, Float)
  , angle : Float
  , power : Float
  }

type alias KeysPressed =
  { shift: Maybe Time
  , alt: Maybe Time
  , meta: Maybe Time
  , space: Maybe Time
  , ctrl: Maybe Time
  , left: Maybe Time
  , right: Maybe Time
  , up: Maybe Time
  , down: Maybe Time
  }

init : (Model, Cmd Msg)
init =
  let
    viewport = Window.Size 0 0
    cannon = Cannon (100, 100) 0 0
    keysPressed = KeysPressed Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    ball = Nothing
    bounds = BnB.bounds (0, 0) 10 0.99 (0, 0) ()
    barriers = []
    time = 0
    model = Model viewport cannon keysPressed ball bounds barriers time
    cmd = Task.perform Resize Window.size
  in
    (model, cmd)




-- UPDATE ###########################################################################

type Msg
  = NoOp
  | Resize Window.Size
  | KeyDown Keyboard.KeyCode
  | KeyUp Keyboard.KeyCode
  | Frame Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp ->
      (model, Cmd.none)

    Resize newViewport ->
      let
        w = toFloat newViewport.width
        h = toFloat newViewport.height
        cx = w / 2
        cy = h / 2
        newBounds = BnB.bounds (w - 100, h - 100) 1 1.0 (cx, cy) ()
        newModel = { model | viewport = newViewport, bounds = newBounds }
      in
        (newModel, Cmd.none)

    KeyDown keyCode ->
      let
        newKeysPressed = keysPressedOn keyCode model.time model.keysPressed
        newModel = { model | keysPressed = newKeysPressed }
      in
        (newModel, Cmd.none)

    KeyUp keyCode ->
      let
        newKeysPressed = keysPressedOff keyCode model.time model.keysPressed
        newModel = { model | keysPressed = newKeysPressed }
      in
        case keyCode of
          32 ->
            let
              cannon = model.cannon
              velX = (cos (degrees cannon.angle)) * (cannon.power / 10)
              velY = (sin (degrees cannon.angle)) * (cannon.power / 10)
              radius = 22
              density = 1.0
              restitution = 1.0
              pos = cannon.pos
              velocity = (velX, velY)
              meta = ()
              newBall = Just (BnB.bubble radius density restitution pos velocity meta)
              newCannon = { cannon | power = 0 }
              modelInMotion = { newModel | ball = newBall, cannon = newCannon }
            in
              (newModel, Cmd.none)
          _ ->
            (newModel, Cmd.none)

    Frame time ->
      let
        newModel = { model | time = model.time + time }
          |> advanceBall
          |> rotateCannon
          |> chargeCannon
      in
        (newModel, Cmd.none)

advanceBall : Model -> Model
advanceBall model =
  case model.ball of
    Just ball ->
      let
        newVelocity = reduceVelocity ball.velocity
        slowerBall = { ball | velocity = newVelocity }
        shapes = slowerBall :: model.bounds
        newShapes = BnB.step (0, 0) (0, 0) shapes
        newBall = findFirst isBubble newShapes
      in
        { model | ball = newBall }
    Nothing ->
      model

rotateCannon : Model -> Model
rotateCannon model =
  let
    keysPressed = model.keysPressed
    isFine = not (keysPressed.shift == Nothing)
    isCoarse = not (keysPressed.alt == Nothing)
    now = model.time
    newCannon = case (keysPressed.left, keysPressed.right) of
      (Just pressTime, Nothing) ->
        rotateCannonBy True isFine isCoarse (now - pressTime) model.cannon
      (Nothing, Just pressTime) ->
        rotateCannonBy False isFine isCoarse (now - pressTime) model.cannon
      _ -> model.cannon
  in
    { model | cannon = newCannon }

rotateCannonBy : Bool -> Bool -> Bool -> Float -> Cannon -> Cannon
rotateCannonBy isLeft isFine isCoarse speed cannon =
  let
    incr = max 1.0 speed
      |> logBase e
      |> (\a -> a * 0.07)
      |> (\a -> if isFine then a / 10 else a)
      |> (\a -> if isCoarse then a * 10 else a)
      |> (\a -> if isLeft then 0 - a else a)
    newAngle = cannon.angle - incr
  in
    { cannon | angle = newAngle }

chargeCannon : Model -> Model
chargeCannon model =
  case model.keysPressed.space of
    Just pressTime ->
      let
        pressDuration = model.time - pressTime
        cannon = model.cannon
        isFine = not (model.keysPressed.shift == Nothing)
        incr = pressDuration
          |> (\d -> (d / 10) + 1)
          |> logBase e
          |> (\p -> if isFine then p / 10 else p)
        newCannon = { cannon | power = cannon.power + incr }
      in
        { model | cannon = newCannon }
    Nothing ->
      model

reduceVelocity : (Float, Float) -> (Float, Float)
reduceVelocity (xVel, yVel) =
  let
    hyp = sqrt (xVel * xVel + yVel * yVel)
    factor = if hyp > 5 then 0.997 else if hyp > 0.3 then 0.994 else if hyp > 0.05 then 0.97 else 0
  in
    (xVel * factor, yVel * factor)


isBubble : Bodies.Body x -> Bool
isBubble body =
  case body.shape of
    Bodies.Box vec -> False
    Bodies.Bubble rad -> True

delay : Time -> msg -> Cmd msg
delay time msg =
  Process.sleep time
    |> Task.andThen (always (Task.succeed msg))
    |> Task.perform identity

findFirst : (a -> Bool) -> List a -> Maybe a
findFirst fn list =
  case list of
    a :: rest ->
      if fn a then
        Just a
      else
        findFirst fn rest
    [] ->
      Nothing

keysPressedOn : Int -> Time -> KeysPressed -> KeysPressed
keysPressedOn keyCode time keysPressed =
  updateKeysPressed keyCode True time keysPressed

keysPressedOff : Int -> Time -> KeysPressed -> KeysPressed
keysPressedOff keyCode time keysPressed =
  updateKeysPressed keyCode False time keysPressed

updateKeysPressed : Int -> Bool -> Time -> KeysPressed -> KeysPressed
updateKeysPressed keyCode isOn time keysPressed =
  case keyCode of
    16 -> { keysPressed | shift = updateKeyPressed isOn keysPressed.shift time }
    17 -> { keysPressed | ctrl =  updateKeyPressed isOn keysPressed.ctrl time  }
    18 -> { keysPressed | alt =   updateKeyPressed isOn keysPressed.alt time   }
    32 -> { keysPressed | space = updateKeyPressed isOn keysPressed.space time }
    37 -> { keysPressed | right = updateKeyPressed isOn keysPressed.right time }
    38 -> { keysPressed | up =    updateKeyPressed isOn keysPressed.up time    }
    39 -> { keysPressed | left =  updateKeyPressed isOn keysPressed.left time  }
    40 -> { keysPressed | down =  updateKeyPressed isOn keysPressed.down time  }
    91 -> { keysPressed | meta =  updateKeyPressed isOn keysPressed.meta time  }
    _ -> keysPressed

updateKeyPressed : Bool -> Maybe Time -> Time -> Maybe Time
updateKeyPressed isOn maybeExistingTime newTime =
  if not isOn then
    Nothing
  else
    case maybeExistingTime of
      Just existingTime -> maybeExistingTime
      Nothing -> Just newTime


-- SUBSCRIPTIONS ####################################################################

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    , Window.resizes Resize
    , AnimationFrame.diffs Frame
    ]



-- VIEW #############################################################################

view : Model -> Html Msg
view model =
  let
    widthStr = toString model.viewport.width
    heightStr = toString model.viewport.height
    widthPx = widthStr ++ "px"
    heightPx = heightStr ++ "px"
    box = drawViewBox 0 0 model.viewport.width model.viewport.height
  in
    Html.div []
      [ Svg.svg
        [ SAttr.viewBox box
        , SAttr.width widthPx
        , SAttr.height heightPx
        ]
        [ drawBarriers model.bounds
        , drawBarriers model.barriers
        , drawCannon model.cannon
        , drawBall model.ball
        ]
      ]

drawViewBox : Int -> Int -> Int -> Int -> String
drawViewBox x y width height =
  (toString x) ++ " " ++ (toString y) ++ " " ++ (toString width) ++ " " ++ (toString height)

drawBall : Maybe Ball -> Svg Msg
drawBall ball =
  case ball of
    Just ball ->
      case ball.shape of
        Bodies.Bubble radius ->
          let
            (cx, cy) = ball.pos
            cxAttr = SAttr.cx (toString cx)
            cyAttr = SAttr.cy (toString cy)
            rAttr = SAttr.r (toString radius)
            classAttr = SAttr.class "ball"
          in
            Svg.circle [cxAttr, cyAttr, rAttr, classAttr] []
        Bodies.Box vec ->
          Svg.g [] []
    Nothing ->
      Svg.g [] []

drawCannon : Cannon -> Svg Msg
drawCannon cannon =
  let
    (x, y) = cannon.pos
    classAttr = SAttr.class "cannon"
    transformAttr = SAttr.transform ("translate(" ++ (toString x) ++ "," ++ (toString y) ++ ") rotate(" ++ (toString cannon.angle) ++ ")")
  in
    Svg.g
      [ transformAttr
      , classAttr
      ]
      [ Svg.rect [SAttr.x "20", SAttr.y "-10", SAttr.width "20", SAttr.height "20", SAttr.class "barrel"] []
      , Svg.circle [SAttr.cx "0", SAttr.cy "0", SAttr.r "20"] []
      , Svg.line [SAttr.x1 "20", SAttr.y1 "0", SAttr.x2 "2100", SAttr.y2 "0"] []
      , drawPowerGauge cannon.power
      ]

drawPowerGauge : Float -> Svg Msg
drawPowerGauge power =
  case power of
    0 ->
      Svg.g [] []
    _ ->
      let
        width = power / 3
        widthAttr = SAttr.width (toString width)
        heightAttr = SAttr.height "12"
        yAttr = SAttr.y "-6"
        xAttr = SAttr.x (toString (36 - width))
      in
        Svg.g
          [ SAttr.class "power-gauge"
          ]
          [ Svg.rect [xAttr, yAttr, widthAttr, heightAttr, SAttr.class "backdrop"] []
          ]

drawBarriers : List Barrier -> Svg Msg
drawBarriers barriers =
  Svg.g [] (List.map drawBarrier barriers)

drawBarrier : Barrier -> Svg Msg
drawBarrier barrier =
  case barrier.shape of
    Bodies.Bubble r ->
      Svg.g [] []
    Bodies.Box (halfWidth, halfHeight) ->
      let
        (cx, cy) = barrier.pos
        x = cx - halfWidth
        y = cy - halfHeight
        width = halfWidth * 2
        height = halfHeight * 2
        xAttr = SAttr.x (toString x)
        yAttr = SAttr.y (toString y)
        widthAttr = SAttr.width (toString width)
        heightAttr = SAttr.height (toString height)
        classAttr = SAttr.class "barrier"
      in
        Svg.rect [xAttr, yAttr, widthAttr, heightAttr, classAttr] []
