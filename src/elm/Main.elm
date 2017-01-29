import Html exposing (Html)
import Html.Attributes as HAttr
import Html.Events as HEv
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
  , playport : Window.Size
  , controlport : Window.Size
  , level : Level
  , keysPressed : KeysPressed
  , ball : Maybe Ball
  , bounds : List Barrier
  , time : Time
  , shotCount : Int
  , totalShotCount : Int
  , remainingLevels : List Level
  , finishedLevels : List Level
  }

type alias Target =
  { pos : (Float, Float, Float, Float)
  }

type PhysObj = BallObj | BarrierObj

type alias Ball =
  Bodies.Body PhysObj

type alias Barrier =
  Bodies.Body PhysObj

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

type alias Level =
  { cannon : Cannon
  , barriers : List Barrier
  , target : Target
  , par : Int
  }

levels : List Level
levels =
  [ Level
    (Cannon (100, 900) 315 0)
    []
    (Target (450, 400, 150, 150))
    1
  , Level
    (Cannon (900, 900) 225 0)
    []
    (Target (100, 100, 150, 150))
    1
  , Level
    (Cannon (500, 900) 225 0)
    []
    (Target (425, 100, 150, 150))
    1
  , Level
    (Cannon (100, 900) 270 0)
    []
    (Target (850, 50, 100, 100))
    1
  , Level
    (Cannon (100, 100) 65 0)
    [ makeBarrier 500 50 1 600
    ]
    (Target (750, 100, 150, 150))
    1
  , Level
    (Cannon (880, 400) 166.56 0)
    [ makeBarrier 50 500 370 1
    , makeBarrier 580 500 370 1
    ]
    (Target (800, 800, 100, 100))
    2
  , Level
    (Cannon (880, 850) 255 0)
    [ makeBarrier 333 50 1 666
    , makeBarrier 666 333 1 618
    ]
    (Target (100, 150, 100, 100))
    2
  , Level
    (Cannon (500, 800) 270 0)
    [ makeBarrier 200 333 600 1
    , makeBarrier 200 666 600 1
    ]
    (Target (450, 140, 100, 100))
    2
  , Level
    (Cannon (900, 900) 270 0)
    [ makeBarrier 840 230 1 720
    , makeBarrier 900 170 50 1
    ]
    (Target (690, 800, 100, 100))
    2
  , Level
    (Cannon (900, 100) 135 0)
    [ makeBarrier 200 200 600 1
    , makeBarrier 800 200 1 600
    , makeBarrier 875 850 1 100
    , makeBarrier 50 125 100 1
    ]
    (Target (600, 300, 100, 100))
    2
  , Level
    (Cannon (100, 500) 0 0)
    [ makeBarrier 200 200 600 1
    , makeBarrier 200 800 600 1
    , makeBarrier 200 200 1 600
    , makeBarrier 800 200 1 230
    , makeBarrier 800 570 1 230
    ]
    (Target (450, 450, 100, 100))
    3
  , Level
    (Cannon (500, 500) 135 0)
    [ makeBarrier 200 200 600 1
    , makeBarrier 50 800 750 1
    , makeBarrier 800 200 1 600
    , makeBarrier 200 200 1 200
    ]
    (Target (75, 825, 100, 100))
    3
  , Level
    (Cannon (500, 100) 90 0)
    [ makeBarrier 50 250 375 1
    , makeBarrier 575 250 375 1
    , makeBarrier 150 500 700 1
    , makeBarrier 50 750 375 1
    , makeBarrier 575 750 375 1
    ]
    (Target (800, 800, 100, 100))
    3
  , Level
    (Cannon (100, 500) 0 0)
    [ makeBallBarrier 500 500 210
    , makeBallBarrier 250 250 100
    , makeBallBarrier 250 750 100
    , makeBallBarrier 750 250 100
    , makeBallBarrier 750 750 100
    ]
    (Target (800, 450, 100, 100))
    3
  , Level
    (Cannon (170, 170) 45 0)
    [ makeBallBarrier 500 500 400
    ]
    (Target (800, 800, 100, 100))
    3
  , Level
    (Cannon (150, 150) 0 0)
    [ makeBarrier 500 500 450 1
    , makeBarrier 50 300 450 1
    , makeBarrier 50 700 450 1
    , makeBallBarrier 250 500 125
    , makeBallBarrier 750 275 125
    , makeBallBarrier 750 725 125
    ]
    (Target (100, 800, 100, 100))
    2
  , Level
    (Cannon (100, 100) 20 0)
    []
    (Target (500, 500, 150, 100))
    1
  , Level
    (Cannon (100, 100) 20 0)
    []
    (Target (500, 500, 150, 100))
    1
  , Level
    (Cannon (100, 100) 20 0)
    []
    (Target (500, 500, 150, 100))
    1
  ]

makeBarrier : Float -> Float -> Float -> Float -> Barrier
makeBarrier x y w h =
  let
    cx = x + (w / 2)
    cy = y + (h / 2)
    inf = 1.0 / 0.0
    density = inf
    restitution = 1.0
    velocity = (0, 0)
    meta = BarrierObj
  in
    BnB.box (w, h) density restitution (cx, cy) velocity meta

makeBallBarrier : Float -> Float -> Float -> Barrier
makeBallBarrier cx cy r =
  let
    inf = 1.0 / 0.0
    density = inf
    restitution = 1.0
    velocity = (0, 0)
    meta = BarrierObj
  in
    BnB.bubble r density restitution (cx, cy) velocity meta

init : (Model, Cmd Msg)
init =
  let
    viewport = Window.Size 0 0
    playport = Window.Size 0 0
    controlport = Window.Size 0 0

    --level = Level
    --  (Cannon (100, 100) 45 0)
    --  []
    --  (Target (200, 200, 150, 150))
    --  1

    level = Level
      (Cannon (150, 150) 0 0)
      [ makeBarrier 500 500 450 1
      , makeBarrier 50 300 450 1
      , makeBarrier 50 700 450 1
      , makeBallBarrier 250 500 125
      , makeBallBarrier 750 275 125
      , makeBallBarrier 750 725 125
      ]
      (Target (100, 800, 100, 100))
      2

    keysPressed = KeysPressed Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    ball = Nothing
    --bounds = BnB.bounds (900, 900) 1000 1.0 ( 500, 500 ) BarrierObj
    bounds =
      [ makeBarrier -500 -450 2000 500
      , makeBarrier -450 -500 500 2000
      , makeBarrier -500 950 2000 500
      , makeBarrier 950 -500 500 2000
      ]
    time = 0
    shotCount = 0
    totalShotCount = 0
    remainingLevels = levels
    finishedLevels = []
    model = Model viewport playport controlport level keysPressed ball bounds time shotCount totalShotCount remainingLevels finishedLevels
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
  | Skip

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Skip ->
      case model.remainingLevels of
        newLevel :: newRemainingLevels ->
          let
            newFinishedLevels = model.level :: model.finishedLevels
            newModel = { model | level = newLevel, remainingLevels = newRemainingLevels, finishedLevels = newFinishedLevels }
          in
            (newModel, Cmd.none)
        [] ->
          (model, Cmd.none)

    NoOp ->
      (model, Cmd.none)

    Resize newViewport ->
      let
        size = min newViewport.width newViewport.height
        newPlayport = { newViewport | width = size, height = size }
        newControlport = { newViewport | width = 400 }
        newModel = { model | viewport = newViewport, playport = newPlayport, controlport = newControlport }
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
              level = model.level
              cannon = level.cannon
              velX = (cos (degrees cannon.angle)) * (cannon.power / 10)
              velY = (sin (degrees cannon.angle)) * (cannon.power / 10)
              radius = 22
              density = 1.0
              restitution = 1.0
              pos = cannon.pos
              velocity = (velX, velY)
              meta = BallObj
              newBall = Just (BnB.bubble radius density restitution pos velocity meta)
              newCannon = { cannon | power = 0 }
              newLevel = { level | cannon = newCannon }
              modelInMotion = { newModel | ball = newBall, level = newLevel }
            in
              (modelInMotion, Cmd.none)
          _ ->
            (newModel, Cmd.none)

    Frame time ->
      let
        hasStopped = case model.ball of
          Just ball -> ball.velocity == (0, 0)
          Nothing -> False
        newModel = { model | time = model.time + time }
          |> advanceBall hasStopped
          |> rotateCannon
          |> chargeCannon
          |> evaluatePlay2 hasStopped
      in
        (newModel, Cmd.none)

evaluatePlay : Bool -> Model -> Model
evaluatePlay hasStopped model =
  case (hasStopped, model.ball) of
    (True, Just ball) ->
      let
        level = model.level
        isInTarget = ballIsInTarget ball level.target
        cannon = level.cannon
        newBall = Nothing
        newShotCount = model.shotCount + 1
        newTotalShotCount = model.totalShotCount + 1
      in
        if isInTarget then
          { model | shotCount = newShotCount, totalShotCount = newTotalShotCount, ball = newBall }
        else
          let
            newCannon = { cannon | pos = ball.pos }
            newLevel = { level | cannon = newCannon }
          in
            { model | level = newLevel, shotCount = newShotCount, totalShotCount = newTotalShotCount, ball = newBall }
    _ ->
      model

evaluatePlay2 : Bool -> Model -> Model
evaluatePlay2 hasStopped model =
  case (hasStopped, model.ball) of
    (True, Just ball) ->
      if ballIsInTarget ball model.level.target then
        case model.remainingLevels of
          newLevel :: newRemainingLevels ->
            -- remove ball from play
            -- pop level from remaining levels
            -- push level onto finished levels
            -- replace cannon
            -- replace target
            -- replace barriers
            -- reset shot count
            -- increment total shot count
            let
              newFinishedLevels = (model.level) :: model.finishedLevels
              newBall = Nothing
              newShotCount = 0
              newTotalShotCount = model.totalShotCount + 1
            in
              { model
              | ball = newBall
              , remainingLevels = newRemainingLevels
              , finishedLevels = newFinishedLevels
              , level = newLevel
              , shotCount = newShotCount
              , totalShotCount = newTotalShotCount
              }
          [] ->
            model
      else
        -- remove ball from play
        -- increment shot count
        -- increment total shot count
        -- move cannon
        let
          newBall = Nothing
          level = model.level
          cannon = level.cannon
          (x1, y1) = ball.pos
          (tx, ty, tw, th) = level.target.pos
          x2 = tx + (tw / 2)
          y2 = ty + (th / 2)
          angle = (atan2 (y2 - y1) (x2 - x1)) * (360 / (pi * 2))
          newCannon = { cannon | pos = ball.pos, angle = angle }
          newLevel = { level | cannon = newCannon }
          newShotCount = model.shotCount + 1
          newTotalShotCount = model.totalShotCount + 1
        in
          { model
          | ball = newBall
          , level = newLevel
          , shotCount = newShotCount
          , totalShotCount = newTotalShotCount
          }
    _ ->
      model

ballIsInTarget : Ball -> Target -> Bool
ballIsInTarget ball target =
  case ball.shape of
    Bodies.Bubble radius ->
      let
        (tx, ty, tw, th) = target.pos
        (bx, by) = ball.pos
        boundLeft = tx + radius
        boundRight = (tx + tw) - radius
        boundHi = ty + radius
        boundLo = (ty + th) - radius
      in
        not (bx < boundLeft || bx > boundRight || by < boundHi || by > boundLo)
    Bodies.Box ext ->
      False

advanceBall : Bool -> Model -> Model
advanceBall hasStopped model =
  if hasStopped then
    model
  else
    case model.ball of
      Just ball ->
        let
          newVelocity = reduceVelocity ball.velocity
          slowerBall = { ball | velocity = newVelocity }
          shapes = (slowerBall :: model.bounds) ++ model.level.barriers
          newShapes = BnB.step (0, 0) (0, 0) shapes
          newBall = findFirst isBall newShapes
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
    level = model.level
    newCannon = case (keysPressed.left, keysPressed.right) of
      (Just pressTime, Nothing) ->
        rotateCannonBy True isFine isCoarse (now - pressTime) level.cannon
      (Nothing, Just pressTime) ->
        rotateCannonBy False isFine isCoarse (now - pressTime) level.cannon
      _ -> level.cannon
    newLevel = { level | cannon = newCannon }
  in
    { model | level = newLevel }

rotateCannonBy : Bool -> Bool -> Bool -> Float -> Cannon -> Cannon
rotateCannonBy isLeft isFine isCoarse speed cannon =
  let
    incr = max 1.0 speed
      |> logBase e
      |> (\a -> a * 0.04)
      |> (\a -> if isFine then a / 6 else a)
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
        level = model.level
        cannon = level.cannon
        isFine = not (model.keysPressed.shift == Nothing)
        incr = pressDuration
          |> (\d -> (d / 100) + 1)
          |> logBase e
          |> (\p -> if isFine then p / 10 else p)
        newPower = min 500 cannon.power + incr
        newCannon = { cannon | power = newPower }
        newLevel = { level | cannon = newCannon }
      in
        { model | level = newLevel }
    Nothing ->
      model

reduceVelocity : (Float, Float) -> (Float, Float)
reduceVelocity (xVel, yVel) =
  let
    speed = sqrt (xVel * xVel + yVel * yVel)
    newSpeed = max 0.0 ((speed * 0.99) - 0.0075)
    slowDown = newSpeed / speed
    newXVel = xVel * slowDown
    newYVel = yVel * slowDown
    --factor = if speed > 5 then
    --  0.997
    --else if speed > 0.3 then
    --  0.990
    --else if speed > 0.12 then
    --  0.98
    --else
    --  0
  in
    (newXVel, newYVel)

isBall : Bodies.Body PhysObj -> Bool
isBall body =
  case body.meta of
    BallObj -> True
    BarrierObj -> False

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
    widthStr = toString model.playport.width
    heightStr = toString model.playport.height
    widthPx = widthStr ++ "px"
    heightPx = heightStr ++ "px"
    attempts = toString model.shotCount
    totalAttempts = toString model.totalShotCount
    finCount = List.length model.finishedLevels
    remCount = List.length model.remainingLevels
    level = toString (finCount + 1)
    totalLevels = toString (finCount + remCount + 1)
  in
    Html.div []
      [ Svg.svg
        [ SAttr.viewBox "0 0 1000 1000"
        , SAttr.width widthPx
        , SAttr.height heightPx
        ]
        [ defs
        , Svg.rect [attrX 50, attrY 50, attrWidth 900, attrHeight 900, SAttr.class "boundary"] []
        , drawBarriers model.level.barriers
        , drawTarget model.level.target
        , drawCannon model.level.cannon
        , drawBall model.ball
        , Svg.text_ [attrX 954, attrY 980, SAttr.class "par-label"] [Svg.text ("par = " ++ (toString model.level.par))]
        ]
      , Html.div
        [ HAttr.id "controls"
        , HAttr.style [("width", (toString model.controlport.width) ++ "px"), ("left", (toString model.playport.width) ++ "px")]
        ]
        [ Html.h1 [] [Html.text "Electron Golf 2000"]
        , Html.div [HAttr.class "tiles"]
          [ Html.div [HAttr.class "tile"]
            [ Html.div [HAttr.class "tile-label"] [Html.text "Shots"]
            , Html.div [HAttr.class "tile-value"] [Html.text attempts]
            ]
          , Html.div [HAttr.class "tile"]
            [ Html.div [HAttr.class "tile-label"] [Html.text "Total Shots"]
            , Html.div [HAttr.class "tile-value"] [Html.text totalAttempts]
            ]
          , Html.div [HAttr.class "tile"]
            [ Html.div [HAttr.class "tile-label"] [Html.text "Level"]
            , Html.div [HAttr.class "tile-value"]
              [ Html.text (level)
              , Html.span [HAttr.class "slash"] [Html.text "/"]
              , Html.text (totalLevels)
              ]
            ]
          ]
        , Html.div [HAttr.class "directions"]
          [ Html.p []
            [ Html.strong [] [Html.text "The goal. "]
            , Html.text "Fire the cannon, positioning the ball into the target frame. Do this in as few moves as possible, like in actual golf."
            ]
          , Html.p []
            [ Html.strong [] [Html.text "How to aim. "]
            , Html.text "Use the left/right arrow keys. Hold down SHIFT to fine-tune your angle. Hold down ALT to rotate quickly."
            ]
          , Html.p []
            [ Html.strong [] [Html.text "How to fire. "]
            , Html.text "Press SPACEBAR to charge the cannon. Release to fire. Hold SHIFT while charging to fine-tune your velocity."
            ]
          ]
        , Html.button [HEv.onClick Skip] [Html.text "skip"]
        ]
      ]

defs : Svg Msg
defs =
  Svg.defs []
    [ Svg.clipPath
      [ SAttr.id "gutter-clip"
      ]
      [ Svg.rect [attrX 50, attrY 50, attrWidth 900, attrHeight 900] []
      ]
    ]

drawViewBox : Int -> Int -> Int -> Int -> String
drawViewBox x y width height =
  (toString x) ++ " " ++ (toString y) ++ " " ++ (toString width) ++ " " ++ (toString height)

drawTarget : Target -> Svg Msg
drawTarget target =
  let
    (x, y, width, height) = target.pos
    translate = "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
  in
    Svg.g
      [ SAttr.transform translate
      , SAttr.class "target"
      ]
      [ drawTargetBound 0     0      width 0
      , drawTargetBound width 0      width height
      , drawTargetBound width height 0     height
      , drawTargetBound 0     height 0     0
      , drawTargetCorner (0) (0) (20) (20)
      , drawTargetCorner (width) (0) (-20) (20)
      , drawTargetCorner (width) (height) (-20) (-20)
      , drawTargetCorner (0) (height) (20) (-20)
      ]

drawTargetBound : Float -> Float -> Float -> Float -> Svg Msg
drawTargetBound x1 y1 x2 y2 =
  Svg.line
    [ attrX1 x1
    , attrY1 y1
    , attrX2 x2
    , attrY2 y2
    , SAttr.class "target-bound"
    ] []

drawTargetCorner : Float -> Float -> Float -> Float -> Svg Msg
drawTargetCorner origX origY extX extY =
  let
    d = drawPath
      [ M origX (origY + extY)
      , V -extY
      , H extX
      , H -extX
      ]
  in
    Svg.path [SAttr.d d, SAttr.class "target-corner"] []

type PathCommand
  = M Float Float
  | L Float Float
  | H Float
  | V Float
  | Z

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
    powerRadius = (cannon.power * 2.55) ^ 1.2
    powerOpacity = min 1.0 (1.0 - (powerRadius / 700))
    opacityStyle = "opacity:" ++ (toString powerOpacity)
  in
    Svg.g
      [ transformAttr
      , classAttr
      ]
      [ Svg.rect [SAttr.x "20", SAttr.y "-10", SAttr.width "20", SAttr.height "20", SAttr.class "barrel"] []
      , Svg.circle [SAttr.cx "0", SAttr.cy "0", SAttr.r "20"] []
      , Svg.line [SAttr.x1 "20", SAttr.y1 "0", SAttr.x2 "2100", SAttr.y2 "0"] []
      , Svg.circle [attrCX 0, attrCY 0, attrR powerRadius, attrClass "power-radius", SAttr.style opacityStyle] []
      , drawPowerGauge cannon.power
      ]

drawPowerGauge : Float -> Svg Msg
drawPowerGauge power =
  case power of
    0 ->
      Svg.g [] []
    _ ->
      Svg.g
        [ SAttr.class "power-gauge"
        ]
        [ Svg.circle [attrCX -power, attrCY 0, SAttr.r "20"] []
        , Svg.circle [attrCX 0, attrCY 0, SAttr.r "10", SAttr.class "gauge-dot"] []
        , Svg.circle [attrCX -power, attrCY 0, SAttr.r "10", SAttr.class "gauge-dot"] []
        ]

drawBarriers : List Barrier -> Svg Msg
drawBarriers barriers =
  Svg.g [] (List.map drawBarrier barriers)

drawBarrier : Barrier -> Svg Msg
drawBarrier barrier =
  case barrier.shape of
    Bodies.Bubble r ->
      let
        (cx, cy) = barrier.pos
        classAttr = SAttr.class "barrier"
      in
        Svg.circle [attrCX cx, attrCY cy, SAttr.r (toString r), classAttr] []
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

attrX : Float -> Svg.Attribute Msg
attrX x = SAttr.x (toString x)

attrY : Float -> Svg.Attribute Msg
attrY y = SAttr.y (toString y)

attrWidth : Float -> Svg.Attribute Msg
attrWidth width = SAttr.width (toString width)

attrHeight : Float -> Svg.Attribute Msg
attrHeight height = SAttr.height (toString height)

attrCX : Float -> Svg.Attribute Msg
attrCX cx = SAttr.cx (toString cx)

attrCY : Float -> Svg.Attribute Msg
attrCY cy = SAttr.cy (toString cy)

attrX1 : Float -> Svg.Attribute Msg
attrX1 x1 = SAttr.x1 (toString x1)

attrY1 : Float -> Svg.Attribute Msg
attrY1 y1 = SAttr.y1 (toString y1)

attrX2 : Float -> Svg.Attribute Msg
attrX2 x2 = SAttr.x2 (toString x2)

attrY2 : Float -> Svg.Attribute Msg
attrY2 y2 = SAttr.y2 (toString y2)

attrR : Float -> Svg.Attribute Msg
attrR r = SAttr.r (toString r)

attrClass : String -> Svg.Attribute Msg
attrClass cls = SAttr.class cls
