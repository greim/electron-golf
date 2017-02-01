
-- import ----------------------------------------------------------------------

--import Debug exposing (log)
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
import Transition exposing (Transition)
import Phys
import KeysPressed exposing (KeysPressed)
import Target exposing (Target)
import Level exposing (Level, allLevels)
import Cannon exposing (Cannon)
import Phase exposing (..)
import View exposing (..)
import Ease

-- main ------------------------------------------------------------------------

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- model -----------------------------------------------------------------------

type alias Model =
  { viewport : Window.Size
  , playport : Window.Size
  , controlport : Window.Size
  , phase : Phase
  , keysPressed : KeysPressed
  , ball : Maybe Phys.Obj
  , time : Time
  , remainingLevels : List Level
  , finishedLevels : List Level
  }

type PathCommand
  = M Float Float
  | L Float Float
  | H Float
  | V Float
  | Z

init : (Model, Cmd Msg)
init =
  let
    viewport = Window.Size 0 0
    playport = Window.Size 0 0
    controlport = Window.Size 0 0
    phase = Starting
    keysPressed = KeysPressed.init
    ball = Nothing
    time = 0
    remainingLevels = allLevels
    finishedLevels = []
    model = Model viewport playport controlport phase keysPressed ball time remainingLevels finishedLevels
    cmd = Task.perform Resize Window.size
  in
    (model, cmd)


-- update ----------------------------------------------------------------------

type Msg
  = NoOp
  | Resize Window.Size
  | KeyDown Keyboard.KeyCode
  | KeyUp Keyboard.KeyCode
  | Frame Time
  | Start

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp ->
      (model, Cmd.none)

    Start ->
      case allLevels of
        level :: remainingLevels ->
          let
            newModel = { model
              | remainingLevels = remainingLevels
              , finishedLevels = []
              , phase = Playing level
              }
          in
            (newModel, Cmd.none)
        [] ->
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
        case (model.ball, model.phase, keyCode) of
          (Nothing, Playing level, 32) ->
            let
              cannon = level.cannon
              adjustedPower = cannon.power / 10
              newBall = Just (Phys.ball cannon.pos cannon.angle adjustedPower)
              newCannon = { cannon | power = 0 }
              newLevel = { level | cannon = newCannon }
              modelInMotion = { newModel | ball = newBall, phase = Playing newLevel }
            in
              (modelInMotion, Cmd.none)
          _ ->
            (newModel, Cmd.none)

    Frame time ->
      let
        tickModel = { model | time = model.time + time }
      in
        case model.phase of
          Starting ->
            (tickModel, Cmd.none)
          Playing level ->
            let
              newModel = tickModel
                |> rotateCannon level
                |> chargeCannon level
                |> advanceBall level
            in
              (newModel, Cmd.none)
          Transitioning trans ->
            let
              newModel = advanceTransition trans tickModel
            in
              (newModel, Cmd.none)
          Ending ->
            (tickModel, Cmd.none)

advanceTransition : Transition -> Model -> Model
advanceTransition transition model =
  case Transition.step transition of
    Just tr ->
      { model | phase = Transitioning tr }
    Nothing ->
      case model.remainingLevels of
        level :: newRemainingLevels ->
          { model | phase = Playing level, remainingLevels = newRemainingLevels }
        [] ->
          { model | phase = Ending }

evaluatePlay : Phys.Obj -> Level -> Model -> Model
evaluatePlay ball level model =
  if ballIsInTarget ball level.target then
    let
      finishedLevel = { level | score = level.score + 1 }
      newFinishedLevels = finishedLevel :: model.finishedLevels
      actionPoint = ball.pos
      parDiff = finishedLevel.score - finishedLevel.par
      message = if parDiff == -2 then "O.o"
      else if parDiff == -1 then "WOW!"
      else if parDiff == 0 then "Nice."
      else "Okay!"
      trans = Transition.successful message ball.pos
    in
      { model
      | ball = Nothing
      , finishedLevels = newFinishedLevels
      , phase = Transitioning trans
      }
  else
    let
      -- move and re-orient cannon
      cannon = level.cannon
      (x1, y1) = ball.pos
      (tx, ty, tw, th) = level.target.pos
      x2 = tx + (tw / 2)
      y2 = ty + (th / 2)
      angle = (atan2 (y2 - y1) (x2 - x1)) * (360 / (pi * 2))
      newCannon = { cannon | pos = ball.pos, angle = angle }
      -- increment score
      newScore = level.score + 1
      newLevel = { level | cannon = newCannon, score = newScore }
    in
      { model | ball = Nothing, phase = Playing newLevel }

ballIsInTarget : Phys.Obj -> Target -> Bool
ballIsInTarget ball target =
  case ball.shape of
    Bodies.Bubble radius ->
      let
        (tx, ty, tw, th) = target.pos
        (bx, by) = ball.pos
        boundLeft = tx
        boundRight = (tx + tw)
        boundHi = ty
        boundLo = (ty + th)
      in
        not (bx < boundLeft || bx > boundRight || by < boundHi || by > boundLo)
    Bodies.Box ext ->
      False

advanceBall : Level -> Model -> Model
advanceBall level model =
  case model.ball of
    Just ball ->
      let
        (isAtRest, isOOB, newBall, newBarriers) = Phys.step ball level.barriers
        newLevel = { level | barriers = newBarriers }
        maybeNewBall = if isAtRest then Nothing else Just newBall
      in
        if isAtRest then
          evaluatePlay ball newLevel model
        else if isOOB then
          let
            endedLevel = { level | score = level.score + 3 }
            newFinishedLevels = endedLevel :: model.finishedLevels
            trans = Transition.unsuccessful "Out of bounds. +3" ball.pos
            newPhase = Transitioning trans
          in
            { model | ball = Nothing, phase = newPhase, finishedLevels = newFinishedLevels }
        else
          { model | ball = Just newBall, phase = Playing newLevel }
    Nothing ->
      model

rotateCannon : Level -> Model -> Model
rotateCannon level model =
  let
    keysPressed = model.keysPressed
    isFine = not (keysPressed.shift == Nothing)
    isCoarse = not (keysPressed.alt == Nothing)
    now = model.time
    newCannon = case (keysPressed.left, keysPressed.right) of
      (Just pressTime, Nothing) ->
        rotateCannonBy True isFine isCoarse (now - pressTime) level.cannon
      (Nothing, Just pressTime) ->
        rotateCannonBy False isFine isCoarse (now - pressTime) level.cannon
      _ -> level.cannon
    newLevel = { level | cannon = newCannon }
  in
    { model | phase = Playing newLevel }

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

chargeCannon : Level -> Model -> Model
chargeCannon level model =
  case model.ball of
    Just ball ->
      model
    Nothing ->
      case model.keysPressed.space of
        Just pressTime ->
          let
            pressDuration = model.time - pressTime
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
            { model | phase = Playing newLevel }
        Nothing ->
          model

delay : Time -> msg -> Cmd msg
delay time msg =
  Process.sleep time
    |> Task.andThen (always (Task.succeed msg))
    |> Task.perform identity

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


-- subscriptions ---------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    , Window.resizes Resize
    , AnimationFrame.diffs Frame
    ]


-- view ------------------------------------------------------------------------

view : Model -> Html Msg
view model =
  let
    attempts = case model.phase of
      Playing level -> toString level.score
      _ ->
        case model.finishedLevels of
          level :: others -> toString level.score
          [] -> "-"
    finCount = List.length model.finishedLevels
    remCount = List.length model.remainingLevels
    levelCount = case model.phase of
      Playing level -> finCount + remCount + 1
      _ -> finCount + remCount
    currentLevel = case model.phase of
      Playing level -> finCount + 1
      _ -> finCount
    futurePar = List.foldl (\lvl tally -> tally + lvl.par) 0 model.remainingLevels
    parSoFar = List.foldl (\lvl tally -> tally + lvl.par) 0 model.finishedLevels
    prevTotalScore = List.foldl (\lvl tally -> tally + lvl.score) 0 model.finishedLevels
    totalScore = case model.phase of
      Playing level -> prevTotalScore + level.score
      _ -> prevTotalScore
    totalAttempts = toString totalScore
    parCompare = prevTotalScore - parSoFar
    totalPar = case model.phase of
      Playing level -> futurePar + parSoFar + level.par
      _ -> futurePar + parSoFar
    parCompareStr = if parCompare < 0 then toString parCompare else "+" ++ (toString parCompare)
    levelPar = case model.phase of
      Playing level -> toString level.par
      _ ->
        case model.finishedLevels of
          level :: others -> toString level.par
          [] -> "-"
  in
    Html.div []
      [ playingFieldWrapper model.playport.width model.playport.height
        [ boundary
        , drawBarriers model
        , drawTarget model
        , drawCannon model
        , drawBall model.ball
        , drawTransition model
        ]
      , drawSplash model
      , Html.div
        [ HAttr.id "controls"
        , HAttr.style [("width", (toString model.controlport.width) ++ "px"), ("left", (toString model.playport.width) ++ "px")]
        ]
        [ Html.div [HAttr.class "tiles one-tile"]
          [ Html.div [HAttr.class "tile"]
            [ Html.div [HAttr.class "tile-label"] [Html.text "Score"]
            , Html.div [HAttr.class "tile-value"]
              [ Html.text totalAttempts
              , Html.span [HAttr.class "slash"] [Html.text "/"]
              , Html.text parCompareStr
              ]
            ]
          ]
        , Html.div [HAttr.class "tiles three-tile"]
          [ Html.div [HAttr.class "tile"]
            [ Html.div [HAttr.class "tile-label"] [Html.text "This Hole"]
            , Html.div [HAttr.class "tile-value"]
              [ Html.text attempts
              , Html.span [HAttr.class "slash"] [Html.text "/"]
              , Html.text levelPar
              ]
            ]
          , Html.div [HAttr.class "tile"]
            [ Html.div [HAttr.class "tile-label"] [Html.text "Level"]
            , Html.div [HAttr.class "tile-value"]
              [ Html.text (toString currentLevel)
              , Html.span [HAttr.class "slash"] [Html.text "/"]
              , Html.text (toString levelCount)
              ]
            ]
          , Html.div [HAttr.class "tile"]
            [ Html.div [HAttr.class "tile-label"] [Html.text "Course Par"]
            , Html.div [HAttr.class "tile-value"] [Html.text (toString totalPar)]
            ]
          ]
        ]
      ]

boundary : Svg Msg
boundary =
  Svg.rect
   [ attrX Phys.gutter
   , attrY Phys.gutter
   , attrWidth Phys.playableWidth
   , attrHeight Phys.playableHeight
   , SAttr.class "boundary"
   ] []

drawSplash : Model -> Html Msg
drawSplash model =
  case model.phase of
    Starting ->
      Html.div
        [ HAttr.id "splash"
        , HAttr.style
          [ ("width", (toString model.playport.width) ++ "px")
          , ("height", (toString model.playport.height) ++ "px")
          , ("font-size", (toString (model.playport.width // 100)) ++ "px")
          ]
        ]
        [ Html.div [HAttr.id "splash-inner"]
          [ Html.h1 [] [Html.text "Electron Golf"]
          , Html.p []
            [ Html.strong [] [Html.text "Goal: "]
            , Html.text "Fire the electron cannon at the proton held in the target frame. Under highly stable conditions, the electron will be absorbed! Achieve this in as few shots as possible; lower scores are better."
            ]
          , Html.p []
            [ Html.strong [] [Html.text "Aim: "]
            , Html.text "LEFT/RIGHT (SHIFT to fine-tune, ALT to spin fast)"
            ]
          , Html.p []
            [ Html.strong [] [Html.text "Fire: "]
            , Html.text "SPACEBAR to charge cannon. Release to fire."
            ]
          , Html.p []
            [ Html.strong [] [Html.text "Putt: "]
            , Html.text "SHIFT while pressing SPACEBAR."
            ]
            , Html.button [HEv.onClick Start] [Html.text "Start"]
          ]
        ]
    _ ->
      Html.div [] []

drawTransition : Model -> Svg Msg
drawTransition model =
  case model.phase of
    Transitioning trans ->
      let
        oldLevel = List.head model.finishedLevels
        newLevel = List.head model.remainingLevels
      in
        case oldLevel of
          Just oldLevel -> drawTheTransition trans oldLevel newLevel
          Nothing -> Svg.g [] []
    _ ->
      Svg.g [] []

drawTheTransition : Transition -> Level -> Maybe Level -> Svg Msg
drawTheTransition transition oldLevel maybeNewLevel =
  case transition.phase of
    Transition.Migrating step ->
      let end = Target.center oldLevel.target
      in drawMigration oldLevel transition.actionPoint end step
    Transition.Absorbing step ->
      let point = Target.center oldLevel.target
      in drawAbsorption oldLevel point step
    Transition.Exploding step ->
      let point = Target.center oldLevel.target
      in drawExplosion oldLevel point step
    Transition.Messaging step ->
      drawMessage oldLevel transition.message step
    Transition.Moving step ->
      let
        oldCannon = oldLevel.cannon
        oldTarget = oldLevel.target
        (newCannon, newTarget) = case maybeNewLevel of
          Just newLevel -> (newLevel.cannon, newLevel.target)
          Nothing -> (Cannon.thrownCannon, Target.thrownTarget)
      in
        drawMoving oldCannon oldTarget newCannon newTarget step

drawMigration : Level -> (Float, Float) -> (Float, Float) -> Int -> Svg Msg
drawMigration level (x1, y1) (x2, y2) step =
  let
    progLin = Transition.migrateProgress step
    prog = Ease.inQuad progLin
    xTween = x1 + ((x2 - x1) * prog)
    yTween = y1 + ((y2 - y1) * prog)
  in
    Svg.g []
      [ drawTheCannon level.cannon
      , drawTheTarget level.target
      , drawTheBall xTween yTween 22
      ]

drawAbsorption : Level -> (Float, Float) -> Int -> Svg Msg
drawAbsorption level (cx, cy) step =
  let
    progLin = Transition.explodeProgress step
    prog = Ease.outQuint progLin
    r = 22 - (prog * 22)
  in
    Svg.g []
      [ drawTheCannon level.cannon
      , drawTheTarget level.target
      , drawTheBall cx cy r
      ]

drawExplosion : Level -> (Float, Float) -> Int -> Svg Msg
drawExplosion level (cx, cy) step =
  let
    prog = Transition.explodeProgress step
    r = prog * 60
    opac = 1.0 - prog
    style = "opacity:" ++ (toString opac)
  in
    Svg.g []
      [ drawTheCannon level.cannon
      , drawTheTarget level.target
      , Svg.circle [attrCX cx, attrCY cy, attrR r, SAttr.style style, attrClass "transition-explosion"] []
      ]

drawMessage : Level -> String -> Int -> Svg Msg
drawMessage level message step =
  Svg.g []
    [ drawTheCannon level.cannon
    , drawTheTarget level.target
    , Svg.text_
      [ attrX 500
      , attrY 500
      , attrClass "transition-message"
      ]
      [ Svg.text message
      ]
    ]

drawMoving : Cannon -> Target -> Cannon -> Target -> Int -> Svg Msg
drawMoving oldCannon oldTarget newCannon newTarget step =
  let
    progLin = Transition.moveProgress step
    prog = Ease.inOutQuint progLin
    tweenCannon = Cannon.tweenCannon prog oldCannon newCannon
    tweenTarget = Target.tweenTarget prog oldTarget newTarget
  in
    Svg.g [] [ drawTheTarget tweenTarget, drawTheCannon tweenCannon ]

drawTarget : Model -> Svg Msg
drawTarget model =
  case model.phase of
    Playing level -> drawTheTarget level.target
    _ -> Svg.g [] []

drawTheTarget : Target -> Svg Msg
drawTheTarget target =
  let
    (x, y, width, height) = target.pos
    translate = "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
    cx = width / 2
    cy = height / 2
    r = ((min width height) / 2) - 15
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
      , Svg.circle [attrCX cx, attrCY cy, attrR r] []
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

drawBall : Maybe Phys.Obj -> Svg Msg
drawBall ball =
  case ball of
    Just ball ->
      case ball.shape of
        Bodies.Bubble radius ->
          let (cx, cy) = ball.pos
          in drawTheBall cx cy radius
        Bodies.Box vec ->
          Svg.g [] []
    Nothing ->
      Svg.g [] []

drawTheBall : Float -> Float -> Float -> Svg msg
drawTheBall cx cy r =
  Svg.circle [attrCX cx, attrCY cy, attrR r, attrClass "ball"] []

drawCannon : Model -> Svg Msg
drawCannon model =
  case model.phase of
    Playing level -> drawTheCannon level.cannon
    _ -> Svg.g [] []

drawTheCannon : Cannon -> Svg Msg
drawTheCannon cannon =
  let
    (x, y) = cannon.pos
    classAttr = SAttr.class "cannon"
    transformAttr = SAttr.transform ("translate(" ++ (toString x) ++ "," ++ (toString y) ++ ") rotate(" ++ (toString cannon.angle) ++ ")")
    powerRadius = (cannon.power * 2.7) ^ 1.2
    powerOpacity = min 1.0 (1.0 - (powerRadius / 900))
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

drawBarriers : Model -> Svg Msg
drawBarriers model =
  case model.phase of
    Playing level -> Svg.g [] (List.map drawBarrier level.barriers)
    _ -> Svg.g [] []

drawBarrier : Phys.Obj -> Svg Msg
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
