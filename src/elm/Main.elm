
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
import Layout exposing (Layout)

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
  { playport : Layout
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
    playport = Layout.from (Window.Size 100 100)
    phase = Starting Phys.splashBouncers
    keysPressed = KeysPressed.init
    ball = Nothing
    time = 0
    remainingLevels = allLevels
    finishedLevels = []
    model = Model playport phase keysPressed ball time remainingLevels finishedLevels
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
        newPlayport = Layout.from newViewport
        newModel = { model | playport = newPlayport }
      in
        (newModel, Cmd.none)

    KeyDown keyCode ->
      let
        newKeysPressed = keysPressedOn keyCode model.time model.keysPressed
        newModel = { model | keysPressed = newKeysPressed }
        isSpaceStart = keyCode == 32 && case model.phase of
          Starting splashBouncers -> True
          Ending splashBouncers -> True
          _ -> False
      in
        if isSpaceStart then
          (newModel, delay 200 Start)
        else
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
          Starting splashBouncers ->
            let
              newSplashBouncers = Phys.stepSplashBouncers splashBouncers
              newPhase = Starting newSplashBouncers
              newModel = { tickModel | phase = newPhase }
            in
              (newModel, Cmd.none)
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
          Ending splashBouncers ->
            let
              newSplashBouncers = Phys.stepSplashBouncers splashBouncers
              newPhase = Ending newSplashBouncers
              newModel = { tickModel | phase = newPhase }
            in
              (newModel, Cmd.none)

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
          { model | phase = Ending Phys.splashBouncers }

evaluatePlay : Phys.Obj -> Level -> Model -> Model
evaluatePlay ball level model =
  if ballIsInTarget ball level.target then
    let
      finishedLevel = { level | score = level.score + 1 }
      newFinishedLevels = finishedLevel :: model.finishedLevels
      actionPoint = ball.pos
      parDiff = finishedLevel.score - finishedLevel.par
      message = if parDiff < 0 then toString parDiff
        --else if parDiff == 0 then "par!"
        else "+" ++ (toString parDiff)
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
      fudge = toFloat (((round model.time) % 120) - 60)
      angle = (atan2 (y2 - y1) (x2 - x1)) * (360 / (pi * 2))
      newCannon = { cannon | pos = ball.pos, angle = angle + fudge }
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
            trans = Transition.unsuccessful "OOB! +3" ball.pos
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
    attempts = case model.phase of --------------------------------
      Playing level -> level.score
      _ ->
        case model.finishedLevels of
          level :: others -> level.score
          [] -> -1
    finCount = List.length model.finishedLevels
    levelCount = List.length Level.allLevels ----------------------------
    currentLevel = case model.phase of ---------------------------------
      Playing level -> finCount + 1
      _ -> finCount
    parSoFar = Level.tallyPar model.finishedLevels
    prevTotalScore = Level.tallyScore model.finishedLevels
    totalScore = case model.phase of ---------------------------------
      Playing level -> prevTotalScore + level.score
      _ -> prevTotalScore
    parCompare = prevTotalScore - parSoFar --------------------------------
    totalPar = Level.tallyPar Level.allLevels ---------------------------
    levelPar = case model.phase of -----------------------------------
      Playing level -> level.par
      _ ->
        case model.finishedLevels of
          level :: others -> level.par
          [] -> -1
  in
    Html.div
      [ HAttr.id "game-main"
      , HAttr.style
        [ ("left", (toString model.playport.left) ++ "px")
        , ("top", (toString model.playport.top) ++ "px")
        , ("width", (toString model.playport.width) ++ "px")
        , ("height", (toString model.playport.height) ++ "px")
        , ("font-size", (toString model.playport.fontSize) ++ "px")
        ]
      ]
      [ playingFieldWrapper model.playport
        [ boundary
        , drawBarriers model
        , drawTarget model
        , drawCannon model
        , drawBall model.ball
        , drawTransition model
        , drawSplashBouncers model
        ]
      , drawSplash model
      , drawEndSplash model (attempts, levelPar) (currentLevel, levelCount) (totalScore, totalPar, parCompare)
      , drawTallies model (attempts, levelPar) (currentLevel, levelCount) (totalScore, totalPar, parCompare)
      , help
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

help : Html Msg
help =
  Html.div
    [ HAttr.id "help"
    ]
    [ Html.span [ HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Aim: " ]
      , Html.text "L"
      , Html.span [ HAttr.class "slash" ] [ Html.text "/" ]
      , Html.text "R"
      ]
    , Html.span [ HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Charge: " ]
      , Html.text "Hold SP"
      ]
    , Html.span [ HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Fire: " ]
      , Html.text "Release SP"
      ]
    , Html.span [ HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Putt: " ]
      , Html.text "SHIFT + SP"
      ]
    ]

drawTallies : Model -> (Int, Int) -> (Int, Int) -> (Int, Int, Int) -> Html Msg
drawTallies model (attempts, levelPar) (currentLevel, levelCount) (totalScore, totalPar, parCompare) =
  Html.div
    [ HAttr.id "tallies"
    ]
    [ Html.span [ HAttr.id "round", HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Current: " ]
      , Html.text (if attempts >= 0 then toString attempts else "-")
      , Html.span [ HAttr.class "slash" ] [ Html.text "/" ]
      , Html.text (if levelPar >= 0 then toString levelPar else "-")
      ]
    , Html.span [ HAttr.id "level", HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Level: " ]
      , Html.text (toString currentLevel)
      , Html.span [ HAttr.class "slash" ] [ Html.text "/" ]
      , Html.text (toString levelCount)
      ]
    , Html.span [ HAttr.id "total", HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Total Score: " ]
      , Html.text (toString totalScore)
      , Html.span [ HAttr.class "slash" ] [ Html.text "/" ]
      , Html.text (toString totalPar)
      ]
    , Html.span [ HAttr.id "total", HAttr.class "section" ]
      [ Html.span [ HAttr.class "label" ] [ Html.text "Status: " ]
      , Html.text (if parCompare > 0 then ("+" ++ (toString parCompare)) else toString parCompare)
      ]
    ]

drawSplashBouncers : Model -> Svg Msg
drawSplashBouncers model =
  let
    maybeBouncers = case model.phase of
      Starting bouncers -> Just bouncers
      Ending bouncers -> Just bouncers
      _ -> Nothing
  in
    case maybeBouncers of
      Just bouncers ->
        let
          drawnBalls = bouncers
            |> List.map (\b -> Just b)
            |> List.map drawBall
        in
          Svg.g [] drawnBalls
      Nothing ->
        Svg.g [] []

drawSplash : Model -> Html Msg
drawSplash model =
  case model.phase of
    Starting bouncers ->
      Html.div
        [ HAttr.id "splash"
        ]
        [ Html.div [HAttr.id "splash-inner"]
          [ Html.h1 [] [Html.text "Electron Golf"]
          , Html.p []
            [ Html.text "You're a bored particle physicist with an electron cannon and a proton isolation beam. The lab is empty. It's time to play Electron Golf! Bring the electron into proximity with the captive proton. Under very specific conditions, they'll merge and form a neutron!"
            ]
          , Html.p []
            [ Html.strong [] [Html.text "Aim: "]
            , Html.text "LEFT/RIGHT (+SHIFT/ALT to modify)."
            ]
          , Html.p []
            [ Html.strong [] [Html.text "Shoot: "]
            , Html.text "SPACEBAR to charge. Release to fire."
            ]
          , Html.p []
            [ Html.strong [] [Html.text "Putt: "]
            , Html.text "SHIFT while pressing SPACEBAR."
            ]
          , Html.p []
            [ Html.strong [] [Html.text "Start: "]
            , Html.text "Press SPACEBAR"
            ]
          ]
        ]
    _ ->
      Html.div [] []

drawEndSplash : Model -> (Int, Int) -> (Int, Int) -> (Int, Int, Int) -> Html Msg
drawEndSplash model (attempts, levelPar) (currentLevel, levelCount) (totalScore, totalPar, parCompare) =
  case model.phase of
    Ending bouncers ->
      let
        absParCompare = abs parCompare
        message = if parCompare <= -3 then "Fantastic!"
          else if parCompare <= -2 then "Terrific!"
          else if parCompare <= -1 then "Nice Job!"
          else if parCompare <= 0 then "Well Done"
          else "Game Over"
        parMessage = if parCompare < 0 then Html.span [] [ Html.text "You shot ", val absParCompare, Html.text " under par."]
          else if parCompare == 0 then Html.text "You got par."
          else Html.span [] [ Html.text "You shot ", val absParCompare, Html.text " over par."]
        parFollowup = if parCompare <= -4 then "I suspect you cheated but I can't prove it."
          else if parCompare == -3 then "Few will be able to top that."
          else if parCompare == -2 then "That's worth tweeting about, I think."
          else if parCompare == -1 then "Solid game."
          else if parCompare == 0 then "Not bad there, sport."
          else if parCompare == 1 then "That seems respectable."
          else if parCompare == 2 then "Not bad for a beginner."
          else if parCompare == 3 then "We won't talk about this."
          else if parCompare == 4 then "...and you call yourself a scientist."
          else if parCompare == 5 then "...and you wanna be my latex salesman."
          else "You know lower is better, right?"
      in
        Html.div
          [ HAttr.id "splash"
          , HAttr.class "end-splash"
          ]
          [ Html.div [HAttr.id "splash-inner"]
            [ Html.h1 [] [Html.text message]
            , Html.p []
              [ Html.text "You cleared "
              , Html.strong [ HAttr.class "value" ] [ Html.text (toString levelCount) ]
              , Html.text " protons in "
              , Html.strong [ HAttr.class "value" ] [ Html.text (toString totalScore) ]
              , Html.text " moves."
              ]
            , Html.p []
              [ Html.text "Course par was "
              , Html.strong [ HAttr.class "value" ] [ Html.text (toString totalPar) ]
              , Html.text ". "
              , parMessage
              ]
            , Html.p []
              [ Html.text parFollowup
              ]
            , Html.hr [] []
            , Html.p []
              [ Html.text "Press "
              , Html.strong [ HAttr.class "value" ] [ Html.text "SPACEBAR" ]
              , Html.text (" to play again.")
              ]
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
  let
    progLin = Transition.messageProgress step
    prog = Ease.inExpo progLin
    offset = prog * 100
    opac = 1.0 - progLin
  in
    Svg.g []
      [ drawTheCannon level.cannon
      , drawTheTarget level.target
      , Svg.text_
        [ attrX 500
        , attrY (500 - offset)
        , attrClass "transition-message"
        , SAttr.style ("opacity:" ++ (toString opac))
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
