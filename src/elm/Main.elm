import Html exposing (Html)
import Html.Attributes as HAttr
import Html.Events as HEv
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Json.Decode as Json
import Window
import Task
import Process
import Time exposing (Time)
import Keyboard
import AnimationFrame
import BoxesAndBubbles.Bodies as Bodies
import BoxesAndBubbles as BnB


--import Set exposing (Set)
--import V
--import History exposing (History)
--import Dom
import Debug exposing (log)

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
  , gun : Gun
  , isShift : Bool
  , ball : Maybe Ball
  , bounds : List Barrier
  , barriers : List Barrier
  }

type alias Ball =
  Bodies.Body ()

type alias Barrier =
  Bodies.Body ()

type alias Gun =
  { pos : (Float, Float)
  , angle : Float
  , power : Float
  }

init : (Model, Cmd Msg)
init =
  let
    viewport = Window.Size 0 0
    gun = Gun (100, 100) 0 0
    isShift = False
    ball = Nothing
    bounds = BnB.bounds (0, 0) 10 0.99 (0, 0) ()
    barriers = []
    model = Model viewport gun isShift ball bounds barriers
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
        newBounds = BnB.bounds (w, h) 1 0.99 (cx, cy) ()
        newModel = { model | viewport = newViewport, bounds = newBounds }
      in
        (newModel, Cmd.none)

    KeyDown keyCode ->
      --let
      --  sdf = log "keyCode" keyCode
      --in
        case keyCode of
          37 -> -- right arrow
            let
              gun = model.gun
              incr = if model.isShift then 0.02 else 1.0
              newAngle = gun.angle - incr
              newGun = { gun | angle = newAngle }
              newModel = { model | gun = newGun }
            in
              (newModel, Cmd.none)
          39 -> -- left arrow
            let
              gun = model.gun
              incr = if model.isShift then 0.02 else 1.0
              newAngle = gun.angle + incr
              newGun = { gun | angle = newAngle }
              newModel = { model | gun = newGun }
            in
              (newModel, Cmd.none)
          16 -> -- shift key
            let newModel = { model | isShift = True }
            in (newModel, Cmd.none)
          32 -> -- space bar
            let
              gun = model.gun
              incr = if model.isShift then 0.2 else 2
              newPower = gun.power + incr
              newGun = { gun | power = newPower }
              newModel = { model | gun = newGun }
            in
              (newModel, Cmd.none)
          _ ->
            (model, Cmd.none)

    KeyUp keyCode ->
      case keyCode of
        16 -> -- shift key
          let newModel = { model | isShift = False }
          in (newModel, Cmd.none)
        32 -> -- space bar
          let
            gun = model.gun
            velX = (cos ((gun.angle / 360) * pi * 2)) * (gun.power / 5)
            velY = (sin ((gun.angle / 360) * pi * 2)) * (gun.power / 5)
            newBall = Just (BnB.bubble 30 1.0 1.0 model.gun.pos (velX, velY) ())
            newGun = { gun | power = 0 }
            newModel = { model | gun = newGun, ball = newBall }
          in
            (newModel, Cmd.none)
        _ ->
          (model, Cmd.none)

    Frame time ->
      case model.ball of
        Just ball ->
          let
            newVelocity = reduceVelocity ball.velocity
            slowerBall = { ball | velocity = newVelocity }
            shapes = slowerBall :: model.bounds
            newShapes = BnB.step (0, 0) (0, 0) shapes
            newBalls = List.filter isBubble newShapes
            newBall = List.head newBalls
            newModel = { model | ball = newBall }
          in
            (newModel, Cmd.none)
        Nothing ->
          (model, Cmd.none)

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
        , drawGun model.gun
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

drawGun : Gun -> Svg Msg
drawGun gun =
  let
    (x, y) = gun.pos
    classAttr = SAttr.class "gun"
    transformAttr = SAttr.transform ("translate(" ++ (toString x) ++ "," ++ (toString y) ++ ") rotate(" ++ (toString gun.angle) ++ ")")
  in
    Svg.g
      [ transformAttr
      , classAttr
      ]
      [ Svg.rect [SAttr.x "20", SAttr.y "-10", SAttr.width "20", SAttr.height "20", SAttr.class "barrel"] []
      , Svg.circle [SAttr.cx "0", SAttr.cy "0", SAttr.r "20"] []
      , Svg.line [SAttr.x1 "20", SAttr.y1 "0", SAttr.x2 "2100", SAttr.y2 "0"] []
      , drawPowerGauge gun.power
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
