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
import Ball exposing (Ball)
import Barrier exposing (..)


--import Set exposing (Set)
--import V
--import History exposing (History)
--import Dom
--import Debug exposing (log)

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
  , ball : Ball
  , barriers : List Barrier
  }

init : (Model, Cmd Msg)
init =
  let
    viewport = Window.Size 0 0
    ball = Ball.new 100 100 50 20 20
    barriers = []
    model = Model viewport ball barriers
    cmd = Task.perform Resize Window.size
  in
    (model, cmd)




-- UPDATE ###########################################################################

type Msg
  = NoOp
  | Resize Window.Size
  | KeyDown Keyboard.KeyCode
  | Frame Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp ->
      (model, Cmd.none)

    Resize viewport ->
      let
        newBarriers =
          [ Barrier.Vertical 0 0 (toFloat viewport.height)
          , Barrier.Vertical (toFloat viewport.width) 0 (toFloat viewport.height)
          , Barrier.Horizontal 0 0 (toFloat viewport.width)
          , Barrier.Horizontal 0 (toFloat viewport.height) (toFloat viewport.width)
          ]
        newModel = { model | viewport = viewport, barriers = newBarriers }
      in
        (newModel, Cmd.none)

    KeyDown keyCode ->
      (model, Cmd.none) -- if keyCode == 27... etc

    Frame time ->
      let
        hasCollided = Ball.intersectsWithAny model.barriers model.ball
        oldBall = model.ball
        newBall = if hasCollided then { oldBall | velX = 0, velY = 0 } else Ball.next model.ball
        newModel = { model | ball = newBall }
      in
        (newModel, Cmd.none)


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
    , Window.resizes Resize
    , Time.every (100 * Time.millisecond) Frame
    ]



-- VIEW #############################################################################

view : Model -> Html Msg
view model =
  let
    widthStr = toString model.viewport.width
    heightStr = toString model.viewport.height
    widthPx = widthStr ++ "px"
    heightPx = heightStr ++ "px"
    box = makeViewBox 0 0 model.viewport.width model.viewport.height
  in
    Html.div []
      [ Svg.svg
        [ SAttr.viewBox box
        , SAttr.width widthPx
        , SAttr.height heightPx
        ]
        [ ball model.ball
        , barriers model.barriers
        ]
      ]

ball : Ball -> Svg Msg
ball ball =
  let
    cx = toString ball.x
    cy = toString ball.y
    r = toString ball.radius
    cls = SAttr.class "ball"
  in
    Svg.circle [cls, SAttr.cx cx, SAttr.cy cy, SAttr.r r] []

barrier : Barrier -> Svg Msg
barrier barrier =
  case barrier of
    Vertical x y len ->
      let
        x1 = SAttr.x1 (toString x)
        y1 = SAttr.y1 (toString y)
        x2 = SAttr.x2 (toString x)
        y2 = SAttr.y2 (toString (y + len))
        cls = SAttr.class "barrier"
      in
        Svg.line [cls, x1, y1, x2, y2] []
    Horizontal x y len ->
      let
        x1 = SAttr.x1 (toString x)
        y1 = SAttr.y1 (toString y)
        x2 = SAttr.x2 (toString (x + len))
        y2 = SAttr.y2 (toString y)
      in
        Svg.line [SAttr.class "barrier", x1, y1, x2, y2] []

barriers : List Barrier -> Svg Msg
barriers barriers =
  Svg.g [SAttr.class "barriers"] (barriers |> List.map barrier)

makeViewBox : Int -> Int -> Int -> Int -> String
makeViewBox x y width height =
  (toString x) ++ " " ++ (toString y) ++ " " ++ (toString width) ++ " " ++ (toString height)
