import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

-- MODEL

type alias Blob =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Float
  , radius : Float
  }

blob : Blob
blob =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = pi/2
  , radius = 45
  }

type alias Keys = { x:Int, y:Int }


-- UPDATE

update : (Float, Keys) -> Blob -> Blob
update (dt, keys) blob =
  blob
  |> turn keys
  |> thrust keys
  |> physics dt

turn : Keys -> Blob -> Blob
turn keys blob =
  let
    turnSpeed = 10
  in
    { blob |
      dir <- blob.dir + (toFloat keys.x)/turnSpeed
    }

thrust : Keys -> Blob -> Blob
thrust keys blob =
  let
    speed = 0.3
    v = (toFloat keys.y) * speed
    dvx = cos blob.dir * v
    dvy = sin blob.dir * v
  in
    if v > 0
      then
        { blob |
          vx <- blob.vx + dvx,
          vy <- blob.vy + dvy
        }
      else blob

physics : Float -> Blob -> Blob
physics dt blob =
  { blob |
    x <- blob.x + dt * blob.vx,
    y <- blob.y + dt * blob.vy
  }

-- VIEW

view : (Int, Int) -> Blob -> Element
view (w',h') blob =
  let
    (w,h) = (toFloat w', toFloat h')
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 38 38 38)
      , blobForm blob.radius
          |> rotate blob.dir
          |> move (blob.x, blob.y)
      ]


blobForm : Float -> Form
blobForm radius =
  let
    (w,h) = (radius, 2)
    positionX = radius/2
  in
    group
      [ circle radius
          |> filled (rgb 255 180 200)
      , rect w h
          |> filled (rgb 0 0 0)
          |> moveX positionX
      ]

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update blob input)

input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
