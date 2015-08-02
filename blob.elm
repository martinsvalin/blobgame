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

playerBlob : Blob
playerBlob =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = pi/2
  , radius = 45
  }

type alias Keys = { x:Int, y:Int }


-- UPDATE

update : ((Float, Keys), (Int, Int)) -> Blob -> Blob
update ((dt, keys), (w, h)) blob =
  blob
  |> turn keys
  |> thrust keys
  |> bounce (w, h)
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
    force = 0.1
    limit = 10 / sqrt blob.radius
    v = (toFloat keys.y) * force
    dvx = cos blob.dir * v
    dvy = sin blob.dir * v
  in
    if v > 0
      then
        { blob |
          vx <- within limit (blob.vx + dvx),
          vy <- within limit (blob.vy + dvy)
        }
      else blob

within : Float -> Float -> Float
within limit value =
  max -limit (min limit value)

bounce : (Int, Int) -> Blob -> Blob
bounce (w, h) blob =
  blob
    |> bounceX w
    |> bounceY h

bounceX : Int -> Blob -> Blob
bounceX w blob =
  let
    w' = toFloat w
    right = w'/2
    left = -w'/2
    touchingRight = blob.x + blob.radius > right
    touchingLeft = blob.x - blob.radius < left
  in
    if touchingRight || touchingLeft
      then { blob | vx <- -blob.vx }
      else blob

bounceY : Int -> Blob -> Blob
bounceY h blob =
  let
    h' = toFloat h
    top = h'/2
    bottom = -h'/2
    touchingTop = blob.y + blob.radius > top
    touchingBottom = blob.y - blob.radius < bottom
  in
    if touchingTop || touchingBottom
      then { blob | vy <- -blob.vy }
      else blob

physics : Float -> Blob -> Blob
physics dt blob =
  { blob |
    x <- blob.x + dt * blob.vx,
    y <- blob.y + dt * blob.vy
  }

-- VIEW

view : (Int, Int) -> Blob -> Element
view (w',h') playerBlob =
  let
    (w,h) = (toFloat w', toFloat h')
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 38 38 38)
      , blobForm playerBlob.radius
          |> rotate playerBlob.dir
          |> move (playerBlob.x, playerBlob.y)
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
  Signal.map2 view Window.dimensions (Signal.foldp update playerBlob (Signal.map2 (,) input Window.dimensions))

input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
