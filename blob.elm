import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

-- MODEL

type alias Blob =
  { x : Float, y : Float, vx : Float, vy : Float, dir : Float, radius : Float }

blob : Blob
blob = { x = 0, y = 0, vx = 0, vy = 0, dir = 0, radius = 15 }

type alias Keys = { x:Int, y:Int }


-- UPDATE

update : (Float, Keys) -> Blob -> Blob
update (dt, keys) blob =
  blob
  |> thrust keys
  |> physics dt

thrust : Keys -> Blob -> Blob
thrust keys blob =
  { blob |
    vy <- blob.vy + (toFloat keys.y)
  }

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
        , circle blob.radius
          |> filled (rgb 255 180 200)
          |> move (blob.x, blob.y)
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
