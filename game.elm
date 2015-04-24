import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Signal exposing (..)
import Color exposing (..)
import List exposing (..)
import Keyboard
import Mouse
import Window
import Text
import Time

type alias Object a = 
    { a | x : Float
        , y : Float
        , vx : Float
        , vy : Float
        , ax : Float
        , ay : Float
        , dir : Float -- In radians
    }

type alias Input = 
    {
      horizontal : Float
    , vertical : Float
    , mouseX : Float
    , mouseY : Float
    }

input : Signal Input
input = let h = toFloat <~ (.x <~ Keyboard.wasd)
            v = toFloat <~ (.y <~ Keyboard.wasd)
            halfWidth = (/) <~ (toFloat <~ (fst <~ Window.dimensions)) ~ constant 2
            halfHeight = (/) <~ (toFloat <~ (snd <~ Window.dimensions)) ~ constant 2
            mx = (-) <~ (toFloat <~ (fst <~ Mouse.position)) ~ halfWidth
            my = (-) <~ halfHeight ~ (toFloat <~ (snd <~ Mouse.position))
        in Input <~ h ~ v ~ mx ~ my

sequence : List (Signal a) -> Signal (List a)
sequence = foldr (Signal.map2 (::)) <| constant []

singleton : a -> List a
singleton a = [a]

type alias Player = Object { acceleration : Float }

defaultPlayer : Player
defaultPlayer = { x = 0
                , y = 0
                , vx = 0
                , vy = 0
                , ax = 0
                , ay = 0
                , dir = 0
                , acceleration = 5
                }

drawPlayer : Object a -> Form
drawPlayer {x, y, dir} = move (x, y) <| rotate dir <| player

stepObject : Float -> Object a -> Object a
stepObject dt obj = 
    { obj | x <- obj.x + obj.vx * dt
          , y <- obj.y + obj.vy * dt
          , vx <- obj.ax + obj.vx - (obj.vx * 0.5) * dt
          , vy <- obj.ay + obj.vy - (obj.vy * 0.5) * dt
          -- , dir <- obj.dir + pi * dt
    }

stepPlayer : (Float, Input) -> Player -> Player
stepPlayer (dt, {horizontal, vertical, mouseX, mouseY}) player =
    let x = horizontal * player.acceleration
        y = vertical * player.acceleration
        c = cos (player.dir - pi/2)
        s = sin (player.dir - pi/2)
        dx = mouseX - player.x
        dy = mouseY - player.y
    in stepObject dt { player | ax <- (x * c - y * s)
                              , ay <- (x * s + y * c)
                              , dir <- atan2 dy dx
                     }

main : Signal Element
main = game

game : Signal Element
game = let width = fst <~ Window.dimensions
           height = snd <~ Window.dimensions
           player' = foldp stepPlayer defaultPlayer ((,) <~ deltaTime ~ input)
       in collage
          <~ width
           ~ height
           ~ sequence [ drawPlayer <~ player' 
                      --, moveX <~ (.mouseX <~ input) ~ (constant <| filled Color.red (circle 3))
                      --, moveY <~ (.mouseY <~ input) ~ (constant <| filled Color.red (circle 3))
                      --, toForm <~ (show <~ input)
                      ]

deltaTime : Signal Float
deltaTime = (\t -> t/Time.second) <~ Time.fps 120

player : Form
player = 
  group [ 
          outlined (solid Color.green) (ngon 3 15)
        ]