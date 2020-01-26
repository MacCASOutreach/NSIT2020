
module OnePaddleGame exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import Random
import Array
import Dict exposing (Dict)
main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init  -- this is the initial state, like you are used to
                        , Cmd.none)-- this requests the first random number
        , update = \ msg model -> (update msg model, Cmd.none)
        , view = \ model -> { title = "Blotter", body = view model }
        , subscriptions = \_ -> Sub.none
        }

view model = collage 192 128 <| myShapes model

-- Keyboard control for two paddles

-- paddles in games usually have no inertia to move like ping-pong paddles
-- their mass is very low compared to the power in arm muscles

type alias Model = { time : Float, paddle : Float }
init : Model
init = { time = 0 , paddle = 0 }

-- we don't need to use any notifyTap or notifyTapAt in this type of game
myShapes model = [ drawPaddle (rgb 255 255 0) model.paddle
                 ]

-- nor do we need any new messages
type Msg = Tick Float GetKeyState

-- position of paddle on Screen
yPaddle = -60

-- since we will have two paddles which look alike, we will use will make a
-- function to draw them, which takes the colour as an input
drawPaddle clr xPos =
  group [ rect 20 3 |> filled clr
        ]
    |> move (xPos,yPaddle)

-- the update function records the time (in case we want animations)
update msg model = case msg of
                     Tick t (_,(dir,_),_)
                       ->  { model
                           | time = t
                           , paddle = dir + model.paddle
                           }
