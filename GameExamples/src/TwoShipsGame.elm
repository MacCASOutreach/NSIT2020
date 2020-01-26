
module TwoShipsGame exposing (..)

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


-- Keyboard control for two players

-- two players are moving around, call them L and R, so we need to keep
-- track of both where they are, and where they are headed (their velocity)
type alias Model = { time : Float , posL : (Float,Float), posR : (Float,Float), velL : (Float,Float), velR : (Float,Float) }
init : Model
init = { time = 0 , posL = (-20,0), posR = (20,0), velL = (0,0), velR = (0,0) }

-- the definition of velocity is the change in the position in a unit of time
-- the definition of acceleration is the change in the velocity in a unit of time
-- Wow!  Those sound very similar.

-- We only need function to make the change needed over one unity of time.
-- We will use the time between Ticks as the unit of time, although
-- this is not a great unit of time, since Ticks come more slowly when
-- your computer is busy, or just gets slow.
timeStep (changeX,changeY) (x,y) = (x + s * changeX, y + s * changeY)
-- To make the game playable, we use a scaling factor, s.
s = 0.1
-- You aren't supposed to know about this defintion of velocity until grade 11,
-- so don't tell anyone we told you.

-- we don't need to use any notifyTap or notifyTapAt in this type of game
myShapes model = [ drawShip (rgb 255 0 0) model.posL model.velL
                 , drawShip (rgb 0 0 255) model.posR model.velR
                 ]

-- nor do we need any new messages
type Msg = Tick Float GetKeyState

-- since we will have two ships which look alike, we will use will make a
-- function to draw them, which takes the colour as an input
-- Do you know why this ship points in the direction of motion?
drawShip clr pos (vx,vy) =
  group [ polygon [(2 * vx,2 * vy),(-vy,vx),(vy,-vx)] |> filled clr
        , circle 1 |> filled clr
        ]
    |> move pos

-- the update function records the time (in case we want animations)
-- and uses the timeStep function to change the two positions and two
-- velocities according to their definitions
update msg model = case msg of
                     Tick t (_,accelR,accelL)
                       ->  { model
                           | time = t
                           , posL = timeStep model.velL model.posL
                           , posR = timeStep model.velR model.posR
                           , velL = timeStep accelL model.velL
                           , velR = timeStep accelR model.velR
                           }
