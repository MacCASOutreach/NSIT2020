
module KickInGoalGame exposing (..)

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

-- In this game, we use notifyTap, introduced in Blotter.elm to nudge a ball in different
-- directions.  The goal is to put the ball in the goal, and not kick it off the field.

myShapes model =
   ( if model.goals >= 0
       then
         [ text ("Tap to nudge your ball, to keep it in! " ++ String.fromInt model.goals)
             |> centered
             |> filled orange
             |> move (0,-10)
         ]
       else
         [ text ("Oops!  You have negative goals!  Tap to start again.")
             |> centered
             |> filled orange
             |> move (10 * sin (7*model.time), 8 * sin (13*model.time))
         , rect 192 128 |> filled (rgba 0 0 0 0) |> notifyTap StartAgain
         ]
   )
   ++
   -- the goal
   [ rect 20 60 |> outlined (dotted 2) green |> move (-85,0)
   -- the ball
   , circle 10 |> filled red |> move model.position |> notifyTapAt TapAt ]


inside (x,y) = x > -110 && x < 110 && y < 72 && y > -72

type Msg = Tick Float GetKeyState
         | TapAt (Float,Float)
         | StartAgain

update msg model = case msg of
                     -- now when time goes by, we have to test the position of the
                     -- ball to see if it goes into the goal, and if not if it
                     -- goes out of bounds
                     Tick t _ -> if inGoal model.position
                                   then
                                     { model | time = t
                                             , position = (40,0)
                                             , velocity = (-3,8)
                                             , goals = model.goals + 1 }
                                   else
                                     if inside model.position
                                       then
                                         { model | time = t
                                                 , position = moveBall model.velocity model.position }
                                       else
                                         { model | time = t
                                                 , position = (0,0), velocity = (3,-4)
                                                 , goals = model.goals - 1
                                         }
                     StartAgain -> { model | goals = 0, position = (0,0), velocity = (3,-4) }
                     TapAt tap -> { model | velocity = nudge tap model.position model.velocity }

-- we are using the same logic here that we used in KickBall.elm.
-- && means both sides have to be true
-- since we only use && for "and" and not || for "or", we don't need ()s
inGoal (x,y) = x < -85 && y < 15 && y > -15

moveBall (vx,vy) (x,y) = (x + vx/30, y + vy/30)

nudge (tapx,tapy) (x,y) (vx,vy) =
  (vx + (x - tapx), vy + (y - tapy))

type alias Model = { time : Float, position : (Float,Float), velocity : (Float,Float), goals : Int }
init : Model
init = { time = 0, position = (0,0), velocity = (7,3), goals = 0 }

