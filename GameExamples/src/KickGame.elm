
module KickGame exposing (..)

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
-- directions.  KickInGoal.elm turns this into a game.

-- the new thing in this example, is keeping track of position and velocity states
-- both are points (also called vectors)
type alias Model =  { time : Float, position : (Float,Float), velocity : (Float,Float) }
init : Model
init = { time = 0, position = (0,0), velocity = (7,3) }

-- the important thing is that the velocity is the change in position
-- we divide the velocity by 30 because we expect to get 30 Tick messages per second
moveBall (vx,vy) (x,y) = (x + vx/30, y + vy/30)

-- and we will call moveBall every time we get a Tick message
update msg model = case msg of
                     Tick t _ -> { model | time = t
                                         , position = moveBall model.velocity model.position }
                     -- we also start the ball off in the middle of the screen
                     StartAgain -> { model | position = (0,0)
                                           -- moving 3 units up and 4 left every second
                                           , velocity = (3,-4) }
                     -- When we tap on the ball, we get the position of that tap
                     TapAt tap -> { model | velocity = nudge tap model.position model.velocity }

-- and use the different between the tap position and the middle of the ball
--         (x - tapx, y - tapy)
-- to change the velocity (which is the direction the ball is moving)
nudge (tapx,tapy) (x,y) (vx,vy) =
  (vx + (x - tapx), vy + (y - tapy))

-- the rest of the module is like what we did with Blotter
myShapes model =
   ( if inside model.position
       then
         [ text ("Tap to nudge your ball, to keep it in!")
             |> centered
             |> filled orange
             |> move (0,-10)
         ]
       else
         [ text ("Oops!  You ran away!")
             |> centered
             |> filled orange
             |> move (10 * sin model.time, 0)
         , rect 192 128 |> filled (rgba 0 0 0 0) |> notifyTap StartAgain
         ]
   )
   ++
   [circle 10 |> filled red |> move model.position |> notifyTapAt TapAt ]


inside (x,y) = x > -96 && x < 96 && y < 32 && y > -32

type Msg = Tick Float GetKeyState
         | TapAt (Float,Float)
         | StartAgain
