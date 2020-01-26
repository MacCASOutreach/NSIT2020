
module ClickableRulerGame exposing (..)

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

-- Clickable ruler example.

-- The model type records where you click on the ruler, 
-- as well as time for future animations
type alias Model = { time : Float, size : Float }
                 
-- Define the initial state of the model.
init = { time = 0
       , size = 0
       }

-- we will show two things:  the size, and the ruler, 
-- which when clicked sends a message ClickRuler with the current location
myShapes model = [ text (String.fromFloat model.size |> String.left 5) 
                     |> filled black
                 , ruler 
                     |> notifyTapAt ClickRuler
                 ]

-- the ruler is interesting, because it uses List.map to draw tick marks on the ruler
ruler = group
  ( [ rectangle 180 20 |> filled (rgb 255 255 0)
    , text "Goochi" 
       |> customFont "cursive" 
       |> size 8 
       |> centered 
       |> filled black
       |> move (0,-7)
    ]
  ++ ( List.map tickMark (List.range 1 30) )
  )
  |> move (0,-50)

-- when we draw the tick mark, we position it, and add a number 
tickMark cm = 
  group [ rect 0.5 4 |> filled black
        , text (String.fromInt cm) 
             |> size 4 
             |> centered
             |> filled black
           |> move (0,-6.5)
        ]
     |> move (180 * (toFloat cm - 15.5) / 31 ,8)

-- to use notifyTapAt above, we need to add a message constructor taking an (x,y) point
type Msg = Tick Float GetKeyState
         | ClickRuler (Float,Float)

-- the update needs to handle the new message
-- we calculate the size based on the x-position only
update msg model = case msg of
                     Tick t _ -> { model | time = t}
                     ClickRuler (x,y) -> { model | size = x /180 * 31 + 15.5}
                                       