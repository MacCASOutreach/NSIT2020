{- 
  Choice of Four Game
-}
module BlotterGame exposing (..)

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
        , update = update
        , view = \ model -> { title = "Blotter", body = view model }
        , subscriptions = \_ -> Sub.none
        }

view model = collage 192 128 <| myShapes model

{-blotter example.-}

-- in this example, we
-- 1) get taps from anywhere on the screen,
-- 2) add to a list, and
-- 3) use List.map to draw circles at all the points in our list

-- The interesting thing here is that List.map has two inputs, and the first is
-- a function which takes an (x,y) position as an input and outputs a shape.
myShapes model =
   (List.map drawAt model.positions)
     ++
   [ rect 192 128 |> filled (rgba 0 0 0 0) |> notifyTapAt TapAt
   ]
-- Maybe you haven't seen (rgba 0 0 0 0) it is a completely transparent colour,
-- which we want so nobody can see the rect we put on top of the shapes for
-- the user to tap on.

-- This function will draw a red circle at every point you tap.
-- For fun you can make it into something more fun (like a happy face).
-- Remember to use "group" to group a list of shapes into one shape.
drawAt : (Float,Float) -> Shape Msg
drawAt pos = circle 10 |> filled red |> move pos

type Msg = Tick Float GetKeyState
         | TapAt (Float,Float)

update msg model = 
  ( case msg of
      Tick t _ -> { model | time = t }
      TapAt pos -> { model | positions = pos :: model.positions }
  , Cmd.none
  )

type alias Model = { time : Float, positions : List (Float,Float) }

init : Model
init = { time = 0, positions = [] }
