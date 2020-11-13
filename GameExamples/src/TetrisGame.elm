
module TetrisGame exposing (..)

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
type alias Model = { time : Float , shape : ((Float,Float), Float, List (Float,Float))  }

init : Model
init = { time = 0 
       , shape = ((0,60),0,[(-10,0),(0,0),(10,0),(20,0)])
       }


-- we don't need to use any notifyTap or notifyTapAt in this type of game
myShapes model = 
    let 
        (pos,angle,boxes) = model.shape
    in
        [ drawShape angle boxes
                     |> move pos
                 ]

drawShape angle boxes = 
  group 
    ( List.map oneSquare boxes )
    |> rotate (degrees angle)
    
oneSquare pos = square 10 |> filled blue |> move pos

-- nor do we need any new messages
type Msg = Tick Float GetKeyState


-- the update function records the time (in case we want animations)
update msg model = case msg of
                     Tick t (_,(leftRight,upDown),accelL)
                       ->   { model
                            | time = t
                            , shape =
                                let
                                  (pos,angle,boxes) = model.shape
                                in
                                    if atBottom angle pos boxes then 
                                        (roundPos pos, roundAngle angle, boxes)
                                    else
                                        (leftRightFunction leftRight pos
                                        ,if upDown < 0 then 
                                                angle - 1 
                                            else if upDown > 0 then
                                                angle + 1
                                            else 
                                                roundAngle angle
                                        , boxes)
                            }

atBottom angle (x,y) boxes =
    List.all (yUnderwater (x,y) << rotBy (roundAngle angle)) boxes

rotBy angle (bx,by) =
    if angle == 0 then 
        (bx,by)
    else if angle == 90 then
        (-by,bx)
    else if angle == 180 then
        (-bx,-by)
    else if angle == 270 then
        (by,-bx)
    else 
        (bx,by)
        
yUnderwater (x,y) (bx,by) = (y + by) < -59.5 

roundPos (x,y) =
    ( 10 * (toFloat (round (0.1 * x)))
    , 10 * (toFloat (round (0.1 * y))) )

leftRightFunction leftRightArrow (x,y) =
    if leftRightArrow == 0 then 
        (10 * (toFloat (round (0.1 * x))), y  - 0.5 )
    else 
        (x + leftRightArrow, y)

roundAngle angle =
    if angle > -45 && angle <= 45 then 
      0
    else if angle > 45 && angle <= 135 then 
      90
    else if angle > 135 && angle <= 225 then 
      180
    else if angle > 225 && angle <= 315 then 
      270
    else if angle < 0 then
      roundAngle (angle + 360)
    else 
      roundAngle (angle - 360)
    
      