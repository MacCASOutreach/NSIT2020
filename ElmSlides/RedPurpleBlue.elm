module RedPurpleBlue  exposing (..)

import StateDiagrams exposing (viewStateDiagram, viewEquivalenceStateDiagram)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List exposing(concat,map,map2,foldr,indexedMap,filter,concatMap)
import Char exposing (isDigit, isUpper, toUpper)
import Html exposing (input)
import Html.Events exposing (onInput)
import Html.Attributes exposing (placeholder, value, style, type_)


type State = Red
           | Purple
           | Blue

toString : State -> String
toString s =
  case s of
    Red -> "Red"
    Purple -> "Purple"
    Blue -> "Blue"

changeColour s =
  case s of
    Red -> Purple
    Purple -> Blue
    Blue -> Red

--- make a list of the states and where to draw them

states = [ (Red  , (0,50))
         , (Purple , (-75,-50))
         , (Blue   , (75,-50))
         ]

--- define the transitions you will use

transitions = [(changeColour,"ChangeColour",
                 [(Red      , (-75,25))
                 ]
               )
              ,(changeColour, "ChangeColour",
                 [(Purple  , (0,-75))
                 ]
               )
              ,(changeColour, "ChangeColour",
                 [(Blue  , (75,25))
                 ]
               )
              ]

-- now you can show any node in an active state

main = gameApp Tick {   title = "Red Purple Blue"
                    ,   model = init
                    ,   view = view
                    ,   update = update
                    }

init = { time = 0, state = Red, soFar = "" }

type alias Model = { time : Float, state : State, soFar : String }

type Msg = Tick Float GetKeyState
         | ClickScreen

update msg model = case msg of
    Tick t _ -> { model | time = t }
    ClickScreen ->
      { model | state = changeColour model.state }

view : Model -> Collage Msg
view model = collage 700 900 <| myShapes model

diagram letters maybeState maybeTransition =
  [ scale 2 (viewStateDiagram toString states transitions maybeState maybeTransition)
  , move (200,100)
      <| rotate (pi/2)
      <| scale 10
      <| filled darkGrey
      <| centered
      <| text
      <| let first = String.left 3 letters
             last = String.dropLeft 3 letters
         in first ++ " " ++ last
  , rect 700 900 
      |> filled blank
      |> notifyTap ClickScreen

  ]

myShapes model =
    diagram model.soFar (Just model.state) Nothing

textBox : String -> Float -> Float -> String -> (String -> Msg) -> Shape Msg
textBox txt w h place msg =
    move ( -w / 2, h / 2 ) <|
        html (w * 1.5) (h * 1.5) <|
            input
                [ placeholder place
                , onInput msg
                , value txt
                , style "width" <| String.fromFloat w ++ "px"
                , style "height" <| String.fromFloat h ++ "px"
                , style "margin-top" <| "1px"
                ]
                []