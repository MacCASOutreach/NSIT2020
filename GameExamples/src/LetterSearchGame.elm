
module LetterSearchGame exposing (..)

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

-- change the positioning of the letters to whatever word you want
myLetters = [("R",(-90,-60)),("o",(80,40)),("b",(80,10)),("o",(13,33)),("t",(-60,10))]

-- change the amount of time allowed in the game
timeAllowed = 30

-- change how you want the letter to be drawn
drawOneLetter (letter,letPos) =
  group 
    [ text letter |> centered |> filled red
        |> move (0,-3)
    , circle 6 |> outlined (solid 0.5) red
    ]
        |> move letPos
    
myShapes model = 
  [ player |> scale 0.25
          |> move model.pos
  , group <| List.map drawOneLetter model.lettersToFind
  , if model.lettersToFind == []
      then
        -- change your victory screen
        group
          [ text "You won!" |> filled red
          ]
      else 
        if model.time > timeAllowed
          then
            -- change your unvictory screen
            group
              [ text "You didn't win!" |> filled purple
              ]
          else
            group
              [ text ("You have " ++ (timeAllowed - model.time |> round |> String.fromInt)
                         ++ " seconds left."
                     )
                   |> size 10 |> centered |> filled black |> move (0,54)
              , text "Move your character with the arrow keys to spell a word."
                  |> size 7 |> centered |> filled black |> move (0,-58)
              ]
  ]


type Msg = Tick Float GetKeyState

update msg model = 
  case msg of
    -- many times a second, we get a message telling us the new time
    -- and which keys are pressed
    -- we will use the direction the arrow keys want us to move (changeX,changeY)
    Tick t (_,(changeX,changeY),_) ->
      -- we use the case, to handle the two cases in our game
      case model.lettersToFind of
        -- in the first case, there are no letters level, so nothing to
        -- to except update the time so animations will work on the unvictory screen
        [] -> { model | time = t }
        
        -- in the secon case, there is at least one letter left to find
        -- the pattern (first :: rest) always matches the first thing in a list
        -- and the rest of the things in the list, and assigns them to the two
        -- variables, but in this case, we know the first thing is a pair of
        -- (the letter, the position), so we can break it down in one step
        -- this is called "pattern matching"
        (letter,(letterX,letterY)) :: restOfLetters
          -> -- code to look for letter
              -- the pattern let ... in ... allows us to make definitions
              -- for parts of a more complicated expression
              let
                -- we know the position is an (x,y) pair, so we can match that pattern
                -- and then use the variables x and y
                (x,y) = model.pos
                -- to figure out if we are on the next letter, compare the positions
                -- of the character and the letter, which depends on comparisons
                -- in the x- and y-directions, and using the function && which is true
                -- if both sides are true, to say that all the comparisons have to be true
                found =  ( x < letterX + 5)
                      && ( x > letterX - 5)
                      && ( y < letterY + 5)
                      && ( y > letterY - 5)
                -- moving in (x,y) coordinates is like counting on two
                -- number lines at the same time, one for s and one for y
                newPosition = (x + changeX,y + changeY)
                -- now we can create a new state of the game, saving the time,
                -- and updating the found and unfound letters (if the next
                -- letter was found)
                newModel = { model | time = t
                                    , pos = newPosition
                                    , letters = if found
                                                  then letter :: model.letters
                                                  else model.letters
                                    , lettersToFind =
                                        if found
                                          then restOfLetters
                                          else model.lettersToFind
                           }
              in
                newModel
          
          
-- everything we need to keep track of the state of the game needs to be
-- initialized in this record (a record is a collection of values called fields
-- with names called field labels)
type alias Model = 
        { time : Float
       , letters : List String
       , pos : (Float,Float)
       , lettersToFind : List (String, (Float,Float))
       }  
init = { time = 0
       , letters = []
       , pos = (0,0)
       , lettersToFind = myLetters
       }

-- this is your player
player = group 
 [ -- put your character here
   ngon 6 20 |> outlined (solid 1) (rgb 0 0 255)
  ]
