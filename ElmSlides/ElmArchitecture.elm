module ElmArchitecture exposing(slide1,slide2,slide3,slide4,slide5,slide6,slide7,slide8,slide9,slide10,slide11,slide12)

import GraphicSVG exposing (..)
import Array
import GraphicSVG.App exposing (..)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

-- a version of the presentation slides that can be copied and pasted into macoutreach.rocks/withUpdate


type Message = Tick Float GetKeyState --The tick needs to have Float and GetKeyState which handles key presses.
              | NextSlide
              | LastSlide

-- MODEL

init = {
              t = 0 ,
            idx = 0 ,
              p = False, -- Pause
              r = 1 , -- Rewind
              a = 1  -- Acceleration
        }


type alias Slide msg = 
    {
        shapes : List (Shape msg)
    ,   audio : Maybe String
    }

-- VIEW
myShapes model =
  let
    t = 0.2 * model.t
    slide = (Maybe.withDefault default (Array.get model.idx slides) t).shapes
  in (slide ++ borders ++ navigators)
  

-- UPDATE

update message model =
  case message of
    Tick tick (getKeyState,changeP1,changeP2) ->
                              if (getKeyState LeftArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = max (model.idx - 1) 0
                              }
                              else if (getKeyState RightArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = min (model.idx + 1) (Array.length slides - 1)
                              }
                              else if (getKeyState Space) == JustDown then
                              { model |
                                  p = not model.p
                              }
                              else if (getKeyState UpArrow) == JustDown then
                              { model |
                                  a = min (model.a * 2) 4
                              }
                              else if (getKeyState DownArrow) == JustDown then
                              { model |
                                  a = max (model.a / 2) 0.5
                              }
                              else if (getKeyState (Key "R")) == JustDown then
                              { model |
                                  r = -model.r
                              }
                              else if (getKeyState Backspace) == JustDown then
                              { model |
                                  t = 0
                              }
                              else if model.p then
                              model
                              else
                              { model |
                                       t = max (model.t + 2.5 * model.a * model.r) 0
                              }
    NextSlide -> { model | t   = 0
                         , idx = min (model.idx + 1) (Array.length slides - 1)
                 }
    LastSlide -> { model | t   = 0
                         , idx = max (model.idx - 1) 0
                 }

--- MISCELLANEOUS

default t = {shapes =  [], audio = Nothing}

borders = [rect 5000 5000
              |> filled white
              |> move (3000,0),
           rect 5000 5000
              |> filled white
              |> move (-3000,0),
           rect 5000 5000
              |> filled white
              |> move (0,2750),
           rect 5000 5000
              |> filled white
              |> move (0,-2750)]

navigators = [ group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                      ] |> move (450,-200)
                        |> makeTransparent 0.5
                |> notifyTap NextSlide
              ,
              group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                    ] |> rotate (degrees 180)
                      |> move (-450,-200)
                      |> makeTransparent 0.5
                |> notifyTap LastSlide
            ]


-- FUNCTIONS

--<< So why do I see (t - 100) or whatever value so often? >>

--   Whenever I do that, I'm basically delaying what I want to happen
--   by that value. Is it measure in seconds, frames or what? What's the unit here?
--   To be honest, I don't know. It has a lot to do with the UPDATE function, and
--   what value for 'x' you are using for " t = model.t + x ".

disappear x n = if x > n then makeTransparent 0 else makeTransparent 1 -- Makes things vanish off the screen!

loop t n = let y = toFloat (floor (t / n)) -- This function is how I make things loop!
           in t - y * n

appear x n =    if x > n then makeTransparent 1 else makeTransparent 0 -- Makes things suddenly appear on the screen!

fadeIn t n = makeTransparent (tranSin (t-n) 1)

fadeOut t n = makeTransparent (1 - (tranSin (t-n) 1))

trans t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) LINEAR.
               then 0
            else Basics.min t y

tranSin t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) Uses sin.
               then 0
            else if t/100 > pi/2 then y
            else sin(t/100) * y

drawLine t (x1,y1) (x2,y2) = line (x1,y1) (x1 + tranSin (t) (x2 - x1), y1 + tranSin (t) (y2 - y1))

-- Down here is where you will find the slides!
-- To add more slides, simply add them to the list below.

slides : Array.Array (Float -> Slide msg)
slides = Array.fromList <| [slide1, slide2, slide3, slide4, slide5, slide6, slide7, slide8, slide9, slide10,slide11,slide12]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-

slide1 t = {shapes = [ background, -- move pointer
             rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             pointer |> scale 0.2
                    |> move (if (-1.5 * t) > (-52) then (-1.5*t) else (-52),if (-50 + t) > (-15) then (-15) else (-50 + t))
                ]
            , audio = Just "sounds/Slide1.mp3"
            }

slide2 t = {shapes = [ background,
            rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (0,-15),
            text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 50 25 |> filled yellow |> move (-75,-20),
             text "DOM" |> size 5 |> customFont "Helvetica" |> bold |> filled black |> move (-92,-16),
             pointer |> scale 0.2 |> move (-52,-15)
                ]
          , audio = Just "sounds/Slide2.mp3"
            }

slide3 t = {shapes = [background, -- change DOM color + msg move to elm runtime
            rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (0,-15),
            text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
            pointer |> scale 0.2 |> move (-52,-15),
            rect 20 10 |> filled yellow |> addOutline (solid 1) black |> move (if (-50 + t) > 1 then 1 else (-50 + t), -16),
            text "Msg" |> size 4 |> bold |> centered |> filled black |> move (if (-50 + t) > 1 then 1 else (-50 + t), -17)
            ]
            , audio = Just "sounds/Slide3.mp3"
            }


slide4 t = {shapes = [ background, -- model and msg move up elm runtime
             rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (10, if (-20 + t) > 18 then 18 else (-20 + t) ),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (10, if (-20 + t) > 18 then 18 else (-20 + t)),
             rect 20 10 |> filled yellow |> addOutline (solid 1) black |> move (1, if (-26 + t) > 11 then 11 else (-26 + t)),
             text "Msg" |> size 4 |> bold |> centered |> filled black |> move (1, if (-26 + t) > 11 then 11 else (-26 + t))
                ]
            , audio = Just "sounds/Slide1.mp3"
            }

slide5 t = {shapes = [ background,-- model and msg move to update
             rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (if (10 + t) > 78 then 78 else (10 + t),18),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (if (10 + t) > 78 then 78 else (10 + t),18),
             rect 20 10 |> filled yellow |> addOutline (solid 1) black |> move (if (1 + t) > 68 then 68 else (1 + t), 11),
             text "Msg" |> size 4 |> bold |> centered |> filled black |> move (if (1 + t) > 68 then 68 else (1 + t), 11)
                ]
            , audio = Just "sounds/Slide2.mp3"
            }

slide6 t =  {shapes = [ background, -- model and cmd move to elm runtime
             rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 20 10 |> filled pink |> addOutline (solid 1) black |> move (if (60 - t) < 1 then 1 else (60 - t),28),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (if (59 - t) < 0 then 0 else (59 - t),26),
             rect 20 10 |> filled lightBlue |> addOutline (solid 1) black |> move (if (50- t) < -10 then -10 else (50 - t), 21),
             text "CMD" |> size 4 |> bold |> centered |> filled black |> move (if (49 - t) < -11 then -11 else (49 - t), 18)
                ]
            , audio = Just "sounds/Slide3.mp3"
            }

slide7 t = {shapes = [ background,
             rect 20 10 |> filled grey |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 20 10 |> filled pink |> addOutline (solid 1) black |> move (0, if (22 - t) < (-15) then (-15) else (22 - t) ),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,if (20 - t) < (-16) then (-16) else (20 - t)),
             rect 20 10 |> filled lightBlue |> addOutline (solid 1) black |> move (if (0- t) < -85 then -85 else (0 - t), 26),
             text "CMD" |> size 4 |> bold |> centered |> filled black |> move (if (-1 - t) < -86 then -86 else (-1 - t), 25)
             ]
             , audio = Just "sounds/Slide1.mp3"
            }

slide8 t = {shapes = [ background,
             rect 20 10 |> filled pink |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 20 10 |> filled lightBlue |> addOutline (solid 1) black |> move (-85,26+t),
             text "CMD" |> size 4 |> bold |> centered |> filled black |> move (-86,25+t),
             square 5 |> filled yellow |> move (-85,35),
             triangle 5 |> filled yellow |> rotate (degrees -30) |> move (-85,39)
             ]
             , audio = Just "sounds/Slide2.mp3"
            }

slide9 t = {shapes = [ background,
             rect 20 10 |> filled pink |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 20 10 |> filled pink |> addOutline (solid 1) black |> move ( if (t) > 75 then 75 else (t),-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (if (t) > 75 then 75 else (t),-16)
             ]
             , audio = Just "sounds/Slide3.mp3"
            }

slide10 t = {shapes = [ background,
            rect 20 10 |> filled pink |> addOutline (solid 1) black |> move (0,-15),
            text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 20 10 |> filled lightPurple|> addOutline (solid 1) black |> move (if (50 - t) < (-75) then (-75) else (50 - t), -28),
             text "Html" |> size 4 |> bold |> centered |> filled black |> move (if (48 - t) < (-77) then (-77) else (48 - t) + 2, -28 - 2)
             ]
             , audio = Just "sounds/Slide1.mp3"
            }

slide11 t = {shapes = [ background,
             rect 20 10 |> filled pink |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             rect 50 25 |> filled yellow |> move (-75,-20),
             text "DOM" |> size 5 |> customFont "Helvetica" |> bold |> filled black |> move (-92,-16)
              ]
            , audio = Just "sounds/Slide2.mp3"
            }

slide12 t = {shapes = [ background,
             rect 20 10 |> filled pink |> addOutline (solid 1) black |> move (0,-15),
             text "Model" |> size 4 |> bold |> centered |> filled black |> move (0,-16),
             triangle 5 |> filled yellow |> rotate (degrees 30) |> move (-75,36),
             square 5 |> filled yellow |> move (-75,40),
             rect 20 10 |> filled yellow |> addOutline (solid 1) black |> move (if(-61 + t) > 0 then 0 else (-61 + t),15),
             text "Msg" |> size 4 |> bold |> centered |> filled black |> move (if(-61 + t) > 0 then 0 else (-61 + t),14)
              ]
              , audio = Just "sounds/Slide3.mp3"
            }

background = group [ rect 50 25
                |> filled grey
                |> move (-75, -20),
             rect 50 25
                |> filled grey
                |> move (-75, 20),
             rect 40 65
                |> filled green
                |> move (0, 0),
             rect 50 25
                |> filled orange
                |> move (75, -20),
             rect 50 25
                |> filled orange
                |> move (75, 20),
             text "Pure functions"
                |> size 10
                |> bold
                |> customFont "Helvetica"
                |> filled black
                |> move (35,50),
             rect 2 5
                |> filled grey
                |> move (28, 55),
             rect 2 5
                |> filled grey
                |> move (28, 45),
             rect 2 5
                |> filled grey
                |> move (28, 36),
             rect 2 5
                |> filled grey
                |> move (28, 27),
             rect 2 5
                |> filled grey
                |> move (28, 17),
             rect 2 5
                |> filled grey
                |> move (28, 7),
             rect 2 5
                |> filled grey
                |> move (28, -3),
             rect 2 5
                |> filled grey
                |> move (28, -13),
            rect 2 5
                |> filled grey
                |> move (28, -23),
             rect 2 5
                |> filled grey
                |> move (28, -34),
             rect 2 5
                |> filled grey
                |> move (28, -43),
             rect 2 5
                |> filled grey
                |> move (28, -53),
             rect 28 2 -- top left A
                |> filled black
                |> move (-36, 15),
             rect 28 2 -- top left B
                |> filled black
                |> move (-34, 25),
             rect 28 2 -- top right A
                |> filled black
                |> move (34, 15),
             rect 28 2 -- top right B
                |> filled black
                |> move (36, 25),
             triangle 1.9 -- top left A
                |> filled black
                |> move (-22, 15),
             triangle 1.9 -- top right A
                |> filled black
                |> move (48, 15),
             triangle 1.9 -- top left B
                |> filled black
                |> rotate (degrees 180)
                |> move (-48, 25),
             triangle 1.9 -- top right B
                |> filled black
                |> rotate (degrees 180)
                |> move (22, 25),
            rect 28 2 -- bottom left B
               |> filled black
               |> move (-36, -15),
            rect 28 2 -- bottom left A
               |> filled black
               |> move (-34, -25),
            rect 28 2 -- bottom right B
               |> filled black
               |> move (34, -15),
            rect 28 2 -- bottom right A
               |> filled black
               |> move (36, -25),
            triangle 1.9 -- bottom left B
               |> filled black
               |> move (-22, -15),
            triangle 1.9 -- bottom right A
               |> filled black
               |> move (48, -15),
            triangle 1.9 -- bottom left A
               |> filled black
               |> rotate (degrees 180)
               |> move (-48, -25),
            triangle 1.9 -- bottom right B
               |> filled black
               |> rotate (degrees 180)
               |> move (22, -25),
             text "Effects"
                |> size 5
                |> bold
                |> customFont "Helvetica"
                |> filled black
                |> move (-92,25),
             text "http requests, ports"
                |> size 3.5
                |> customFont "Helvetica"
                |> filled black
                |> move (-92,20),
             text "DOM"
                |> size 5
                |> customFont "Helvetica"
                |> bold
                |> filled black
                |> move (-92,-16),
             text "Elm Runtime"
                |> size 5
                |> customFont "Helvetica"
                |> bold
                |> filled black
                |> move (-15,0),
             text "Update"
                |> size 5
                |> customFont "Helvetica"
                |> bold
                |> filled black
                |> move (55,23),
             text "View"
                |> size 5
                |> customFont "Helvetica"
                |> bold
                |> filled black
                |> move (55,-16),
            square 5
                |> filled grey |> move (-85,35),
            triangle 5
                |> filled grey |> rotate (degrees -30) |> move (-85,39),
            triangle 5
                |> filled grey |> rotate (degrees 30) |> move (-75,36),
            square 5
                |> filled grey |> move (-75,40)
                ]

pointer = group [triangle 20
                   |> filled white
                   |> rotate (degrees -30)
                   |> scaleY 1.4
                   |> rotate (degrees 30)
                   |> addOutline (solid 3) black
                   ,
                   rect 10 15
                   |> filled white
                   |> addOutline (solid 3) black
                   |> rotate (degrees 30)
                   |> move (11,-19)
                   ,
                   rect 7 6
                   |> filled white
                   |> rotate (degrees 30)
                   |> move (7.5,-12.5)
                   ]
