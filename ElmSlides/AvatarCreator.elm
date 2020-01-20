module AvatarCreator exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

import ColourPalette exposing (drawRGB, RGB(..))

{-
-}
theFace : Model -> Shape Msg
theFace model =
    let
        mouth = curve (-50,-30) [Pull (0,(-190-5*2)/2) (50, -30)]
    in
      group [
              --THIS IS THE WHOLE FACE
                    --The portion of the hair that shows behind the head
                    (case model.hairFront of
                      15 -> group []
                      16 -> group []
                      14 -> group []
                      _ -> myHairBack model.hairFront model)
                        --|> notifyTap ClickHair

                   ,ears model

                   --The oval that makes up the face
                   ,oval 143 156
                        |> filled black
                        |> move (0,-7)
                   ,oval 140 153
                        |> filled (drawRGB model.skinColour)
                        |> move (0,-7)
                        --|> notifyTap ClickSkin

                   ,freckles |> move (-40,-30) -- modified y value from -20 to -30
                   ,freckles |> move (40,-30) -- modified y value from -20 to -30


                   ,myEyeShape model.eyeShape model

{- This is where other accessories go that can be accessed by
   removing the uncommenting the code.
-}
                   --,blush |> move (-40,-20)
                   --,blush |> move (40,-20)
                   --,hipsterGlasses |> move (0,10)
                -- ,myExtra model.extra model

                 --This is the curve for the mouth
                 ,mouth
                      |> outlined (solid 3) red
                      |> move (0,-60) -- modified y value from -40 to -60
                      |> scale 0.4

                --The portion of the hair that shows infront of the face
                ,(myHairFront model.hairFront model)
                        --|> notifyTap ClickHair
                        |> move(0,60)

                         ] |> scale 0.35 |> move (0,8)--Scale the whole face


view : Model -> List (Shape Msg)
view model =
    [ rect 192 342 |> filled gray
    , theFace model |> scale 2.5
                    |> move(0,50)
    , case model.selected of
        SelectNone ->
           group [ 
                 group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Hair Style"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-2)
                         ] |> move (-20,30)
                            |> notifyTap ClickHair
                 , group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Hair Colour"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-2)
                          ] |> move (20,30)
                            |> notifyTap ClickHairColour
                  , group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Eye Shape"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-2)
                         ] |> move (-20,15)
                            |> notifyTap ClickEye
                  ,group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black

                            , text "Eye Colour"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-2)
                          ] |> move (20,15)
                            |> notifyTap Click
                  ,group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Head"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,2)
                             , text "Covering"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-5)
                          ] |> move (-20,0)
                            |> notifyTap ClickCoverButton
                
                ,group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Head Cover"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,2)
                             , text "Colour"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-5)
                          ] |> move (20,0)
                          |> notifyTap ClickHeadCover
                  ,group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Extra"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,2)
                             , text "Glasses"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-5)
                          ] |> move (-20,-15)
                            |> notifyTap ClickExtra
                ,group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Extra"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,2)
                             , text "Colour"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-5)
                          ] |> move (20,-15)
                          |> notifyTap ClickExtraColour
                ,group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Skin Colour"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-2)
                         ] |> move (-20,-30)
                            |> notifyTap ClickSkin
                ,group [   roundedRect 33 15 4 |> filled (rgb 56 173 180) |> addOutline (solid 1) black
                            , text "Highlight"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,2)
                             , text "Colour"
                                |> centered
                                |> filled black
                                |> scale 0.5
                                |> move (0,-5)
                          ] |> move (20,-30)
                            |> notifyTap ClickHighlightColour                
            ] |> scale 1.75
              |> move (0,-90)
        somethingSelected ->
          group
            [ --rect 1000 1000 |> filled (rgba 0 0 0 0.01) |> notifyTap ClickColourSelected
             ColourPalette.palette ClickColour
                            |> scale 1.3
             ,group [roundedRect 35 17 5 |> filled black
                            , roundedRect 33 15 4 |> filled (rgb 56 173 180)
                            , text "OK"
                                |> centered
                                |> size 16
                                |> filled black
                                |> scale 0.5
                                |> move (-0,-2)
                         ] |> move (0,76)
                            |> notifyTap ClickColourSelected
            ]
              |> move(0,-80)

                   ]

eyes1 model = group [     eyebrows model
                          , curve (10,18) [Pull (35,((0+60)/3)+10)(60, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (15,10) --(55,18)
                          , curve (-60,18) [Pull (-35,((0+60)/3)+10)(-10, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (-15,10)  --(-55,18)
                          ,filled black (circle 20)|> move (-27,0) --Left eye
                          ,filled white (circle 18) |> move (-27,0)
                          ,circle 15
                              |> filled (drawRGB model.eyeColour)
                              |> move (-24,0)
                          ,filled black (circle 12) |> move (-21,2)
                          ,filled white (circle 6) |> move (-18,3)
                          ,filled black (circle 20)|> move (27,0) --Right eye
                          ,filled white (circle 18) |> move (27,0)
                          ,circle 15
                              |> filled (drawRGB model.eyeColour)
                              |> move (30,0)
                          ,filled black (circle 12) |> move (32,2)
                          ,filled white (circle 6) |> move (35,3)
                        ] --|> notifyTap ClickEye

eyes2 model = group [   eyebrows model
                                |> move (0,-10)
                        --, curve (10,18) [Pull (35,((0+60)/3)+10)(60, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (10,-5)--(55,18)
                        --, curve (-60,18) [Pull (-35,((0+60)/3)+10)(-10, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (-10,-5) --(-55,18)
                        , rect 15 1 |> filled black |> rotate (degrees 225) |> move (10,5)
                        , rect 15 1 |> filled black |> rotate (degrees 135) |> move (-10,5)
                        ,wedge 21 0.5 |> filled black |> rotate (degrees -90) |> move(-27,1)
                        , wedge 18 0.5 |> filled white |> rotate (degrees -90)|> move(-27,0)
                        , wedge 15 0.5 |> filled (drawRGB model.eyeColour)
                                      |> rotate (degrees -90)|> move(-24,0)
                        , wedge 12 0.5 |> filled black |> rotate (degrees -90)|> move(-22,0)
                        , wedge 4 0.5|> filled white |> rotate (degrees -90)|> move (-28,0)

                        , wedge 21 0.5 |> filled black |> rotate (degrees -90) |> move(27,1)
                        , wedge 18 0.5 |> filled white |> rotate (degrees -90)|> move(27,0)
                        , wedge 15 0.5 |> filled (drawRGB model.eyeColour) |> rotate (degrees -90)|> move(30,0)
                        , wedge 12 0.5 |> filled black |> rotate (degrees -90)|> move(31,0)
                        , wedge 4 0.5|> filled white |> rotate (degrees -90)|> move (25,0)
                        ] --|> notifyTap ClickEye

eyes3 model = group [ wedge 21 0.5 |> filled black |> rotate (degrees 90) |> move(-27,-1)
                        ,eyebrows model
                       , curve (10,18) [Pull (35,((0+60)/3)+10)(60, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (15,10)--(55,18)
                       , curve (-60,18) [Pull (-35,((0+60)/3)+10)(-10, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (-15,10) --(-55,18)

                       , wedge 18 0.5 |> filled white |> rotate (degrees 90)|> move(-27,0)
                       , wedge 15 0.5 |> filled (drawRGB model.eyeColour) |> rotate (degrees 90)|> move(-24,0)
                       , wedge 12 0.5 |> filled black |> rotate (degrees 90)|> move(-22,-1)
                       , wedge 4 0.5  |> filled white |> rotate (degrees 90)|> move (-31,0)

                       , wedge 21 0.5 |> filled black |> rotate (degrees 90) |> move(27,-1)
                       , wedge 18 0.5 |> filled white |> rotate (degrees 90)|> move(27,0)
                       , wedge 15 0.5 |> filled (drawRGB model.eyeColour)
                                      |> rotate (degrees 90)
                                      |> move(30,0)
                       , wedge 12 0.5 |> filled black |> rotate (degrees 90)|> move(31,-1)
                       , wedge 4 0.5|> filled white |> rotate (degrees 90)|> move (22,0)
                       ] |> move (0,-15)-- |> notifyTap ClickEye


almond colour = group[ curve (15,-15) [Pull (-17,-13) (-15,15)] |> filled colour
                     , curve (15,-15) [Pull (15,20) (-15,15)] |> filled colour]

hollowAlmond thickness colour = group[ curve (15,-15) [Pull (-17,-13) (-15,15)] |> outlined (solid thickness) colour
                     , curve (15,-15) [Pull (15,20) (-15,15)] |> outlined (solid thickness) colour]

eyes4 model = group [ almond white |> rotate (degrees 40) |> move (-28,-4) --|> rotate (degrees 20) ----modified x value (-30 to -28), y value (-5 to 4)
                     , eyebrows model
                            |> move (0,-5)
                     , curve (10,18) [Pull (35,((0+60)/3)+10)(60, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (23,0)--(55,18)
                     , curve (-60,18) [Pull (-35,((0+60)/3)+10)(-10, 18)] |> outlined  (solid 6) black |> scale 0.4 |> move (-18,0) --(-55,18)

                     , almond white |> rotate (degrees 40) |> move (-33,-4) |> scaleX(-1) --|> rotate (degrees -15) |> move (-33,3) --modified x  value (30 to -33) and y value (-5 to 3), degrees from (-20 to -15)
                     , circle 11 |> filled (drawRGB model.eyeColour) |> move (-29,-3)
                     , circle 11 |> filled  (drawRGB model.eyeColour) |> move (34,-3)
                     , circle 8 |> filled black |> move (-28,-3)
                     , circle 8 |> filled black |> move (35,-3)
                     , circle 2 |> filled white |> move (38,0)
                     , circle 2 |> filled white |> move (-25,0)
                     , hollowAlmond 1.85 black |> rotate (degrees 40) |> move(-28,-4)-- |> rotate (degrees 20) --modified x value (-30 to -28), y value (-5 to 5)
                     , hollowAlmond 1.85 black |> rotate (degrees 40)|> move(-33,-4) |> scaleX(-1) --|> rotate (degrees -19)  --modified x value (30 to -32) and y value (-5 to 5), degrees (-20 to -19)
                      ] --|> notifyTap ClickEye

ears model = group [
                   group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled (drawRGB model.skinColour)
                         ] |> move (-70,0)
                           |> notifyTap ClickSkin
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled (drawRGB model.skinColour)
                         ] |> move (70,0)
                           |> notifyTap ClickSkin

                    ]

eyebrows model = group [
                    curve (0,0) [Pull (20,10) (40,0)]
                                |> filled black
                                |> move (-47,23)
                    ,curve (0,0) [Pull (20,10) (40,0)]
                                |> filled black
                                |> move (7,23)

                    ,curve (4,0) [Pull (20,5) (36,0)]
                                |> filled (drawRGB model.hairColour)
                                |> move (-47,24)
                    ,curve (4,0) [Pull (20,5) (36,0)]
                                |> filled (drawRGB model.hairColour)
                                |> move (7,24)
                          ]


--blush = group [circle 15 |> filled (rgba 255 158 158 0.2)]

glasses01 model = group[ roundedRect 40 33 4 |> outlined (solid 4) black |> move(-29,-10)
                             , roundedRect 40 33 4 |> outlined (solid 4) black |> move(29,-10)
                             , curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) black |> move (-6,-3)
                             , curve (-2,0) [Pull (10,-5) (22,0)] |> outlined (solid 3) black |> move (-10,7)
                             , rect 15 6 |> filled black |> move(0,2)
                             , rect 6 5 |> filled black |> move (48,6)
                             , rect 6 5 |> filled black |> move (-48,6)
                             , oval 3 2 |> filled lightGrey |> move (-48.5,6)
                             , oval 3 2 |> filled lightGrey |> move (48.5,6)]
                              |> scale 1.25  |> move (0,10)
--Tilted Glasses
glasses02 model = group [ 
                 curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) (drawRGB model.extraColour) |> move (-6,-3)
                 , roundedRect 40 30 5 |> filled (drawRGB model.extraColour)  |> rotate (degrees -15) |> move(-29,-10) |> makeTransparent 0.25
                 , roundedRect 40 30 5 |> filled (drawRGB model.extraColour)  |> rotate (degrees 15) |> move(29,-10) |> makeTransparent 0.25
                 , roundedRect 40 30 5 |> outlined (solid 5) (drawRGB model.extraColour)  |> rotate (degrees -15) |> move(-29,-10)  
                 , roundedRect 40 30 5 |> outlined (solid 5) (drawRGB model.extraColour) |> rotate (degrees 15) |> move(29,-10)
                  ] |> scale 1.25 |> move (0,10) 
glasses03 model = group [ 
                 curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) (drawRGB model.extraColour) |> move (-6,-6)
                , roundedRect 40 20 5 |> filled (drawRGB model.extraColour)  |> rotate (degrees -5) |> move(-25,-10)   |> makeTransparent 0.75           
                 , roundedRect 40 20 5 |> filled (drawRGB model.extraColour) |> rotate (degrees 5) |> move(25,-10)    |> makeTransparent 0.75
                 , wedge 20 0.5 |> filled (drawRGB model.extraColour) |> rotate (degrees -90) |> move (-25,-15) |> makeTransparent 0.75
                  , wedge 20 0.5 |> filled (drawRGB model.extraColour) |> rotate (degrees -90) |> move (25,-15)  |> makeTransparent 0.75
                   ] |> scale 1.25 |> move (0,10)
glasses04 model  =  group [ 
                 curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) black |> move (-6,-6)
                , roundedRect 40 20 5 |> outlined (solid 1) black  |> rotate (degrees -5) |> move(-25,-10)           
                 , roundedRect 40 20 5 |> outlined (solid 1) black |> rotate (degrees 5) |> move(25,-10)   
                 , roundedRect 40 20 5 |> filled black   |> rotate (degrees -5) |> move(-25,-10)        |> makeTransparent 0.75   
                 , roundedRect 40 20 5 |> filled black  |> rotate (degrees 5) |> move(25,-10)   |> makeTransparent 0.75
                , oval 38 18  |> filled (drawRGB model.extraColour)  |> rotate (degrees -5) |> move(-25,-10)  |> makeTransparent 0.5              
                 , oval 38 18  |> filled (drawRGB model.extraColour) |> rotate (degrees 5) |> move(25,-10) |> makeTransparent 0.5  
                ] |> scale 1.25 |> move (0,10)
glasses05 model = group [ 
                 curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) (drawRGB model.extraColour) |> move (-6,-6)
                , roundedRect 40 20 5 |> filled (drawRGB model.extraColour)  |> rotate (degrees -5) |> move(-25,-10)              
                 , roundedRect 40 20 5 |> filled (drawRGB model.extraColour) |> rotate (degrees 5) |> move(25,-10)   
                , roundedRect 40 20 5 |> filled (drawRGB model.extraColour)  |> rotate (degrees 5) |> move(-25,-15)               
                 , roundedRect 40 20 5 |> filled (drawRGB model.extraColour) |> rotate (degrees -5) |> move(25,-15)  
                ] |> scale 1.25 |> move (0,10)                
glasses06 model =  group [ 
                 curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) (drawRGB model.extraColour) |> move (-6,-6)
                , roundedRect 40 20 5 |> filled (drawRGB model.extraColour)  |> rotate (degrees -5) |> move(-25,-10)         
                 , roundedRect 40 20 5 |> filled (drawRGB model.extraColour) |> rotate (degrees 5) |> move(25,-10) 
                , circle 20 |> filled (drawRGB model.extraColour)  |> rotate (degrees 5) |> move(-25,-15)              
                 , circle 20 |> filled (drawRGB model.extraColour) |> rotate (degrees -5) |> move(25,-15) 
                ] |> scale 1.25 |> move (0,10) 
glasses07 model =  group [ 
                 curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) (drawRGB model.extraColour) |> move (-6,-6)
                 , circle 20 |> outlined (solid 1) (drawRGB model.extraColour)  |> rotate (degrees 5) |> move(-25,-15)              
                 , circle 20 |> outlined (solid 1) (drawRGB model.extraColour) |> rotate (degrees -5) |> move(25,-15)
                 , circle 20 |> filled (drawRGB model.extraColour)  |> rotate (degrees 5) |> move(-25,-15)  |> makeTransparent 0.5            
                 , circle 20 |> filled (drawRGB model.extraColour) |> rotate (degrees -5) |> move(25,-15) |> makeTransparent 0.5
                ] |> scale 1.25 |> move (0,10) 
glasses08 model =  group [ 
                 curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) (drawRGB model.extraColour) |> move (-6,-6)
                  , roundedRect 40 20 5 |> filled (drawRGB model.extraColour)  |> rotate (degrees -5) |> move(-25,-10)         
                 , roundedRect 40 20 5 |> filled (drawRGB model.extraColour) |> rotate (degrees 5) |> move(25,-10) 
                 , oval 40 20  |> filled (drawRGB model.extraColour)  |> rotate (degrees 15) |> move(-25,-13)  |> makeTransparent 0.75              
                 , oval 40 20  |> filled (drawRGB model.extraColour)|> rotate (degrees -15) |> move(25,-13) |> makeTransparent 0.75 
                ] |> scale 1.25 |> move (0,10) 


--Basic Curly hair
hair1f model = group [

             oval 63 43 |> filled black |> move(0,10) --(0,5)
              ,oval 63 43 |> filled black
                      |> move (-35,0) --|> rotate (degrees 20) (-30,0)
              ,oval 63 43 |> filled black
              |> move (35,0) --|> rotate (degrees 160) --(30,0)
              ,oval 58 38 |> filled black
              |> move (60,-20) --|> rotate (degrees 130) --(55,15)
              ,oval 58 38 |> filled black
              |> move (-60,-20) --|> rotate (degrees 50) --(-55,-15)


              ,oval 60 40 |> filled (drawRGB model.hairColour) |> move(0,10) --(0,5)
              ,oval 60 40 |> filled (drawRGB model.hairColour)
                  |> move (-35,0) --|> rotate (degrees 20) --(-30,0)
              ,oval 60 40 |> filled (drawRGB model.hairColour)
              |> move (35,0) -- |> rotate (degrees 160) --(30,0)
              ,oval 55 35 |> filled (drawRGB model.hairColour)
              |> move (60,-20) --|> rotate (degrees 130) --(55,15)
              ,oval 55 35 |> filled (drawRGB model.hairColour)
              |> move (-60,-20) -- |> rotate (degrees 50) --(-55,-15)
              ]


hair1b model = group []


hair1c model = group [curve (-60,-45) [Pull (0,40) (60,-45) ] 
                        |> outlined (solid 10) (drawRGB model.hairColour) 
                        |> move (0,17)
                     ]

--Dreadlocks
hair2f model= group [
              oval 17 27 |> filled black|> move (-70,-35) --|> rotate (degrees 60)
              ,oval 17 27 |> filled black |> move (70,-35) --|> rotate (degrees -60)

              ,oval 12 22 |> filled (drawRGB model.hairColour)|> move (-70,-35) --|> rotate (degrees 60)
              ,oval 12 22 |> filled (drawRGB model.hairColour) |> move (70,-35) --|> rotate (degrees -60)

              ,oval 20 30 |> filled black |> move (65,-25) --|> rotate (degrees -60)
              ,oval 15 25 |> filled (drawRGB model.hairColour) |> move (65,-25) --|> rotate (degrees -60)

              ,oval 20 30 |> filled black |> move (-65,-25) --|> rotate (degrees 60)
              ,oval 15 25 |> filled (drawRGB model.hairColour) |> move (-65,-25) --|> rotate (degrees 60)

              ,oval 20 35 |> filled black |> move (-55,-15) --|> rotate (degrees 50)
              ,oval 15 30 |> filled (drawRGB model.hairColour) |> move (-55,-15)  --|> rotate (degrees 50)


              ,oval 20 35 |> filled black |> move (-45,-5)-- |> rotate (degrees 40)
              ,oval 15 30 |> filled (drawRGB model.hairColour) |> move (-45,-5) --|> rotate (degrees 40)

              ,oval 20 35 |> filled black |> move (55,-15) --|> rotate (degrees 130)
              ,oval 15 30 |> filled (drawRGB model.hairColour) |> move (55,-15)-- |> rotate (degrees 130)

              ,oval 20 35 |> filled black |> move (45,-5)-- |> rotate (degrees -40)
              ,oval 15 30 |> filled (drawRGB model.hairColour) |> move (45,-5)-- |> rotate (degrees -40)

              ,oval 20 40 |> filled black |> move (30,0) --|> rotate (degrees 160)
              ,oval 15 35 |> filled (drawRGB model.hairColour) |> move (30,0)-- |> rotate (degrees 160)

              ,oval 20 40 |> filled black |> move (-30,0)-- |> rotate (degrees 20)
              ,oval 15 35 |> filled (drawRGB model.hairColour) |> move (-30,0)-- |> rotate (degrees 20)

              ,oval 20 40 |> filled black |> move(-15,5) --|> rotate (degrees 15)
              ,oval 15 35 |> filled (drawRGB model.hairColour) |> move(-15,5)-- |> rotate (degrees 15)

             ,oval 20 40 |> filled black |> move(15,5)-- |> rotate (degrees -15)
             ,oval 15 35 |> filled (drawRGB model.hairColour) |> move(15,5)-- |> rotate (degrees -15)
             ,oval 23 43 |> filled black |> move(0,7)
             ,oval 18 38 |> filled (drawRGB model.hairColour) |> move(0,7)
                ]


hair2b model = group[--Long hair
          roundedRect 18 75 10 |> filled black |> move (-70,-50)
          ,roundedRect 13 70 10 |> filled (drawRGB model.hairColour) |> move (-70,-50)

          ,roundedRect 18 75 10 |> filled black |> move (-55,-65)
          ,roundedRect 13 70 10 |> filled (drawRGB model.hairColour) |> move (-55,-65)

          ,roundedRect 18 75 10 |> filled black |> move (70,-50)
          ,roundedRect 13 70 10 |> filled (drawRGB model.hairColour) |> move (70,-50)

          ,roundedRect 18 75 10 |> filled black |> move (55,-65)
          ,roundedRect 13 70 10 |> filled (drawRGB model.hairColour) |> move (55,-65)

          ,roundedRect 18 75 10 |> filled black |> move (-38,-75) |> rotate (degrees -5)
          ,roundedRect 13 70 10 |> filled (drawRGB model.hairColour) |> move (-38,-75) |> rotate (degrees -5)

          ,roundedRect 18 75 10 |> filled black |> move (38,-75) |> rotate (degrees 5)
          ,roundedRect 13 70 10 |> filled (drawRGB model.hairColour) |> move (38,-75) |> rotate (degrees 5)


            --HIGHLIGHTS

          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (50,-50) --(-74.5,-50)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (-70,-55)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (-59,-65)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (-54,-69)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (-42,-75)
                |> rotate (degrees -5)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (-37,-79)
                |> rotate (degrees -5)

          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (74.5,-50)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (70,-55)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (59,-65)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (54,-69)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (42,-75)
                |> rotate (degrees 5)
          ,rect 0.8 60
                |> filled (drawRGB model.highlightColour)
                |> move (37,-79)
                |> rotate (degrees 5)
          ]

--Long hair with bangs
hair3f model = group [
                    bangs model
                    |> scale 1.2
                    |> move (70,50) --(11,10)(70,50)
                   ,bangs model
                    |> scale 1.2
                    |> move (68,50) --(-12,10)(68,50)
                    |> mirrorX

                   ,circle 5
                    |> filled (drawRGB model.hairColour)
                    |> move(0,23.2)

                     --highlights
                   ,curve (0,0) [Pull (50,10) (40,50)]
                    |> outlined (solid 0.8) (drawRGB model.highlightColour)
                    |> move (-44,-37) --modified from(-75,-65)
                    |> scale 1.6
                    ,curve (0,0) [Pull (40,10) (40,50)]
                    |> outlined (solid 0.8) (drawRGB model.highlightColour)
                    |> move (-53,-36) --modified from(-72,-55)
                    |> scale 1.3

                    ,curve (0,0) [Pull (50,10) (40,50)]
                    |> outlined (solid 0.8) (drawRGB model.highlightColour)
                    |> move (-45,-37) --modified from(75,-65)
                    |> scale 1.6
                    |> mirrorX
                    ,curve (0,0) [Pull (40,10) (40,50)]
                    |> outlined (solid 0.8) (drawRGB model.highlightColour)
                    |> move  (-54,-36) --modified from(72,-55)
                    |> scale 1.3
                    |> mirrorX
               ]

hair3b model = group [

               roundedRect 148 143 30
                    |> filled black
                    |> move (0,-50)

               ,roundedRect 145 140 30
                    |> filled (drawRGB model.hairColour)
                    |> move (0,-50)
                , rect 1 140
                    |> filled (drawRGB model.highlightColour)
                    |> move (-60,-30)
                , rect 1 150
                    |> filled (drawRGB model.highlightColour)
                    |> move (-50,-40)
                , rect 1 150
                    |> filled (drawRGB model.highlightColour)
                    |> move (-30,-40)
                , rect 1 140
                    |> filled (drawRGB model.highlightColour)
                    |> move (60,-30)
                , rect 1 150
                    |> filled (drawRGB model.highlightColour)
                    |> move (50,-40)
                , rect 1 150
                    |> filled (drawRGB model.highlightColour)
                    |> move (30,-40)
                , rect 33 153
                    |> filled black
                    |> move (0,-45)

                , rect 30 150
                    |> filled white
                    |> move (0,-50)


               ]

bangs model = group [
                curve (0,0) [Pull (10,50) (40,50)]
                    |> filled black
                    |> move (-73,-63) --(-78,-68)
                    |> scale 1.7
                ,curve (0,0) [Pull (50,10) (40,50)]
                    |> filled black
                    |> move(-73,-63) --(-78,-68)
                    |> scale 1.7

                ,curve (0,0) [Pull (10,50) (40,50)]
                    |> filled (drawRGB model.hairColour)
                    |> move (-75,-64.5)
                    |> scale 1.6
                ,curve (0,0) [Pull (50,10) (40,50)]
                    |> filled (drawRGB model.hairColour)
                    |> move (-76,-64.5)
                    |> scale 1.6
                    ]

--Long curly hair
hair4f model = group [
                oval 60 95
                      |> filled black
                      |> rotate (degrees -65)
                      |> move (-20,-15)
                ,oval 20 30
                      |> filled black
                      |> rotate (degrees 60)
                      |> move (27,0)
                ,oval 60 95
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees -65)
                      |> move (-20,-13)
                ,oval 20 30
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees 60)
                      |> move (27,2)

                --HIGHLIGHTS
                ,curve (14,-10) [Pull (-10,-50) (-54,-38)]
                      |> outlined (solid 1) (drawRGB model.highlightColour)
                ,curve (14,-10) [Pull (-10,-50) (-54,-38)]
                      |> outlined (solid 1) (drawRGB model.highlightColour)
                      |> scale 0.8
                      |> move (-10,0)

                  ]

hair4b model = group [
            -- top of the head
            oval 154 74
                |> filled black
                |> move (0,55)
            ,oval 54 74
                |> filled black
                |> move (55,40)
              |> rotate (degrees -40)
            ,oval 54 74
                |> filled black
                |> move (-55,40)
               |> rotate (degrees 40)
            ,oval 74 184
                |> filled black
              |> rotate (degrees 5)
                |> move (40,-15)
            ,oval 44 74
                |> filled black
                |> move (80,10)
               |> rotate (degrees 5)
            ,oval 74 184
                |> filled black
               |> rotate (degrees -5)
                |> move (-40,-15)
            ,oval 44 74
                |> filled black
                |> move (-80,10)
                |> rotate (degrees -5)
            ,oval 74 184
                |> filled black
                |> rotate (degrees -25)
                |> move (-50,0)
            --long
           ,oval 74 184
                |> filled black
                |> rotate (degrees 25)
                |> move (50,0)
           ,oval 150 70
                |> filled (drawRGB model.hairColour)
                |> move (0,55)
           ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees 25)
                |> move (50,0)
            --long
            ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees 5)
                |> move (40,-15)
            --long
            ,oval 40 70
                |> filled (drawRGB model.hairColour)
                |> move (-80,10)
                |> rotate (degrees -5)
           ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees -25)
                |> move (-50,0)
            --long
            ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees -5)
                |> move (-40,-15)
            --side head
            ,oval 50 70
                |> filled (drawRGB model.hairColour)
                |> move (55,40)
                |> rotate (degrees -40)
            --side head
            ,oval 50 70
                |> filled (drawRGB model.hairColour)
                |> move (-55,40)
                |> rotate (degrees 40)
            ,oval 40 70
                |> filled (drawRGB model.hairColour)
                |> move (80,10)
                |> rotate (degrees 5)

            --HIGHLIGHTS
            ,curve (0,0) [Pull (-10,-20) (0,-40)]
                  |> outlined (solid 1) (drawRGB model.highlightColour)
                  |> move (-90,32)
            ,curve (0,0) [Pull (-10,-20) (0,-40)]
                  |> outlined (solid 1) (drawRGB model.highlightColour)
                  |> move (-90,32) --modified from (90,32)
                  |> mirrorX
            ,curve (-2,0) [Pull (-10,-55) (20,-45)]
                  |> outlined (solid 1) (drawRGB model.highlightColour)
                  |> move (-90,-30)
            ,curve (-2,0) [Pull (-10,-55) (20,-45)]
                  |> outlined (solid 1) (drawRGB model.highlightColour)
                  |> move (-90,-30) --modified from (90,-30)
                  |> mirrorX

                ]
--Two buns
hair5f model = group [

                      curve (0,5) [Pull (-20,-50) (0,-105)]
                      |> filled black
                      |> move (-68,-28)
                      |> rotate (degrees 5)

                     ,curve (0,5) [Pull (-20,-50) (0,-105)]
                      |> filled black
                      |> move (68,-28)
                      |> rotate (degrees -5)
                      |> mirrorX

                      , oval 105 45
                      |> filled black
                      |> rotate (degrees 35)
                      |> move (-30,-20)

                      , oval 105 45
                      |> filled black
                      |> rotate (degrees -35)
                      |> move (30,-20)





                      ,curve (0,0) [Pull (-15,-50) (0,-100)]
                      |> filled (drawRGB model.hairColour)
                      |> move (-69,-28)
                      |> rotate (degrees 5)
                      ,curve (0,0) [Pull (-15,-50) (0,-100)]
                      |> filled (drawRGB model.hairColour)
                      |> move (69,-28)
                      |> rotate (degrees -5)
                      |>mirrorX






                      , oval 102 42
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees 35)
                      |> move (-30,-20)

                      , oval 102 42
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees -35)
                      |> move (30,-20)
                    ]

hair5b model = group [
                  circle 32 |> filled black |> move (-55,60)
                  ,circle 30 |> filled (drawRGB model.hairColour)  |> move (-55,60)
                  ,circle 32 |> filled black |> move (55,60)
                  ,circle 30 |> filled (drawRGB model.hairColour)  |> move (55,60)
                     ]
--Short side part
hair6f model = group [ oval 42 102
                            |> filled black
                            |> rotate (degrees -55)
                            |> move (-20,-10)


                       ,group [curve (0,0) [Pull (10,-10) (-20,-30)]
                                   |> filled black
                               ,curve (0,0) [Pull (-30,-35) (-20,-30)]
                                    |> filled black
                              ] |> move (26,13)
                                |> rotate (degrees 13)
                                |> scale 1.3


                        ,group [curve (0,0) [Pull (10,-10) (-20,-30)]
                                   |> filled (drawRGB model.hairColour)
                                ,curve (0,0) [Pull (-30,-35) (-20,-30)]
                                    |> filled (drawRGB model.hairColour)
                              ] |> move (26,10)
                                |> rotate (degrees 13)

                        ,oval 40 100
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-20,-8)

                        ,oval 10 10
                              |> filled (drawRGB model.hairColour)
                              |> move (28.5,9.5)
                       ,oval 5 11
                              |> filled (drawRGB model.hairColour)
                              |> move (23,13.5)


                         ]

hair6b model = group [oval 73 123
                            |> filled black
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 53 83
                            |> filled black
                            |> rotate (degrees 40)
                            |> move (50,45)


                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 50 80
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees 40)
                            |> move (50,45)


                            ]

--Short bob
hair7f model = group [ oval 60 122
                      |> filled black
                      |> rotate (degrees -65)
                      |> move (-20,-15)
                ,oval 20 30
                      |> filled black
                      |> rotate (degrees 60)
                      |> move (35,0)
                ,oval 60 120
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees -65)
                      |> move (-20,-13)
                ,oval 20 30
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees 60)
                      |> move (35,2)

                      ] |> scale 0.8
hair7b model = group [
                      oval 73 123
                            |> filled (black)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 73 123
                            |> filled black
                            |> rotate (degrees 55)
                            |> move (25,50)
                      ,oval 73 123
                            |> filled black
                            |> move (-50,0)
                      ,oval 73 123
                            |> filled black
                            |> move (50,0)


                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees 55)
                            |> move (25,50)
                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> move (-50,0)
                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> move (50,0)

                      ]
--Close army cut
hair8f model = group [
                      oval 100 30 |> filled black |> move (0,-2)
                      ,oval 100 30 |> filled (drawRGB model.hairColour)
                      , circle 5
                            |> filled (drawRGB model.hairColour)
                            |> move (-46,-2)
                      , circle 5
                            |> filled (drawRGB model.hairColour)
                            |> move (46,-2)

                      --HIGHLIGHTS
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (-25,5) --(-25,5)
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (-30,5) --(-30,5)
                            |> mirrorX



                      ]
hair8b model = group [oval 73 123
                            |> filled (black)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 73 123
                            |> filled black
                            |> rotate (degrees 55)
                            |> move (25,50)



                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees 55)
                            |> move (25,50)

                        --HIGHLIGHTS
                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (-30,80)

                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (30,80)
                            |> mirrorX

                      ]
--Tall hair
hair9f model = group [oval 130 40 |> filled black |> move (0,-8)
                      ,oval 130 40
                            |> filled (drawRGB model.hairColour)
                            |> move (0,-6)


                      , circle 7
                            |> filled (drawRGB model.hairColour)
                            |> move (-60,-12)
                      , circle 7
                            |> filled (drawRGB model.hairColour)
                            |> move (60,-12)
                      ]
hair9b model = group [ oval 153 103 |> filled black
                            |> move (0,40)

                      ,oval 150 100 |> filled (drawRGB model.hairColour)
                            |> move (0,40)



                          ]
--Pony tail
hair10f model = group [oval 120 40
                            |> filled black
                            |> move (0,-7.5)
                      ,oval 120 40
                            |> filled (drawRGB model.hairColour)
                            |> move (0,-6)
                      , circle 5
                            |> filled (drawRGB model.hairColour)
                            |> move (-58,-12)
                      , circle 5
                            |> filled (drawRGB model.hairColour)
                            |> move (58,-12)
                      ,rect 1.4 69 |> filled black
                                |> move (-70,-123)

                      ,curve (0,1) [Pull (-10,15) (-31,10)]
                            |> outlined (solid 1) black
                            |> move (-15,32)

                          --HIGHLIGHTS
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (-25,5)
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (-25,5) --(25,5)
                            |> mirrorX


                            ]
hair10b model = group [

                      oval 113 153
                            |> filled black
                            |> move (-35,35)
                            |> rotate (degrees -10)

                      ,curve (0,0) [Pull (-40,-10) (0,-120) ]
                            |> filled black
                            |> move (-70,35)
                            |> scale 1.1



                      ,oval 110 150
                            |> filled (drawRGB model.hairColour)
                            |> move (-35,35)
                            |> rotate (degrees -10)

                      ,curve (0,0) [Pull (-40,-10) (0,-120) ]
                            |> filled (drawRGB model.hairColour)
                            |> move (-70,30)


                      ,oval 73 123
                            |> filled (black)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 73 123
                            |> filled black
                            |> rotate (degrees 55)
                            |> move (25,50)



                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 70 120
                            |> filled (drawRGB model.hairColour)
                            |> rotate (degrees 55)
                            |> move (25,50)

                        --HIGHLIGHTS
                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (-30,80)

                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (drawRGB model.highlightColour)
                            |> move (30,80)
                            |> mirrorX

                      ]
--Mid length
hair11f model = group [
                oval 55 95
                      |> filled black
                      |> rotate (degrees -45)
                      |> move (-30,-15)
                ,oval 45 90
                      |> filled black
                      |> rotate (degrees 50)
                      |> move (27,-8)
                ,oval 55 95
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees -45)
                      |> move (-30,-13)
                ,oval 45 90
                      |> filled (drawRGB model.hairColour)
                      |> rotate (degrees 50)
                      |> move (26,-6)

                  ]

hair11b model = group [
           oval 58 25
               |> filled black
               |> move (0,80)

           ,oval 74 184
                |> filled black
                |> rotate (degrees 25)
                |> move (50,5)

           ,oval 74 184
                |> filled black
                |> rotate (degrees 5)
                |> move (40,-10)

           ,oval 74 184
                |> filled black
                |> rotate (degrees -25)
                |> move (-50,5)

           ,oval 74 184
                |> filled black
                |> rotate (degrees -5)
                |> move (-40,-10)

           ,oval 58 25
               |> filled (drawRGB model.hairColour)
               |> move (0,78)

           ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees 25)
                |> move (50,5)
            ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees 5)
                |> move (40,-10)


           ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees -25)
                |> move (-50,5)
            ,oval 70 180
                |> filled (drawRGB model.hairColour)
                |> rotate (degrees -5)
                |> move (-40,-10)


                ]
--Side tufts
hair12f model = group [circle 19
                      |> filled black
                      |> move (55,-20)
                ,circle 14
                      |> filled black
                      |> move (62,-33)
                ,circle 14
                      |> filled black
                      |> move (42,-10)

                ,circle 19
                      |> filled black
                      |> move (-55,-20)
                ,circle 14
                      |> filled black
                      |> move (-62,-33)
                ,circle 14
                      |> filled black
                      |> move (-42,-10)



                ,circle 17
                      |> filled (drawRGB model.hairColour)
                      |> move (55,-20)
                ,circle 12
                      |> filled (drawRGB model.hairColour)
                      |> move (62,-33)
                ,circle 12
                      |> filled (drawRGB model.hairColour)
                      |> move (42,-10)

                ,circle 17
                      |> filled (drawRGB model.hairColour)
                      |> move (-55,-20)
                ,circle 12
                      |> filled (drawRGB model.hairColour)
                      |> move (-62,-33)
                ,circle 12
                      |> filled (drawRGB model.hairColour)
                      |> move (-42,-10)

                ]

hair12b model = group []

--One bun
hair13f model = group [ circle 19
                      |> filled black
                      |> move (55,-20)
                ,circle 14
                      |> filled black
                      |> move (62,-33)
                ,circle 18
                      |> filled black
                      |> move (42,-10)
                ,circle 20
                      |> filled black
                      |> move (30,-3)
                ,circle 18
                      |> filled black
                      |> move (15,5)
                ,circle 20
                      |> filled black
                      |> move (0,5)


                ,circle 19
                      |> filled black
                      |> move (-55,-20)
                ,circle 14
                      |> filled black
                      |> move (-62,-33)
                ,circle 18
                      |> filled black
                      |> move (-42,-10)
                ,circle 20
                      |> filled black
                      |> move (-30,-3)
                ,circle 18
                      |> filled black
                      |> move (-15,5)






                ,circle 17
                      |> filled (drawRGB model.hairColour)
                      |> move (55,-20)
                ,circle 12
                      |> filled (drawRGB model.hairColour)
                      |> move (62,-33)
                ,circle 16
                      |> filled (drawRGB model.hairColour)
                      |> move (42,-10)
                ,circle 18
                      |> filled (drawRGB model.hairColour)
                      |> move (30,-3)
                ,circle 16
                      |> filled (drawRGB model.hairColour)
                      |> move (15,5)
                ,circle 18
                      |> filled (drawRGB model.hairColour)
                      |> move (0,5)


                ,circle 17
                      |> filled (drawRGB model.hairColour)
                      |> move (-55,-20)
                ,circle 12
                      |> filled (drawRGB model.hairColour)
                      |> move (-62,-33)
                ,circle 16
                      |> filled (drawRGB model.hairColour)
                      |> move (-42,-10)
                ,circle 18
                      |> filled (drawRGB model.hairColour)
                      |> move (-30,-3)
                ,circle 16
                      |> filled (drawRGB model.hairColour)
                      |> move (-15,5)

                    ]

hair13b model = group [   circle 32 |> filled black |> move (0,80)
                  ,circle 30 |> filled (drawRGB model.hairColour) |> move (0,80)]


beard model =
  group [ circle 19
              |> filled black
              |> move (55,-20)
        ,circle 14
              |> filled black
              |> move (62,-33)
        ,circle 18
              |> filled black
              |> move (42,-10)
        ,circle 20
              |> filled black
              |> move (30,-3)
        ,circle 18
              |> filled black
              |> move (15,5)
        ,circle 20
              |> filled black
              |> move (0,5)
        ,circle 19
              |> filled black
              |> move (-55,-20)
        ,circle 14
              |> filled black
              |> move (-62,-33)
        ,circle 18
              |> filled black
              |> move (-42,-10)
        ,circle 20
              |> filled black
              |> move (-30,-3)
        ,circle 18
              |> filled black
              |> move (-15,5)
        ,circle 17
              |> filled (drawRGB model.hairColour)
              |> move (55,-20)
        ,circle 12
              |> filled (drawRGB model.hairColour)
              |> move (62,-33)
        ,circle 16
              |> filled (drawRGB model.hairColour)
              |> move (42,-10)
        ,circle 18
              |> filled (drawRGB model.hairColour)
              |> move (30,-3)
        ,circle 16
              |> filled (drawRGB model.hairColour)
              |> move (15,5)
        ,circle 18
              |> filled (drawRGB model.hairColour)
              |> move (0,5)
        ,circle 17
              |> filled (drawRGB model.hairColour)
              |> move (-55,-20)
        ,circle 12
              |> filled (drawRGB model.hairColour)
              |> move (-62,-33)
        ,circle 16
              |> filled (drawRGB model.hairColour)
              |> move (-42,-10)
        ,circle 18
              |> filled (drawRGB model.hairColour)
              |> move (-30,-3)
        ,circle 16
              |> filled (drawRGB model.hairColour)
              |> move (-15,5)
              -- Moustache curves
        , curve (45,0) [Pull (45,14) (-1,-7) ]  |> filled black |> mirrorY |> move (-48,-30)
        , curve (45,0) [Pull (45,14) (-1,-7) ]  |> filled black |> mirrorY |> mirrorX |> move (43,-30)
        , curve (45,0) [Pull (44,12) (-1,-7) ]  |> filled (drawRGB model.hairColour) |> mirrorY |> move (-48,-30)
        , curve (45,0) [Pull (44,12) (-1,-7) ]  |> filled (drawRGB model.hairColour) |> mirrorY |> mirrorX |> move (43,-30)
        ]
        |> rotate (degrees 180)
        |> scaleY 1.5
        |> move (0,-135)

freckles =
  let
    brown1 = rgb 61 27 0 -- FIXME should we let them choose?
  in
      group [
            circle 1.3 |> filled brown1
            ,circle 1.3 |> filled brown1 |> move (-4,-4)
            ,circle 1.3 |> filled brown1 |> move (4,-4)]

nothing = group[]



myHairBack hair = case hair of
                  0 -> hair1b
                  1 -> hair2b
                  2 -> hair3b
                  3 -> hair4b
                  4 -> hair5b
                  5 -> hair6b
                  6 -> hair7b
                  7 -> hair8b
                  8 -> hair9b
                  9 -> hair10b
                  10 -> hair11b
                  11 -> hair12b
                  12 -> hair13b
               
                  _ -> ( \ _ -> group [] )

changeCover hf =
  case hf of
    13 -> 14
    14 -> 15
    15 -> 0
    _ -> 13

changeHairFront old = case old of
                  0 -> 1
                  1 -> 2
                  2 -> 3
                  3 -> 4
                  4 -> 5
                  5 -> 6
                  6 -> 7
                  7 -> 8
                  8 -> 9
                  9 -> 10
                  10 -> 11
                  12 -> 0
                  13 -> 13
                  14 -> 14
                  15 -> 15
                  _ -> 0



myHairFront hair = case hair of
                  0 -> hair1f
                  1 -> hair2f
                  2 -> hair3f
                  3 -> hair4f
                  4 -> hair5f
                  5 -> hair6f
                  6 -> hair7f
                  7 -> hair8f
                  8 -> hair9f
                  9 -> hair10f
                  10 -> hair11f
                  11 -> hair12f
                  12 -> hair13f
                  13 -> hijab
                  14 -> turban False
                  15 -> turban True
                  16 -> hair1c
                  _ -> hair1f

--hijab
hijab model = group [
                       wedge 75 0.65 |> filled (drawRGB model.headcoverColour) |> rotate (degrees 90) |> move (0,-15) |> scaleY 0.8
                      , curve (-50,0) [Pull (0,-87)(60,0), Pull (0,-26) (-50,0) ] |> filled (drawRGB model.headcoverColour) |> rotate (degrees 90) |> move (50,-38) |>scaleY 1.5
                      , curve (-50,0) [Pull (0,-87)(60,0), Pull (0,-26) (-50,0) ] |> filled (drawRGB model.headcoverColour) |> rotate (degrees 90)|> mirrorX  |> move (-50,-38) |>scaleY 1.5
                      , curve (-56,0) [Pull (0,-60) (72,9), Pull (0,-20) (-45,19) ] |> filled (drawRGB model.headcoverColour) |> rotate (degrees -30) |> move (-34,-100) |> scaleY 1
                      , curve (-56,0) [Pull (0,-60) (72,9), Pull (0,-20) (-45,19) ] |> filled (drawRGB model.headcoverColour) |> rotate (degrees -30) |> mirrorX |>  move (34,-100) |> scaleY 1
                      , curve (-56,0) [Pull (0,-60) (72,9), Pull (0,-20) (-45,19) ] |> filled (drawRGB model.headcoverColour) |> mirrorX |> move (35,-140)
                      , curve (-56,0) [Pull (0,-60) (72,9), Pull (0,-20) (-45,19) ] |> filled (drawRGB model.headcoverColour) |> move (-35,-140)
                      , curve (-56,0) [Pull (0,-60) (72,9), Pull (0,-20) (-45,19) ] |> filled black |> makeTransparent 0.25 |> rotate(degrees 2) |> move (-25,-122)
                      , curve (-56,0) [Pull (0,-60) (72,9), Pull (0,-20) (-45,19) ] |> filled (drawRGB model.headcoverColour) |> move (-25,-120)
                      , curve (-56,0) [Pull (0,-60) (72,9), Pull (0,-20) (-45,19) ] |> filled (drawRGB model.headcoverColour) |> mirrorX |> move (25,-122)
                      , curve (-27,0) [Pull (0,-25) (70,-5), Pull (0,-20) (-27,0) ] |> filled black |> makeTransparent 0.25 |> rotate (degrees 6) |> move(6,-115)
                      , curve (-27,0) [Pull (0,-3) (72,13) ] |> filled black |> makeTransparent 0.25 |>  rotate (degrees 20)|> move (12,-155)
                       ]
--turban
turban withBeard model = group
  [ if withBeard then beard model else group []
  , wedge 90 0.65 |> filled (drawRGB model.headcoverColour)
    |> rotate (degrees 90)
    |> move (0,-10)
  , polygon [(0,2),(90,0),(0,-2),(0,2)]
    |> filled black |> makeTransparent 0.25
    |> rotate (degrees 30)
    |> move (0,-10)
  ]

cap01 model = group [ oval 50 15 |> filled (drawRGB model.headcoverColour) |> move (0, -8)
        , roundedRect 15 13 5 |> filled (drawRGB model.headcoverColour) |> rotate (degrees 180) |> scale 1.5 -- |> makeTransparent 0.5
        , curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) black  |> rotate (degrees 180) |> scaleX 1.5|> move (9,-5)
        , curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 1) lightGrey  |> rotate (degrees 180) |> scaleX 1.5|> move (9,-5)
        ]  |> scale 4 |> move(0,15)

cap02 model = group [ wedge 15 0.5 |> filled red |> rotate (degrees 90) |> move(0,-10)
        , roundedRect 40 5 5 |> filled lightGrey |> move(0,-10)
        , roundedRect 40 5 5 |> outlined (solid 0.5) darkGrey |> move(0,-10)
        , roundedRect 20 7 5 |> filled red |> rotate (degrees 190) |> move (3,4)
        , oval 10 5 |> filled red |> rotate (degrees -35) |> move (12,4)
        , circle 3 |> outlined (solid 1) darkGrey |> move (15,0)
        , circle 3 |> filled lightGrey |> move (15,0)
       ]  |> scale 4 |> move(0,15)

cap03 model = group [  roundedRect 75 30 5 |> filled brown |> rotate (degrees 5)|> move (30,-20)
                    , roundedRect 75 30 5 |> filled brown |> rotate (degrees -5) |> move (-30,-20)
                    , roundedRect 60 45 5 |> filled brown |> rotate (degrees 25) |> move (15,-5) 
                    , roundedRect 60 45 5 |> filled brown |> rotate (degrees -25) |> move (-15,-5) 
                    , curve (55,0) [Pull (18,-10) (0,0) ]  |> filled black |> scaleX 1.8 |> move (-50,-5)   |> makeTransparent 0.5
                    ]

cap04 model = group [ wedge 60 0.5 |> filled (drawRGB model.headcoverColour) |> rotate (degrees 90) |> scaleY 1  |> move (0,-10)
                    , curve (0,0) [Pull (0,-58) (111,0) ]  |> filled white |> rotate (degrees 117) |> scale 0.5 |> move (30,0)
                    , curve (0,0) [Pull (0,-58) (111,0) ]  |> filled white |> rotate (degrees 117) |> scale 0.5 |> mirrorX |> move (-30,0)
                    , circle 20 |> filled white |> move (-60,-20)
                    , circle 20 |> outlined (solid 1) (drawRGB model.headcoverColour) |> move (-60,-20) 
                    , circle 20 |> filled (drawRGB model.headcoverColour) |> move (-30,-10)
                    , circle 20 |> outlined (solid 1) white |> move (-30,-10)
                    , circle 20 |> filled white |> move (0,0)
                    , circle 20 |> outlined (solid 1) (drawRGB model.headcoverColour) |> move (0,0)
                    , circle 20 |> filled (drawRGB model.headcoverColour) |> move (30,-10)
                    , circle 20 |> outlined (solid 1) white |> move (30,-10)
                    , circle 20 |> filled white |> move (60,-20) 
                    , circle 20 |> outlined (solid 1) (drawRGB model.headcoverColour) |> move (60,-20)      
                    , circle 10 |> filled (drawRGB model.headcoverColour) |> move (0,50)
                    , circle 10 |> outlined (solid 1) white |> move (0,50)              
                    ]

cap05 model = group [ curve (0,0) [Pull (0,-58) (111,0) ]  |> filled black |> rotate (degrees 340) |> scale 0.5 |> move (60,-20)
                    , curve (0,0) [Pull (0,-58) (111,0) ]  |> outlined (solid 1) black |> rotate (degrees 340) |> scale 0.5 |> move (62,-20)  |> makeTransparent 0.5
                    , wedge 65 0.5 |> filled (drawRGB model.headcoverColour) |> rotate (degrees 90) |> scaleY 1  |> move (0,-35)
                    , wedge 65 0.5 |> outlined (solid 1) black |> rotate (degrees 90) |> scaleY 1  |> move (0,-35) |> makeTransparent 0.5
                    , wedge 15 0.5 |> filled black |> rotate (degrees 90) |> scaleX 1  |> move (0,-25) 
                    , roundedRect 35 10 5 |> filled black |> move (0, -30)|> makeTransparent 0.5
                    , curve (0,0) [Pull (0,-14) (27,32) ]  |> outlined (solid 1) black |> rotate (degrees 55) |> scale 1.55 |> move (23,-33)
                    , curve (0,0) [Pull (0,-14) (27,32) ]  |> outlined (solid 1) black |> rotate (degrees 55) |> scale 1.55 |> mirrorX |> move (-23,-33)
                    , oval 10 5 |> filled black |> move (0,30)
                    , oval 10 5 |> outlined (solid 1) black |> move (0,30) |> makeTransparent 0.5
                   
                    ]
--ClickEye  -> {model | eyeShape = changeEyeShape model.eyeShape}

changeEyeShape old = 
    let
        numEyes = 4
    in
    if old == numEyes then
        0
    else
        old + 1

myEyeShape c model = case c of
                0 -> eyes1 model
                1 -> eyes2 model
                2 -> eyes3 model
                3 -> eyes4 model
                _ -> eyes1 model

changeExtra old = case old of
                    0 -> 1
                    1 -> 2
                    2 -> 0
                    _ -> 0

myExtra c = case c of
            0 -> nothing
            1 -> nothing
            _ -> nothing

tick : InputHandler Msg
tick t msg = Tick t msg

type Msg = Tick Float GetKeyState
                | Click | ClickSkin | ClickHair
                | ClickHairColour | ClickHighlightColour
                | ClickEye | ClickCoverButton
                | ClickColour RGB
                | ClickColourSelected  | ClickExtra | ClickExtraColour | ClickHeadCover

update : Msg -> Model -> Model
update msg model = case msg of

                    Tick t _  -> { model | time = t }

                    Click     ->  { model | selected = SelectEye}

                    ClickCoverButton -> { model | hairFront = changeCover model.hairFront}

                    ClickEye  -> {model | eyeShape = changeEyeShape model.eyeShape}

                    ClickSkin ->  { model | selected = SelectSkin}

                    ClickHair -> { model | hairFront = changeHairFront model.hairFront }

                    ClickHairColour -> { model | selected = SelectHair }

                    ClickHighlightColour ->  { model | selected = SelectHighlight}

                    ClickHeadCover -> {model | selected = SelectHeadcover}

                    ClickExtra -> {model | extra = changeExtra model.extra}

                    ClickExtraColour -> {model | selected = SelectExtra}

                    ClickColour clr -> case model.selected of
                        SelectNone ->
                            model  -- this is impossible

                        SelectEye ->
                            { model | eyeColour = clr }

                        SelectHair ->
                          { model | hairColour = clr }

                        SelectHighlight ->
                          { model | highlightColour = clr }

                        SelectSkin ->
                          { model | skinColour = clr }

                        SelectHeadcover ->
                          { model | headcoverColour = clr }
                        SelectExtra -> model
                         -- { model | extraColour = clr }

                    ClickColourSelected -> { model | selected = SelectNone }

                   -- ClickOk -> { model | selected = SelectNone}

type Selection
  = SelectNone
  | SelectEye
  | SelectHair
  | SelectHighlight
  | SelectSkin
  | SelectExtra
  | SelectHeadcover

type alias Model =
       { time : Float,
         eyeColour : RGB,
         skinColour : RGB,
         headcoverColour : RGB,
         hairFront : Int,
         hairColour : RGB,
         highlightColour : RGB,
         eyeShape : Int,
         extra : Int,
         selected : Selection
        }

init : Model
init = { time = 0,
         eyeColour = RGB 153 153 0,
         skinColour = RGB 219 144 101,
         headcoverColour = RGB 204 255 204,
         hairFront = 3,
         hairColour = RGB 25 0 51,
         highlightColour = RGB 102 0 204,
         eyeShape = 0,
         extra = 0,
         selected = SelectNone
        }
