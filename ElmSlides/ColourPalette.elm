module ColourPalette exposing (..)

import GraphicSVG exposing (..)

type RGB = RGB Float Float Float

drawRGB (RGB r g b) = rgb r g b

palette clickMsg = group 
          [ rect 140 100 |> outlined (solid 1) black
          , square 9 |> filled (rgb 51 0 0) |> move(-60,40) |> notifyTap (clickMsg (RGB 51 0 0))
          , square 9 |> filled (rgb 51 25 0) |> move(-50,40) |> notifyTap (clickMsg (RGB 51 25 0))
          , square 9 |> filled (rgb 51 51 0) |> move(-40,40) |> notifyTap (clickMsg (RGB 51 51 0))
          , square 9 |> filled (rgb 25 51 0) |> move(-30,40) |> notifyTap (clickMsg (RGB 25 51 0))
          , square 9 |> filled (rgb 0 51 0) |> move(-20,40) |> notifyTap (clickMsg (RGB 0 51 0))
          , square 9 |> filled (rgb 0 51 25) |> move(-10,40) |> notifyTap (clickMsg (RGB 0 51 25))
          , square 9 |> filled (rgb 0 51 51) |> move(0,40) |> notifyTap (clickMsg (RGB 0 51 51))
          , square 9 |> filled (rgb 0 25 51) |> move(10,40) |> notifyTap (clickMsg (RGB 0 25 51))
          , square 9 |> filled (rgb 0 0 51) |> move(20,40) |> notifyTap (clickMsg (RGB 0 0 51))
          , square 9 |> filled (rgb 25 0 51) |> move(30,40) |> notifyTap (clickMsg (RGB 25 0 51))
          , square 9 |> filled (rgb 51 0 51) |> move(40,40) |> notifyTap (clickMsg (RGB 51 0 51))
          , square 9 |> filled (rgb 51 0 25) |> move(50,40) |> notifyTap (clickMsg (RGB 51 0 25))
          , square 9 |> filled (rgb 0 0 0) |> move(60,40) |> notifyTap (clickMsg (RGB 0 0 0))
          , square 9 |> filled (rgb 102 0 0) |> move(-60,30) |> notifyTap (clickMsg (RGB 102 0 0))
          , square 9 |> filled (rgb 102 51 0) |> move(-50,30) |> notifyTap (clickMsg (RGB 102 51 0))
          , square 9 |> filled (rgb 102 102 0) |> move(-40,30) |> notifyTap (clickMsg (RGB 102 102 0))
          , square 9 |> filled (rgb 51 102 0) |> move(-30,30) |> notifyTap (clickMsg (RGB 51 102 0))
          , square 9 |> filled (rgb 0 102 0) |> move(-20,30) |> notifyTap (clickMsg (RGB 0 102 0))
          , square 9 |> filled (rgb 0 102 51) |> move(-10,30) |> notifyTap (clickMsg (RGB 0 102 51))
          , square 9 |> filled (rgb 0 102 102) |> move(0,30) |> notifyTap (clickMsg (RGB 0 102 102))
          , square 9 |> filled (rgb 0 51 102) |> move(10,30) |> notifyTap (clickMsg (RGB 0 51 102))
          , square 9 |> filled (rgb 0 0 102) |> move(20,30) |> notifyTap (clickMsg (RGB 0 0 102))
          , square 9 |> filled (rgb 51 0 102) |> move(30,30) |> notifyTap (clickMsg (RGB 51 0 102))
          , square 9 |> filled (rgb 102 0 102) |> move(40,30) |> notifyTap (clickMsg (RGB 102 0 102))
          , square 9 |> filled (rgb 102 0 51) |> move(50,30) |> notifyTap (clickMsg (RGB 102 0 51))
          , square 9 |> filled (rgb 32 32 32) |> move(60,30) |> notifyTap (clickMsg (RGB 32 32 32))
          , square 9 |> filled (rgb 153 0 0) |> move(-60,20) |> notifyTap (clickMsg (RGB 153 0 0))
          , square 9 |> filled (rgb 153 76 0) |> move(-50,20) |> notifyTap (clickMsg (RGB 153 76 0))
          , square 9 |> filled (rgb 153 153 0) |> move(-40,20) |> notifyTap (clickMsg (RGB 153 153 0))
          , square 9 |> filled (rgb 76 153 0) |> move(-30,20) |> notifyTap (clickMsg (RGB 76 153 0))
          , square 9 |> filled (rgb 0 153 0) |> move(-20,20) |> notifyTap (clickMsg (RGB 0 153 0))
          , square 9 |> filled (rgb 0 153 76) |> move(-10,20) |> notifyTap (clickMsg (RGB 0 153 76))
          , square 9 |> filled (rgb 0 153 153) |> move(0,20) |> notifyTap (clickMsg (RGB 0 153 153))
          , square 9 |> filled (rgb 0 76 153) |> move(10,20) |> notifyTap (clickMsg (RGB 0 76 153))
          , square 9 |> filled (rgb 0 0 153) |> move(20,20) |> notifyTap (clickMsg (RGB 0 0 153))
          , square 9 |> filled (rgb 76 0 153) |> move(30,20) |> notifyTap (clickMsg (RGB 76 0 153))
          , square 9 |> filled (rgb 153 0 153) |> move(40,20) |> notifyTap (clickMsg (RGB 153 0 153))
          , square 9 |> filled (rgb 153 0 76) |> move(50,20) |> notifyTap (clickMsg (RGB 153 0 76))
          , square 9 |> filled (rgb 64 64 64) |> move(60,20) |> notifyTap (clickMsg (RGB 64 64 64))
          , square 9 |> filled (rgb 204 0 0) |> move(-60,10) |> notifyTap (clickMsg (RGB 204 0 0))
          , square 9 |> filled (rgb 204 102 0) |> move(-50,10) |> notifyTap (clickMsg (RGB 204 102 0))
          , square 9 |> filled (rgb 204 204 0) |> move(-40,10) |> notifyTap (clickMsg (RGB 204 204 0))
          , square 9 |> filled (rgb 102 204 0) |> move(-30,10) |> notifyTap (clickMsg (RGB 102 204 0))
          , square 9 |> filled (rgb 0 204 0) |> move(-20,10) |> notifyTap (clickMsg (RGB 0 204 0))
          , square 9 |> filled (rgb 0 204 102) |> move(-10,10) |> notifyTap (clickMsg (RGB 0 204 102))
          , square 9 |> filled (rgb 0 204 204) |> move(0,10) |> notifyTap (clickMsg (RGB 0 204 204))
          , square 9 |> filled (rgb 0 102 204) |> move(10,10) |> notifyTap (clickMsg (RGB 0 102 204))
          , square 9 |> filled (rgb 0 0 204) |> move(20,10) |> notifyTap (clickMsg (RGB 0 0 204))
          , square 9 |> filled (rgb 102 0 204) |> move(30,10) |> notifyTap (clickMsg (RGB 102 0 204))
          , square 9 |> filled (rgb 204 0 204) |> move(40,10) |> notifyTap (clickMsg (RGB 204 0 204))
          , square 9 |> filled (rgb 204 0 102) |> move(50,10) |> notifyTap (clickMsg (RGB 204 0 102))
          , square 9 |> filled (rgb 96 96 96) |> move(60,10) |> notifyTap (clickMsg (RGB 96 96 96))
          , square 9 |> filled (rgb 255 0 0) |> move(-60,0) |> notifyTap (clickMsg (RGB 255 0 0))
          , square 9 |> filled (rgb 255 128 0) |> move(-50,0) |> notifyTap (clickMsg (RGB 255 128 0))
          , square 9 |> filled (rgb 255 255 0) |> move(-40,0) |> notifyTap (clickMsg (RGB 255 255 0))
          , square 9 |> filled (rgb 128 255 0) |> move(-30,0) |> notifyTap (clickMsg (RGB 128 255 0))
          , square 9 |> filled (rgb 0 255 0) |> move(-20,0) |> notifyTap (clickMsg (RGB 0 255 0))
          , square 9 |> filled (rgb 0 255 128) |> move(-10,0) |> notifyTap (clickMsg (RGB 0 255 128))
          , square 9 |> filled (rgb 0 255 255) |> move(0,0) |> notifyTap (clickMsg (RGB 0 255 255))
          , square 9 |> filled (rgb 0 128 255) |> move(10,0) |> notifyTap (clickMsg (RGB 0 128 255))
          , square 9 |> filled (rgb 0 0 255) |> move(20,0) |> notifyTap (clickMsg (RGB 0 0 255))
          , square 9 |> filled (rgb 127 0 255) |> move(30,0) |> notifyTap (clickMsg (RGB 127 0 255))
          , square 9 |> filled (rgb 255 0 255) |> move(40,0) |> notifyTap (clickMsg (RGB 255 0 255))
          , square 9 |> filled (rgb 255 0 127) |> move(50,0) |> notifyTap (clickMsg (RGB 255 0 127))
          , square 9 |> filled (rgb 128 128 128) |> move(60,0) |> notifyTap (clickMsg (RGB 128 128 128))
          , square 9 |> filled (rgb 255 51 51) |> move(-60,-10) |> notifyTap (clickMsg (RGB 255 51 51))
          , square 9 |> filled (rgb 255 153 51) |> move(-50,-10) |> notifyTap (clickMsg (RGB 255 153 51))
          , square 9 |> filled (rgb 255 255 51) |> move(-40,-10) |> notifyTap (clickMsg (RGB 255 255 51))
          , square 9 |> filled (rgb 153 255 51) |> move(-30,-10) |> notifyTap (clickMsg (RGB 153 255 51))
          , square 9 |> filled (rgb 51 255 51) |> move(-20,-10) |> notifyTap (clickMsg (RGB 51 255 51))
          , square 9 |> filled (rgb 51 255 153) |> move(-10,-10) |> notifyTap (clickMsg (RGB 51 255 153))
          , square 9 |> filled (rgb 51 255 255) |> move(0,-10) |> notifyTap (clickMsg (RGB 51 255 255))
          , square 9 |> filled (rgb 51 153 255) |> move(10,-10) |> notifyTap (clickMsg (RGB 51 153 255))
          , square 9 |> filled (rgb 51 51 255) |> move(20,-10) |> notifyTap (clickMsg (RGB 51 51 255))
          , square 9 |> filled (rgb 153 51 255) |> move(30,-10) |> notifyTap (clickMsg (RGB 153 51 255))
          , square 9 |> filled (rgb 255 51 255) |> move(40,-10) |> notifyTap (clickMsg (RGB 255 51 255))
          , square 9 |> filled (rgb 255 51 153) |> move(50,-10) |> notifyTap (clickMsg (RGB 255 51 153))
          , square 9 |> filled (rgb 160 160 160) |> move(60,-10) |> notifyTap (clickMsg (RGB 160 160 160))
          , square 9 |> filled (rgb 255 102 102) |> move(-60,-20) |> notifyTap (clickMsg (RGB 255 102 102))
          , square 9 |> filled (rgb 255 178 102) |> move(-50,-20) |> notifyTap (clickMsg (RGB 255 178 102))
          , square 9 |> filled (rgb 255 255 102) |> move(-40,-20) |> notifyTap (clickMsg (RGB 255 255 102))
          , square 9 |> filled (rgb 178 255 102) |> move(-30,-20) |> notifyTap (clickMsg (RGB 178 255 102))
          , square 9 |> filled (rgb 102 255 102) |> move(-20,-20) |> notifyTap (clickMsg (RGB 102 255 102))
          , square 9 |> filled (rgb 102 255 178) |> move(-10,-20) |> notifyTap (clickMsg (RGB 102 255 178))
          , square 9 |> filled (rgb 102 255 255) |> move(0,-20) |> notifyTap (clickMsg (RGB 102 255 255))
          , square 9 |> filled (rgb 102 178 255) |> move(10,-20) |> notifyTap (clickMsg (RGB 102 178 255))
          , square 9 |> filled (rgb 102 102 255) |> move(20,-20) |> notifyTap (clickMsg (RGB 102 102 255))
          , square 9 |> filled (rgb 178 102 255) |> move(30,-20) |> notifyTap (clickMsg (RGB 178 102 255))
          , square 9 |> filled (rgb 255 102 255) |> move(40,-20) |> notifyTap (clickMsg (RGB 255 102 255))
          , square 9 |> filled (rgb 255 102 178) |> move(50,-20) |> notifyTap (clickMsg (RGB 255 102 178))
          , square 9 |> filled (rgb 192 192 192) |> move(60,-20) |> notifyTap (clickMsg (RGB 192 192 192))
          , square 9 |> filled (rgb 255 153 153) |> move(-60,-30) |> notifyTap (clickMsg (RGB 255 153 153))
          , square 9 |> filled (rgb 255 204 153) |> move(-50,-30) |> notifyTap (clickMsg (RGB 255 204 153))
          , square 9 |> filled (rgb 255 255 153) |> move(-40,-30) |> notifyTap (clickMsg (RGB 255 255 153))
          , square 9 |> filled (rgb 204 255 153) |> move(-30,-30) |> notifyTap (clickMsg (RGB 204 255 153))
          , square 9 |> filled (rgb 153 255 153) |> move(-20,-30) |> notifyTap (clickMsg (RGB 153 255 153))
          , square 9 |> filled (rgb 153 255 204) |> move(-10,-30) |> notifyTap (clickMsg (RGB 153 255 204))
          , square 9 |> filled (rgb 153 255 255) |> move(0,-30) |> notifyTap (clickMsg (RGB 153 255 255))
          , square 9 |> filled (rgb 153 204 255) |> move(10,-30) |> notifyTap (clickMsg (RGB 153 204 255))
          , square 9 |> filled (rgb 153 153 255) |> move(20,-30) |> notifyTap (clickMsg (RGB 153 153 255))
          , square 9 |> filled (rgb 204 153 255) |> move(30,-30) |> notifyTap (clickMsg (RGB 204 153 255))
          , square 9 |> filled (rgb 255 153 255) |> move(40,-30) |> notifyTap (clickMsg (RGB 255 153 255))
          , square 9 |> filled (rgb 255 153 204) |> move(50,-30) |> notifyTap (clickMsg (RGB 255 153 204))
          , square 9 |> filled (rgb 224 224 224) |> move(60,-30) |> notifyTap (clickMsg (RGB 224 224 224))
          , square 9 |> filled (rgb 255 204 204) |> move(-60,-40) |> notifyTap (clickMsg (RGB 255 204 204))
          , square 9 |> filled (rgb 255 229 204) |> move(-50,-40) |> notifyTap (clickMsg (RGB 255 229 204))
          , square 9 |> filled (rgb 255 255 204) |> move(-40,-40) |> notifyTap (clickMsg (RGB 255 255 204))
          , square 9 |> filled (rgb 229 255 204) |> move(-30,-40) |> notifyTap (clickMsg (RGB 229 255 204))
          , square 9 |> filled (rgb 204 255 204) |> move(-20,-40) |> notifyTap (clickMsg (RGB 204 255 204))
          , square 9 |> filled (rgb 204 255 229) |> move(-10,-40) |> notifyTap (clickMsg (RGB 204 255 229))
          , square 9 |> filled (rgb 204 255 255) |> move(0,-40) |> notifyTap (clickMsg (RGB 204 255 255))
          , square 9 |> filled (rgb 204 229 255) |> move(10,-40) |> notifyTap (clickMsg (RGB 204 229 255))
          , square 9 |> filled (rgb 204 204 255) |> move(20,-40) |> notifyTap (clickMsg (RGB 204 204 255))
          , square 9 |> filled (rgb 229 204 255) |> move(30,-40) |> notifyTap (clickMsg (RGB 229 204 255))
          , square 9 |> filled (rgb 255 204 255) |> move(40,-40) |> notifyTap (clickMsg (RGB 255 204 255))
          , square 9 |> filled (rgb 255 204 229) |> move(50,-40) |> notifyTap (clickMsg (RGB 255 204 229))
          , square 9 |> filled (rgb 255 255 255) |> move(60,-40) |> notifyTap (clickMsg (RGB 255 255 255))
          , rect 140 10 |> outlined (solid 1) black |> move (0,-55)
          --, text "Special colours" |> filled black |> move (0,-70)
          , square 9 |> filled (rgb 255 223 196) |> move(-65,-55) |> notifyTap (clickMsg (RGB 255 223 196))
          , square 9 |> filled (rgb 238 206 179) |> move(-55,-55) |> notifyTap (clickMsg (RGB 238 206 179))
          , square 9 |> filled (rgb 225 170 100) |> move(-45,-55) |> notifyTap (clickMsg (RGB 225 170 100))
          , square 9 |> filled (rgb 229 184 143) |> move(-35,-55) |> notifyTap (clickMsg (RGB 229 184 143))
          , square 9 |> filled (rgb 231 158 109) |> move(-25,-55) |> notifyTap (clickMsg (RGB 231 158 109))
          , square 9 |> filled (rgb 219 144 101) |> move(-15,-55) |> notifyTap (clickMsg (RGB 219 144 101))
          , square 9 |> filled (rgb 186 108 73) |> move(-5,-55) |> notifyTap (clickMsg (RGB 186 108 73))
          , square 9 |> filled (rgb 198 120 86) |> move(5,-55) |> notifyTap (clickMsg (RGB 198 120 86))
          , square 9 |> filled (rgb 130 72 24) |> move(15,-55) |> notifyTap (clickMsg (RGB 130 72 24))
          , square 9 |> filled (rgb 124 80 26) |> move(25,-55) |> notifyTap (clickMsg (RGB 124 80 26))
          , square 9 |> filled (rgb 126 77 28) |> move(35,-55) |> notifyTap (clickMsg (RGB 126 77 28))
          , square 9 |> filled (rgb 137 78 26) |> move(45,-55) |> notifyTap (clickMsg (RGB 137 78 26))
          , square 9 |> filled (rgb 88 53 18) |> move(55,-55) |> notifyTap (clickMsg (RGB 88 53 18))
          , square 9 |> filled (rgb 62 43 19) |> move(65,-55) |> notifyTap (clickMsg (RGB 62 43 19))

          ]

{-}
-- COLOURS
skin = skin9

skin1 = rgb 255 223 196

skin2 = rgb 238 206 179

skin3 = rgb 225 170 100

skin4 = rgb 229 184 143

skin5 = rgb 231 158 109

skin6 = rgb 219 144 101

skin7 = rgb 186 108 73

skin8 = rgb 198 120 86

skin9 = rgb 130 72 24

--skin10 = rgb 146 106 45 -- weird

--skin11 = rgb 135 97 39

skin12 = rgb 124 80 26

skin13 = rgb 126 77 28

skin14 = rgb 137 78 26

skin15 = rgb 88 53 18

skin16 = rgb 62 43 19-}