port module TEAStateDiagrams exposing (main, slides)

import Array
import AvatarCreator
import Browser.Events as BEvents
import ColourPalette exposing (RGB(..), drawRGB)
import ElmArchitecture
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Html as H
import Html.Attributes as HA
import Json.Decode as Decode
import RedPurpleBlue
import WebFramework exposing (webFrameworkSlides)
import DoubleDiamond exposing (..)
import Url as Url
import Browser
import Browser.Navigation exposing (Key(..))
import Json.Encode as E

port play : E.Value -> Cmd msg

port getAudioDuration : (E.Value -> msg) -> Sub msg

people =
    []


type Message
    = GameTick Float GetKeyState --The tick needs to have Float and GetKeyState which handles key presses.
    | NextSlide
    | LastSlide
    | RPBMsg RedPurpleBlue.Msg -- (1) include message from included module
    | AvatarMsg AvatarCreator.Msg
    | NoOp
    | MakeRequest Browser.UrlRequest
    | UrlChange Url.Url
    | GetNewAudioDuration (Maybe Float)
    | DDiamondMsg DoubleDiamond.Msg



-- this is the main function, and for simple animations, you would only replace the view function, or edit it below


main : AppWithTick () Model Message
main =
    appWithTick GameTick
        { init = \_ _ _ -> ( init, playSlideSound <| slide1 init)
        , view = \model -> { title = "The Elm Architecture and State Diagrams", body = view model }
        , update = update
        , subscriptions = \_ -> Sub.batch [BEvents.onKeyUp controlSlide, subAudioDuration]
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        }

subAudioDuration : Sub Message
subAudioDuration =
    let
        durationDecoder val =
            case Decode.decodeValue Decode.float val of
                Ok duration -> GetNewAudioDuration <| Just duration
                Err _ -> GetNewAudioDuration Nothing
    in
        getAudioDuration durationDecoder



controlSlide : Decode.Decoder Message
controlSlide =
    Decode.map
        (\key ->
            if key == "PageUp" then
                LastSlide

            else if key == "PageDown" then
                NextSlide
        
            else if key == "ArrowRight" then
                NextSlide
            
            else if key == "ArrowLeft" then
                LastSlide

            else
                NoOp
        )
        keyDecoder


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- MODEL


type alias Model =
    { t : Float
    , idx : Int
    , p : Bool
    , -- Pause
      r : Float
    , -- Rewind
      a : Float -- Acceleration
    , rpb : RedPurpleBlue.Model
    , audioDuration : Maybe Float
    , time : Float
    , slideStartTime : Float
    , ddiamond : DoubleDiamond.Model
    }

init : Model
init =
    { t = 0
    , idx = 0
    , p = False
    , -- Pause
      r = 1
    , -- Rewind
      a = 1 -- Acceleration
    , rpb = RedPurpleBlue.init
    , audioDuration = Nothing
    , time = 0
    , slideStartTime = 0
    , ddiamond = DoubleDiamond.init
    }



-- VIEW


view model =
    let
        t =
            model.t

        slide m =
            ((Maybe.withDefault default (Array.get model.idx slides)) m).shapes
    in
    collage 1000 500 (slide model ++ [audioBar model] ++ borders ++ navigators)

audioBar model =
    case model.audioDuration of
        Just dur ->
            let
                percentage = (model.time - model.slideStartTime)/dur
            in
            
            group 
                [
                    roundedRect 1000 10 5
                        |> filled gray
                ,   roundedRect (1000 * percentage) 10 5
                        |> filled green
                        |> makeTransparent 0.3
                ]
                |> move(0,-245)
        Nothing -> group []


-- UPDATE
playSlideSound : Slide Message -> Cmd Message
playSlideSound slide =
    case slide.audio of
        Just s -> play (E.string s)
        _ -> Cmd.none

update : Message -> Model -> ( Model, Cmd Message )
update message model =
    let
        _ = 
            case message of
                GameTick _ _ ->
                    message
                otherwise -> 
                    Debug.log "message" message 
    in
    case message of
        NextSlide ->
            let
                newSlideId = min (model.idx + 1) (Array.length slides - 1)
                newSlide =
                    case Array.get newSlideId slides of
                        Just slide -> slide model
                        _ -> default model
            in
            
            ({ model
                | t = 0
                , idx = newSlideId
                , slideStartTime = model.time
                , audioDuration = Nothing
            }, Debug.log "next slide cmd" playSlideSound newSlide )

        LastSlide ->
            let
                newSlideId = max (model.idx - 1) 0
                newSlide =
                    case Array.get newSlideId slides of
                        Just slide -> slide model
                        _ -> default model
            in
               ({ model
                        | t = 0
                        , idx = newSlideId
                        , slideStartTime = model.time
                        , audioDuration = Nothing
                 } , Debug.log "last slide cmd" playSlideSound newSlide )

        otherwise ->
            ( 
            case message of
                GameTick tick ( getKeyState, changeP1, changeP2 ) ->
                        { model
                            | t = max (model.t + 2.5 * model.a * model.r) 0
                            , time = tick
                        }

                NextSlide ->
                    { model
                        | t = 0
                        , idx = min (model.idx + 1) (Array.length slides - 1)
                    }

                LastSlide ->
                    { model
                        | t = 0
                        , idx = max (model.idx - 1) 0
                    }

                RPBMsg msg ->
                    { model | rpb = RedPurpleBlue.update msg model.rpb }
                    
                DDiamondMsg msg ->
                    { model | ddiamond = DoubleDiamond.update msg model.ddiamond }

                AvatarMsg msg ->
                    model

                NoOp ->
                     model

                MakeRequest _ -> model

                UrlChange _ -> model

                GetNewAudioDuration mDur ->
                    { model | audioDuration = mDur }
            , Cmd.none
            )



--- MISCELLANEOUS   


heritageMaroon =
    rgb 122 0 60


heritageGold =
    rgb 253 191 87


heritageGrey =
    rgb 94 106 113


default _ =
    { shapes = [], audio = Nothing }


borders =
    [ rect 5000 5000
        |> filled white
        |> move ( 3000, 0 )
    , rect 5000 5000
        |> filled white
        |> move ( -3000, 0 )
    , rect 5000 5000
        |> filled white
        |> move ( 0, 2750 )
    , rect 5000 5000
        |> filled white
        |> move ( 0, -2750 )
    ]


navigators =
    [ group
        [ circle 40
            |> filled gray
        , triangle 30
            |> filled white
        ]
        |> move ( 450, -200 )
        |> makeTransparent 0.5
        |> notifyTap NextSlide
    , group
        [ circle 40
            |> filled gray
        , triangle 30
            |> filled white
        ]
        |> rotate (degrees 180)
        |> move ( -450, -200 )
        |> makeTransparent 0.5
        |> notifyTap LastSlide
    ]


renderAvatar : AvatarCreator.Model -> Shape Message
renderAvatar avatarModel =
    GraphicSVG.map AvatarMsg <| AvatarCreator.theFace avatarModel


makeBullets : Float -> List ( Int, String ) -> Float -> List (Shape msg)
makeBullets t l start =
    case l of
        ( indt, x ) :: xs ->
            (group
                [ text x
                    |> size 20
                    |> customFont "Helvetica"
                    |> filled black
                    |> move ( -200, start )
                    |> fadeIn t 100
                , circle 5
                    |> filled black
                    |> move ( -220, start + 5 )
                    |> fadeIn t 100
                ]
                |> move ( 30 * toFloat indt, 0 )
            )
                :: makeBullets (t - 100) xs (start - 35)

        _ ->
            []



-- FUNCTIONS
--<< So why do I see (t - 100) or whatever value so often? >>
--   Whenever I do that, I'm basically delaying what I want to happen
--   by that value. Is it measure in seconds, frames or what? What's the unit here?
--   To be honest, I don't know. It has a lot to do with the UPDATE function, and
--   what value for 'x' you are using for " t = model.t + x ".


disappear x n =
    if x > n then
        makeTransparent 0

    else
        makeTransparent 1



-- Makes things vanish off the screen!


loop t n =
    let
        y =
            toFloat (floor (t / n))

        -- This function is how I make things loop!
    in
    t - y * n


appear x n =
    if x > n then
        makeTransparent 1

    else
        makeTransparent 0



-- Makes things suddenly appear on the screen!


fadeIn t n =
    makeTransparent (tranSin (t - n) 1)


fadeOut t n =
    makeTransparent (1 - tranSin (t - n) 1)


trans t y =
    if
        t < 0
        -- Used for all permanent transitions (fading out, color change, etc.) LINEAR.
    then
        0

    else
        Basics.min t y


tranSin t y =
    if
        t < 0
        -- Used for all permanent transitions (fading out, color change, etc.) Uses sin.
    then
        0

    else if t / 100 > pi / 2 then
        y

    else
        sin (t / 100) * y


drawLine t ( x1, y1 ) ( x2, y2 ) =
    line ( x1, y1 ) ( x1 + tranSin t (x2 - x1), y1 + tranSin t (y2 - y1) )



-- Down here is where you will find the slides!
-- To add more slides, simply add them to the list below.


fm : { a | t : Float } -> Float
fm m =
    m.t

scaleTEA : Slide msg -> Slide msg 
scaleTEA rec = {rec | shapes = [ move ( 0, -50 ) <| scale 3 <| group rec.shapes, outreachLogo, title "The Elm Architecture" ] }
 
type alias Slide msg = 
    { shapes : List (Shape msg)
    ,   audio : Maybe String
    }


--slides : Array.Array ({ a | t : Float } -> Slide Message)
slides =
    Array.fromList <|
        [ slide1
        , slide2 << fm  
       , scaleTEA << ElmArchitecture.slide1 << fm 
         , scaleTEA << ElmArchitecture.slide2 << fm 
         , scaleTEA << ElmArchitecture.slide3 << fm
         , scaleTEA << ElmArchitecture.slide4 << fm
         , scaleTEA << ElmArchitecture.slide5 << fm
         , scaleTEA << ElmArchitecture.slide6 << fm
         , scaleTEA << ElmArchitecture.slide7 << fm
         , scaleTEA << ElmArchitecture.slide8 << fm
         , scaleTEA << ElmArchitecture.slide9 << fm
         , scaleTEA << ElmArchitecture.slide10 << fm
         , scaleTEA << ElmArchitecture.slide11 << fm
         , scaleTEA << ElmArchitecture.slide12 << fm
         , slidePC0
         , slidePC1        
        , slidePC2
        , slidePC3
        , slidePC4
        , slidePC5
        , slidePC0
        , slidePC6
        , slidePC6a
        , slidePC6b 
        , slideSolid
        , slideLiquid
        , slideGas
       ]

title t =
    text t
        |> size 40
        |> bold
        |> customFont "Helvetica"
        |> centered
        |> filled heritageMaroon
        |> move ( 0, 200 )




--<< EVERYTHING FOR FIRST PAGE  >>-


outreachLogo =
    group
        [ GraphicSVG.html 100 100 (H.img [ HA.style "width" "100%", HA.src "images/Logo.png" ] [])
            |> move ( -500, 250 )
            |> addHyperlink "http://outreach.mcmaster.ca"
        , text "I4CS 2020"
            |> size 12
            |> customFont "Arial"
            |> filled black
            |> move ( -485, 170 )
        ]

insertPicture url =
    group
        [ GraphicSVG.html 100 100 (H.img [ HA.style "width" "100%", HA.src url ] [])
            |> addHyperlink "http://outreach.mcmaster.ca"
        ]

subtitle t =
    text t
        |> size 40
        |> bold
        |> customFont "Helvetica"
        |> centered
        |> filled green


slide0 model =
    { shapes = [], audio = Nothing }


arm shirtCol skinCol =
    group
        [ roundedRect 2 5 1
            -- left arm
            |> filled skinCol
            |> rotate (degrees 120)
            |> move ( -8, -4 )
        , roundedRect 3 5 1
            -- left sleeve
            |> filled shirtCol
            |> rotate (degrees 120)
            |> move ( -5, -2 )
        ]


body shirtCol skinCol t =
    group
        [ roundedRect 3 8 1
            -- left leg
            |> filled blue
            |> move ( -2, -11 )
        , roundedRect 3 8 1
            -- right leg
            |> filled blue
            |> move ( 2, -11 )
        , roundedRect 2 5 1
            -- neck
            |> filled skinCol
        , roundedRect 8 10 2
            --body
            |> filled shirtCol
            |> move ( 0, -4 )
            |> addHyperlink "http://outreach.mcmaster.ca"
        , GraphicSVG.html 28 42 (H.img [ HA.style "width" "100%", HA.src "images/Shirt.png" ] [])
            |> scale 0.2
            |> move ( -3, 0 )
            |> addHyperlink "http://outreach.mcmaster.ca"
        , wedge 1.5 0.5
            -- right shoe
            |> filled lightBrown
            |> rotate (degrees 90)
            |> move ( 2, -15 )
        , wedge 1.5 0.5
            -- left shoe
            |> filled lightBrown
            |> rotate (degrees 90)
            |> move ( -2, -15 )
        ]



slide1 model =
    let
        authorsColour =
            black

        -- Chris Schankula avatar
        char1 =
            group
                [ arm (rgb 255 0 0) (drawRGB char1R.skinColour)
                    -- left arm
                    |> rotate
                        (if model.t < 320 then
                            degrees (20 * sin (model.t * 0.1) - 20)

                         else
                            degrees 20 - 0.5
                        )
                    |> move ( 0, -1 )
                , arm (rgb 255 0 0) (drawRGB char1R.skinColour)
                    -- right arm
                    |> mirrorX
                    |> rotate (degrees -20)
                    |> move ( 0, 2 )
                , body (rgb 255 0 0) (drawRGB char1R.skinColour) model.t
                , renderAvatar char1R |> scale 0.2 |> move ( 0, 6 )
                ]

        -- Nhan avatar
        char2 =
            group
                [ arm yellow (drawRGB char2R.skinColour)
                    -- left arm
                    |> rotate
                        (if model.t < 320 then
                            degrees (20 * sin (model.t * 0.1) - 20)

                         else
                            degrees 20 - 0.5
                        )
                    |> move ( 0, -1 )
                , arm yellow (drawRGB char2R.skinColour)
                    -- right arm
                    |> mirrorX
                    |> rotate (degrees -20)
                    |> move ( 0, 2 )
                , body yellow (drawRGB char2R.skinColour) model.t
                , renderAvatar char2R |> scale 0.2 |> move ( 0, 6 )
                ]

        -- Yumna Irfan avatar
        char3 =
            group
                [ arm green (drawRGB char3R.skinColour)
                    -- left arm
                    |> rotate (degrees -70)
                , arm green (drawRGB char3R.skinColour)
                    -- right arm
                    |> mirrorX
                    |> rotate (degrees -20)
                    |> move ( 0, 2 )
                , body green (drawRGB char3R.skinColour) model.t
                    |> mirrorX
                , renderAvatar char3R |> scale 0.2 |> move ( 0, 6 )
                ]

        -- Tanya avatar
        char4 =
            group
                [ arm orange (drawRGB char5R.skinColour)
                    --left arm
                    |> rotate (degrees -68)
                    |> move ( 0, -2 )
                , arm orange (drawRGB char5R.skinColour)
                    -- right arm
                    |> mirrorX
                    |> rotate (degrees -20)
                    |> move ( 0, 2 )
                , body orange (drawRGB char5R.skinColour) model.t
                , renderAvatar char5R |> scale 0.2 |> move ( 0, 6 )
                ] |> move(6,0)

        heading =
            group
                [ text "The Elm Architecture and State Diagrams"
                    |> size 6.5
                    |> bold
                    |> customFont "Helvetica"
                    |> centered
                    |> filled heritageMaroon
                ]

        authors =
            group
                [ text "Christopher W. Schankula"
                    |> size 4
                    |> bold
                    |> customFont "Helvetica"
                    |> centered
                    |> filled authorsColour
                    |> move ( -35, -26 )
                , text ", Lucas Dutton,"
                    |> size 4
                    |> bold
                    |> customFont "Helvetica"
                    |> centered
                    |> filled authorsColour
                    |> move ( 4.5, -26 )
                , text " Nhan Q. D. Thai"
                    |> size 4
                    |> bold
                    |> customFont "Helvetica"
                    |> centered
                    |> filled authorsColour
                    |> move ( 35, -26 )
                , text "Yumna Irfan, Padma Pasupathi,"
                    |> size 4
                    |> bold
                    |> customFont "Helvetica"
                    |> centered
                    |> filled authorsColour
                    |> move ( 5, -32 )
                , text "Chinmay Sheth, Christopher Kumar Anand"
                    |> size 4
                    |> bold
                    |> customFont "Helvetica"
                    |> centered
                    |> filled authorsColour
                    |> move ( 5, -38 )
                ]
    in
    { shapes = 
        [ group
            [ group
                [ GraphicSVG.html 40 40 (H.img [ HA.style "width" "100%", HA.src "images/Logo.png" ] [])
                    |> scale 0.6
                    |> move ( -12, 2 )
                    |> fadeIn model.t 150
                    |> addHyperlink "http://outreach.mcmaster.ca"
                , group
                    [ text "Software:"
                        |> size 7
                        |> bold
                        |> customFont "Helvetica"
                        |> centered
                        |> filled heritageMaroon
                        |> addHyperlink "http://outreach.mcmaster.ca"
                        |> move ( 0, -25 )
                    , text "Tool For Change"
                        |> size 4
                        |> bold
                        |> customFont "Helvetica"
                        |> centered
                        |> filled heritageGold
                        |> addOutline (solid 0.125) heritageGrey
                        |> addHyperlink "http://outreach.mcmaster.ca"
                        |> move ( 0, -30 )
                    ]
                    |> scale 0.85
                ]
                |> move ( -31, -20 )
                , GraphicSVG.html 40 40 (H.img [ HA.style "width" "100%", HA.src "images/MacEng.jpg" ] [])
                    |> move ( 20, -22 )
                    |> fadeIn model.t 150
                    |> addHyperlink "https://www.eng.mcmaster.ca/"
            , group
                [ group [ char1 |> move ( -10, 0 ), char2 |> move ( 10, 0 ) ]
                    |> move
                        ( if model.t < 400 then
                            0

                        else
                            clamp 0 80 (model.t - 400)
                        , if model.t < 400 then
                            0

                        else
                            clamp -30 0 (-1 * model.t + 400)
                        )
                , rect 100 40
                    |> filled white
                    |> fadeOut model.t 20
                , heading
                    |> move
                        ( if model.t < 200 then
                            -400

                        else
                            clamp -400 0 (-210 + (model.t - 200) * 0.3)
                        , 8
                        )
                , authors
                    |> move
                        ( if model.t < 200 then
                            400

                        else
                            clamp -6 400 (210 - (model.t - 200) * 0.3)
                        , 16
                        )
                , char4
                    |> scale 0.7
                    |> move
                        ( if model.t < 200 then
                            -400

                        else
                            clamp -400 90 (-128 + (model.t - 200) * 0.3)
                        , 4
                        )
                , char3
                    |> mirrorX
                    |> scale 0.7
                    |> move
                        ( if model.t < 200 then
                            400

                        else
                            clamp -90 120 (150 - (model.t - 200) * 0.3)
                        , -21
                        )
                ]
                |> move ( 0, 25 )
            ]
            |> scale 5
        ]
    , audio = Nothing
    }


decoratingPartPC6a =
    let
        customRed =
            rgb 255 0 0

        customBlue =
            rgb 0 0 255

        customPurple =
            rgb 255 0 255
    in
   
      group  [ circle 32 |> filled customRed |> move ( -240, 80 )
        , rect 40 20 |> filled customRed |> move ( 150, 105 )
        , circle 32 |> filled customPurple |> move ( -360, -80 )
        , rect 64 20 |> filled customPurple |> move ( 221, 105 )
        , circle 32 |> filled customBlue |> move ( -120, -80 )
        , rect 42 20 |> filled customBlue |> move ( 298, 105 )
        , rectangle 340 45 |> filled customPurple |> move ( 0, 157 )
        ]
slideDesignThinking: { a | ddiamond : DoubleDiamond.Model , t : Float } -> Slide Message
slideDesignThinking model = {shapes = List.map (GraphicSVG.map DDiamondMsg) 
                                        [ title "Design Thinking"
                                        , group (DoubleDiamond.myShapes model.ddiamond) |> scale 3.7
                                        ]
                            , audio = Nothing}


slidePC0 model =
             {shapes =  List.map (GraphicSVG.map RPBMsg) <| [ title "Reasoning About the Model", group [ roundedRect 100 20 5 |> filled red, text "Click Me!" |> centered |> filled black |> move ( 0, -5 ) ] |> move ( 0, -220 ) ] ++ RedPurpleBlue.diagram "" (Just RedPurpleBlue.Red) Nothing ++ [ outreachLogo ]
            , audio = Nothing
             }

slidePC1 =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "Reasoning About the Model" ] ++ RedPurpleBlue.diagram "" Nothing (Just ( RedPurpleBlue.Red, "ChangeColour" )) ++ [ outreachLogo ]
                , audio = Nothing 
              }

slidePC2 =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "Reasoning About the Model", group [ roundedRect 100 20 5 |> filled (rgb 200 0 200), text "Click Me!" |> centered |> filled black |> move ( 0, -5 ) ] |> move ( 0, -220 ) ] ++ RedPurpleBlue.diagram "" (Just RedPurpleBlue.Purple) Nothing ++ [ outreachLogo ]
                , audio = Nothing
              }

slidePC3 =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "Reasoning About the Model" ] ++ RedPurpleBlue.diagram "" Nothing (Just ( RedPurpleBlue.Purple, "ChangeColour" )) ++ [ outreachLogo ]
                , audio = Nothing 
              }

slidePC4 =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "Reasoning About the Model", group [ roundedRect 100 20 5 |> filled (rgb 0 0 255), text "Click Me!" |> centered |> filled black |> move ( 0, -5 ) ] |> move ( 0, -220 ) ] ++ RedPurpleBlue.diagram "" (Just RedPurpleBlue.Blue) Nothing ++ [ outreachLogo ]
                , audio = Nothing 
              }

slidePC5 =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "Reasoning About the Model" ] ++ RedPurpleBlue.diagram "" Nothing (Just ( RedPurpleBlue.Blue, "ChangeColour" )) ++ [ outreachLogo ]
                , audio = Nothing 
              }

slidePC6 =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "State Diagram → Elm" ] ++ [ scale 0.8 <| move ( -300, 0 ) <| group <| RedPurpleBlue.diagram "" Nothing Nothing, group (rpbTxt model) |> move ( 225, 50 ) ] ++ [ outreachLogo ]
                , audio = Nothing
              }

slidePC6a =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "State Diagram → Elm", decoratingPartPC6a, pcUnder "states → constructors" ] ++ [ scale 0.8 <| move ( -300, 0 ) <| group <| RedPurpleBlue.diagram "" Nothing Nothing, group (rpbTxt model) |> move ( 225, 50 ) ] ++ [ outreachLogo ]
                , audio = Nothing 
              }

slidePC6b =
    \model -> {shapes = List.map (GraphicSVG.map RPBMsg) <| [ title "State Diagram → Elm", rectangle 420 45 |> filled heritageGold |> move ( 0, 157 ), pcUnder "transitions → update functions", rectangle 340 45 |> filled heritageGold |> move ( 214, -72 ), rectangle 125 25 |> filled heritageGold |> move ( 174, 67 ), rectangle 100 25 |> filled heritageGold |> move ( -120, 43 ), rectangle 100 25 |> filled heritageGold |> move ( -360, 43 ), rectangle 100 25 |> filled heritageGold |> move ( -240, -117 ) ] ++ [ scale 0.8 <| move ( -300, 0 ) <| group <| RedPurpleBlue.diagram "" Nothing Nothing, group (rpbTxt model) |> move ( 225, 50 ) ] ++ [ outreachLogo ]
                , audio = Nothing 
              }

pcUnder txt =
    text txt
        |> size 30
        |> italic
        |> customFont "Helvetica"
        |> centered
        |> filled black
        |> move ( 0, 150 )


user1State t f =
    let
        frac =
            0.002 * t - toFloat (floor (0.002 * t))
    in
    if 6 * frac < 1 then
        f (Just RedPurpleBlue.Red) Nothing

    else if 6 * frac < 2 then
        f Nothing (Just ( RedPurpleBlue.Red, "ChangeColour" ))

    else if 6 * frac < 3 then
        f (Just RedPurpleBlue.Purple) Nothing

    else if 6 * frac < 4 then
        f Nothing (Just ( RedPurpleBlue.Purple, "ChangeColour" ))

    else if 6 * frac < 5 then
        f (Just RedPurpleBlue.Blue) Nothing

    else
        f Nothing (Just ( RedPurpleBlue.Blue, "ChangeColour" ))


user1State1 t f =
    let
        frac =
            0.002 * t - toFloat (floor (0.002 * t))
    in
    if 6 * frac < 1 then
        f (Just RedPurpleBlue.Blue) Nothing

    else if 6 * frac < 2 then
        f Nothing (Just ( RedPurpleBlue.Blue, "ChangeColour" ))

    else if 6 * frac < 3 then
        f (Just RedPurpleBlue.Red) Nothing

    else if 6 * frac < 4 then
        f Nothing (Just ( RedPurpleBlue.Red, "ChangeColour" ))

    else if 6 * frac < 5 then
        f (Just RedPurpleBlue.Purple) Nothing

    else
        f Nothing (Just ( RedPurpleBlue.Purple, "ChangeColour" ))


user1State2 t f =
    let
        frac =
            0.002 * t - toFloat (floor (0.002 * t))
    in
    if 6 * frac < 1 then
        f (Just RedPurpleBlue.Purple) Nothing

    else if 6 * frac < 2 then
        f Nothing (Just ( RedPurpleBlue.Purple, "ChangeColour" ))

    else if 6 * frac < 3 then
        f (Just RedPurpleBlue.Blue) Nothing

    else if 6 * frac < 4 then
        f Nothing (Just ( RedPurpleBlue.Blue, "ChangeColour" ))

    else if 6 * frac < 5 then
        f (Just RedPurpleBlue.Red) Nothing

    else
        f Nothing (Just ( RedPurpleBlue.Red, "ChangeColour" ))

codeMarkdown codeString co =
    let
        code c ( txt, ln ) =
            text txt
                |> size 8
                |> bold
                |> customFont "Helvetica"
                |> fixedwidth
                |> filled c
                |> move ( 0, -9 * ln )

        lns =
            String.lines codeString

        positions : List String -> Float -> List ( String, Float )
        positions lines i =
            case lines of
                l :: ls ->
                    ( l, i ) :: positions ls (i + 1)

                [] ->
                    []
    in
    group (List.map (code co) (positions lns 0))


rpbTxt model =
    [ group
        [ codeMarkdown """     Model = Red   Purple   Blue

     Msg = ChangeColour

view   Model    Html Msg

update   Msg    Model      Model   Cmd Msg
update msg model
       msg
    ChangeColour
        updateColour model   Cmd.none

updateColour   Model    Model
updateColour model =
       model
    Red    Purple
    Purple    Blue
    Blue    Red
""" black
            |> scale 2
            |> move ( -215, 50 )
        , codeMarkdown """type             |        |

type

     :       ->

       :     ->       -> (       ,         )
                 =
  case     of
                 ->
      (                    ,          )

             :       ->

  case       of
        ->
           ->
         ->
""" pink
            |> scale 2
            |> move ( -215, 50 )
        ]
    ]


slide2 model =
    {shapes = [ title "The Elm Language"
        |> move ( -5, -30 )
    , group
        (makeBullets model
            [ ( 0, "Functional language" )
            , ( 0, "Compact syntax" )
            , ( 0, "Pure functions" )
            , ( 0, "Composable functions" )
            , ( 0, "Strongly typed" )
            , ( 1, "Detailed compiler errors for refactoring" )
            , ( 0, "Compiles into JavaScript" )
            ]
            0
        )
        |> move ( -80, 115 )
    , outreachLogo
    , html 200 200 (H.img [ HA.src "images/Elm.png", HA.style "width" "100%" ] [])
        |> move ( 170, 70 )
    ]
    , audio = Nothing
    }



slideModelDrivenDevelopment model =
    {shapes = [ title "Model Driven Development"
        |> move ( -5, -30 )
    , group
        (makeBullets model.t
            [ ( 0, "Software development approach" )
            , ( 0, "For large-scale software development projects" )
            , ( 0, "Higher level of abstraction" )
            , ( 1, "Accelerates development" )
            , ( 1, "Increases software quality" )
            , ( 0, "Low code" )
            , ( 1, "because code is automatically (re)generated" )
            ]
            0
        )
        |> move ( -140, 90 )
    , insertPicture "images/FoodGameStateDiagram.png"
        |> scale 4.2
        |> move (60,120)
    , outreachLogo
    ]
    , audio = Nothing
    }
    
slidePALDraw model =
    {shapes = [ title "PALDraw"
        |> scale 0.9
        |> move ( -310, -175 )
    , group
        (makeBullets model.t
            [ ( 0, "PALDraw is a visual specification tool" )
            ]
            0
        )
        |> scale 0.75
        |> move ( -280, -20 )
    , insertPicture "images/Combined.png"
        |> scale 6
        |> move (-180,200)
    , outreachLogo
    ]
    , audio = Nothing
    }

solidState model =
    let
        ( vx, vy ) =
            ( 0.2, 0.7 )

        headBack =
            group
                [ rect 80 15
                    |> filled lightBlue
                    |> makeTransparent 0.3
                , rect 80 15
                    |> outlined (solid 1) black
                ]

        molecule =
            group
                [ circle 2 |> filled (rgb 0 200 0) |> move ( 8, 0 )
                , circle 7 |> filled (rgb 0 0 250)
                , circle 2 |> filled (rgb 0 200 0) |> move ( 0, 8 )
                ]
    in
    [ molecule
        |> rotate (vx * (0.1 * model.t))
        |> move
            ( 60 * sin (vx * (0.01 * model.t))
            , 40 * sin (vy * (0.01 * model.t))
            )
    , text "Solid State"
        |> centered
        |> filled black
        |> move ( 0, 55 )
    , headBack
        |> move ( 0, 58 )
    ]


liquidState model =
    let
        ( vx, vy ) =
            ( pi, 2 )

        headBack =
            group
                [ rect 80 15
                    |> filled lightBlue
                    |> makeTransparent 0.3
                , rect 80 15
                    |> outlined (solid 1) black
                ]

        molecule =
            group
                [ circle 2 |> filled (rgb 0 200 0) |> move ( 8, 0 )
                , circle 7 |> filled (rgb 0 0 250)
                , circle 2 |> filled (rgb 0 200 0) |> move ( 0, 8 )
                ]
    in
    [ molecule
        |> rotate (vx * (0.02 * model.t))
        |> move
            ( 60 * sin (vx * (0.015 * model.t))
            , 40 * sin (vy * (0.015 * model.t))
            )
    , text "Liquid State"
        |> centered
        |> filled black
        |> move ( 0, 55 )
    , headBack
        |> move ( 0, 58 )
    ]


gasState model =
    let
        ( vx, vy ) =
            ( 57, 64 )

        headBack =
            group
                [ rect 80 15
                    |> filled lightBlue
                    |> makeTransparent 0.3
                , rect 80 15
                    |> outlined (solid 1) black
                ]

        molecule =
            group
                [ circle 2 |> filled (rgb 0 200 0) |> move ( 8, 0 )
                , circle 7 |> filled (rgb 0 0 250)
                , circle 2 |> filled (rgb 0 200 0) |> move ( 0, 8 )
                ]
    in
    [ molecule
        |> rotate (vx * (0.02 * model.t))
        |> move
            ( 60 * sin (vx * (0.019 * model.t))
            , 40 * sin (vy * (0.019 * model.t))
            )
    , text "Gaseous State"
        |> centered
        |> filled black
        |> move ( 0, 55 )
    , headBack
        |> move ( 0, 58 )
    ]



slideSolid model =
    {shapes = [ group (solidState model) |> scale 4 |> move ( 0, -50 ), outreachLogo ]
    , audio = Nothing 
    }


slideLiquid model =
    {shapes = [ group (liquidState model) |> scale 4 |> move ( 0, -50 ), outreachLogo ]
    , audio = Nothing 
    }

slideGas model =
    {shapes = [ group (gasState model) |> scale 4 |> move ( 0, -50 ), outreachLogo ]
    , audio = Nothing 
    }



--Chris


char1R =
    { time = 0
    , eyeColour = RGB 211 255 255
    , skinColour = RGB 255 223 196
    , headcoverColour = RGB 204 255 204
    , hairFront = 7
    , hairColour = RGB 255 153 51
    , highlightColour = RGB 204 102 0
    , eyeShape = 0
    , extra = 0
    , selected = AvatarCreator.SelectNone
    }



-- Dr Anand


char2R =
    { time = 0
    , eyeColour = RGB 129 73 31
    , skinColour = RGB 217 144 105
    , headcoverColour = RGB 204 255 204
    , hairFront = 16
    , hairColour = RGB 32 32 32
    , highlightColour = RGB 102 0 204
    , eyeShape = 3
    , extra = 0
    , selected = AvatarCreator.SelectNone
    }



-- Yumna


char3R =
    { time = 0
    , eyeColour = RGB 32 32 32
    , skinColour = RGB 219 144 101
    , headcoverColour = RGB 204 255 204
    , hairFront = 13
    , hairColour = RGB 250 240 190
    , highlightColour = RGB 102 0 204
    , eyeShape = 0
    , extra = 0
    , selected = AvatarCreator.SelectNone
    }



--Ankit


char4R =
    { time = 0
    , eyeColour = RGB 32 32 32
    , skinColour = RGB 219 144 101
    , headcoverColour = RGB 204 255 204
    , hairFront = 5
    , hairColour = RGB 0 0 0
    , highlightColour = RGB 102 0 204
    , eyeShape = 0
    , extra = 0
    , selected = AvatarCreator.SelectNone
    }



-- Tanya


char5R =
    { time = 0
    , eyeColour = RGB 47 75 75
    , skinColour = RGB 255 223 196
    , headcoverColour = RGB 204 255 204
    , hairFront = 9
    , hairColour = RGB 130 72 24
    , highlightColour = RGB 32 32 32
    , eyeShape = 3
    , extra = 0
    , selected = AvatarCreator.SelectNone
    }



-- Pedram


char6R =
    { time = 0
    , eyeColour = RGB 147 0 73
    , skinColour = RGB 255 204 204
    , headcoverColour = RGB 204 255 204
    , hairFront = 0
    , hairColour = RGB 32 32 32
    , highlightColour = RGB 32 32 32
    , eyeShape = 0
    , extra = 0
    , selected = AvatarCreator.SelectNone
    }



-- Chitwan


char7R =
    { time = 0
    , eyeColour = RGB 198 120 86
    , skinColour = RGB 229 184 143
    , headcoverColour = RGB 204 255 204
    , hairFront = 3
    , hairColour = RGB 62 43 19
    , highlightColour = RGB 32 32 32
    , eyeShape = 0
    , extra = 0
    , selected = AvatarCreator.SelectNone
    }



{-
To use slides with sound: elm make <moduleName>.elm --output=soundslides.js
- use elm reactor and go to soundslides.html
-}