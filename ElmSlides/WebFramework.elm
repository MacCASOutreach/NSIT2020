module WebFramework exposing (webFrameworkSlides)

import Array
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Html as H
import Html.Attributes as HA


type alias Slide msg =
    { shapes : List (Shape msg)
    ,   audio : Maybe String
    }


webFrameworkSlides : List (Float -> Slide msg)
webFrameworkSlides =
    let
        page1 =
            lineByLine
                [ ( 0, "Current Haskell web frameworks" )
                , ( 1, "• Yesod, Scotty, Happstack, etc" )
                , ( 1, "• Defining routes" )
                , ( 1, "• Parsing requests" )
                , ( 2, "• Query string parameters" )
                , ( 2, "• Parsing request" )
                , ( 1, "• Returning responses" )
                , ( 2, "• blaze-html, hamlet, lucid, etc. (write HTML in Monad)" )
                , ( 1, "• Serving JS" )
                , ( 2, "• jmacro, julius, etc (most use QuasiQuote)" )
                , ( 1, "• JSON" )
                , ( 2, "• aeson (FromJSON, ToJSON, type classes)" )
                ]
                |> List.map always

        page2 =
            lineByLine
                [ ( 0, "Prerequisites" )
                , ( 1, "• Current Haskell web frameworks" )
                , ( 2, "• Monad" )
                , ( 2, "• Template Haskell" )
                , ( 2, "• Encoder and Decoder" )
                , ( 2, "• Configurations (WebSocket, ...)" )
                , ( 1, "" )
                , ( 1, "• Petri App Land (PAL)" )
                , ( 2, "• Algebraic Data Types" )
                , ( 2, "• Functions" )
                , ( 2, "• Functions" )
                , ( 2, "• Functions" )
                ]
                |> List.map always
    in
        page1 ++ page2
    


outreachLogo =
    GraphicSVG.html 100 100 (H.img [ HA.style "width" "100%", HA.src "https://macoutreach.rocks/Logo.png" ] [])
        |> move ( -500, 250 )
        |> addHyperlink "http://outreach.mcmaster.ca"


withTitle : String -> List (Shape msg) -> Slide msg
withTitle t sh =
    {shapes = [ move ( 0, -50 ) <| scale 3 <| group sh, outreachLogo, title t ]
    , audio = Just "sounds/Slide2.mp3" }


title t =
    text t
        |> size 40
        |> bold
        |> customFont "Helvetica"
        |> centered
        |> filled blue
        |> move ( 0, 200 )


lines : List ( Float, String ) -> Shape msg
lines ls =
    let
        lineShape i ( n, str ) =
            line n str |> move ( 0, toFloat <| -10 * i )

        line n str =
            let
                sz =
                    9 - n * 2
            in
            group [ text str |> size sz |> bold |> customFont "Helvetica" |> filled black ]
                |> move ( n * 10, 0 )
    in
    group (List.indexedMap lineShape ls)


lineByLine : List ( Float, String ) -> List (Slide msg)
lineByLine ls =
    let
        --        makeSlide
        a =
            1
    in
    List.range 1 (List.length ls)
        |> List.map (\n -> List.take n ls)
        |> List.map makeSlide


makeSlide : List ( Float, String ) -> Slide msg
makeSlide ls =
    withTitle "Why Yet Another Web Framework?"
        [ lines ls
            |> move ( -130, 60 )
        ]
