module StateDiagrams exposing(viewStateDiagram, viewEquivalenceStateDiagram, twoDigitFloats)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List exposing(map,map2,foldr,concatMap)
import String exposing (length)
import Char
import Dict exposing (Dict)
import Array
import Tuple exposing (first,second)

-- make an Element showing the state diagram
viewEquivalenceStateDiagram : (a -> String) -> (a -> a) -> List (a,(Float,Float)) -> (List ( a -> a, String.String, List ( a, ( Float, Float ) ) )) -> Maybe a -> Maybe (a,String) -> Shape msg
viewEquivalenceStateDiagram toString representative states transitions activeState activeTransition
  = let
       dispStates = List.map (displayState toString) states
       dispTrans = concatMap displayTransition transitions
       locsSizes = Dict.fromList <| List.map (extractStatePosSize representative) dispStates
    in
       group (
              (List.concat <| List.map ((drawTransition toString) representative locsSizes activeTransition)
                                  dispTrans
              ) ++
              (List.map (drawWithActive activeState) dispStates)
             )

-- make an Element showing the state diagram
viewStateDiagram : (a -> String) -> List (a,(Float,Float)) -> (List ( a -> a, String.String, List ( a, ( Float, Float ) ) )) -> Maybe a -> Maybe (a,String) -> Shape msg
viewStateDiagram toString states transitions activeState activeTransition
  = viewEquivalenceStateDiagram toString (\ x -> x ) states transitions activeState activeTransition

-- apply transition function to initial states for display
displayTransition :  (a -> a, String, List (a, (Float, Float))) -> List ((a, a), (Float, Float), String)
displayTransition (tfun,label,initialStateAndPoss) =
  let mkT (state,pos) = ((state,tfun state),pos,label)
  in List.map mkT initialStateAndPoss

type alias ViewNode a = { node : a
                        , toString : a -> String
                        , position : {x : Float, y : Float}
                        , size : {x : Float, y : Float}
                        }

xy : {x : Float, y : Float} -> (Float, Float)
xy record = (record.x,record.y)

toXY : (Float, Float) -> {x : Float, y : Float}
toXY (x,y) = {x = x, y = y}

mkSize : (Int, Int) -> {x : Float, y : Float}
mkSize (x,y) = {x = 10 + (toFloat x), y = toFloat y}

displayState : (a -> String) -> (a, (Float, Float)) -> ViewNode a
displayState toString (state,pos) = {node = state, toString = toString, position = toXY pos, size = mkSize (sizeOf (strWMax <| toString state))}

strWMax a = String.left 40 <| String.fromList <| twoDigitFloats <| String.toList a

twoDigitFloats cs = case cs of
                      c :: more -> c :: if Char.isDigit c
                                          then haveDigit more
                                          else twoDigitFloats more
                      []        -> []

haveDigit cs = case cs of
                 '.' :: more -> '.' :: havePoint more
                 c :: more   -> if Char.isDigit c
                                  then c :: haveDigit more
                                  else c :: twoDigitFloats more
                 []          -> []

havePoint cs = case cs of
                 c :: more -> if Char.isDigit c
                                then c :: havePointDigit more
                                else c :: twoDigitFloats more
                 []        -> []

havePointDigit cs = case cs of
                      c :: more -> if Char.isDigit c
                                     then c :: discardDigits more
                                     else c :: twoDigitFloats more
                      []        -> []

discardDigits cs = case cs of
                     c :: more -> if Char.isDigit c
                                    then discardDigits more
                                    else c :: twoDigitFloats more
                     []        -> []

extractStatePosSize : ( a -> a ) -> ViewNode a -> (String, ((Float, Float), (Float, Float)))
extractStatePosSize rep viewNode = (viewNode.toString <| rep viewNode.node, (xy viewNode.position,xy viewNode.size))

stateRadius = 20

drawWithActive : Maybe a -> ViewNode a -> Shape s
drawWithActive activeState viewNode
  = group ((if activeState == Just viewNode.node
               then [outlined (solid 2.0)
                              (highlight)
                              --(roundedRect viewNode.size.x viewNode.size.y nodeRadius)
                              (circle stateRadius)
                    ]
               else []
              )
              ++ [ outlined (solid 0.5)
                            (black)
                            --(roundedRect viewNode.size.x viewNode.size.y nodeRadius)
                            (circle stateRadius)
                 , move (0, -4.0) (monospace False {-(activeState == Just viewNode.node)-} (strWMax <| viewNode.toString viewNode.node) 40 )
                 ]
          )
      |> move (xy viewNode.position)

nodeRadius = 6

drawTransition : (a -> String) -> ( a -> a ) ->  Dict String ((Float, Float), (Float, Float)) -> Maybe (a, String)  -> ((a, a), (Float, Float), String)  -> List (Shape s)
drawTransition toString representative locsSizes activeT ((pre,post),pos,name)=
  let preS = toString <| representative pre
      postS = toString <| representative post
      prePS = Dict.get preS locsSizes
      postPS = Dict.get postS locsSizes
      textWidth = hOffset + halfWidthChar * (toFloat <| String.length name )
      isActive = activeT == Just (pre,name)
  in case (prePS,postPS) of
       (Just (prePos,preSize), Just (postPos,postSize)) ->
           [ if preS == postS
               then selfLoop prePos pos preSize isActive
               else parabola3 prePos pos postPos preSize postSize isActive
           , filled (rgba 255 255 255 0.8)
                    (rect textWidth 12)
                    |> move pos
                    |> move(0,2)
           , move pos (monospace isActive name 60)
           ]
       otherwise    -> []

normalize : (Float, Float) -> (Float, Float)
normalize (x,y) = let r = sqrt (x*x + y*y) in (x/r,y/r)

rotateTo : (Float, Float) -> (Float, Float) -> (Float, Float)
rotateTo (x,y) (u,v) = (x*1 + y*1, x*1 - y*1)

subV (x,y) (u,v) = (x-u,y-v)
addV (x,y) (u,v) = (x+u,y+v)
perp : (Float, Float) -> (Float, Float)
perp (x,y) = (-y,x)
scaleV s (x,y) = (s*x,s*y)
dot : (Float, Float) -> (Float, Float) -> Float
dot (x,y) (u,v) = x*u + y*v
absdot : (Float, Float) -> (Float, Float) -> Float
absdot (x,y) (u,v) = abs(x*u) + abs(y*v)

selfLoop : (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool -> Shape s
selfLoop (x0,y0) (x1,y1) sz0 isActive =
    let
        inter = interpxy startPt (x1,y1) endPt
        inner = List.map (inter << toFloat) <| List.range 2 18
        tangent = perp <| subV (x1,y1) (x0,y0)  
        (xs,ys) = normalize <| subV (subV (x1,y1) tangent) (x0,y0)
        (xe,ye) = normalize <| subV (addV (x1,y1) tangent) (x0,y0)
        endDir = normalize <| subV (inter 18) endPt
        endPt = addV (0.4 * xe * first sz0,0.4 * ye * second sz0) (x0,y0)
        t1 = addV (addV endPt (scaleV 10 endDir)) (scaleV 4 (perp endDir))
        t2 = subV (addV endPt (scaleV 10 endDir)) (scaleV 4 (perp endDir))
        startPt = addV (0.4 * xs * first sz0,0.4 * ys * second sz0) (x0,y0)
        disp = scaleV 1 ( subV endPt startPt )
        arc = curve startPt [Pull (subV (x1,y1) disp) (x1,y1), Pull (addV (x1,y1) disp) endPt]
        testPts = List.map (\ t -> let pt = (first sz0 * cos t, second sz0 * sin t)
                                       len = 0.5 * (absdot pt sz0)
                                   in scaleV 0.5 pt              ) <| List.map toFloat <| List.range 0 16
    in group <| (if isActive
                   then [ outlined (solid 2)
                                   (highlight)
                                   arc
                        , filled  (highlight)
                                  (polygon [endPt,t1,t2])
                        ]
                   else []
                ) ++ [ outlined (solid 0.5)
                                (black)
                                arc
                     , filled (black)
                              (polygon [endPt,t1,t2])
                              |> move(-(first endPt), -(second endPt))
                              |> rotate(degrees 39)
                              |> move(first endPt+0.75, second endPt - 1)

                ]

parabola3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool -> Shape s
parabola3 (x0,y0) (x1,y1) (x2,y2) sz0 sz2 isActive
  = let
        inter = interpxy startPt (x1,y1) endPt
        (xe,ye) = normalize <| subV (x1,y1) (x2,y2)
        endDir = normalize <| subV (inter 18) endPt
        endPt = addV (0.5 * xe * 2 * stateRadius,0.5 * ye * 2 * stateRadius) (x2,y2)
        t1 = addV (addV endPt (scaleV 10 endDir)) (scaleV 4 (perp endDir))
        t2 = subV (addV endPt (scaleV 10 endDir)) (scaleV 4 (perp endDir))
        t1active = addV (addV endPt (scaleV 12 endDir)) (scaleV 6 (perp endDir))
        t2active = subV (addV endPt (scaleV 12 endDir)) (scaleV 6 (perp endDir))
        (xs,ys) = normalize <| subV (x1,y1) (x0,y0)
        --startPt = addV (0.5 * xs * first sz0,0.5 * ys * second sz0) (x0,y0)
        startPt = addV (0.5 * xs * 2 * stateRadius,0.5 * ys * 2 * stateRadius) (x0,y0)
        -- half the vector between start and end
        disp = scaleV 0.25 ( subV endPt startPt )
        arc = curve startPt [Pull (subV (x1,y1) disp) (x1,y1), Pull (addV (x1,y1) disp) endPt]
        testPts = List.map (\ t -> let pt = (first sz0 * cos t, second sz0 * sin t)
                                       len = 0.5 * (absdot pt sz0)
                                   in scaleV 0.5 pt              ) <| List.map toFloat <| List.range 0 16

    in group <| (if isActive
                   then [ outlined (solid 2)
                                   (highlight)
                                   (arc)
                        , filled (highlight)
                                 (polygon [subV endPt (scaleV 4 endDir),t1active,t2active])
                        ]
                   else []
                ) ++ [ outlined (solid 0.5)
                                (black)
                                (arc)
                     , filled (black)
                              (polygon [endPt,t1,t2])
                ]


--a*t^2 + b*t + c = x
--c = x0
--a + b + c = x1
--4*a + 2*b + c = x2
--a + b = x1 - x0
--4*a + 2*b = x2 - x0
--a = 0.5 * (x2 - x0 - 2*(x1-x0)) = 0.5 * (x2 - 2*x1 + x0)
--b = 0.5 * (-(x2 - x0) + 4*(x1-x0)) = 0.5 * (-x2 + 4*x1 - 3*x0)

interpxy : (Float, Float) -> (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
interpxy (x0,y0) (x1,y1) (x2,y2) t = (interp (x0,x1,x2) t, interp (y0,y1,y2) t)

interp : (Float, Float, Float) -> Float -> Float
interp (x0,x1,x2) t =
    let a = 0.5 * (x2 - 2*x1 + x0)
        b = 0.5 * (-x2 + 4*x1 - 3*x0)
        c = x0
    in a*0.01*t*t + b*0.1*t + c

highlight = rgb 0 191 255

halfWidthChar = 4
hOffset = 11

-- draw to fit within width
monospace : Bool -> String -> Float -> Shape s
monospace isActive str width =
  let
      len = String.length str
      txtScale = width * 1.5 / (toFloat (max 6 len))
  in
      text str |> centered |> fixedwidth |> size txtScale |> filled black |> addOutline (solid 0.5) (if isActive then highlight else blank) -- |> move (width, 0)

displayNode : Stencil -> Shape notification
displayNode shp = filled (black) shp

sizeOf : String -> (Int, Int)
sizeOf str = (8 * (length str), 16)
