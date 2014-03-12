import Window
import Keyboard

(width, height) = (600, 400)
(hWidth, hHeight) = (width/2, height/2)

speed : Float
speed = 5

size : Float
size = 10

data Event = Tick (Time, (Int, Int))

formString : Float -> Float -> String -> Form
formString y scl str = toText str |> Text.color black
                          |> text 
                          |> toForm
                          |> scale scl 
                          |> move (0, y)

delta : Signal Float
delta = (fps 30)

input : Signal (Float, (Int, Int))
input = (,) <~ lift inSeconds delta
             ~ sampleOn delta ((\{ x, y } -> (x, y)) <~ Keyboard.arrows)

type Vec = (Float, Float) 

data Heading = Up | Down | Left | Right

type Snake = { pos     : Vec
             , heading : Heading
             , body    : [Vec] }

defaultSnake : Snake
defaultSnake = { pos = (0, 0)
               , heading = Right 
               , body = [ (0,0), 
                          (0,0), 
                          (0,0), 
                          (0,0), 
                          (0,0) ] }

type Game = { snake : Snake
            , score : Int }

defaultGame : Game
defaultGame = { snake = defaultSnake
              , score = 0 }

updatePos : Heading -> Float ->  Vec -> Vec
updatePos h sp (x,y) = if | h == Left -> (x - sp, y)
          | h == Right -> (x + sp, y)
          | h == Down -> (x, y - sp) 
          | h == Up -> (x, y + sp)
          | otherwise -> (x, y)

init : [Vec] -> [Vec]
init list = if | length list < 2 -> list
               | otherwise -> let (x::xs) = list in x :: (initHelper xs)

initHelper : [Vec] -> [Vec]
initHelper (x::xs) = if | xs == [] -> []
                        | otherwise -> x :: (initHelper xs)

updateBody : Vec -> Heading -> [Vec] -> [Vec]
updateBody pos h body = map (updatePos h 0) (pos :: (init body))

stepSnake : (Int, Int) -> Snake -> Snake
stepSnake dir ({pos, heading} as snake) = 
    let h = heading
        left dir = (fst dir) < 0
        right dir = (fst dir) > 0
        down dir = (snd dir) < 0
        up dir = (snd dir) > 0
        getHeading dir = if | left dir -> Left
                            | right dir -> Right
                            | down dir -> Down
                            | up dir -> Up
    in { snake | pos <- updatePos h speed pos
               , heading <- if | left dir || right dir -> if h == Left || h == Right 
                                                          then h 
                                                          else getHeading dir
                               | down dir || up dir -> if h == Up || h == Down 
                                                       then h 
                                                       else getHeading dir
                               | otherwise -> h
               , body <- updateBody pos h snake.body }

stepGame : Event -> Game -> Game
stepGame event g = 
    case event of
        Tick (t, dir) -> let g' = { g | snake <- stepSnake dir g.snake
                                      , score <- g.score + 1 }
                             out = let (x, y) = g.snake.pos 
                                   in abs x > hWidth - size || abs y > hHeight - size
                         in if out then defaultGame else g'
        _             -> g

event : Signal Event
event = merges [ lift Tick input ]

render : (Int, Int) -> Game -> Element
render (w, h) g = 
    let formSnake pos = circle size |> filled black |> move pos
        txts  = [ {-(formString 100 2 (show g.snake.body))-} ]
        forms = txts ++ map formSnake (g.snake.pos :: g.snake.body)
    in color black <| container w h middle <| color white <| collage width height forms

main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame event
