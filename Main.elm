import Window
import Keyboard

(width, height) = (600, 400)
(hWidth, hHeight) = (width/2, height/2)
speed = 10

data Event = Tick (Time, (Int, Int))

tf : Float -> Float -> String -> Form
tf y scl str = toText str |> Text.color black
                          |> text 
                          |> toForm
                          |> scale scl 
                          |> move (0, y)
delta = (fps 30)
input = (,) <~ lift inSeconds delta
             ~ sampleOn delta ((\{ x, y } -> (x, y)) <~ Keyboard.arrows)

type Vec = (Float, Float) 

data Heading = Up | Down | Left | Right

type Snake = {pos:Vec, heading:Heading}

defaultSnake = { pos = (0, 0),
                 heading = Down }

type Game = { snake : Snake
            , score : Int }

defaultGame = { snake = defaultSnake
              , score = 0 }

left dir = (fst dir) < 0
right dir = (fst dir) > 0
down dir = (snd dir) < 0
up dir = (snd dir) > 0

stepSnake dir snake = { snake | pos <- if | snake.heading == Left -> ((fst snake.pos) - speed, (snd snake.pos))
                                          | snake.heading == Right -> ((fst snake.pos) + speed, (snd snake.pos))
                                          | snake.heading == Up -> ((fst snake.pos), (snd snake.pos) + speed)
                                          | otherwise -> ((fst snake.pos), (snd snake.pos) - speed)
                              , heading <- if | left dir -> if snake.heading == Left || snake.heading == Right then snake.heading else Left 
                                              | right dir -> if snake.heading == Left || snake.heading == Right then snake.heading else Right 
                                              | down dir -> if snake.heading == Up || snake.heading == Down then snake.heading else Down
                                              | up dir -> if snake.heading == Up || snake.heading == Down then snake.heading else Up
                                              | otherwise -> snake.heading }

stepGame event g = 
    case event of
        Tick (t, dir) -> { g | snake <- stepSnake dir g.snake
                         , score     <- g.score + 1 }
        _             -> g

event = merges [ lift Tick input ]

render (w, h) g = 
    let formSnake {pos} = circle 10 |> filled black |> move pos
        txts  = [ (tf 0 4 (show g.snake.heading)), (tf 10 2 (show g.snake.pos)) ]
        forms = txts ++ [formSnake g.snake]
    in color black <| container w h middle <| color white <| collage width height forms

main = render <~ Window.dimensions ~ foldp stepGame defaultGame event

