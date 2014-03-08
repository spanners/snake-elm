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

type Snake = {pos:Vec}

defaultSnake = { pos = (0, 50) }

type Game = { snake : Snake
            , score : Int }

defaultGame = { snake = defaultSnake
              , score = 0 }

stepSnake dir snake = { snake | pos <- if | (fst dir) < 0 -> (((fst snake.pos) - speed), (snd snake.pos))
                                             | (fst dir) > 0 -> (((fst snake.pos) + speed), (snd snake.pos))
                                             | (snd dir) < 0 -> ((fst snake.pos), ((snd snake.pos) - speed))
                                             | (snd dir) > 0 -> ((fst snake.pos), ((snd snake.pos) + speed)) 
                                             | otherwise -> snake.pos }

stepGame event g = 
    case event of
        Tick (t, dir) -> { g | snake <- stepSnake dir g.snake
                         , score     <- g.score + 1 }
        _             -> g

event = merges [ lift Tick input ]

render (w, h) g = 
    let formSnake {pos} = circle 10 |> filled black |> move pos
        txts  = [ tf 0 4 "Hello World"]
        forms = txts ++ [formSnake g.snake]
    in color black <| container w h middle <| color white <| collage width height forms

main = render <~ Window.dimensions ~ foldp stepGame defaultGame event

