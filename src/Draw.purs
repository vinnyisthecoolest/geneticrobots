module Draw where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (select, setText)
import Data.Array (length)
import Data.Traversable (sum, traverse_)
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Graphics.Canvas
  ( Context2D
  , Rectangle
  , setFillStyle
  , fillRect
  )

import State (Game, Robot, Object, Point, Population, canvasSize, screen)

drawRect :: Context2D -> Rectangle -> String -> Eff (_) Unit
drawRect ctx { x, y, w, h } color = do
  _ <- setFillStyle color ctx
  _ <- fillRect ctx { x: x, y: y, w: w, h: h }
  pure unit

block :: Point -> Rectangle
block (Tuple x y) =
  { x: toNumber $ screen.block * x
  , y: toNumber $ screen.block * y
  , w: toNumber $ screen.block
  , h: toNumber $ screen.block
  }

drawBlock :: Context2D -> Point -> String -> Eff _ Unit
drawBlock ctx (Tuple x y) color =
  drawRect ctx (block (Tuple x y)) color

clearScreen :: Context2D -> Eff (_) Unit
clearScreen ctx =
  drawRect ctx { x: 0.0
               , y: 0.0
               , w: canvasSize.w
               , h: canvasSize.h
               } "black"

drawRobot :: Context2D -> Robot -> Eff _ Unit
drawRobot ctx { pos, color } =
  drawBlock ctx pos color

drawObject :: Context2D -> Object -> Eff _ Unit
drawObject ctx { poss, color } = traverse_ draw poss
  where
    draw x = drawBlock ctx x color

drawPopulation :: Context2D -> Population -> Eff _ Unit
drawPopulation ctx xs = traverse_ (drawRobot ctx) xs

render :: Context2D -> Game -> Eff _ Unit
render ctx { population, obstacles, target, step, gen } = do
  let fitness = ((sum $ map (\x -> x.fitness) population) / (toNumber $ length population)) * 100.0

  -- JQuery
  stepHtml <- select "#step"
  genHtml <- select "#gen"
  fitHtml <- select "#fit"

  setText ("Step: " <> show step) stepHtml
  setText ("Generation: " <> show gen) genHtml
  setText ("Average Fitness: " <> show fitness) fitHtml

  -- Canvas
  clearScreen ctx
  drawObject ctx obstacles
  drawObject ctx target
  drawPopulation ctx population
