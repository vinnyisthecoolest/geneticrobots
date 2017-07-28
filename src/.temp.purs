module Main where

import Prelude hiding (append)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array
import Data.Traversable
-- import Data.Foldable
import Data.Int
import Data.Tuple
import Data.Maybe
import Data.Show
import Graphics.Canvas
  ( Context2D
  , Rectangle
  , setFillStyle
  , fillRect
  , getCanvasElementById
  , getContext2D
  )
import Math (sqrt, max)
import Partial.Unsafe (unsafePartial)
import Signal (Signal, runSignal, foldp, sampleOn, map4, constant, unwrap, merge)
import Signal.DOM (animationFrame, mouseButton, mousePos, keyPressed)
import Signal.Time (Time, every, second)

import Draw (drawRect, drawBlock, drawRobot, drawTarget
            , clearScreen, drawObstacles)
import State (Game, Point, Robot, Population, Dna, Obstacle, Target, RandSelect, screen,
              defaultObstacles, target)
import Update (isCollided, outBounds, delta)

import Control.Monad.Eff.JQuery
import Control.Monad.Except (runExcept)
import Data.Foreign (readString)
import DOM (DOM)
import Partial.Unsafe (unsafePartial)


updateRobot :: Obstacle -> Target -> Int -> Robot -> Robot
updateRobot obstacles target step robot
  | outBounds robot.pos /= robot.pos
    = robot { pos = outBounds robot.pos
            , dir = Tuple 0 0
            , alive = false
            , color = "red"
            }
  | isCollided robot obstacles = robot { alive = false, color = "red" }
  -- | isCollided robot target = robot { alive = false, color = "green" }
  | otherwise = moveRobot robot step

moveRobot :: Robot -> Int -> Robot
moveRobot r@{ pos, alive, dna } step = do
  if alive
    then
      r { pos = pos + (move step dna) }
    else
      r

move :: Int -> Array Int -> Point
move i xs =
  go (unsafePartial $ unsafeIndex xs i)
   where
      go x
        | x == 1 = Tuple 0 1
        | x == 2 = Tuple 0 (-1)
        | x == 3 = Tuple (-1) 0
        | otherwise = Tuple 1 0

drawPopulation :: Context2D -> Population -> Eff _ Unit
drawPopulation ctx xs =
  traverse_ (drawRobot ctx) xs

render :: Context2D -> Game -> Eff _ Unit
render ctx { population, obstacles, target, step } = do
  -- JQuery
  test <- select "#test"
  setText (show step) test

  -- Canvas
  clearScreen ctx
  drawObstacles ctx obstacles
  drawTarget ctx target
  drawPopulation ctx population

-- GENETIC SHIT

calcDistance :: Point -> Point -> Number
calcDistance (Tuple x y) (Tuple a b) = 1.0 / (dist x y a b)
  where
    dist x1 y1 x2 y2 = sqrt $ (toNumber x2 - toNumber x1) + (toNumber y2 - toNumber y1)

matingPool :: Population -> Population
matingPool p =
  concatMap addDna p
  where
    addDna x = replicate (round $ x.fitness*100.0) x

crossOver :: Int -> Dna -> Dna -> Dna
crossOver r x y = take r x <> drop r y

calcFitness :: Population -> Population
calcFitness p = map (\x -> x { fitness = x.fitness / maxfit }) p
  where maxfit = foldl (\x y -> max y.fitness x) 0.0 p

initSelect :: Eff _ (Array { p1 :: Int, p2 :: Int, c :: Int })
initSelect = traverse go (range 1 populationSize)
  where
    go x = do
      r1 <- randomInt 0 populationSize
      r2 <- randomInt 0 populationSize
      rc <- randomInt 0 lifeSpan

      pure { p1: r1
           , p2: r2
           , c: rc
           }

selection :: Array { p1 :: Int, p2 :: Int, c :: Int } -> Population -> Population
selection randSelect pop =
  zipWith go randSelect (calcFitness pop)
  where
    mp = matingPool pop
    parent x = unsafePartial $ unsafeIndex mp x
    go { p1, p2, c } rob =
      rob { dna = crossOver c (parent p1).dna (parent p2).dna }


stepGame :: Game -> Game -> Game
stepGame initGame g@{ population, obstacles, target, step, randSelect } =
  if
    step > lifeSpan
  then
    initGame
    -- g { population = selection randSelect population }
  else
    g { population = map (updateRobot obstacles target step) population
      , step = step + 1
      }

main = ready $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  dna <- initArrDna
  select <- initSelect

  let initGame = { robot: initRobot
                 , obstacles: defaultObstacles
                 , target: target
                 , population: makePopulation initRobot dna
                 , step: 0
                 , randSelect: select
                 }
      game = foldp (\a b -> stepGame initGame b) initGame delta

  runSignal $ render ctx <$> game

-- main = unsafePartial do
  -- game <- unwrap $ foldp (\a b -> randomInt 1 5) (randomInt 1 5) delta
  -- runSignal $ map logShow game

  -- game <- unwrap $ foldp (\a b -> randomInt 1 5) (randomInt 1 5) delta
  -- game' <- unwrap $ foldp (\a b -> randomInt 6 10) (randomInt 6 10) delta
  -- runSignal $ map logShow (merge game game')

--   rand <- randomInt 1 5
--   game <- unwrap $ foldp stepTest (stepTest 0 0) delta
--   runSignal $ map (\x -> logShow x.x) game
--
-- stepTest a b = do
--   rand <- randomInt 1 5
--   pure $ test rand

test n = { x: n, y: "cool" }

initRobot :: Robot
initRobot =
  { pos: Tuple 10 (screen.height/2)
  , dir: Tuple 0 0
  , color: "blue"
  , alive: true
  , step: 0
  , fitness: 0.0
  , dna: [0]
  }

makePopulation :: Robot -> Array Dna -> Population
makePopulation r arrDna = map go arrDna
  where
    go x = r { dna = x }

lifeSpan = 50
populationSize = 1

initDna = traverse (\_ -> randomInt 1 4) (range 1 lifeSpan)
initArrDna = traverse (\_ -> initDna) (range 1 populationSize)
