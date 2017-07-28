module Update where

import Prelude
import Data.Array (unsafeIndex)
import Data.Traversable (elem)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Signal (Signal)
import Signal.Time (Time, every, second)

import State (Game, Object, Point, Robot, lifeSpan, screen)
import Genetic (selection)

fps :: Time -> Signal Time
fps x = every (second/x)

delta :: Signal Time
delta = fps 60.0

isCollided :: Robot -> Object -> Boolean
isCollided { pos } { poss } =
  pos `elem` poss

outBounds :: Point -> Point
outBounds (Tuple x y)
  | (x < 0) = Tuple 0 y
  | (y < 0) = Tuple x 0
  | x > screen.width = Tuple (screen.width) y
  | y > screen.height = Tuple x (screen.height)
  | otherwise = Tuple x y

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

updateRobot :: Object -> Object -> Int -> Robot -> Robot
updateRobot obstacles target step robot
  | outBounds robot.pos /= robot.pos = robot { pos = outBounds robot.pos
                                             , alive = false
                                             , color = "red"
                                             , fitness = 0.0
                                             }
  | isCollided robot obstacles = robot { alive = false
                                       , color = "red"
                                       , fitness = 0.0
                                       }
  | isCollided robot target = robot { alive = false
                                    , color = "green"
                                    }
  | otherwise = moveRobot robot step

stepGame :: Game -> Game
stepGame g@{ population, obstacles, target, step, gen, seed }
  | step > (lifeSpan - 1) = (selection g) { step = 0 , gen = gen + 1 }
  | otherwise =
    g { population = map (updateRobot obstacles target step) population
      , step = step + 1
      }
