module Genetic where

import Prelude (map, mod, otherwise, ($), (*), (+), (-), (/), (<=), (<>), (==))
import Data.Array (concatMap, drop, length, replicate, take, unsafeIndex, zip, zipWith)
import Data.Int (toNumber, round)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Math (pow, sqrt, max)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Gen (choose, chooseInt, sample, vectorOf)
import Test.QuickCheck.LCG (Seed, lcgNext)

import State

calcDistance :: Point -> Point -> Number
calcDistance (Tuple x y) (Tuple a b) =
  if (dist x y a b) == 0.0
    then
      1.0
    else
      1.0 / (dist x y a b)
  where
    dist x1 y1 x2 y2 =
      sqrt $ (pow (toNumber x2 - toNumber x1) 2.0) + (pow (toNumber y2 - toNumber y1) 2.0)

matingPool :: Population -> Population
matingPool p = concatMap addDna p
  where
    addDna x = replicate (round $ x.fitness*100.0) x

crossOver :: Int -> Dna -> Dna -> Dna
crossOver p x y = take p x <> drop p y

calcFitness :: Point -> Population -> Population
calcFitness t p = calcFit
  where
    calcFit = map (\x -> x { fitness = calcDistance x.pos t }) p
    maxFit = foldl (\x y -> max y.fitness x) 0.0 calcFit
    normalize f = f / maxFit

mutationRate :: Number
mutationRate = 0.01

mutate :: Seed -> Population -> Population
mutate sd p = zipWith go mutationArr p
  where
    mutationArr =
      sample sd populationSize (vectorOf lifeSpan (choose 0.0 1.0))
    go ms r = r { dna = zipWith goMutate ms r.dna }
    goMutate m g
      | m <= mutationRate = (g `mod` 4) + 1
      | otherwise = g

selection :: Game -> Game
selection g@{ population, seed, target } =
  g { population = mutate seed4 crossPop, seed = seed4 }
  where
    seed1 = lcgNext seed
    seed2 = lcgNext seed1
    seed3 = lcgNext seed2
    seed4 = lcgNext seed3
    p1 = sample (seed1) populationSize (chooseInt 0 mpSize)
    p2 = sample (seed2) populationSize (chooseInt 0 mpSize)
    cr = sample (seed3) populationSize (chooseInt 0 lifeSpan)

    targetPos = unsafePartial $ unsafeIndex target.poss 0
    fitPop = calcFitness targetPos population
    mp = matingPool fitPop
    mpSize = (length mp) - 1

    parent x = unsafePartial $ unsafeIndex mp x
    randParent = zip p1 p2
    crossPop = zipWith goCross randParent cr
    goCross p c =
      initRobot { dna = crossOver c (parent (fst p)).dna (parent (snd p)).dna
                , fitness = (parent (snd p)).fitness
                }
