module State where

import Prelude
import Data.Tuple (Tuple(..))
import Test.QuickCheck.Gen (chooseInt, sample, vectorOf)
import Test.QuickCheck.LCG (Seed)

screen :: { width :: Int, height :: Int, block :: Int }
screen =
  { width: 80
  , height: 60
  , block: 10
  }

canvasSize :: { w :: Number, h :: Number }
canvasSize =
  { w: 810.0
  , h: 610.0
  }

lifeSpan :: Int
lifeSpan = 200

populationSize :: Int
populationSize = 50

type Point = Tuple Int Int

type Game =
  { obstacles :: Object
  , target :: Object
  , population :: Population
  , step :: Int
  , gen :: Int
  , seed :: Seed
  }

initArrDna :: Seed -> Array (Array Int)
initArrDna s = sample s populationSize (vectorOf lifeSpan (chooseInt 1 4))

makePopulation :: Robot -> Array Dna -> Population
makePopulation r arrDna = map go arrDna
  where
    go x = r { dna = x }

initGame :: Seed -> Game
initGame seed =
  { obstacles: defaultObstacles
  , target: target
  , population: makePopulation initRobot (initArrDna seed)
  , step: 0
  , gen: 1
  , seed: seed
  }

type Robot = { pos :: Point
             , dir :: Point
             , color :: String
             , alive :: Boolean
             , step :: Int
             , fitness :: Number
             , dna :: Dna
             }

initRobot :: Robot
initRobot =
  { pos: Tuple 10 30
  , dir: Tuple 0 0
  , color: "blue"
  , alive: true
  , step: 0
  , fitness: 0.0
  , dna: [0]
  }

type Dna = Array Int

type Population = Array Robot

defaultRobot :: Robot
defaultRobot = { pos: Tuple (screen.width/2) (screen.height/2)
               , dir: Tuple 0 0
               , color: "blue"
               , alive: true
               , step: 0
               , fitness: 0.0
               , dna: [0]
               }

type Object =
  { poss :: Array Point
  , color :: String
  }

target :: Object
target =
  { poss : [Tuple 70 30]
  , color: "yellow"
  }

defaultObstacles :: Object
defaultObstacles =
  { poss: [ Tuple 0 0
          -- Mid wall
          -- , Tuple 40 25
          -- , Tuple 40 26
          -- , Tuple 40 27
          -- , Tuple 40 28
          -- , Tuple 40 29
          -- , Tuple 40 30
          -- , Tuple 40 31
          -- , Tuple 40 32
          -- , Tuple 40 33
          -- , Tuple 40 34
          -- , Tuple 40 35

          -- Two walls
          , Tuple 30 31
          , Tuple 30 32
          , Tuple 30 33
          , Tuple 30 34
          , Tuple 30 35
          , Tuple 30 36
          , Tuple 30 37
          , Tuple 30 38
          , Tuple 30 39
          , Tuple 30 40
          , Tuple 30 41
          , Tuple 30 42
          , Tuple 30 43
          , Tuple 30 44
          , Tuple 30 45
          , Tuple 30 46
          , Tuple 30 47
          , Tuple 30 48
          , Tuple 30 49
          , Tuple 30 50

          , Tuple 50 31
          , Tuple 50 30
          , Tuple 50 29
          , Tuple 50 28
          , Tuple 50 27
          , Tuple 50 26
          , Tuple 50 25
          , Tuple 50 24
          , Tuple 50 23
          , Tuple 50 22
          , Tuple 50 21
          , Tuple 50 20
          , Tuple 50 19
          , Tuple 50 18
          , Tuple 50 17
          , Tuple 50 16
          , Tuple 50 15
          , Tuple 50 14
          , Tuple 50 13
          , Tuple 50 12
          , Tuple 50 11
          , Tuple 50 10

          ]
  , color: "white"
  }
