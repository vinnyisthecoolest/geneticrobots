module Main where

import Prelude (Unit, bind, ($), (<$>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (ready)
import Control.Monad.Eff.Random (RANDOM)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Signal (foldp, runSignal)
import Test.QuickCheck.LCG

import Draw (render)
import State (initGame)
import Update (delta, stepGame)

main :: ∀ eff. Eff (dom:: DOM, canvas :: CANVAS, random :: RANDOM | eff) Unit
main = ready $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  seed <- randomSeed

  let game = foldp (\a b -> stepGame b) (initGame seed) delta
  runSignal $ render ctx <$> game
