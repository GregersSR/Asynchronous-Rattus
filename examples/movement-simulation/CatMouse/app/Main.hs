module Main (main) where
import CatMouse(initial, render, step)
import System.Random
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)
import Graphics.Gloss.Data.Color

main :: IO ()
main = do
    gen <- newStdGen
    do simulate (InWindow "Tom & Jerry" (500, 500) (100,100)) black 30 (initial gen) render step
