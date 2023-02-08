-- Simulate a robot with a distance sensor in front.
-- The robot receives a stream of measurements and outputs commands
-- so that it always stays 50 distance units from the thing in front of it.
-- However, the robot can move a maximum of 10 distance units per step.

{-# OPTIONS -fplugin=Rattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}

import Rattus
import Rattus.Primitives
import Rattus.Stream (Str)
import qualified Rattus.Stream as Stream
import Rattus.ToHaskell

{-# ANN module Rattus #-}

data Movement = Forward !Int | Back !Int | Idle deriving (Show)

-- Position of wall, robot
type World = (Int :* Int)

worlds :: Str World
worlds = Stream.unfold (box stepWorld) (50 :* 0)

move :: World -> Movement
move (wp :* rp)
    | diff >= 10          = Forward diff
    | inRange (-10) 10 diff = Idle
    | diff <= -10         = Back diff
    where diff = wp - rp

newPos :: Int -> Movement -> Int
newPos p m =
    p + (movementDelta $ validate m)

stepWorld :: World -> World
stepWorld world@(w :* r) = (w+1 :* (newPos r $ move world))

inRange :: Int -> Int -> Int -> Bool
inRange min max n
    | n < min = False
    | n > max = False
    | otherwise = True

{-
worlds :: Str World
worlds =
    (wp ::: wps) = wallPos
    world = (wp :* 0)
    delay(
--worlds = Stream.unfold (box stepWorld) (50 :* 0)


measurements :: Str Int
measurements = Stream.unfold (box (max 0 . (-1))) 200  
--measurements = 200 ::: delay (nextM (200-1))
--    where nextM x = max 0 x ::: delay (nextM (x-1))
printStep :: a -> Trans a b -> IO (b, Trans a b)
printStep state (Trans f) = do
    let (out, next) = f state
    print out
    return (out, next)

stepThrough :: a -> Trans a b -> IO b
stepThrough s trans = do
	(stepOut, next) <- printStep s trans
	stepThrough stepOut next
-}

-- Can be either positive or negative
movementDelta :: Movement -> Int
movementDelta (Forward n) = n
movementDelta (Back n)    = -n
movementDelta (Idle)      = 0

withDistance :: Movement -> Int -> Movement
withDistance (Forward _) n = Forward n
withDistance (Back _) n = Back n
withDistance (Idle) _ = Idle

main :: IO ()
main = mapM_ print $ fromStr worlds 

constrain :: Int -> Int -> Int -> Int
constrain lower upper = max lower . min upper

validate :: Movement -> Movement
validate m = withDistance m . constrain 0 10 $ abs $ movementDelta m
