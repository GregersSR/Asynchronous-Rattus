{-# OPTIONS -fplugin=Rattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}

module CatMouse where

-- Simulate Cat & Mouse


import Rattus
import Rattus.Primitives
import Rattus.Stream (Str(..))
import qualified Rattus.Stream as Stream
import Rattus.ToHaskell
import System.Random
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)

{-# ANN module Rattus #-}

data Direction = None | N | NE | E | SE | S | SW | W | NW deriving (Enum, Bounded, Show)

-- Inspired by: https://stackoverflow.com/questions/11811498/generate-a-random-value-from-a-user-defined-data-type-in-haskell
instance Random Direction where
    random g = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')


type Point = (Float, Float)
type World = (Point, Point, Trans Direction (Point :* Point), StdGen)

initial :: StdGen -> World
initial gen = ((10,0),(0, 10), runTransducer $ Stream.map $ box delta, gen)

render :: World -> Picture
render ((xm, ym),(xc, yc),_,_) = 
  Pictures
  [ translate xm ym
    (Color white (ThickCircle 3 6))
  , translate xc yc
    (Color red (ThickCircle 4 7))]
{-
mousePos :: RandomGen -> Str Point
mousePos rand initialP = Stream.unfold (box mouseMove) (rand, initialP)

directions :: StdGen -> Str Direction
directions gen = (r :: Direction) ::: delay (directions gen')
    where (r, gen') = random gen

mouseMove :: (StdGen, Point) -> (StdGen, Point)
mouseMove (gen, p) = (gen', mouseMove r)
    where (r,gen') = randomR (0, 8) gen
-}

mouseDelta :: Direction -> Point
mouseDelta d =
    case d of
    None -> (0,   0)
    N    -> (0,   1)
    NE   -> (1,   1)
    E    -> (1,   0)
    SE   -> (1,  -1)
    S    -> (0,  -1)
    SW   -> (-1, -1)
    W    -> (-1,  0)
    NW   -> (-1,  1)

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1+x2, y1+y2)

delta :: Direction -> (Point :* Point)
delta d = (mouseDelta d :* (0,0))

step :: ViewPort -> Float -> World -> World
step _ _ (m,c, Trans st, gen) =  
    (m .+. m', c .+. c', st', gen')
    where (randomDirection, gen') = random gen
          ((m' :* c'), st') = st randomDirection
                  
{-
catmouse :: Str RandomGen -> Str (Point :* Point, RandomGen)
catmouse = 
-}

