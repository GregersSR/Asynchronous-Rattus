{-# OPTIONS -fplugin=Rattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module CatMouse where

-- Simulate Cat & Mouse


import Rattus
import Rattus.Primitives
import Rattus.Stream (Str(..))
import qualified Rattus.Stream as Stream
import Rattus.Plugin.Annotation
import Rattus.ToHaskell
import System.Random
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Simulate hiding (Point)

{-# ANN module Rattus #-}

data Direction = None | N | NE | E | SE | S | SW | W | NW deriving (Enum, Bounded, Show)

reverseDirection :: Direction -> Direction
reverseDirection None = None
reverseDirection N    = S
reverseDirection NE   = SW
reverseDirection E    = W
reverseDirection SE   = NW
reverseDirection S    = N
reverseDirection SW   = NE
reverseDirection W    = E
reverseDirection NW   = SE

rotateClockwise :: Direction -> Direction
rotateClockwise None = None
rotateClockwise N    = E
rotateClockwise NE   = SE
rotateClockwise E    = S
rotateClockwise SE   = SW
rotateClockwise S    = W
rotateClockwise SW   = NW
rotateClockwise W    = N
rotateClockwise NW   = NE

-- Inspired by: https://stackoverflow.com/questions/11811498/generate-a-random-value-from-a-user-defined-data-type-in-haskell
instance Random Direction where
    random g = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')


type Point = (Float, Float)

-- Mouse pos, Cat pos, State machine ((mouse pos, mouse direction), cat pos), random gen
type World rng = (Point, Point, Trans ((Point :* Direction) :* Point) (Point :* Point), rng)

initial :: rng -> World rng
initial gen = ((10,0),(0, 10), runTransducer $ Stream.map $ box newPositions, gen)

render :: World rng -> Picture
render ((xm, ym),(xc, yc),_,_) = 
  Pictures
  [ translate xm ym
    (Color white (ThickCircle 3 6))
  , translate xc yc
    (Color red (ThickCircle 4 7))]

-- Screen bounds: top, bottom, left, right
bounds :: (Float, Float, Float, Float)
bounds = (250, -250, -250, 250)

inRange :: Float -> Float -> Float -> Bool
inRange min max f = f < max && f >= min

degrees :: Float -> Float
degrees = (/pi) . (180*)

closestDirection :: Float -> Direction
closestDirection degrees
    | inRange (-22.5) 22.5 degrees = E
    | inRange 22.5 67.5 degrees = NE
    | inRange 67.5 112.5 degrees = N
    | inRange 112.5 157.5 degrees = NW
    | inRange (-67.5) (-22.5) degrees = SE
    | inRange (-112.5) (-67.5) degrees = S
    | inRange (-157.5) (-112.5) degrees = SW
    | otherwise = W

{-# ANN simpleBestPath NotRattus #-}
-- Computes a best path simply by calculating the direction most closely matching the angle to other point at each step.
simpleBestPath :: Point -> Point -> List Direction
simpleBestPath start@(xs, ys) goal@(xg, yg) 
    | start == goal = None :! Nil
    | xd == 0 && yd > 0 = N :! simpleBestPath (move (start :* N) 1) goal
    | xd == 0 = S :! simpleBestPath (move (start :* S) 1) goal
    | otherwise = go $ closestDirection $ degrees $ atan2 yd xd 
    where xd = xg - xs
          yd = yg - ys
          go d = d :! (simpleBestPath (move (start :* d) 1) goal)

{-# ANN takeStrict NotRattus #-}
takeStrict :: Int -> List a -> List a
takeStrict _ Nil = Nil
takeStrict n (x :! xs) = if n > 0 then x :! takeStrict (n-1) xs else Nil

walk :: Point -> List Direction -> Int -> Point
walk p ds n = foldl (\newp d -> move (newp :* d) 1) p (takeStrict n ds)

delta :: Direction -> Point
delta d =
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

newMousePos :: (Point :* Direction) -> Point
newMousePos m = move m 20

-- cat pos, mouse pos, new cat pos
newCatPos :: Point -> Point -> Point
newCatPos c m = walk c (simpleBestPath c m) 5

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1+x2, y1+y2)
infixl 6 .+.

(.*) :: Point -> Float -> Point
(x, y) .* k = (k*x, k*y)
infixl 7 .*

newPositions :: ((Point :* Direction) :* Point) -> (Point :* Point)
newPositions (m :* c) = (newMouse :* newCat)
    where newMouse = newMousePos m
          newCat   = newCatPos c newMouse

moveSimple :: (Point :* Direction) -> Float -> Point
moveSimple (p :* d) k = p .+. delta d .* k

move :: (Point :* Direction) -> Float -> Point
move mv@(p :* d) k
    | isInBounds newPos = newPos
    | isInBounds reverseD = reverseD
    | isInBounds rotatedD = rotatedD
    | otherwise = moveSimple (p :* (reverseDirection $ rotateClockwise d)) k
    where newPos = moveSimple mv k
          reverseD = moveSimple (p :* reverseDirection d) k
          rotatedD = moveSimple (p :* rotateClockwise d) k

isInBounds :: Point -> Bool
isInBounds (x, y)
    | y > t = False
    | y < b = False
    | x < l = False
    | x > r = False
    | otherwise = True
    where (t, b, l, r) = bounds

step :: (RandomGen rng) => ViewPort -> Float -> World rng -> World rng
step _ _ (m,c, Trans st, gen) =  
    (m', c', st', gen')
    where (randomDirection, gen') = random gen
          ((m' :* c'), st') = st ((m :* randomDirection) :* c)
                  
{-
catmouse :: Str RandomGen -> Str (Point :* Point, RandomGen)
catmouse = 
-}

