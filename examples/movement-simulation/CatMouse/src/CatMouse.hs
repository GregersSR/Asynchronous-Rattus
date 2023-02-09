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
type World = (Point, Point, Trans (Point :* Direction) (Point :* Point), StdGen)

initial :: StdGen -> World
initial gen = ((10,0),(0, 10), runTransducer $ Stream.map $ box newPositions, gen)

render :: World -> Picture
render ((xm, ym),(xc, yc),_,_) = 
  Pictures
  [ translate xm ym
    (Color white (ThickCircle 3 6))
  , translate xc yc
    (Color red (ThickCircle 4 7))]

-- Screen bounds: top, bottom, left, right
bounds :: (Float, Float, Float, Float)
bounds = (250, -250, -250, 250)

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

newMousePos :: Point -> Direction -> Point
newMousePos p d = p .+. mouseDelta d .* 5

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1+x2, y1+y2)
infixl 6 .+.

(.*) :: Point -> Float -> Point
(x, y) .* k = (k*x, k*y)
infixl 7 .*

newPositions :: (Point :* Direction) -> (Point :* Point)
newPositions m = (newMouse :* (0,0))
    where newMouse = applyDelta m

applyDelta :: (Point :* Direction) -> Point
applyDelta (p :* d)
    | isInBounds newMouse = newMouse
    | isInBounds reverseD = reverseD
    | isInBounds rotatedD = rotatedD
    | otherwise = newMousePos p $ reverseDirection $ rotateClockwise d
    where newMouse = newMousePos p d
          reverseD = newMousePos p $ reverseDirection d
          rotatedD = newMousePos p $ rotateClockwise d

isInBounds :: Point -> Bool
isInBounds (x, y)
    | y > t = False
    | y < b = False
    | x < l = False
    | x > r = False
    | otherwise = True
    where (t, b, l, r) = bounds

step :: ViewPort -> Float -> World -> World
step _ _ (m,c, Trans st, gen) =  
    (m', c', st', gen')
    where (randomDirection, gen') = random gen
          ((m' :* c'), st') = st (m :* randomDirection)
                  
{-
catmouse :: Str RandomGen -> Str (Point :* Point, RandomGen)
catmouse = 
-}

