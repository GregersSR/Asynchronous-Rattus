{-# LANGUAGE RebindableSyntax #-}

module Main (module Main) where

import Rattus
import Rattus.Stream as S
import Prelude
import Rattus.Plugin.Annotation (InternalAnn (..))


{-# ANN module Rattus #-}



{-# ANN loopIndirect' ExpectError #-}
loopIndirect' :: Str v Int
loopIndirect' = let run = loopIndirect' in run

{-# ANN nestedUnguard ExpectError #-}
nestedUnguard :: Str v Int
nestedUnguard = run 0
  where run :: Int -> Str v Int
        run 0 = nestedUnguard
        run n = n ::: delay (run (n-1))

{-# ANN advDelay ExpectError #-}
advDelay :: O v (O v a) -> O v a
advDelay y = delay (let x = adv y in adv x)

{-# ANN advDelay' ExpectError #-}
advDelay' :: O v a -> a
advDelay' y = let x = adv y in x

{-# ANN dblAdv ExpectError #-}
dblAdv :: O v (O v a) -> O v a
dblAdv y = delay (adv (adv y))

{-# ANN advScope ExpectError #-}
advScope :: O v (O v Int -> Int)
advScope = delay (\x -> adv x)

{-# ANN advScope' ExpectError #-}
advScope' :: O v (Int -> Int)
advScope' = delay (let f x =  adv (delay x) in f)

{-# ANN grec ExpectError #-}
grec :: a
grec = grec

{-# ANN boxStream ExpectError #-}
boxStream :: Str v Int -> Box (Str v Int)
boxStream s = box (0 ::: tl s)

{-# ANN boxStream' ExpectError #-}
boxStream' :: Str v Int -> Box (Str v Int)
boxStream' s = box s

{-# ANN intDelay ExpectError #-}
intDelay :: Int -> O v Int
intDelay = delay

{-# ANN intAdv ExpectError #-}
intAdv :: O v Int -> Int
intAdv = adv


{-# ANN newDelay ExpectError #-}
newDelay :: a -> O v a
newDelay x = delay x

{-# ANN mutualLoop ExpectError #-}
mutualLoop :: a
mutualLoop = mutualLoop'

{-# ANN mutualLoop' ExpectError #-}
mutualLoop' :: a
mutualLoop' = mutualLoop

{-# ANN constUnstable ExpectError #-}
constUnstable :: a -> Str v a
constUnstable a = run
  where run = a ::: delay run

{-# ANN mapUnboxed ExpectError #-}
mapUnboxed :: (a -> b) -> Str v a -> Str v b
mapUnboxed f = run
  where run (x ::: xs) = f x ::: delay (run (adv xs))

{-# ANN mapUnboxedMutual ExpectError #-}
mapUnboxedMutual :: (a -> b) -> Str v a -> Str v b
mapUnboxedMutual f = run
  where run (x ::: xs) = f x ::: delay (run' (adv xs))
        run' (x ::: xs) = f x ::: delay (run (adv xs))

-- mutual recursive pattern definitions are not supported
-- foo1,foo2 :: Box (a -> b) -> Str a -> Str b
-- (foo1,foo2) = (\ f (x ::: xs) -> unbox f x ::: (delay (foo2 f) <#> xs),
--                \ f (x ::: xs) -> unbox f x ::: (delay (foo1 f) <#> xs))

{-# ANN nestedPattern ExpectError #-}
nestedPattern :: Box (a -> b) -> Str v a -> Str v b
nestedPattern = foo1 where
  foo1,foo2 :: Box (a -> b) -> Str v a -> Str v b
  (foo1,foo2) = (\ f (x ::: xs) -> unbox f x ::: (delay (foo2 f (adv xs))),
                 \ f (x ::: xs) -> unbox f x ::: (delay (foo1 f (adv xs))))


data Input = Input {jump :: !Bool, move :: Move}
data Move = StartLeft | EndLeft | StartRight | EndRight | NoMove

{-# ANN constS ExpectError #-}
-- Input is not a stable type (it is not strict). Therefore this
-- should not type check.
constS :: Input -> Str v Input
constS a = a ::: delay (constS a)


-- Since Input is not strict, we cannot instantiate the 'const'
-- function.
-- Uncomment the definition below to check this.

-- constS' :: Input -> Str Input
-- constS' = const


{-# ANN incompatibleAdvSelect ExpectError #-}
incompatibleAdvSelect :: O v Int -> O v Int -> O v Int
incompatibleAdvSelect li lk = delay (adv li + adv lk)

{-# ANN intPlusOne ExpectError #-}
intPlusOne :: O v Int -> Int
intPlusOne laterI = adv laterI + 1

{-# ANN stutter ExpectError #-}
stutter :: Int -> Str v Int
stutter n = n ::: delay (n ::: delay (stutter (n+1)))

{-# ANN main NotRattus #-}
main = putStrLn "This file should not type check"
