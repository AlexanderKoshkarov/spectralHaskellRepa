module Utils (
    squear
  , mag2
  , indexToK
  , indexFromK
) where

import Control.Monad (join)
import Data.Complex (Complex(..), realPart, conjugate)

squear :: Num a => a -> a
squear = join (*)

mag2 :: RealFloat a => Complex a -> a
mag2 x = realPart $ (conjugate x) * x


-- functions to convert indexies [0..(n-1)]  <--> [-k..k]
-- it is needed because of DFT properties
indexToK :: Int -> Int -> Int
indexToK n i
  | i <= nk   = i
  | otherwise = i - n
  where nk = (n - 1) `div` 2

indexFromK :: Int -> Int -> Int
indexFromK n k
  | k >= 0    = k
  | otherwise = n + k
  where nk = (n-1) `div` 2



