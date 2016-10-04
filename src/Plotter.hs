{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Plotter (
    plotAll
) where

import System.Directory (listDirectory)
import Data.List (isPrefixOf,isSuffixOf)
import Data.List.Extra (dropEnd)
import Control.Monad (forM,zipWithM_)
import Data.Complex (Complex(..),magnitude)

import Data.Array.Repa.Repr.ForeignPtr (F(..))
import Data.Array.Repa.IO.Binary (readArrayFromStorableFile)
import Data.Array.Repa (Array(..),Z(..),(:.)(..),DIM3
                       ,(!)
                       ,toFunction
                       )

import Constants
import Utils (indexFromK)

import Graphics.Gnuplot.Simple (Attribute(..),Attribute3d(..),Plot3dType(..),plotFunc3d)
--import Control.Concurrent (threadDelay)
--import Debug.Trace (trace)

plotAll :: IO ()
plotAll = do
  let isStFile = and . sequence [isPrefixOf "st", isSuffixOf ".dat"]
      sh       = Z :. (3 :: Int) :. ny :. nx
      storage  = "./results/state/"
  stList <-  filter isStFile <$> listDirectory storage
  arrs  <- forM (map ((,sh) . (storage ++)) stList) (uncurry readArrayFromStorableFile) :: IO [Array F DIM3 (Complex Double)]
  let tStrList = map (dropEnd 4 . drop 2) stList
      plotState st tStr = do
        pl ("results/den/den"     ++ tStr  ++ ".png") 0
        pl ("results/theta/theta" ++ tStr  ++ ".png") 1
        pl ("results/vort/vort"   ++ tStr  ++ ".png") 2
          where pl path num = plotFunc3d [PNG path]
                                         [Plot3dType ColorMap]
                                         [-nkx..nkx]
                                         [-nky..nky]
                                         $ \kx ky -> log . magnitude $ st ! (Z :. num :. indexFromK nx ky :. indexFromK nx kx)

  zipWithM_ plotState arrs tStrList
