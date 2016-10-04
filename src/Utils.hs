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

-- why not (Num a => Complex a -> a) ???
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



--import Data.Array.Repa.IO.Binary (writeArrayToStorableFile)
--writeArrayToStorableFile :: forall sh a r. (Shape sh, Source r a, Storable a) => FilePath -> Array r sh a -> IO ()
--

--im = [hyperV,advU0,c1]


--fmax :: Field -> Double
--fmax = foldAllS max 0 . R.map magnitude
--

{-

computeAll ::  IO ()
computeAll = do
  putStrLn "#time en"
  st <- solution
  let en = energy st
      enTime = zip time en
  writeFile "results/en.dat" . unlines $ zipWith (\t en -> show t ++ " " ++ show en) time en



iterateMN :: Int -> (State -> IO State) -> State -> IO [State]
iterateMN 0 _ _ = return []
iterateMN n f st = do
  if n `mod` 100 == 0 then logging else return ()
  newSt <- f st
  return (st:) <*> iterateMN (n-1) f newSt
    where logging = do
            putStrLn $ t P.++ " " P.++ show (sumAllS . R.map mag2 $ st)
            plotFunc3d [PNG ("results/den/den"   P.++ t  P.++ ".png")] [Plot3dType ColorMap] [-nkx..nkx] [-nky..nky] (\kx ky -> log . magnitude $ st ! (Z :. 0 :. indexFromK nx ky :. indexFromK nx kx))
            plotFunc3d [PNG ("results/theta/theta" P.++ t  P.++ ".png")] [Plot3dType ColorMap] [-nkx..nkx] [-nky..nky] (\kx ky -> log . magnitude $ st ! (Z :. 1 :. indexFromK nx ky :. indexFromK nx kx))
            plotFunc3d [PNG ("results/vort/vort"  P.++ t  P.++ ".png")] [Plot3dType ColorMap] [-nkx..nkx] [-nky..nky] (\kx ky -> log . magnitude $ st ! (Z :. 2 :. indexFromK nx ky :. indexFromK nx kx))
          t = printf "%.4f" (time !! (nt - n)) :: String



plf3d f st = plotFunc3d [] [Plot3dType ColorMap] [-nkx..nkx] [-nky..nky] (\kx ky ->  f $ st ! (Z :. indexFromK nx ky :. indexFromK nx kx))


pl3d st = plotFunc3d [] [Plot3dType ColorMap] [-nkx..nkx] [-nky..nky] (\kx ky ->  magnitude $ st ! (Z :. indexFromK nx ky :. indexFromK nx kx))


plx3d st = plotFunc3d [] [Plot3dType ColorMap] [0..(nx-1)] [0..(ny-1)] (\x y ->  magnitude $ st ! (Z :. y :. x ))

solution :: IO [State]
solution =  iterateMN nt solve st0


point :: Int -> Int -> Int -> [State] -> [Complex Double]
point n x y = ((! (Z :. n :. y :. x)) <$>)

energy :: [State] -> [Double]
energy = P.map (sumAllS . R.map mag2)


-}
--mag2 x = realPart $ x * (conjugate x)





