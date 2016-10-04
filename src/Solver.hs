{-# LANGUAGE FlexibleContexts #-}
module Solver (
    computeAll
) where

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array(..),U(..),D(..),Z(..),(:.)(..),DIM1,DIM2,DIM3
                       ,(+^),(-^),(*^),(!)
                       ,delay,fromFunction
                       ,sumAllS,sumAllP,computeS,computeP
                       )

import Data.Array.Repa.Repr.ForeignPtr (F(..))
import Data.Array.Repa.FFTW (fft2d,ifft2d)
import Data.Array.Repa.IO.Binary (writeArrayToStorableFile)

import Text.Printf (printf)
import Data.Complex (Complex(..))

--import Debug.Trace (trace)

-- My modules
import Constants
import Utils (squear, mag2, indexToK, indexFromK)

-- Data types synonyms
type Field  = Array U DIM2 (Complex Double)
type State  = Array U DIM3 (Complex Double)
type FieldD = Array D DIM2 (Complex Double)
type StateD = Array D DIM3 (Complex Double)

-- for FFTW
type FieldF = Array F DIM2 (Complex Double)
type StateF = Array F DIM3 (Complex Double)


-- initial conditions
n0,theta0,vort0 :: FieldD
--n0     =  delay . fft2d . computeS $ fromFunction (Z :.  ny :. nx) $ \(Z :. y :. x) ->
--  (0.00001*) . sum $ [ fun x y i j :+ 0 | i <- [1..3], j <- [1..3]]
--    where fun x y i j = sin (2*pi*i*(fromIntegral x)/(fromIntegral nx))
--                      * sin (2*pi*j*(fromIntegral y)/(fromIntegral ny))

-- ring
n0 =  fromFunction (Z :.  ny :. nx) $ \(Z :. y :. x) ->
  let kx = indexToK nx x
      ky = indexToK ny y
      k  = sqrt . fromIntegral $ (kx*kx + ky*ky)
  in if (k > 2) && (k < 5) then 1 else 0

theta0 = fromFunction (Z :.  ny :. nx) (const 0)
vort0  = fromFunction (Z :.  ny :. nx) (const 0)

st0 :: State
st0 = computeS . pack $ (n0,theta0,vort0)
-- end initial conditions


-- Precomputation
-- representation of differential operators in Furier space
ddx, ddy, laplace, laplaceInv, advV0, advU0, hyperV, deAl :: Field

ddx = computeS .  fromFunction (Z :. ny :. nx) $ \(Z :. _ :. x) ->
  let kx = kxMin*(fromIntegral $ indexToK nx x)
  in  0 :+ kx

ddy = computeS . fromFunction (Z :. ny :. nx) $ \(Z :. y :. _) ->
  let ky = kyMin*(fromIntegral $ indexToK ny y)
  in 0 :+ ky

laplace = computeS  . fromFunction (Z :. ny :. nx) $ \(Z :. y :. x) ->
    let kx = kxMin*(fromIntegral $ indexToK nx x)
        ky = kyMin*(fromIntegral $ indexToK ny y)
    in (-1)*(kx*kx+ky*ky) :+ 0

laplaceInv = computeS  . fromFunction (Z :. ny :. nx) $ \sh ->
  let k2 = laplace ! sh
  in if k2 == 0 then 0 else 1/k2

advV0 = computeS .  fromFunction (Z :. ny :. nx) $ \sh ->
  let ikx = ddx ! sh
  in (-1)*v0*ikx

advU0 = computeS .  fromFunction (Z :. ny :. nx) $ \sh ->
  let iky = ddy ! sh
  in (-1)*u0*iky

hyperV = computeS .  fromFunction (Z :. ny :. nx) $ \sh ->
  let mk2 = laplace ! sh
  in dhv*mk2*mk2

deAl = computeS . fromFunction (Z :. ny :. nx) $ \(Z :. y :. x) ->
  let kMax = (2/3) * fromIntegral nkx 
      kx2 = (^2) . fromIntegral $ indexToK nx x
      ky2 = (^2) . fromIntegral $ indexToK ny y
      k   = sqrt $ kx2 + ky2
  in if k > kMax then 0 else 1


-- non intuitive constants
c1, c2 :: Field
c1 =  computeS .  fromFunction (Z :. ny :. nx) $ \sh ->
  let ky  = ddy ! sh
      ik2 = laplaceInv ! sh
  in (-1)*rhoS*ky*ik2*invMu

c2 =  computeS .  fromFunction (Z :. ny :. nx) $ \sh ->
  let ik2 = laplaceInv ! sh
  in ik2*invMu


-- Theoretically, I could have write all with "traverse" command, without pack/unpack
-- and I think I eventually will.
-- however, to use fftw, I need to form foreign arrays...
-- fftw in repa does not use multiple cores (or I do not know how to make so)
-- so I think, straight convolution may be faster on multiple cores, as my array sizes are not very huge...
--
-- unpack/pack commands to go from State to Field and back
unpack :: StateD -> (FieldD,FieldD,FieldD)
unpack st = (f1,f2,f3)
  where f1 = R.traverse st changeShape (sl 0)
        f2 = R.traverse st changeShape (sl 1)
        f3 = R.traverse st changeShape (sl 2)
        changeShape (Z :. _ :. y :. x) = (Z :. y :. x)
        sl n f (Z :. y :. x) = f (Z :. n :. y :. x)


pack :: (FieldD,FieldD,FieldD) -> StateD
pack (f1,f2,f3) = fromFunction (Z :. 3 :. ny :. nx) $ \(Z :. i :. y :. x) ->
  case i of
    0 -> f1 ! (Z :. y :. x)
    1 -> f2 ! (Z :. y :. x)
    2 -> f3 ! (Z :. y :. x)
    _ -> error "Impossible (pack)"


rhs :: StateD -> StateD
rhs = if isLinear then rhsLinear else rhsNonlinear

rhsLinear :: StateD -> StateD
rhsLinear st = pack (newN, newTheta, newVort)
  where (n,theta,vort) = unpack st
        newN     = advV0*^n +^ theta                              -^ hyperV *^ n
        newTheta = advV0*^theta +^ vortMn                         -^ hyperV *^ theta
        newVort  = advU0*^vort -^ (R.map (*nuS) vortMn) +^ advPhi -^ hyperV *^ vort
        advPhi   = c1*^vortMn
        vortMn   = vort -^ n

rhsNonlinear :: StateD -> StateD
rhsNonlinear st = pack (newN, newTheta, newVort)
  where (n,theta,vort) = unpack st
        newN     =  advV0*^n +^ theta +^ nTheta  +^ nChi                        -^ hyperV *^ n
        newTheta =  advV0*^theta +^ vortMn +^ laplace *^ twoChi                 -^ hyperV *^ theta
        newVort  =  advU0*^vort -^ (R.map (*nuS) vortMn) +^ advPhi +^ poissonBr -^ hyperV *^ vort
        advPhi   = c1*^vortMn
        phi      = c2*^vortMn
        vortMn   = vort -^ n
        chi      = laplaceInv *^ theta
        -- for Poisson bracket
        ddxVort  = computeS $ deAl *^ ddx *^ vort :: FieldF
        ddyVort  = computeS $ deAl *^ ddy *^ vort :: FieldF
        ddxPhi   = computeS $ deAl *^ ddx *^ phi  :: FieldF
        ddyPhi   = computeS $ deAl *^ ddy *^ phi  :: FieldF
        poissonBr = (fft2d . computeS) (ifft2d ddxPhi *^ ifft2d ddyVort -^ ifft2d ddyPhi *^ ifft2d ddxVort)
        -- for nTheta
        nNL       = computeS $ deAl *^ n          :: FieldF
        thetaNL   = computeS $ deAl *^ theta      :: FieldF
        nTheta    = (fft2d . computeS)  (ifft2d nNL *^ ifft2d thetaNL)
        -- for \nabla n \dot \nabla \chi and (\nabla \chi) ^2
        ddxChi    = computeS $ deAl *^ ddx *^ chi :: FieldF
        ddyChi    = computeS $ deAl *^ ddy *^ chi :: FieldF
        ddxN      = computeS $ deAl *^ ddx *^ n :: FieldF
        ddyN      = computeS $ deAl *^ ddy *^ n :: FieldF
        nChi      = (fft2d . computeS) (ifft2d ddxN *^ ifft2d ddxChi +^ ifft2d ddyN *^ ifft2d ddyChi )
        twoChi    = (fft2d . computeS . R.map (*0.5)) (ifft2d ddxChi *^ ifft2d ddxChi +^ ifft2d ddyChi *^ ifft2d ddyChi )


-- Runge-Kutta-4 solver
-- Maybe I should generalise the ODE solver.
-- But for now, let see if it works
--     dt        rhs                   y0        y1
rk4 :: Double -> (StateD -> StateD) -> StateD -> StateD
rk4 dtInternal f y0  = applyFilter y1
  where k1 = f $ y0
        k2 = f $ y0 +^ R.map (dt2*) k1
        k3 = f $ y0 +^ R.map (dt2*) k2
        k4 = f $ y0 +^ R.map (dt1*) k3
        twoK2 = R.map (2*) k2
        twoK3 = R.map (2*) k3
        dt2 = dtInternal/2 :+ 0
        dt6 = dtInternal/6 :+ 0
        dt1 = dtInternal   :+ 0
        y1  = y0 +^ R.map (dt6*) (k1 +^ twoK2 +^ twoK3 +^ k4)
        applyFilter = if isCircleFilter then applyCircleFilter else id

-- experimental function
applyCircleFilter :: StateD -> StateD
applyCircleFilter st = R.traverse st id $ \f sh@(Z :. _ :. y :.x) ->
  let kMax = fromIntegral $ min nkx nky
      kx2  = fromIntegral . squear $ indexToK nx x
      ky2  = fromIntegral . squear $ indexToK ny y
      k    = sqrt $ kx2 + ky2
  in if k >= kMax then 0 else f sh




-- maybe I need to compute more steps in Delayed mode...
solve :: State -> IO State
solve st = computeP $ rk4 dt rhs (delay st)

solveN :: Int -> State -> IO State
solveN 0 s0 = return s0
solveN n s0 = solve s0  >>= solveN (n-1)


computeAll ::  IO ()
computeAll = do
  putStrLn "# Here is simulation state output"
  putStrLn "# time L2-norm"
  let loop n st = do
                     let t  = time !! (outputNumber - n)
                     en <- sumAllP . R.map mag2 $ st
                     if isNaN en then error "Simulation divergeed" else return ()
                     writeArrayToStorableFile (printf "results/state/st%.5f.dat" t) st
                     printf "%.5f %.5f\n" t en
                     if  n > 1 then solveN outputFr st >>= loop (n-1) else return ()

  loop outputNumber st0






