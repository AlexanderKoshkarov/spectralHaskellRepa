module Constants where


-- Model parameters
v0, rhoS, u0, nuS, invMu, dt, dhv :: (Floating a) => a

v0    = 0.353
u0    = 11.7
rhoS  = 0.1
nuS   = 0.1826
invMu = 491
dhv   = 0.0001
-----------------------

-- Numerical parameters
nkx, nky, nx, ny, outputNumber, outputFr :: Int
lx, ly :: (Floating a) => a

nkx = 63
nky = 63

nx  = 2*nkx+1
ny  = 2*nky+1

lx    = 5*pi
ly    = 5*pi

dt = 0.001
outputFr = 5
outputNumber = 3

time :: [Double]
time = ((* fromIntegral outputFr).(*dt)) <$> [0..]

kxMin, kyMin :: Double
[kxMin, kyMin] = (2*pi/) <$> [lx, ly]

isLinear, isCircleFilter :: Bool
isLinear       = False
isCircleFilter = False


