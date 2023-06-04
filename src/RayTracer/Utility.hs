{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module RayTracer.Utility 
(
    module RayTracer.Utility,
    Generic, -- GHC.Generics (Generic)
    module Control.DeepSeq,
    module System.Random.Stateful,
    module Data.Array.Unboxed,
    module Data.Vec3,
    module Data.Bits,
    module Data.Word
) where

import System.Random.Stateful
import Data.Array.Unboxed
import Data.Vec3
import Data.Bits
import Data.Word
import Control.Parallel.Strategies
import GHC.Generics (Generic)
import Control.DeepSeq

deriving instance Generic CVec3
deriving instance NFData CVec3

type Color      = CVec3
type Point      = CVec3
type Direction  = CVec3
--type Image      = Array(Int, Int) Color
type Image      = UArray(Int, Int) Word32 

-- constant
kInfinity :: Double
kInfinity = 999999999.9

kPi :: Double
kPi = 3.1415926535897932385

-- utility function
--{-# INLINE getImageWidth #-}
getImageWidth :: Image -> Int
getImageWidth image = fst $ snd $ bounds image

--{-# INLINE getImageHeight #-}
getImageHeight :: Image -> Int
getImageHeight image = snd $ snd $ bounds image

--{-# INLINE deg2rad #-}
deg2rad :: Double -> Double
deg2rad deg = deg * kPi / 180.0

--{-# INLINE rad2deg #-}
rad2deg :: Double -> Double
rad2deg rad = rad / kPi * 180.0

-- too slow

-- toList :: CVec3 -> [Double]
-- toList src = [x, y, z]
--     where (x, y, z) = toXYZ src
-- mapCVec3 :: CVec3 -> (Double -> Double) -> CVec3
-- mapCVec3 src f = fromXYZ (f x, f y, f z)
--                 where (x, y, z) = toXYZ src

--{-# INLINE fill #-}
fill :: Double -> CVec3
fill val = fromXYZ (val, val, val)

--{-# INLINE reflect #-}
-- calc reflect vector
reflect :: Direction -> Direction -> Direction
reflect v n = v <-> (n .^ (2.0 * (v .* n)))

--{-# INLINE refract #-}
-- calc refract vector
refract :: Direction -> Direction -> Double -> Direction
refract uv n etaiOverEtat = rOutParallel <+> rOutPerp
                            where cosTheta      = (uv .^ (-1.0)) .* n
                                  rOutParallel  = (uv <+> (n .^ cosTheta)) .^ etaiOverEtat
                                  nrOut         = (norm rOutParallel)
                                  rOutPerp      = n .^ ((-1.0) * sqrt (1.0 - nrOut * nrOut))

--{-# INLINE schlick #-}
-- schlick's approximation
schlick :: Double -> Double -> Double
schlick cosine refIdx = r0sq + (1.0 - r0sq) * ((1.0 - cosine) ** 5.0)
                        where r0    = (1.0 - refIdx) / (1.0 + refIdx)
                              r0sq  = r0 * r0
