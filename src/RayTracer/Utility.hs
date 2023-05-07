{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module RayTracer.Utility 
(
    module RayTracer.Utility,
    module GHC.Generics (Generic),
    module Control.DeepSeq,
    module System.Random.Stateful,
    module Data.Array,
    module Data.Vec3,
    module Control.Parallel.Strategies,
    module Data.Text
) where

import System.Random.Stateful
import Data.Array
import Data.Vec3
import Control.Parallel.Strategies
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Text 


deriving instance Generic CVec3
deriving instance NFData CVec3

type Color      = CVec3
type Point      = CVec3
type Direction  = CVec3
type Image      = Array(Int, Int) Color
type Text       = Data.Text.Text

-- constant
kInfinity :: Double
kInfinity = 999999999.9

kPi :: Double
kPi = 3.1415926535897932385

kBlack :: Color
kBlack = origin

kRed :: Color
kRed = fromXYZ (0.8, 0.1, 0.1)

kGreen :: Color
kGreen = fromXYZ (0.1, 0.8, 0.1)

kBlue :: Color
kBlue = fromXYZ (0.25, 0.25, 0.8)

kWhite :: Color
kWhite = fromXYZ (0.9, 0.9, 0.9)

-- utility
getImageWidth :: Image -> Int
getImageWidth image = fst $ snd $ bounds image

getImageHeight :: Image -> Int
getImageHeight image = snd $ snd $ bounds image

deg2rad :: Double -> Double
deg2rad deg = deg * kPi / 180.0

rad2deg :: Double -> Double
rad2deg rad = rad / kPi * 180.0

toList :: CVec3 -> [Double]
toList src = [x] ++ [y] ++ [z]
    where (x, y, z) = toXYZ src

mapCVec3 :: CVec3 -> (Double -> Double) -> CVec3
mapCVec3 src f = fromXYZ (f x, f y, f z)
                where (x, y, z) = toXYZ src

fill :: Double -> CVec3
fill val = fromXYZ (val, val, val)

-- calc reflect vector
reflect :: Direction -> Direction -> Direction
reflect v n = v <-> (n .^ (2.0 * (v .* n)))

-- calc refract vector
refract :: Direction -> Direction -> Double -> Direction
refract uv n etaiOverEtat = rOutParallel <+> rOutPerp
                            where cosTheta      = (uv .^ (-1.0)) .* n
                                  rOutParallel  = (uv <+> (n .^ cosTheta)) .^ etaiOverEtat
                                  rOutPerp      = n .^ ((-1.0) * sqrt (1.0 - (norm rOutParallel) ** 2))

-- schlick's approximation
schlick :: Double -> Double -> Double
schlick cosine refIdx = r0sq + (1.0 - r0sq) * ((1.0 - cosine) ** 5.0)
                        where r0    = (1.0 - refIdx) / (1.0 + refIdx)
                              r0sq  = r0 * r0