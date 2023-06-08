{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Ray
(
    Ray(..),
    at,
    module RayTracer.Utility
) where 

import RayTracer.Utility

data Ray = Ray
  {
    _origin     :: Point,
    _direction  :: Direction
  } deriving (Generic, NFData)

--{-# INLINE at #-}
at :: Ray -> Double -> Point
at ray t = _origin ray <+> (_direction ray .^ t)