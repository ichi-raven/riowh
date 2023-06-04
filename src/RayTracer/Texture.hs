{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Texture
(
    TextureType(..),
    value
) where

import RayTracer.Utility
import RayTracer.Ray
import RayTracer.Random

data TextureType = SolidColor
                 {
                   _color :: !Color
                 }
                 | Checker
                 {
                    _even   :: !Color,
                    _odd    :: !Color
                 }
                 deriving (Generic, NFData)


--{-# INLINE value #-}
value :: TextureType -> Double -> Double -> Point -> Color
value (SolidColor color) u v pos = color
value (Checker even odd) u v pos | sines < 0.0  = even
                                 | otherwise    = odd
                                 where (x, y, z) = toXYZ $ pos .^ (10.0)
                                       sines     = sin x * sin y * sin z