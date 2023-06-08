{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Texture
(
    TextureType(..),
    sample
) where

import RayTracer.Utility
import RayTracer.Ray
import RayTracer.Random
import RayTracer.Color

-- import Data.Ord (clamp)
-- import Codec.Picture

data TextureType = SolidColor
                 {
                   _color :: !Color
                 }
                 | Checker
                 {
                    _even       :: !Color,
                    _odd        :: !Color,
                    _checkSize  :: !Double
                 } deriving (Generic, NFData)
                --  | ImageSample
                --  {
                --     _image      :: Image
                --  }
                --  deriving (Generic, NFData)

{-# INLINE clamp' #-}
clamp' :: (Double, Double) -> Double -> Double
clamp' (low, high) a = min high (max a low)

--{-# INLINE value #-}
sample :: TextureType -> Double -> Double -> Point -> Color
sample (SolidColor color) _ _ _ = color
sample (Checker even odd size) u v pos | sines < 0.0  = even
                                 | otherwise    = odd
                                 where (x, y, z) = toXYZ $ pos .^ size
                                       sines     = sin x * sin y * sin z
-- value (ImageSample image) u v _ = sampledColor
--                                 where (cu, cv)  = (clamp' (0.0, 1.0) u, 1.0 - clamp' (0.0, 1.0) v)
--                                       (width, height) = snd $ bounds image
--                                       (i, j)    = (round u * width, round v * height)
--                                       (ti, tj)  = (if i >= width then i - 1 else i, if j >= height then j - 1 else j)
--                                       (sr, sg, sb, _) = unpackR8G8B8A8 $ image ! (ti, tj)
--                                       sampledColor    = fromXYZ (fromIntegral sr / 255.0, fromIntegral sg / 255.0, fromIntegral sb / 255.0)

-- bitmap2Image :: Image PixelRGBA8 -> RayTracer.Utility.Image
-- bitmap2Image src = array ((0, 0), (width - 1, height - 1)) $ runEval $ parListChunk splitNum rdeepseq [((x, y), pixelAt srcimage x y) | x <- [0..width - 1], y <- [0..height - 1]]
--                 where width     = imageWidth src
--                       height    = imageHeight src
--                       srcimage  = imageData src