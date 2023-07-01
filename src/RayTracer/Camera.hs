{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module RayTracer.Camera where

import System.Random.Stateful

import RayTracer.Ray
import RayTracer.Random

data Camera = Camera
  {
    _camPoint         :: !Point,
    _lowerLeftCorner  :: !Point,
    _horizontal       :: !Direction,
    _vertical         :: !Direction,
    _lensRadius       :: !Double,
    _basis            :: !ONB,
    _width            :: !Int,
    _height           :: !Int,
    _spp              :: !Int
  } deriving (Generic, NFData)

createCamera :: Int -> Int -> Int -> Point -> Point -> Direction -> Double -> Double -> Double -> Camera
createCamera width height spp lookFrom lookAt up vfov aperture focusDist = Camera pos llc vertical horizontal lensRadius onb width height spp
          where pos             = lookFrom
                theta           = deg2rad vfov
                h               = tan (theta / 2)
                viewportHeight  = 2.0 * h
                aspectRatio     = fromIntegral width / fromIntegral height
                viewportWidth   = aspectRatio * viewportHeight
                w               = normalize (lookFrom <-> lookAt)
                u               = normalize (up >< w)
                v               = w >< u
                onb             = ONB u v w 
                vertical        = v .^ (viewportHeight * focusDist)
                horizontal      = u .^ (viewportWidth  * focusDist)
                llc             = pos <-> (horizontal .^ 0.5) <-> (vertical .^ 0.5) <-> (w .^ focusDist)
                lensRadius      = 0.5 * aperture

-- return the ray corresponding to viewport's uv coord
--{-# INLINE getRay #-}
getRay :: StatefulGen genType m => Double -> Double -> Camera -> genType -> m Ray
getRay s t (Camera orig llc horizontal vertical lensRadius uvw _ _ _) gen = do
                        rdu <- randomInUnitDisk gen
                        let u           = _ub               uvw
                            v           = _vb               uvw
                            (x, y, _)   = toXYZ (rdu .^ lensRadius)
                            offset      = (u .^ x) <+> (v .^ y)
                        return $ Ray (orig <+> offset) (llc <+> (horizontal .^ s) <+> (vertical .^ t) <-> orig <-> offset)
