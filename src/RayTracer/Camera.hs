{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

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
    _u                :: !Direction,
    _v                :: !Direction,
    _w                :: !Direction,
    _width            :: !Int,
    _height           :: !Int,
    _spp              :: !Int
  } deriving (Generic, NFData)

createCamera :: Int -> Int -> Int -> Point -> Point -> Direction -> Double -> Double -> Double -> Camera
createCamera width height spp lookFrom lookAt up vfov aperture focusDist = Camera pos llc vertical horizontal lensRadius u v w width height spp
          where pos             = lookFrom
                theta           = deg2rad vfov
                h               = tan (theta / 2)
                viewportHeight  = 2.0 * h
                aspectRatio     = fromIntegral width / fromIntegral height
                viewportWidth   = aspectRatio * viewportHeight
                w               = normalize (lookFrom <-> lookAt)
                u               = normalize (up >< w)
                v               = w >< u
                vertical        = v .^ (viewportHeight * focusDist)
                horizontal      = u .^ (viewportWidth  * focusDist)
                llc             = pos <-> (horizontal .^ 0.5) <-> (vertical .^ 0.5) <-> (w .^ focusDist)
                lensRadius      = 0.5 * aperture

-- return the ray corresponding to viewport's uv coord
{-# INLINE getRay #-}
getRay :: StatefulGen genType m => Double -> Double -> Camera -> genType -> m Ray
getRay s t camera gen = do
                        rdu <- randomInUnitDisk gen
                        let orig        = _camPoint         camera
                            llc         = _lowerLeftCorner  camera
                            horizontal  = _horizontal       camera
                            vertical    = _vertical         camera
                            lensRadius  = _lensRadius       camera
                            u           = _u                camera
                            v           = _v                camera  
                            (x, y, _)   = toXYZ (rdu .^ lensRadius)
                            offset      = (u .^ x) <+> (v .^ y)
                        return $ Ray (orig <+> offset) (llc <+> (horizontal .^ s) <+> (vertical .^ t) <-> orig <-> offset)
                        