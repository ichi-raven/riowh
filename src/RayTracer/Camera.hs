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
    _width            :: !Int,
    _height           :: !Int
  } deriving (Generic, NFData)

createCamera :: Int -> Int -> Point -> Point -> Direction -> Double -> Double -> Double -> Camera
createCamera width height lookFrom lookAt up vfov aperture focusDist = Camera pos llc vertical horizontal lensRadius width height
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
                lensRadius      = aperture / 2.0

-- return the ray corresponding to viewport's uv coord
getRay :: StatefulGen genType m => Double -> Double -> Camera -> genType -> m Ray
getRay u v camera gen = do
                        rdu <- randomInUnitDisk gen
                        let orig        = _camPoint         camera
                            llc         = _lowerLeftCorner  camera
                            horizontal  = _horizontal       camera
                            vertical    = _vertical         camera
                            lensRadius  = _lensRadius       camera
                            (x, y, _)   = toXYZ (rdu .^ lensRadius)
                            xv          = fromXYZ (x, 0, 0)
                            yv          = fromXYZ (0, y, 0)
                            offset      = (xv .^ u) <+> (yv .^ v)
                        return $ Ray (orig <+> offset) (normalize (llc <+> (horizontal .^ u) <+> (vertical .^ v) <-> orig <-> offset))