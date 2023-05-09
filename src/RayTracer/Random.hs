module RayTracer.Random 
(
    module RayTracer.Random,
    module System.Random.Stateful
) where

import System.Random.Stateful

import RayTracer.Utility

randomColor :: StatefulGen genType m => genType -> m Color
randomColor gen = do
                  r <- uniformRM (0.0, 1.0) gen
                  g <- uniformRM (0.0, 1.0) gen
                  b <- uniformRM (0.0, 1.0) gen
                  return $ fromXYZ (r, g, b)

randomUnitVector :: StatefulGen genType m => genType -> m Point
randomUnitVector gen = do
                      a     <- uniformRM (0.0,  2.0 * kPi) gen
                      z     <- uniformRM (-1.0, 1.0)       gen
                      let r = sqrt (1 - z ** 2)
                      return $ fromXYZ (r * cos a, r * sin a, z)

randomInUnitSphere :: StatefulGen genType m => genType -> m Point
randomInUnitSphere gen = do
                          r     <- uniformRM (0, 1.0)                 gen
                          theta <- uniformRM (0, 2.0 * kPi)           gen
                          phi   <- uniformRM (-0.5 * kPi, 0.5 * kPi)  gen
                          return $ fromXYZ (r * sin theta * cos phi, r * sin theta * sin phi, r * cos theta)
                         -- rejection method
--                       rx <- (uniformRM (-1.0, 1.0) gen)
--                       ry <- (uniformRM (-1.0, 1.0) gen)
--                       rz <- (uniformRM (-1.0, 1.0) gen)
--                       let rv = fromXYZ(rx, ry, rz)
--                       if norm rv >= 1 then (randomInUnitSphere gen) else return rv

randomInUnitHemisphere :: StatefulGen genType m => genType -> Direction -> m Point
randomInUnitHemisphere gen normal = do
                              inUnitSphere <- randomInUnitSphere gen
                              return $ if inUnitSphere .* normal > 0 then inUnitSphere else inUnitSphere .^ (-1.0)

randomInUnitDisk :: StatefulGen genType m => genType -> m Point
randomInUnitDisk gen = do
                        r     <- uniformRM (0, 1.0)       gen
                        theta <- uniformRM (0, 2.0 * kPi) gen
                        return $ fromXYZ (r * cos theta, r * sin theta, 0)