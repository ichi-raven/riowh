
module RayTracer.Random
(
    module RayTracer.Random,
    module System.Random.Stateful
) where

import System.Random.Stateful

import RayTracer.Utility

--{-# INLINE randomColor #-}
randomColor :: StatefulGen genType m => genType -> m Color
randomColor gen = do
                  r <- uniformRM (0.0, 1.0) gen
                  g <- uniformRM (0.0, 1.0) gen
                  b <- uniformRM (0.0, 1.0) gen
                  return $ fromXYZ (r, g, b)

--{-# INLINE randomUnitVector #-}
randomUnitVector :: StatefulGen genType m => genType -> m Point
randomUnitVector gen = do
                      a     <- uniformRM (0.0,  2.0 * kPi) gen
                      z     <- uniformRM (-1.0, 1.0)       gen
                      let r = sqrt (1 - z * z)
                      return $ fromXYZ (r * cos a, r * sin a, z)

--{-# INLINE randomInUnitSphere #-}
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

--{-# INLINE randomInUnitHemisphere #-}
randomInUnitHemisphere :: StatefulGen genType m => genType -> Direction -> m Point
randomInUnitHemisphere gen normal = do
                              inUnitSphere <- randomInUnitSphere gen
                              return $ if inUnitSphere .* normal > 0 then inUnitSphere else inUnitSphere .^ (-1.0)

--{-# INLINE randomInUnitDisk #-}
randomInUnitDisk :: StatefulGen genType m => genType -> m Point
randomInUnitDisk gen = do
                        r     <- uniformRM (0, 1.0)       gen
                        theta <- uniformRM (0, 2.0 * kPi) gen
                        return $ fromXYZ (r * cos theta, r * sin theta, 0)

randomCosineDirection :: StatefulGen genType m => genType -> m Direction
randomCosineDirection gen = do
                            r1 <- uniformRM (0, 1.0) gen
                            r2 <- uniformRM (0, 1.0) gen
                            let z   = sqrt (1 - r2)
                                phi = 2 * kPi * r1
                                x   = cos phi * sqrt r2
                                y   = sin phi * sqrt r2
                            return $ fromXYZ (x, y, z)

randomToSphere :: StatefulGen genType m => Double -> Double -> genType -> m Direction
randomToSphere radius distanceSquared gen = do 
                                        r1 <- uniformRM (0.0, 1.0) gen
                                        r2 <- uniformRM (0.0, 1.0) gen
                                        let z   = 1.0 + r2 * (sqrt (1.0 - radius * radius / distanceSquared) - 1.0)
                                            phi = 2 * kPi * r1
                                            sq = sqrt (1.0 - z * z)
                                            x = cos phi * sq
                                            y = sin phi * sq
                                        return $ fromXYZ (x, y, z)