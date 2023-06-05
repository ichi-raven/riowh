{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Scene
(
    Scene(..),
    buildScene,
    createRandomSpheres,
    createTestSpheres,
    module RayTracer.Geometry
) where

import RayTracer.Color
import RayTracer.Texture
import RayTracer.Geometry
import RayTracer.Material
import RayTracer.Random

import Prelude hiding (zipWith)
import Data.List hiding (zipWith)

data Scene = Scene
  {
    _graphRoot      :: !HittableType,
    _recursiveDepth :: !Int
  } deriving (Generic, NFData)

-- eta reduction
buildScene :: [HittableType] -> Int -> Scene
buildScene objects = Scene (createBVH objects)

-- generate random spheres (amax - a) * (bmax - bmin) times
makeRandomSpheres :: StatefulGen genType m => Int -> Int -> Int -> (Int, Int) -> genType -> m [HittableType]
makeRandomSpheres a amax b (bmin, bmax) gen = do
                                            chooseMat <- uniformRM (0, 1.0) gen
                                            rx        <- uniformRM (0, 1.0) gen
                                            ry        <- uniformRM (0, 1.0) gen
                                            rColor    <- randomColor gen
                                            fuzz      <- uniformRM (0.0, 0.5) gen
                                            let center = fromXYZ (fromIntegral a + 0.9 * rx, 0.2, fromIntegral b + 0.9 * ry)
                                                offset = fromXYZ (4, 0.2, 0)
                                                addSphere = if norm (center <-> offset) > 0.9
                                                              then  if chooseMat < (0.8 :: Double)
                                                                    then
                                                                      let albedo = rColor
                                                                      in  [Sphere center 0.2 (Lambertian (SolidColor (zipWith (*) albedo albedo)))]
                                                                    else if chooseMat < (0.95 :: Double)
                                                                    then
                                                                      let albedo  = rColor .^ 0.5 <+> fromXYZ (0.5, 0.5, 0.5)
                                                                          texture = SolidColor albedo
                                                                      in  [Sphere center 0.2 (Metal texture fuzz)]
                                                                    else [Sphere center 0.2 (Dielectric 1.5)]
                                                              else []
                                            if a < amax
                                              then if b < bmax
                                                    then makeRandomSpheres a        amax  (b + 1) (bmin, bmax) gen >>= (\ret -> return $ addSphere ++ ret)
                                                    else makeRandomSpheres (a + 1)  amax  bmin    (bmin, bmax) gen >>= (\ret -> return $ addSphere ++ ret)
                                              else return addSphere

-- create random sphere scene
createRandomSpheres :: Int -> [HittableType]
createRandomSpheres seed = presetSpheres ++ randomSpheres
                        where   presetSpheres =
                                            [
                                              Sphere (fromXYZ (0,   -1000, 0))  1000  (Lambertian (Checker (fromXYZ (0.5, 0.5, 0.5)) kGreen)),
                                              Sphere (fromXYZ (0,     1.0, 0))  1.0   (Dielectric 1.5),
                                              Sphere (fromXYZ (-4.0,  1.0, 0))  1.0   (Lambertian (SolidColor (fromXYZ (0.4, 0.2, 0.1)))),
                                              Sphere (fromXYZ (4.0,   1.0, 0))  1.0   (Metal      (SolidColor (fromXYZ (0.7, 0.6, 0.5))) 0.0)
                                            ]
                                randomRange   = (-11, 11)
                                randomSpheres = runStateGen_ (mkStdGen seed) $ uncurry makeRandomSpheres randomRange (fst randomRange) randomRange

createTestSpheres :: [HittableType]
createTestSpheres =   [
                        Sphere (fromXYZ (0,     -100.5, -1.0))  100   (Lambertian (SolidColor kGreen)),
                        Sphere (fromXYZ (0,     0.1,    -2.0))  0.3   (Lambertian (SolidColor kRed)),
                        Sphere (fromXYZ (-1.0,  0,      -1.0))  0.4   (Lambertian (SolidColor kWhite)),
                        Sphere (fromXYZ (1.0,   0,      -1.0))  0.4   (Metal (SolidColor kBlue) 0.6),
                        Sphere (fromXYZ (-0.6,  1.8,    -1.7))  0.9   (Metal (SolidColor kWhite) 0.07),
                        Sphere (fromXYZ (0,     0,      -1.0))  0.45  (Dielectric 2.4)
                      ]