{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Scene
(
    Scene(..),
    buildScene,
    createRandomSpheresScene,
    createTestSpheresScene,
    createSimpleLightScene,
    createCornellBoxScene,
    module RayTracer.Geometry
) where

import RayTracer.Color
import RayTracer.Texture
import RayTracer.Geometry
import RayTracer.Material
import RayTracer.Random
import RayTracer.Camera

import Prelude hiding (zipWith)
import Data.List hiding (zipWith)

data Scene = Scene
  {
    _graphRoot      :: !HittableType,
    _objectNum      :: !Int,  
    _recursiveDepth :: !Int,
    _background     :: !Color
  } deriving (Generic, NFData)

-- eta reduction
buildScene :: [HittableType] -> Int -> Color -> Scene
buildScene objects = Scene (createBVH objects) (length objects)

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
createRandomSpheresScene :: Int -> Int -> Int -> Int -> Int -> Color -> (Scene, Camera)
createRandomSpheresScene seed width height spp recursiveDepth background = (buildScene objects recursiveDepth background, camera)
                        where   objects       = presetSpheres ++ randomSpheres
                                presetSpheres =
                                            [
                                              Sphere (fromXYZ (0,   -1000, 0))  1000  (Lambertian (Checker (fromXYZ (0.5, 0.5, 0.5)) kGreen 10.0)),
                                              Sphere (fromXYZ (0,     1.0, 0))  1.0   (Dielectric 1.5),
                                              Sphere (fromXYZ (-4.0,  1.0, 0))  1.0   (Lambertian (SolidColor (fromXYZ (0.4, 0.2, 0.1)))),
                                              Sphere (fromXYZ (4.0,   1.0, 0))  1.0   (Metal      (SolidColor (fromXYZ (0.7, 0.6, 0.5))) 0.0)
                                            ]
                                randomRange   = (-11, 11)
                                randomSpheres = runStateGen_ (mkStdGen seed) $ uncurry makeRandomSpheres randomRange (fst randomRange) randomRange
                                lookAt      = origin
                                lookFrom    = fromXYZ (13.0, 2.0, 3.0)
                                up          = fromXYZ (0, 1.0, 0)
                                vfov        = 20
                                distToFocus = 10.0
                                aperture    = 0.07
                                camera = createCamera width height spp lookFrom lookAt up vfov aperture distToFocus

createTestSpheresScene :: Int -> Int -> Int -> Int -> Color -> (Scene, Camera)
createTestSpheresScene width height spp recursiveDepth background = (buildScene objects recursiveDepth background, camera)
                    where objects = [
                                      Sphere (fromXYZ (0,     -100.5, -1.0))  100   (Lambertian (SolidColor kGreen)),
                                      Sphere (fromXYZ (0,     0.1,    -2.0))  0.3   (Lambertian (SolidColor kRed)),
                                      Sphere (fromXYZ (-1.0,  0,      -1.0))  0.4   (Lambertian (SolidColor kWhite)),
                                      Sphere (fromXYZ (1.0,   0,      -1.0))  0.4   (Metal (SolidColor kBlue) 0.6),
                                      Sphere (fromXYZ (-0.6,  1.8,    -1.7))  0.9   (Metal (SolidColor kWhite) 0.07),
                                      Sphere (fromXYZ (0,     0,      -1.0))  0.45  (Dielectric 2.4)
                                    ]
                          lookAt      = fromXYZ (0, 0, -1.0)
                          lookFrom    = origin
                          up          = fromXYZ (0, 1.0, 0)
                          vfov        = 90
                          distToFocus = norm $ lookFrom <-> lookAt 
                          aperture    = 0.02
                          camera = createCamera width height spp lookFrom lookAt up vfov aperture distToFocus

createSimpleLightScene :: Int -> Int -> Int -> Int -> Color -> (Scene, Camera)
createSimpleLightScene width height spp recursiveDepth background = (buildScene objects recursiveDepth background, camera)
                    where objects = [
                                      Sphere (fromXYZ (0,   -1000, 0))  1000  (Lambertian (Checker (fromXYZ (0.5, 0.5, 0.5)) kGreen 10.0)),
                                      Sphere (fromXYZ (0,   2.0, 0))    1.0   (Lambertian (SolidColor kBlue)),
                                      Sphere (fromXYZ (0,   7.0, 0))    2.0   (DiffuseLight (SolidColor (fromXYZ (4.0, 4.0, 4.0)))),
                                      XYRect 3.0 5.0 1.0 3.0 (-2.0) (DiffuseLight (SolidColor (fromXYZ (4.0, 4.0, 4.0))))
                                    ]
                          lookAt      = fromXYZ (13.0, 2.0, 3.0)
                          lookFrom    = fromXYZ (13.0, 4.0, 0.0)
                          up          = fromXYZ (0, 1.0, 0)
                          vfov        = 20
                          distToFocus = norm $ lookFrom <-> lookAt
                          aperture    = 0.07
                          camera = createCamera width height spp lookFrom lookAt up vfov aperture distToFocus

createCornellBoxScene :: Int -> Int -> Int -> Int -> Color -> (Scene, Camera)
createCornellBoxScene width height spp recursiveDepth background = (buildScene objects recursiveDepth background, camera)
                    where objects = [
                                      YZRect 0 555.0 0 555.0 555.0  green,
                                      YZRect 0 555.0 0 555.0 0      red,
                                      XZRect 213.0 343.0 227.0 332.0 554.0 light,
                                      XZRect 0 555.0 0 555.0 0 white, -- floor
                                      XZRect 0 555.0 0 555.0 555.0 white,
                                      XYRect 0 555.0 0 555.0 555.0 white,
                                      -- Sphere (fromXYZ (150, 100.0, 230)) 100.0 metal,
                                      -- Sphere (fromXYZ (390, 100.0, 230)) 100.0 dielectric
                                      createBox (fromXYZ (200, 10, 170)) (fromXYZ (390, 200, 230)) dielectric
                                    ]
                          red   = Lambertian    $ SolidColor $ fromXYZ (0.65, 0.05, 0.05)
                          white = Lambertian    $ SolidColor $ fromXYZ (0.73, 0.73, 0.73)
                          green = Lambertian    $ SolidColor $ fromXYZ (0.12, 0.45, 0.15)
                          light = DiffuseLight  $ SolidColor $ fromXYZ (15.0, 15.0, 15.0)
                          metal = Metal (SolidColor kBlue) 0.6
                          dielectric = Dielectric 1.5

                          lookAt      = fromXYZ (278.0, 278.0, 0)
                          lookFrom    = fromXYZ (278.0, 278.0, -800.0)
                          up          = fromXYZ (0, 1.0, 0)
                          vfov        = 40
                          distToFocus = norm $ lookFrom <-> lookAt
                          aperture    = 0.0
                          camera      = createCamera width height spp lookFrom lookAt up vfov aperture distToFocus
