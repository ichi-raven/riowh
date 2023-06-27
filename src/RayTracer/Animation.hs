{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Animation 
(
    createTestAnimatedScene
)
where

import RayTracer.Color
import RayTracer.Texture
import RayTracer.Geometry
import RayTracer.Scatter
import RayTracer.Random
import RayTracer.Camera
import RayTracer.Scene

-- [AnimateObject] -> [[Object], [Object], [Object]...] -> [Scene, Scene, Scene...] -> [Image, Image, Image...]

data AnimateObject = AnimateObject
    {
        _object     :: !HittableType,
        _vel        :: !CVec3,
        _acc        :: !CVec3,
        _isLight    :: !Bool
    }

-- 

splitWhen' :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen' d (x:xs) = if d x then ([x] ++ t, f) else (t, [x] ++ f)
                where  (t, f) = splitWhen' d xs
splitWhen' _ [] = ([], [])
                        

translate :: Int -> Double -> AnimateObject -> HittableType
translate frame deltaTime (AnimateObject object vel acc _) = Translate object offset
                                    where   time    = fromIntegral frame * deltaTime
                                            nvel    = vel <+> (acc  .^ time)
                                            offset  = (nvel .^ time)
                                    
transform :: Int -> Double -> [AnimateObject] -> ([HittableType], [HittableType])
transform frame deltaTime animObjects = (transLights ++ transOthers, transLights)
                                    where trans              = translate frame deltaTime
                                          (lights, others)   = splitWhen' _isLight animObjects
                                          transLights = map trans lights
                                          transOthers = map trans others

animate :: Int -> Double -> [AnimateObject] -> [([HittableType], [HittableType])]
animate frame deltaTime objects | frame >= 0 = animate (frame - 1) deltaTime objects ++ [transform (frame - 1) deltaTime objects]
                                | otherwise  = []

buildAnimatedScene :: [AnimateObject] -> Int -> Double -> Int -> Color -> [Scene]
buildAnimatedScene animObjects frame deltaTime recursiveDepth backGroundColor = [Scene (createBVH objects) (List lights) (length objects) recursiveDepth backGroundColor | (objects, lights) <- animated]
                                                                            where animated = animate frame deltaTime animObjects
                                                                                    


createTestAnimatedScene :: Int -> Int -> Int -> Int -> Double -> Int -> Color -> [(Scene, Camera)]
createTestAnimatedScene width height spp frame deltaTime recursiveDepth background = sceneAndCameras
                    where objects = 
                                    [
                                      AnimateObject (Sphere (fromXYZ (0,     -100.5, -1.0))  100   (Lambertian (SolidColor kGreen)))  origin origin False,
                                      AnimateObject (Sphere (fromXYZ (0,     0.1,    -2.0))  0.3   (Lambertian (SolidColor kRed)))    xvel origin False,
                                      AnimateObject (Sphere (fromXYZ (-1.0,  0,      -1.0))  0.4   (Lambertian (SolidColor kWhite)))  xvel origin False,
                                      AnimateObject (Sphere (fromXYZ (1.0,   0,      -1.0))  0.4   (Metal (SolidColor kBlue) 0.6))    yvel origin False,
                                      AnimateObject (Sphere (fromXYZ (-0.6,  1.8,    -1.7))  0.9   (Metal (SolidColor kWhite) 0.07))  origin origin False,
                                      AnimateObject (Sphere (fromXYZ (0,     0,      -1.0))  0.45  (Dielectric 2.4))                  xvel origin False
                                    ]
                          xvel        = fromXYZ (0.5, 0, 0)
                          yvel        = fromXYZ (0, 0.5, 0)
                          lookAt      = fromXYZ (0, 0, -1.0)
                          lookFrom    = origin
                          up          = fromXYZ (0, 1.0, 0)
                          vfov        = 90
                          distToFocus = norm $ lookFrom <-> lookAt
                          aperture    = 0.02
                          camera = createCamera width height spp lookFrom lookAt up vfov aperture distToFocus
                          scenes = buildAnimatedScene objects frame deltaTime recursiveDepth background
                          sceneAndCameras = zip scenes $ take (length scenes) $ repeat camera