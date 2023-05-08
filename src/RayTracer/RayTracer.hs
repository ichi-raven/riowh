module RayTracer.RayTracer
(
    render
) where

import RayTracer.Scene
import RayTracer.Camera
import RayTracer.Geometry
import RayTracer.Material
import RayTracer.Color
import RayTracer.Ray


import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Conc (numCapabilities)
import Prelude hiding (zipWith)

-- throw ray to the scene recursively
traceRay :: StatefulGen genType m => Ray -> Double -> Double -> Int -> genType -> Scene -> m Color
traceRay ray tmin tmax depth gen scene = case hit (_graphRoot scene) ray tmin tmax of
                                          Just hr | depth <= 0  ->  return kBlack
                                                  | otherwise   ->  do
                                                                    msr <- scatter (_surfaceMat hr) ray hr gen
                                                                    case msr of
                                                                      Just sr -> do
                                                                                 res <- traceRay (_scatter sr) 0.001 tmax (depth - 1) gen scene
                                                                                 return $ zipWith (*) (_attenuation sr) res
                                                                      Nothing -> return kBlack -- almost never happens
                                          Nothing ->  -- illumination (HACKY sky)
                                                    return $ (fromXYZ (1.0, 1.0, 1.0) .^ (1.0 - t)) <+> (fromXYZ (0.5, 0.7, 1.0) .^ t)
                                                    where unitDir   = normalize (_direction ray)
                                                          (_, y, _) = toXYZ unitDir
                                                          t         = 0.5 * (y  + 1.0)

-- sampling one time by random ray
sample :: StatefulGen genType m => Scene -> Camera -> Int -> Int -> genType -> m Color
sample scene camera x y gen = do
                              rx    <- uniformRM (-0.5, 0.5) gen
                              ry    <- uniformRM (-0.5, 0.5) gen
                              let width           = _width camera
                                  height          = _height camera
                                  recursiveDepth  = _recursiveDepth scene
                                  u = (rx + fromIntegral x) / (fromIntegral width - 1)
                                  v = (ry + fromIntegral y) / (fromIntegral height - 1)
                              ray <- getRay (1.0 - v) u camera gen
                              traceRay ray 0 kInfinity recursiveDepth gen scene

-- rendering one pixel by sampling "spp" times
renderPixel :: Scene -> Camera -> Int -> Int -> Color
renderPixel scene camera x y = foldr1 (<+>) sampledColors .^ (1.0 / fromIntegral spp)
                  where width    = _width camera
                        height   = _height camera
                        spp      = _spp scene
                        pixelIdx = x * height + y
                        -- NO parallelization (too much SPARKs)
                        sampledColors = [runStateGen_ (mkStdGen (pixelIdx + (sppIdx * width * height))) (sample scene camera x y) | sppIdx <- [0..spp]]


-- rendering (split tasks according to the number of runtime threads)
render :: Scene -> Camera -> Image
render scene camera = array ((0, 0), (width - 1, height - 1)) $ runEval $ parListChunk splitNum rdeepseq [((x, y), renderPixel scene camera x y) | x <- [0..width - 1], y <- [0..height - 1]]
                                 where width      = _width  camera
                                       height     = _height camera
                                       splitNum   = div (width * height) numCapabilities