module RayTracer.Renderer
(
    render
) where

import RayTracer.Scene
import RayTracer.Camera
import RayTracer.Geometry
import RayTracer.Material
import RayTracer.Color
import RayTracer.Ray
import RayTracer.SamplingStrategy

import Data.List hiding (zipWith)
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Conc (numCapabilities)
import Prelude hiding (zipWith)

-- throw ray to the scene recursively
--{-# INLINE traceRay #-}
traceRay :: StatefulGen genType m => Ray -> Double -> Double -> Int -> genType -> Scene -> m Color
traceRay ray tmin tmax depth gen scene | depth <= 0 = return kBlack
                                       | otherwise  = case hit (_graphRoot scene) ray tmin tmax of
                                          Just hr -> do
                                                    let mat = _surfaceMat hr
                                                        emit = emitted mat (_u hr) (_v hr) (_point hr) (_frontFace hr)
                                                    msr <- scatter mat ray hr gen
                                                    case msr of
                                                      Just sr -> do
                                                                 let point       = _point hr
                                                                     normal      = _normal hr
                                                                     --cosinePDF   = CosinePDF $ buildFromW normal
                                                                     light  = Dielectric 0
                                                                     target = XZRect 213.0 343.0 227.0 332.0 554.0 light
                                                                     hittablePDF = HittablePDF target point
                                                                 direction <- generateAlongPDF hittablePDF gen
                                                                 let scatter     = Ray point direction
                                                                     atten       = _attenuation sr .^ scatteringPDF mat ray hr scatter
                                                                     pdf         = value hittablePDF direction
                                                                 res <- traceRay scatter 0.001 tmax (depth - 1) gen scene
                                                                 return $ emit <+> (zipWith (*) atten res .^ (1.0 / pdf))
                                                      Nothing -> return emit
                                          Nothing -> return $ _background scene

-- sampling one time by random ray
--{-# INLINE sample #-}
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

--{-# INLINE iterateStatefulGen #-}
iterateStatefulGen :: RandomGen genType => Scene -> Camera -> Int -> Int -> (Color, genType) -> [Color]
iterateStatefulGen scene camera x y (e, gen) = e : iterateStatefulGen scene camera x y (runStateGen gen (sample scene camera x y))

-- rendering one pixel by sampling "spp" times
--{-# INLINE renderPixel #-}
renderPixel :: Scene -> Camera -> Int -> Int -> Color
renderPixel scene camera x y = foldl1' (<+>) sampledColors .^ (1.0 / fromIntegral spp)
                  where height   = _height  camera
                        spp      = _spp     camera
                        pixelIdx = x * height + y
                        gen      = mkStdGen pixelIdx
                        takeDrop = drop 1 . take (spp + 1)
                        -- NO parallelization (too much SPARKs)
                        sampledColors = takeDrop $ iterateStatefulGen scene camera x y (kBlack, gen)

-- rendering (split tasks parallel according to the number of runtime threads)
--{-# INLINE render #-}
render :: Scene -> Camera -> Image
render scene camera = array ((0, 0), (width - 1, height - 1)) $ runEval $ parListChunk splitNum rdeepseq [((x, y), perPixel scene camera x y) | x <- [0..width - 1], y <- [0..height - 1]]
                                 where width      = _width  camera
                                       height     = _height camera
                                       splitNum   = div (width * height) numCapabilities
                                       perPixel   = (((postProduction .) .) .) . renderPixel