module RayTracer.Renderer
(
    render,
    renderAnimation
) where

import RayTracer.Scene
import RayTracer.Camera
import RayTracer.Geometry
import RayTracer.Scatter
import RayTracer.Color
import RayTracer.Ray
import RayTracer.SamplingStrategy

import Data.List hiding (zipWith)
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Conc (numCapabilities)
import Prelude hiding (zipWith)
import RayTracer.Material (HitRecord)

importanceSampling :: StatefulGen genType m => Ray -> Double -> Int -> Color -> HitRecord -> ScatterRecord -> genType -> Scene -> m Color
importanceSampling ray tmax depth emit hr sr gen scene = if _isSpecular sr 
                                                        then do
                                                            res <- traceRay (_scatter sr) 0.001 tmax (depth - 1) gen scene
                                                            return $ zipWith (*) (_attenuation sr) res
                                                        else do
                                                            let point       = _point hr
                                                                normal      = _normal hr
                                                                lights      = _lights scene
                                                                hittablePDF = HittablePDF lights point
                                                                usingPDF  = if null (_list lights) then _pdf sr else MixturePDF hittablePDF (_pdf sr)
                                                            direction <- generateAlongPDF usingPDF gen
                                                            let scatter        = Ray point direction
                                                                mat            = _surfaceMat hr
                                                                correctedAtten = _attenuation sr .^ scatteringPDF mat ray hr scatter
                                                                invPDFVal      = 1.0 / value usingPDF direction
                                                            res <- traceRay scatter 0.001 tmax (depth - 1) gen scene
                                                            return $ emit <+> zipWith (*) correctedAtten res .^ invPDFVal

checkScatter :: StatefulGen genType m => Ray -> Double -> Int -> HitRecord -> genType -> Scene -> m Color
checkScatter ray tmax depth hr gen scene = do
                            let mat   = _surfaceMat hr
                                emit  = emitted mat (_u hr) (_v hr) (_point hr) (_frontFace hr)
                            msr <- scatter mat ray hr gen
                            case msr of
                              Just sr -> importanceSampling ray tmax depth emit hr sr gen scene
                              Nothing -> return emit

-- throw ray to the scene recursively
--{-# INLINE traceRay #-}
traceRay :: StatefulGen genType m => Ray -> Double -> Double -> Int -> genType -> Scene -> m Color
traceRay ray tmin tmax depth gen scene | depth <= 0 = return kBlack
                                       | otherwise  = case hit (_root scene) ray tmin tmax of
                                          Just hr -> checkScatter ray tmax depth hr gen scene
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
iteratePRNG :: RandomGen genType => Scene -> Camera -> Int -> Int -> (Color, genType) -> [Color]
iteratePRNG scene camera x y (e, gen) = correctNaN e : iteratePRNG scene camera x y (runStateGen gen (sample scene camera x y))

-- rendering one pixel by sampling "spp" times
--{-# INLINE renderPixel #-}
renderPixel :: Scene -> Camera -> Int -> Int -> Color
renderPixel scene camera x y = foldl1' (<+>) sampledColors .^ (1.0 / fromIntegral spp)
                  where height   = _height  camera
                        spp      = _spp     camera
                        seed     = x * height + y + 42 -- praying to the ANSWER OF ALL
                        prng     = mkStdGen seed
                        takeDrop = drop 1 . take (spp + 1)
                        sampledColors = takeDrop $ iteratePRNG scene camera x y (kBlack, prng)

-- rendering (split tasks parallel according to the number of runtime threads)
--{-# INLINE render #-}
render :: Scene -> Camera -> Image
render scene camera = array ((0, 0), (width - 1, height - 1)) $ runEval $ parListChunk splitNum rdeepseq [((x, y), perPixel scene camera x y) | x <- [0..width - 1], y <- [0..height - 1]]
                                 where width      = _width  camera
                                       height     = _height camera
                                       splitNum   = div (width * height) numCapabilities
                                       perPixel   = (((postProduction .) .) .) . renderPixel

-- rendering continuous scenes
renderAnimation :: [(Scene, Camera)] -> [Image]
renderAnimation (sc:scs) = render scene camera : renderAnimation scs
                        where scene  = fst sc
                              camera = snd sc
renderAnimation [] = []