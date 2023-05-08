{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import RayTracer.Scene
import RayTracer.Output
import RayTracer.Camera
import Data.Time

import GHC.Conc (numCapabilities)
import RayTracer.RayTracer (render)

main :: IO ()
main = do
      -- exec time measurement (start)
    startTime <- getCurrentTime

    -- parameter
    let width           = 640
        height          = 480
        spp             = 512
        recursiveDepth  = 25
        outputFileName  = "output.ppm"

    -- camera
    let lookAt      = origin
        lookFrom    = fromXYZ (13.0, 2.0, 3.0)
        up          = fromXYZ (0, 1.0, 0)
        vfov        = 20
        distToFocus = 10.0
        aperture    = 0.08
        camera      = createCamera width height lookFrom lookAt up vfov aperture distToFocus

    -- geometry (spheres)
    let spheres = createRandomSpheres 42
    
    let scene = buildScene spheres spp recursiveDepth

    -- runtime threads num
    let nc = numCapabilities
    putStrLn $ "start rendering... (Running in " ++  if nc > 1 then show nc ++ " threads parallel)" else "serial)"
    putStrLn $ "sphere num in scene : " ++ show (length spheres)

    -- rendering and output
    outputImageByPPM outputFileName $ render scene camera

    -- exec time measurement (end)
    endTime <- getCurrentTime

    putStrLn $ "finished (elapsed time : " ++ show (diffUTCTime endTime startTime) ++ ")"
