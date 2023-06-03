{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import RayTracer.Scene
import RayTracer.Output
import RayTracer.Camera
import RayTracer.Renderer

import Data.Time
import GHC.Conc (numCapabilities)

main :: IO ()
main = do
    -- exec time measurement (start)
    startTime <- getCurrentTime

    -- parameter
    let width           = 1920
        height          = 1080
        spp             = 7
        recursiveDepth  = 10
        outputFileName  = "output(" ++ show spp ++ "spp" ++ ").ppm"

    -- camera
    let lookAt      = origin
        lookFrom    = fromXYZ (13.0, 2.0, 3.0)
        up          = fromXYZ (0, 1.0, 0)
        vfov        = 20
        distToFocus = 10.0
        aperture    = 0.07
    -- let lookAt      = fromXYZ (0, 0, -2.0)
    --     lookFrom    = origin
    --     up          = fromXYZ (0, 1.0, 0)
    --     vfov        = 90
    --     distToFocus = norm $ lookFrom <-> lookAt 
    --     aperture    = 0.05
        camera      = createCamera width height spp lookFrom lookAt up vfov aperture distToFocus

    -- geometry (spheres)
    let seed    = 0
        spheres = createRandomSpheres seed
                  --createTestSpheres

    -- build scene data
    let scene = buildScene spheres recursiveDepth

    -- runtime threads num
    let nc = numCapabilities

    putStrLn $ "start rendering... (Running in " ++  if nc > 1 then show nc ++ " threads parallel)" else "serial)"
    putStrLn $ "sphere num in scene : " ++ show (length spheres)

    let image = render scene camera

    -- render and output
    outputImageByPPM outputFileName image

    -- exec time measurement (end)
    endTime <- getCurrentTime

    putStrLn $ "finished (elapsed time : " ++ show (diffUTCTime endTime startTime) ++ ")"
    putStrLn $ "saved result image to " ++ outputFileName
