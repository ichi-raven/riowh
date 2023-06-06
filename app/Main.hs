{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import RayTracer.Scene
import RayTracer.Output
import RayTracer.Camera
import RayTracer.Renderer

import Data.Time
import GHC.Conc (numCapabilities)
import RayTracer.Color (kBlack)

main :: IO ()
main = do
    -- exec time measurement (start)
    startTime <- getCurrentTime

    -- rendering parameter
    let width           = 500
        height          = 500
        spp             = 300
        recursiveDepth  = 10
        outputFileName  = "output(" ++ show spp ++ "spp" ++ ").ppm"

    -- camera
    -- let lookAt      = origin
    --     lookFrom    = fromXYZ (13.0, 2.0, 3.0)
    --     up          = fromXYZ (0, 1.0, 0)
    --     vfov        = 20
    --     distToFocus = 10.0
    --     aperture    = 0.07
    -- let lookAt      = fromXYZ (0, 0, -1.0)
    --     lookFrom    = origin
    --     up          = fromXYZ (0, 1.0, 0)
    --     vfov        = 90
    --     distToFocus = norm $ lookFrom <-> lookAt 
    --     aperture    = 0.02
    let lookAt      = fromXYZ (278.0, 278.0, 0)
        lookFrom    = fromXYZ (278.0, 278.0, -800.0)
        up          = fromXYZ (0, 1.0, 0)
        vfov        = 40
        distToFocus = norm $ lookFrom <-> lookAt 
        aperture    = 0.0
        camera      = createCamera width height spp lookFrom lookAt up vfov aperture distToFocus

    -- build scene data
    let seed            = 42
        backgroundColor = kBlack
        scene = createEmptyCornellBoxScene recursiveDepth backgroundColor
    putStrLn $ "object num in scene : " ++ show (_objectNum scene)

    -- runtime threads num
    let nc = numCapabilities
    putStrLn $ "start rendering... (Running in " ++ if nc > 1 then show nc ++ " threads parallel)" else "serial)"

    -- rendering
    let image = render scene camera

    -- output
    outputImageByPPM outputFileName image

    -- exec time measurement (end)
    endTime <- getCurrentTime

    outputInfo startTime endTime "info.txt"

    putStrLn $ "finished (elapsed time : " ++ show (diffUTCTime endTime startTime) ++ ")"
    putStrLn $ "saved result image to " ++ outputFileName
