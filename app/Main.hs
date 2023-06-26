module Main where

import RayTracer.Scene
import RayTracer.Output
import RayTracer.Camera
import RayTracer.Renderer
import RayTracer.Color (kBlack, kWhite)

import Data.Time
import GHC.Conc (numCapabilities)

main :: IO ()
main = do
    -- exec time measurement (start)
    startTime <- getCurrentTime

    -- parameter
    let width           = 1920
        height          = 1080
        spp             = 1024
        recursiveDepth  = 10
        outputFileName  = "output(" ++ show spp ++ "spp" ++ ").ppm"
        infoFileName    = "info.txt" 

    -- build scene and camera
    let backgroundColor = kWhite
        (scene, camera) = createCornellBoxScene width height spp recursiveDepth backgroundColor
        --(scene, camera) = createRandomSpheresScene 42 width height spp recursiveDepth backgroundColor
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

    outputInfo startTime endTime infoFileName

    putStrLn $ "finished (elapsed time : " ++ show (diffUTCTime endTime startTime) ++ ")"
    putStrLn $ "saved result image to " ++ outputFileName
