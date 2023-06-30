module Main where

import RayTracer.Animation
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
    let width           = 500
        height          = 500
        spp             = 100
        frame           = 10
        deltaTime       = 0.16
        recursiveDepth  = 10
        --outputFileName  = "output(" ++ show spp ++ "spp" ++ ").png"
        outputFileName  = "anim/output.png"
        infoFileName    = "info.txt" 


    -- build scene and camera
    let backgroundColor = kWhite
        --(scene, camera) = createCornellBoxScene width height spp recursiveDepth backgroundColor
        --(scene, camera) = createRandomSpheresScene 42 width height spp recursiveDepth backgroundColor
        --(scene, camera) = createTestSpheresScene width height spp recursiveDepth backgroundColor
        animation = createTestAnimatedScene width height spp frame deltaTime recursiveDepth backgroundColor
        objectNum = _objectNum $ fst $ head animation
        --objectNum = _objectNum scene
    putStrLn $ "object num in scene : " ++ show objectNum

    -- runtime threads num
    let nc = numCapabilities
    putStrLn $ "start rendering... (Running in " ++ if nc > 1 then show nc ++ " threads parallel)" else "serial)"

    -- rendering
    --let image = render scene camera
    let images = renderAnimation animation

    -- output
    --output outputFileName image
    outputSerialImages outputFileName images 

    -- exec time measurement (end)
    endTime <- getCurrentTime

    outputInfo startTime endTime infoFileName

    putStrLn $ "finished (elapsed time : " ++ show (diffUTCTime endTime startTime) ++ ")"
    putStrLn $ "saved result image to " ++ outputFileName
