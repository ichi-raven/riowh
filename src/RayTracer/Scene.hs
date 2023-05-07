module RayTracer.Scene
(
    Scene,
    buildScene,
    module Raytracer.BVH
) where

import RayTracer.BVH

data Scene = Scene
  {
    _graphRoot      :: !HittableType,
    _spp            :: !Int,
    _recursiveDepth :: !Int
  } deriving (Generic, NFData)

-- eta reduce
buildScene :: [HittableType] -> Int -> Int -> Scene
buildScene objects spp recursiveDepth = Scene (createBVH objects) spp recursiveDepth