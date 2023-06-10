{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module RayTracer.Material where

import RayTracer.Utility
import RayTracer.Texture

data MaterialType = Lambertian
                {
                  _albedo :: !TextureType
                }
                | Metal
                {
                  _albedo  :: !TextureType,
                  _fuzz    :: !Double
                }
                | Dielectric
                {
                  _refIdx :: !Double
                }
                | Emitter
                {
                  _emitColor :: !TextureType
                }
                deriving (Generic, NFData)

data HitRecord = HitRecord
  {
    _point        :: !Point,
    _normal       :: !Direction,
    _t            :: !Double,
    _u            :: !Double,
    _v            :: !Double,
    _frontFace    :: !Bool,
    _surfaceMat   :: !MaterialType
  } deriving (Generic, NFData)