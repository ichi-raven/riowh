{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Scatter 
(
    module RayTracer.Scatter,
    module RayTracer.Material
) where

import RayTracer.Utility
import RayTracer.Ray
import RayTracer.Random
import RayTracer.Texture
import RayTracer.Color (kBlack)
import RayTracer.Material
import RayTracer.SamplingStrategy

data ScatterRecord = ScatterRecord
  {
    _attenuation   :: !Color,
    _scatter       :: !Ray,
    _pdf           :: !PDF,
    _isSpecular    :: !Bool
  } deriving (Generic, NFData)

--{-# INLINE scatter #-}
scatter :: StatefulGen genType m => MaterialType -> Ray -> HitRecord -> genType -> m (Maybe ScatterRecord)
scatter (Lambertian albedo) ray hr gen = do
                                  rv <- randomCosineDirection gen
                                  let point     = _point hr
                                      normal    = _normal hr
                                      onb       = buildFromW normal 
                                      direction = normalize $ localPos onb rv
                                      sampled   = sample albedo (_u hr) (_v hr) point
                                      pdf       = CosinePDF $ buildFromW normal
                                  return $ Just $ ScatterRecord sampled (Ray point direction) pdf False

scatter (Metal albedo fuzz) ray hr gen = do
                                  v <- randomInUnitSphere gen
                                  let   point     = _point hr
                                        normal    = _normal hr
                                        reflected = reflect (normalize (_direction ray)) normal
                                        scattered = Ray point $ normalize (reflected <+> (v .^ fuzz))
                                        sampled   = sample albedo (_u hr) (_v hr) point
                                  return $ if (reflected .* normal) > 0
                                            then Just $ ScatterRecord sampled scattered Uniform True-- TODO
                                            else Nothing

scatter (Dielectric refIdx) ray hr gen = do
                              let attenuation   = fromXYZ (1.0, 1.0, 1.0)
                                  etaiOverEtat  = if _frontFace hr then 1.0 / refIdx else refIdx
                                  unitDir       = normalize (_direction ray)
                                  point         = _point hr
                                  normal        = _normal hr
                                  cosTheta      = min ((unitDir .^ (-1.0)) .* normal) 1.0
                                  sinTheta      = sqrt (1.0 - cosTheta * cosTheta)
                                  reflected     = Ray point $ normalize (reflect unitDir normal)
                                  refracted     = Ray point $ normalize (refract unitDir normal etaiOverEtat)
                                  reflectProb   = schlick cosTheta etaiOverEtat
                              r <- uniformRM (0.0, 1.0) gen
                              if etaiOverEtat * sinTheta > 1.0 || r < reflectProb
                                  then return $ Just $ ScatterRecord attenuation reflected Uniform True-- TODO
                                  else return $ Just $ ScatterRecord attenuation refracted Uniform True-- TODO

scatter (Emitter _) _ _ _ = return Nothing

scatteringPDF :: MaterialType -> Ray -> HitRecord -> Ray -> Double
scatteringPDF (Lambertian _) inRay hr scatteredRay = if cosine < 0.0 then 0.0 else cosine / pi
                                                where sdir    = normalize $ _direction scatteredRay
                                                      cosine  = _normal hr .* sdir
scatteringPDF (Metal _ _) inRay hr scatteredRay      = 1.0 -- TODO
scatteringPDF (Dielectric _) inRay hr scatteredRay   = 1.0 -- TODO
scatteringPDF (Emitter _) inRay hr scatteredRay = 1.0 -- TODO

-- could not use eta reduction
emitted :: MaterialType -> Double -> Double -> Point -> Bool -> Color
emitted (Emitter emitColor) u v p frontFace = if frontFace then sample emitColor u v p else kBlack
emitted _ _ _ _ _ = kBlack