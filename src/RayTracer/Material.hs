module RayTracer.Material where 

import RayTracer.Utility

data MaterialType = Lambertian
                {
                  _albedo :: !Color
                }
                | Metal
                {
                  _albedo  :: !Color,
                  _fuzz    :: !Double
                }
                | Dielectric
                {
                  _refIdx :: !Double
                }
                deriving (Generic, NFData)

scatter :: StatefulGen genType m => MaterialType -> Ray -> HitRecord -> genType -> m (Maybe ScatterResult)
scatter (Lambertian albedo) ray hr gen = do
                                  v <- randomUnitVector gen
                                  let point     = _point hr
                                      normal    = _normal hr
                                      direction = normalize $ normal <+> v
                                  return $ Just $ ScatterResult albedo (Ray point direction)

scatter (Metal albedo fuzz) ray hr gen = do
                                  v <- randomInUnitSphere gen
                                  let   point     = _point hr
                                        normal    = _normal hr
                                        reflected = reflect (normalize (_direction ray)) normal
                                        scattered = Ray point $ normalize (reflected <+> (v .^ fuzz))
                                  return $ if (reflected .* normal) > 0
                                            then Just (ScatterResult albedo scattered)
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
                              --r <- uniformRM (0.0, 1.0) gen
                              if etaiOverEtat * sinTheta > 1.0 -- || r < reflectProb
                                  then return $ Just (ScatterResult attenuation reflected)
                                  else return $ Just (ScatterResult attenuation refracted)