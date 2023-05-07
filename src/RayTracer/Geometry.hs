module RayTracer.Geometry
(
    HittableType(..),
    AABB,
    hit,
    createAABB,
    module RayTracer.Ray
) where

import RayTracer.Ray

data AABB = AABB
  {
    _minPos :: !Point,
    _maxPos :: !Point
  } deriving (Generic, NFData)

-- return the closer t under the assumption that one of t1 and t2 is always inside [tmin, tmax]
closer :: Double -> Double -> Double -> Double -> Double
closer tmin tmax t1 t2 = if tmin < t1 && tmax > t1
                            then (if tmin < t2 && tmax > t2 then min t1 t2 else t1)
                            else t2

-- create sphere's axis-aligned bounding box
createAABB :: HittableType -> AABB
createAABB (Sphere pos radius _)  = AABB (pos <-> fill radius) (pos <+> fill radius)
createAABB (BVH node) = _aabb node

-- sphere's collision detection
hit :: HittableType -> Ray -> Double -> Double -> Maybe HitRecord
hit (Sphere position radius mat) ray tmin tmax = if discriminant < 0 || (t1 < tmin || t1 > tmax) && (t2 < tmin || t2 > tmax)
                                                  then Nothing
                                                  else Just $ HitRecord rpos normal t frontFace mat
                                            where oc            = _origin ray <-> position
                                                  rdir          = _direction ray
                                                  a             = norm rdir ** 2
                                                  halfB         = oc .* rdir
                                                  c             = norm oc ** 2 - radius ** 2
                                                  discriminant  = halfB * halfB - a * c
                                                  sqrtd         = sqrt discriminant
                                                  t1            = (-halfB - sqrtd) / a
                                                  t2            = (-halfB + sqrtd) / a
                                                  t             = closer tmin tmax t1 t2
                                                  rpos           = ray `at` t
                                                  outwardNormal = (rpos <-> position) .^ (1.0 / radius)
                                                  frontFace     = (rdir .* outwardNormal) <= 0
                                                  normal        = if frontFace then outwardNormal else outwardNormal .^ (-1.0)

hit (BVH node) ray tmin tmax = if hitAABB aabb ray tmin tmax
                                then case hit left ray tmin tmax of
                                      (Just lhr)  -> case hit right ray tmin (_t lhr) of
                                                      (Just rhr) -> Just rhr -- either left or right
                                                      Nothing    -> Just lhr
                                      Nothing     -> hit right ray tmin tmax
                                else Nothing
                              where aabb  = _aabb  node
                                    left  = _left  node
                                    right = _right node