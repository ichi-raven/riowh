{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.Geometry
(
    HittableType(..),
    AABB,
    hit,
    createAABB,
    createBVH,
    module RayTracer.Ray
) where

import RayTracer.Ray
import RayTracer.Material

--import Control.Parallel
import Data.List hiding (zipWith)
import Prelude hiding (zipWith)

data BVHNode = BVHNode
  {
    _aabb   :: !AABB,
    _left   :: !HittableType,
    _right  :: !HittableType
  } deriving (Generic, NFData)

data HittableType =
  Sphere
  {
    _position :: !Point,
    _radius   :: !Double,
    _mat      :: !MaterialType
  }
  | BVH
  {
    _node :: !BVHNode
  } deriving (Generic, NFData)

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

{-# INLINE getSphereUV #-}
getSphereUV :: Point -> (Double, Double)
getSphereUV pos = (u, v)
                where (x, y, z) = toXYZ pos
                      phi       = atan2 z x
                      theta     = asin y
                      u         = 1.0 - (phi + kPi) / (2.0 * kPi)
                      v         = (theta + kPi / 2.0) / kPi

-- create sphere's axis-aligned bounding box
createAABB :: HittableType -> AABB
createAABB (Sphere pos radius _)  = AABB (pos <-> fill radius) (pos <+> fill radius)
createAABB (BVH node) = _aabb node

-- collision detection
-- improving efficiency here is very important
{-# INLINE hit #-}
hit :: HittableType -> Ray -> Double -> Double -> Maybe HitRecord
hit (Sphere position radius mat) ray tmin tmax = if discriminant < 0 || (t1 < tmin || t1 > tmax) && (t2 < tmin || t2 > tmax)
                                                  then Nothing
                                                  else Just $ HitRecord rpos normal t u v frontFace mat
                                            where oc            = _origin ray <-> position
                                                  rdir          = _direction ray
                                                  nrdir         = norm rdir 
                                                  a             = nrdir * nrdir
                                                  halfB         = oc .* rdir
                                                  noc           = norm oc
                                                  c             = noc * noc - radius * radius
                                                  discriminant  = halfB * halfB - a * c
                                                  sqrtd         = sqrt discriminant
                                                  t1            = (-halfB - sqrtd) / a
                                                  t2            = (-halfB + sqrtd) / a
                                                  t             = closer tmin tmax t1 t2
                                                  rpos          = ray `at` t
                                                  outwardNormal = (rpos <-> position) .^ (1.0 / radius)
                                                  frontFace     = (rdir .* outwardNormal) <= 0
                                                  normal        = if frontFace then outwardNormal else outwardNormal .^ (-1.0)
                                                  (u, v)        = getSphereUV outwardNormal

hit (BVH node) ray tmin tmax = if hitAABB aabb ray tmin tmax
                                then case hit left ray tmin tmax of
                                      (Just lhr)  -> case hit right ray tmin (_t lhr) of
                                                      (Just rhr) -> if (_t lhr) < (_t rhr) then Just lhr else Just rhr 
                                                      Nothing    -> Just lhr
                                      Nothing     -> hit right ray tmin tmax
                                else Nothing
                              where aabb  = _aabb  node
                                    left  = _left  node
                                    right = _right node

-- AABB
-- create surrounding two bounding box
surroundingAABB :: AABB -> AABB -> AABB
surroundingAABB (AABB minPos1 maxPos1) (AABB minPos2 maxPos2) = AABB small big
                                                              where small = zipWith min minPos1 minPos2
                                                                    big   = zipWith max maxPos1 maxPos2

-- check hit in AABB 
hitAABB :: AABB -> Ray -> Double -> Double -> Bool
hitAABB (AABB minPos maxPos) ray tmin tmax = inSlabs
                                    where (dx, dy, dz)  = toXYZ(_direction ray)
                                          invD          = fromXYZ(1.0 / dx, 1.0 / dy, 1.0 / dz)
                                          tmp0          = zipWith (*) invD $ zipWith (-) minPos (_origin ray)
                                          tmp1          = zipWith (*) invD $ zipWith (-) maxPos (_origin ray)
                                          t0s           = zipWith max (fill tmin) $ zipWith min tmp0 tmp1
                                          t1s           = zipWith min (fill tmax) $ zipWith max tmp0 tmp1
                                          (x, y, z)     = toXYZ (t1s <-> t0s)
                                          inSlabs       = x >= 0.0 && y >= 0.0 && z >= 0.0

-- BVH

-- -- sort AABB by specified axis
sortObjects :: [HittableType] -> Int -> [HittableType]
sortObjects objects axis = sortBy cmp objects
                        where cmp ht1 ht2 = case axis of
                                              0 -> compare lx rx
                                              1 -> compare ly ry
                                              2 -> compare lz rz
                                              _ -> undefined
                                              where
                                                laabb = createAABB ht1
                                                raabb = createAABB ht2
                                                (lx, ly, lz) = toXYZ $ _minPos laabb
                                                (rx, ry, rz) = toXYZ $ _minPos raabb
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n <= 0 = ([], xs)
splitAt' _ []          = ([], [])
splitAt' n (x:xs)      = (x:ys, zs)
    where (ys, zs) = splitAt' (n - 1) xs

createBVH :: [HittableType] -> HittableType
createBVH [object] = BVH $ BVHNode aabb object object
                  where aabb = createAABB object
createBVH [object1, object2] = BVH $ BVHNode (surroundingAABB laabb raabb) object1 object2
                            where laabb = createAABB object1
                                  raabb = createAABB object2
createBVH objects = BVH $ BVHNode (surroundingAABB laabb raabb) leftNode rightNode
                where axis    = length objects `mod` 3 -- random
                      sorted  = sortObjects objects axis
                      mid     = length sorted `div` 2
                      (leftObjects, rightObjects) = splitAt' mid sorted
                      leftNode  = createBVH leftObjects
                      rightNode = createBVH rightObjects
                      laabb = _aabb $ _node leftNode
                      raabb = _aabb $ _node rightNode