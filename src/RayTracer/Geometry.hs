{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module RayTracer.Geometry
(
    HittableType(..),
    AABB,
    hit,
    createAABB,
    createBVH,
    createBox,
    module RayTracer.Ray
) where

import RayTracer.Ray
import RayTracer.Material

--import Control.Parallel
import Data.List hiding (zipWith)
import Prelude hiding (zipWith)

data HittableType =
  Sphere
  {
    _position :: !Point,
    _radius   :: !Double,
    _mat      :: !MaterialType
  }
  | XYRect
  {
    _x0       :: !Double,
    _x1       :: !Double,
    _y0       :: !Double,
    _y1       :: !Double,
    _k        :: !Double,
    _mat      :: !MaterialType
  }
  | XZRect
  {
    _x0       :: !Double,
    _x1       :: !Double,
    _z0       :: !Double,
    _z1       :: !Double,
    _k        :: !Double,
    _mat      :: !MaterialType
  }
  | YZRect
  {
    _y0       :: !Double,
    _y1       :: !Double,
    _z0       :: !Double,
    _z1       :: !Double,
    _k        :: !Double,
    _mat      :: !MaterialType
  }
  | Box
  {
    _aabb   :: !AABB,
    _sides  :: ![HittableType]
  }
  | BVH
  {
    _aabb   :: !AABB,
    _left   :: !HittableType,
    _right  :: !HittableType
  } deriving (Generic, NFData)

data AABB = AABB
  {
    _minPos :: !Point,
    _maxPos :: !Point
  } deriving (Generic, NFData)

-- return the closer t under the assumption that one of t1 and t2 is always inside [tmin, tmax]
{-# INLINE closer #-}
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


createBox :: Point -> Point -> MaterialType -> HittableType
createBox minPos maxPos mat = Box (AABB minPos maxPos) sides
                          where (ix, iy, iz) = toXYZ minPos
                                (ax, ay, az) = toXYZ maxPos
                                sides = [
                                          XYRect ix ax iy ay az mat,
                                          XYRect ix ax iy ay iz mat,
                                          XZRect ix ax iz az ay mat,
                                          XZRect ix ax iz az iy mat,
                                          YZRect iy ay iz az ax mat,
                                          YZRect iy ay iz az ix mat
                                        ]

-- create sphere's axis-aligned bounding box
createAABB :: HittableType -> AABB
createAABB (Sphere pos radius _)    = AABB (pos <-> fill radius) (pos <+> fill radius)
createAABB (XYRect x0 x1 y0 y1 k _) = AABB (fromXYZ (x0, y0, k - 0.0001)) (fromXYZ (x1, y1, k + 0.0001))
createAABB (XZRect x0 x1 z0 z1 k _) = AABB (fromXYZ (x0, k - 0.0001, z0)) (fromXYZ (x1, k + 0.0001, z1))
createAABB (YZRect y0 y1 z0 z1 k _) = AABB (fromXYZ (k - 0.0001, y0, z0)) (fromXYZ (k + 0.0001, y1, z1))
createAABB (Box aabb _)             = aabb
createAABB (BVH aabb _ _)           = aabb

-- collision detection
-- improving efficiency here is very important
--{-# INLINE hit #-}
hit :: HittableType -> Ray -> Double -> Double -> Maybe HitRecord
hit (Sphere position radius mat) ray tmin tmax = if discriminant < 0 || ((t1 < tmin || t1 > tmax) && (t2 < tmin || t2 > tmax))
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
                                                  rpos          = at ray t
                                                  outwardNormal = (rpos <-> position) .^ (1.0 / radius)
                                                  frontFace     = (rdir .* outwardNormal) <= 0
                                                  normal        = if frontFace then outwardNormal else outwardNormal .^ (-1.0)
                                                  (u, v)        = getSphereUV outwardNormal

hit (XYRect x0 x1 y0 y1 k mat) ray tmin tmax = if (t < tmin || t > tmax)
                                                  || (x < x0 || x > x1 || y < y0 || y > y1)
                                                  then Nothing
                                                  else Just $ HitRecord rpos normal t u v frontFace mat
                                                  where (ox, oy, oz) = toXYZ $ _origin ray
                                                        rdir         = _direction ray
                                                        (dx, dy, dz) = toXYZ rdir
                                                        t = (k - oz) / dz
                                                        x = ox + t * dx
                                                        y = oy + t * dy
                                                        u = (x - x0) / (x1 - x0)
                                                        v = (y - y0) / (y1 - y0)
                                                        rpos          = at ray t
                                                        outwardNormal = fromXYZ (0, 0, 1)
                                                        frontFace     = (rdir .* outwardNormal) <= 0
                                                        normal        = if frontFace then outwardNormal else outwardNormal .^ (-1.0)

hit (XZRect x0 x1 z0 z1 k mat) ray tmin tmax = if (t < tmin || t > tmax)
                                                  || (x < x0 || x > x1 || z < z0 || z > z1)
                                                  then Nothing
                                                  else Just $ HitRecord rpos normal t u v frontFace mat
                                                  where (ox, oy, oz) = toXYZ $ _origin ray
                                                        rdir         = _direction ray
                                                        (dx, dy, dz) = toXYZ rdir
                                                        t = (k - oy) / dy
                                                        x = ox + t * dx
                                                        z = oz + t * dz
                                                        u = (x - x0) / (x1 - x0)
                                                        v = (z - z0) / (z1 - z0)
                                                        rpos          = at ray t
                                                        outwardNormal = fromXYZ (0, 1, 0)
                                                        frontFace     = (rdir .* outwardNormal) <= 0
                                                        normal        = if frontFace then outwardNormal else outwardNormal .^ (-1.0)

hit (YZRect y0 y1 z0 z1 k mat) ray tmin tmax = if (t < tmin || t > tmax)
                                                  || (y < y0 || y > y1 || z < z0 || z > z1)
                                                  then Nothing
                                                  else Just $ HitRecord rpos normal t u v frontFace mat
                                                  where (ox, oy, oz) = toXYZ $ _origin ray
                                                        rdir         = _direction ray
                                                        (dx, dy, dz) = toXYZ rdir
                                                        t = (k - ox) / dx
                                                        y = oy + t * dy
                                                        z = oz + t * dz
                                                        u = (y - y0) / (y1 - y0)
                                                        v = (z - z0) / (z1 - z0)
                                                        rpos          = at ray t
                                                        outwardNormal = fromXYZ (1, 0, 0)
                                                        frontFace     = (rdir .* outwardNormal) <= 0
                                                        normal        = if frontFace then outwardNormal else outwardNormal .^ (-1.0)

hit (Box _ sides) ray tmin tmax = hitToList sides ray tmin tmax

hit (BVH aabb left right) ray tmin tmax = if hitAABB aabb ray tmin tmax
                                then case hit left ray tmin tmax of
                                      (Just lhr)  -> case hit right ray tmin (_t lhr) of
                                                      (Just rhr) -> if _t lhr < _t rhr then Just lhr else Just rhr
                                                      Nothing    -> Just lhr
                                      Nothing     -> hit right ray tmin tmax
                                else Nothing

-- Box等用
hitToList :: [HittableType] -> Ray -> Double -> Double -> Maybe HitRecord
hitToList [] _ _ _ = Nothing
hitToList (e:es) ray tmin tmax = case hit e ray tmin tmax of 
                                  Just hr -> Just hr  
                                  Nothing -> hitToList es ray tmin tmax

-- AABB
-- create AABB surrounding two AABB
surroundingAABB :: AABB -> AABB -> AABB
surroundingAABB (AABB minPos1 maxPos1) (AABB minPos2 maxPos2) = AABB small big
                                                              where small = zipWith min minPos1 minPos2
                                                                    big   = zipWith max maxPos1 maxPos2

-- check hit in AABB
hitAABB :: AABB -> Ray -> Double -> Double -> Bool
hitAABB (AABB minPos maxPos) ray tmin tmax = inSlabs
                                    where (dx, dy, dz)  = toXYZ (_direction ray)
                                          invD          = fromXYZ (1.0 / dx, 1.0 / dy, 1.0 / dz)
                                          orig          = _origin ray
                                          tmp0          = zipWith (*) invD $ zipWith (-) minPos orig
                                          tmp1          = zipWith (*) invD $ zipWith (-) maxPos orig
                                          ftmins        = fromXYZ (tmin, tmin, tmin)
                                          ftmaxs        = fromXYZ (tmax, tmax, tmax)
                                          t0s           = zipWith max ftmins $ zipWith min tmp0 tmp1
                                          t1s           = zipWith min ftmaxs $ zipWith max tmp0 tmp1
                                          (x, y, z)     = toXYZ (t1s <-> t0s)
                                          inSlabs       = x >= 0.0 && y >= 0.0 && z >= 0.0

-- experimental
hitAABB' :: AABB -> Ray -> Double -> Double -> Bool
hitAABB' (AABB minPos maxPos) ray tmin tmax = inSlabs
                                    where (dx, dy, dz)  = toXYZ (_direction ray)
                                          (ox, oy, oz)  = toXYZ $ _origin ray
                                          (ix, iy, iz)  = toXYZ minPos
                                          (ax, ay, az)  = toXYZ maxPos
                                          (t0x, t0y, t0z) = ((ix - ox) / dx, (iy - oy) / dy, (iz - oz) / dz)
                                          (t1x, t1y, t1z) = ((ax - ox) / dx, (ay - oy) / dy, (az - oz) / dz)
                                          inSlabs = check tmin tmax dx t0x t1x && check tmin tmax dy t0y t1y && check tmin tmax dz t0z t1z

{-# INLINE check #-}
check :: Double -> Double -> Double -> Double -> Double -> Bool
check tmin tmax invD t0 t1 = ntmax > ntmin
                          where (nt0, nt1) = if invD < 0.0 then (t1, t0) else (t0, t1)
                                ntmin      = max nt0 tmin
                                ntmax      = min nt1 tmax

-- BVH
-- sort AABB by specified axis
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

-- probably faster (requires only one list traversal)
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n <= 0 = ([], xs)
splitAt' _ []          = ([], [])
splitAt' n (x:xs)      = (x:ys, zs)
    where (ys, zs) = splitAt' (n - 1) xs

createBVH :: [HittableType] -> HittableType
createBVH [object] = BVH aabb object object
                  where aabb = createAABB object
createBVH [object1, object2] = BVH (surroundingAABB laabb raabb) object1 object2
                            where laabb = createAABB object1
                                  raabb = createAABB object2
createBVH objects = BVH (surroundingAABB laabb raabb) leftNode rightNode
                where axis    = length objects `mod` 3 -- random
                      sorted  = sortObjects objects axis
                      mid     = length sorted `div` 2
                      (leftObjects, rightObjects) = splitAt' mid sorted
                      leftNode  = createBVH leftObjects
                      rightNode = createBVH rightObjects
                      laabb = _aabb leftNode
                      raabb = _aabb rightNode