{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module RayTracer.Geometry
(
    HittableType(..),
    AABB,
    hit,
    createAABB,
    createBVH,
    createBox,
    pdfValue,
    randomIn,
    module RayTracer.Ray
) where

import RayTracer.Utility
import RayTracer.Random
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
  | List
  {
    _list :: [HittableType]
  }
  | Box
  {
    _aabb   :: !AABB,
    _sides  :: !HittableType
  }
  -- | Transform
  -- {
  --   _object     :: !HittableType,
  --   _rotAxis    :: !Direction,
  --   _rotAngle   :: !Double,
  --   _translate  :: !Point
  -- }
  | Translate
  {
    _object :: !HittableType,
    _offset :: !Point
  }
  | FlipFace
  {
    _object :: !HittableType
  }
  | BVH
  {
    _aabb   :: !AABB,
    _left   :: HittableType,
    _right  :: HittableType
  } deriving (Generic, NFData)

data AABB = AABB
  {
    _minPos :: !Point,
    _maxPos :: !Point
  } deriving (Generic, NFData)

--{-# INLINE getRotMat #-}
-- getRotMat :: Direction -> Double -> Matrix
-- getRotMat axis angle = rotMat
--             where (ax, ay, az) = toXYZ axis
--                   c   = cos angle
--                   s   = sin angle
--                   c1  = 1 - c
--                   r1  = fromXYZ(c + ax * ax * c1, ax * ay * c1 - az * s, az * ax * c1 + ay * s)
--                   r2  = fromXYZ(ax * ay * c1 + az * s, c + ay * ay * c1, ay * az * c1 - ax * s)
--                   r3  = fromXYZ(az * ax * c1 - ay * s, ay * az * c1 + ax * s, c + az * az * c1)
--                   rotMat = fromRows(r1, r2, r3)

-- TODO
-- getRotateAABBPoints :: Point -> Point -> Matrix -> [Point]
-- getRotateAABBPoints minPos maxPos mat = points
--                                 where from          = toXYZ $ mxv mat minPos
--                                       (rx, ry, rz)  = toXYZ $ mxv mat $ maxPos <-> minPos
--                                       onb           = ONB (fromXYZ (rx )) 
--                                       points = [from <+> ]

                                      

-- return the closer t under the assumption that one of t1 and t2 is inside [tmin, tmax]
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
                                sides = List [
                                          XYRect ix ax iy ay az mat,
                                          XYRect ix ax iy ay iz mat,
                                          XZRect ix ax iz az ay mat,
                                          XZRect ix ax iz az iy mat,
                                          YZRect iy ay iz az ax mat,
                                          YZRect iy ay iz az ix mat
                                        ]

-- create sphere's axis-aligned bounding box
createAABB :: HittableType -> AABB
createAABB (Sphere pos radius _)      = AABB (pos <-> fill radius) (pos <+> fill radius)
createAABB (XYRect x0 x1 y0 y1 k _)   = AABB (fromXYZ (x0, y0, k - 0.0001)) (fromXYZ (x1, y1, k + 0.0001))
createAABB (XZRect x0 x1 z0 z1 k _)   = AABB (fromXYZ (x0, k - 0.0001, z0)) (fromXYZ (x1, k + 0.0001, z1))
createAABB (YZRect y0 y1 z0 z1 k _)   = AABB (fromXYZ (k - 0.0001, y0, z0)) (fromXYZ (k + 0.0001, y1, z1))
createAABB (List list)                = foldl1' surroundingAABB $ map createAABB list
createAABB (Box aabb _)               = aabb
createAABB (BVH aabb _ _)             = aabb
-- TODO
-- createAABB (Transform object axis angle translate)  = AABB (minPos <+> offset) (maxPos <+> offset)
--                                     where aabb   = createAABB object
--                                           minPos = --_minPos aabb
--                                           maxPos = --_maxPos aabb


createAABB (Translate object offset)  = AABB (minPos <+> offset) (maxPos <+> offset)
                                    where aabb   = createAABB object
                                          minPos = _minPos aabb
                                          maxPos = _maxPos aabb
createAABB (FlipFace object)          = createAABB object

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

hit (List list) ray tmin tmax = hitToList list ray tmin tmax

hit (Box _ sides) ray tmin tmax = hit sides ray tmin tmax

-- hit (Transform object axis angle translate) ray tmin tmax = case hit object movedRay tmin tmax of
--                                                 Nothing -> Nothing
--                                                 Just hr -> Just nhr
--                                                       where (HitRecord point outwardNormal t u v _ mat) = hr
--                                                             translatedPos = offset <+> point
--                                                             nrdir         = _direction movedRay
--                                                             frontFace     = (nrdir .* outwardNormal) <= 0
--                                                             rotNormal     = mxv rotMat outwardNormal 
--                                                             normal        = if frontFace then rotNormal else rotNormal .^ (-1.0)
--                                                             nhr           = HitRecord translatedPos normal t u v frontFace mat
--                                           where orig      = _origin ray
--                                                 dir       = _direction ray
--                                                 rotMat    = getRotMat axis (deg2rad -angle)
--                                                 movedRay  = Ray (orig <-> offset) (mxv rotMat dir)

hit (Translate object offset) ray tmin tmax = case hit object movedRay tmin tmax of
                                                Nothing -> Nothing
                                                Just hr -> Just nhr
                                                      where (HitRecord point outwardNormal t u v _ mat) = hr
                                                            translatedPos = offset <+> point
                                                            nrdir         = _direction movedRay
                                                            frontFace     = (nrdir .* outwardNormal) <= 0
                                                            normal        = if frontFace then outwardNormal else outwardNormal .^ (-1.0)
                                                            nhr           = HitRecord translatedPos normal t u v frontFace mat
                                          where orig  = _origin ray
                                                dir   = _direction ray
                                                movedRay = Ray (orig <-> offset) dir

hit (FlipFace object) ray tmin tmax = case hit object ray tmin tmax of
                                        Just hr -> Just nhr
                                                where (HitRecord p n t u v ff mat) = hr
                                                      nhr = HitRecord p n t u v (not ff) mat
                                        Nothing -> Nothing

hit (BVH aabb left right) ray tmin tmax = if hitAABB aabb ray tmin tmax
                                then case hit left ray tmin tmax of
                                      (Just lhr)  -> case hit right ray tmin (_t lhr) of
                                                      (Just rhr) -> if _t lhr < _t rhr then Just lhr else Just rhr
                                                      Nothing    -> Just lhr
                                      Nothing     -> hit right ray tmin tmax
                                else Nothing


hitToList :: [HittableType] -> Ray -> Double -> Double -> Maybe HitRecord
hitToList [] _ _ _ = Nothing
hitToList (e:es) ray tmin tmax = case (hit e ray tmin tmax, hitToList es ray tmin tmax) of
                                  (Just hr, Just nhr) -> if _t hr < _t nhr then Just hr else Just nhr
                                  (Just hr, Nothing)  -> Just hr
                                  (Nothing, mnhr)     -> mnhr

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

pdfValue :: HittableType -> Point -> Direction -> Double
pdfValue (Sphere position radius mat) from direction = case hit this (Ray from direction) 0.001 kInfinity of
                                                        Just hr -> 1.0 / solidAngle
                                                            where co    = position <-> from
                                                                  sqnco = norm co * norm co
                                                                  cosThetaMax = sqrt $ 1 - radius * radius / sqnco
                                                                  solidAngle  = 2.0 * kPi * (1.0 - cosThetaMax)
                                                        Nothing -> 0.0
                                                      where this = Sphere position radius mat
pdfValue (XZRect x0 x1 z0 z1 k mat) from direction = case hit this (Ray from direction) 0.001 kInfinity of
                                                      Just hr -> distSq / (cosine * area)
                                                        where area    = (x1 - x0) * (z1 - z0)
                                                              t       = _t hr
                                                              normal  = _normal hr
                                                              ndir    = norm direction
                                                              distSq  = t * t * ndir * ndir
                                                              cosine  = abs $ direction .* normal / ndir
                                                      Nothing -> 0.0
                                                    where this = XZRect x0 x1 z0 z1 k mat

pdfValue (List objects) from direction = foldl1' (+) pdfValues / fromIntegral (length objects)
                                      where per o = pdfValue o from direction
                                            pdfValues = map per objects

pdfValue (FlipFace object) from direction = pdfValue object from direction

pdfValue _ _ _  = 0.0 --TODO

randomIn :: StatefulGen genType m => HittableType -> Point -> genType -> m Direction
randomIn (Sphere position radius mat) from gen = do
                                              let direction = position <-> from
                                                  nd = norm direction
                                                  distSq    =  nd * nd
                                                  uvw = buildFromW direction
                                              rts <- randomToSphere radius distSq gen
                                              return $ localPos uvw rts

randomIn (XZRect x0 x1 z0 z1 k mat) from gen = do
                                            rx <- uniformRM (x0, x1) gen
                                            rz <- uniformRM (z0, z1) gen
                                            let randomPoint = fromXYZ (rx, k, rz)
                                            return $ randomPoint <-> from

randomIn (List objects) from gen = do
                                ridx <- uniformRM (0, length objects - 1) gen
                                let chosen = objects !! ridx
                                randomIn chosen from gen

randomIn (FlipFace object) from gen = randomIn object from gen

randomIn _ _ _ = return $ fromXYZ (1.0, 0, 0) --TODO

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