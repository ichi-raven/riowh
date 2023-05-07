module RayTracer.BVH
(
    createBVH,
    module RayTracer.Geometry
) where 

import RayTracer.Geometry

data BVHNode = BVHNode
  {
    _aabb   :: !AABB,
    _left   :: !HittableType,
    _right  :: !HittableType
  } deriving (Generic, NFData)

compareObjects :: Int -> HittableType -> HittableType -> Bool
compareObjects axis ht1 ht2 = case axis of
                                  0 -> lx < rx
                                  1 -> ly < ry
                                  2 -> lz < rz
                                  _ -> undefined -- !!!!!!!!!!!!!!!!!
                                  where laabb = createAABB ht1
                                        raabb = createAABB ht2
                                        (lx, ly, lz) = toXYZ $ _minPos laabb
                                        (rx, ry, rz) = toXYZ $ _minPos raabb

-- quick sort AABB by specified axis
sortObjects :: [HittableType] -> Int -> [HittableType]
sortObjects [] axis     = []
sortObjects [o] axis    = [o]
sortObjects (o:xo) axis = sortObjects left axis ++ [o] ++ sortObjects right axis
                      where left  = filter (compareObjects axis o) xo
                            right = filter (not . compareObjects axis o) xo

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
                      (leftObjects, rightObjects) = splitAt mid sorted
                      leftNode  = createBVH leftObjects
                      rightNode = createBVH rightObjects
                      laabb = _aabb $ _node leftNode
                      raabb = _aabb $ _node rightNode