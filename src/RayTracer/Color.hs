module RayTracer.Color where

import RayTracer.Utility

kBlack :: Color
kBlack = origin

kRed :: Color
kRed = fromXYZ (0.8, 0.1, 0.1)

kGreen :: Color
kGreen = fromXYZ (0.1, 0.8, 0.1)

kBlue :: Color
kBlue = fromXYZ (0.25, 0.25, 0.8)

kWhite :: Color
kWhite = fromXYZ (0.9, 0.9, 0.9)

-- clamp pixel rgb values to [0, 1)
--{-# INLINE clampColor #-}
clampColor :: Color -> Color
clampColor src = fromXYZ (max (min 0.99999 r) 0.0, max (min 0.99999 g) 0.0, max (min 0.99999 b) 0.0)
                  where (r, g, b) = toXYZ src

-- tone mapping (gamma correction)
--{-# INLINE toneMapping #-}
toneMapping :: Color -> Color
toneMapping src = fromXYZ (mapping r, mapping g, mapping b)
                  where (r, g, b) = toXYZ src
                        mapping   = sqrt 

correctNaN :: Color -> Color
correctNaN src = fromXYZ (nr, ng, nb)
            where (r, g, b) = toXYZ src
                  -- NaN is not equal to itself
                  nr = if r == r then r else 0.0
                  ng = if g == g then g else 0.0
                  nb = if b == b then b else 0.0

--{-# INLINE packR8G8B8A8 #-}
packR8G8B8A8 :: Color -> Word32
packR8G8B8A8 color = shiftL ur 24 .|. shiftL ug 16 .|. shiftL ub 8
                  where (r, g, b)       = toXYZ color
                        (ur, ug, ub)    = (floor (255.999 * r) :: Word32, floor (255.999 * g) :: Word32, floor (255.999 * b) :: Word32)

--{-# INLINE unpackR8G8B8A8 #-}
unpackR8G8B8A8 :: Word32 -> (Word32, Word32, Word32, Word32)
unpackR8G8B8A8 bytes = (r, g, b, a)
                    where r = shiftR (bytes .&. 0xFF000000) 24  
                          g = shiftR (bytes .&. 0x00FF0000) 16  
                          b = shiftR (bytes .&. 0x0000FF00) 8   
                          a =         bytes .&. 0x000000FF              

--{-# INLINE postProduction #-}
postProduction :: Color -> Word32
postProduction = packR8G8B8A8 . toneMapping . clampColor