module RayTracer.Color 
(
    clampColor,
    toneMapping
) where

import RayTracer.Utility

-- clamp pixel rgb values to [0, 1)
clampColor :: Color -> Color
clampColor src = fromXYZ (max (min 0.99999 r) 0.0, max (min 0.99999 g) 0.0, max (min 0.99999 b) 0.0)
                  where (r, g, b) = toXYZ src

-- tone mapping (gamma correction)
toneMapping :: Color -> Color
toneMapping src = fromXYZ (sqrt r, sqrt g, sqrt b)
                  where (r, g, b) = toXYZ src