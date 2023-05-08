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
clampColor :: Color -> Color
clampColor src = fromXYZ (max (min 0.99999 r) 0.0, max (min 0.99999 g) 0.0, max (min 0.99999 b) 0.0)
                  where (r, g, b) = toXYZ src

-- tone mapping (gamma correction)
toneMapping :: Color -> Color
toneMapping src = fromXYZ (sqrt r, sqrt g, sqrt b)
                  where (r, g, b) = toXYZ src