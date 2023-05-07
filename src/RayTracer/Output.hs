module RayTracer.Output 
(
    outputImageByPPM
) where 

import System.IO ( hClose, hPutStr, openFile, IOMode(WriteMode) )
import qualified Data.Text as T

import RayTracer.Utility

-- header string for ppm
ppmHeader :: Int -> Int -> Text
ppmHeader width height = T.pack $ "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"

-- translate pixel color to ppm pixel string("R G B")
showColor :: Color -> Text
showColor color = T.pack $ show (floor (255.999 * r)) ++ " " ++ show (floor (255.999 * g)) ++ " " ++ show (floor (255.999 * b))
                  where (r, g, b) = toXYZ $ toneMapping $ clampColor color

-- translate output image to ppm format
ppmFormat :: Image -> Text
ppmFormat image = T.unlines [showColor (image ! (x, y)) | y <- [0..height - 1],  x <- [0..width - 1]]
                where width     = getImageWidth   image
                      height    = getImageHeight  image

-- combined output
outputImageByPPM :: Image -> String -> IO()
outputImageByPPM image outputFileName = do
                                        let width   = getImageWidth   image
                                            height  = getImageHeight  image
                                        -- open file
                                        handle <- openFile outputFileName WriteMode
                                        -- rendering (output ppm image)
                                        hPutStr handle $ T.unpack $ T.append (ppmHeader width height) (ppmFormat image)
                                        -- close file
                                        hClose handle
