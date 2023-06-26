module RayTracer.Output 
(
    outputImageByPPM,
    outputInfo
) where 

import Control.Parallel.Strategies
import Control.DeepSeq
import System.IO ( hClose, hPutStr, openFile, IOMode(WriteMode) )
import qualified Data.Text as T
import GHC.Conc (numCapabilities)
import Data.Time

import RayTracer.Utility
import RayTracer.Color

type Text = T.Text

-- header string for ppm
ppmHeader :: Int -> Int -> Text
ppmHeader width height = T.pack $ "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"

-- translate pixel color to ppm pixel string("R G B")
showColor :: Word32 -> Text
showColor colorBytes = T.pack $ show r ++ " " ++ show g ++ " " ++ show b
                  where (r, g, b, _) = unpackR8G8B8A8 colorBytes

-- translate output image to ppm format
ppmFormat :: Image -> Text
ppmFormat image = T.unlines $ runEval $ parListChunk splitNum rdeepseq [showColor (image ! (x, y)) | y <- [0..height - 1],  x <- [0..width - 1]]
                where width     = getImageWidth   image
                      height    = getImageHeight  image
                      splitNum  = div (width * height) numCapabilities

-- combined output
outputImageByPPM :: String -> Image -> IO ()
outputImageByPPM outputFileName image = do
                                        let width   = getImageWidth   image
                                            height  = getImageHeight  image
                                        -- open file
                                        handle <- openFile outputFileName WriteMode
                                        -- output ppm image
                                        hPutStr handle $ T.unpack $ T.append (ppmHeader width height) (ppmFormat image)
                                        -- close file
                                        hClose handle

-- output serial images (animation)
outputSerialImagesByPPM :: String -> [Image] -> IO()
outputSerialImagesByPPM baseOutputFileName images = outputSelectedSerialImagesByPPM baseOutputFileName (length images) images

-- output selected(truncated) serial images (animation)
outputSelectedSerialImagesByPPM :: String -> Int -> [Image] -> IO()
outputSelectedSerialImagesByPPM baseOutputFileName frameNum (image:imgs) = do 
                                                                let number      = frameNum - (length imgs) - 1
                                                                    splitted    = splitBy isDot baseOutputFileName
                                                                    name        = concat $ init splitted
                                                                    ext         = last splitted                                                                    
                                                                    fileName    = name ++ (show number) ++ "." ++ ext
                                                                outputImageByPPM fileName image
                                                                outputSelectedSerialImagesByPPM baseOutputFileName frameNum imgs

outputSelectedSerialImagesByPPM _ _ [] = return ()


outputInfo :: UTCTime -> UTCTime -> String -> IO()
outputInfo startTime endTime outputFile = do
                                        let execTime = diffUTCTime endTime startTime
                                        handle <- openFile outputFile WriteMode

                                        hPutStr handle $ "start at : " ++ show startTime ++ "\n"
                                        hPutStr handle $ "end at : " ++ show endTime ++ "\n"
                                        hPutStr handle $ "elapsed time : " ++ show execTime ++ "\n"

                                        hClose handle