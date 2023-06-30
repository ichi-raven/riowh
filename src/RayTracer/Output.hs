module RayTracer.Output 
(
    output,
    outputSerialImages,
    outputSelectedSerialImages,
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

import qualified Codec.Picture as Pic

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
ppmFormat image = T.unlines $ runEval $ parListChunk splitNum rdeepseq [showColor (image ! (x, y)) | y <- [0..height],  x <- [0..width]]
                where width     = getImageWidth   image
                      height    = getImageHeight  image
                      splitNum  = div (width * height) numCapabilities

-- combined output
outputImageByPPM :: String -> Image -> IO ()
outputImageByPPM outputFilePath image = do
                                        let width   = getImageWidth   image
                                            height  = getImageHeight  image
                                        -- open file
                                        handle <- openFile outputFilePath WriteMode
                                        -- output ppm image
                                        hPutStr handle $ T.unpack $ T.append (ppmHeader width height) (ppmFormat image)
                                        -- close file
                                        hClose handle

translateImageToPicImage :: Image -> Pic.DynamicImage
translateImageToPicImage src = Pic.ImageRGB8 $ Pic.generateImage pixelConverter width height
                            where  width  = getImageWidth  src
                                   height = getImageHeight src
                                   pixelConverter x y = Pic.PixelRGB8 r g b
                                                    where (r, g, b, _) = unpackR8G8B8A8 $ src ! (x, y)

outputImageByPNG :: String -> Image -> IO()
outputImageByPNG outputFilePath image = Pic.savePngImage outputFilePath (translateImageToPicImage image)

outputImageByBmp :: String -> Image -> IO()
outputImageByBmp outputFilePath image = Pic.saveBmpImage outputFilePath (translateImageToPicImage image)

outputImageByTiff :: String -> Image -> IO()
outputImageByTiff outputFilePath image = Pic.saveTiffImage outputFilePath (translateImageToPicImage image)

outputImageByJPG :: String -> Image -> IO()
outputImageByJPG outputFilePath image = Pic.saveJpgImage 100 outputFilePath (translateImageToPicImage image)

chooseOutput :: String -> (String -> Image -> IO())
chooseOutput ext = case ext of
                        "bmp"  -> outputImageByBmp
                        "png"  -> outputImageByPNG
                        "jpg"  -> outputImageByJPG
                        "tiff" -> outputImageByTiff
                        "ppm"  -> outputImageByPPM

output :: String -> Image -> IO()
output path image = outputFunc path image
                 where  splitted    = splitBy isDot path
                        name        = concat $ init splitted
                        ext         = last splitted
                        outputFunc  = chooseOutput ext

-- output serial images (animation)
outputSerialImages :: String -> [Image] -> IO()
outputSerialImages baseOutputFilePath images = outputSelectedSerialImages baseOutputFilePath (length images) images

-- output selected(truncated) serial images (animation)
outputSelectedSerialImages :: String -> Int -> [Image] -> IO()
outputSelectedSerialImages baseOutputFilePath frameNum (image:imgs) = do 
                                                                let number      = frameNum - (length imgs) - 1
                                                                    splitted    = splitBy isDot baseOutputFilePath
                                                                    name        = concat $ init splitted
                                                                    ext         = last splitted                                                                    
                                                                    fileName    = name ++ (show number) ++ "." ++ ext
                                                                (chooseOutput ext) fileName image
                                                                outputSelectedSerialImages baseOutputFilePath frameNum imgs

outputSelectedSerialImages _ _ [] = return ()


outputInfo :: UTCTime -> UTCTime -> String -> IO()
outputInfo startTime endTime outputFile = do
                                        let execTime = diffUTCTime endTime startTime
                                        handle <- openFile outputFile WriteMode

                                        hPutStr handle $ "start at : " ++ show startTime ++ "\n"
                                        hPutStr handle $ "end at : " ++ show endTime ++ "\n"
                                        hPutStr handle $ "elapsed time : " ++ show execTime ++ "\n"

                                        hClose handle