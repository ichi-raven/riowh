
{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.SamplingStrategy
(
    module RayTracer.SamplingStrategy,
) where

import RayTracer.Utility
import RayTracer.Random
import RayTracer.Geometry

import System.Random.Stateful


data PDF =  CosinePDF
            {
                _uvw :: !ONB
            } 
            | HittablePDF
            {
                _target :: !HittableType,
                _from   :: !Point
            } 
            | MixturePDF
            {
                pdf1 :: !PDF,
                pdf2 :: !PDF
            } 
            | Uniform deriving (Generic, NFData)

value :: PDF -> Direction -> Double
value (CosinePDF uvw) direction = if cosine <= 0.0 then 0.0 else cosine / kPi
                                where w       = _wb uvw
                                      cosine  = normalize direction .* w

value (HittablePDF target from) direction = pdfValue target from direction

value (MixturePDF pdf1 pdf2) direction = 0.5 * v1 + 0.5 * v2
                                    where v1 = value pdf1 direction
                                          v2 = value pdf2 direction

value (Uniform) direction = 0.0 

generateAlongPDF :: StatefulGen genType m => PDF -> genType -> m Direction
generateAlongPDF (CosinePDF uvw) gen = do
                                    rv <- randomCosineDirection gen
                                    return $ localPos uvw rv

generateAlongPDF (HittablePDF target from) gen = randomIn target from gen

generateAlongPDF (MixturePDF pdf1 pdf2) gen = do
                                             r <- uniformRM (0.0, 1.0) gen 
                                             let chosen = if r > (0.5 :: Double) then pdf1 else pdf2
                                             generateAlongPDF chosen gen

generateAlongPDF (Uniform) gen = return $ fromXYZ (1.0, 0.0, 0.0)