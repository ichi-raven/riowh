
{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module RayTracer.SamplingStrategy
(
    module RayTracer.SamplingStrategy,
) where

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
            } deriving (Generic, NFData)

value :: PDF -> Direction -> Double
value (CosinePDF uvw) direction = if cosine <= 0.0 then 0.0 else cosine / kPi
                                where w       = _wb uvw
                                      cosine  = normalize direction .* w

value (HittablePDF target from) direction = pdfValue target from direction



generateAlongPDF :: StatefulGen genType m => PDF -> genType -> m Direction
generateAlongPDF (CosinePDF uvw) gen = do
                                    rv <- randomCosineDirection gen
                                    return $ localPos uvw rv

generateAlongPDF (HittablePDF target from) gen = randomIn target from gen