{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module Main where

import RayTracer.Scene
import RayTracer.Output
import Data.Time

main :: IO ()
main = do
    putStrLn "hello!"
