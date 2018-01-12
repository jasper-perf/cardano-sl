module Main where

import Universum
import Formatting

import UTxO.Simplified

main :: IO ()
main = putStrLn $ runTranslate $ do
    actors <- generatedActors
    liftPure $ format ("The genesis actors are\n" % build) actors
