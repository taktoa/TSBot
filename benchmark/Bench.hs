module Main (main) where

import qualified TSBotBench
-- GENERATE: import qualified New.ModuleBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "TSBot" TSBotBench.benchmarks
    -- GENERATE: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
