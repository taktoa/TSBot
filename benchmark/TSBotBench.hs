module TSBotBench (benchmarks) where

import TSBot

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]
