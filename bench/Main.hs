module Main (main) where

import Data.Interieur.ByteArray (findFirstEq, findFirstGt, findLastEq)
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (fromListN)
import qualified Naive
import Test.Tasty.Bench
  ( Benchmark,
    bcompare,
    bench,
    bgroup,
    defaultMain,
    nf,
  )

main :: IO ()
main =
  defaultMain
    [ ffeBenches,
      fleBenches,
      ffgBenches
    ]

-- Benches

ffeBenches :: Benchmark
ffeBenches =
  bgroup
    "findFirstEq"
    [ bench "0, optimized" . nf (findFirstEq all42) $ 0,
      bcompare "$2 == \"findFirstEq\" && $NF == \"0, optimized\""
        . bench "0, naive"
        . nf (Naive.findFirstEq all42)
        $ 0,
      bench "Other, optimized" . nf (findFirstEq allZero) $ 42,
      bcompare "$2 == \"findFirstEq\" && $NF == \"Other, optimized\""
        . bench "Other, naive"
        . nf (Naive.findFirstEq allZero)
        $ 42
    ]

ffgBenches :: Benchmark
ffgBenches =
  bgroup
    "findFirstGt"
    [ bench "0, optimized" . nf (findFirstGt allZero) $ 0,
      bcompare "$2 == \"findFirstGt\" && $NF == \"0, optimized\""
        . bench "0, naive"
        . nf (Naive.findFirstGt all42)
        $ 0,
      bench "(1, 126), optimized" . nf (findFirstGt all42) $ 42,
      bcompare "$2 == \"findFirstGt\" && $NF == \"(1, 126), optimized\""
        . bench "(1, 126), naive"
        . nf (Naive.findFirstGt all42)
        $ 42,
      bench "127, optimized" . nf (findFirstGt all127) $ 127,
      bcompare "$2 == \"findFirstGt\" && $NF == \"127, optimized\""
        . bench "127, naive"
        . nf (Naive.findFirstGt all127)
        $ 127,
      bench "128+, optimized" . nf (findFirstGt all142) $ 142,
      bcompare "$2 == \"findFirstGt\" && $NF == \"128+, optimized\""
        . bench "Other, naive"
        . nf (Naive.findFirstGt all142)
        $ 142
    ]

fleBenches :: Benchmark
fleBenches =
  bgroup
    "findLastEq"
    [ bench "0, optimized" . nf (findLastEq all42) $ 0,
      bcompare "$2 == \"findLastEq\" && $NF == \"0, optimized\""
        . bench "0, naive"
        . nf (Naive.findLastEq all42)
        $ 0,
      bench "Other, optimized" . nf (findLastEq allZero) $ 42,
      bcompare "$2 == \"findLastEq\" && $NF == \"Other, optimized\""
        . bench "Other, naive"
        . nf (Naive.findLastEq allZero)
        $ 42
    ]

-- Helpers

tenMegabytes :: Int
tenMegabytes = 10 * 1024 * 1024

allZero :: ByteArray
allZero = fromListN tenMegabytes . replicate tenMegabytes $ 0

all42 :: ByteArray
all42 = fromListN tenMegabytes . replicate tenMegabytes $ 42

all127 :: ByteArray
all127 = fromListN tenMegabytes . replicate tenMegabytes $ 127

all142 :: ByteArray
all142 = fromListN tenMegabytes . replicate tenMegabytes $ 142
