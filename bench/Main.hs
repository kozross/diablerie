module Main (main) where

import Data.Diablerie.ByteArray
  ( countEq,
    findFirstEq,
    findFirstGt,
    findFirstLt,
    findFirstMatch,
    findFirstNe,
    findLastEq,
  )
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
      ffnBenches,
      fleBenches,
      ffgBenches,
      fflBenches,
      ceBenches,
      ffmBenches
    ]

-- Benches

fflBenches :: Benchmark
fflBenches =
  bgroup
    "findFirstLt"
    [ bench "Optimized" . nf (findFirstLt all42) $ 42,
      bcompare "$2 == \"findFirstLt\" && $NF == \"Optimized\""
        . bench "Naive"
        . nf (Naive.findFirstLt all42)
        $ 42
    ]

ffmBenches :: Benchmark
ffmBenches =
  bgroup
    "findFirstMatch"
    [ bench "Optimized (16)" . nf (findFirstMatch a16) $ abab,
      bcompare "$2 == \"findFirstMatch\" && $NF == \"Optimized (16)\""
        . bench "Naive (16)"
        . nf (Naive.findFirstMatch a16)
        $ abab,
      bench "Optimized (32)" . nf (findFirstMatch a32) $ abab,
      bcompare "$2 == \"findFirstMatch\" && $NF == \"Optimized (32)\""
        . bench "Naive (32)"
        . nf (Naive.findFirstMatch a32)
        $ abab,
      bench "Optimized (64)" . nf (findFirstMatch a64) $ abab,
      bcompare "$2 == \"findFirstMatch\" && $NF == \"Optimized (64)\""
        . bench "Naive (64)"
        . nf (Naive.findFirstMatch a64)
        $ abab,
      bench "Optimized (128)" . nf (findFirstMatch a128) $ abab,
      bcompare "$2 == \"findFirstMatch\" && $NF == \"Optimized (128)\""
        . bench "Naive (128)"
        . nf (Naive.findFirstMatch a128)
        $ abab,
      bench "Optimized (256)" . nf (findFirstMatch a256) $ abab,
      bcompare "$2 == \"findFirstMatch\" && $NF == \"Optimized (256)\""
        . bench "Naive (256)"
        . nf (Naive.findFirstMatch a256)
        $ abab,
      bench "Optimized (272)" . nf (findFirstMatch a272) $ abab,
      bcompare "$2 == \"findFirstMatch\" && $NF == \"Optimized (272)\""
        . bench "Naive (272)"
        . nf (Naive.findFirstMatch a272)
        $ abab
    ]

ceBenches :: Benchmark
ceBenches =
  bgroup
    "countEq"
    [ bench "Optimized" . nf (countEq all42) $ 42,
      bcompare "$2 == \"countEq\" && $NF == \"Optimized\""
        . bench "Naive"
        . nf (Naive.countEq all42)
        $ 42
    ]

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

ffnBenches :: Benchmark
ffnBenches =
  bgroup
    "findFirstNe"
    [ bench "0, optimized" . nf (findFirstNe allZero) $ 0,
      bcompare "$2 == \"findFirstNe\" && $NF == \"0, optimized\""
        . bench "0, naive"
        . nf (Naive.findFirstNe allZero)
        $ 0,
      bench "Other, optimized" . nf (findFirstNe all42) $ 42,
      bcompare "$2 == \"findFirstNe\" && $NF == \"Other, optimized\""
        . bench "Other, naive"
        . nf (Naive.findFirstNe all42)
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

a16 :: ByteArray
a16 = fromListN 16 . replicate 16 $ 97

a32 :: ByteArray
a32 = a16 <> a16

a64 :: ByteArray
a64 = a32 <> a32

a128 :: ByteArray
a128 = a64 <> a64

a256 :: ByteArray
a256 = a128 <> a128

a272 :: ByteArray
a272 = a256 <> a16

abab :: ByteArray
abab =
  fromListN tenMegabytes
    . take tenMegabytes
    . cycle
    $ [97, 98]

allZero :: ByteArray
allZero = fromListN tenMegabytes . replicate tenMegabytes $ 0

all42 :: ByteArray
all42 = fromListN tenMegabytes . replicate tenMegabytes $ 42

all127 :: ByteArray
all127 = fromListN tenMegabytes . replicate tenMegabytes $ 127

all142 :: ByteArray
all142 = fromListN tenMegabytes . replicate tenMegabytes $ 142
