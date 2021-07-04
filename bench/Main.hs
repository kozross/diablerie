module Main (main) where

import Control.DeepSeq (force)
import Data.Interieur.ByteArray (findFirstEq)
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (fromListN)
import qualified Naive
import Test.Tasty.Bench
  ( Benchmark,
    bcompare,
    bench,
    bgroup,
    defaultMain,
    env,
    nf,
  )

main :: IO ()
main =
  defaultMain
    [ env (pure . force $ (allZero, all42)) (uncurry ffeBenches)
    ]

-- Benches

ffeBenches :: ByteArray -> ByteArray -> Benchmark
ffeBenches zeroes fortyTwos =
  bgroup
    "findFirstEq"
    [ bench "0, optimized" . nf (findFirstEq fortyTwos) $ 0,
      bcompare "$NF == \"0, optimized\""
        . bench "0, naive"
        . nf (Naive.findFirstEq fortyTwos)
        $ 0,
      bench "Other, optimized" . nf (findFirstEq zeroes) $ 42,
      bcompare "$NF == \"Other, optimized\""
        . bench "Other, naive"
        . nf (Naive.findFirstEq zeroes)
        $ 42
    ]

-- Helpers

tenMegabytes :: Int
tenMegabytes = 10 * 1024 * 1024

allZero :: ByteArray
allZero = fromListN tenMegabytes . replicate tenMegabytes $ 0

all42 :: ByteArray
all42 = fromListN tenMegabytes . replicate tenMegabytes $ 42

{-
import qualified Data.ByteString as BS
import Data.Interieur.ByteArray
  ( countEq,
    countEqIn,
    findFirstEq,
    findFirstEqIn,
    findFirstGt,
    findFirstGtIn,
    findLastEq,
    findLastEqIn,
  )
import Data.Primitive.ByteArray (ByteArray)
import Data.Word (Word8)
import GHC.Exts (fromList, fromListN, toList)
import qualified Naive
import System.IO (Handle, IOMode (ReadMode), withFile)
import Test.Tasty.Bench (Benchmark, bcompare, bench, bgroup, defaultMain, nf)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = withFile "./bench-data/big.txt" ReadMode go
  where
    go :: Handle -> IO ()
    go h = do
      asBS <- BS.hGetContents h
      runAllTests . fromListN (BS.length asBS) . toList $ asBS

-- Helpers

lotsOfZero :: ByteArray
lotsOfZero = fromList $ replicate 6488665 0 <> [1]

everyByte :: [Word8]
everyByte = [minBound .. maxBound]

runAllTests :: ByteArray -> IO ()
runAllTests asBA =
  defaultMain
    [ bgroup "findFirstEq" . ffeTests $ asBA,
      bgroup "findFirstGt" . ffgTests $ asBA,
      bgroup "findLastEq" . fleTests $ asBA,
      bgroup "countEq" . ceTests $ asBA
    ]

ceTests :: ByteArray -> [Benchmark]
ceTests asBA =
  [ testCase "countEq, wrapped, correctness"
      . assertEqual "countEq" (Naive.countEq asBA <$> everyByte)
      . fmap (countEq asBA)
      $ everyByte,
    bench "countEq, wrapped" . nf (countEq asBA) $ 42,
    bcompare "$NF == \"countEq, wrapped\""
      . bench "countEq, naive"
      . nf (Naive.countEq asBA)
      $ 42,
    testCase "countEqIn, wrapped, correctness"
      . assertEqual "countEqIn" (Naive.countEqIn asBA 3000000 2000000 <$> everyByte)
      . fmap (countEqIn asBA 3000000 2000000)
      $ everyByte,
    bench "countEqIn, wrapped" . nf (countEqIn asBA 3000000 2000000) $ 42,
    bcompare "$NF == \"countEqIn, wrapped\""
      . bench "countEqIn, naive"
      . nf (Naive.countEqIn asBA 3000000 2000000)
      $ 42
  ]

ffeTests :: ByteArray -> [Benchmark]
ffeTests asBA =
  [ testCase "findFirstEq, wrapped, correctness"
      . assertEqual "findFirstEq" (Naive.findFirstEq asBA <$> everyByte)
      . fmap (findFirstEq asBA)
      $ everyByte,
    bench "findFirstEq, wrapped" . nf (fmap (findFirstEq asBA)) $ everyByte,
    bcompare "$NF == \"findFirstEq, wrapped\""
      . bench "findFirstEq, naive"
      . nf (fmap (Naive.findFirstEq asBA))
      $ everyByte,
    testCase "findFirstEqIn, wrapped, correctness"
      . assertEqual "findFirstEqIn" (Naive.findFirstEqIn asBA 3000000 2000000 <$> everyByte)
      . fmap (findFirstEqIn asBA 3000000 2000000)
      $ everyByte,
    bench "findFirstEqIn, wrapped" . nf (fmap (findFirstEqIn asBA 3000000 2000000)) $ everyByte,
    bcompare "$NF == \"findFirstEqIn, wrapped\""
      . bench "findFirstEqIn, naive"
      . nf (fmap (Naive.findFirstEqIn asBA 3000000 2000000))
      $ everyByte
  ]

ffgTests :: ByteArray -> [Benchmark]
ffgTests asBA =
  [ testCase "findFirstGt, wrapped, correctness"
      . assertEqual "findFirstGt" (Naive.findFirstGt asBA <$> everyByte)
      . fmap (findFirstGt asBA)
      $ everyByte,
    bench "findFirstGt, wrapped" . nf (fmap (findFirstGt asBA)) $ everyByte,
    bcompare "$NF == \"findFirstGt, wrapped\""
      . bench "findFirstGt, naive"
      . nf (fmap (Naive.findFirstGt asBA))
      $ everyByte,
    bench "findFirstGt, 0" . nf (findFirstGt lotsOfZero) $ 0,
    bcompare "$NF == \"findFirstGt, 0\""
      . bench "findFirstGt, 0, naive"
      . nf (Naive.findFirstGt lotsOfZero)
      $ 0,
    bench "findFirstGt, 127" . nf (findFirstGt asBA) $ 127,
    bcompare "$NF == \"findFirstGt, 127\""
      . bench "findFirstGt, 127, naive"
      . nf (Naive.findFirstGt asBA)
      $ 127,
    testCase "findFirstGtIn, wrapped, correctness"
      . assertEqual "findFirstGtIn" (Naive.findFirstGtIn asBA 3000000 2000000 <$> everyByte)
      . fmap (findFirstGtIn asBA 3000000 2000000)
      $ everyByte,
    bench "findFirstGtIn, wrapped" . nf (fmap (findFirstGtIn asBA 3000000 2000000)) $ everyByte,
    bcompare "$NF == \"findFirstGtIn, wrapped\""
      . bench "findFirstGtIn, naive"
      . nf (fmap (Naive.findFirstGtIn asBA 3000000 2000000))
      $ everyByte,
    bench "findFirstGtIn, 0" . nf (findFirstGtIn lotsOfZero 3000000 2000000) $ 0,
    bcompare "$NF == \"findFirstGtIn, 0\""
      . bench "findFirstGtIn, 0, naive"
      . nf (Naive.findFirstGtIn lotsOfZero 3000000 2000000)
      $ 0,
    bench "findFirstGtIn, 127" . nf (findFirstGtIn asBA 3000000 2000000) $ 127,
    bcompare "$NF == \"findFirstGtIn, 127\""
      . bench "findFirstGtIn, 127, naive"
      . nf (Naive.findFirstGtIn asBA 3000000 2000000)
      $ 127
  ]

fleTests :: ByteArray -> [Benchmark]
fleTests asBA =
  [ testCase "findLastEq, wrapped, correctness"
      . assertEqual "findLastEq" (Naive.findLastEq asBA <$> everyByte)
      . fmap (findLastEq asBA)
      $ everyByte,
    bench "findLastEq, wrapped" . nf (fmap (findLastEq asBA)) $ everyByte,
    bcompare "$NF == \"findLastEq, wrapped\""
      . bench "findLastEq, naive"
      . nf (fmap (Naive.findLastEq asBA))
      $ everyByte,
    testCase "findLastEqIn, wrapped, correctness"
      . assertEqual "findLastEqIn" (Naive.findLastEqIn asBA 3000000 2000000 <$> everyByte)
      . fmap (findLastEqIn asBA 3000000 2000000)
      $ everyByte,
    bench "findLastEqIn, wrapped" . nf (fmap (findLastEqIn asBA 3000000 2000000)) $ everyByte,
    bcompare "$NF == \"findLastEqIn, wrapped\""
      . bench "findLastEqIn, naive"
      . nf (fmap (Naive.findLastEqIn asBA 3000000 2000000))
      $ everyByte
  ]
-}
