module Main (main) where

import qualified Data.ByteString as BS
import Data.Interieur.ByteArray
  ( findFirstByte,
    findFirstByteIn,
    findLastByte,
    findLastByteIn,
  )
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (fromListN, toList)
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

runAllTests :: ByteArray -> IO ()
runAllTests asBA =
  defaultMain
    [ bgroup "findFirstByte (wrapped)" . ffbTests $ asBA,
      bgroup "findLastByte (wrapped)" . flbTests $ asBA
    ]

ffbTests :: ByteArray -> [Benchmark]
ffbTests asBA =
  [ testCase "findFirstByte, wrapped, correctness"
      . assertEqual "findFirstByte" (Naive.findFirstByte asBA 0x5a)
      . findFirstByte asBA
      $ 0x5a,
    bench "findFirstByte, wrapped" . nf (findFirstByte asBA) $ 0x5a,
    bcompare "$NF == \"findFirstByte, wrapped\""
      . bench "findFirstByte, naive"
      . nf (Naive.findFirstByte asBA)
      $ 0x5a,
    testCase "findFirstByteIn, wrapped, correctness"
      . assertEqual "findFirstByteIn" (Naive.findFirstByteIn asBA 3000000 2000000 0x5a)
      . findFirstByteIn asBA 3000000 2000000
      $ 0x5a,
    bench "findFirstByteIn, wrapped" . nf (findFirstByteIn asBA 3000000 2000000) $ 0x5a,
    bcompare "$NF == \"findFirstByteIn, wrapped\""
      . bench "findFirstByteIn, naive"
      . nf (Naive.findFirstByteIn asBA 3000000 2000000)
      $ 0x5a
  ]

flbTests :: ByteArray -> [Benchmark]
flbTests asBA =
  [ testCase "findLastByte, wrapped, correctness"
      . assertEqual "findLastByte" (Naive.findLastByte asBA 0x5a)
      . findLastByte asBA
      $ 0x5a,
    bench "findLastByte, wrapped" . nf (findLastByte asBA) $ 0x5a,
    bcompare "$NF == \"findLastByte, wrapped\""
      . bench "findLastByte, naive"
      . nf (Naive.findLastByte asBA)
      $ 0x5a,
    testCase "findLastByteIn, wrapped, correctness"
      . assertEqual "findLastByteIn" (Naive.findLastByteIn asBA 3000000 2000000 0x5a)
      . findLastByteIn asBA 3000000 2000000
      $ 0x5a,
    bench "findLastByteIn, wrapped" . nf (findLastByteIn asBA 3000000 2000000) $ 0x5a,
    bcompare "$NF == \"findLastByteIn, wrapped\""
      . bench "findLastByteIn, naive"
      . nf (Naive.findLastByteIn asBA 3000000 2000000)
      $ 0x5a
  ]
