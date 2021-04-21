module Main (main) where

import qualified Data.ByteString as BS
import Data.Interieur.ByteArray
  ( countBytesEq,
    findFirstByte,
    findFirstByteIn,
    findLastByte,
    findLastByteIn,
  )
import Data.Primitive.ByteArray (ByteArray)
import Data.Word (Word8)
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

everyByte :: [Word8]
everyByte = [minBound .. maxBound]

runAllTests :: ByteArray -> IO ()
runAllTests asBA =
  defaultMain
    [ bgroup "findFirstByte (wrapped)" . ffbTests $ asBA,
      bgroup "findLastByte (wrapped)" . flbTests $ asBA,
      bgroup "countBytesEq (wrapped)" . cbeTests $ asBA
    ]

cbeTests :: ByteArray -> [Benchmark]
cbeTests asBA =
  [ testCase "countBytesEq, wrapped, correctness"
      . assertEqual "countBytesEq" (Naive.countBytesEq asBA <$> everyByte)
      . fmap (countBytesEq asBA)
      $ everyByte,
    bench "countBytesEq, wrapped" . nf (fmap (countBytesEq asBA)) $ everyByte,
    bcompare "$NF == \"countBytesEq, wrapped\""
      . bench "countBytesEq, naive"
      . nf (fmap (Naive.countBytesEq asBA))
      $ everyByte
  ]

ffbTests :: ByteArray -> [Benchmark]
ffbTests asBA =
  [ testCase "findFirstByte, wrapped, correctness"
      . assertEqual "findFirstByte" (Naive.findFirstByte asBA <$> everyByte)
      . fmap (findFirstByte asBA)
      $ everyByte,
    bench "findFirstByte, wrapped" . nf (fmap (findFirstByte asBA)) $ everyByte,
    bcompare "$NF == \"findFirstByte, wrapped\""
      . bench "findFirstByte, naive"
      . nf (fmap (Naive.findFirstByte asBA))
      $ everyByte,
    testCase "findFirstByteIn, wrapped, correctness"
      . assertEqual "findFirstByteIn" (Naive.findFirstByteIn asBA 3000000 2000000 <$> everyByte)
      . fmap (findFirstByteIn asBA 3000000 2000000)
      $ everyByte,
    bench "findFirstByteIn, wrapped" . nf (fmap (findFirstByteIn asBA 3000000 2000000)) $ everyByte,
    bcompare "$NF == \"findFirstByteIn, wrapped\""
      . bench "findFirstByteIn, naive"
      . nf (fmap (Naive.findFirstByteIn asBA 3000000 2000000))
      $ everyByte
  ]

flbTests :: ByteArray -> [Benchmark]
flbTests asBA =
  [ testCase "findLastByte, wrapped, correctness"
      . assertEqual "findLastByte" (Naive.findLastByte asBA <$> everyByte)
      . fmap (findLastByte asBA)
      $ everyByte,
    bench "findLastByte, wrapped" . nf (fmap (findLastByte asBA)) $ everyByte,
    bcompare "$NF == \"findLastByte, wrapped\""
      . bench "findLastByte, naive"
      . nf (fmap (Naive.findLastByte asBA))
      $ everyByte,
    testCase "findLastByteIn, wrapped, correctness"
      . assertEqual "findLastByteIn" (Naive.findLastByteIn asBA 3000000 2000000 <$> everyByte)
      . fmap (findLastByteIn asBA 3000000 2000000)
      $ everyByte,
    bench "findLastByteIn, wrapped" . nf (fmap (findLastByteIn asBA 3000000 2000000)) $ everyByte,
    bcompare "$NF == \"findLastByteIn, wrapped\""
      . bench "findLastByteIn, naive"
      . nf (fmap (Naive.findLastByteIn asBA 3000000 2000000))
      $ everyByte
  ]
