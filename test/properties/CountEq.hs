{-# LANGUAGE DerivingStrategies #-}

module CountEq (CountEq (..)) where

import Control.Monad (replicateM)
import Data.List (delete, intercalate)
import Data.List.Split (splitOn)
import Data.Primitive.ByteArray (ByteArray)
import Data.Word (Word8)
import GHC.Exts (fromList, toList)
import Test.QuickCheck.Arbitrary
  ( Arbitrary (arbitrary, shrink),
    shrinkList,
  )
import Test.QuickCheck.Gen (elements, listOf)

data CountEq
  = CountEq
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show)

instance Arbitrary CountEq where
  arbitrary = do
    w8 <- arbitrary
    let gen = elements . delete w8 $ [0x00 .. 0xFF]
    count <- abs <$> arbitrary
    ba <- fromList . intercalate [w8] <$> replicateM (count + 1) (listOf gen)
    pure . CountEq ba w8 $ count
  shrink (CountEq ba w8 _) = do
    let baList = toList ba
    let pieces = splitOn [w8] baList
    pieces' <- shrinkList (const []) pieces
    case pieces' of
      [] -> pure . CountEq (fromList []) w8 $ 0
      _ -> do
        let insertions = length pieces' - 1
        let ba' = fromList . intercalate [w8] $ pieces'
        pure . CountEq ba' w8 $ insertions
