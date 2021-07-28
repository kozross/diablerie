{-# LANGUAGE DerivingStrategies #-}

module FindFirstNe (Inclusion (..), Exclusion (..)) where

import Data.List (delete)
import Data.Primitive.ByteArray (ByteArray)
import Data.Word (Word8)
import GHC.Exts (fromList, toList)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), shrinkList)
import Test.QuickCheck.Gen (elements, listOf)

data Inclusion
  = Inclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show)

instance Arbitrary Inclusion where
  arbitrary = do
    w8 <- arbitrary
    prefix <- listOf (pure w8)
    target <- elements (delete w8 [0x00 .. 0xFF])
    suffix <- listOf arbitrary
    let ix = length prefix
    pure . Inclusion (fromList $ prefix <> [target] <> suffix) w8 $ ix
  shrink (Inclusion ba w8 ix) = do
    let contents = toList ba
    let (prefix, rest) = splitAt ix contents
    prefix' <- shrinkList (const []) prefix
    ba' <- case rest of
      [x] -> pure . fromList $ prefix' <> [x]
      _ -> do
        suffix' <- shrink . tail $ rest
        let target = head rest
        pure . fromList $ prefix' <> [target] <> suffix'
    pure . Inclusion ba' w8 . length $ prefix'

data Exclusion
  = Exclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Word8
  deriving stock (Eq, Show)

instance Arbitrary Exclusion where
  arbitrary = do
    w8 <- arbitrary
    Exclusion <$> (fromList <$> listOf (pure w8)) <*> pure w8
  shrink (Exclusion ba w8) = do
    let contents = toList ba
    contents' <- shrinkList (const []) contents
    pure . Exclusion (fromList contents') $ w8
