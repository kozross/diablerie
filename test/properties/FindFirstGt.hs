{-# LANGUAGE DerivingStrategies #-}

module FindFirstGt (Inclusion (..), Exclusion (..)) where

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
    w8 <- elements [0x01 .. 0xFF]
    let w8' = w8 - 1
    prefix <- listOf . elements $ [0x00 .. w8']
    suffix <- arbitrary
    let ix = length prefix
    pure . Inclusion (fromList $ prefix <> [w8] <> suffix) w8' $ ix
  shrink (Inclusion ba w8 ix) = do
    let contents = toList ba
    let (prefix, rest) = splitAt ix contents
    prefix' <- shrinkList (const []) prefix
    ba' <- case rest of
      [_] -> pure . fromList $ prefix' <> [w8]
      _ -> do
        suffix' <- shrink . tail $ rest
        pure . fromList $ prefix' <> [w8] <> suffix'
    pure . Inclusion ba' w8 . length $ prefix'

data Exclusion
  = Exclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Word8
  deriving stock (Eq, Show)

instance Arbitrary Exclusion where
  arbitrary = do
    w8 <- arbitrary
    let byteGen = elements [0x00 .. w8]
    Exclusion <$> (fromList <$> listOf byteGen) <*> pure w8
  shrink (Exclusion ba w8) = do
    ba' <- fmap fromList . shrinkList (const []) . toList $ ba
    pure . Exclusion ba' $ w8
