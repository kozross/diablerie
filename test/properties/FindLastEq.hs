{-# LANGUAGE DerivingStrategies #-}

module FindLastEq (Inclusion (..), Exclusion (..)) where

import Data.List (delete)
import Data.Primitive.ByteArray (ByteArray)
import Data.Word (Word8)
import GHC.Exts (fromList, toList)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), shrinkList)
import Test.QuickCheck.Gen (Gen, elements, listOf)

data Inclusion
  = Inclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show)

instance Arbitrary Inclusion where
  arbitrary = do
    w8 <- arbitrary
    prefix <- arbitrary
    suffix <- listOf (elements . delete w8 $ [0x00 .. 0xFF])
    let ix = length prefix
    pure . Inclusion (fromList $ prefix <> [w8] <> suffix) w8 $ ix
  shrink (Inclusion ba w8 ix) = do
    let contents = toList ba
    let (prefix, rest) = splitAt ix contents
    prefix' <- shrink prefix
    ba' <- case rest of
      [_] -> pure . fromList $ prefix' <> [w8]
      _ -> do
        suffix' <- shrinkList (const []) . tail $ rest
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
    Exclusion <$> (fromList <$> (listOf . go $ w8)) <*> pure w8
    where
      go :: Word8 -> Gen Word8
      go w8 = elements (delete w8 [0x00 .. 0xFF])
  shrink (Exclusion ba w8) = do
    ba' <- fmap fromList . shrinkList (const []) . toList $ ba
    pure . Exclusion ba' $ w8
