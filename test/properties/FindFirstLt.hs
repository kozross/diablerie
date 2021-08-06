{-# LANGUAGE DerivingStrategies #-}

module FindFirstLt (Exclusion (..), Inclusion (..)) where

import Data.Primitive.ByteArray (ByteArray)
import Data.Word (Word8)
import GHC.Exts (fromList, toList)
import Test.QuickCheck.Arbitrary
  ( Arbitrary (arbitrary, shrink),
    shrinkList,
  )
import Test.QuickCheck.Gen (elements, listOf)

data Exclusion
  = Exclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Word8
  deriving stock (Eq, Show)

instance Arbitrary Exclusion where
  arbitrary = do
    w8 <- arbitrary
    let byteGen = elements [w8 ..]
    bytes <- listOf byteGen
    pure . Exclusion (fromList bytes) $ w8
  shrink (Exclusion ba w8) = do
    let byteList = toList ba
    byteList' <- shrinkList (const []) byteList
    pure . Exclusion (fromList byteList') $ w8

data Inclusion
  = Inclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show)

instance Arbitrary Inclusion where
  arbitrary = do
    w8 <- elements [0x01 ..]
    let target = pred w8
    let prefixGen = elements [w8 ..]
    prefix <- listOf prefixGen
    suffix <- listOf arbitrary
    let ba = fromList $ prefix <> [target] <> suffix
    let ix = length prefix
    pure . Inclusion ba w8 $ ix
  shrink (Inclusion ba w8 ix) = do
    let byteList = toList ba
    let (prefix, rest) = splitAt ix byteList
    prefix' <- shrinkList (const []) prefix
    let ix' = length prefix'
    case rest of
      [_] -> pure . Inclusion (fromList $ prefix' <> rest) w8 $ ix'
      _ -> do
        let target = head rest
        let suffix = tail rest
        suffix' <- shrink suffix
        let ba' = prefix' <> [target] <> suffix'
        pure . Inclusion (fromList ba') w8 $ ix'
