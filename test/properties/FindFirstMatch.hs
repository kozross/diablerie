{-# LANGUAGE DerivingStrategies #-}

module FindFirstMatch
  ( Inclusion (..),
    Exclusion (..),
    Prefix (..),
  )
where

import Control.Applicative (empty)
import Data.List (nub, (\\))
import Data.Primitive.ByteArray (ByteArray)
import GHC.Exts (fromList, toList)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), shrinkList)
import Test.QuickCheck.Gen (chooseInt, elements, listOf)

data Prefix
  = Prefix
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !ByteArray
  deriving stock (Eq, Show)

instance Arbitrary Prefix where
  arbitrary = do
    needle <- listOf arbitrary
    case needle of
      [] -> do
        h <- arbitrary
        hs <- listOf arbitrary
        let haystack = h : hs
        pure . Prefix (fromList needle) (fromList haystack) . fromList $ []
      (_ : _) -> do
        lim <- chooseInt (0, length needle)
        let prefix = take lim needle
        haystackPrefix <- listOf arbitrary
        haystackSuffix <- listOf arbitrary
        let haystack = haystackPrefix <> needle <> haystackSuffix
        pure . Prefix (fromList needle) (fromList haystack) . fromList $ prefix
  shrink (Prefix needle haystack prefix) = do
    let prefixContents = toList prefix
    case prefixContents of
      [] -> empty
      (_ : _) -> do
        newPrefixLen <- [0 .. length prefixContents]
        pure . Prefix needle haystack
          . fromList
          . take newPrefixLen
          $ prefixContents

data Inclusion
  = Inclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show)

instance Arbitrary Inclusion where
  arbitrary = do
    needle <- listOf arbitrary
    case needle of
      [] -> do
        h <- arbitrary
        hs <- listOf arbitrary
        pure . Inclusion (fromList needle) (fromList $ h : hs) $ 0
      (n : ns) -> do
        let usedBytes = n : nub ns
        let unusedByte = elements ([0x00 .. 0xFF] \\ usedBytes)
        prefix <- listOf unusedByte
        suffix <- listOf arbitrary
        let ix = length prefix
        let haystack = prefix <> needle <> suffix
        pure . Inclusion (fromList needle) (fromList haystack) $ ix
  shrink (Inclusion needle haystack ix) = do
    let needleContents = toList needle
    let haystackContents = toList haystack
    case needleContents of
      [] -> do
        haystack' <- fromList <$> shrinkList (const []) haystackContents
        pure . Inclusion needle haystack' $ ix
      (_ : _) -> do
        let prefix = take ix haystackContents
        prefix' <- shrinkList (const []) prefix
        let suffix = drop (ix + length needleContents) haystackContents
        suffix' <- shrinkList (const []) suffix
        let ix' = length prefix'
        let haystack' = prefix' <> needleContents <> suffix'
        pure . Inclusion needle (fromList haystack') $ ix'

data Exclusion
  = Exclusion
      {-# UNPACK #-} !ByteArray
      {-# UNPACK #-} !ByteArray
  deriving stock (Eq, Show)

instance Arbitrary Exclusion where
  arbitrary = do
    needleHead <- arbitrary
    needle <- listOf arbitrary
    let usedBytes = needleHead : nub needle
    let unusedByte = elements ([0x00 .. 0xFF] \\ usedBytes)
    haystack <- listOf unusedByte
    pure . Exclusion (fromList $ needleHead : needle) . fromList $ haystack
  shrink (Exclusion needle haystack) = do
    let needleContents = toList needle
    case needleContents of
      [] -> empty -- technically impossible
      (n : ns) -> do
        needle' <- shrinkList (const []) ns
        let haystackContents = toList haystack
        haystack' <- shrinkList (const []) haystackContents
        pure . Exclusion (fromList $ n : needle') . fromList $ haystack'
