{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: Data.Diablerie.Byte
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- Helpers for doing [SWAR](https://en.wikipedia.org/wiki/SWAR)-style things in
-- pure Haskell.
module Data.Diablerie.Swar
  ( -- * Type
    Laned8,

    -- * Functions

    -- ** Creation
    broadcast,
    zeroes,
    ones,

    -- ** Comparisons
    compareEq,

    -- ** Processing
    foldSwar,
  )
where

import Data.Bits (complement, xor, (.&.), (.|.))
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Primitive.ByteArray
  ( ByteArray,
    indexByteArray,
    sizeofByteArray,
  )
import Data.Primitive.Types (Prim)
import Data.Word (Word64, Word8)

-- | A wrapper for viewing a 'Word64' as eight lanes, each comprised of 8 bits.
--
-- @since 1.0
newtype Laned8 = Laned8 Word64
  deriving
    ( -- | @since 1.0
      Eq,
      Prim
    )
    via Word64
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | Copy a 'Word8' value to every lane.
--
-- If you need to broadcast a predefined constant, rather than an arbitrary
-- value, see if any of the following are suitable, as this could be faster:
--
-- * 'zeroes'
-- * 'ones'
-- * 'lowBits'
--
-- @since 1.0
{-# INLINE broadcast #-}
broadcast :: Word8 -> Laned8
broadcast w8 = Laned8 $ fromIntegral w8 * 0x0101010101010101

-- | Broadcast @0x00@ to every lane.
--
-- @since 1.0
zeroes :: Laned8
zeroes = Laned8 0

-- | Broadcast @0xFF@ to every lane.
--
-- @since 1.0
ones :: Laned8
ones = Laned8 . fromIntegral @Int64 $ (-1)

-- | Broadcast @0x7F@ to every lane.
--
-- @since 1.0
lowBits :: Laned8
lowBits = Laned8 0x7F7F7F7F7F7F7F7F

-- | Compare corresponding lanes in both arguments. Return a new 'Laned8' where
-- the corresponding lane contains @0x80@ if the lanes of the arguments matched,
-- and @0x00@ otherwise.
--
-- @since 1.0
{-# INLINE compareEq #-}
compareEq :: Laned8 -> Laned8 -> Laned8
compareEq (Laned8 w) (Laned8 w') =
  let smashed = w `xor` w'
      Laned8 mask = lowBits
      tmp = (smashed .&. mask) + mask
   in Laned8 $ complement (tmp .|. smashed .|. mask)

-- | Given a means to process an entire block of 8 bytes into a 'Monoid', as
-- well as a way to handle leftovers, collapse an entire 'ByteArray' into that
-- 'Monoid'.
--
-- This can be much more efficient than a byte-by-byte fold, as it traffics less
-- memory, and may gain parallel speedups by processing 'Laned8's instead of
-- individual 'Word8's.
--
-- @since 1.0
foldSwar ::
  forall (m :: Type).
  (Monoid m) =>
  (Laned8 -> m) ->
  (Word8 -> m) ->
  ByteArray ->
  m
foldSwar bigF smallF ba =
  let len = sizeofByteArray ba
      bigStrides = len `quot` 8
      bigResult = foldMap goBig [0 .. bigStrides - 1]
      smallResult = foldMap goHome [8 * bigStrides .. len - 1]
   in bigResult <> smallResult
  where
    goBig :: Int -> m
    goBig = bigF . indexByteArray ba
    goHome :: Int -> m
    goHome = smallF . indexByteArray ba
