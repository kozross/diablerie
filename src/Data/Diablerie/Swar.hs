{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    lowBits,
    highBits,

    -- ** Comparisons
    compareEq,
    compareNe,

    -- ** Evacuation
    extractHighBits,

    -- ** Bulk processing
    foldSwar,
  )
where

import Data.Bits (Bits (complement, xor, (.&.), (.|.)), FiniteBits)
import Data.Kind (Type)
import Data.Primitive.ByteArray
  ( ByteArray,
    indexByteArray,
    sizeofByteArray,
  )
import Data.Primitive.Types (Prim)
import GHC.Exts (pext64#)
import GHC.Word (Word64 (W64#), Word8 (W8#))

-- | A wrapper for viewing a 'Word64' as eight lanes, each comprised of a
-- 'Word8'.
--
-- @since 1.0
newtype Laned8 = Laned8 Word64
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Prim,
      -- | @since 1.0
      Bits,
      -- | @since 1.0
      FiniteBits
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
-- * 'highBits'
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
ones = Laned8 0xFFFFFFFFFFFFFFFF

-- | Broadcast @0x7F@ to every lane.
--
-- @since 1.0
lowBits :: Laned8
lowBits = Laned8 0x7F7F7F7F7F7F7F7F

-- | Broadcast @0x80@ to every lane.
--
-- @since 1.0
highBits :: Laned8
highBits = Laned8 0x8080808080808080

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

-- | Compare corresponding lanes in both arguments. Return a new 'Laned8' where
-- the corresponding lane contains @0x80@ if the lanes of the arguments didn't
-- match, and @0x00@ otherwise.
--
-- @since 1.0
{-# INLINE compareNe #-}
compareNe :: Laned8 -> Laned8 -> Laned8
compareNe (Laned8 w) (Laned8 w') =
  let smashed = w `xor` w'
      Laned8 mask = lowBits
      tmp = (smashed .&. mask) + mask
   in Laned8 $ mask `xor` (tmp .|. smashed .|. mask)

-- | Similar to the @movemask@ operations provided by the Intel SIMD instruction
-- sets. Collects the high-order bits (set or not), and packs them into a
-- 'Word8'.
--
-- @since 1.0
{-# INLINE extractHighBits #-}
extractHighBits :: Laned8 -> Word8
extractHighBits (Laned8 (W64# w#)) =
  let !(Laned8 (W64# mask#)) = highBits
   in W8# (pext64# w# mask#)

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
