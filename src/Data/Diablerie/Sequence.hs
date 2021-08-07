{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module: Data.Diablerie.Sequence
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- Functions for searching through 'ByteArray's (and 'ByteArray#'s) in a
-- sequence-oriented manner (that is, for sequences of bytes).
--
-- = Important note
--
-- No bounds checking (or indeed, checking of any prerequisites) is done by any
-- of these functions. Use with care.
module Data.Diablerie.Sequence
  ( -- * Wrapped operations

    -- ** Search
    findFirstMatch,
    findFirstMatchIn,

    -- * Raw operations

    -- ** Search
    findFirstMatchIn#,
  )
where

import Data.Primitive.ByteArray
  ( ByteArray (ByteArray),
    sizeofByteArray,
  )
import Foreign.C.Types
  ( CPtrdiff (CPtrdiff),
    CSize (CSize),
  )
import GHC.Exts (ByteArray#)

-- Wrapped ops

-- | A convenience wrapper for searching entire 'ByteArray's. More precisely,
-- @findFirstMatch needle haystack@ is the same as @'findFirstMatchIn needle 0
-- ('sizeofByteArray' needle) haystack 0 ('sizeofByteArray' haystack).
--
-- @since 1.0
findFirstMatch :: ByteArray -> ByteArray -> Maybe Int
findFirstMatch needle haystack =
  findFirstMatchIn
    needle
    0
    (sizeofByteArray needle)
    haystack
    0
    (sizeofByteArray haystack)

-- | Identical to 'findFirstMatchIn#', except using lifted types, and using a
-- 'Maybe' for the result to avoid negative-number indices.
--
-- = Prerequisites
--
-- Let @needle@ be the needle argument, @haystack@ be the haystack argument,
-- @off_n@ and @off_h@ be needle and haystack offsets respectively, and @len_n@
-- and @len_h@ be needle and haystack lengths respectively.
--
-- * @0 '<=' off_n@ and @off_n '<' 'sizeofByteArray' needle@
-- * @0 '<=' off_h@ and @off_h '<' 'sizeofByteArray' haystack@
-- * @0 '<=' len_n@ and @len_n + off_n '<=' 'sizeofByteArray' needle@
-- * @0 '<=' len_h@ and @len_h + off_h '<=' 'sizeofByteArray' haystack@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstMatchIn needle off_n len_n
-- haystack off_h len_h@.
--
-- * If @len_h = 0@ or @len_n > len_h@, then @res = -1@.
-- * If @len_n = 0@ and @len_h > 0@, then @res = 0@.
-- * If there exists @off_h <= i < off_h + len_h - len_n@ such that a
--   @len_n@-long match of @needle@ from @off_n@ exists in @haystack@ starting
--   at @i@, then @res@ will be the smallest such @i@.
-- * Otherwise, @res = -1@.
--
-- We observe that this follows the \'prefix rule\': if we find a match at
-- position @i@, then for any prefix of the needle so matched, we will also find
-- a match at a position not greater than @i@.
--
-- @since 1.0
findFirstMatchIn ::
  ByteArray ->
  Int ->
  Int ->
  ByteArray ->
  Int ->
  Int ->
  Maybe Int
findFirstMatchIn (ByteArray n#) off_n len_n (ByteArray h#) off_h len_h =
  let (CPtrdiff res) =
        findFirstMatchIn#
          n#
          (fromIntegral off_n)
          (fromIntegral len_n)
          h#
          (fromIntegral off_h)
          (fromIntegral len_h)
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

-- Raw ops

-- | Searches for the first occurrence of a needle in a haystack, with both
-- needle and haystack given offsets individually.
--
-- = Prerequisites
--
-- Let @needle@ be the needle argument, @haystack@ be the haystack argument,
-- @off_n@ and @off_h@ be needle and haystack offsets respectively, and @len_n@
-- and @len_h@ be needle and haystack lengths respectively.
--
-- * @0 '<=' off_n@ and @off_n '<' 'GHC.Exts.sizeofByteArray#' needle@
-- * @0 '<=' off_h@ and @off_h '<' 'GHC.Exts.sizeofByteArray#' haystack@
-- * @0 '<=' len_n@ and @len_n + off_n '<=' 'GHC.Exts.sizeofByteArray#' needle@
-- * @0 '<=' len_h@ and @len_h + off_h '<=' 'GHC.Exts.sizeofByteArray#' haystack@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstMatchIn# needle off_n len_n
-- haystack off_h len_h@.
--
-- * If @len_h = 0@ or @len_n > len_h@, then @res = -1@.
-- * If @len_n = 0@ and @len_h > 0@, then @res = 0@.
-- * If there exists @off_h <= i < off_h + len_h - len_n@ such that a
--   @len_n@-long match of @needle@ from @off_n@ exists in @haystack@ starting
--   at @i@, then @res@ will be the smallest such @i@.
-- * Otherwise, @res = -1@.
--
-- We observe that this follows the \'prefix rule\': if we find a match at
-- position @i@, then for any prefix of the needle so matched, we will also find
-- a match at a position not greater than @i@.
--
-- @since 1.0
foreign import ccall unsafe "find_first_match"
  findFirstMatchIn# ::
    -- | Needle storage
    ByteArray# ->
    -- | Needle offset
    CSize ->
    -- | Needle length
    CSize ->
    -- | Haystack storage
    ByteArray# ->
    -- | Haystack offset
    CSize ->
    -- | Haystack length
    CSize ->
    -- | Location as index, or -1 if not found
    CPtrdiff
