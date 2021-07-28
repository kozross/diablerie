{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module: Data.Diablerie.ByteArray
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- Several functions working with 'ByteArray's and 'ByteArray#'s.
--
-- = Important note
--
-- No bounds checking (or indeed, checking of any prerequisites) is done by any
-- of these functions. Use with care.
module Data.Diablerie.ByteArray
  ( -- * Wrapped operations

    -- ** Search

    -- *** Byte
    findFirstEq,
    findFirstEqIn,
    findFirstNe,
    findFirstNeIn,
    findFirstGt,
    findFirstGtIn,
    findLastEq,
    findLastEqIn,

    -- *** Sequence
    findFirstMatch,
    findFirstMatchIn,

    -- ** Accumulate
    countEq,
    countEqIn,

    -- * Raw operations

    -- ** Search

    -- *** Byte
    findFirstEqIn#,
    findFirstNeIn#,
    findFirstGtIn#,
    findLastEqIn#,

    -- *** Sequence
    findFirstMatchIn#,

    -- ** Accumulate
    countEqIn#,
  )
where

import Data.Primitive.ByteArray
  ( ByteArray (ByteArray),
    sizeofByteArray,
  )
import Foreign.C.Types
  ( CInt (CInt),
    CPtrdiff (CPtrdiff),
    CSize (CSize),
  )
import GHC.Exts
  ( ByteArray#,
    Int#,
    word2Int#,
  )
import GHC.Word (Word8 (W8#))

-- Wrapped ops

-- | A convenience wrapper for searching the entire 'ByteArray'. More precisely,
-- @findFirstEq ba w8@ is the same as @'findFirstEqIn' ba 0 ('sizeofByteArray'
-- ba) w8@.
--
-- @since 1.0
findFirstEq :: ByteArray -> Word8 -> Maybe Int
findFirstEq ba = findFirstEqIn ba 0 (sizeofByteArray ba)

-- | Identical to 'findFirstEqIn#', except using lifted types, and using a
-- 'Maybe' for the result to avoid negative-number indices.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray' argument, and @w8@ the byte to match.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray' ba@
-- * @0 '<=' len@ and @len + off <= 'sizeofByteArray' ba@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstEqIn ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be in a 'Just' with a value in the range @off .. off + len - 1@
-- * Otherwise, @res = 'Nothing'@.
--
-- @since 1.0
findFirstEqIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findFirstEqIn (ByteArray ba#) off len (W8# w8#) =
  let (CPtrdiff res) =
        findFirstEqIn#
          ba#
          (fromIntegral off)
          (fromIntegral len)
          (word2Int# w8#)
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

-- | A convenience wrapper for searching the entire 'ByteArray'. More precisely,
-- @findFirstNe ba w8@ is the same as @'findFirstNeIn' ba 0 ('sizeofByteArray'
-- ba) w8@.
--
-- @since 1.0
findFirstNe :: ByteArray -> Word8 -> Maybe Int
findFirstNe ba = findFirstNeIn ba 0 (sizeofByteArray ba)

-- | Identical to 'findFirstNeIn#', except using lifted types, and using a
-- 'Maybe' for the result to avoid negative-number indices.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray' argument, and @w8@ the byte to differ from.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray' ba@
-- * @0 '<=' len@ and @len + off <= 'sizeofByteArray' ba@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstEqIn ba off len w8@.
--
-- * If a byte different to @w8@ exists in the index range @off .. off + len -
--   1@, then @res@ will be 'Just' the index of the first such byte, in the
--   range @off .. off + len - 1@
-- * Otherwise, @res = 'Nothing'@.
--
-- @since 1.0
findFirstNeIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findFirstNeIn (ByteArray ba#) off len (W8# w8#) =
  let (CPtrdiff res) =
        findFirstNeIn#
          ba#
          (fromIntegral off)
          (fromIntegral len)
          (word2Int# w8#)
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

-- | A convenience wrapper for searching the entire 'ByteArray'. More precisely,
-- @findFirstGt ba w8@ is the same as @'findFirstGtIn' ba 0 ('sizeofByteArray'
-- ba) w8@.
--
-- @since 1.0
findFirstGt :: ByteArray -> Word8 -> Maybe Int
findFirstGt ba = findFirstGtIn ba 0 (sizeofByteArray ba)

-- | Identical to 'findFirstGtIn#', except using lifted types, and using a
-- 'Maybe' for the result to avoid negative-number indices.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray' argument, and @w8@ the byte to match.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray' ba@
-- * @0 '<=' len@ and @len + off <= 'sizeofByteArray' ba@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstGtIn ba off len w8@.
--
-- * If @res > -1@, then @'Data.Primitive.ByteArray.indexByteArray' ba res '>' w8@.
-- * If @res > -1@, then for any @0 <= i < res@, @'Data.Primitive.ByteArray.indexByteArray' ba i '<='
--   w8@.
-- * If @res = -1@, then every byte in @ba@ is equal to, or less than, @w8@.
--
-- @since 1.0
findFirstGtIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findFirstGtIn (ByteArray ba#) off len (W8# w8#) =
  let (CPtrdiff res) =
        findFirstGtIn#
          ba#
          (fromIntegral off)
          (fromIntegral len)
          (word2Int# w8#)
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

-- | A convenience wrapper for searching the entire 'ByteArray'. More precisely,
-- @findLastEq ba w8@ is the same as @'findLastEqIn' ba 0 ('sizeofByteArray'
-- ba) w8@.
--
-- @since 1.0
findLastEq :: ByteArray -> Word8 -> Maybe Int
findLastEq ba = findLastEqIn ba 0 (sizeofByteArray ba)

-- | Identical to 'findLastEqIn#', except using lifted types, and using a
-- 'Maybe' for the result to avoid negative-number indices.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray' argument, and @w8@ the byte to match.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray' ba@
-- * @0 '<=' len@ and @len + off <= 'sizeofByteArray' ba@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findLastEqIn ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be in a 'Just' with a value in the range @off .. off + len - 1@
-- * Otherwise, @res = 'Nothing'@.
--
-- @since 1.0
findLastEqIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findLastEqIn (ByteArray ba#) off len (W8# w8#) =
  let (CPtrdiff res) =
        findLastEqIn#
          ba#
          (fromIntegral off)
          (fromIntegral len)
          (word2Int# w8#)
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

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

-- | A convenience wrapper for counting the entire 'ByteArray'. More precisely,
-- @countEq ba w8@ is the same as @'countEqIn' ba 0 ('sizeofByteArray' ba)
-- w8@.
--
-- @since 1.0
countEq :: ByteArray -> Word8 -> Int
countEq ba = countEqIn ba 0 (sizeofByteArray ba)

-- | Identical to 'countEqIn#', except using lifted types.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length argument, @ba@ the
-- 'ByteArray' argument, and @w8@ the byte to count.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray' ba@
-- * @0 '<=' len@ and @len + off <= 'sizeofByteArray' ba@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @countEqIn ba off len w8@.
--
-- * @0 '<=' res@ and @res '<' len@.
--
-- @since 1.0
countEqIn :: ByteArray -> Int -> Int -> Word8 -> Int
countEqIn (ByteArray ba#) off len (W8# w8#) =
  let (CInt res) =
        countEqIn#
          ba#
          (fromIntegral off)
          (fromIntegral len)
          (word2Int# w8#)
   in fromIntegral res

-- Raw ops

-- | Searches a byte array from an offset for the index of the
-- first occurrence of a byte.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray#' argument, and @w8@ the byte to match.
--
-- * @0 '<=' off@ and @off '<' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstEqIn# ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be the index of the first match, in the range @off .. off + len - 1@
-- * Otherwise, @res = -1@
--
-- @since 1.0
foreign import ccall unsafe "find_first_eq"
  findFirstEqIn# ::
    -- | The memory area to search
    ByteArray# ->
    -- | Offset from the start
    CSize ->
    -- | How many bytes to check
    CSize ->
    -- | What byte to match (only low 8 bits)
    Int# ->
    -- | Location as index, or -1 if not found
    CPtrdiff

-- | Searches a byte array from an offset for the index of the
-- first non-occurrence of a byte.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray#' argument, and @w8@ the byte to differ from.
--
-- * @0 '<=' off@ and @off '<' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstEqIn# ba off len w8@.
--
-- * If a byte different to @w8@ exists in the index range @off .. off + len -
--   1@, then @res@ will be the index of the first such byte, in the range @off
--   .. off + len - 1@
-- * Otherwise, @res = -1@.
--
-- @since 1.0
foreign import ccall unsafe "find_first_ne"
  findFirstNeIn# ::
    -- | The memory area to search
    ByteArray# ->
    -- | Offset from the start
    CSize ->
    -- | How many bytes to check
    CSize ->
    -- | What byte to differ from (only low 8 bits)
    Int# ->
    -- | Location as index, or -1 if not found
    CPtrdiff

-- | Searches a byte array from an offset for the index of the
-- first occurrence of a byte greater than the argument.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray#' argument, and @w8@ the byte argument.
--
-- * @0 '<=' off@ and @off '<' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstGtIn# ba off len w8@.
--
-- * If @res > -1@, then @'GHC.Exts.indexByteArray#' ba res '>' w8@.
-- * If @res > -1@, then for any @0 <= i < res@, @'GHC.Exts.indexByteArray#' ba i '<='
--   w8@.
-- * If @res = -1@, then every byte in @ba@ is equal to, or less than, @w8@.
--
-- @since 1.0
foreign import ccall unsafe "find_first_gt"
  findFirstGtIn# ::
    -- | The memory area to search
    ByteArray# ->
    -- | Offset from the start
    CSize ->
    -- | How many bytes to check
    CSize ->
    -- | Byte target (only low 8 bits)
    Int# ->
    -- | Location as index, or -1 if not found
    CPtrdiff

-- | Searches a byte array from an offset for the index of the
-- last occurrence of a byte.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray#' argument, and @w8@ the byte to match.
--
-- * @0 '<=' off@ and @off '<' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findLastEqIn# ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be in the range @off .. off + len - 1@
-- * Otherwise, @res = -1@
--
-- @since 1.0
foreign import ccall unsafe "find_last_eq"
  findLastEqIn# ::
    -- | The memory area to search
    ByteArray# ->
    -- | Offset from the start
    CSize ->
    -- | How many bytes to check
    CSize ->
    -- | What byte to match (only low 8 bits)
    Int# ->
    -- | Location as index, or -1 if not found
    CPtrdiff

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

-- | Counts the occurrences of a specific byte in a byte array from an offset.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length argument, @ba@ the
-- 'ByteArray#' argument, and @w8@ the byte to count.
--
-- * @0 '<=' off@ and @off '<' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'GHC.Exts.sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @countEqIn# ba off len w8@.
--
-- * @0 '<=' res@ and @res '<' len@.
--
-- @since 1.0
foreign import ccall unsafe "count_eq"
  countEqIn# ::
    -- | The memory area to count
    ByteArray# ->
    -- | Offset from the start
    CSize ->
    -- | How many bytes to check
    CSize ->
    -- | What byte to count (only low 8 bits)
    Int# ->
    -- | How many times the byte occurs
    CInt
