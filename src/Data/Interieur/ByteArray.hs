{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module: Data.ByteArray.Interieur
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
module Data.Interieur.ByteArray
  ( -- * Wrapped operations
    findFirstByte,
    findFirstByteIn,
    findFirst,
    findFirstIn,
    findLastByte,
    findLastByteIn,
    countBytesEq,
    countBytesEqIn,
    countBitsSet,
    countBitsSetIn,

    -- * Raw operations
    findFirstByte#,
    findFirstByteIn#,
    findLastByte#,
    findLastByteIn#,
    findFirst#,
    findFirstIn#,
    countBytesEq#,
    countBytesEqIn#,
    countBitsSet#,
    countBitsSetIn#,
  )
where

import Data.Primitive.ByteArray
  ( ByteArray (ByteArray),
    sizeofByteArray,
  )
import Foreign.C.Types (CInt (CInt), CPtrdiff (CPtrdiff))
import GHC.Exts
  ( ByteArray#,
    Int (I#),
    Int#,
    sizeofByteArray#,
    word2Int#,
  )
import GHC.Word (Word8 (W8#))

-- Wrapped ops

-- | A convenience wrapper for searching the entire 'ByteArray'. More precisely,
-- @findFirstByte ba w8@ is the same as @'findFirstByteIn' ba 0 ('sizeofByteArray'
-- ba) w8@.
--
-- @since 1.0.0
findFirstByte :: ByteArray -> Word8 -> Maybe Int
findFirstByte ba = findFirstByteIn ba 0 (sizeofByteArray ba)

-- | Identical to 'findFirstByteIn#', except using lifted types, and using a
-- 'Maybe' argument for the result to avoid negative-number indices.
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
-- Let @res@ be the result of a call @findFirstByteIn ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be in a 'Just' with a value in the range @off .. off + len - 1@
-- * Otherwise, @res = 'Nothing'@.
--
-- @since 1.0.0
findFirstByteIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findFirstByteIn (ByteArray ba#) (I# off#) (I# len#) (W8# w8#) =
  let (CPtrdiff res) = findFirstByteIn# ba# off# len# (word2Int# w8#)
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

-- | A convenience wrapper for searching the entire haystack for the entire
-- needle. More precisely, @findFirst haystack needle@ is the same as
-- @'findFirstIn' haystack 0 ('sizeofByteArray' haystack) needle 0
-- ('sizeofByteArray' needle).
--
-- @since 1.0.0
findFirst :: ByteArray -> ByteArray -> Maybe Int
findFirst haystack needle =
  findFirstIn
    haystack
    0
    (sizeofByteArray haystack)
    needle
    0
    (sizeofByteArray needle)

-- | Identical to 'findFirstIn#', except using lifted types, and using a 'Maybe'
-- argument for the result to avoid negative-number indices.
--
-- = Prerequisites
--
-- Let @haystack@ be the 'ByteArray' argument we want to search /in/, @needle@
-- be the 'ByteArray' argument we want to search /for/, @haystackOff@ be the
-- offset into @haystack@, @needleOff@ be the offset into @needle@,
-- @haystackLen@ be the length of @haystack@ to check in, @needleLen@ be the
-- length of the @needle@ to check in.
--
-- * @0 '<=' haystackOff@ and @haystackOff '<' 'sizeofByteArray' haystack@
-- * @0 '<=' needleOff@ and @needleOff '<' 'sizeofByteArray' needle@
-- * @0 '<=' haystackLen@ and @haystackLen + haystackOff '<=' 'sizeofByteArray' haystack@
-- * @0 '<=' needleLen@ and @needleLen + needleOff '<=' 'sizeofByteArray' needle@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstIn haystack haystackOff
-- haystackLen needle needleOff needleLen@.
--
-- * If @needleLen '==' 0@ or @needleLen > haystackLen@, @res = -1@
-- * If @needle@ at @needleOff@ for @needleLen@ exists in @haystack@ at or after
--   @haystackOff@ and entirely within @haystackLen@, @res@ is the first index
--   at which the specified chunk of @needle@ can be found in @haystack@.
-- * Otherwise, @res = -1@.
--
-- @since 1.0.0
findFirstIn :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> Maybe Int
findFirstIn (ByteArray h#) (I# hoff#) (I# hlen#) (ByteArray n#) (I# noff#) (I# nlen#) =
  let (CPtrdiff res) = findFirstIn# h# hoff# hlen# n# noff# nlen#
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

-- | A convenience wrapper for searching the entire 'ByteArray'. More precisely,
-- @findLastByte ba w8@ is the same as @'findLastByteIn' ba 0 ('sizeofByteArray'
-- ba) w8@.
--
-- @since 1.0.0
findLastByte :: ByteArray -> Word8 -> Maybe Int
findLastByte ba = findLastByteIn ba 0 (sizeofByteArray ba)

-- | A convenience wrapper for counting the entire 'ByteArray'. More precisely,
-- @countBytesEq ba w8@ is the same as @'countBytesEqIn' ba 0 ('sizeofByteArray' ba)
-- w8@.
--
-- @since 1.0.0
countBytesEq :: ByteArray -> Word8 -> Int
countBytesEq ba = countBytesEqIn ba 0 (sizeofByteArray ba)

-- | A convenience wrapper for counting the entire 'ByteArray'. More precisely,
-- @countBitsSet ba@ is the same as @countBitsSetIn ba 0# ('sizeofByteArray'
-- ba)@.
--
-- @since 1.0.0
countBitsSet :: ByteArray -> Int
countBitsSet ba = countBitsSetIn ba 0 (sizeofByteArray ba)

-- | Identical to 'findLastByteIn#', except using lifted types, and using a
-- 'Maybe' argument for the result to avoid negative-number indices.
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
-- Let @res@ be the result of a call @findFirstByteIn ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be in a 'Just' with a value in the range @off .. off + len - 1@
-- * Otherwise, @res = 'Nothing'@.
--
-- @since 1.0.0
findLastByteIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findLastByteIn (ByteArray ba#) (I# off#) (I# len#) (W8# w8#) =
  let (CPtrdiff res) = findLastByteIn# ba# off# len# (word2Int# w8#)
   in case signum res of
        (-1) -> Nothing
        _ -> Just . fromIntegral $ res

-- | Identical to 'countBytesEqIn#', except using lifted types.
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
-- Let @res@ be the result of a call @countBytesEqIn ba off len w8@.
--
-- * @0 '<=' res@ and @res '<' len@.
--
-- @since 1.0.0
countBytesEqIn :: ByteArray -> Int -> Int -> Word8 -> Int
countBytesEqIn (ByteArray ba#) (I# off#) (I# len#) (W8# w8#) =
  let (CInt res) = countBytesEqIn# ba# off# len# (word2Int# w8#)
   in fromIntegral res

-- | Counts the number of set bits (1-bits) in a byte array from an offset.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length argument, and @ba@ be
-- the 'ByteArray' argument.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray' ba@
-- * @0 '<=' len@ and @len + off '<=' 'sizeofByteArray' ba@
--
-- = Outcomes
--
-- Let @resSet@ be the result of a call @countBitsSetIn ba off len@, and
-- @resClear@ be the result of a call @countBitsClearIn ba off len@.
--
-- * @0 '<=' resSet@ and @resSet '<' (len '*' 8)@
-- * @resSet '+' resClear '==' len '*' 8@
--
-- @since 1.0.0
countBitsSetIn :: ByteArray -> Int -> Int -> Int
countBitsSetIn (ByteArray ba#) (I# off#) (I# len#) =
  let (CInt res) = countBitsSetIn# ba# off# len#
   in fromIntegral res

-- Raw ops

-- | A convenience wrapper for searching the entire 'ByteArray#'. More
-- precisely, @findFirstByte# ba w8#@ is the same as @'findFirstByteIn#' ba 0#
-- ('sizeofByteArray#' ba) w8@.
--
-- @since 1.0.0
findFirstByte# :: ByteArray# -> Int# -> CPtrdiff
findFirstByte# ba# = findFirstByteIn# ba# 0# (sizeofByteArray# ba#)

-- | A convenience wrapper for searching the entire haystack for the entire
-- needle. More precisely, @findFirst# haystack needle@ is the same as
-- @'findFirst# haystack 0 ('sizeofByteArray# haystack) needle 0
-- ('sizeofByteArray# needle)@.
--
-- @since 1.0.0
findFirst# :: ByteArray# -> ByteArray# -> CPtrdiff
findFirst# haystack# needle# =
  findFirstIn#
    haystack#
    0#
    (sizeofByteArray# haystack#)
    needle#
    0#
    (sizeofByteArray# needle#)

-- | A convenience wrapper for searching the entire 'ByteArray#'. More
-- precisely, @findLastByte# ba w8@ is the same as @'findLastByteIn#' ba 0#
-- ('sizeofByteArray#' ba) w8@.
--
-- @since 1.0.0
findLastByte# :: ByteArray# -> Int# -> CPtrdiff
findLastByte# ba# = findLastByteIn# ba# 0# (sizeofByteArray# ba#)

-- | A convenience wrapper for counting in the entire 'ByteArray#'. More
-- precisely, @countBytesEq# ba w8@ is the same as @'countBytesEqIn#' ba 0#
-- ('sizeofByteArray#' ba) w8@.
--
-- @since 1.0.0
countBytesEq# :: ByteArray# -> Int# -> CInt
countBytesEq# ba# = countBytesEqIn# ba# 0# (sizeofByteArray# ba#)

-- | A convenience wrapper for counting the entire 'ByteArray#'. More precisely,
-- @countBitsSet# ba@ is the same as @countBitsSetIn# ba 0# ('sizeofByteArray#'
-- ba)@.
--
-- @since 1.0.0
countBitsSet# :: ByteArray# -> CInt
countBitsSet# ba# = countBitsSetIn# ba# 0# (sizeofByteArray# ba#)

-- | Searches a byte array from an offset for the index of the
-- first occurrence of a byte.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length
-- argument, @ba@ the 'ByteArray#' argument, and @w8@ the byte to match.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstByteIn# ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be in the range @off .. off + len - 1@
-- * Otherwise, @res = -1@
--
-- = Notes
--
-- This calls @[memchr](https://man7.org/linux/man-pages/man3/memchr.3.html)@ underneath.
--
-- @since 1.0.0
foreign import ccall unsafe "find_first_byte"
  findFirstByteIn# ::
    -- | The memory area to search
    ByteArray# ->
    -- | Offset from the start
    Int# ->
    -- | How many bytes to check
    Int# ->
    -- | What byte to match (only low 8 bits)
    Int# ->
    -- | Location as index, or -1 if not found
    CPtrdiff

-- | Searches a byte array from an offset for the index of the first occurrence
-- of another byte array, also from an offset.
--
-- = Prerequisites
--
-- Let @haystack@ be the 'ByteArray#' argument we want to search /in/, @needle@
-- be the 'ByteArray#' argument we want to search /for/, @haystackOff@ be the
-- offset into @haystack@, @needleOff@ be the offset into @needle@,
-- @haystackLen@ be the length of @haystack@ to check in, @needleLen@ be the
-- length of the @needle@ to check in.
--
-- * @0 '<=' haystackOff@ and @haystackOff '<' 'sizeofByteArray#' haystack@
-- * @0 '<=' needleOff@ and @needleOff '<' 'sizeofByteArray#' needle@
-- * @0 '<=' haystackLen@ and @haystackLen + haystackOff '<=' 'sizeofByteArray#' haystack@
-- * @0 '<=' needleLen@ and @needleLen + needleOff '<=' 'sizeofByteArray#' needle@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstIn# haystack haystackOff
-- haystackLen needle needleOff needleLen@.
--
-- * If @needleLen '==' 0@ or @needleLen > haystackLen@, @res = -1@
-- * If @needle@ at @needleOff@ for @needleLen@ exists in @haystack@ at or after
--   @haystackOff@ and entirely within @haystackLen@, @res@ is the first index
--   at which the specified chunk of @needle@ can be found in @haystack@.
-- * Otherwise, @res = -1@.
--
-- = Notes
--
-- This calls @[memmem](https://man7.org/linux/man-pages/man3/memmem.3.html)
-- underneath. While this is not mandated by any C standard, implementations
-- exist for Glibc, musl, the FreeBSD and OpenBSD libcs, and the macOS libc.
-- Thus, we consider this \'sufficiently portable\' to include.
--
-- @since 1.0.0
foreign import ccall unsafe "find_first_block"
  findFirstIn# ::
    -- | The memory area to search /in/ (the \'haystack\')
    ByteArray# ->
    -- | Offset from the start of the memory area to search in
    Int# ->
    -- | How many bytes to search in
    Int# ->
    -- | The memory area to search /for/ (the \'needle\')
    ByteArray# ->
    -- | Offset from the start of the memory area to search for
    Int# ->
    -- | How many bytes to search for
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
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off <= 'sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @findFirstByteIn# ba off len w8@.
--
-- * If @w8@ exists in the index range @off .. off + len - 1@, then @res@ will
--   be in the range @off .. off + len - 1@
-- * Otherwise, @res = -1@
--
-- = Notes
--
-- This calls @[memrchr](https://linux.die.net/man/3/memrchr)@ underneath. While
-- this is not mandated by any C standard, implementations exist for Glibc,
-- musl, and both the FreeBSD and OpenBSD libc. On MacOS, we provide an
-- implementation in portable C.
--
-- @since 1.0.0
foreign import ccall unsafe "find_last_byte"
  findLastByteIn# ::
    -- | The memory area to search
    ByteArray# ->
    -- | Offset from the start
    Int# ->
    -- | How many bytes to check
    Int# ->
    -- | What byte to match (only low 8 bits)
    Int# ->
    -- | Location as index, or -1 if not found
    CPtrdiff

-- | Counts the occurrences of a specific byte in a byte array from an offset.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length argument, @ba@ the
-- 'ByteArray#' argument, and @w8@ the byte to count.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'sizeofByteArray#' ba@
-- * @0 '<=' w8@ and @w8 '<' 255@
--
-- = Outcomes
--
-- Let @res@ be the result of a call @countBytesEqIn# ba off len w8@.
--
-- * @0 '<=' res@ and @res '<' len@.
--
-- @since 1.0.0
foreign import ccall unsafe "count_bytes_eq"
  countBytesEqIn# ::
    -- | The memory area to count
    ByteArray# ->
    -- | Offset from the start
    Int# ->
    -- | How many bytes to check
    Int# ->
    -- | What byte to count (only low 8 bits)
    Int# ->
    -- | How many times the byte occurs
    CInt

-- | Counts the number of set bits (1-bits) in a byte array from an offset.
--
-- = Prerequisites
--
-- Let @off@ be the offset argument, @len@ be the length argument, and @ba@ be
-- the 'ByteArray#' argument.
--
-- * @0 '<=' off@ and @off '<' 'sizeofByteArray#' ba@
-- * @0 '<=' len@ and @len + off '<=' 'sizeofByteArray#' ba@
--
-- = Outcomes
--
-- Let @resSet@ be the result of a call @countBitsSetIn# ba off len@, and
-- @resClear@ be the result of a call @countBitsClearIn# ba off len@.
--
-- * @0 '<=' resSet@ and @resSet '<' (len '*' 8)@
-- * @resSet '+' resClear '==' len '*' 8@
--
-- @since 1.0.0
foreign import ccall unsafe "count_bits_set"
  countBitsSetIn# ::
    -- | The memory area to count
    ByteArray# ->
    -- | Offset from the start
    Int# ->
    -- | How many bytes to count in
    Int# ->
    -- | How many set bits there are
    CInt
