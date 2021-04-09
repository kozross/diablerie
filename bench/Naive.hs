{-# LANGUAGE MagicHash #-}

module Naive
  ( findFirstByte,
    findFirstByteIn,
    findFirstByte#,
    findFirstByteIn#,
  )
where

import Data.Foldable (foldl')
import Data.Primitive.ByteArray
  ( ByteArray,
    indexByteArray,
    sizeofByteArray,
  )
import Data.Word (Word8)
import GHC.Exts
  ( ByteArray#,
    Int#,
    indexInt8Array#,
    isTrue#,
    sizeofByteArray#,
    (+#),
    (-#),
    (==#),
  )

findFirstByte :: ByteArray -> Word8 -> Maybe Int
findFirstByte ba w8 = foldl' go Nothing [0 .. sizeofByteArray ba - 1]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i == w8
          then Just i
          else Nothing
      Just _ -> acc

findFirstByteIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findFirstByteIn ba off len w8 = foldl' go Nothing [off .. off + len - 1]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i == w8
          then Just i
          else Nothing
      Just _ -> acc

findFirstByte# :: ByteArray# -> Int# -> Int#
findFirstByte# ba# w8# = go (-1#) 0#
  where
    go :: Int# -> Int# -> Int#
    go acc# i#
      | isTrue# (i# ==# lim#) = acc#
      | isTrue# (acc# ==# (-1#)) =
        if isTrue# (indexInt8Array# ba# i# ==# w8#)
          then i#
          else go acc# (i# +# 1#)
      | otherwise = go acc# (i# +# 1#)
    lim# :: Int#
    lim# = sizeofByteArray# ba# -# 1#

findFirstByteIn# :: ByteArray# -> Int# -> Int# -> Int# -> Int#
findFirstByteIn# ba# off# len# w8# = go (-1#) off#
  where
    go :: Int# -> Int# -> Int#
    go acc# i#
      | isTrue# (i# ==# lim#) = acc#
      | isTrue# (acc# ==# (-1#)) =
        if isTrue# (indexInt8Array# ba# i# ==# w8#)
          then i#
          else go acc# (i# +# 1#)
      | otherwise = go acc# (i# +# 1#)
    lim# :: Int#
    lim# = off# +# len# -# 1#
