module Naive
  ( findFirstByte,
    findFirstByteIn,
    findLastByte,
    findLastByteIn,
  )
where

import Data.Foldable (foldl')
import Data.Primitive.ByteArray
  ( ByteArray,
    indexByteArray,
    sizeofByteArray,
  )
import Data.Word (Word8)

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

findLastByte :: ByteArray -> Word8 -> Maybe Int
findLastByte ba w8 = foldl' go Nothing [sizeofByteArray ba - 1, sizeofByteArray ba - 2 .. 0]
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

findLastByteIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findLastByteIn ba off len w8 = foldl' go Nothing [off + len - 1, off + len - 2 .. off]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i == w8
          then Just i
          else Nothing
      Just _ -> acc
