module Naive
  ( findFirstEq,
    findFirstEqIn,
    findFirstGt,
    findFirstGtIn,
    findLastEq,
    findLastEqIn,
    countEq,
    countEqIn,
  )
where

import Data.Foldable (foldl')
import Data.Primitive.ByteArray
  ( ByteArray,
    indexByteArray,
    sizeofByteArray,
  )
import Data.Word (Word8)

findFirstEq :: ByteArray -> Word8 -> Maybe Int
findFirstEq ba w8 = foldl' go Nothing [0 .. sizeofByteArray ba - 1]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i == w8
          then Just i
          else Nothing
      Just _ -> acc

findFirstGt :: ByteArray -> Word8 -> Maybe Int
findFirstGt ba w8 = foldl' go Nothing [0 .. sizeofByteArray ba - 1]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i > w8
          then Just i
          else Nothing
      Just _ -> acc

findLastEq :: ByteArray -> Word8 -> Maybe Int
findLastEq ba w8 =
  foldl' go Nothing [sizeofByteArray ba - 1, sizeofByteArray ba - 2 .. 0]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i == w8
          then Just i
          else Nothing
      Just _ -> acc

findFirstEqIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findFirstEqIn ba off len w8 = foldl' go Nothing [off .. off + len - 1]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i == w8
          then Just i
          else Nothing
      Just _ -> acc

findFirstGtIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findFirstGtIn ba off len w8 = foldl' go Nothing [off .. off + len - 1]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i > w8
          then Just i
          else Nothing
      Just _ -> acc

findLastEqIn :: ByteArray -> Int -> Int -> Word8 -> Maybe Int
findLastEqIn ba off len w8 =
  foldl' go Nothing [off + len - 1, off + len - 2 .. off]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i = case acc of
      Nothing ->
        if indexByteArray ba i == w8
          then Just i
          else Nothing
      Just _ -> acc

countEq :: ByteArray -> Word8 -> Int
countEq ba w8 = sum . fmap go $ [0 .. sizeofByteArray ba - 1]
  where
    go :: Int -> Int
    go i
      | indexByteArray ba i == w8 = 1
      | otherwise = 0

countEqIn :: ByteArray -> Int -> Int -> Word8 -> Int
countEqIn ba off len w8 = sum . fmap go $ [off .. off + len - 1]
  where
    go :: Int -> Int
    go i
      | indexByteArray ba i == w8 = 1
      | otherwise = 0
