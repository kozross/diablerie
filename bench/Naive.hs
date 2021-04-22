{-# LANGUAGE TypeApplications #-}

module Naive
  ( findFirstByte,
    findFirstByteIn,
    findLastByte,
    findLastByteIn,
    countBytesEq,
    countBytesEqIn,
    countBitsSet,
    countBitsSetIn,
  )
where

import Data.Bits (popCount)
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

countBytesEq :: ByteArray -> Word8 -> Int
countBytesEq ba w8 = sum . fmap go $ [0 .. sizeofByteArray ba - 1]
  where
    go :: Int -> Int
    go i
      | indexByteArray ba i == w8 = 1
      | otherwise = 0

countBytesEqIn :: ByteArray -> Int -> Int -> Word8 -> Int
countBytesEqIn ba off len w8 = sum . fmap go $ [off .. off + len - 1]
  where
    go :: Int -> Int
    go i
      | indexByteArray ba i == w8 = 1
      | otherwise = 0

countBitsSet :: ByteArray -> Int
countBitsSet ba = sum . fmap go $ [0 .. sizeofByteArray ba - 1]
  where
    go :: Int -> Int
    go = popCount . indexByteArray @Word8 ba

countBitsSetIn :: ByteArray -> Int -> Int -> Int
countBitsSetIn ba off len = sum . fmap go $ [off .. off + len - 1]
  where
    go :: Int -> Int
    go = popCount . indexByteArray @Word8 ba
