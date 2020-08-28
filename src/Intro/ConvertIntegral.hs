-----------------------------------------------------------------------------
-- |
-- Module      :  Intro.ConvertInt
-- Copyright   :  (c) Daniel Mendler 2020
-- License     :  MIT
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Integer conversion
--
-----------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Intro.ConvertIntegral (
  ToIntegral(..)
  , fromIntegralUnsafe
  , fromIntegerUnsafe
) where

import Data.Int
import Data.Word

class ToIntegral a b where
  toIntegral :: a -> b
  default toIntegral :: (Integral a, Integral b) => a -> b
  toIntegral = fromIntegral
  {-# INLINE toIntegral #-}

instance ToIntegral Word8 Int16
instance ToIntegral Word8 Int32
instance ToIntegral Word8 Int64
instance ToIntegral Word8 Int
instance ToIntegral Word16 Int32
instance ToIntegral Word16 Int64
instance ToIntegral Word16 Int
instance ToIntegral Word32 Int64

instance ToIntegral Word8 Word16
instance ToIntegral Word8 Word32
instance ToIntegral Word8 Word64
instance ToIntegral Word8 Word
instance ToIntegral Word16 Word32
instance ToIntegral Word16 Word64
instance ToIntegral Word16 Word
instance ToIntegral Word32 Word64

instance ToIntegral Int8 Integer
instance ToIntegral Int16 Integer
instance ToIntegral Int32 Integer
instance ToIntegral Int64 Integer
instance ToIntegral Int Integer

instance ToIntegral Word8 Integer
instance ToIntegral Word16 Integer
instance ToIntegral Word32 Integer
instance ToIntegral Word64 Integer
instance ToIntegral Word Integer

instance ToIntegral Int8 Int16
instance ToIntegral Int8 Int32
instance ToIntegral Int8 Int64
instance ToIntegral Int8 Int
instance ToIntegral Int16 Int32
instance ToIntegral Int16 Int64
instance ToIntegral Int16 Int
instance ToIntegral Int32 Int64

fromIntegralUnsafe :: (Integral a, Num b) => a -> b
fromIntegralUnsafe = fromIntegral
{-# INLINE fromIntegralUnsafe #-}

fromIntegerUnsafe :: Num a => Integer -> a
fromIntegerUnsafe = fromInteger
{-# INLINE fromIntegerUnsafe #-}
