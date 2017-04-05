{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import BaseCompat ()
import Data.ByteString.Short (ShortByteString)
import Intro
import LensCompat ()
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

instance Arbitrary ByteString      where arbitrary = B.pack <$> arbitrary
instance Arbitrary LB.ByteString   where arbitrary = LB.pack <$> arbitrary
instance Arbitrary ShortByteString where arbitrary = SB.pack <$> arbitrary
instance Arbitrary Text            where arbitrary = T.pack <$> arbitrary
instance Arbitrary LT.Text         where arbitrary = LT.pack <$> arbitrary

main :: IO ()
main = do
  encode (Proxy :: Proxy LText)           (Proxy :: Proxy ByteString)
  encode (Proxy :: Proxy LText)           (Proxy :: Proxy LByteString)
  encode (Proxy :: Proxy LText)           (Proxy :: Proxy ShortByteString)
  encode (Proxy :: Proxy LText)           (Proxy :: Proxy [Word8])
  encode (Proxy :: Proxy String)          (Proxy :: Proxy ByteString)
  encode (Proxy :: Proxy String)          (Proxy :: Proxy LByteString)
  encode (Proxy :: Proxy String)          (Proxy :: Proxy ShortByteString)
  encode (Proxy :: Proxy String)          (Proxy :: Proxy [Word8])
  encode (Proxy :: Proxy Text)            (Proxy :: Proxy ByteString)
  encode (Proxy :: Proxy Text)            (Proxy :: Proxy LByteString)
  encode (Proxy :: Proxy Text)            (Proxy :: Proxy ShortByteString)
  encode (Proxy :: Proxy Text)            (Proxy :: Proxy [Word8])
  iso    (Proxy :: Proxy ByteString)      (Proxy :: Proxy ByteString)
  iso    (Proxy :: Proxy ByteString)      (Proxy :: Proxy LByteString)
  iso    (Proxy :: Proxy ByteString)      (Proxy :: Proxy ShortByteString)
  iso    (Proxy :: Proxy ByteString)      (Proxy :: Proxy [Word8])
  iso    (Proxy :: Proxy ShortByteString) (Proxy :: Proxy ByteString)
  iso    (Proxy :: Proxy ShortByteString) (Proxy :: Proxy LByteString)
  iso    (Proxy :: Proxy ShortByteString) (Proxy :: Proxy ShortByteString)
  iso    (Proxy :: Proxy ShortByteString) (Proxy :: Proxy [Word8])
  iso    (Proxy :: Proxy LByteString)     (Proxy :: Proxy ByteString)
  iso    (Proxy :: Proxy LByteString)     (Proxy :: Proxy LByteString)
  iso    (Proxy :: Proxy LByteString)     (Proxy :: Proxy ShortByteString)
  iso    (Proxy :: Proxy LByteString)     (Proxy :: Proxy [Word8])
  iso    (Proxy :: Proxy LText)           (Proxy :: Proxy LText)
  iso    (Proxy :: Proxy LText)           (Proxy :: Proxy String)
  iso    (Proxy :: Proxy LText)           (Proxy :: Proxy Text)
  iso    (Proxy :: Proxy String)          (Proxy :: Proxy LText)
  iso    (Proxy :: Proxy String)          (Proxy :: Proxy String)
  iso    (Proxy :: Proxy String)          (Proxy :: Proxy Text)
  iso    (Proxy :: Proxy Text)            (Proxy :: Proxy LText)
  iso    (Proxy :: Proxy Text)            (Proxy :: Proxy String)
  iso    (Proxy :: Proxy Text)            (Proxy :: Proxy Text)
  iso    (Proxy :: Proxy [Word8])         (Proxy :: Proxy ByteString)
  iso    (Proxy :: Proxy [Word8])         (Proxy :: Proxy LByteString)
  iso    (Proxy :: Proxy [Word8])         (Proxy :: Proxy ShortByteString)
  iso    (Proxy :: Proxy [Word8])         (Proxy :: Proxy [Word8])

iso :: forall a b proxy. (Eq a, Eq b, Show a, Show b, Arbitrary a, Arbitrary b, ConvertString a b, ConvertString b a) => proxy a -> proxy b -> IO ()
iso _ _ = do
  quickCheck $ \(a :: a) -> convertString (convertString a :: b) == a
  quickCheck $ \(b :: b) -> convertString (convertString b :: a) == b

encode :: forall a b proxy. (Eq a, Show a, Arbitrary a, EncodeString a b) => proxy a -> proxy b -> IO ()
encode _ _ = do
  quickCheck $ \(a :: a) -> decodeString (encodeString a :: b) == Just a
  quickCheck $ \(a :: a) -> decodeStringLenient (encodeString a :: b) == a
