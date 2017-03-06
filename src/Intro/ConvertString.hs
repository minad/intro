-----------------------------------------------------------------------------
-- |
-- Module      :  Intro.ConvertString
-- Copyright   :  (c) Daniel Mendler 2017
-- License     :  MIT
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- String conversion and decoding
--
-----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DefaultSignatures #-}

module Intro.ConvertString (
    ConvertString(..)
  , EncodeString(..)
  , Lenient(..)
) where

import Data.ByteString (ByteString)
import Data.Either.Extra (eitherToMaybe)
import Data.Eq (Eq)
import Data.Foldable (Foldable)
import Data.Function (id, (.))
import Data.Functor (Functor(fmap))
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.String (String)
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Traversable (Traversable)
import Data.Word (Word8)
import Text.Show (Show)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- | Convert between two string types
--
-- @
-- ('convertString' :: b -> a)           . ('convertString' :: a -> b) ≡ ('id'      :: a -> a)
-- ('convertString' :: b -> 'Maybe' a)   . ('convertString' :: a -> b) ≡ ('Just'    :: a -> 'Maybe' a)
-- ('convertString' :: b -> 'Lenient' a) . ('convertString' :: a -> b) ≡ ('Lenient' :: a -> 'Lenient' a)
-- @
class ConvertString a b where
  -- | Convert a string type into another
  convertString :: a -> b

-- | Encode string as a byte sequence
--
-- @
-- 'decodeString'        . 'encodeString' ≡ 'Just'
-- 'decodeStringLenient' . 'encodeString' ≡ 'id'
-- @
class (ConvertString a b, ConvertString b (Maybe a), ConvertString b (Lenient a)) => EncodeString a b where
  -- | Encode string as byte sequence
  encodeString :: a -> b
  encodeString = convertString
  {-# INLINE encodeString #-}

  -- | Lenient decoding of byte sequence
  --
  -- Lenient means that invalid characters are replaced
  -- by the Unicode replacement character '\FFFD'.
  decodeStringLenient :: b -> a
  decodeStringLenient = getLenient . convertString
  {-# INLINE decodeStringLenient #-}

  -- | Decode byte sequence
  --
  -- If the decoding fails, return Nothing.
  decodeString :: b -> Maybe a
  decodeString = convertString
  {-# INLINE decodeString #-}

-- | Newtype wrapper for String type which was decoded leniently.
newtype Lenient a = Lenient { getLenient :: a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ConvertString BL.ByteString (Lenient String)    where {-# INLINE convertString #-}; convertString = Lenient . TL.unpack . TLE.decodeUtf8With lenientDecode
instance ConvertString BL.ByteString (Lenient TL.Text)   where {-# INLINE convertString #-}; convertString = Lenient . TLE.decodeUtf8With lenientDecode
instance ConvertString BL.ByteString (Lenient Text)      where {-# INLINE convertString #-}; convertString = Lenient . TE.decodeUtf8With lenientDecode . BL.toStrict
instance ConvertString BL.ByteString (Maybe   String)    where {-# INLINE convertString #-}; convertString = fmap TL.unpack . eitherToMaybe . TLE.decodeUtf8'
instance ConvertString BL.ByteString (Maybe   TL.Text)   where {-# INLINE convertString #-}; convertString = eitherToMaybe . TLE.decodeUtf8'
instance ConvertString BL.ByteString (Maybe   Text)      where {-# INLINE convertString #-}; convertString = eitherToMaybe . TE.decodeUtf8' . BL.toStrict
instance ConvertString BL.ByteString BL.ByteString       where {-# INLINE convertString #-}; convertString = id
instance ConvertString BL.ByteString ByteString          where {-# INLINE convertString #-}; convertString = BL.toStrict
instance ConvertString BL.ByteString [Word8]             where {-# INLINE convertString #-}; convertString = BL.unpack
instance ConvertString ByteString    (Lenient String)    where {-# INLINE convertString #-}; convertString = Lenient . T.unpack . TE.decodeUtf8With lenientDecode
instance ConvertString ByteString    (Lenient TL.Text)   where {-# INLINE convertString #-}; convertString = Lenient . TLE.decodeUtf8With lenientDecode . BL.fromStrict
instance ConvertString ByteString    (Lenient Text)      where {-# INLINE convertString #-}; convertString = Lenient . TE.decodeUtf8With lenientDecode
instance ConvertString ByteString    (Maybe   String)    where {-# INLINE convertString #-}; convertString = fmap T.unpack . eitherToMaybe . TE.decodeUtf8'
instance ConvertString ByteString    (Maybe   TL.Text)   where {-# INLINE convertString #-}; convertString = eitherToMaybe . TLE.decodeUtf8' . BL.fromStrict
instance ConvertString ByteString    (Maybe   Text)      where {-# INLINE convertString #-}; convertString = eitherToMaybe . TE.decodeUtf8'
instance ConvertString ByteString    BL.ByteString       where {-# INLINE convertString #-}; convertString = BL.fromStrict
instance ConvertString ByteString    ByteString          where {-# INLINE convertString #-}; convertString = id
instance ConvertString ByteString    [Word8]             where {-# INLINE convertString #-}; convertString = B.unpack
instance ConvertString String        BL.ByteString       where {-# INLINE convertString #-}; convertString = TLE.encodeUtf8 . TL.pack
instance ConvertString String        ByteString          where {-# INLINE convertString #-}; convertString = TE.encodeUtf8 . T.pack
instance ConvertString String        String              where {-# INLINE convertString #-}; convertString = id
instance ConvertString String        TL.Text             where {-# INLINE convertString #-}; convertString = TL.pack
instance ConvertString String        Text                where {-# INLINE convertString #-}; convertString = T.pack
instance ConvertString String        [Word8]             where {-# INLINE convertString #-}; convertString = BL.unpack . TLE.encodeUtf8 . TL.pack
instance ConvertString TL.Text       BL.ByteString       where {-# INLINE convertString #-}; convertString = TLE.encodeUtf8
instance ConvertString TL.Text       ByteString          where {-# INLINE convertString #-}; convertString = BL.toStrict . TLE.encodeUtf8
instance ConvertString TL.Text       String              where {-# INLINE convertString #-}; convertString = TL.unpack
instance ConvertString TL.Text       TL.Text             where {-# INLINE convertString #-}; convertString = id
instance ConvertString TL.Text       Text                where {-# INLINE convertString #-}; convertString = TL.toStrict
instance ConvertString TL.Text       [Word8]             where {-# INLINE convertString #-}; convertString = BL.unpack . TLE.encodeUtf8
instance ConvertString Text          BL.ByteString       where {-# INLINE convertString #-}; convertString = BL.fromStrict . TE.encodeUtf8
instance ConvertString Text          ByteString          where {-# INLINE convertString #-}; convertString = TE.encodeUtf8
instance ConvertString Text          String              where {-# INLINE convertString #-}; convertString = T.unpack
instance ConvertString Text          TL.Text             where {-# INLINE convertString #-}; convertString = TL.fromStrict
instance ConvertString Text          Text                where {-# INLINE convertString #-}; convertString = id
instance ConvertString Text          [Word8]             where {-# INLINE convertString #-}; convertString = BL.unpack . BL.fromStrict . TE.encodeUtf8
instance ConvertString [Word8]       (Lenient String)    where {-# INLINE convertString #-}; convertString = Lenient . TL.unpack . TLE.decodeUtf8With lenientDecode . BL.pack
instance ConvertString [Word8]       (Lenient TL.Text)   where {-# INLINE convertString #-}; convertString = Lenient . TLE.decodeUtf8With lenientDecode . BL.pack
instance ConvertString [Word8]       (Lenient Text)      where {-# INLINE convertString #-}; convertString = Lenient . TE.decodeUtf8With lenientDecode . B.pack
instance ConvertString [Word8]       (Maybe String)      where {-# INLINE convertString #-}; convertString = fmap TL.unpack . eitherToMaybe . TLE.decodeUtf8' . BL.pack
instance ConvertString [Word8]       (Maybe TL.Text)     where {-# INLINE convertString #-}; convertString = eitherToMaybe . TLE.decodeUtf8' . BL.pack
instance ConvertString [Word8]       (Maybe Text)        where {-# INLINE convertString #-}; convertString = eitherToMaybe . TE.decodeUtf8' . B.pack
instance ConvertString [Word8]       BL.ByteString       where {-# INLINE convertString #-}; convertString = BL.pack
instance ConvertString [Word8]       ByteString          where {-# INLINE convertString #-}; convertString = B.pack
instance ConvertString [Word8]       [Word8]             where {-# INLINE convertString #-}; convertString = id

instance EncodeString  String        BL.ByteString
instance EncodeString  String        ByteString
instance EncodeString  String        [Word8]
instance EncodeString  TL.Text       BL.ByteString
instance EncodeString  TL.Text       ByteString
instance EncodeString  TL.Text       [Word8]
instance EncodeString  Text          BL.ByteString
instance EncodeString  Text          ByteString
instance EncodeString  Text          [Word8]
