{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Intro
-- Copyright   :  (c) Daniel Mendler 2016-2017
-- License     :  MIT
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Intro is a modern Prelude which provides safe alternatives
-- for most of the partial functions and follows other
-- best practices, e.g., Text is preferred over String.
-- For String overloading the extension 'OverloadedStrings' should be used.
-- Container types and Monad transformers are provided.
--
-- Most important - this Prelude tries to keep things simple.
-- This means it just reexports from base and commonly used libraries
-- and adds only very few additional functions.
--
-- List of design decisions:
--
-- * Keep everything at one place (There are only three modules and Intro.Trustworthy is only there for Safe Haskell)
-- * Conservative extension over the base Prelude
-- * Rely only on very common external libraries
-- * Avoid writing custom functions
-- * Export everything explicitly to provide a stable interface and for good documentation
-- * Export only total functions or provide safe alternatives (Very few exceptions like div etc.)
-- * Prefer Text over String, provide 'ConvertString' and 'EncodeString'
-- * Provide Monad transformers
-- * Provide container types
-- * Prefer generic functions
-- * Debugging functions, like 'trace' and 'undefined' are available but produce compile time warnings
-- * Don't provide error, only panic instead
-- * Compatibility with unqualified import of Control.Lens
--
-- Some 'Prelude' functions are missing from 'Intro'. More general variants are available for the following functions:
--
-- * '>>' = 'Control.Applicative.*>'
-- * '++' = '<>'
-- * 'concat' = 'Data.Monoid.mconcat'
-- * 'fmap' is replaced by generalized 'map'
-- * 'mapM' = 'Control.Applicative.traverse'
-- * 'mapM_' = 'Data.Foldable.traverse_'
-- * 'return' = 'Control.Applicative.pure'
-- * 'sequence' = 'Control.Applicative.sequenceA'
-- * 'sequence_' = 'Control.Applicative.sequenceA_'
--
-- Unsafe functions are not provided. Use the '*May' or '*Def' alternatives instead.
--
-- * 'cycle', 'head', 'tail', 'init', 'last'
-- * 'foldl1', 'foldr1', 'maximum', 'minimum'
-- * 'toEnum', 'pred', 'succ'
-- * 'read' is replaced by 'readMaybe'
--
-- These functions are not provided for various reasons:
--
-- * '!!' is unsafe and /O(n)/. Use a 'Data.Map.Map' instead.
-- * 'lines', 'unlines', 'words' and 'unwords' are not provided. Use qualified 'Data.Text' import instead.
-- * Instead of 'foldl', it is recommended to use 'Data.Foldable.foldl''.
-- * 'lex' is not commonly used. Use a parser combinator library instead.
-- * 'gcd' and 'lcm' are not commonly used.
-- * 'error' and 'errorWithoutStackTrace' are not provided. Use 'panic' instead.
-- * 'ioError' and 'userError' are not provided. Import modules for exception handling separately if needed.
-- * Some 'Text.Read.Read' and 'Show' class functions are not provided. Don't write these instances yourself.
--
-- Additional types and functions:
--
-- * 'LText' alias for lazy 'Text'
-- * 'LByteString' alias for lazy 'ByteString'
-- * 'fromFoldable' to convert from 'Data.Foldable.Foldable' to an 'IsList' type
-- * 'convertList' to convert between two 'IsList' types. This function can be used instead of the 'toList' function
--   originally provided by the 'IsList' class.
-- * 'showT' and 'showS' are monomorphic 'show' functions.
-- * '<>^' lifted composition
-- * '.:' function composition
-- * '?:' as an alias for 'fromMaybe'
-- * 'skip' as an alias for @pure ()@
-- * 'panic' as a replacement for 'error'
-----------------------------------------------------------------------------

module Intro (
  -- * Basic functions
  -- Data.Function.id
  -- , (Data.Function..)
  Data.Function.const
  , Data.Function.flip
  , (Data.Function.$)
  , (Prelude.$!)
  , (Data.Function.&)
  , Data.Function.fix
  , Data.Function.on
  , (.:)
  , Prelude.until
  , Prelude.asTypeOf
  , Prelude.seq

  -- * Basic algebraic types

  -- ** Void
  , Data.Void.Void

  -- ** Bool
  , Data.Bool.Bool(False, True)
  , (Data.Bool.&&)
  , (Data.Bool.||)
  , Data.Bool.bool
  , Data.Bool.not
  , Data.Bool.otherwise

  -- ** Maybe
  , Data.Maybe.Maybe(Nothing, Just)
  , Data.Maybe.catMaybes
  , Data.Maybe.fromMaybe
  , (?:)
  , Data.Maybe.isJust
  , Data.Maybe.isNothing
  --, Data.Maybe.listToMaybe -- use headMay
  -- , Data.Maybe.maybeToList -- use toList
  , Data.Maybe.mapMaybe
  , Data.Maybe.maybe

  -- ** List
  , Intro.Trustworthy.IsList(
      Item
      , fromList
      -- , toList -- provided by Foldable
      )
  , convertList
  , fromFoldable
  , Data.List.break
  , Data.List.Extra.breakOn
  , Data.List.Extra.breakOnEnd
  , Data.List.drop
  , Data.List.Extra.dropEnd
  , Data.List.dropWhile
  , Data.List.dropWhileEnd
  , Data.List.filter
  , Data.List.group
  , Data.List.groupBy
  , Data.List.Extra.groupOn
  , Data.List.Extra.groupSort
  , Data.List.Extra.groupSortBy
  , Data.List.Extra.groupSortOn
  , Data.List.inits
  , Data.List.intercalate
  , Data.List.intersperse
  , Data.List.isPrefixOf
  , Data.List.isSuffixOf
  , Data.List.iterate
#if MIN_VERSION_base(4,11,0)
  , Data.List.iterate'
#endif
  , Data.List.lookup
  , Data.List.Extra.nubOrd
  , Data.List.Extra.nubOrdBy
  , Data.List.Extra.nubOrdOn
  , Data.List.permutations
  , Data.List.repeat
  , Data.List.replicate
  , Data.List.reverse
  , Data.List.scanl
  , Data.List.scanr
  , Data.List.sort
  , Data.List.sortBy
  , Data.List.sortOn
  , Data.List.span
  , Data.List.Extra.spanEnd
  , Data.List.splitAt
  , Data.List.Extra.split
  , Data.List.Extra.splitOn
  , Data.List.subsequences
  , Data.List.tails
  , Data.List.take
  , Data.List.Extra.takeEnd
  , Data.List.takeWhile
  , Data.List.Extra.takeWhileEnd
  , Data.List.transpose
  , Data.List.unfoldr
  , Data.List.unzip
  , Data.List.unzip3
  , Data.List.zip
  , Data.List.zip3
  , Data.List.zipWith
  , Data.List.zipWith3
  , Safe.headDef
  , Safe.headMay -- prefer pattern match
  , Safe.initDef
  , Safe.initMay
  , Safe.lastDef
  , Safe.lastMay
  , Safe.tailDef
  , Safe.tailMay -- prefer pattern match
  , Safe.cycleMay
  , Safe.cycleDef

  -- ** NonEmpty
  , Data.List.NonEmpty.NonEmpty((:|))
  -- (<|), -- in lens
  , Data.List.NonEmpty.scanl1
  , Data.List.NonEmpty.scanr1

  -- ** Tuple
  , Data.Tuple.fst
  , Data.Tuple.snd
  , Data.Tuple.curry
  , Data.Tuple.uncurry
  , Data.Tuple.swap

  -- ** Either
  , Data.Either.Either(Left, Right)
  , Data.Either.either
  , Data.Either.Extra.fromLeft
  , Data.Either.Extra.fromRight
  , Data.Either.isLeft
  , Data.Either.isRight
  , Data.Either.lefts
  , Data.Either.rights
  , Data.Either.partitionEithers
  , Data.Either.Extra.eitherToMaybe
  , Data.Either.Extra.maybeToEither

  -- * Text types

  -- ** Char and String
  , Data.Char.Char
  , Data.String.String

  -- ** Text
  , Data.Text.Text
  , LText
--  Data.Text.lines, -- Use qualified import instead
--  Data.Text.words,
--  Data.Text.unlines,
--  Data.Text.unwords,

  -- ** ByteString
  , Data.ByteString.ByteString
  , LByteString
  , Data.ByteString.Short.ShortByteString

  -- ** String conversion
  , Data.String.IsString(fromString)
  , Intro.ConvertString.ConvertString(convertString)
  , Intro.ConvertString.EncodeString(encodeString, decodeString, decodeStringLenient)
  , Lenient(..)

  -- * Container types

  -- ** Map and Set (Ordered)
  , Data.Map.Map
  , Data.Set.Set
  , Data.IntMap.IntMap
  , Data.IntSet.IntSet

  -- ** HashedMap and HashSet
  , Data.HashMap.Strict.HashMap
  , Data.HashSet.HashSet
  , Data.Hashable.Hashable(hash, hashWithSalt)
  , Intro.Trustworthy.Hashable1
  , Intro.Trustworthy.Hashable2

  -- ** DList and Seq
  , Intro.Trustworthy.DList
  , Data.Sequence.Seq

  -- * Numeric types

  -- ** Big integers
  , Prelude.Integer
  , Numeric.Natural.Natural

  -- ** Small integers
  , Data.Int.Int
  , Data.Int.Int8
  , Data.Int.Int16
  , Data.Int.Int32
  , Data.Int.Int64
  , Data.Word.Word
  , Data.Word.Word8
  , Data.Word.Word16
  , Data.Word.Word32
  , Data.Word.Word64

  -- ** Floating point
  , Prelude.Float
  , Prelude.Double

  -- ** Rational
  , Data.Ratio.Ratio
  , Data.Ratio.Rational
  , (Data.Ratio.%)
  , Data.Ratio.numerator
  , Data.Ratio.denominator
  , Data.Ratio.approxRational

  -- * Numeric type classes

  -- ** Num
  , Prelude.Num((+), (-), (*), negate, abs, signum, fromInteger)
  , Prelude.subtract
  , (Prelude.^) -- partial functions!

  -- ** Real
  , Prelude.Real(toRational)
  , Prelude.realToFrac

  -- ** Integral
  , Prelude.Integral(quot, rem, div, mod, quotRem, divMod, toInteger) -- partial functions!
  , Prelude.fromIntegral
  , Prelude.even
  , Prelude.odd
  --, Prelude.gcd
  --, Prelude.lcm

  -- ** Fractional
  , Prelude.Fractional((/), recip, fromRational) -- partial functions
  , (Prelude.^^)

  -- ** Floating
  , Prelude.Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
                     asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh)
  -- ** RealFrac
  , Prelude.RealFrac(properFraction, truncate, round, ceiling, floor) -- partial functions

  -- ** RealFloat
  , Prelude.RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
                      encodeFloat, exponent, significand, scaleFloat, isNaN,
                      isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2)

  -- ** Bits
  , Data.Bits.Bits((.&.), (.|.), xor, complement, shift, rotate, zeroBits,
                   bit, setBit, clearBit, complementBit, testBit,
                   -- bitSize, bitSizeMaybe
                   isSigned,
                   -- unsafeShiftL
                   -- unsafeShiftR
                   -- shiftR, shiftL,
                   rotateL, rotateR, popCount)
  , Data.Bits.FiniteBits(finiteBitSize, countLeadingZeros, countTrailingZeros)

  -- * Read and Show

  -- ** Show
  , Text.Show.Show
#if MIN_VERSION_base(4,9,0)
  , Data.Functor.Classes.Show1
  , Data.Functor.Classes.Show2
#endif
  , show
  , showT
  , showS

  -- ** Read
  , Text.Read.Read
#if MIN_VERSION_base(4,9,0)
  , Data.Functor.Classes.Read1
  , Data.Functor.Classes.Read2
#endif
  , readMaybe

  -- * Equality and ordering

  -- ** Eq
  , Data.Eq.Eq((==), (/=))
#if MIN_VERSION_base(4,9,0)
  , Data.Functor.Classes.Eq1
  , Data.Functor.Classes.Eq2
#endif

  -- ** Ord
  , Data.Ord.Ord(compare, (<), (>), (<=), (>=), max, min)
#if MIN_VERSION_base(4,9,0)
  , Data.Functor.Classes.Ord1
  , Data.Functor.Classes.Ord2
#endif
  , Data.Ord.Ordering(LT,GT,EQ)
  , Data.Ord.Down(Down)
  , Data.Ord.comparing

  -- ** Enum
  , Prelude.Enum(-- toEnum, succ, pred, -- partial
       fromEnum, enumFrom, enumFromThen,
       enumFromTo, enumFromThenTo)
  , Safe.toEnumMay
  , Safe.toEnumDef
  , Safe.predMay
  , Safe.predDef
  , Safe.succMay
  , Safe.succDef

  -- ** Bounded
  , Prelude.Bounded(minBound, maxBound)

  -- * Algebraic type classes

  -- ** Category
  , Control.Category.Category(id, (.))
  , (Control.Category.<<<)
  , (Control.Category.>>>)

  -- ** Semigroup
  , Data.Semigroup.Semigroup((<>), sconcat, stimes)
  , Data.Semigroup.First(First, getFirst)
  , Data.Semigroup.Last(Last, getLast)
  , Data.Semigroup.Min(Min, getMin)
  , Data.Semigroup.Max(Max, getMax)
  , Data.Semigroup.Option(Option, getOption)

  -- ** Monoid
  , Data.Monoid.Monoid(mempty, mappend, mconcat)
  , Data.Monoid.Dual(Dual, getDual)
  , Data.Monoid.Endo(Endo, appEndo)
  , Data.Monoid.All(All, getAll)
  , Data.Monoid.Any(Any, getAny)
  -- Hide because of name clash with sum functors
  --, Data.Monoid.Sum(Sum, getSum)
  --, Data.Monoid.Product(Product, getProduct)
  -- Provide semigroup instances instead
  --, Data.Monoid.First(First, getFirst)
  --, Data.Monoid.Last(Last, getLast)
  , Data.Monoid.Alt(Alt, getAlt)

  -- ** Functor
  , Data.Functor.Functor(
      (<$)
      --, fmap -- hide fmap, use map instead
      )
  , (Data.Functor.$>)
  , (Data.Functor.<$>)
#if MIN_VERSION_base(4,11,0)
  , (Data.Functor.<&>)
#endif
  , map
  , Data.Functor.void
  , Control.Applicative.Const(Const, getConst) -- Data.Functor.Const
  , Data.Functor.Identity.Identity(Identity, runIdentity)

  -- ** Foldable
  , Data.Foldable.Foldable(elem, fold, foldMap,
           foldr, foldr',
           -- foldl, -- hide the bad one
           foldl',
           product, sum, toList)
  , Data.Foldable.null
  , Data.Foldable.length
  , Data.Foldable.foldrM
  , Data.Foldable.foldlM
  , Data.Foldable.traverse_
  , Data.Foldable.for_
  , Data.Foldable.asum
  , Data.Foldable.concatMap
  , Data.Foldable.all
  , Data.Foldable.any
  , Data.Foldable.or
  , Data.Foldable.and
  , Data.Foldable.find
  , Data.Foldable.notElem
  , Data.Foldable.sequenceA_
  , Safe.Foldable.foldl1May
  , Safe.Foldable.foldl1Def
  , Safe.Foldable.foldr1May
  , Safe.Foldable.foldr1Def
  , Safe.Foldable.maximumByMay
  , Safe.Foldable.maximumByDef
  , Safe.Foldable.minimumByMay
  , Safe.Foldable.minimumByDef
  , Safe.Foldable.maximumMay
  , Safe.Foldable.maximumDef
  , Safe.Foldable.minimumMay
  , Safe.Foldable.minimumDef

  -- ** Traversable
  , Data.Traversable.Traversable(traverse, sequenceA)
  , Data.Traversable.for
  , Data.Traversable.mapAccumL
  , Data.Traversable.mapAccumR

  -- ** Applicative
  , Control.Applicative.Applicative(pure, (<*>), (*>), (<*))
  , Control.Applicative.ZipList(ZipList, getZipList)
  , (Control.Applicative.<**>)
  , Control.Applicative.liftA2
  , Control.Applicative.liftA3
  , skip
  , (<>^)

  -- ** Alternative
  , Control.Applicative.Alternative((<|>), empty, many {-, some -})
  , Control.Applicative.optional
  , Data.List.NonEmpty.some1

  -- ** Monad
  , Control.Monad.Monad((>>=))
  , Control.Monad.Fix.MonadFix(mfix)
  , (Control.Monad.=<<)
  , (Control.Monad.<=<)
  , (Control.Monad.>=>)
  , Control.Monad.join
  , Control.Monad.guard
  , Control.Monad.when
  , Control.Monad.unless
  , Control.Monad.replicateM
  , Control.Monad.replicateM_
  , (Control.Monad.<$!>)
  , Control.Monad.Extra.whenM
  , Control.Monad.Extra.unlessM
  , Control.Monad.Extra.ifM
  , Control.Monad.Extra.allM
  , Control.Monad.Extra.anyM
  , Control.Monad.Extra.andM
  , Control.Monad.Extra.orM
  , Control.Monad.Extra.concatMapM
  , (Control.Monad.Extra.&&^)
  , (Control.Monad.Extra.||^)

  -- ** Bifunctor
  , Data.Bifunctor.Bifunctor(bimap, first, second)

  -- ** Bifoldable
  , Data.Bifoldable.Bifoldable(bifoldr
                              --, bifoldl -- not strict enough
                              , bifoldMap)
  , Data.Bifoldable.bifoldl'
  , Data.Bifoldable.bifoldr'
  , Data.Bifoldable.bitraverse_
  , Data.Bifoldable.bisequenceA_
  , Data.Bifoldable.bifor_

  -- ** Bitraversable
  , Data.Bitraversable.Bitraversable(bitraverse)
  , Data.Bitraversable.bifor
  , Data.Bitraversable.bisequenceA

  -- * Effects and monad transformers
  , Control.Monad.Trans.MonadTrans(lift)

  -- ** MonadPlus and MaybeT
  , Control.Monad.MonadPlus
  , Control.Monad.Trans.Maybe.MaybeT(MaybeT, runMaybeT)
  , Control.Monad.Trans.Maybe.mapMaybeT

  -- ** MonadError and ExceptT
  , Control.Monad.Except.MonadError(throwError, catchError)
  , Control.Monad.Except.Except
  , Control.Monad.Except.runExcept
  , Control.Monad.Except.mapExcept
  , Control.Monad.Except.withExcept
  , Control.Monad.Except.ExceptT(ExceptT)
  , Control.Monad.Except.runExceptT
  , Control.Monad.Except.mapExceptT
  , Control.Monad.Except.withExceptT

  -- ** MonadReader and ReaderT
  , Control.Monad.Reader.MonadReader(ask, local, reader)
  , Control.Monad.Reader.asks
  , Control.Monad.Reader.Reader
  , Control.Monad.Reader.runReader
  , Control.Monad.Reader.mapReader
  , Control.Monad.Reader.withReader
  , Control.Monad.Reader.ReaderT(ReaderT, runReaderT)
  , Control.Monad.Reader.mapReaderT
  , Control.Monad.Reader.withReaderT

  -- ** MonadWriter and WriterT
  , Control.Monad.Writer.CPS.MonadWriter(writer, tell, listen, pass)
  , Control.Monad.Writer.CPS.Writer
  , Control.Monad.Writer.CPS.runWriter
  , Control.Monad.Writer.CPS.execWriter
  , Control.Monad.Writer.CPS.mapWriter
  , Control.Monad.Writer.CPS.WriterT
  , Control.Monad.Writer.CPS.writerT
  , Control.Monad.Writer.CPS.runWriterT
  , Control.Monad.Writer.CPS.execWriterT
  , Control.Monad.Writer.CPS.mapWriterT

  -- ** MonadState and StateT
  , Control.Monad.State.Strict.MonadState(get, put, state)
  , Control.Monad.State.Strict.State
  , Control.Monad.State.Strict.gets
  , Control.Monad.State.Strict.modify
  , Control.Monad.State.Strict.modify'
  , Control.Monad.State.Strict.runState
  , Control.Monad.State.Strict.evalState
  , Control.Monad.State.Strict.execState
  , Control.Monad.State.Strict.mapState
  , Control.Monad.State.Strict.withState
  , Control.Monad.State.Strict.StateT(StateT, runStateT)
  , Control.Monad.State.Strict.evalStateT
  , Control.Monad.State.Strict.execStateT
  , Control.Monad.State.Strict.mapStateT
  , Control.Monad.State.Strict.withStateT

  -- ** MonadRWS and RWST
  , Control.Monad.RWS.CPS.MonadRWS
  , Control.Monad.RWS.CPS.RWS
  , Control.Monad.RWS.CPS.rws
  , Control.Monad.RWS.CPS.runRWS
  , Control.Monad.RWS.CPS.evalRWS
  , Control.Monad.RWS.CPS.execRWS
  , Control.Monad.RWS.CPS.mapRWS
  , Control.Monad.RWS.CPS.RWST
  , Control.Monad.RWS.CPS.rwsT
  , Control.Monad.RWS.CPS.runRWST
  , Control.Monad.RWS.CPS.evalRWST
  , Control.Monad.RWS.CPS.execRWST
  , Control.Monad.RWS.CPS.mapRWST

  -- * Generic type classes
  , GHC.Generics.Generic
  , GHC.Generics.Generic1
  , Data.Typeable.Typeable
  , Control.DeepSeq.NFData
#if MIN_VERSION_base(4,10,0)
  , Control.DeepSeq.NFData1
  , Control.DeepSeq.NFData2
#endif

  -- * Type level
#if MIN_VERSION_base(4,9,0)
  , Data.Kind.Type
#endif
  , Intro.Trustworthy.Constraint
  , Data.Proxy.Proxy(Proxy)
  --, Data.Tagged.Tagged(Tagged)
  --, Data.Tagged.unTagged

  -- * IO
  , System.IO.IO
  , Control.Monad.Trans.MonadIO(liftIO)

  -- ** Console
  , print
  , putChar
  , putStr
  , putStrLn
  --, interact

  -- ** File
  , System.IO.FilePath
  , readFile
  , writeFile
  , appendFile
  , readFileUtf8
  , writeFileUtf8
  , appendFileUtf8

  -- * Error handling and debugging
  , HasCallStack
#if MIN_VERSION_base(4,9,0)
  , Control.Monad.Fail.MonadFail
#endif
  , fail
  , panic
  , undefined
  , Intro.Trustworthy.trace
  , Intro.Trustworthy.traceIO
  , Intro.Trustworthy.traceId
  , Intro.Trustworthy.traceM
  , Intro.Trustworthy.traceShow
  , Intro.Trustworthy.traceShowId
  , Intro.Trustworthy.traceShowM
) where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Char (Char)
import Data.Function ((.), ($))
import Data.Functor (Functor(fmap))
import Data.Maybe (Maybe, fromMaybe)
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(fromString), String)
import Data.Text (Text)
import Intro.ConvertString
import Intro.Trustworthy
import System.IO (FilePath)
import Text.Show (Show)
import qualified Control.Applicative
import qualified Control.Category
import qualified Control.DeepSeq
import qualified Control.Monad
import qualified Control.Monad.Except
import qualified Control.Monad.Extra
import qualified Control.Monad.Fix
import qualified Control.Monad.RWS.CPS
import qualified Control.Monad.Reader
import qualified Control.Monad.State.Strict
import qualified Control.Monad.Trans
import qualified Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.CPS
import qualified Data.Bifoldable
import qualified Data.Bifunctor
import qualified Data.Bitraversable
import qualified Data.Bits
import qualified Data.Bool
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Either
import qualified Data.Either.Extra
import qualified Data.Eq
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Functor.Identity
import qualified Data.HashMap.Strict
import qualified Data.HashSet
import qualified Data.Hashable
import qualified Data.Int
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.List
import qualified Data.List.Extra
import qualified Data.List.NonEmpty
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Proxy
import qualified Data.Ratio
import qualified Data.Semigroup
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Traversable
import qualified Data.Tuple
import qualified Data.Typeable
import qualified Data.Void
import qualified Data.Word
import qualified GHC.Generics
import qualified Numeric.Natural
import qualified Prelude
import qualified Safe
import qualified Safe.Foldable
import qualified System.IO
import qualified Text.Read
import qualified Text.Show

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail
import qualified Data.Functor.Classes
import qualified Data.Kind
#endif

-- | Alias for lazy 'Data.Text.Lazy.Text'
type LText = Data.Text.Lazy.Text

-- | Alias for lazy 'Data.ByteString.Lazy.ByteString'
type LByteString = Data.ByteString.Lazy.ByteString

-- | Convert from 'Data.Foldable.Foldable' to an 'IsList' type.
fromFoldable :: (Data.Foldable.Foldable f, IsList a) => f (Item a) -> a
fromFoldable = fromList . Data.Foldable.toList
{-# INLINE fromFoldable #-}

-- | Convert between two different 'IsList' types.
-- This function can be used instead of the 'toList' function
-- originally provided by the 'IsList' class.
convertList :: (IsList a, IsList b, Item a ~ Item b) => a -> b
convertList = fromList . toList
{-# INLINE convertList #-}

-- | A synonym for 'fmap'.
--
--   @map = 'fmap'@
map :: Functor f => (a -> b) -> f a -> f b
map = fmap
{-# INLINE map #-}

-- | Convert a value to a readable string type supported by 'ConvertString' using the 'Show' instance.
show :: (Show a, ConvertString String s) => a -> s
show = convertString . showS
{-# INLINE show #-}

-- | Convert a value to a readable 'Text' using the 'Show' instance.
showT :: Show a => a -> Text
showT = show
{-# INLINE showT #-}

-- | Convert a value to a readable 'String' using the 'Show' instance.
showS :: Show a => a -> String
showS = Text.Show.show
{-# INLINE showS #-}

-- | Parse a string type using the 'Text.Read.Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: (Text.Read.Read b, ConvertString a String) => a -> Maybe b
readMaybe = Text.Read.readMaybe . convertString
{-# INLINE readMaybe #-}

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])
--
-- __Note__: This function is lifted to the 'MonadIO' class.
print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . System.IO.print
{-# INLINE print #-}

-- | Write a strict 'Text' to the standard output device.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
putStr :: MonadIO m => Text -> m ()
putStr = liftIO . Data.Text.IO.putStr
{-# INLINE putStr #-}

-- | The same as 'putStr', but adds a newline character.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . Data.Text.IO.putStrLn
{-# INLINE putStrLn #-}

-- | Write a character to the standard output device.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
putChar :: MonadIO m => Char -> m ()
putChar = liftIO . System.IO.putChar
{-# INLINE putChar #-}

-- | Read an entire file strictly into a 'ByteString'.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
readFile :: MonadIO m => FilePath -> m ByteString
readFile = liftIO . Data.ByteString.readFile
{-# INLINE readFile #-}

-- | Write a 'ByteString' to a file.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
writeFile :: MonadIO m => FilePath -> ByteString -> m ()
writeFile = liftIO .: Data.ByteString.writeFile
{-# INLINE writeFile #-}

-- | Append a 'ByteString' to a file.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
appendFile :: MonadIO m => FilePath -> ByteString -> m ()
appendFile = liftIO .: Data.ByteString.appendFile
{-# INLINE appendFile #-}

-- | Read an entire file strictly into a 'Text' using UTF-8 encoding.
-- The decoding is done using 'decodeStringLenient'. Invalid characters are replaced
-- by the Unicode replacement character '\FFFD'.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 = map decodeStringLenient . readFile
{-# INLINE readFileUtf8 #-}

-- | Write a 'Text' to a file using UTF-8 encoding.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 file = writeFile file . convertString
{-# INLINE writeFileUtf8 #-}

-- | Append a 'Text' to a file using UTF-8 encoding.
--
-- __Note__: This function is lifted to the 'MonadIO' class.
appendFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
appendFileUtf8 file = appendFile file . convertString
{-# INLINE appendFileUtf8 #-}

-- | Throw an undefined error. Use only for debugging.
undefined :: HasCallStack => a
undefined = Prelude.undefined
{-# WARNING undefined "'undefined' should be used only for debugging" #-}

-- | '<>' lifted to 'Control.Applicative.Applicative'
(<>^) :: (Control.Applicative.Applicative f, Semigroup a) => f a -> f a -> f a
(<>^) = Control.Applicative.liftA2 (<>)
infixr 6 <>^
{-# INLINE (<>^) #-}

-- | Compose functions with one argument with function with two arguments.
--
--   @f .: g = \\x y -> f (g x y)@.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:
{-# INLINE (.:) #-}

-- | An infix form of 'fromMaybe' with arguments flipped.
(?:) :: Maybe a -> a -> a
(?:) = Data.Function.flip fromMaybe
infix 1 ?:
{-# INLINE (?:) #-}

-- | @()@ lifted to an 'Control.Applicative.Applicative'.
--
--   @skip = 'Control.Applicative.pure' ()@
skip :: Control.Applicative.Applicative m => m ()
skip = Control.Applicative.pure ()
{-# INLINE skip #-}

-- | Throw an unhandled error to terminate the program in case
-- of a logic error at runtime. Use this function instead of 'Prelude.error'.
-- A stack trace will be provided.
--
-- In general, prefer total functions. You can use 'Maybe', 'Data.Either.Either',
-- 'Control.Monad.Except.ExceptT' or 'Control.Monad.Except.MonadError' for error handling.
panic :: HasCallStack => Text -> a
panic msg = Prelude.error $ convertString $
  "Panic: " <> msg <> "\n\n" <>
  "Please submit a bug report including the stacktrace\n" <>
  "and a description on how to reproduce the bug."

-- | Monad fail function from the 'Control.Monad.Fail.MonadFail' class.
--
-- When a value is bound in @do@-notation, the pattern on the left
-- hand side of @<-@ might not match. In this case, this class
-- provides a function to recover.
--
-- A 'Monad' without a 'MonadFail' instance may only be used in conjunction
-- with pattern that always match, such as newtypes, tuples, data types with
-- only a single data constructor, and irrefutable patterns (@~pat@).
--
-- Instances of 'MonadFail' should satisfy the following law: @fail s@ should
-- be a left zero for '>>=',
--
-- @
-- fail s >>= f  =  fail s
-- @
--
-- If your 'Monad' is also 'MonadPlus', a popular definition is
--
-- @
-- fail _ = mzero
-- @
#if MIN_VERSION_base(4,9,0)
fail :: Control.Monad.Fail.MonadFail m => Text -> m a
fail = Control.Monad.Fail.fail . convertString
#else
fail :: Control.Monad.Monad m => Text -> m a
fail = Control.Monad.fail . convertString
#endif
{-# INLINE fail #-}
