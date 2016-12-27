{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
module Intro (
  module X,
  LText,
  LByteString,
  (.:),
  (<>^),
  skip,
  panic,
  map,
  print,
  getContents,
  getLine,
  getChar,
  putChar,
  putStr,
  putStrLn,
  show,
  showS,
  readMaybe,
  undefined,
  readFile,
  writeFile,
  appendFile,
  readFileUtf8,
  writeFileUtf8,
  appendFileUtf8,
) where

import Data.Either.Extra as X (
  fromLeft,
  fromRight,
  eitherToMaybe,
  maybeToEither,
  )

import Control.Monad.Extra as X (
  whenM,
  unlessM,
  ifM,
  allM,
  anyM,
  andM,
  orM,
  concatMapM,
  (&&^),
  (||^),
  )

import Data.List.Extra as X (
  nubOrd,
  nubOrdOn,
  nubOrdBy,
  groupOn,
  dropEnd,
  takeEnd,
  )

import Data.Text as X (
  Text,
--  lines,
--  words,
--  unlines,
--  unwords,
  )

import Data.ByteString as X (
  ByteString
  )

import Data.Map.Strict as X (
  Map,
  )

import Data.IntMap.Strict as X (
  IntMap,
  )

import Data.Set as X (
  Set,
  )

import Data.IntSet as X (
  IntSet,
  )

import Data.Sequence as X (
  Seq,
  )

import Data.HashMap.Strict as X (
  HashMap,
  )

import Data.HashSet as X (
  HashSet,
  )

import Data.Hashable as X (
  Hashable,
  )

import Data.Int as X (
  Int,
  Int8,
  Int16,
  Int32,
  Int64,
  )

import Data.Bits as X (
  Bits((.&.), (.|.), xor, complement, shift, rotate, zeroBits,
       bit, setBit, clearBit, complementBit, testBit,
       -- bitSize, bitSizeMaybe
       isSigned,
       -- unsafeShiftL
       -- unsafeShiftR
       -- shiftR, shiftL,
       rotateL, rotateR, popCount),
  FiniteBits(finiteBitSize, countLeadingZeros, countTrailingZeros)
  )

import Data.Tuple as X (
  fst,
  snd,
  curry,
  uncurry,
  swap
  )

import Data.Word as X (
  Word,
  Word8,
  Word16,
  Word32,
  Word64,
  )

import Data.Function as X (
--  id,
  const,
--  (.),
  flip,
  ($),
  (&),
  fix,
  on,
  )

import Data.Either as X (
  Either(Left, Right),
  either,
  lefts,
  rights,
  isLeft,
  isRight,
  partitionEithers,
  )

import Data.Maybe as X (
  Maybe(Nothing, Just),
  maybe,
  isJust,
  isNothing,
  fromMaybe,
  listToMaybe,
  maybeToList,
  catMaybes,
  mapMaybe,
  )

import Data.Bool as X (
  Bool(False, True),
  (&&),
  (||),
  not,
  otherwise,
  bool,
  )

import Data.Functor as X (
  Functor((<$)),
  --fmap,
  ($>),
  (<$>),
  void
  )

import Data.Functor.Classes as X (
  Eq1,   Eq2,
  Ord1,  Ord2,
  Show1, Show2,
  Read1, Read2
  )

import Data.Bifunctor as X (
  Bifunctor(bimap, first, second),
  )

import Data.Bifoldable as X (
  Bifoldable(bifoldr, bifoldl, bifoldMap),
  bitraverse_,
  bisequenceA_,
  bifor_,
  )

import Data.Bitraversable as X (
  Bitraversable(bitraverse),
  bifor,
  bisequenceA,
  )

import Data.Void as X (
  Void,
  absurd
  )

import Data.Kind as X (
  Type,
  Constraint,
  )

import Control.Category as X (
  id,
  (.),
  (<<<),
  (>>>)
  )

import Data.Semigroup as X (
  Semigroup((<>), sconcat, stimes),
  First(First, getFirst),
  Last(Last, getLast),
  Min(Min, getMin),
  Max(Max, getMax),
  Option(Option, getOption),
  )

import Data.Monoid as X (
  Monoid(mempty, mappend, mconcat),
  Dual(Dual, getDual),
  Endo(Endo, appEndo),
  All(All, getAll),
  Any(Any, getAny),
  -- Hide because of name clash with sum functors
  --Sum(Sum, getSum),
  --Product(Product, getProduct),
  -- Provide semigroup instances instead
  --First(First, getFirst),
  --Last(Last, getLast),
  Alt(Alt, getAlt),
  )

import Data.Eq as X (
  Eq((==), (/=)),
  )

import Data.Ord as X (
  Ord(compare, (<), (>), (<=), (>=), max, min),
  Ordering(LT,GT,EQ),
  Down(Down),
  comparing
  )

import Text.Show as X (
  Show
  )

import Text.Read as X (
  Read
  )

import Control.Applicative as X (
  Applicative(pure, (<*>), (*>), (<*)),
  Alternative((<|>), empty, many {-, some -}),
  Const(Const, getConst),
  ZipList(ZipList, getZipList),
  (<**>),
  optional,
  liftA2,
  liftA3,
  )

import Control.Monad as X (
  Monad((>>=)),
  (=<<),
  (<=<),
  (>=>),
  MonadPlus(mzero, mplus),
  join,
  guard,
  when,
  unless,
  replicateM,
  replicateM_,
  (<$!>)
  )

import Control.Monad.Fail as X (
  MonadFail(fail),
  )

import Data.Foldable as X (
  Foldable(elem, fold, foldMap,
           foldr, foldr',
           -- foldl, -- hide the bad one
           foldl',
           product, sum, toList),
  null,
  length,
  foldrM,
  foldlM,
  traverse_,
  for_,
  asum,
  concatMap,
  all,
  any,
  or,
  and,
  find,
  notElem,
  sequenceA_,
  )

import Safe as X (
  headMay,
  headDef,
  tailMay,
  tailDef,
  initMay,
  initDef,
  lastMay,
  lastDef,
  toEnumMay,
  toEnumDef,
  cycleMay,
  cycleDef
  )

import Safe.Foldable as X (
  maximumByMay,
  maximumByDef,
  minimumByMay,
  minimumByDef,
  maximumMay,
  maximumDef,
  minimumMay,
  minimumDef,
  )

import Data.Ratio as X (
  Ratio,
  Rational,
  (%),
  numerator,
  denominator,
  approxRational
  )

import Numeric.Natural as X (
  Natural
  )

import Prelude as X (
  ($!),
  (^), -- partial functions!
  (^^),
  seq,
  Char,
  String,
  Float,
  Double,
  Integer,
  FilePath,
  realToFrac,
  even,
  odd,
  asTypeOf,
  until,
  fromIntegral,
  Num((+), (-), (*), negate, abs, signum, fromInteger),
  Real(toRational),
  Integral(quot, rem, div, mod, quotRem, divMod, toInteger), -- partial functions!
  Fractional((/), recip, fromRational), -- partial functions
  Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
           asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
  RealFrac(properFraction, truncate, round, ceiling, floor), -- partial functions
  RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
            encodeFloat, exponent, significand, scaleFloat, isNaN,
            isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),
  Enum(-- toEnum, succ, pred, -- partial
       fromEnum, enumFrom, enumFromThen,
       enumFromTo, enumFromThenTo),
  Bounded(minBound, maxBound),
  )

import System.IO as X (
  IO
  )

import Data.List as X (
  splitAt,
  break,
  span,
  intercalate,
  isPrefixOf,
  isSuffixOf,
  drop,
  filter,
  reverse,
  replicate,
  take,
  sortBy,
  sortOn,
  sort,
  intersperse,
  transpose,
  subsequences,
  permutations,
  scanl,
  scanr,
  iterate,
  repeat,
  -- cycle, -- partial
  unfoldr,
  takeWhile,
  dropWhile,
  dropWhileEnd,
  group,
  groupBy,
  inits,
  tails,
  zip,
  zip3,
  zipWith,
  zipWith3,
  lookup,
  unzip,
  unzip3
  )

import Data.List.NonEmpty as X (
  NonEmpty((:|)),
  -- (<|), -- in lens
  some1,
  scanl1,
  scanr1
  )

import Data.Traversable as X (
  Traversable(traverse, sequenceA),
  for,
  mapAccumL,
  mapAccumR,
  )

import Data.Functor.Identity as X (
  Identity(Identity, runIdentity),
  )

import Control.Monad.Reader as X (
  MonadReader(ask, local, reader),
  --asks,
  Reader,
  runReader,
  mapReader,
  withReader,
  ReaderT(ReaderT, runReaderT),
  mapReaderT,
  withReaderT
  )

import Control.Monad.Trans.Maybe as X (
  MaybeT(MaybeT, runMaybeT),
  mapMaybeT,
  )

import Control.Monad.Except as X (
  MonadError(throwError, catchError),
  ExceptT(ExceptT),
  runExceptT,
  mapExceptT,
  withExceptT,
  Except,
  runExcept,
  mapExcept,
  withExcept,
  )

import Control.Monad.State.Strict as X (
  MonadState(get, put, state),
  State,
  --gets,
  modify,
  modify',
  runState,
  evalState,
  execState,
  mapState,
  withState,
  StateT(StateT, runStateT),
  evalStateT,
  execStateT,
  mapStateT,
  withStateT,
  )

import Control.Monad.RWS.CPS as X (
  MonadRWS,
  RWS,
  runRWS,
  evalRWS,
  execRWS,
  mapRWS,
  RWST,
  runRWST,
  evalRWST,
  execRWST,
  mapRWST,
  )

import Control.Monad.Writer.CPS as X (
  MonadWriter(writer, tell, listen, pass),
  Writer,
  runWriter,
  execWriter,
  mapWriter,
  WriterT,
  runWriterT,
  execWriterT,
  mapWriterT,
  )

import Control.Monad.Trans as X (
  MonadTrans(lift),
  MonadIO(liftIO),
  )

import GHC.Generics as X (
  Generic
  )

import Data.Typeable as X (
  Typeable
  )

-- import Data.Data as X (
--   Data
--   )

import Data.String as X (
  IsString(fromString)
  )

import Data.Proxy as X (
  Proxy(Proxy)
  )

import Data.Tagged as X (
  Tagged(Tagged, unTagged)
  )

import Data.String.Conversions as X (
  ConvertibleStrings(convertString)
  )

import Control.DeepSeq as X (
  NFData
  )

import Data.Binary as X (
  Binary
  )

import Intro.Trustworthy as X

import qualified Prelude
import qualified GHC.Stack.Types
import qualified Data.Functor
import qualified Data.Text.IO
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Text.Read
import qualified Text.Show
import qualified System.IO

type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString

map :: Functor f => (a -> b) -> f a -> f b
map = Data.Functor.fmap
{-# INLINE map #-}

show :: (Show a, ConvertibleStrings String b) => a -> b
show = convertString . showS
{-# INLINE show #-}

showS :: Show a => a -> String
showS = Text.Show.show
{-# INLINE showS #-}

readMaybe :: (Read b, ConvertibleStrings a String) => a -> Maybe b
readMaybe = Text.Read.readMaybe . convertString
{-# INLINE readMaybe #-}

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . System.IO.print
{-# INLINE print #-}

getContents :: MonadIO m => m Text
getContents = liftIO Data.Text.IO.getContents
{-# INLINE getContents #-}

getLine :: MonadIO m => m Text
getLine = liftIO Data.Text.IO.getLine
{-# INLINE getLine #-}

getChar :: MonadIO m => m Char
getChar = liftIO System.IO.getChar
{-# INLINE getChar #-}

putStr, putStrLn :: MonadIO m => Text -> m ()
putStr = liftIO . Data.Text.IO.putStr
putStrLn = liftIO . Data.Text.IO.putStrLn
{-# INLINE putStr #-}
{-# INLINE putStrLn #-}

putChar :: MonadIO m => Char -> m ()
putChar = liftIO . System.IO.putChar
{-# INLINE putChar #-}

readFile :: MonadIO m => FilePath -> m ByteString
readFile = liftIO . Data.ByteString.readFile
{-# INLINE readFile #-}

writeFile :: MonadIO m => FilePath -> ByteString -> m ()
writeFile = liftIO .: Data.ByteString.writeFile
{-# INLINE writeFile #-}

appendFile :: MonadIO m => FilePath -> ByteString -> m ()
appendFile = liftIO .: Data.ByteString.appendFile
{-# INLINE appendFile #-}

readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 = map convertString . readFile
{-# INLINE readFileUtf8 #-}

writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 file = writeFile file . convertString
{-# INLINE writeFileUtf8 #-}

appendFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
appendFileUtf8 file = appendFile file . convertString
{-# INLINE appendFileUtf8 #-}

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: GHC.Stack.Types.HasCallStack => a
undefined = Prelude.undefined

infixr 6 <>^
(<>^) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<>^) = liftA2 (<>)
{-# INLINE (<>^) #-}

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
{-# INLINE (.:) #-}

skip :: Applicative m => m ()
skip = pure ()
{-# INLINE skip #-}

panic :: GHC.Stack.Types.HasCallStack => a
panic = Prelude.error $
  "Panic!\n" <>
  "Please submit a bug report including the stacktrace\n" <>
  "and a description on how to reproduce the bug."
