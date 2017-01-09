{-# LANGUAGE NoImplicitPrelude #-}
module BaseCompatibility (
  module X
) where

import Intro as X
--import Data.Monoid as X
--import Debug.Trace as X hiding (traceShowId, traceM, traceShowM)
--import GHC.Exts as X (lazy, inline, sortWith, groupWith)
--import Prelude as X hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, id, (.))
--import Text.Read as X -- (Read(..), readMaybe, readEither)
import Control.Applicative as X
import Control.Arrow as X hiding (first, second)
import Control.Category as X
import Control.Concurrent as X
import Control.Exception as X
import Control.Monad as X hiding (fail)
import Control.Monad.Fail as X
import Control.Monad.Fix as X
import Control.Monad.ST as X
import Data.Bifunctor as X
import Data.Bits as X
import Data.Bool as X
import Data.Char as X
import Data.Complex as X
import Data.Data as X
import Data.Dynamic as X
import Data.Either as X
import Data.Fixed as X
import Data.Foldable as X
import Data.Function as X hiding ((.), id)
import Data.Functor as X
import Data.IORef as X
import Data.Int as X
import Data.Ix as X
import Data.List as X hiding (scanl1, scanr1, map)
import Data.Maybe as X
import Data.Ord as X
import Data.Ratio as X
import Data.STRef as X
import Data.String as X
import Data.Traversable as X
import Data.Tuple as X
import Data.Unique as X
import Data.Version as X
import Data.Word as X
import Foreign.Storable as X
import GHC.Conc as X hiding (withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
import GHC.Generics as X (Generic)
import GHC.IO.Exception as X
import Numeric as X
import System.Environment as X
import System.Exit as X
import System.IO as X (Handle, hClose)
import System.IO.Error as X
import System.IO.Unsafe as X
import System.Mem as X
import System.Mem.StableName as X
import System.Timeout as X
import Text.ParserCombinators.ReadP as X (ReadP, ReadS, readP_to_S, readS_to_P)
import Text.ParserCombinators.ReadPrec as X (ReadPrec, readPrec_to_P, readP_to_Prec, readPrec_to_S, readS_to_Prec)
import Text.Printf as X
import Unsafe.Coerce as X
