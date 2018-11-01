{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module BaseCompat (
  module X
) where

import Intro as X

import Control.Applicative as X
import Control.Arrow as X hiding (first, second)
import Control.Category as X
import Control.Concurrent as X
import Control.Exception as X
import Control.Exception.Base as X
import Control.Monad as X hiding (fail)
import Control.Monad.Fix as X
import Control.Monad.IO.Class as X
import Control.Monad.ST as X
import Control.Monad.ST.Unsafe as X
import Control.Monad.Zip as X
import Data.Bifunctor as X
import Data.Bits as X
import Data.Bool as X
import Data.Char as X
import Data.Coerce as X
import Data.Complex as X
import Data.Data as X
import Data.Dynamic as X
import Data.Either as X
import Data.Eq as X
import Data.Fixed as X
import Data.Foldable as X
import Data.Function as X hiding ((.), id)
import Data.Functor as X
import Data.Functor.Classes as X
import Data.Functor.Compose as X
import Data.Functor.Identity as X
import Data.IORef as X
import Data.Int as X
import Data.Ix as X
import Data.List as X hiding (scanl1, scanr1, map, cycle, head, init, last, tail)
import Data.Maybe as X
import Data.Monoid as X hiding (First(..), Last(..), (<>))
import Data.Ord as X
import Data.Proxy as X
import Data.Ratio as X
import Data.STRef as X
import Data.Semigroup as X
import Data.String as X
import Data.Traversable as X
import Data.Tuple as X
import Data.Type.Bool as X
import Data.Type.Coercion as X
import Data.Type.Equality as X hiding (trans, sym)
import Data.Unique as X
import Data.Version as X
import Data.Void as X
import Data.Word as X
import Numeric as X
import Numeric.Natural as X
import Prelude as X hiding ((.), id, map, putChar, putStrLn, putStr, getContents, getLine, print, getChar, appendFile, readFile, writeFile, fail, show, undefined, scanl1, scanr1, cycle, head, init, last, tail)
import System.CPUTime as X
import System.Console.GetOpt as X hiding (Option)
import System.Environment as X
import System.Exit as X
import System.IO as X hiding (putChar, putStrLn, putStr, getContents, getLine, print, getChar, appendFile, readFile, writeFile)
import System.IO.Error as X
import System.IO.Unsafe as X
import System.Info as X
import System.Mem as X
import System.Mem.StableName as X
import System.Mem.Weak as X
import System.Timeout as X
import Text.ParserCombinators.ReadP as X hiding (get, (+++), optional, (<++), look, pfail, many, choice, option)
import Text.ParserCombinators.ReadPrec as X hiding (get, (+++), lift)
import Text.Printf as X
import Text.Read as X hiding (readMaybe, get, lift, EOF, (+++))
import Text.Show as X hiding (show)
import Unsafe.Coerce as X

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail as X hiding (fail)
import Data.Kind as X
#endif
