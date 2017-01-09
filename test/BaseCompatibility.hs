{-# LANGUAGE NoImplicitPrelude #-}
module BaseCompatibility (
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
import Control.Monad.Fail as X
import Control.Monad.Fix as X
import Control.Monad.ST as X
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
import Data.IORef as X
import Data.Int as X
import Data.Ix as X
import Data.List as X hiding (scanl1, scanr1, map)
import Data.Maybe as X
import Data.Monoid as X hiding (First(..), Last(..), (<>))
import Data.Ord as X
import Data.Ratio as X
import Data.STRef as X
import Data.Semigroup as X
import Data.String as X
import Data.Traversable as X
import Data.Tuple as X
import Data.Unique as X
import Data.Version as X
import Data.Word as X
import Foreign.Storable as X
import GHC.Generics as X hiding (Fixity(..), prec)
import GHC.IO.Exception as X
import Numeric as X
import Prelude as X hiding ((.), id, map, putChar, putStrLn, putStr, getContents, getLine, print, getChar, appendFile, readFile, writeFile, fail, show, undefined, scanl1, scanr1)
import System.Environment as X
import System.Exit as X
import System.IO as X hiding (putChar, putStrLn, putStr, getContents, getLine, print, getChar, appendFile, readFile, writeFile)
import System.IO.Error as X
import System.IO.Unsafe as X
import System.Mem as X
import System.Mem.StableName as X
import System.Timeout as X
import Text.ParserCombinators.ReadP as X hiding (get, (+++), optional, (<++), look, pfail, many, choice, option)
import Text.ParserCombinators.ReadPrec as X hiding (get, (+++), lift)
import Text.Printf as X
import Text.Read as X hiding (readMaybe, get, lift, EOF, (+++))
import Unsafe.Coerce as X
import Data.Proxy as X

-- import Control.Monad as X
-- import Control.Monad.Fail as X
-- import Control.Monad.Fix as X
-- import Control.Monad.IO.Class as X
-- import Control.Monad.Instances as X
-- import Control.Monad.ST as X
-- import Control.Monad.ST.Lazy as X
-- import Control.Monad.ST.Lazy.Safe as X
-- import Control.Monad.ST.Lazy.Unsafe as X
-- import Control.Monad.ST.Safe as X
-- import Control.Monad.ST.Strict as X
-- import Control.Monad.ST.Unsafe as X
-- import Control.Monad.Zip as X
-- import Data.Function as X
-- import Data.Functor.Compose as X
-- import Data.Functor.Const as X
-- import Data.Functor.Identity as X
-- import Data.Functor.Product as X
-- import Data.Functor.Sum as X
-- import Data.IORef as X
-- import Data.Int as X
-- import Data.Ix as X
-- import Data.Kind as X
-- import Data.List as X
-- import Data.List.NonEmpty as X
-- import Data.Monoid as X

-- import Data.STRef as X
-- import Data.STRef.Lazy as X
-- import Data.STRef.Strict as X
-- import Data.Type.Bool as X
-- import Data.Type.Coercion as X
-- import Data.Type.Equality as X
-- import Data.Typeable as X
-- import Data.Typeable.Internal as X
-- import Data.Unique as X
-- import Data.Version as X
-- import Data.Void as X
-- import Data.Word as X
-- import Debug.Trace as X
-- import Foreign as X
-- import Foreign.C as X
-- import Foreign.C.Error as X
-- import Foreign.C.String as X
-- import Foreign.C.Types as X
-- import Foreign.Concurrent as X
-- import Foreign.ForeignPtr as X
-- import Foreign.ForeignPtr.Safe as X
-- import Foreign.ForeignPtr.Unsafe as X
-- import Foreign.Marshal as X
-- import Foreign.Marshal.Alloc as X
-- import Foreign.Marshal.Array as X
-- import Foreign.Marshal.Error as X
-- import Foreign.Marshal.Pool as X
-- import Foreign.Marshal.Safe as X
-- import Foreign.Marshal.Unsafe as X
-- import Foreign.Marshal.Utils as X
-- import Foreign.Ptr as X
-- import Foreign.Safe as X
-- import Foreign.StablePtr as X
-- import Foreign.Storable as X
-- import GHC.Arr as X
-- import GHC.Base as X
-- import GHC.Char as X
-- import GHC.Conc as X
-- import GHC.Conc.IO as X
-- import GHC.Conc.Signal as X
-- import GHC.Conc.Sync as X
-- import GHC.ConsoleHandler as X
-- import GHC.Constants as X
-- import GHC.Desugar as X
-- import GHC.Enum as X
-- import GHC.Environment as X
-- import GHC.Err as X
-- import GHC.Event as X
-- import GHC.Exception as X
-- import GHC.ExecutionStack as X
-- import GHC.ExecutionStack.Internal as X
-- import GHC.Exts as X
-- import GHC.Fingerprint as X
-- import GHC.Fingerprint.Type as X
-- import GHC.Float as X
-- import GHC.Float.ConversionUtils as X
-- import GHC.Float.RealFracMethods as X
-- import GHC.Foreign as X
-- import GHC.ForeignPtr as X
-- import GHC.GHCi as X
-- import GHC.Generics as X
-- import GHC.IO as X
-- import GHC.IO.Buffer as X
-- import GHC.IO.BufferedIO as X
-- import GHC.IO.Device as X
-- import GHC.IO.Encoding as X
-- import GHC.IO.Encoding.CodePage as X
-- import GHC.IO.Encoding.Failure as X
-- import GHC.IO.Encoding.Iconv as X
-- import GHC.IO.Encoding.Latin1 as X
-- import GHC.IO.Encoding.Types as X
-- import GHC.IO.Encoding.UTF16 as X
-- import GHC.IO.Encoding.UTF32 as X
-- import GHC.IO.Encoding.UTF8 as X
-- import GHC.IO.Exception as X
-- import GHC.IO.FD as X
-- import GHC.IO.Handle as X
-- import GHC.IO.Handle.FD as X
-- import GHC.IO.Handle.Internals as X
-- import GHC.IO.Handle.Text as X
-- import GHC.IO.Handle.Types as X
-- import GHC.IO.IOMode as X
-- import GHC.IO.Unsafe as X
-- import GHC.IOArray as X
-- import GHC.IORef as X
-- import GHC.Int as X
-- import GHC.List as X
-- import GHC.MVar as X
-- import GHC.Natural as X
-- import GHC.Num as X
-- import GHC.OldList as X
-- import GHC.OverloadedLabels as X
-- import GHC.PArr as X
-- import GHC.Pack as X
-- import GHC.Profiling as X
-- import GHC.Ptr as X
-- import GHC.RTS.Flags as X
-- import GHC.Read as X
-- import GHC.Real as X
-- import GHC.ST as X
-- import GHC.STRef as X
-- import GHC.Show as X
-- import GHC.Stable as X
-- import GHC.Stack as X
-- import GHC.Stack.CCS as X
-- import GHC.Stack.Types as X
-- import GHC.StaticPtr as X
-- import GHC.Stats as X
-- import GHC.Storable as X
-- import GHC.TopHandler as X
-- import GHC.TypeLits as X
-- import GHC.Unicode as X
-- import GHC.Weak as X
-- import GHC.Word as X
-- import Numeric as X
-- import Numeric.Natural as X
-- import Prelude as X
-- import System.CPUTime as X
-- import System.Console.GetOpt as X
-- import System.Environment as X
-- import System.Exit as X
-- import System.IO as X
-- import System.IO.Error as X
-- import System.IO.Unsafe as X
-- import System.Info as X
-- import System.Mem as X
-- import System.Mem.StableName as X
-- import System.Mem.Weak as X
-- import System.Posix.Internals as X
-- import System.Posix.Types as X
-- import System.Timeout as X
-- import Text.ParserCombinators.ReadP as X
-- import Text.ParserCombinators.ReadPrec as X
-- import Text.Printf as X
-- import Text.Read as X
-- import Text.Read.Lex as X
-- import Text.Show as X
-- import Text.Show.Functions as X
-- import Unsafe.Coerce as X
