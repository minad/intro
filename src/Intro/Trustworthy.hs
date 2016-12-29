{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Intro.Trustworthy (
  Data.DList.DList
  , GHC.Exts.IsList(
      Item
      , fromList
      -- , toList -- provided by Foldable
      )
  , trace
  , traceIO
  , traceM
  , traceShow
  , traceShowM
  , traceStack
  , traceStackM
) where

import Control.Applicative (Applicative(pure))
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Function ((.), ($))
import Data.Text (Text)
import Text.Show (Show)
import qualified Data.DList
import qualified Data.Text
import qualified Debug.Trace
import qualified GHC.Exts

-- | The 'trace' function outputs the trace message given as its first argument,
-- before returning the second argument as its result.
--
-- For example, this returns the value of @f x@ but first outputs the message.
--
-- > trace ("calling f with x = " ++ show x) (f x)
--
-- The 'trace' function should /only/ be used for debugging, or for monitoring
-- execution. The function is not referentially transparent: its type indicates
-- that it is a pure function but it has the side effect of outputting the
-- trace message.
trace :: Text -> a -> a
trace = Debug.Trace.trace . Data.Text.unpack
{-# WARNING trace "'trace' remains in code" #-}

-- | Like 'trace' but returning unit in an arbitrary 'Applicative' context. Allows
-- for convenient use in do-notation.
--
-- Note that the application of 'traceM' is not an action in the 'Applicative'
-- context, as 'traceIO' is in the 'MonadIO' type. While the fresh bindings in the
-- following example will force the 'traceM' expressions to be reduced every time
-- the @do@-block is executed, @traceM "not crashed"@ would only be reduced once,
-- and the message would only be printed once.  If your monad is in 'MonadIO',
-- @traceIO@ may be a better option.
--
-- > ... = do
-- >   x <- ...
-- >   traceM $ "x: " ++ show x
-- >   y <- ...
-- >   traceM $ "y: " ++ show y
traceM :: Applicative m => Text -> m ()
traceM = Debug.Trace.traceM . Data.Text.unpack
{-# WARNING traceM "'traceM' remains in code" #-}

-- | like 'trace', but additionally prints a call stack if one is
-- available.
--
-- In the current GHC implementation, the call stack is only
-- available if the program was compiled with @-prof@; otherwise
-- 'traceStack' behaves exactly like 'trace'.  Entries in the call
-- stack correspond to @SCC@ annotations, so it is a good idea to use
-- @-fprof-auto@ or @-fprof-auto-calls@ to add SCC annotations automatically.
traceStack :: Text -> a -> a
traceStack = Debug.Trace.traceStack . Data.Text.unpack
{-# WARNING traceStack "'traceStack' remains in code" #-}

-- | Like 'traceStack' but returning unit in an arbitrary 'Applicative' context. Allows
-- for convenient use in do-notation.
traceStackM :: Applicative m => Text -> m ()
traceStackM s = traceStack s $ pure ()
{-# WARNING traceStackM "'traceStackM' remains in code" #-}

-- | Like 'trace', but uses 'show' on the argument to convert it to a 'String'.
--
-- This makes it convenient for printing the values of interesting variables or
-- expressions inside a function. For example here we print the value of the
-- variables @x@ and @z@:
--
-- > f x y =
-- >     traceShow (x, z) $ result
-- >   where
-- >     z = ...
-- >     ...
traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow
{-# WARNING traceShow "'traceShow' remains in code" #-}

-- | Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.
--
-- > ... = do
-- >   x <- ...
-- >   traceShowM $ x
-- >   y <- ...
-- >   traceShowM $ x + y
traceShowM :: (Show a, Applicative m) => a -> m ()
traceShowM = Debug.Trace.traceShowM
{-# WARNING traceShowM "'traceShowM' remains in code" #-}

-- | The 'traceIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
traceIO :: MonadIO m => Text -> m ()
traceIO = liftIO . Debug.Trace.traceIO . Data.Text.unpack
{-# WARNING traceIO "'traceIO' remains in code" #-}
