{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Intro.Trustworthy
-- Copyright   :  (c) Daniel Mendler 2016-2017
-- License     :  MIT
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Trustworthy reexports from 'GHC.Exts' and 'Debug.Trace'
--
-----------------------------------------------------------------------------

module Intro.Trustworthy (
  Data.DList.DList
  , GHC.Exts.IsList(
      Item
      , fromList
      , toList
      )
  , Constraint
  , HasCallStack
  , trace
  , traceIO
  , traceId
  , traceM
  , traceShow
  , traceShowId
  , traceShowM
  , Data.Hashable.Lifted.Hashable1
  , Data.Hashable.Lifted.Hashable2
) where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.Function ((.))
import Data.Text (Text, unpack)
import Text.Show (Show)
import qualified Data.DList
import qualified Debug.Trace
import qualified GHC.Exts
import qualified Data.Hashable.Lifted

#if MIN_VERSION_base(4,9,0)
import Control.Applicative (Applicative)
import Data.Kind (Constraint)
import GHC.Stack (HasCallStack)
#define APPLICATIVE Applicative
#else
import Control.Monad (Monad)
import GHC.Exts (Constraint)
type HasCallStack = (() :: GHC.Exts.Constraint)
#define APPLICATIVE Monad
#endif

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
trace = Debug.Trace.trace . unpack
{-# WARNING trace "'trace' should be used only for debugging" #-}

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
traceM :: APPLICATIVE m => Text -> m ()
traceM = Debug.Trace.traceM . unpack
{-# WARNING traceM "'traceM' should be used only for debugging" #-}

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
{-# WARNING traceShow "'traceShow' should be used only for debugging" #-}

-- | Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.
--
-- > ... = do
-- >   x <- ...
-- >   traceShowM $ x
-- >   y <- ...
-- >   traceShowM $ x + y
traceShowM :: (Show a, APPLICATIVE m) => a -> m ()
traceShowM = Debug.Trace.traceShowM
{-# WARNING traceShowM "'traceShowM' should be used only for debugging" #-}

-- | The 'traceIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
traceIO :: MonadIO m => Text -> m ()
traceIO = liftIO . Debug.Trace.traceIO . unpack
{-# WARNING traceIO "'traceIO' should be used only for debugging" #-}

-- | Like 'traceShow' but returns the shown value instead of a third value.
traceShowId :: Show a => a -> a
traceShowId = Debug.Trace.traceShowId
{-# WARNING traceShowId "'traceShowId' should be used only for debugging" #-}

-- | Like 'trace' but returns the message instead of a third value.
traceId :: Text -> Text
traceId a = Debug.Trace.trace (unpack a) a
{-# WARNING traceId "'traceId' should be used only for debugging" #-}
