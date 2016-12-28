{-# LANGUAGE Trustworthy #-}
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

import qualified Data.DList
import qualified GHC.Exts
import qualified Data.Text
import qualified Debug.Trace

-- | See 'Debug.trace.trace'
trace :: Data.Text.Text -> a -> a
trace = Debug.Trace.trace . Data.Text.unpack
{-# WARNING trace "'trace' remains in code" #-}

-- | See 'Debug.trace.traceStack'
traceStack :: Data.Text.Text -> a -> a
traceStack = Debug.Trace.traceStack . Data.Text.unpack
{-# WARNING traceStack "'traceStack' remains in code" #-}

-- | 'traceStack' lifted to a 'Monad'
traceStackM :: Monad m => Data.Text.Text -> m ()
traceStackM s = traceStack s $ pure ()
{-# WARNING traceStackM "'traceStackM' remains in code" #-}

-- | See 'Debug.Trace.traceShow'
traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow
{-# WARNING traceShow "'traceShow' remains in code" #-}

-- | See 'Debug.Trace.traceShowM'
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = Debug.Trace.traceShowM
{-# WARNING traceShowM "'traceShowM' remains in code" #-}

-- | See 'Debug.Trace.traceM'
traceM :: Monad m => Data.Text.Text -> m ()
traceM = Debug.Trace.traceM . Data.Text.unpack
{-# WARNING traceM "'traceM' remains in code" #-}

-- | See 'Debug.Trace.traceIO'
traceIO :: Data.Text.Text -> IO ()
traceIO = Debug.Trace.traceIO . Data.Text.unpack
{-# WARNING traceIO "'traceIO' remains in code" #-}
