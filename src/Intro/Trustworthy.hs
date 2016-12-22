{-# LANGUAGE Trustworthy #-}
module Intro.Trustworthy (
  module X,
  trace,
  traceIO,
  traceM,
  traceShow,
  traceShowM,
  traceStack,
  traceStackM,
) where

import Data.DList as X (
  DList,
  )

import GHC.Exts as X (
  IsList(Item, fromList
          -- , toList
         )
  )

import qualified Data.Text
import qualified Debug.Trace

{-# WARNING trace "'trace' remains in code" #-}
trace :: Data.Text.Text -> a -> a
trace = Debug.Trace.trace . Data.Text.unpack

{-# WARNING traceStack "'traceStack' remains in code" #-}
traceStack :: Data.Text.Text -> a -> a
traceStack = Debug.Trace.traceStack . Data.Text.unpack

{-# WARNING traceStackM "'traceStackM' remains in code" #-}
traceStackM :: Monad m => Data.Text.Text -> m ()
traceStackM s = traceStack s $ pure ()

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = Debug.Trace.traceShowM

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: Monad m => Data.Text.Text -> m ()
traceM = Debug.Trace.traceM . Data.Text.unpack

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: Data.Text.Text -> IO ()
traceIO = Debug.Trace.traceIO . Data.Text.unpack
