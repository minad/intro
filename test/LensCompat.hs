{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module LensCompat (
  module X
) where

import Intro as X

#if __GLASGOW_HASKELL__ < 820
import Control.Lens as X
#endif
