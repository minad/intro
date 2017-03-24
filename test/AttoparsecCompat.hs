{-# LANGUAGE NoImplicitPrelude #-}
module AttoparsecCompat (
  module X
) where

import Intro as X
import Data.Attoparsec.ByteString as X hiding (take, skip, takeWhile)
