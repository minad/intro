{-# LANGUAGE NoImplicitPrelude #-}
module AesonCompat (
  module X
) where

import Intro as X
import Data.Aeson as X hiding ((.:))
