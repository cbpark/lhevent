--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.LHCO.PipesUtil
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Helper functions for analyses of LHCO data files using pipes.
--
--------------------------------------------------------------------------------

module HEP.Data.LHCO.PipesUtil (getLHCOEvent) where

import Pipes
import Pipes.ByteString     (fromHandle)
import System.IO            (Handle)

import HEP.Data.LHCO.Parser (lhcoEvent)
import HEP.Data.LHCO.Type   (Event)
import HEP.Data.ParserUtil  (parseEvent)

getLHCOEvent :: MonadIO m => Handle -> Producer Event m ()
getLHCOEvent = parseEvent lhcoEvent . fromHandle
