--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.HepMC.PipesUtil
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Helper functions for analyses of HepMC data files using pipes.
--
--------------------------------------------------------------------------------


module HEP.Data.HepMC.PipesUtil (getHepMCEvent) where

import           Control.Monad.Trans.State.Strict (execStateT)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import           Pipes.ByteString                 (fromHandle)
import           System.IO                        (Handle)

import           HEP.Data.HepMC.Parser            (hepmcEvent, hepmcHeader)
import           HEP.Data.HepMC.Type              (GenEvent)
import           HEP.Data.ParserUtil              (parseEvent)

getHepMCEvent :: MonadIO m => Handle -> Producer GenEvent m ()
getHepMCEvent hin = (lift . evStr) hin >>= parseEvent hepmcEvent
  where evStr = execStateT (PA.parse hepmcHeader) . fromHandle
