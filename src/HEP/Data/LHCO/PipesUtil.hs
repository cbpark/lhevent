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

module HEP.Data.LHCO.PipesUtil
       (
         getLHCOEvent
       , eventFromHandle
       , eventFromBS
       ) where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.ByteString.Char8            (ByteString)
import Pipes
import Pipes.Attoparsec                 (parse)
import Pipes.ByteString                 (fromHandle)
import System.IO                        (Handle)

import HEP.Data.LHCO.Parser             (lhcoEvent)
import HEP.Data.LHCO.Type               (Event)

getLHCOEvent :: Monad m => Producer ByteString m () -> Producer Event m ()
getLHCOEvent s = do (r, s') <- lift $ runStateT (parse lhcoEvent) s
                    case r of Just (Right ev) -> yield ev >> getLHCOEvent s'
                              _               -> return ()

eventFromBS :: Monad m => ByteString -> Producer Event m ()
eventFromBS = getLHCOEvent . yield

eventFromHandle :: MonadIO m => Handle -> Producer Event m ()
eventFromHandle = getLHCOEvent . fromHandle
