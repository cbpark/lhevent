module HEP.Data.HepMC.PipesUtil (getHepmcEvent) where

import           Control.Monad.Trans.State.Strict (StateT (..), execStateT)
import           Data.ByteString.Char8            (ByteString)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import           Pipes.ByteString                 (fromHandle)
import           System.IO                        (Handle)

import           HEP.Data.HepMC.Parser            (hepmcEvent, hepmcHeader)
import           HEP.Data.HepMC.Type              (GenEvent)

getHepmcEvent :: MonadIO m => Handle -> Producer GenEvent m ()
getHepmcEvent hin = (lift . evStr) hin >>= parseEvent
  where evStr = execStateT (PA.parse hepmcHeader) . fromHandle

parseEvent :: Monad m => Producer ByteString m () -> Producer GenEvent m ()
parseEvent s = do (r, s') <- lift $ runStateT (PA.parse hepmcEvent) s
                  case r of Just (Right ev) -> yield ev >> parseEvent s'
                            _               -> return ()
