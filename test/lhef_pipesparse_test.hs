module Main where

import           Control.Monad      (when)
import           Pipes
import qualified Pipes.Prelude      as P
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (IOMode (..), withFile)

import           HEP.Data.LHEF

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
           putStrLn "Usage: lhef_pipesparse_test filename"
           exitFailure

    let infile = head args
    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \hin ->
        runEffect $ getLHEFEvent hin >-> P.take 3 >-> P.print
    putStrLn "-- Done parsing."
