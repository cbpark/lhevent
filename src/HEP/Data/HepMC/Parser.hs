{-# LANGUAGE OverloadedStrings #-}

module HEP.Data.HepMC.Parser where

import Control.Applicative              ((<|>))
import Control.Monad                    (replicateM)
import Data.Attoparsec.ByteString.Char8
import Prelude                          hiding (takeWhile)

import HEP.Data.HepMC.Type
import HEP.Data.ParserUtil              (skipTillEnd)

lineN :: Parser WeightNames
lineN = do
    skipSpace
    n     <- decimal <* skipSpace
    wstrs <- replicateM n (skipSpace *> weightStrings)
    skipTillEnd
    return (WeightNames n wstrs)
  where
    weightStrings = char '"' *> takeWhile (/= '"') <* char '"'

lineU :: Parser MomentumPositionUnit
lineU = do
    skipSpace
    mUnit <- (string "GEV" >> return GeV) <|> (string "MEV" >> return MeV)
    skipSpace
    lUnit <- (string "MM" >> return MM) <|> (string "CM" >> return CM)
    skipTillEnd
    return (MomentumPositionUnit mUnit lUnit)

lineC :: Parser GenCrossSection
lineC = do
    skipSpace
    xs  <- double <* skipSpace
    err <- double
    skipTillEnd
    return (GenCrossSection xs err)
