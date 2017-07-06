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

lineH :: Parser HeavyIon
lineH = do
    skipSpace
    nhsc        <- decimal <* skipSpace
    npp         <- decimal <* skipSpace
    ntp         <- decimal <* skipSpace
    nnn         <- decimal <* skipSpace
    nsn         <- decimal <* skipSpace
    nsp         <- decimal <* skipSpace
    nnnw        <- decimal <* skipSpace
    nnwn        <- decimal <* skipSpace
    nnwnw       <- decimal <* skipSpace
    impct       <- double  <* skipSpace
    epangle     <- double  <* skipSpace
    ecc         <- double  <* skipSpace
    inelstcxsec <- double
    skipTillEnd
    return HeavyIon { numHardScattering             = nhsc
                    , numProjectileParticipants     = npp
                    , numTargetParticipants         = ntp
                    , numNNCollisions               = nnn
                    , numSpectatorNeutrons          = nsn
                    , numSpectatorProtons           = nsp
                    , numNNwoundedCollisions        = nnnw
                    , numNwoundedNCollisions        = nnwn
                    , numNwoundedNwoundedCollisions = nnwnw
                    , impactParamCollision          = impct
                    , eventPlaneAngle               = epangle
                    , eccentricity                  = ecc
                    , inelasticXsecNN               = inelstcxsec
                    }

lineF :: Parser PdfInfo
lineF = do
    skipSpace
    f1    <- signed decimal <* skipSpace
    f2    <- signed decimal <* skipSpace
    bx1   <- double         <* skipSpace
    bx2   <- double         <* skipSpace
    sqpdf <- double         <* skipSpace
    xfx1  <- double         <* skipSpace
    xfx2  <- double         <* skipSpace
    id1   <- signed decimal <* skipSpace
    id2   <- signed decimal
    skipTillEnd
    return PdfInfo { flavor           = (f1, f2)
                   , beamMomentumFrac = (bx1, bx2)
                   , scaleQPDF        = sqpdf
                   , xfx              = (xfx1, xfx2)
                   , idLHAPDF         = (id1, id2)
                   }

lineV :: Parser GenVertex
lineV = do
    char 'V' >> skipSpace
    vbcd     <- signed decimal <* skipSpace
    vid'     <- signed decimal <* skipSpace
    vx       <- double         <* skipSpace
    vy       <- double         <* skipSpace
    vz       <- double         <* skipSpace
    vctau    <- double         <* skipSpace
    norphans <- decimal        <* skipSpace
    nouts    <- decimal        <* skipSpace
    nwgts    <- decimal
    wgts     <- replicateM nwgts (skipSpace *> double)
    let wList = if nwgts > 0 then Just (nwgts, wgts) else Nothing
    skipTillEnd
    return GenVertex { vbarcode    = vbcd
                     , vid         = vid'
                     , vposition   = (vx, vy, vz, vctau)
                     , numOrphan   = norphans
                     , numOutgoing = nouts
                     , vWeightList = wList
                     }

lineP :: Parser GenParticle
lineP = do
    char 'P' >> skipSpace
    pbcd   <- decimal        <* skipSpace
    pid'   <- signed decimal <* skipSpace
    px     <- double         <* skipSpace
    py     <- double         <* skipSpace
    pz     <- double         <* skipSpace
    pe     <- double         <* skipSpace
    gmass  <- double         <* skipSpace
    scode  <- decimal        <* skipSpace
    polTh  <- double         <* skipSpace
    polPh  <- double         <* skipSpace
    vbcd   <- signed decimal <* skipSpace
    nflows <- decimal
    fList' <- replicateM nflows
              ((,) <$> (skipSpace *> signed decimal <* skipSpace)
                   <*> signed decimal)
    let fList = if nflows > 0 then Just (nflows, fList') else Nothing
    skipTillEnd
    return GenParticle { pbarcode             = pbcd
                       , pdgID                = pid'
                       , pMomentum            = (px, py, pz, pe)
                       , pMass                = gmass
                       , statusCode           = scode
                       , polarization         = (polTh, polPh)
                       , vbarcodeThisIncoming = vbcd
                       , flows                = fList
                       }
