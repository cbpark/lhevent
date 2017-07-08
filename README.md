# lhevent

Tools for analyzing data of Monte Carlo event generators in high energy physics using Haskell. For projects with C++, see [colevent](https://github.com/cbpark/colevent). It supports the [Les Houches Event File](http://home.thep.lu.se/~leif/LHEF/) (LHEF), LHC Olympics (LHCO), and [HepMC](http://hepmc.web.cern.ch/hepmc/).

### Prerequisite

[hep-utilities](https://github.com/cbpark/hep-utilities) is required (not uploaded to [Hackage](https://hackage.haskell.org) nor [Stackage](https://www.stackage.org) yet).

For [Nix](https://hackage.haskell.org) users, see [`default.nix`](default.nix).

## Usage

See codes in [test](test). If you'd use [pipes](http://hackage.haskell.org/package/pipes), see [`HEP.Data.LHEF.PipesUtil`](src/HEP/Data/LHEF/PipesUtil.hs), [`HEP.Data.LHCO.PipesUtil`](src/HEP/Data/LHCO/PipesUtil.hs), and [`HEP.Data.HepMC.PipesUtil`](src/HEP/Data/HepMC/PipesUtil.hs). For instance,

``` haskell
module Main where

import           Pipes              (runEffect, (>->))
import qualified Pipes.Prelude      as P
import           System.Environment (getArgs)
import           System.IO          (IOMode (..), withFile)

import           HEP.Data.LHEF      (getLHEFEvent)

main :: IO ()
main = do
    infile <- head <$> getArgs
    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \hin ->
        runEffect $ getLHEFEvent hin >-> P.take 3 >-> P.print
    putStrLn "-- Done parsing."

```

## References

- [A standard format for Les Houches Event Files](http://arxiv.org/abs/hep-ph/0609017).
- [Les Houches Event File](http://home.thep.lu.se/~leif/LHEF/).
- [How to Read LHC Olympics Data Files](http://madgraph.phys.ucl.ac.be/Manual/lhco.html).
- [LHC Olympics Wiki](http://www.jthaler.net/olympicswiki/doku.php).
- [PGS 4 and the LHC Olympics](http://online.kitp.ucsb.edu/online/lhco_c06/conway/).
- [HepMC 2 user manual](http://hepmc.web.cern.ch/hepmc/releases/HepMC2_user_manual.pdf).
