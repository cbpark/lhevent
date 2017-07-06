# lhevent

Tools for analyzing data of Monte Carlo event generators in high energy physics using Haskell. For projects with C++, see [colevent](https://github.com/cbpark/colevent). It supports the [Les Houches Event File](http://home.thep.lu.se/~leif/LHEF/) (LHEF), LHC Olympics (LHCO), and [HepMC](http://hepmc.web.cern.ch/hepmc/).

### Prerequisite

[hep-utilities](https://github.com/cbpark/hep-utilities) is required.

## Usage

See [`lhef_parse_test.hs`](test/lhef_parse_test.hs) and [`lhco_parse_test.hs`](test/lhco_parse_test.hs). If you'd use [pipes](http://hackage.haskell.org/package/pipes), see [`HEP.Data.LHEF.PipesUtil`](src/HEP/Data/LHEF/PipesUtil.hs) and [`HEP.Data.LHCO.PipesUtil`](src/HEP/Data/LHCO/PipesUtil.hs).

## References

- [A standard format for Les Houches Event Files](http://arxiv.org/abs/hep-ph/0609017).
- [Les Houches Event File](http://home.thep.lu.se/~leif/LHEF/).
- [How to Read LHC Olympics Data Files](http://madgraph.phys.ucl.ac.be/Manual/lhco.html).
- [LHC Olympics Wiki](http://www.jthaler.net/olympicswiki/doku.php).
- [PGS 4 and the LHC Olympics](http://online.kitp.ucsb.edu/online/lhco_c06/conway/).
- [HepMC 2 user manual](http://hepmc.web.cern.ch/hepmc/releases/HepMC2_user_manual.pdf).
