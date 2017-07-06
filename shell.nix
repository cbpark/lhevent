{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, containers
      , hep-utilities, pipes, pipes-attoparsec, pipes-bytestring, stdenv
      , transformers
      }:
      mkDerivation {
        pname = "lhevent";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = false;
        libraryHaskellDepends = [
          attoparsec base bytestring containers hep-utilities pipes
          pipes-attoparsec pipes-bytestring transformers
        ];
        homepage = "https://github.com/cbpark/lhevent";
        description = "Tools for analyzing the Monte Carlo event data in high energy physics";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
