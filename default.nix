{ mkDerivation, base, diagrams, diagrams-lib, diagrams-rasterific
, diagrams-svg, free, stdenv, transformers
}:
mkDerivation {
  pname = "diagrams-playground";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams diagrams-lib diagrams-rasterific diagrams-svg free
    transformers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
