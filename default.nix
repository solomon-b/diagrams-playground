{ mkDerivation, base, diagrams, lens, mtl, profunctors, stdenv
, transformers
}:
mkDerivation {
  pname = "diagrams-playground";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams lens mtl profunctors transformers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
