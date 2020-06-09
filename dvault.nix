{ mkDerivation, base, bytestring, containers, crypto-rng
, data-default, directory, process, stdenv, vector
}:
mkDerivation {
  pname = "dvault";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers crypto-api monadcryptorandom data-default directory
    process vector
  ];
  homepage = "https://github.com/zenhack/dvault";
  description = "Dead simple password manager";
  license = stdenv.lib.licenses.mit;
}
