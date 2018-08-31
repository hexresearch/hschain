{ mkDerivation, base, bytestring, exceptions, network
, safe-exceptions, stdenv, transformers
}:
mkDerivation {
  pname = "network-simple";
  version = "0.4.2";
  sha256 = "3792778dcb64e60e9fef39841b65e259f31c35ce31a03a52fd0cf3b429c07d40";
  libraryHaskellDepends = [
    base bytestring exceptions network safe-exceptions transformers
  ];
  homepage = "https://github.com/k0001/network-simple";
  description = "Simple network sockets usage patterns";
  license = stdenv.lib.licenses.bsd3;
}
