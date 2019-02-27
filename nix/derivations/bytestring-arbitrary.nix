{ mkDerivation, base, bytestring, criterion, cryptohash, QuickCheck
, stdenv, fetchgitPrivate
}:
mkDerivation {
  pname = "bytestring-arbitrary";
  version = "0.1.2";
  src = fetchgitPrivate { url = "https://github.com/hexresearch/bytestring-arbitrary";
                          rev = "fada44f857c082a1e2b4eafaf3e679ff4576ae56";
                          sha256 = "1m05w87yf9q6sm9yb52rjswrbp9p55irfqpgwv2mb1bcbjkqagsh";
                        };
  libraryHaskellDepends = [ base bytestring cryptohash QuickCheck ];
  testHaskellDepends = [ base bytestring cryptohash QuickCheck ];
  benchmarkHaskellDepends = [
    base bytestring criterion cryptohash QuickCheck
  ];
  homepage = "https://github.com/tsuraan/bytestring-arbitrary";
  description = "Arbitrary instances for ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}
