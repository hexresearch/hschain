{ mkDerivation, aeson, async, base, bloodhound, bytestring
, containers, criterion, deepseq, enclosed-exceptions, exceptions
, http-client, http-types, katip, lens, lens-aeson
, quickcheck-instances, random, retry, scientific, semigroups
, stdenv, stm, stm-chans, tagged, tasty, tasty-hunit
, tasty-quickcheck, text, time, transformers, unordered-containers
, uuid, vector
}:
mkDerivation {
  pname = "katip-elasticsearch";
  version = "0.5.1.0";
  sha256 = "7003fe3390871e95c6260db3b9a2787ced90e2ecf8a447023cf170d0b346885a";
  libraryHaskellDepends = [
    aeson async base bloodhound bytestring enclosed-exceptions
    exceptions http-client http-types katip retry scientific semigroups
    stm stm-chans text time transformers unordered-containers uuid
  ];
  testHaskellDepends = [
    aeson base bloodhound bytestring containers http-client http-types
    katip lens lens-aeson quickcheck-instances scientific stm tagged
    tasty tasty-hunit tasty-quickcheck text time transformers
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    aeson base bloodhound criterion deepseq random text
    unordered-containers uuid
  ];
  homepage = "https://github.com/Soostone/katip";
  description = "ElasticSearch scribe for the Katip logging framework";
  license = stdenv.lib.licenses.bsd3;
}
