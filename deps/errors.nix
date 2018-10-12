{ mkDerivation, base, exceptions, safe, stdenv, text, transformers
, transformers-compat
}:
mkDerivation {
  pname = "errors";
  version = "2.3.0";
  sha256 = "6772e5689f07e82077ffe3339bc672934d83d83a97a7d4f1349de1302cb71f75";
  libraryHaskellDepends = [
    base exceptions safe text transformers transformers-compat
  ];
  description = "Simplified error-handling";
  license = stdenv.lib.licenses.bsd3;
}
