{ mkDerivation, ansi-terminal, async, base, directory, exceptions
, process, stdenv, stm, terminal-size, text, transformers, unix
}:
mkDerivation {
  pname = "concurrent-output";
  version = "1.10.5";
  sha256 = "98c096228664d591eb2c352080955a202e883cdab8452efae3cae407e30fa5c3";
  libraryHaskellDepends = [
    ansi-terminal async base directory exceptions process stm
    terminal-size text transformers unix
  ];
  description = "Ungarble output from several threads or commands";
  license = stdenv.lib.licenses.bsd2;
}
