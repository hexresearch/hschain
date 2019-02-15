rec {
  tryEval = var : default :
    with builtins.tryEval var;
      if success
        then value
        else default;

   readConfig = overrideCfg : cfgFile :
     let inFile = tryEval overrideCfg cfgFile;
     in builtins.fromJSON (builtins.readFile inFile);
}
