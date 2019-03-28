rec {
  tryEval = var : default :
    with builtins.tryEval var;
      if success
        then value
        else default;

  # Apply function if flag is true. Otherwise do nothing
  # 
  # doIf :: Bool -> (a -> a) -> (a -> a)
  doIf = flag: fun: if flag then fun else (x: x);
}
