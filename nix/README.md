# Using Nix for HSChain development

In order to start nix shell in which one could develop HSChain one only need to
run `nix-shell` in project root. In nix shelle project could be build using
cabal:

```
cabal new-build all
```
