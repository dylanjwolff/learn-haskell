Using Haskell.nix 'Alternate' method for building Haskell projects with nix

Get a shell with the appropriate dependencies:
```
nix-shell -A learn-haskell
```
From there you can do e.g. ```cabal repl```

To build the executable with Nix/the nix package itself:
```
nix-build -A learn-haskell.components.exes
```
