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

## NOTE: Haskell.nix is heavily reliant on Git!!! If files are not checked in, the build **will** fail
(GitClean deletes files not checked in)
