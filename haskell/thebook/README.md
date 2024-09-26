## Testing 

To run with `QuickCheck`:

```sh
$  cabal install QuickCheck
```
and then
```sh
$ cabal repl --build-depends "QuickCheck >= 2.14, checkers >= 0.6.0"
```