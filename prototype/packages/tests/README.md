# Cavefish tests
Cavefish tests is the test suite for the Cavefish server application. It contains integration and end-to-end tests to validate flow correctness.

## Installing / Getting started
> Using `nix` is the most convenient way to install all dependencies.  Non nix user, may refer to the [github action script][def] to build their development environment.

From [`packages/tests/`](./):
```shell
nix develop
```

## Developing

```shell
cabal clean && cabal update 
cabal build cavefish-tests
```

To execute the tests:
```shell
cabal test cavefish-tests
```
For continuous builds, you may use [watchexec](https://github.com/watchexec/watchexec)

```shell
watchexec --clear -w .. 'cabal build all && cabal test cavefish-tests'
```

## Links
TBD

[def]: ../../../.github/workflows/cavefish-server-linux-ci.yml#L1-L5
