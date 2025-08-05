# Tech Team Haskell Trial

This is a simple
[Servant](https://hackage.haskell.org/package/servant) API to help us
evaluate potential Haskell developers. It implements a few endpoints
of an API for shopping carts.

Helper scripts for exercising the API are in the `scripts/` subdirectory.

# Setup

Clone the repo (please don't fork it), and then you can use:

- `cabal build`/`cabal run tech-team-haskell-trial`/`cabal test` directly;
- `nix develop`, then `cabal build`/`cabal run`; or
- `stack build`, and other stack commands
