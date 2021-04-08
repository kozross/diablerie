# Contribution guide

## Introduction

First of all, thank you for wanting to contribute! This guide is designed to
help make sure that your contribution experience is as stress-free and
straightforward as possible.

## Git practices

Please fork, and make PRs to, the `dev` branch. `main` is used only for
releases.

Ensure that your commits are individually buildable, and that all tests pass on
each commit (doctests and otherwise). Each commit should have a concise, but
clear description of what it fixes or adds. Refer to issues if relevant by
tagging with # followed by the issue number (for example, "Fix #1234"). To check
if your doctests pass, we recommend `cabal-docspec` from
[`cabal-extras`](https://github.com/phadej/cabal-extras).

## Cabal file standards

The cabal file for this project must be formatted according to
[`cabal-fmt`](http://hackage.haskell.org/package/cabal-fmt). All dependencies
must have bounds; where possible, `^>=`-style bounds are preferable.

## Code standards

We follow the [Package Versioning Policy](https://pvp.haskell.org). If your
changes are significant enough to warrant a version change by the Policy, ensure
that you do so, and update the changelog to match. If you are unsure, you can
use [`Policeman`](http://hackage.haskell.org/package/policeman) to check.

All code is to be formatted using
[`ormolu`](http://hackage.haskell.org/package/ormolu), and must be free of
warnings as emitted by [Hlint](http://hackage.haskell.org/package/hlint), both
with default settings. If a warning is spurious, it must be silenced in the
narrowest possible scope, with an explanatory comment.

Imports into a module may take one of the following forms only:

* `import Foo (Bar, baz, quux)`; or
* `import qualified Foo as Baz`

For data type imports, wildcard imports should not be used; instead, specify the
constructor(s) you want explicitly:

```haskell
-- Not like this: import Foo (Bar(..))
import Foo (Bar (Baz, Quux))
```

Every publically-facing module must have an explicit export list (internal
modules can omit this). All publically-exported identifiers should have
Haddocks, indicating an `@since` with the version where they first appeared, or
last changed semantically. For functions, doctests should be provided, ideally
demonstrating as much of the functionality as reasonable. Edge cases are
_especially_ critical: provide a clear explanation of these in the Haddocks, or
show the behaviour with doctests, preferably both.

Where possible, keep to a similar style to the rest of the module (and the
package). This isn't a hard-and-fast rule, but a good thing to keep in mind for
consistency reasons.
