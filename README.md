# `linterieur` 

## What is this thing?

A collection of tools for doing bit-bashing that you didn't realize you needed,
but aren't (easily) available in Haskell right now.

## How on earth do you pronounce that?

[In the language of
love](https://translate.google.com/?sl=fr&tl=en&text=l%27int%C3%A9rieur&op=translate).

## What are the goals of this project?

### Efficiency

These are 'hot' functions, which are the bedrock on which other functionality is
built. Thus, they need to be as fast, and consume as little memory, as possible.
It should never be the case that hand-rolling this functionality gives better
results than we provide.

This extends to any additional layers on top of these functions, such as bounds
checking, type classes and so on. We are working in the spirit of (and in
complement to)
[`primitive`](https://hackage.haskell.org/package/primitive-0.7.1.0). The only
wrapping we allow ourselves is the use of lifted types, and perhaps a `Maybe`
here or there.

Additionally, we aim to have benchmarks demonstrating the improvements that our
functions provide over reasonable, but naive, implementations. This ensures that
the gains are clear to our users, and that we don't have regressions.

### Portability

While we don't exclude the use of C, we certainly don't want to limit the
platforms on which this code will run. We don't rule out, for example, more
efficient SIMDed implementations of these functions hidden behind flags, but we
don't ever want to require this. If you can get a GHC on the platform, this
should work too.

### Clarity

Low-level operations can be hard to understand, and usually have _extremely_
sharp edges. Through a combination of documentation and testing, we want to make
it _absolutely_ clear what can and cannot happen with our functions.

### Low dependencies

We see this library as a cog in many future machines, and don't want to ask
people to carry around dependencies outside of [boot packages](). Thus, we will
avoid included non-boot packages as dependencies unless absolutely necessary.
Furthermore, the packages we aim to use should not introduce transitive
dependencies (again, unless absolutely necessary).

## What's with all the cat stuff?

[I am a Haskell catboy.](https://twitter.com/KozRoss)

## What does this run on?

Currently, our CI checks the following versions of GHC:

* 8.6.5
* 8.8.4
* 8.10.4
* 9.0.1

We check on the following platforms:

* Windows
* Linux
* MacOS

## What can I do with this?

The project is licensed Apache 2.0 (SPDX code
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html)). For more details,
please see the `LICENSE.md` file.
