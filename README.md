# Minimal Haskell curve25519 diffie-hellman function

[![Build Status](https://travis-ci.org/thoughtpolice/hs-curve25519.png?branch=master)](https://travis-ci.org/thoughtpolice/hs-curve25519)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](http://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://www.haskell.org)

This package implements minimal bindings to the [curve25519][]
diffie-hellman function. It should be relatively easy to both depend
on, or include outright in your executable/package itself.

The underlying implementation is the `ref` code from [SUPERCOP][],
which was originally implemented by Dan J. Bernstein.

[curve25519]: http://cr.yp.to/ecdh.html
[SUPERCOP]: http://bench.cr.yp.to/supercop.html

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install curve25519
```

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-curve25519.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-curve25519.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-curve25519/master/AUTHORS.txt).

# License

MIT. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/hs-curve25519/master/LICENSE.txt)
for terms of copyright and redistribution.

[contribute]: https://github.com/thoughtpolice/hs-curve25519/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/hs-curve25519/issues
[gh]: http://github.com/thoughtpolice/hs-curve25519
[bb]: http://bitbucket.org/thoughtpolice/hs-curve25519
[Hackage]: http://hackage.haskell.org/package/curve25519
