# [Octane][]

Octane parses [Rocket League][] replays.

## Usage

To install Octane, start off by installing [Stack][]. Then you can build Octane
with:

``` sh
> stack build --install-ghc
```

That may take a while. When it finishes, you can parse replays with:

``` sh
> stack exec -- octane path/to/the.replay
```

If you need some replays to play with, check out [Rocket League Replays][].

[octane]: https://github.com/tfausak/octane
[rocket league]: http://rocketleague.psyonix.com
[stack]: http://haskellstack.org
[rocket league replays]: http://www.rocketleaguereplays.com
