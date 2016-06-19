# Contributing

-   [Issues](#issues)
-   [Pull requests](#pull-requests)

## Issues

If Octane fails to parse a Rocket League replay, please report it! Include the
replay file itself, either attached to the issue or uploaded somewhere else
like [Rocket League Replays][]. Also please include the version of Octane that
you are using if you know it.

## Pull requests

Octane is written in Haskell. If you want to make changes to Octane, the
recommended tool is [Stack][]. Once you have that installed, you should be able
to get up and running with `stack setup` and then `stack build`. When you are
happy with your changes, open a pull request. AppVeyor will build it on Windows
and Travis CI will build it on Linux and macOS.

[Rocket League Replays]: https://www.rocketleaguereplays.com/replays/upload/
[Stack]: http://docs.haskellstack.org/en/stable/README/
