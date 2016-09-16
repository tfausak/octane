# [Octane][]

[![Version badge][]][version]
[![Windows build badge][]][windows build]
[![Build badge][]][build]

Octane is the premier [Rocket League][] replay parser. [Rocket League
Replays][] parses tens of thousands of replays with it. Octane parses most
replays in less than 5 seconds. It outputs easy-to-read JSON.

Octane has a command-line interface. To get it, download and unpack [the latest
release][] for your platform. You can run the executable one of three ways:

1.  Pipe a replay file into it. It will output a compact JSON object to
    standard out.

    ``` sh
    $ octane < a.replay > replay.json
    ```

2.  Pass it a path to a replay file. Both file paths and URLs work. It will
    output a compact JSON object to standard out.

    ``` sh
    $ octane a.replay > replay.json
    $ octane https://media.rocketleaguereplays.com/uploads/replay_files/9A06783F4FEA7AFF3D8298A3E5A412F5.replay > replay.json
    ```

3.  Pass it several paths to replay files. Both file paths and URLs work. It
    will output a compact JSON array of objects to standard out.

    ``` sh
    $ octane first.replay second.replay > replays.json
    ```

Rocket League saves your replays in a folder that depends on your operating
system.

- Windows: `%UserProfile%\Documents\My Games\Rocket League\TAGame\Demos`
- macOS: `$HOME/Library/Application Support/Rocket League/TAGame/Demos`
- Linux: `$HOME/.local/share/Rocket League/TAGame/Demos`

Octane is written in Haskell. If you're looking for a library written in
another language, check out the [Rocket League Replays wiki][]. It has links to
many other Rocket League replay parsers.

[Octane]: https://github.com/tfausak/octane
[Version badge]: https://www.stackage.org/package/octane/badge/nightly?label=version
[version]: https://www.stackage.org/nightly/package/octane
[Windows build badge]: https://ci.appveyor.com/api/projects/status/github/tfausak/octane?branch=main&svg=true
[windows build]: https://ci.appveyor.com/project/TaylorFausak/octane
[Build badge]: https://travis-ci.org/tfausak/octane.svg?branch=main
[build]: https://travis-ci.org/tfausak/octane
[Rocket League]: http://www.rocketleaguegame.com
[Rocket League Replays]: https://www.rocketleaguereplays.com/replays/
[the latest release]: https://github.com/tfausak/octane/releases/latest
[Rocket League Replays wiki]: https://github.com/rocket-league-replays/rocket-league-replays/wiki/Rocket-League-Replay-Parsers
