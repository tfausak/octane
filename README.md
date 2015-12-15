# [Octane][]

Octane parses Rocket League replays.

I have not figured out the replay file format yet. I can parse the entire thing
into discrete sections, but I do not yet know the meaning of each section.

-   The replay file is little-endian.

-   String are both length prefixed and null terminated.

-   Integers and floats are mostly 32 bits.

-   There are some 64-bit integers.

-   Each byte in the frame section is reversed.

-   Lists are length prefixed.

-   Dictionaries are keyed on strings and stop when a key is "None".

Here is an overview of what I know:

1.  The first 4 bytes are an integral size of some kind. This is usually a
    small number (less than 4,000 or so). This is equal to the byte size of the
    properties section plus 36.

2.  The next 4 bytes are a CRC.

3.  The next 8 bytes are some version numbers. There are two 4-byte integers.
    For every replay I can get my hands on, they are always 867 and 9.

4.  The next 28 bytes are a string identifier. It is always
    "TAGame.Replay_Soccar_TA".

5.  Then the property section happens. It's basically a dictionary. You parse
    string keys and property values. You keep reading properties until a key is
    "None". The properties have a kind (a string) and a size (a 64-bit
    integer). Not every property uses the size. The parsing for each property
    type is different, but usually they read a string or a number. Array
    properties contain a list of dictionaries. Lists are length-prefixed.

    Almost every replay has the same properties. The "PrimaryPlayerTeam"
    property will either be missing, signifying the blue team, or present with
    the value 1 for the orange team. Both "Team0Score" and "Team1Score" are
    only present if they're greater than 0.

6.  After the properties there is another 32-bit size integer. This is usually
    a large number (greater than 40,000 or so). This number plus the number
    from step one plus 16 equals the file size in bytes.

7.  Then there is a 4-byte separator. I have not discovered the meaning of this
    part yet.

8.  Then there is a list of levels to load. This is a list of strings. It
    always has one element.

9.  Then there are the key frames. Each one has a time since the beginning of
    the match as a 32-bit float. Then they have a 32-bit integer that
    corresponds to a frame. Finally they have a 32-bit integer of the bit
    position in the frames section.

10. The actual frames come next. These are prefixed by a 32-bit integer of the
    size of the frames section in bytes. The frames themselves are not byte
    aligned.

11. After the frames are a list of debug messages. These only exist in older
    replays.

12. Then there's a list of all the tick marks in the replay. These correspond
    to goals. (Maybe in the future they will mean something else.) They have a
    string label (like "Team0Goal") and a integral frame number.

13. Then there is a list of packages. This is a list of strings. 9 of these are
    always the same. 1 is either "GameInfo_Soccar_SF" or "GameInfo_Season_SF".
    2 more are for the map; one is the map itself and the other is the sound
    effects for the map.

14. Objects. List of strings.

15. Names. List of strings. Always empty.

16. Actors. List of strings.

17. Cache. List of cache items, which are 3 ints and a list of cache
    properties. Each cache property is 2 ints.

[octane]: https://github.com/tfausak/octane
