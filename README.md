# dredd
Dredd IRC Bot

Usage
-----
dredd is still in heavy development an is missing lots of useful features.
However you can get some basic functionality by connecting to a server and
using the send command to join a channel.

    > application:start(dredd).
    > dredd:connect("irc.freenode.net").
    > dredd:send("irc.freenode.net", "JOIN #erlang").
