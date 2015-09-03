-module(dredd_irc).

%% connection operations (4.1)
-export([pass/2, nick/2, user/3, oper/3, quit/1, quit/2]).

%% channel operations (4.2)
-export([join/2, join/3, part/2, mode/3, mode/4, mode/5, mode/6]).
-export([topic/2, topic/3, names/2, list/2, list/3, invite/3, kick/3]).
-export([kick/4]).

%% API Functions
%% connection operations
pass(H, P) -> cmd(H, "PASS", P).
nick(H, N) -> cmd(H, "NICK", N).
user(H, N, FN) -> cmd(H, "USER", [N, "8", "*", ":" ++ FN]).
oper(H, U, P) -> cmd(H, "OPER", [U, P]).
quit(H) -> cmd(H, "QUIT").
quit(H, M) -> cmd(H, "QUIT", M).

%% channel operations
join(H, [CH=[_|_]|CT]) ->
    cmd(H, "JOIN", lists:foldl(fun r_comma_join/2, CH, CT));
join(H, C) -> join(H, [C]).
join(H, [CH=[_|_]|CT], [KH|KT]) ->
    cmd(H, "JOIN", lists:foldl(fun r_comma_join/2, CH, CT)
        ++ lists:foldl(fun r_comma_join/2, KH, KT));
join(H, C, K) -> join(H, [C], [K]).

part(H, [CH=[_|_]|CT]) ->
    cmd(H, "PART", lists:foldl(fun r_comma_join/2, CH, CT));
part(H, C) -> part(H, [C]).

mode(H, C, M) -> cmd(H, "MODE", [C, M]).
mode(H, C, M, L) -> cmd(H, "MODE", [C, M, L]).
mode(H, C, M, L, U) -> cmd(H, "MODE", [C, M, L, U]).
mode(H, C, M, L, U, B) -> cmd(H, "MODE", [C, M, L, U, B]).

topic(H, C) -> cmd(H, "TOPIC", C).
topic(H, C, N) -> cmd(H, "TOPIC", [C, N]).

names(H, [CH=[_|_],CT]) ->
    cmd(H, "NAMES", lists:foldl(fun r_comma_join/2, CH, CT));
names(H, C) -> names(H, [C]).

list(H, [CH=[_|_],CT]) ->
    cmd(H, "LIST", lists:foldl(fun r_comma_join/2, CH, CT));
list(H, C) -> list(H, [C]).
list(H, [CH=[_|_],CT], S) ->
    cmd(H, "LIST", [lists:foldl(fun r_comma_join/2, CH, CT),S]);
list(H, C, S) -> list(H, [C], S).

invite(H, N, C) -> cmd(H, "INVITE", [N, C]).

kick(H, C, U) -> cmd(H, "KICK", [C, U]).
kick(H, C, U, O) -> cmd(H, "KICK", [C, U, O]).

%% Helper Functions
-spec join_with(char(), string(), string()) -> string().
join_with(C, A, B) -> A ++ [C|B].

-spec r_space_join(string(), string()) -> string().
r_space_join(A, B) -> join_with($ , B, A).

-spec r_comma_join(string(), string()) -> string().
r_comma_join(A, B) -> join_with($,, B, A).

-spec cmd(string(), string()) -> string().
cmd(Host, Cmd) -> ":" ++ Host ++ " " ++ Cmd.

-spec cmd(string(), string(), [string()]|string()) -> string().
cmd(Host, Cmd, [H=[_|_]|T]) ->
    cmd(Host, Cmd) ++ " " ++ lists:foldl(fun r_space_join/2, H, T);
cmd(Host, Cmd, Arg) -> cmd(Host, Cmd, [Arg]).
