-module(dredd_irc).

%% connection operations (4.1)
-export([pass/2, nick/1, nick/2, user/3, oper/3, quit/1, quit/2]).

%% channel operations (4.2)
-export([join/2, join/3, part/2, mode/3, mode/4, mode/5, mode/6]).
-export([topic/2, topic/3, names/2, list/2, list/3, invite/3, kick/3]).
-export([kick/4]).

%% server queries and commands (4.3)
-export([version/1, version/2, stats/1, stats/2, stats/3, links/1]).
-export([links/2, links/3, time/1, time/2, connect/2, connect/3, connect/4]).
-export([trace/1, trace/2, admin/1, admin/2, info/1, info/2]).

%% sending messages (4.4)
-export([privmsg/3, notice/3]).

%% user based queries (4.5)
-export([who/1, who/2, who/3, whois/2, whois/3, whowas/2, whowas/3]).
-export([whowas/4]).

%% miscellaneous messages (4.6)
-export([kill/3, pong/2, pong/3]).

%% optionals (5)
-export([away/1, away/2, rehash/1, restart/1, summon/2, summon/3]).
-export([users/1, users/2, userhost/2, ison/2]).

-type irc_error_code() :: 401..407 | 409 | 411..414 | 421..424
                        | 431..433 | 436 | 441..446 | 451 | 461..465
                        | 467 | 471..475 | 481..483 | 491 | 501 | 502.
-type irc_code() :: 200..206 | 208 | 211..216 | 218 | 219 | 221
                  | 241..244 | 251..259 | 261 | 300..303 | 305 | 306
                  | 311..315 | 317..319 | 321..324 | 331 | 332 | 341
                  | 342 | 351..353 | 364..369 | 371 | 372 | 374..376
                  | 381 | 382 | 391..395.

-record(irc_error, {code :: irc_error_code(),
                    text :: string()
                   }).

-record(irc_reply, {code :: irc_code(),
                    text :: string()
                   }).

-record(irc_message, {text :: string()}).

%% API Functions
%% connection operations
pass(H, P) -> cmd(H, "PASS", P).
nick(N) -> "NICK " ++ N.
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

names(H, [CH=[_|_]|CT]) ->
    cmd(H, "NAMES", lists:foldl(fun r_comma_join/2, CH, CT));
names(H, C) -> names(H, [C]).

list(H, [CH=[_|_]|CT]) ->
    cmd(H, "LIST", lists:foldl(fun r_comma_join/2, CH, CT));
list(H, C) -> list(H, [C]).
list(H, [CH=[_|_]|CT], S) ->
    cmd(H, "LIST", [lists:foldl(fun r_comma_join/2, CH, CT),S]);
list(H, C, S) -> list(H, [C], S).

invite(H, N, C) -> cmd(H, "INVITE", [N, C]).

kick(H, C, U) -> cmd(H, "KICK", [C, U]).
kick(H, C, U, O) -> cmd(H, "KICK", [C, U, O]).

%% server queries and commands
version(H) -> cmd(H, "VERSION").
version(H, S) -> cmd(H, "VERSION", S).

stats(H) -> cmd(H, "STATS").
stats(H, Q) -> cmd(H, "STATS", Q).
stats(H, Q, S) -> cmd(H, "STATS", [Q, S]).

links(H) -> cmd(H, "LINKS").
links(H, M) -> cmd(H, "LINKS", M).
links(H, R, M) -> cmd(H, "LINKS", [R, M]).

time(H) -> cmd(H, "TIME").
time(H, S) -> cmd(H, "TIME", S).

connect(H, S) -> cmd(H, "CONNECT", S).
connect(H, S, P) -> cmd(H, "CONNECT", [S, P]).
connect(H, S, P, R) -> cmd(H, "CONNECT", [S, P, R]).

trace(H) -> cmd(H, "TRACE").
trace(H, S) -> cmd(H, "TRACE", S).

admin(H) -> cmd(H, "ADMIN").
admin(H, S) -> cmd(H, "ADMIN", S).

info(H) -> cmd(H, "INFO").
info(H, S) -> cmd(H, "INFO", S).

%% sending messages
privmsg(H, [RH=[_|_]|RT], M) ->
    cmd(H, "PRIVMSG", [lists:foldl(fun r_comma_join/2, RH, RT),":" ++ M]);
privmsg(H, R, M) -> privmsg(H, [R], M).

notice(H, N, M) -> cmd(H, "NOTICE", [N, ":" ++ M]).

%% user based queries
who(H) -> cmd(H, "WHO").
who(H, N) -> cmd(H, "WHO", N).
who(H, N, true) -> cmd(H, "WHO", [N, "o"]);
who(H, N, false) -> cmd(H, "WHO", N).

whois(H, [MH=[_|_]|MT]) ->
    cmd(H, "WHOIS", lists:foldl(fun r_comma_join/2, MH, MT));
whois(H, M) -> whois(H, [M]).
whois(H, S, [MH=[_|_]|MT]) ->
    cmd(H, "WHOIS", [S, lists:foldl(fun r_comma_join/2, MH, MT)]);
whois(H, S, M) -> whois(H, S, [M]).

whowas(H, N) -> cmd(H, "WHOWAS", N).
whowas(H, N, C) -> cmd(H, "WHOWAS", [N, C]).
whowas(H, N, C, S) -> cmd(H, "WHOWAS", [N, C, S]).

%% Miscellaneous messages
kill(H, N, C) -> cmd(H, "KILL", [N, C]).

pong(H, D) -> cmd(H, "PONG", D).
pong(H, D1, D2) -> cmd(H, "PONG", [D1, D2]).

%% Optionals
away(H) -> cmd(H, "AWAY").
away(H, M) -> cmd(H, "AWAY", ":" ++ M).

rehash(H) -> cmd(H, "REHASH").

restart(H) -> cmd(H, "RESTART").

summon(H, U) -> cmd(H, "SUMMON", U).
summon(H, U, S) -> cmd(H, "SUMMON", [U, S]).

users(H) -> cmd(H, "USERS").
users(H, S) -> cmd(H, "USERS", S).

userhost(H, [NH=[_|_]|NT]) ->
    cmd(H, "USERHOST", lists:foldl(fun r_space_join/2, NH, NT));
userhost(H, N) -> userhost(H, [N]).

ison(H, [NH=[_|_]|NT]) ->
    cmd(H, "ISON", lists:foldl(fun r_space_join/2, NH, NT));
ison(H, N) -> ison(H, [N]).


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
