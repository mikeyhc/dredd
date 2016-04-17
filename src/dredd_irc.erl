-module(dredd_irc).

%% connection operations (4.1)
-export([pass/1, nick/1, nick/2, user/3, oper/3, quit/1, quit/2]).

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

%% response parsing
-export([parse_response/1, pretty/1, is_error/1, is_reply/1]).
-export([is_message/1, is_ping/1, is_general/1]).

%% message getters
-export([message_name/1, message_channel/1, message_text/1]).

%% general getters
-export([general_name/1, general_command/1, general_message/1]).

%% ping getters
-export([ping_name/1, ping_message/1]).

%% reply getters
-export([reply_name/1, reply_code/1, reply_target/1, reply_parts/1]).

%% types
-export_type([irc_error/0, irc_reply/0, irc_message/0, irc_ping/0]).
-export_type([irc_general/0, irc_unknown/0]).

-type irc_error_code() :: 401..407 | 409 | 411..414 | 421..424
                        | 431..433 | 436 | 441..446 | 451 | 461..465
                        | 467 | 471..475 | 481..483 | 491 | 501 | 502.

-type irc_code() :: 200..206 | 208 | 211..216 | 218 | 219 | 221
                  | 241..244 | 251..259 | 261 | 300..303 | 305 | 306
                  | 311..315 | 317..319 | 321..324 | 331 | 332 | 341
                  | 342 | 351..353 | 364..369 | 371 | 372 | 374..376
                  | 381 | 382 | 391..395.

-type irc_name() :: undefined | string().

-record(irc_error, {name = undefined :: irc_name(),
                    code             :: irc_error_code(),
                    target           :: string(),
                    parts            :: [string()]
                   }).
-opaque irc_error() :: #irc_error{}.

-record(irc_reply, {name   :: irc_name(),
                    code   :: irc_code(),
                    target :: string(),
                    parts  :: [string()]
                   }).
-opaque irc_reply() :: #irc_reply{}.

-record(irc_message, {name    :: irc_name(),
                      channel :: string(),
                      text    :: string()
                     }).
-opaque irc_message() :: #irc_message{}.

-record(irc_ping, {name    :: string(),
                   message :: string()}).
-opaque irc_ping() :: #irc_ping{}.

-record(irc_general, {name    :: string(),
                      command :: string(),
                      message :: string()}).
-opaque irc_general() :: #irc_general{}.

-record(irc_unknown, {name  :: string(),
                      parts :: [string()]}).
-opaque irc_unknown() :: #irc_unknown{}.

-type irc_response() :: irc_error() | irc_reply() | irc_message()
                      | irc_ping() | irc_general() | irc_unknown().

%% API Functions
%% connection operations
pass(P) -> "PASS " ++ P.
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

%% response parsing
-spec parse_response(string()) -> irc_response().
parse_response([$:|Resp]) ->        % contains name
    [Name|Tokens] = string:tokens(Resp, " "),       %% TODO: smarter split
    add_name(parse_tokens(Tokens), Name);
parse_response(Resp) -> parse_tokens(string:tokens(Resp, " ")).

parse_tokens([Num=[NH|_],T|P]) when NH >= $4 andalso NH =< $5 ->
    {Code, Rest} = string:to_integer(Num),
    case Rest of
        [] -> ok;
        _  -> io:format("warning: remaining string ~w", [Rest])
    end,
    #irc_error{ code   = Code,
                target = T,
                parts  = P
              };
parse_tokens([Num=[NH|_],T|P]) when NH >= $2 andalso NH =< $3 ->
    {Code, Rest} = string:to_integer(Num),
    case Rest of
        [] -> ok;
        _  -> io:format("warning: remaining string ~w", [Rest])
    end,
    #irc_reply{ code   = Code,
                target = T,
                parts  = P
              };
parse_tokens(["PING",Msg]) -> #irc_ping{message = Msg};
parse_tokens(["PRIVMSG",C,[$:|H]|T]) ->
    #irc_message{ channel = C,
                  text    = string:join([H|T], " ")
                };
parse_tokens(["JOIN"|Parts]) ->
    #irc_general{ command = "JOIN",
                  message = string:join(Parts, " ")
                };
parse_tokens(["NICK"|Parts]) ->
    #irc_general{ command = "NICK",
                  message = string:join(Parts, " ")
                };
parse_tokens(Parts) -> #irc_unknown{parts = Parts}.

pretty(#irc_error{name=N, code=C, target=T, parts=P}) ->
    io_lib:format("~s: ERROR(~w:~s) ~s", [N, C, T, string:join(P, " ")]);
pretty(#irc_reply{name=N, code=C, target=T, parts=P}) ->
    io_lib:format("~s: REPLY(~w:~s) ~s", [N, C, T, string:join(P, " ")]);
pretty(#irc_message{name=N, channel=C, text=M}) ->
    io_lib:format("~s: (~s) ~s", [N, C, M]);
pretty(#irc_ping{name=N, message=M}) -> io_lib:format("~s: PING ~s", [N, M]);
pretty(#irc_general{name=N, command=C, message=M}) ->
    io_lib:format("~s: (~s) ~s", [N, C, M]);
pretty(#irc_unknown{name=N, parts=P}) ->
    io_lib:format("~s: UNKNOWN ~s", [N, string:join(P, " ")]).

is_error(#irc_error{}) -> true;
is_error(_) -> false.

is_reply(#irc_reply{}) -> true;
is_reply(_) -> false.

is_message(#irc_message{}) -> true;
is_message(_) -> false.

is_ping(#irc_ping{}) -> true;
is_ping(_) -> false.

is_general(#irc_general{}) -> true;
is_general(_) -> false.

%% message getters
message_name(#irc_message{name=N}) -> N.
message_channel(#irc_message{channel=C}) -> C.
message_text(#irc_message{text=T}) -> T.

%% general getters
general_name(#irc_general{name=N}) -> N.
general_command(#irc_general{command=C}) -> C.
general_message(#irc_general{message=M}) -> M.

%% ping getters
ping_name(#irc_ping{name=N}) -> N.
ping_message(#irc_ping{message=M}) -> M.

%% reply getters
reply_name(#irc_reply{name=N}) -> N.
reply_code(#irc_reply{code=C}) -> C.
reply_target(#irc_reply{target=T}) -> T.
reply_parts(#irc_reply{parts=P}) -> P.

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

-spec add_name(irc_response(), string()) -> irc_response().
add_name(E=#irc_error{}, N) -> E#irc_error{name = N};
add_name(R=#irc_reply{}, N) -> R#irc_reply{name = N};
add_name(M=#irc_message{}, N) -> M#irc_message{name = N};
add_name(P=#irc_ping{}, N) -> P#irc_ping{name = N};
add_name(G=#irc_general{}, N) -> G#irc_general{name = N};
add_name(U=#irc_unknown{}, N) -> U#irc_unknown{name = N}.
