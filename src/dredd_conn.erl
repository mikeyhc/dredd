-module(dredd_conn).

-define(TIMEOUT, 10000).

%% API
-export([start_link/3, register/2, send/2, status/1]).

%% Helper Functions
-export([loop/1, message_loop/1, tcp_loop/1]).

%% API functions

start_link(Host, Port, Parent) ->
    Pid = spawn_link(fun() -> loop({not_connected, Parent}) end),
    Pid ! {connect, Host, Port},
    {ok, Pid}.

% register a Pid to get a message whenerver Target (a dredd_conn)
% receives one
register(Target, Pid) ->
    Target ! {self(), register, Pid},
    receive
        {register, Status, Pid} -> Status
    after ?TIMEOUT ->
        timeout
    end.

% send a message using Target dredd_conn
send(Target, Message) ->
    case lists:reverse(Message) of
        [10|_] -> Target ! {send, Message};
        _      -> Target ! {send, Message ++ "\n"}
    end.

% return the status of Target dredd_conn
status(Target) ->
    Target ! {self(), status},
    receive
        {status, Status} -> Status
    after ?TIMEOUT ->
        timeout
    end.

%% Helper functions

% The main loop, if not_connect accept status, connect or register
% messages
loop(State={not_connected, Parent}) ->
    receive
        {connect, Host, Port} ->
            Opts = [ {active, false}, {packet, raw} ],
            case gen_tcp:connect(Host, Port, Opts, 3000) of
                {ok, Sock} ->
                    dredd_conn:loop({connected, Host, Port, Sock, Parent,
                                     [Parent]});
                {error, Err} ->
                    dredd_conn:loop({conn_error, Err, Parent})
            end;
        {Pid, register, TPid} ->
            Pid ! {register, not_connected, TPid},
            dredd_conn:loop(State);
        {Pid, status} ->
            Pid ! {status, not_connected},
            dredd_conn:loop(State);
        _ -> dredd_conn:loop(State)
    end;
% if a connect error has occured accept status, register or clear_error
% messages
loop(State={conn_error, _, Parent}) ->
    receive
        {Pid, status} ->
            Pid ! {status, State},
            dredd_conn:loop(State);
        {Pid, register, TPid} ->
            Pid ! {register, conn_error, TPid},
            dredd_conn:loop(State);
        clear_error -> dredd_conn:loop({not_connected, Parent});
        _ -> dredd_conn:loop(State)
    end;
% if connected accept the status, send or register messages
loop(State={connected, _, _, _, _, _}) ->
    NewState = message_loop(State),
    tcp_loop(NewState),
    dredd_conn:loop(NewState).

message_loop(State={connected, Host, Port, Sock, Parent, Pids}) ->
    receive
        {Pid, status} ->
            Pid ! {status, {connected, Host, Port}},
            dredd_conn:message_loop(State);
        {send, Message} ->
            io:format("> ~s", [Message]),
            gen_tcp:send(Sock, Message),
            dredd_conn:message_loop(State);
        {InPid, register, Pid} ->
            InPid ! {register, registered, Pid},
            dredd_conn:message_loop({connected, Host, Port, Sock,
                                     Parent, [Pid|Pids]})
    after 250 -> State end.

tcp_loop(State={_, Host, _, Sock, _, Pids}) ->
    case gen_tcp:recv(Sock, 0, 250) of
        {ok, Packet} ->
            AllMsgs = process_packet(Packet),
            SendPings = fun(X, Acc) ->
                                case dredd_irc:is_ping(X) of
                                    true  -> ping_reply(Host, X), Acc;
                                    false -> [X|Acc]
                                end
                        end,
            Msgs = lists:reverse(lists:foldl(SendPings, [], AllMsgs)),
            SFun =  fun(X) ->
                            Send = fun(Y) -> dredd:message(X, Y) end,
                            lists:foreach(Send, Msgs)
                    end,
            lists:foreach(SFun, Pids),
            tcp_loop(State);
        {error, timeout} -> ok;
        {error, Err} -> throw({socket_read_error, Err})
    end.

% split a packet on newlines
process_packet(Packet) ->
    L = lists:foldl(fun(X, Acc) when X =:= 10 -> [[]|Acc];
                       (X, [H|T]) -> [[X|H]|T]
                    end, [[]], Packet),
    lists:map(fun(X) ->
                      io:format("< ~s~n", [X]),
                      dredd_irc:parse_response(X)
              end,
              lists:filter(fun(X) -> X =/= [] end,
                           lists:map(fun lists:reverse/1, L))).

%% reply to a ping
ping_reply(Host, P) ->
    send(self(), dredd_irc:pong(Host, dredd_irc:ping_message(P))).
