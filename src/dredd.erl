-module(dredd).

-behaviour(gen_server).

%% API
-export([start_link/0, connect/1, connect/2, connect/3, message/1]).
-export([message/2, send/2]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% default values
-define(NICK, "dredd").

% types
% the timestamp() type is copied from the erlang module
% see: http://www.erlang.org/doc/man/erlang.html
-type timestamp() :: {pos_integer(), pos_integer(), pos_integer()}.

%% records
-record(connection, { pid               :: pid(),
                      host              :: string(),
                      port              :: pos_integer(),
                      nick              :: string(),
                      registered=false  :: boolean()
                    }).
-record(state, {connections = [] :: [{pid(), string(), integer()}],
                plugins     = [] :: [{pid(), string()}],
                start_time       :: timestamp()
               }).

%% API

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect(Host) -> connect(Host, 6667, ?NICK).
connect(Host, Port) -> connect(Host, Port, ?NICK).
connect(Host, Port, Nick) ->
    gen_server:cast(dredd, {connect, Host, Port, Nick}).

message(Msg) -> gen_server:cast(?MODULE, {message, Msg}).
message(Pid, Msg) -> gen_server:cast(Pid, {message, self(), Msg}).

send(Host, Msg) -> gen_server:cast(?MODULE, {send, Host, Msg}).

%% gen_server callbacks

init(_Args) ->
    {ok, #state{start_time = erlang:now()}}.
terminate(_Reason, _State) -> ok. % TODO: terminate children

code_change(OldVsn, State, _Extra) ->
    io:format("upgrading from version ~w~n", [OldVsn]),
    {ok, State}.

handle_call(_Message, _From, State) -> {noreply, State}.

handle_cast({connect, Host, Port, Nick}, State) ->
    {ok, Pid} = dredd_conn_sup:start_child(Host, Port),
    Conn = #connection{pid=Pid, host=Host, port=Port, nick=Nick},
    {noreply, State#state{connections=[Conn|State#state.connections]}};
handle_cast({message, Pid, Msg}, State) ->
    handle_message(lists:keyfind(Pid, 2, State#state.connections), Msg, State);
handle_cast({send, Host, Msg}, State) ->
    send_message(lists:keyfind(Host, 3, State#state.connections), Msg),
    {noreply, State}.

handle_info(_Message, State) -> {noreply, State}.

%% helper functions

handle_message(false, _, State) -> {noreply, State};
handle_message(Conn=#connection{registered=false, pid=Pid}, Msg, State) ->
    NewConn = register(Conn),
    NewConns = lists:keyreplace(Pid, 2, State#state.connections, NewConn),
    handle_message(NewConn, Msg, State#state{connections=NewConns});
handle_message(Conn, Msg, State) ->
    io:format("main thread got ~s~n", [dredd_irc:pretty(Msg)]),
    case dredd_irc:is_message(Msg) of
        true  -> handle_privmsg(Conn, Msg, State);
        false -> ok
    end,
    {noreply, State}.

send_message(#connection{registered=true, pid=Pid}, Msg) ->
    dredd_conn:send(Pid, Msg);
send_message(_, _) -> false.

handle_privmsg(Conn, Msg, State) ->
    % FIXME: just a simple test
    Chan = dredd_irc:message_channel(Msg),
    Text = dredd_irc:message_text(Msg),
    handle_privmsg(Conn, Chan, Text, Msg, State).

handle_privmsg(Conn, Chan, _Text, Mesg, State)
  when Conn#connection.nick =:= Chan ->
    [SendChan|_] = string:tokens(dredd_irc:message_name(Mesg), "!"),
    io:format("~s", [SendChan]),
    i_am_the_law(Conn, SendChan),
    {noreply, State};
handle_privmsg(Conn, Chan, "dredd" ++ _Text, _Mesg, State) ->
    i_am_the_law(Conn, Chan),
    {noreply, State};
handle_privmsg(_, _, _, _, State) -> {noreply, State}.

i_am_the_law(Conn, Chan) ->
    send_message(Conn, dredd_irc:privmsg("dredd", Chan, "I AM THE LAW")).

register(Conn=#connection{pid=Pid, nick=Nick}) ->
    NickMsg = io_lib:format("NICK ~s~n", [Nick]),
    dredd_conn:send(Pid, NickMsg),
    UserMsg = io_lib:format("USER ~s 8 * : Dredd Bot~n", [Nick]),
    dredd_conn:send(Pid, UserMsg),
    Conn#connection{registered=true}.
