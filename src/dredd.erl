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

%% records
-record(connection, { pid               :: pid(),
                      host              :: string(),
                      port              :: pos_integer(),
                      nick              :: string(),
                      registered=false  :: boolean()
                    }).
-record(state, {connections = [] :: [{pid(), string(), integer()}]
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

init(_Args) -> {ok, #state{}}.
terminate(_Reason, _State) -> ok. % TODO: terminate children
code_change(_OldVsn, State, _Extra) -> {ok, State}.

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
    io:format("~s: ~s~n", [Conn#connection.host, Msg]),
    {noreply, State}.

send_message(#connection{registered=true, pid=Pid}, Msg) ->
    dredd_conn:send(Pid, Msg);
send_message(_, _) -> false.


register(Conn=#connection{pid=Pid, nick=Nick}) ->
    NickMsg = io_lib:format("NICK ~s~n", [Nick]),
    dredd_conn:send(Pid, NickMsg),
    UserMsg = io_lib:format("USER ~s 8 * : Dredd Bot~n", [Nick]),
    dredd_conn:send(Pid, UserMsg),
    Conn#connection{registered=true}.
