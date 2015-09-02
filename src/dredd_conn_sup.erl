-module(dredd_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Host, Port) ->
    supervisor:start_child(?MODULE, [Host, Port, self()]).

%%  Supervisor callbacks

init([]) ->
    {ok, { {simple_one_for_one, 1, 120},
           [{connections,
             {dredd_conn, start_link, []},
             permanent, 1000, worker, [dredd_conn]}
           ]}}.
