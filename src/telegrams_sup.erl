
-module(telegrams_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
     RestartStrategy = {simple_one_for_one, 10, 10},
     ChildSpec = {channel, {gen_server, start_link, [telegrams_chan]},
          permanent, brutal_kill, worker, [telegrams_chan]},
     {ok, {RestartStrategy, [ChildSpec]}}.

