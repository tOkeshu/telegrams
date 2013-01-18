-module(telegrams).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = [{'_', [{['...'], telegrams_api, []}]}],
    cowboy:start_http(telegrams_listener, 100,
                      [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    telegrams_sup:start_link().

stop(_State) ->
    ok.
