-module(telegrams).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    net_adm:world(),
    {ok, Port} = application:get_env(telegrams, port),
    ets:new(telegrams_channels, [set, public, named_table]),
    Dispatch = [{'_', [{['...'], telegrams_api, []}]}],
    cowboy:start_http(telegrams_listener, 100,
                      [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    telegrams_sup:start_link().

stop(_State) ->
    ok.
