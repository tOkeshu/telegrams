-module(telegrams_api).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = dispatch(Req),
    {ok, Req2, State}.

dispatch(Req) ->
    {Channel, Req2} = cowboy_req:path(Req),
    case cowboy_req:method(Req2) of
        {<<"GET">>, Req2} ->
            subscribe(Channel, Req2);
        {<<"POST">>, Req2} ->
            publish(Channel, Req2)
    end.

subscribe(_Channel, Req) ->
    {ok, Req}.

publish(_Channel, Req) ->
    {ok, Req}.

terminate(_Req, _State) ->
    ok.
