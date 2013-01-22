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

subscribe(Channel) ->
    {ok, Chan} = telegrams_chan:chan(Channel),
    link(Chan),
    telegrams_chan:subscribe(Chan, self()).
subscribe(Channel, Req) ->
    process_flag(trap_exit, true),
    ok = subscribe(Channel),
    {ok, Req2} = cowboy_req:chunked_reply(200, Req),
    stream(Channel, Req2).

stream(Channel, Req) ->
    receive
        {event, Event} ->
            ok = cowboy_req:chunk(Event, Req),
            stream(Channel, Req);
        {'EXIT', _Chan, _Reason} ->
            subscribe(Channel),
            stream(Channel, Req);
        _ ->
            stream(Channel, Req)
    end.

publish(Channel, Req) ->
    {ok, Chan} = telegrams_chan:chan(Channel),
    {ok, Payload, Req2} = cowboy_req:body(Req),
    ok = telegrams_chan:push(Chan, Payload),
    cowboy_req:reply(200, [], <<>>, Req2).

terminate(_Req, _State) ->
    ok.
