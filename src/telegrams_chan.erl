-module(telegrams_chan).
-behaviour(gen_server).

-export([chan/1, push/2, subscribe/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


chan(Name) ->
    case ets:lookup(telegrams_channels, Name) of
        [{Name, Chan}] ->
            {ok, Chan};
        [] ->
            {ok, Chan} = gen_server:start_link(?MODULE, [], []),
            ets:insert(telegrams_channels, {Name, Chan}),
            {ok, Chan}
    end.

push(Chan, Event) ->
    gen_server:call(Chan, {push, Event}).

subscribe(Chan, Subscriber) ->
    gen_server:call(Chan, {subscribe, Subscriber}).


init([]) ->
    {ok, []}.

handle_call({push, Event}, _From, Subscribers) ->
    [Subscriber ! {event, Event} || Subscriber <- Subscribers],
    {reply, ok, Subscribers};
handle_call({subscribe, Subscriber}, _From, Subscribers) ->
    {reply, ok, [Subscriber|Subscribers]}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

