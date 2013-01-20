-module(telegrams_chan).
-behaviour(gen_server).

-export([chan/1, find/1, push/2, subscribe/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


chan(Name) ->
    case find(Name) of
        {ok, Chan} ->
            {ok, Chan};
        {error, not_found} ->
            {ok, Chan} = gen_server:start_link(?MODULE, Name, []),
            ets:insert(telegrams_channels, {Name, Chan}),
            {ok, Chan}
    end.

find(Name) ->
    case ets:lookup(telegrams_channels, Name) of
        [{Name, Chan}] ->
            {ok, Chan};
        [] ->
            {error, not_found}
    end.

push(Chan, Event) ->
    gen_server:call(Chan, {push, Event}).

forward(Chan, Event) ->
    gen_server:cast(Chan, {forward, Event}).

subscribe(Chan, Subscriber) ->
    gen_server:call(Chan, {subscribe, Subscriber}).

bind(Chan, Remote) ->
    gen_server:cast(Chan, {bind, Remote}).


init(Name) ->
    {Values, _BadNodes} = rpc:multicall(nodes(), telegrams_chan, find, [Name]),
    Remotes = [Chan || {ok, Chan} <- Values],
    [bind(Remote, self()) || Remote <- Remotes],
    {ok, {[], Remotes}}.

handle_call({push, Event}, _From, Subscribers) ->
    [Subscriber ! {event, Event} || Subscriber <- Subscribers],
    {reply, ok, Subscribers};
handle_call({subscribe, Subscriber}, _From, Subscribers) ->
    {reply, ok, [Subscriber|Subscribers]}.

handle_cast({forward, Event}, {Subscribers, Remotes}) ->
    [Subscriber ! {event, Event} || Subscriber <- Subscribers],
    {noreply, {Subscribers, Remotes}};
handle_cast({bind, Remote}, {Subscribers, Remotes}) ->
    {noreply, {Subscribers, [Remote|Remotes]}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

