-module(telegrams_chan).
-behaviour(gen_server).

-export([chan/1, find/1]).
-export([push/2, forward/2]).
-export([subscribe/2, unsubscribe/2, bind/2, unbind/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


chan(Name) ->
    case find(Name) of
        {ok, Chan} ->
            {ok, Chan};
        {error, not_found} ->
            case supervisor:start_child(telegrams_sup, [Name, []]) of
                {ok, undefined} ->
                    find(Name);
                {ok, Chan} ->
                    {ok, Chan}
            end
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

unsubscribe(Chan, Subscriber) ->
    gen_server:call(Chan, {unsubscribe, Subscriber}).

bind(Chan, Remote) ->
    gen_server:cast(Chan, {bind, Remote}).

unbind(Chan, Remote) ->
    gen_server:cast(Chan, {unbind, Remote}).


init(Name) ->
    process_flag(trap_exit, true),
    case find(Name) of
        {ok, Chan} ->
            ignore;
        _Else ->
            ets:insert(telegrams_channels, {Name, self()}),
            Remotes = remotes(Name),
            [bind(Remote, self()) || Remote <- Remotes],
            {ok, {Name, [], Remotes}}
    end.

handle_call({push, Event}, _From, {Name, Subscribers, Remotes}) ->
    [forward(Chan, Event) || Chan <- Remotes],
    [Subscriber ! {event, Event} || Subscriber <- Subscribers],
    {reply, ok, {Name, Subscribers, Remotes}};
handle_call({subscribe, Subscriber}, _From, {Name, Subscribers, Remotes}) ->
    {reply, ok, {Name, [Subscriber|Subscribers], Remotes}};
handle_call({unsubscribe, Subscriber}, _From, {Name, Subscribers, Remotes}) ->
    {reply, ok, {Name, lists:delete(Subscriber, Subscribers), Remotes}}.

handle_cast({forward, Event}, {Name, Subscribers, Remotes}) ->
    [Subscriber ! {event, Event} || Subscriber <- Subscribers],
    {noreply, {Name, Subscribers, Remotes}};
handle_cast({bind, Remote}, {Name, Subscribers, Remotes}) ->
    {noreply, {Name, Subscribers, [Remote|Remotes]}};
handle_cast({unbind, Remote}, {Name, Subscribers, Remotes}) ->
    {noreply, {Name, Subscribers, lists:delete(Remote, Remotes)}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Subscriber, _Reason}, {Name, Subscribers, Remotes}) ->
    {noreply, {Name, lists:delete(Subscriber, Subscribers), Remotes}};
handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, {Name, _Subscribers, Remotes}) ->
    ets:delete(telegrams_channels, Name),
    [unbind(Remote, self()) || Remote <- Remotes],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

remotes(Name) ->
    {Values, _BadNodes} = rpc:multicall(nodes(), telegrams_chan, find, [Name]),
    [Chan || {ok, Chan} <- Values].

