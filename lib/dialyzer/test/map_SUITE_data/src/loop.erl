-module(loop).

-export([timeout/2]).

-export([idle/2, waiting/2]).

-type request_category() :: category1 | category2.

-type counter() :: counter1 | 2.

-type counters() :: #{counter() => non_neg_integer()}.

-record(queue, {limit  = 0  :: non_neg_integer(),
                buffer = [] :: list()}).

-type request_queues() :: #{request_category() => #queue{}}.

-record(?MODULE,
        {state    = idle                      :: idle | waiting,
         timer    = undefined                 :: undefined | timer:tref(),
         queues   = #{category1 => #queue{},
                      category2 => #queue{}}  :: request_queues(),
         counters = new_counters()            :: counters()}).
-spec timeout(Ref, Timer :: timer:tref()) -> {noreply, Ref}.
timeout(Ref, Timer) ->
    handle_message(Ref, {timeout, Timer}).

-type message() :: {reset, request_category()}
                 | {timeout, timer:tref()}.

-spec handle_message(Ref, Message :: message()) ->
                            {reply, boolean(), Ref} | {noreply, Ref}.
handle_message(Ref, Msg) ->
    MV = #?MODULE{state = State} = get(mv),
    case apply(?MODULE, State, [Msg, MV]) of
        {reply, Result, NewMV} ->
            put(mv, NewMV),
            {reply, Result, Ref};
        {noreply, NewMV} ->
            put(mv, NewMV),
            {noreply, Ref}
    end.

-spec idle(Message :: message(), #?MODULE{}) ->
                  {reply, boolean(), #?MODULE{}} | {noreply, #?MODULE{}}.
idle({reset, Category}, MV = #?MODULE{queues = Queues}) ->
    case Queues of
        #{Category := #queue{limit = 0}} ->
            {reply, false, MV};
        _ ->
            wait(MV)
    end;
idle(_, MV) ->
    {noreply, MV}.

-spec waiting(Message :: message(), #?MODULE{}) ->
                     {reply, boolean(), #?MODULE{}} | {noreply, #?MODULE{}}.
waiting({reset, _Category}, MV = #?MODULE{}) ->
    NewMV = stop_timer(MV),
    {noreply, NewMV#?MODULE{state  = idle}};
waiting({timeout, Timer}, #?MODULE{timer = Timer} = MV) ->
    %% The opaque warning is an effect of the call to timer:send_after().
    {noreply, start_timer(MV#?MODULE{timer    = undefined,
                                     counters = new_counters()})}.

-spec wait(#?MODULE{}) -> {noreply, #?MODULE{}}.
wait(MV) ->
    {noreply, start_timer(MV#?MODULE{state = waiting})}.

-spec stop_timer(#?MODULE{}) -> #?MODULE{}.
stop_timer(MV) ->
    case MV#?MODULE.timer of
        undefined ->
            MV;
        Timer ->
            timer:cancel(Timer),
            MV#?MODULE{timer = undefined}
    end.

-spec start_timer(MV :: #?MODULE{}) -> #?MODULE{}.
start_timer(MV) ->
    case MV#?MODULE.timer of
        undefined ->
            %% Note: timer:send_after() returns {ok, TRef} | {error, _}.
            MV#?MODULE{timer = timer:send_after(1000, ?MODULE)};
        _Timer ->
            start_timer(stop_timer(MV))
    end.

-spec new_counters() -> counters().
new_counters() ->
    #{counter1 => 10, 2 => 10}.
