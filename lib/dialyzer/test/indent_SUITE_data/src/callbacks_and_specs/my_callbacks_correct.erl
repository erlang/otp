-module(my_callbacks_correct).

-export([
	 callback_init/1
	 , callback_call/3
	 , callback_cast/3
	 , callback_exit/1
	]).

-record(state, {
	  parent           :: pid(),
	  status    = init :: 'init' | 'open' | 'closed',
	  subscribe = []   :: list({pid(), integer()}),
	  counter   = 1    :: integer()
	 }).

-type state()        :: #state{}.

-type cast_message() :: 'open' | 'closed'.

-type call_message() :: 'subscribe' | 'unsubscribe'.
-type call_reply()   :: 'accepted' | 'rejected'.

-spec callback_init(Parent::pid()) -> {'ok', state()}.

callback_init(Parent) ->
    {ok, #state{parent = Parent}}.

-spec callback_cast(state(), pid(), cast_message()) -> {'noreply', state()}.

callback_cast(#state{parent = Pid} = State, Pid, Message)
  when Message =:= 'open'; Message =:= 'close' ->
    {noreply, State#state{status = Message}};
callback_cast(State, _Pid, _Message) ->
    {noreply, State}.

-spec callback_call(state(), pid(), call_message()) ->
			   {'reply', state(), call_reply()}.

callback_call(#state{status = open, subscribe = Subscribers} = State,
	      Pid, Message)
  when Message =:= 'subscribe';
       Message =:= 'unsubscribe' ->
    NewState =
	case Message of
	    subscribe ->
		N = State#state.counter,
		State#state{subscribe = [{Pid, N}|Subscribers], counter = N+1};
	    unsubscribe ->
		State#state{subscribe = lists:keydelete(Pid, 1, Subscribers)}
	end,
    {reply, NewState, accepted};
callback_call(State, _Pid, _Message) ->
    {reply, State, rejected}.

-spec callback_exit(state()) -> 'ok'.

callback_exit(_State) ->
    ok.
