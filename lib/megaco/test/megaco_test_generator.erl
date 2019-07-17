%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% Purpose: Sequence generator for the megaco test suite
%%----------------------------------------------------------------------

-module(megaco_test_generator).

-behaviour(gen_server).

-compile({no_auto_import,[error/2]}).

%% ----

-export([
	 start_link/3, 
	 start_link/4, 
	 exec/2, exec/3, 
	 stop/1
	]).

%% Misc utility function for modules implementing this behaviour
-export([
	 sleep/1, 
	 sz/1, 
	 debug/1, debug/2, 
	 error/2, 
	 print/3, print/4
	]).


%% Internal exports
-export([start/4]).
-export([handler_init/5]).

%% Internal gen_server exports
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3
	]).


-include_lib("megaco/include/megaco.hrl").
-include("megaco_test_lib.hrl").


%%----------------------------------------------------------------------

-define(TIMEOUT, timer:minutes(5)).


%%----------------------------------------------------------------------

-record(state, 
	{
	  parent, 
	  callback_module, 
	  callback_state, 
	  handler = {undefined, undefined}, 
	  timer, 
	  name,
	  id
	 }).


%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init(Args) -> {ok, State} | {error, Reason} when
      Args :: term(),
      State :: term(),
      Reason :: term().

-callback handle_parse(Instruction, State) -> 
    {ok, NewInstruction, NewState} |
    {error, Reason} when
      Instruction    :: term(),
      State          :: term(),
      NewInstruction :: term(),
      NewState       :: term(),
      Reason         :: term().

-callback handle_exec(Instruction, State) -> 
    {ok, NewState} |
    {error, Reason} when
      Instruction :: term(),
      State       :: term(),
      NewState    :: term(),
      Reason      :: term().

-callback terminate(Reason, State) -> 
    megaco:void() when
      Reason :: term(),
      State :: term().


%%----------------------------------------------------------------------

start_link(Mod, Args, Name) 
  when is_atom(Mod) andalso is_list(Name) ->
    start(Mod, Args, Name, self()).

start_link(Mod, Args, Name, Node) 
  when is_atom(Mod) andalso is_list(Name) andalso (Node =/= node()) ->
    case rpc:call(Node, ?MODULE, start, [Mod, Args, Name, self()]) of
	{ok, Pid} ->
	    link(Pid),
	    {ok, Pid};
	Error ->
	    Error
    end;
start_link(Mod, Args, Name, Node) 
  when is_atom(Mod) andalso is_list(Name) andalso (Node =:= node()) ->
    case start(Mod, Args, Name, self()) of
	{ok, Pid} ->
	    link(Pid),
	    {ok, Pid};
	Error ->
	    Error
    end.    

start(Mod, Args, Name, Pid) when is_pid(Pid) ->
    gen_server:start({local, Mod}, ?MODULE, [Mod, Args, Name, Pid], []).


exec(Server, Instructions) ->
    exec(Server, Instructions, infinity).

exec(Server, Instructions, Timeout) 
  when ((Timeout == infinity) orelse 
	(is_integer(Timeout) andalso (Timeout > 0))) ->
    call(Server, {exec, Instructions, Timeout}).


stop(Server) ->
    call(Server, stop).


%%----------------------------------------------------------------------
    
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------

init([Mod, Args, Name, Parent]) ->
    put(name, Name ++ "-CTRL"),
    process_flag(trap_exit, true), 
    put(debug, true),
    d("init -> entry with"
      "~n   Mod:     ~p"
      "~n   Args:    ~p"
      "~n   Name:    ~p"
      "~n   Parent:  ~p", [Mod, Args, Name, Parent]),
    case (catch Mod:init(Args)) of
	{ok, CallbackState} ->
	    d("init -> ~p initiated:"
	      "~n   CallbackState: ~p", [Mod, CallbackState]),
	    State = #state{callback_module = Mod, 
			   callback_state  = CallbackState, 
			   parent          = Parent, 
			   name            = Name},
	    d("init -> initiated"),
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({exec, Instructions, Timeout}, _From, 
	    #state{callback_module = Mod, 
		   callback_state  = CallbackState,
		   name            = Name} = State) ->
    d("handle_call(exec) -> entry with"
      "~n   Timeout: ~p", [Timeout]),
    case (catch handle_parse(Mod, CallbackState, Instructions)) of
	{ok, NewCallbackState, NewInstructions} ->
	    d("handle_call(exec) -> parsed"
	      "~n   NewCallbackState: ~p", [NewCallbackState]),
	    case handler_start(Name, Mod, NewCallbackState, NewInstructions) of
		{ok, Pid} ->
		    d("handle_call(exec) -> handler started"
		      "~n   Pid: ~p", [Pid]),
		    Timer = maybe_start_timer(Timeout),
		    Id    = {node(), make_ref()},
		    Reply = {ok, Id}, 
		    {reply, Reply, 
		     State#state{callback_state = NewCallbackState,
				 handler        = {running, Pid},
				 timer          = Timer,
				 id             = Id}};
		{error, Reason} ->
		    e("failed starting handler process"
		      "~n   Reason: ~p", [Reason]),		    
		    Reply = {error, {failed_starting_handler, Reason}}, 
		    {stop, Reason, Reply, State}
	    end;
	{error, Reason} ->
	    e("failed parsing instructions"
	      "~n   Reason: ~p", [Reason]),		    
	    Reply = {error, {invalid_instruction, Reason}}, 
	    {stop, Reason, Reply, State}
    end;

handle_call(stop, _From, State) ->
    Reply = ok,
    {stop, normal, Reply, State};

handle_call(Request, From, State) ->
    e("unexpected request"
      "~n   Request: ~p"
      "~n   From:    ~p", [Request, From]),		    
    Reason = {error, {unknown_request, Request, From}},
    Reply  = {error, unknown_request},
    {stop, Reason, Reply, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    e("unexpected message"
      "~n   Msg: ~p", [Msg]),		    
    Reason = {error, {unknown_message, Msg}},
    {stop, Reason, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({handler_result, Pid, Result}, 
	    #state{parent  = Parent,
		   handler = {running, Pid},
		   timer   = Timer,
		   id      = Id} = State) ->
    d("handle_info(handler_result) -> entry with"
      "~n   Result: ~p", [Result]),
    maybe_stop_timer(Timer),
    handler_stop(Pid), 
    deliver_exec_result(Parent, Id, Result),
    NewState = State#state{handler = {stopping, Pid}, 
			   timer   = undefined,
			   id      = undefined},
    {noreply, NewState};

handle_info(handler_timeout, #state{handler = {running, Pid}} = State) ->
    d("handle_info(handler_timeout) -> entry with"),
    handler_stop(Pid), 
    {noreply, State#state{handler = {stopping, Pid}}};

handle_info({'EXIT', Pid, {stopped, Result}}, 
	    #state{parent  = Parent,
		   handler = {stopping, Pid},
		   id      = Id} = State) ->
    d("handle_info(handler stopped EXIT) -> entry with"
      "~n   Result: ~p", [Result]),
    deliver_exec_result(Parent, Id, {error, {handler_timeout, Result}}),
    {noreply, State#state{handler = {stopped, undefined}, 
			  timer   = undefined,
			  id      = undefined}};

handle_info({'EXIT', Pid, normal}, 
	    #state{handler = {_, Pid},
		   timer   = Timer} = State) ->
    d("handle_info(handler normal EXIT) -> entry"), 
    maybe_stop_timer(Timer),
    {noreply, State#state{handler = {stopped, undefined}, timer = undefined}};

handle_info({'EXIT', Pid, Reason}, 
	    #state{parent  = Parent,
		   handler = {_, Pid},
		   timer   = Timer,
		   id      = Id} = State) ->
    d("handle_info(handler EXIT) -> entry with"
      "~n   Reason: ~p", [Reason]),
    maybe_stop_timer(Timer),
    deliver_exec_result(Parent, Id, {error, {handler_crashed, Reason}}),
    {noreply, State#state{handler = {crashed, undefined}, 
			  timer   = undefined,
			  id      = undefined}};

handle_info(Info, State) ->
    e("unexpected info"
      "~n   Info:  ~p"
      "~n   State: ~p", [Info, State]),		    
    Reason = {error, {unknown_info, Info}},
    {stop, Reason, State}.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, #state{handler = {_HandlerState, Pid}} = _State) ->
    d("terminate(normal) -> entry"),
    handler_stop(Pid),
    ok;

terminate(Reason, #state{handler         = {_HandlerState, Pid},
			 callback_module = Mod, 
			 callback_state  = CallbackState} = _State) ->
    d("terminate -> entry with"
      "~n   Reason: ~p", [Reason]),
    handler_kill(Pid),
    (catch Mod:terminate(Reason, CallbackState)),
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(_Vsn, S, _Extra) ->
    {ok, S}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

deliver_exec_result(Parent, Id, {ok, Result}) ->
    Parent ! {exec_complete, Id, ok, Result};
deliver_exec_result(Parent, Id, {error, Reason}) ->
    Parent ! {exec_complete, Id, error, Reason}.


handle_parse(Mod, State, Instructions) ->
    handle_parse(Mod, State, Instructions, []).

handle_parse(_Mod, State, [], Acc) ->
    {ok, State, lists:reverse(Acc)};

handle_parse(Mod, State, [Instruction|Instructions], Acc) ->
    case (catch Mod:handle_parse(Instruction, State)) of
	{ok, NewInstruction, NewState} ->
	    handle_parse(Mod, NewState, Instructions, [NewInstruction|Acc]);
	{error, Reason} ->
	    {error, {invalid_instruction, Instruction, Reason}};
	{'EXIT', Reason} ->
	    {error, {exit, Instruction, Reason}}
    end.


%%%-------------------------------------------------------------------

handler_kill(Pid) when is_pid(Pid) ->
    erlang:exit(Pid, kill);
handler_kill(_) ->
    ok.

handler_stop(Pid) when is_pid(Pid) ->
    Pid ! {stop, self()};
handler_stop(_) ->
    ok.

handler_start(Name, Mod, State, Instructions) ->
    Args = [Name, self(), Mod, State, Instructions], 
    proc_lib:start_link(?MODULE, handler_init, Args).


handler_init(Name, Parent, Mod, State, Instructions) ->
    put(name, Name ++ "-HANDLER"),
    proc_lib:init_ack(Parent, {ok, self()}),
    d("handler_init -> initiated"),
    handler_main(Parent, Mod, State, Instructions).

handler_main(Parent, Mod, State, []) ->
    d("handler_main -> done when"
      "~n   State: ~p", [State]),
    Result = (catch Mod:terminate(normal, State)),
    Parent ! {handler_result, self(), {ok, Result}},
    receive
	{stop, Parent} ->
	    exit(normal);
	{'EXIT', Parent, Reason} ->
	    exit({parent_died, Reason})
    end;
	
handler_main(Parent, Mod, State, [Instruction|Instructions]) ->
    d("handler_main -> entry with"
      "~n   Instruction: ~p", [Instruction]),
    receive
	{stop, Parent} ->
	    d("handler_main -> premature stop requested"),
	    Result = (catch Mod:terminate(stopped, State)),
	    exit({stopped, Result});
	{'EXIT', Parent, Reason} ->
	    d("handler_main -> parent exited"
	      "~n   Reason: ~p", [Reason]),
	    Result = (catch Mod:terminate({parent_died, Reason}, State)),
	    exit({parent_died, Reason, Result})
    after 0 ->
	    case (catch handler_callback_exec(Mod, State, Instruction)) of
		{ok, NewState} ->
		    handler_main(Parent, Mod, NewState, Instructions);
		{error, Reason} ->
		    d("handler_main -> exec failed"
		      "~n   Reason: ~p", [Reason]),
		    case (catch Mod:terminate(normal, State)) of
			{ok, Result} ->
			    Parent ! {handler_result, self(), {error, Result}};
			Error ->
			    Result = {bad_terminate, Error},
			    Parent ! {handler_result, self(), {error, Result}}
		    end,
		    receive
			{stop, Parent} ->
			    exit(normal);
			{'EXIT', Parent, Reason} ->
			    exit({parent_died, Reason})
		    end;
		{'EXIT', Reason} ->
		    d("handler_main -> exec EXIT"
		      "~n   Reason: ~p", [Reason]),
		    exit({callback_exec_exit, Reason})
		    
	    end
    end.

handler_callback_exec(Mod, State, Instruction) ->
    Mod:handle_exec(Instruction, State).

    
%%%-------------------------------------------------------------------

maybe_start_timer(Timeout) when is_integer(Timeout) ->
    erlang:send_after(Timeout, self(), handler_timeout);
maybe_start_timer(_) ->
    undefined.


maybe_stop_timer(undefined) ->
    ok;
maybe_stop_timer(Timer) ->
    (catch erlang:cancel_timer(Timer)).


%%% ----------------------------------------------------------------

call(Server, Request) ->
    call(Server, Request, infinity).

call(Server, Request, Timeout) ->
    case (catch gen_server:call(Server, Request, Timeout)) of
        {'EXIT', _} ->
            {error, not_started};
        Res ->
            Res
    end.

%% cast(Server, Msg) ->
%%     case (catch gen_server:cast(Server, Msg)) of
%%         {'EXIT', _} ->
%%             {error, not_started};
%%         Res ->
%%             Res
%%     end.


%%% ----------------------------------------------------------------

sleep(X) when is_integer(X) andalso (X =< 0) -> ok;
sleep(X) -> receive after X -> ok end.

sz(Bin) when is_binary(Bin) ->
    size(Bin);
sz(L) when is_list(L) ->
    length(L);
sz(_) ->
    -1.


%%% ----------------------------------------------------------------

d(F)    -> debug(F).
d(F, A) -> debug(F, A).

e(F, A) -> error(F, A).

%% p(P,    F, A) -> print(P,    F, A).
%% p(P, N, F, A) -> print(P, N, F, A).


%% -------------------------

debug(F) ->
    debug(F, []).

debug(F, A) ->
    debug(get(debug), F, A).

debug(true, F, A) ->
    print(" DBG", F, A);
debug(_, _F, _A) ->
    ok.


error(F, A) ->
    print(" ERROR", F, A).


print(P, F, A) ->
    print(P, get(name), F, A).

print([], undefined, F, A) ->
    io:format("*** [~s] ~p *** " ++ 
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]);
print(P, undefined, F, A) ->
    io:format("*** [~s] ~p ~s *** " ++ 
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self(), P | A]);
print(P, N, F, A) ->
    io:format("*** [~s] ~p ~s~s *** " ++ 
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self(), N, P | A]).

