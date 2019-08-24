%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

-module(snmp_note_store).

-behaviour(gen_server).

-include_lib("snmp/src/app/snmp_internal.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").

%% External exports
-export([start_link/3, stop/1, 
	 get_note/2, 
	 set_note/3, set_note/4, 
	 info/1, verbosity/2]).

%% Internal exports
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3]).

-export([timer/3]).

-define(timeout, 30000).  % Perform gc twice in a minute.

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-ifdef(snmp_debug).
-define(GS_START_LINK(Args),
	gen_server:start_link(?MODULE, Args, [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Args),
	gen_server:start_link(?MODULE, Args, [])).
-endif.


-record(state, {mod, notes, timer, timeout, active = false}).


%%%-----------------------------------------------------------------
%%% Implements a database for notes with a lifetime. Once in a
%%% while, the database will be gc:ed, to get rid of old notes.
%%% This database will not contain much data.
%%% Options is a list of Option, where Option is
%%%   {verbosity, silence|log|debug|trace} % undocumented feature
%%%-----------------------------------------------------------------
start_link(Prio, Mod, Opts) ->
    ?d("start_link -> entry with"
	"~n   Prio: ~p"
	"~n   Mod:  ~p"
	"~n   Opts: ~p", [Prio, Mod, Opts]),
    ?GS_START_LINK([Prio, Mod, Opts]).


%%-----------------------------------------------------------------
%% Interface functions.
%%-----------------------------------------------------------------

stop(Pid) ->
    call(Pid, stop).

get_note(Pid, Key) ->
    call(Pid, {get_note, Key}).

%% Lifetime is in 1/10 sec or infinity
set_note(Pid, Key, Value) ->
    set_note(Pid, infinity, Key, Value).
set_note(Pid, Lifetime, Key, Value) ->
    call(Pid, {set_note, Lifetime, Key, Value}).

info(Pid) ->
    call(Pid, info).

verbosity(Pid, Verbosity) -> 
    cast(Pid, {verbosity, Verbosity}).


init([Prio, Mod, Opts]) ->
    ?d("init -> entry with"
	"~n   Prio: ~p"
	"~n   Mod:  ~p"
	"~n   Opts: ~p", [Prio, Mod, Opts]),
    case (catch do_init(Prio, Mod, Opts)) of
	{ok, State} ->
	    {ok, State};
	E ->
	    error_msg("failed starting note-store: ~n~p", [E]),
	    {stop, E}
    end.

do_init(Prio, Mod, Opts) ->
    process_flag(trap_exit, true),
    process_flag(priority, Prio),
    put(sname, get_sname(Opts)),
    put(verbosity, get_verbosity(Opts)),
    put(snmp_component, get_component(Mod)),
    ?vlog("starting",[]),
    Notes   = ets:new(snmp_note_store, [set, protected]), 
    Timeout = get_timeout(Opts),
    State   = #state{mod     = Mod, 
		     notes   = Notes, 
		     timeout = Timeout, 
		     timer   = start_timer(Timeout)},
    ?vdebug("started",[]),
    {ok, State}.


%%-----------------------------------------------------------------
%% A note is any internal information that has to be
%% stored for some time (the Lifetime).
%% A note is stored in ets as {Key, {BestBefore, Value}},
%% where BestBefore is currentTime + Lifetime. 
%% A GC-op can destroy any notes with CurTime > BestBore.
%% Lifetime is in centiseconds or infinity, in which case
%% the note is eternal.
%%-----------------------------------------------------------------
handle_call({set_note, Lifetime, Key, Value}, _From, 
	    #state{mod = Mod, notes = Notes} = State) 
  when is_integer(Lifetime) ->
    ?vlog("set note <~p,~p> with life time ~p", [Key,Value,Lifetime]),
    case (catch Mod:system_start_time()) of
	SysStartTime when is_integer(SysStartTime) ->
	    ?vtrace("handle_call(set_note) -> SysStartTime: ~p", 
		    [SysStartTime]),
	    Now = snmp_misc:now(cs), 
	    ?vtrace("handle_call(set_note) -> Now: ~p", [Now]),
	    RealUpTime = Now - SysStartTime,
	    ?vtrace("handle_call(set_note) -> RealUpTime: ~p", [RealUpTime]),
	    BestBefore = RealUpTime + Lifetime,
	    ?vtrace("handle_call(set_note) -> BestBefore: ~p", [BestBefore]),
	    Val = ets:insert(Notes, {Key, {BestBefore, Value}}),
	    NState = activate_timer(State),
	    {reply, Val, NState};
	_Crap ->
	    ?vinfo("handle_call(set_note) -> "
		   "failed retreiving system start time from ~w: "
		   "~n   ~p", [Mod, _Crap]),
	    {reply, {error, failed_retreive_system_start_time}, State}
    end;

handle_call({set_note, infinity, Key, Value}, _From, 
	    #state{notes = Notes} = State) ->
    ?vlog("set note <~p,~p>",[Key,Value]),
    Val = ets:insert(Notes, {Key, {infinity, Value}}),
    ?vdebug("set note; old value: ~p",[Val]),
    {reply, Val, State};

handle_call({get_note, Key}, _From, 
	    #state{mod = Mod, notes = Notes} = State) ->
    ?vlog("get note ~p",[Key]),
    Val = handle_get_note(Notes, Mod, Key),
    ?vdebug("get note: ~p",[Val]),
    {reply, Val, State};

handle_call(info, _From, #state{timer = Pid, notes = Notes} = State) ->
    ?vlog("info",[]),
    Info = get_info(Pid, Notes),
    {reply, Info, State};

handle_call(stop, _From, State) ->
    ?vlog("stop",[]),
    {stop, normal, ok, State};

handle_call(Req, From, State) ->
    warning_msg("received unexpected request from ~p: ~n~p",[From, Req]),
    {reply, {error, {unknown_request, Req}}, State}.


handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    {noreply, State};
    
handle_cast(Msg, State) ->
    warning_msg("received unexpected message: ~n~p",[Msg]),
    {noreply, State}.
    

%%-----------------------------------------------------------------
%% If there are no possible garbage left, we don't
%% have to wait for timeout, and perform another
%% gc, because we won't do anything. So
%% we switch the timeout off in that case.
%% It will be switched on as soon as we get some
%% other message.
%%-----------------------------------------------------------------
handle_info(timeout, State) ->
    ?vdebug("timeout",[]),
    case gc(State) of
	nothing_left ->
	    NState = deactivate_timer(State),
	    {noreply, NState};
	work_to_do ->
	    NState = activate_timer(State),
	    {noreply, NState}
    end;

handle_info({'EXIT', Pid, Reason}, 
	    #state{timer = Pid, timeout = Timeout} = State) ->
    ?vinfo("exit message from the timer process ~p for reason ~p",
	   [Pid, Reason]),
    set_state(State#state{timer = start_timer(Timeout)});

handle_info({'EXIT',Pid,Reason}, State) ->
    ?vlog("exit message from ~p for reason ~p",[Pid,Reason]),
    {noreply, State};

handle_info(Info, State) ->
    warning_msg("received unexpected info: ~n~p",[Info]),
    {noreply, State}.


set_state(S) ->
    case gc(S) of
	nothing_left ->
	    NState = deactivate_timer(S),
	    {noreply, NState};
	work_to_do ->
	    NState = activate_timer(S),
	    {noreply, NState}
    end.


terminate(Reason, _State) ->
    ?vdebug("terminate: ~p",[Reason]),
    ok.


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

% downgrade
code_change({down, _Vsn}, State, _Extra) ->
    NState = activate_timer(deactivate_timer(State)),
    {ok, NState};

% upgrade
code_change(_Vsn, State0, _Extra) ->
    process_flag(trap_exit, true),
    State1 = 
	case State0 of
	    #state{timeout = false} ->
		State0#state{timeout = ?timeout};
	    _ ->
		State0
	end,
    State2 = restart_timer(State1),
    {ok, State2}.


%%----------------------------------------------------------
%% Timer
%%----------------------------------------------------------

activate_timer(#state{timer = Pid, active = false} = State) ->
    Pid ! activate,
    receive
	activated -> ok
    end,
    State#state{active = true};
activate_timer(State) ->
    State.

deactivate_timer(#state{timer = Pid, active = true} = State) ->
    Pid ! deactivate,
    receive
	deactivated -> ok
    end,
    State#state{active = false};
deactivate_timer(State) ->
    State.

start_timer(Timeout) ->
    spawn_link(?MODULE, timer, [self(), passive, Timeout]).

%% Kill, restart and activate timer.
restart_timer(#state{timer = Pid, timeout = Timeout} = State) ->
    ?d("restart_timer -> kill current timer process ~p",[Pid]),
    exit(Pid, kill),
    ?d("restart_timer -> await acknowledgement",[]),
    receive
	{'EXIT', Pid, _Reason} ->
	    ok
    end,
    ?d("restart_timer -> start a new timer process",[]),
    activate_timer(State#state{timer = start_timer(Timeout), active = false}).

timer(Pid, passive, Timeout) ->
    receive
	deactivate ->
	    ?d("timer(passive) -> deactivate request, just send ack",[]),
	    Pid ! deactivated,
	    ?MODULE:timer(Pid, passive, Timeout);

	activate ->
	    ?d("timer(deactive) -> activate request, send ack",[]),
	    Pid ! activated,
	    ?d("timer(deactive) -> activate",[]),
	    ?MODULE:timer(Pid, active, Timeout)		% code replacement
    after
	Timeout ->
	    ?d("timer(deactive) -> timeout",[]),
	    ?MODULE:timer(Pid, passive, Timeout)
    end;
timer(Pid, active, Timeout) ->
    receive
	activate ->
	    ?d("timer(active) -> activate request, just send ack",[]),
	    Pid ! activated,
	    ?MODULE:timer(Pid, active, Timeout);

	deactivate ->
	    ?d("timer(active) -> deactivate request, send ack",[]),
	    Pid ! deactivated,
	    ?d("timer(active) -> deactivate",[]),
	    ?MODULE:timer(Pid, passive, Timeout)
    after
	Timeout ->
	    ?d("timer(active) -> timeout",[]),
	    Pid ! timeout,
	    ?MODULE:timer(Pid, active, Timeout)
    end.
    

handle_get_note(Notes, Mod, Key) ->
    case ets:lookup(Notes, Key) of
	[{Key, {infinity, Val}}] ->
	    Val;
	[{Key, {BestBefore, Val}}] ->
	    ?vtrace("get note -> BestBefore: ~w", [BestBefore]),
	    StartTime = Mod:system_start_time(), 
	    ?vtrace("get note -> StartTime: ~w", [StartTime]),
	    Now = snmp_misc:now(cs), 
	    ?vtrace("get note -> Now: ~w", [Now]),
	    case (Now - StartTime) of
		Diff when BestBefore >= Diff ->
		    ?vtrace("get note -> Diff: ~w", [Diff]),
		    Val;
		OldDiff ->
		    ?vtrace("get note -> note to old [~w] - delete", [OldDiff]),
		    ets:delete(Notes, Key),
		    undefined
	    end;
	[] -> 
	    undefined
    end.


%%-----------------------------------------------------------------
%% Clean up all old notes in the database.
%%-----------------------------------------------------------------
gc(#state{mod = Mod, notes = Notes}) ->
    RealUpTime = snmp_misc:now(cs) - Mod:system_start_time(),
    gc(nothing_left, ets:tab2list(Notes), Notes, RealUpTime).

gc(Flag, [{_Key, {infinity, _}} | T], Tab, Now) -> gc(Flag, T, Tab, Now);
gc(Flag, [{Key, {BestBefore, _}} | T], Tab, Now) 
  when is_integer(BestBefore) andalso (BestBefore < Now) ->
    ets:delete(Tab, Key),
    gc(Flag, T, Tab, Now);
gc(_Flag, [_ | T], Tab, Now) -> gc(work_to_do, T, Tab, Now);
gc(Flag, [], _Tab, _Now) -> Flag.
    
    
%%-----------------------------------------------------------------

get_info(Tmr, Notes) ->
    ProcSize = proc_mem(self()),
    TMRSz    = proc_mem(Tmr),
    NotesSz  = tab_size(Notes),
    [{process_memory, [{notes, ProcSize}, {timer, TMRSz}]},
     {db_memory, [{notes, NotesSz}]}].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} ->
	    Sz;
	_ ->
	    undefined
    end;
proc_mem(_) ->
    undefined.

tab_size(T) ->
    case (catch ets:info(T, memory)) of
	Sz when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.


%%-----------------------------------------------------------------

call(Pid, Req) ->
    call(Pid, Req, infinity).

call(Pid, Req, Timeout) ->
    gen_server:call(Pid, Req, Timeout).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).


%%-----------------------------------------------------------------

%% info_msg(F, A) ->
%%     ?snmp_info(get(snmp_component), "Note store server " ++ F, A).

warning_msg(F, A) ->
    ?snmp_warning(get(snmp_component), "Note store server " ++ F, A).

error_msg(F, A) ->
    ?snmp_error(get(snmp_component), "Note store server " ++ F, A).


%%-----------------------------------------------------------------

get_verbosity(Opts) ->
    snmp_misc:get_option(verbosity, Opts, ?default_verbosity).

get_sname(Opts) ->
    snmp_misc:get_option(sname, Opts, ns).

get_timeout(Opts) ->
    snmp_misc:get_option(timeout, Opts, ?timeout).

get_component(snmpm) -> 
    "manager";
get_component(snmpa) ->
    "agent";
get_component(_) ->
    "".

