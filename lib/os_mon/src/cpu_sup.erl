%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(cpu_sup).

%% API
-export([start_link/0, start/0, stop/0]).
-export([nprocs/0, avg1/0, avg5/0, avg15/0, util/0, util/1]).
-export([dummy_reply/1]).

%% For testing
-export([ping/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal protocol with the port program
-define(nprocs,"n").
-define(avg1,"1").
-define(avg5,"5").
-define(avg15,"f").
-define(quit,"q").
-define(ping,"p").
-define(util,"u").

-define(cu_cpu_id, 0).
-define(cu_user, 1).
-define(cu_nice_user, 2).
-define(cu_kernel, 3).
-define(cu_io_wait, 4).
-define(cu_idle, 5).
-define(cu_hard_irq, 6).
-define(cu_soft_irq, 7).
-define(cu_steal, 8).

-define(INT32(D3,D2,D1,D0),
	(((D3) bsl 24) bor ((D2) bsl 16) bor ((D1) bsl 8) bor (D0))).

-define(MAX_UINT32, ((1 bsl 32) - 1)).

-record(cpu_util, {cpu, busy = [], non_busy = []}).

-record(state, {server, os_type}).
%-record(state, {server, port = not_used, util = [], os_type}).

-record(internal, {port = not_used, util = [], os_type}).

%%----------------------------------------------------------------------
%% Contract specifications 
%%----------------------------------------------------------------------

-type util_cpus() :: 'all' | integer() | [integer()].
-type util_state() :: 'user' | 'nice_user' | 'kernel' | 'wait' | 'idle'.
-type util_value() :: [{util_state(), float()}] | float().
-type util_desc() :: {util_cpus(), util_value(), util_value(), []}.

%%----------------------------------------------------------------------
%% Exported functions
%%----------------------------------------------------------------------

start() ->
    gen_server:start({local, cpu_sup}, cpu_sup, [], []).

start_link() ->
    gen_server:start_link({local, cpu_sup}, cpu_sup, [], []).

stop() ->
    gen_server:call(cpu_sup, ?quit, infinity).

-spec nprocs() -> integer() | {'error', any()}.

nprocs() ->
    os_mon:call(cpu_sup, ?nprocs, infinity).

-spec avg1() -> integer() | {'error', any()}.

avg1() ->
    os_mon:call(cpu_sup, ?avg1, infinity).

-spec avg5() -> integer() | {'error', any()}.

avg5() ->
    os_mon:call(cpu_sup, ?avg5, infinity).

-spec avg15() -> integer() | {'error', any()}.

avg15() ->
    os_mon:call(cpu_sup, ?avg15, infinity).

-spec util(['detailed' | 'per_cpu']) ->
	util_desc() | [util_desc()] | {'error', any()}.

util(Args) when is_list (Args) ->
   % Get arguments
   case lists:foldl(
	    fun (detailed, {_ , PC}) -> {true, PC  };
	        (per_cpu , {D , _ }) -> {D   , true};
	        (_       , _       ) -> badarg
	    end, {false, false}, Args) of
	badarg -> 
	    erlang:error(badarg);
	{Detailed, PerCpu} ->
	    os_mon:call(cpu_sup, {?util, Detailed, PerCpu}, infinity)
    end;
util(_) ->
    erlang:error(badarg).

-spec util() -> float() | {'error', any()}.

util() ->
    case util([]) of
	{all, Busy, _, _} -> Busy;
	Error -> Error
    end.

dummy_reply(?nprocs) -> 0;
dummy_reply(?avg1) ->   0;
dummy_reply(?avg5) ->   0;
dummy_reply(?avg15) ->  0;
dummy_reply({?util,_,_}) -> {all, 0, 0, []}.

%%----------------------------------------------------------------------
%% For testing
%%----------------------------------------------------------------------

ping() ->
    gen_server:call(cpu_sup,?ping).

%%----------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------

%% init
init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),
    {ok, 
	#state{	os_type = os:type(), 
		server = measurement_server_start()
	}
    }.
handle_call(?quit, _From, State) ->
    {stop, normal, ok, State};
handle_call({?util, D, PC}, {Client, _Tag},
	#state{os_type = {unix, Flavor}} = State) 
	when Flavor == sunos;
	     Flavor == linux;
	     Flavor == freebsd;
	     Flavor == darwin ->
    case measurement_server_call(State#state.server, {?util, D, PC, Client}) of
	{error, Reason} -> 
	    {	reply, 
		{error, Reason}, 
		State#state{server=measurement_server_restart(State#state.server)}
	    };
	Result -> {reply, Result, State}
    end;
handle_call({?util, Detailed, PerCpu}, _From, State) ->
    String = "OS_MON (cpu_sup), util/1 unavailable for this OS~n",
    error_logger:warning_msg(String),
    {reply, dummy_reply({?util, Detailed, PerCpu}), State};
handle_call(Request, _From, State) when Request==?nprocs;
					Request==?avg1;
					Request==?avg5;
					Request==?avg15;
					Request==?ping ->
    case measurement_server_call(State#state.server, Request) of
	{error, Reason} -> 
	    {	reply, 
		{error, Reason}, 
		State#state{server=measurement_server_restart(State#state.server)}
	    };
	Result -> {reply, Result, State}
    end.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {server_died, Reason}, State#state{server=not_used}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    exit(State#state.server, normal).

%% os_mon-2.0
%% For live downgrade to/upgrade from os_mon-1.8[.1]
code_change(Vsn, PrevState, "1.8") ->
    case Vsn of

	%% Downgrade from this version
	{down, _Vsn} ->
	    process_flag(trap_exit, false);

	%% Upgrade to this version
	_Vsn ->
	    process_flag(trap_exit, true)
    end,
    {ok, PrevState};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% internal functions 
%%----------------------------------------------------------------------

get_uint32_measurement(Request, #internal{os_type = {unix, linux}}) ->
    {ok,F} = file:open("/proc/loadavg",[read,raw]),
    {ok,D} = file:read_line(F),
    ok = file:close(F),
    {ok,[Load1,Load5,Load15,_PRun,PTotal],_} = io_lib:fread("~f ~f ~f ~d/~d", D),
    case Request of
	?avg1  -> sunify(Load1);
	?avg5  -> sunify(Load5);
	?avg15 -> sunify(Load15);
	?ping -> 4711;
	?nprocs -> PTotal
    end;
get_uint32_measurement(Request, #internal{port = P, os_type = {unix, Sys}}) when
								Sys == sunos;
								Sys == dragonfly;
								Sys == openbsd;
								Sys == freebsd;
								Sys == darwin ->
    port_server_call(P, Request);
get_uint32_measurement(Request, #internal{os_type = {unix, Sys}}) when Sys == irix64;
								 Sys == irix ->
    %% Get the load average using uptime.
    %% "8:01pm  up 2 days, 22:12,  4 users,  load average: 0.70, 0.58, 0.43"
    D = os:cmd("uptime") -- "\n",
    Avg = lists:reverse(hd(string:lexemes(lists:reverse(D), ":"))),
    {ok, [L1, L5, L15], _} = io_lib:fread("~f, ~f, ~f", Avg),
    case Request of
	?avg1  -> sunify(L1);
	?avg5  -> sunify(L5);
	?avg15 -> sunify(L15);
	?ping -> 4711;
	?nprocs ->
	    {ok, ProcList} = file:list_dir("/proc/pinfo"),
	    length(ProcList)
    end;
get_uint32_measurement(_, _) -> 
    throw(not_implemented).


get_util_measurement(?util, #internal{port = P }) ->
    case port_server_call(P, ?util) of
	{error, Error} -> {error, Error};
        NewCpuUtil -> NewCpuUtil
    end;
get_util_measurement(_,_) ->
    throw(not_implemented).

%%----------------------------------------------------------------------
%% BEGIN: tainted internal functions 
%%----------------------------------------------------------------------

sunify(Val)  ->
    round(Val*256). % Note that Solaris and Linux load averages are
		% measured quite differently anyway


keysearchdelete(_, _, []) ->
    {false, []};
keysearchdelete(K, N, [T|Ts]) when element(N, T) == K ->
    {{value, T}, Ts};
keysearchdelete(K, N, [T|Ts]) ->
    {X, NTs} = keysearchdelete(K, N, Ts),
    {X, [T|NTs]}.

%% Internal cpu utilization functions 

%% cpu_util_diff(New, Old) takes a list of new cpu_util records as first
%% argument and a list of old cpu_util records as second argument. The
%% two lists have to be sorted on cpu index in ascending order.
%%
%% The returned value is a difference list in descending order.
cpu_util_diff(New, Old) ->
    cpu_util_diff(New, Old, []).

cpu_util_diff([], [], Acc) ->
    Acc;
cpu_util_diff([#cpu_util{cpu      = Cpu,
		     busy     = NewBusy,
		     non_busy = NewNonBusy} | NewCpuUtils],
	  [#cpu_util{cpu      = Cpu,
		     busy     = OldBusy,
		     non_busy = OldNonBusy} | OldCpuUtils],
	  Acc) ->
    {PreBusy, GotBusy} = state_list_diff(NewBusy, OldBusy),
    {NonBusy, GotNonBusy} = state_list_diff(NewNonBusy, OldNonBusy),
    Busy = case GotBusy orelse GotNonBusy of
	   true ->
	       PreBusy;
	   false ->
	       %% This can happen if cpu_sup:util/[0,1] is called
	       %% again immediately after the previous call has
	       %% returned. Because the user obviously is doing
	       %% something we charge "user".
	       lists:map(fun ({user, 0}) -> {user, 1};
			     ({_, 0} = StateTup) -> StateTup
			 end,
			 PreBusy)
       end,
cpu_util_diff(NewCpuUtils, OldCpuUtils, [#cpu_util{cpu      = Cpu,
						   busy     = Busy,
						   non_busy = NonBusy}
					 | Acc]);

%% A new cpu appeared
cpu_util_diff([#cpu_util{cpu = NC}|_] = New,
	  [#cpu_util{cpu = OC}|_] = Old,
	  Acc) when NC < OC ->
cpu_util_diff(New, [#cpu_util{cpu = NC}|Old], Acc);
cpu_util_diff([#cpu_util{cpu = NC}|_] = New, [], Acc) ->
cpu_util_diff(New, [#cpu_util{cpu = NC}], Acc);

%% An old cpu disappeared
cpu_util_diff([#cpu_util{cpu = NC}|Ns],
	  [#cpu_util{cpu = OC}|_] = Old,
	  Acc) when NC > OC ->
cpu_util_diff(Ns, Old, Acc);
cpu_util_diff([], _Old, Acc) ->
cpu_util_diff([], [], Acc).

cpu_util_rel(NewCpuUtils, OldCpuUtils, Detailed, PerCpu) ->
    cpu_util_rel(cpu_util_diff(NewCpuUtils, OldCpuUtils), Detailed, PerCpu).

%% 
%% cpu_util_rel/3 takes a difference list of cpu_util records as first
%% argument, a boolean determining if the result should be detailed as
%% second argument, and a boolean determining if the result should be
%% per cpu as third argument. The first argument (the difference list)
%% has to be sorted on cpu index in descending order.
%% 
cpu_util_rel(CUDiff, false, false) ->
    {B, T} = lists:foldl(fun (#cpu_util{busy     = BusyList,
					non_busy = NonBusyList},
			      {BusyAcc, TotAcc}) ->
				 Busy  = state_list_sum(BusyList),
				 NonBusy = state_list_sum(NonBusyList),
				 {BusyAcc+Busy, TotAcc+Busy+NonBusy}
			 end,
			 {0, 0},
			 CUDiff),
    BRel = B/T*100,
    {all, BRel, 100-BRel, []};
cpu_util_rel(CUDiff, true, false) ->
    cpu_util_rel_det(CUDiff, #cpu_util{cpu = [], busy = [], non_busy = []}); 
cpu_util_rel(CUDiff, false, true) ->
    cpu_util_rel_pcpu(CUDiff, []);
cpu_util_rel(CUDiff, true, true) ->
    cpu_util_rel_det_pcpu(CUDiff, []).

cpu_util_rel_pcpu([], Acc) ->
    Acc;
cpu_util_rel_pcpu([#cpu_util{cpu      = C,
			     busy     = BusyList,
			     non_busy = NonBusyList} | Rest], Acc) ->
    Busy  = state_list_sum(BusyList),
    NonBusy = state_list_sum(NonBusyList),
    Tot = Busy + NonBusy,
    cpu_util_rel_pcpu(Rest, [{C, Busy/Tot*100, NonBusy/Tot*100, []}|Acc]).

cpu_util_rel_det([], #cpu_util{cpu      = CpuAcc,
			       busy     = BusyAcc,
			       non_busy = NonBusyAcc}) ->
    Total = state_list_sum(BusyAcc) + state_list_sum(NonBusyAcc),
    {CpuAcc, mk_rel_states(BusyAcc,Total), mk_rel_states(NonBusyAcc,Total), []};
cpu_util_rel_det([#cpu_util{cpu      = Cpu,
			    busy     = Busy,
			    non_busy = NonBusy} | Rest],
		 #cpu_util{cpu      = CpuAcc,
			   busy     = BusyAcc,
			   non_busy = NonBusyAcc}) ->
    cpu_util_rel_det(Rest, #cpu_util{cpu      = [Cpu|CpuAcc],
				     busy     = state_list_add(Busy,
							       BusyAcc),
				     non_busy = state_list_add(NonBusy,
							       NonBusyAcc)}).

cpu_util_rel_det_pcpu([], Acc) ->
    Acc;
cpu_util_rel_det_pcpu([#cpu_util{cpu      = Cpu,
				 busy     = Busy,
				 non_busy = NonBusy}| Rest], Acc) ->
    Total = state_list_sum(Busy) + state_list_sum(NonBusy),
    cpu_util_rel_det_pcpu(Rest,
			  [{Cpu,
			    mk_rel_states(Busy, Total),
			    mk_rel_states(NonBusy, Total),
			    []} | Acc]).

mk_rel_states(States, Total) ->
    lists:map(fun ({State, Value}) -> {State, 100*Value/Total} end, States).

state_list_sum(StateList) ->
    lists:foldl(fun ({_, X}, Acc) -> Acc+X end, 0, StateList).

state_list_diff([],[]) ->
    {[], false};
state_list_diff([{State,ValueNew}|RestNew], []) ->
    state_list_diff([{State, ValueNew} | RestNew], [{State, 0}]);
state_list_diff([{State,ValueNew}|RestNew], [{State,ValueOld}|RestOld]) ->
    ValDiff = val_diff(State, ValueNew, ValueOld),
    {RestStateDiff, FoundDiff} = state_list_diff(RestNew, RestOld),
    {[{State, ValDiff} | RestStateDiff], FoundDiff orelse ValDiff /= 0}.

state_list_add([],[]) ->
    [];
state_list_add([{State, ValueA}|RestA], []) ->
    [{State, ValueA} | state_list_add(RestA, [])];
state_list_add([{State, ValueA} | RestA], [{State, ValueB} | RestB]) ->
    [{State, ValueA + ValueB} | state_list_add(RestA, RestB)].

one_step_backwards(State, New, Old) ->
    case os:type() of
	{unix, linux} ->
	    %% This should never happen! But values sometimes takes a step
	    %% backwards on linux. We'll ignore it as long as it's only
	    %% one step...
	    0;
	_ ->
	    val_diff2(State, New, Old)
    end.

val_diff(State, New, Old) when New == Old - 1 ->
    one_step_backwards(State, New, Old);
val_diff(State, ?MAX_UINT32, 0) ->
    one_step_backwards(State, ?MAX_UINT32, 0);
val_diff(State, New, Old) ->
    val_diff2(State, New, Old).

val_diff2(State, New, Old) when New > ?MAX_UINT32; Old > ?MAX_UINT32 ->
    %% We obviously got uints > 32 bits
    ensure_positive_diff(State, New - Old);
val_diff2(State, New, Old) when New < Old ->
    %% 32-bit integer wrapped
    ensure_positive_diff(State, (?MAX_UINT32 + 1) + New - Old);
val_diff2(_State, New, Old) ->
    New - Old.

ensure_positive_diff(_State, Diff) when Diff >= 0 ->
    Diff;
ensure_positive_diff(State, Diff) ->
    throw({error, {negative_diff, State, Diff}}).
%%----------------------------------------------------------------------
%% END: tainted internal functions 
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% cpu_sup measurement server wrapper
%%----------------------------------------------------------------------

measurement_server_call(Pid, Request) ->
    Timeout = 5000,
    Pid ! {self(), Request},
    receive
	{data, Data} -> Data
    after Timeout -> 
	{error, timeout}
    end.

measurement_server_restart(Pid) ->
    exit(Pid, kill),
    measurement_server_start().

measurement_server_start() ->
    spawn(fun() -> measurement_server_init() end).

measurement_server_init() ->
    process_flag(trap_exit, true),
    OS = os:type(),
    Server = case OS of
	{unix, Flavor} when
			    Flavor==sunos;
			    Flavor==linux;
			    Flavor==darwin;
			    Flavor==freebsd;
			    Flavor==dragonfly;
			    Flavor==openbsd ->
	    {ok, Pid} = port_server_start_link(),
	    Pid;
	{unix, Flavor} when
			    Flavor==irix64;
			    Flavor==irix ->
	    not_used;
	_ ->
	    exit({unsupported_os, OS})
    end, 
    measurement_server_loop(#internal{port=Server, os_type=OS}).

measurement_server_loop(State) ->
    receive
	{_, quit} ->
	    State#internal.port ! {self(), ?quit}, 
	    ok;
	{'DOWN',Monitor,process,_,_} ->
	    measurement_server_loop(State#internal{ util = lists:keydelete(
		Monitor,
		2,
		State#internal.util)});
	{Pid, {?util, D, PC, Client}} ->
	    {Monitor, OldCpuUtil, Utils2} = case keysearchdelete(Client, 1, State#internal.util) of
		{{value, {Client, Mon, U}}, Us} -> {Mon, U, Us};
		{false, Us} -> {erlang:monitor(process, Client), [], Us}
	    end,
	    try get_util_measurement(?util, State) of
		NewCpuUtil ->
		    Result = cpu_util_rel(NewCpuUtil, OldCpuUtil, D, PC),
		    Pid ! {data, Result},
	   	    measurement_server_loop(State#internal{util=[{Client,Monitor,NewCpuUtil}|Utils2]})
	    catch
		Error -> 
		    Pid ! {error, Error},
		    measurement_server_loop(State)
	    end;
	{Pid, Request} ->
            _ = try get_uint32_measurement(Request, State) of
                    Result -> Pid ! {data, Result}
                catch
                    Error -> Pid ! {error, Error}
                end,
	    measurement_server_loop(State);
        {'EXIT', OldPid, _n} when State#internal.port == OldPid ->
	    {ok, NewPid} = port_server_start_link(),
	    measurement_server_loop(State#internal{port = NewPid});
	_Other ->
	    measurement_server_loop(State)
    end.

%%----------------------------------------------------------------------
%% cpu_sup port program server wrapper
%%----------------------------------------------------------------------

port_server_call(Pid, Command) ->
    Pid ! {self(), Command},
    receive
	{Pid, {data, Result}} -> Result;
	{Pid, {error, Reason}} -> {error, Reason}
    end.
    
port_server_start_link() ->
    Timeout = 6000,
    Pid = spawn_link(fun() -> port_server_init(Timeout) end),
    Pid ! {self(), ?ping},
    receive
	{Pid, {data,4711}} -> {ok, Pid};
	{error,Reason} -> {error, Reason}
    after Timeout -> 
	{error, timeout}
    end.

port_server_init(Timeout) ->
    Port = start_portprogram(),
    port_server_loop(Port, Timeout).

port_server_loop(Port, Timeout) ->
    receive

	% Adjust timeout
	{Pid, {timeout, Timeout}} ->
	    Pid ! {data, Timeout},
	    port_server_loop(Port, Timeout);
	% Number of processors
        {Pid, ?nprocs} ->
	    port_command(Port, ?nprocs),
	    Result = port_receive_uint32(Port, Timeout),
	    Pid ! {self(), {data, Result}},
	    port_server_loop(Port, Timeout);

	% Average load for the past minute
        {Pid, ?avg1} ->
	    port_command(Port, ?avg1),
	    Result = port_receive_uint32(Port, Timeout),
	    Pid ! {self(), {data, Result}},
	    port_server_loop(Port, Timeout);

	% Average load for the past five minutes
        {Pid, ?avg5} ->
	    port_command(Port, ?avg5),
	    Result = port_receive_uint32(Port, Timeout),
	    Pid ! {self(), {data, Result}},
	    port_server_loop(Port, Timeout);
	
	% Average load for the past 15 minutes
        {Pid, ?avg15} ->
	    port_command(Port, ?avg15),
	    Result = port_receive_uint32(Port, Timeout),
	    Pid ! {self(), {data, Result}},
	    port_server_loop(Port, Timeout);

	{Pid, ?util} ->
	    port_command(Port, ?util),
	    Result = port_receive_util(Port, Timeout),
	    Pid ! {self(), {data, Result}},
	    port_server_loop(Port, Timeout);

	% Port ping
	{Pid, ?ping} ->
	    port_command(Port, ?ping),
	    Result = port_receive_uint32(Port, Timeout),
	    Pid ! {self(), {data, Result}},
	    port_server_loop(Port, Timeout);

	% Close port and this server
	{Pid, ?quit} ->
	    port_command(Port, ?quit),
	    port_close(Port),
	    Pid ! {self(), {data, quit}},
	    ok;

	% Ignore other commands
	_ -> port_server_loop(Port, Timeout)
    end.	

port_receive_uint32( Port,  Timeout) -> port_receive_uint32(Port, Timeout, []).
port_receive_uint32(_Port, _Timeout, [D3,D2,D1,D0]) -> ?INT32(D3,D2,D1,D0);
port_receive_uint32(_Port, _Timeout, [_,_,_,_ | G]) -> exit({port_garbage, G});
port_receive_uint32(Port, Timeout, D) ->
    receive
	{'EXIT', Port, Reason} -> exit({port_exit, Reason});
	{Port, {data, ND}} -> port_receive_uint32(Port, Timeout, D ++ ND)
    after Timeout -> exit(timeout_uint32) end.

port_receive_util(Port, Timeout) ->
    receive
	{Port, {data, [ NP3,NP2,NP1,NP0,  % Number of processors
		        NE3,NE2,NE1,NE0   % Number of entries per processor
		      | CpuData]}} ->
	    port_receive_cpu_util( ?INT32(NP3,NP2,NP1,NP0),
				   ?INT32(NE3,NE2,NE1,NE0),
				   CpuData, []);
	{'EXIT', Port, Reason} -> exit({port_exit, Reason})
    after Timeout -> exit(timeout_util) end.

% per processor receive loop
port_receive_cpu_util(0, _NE, [], CpuList) ->
    % Return in ascending cpu_id order
    lists:reverse(CpuList); 
port_receive_cpu_util(0, _NE, Garbage, _) ->
    exit( {port_garbage, Garbage});
port_receive_cpu_util(NP, NE, CpuData, CpuList) ->
    {CpuUtil, Rest} = port_receive_cpu_util_entries(NE, #cpu_util{}, CpuData),
    port_receive_cpu_util(NP - 1, NE, Rest, [ CpuUtil | CpuList]).

% per entry receive loop
port_receive_cpu_util_entries(0, CU, Rest) ->
    {CU, Rest};
port_receive_cpu_util_entries(NE, CU,
	[ CID3, CID2, CID1, CID0,
	  Val3, Val2, Val1, Val0 |
	  CpuData]) ->

    TagId = ?INT32(CID3,CID2,CID1,CID0),
    Value = ?INT32(Val3,Val2,Val1,Val0),

    % Conversions from integers to atoms
    case TagId of
	?cu_cpu_id ->
	    NewCU = CU#cpu_util{cpu = Value},
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_user ->
	    NewCU = CU#cpu_util{
		busy     = [{user, Value} | CU#cpu_util.busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_nice_user ->
	    NewCU = CU#cpu_util{
		busy     = [{nice_user, Value} | CU#cpu_util.busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_kernel ->
	    NewCU = CU#cpu_util{
		busy     = [{kernel, Value} | CU#cpu_util.busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_io_wait ->
	    NewCU = CU#cpu_util{
		non_busy = [{wait, Value} | CU#cpu_util.non_busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_idle ->	
	    NewCU = CU#cpu_util{
		non_busy = [{idle, Value} | CU#cpu_util.non_busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_hard_irq -> 	
	    NewCU = CU#cpu_util{
		busy =     [{hard_irq, Value} | CU#cpu_util.busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_soft_irq ->
	    NewCU = CU#cpu_util{
		busy =     [{soft_irq, Value} | CU#cpu_util.busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
	?cu_steal ->
	    NewCU = CU#cpu_util{
		non_busy = [{steal, Value} | CU#cpu_util.non_busy] },
    	    port_receive_cpu_util_entries(NE - 1, NewCU, CpuData);
    	Unhandled ->
	    exit({unexpected_type_id, Unhandled})
    end;
port_receive_cpu_util_entries(_, _, Data) -> 
     exit({data_mismatch, Data}).

start_portprogram() ->
    Port = os_mon:open_port("cpu_sup", [stream]),
    port_command(Port, ?ping),
    4711 = port_receive_uint32(Port, 5000),
    Port.
