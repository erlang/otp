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
-module(memsup).
-behaviour(gen_server).

%% API
-export([start_link/0]). % for supervisor
-export([get_memory_data/0, get_system_memory_data/0,
	 get_check_interval/0, set_check_interval/1,
	 get_procmem_high_watermark/0, set_procmem_high_watermark/1,
	 get_sysmem_high_watermark/0, set_sysmem_high_watermark/1,
	 get_helper_timeout/0, set_helper_timeout/1,
	 get_os_wordsize/0]).
-export([dummy_reply/1, param_type/2, param_default/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Other exports
-export([format_status/2]).

-include("memsup.hrl").

-record(state,
	{os,                  % {OSfamily,OSname} | OSfamily
	 port_mode,           % bool()

	 mem_usage,           % undefined | {Alloc, Total}
	 worst_mem_user,      % undefined | {Pid, Alloc}

	 sys_only,            % bool()  memsup_system_only
	 timeout,             % int()   memory_check_interval, ms
	 helper_timeout,      % int()   memsup_helper_timeout, ms
	 sys_mem_watermark,   % float() system_memory_high_watermark, %
	 proc_mem_watermark,  % float() process_memory_high_watermark, %

	 pid,                 % undefined | pid()
	 wd_timer,            % undefined | TimerRef
	 ext_wd_timer,        % undefined | TimerRef
	 pending = [],        % [reg | {reg,From} | {ext,From}]
	 ext_pending = []     % [{ext,From}]
	}).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_os_wordsize() ->
    os_mon:call(memsup, get_os_wordsize, infinity).

get_memory_data() ->
    os_mon:call(memsup, get_memory_data, infinity).

get_system_memory_data() ->
    os_mon:call(memsup, get_system_memory_data, infinity).

get_check_interval() ->
    os_mon:call(memsup, get_check_interval, infinity).
set_check_interval(Minutes) ->
    case param_type(memory_check_interval, Minutes) of
	true ->
	    MS = minutes_to_ms(Minutes), % for backwards compatibility
	    os_mon:call(memsup, {set_check_interval, MS}, infinity);
	false ->
	    erlang:error(badarg)
    end.

get_procmem_high_watermark() ->
    os_mon:call(memsup, get_procmem_high_watermark, infinity).
set_procmem_high_watermark(Float) ->
    case param_type(process_memory_high_watermark, Float) of
	true ->
	    os_mon:call(memsup, {set_procmem_high_watermark, Float},
			infinity);
	false ->
	    erlang:error(badarg)
    end.

get_sysmem_high_watermark() ->
    os_mon:call(memsup, get_sysmem_high_watermark, infinity).
set_sysmem_high_watermark(Float) ->
    case param_type(system_memory_high_watermark, Float) of
	true ->
	    os_mon:call(memsup, {set_sysmem_high_watermark, Float},
			infinity);
	false ->
	    erlang:error(badarg)
    end.

get_helper_timeout() ->
    os_mon:call(memsup, get_helper_timeout, infinity).
set_helper_timeout(Seconds) ->
    case param_type(memsup_helper_timeout, Seconds) of
	true ->
	    os_mon:call(memsup, {set_helper_timeout, Seconds}, infinity);
	false ->
	    erlang:error(badarg)
    end.

dummy_reply(get_memory_data) ->
    dummy_reply(get_memory_data,
		os_mon:get_env(memsup, memsup_system_only));
dummy_reply(get_system_memory_data) ->
    [];
dummy_reply(get_os_wordsize) ->
    0;
dummy_reply(get_check_interval) ->
    minutes_to_ms(os_mon:get_env(memsup, memory_check_interval));
dummy_reply({set_check_interval, _}) ->
    ok;
dummy_reply(get_procmem_high_watermark) ->
    trunc(100 * os_mon:get_env(memsup, process_memory_high_watermark));
dummy_reply({set_procmem_high_watermark, _}) ->
    ok;
dummy_reply(get_sysmem_high_watermark) ->
    trunc(100 * os_mon:get_env(memsup, system_memory_high_watermark));
dummy_reply({set_sysmem_high_watermark, _}) ->
    ok;
dummy_reply(get_helper_timeout) ->
    os_mon:get_env(memsup, memsup_helper_timeout);
dummy_reply({set_helper_timeout, _}) ->
    ok.
dummy_reply(get_memory_data, true) ->
    {0,0,undefined};
dummy_reply(get_memory_data, false) ->
    {0,0,{self(),0}}.

param_type(memsup_system_only, Val) when Val==true; Val==false -> true;
param_type(memory_check_interval, Val) when is_integer(Val),
                                            Val>0 -> true;
param_type(memsup_helper_timeout, Val) when is_integer(Val),
                                            Val>0 -> true;
param_type(system_memory_high_watermark, Val) when is_number(Val),
						   0=<Val,
						   Val=<1 -> true;
param_type(process_memory_high_watermark, Val) when is_number(Val),
						    0=<Val,
						    Val=<1 -> true;
param_type(_Param, _Val) -> false.

param_default(memsup_system_only) -> false;
param_default(memory_check_interval) -> 1;
param_default(memsup_helper_timeout) -> 30;
param_default(system_memory_high_watermark) -> 0.80;
param_default(process_memory_high_watermark) -> 0.05.

%%----------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),

    OS = os:type(),
    PortMode = case OS of
		   {unix, darwin} -> false;
		   {unix, freebsd} -> false;
		   {unix, dragonfly} -> false;
		   % Linux supports this.
		   {unix, linux} -> true;
		   {unix, openbsd} -> true;
		   {unix, netbsd} -> true;
		   {unix, irix64} -> true;
		   {unix, irix} -> true;
		   {unix, sunos} -> true;
		   {win32, _OSname} -> false;
		   _ ->
		       exit({unsupported_os, OS})
	       end,
    Pid = if
	      PortMode ->
		  spawn_link(fun() -> port_init() end);
	      not PortMode ->
		  undefined
	  end,

    %% Read the values of some configuration parameters
    SysOnly = os_mon:get_env(memsup, memsup_system_only),
    Timeout = os_mon:get_env(memsup, memory_check_interval),
    HelperTimeout = os_mon:get_env(memsup, memsup_helper_timeout),
    SysMem = os_mon:get_env(memsup, system_memory_high_watermark),
    ProcMem = os_mon:get_env(memsup, process_memory_high_watermark),

    %% Initiate first data collection
    self() ! time_to_collect,

    {ok, #state{os=OS, port_mode=PortMode,

		sys_only           = SysOnly,
		timeout            = minutes_to_ms(Timeout),
		helper_timeout     = sec_to_ms(HelperTimeout),
		sys_mem_watermark  = SysMem,
		proc_mem_watermark = ProcMem,

		pid=Pid}}.

handle_call(get_os_wordsize, _From, State) ->
    Wordsize = get_os_wordsize(State#state.os),
    {reply, Wordsize, State};
handle_call(get_memory_data, From, State) ->
    %% Return result of latest memory check
    case State#state.mem_usage of
	{Alloc, Total} ->
	    Worst = State#state.worst_mem_user,
	    {reply, {Total, Alloc, Worst}, State};
	
	%% Special case: get_memory_data called before any memory data
	%% has been collected
	undefined ->
	    case State#state.wd_timer of
		undefined ->
		    WDTimer = erlang:send_after(State#state.timeout,
						self(),
						reg_collection_timeout),
		    Pending = [{reg,From}],
		    if
			State#state.port_mode ->
			    State#state.pid ! {self(), collect_sys},
			    {noreply, State#state{wd_timer=WDTimer,
						  pending=Pending}};
			true ->
			    OS = State#state.os,
			    Self = self(),
			    Pid = spawn_link(fun() ->
						     MU = get_memory_usage(OS),
						     Self ! {collected_sys,MU}
					     end),
			    {noreply, State#state{pid=Pid,
						  wd_timer=WDTimer,
						  pending=Pending}}
		    end;
		_TimerRef ->
		    Pending = [{reg,From} | State#state.pending],
		    {noreply, State#state{pending=Pending}}
	    end
    end;

handle_call(get_system_memory_data,From,#state{port_mode=true}=State) ->
    %% When using a port, the extensive memory collection is slightly
    %% different than a regular one
    case State#state.ext_wd_timer of
	undefined ->
	    WDTimer = erlang:send_after(State#state.helper_timeout,
					self(),
					ext_collection_timeout),
	    State#state.pid ! {self(), collect_ext_sys},
	    {noreply, State#state{ext_wd_timer=WDTimer,
				  ext_pending=[{ext,From}]}};
	_TimerRef ->
	    Pending = [{ext,From} | State#state.ext_pending],
	    {noreply, State#state{ext_pending=Pending}}
    end;
handle_call(get_system_memory_data, From, State) ->
    %% When not using a port, the regular memory collection is used
    %% for extensive memory data as well
    case State#state.wd_timer of
	undefined ->
	    WDTimer = erlang:send_after(State#state.helper_timeout,
					self(),
					reg_collection_timeout),
	    OS = State#state.os,
	    Self = self(),
	    Pid = spawn_link(fun() ->
				     MemUsage = get_memory_usage(OS),
				     Self ! {collected_sys, MemUsage}
			     end),
	    {noreply, State#state{pid=Pid, wd_timer=WDTimer,
				  pending=[{ext,From}]}};
	_TimerRef ->
	    Pending = [{ext,From} | State#state.pending],
	    {noreply, State#state{pending=Pending}}
    end;

handle_call(get_check_interval, _From, State) ->
    {reply, State#state.timeout, State};
handle_call({set_check_interval, MS}, _From, State) ->
    {reply, ok, State#state{timeout=MS}};

handle_call(get_procmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.proc_mem_watermark), State};
handle_call({set_procmem_high_watermark, Float}, _From, State) ->
    {reply, ok, State#state{proc_mem_watermark=Float}};

handle_call(get_sysmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.sys_mem_watermark), State};
handle_call({set_sysmem_high_watermark, Float}, _From, State) ->
    {reply, ok, State#state{sys_mem_watermark=Float}};

handle_call(get_helper_timeout, _From, State) ->
    {reply, ms_to_sec(State#state.helper_timeout), State};
handle_call({set_helper_timeout, Seconds}, _From, State) ->
    {reply, ok, State#state{helper_timeout=sec_to_ms(Seconds)}};

%% The following are only for test purposes (whitebox testing).
handle_call({set_sys_hw, HW}, _From, State) ->
    {reply, ok, State#state{sys_mem_watermark=HW}};
handle_call({set_pid_hw, HW}, _From, State) ->
    {reply, ok, State#state{proc_mem_watermark=HW}};
handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% It's time to check memory
handle_info(time_to_collect, State) ->
    case State#state.wd_timer of
	undefined ->
	    WDTimer = erlang:send_after(State#state.helper_timeout,
					self(),
					reg_collection_timeout),
	    if
		State#state.port_mode ->
		    State#state.pid ! {self(), collect_sys},
		    {noreply, State#state{wd_timer=WDTimer,
					  pending=[reg]}};
		true ->
		    OS = State#state.os,
		    Self = self(),
		    Pid = spawn_link(fun() ->
					     MU = get_memory_usage(OS),
					     Self ! {collected_sys,MU}
				     end),
		    {noreply, State#state{pid=Pid, wd_timer=WDTimer,
					  pending=[reg]}}
	    end;
	_TimerRef ->
	    {noreply, State#state{pending=[reg|State#state.pending]}}
    end;

%% Memory data collected
handle_info({collected_sys, {Alloc,Total}}, State) ->

    %% Cancel watchdog timer (and as a security measure,
    %% also flush any reg_collection_timeout message)
    TimeSpent = case erlang:cancel_timer(State#state.wd_timer) of
		    false ->
			State#state.helper_timeout;
		    TimeLeft ->
			State#state.helper_timeout-TimeLeft
		end,
    flush(reg_collection_timeout),

    %% First check if this is the result of a periodic memory check
    %% and update alarms and State if this is the case
    State2 =
	case lists:member(reg, State#state.pending) of
	    true ->

		%% Check if system alarm should be set/cleared
		if
		    Alloc > State#state.sys_mem_watermark*Total ->
			set_alarm(system_memory_high_watermark, []);
		    true ->
			clear_alarm(system_memory_high_watermark)
		end,

		%% Check if process data should be collected
		case State#state.sys_only of
		    false ->
			{Pid, Bytes} = get_worst_memory_user(),
			Threshold= State#state.proc_mem_watermark*Total,

			%% Check if process alarm should be set/cleared
			if
			    Bytes > Threshold ->
				set_alarm(process_memory_high_watermark,
					  Pid);
			    true ->
				clear_alarm(process_memory_high_watermark)
			end,
			
			State#state{mem_usage={Alloc, Total},
				    worst_mem_user={Pid, Bytes}};
		    true ->
			State#state{mem_usage={Alloc, Total}}
		end;
	    false ->
		State
	end,

    %% Then send a reply to all waiting clients, in preserved time order
    Worst = State2#state.worst_mem_user,
    SysMemUsage = get_ext_memory_usage(State2#state.os, {Alloc,Total}),
    reply(State2#state.pending, {Total,Alloc,Worst}, SysMemUsage),

    %% Last, if this was a periodic check, start a timer for the next
    %% one. New timeout = interval-time spent collecting,
    _ = case lists:member(reg, State#state.pending) of
            true ->
                Time = case State2#state.timeout - TimeSpent of
                           MS when MS<0 ->
                               0;
                           MS ->
                               MS
                       end,
                erlang:send_after(Time, self(), time_to_collect);
            false ->
                ignore
        end,
    {noreply, State2#state{wd_timer=undefined, pending=[]}};
handle_info({'EXIT', Pid, normal}, State) when is_pid(Pid) ->
    %% Temporary pid terminating when job is done
    {noreply, State};

%% Timeout during data collection
handle_info(reg_collection_timeout, State) ->

    %% Cancel memory collection (and as a security measure,
    %% also flush any collected_sys message)
    if
	State#state.port_mode -> State#state.pid ! cancel;
	true -> exit(State#state.pid, cancel)
    end,
    flush(collected_sys),

    %% Issue a warning message
    Str = "OS_MON (memsup) timeout, no data collected~n",
    error_logger:warning_msg(Str),

    %% Send a dummy reply to all waiting clients, preserving time order
    reply(State#state.pending,
	  dummy_reply(get_memory_data, State#state.sys_only),
	  dummy_reply(get_system_memory_data)),

    %% If it is a periodic check which has timed out, start a timer for
    %% the next one
    %% New timeout = interval-helper timeout
    _ = case lists:member(reg, State#state.pending) of
            true ->
                Time =
                case State#state.timeout-State#state.helper_timeout of
                    MS when MS<0 -> 0;
                    MS -> MS
                end,
                erlang:send_after(Time, self(), time_to_collect);
            false ->
                ignore
        end,
    {noreply, State#state{wd_timer=undefined, pending=[]}};
handle_info({'EXIT', Pid, cancel}, State) when is_pid(Pid) ->
    %% Temporary pid terminating as ordered
    {noreply, State};

%% Extensive memory data collected (port_mode==true only)
handle_info({collected_ext_sys, SysMemUsage}, State) ->

    %% Cancel watchdog timer (and as a security mearure,
    %% also flush any ext_collection_timeout message)
    ok = erlang:cancel_timer(State#state.ext_wd_timer, [{async,true}]),
    flush(ext_collection_timeout),

    %% Send the reply to all waiting clients, preserving time order
    reply(State#state.ext_pending, undef, SysMemUsage),

    {noreply, State#state{ext_wd_timer=undefined, ext_pending=[]}};

%% Timeout during ext memory data collection (port_mode==true only)
handle_info(ext_collection_timeout, State) ->

    %% Cancel memory collection (and as a security measure,
    %% also flush any collected_ext_sys message)
    State#state.pid ! ext_cancel,
    flush(collected_ext_sys),

    %% Issue a warning message
    Str = "OS_MON (memsup) timeout, no data collected~n",
    error_logger:warning_msg(Str),

    %% Send a dummy reply to all waiting clients, preserving time order
    SysMemUsage = dummy_reply(get_system_memory_data),
    reply(State#state.ext_pending, undef, SysMemUsage),

    {noreply, State#state{ext_wd_timer=undefined, ext_pending=[]}};

%% Error in data collecting (port connected or temporary) process
handle_info({'EXIT', Pid, Reason}, State) when is_pid(Pid) ->
    {stop, Reason, State};
	    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    if
	State#state.port_mode -> State#state.pid ! close;
	true -> ok
    end,
    clear_alarms(),
    ok.

%% os_mon-2.0.1
%% For live downgrade to/upgrade from os_mon-1.8[.1] and -2.0
code_change(Vsn, PrevState, "1.8") ->
    case Vsn of

	%% Downgrade from this version
	{down, _Vsn} ->

	    %% Kill the helper process, if there is one,
	    %% and flush messages from it
	    case PrevState#state.pid of
		Pid when is_pid(Pid) ->
		    unlink(Pid), % to prevent 'EXIT' message
		    exit(Pid, cancel);
		undefined -> ignore
	    end,
	    flush(collected_sys),
	    flush(collected_ext_sys),

	    %% Cancel timers, flush timeout messages
	    %% and send dummy replies to any pending clients
	    case PrevState#state.wd_timer of
		undefined ->
		    ignore;
		TimerRef1 ->
                    ok = erlang:cancel_timer(TimerRef1, [{async,true}]),
		    SysOnly = PrevState#state.sys_only,
		    MemUsage = dummy_reply(get_memory_data, SysOnly),
		    SysMemUsage1 = dummy_reply(get_system_memory_data),
		    reply(PrevState#state.pending,MemUsage,SysMemUsage1)
	    end,
	    case PrevState#state.ext_wd_timer of
		undefined ->
		    ignore;
		TimerRef2 ->
                    ok = erlang:cancel_timer(TimerRef2, [{async,true}]),
		    SysMemUsage2 = dummy_reply(get_system_memory_data),
		    reply(PrevState#state.pending, undef, SysMemUsage2)
	    end,
	    flush(reg_collection_timeout),
	    flush(ext_collection_timeout),

	    %% Downgrade to old state record
	    State = {state,
		     PrevState#state.timeout,
		     PrevState#state.mem_usage,
		     PrevState#state.worst_mem_user,
		     PrevState#state.sys_mem_watermark,
		     PrevState#state.proc_mem_watermark,
		     not PrevState#state.sys_only, % collect_procmem
		     undefined, % wd_timer
		     [],        % pending
		     undefined, % ext_wd_timer
		     [],        % ext_pending
		     PrevState#state.helper_timeout},
	    {ok, State};

	%% Upgrade to this version
	_Vsn ->

	    %% Old state record
	    {state,
	     Timeout, MemUsage, WorstMemUser,
	     SysMemWatermark, ProcMemWatermark, CollProc,
	     WDTimer, Pending, ExtWDTimer, ExtPending,
	     HelperTimeout} = PrevState,
	    SysOnly = not CollProc,

	    %% Flush memsup_helper messages
	    flush(collected_sys),
	    flush(collected_proc),
	    flush(collected_ext_sys),
		     
	    %% Cancel timers, flush timeout messages
	    %% and send dummy replies to any pending clients
	    case WDTimer of
		undefined ->
		    ignore;
		TimerRef1 ->
                    ok = erlang:cancel_timer(TimerRef1, [{async,true}]),
		    MemUsage = dummy_reply(get_memory_data, SysOnly),
		    Pending2 = lists:map(fun(From) -> {reg,From} end,
					 Pending),
		    reply(Pending2, MemUsage, undef)
	    end,
	    case ExtWDTimer of
		undefined ->
		    ignore;
		TimerRef2 ->
                    ok = erlang:cancel_timer(TimerRef2, [{async,true}]),
		    SysMemUsage = dummy_reply(get_system_memory_data),
		    ExtPending2 = lists:map(fun(From) -> {ext,From} end,
					    ExtPending),
		    reply(ExtPending2, undef, SysMemUsage)
	    end,
	    flush(reg_collection_timeout),
	    flush(ext_collection_timeout),

	    OS = os:type(),
	    PortMode = case OS of
			   {unix, darwin} -> false;
			   {unix, freebsd} -> false;
			   {unix, dragonfly} -> false;
			   {unix, linux} -> false;
			   {unix, openbsd} -> true;
			   {unix, netbsd} -> true;
			   {unix, sunos} -> true;
			   {win32, _OSname} -> false
		       end,
	    Pid = if
		      PortMode -> spawn_link(fun() -> port_init() end);
		      not PortMode -> undefined
		  end,

	    %% Upgrade to this state record
	    State = #state{os = OS,
			   port_mode = PortMode,
			   mem_usage = MemUsage,
			   worst_mem_user = WorstMemUser,
			   sys_only  = SysOnly,
			   timeout         = Timeout,
			   helper_timeout  = HelperTimeout,
			   sys_mem_watermark = SysMemWatermark,
			   proc_mem_watermark = ProcMemWatermark,
			   pid                = Pid,
			   wd_timer           = undefined,
			   ext_wd_timer       = undefined,
			   pending            = [],
			   ext_pending        = []},
	    {ok, State}
    end;
code_change(_Vsn, State, "2.0") ->

    %% Restart the port process (it must use new memsup code)
    Pid = case State#state.port_mode of
	      true ->
		  State#state.pid ! close,
		  spawn_link(fun() -> port_init() end);
	      false ->
		  State#state.pid
	  end,
    {ok, State#state{pid=Pid}};
	  
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Other exports
%%----------------------------------------------------------------------

format_status(_Opt, [_PDict, #state{timeout=Timeout, mem_usage=MemUsage,
				    worst_mem_user=WorstMemUser}]) ->
    {Allocated, Total} = MemUsage,
    WorstMemFormat = case WorstMemUser of
			 {Pid, Mem} ->
			     [{"Pid", Pid}, {"Memory", Mem}];
			 undefined ->
			     undefined
		     end,
    [{data, [{"Timeout", Timeout}]},
     {items, {"Memory Usage", [{"Allocated", Allocated},
			       {"Total", Total}]}},
     {items, {"Worst Memory User", WorstMemFormat}}].


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

%%-- Fetching kernel bit support ---------------------------------------

get_os_wordsize({unix, sunos}) ->
    String = clean_string(os:cmd("isainfo -b")),
    erlang:list_to_integer(String);
get_os_wordsize({unix, irix64})  -> 64;
get_os_wordsize({unix, irix})    -> 32;
get_os_wordsize({unix, linux})   -> get_os_wordsize_with_uname();
get_os_wordsize({unix, darwin})  -> get_os_wordsize_with_uname();
get_os_wordsize({unix, netbsd})  -> get_os_wordsize_with_uname();
get_os_wordsize({unix, freebsd}) -> get_os_wordsize_with_uname();
get_os_wordsize({unix, dragonfly}) -> get_os_wordsize_with_uname();
get_os_wordsize({unix, openbsd}) -> get_os_wordsize_with_uname();
get_os_wordsize(_)               -> unsupported_os.

get_os_wordsize_with_uname() ->
    String = clean_string(os:cmd("uname -m")),
    case String of
	"x86_64"  -> 64;
	"sparc64" -> 64;
	"amd64"   -> 64;
	"ppc64"   -> 64;
	_         -> 32
    end.

clean_string(String) -> lists:flatten(string:tokens(String,"\r\n\t ")).
    

%%--Replying to pending clients-----------------------------------------

reply(Pending, MemUsage, SysMemUsage) ->
    lists:foreach(fun(reg) ->
			  ignore;
		     ({reg, From}) ->
			  gen_server:reply(From, MemUsage);
		     ({ext, From}) ->
			  gen_server:reply(From, SysMemUsage)
		  end,
		  lists:reverse(Pending)).

%%--Collect memory data, no port----------------------------------------

%% get_memory_usage(OS) -> {Alloc, Total}

%% Darwin:
%% Uses vm_stat command.
get_memory_usage({unix,darwin}) ->
    Str1 = os:cmd("/usr/bin/vm_stat"),
    PageSize = 4096,

    {[Free],        Str2} = fread_value("Pages free:~d.", Str1),
    {[Active],      Str3} = fread_value("Pages active:~d.", Str2),
    {[Inactive],    Str4} = fread_value("Pages inactive:~d.", Str3),
    {[Speculative], Str5} = fread_value("Pages speculative:~d.", Str4),
    {[Wired],       _} = fread_value("Pages wired down:~d.", Str5),

    NMemUsed  = (Wired + Active + Inactive) * PageSize,
    NMemTotal = NMemUsed + (Free + Speculative) * PageSize,
    {NMemUsed,NMemTotal};

%% FreeBSD: Look in /usr/include/sys/vmmeter.h for the format of struct
%% vmmeter
get_memory_usage({unix,OSname}) when OSname == freebsd; OSname == dragonfly ->
    PageSize  = freebsd_sysctl("vm.stats.vm.v_page_size"),
    PageCount = freebsd_sysctl("vm.stats.vm.v_page_count"),
    FreeCount = freebsd_sysctl("vm.stats.vm.v_free_count"),
    NMemUsed  = (PageCount - FreeCount) * PageSize,
    NMemTotal = PageCount * PageSize,
    {NMemUsed, NMemTotal};

%% Win32: Find out how much memory is in use by asking
%% the os_mon_sysinfo process.
get_memory_usage({win32,_OSname}) ->
    [Result|_] = os_mon_sysinfo:get_mem_info(),
    {ok, [_MemLoad, TotPhys, AvailPhys,
	  _TotPage, _AvailPage, _TotV, _AvailV], _RestStr} =
	io_lib:fread("~d~d~d~d~d~d~d", Result),
    {TotPhys-AvailPhys, TotPhys}.

fread_value(Format, Str0) ->
    case io_lib:fread(Format, skip_to_eol(Str0)) of
    	{error, {fread, input}} -> {[0], Str0};
	{ok, Value, Str1} -> {Value, Str1}
    end.

skip_to_eol([]) -> [];
skip_to_eol([$\n | T]) -> T;
skip_to_eol([_ | T]) -> skip_to_eol(T).

freebsd_sysctl(Def) ->
    list_to_integer(os:cmd("/sbin/sysctl -n " ++ Def) -- "\n").

%% get_ext_memory_usage(OS, {Alloc, Total}) -> [{Tag, Bytes}]
get_ext_memory_usage(OS, {Alloc, Total}) ->
    case OS of
	{win32, _} ->
	    [{total_memory, Total}, {free_memory, Total-Alloc},
	     {system_total_memory, Total}];
	{unix, linux} ->
	    [{total_memory, Total}, {free_memory, Total-Alloc},
	     %% corr. unless setrlimit() set
	     {system_total_memory, Total}];
	{unix, freebsd} ->
	    [{total_memory, Total}, {free_memory, Total-Alloc},
	     {system_total_memory, Total}];
	{unix, dragonfly} ->
	    [{total_memory, Total}, {free_memory, Total-Alloc},
	     {system_total_memory, Total}];
	{unix, darwin} ->
	    [{total_memory, Total}, {free_memory, Total-Alloc},
	     {system_total_memory, Total}];
	_ -> % OSs using a port
	    dummy % not sent anyway
    end.

%%--Collect memory data, using port-------------------------------------

port_init() ->
    process_flag(trap_exit, true),
    Port = start_portprogram(),
    port_idle(Port).

start_portprogram() ->
    os_mon:open_port("memsup",[{packet,1}]).

%% The connected process loops are a bit awkward (several different
%% functions doing almost the same thing) as
%%   a) strategies for receiving regular memory data and extensive
%%      memory data are different
%%   b) memory collection can be cancelled, in which case the process
%%      should still wait for port response (which should come
%%      eventually!) but not receive any requests or cancellations
%%      meanwhile to prevent getting out of synch.
port_idle(Port) ->
    receive
	{Memsup, collect_sys} ->
	    Port ! {self(), {command, [?SHOW_MEM]}},
	    get_memory_usage(Port, undefined, Memsup);
	{Memsup, collect_ext_sys} ->
	    Port ! {self(), {command, [?SHOW_SYSTEM_MEM]}},
	    get_ext_memory_usage(Port, [], Memsup);
	cancel ->
	    %% Received after reply already has been delivered...
	    port_idle(Port);
	ext_cancel ->
	    %% Received after reply already has been delivered...
	    port_idle(Port);
	close ->
	    port_close(Port);
	{Port, {data, Data}} ->
	    exit({port_error, Data});
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_close(Port)
    end.

get_memory_usage(Port, Alloc, Memsup) ->
    receive
	{Port, {data, Data}} when Alloc==undefined ->
	    get_memory_usage(Port, erlang:list_to_integer(Data, 16), Memsup);
	{Port, {data, Data}} ->
	    Total = erlang:list_to_integer(Data, 16),
	    Memsup ! {collected_sys, {Alloc, Total}},
	    port_idle(Port);
	cancel ->
	    get_memory_usage_cancelled(Port, Alloc);
	close ->
	    port_close(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_close(Port)
    end.
get_memory_usage_cancelled(Port, Alloc) ->
    receive
	{Port, {data, _Data}} when Alloc==undefined ->
	    get_memory_usage_cancelled(Port, 0);
	{Port, {data, _Data}} ->
	    port_idle(Port);
	close ->
	    port_close(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_close(Port)
    end.

get_ext_memory_usage(Port, Accum, Memsup) ->
    Tab = [
	    {?MEM_SYSTEM_TOTAL,   system_total_memory},
	    {?MEM_TOTAL,          total_memory},
	    {?MEM_FREE,           free_memory},
	    {?MEM_BUFFERS,        buffered_memory},
	    {?MEM_CACHED,         cached_memory},
	    {?MEM_SHARED,         shared_memory},
	    {?MEM_LARGEST_FREE,   largest_free},
	    {?MEM_NUMBER_OF_FREE, number_of_free},
	    {?SWAP_TOTAL,	  total_swap},
	    {?SWAP_FREE,	  free_swap}
	],
    receive
	{Port, {data, [?SHOW_SYSTEM_MEM_END]}} ->
	    Memsup ! {collected_ext_sys, Accum},
	    port_idle(Port);
	{Port, {data, [Tag]}} ->
	    case lists:keysearch(Tag, 1, Tab) of
		{value, {Tag, ATag}} ->
		    get_ext_memory_usage(ATag, Port, Accum, Memsup);
		_ ->
		    exit({memsup_port_error, {Port,[Tag]}})
	    end;
	ext_cancel ->
	    get_ext_memory_usage_cancelled(Port);
	close ->
	    port_close(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_close(Port)
    end.
get_ext_memory_usage_cancelled(Port) ->
    Tab = [
	    {?MEM_SYSTEM_TOTAL,   system_total_memory},
	    {?MEM_TOTAL,          total_memory},
	    {?MEM_FREE,           free_memory},
	    {?MEM_BUFFERS,        buffered_memory},
	    {?MEM_CACHED,         cached_memory},
	    {?MEM_SHARED,         shared_memory},
	    {?MEM_LARGEST_FREE,   largest_free},
	    {?MEM_NUMBER_OF_FREE, number_of_free},
	    {?SWAP_TOTAL,	  total_swap},
	    {?SWAP_FREE,	  free_swap}
	],
    receive
	{Port, {data, [?SHOW_SYSTEM_MEM_END]}} ->
	    port_idle(Port);
	{Port, {data, [Tag]}} ->
	    case lists:keysearch(Tag, 1, Tab) of
		{value, {Tag, ATag}} ->
		    get_ext_memory_usage_cancelled(ATag, Port);
		_ ->
		    exit({memsup_port_error, {Port,[Tag]}})
	    end;
	close ->
	    port_close(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_close(Port)
    end.

get_ext_memory_usage(ATag, Port, Accum0, Memsup) ->
    receive
	{Port, {data, Data}} ->
	    Accum = [{ATag,erlang:list_to_integer(Data, 16)}|Accum0],
	    get_ext_memory_usage(Port, Accum, Memsup);
	cancel ->
	    get_ext_memory_usage_cancelled(ATag, Port);
	close ->
	    port_close(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_close(Port)
    end.
get_ext_memory_usage_cancelled(_ATag, Port) ->
    receive
	{Port, {data, _Data}} ->
	    get_ext_memory_usage_cancelled(Port);
	close ->
	    port_close(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_close(Port)
    end.

%%--Collect process data------------------------------------------------

%% get_worst_memory_user() -> {Pid, Bytes}
get_worst_memory_user()  ->
    get_worst_memory_user(processes(), self(), 0).

get_worst_memory_user([Pid|Pids], MaxPid, MaxMemBytes) ->
    case process_memory(Pid) of
	undefined ->
	    get_worst_memory_user(Pids, MaxPid, MaxMemBytes);
	MemoryBytes when MemoryBytes>MaxMemBytes ->
	    get_worst_memory_user(Pids, Pid, MemoryBytes);
	_MemoryBytes ->
	    get_worst_memory_user(Pids, MaxPid, MaxMemBytes)
    end;
get_worst_memory_user([], MaxPid, MaxMemBytes) ->
    {MaxPid, MaxMemBytes}.

process_memory(Pid) ->
    case process_info(Pid, memory) of
	{memory, Bytes} ->
	    Bytes;
	undefined -> % Pid must have died
	    undefined
    end.

%%--Alarm handling------------------------------------------------------

set_alarm(AlarmId, AlarmDescr) ->
    case get(AlarmId) of
	set ->
	    ok;
	undefined ->
	    alarm_handler:set_alarm({AlarmId, AlarmDescr}),
	    put(AlarmId, set)
    end.

clear_alarm(AlarmId) ->
    case get(AlarmId) of
	set ->
	    alarm_handler:clear_alarm(AlarmId),
	    erase(AlarmId);
	_ ->
	    ok
    end.

clear_alarms() ->
    lists:foreach(fun({system_memory_high_watermark = Id, set}) ->
			  alarm_handler:clear_alarm(Id);
		     ({process_memory_high_watermark = Id, set}) ->
			  alarm_handler:clear_alarm(Id);
		     (_Other) ->
			  ignore
		  end,
		  get()).

%%--Auxiliary-----------------------------------------------------------

%% Type conversions
minutes_to_ms(Minutes) -> trunc(60000*Minutes).
sec_to_ms(Sec) -> trunc(1000*Sec).
ms_to_sec(MS) -> MS div 1000.

flush(Msg) ->
    receive
	{Msg, _} -> true;
	Msg -> true
    after 0 ->
	    true
    end.
