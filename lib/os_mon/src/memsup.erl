%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-moduledoc """
A Memory Supervisor Process

`memsup` is a process which supervises the memory usage for the system and for
individual processes. It is part of the OS_Mon application, see
[os_mon(6)](os_mon_app.md). Available for Unix and Windows.

Periodically performs a memory check:

- If more than a certain amount of available system memory is allocated, as
  reported by the underlying operating system, the alarm
  `{system_memory_high_watermark, []}` is set.
- If any Erlang process `Pid` in the system has allocated more than a certain
  amount of total system memory, the alarm
  `{process_memory_high_watermark, Pid}` is set.

Alarms are reported to the SASL alarm handler, see `m:alarm_handler`. To set an
alarm, `alarm_handler:set_alarm(Alarm)` is called where `Alarm` is either of the
alarms specified above.

The alarms are cleared automatically when the alarm cause is no longer valid.

The function [get_memory_data()](`get_memory_data/0`) can be used to retrieve
the result of the latest periodic memory check.

There is also a interface to system dependent memory data,
[get_system_memory_data()](`get_system_memory_data/0`). The result is highly
dependent on the underlying operating system and the interface is targeted
primarily for systems without virtual memory. However, the output on other
systems is still valid, although sparse.

A call to `get_system_memory_data/0` is more costly than a call to
`get_memory_data/0` as data is collected synchronously when this function is
called.

The total system memory reported under UNIX is the number of physical pages of
memory times the page size, and the available memory is the number of available
physical pages times the page size. This is a reasonable measure as swapping
should be avoided anyway, but the task of defining total memory and available
memory is difficult because of virtual memory and swapping.

[](){: #config }

## Configuration

The following configuration parameters can be used to change the default values
for time intervals and thresholds:

- **`memory_check_interval = int()>0`** - The time interval, in minutes, for the
  periodic memory check. The default is one minute.

- **`system_memory_high_watermark = float()`** - The threshold, as percentage of
  system memory, for how much system memory can be allocated before the
  corresponding alarm is set. The default is 0.80 (80%).

- **`process_memory_high_watermark = float()`** - The threshold, as percentage
  of system memory, for how much system memory can be allocated by one Erlang
  process before the corresponding alarm is set. The default is 0.05 (5%).

- **`memsup_helper_timeout = int()>0`** - A timeout, in seconds, for how long
  the `memsup` process should wait for a result from a memory check. If the
  timeout expires, a warning message `"OS_MON (memsup) timeout"` is issued via
  `error_logger` and any pending, synchronous client calls will return a dummy
  value. Normally, this situation should not occur. There have been cases on
  Linux, however, where the pseudo file from which system data is read is
  temporarily unavailable when the system is heavily loaded.

  The default is 30 seconds.

- **`memsup_system_only = bool()`** - Specifies whether the `memsup` process
  should only check system memory usage (`true`) or not. The default is `false`,
  meaning that information regarding both system memory usage and Erlang process
  memory usage is collected.

  It is recommended to set this parameter to `false` on systems with many
  concurrent processes, as each process memory check makes a traversal of the
  entire list of processes.

See [config(4)](`e:kernel:config.md`) for information about how to change the
value of configuration parameters.

## See Also

`m:alarm_handler`, [os_mon(3)](os_mon_app.md)
""".
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
	 terminate/2]).

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

-define(EXT_MEM_MAP, memsup_ext_memory_type_map__).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-doc false.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
get_os_wordsize() -> Wordsize

Returns the wordsize of the current running operating system.
""".
-spec get_os_wordsize() -> Wordsize when Wordsize :: 32 | 64 | unsupported_os.
get_os_wordsize() ->
    os_mon:call(memsup, get_os_wordsize, infinity).

-doc """
get_memory_data() -> {Total,Allocated,Worst}

Returns the result of the latest memory check, where `Total` is the total memory
size and `Allocated` the allocated memory size, in bytes.

`Worst` is the pid and number of allocated bytes of the largest Erlang process
on the node. If `memsup` should not collect process data, that is if the
configuration parameter `memsup_system_only` was set to `true`, `Worst` is
`undefined`.

The function is normally asynchronous in the sense that it does not invoke a
memory check, but returns the latest available value. The one exception if is
the function is called before a first memory check is finished, in which case it
does not return a value until the memory check is finished.

Returns `{0,0,{pid(),0}}` or `{0,0,undefined}` if `memsup` is not available, or
if all memory checks so far have timed out.
""".
-spec get_memory_data() -> {Total, Allocated, Worst} when
      Total :: integer(),
      Allocated :: integer(),
      Worst :: {Pid, PidAllocated} | undefined,
      Pid :: pid(),
      PidAllocated :: integer().
get_memory_data() ->
    os_mon:call(memsup, get_memory_data, infinity).

-doc """
get_system_memory_data() -> MemDataList

Invokes a memory check and returns the resulting, system dependent, data as a
list of tagged tuples, where `Tag` currently can be one of the following:

- **`total_memory`** - The total amount of memory available to the Erlang
  emulator, allocated and free. May or may not be equal to the amount of memory
  configured in the system.

- **`available_memory`** - Informs about the amount memory that is available for
  increased usage if there is an increased memory need. This value is not based
  on a calculation of the other provided values and should give a better value
  of the amount of memory that actually is available than calculating a value
  based on the other values reported. This value is currently only present on
  newer Linux kernels. If this value is not available on Linux, you can use the
  sum of `cached_memory`, `buffered_memory`, and `free_memory` as an
  approximation.

- **`free_memory`** - The amount of free memory available to the Erlang emulator
  for allocation.

- **`system_total_memory`** - The amount of memory available to the whole
  operating system. This may well be equal to `total_memory` but not
  necessarily.

- **`buffered_memory`** - The amount of memory the system uses for temporary
  storing raw disk blocks.

- **`cached_memory`** - The amount of memory the system uses for cached files
  read from disk. On Linux, also memory marked as reclaimable in the kernel slab
  allocator will be added to this value.

- **`total_swap`** - The amount of total amount of memory the system has
  available for disk swap.

- **`free_swap`** - The amount of memory the system has available for disk swap.

> #### Note {: .info }
>
> Note that new tagged tuples may be introduced in the result at any time
> without prior notice

Note that the order of the tuples in the resulting list is undefined and may
change at any time.

All memory sizes are presented as number of _bytes_.

Returns the empty list [] if `memsup` is not available, or if the memory check
times out.
""".
-spec get_system_memory_data() -> MemDataList when
      MemDataList :: [{Tag, Size}],
      Tag :: atom(),
      Size :: integer().
get_system_memory_data() ->
    os_mon:call(memsup, get_system_memory_data, infinity).

-doc """
get_check_interval() -> MS

Returns the time interval, in milliseconds, for the periodic memory check.
""".
-spec get_check_interval() -> Milliseconds :: integer().
get_check_interval() ->
    os_mon:call(memsup, get_check_interval, infinity).
-doc """
set_check_interval(Minutes) -> ok

Changes the time interval, given in minutes, for the periodic memory check.

The change will take effect after the next memory check and is non-persistent.
That is, in case of a process restart, this value is forgotten and the default
value will be used. See [Configuration](`m:memsup#config`) above.
""".
-spec set_check_interval(Minutes :: non_neg_integer()) -> ok.
set_check_interval(Minutes) ->
    case param_type(memory_check_interval, Minutes) of
	true ->
	    MS = minutes_to_ms(Minutes), % for backwards compatibility
	    os_mon:call(memsup, {set_check_interval, MS}, infinity);
	false ->
	    erlang:error(badarg)
    end.

-doc """
get_procmem_high_watermark() -> int()

Returns the threshold, in percent, for process memory allocation.
""".
-spec get_procmem_high_watermark() -> integer().
get_procmem_high_watermark() ->
    os_mon:call(memsup, get_procmem_high_watermark, infinity).
-doc """
set_procmem_high_watermark(Float) -> ok

Changes the threshold, given as a float, for process memory allocation.

The change will take effect during the next periodic memory check and is
non-persistent. That is, in case of a process restart, this value is forgotten
and the default value will be used. See [Configuration](`m:memsup#config`)
above.
""".
-spec set_procmem_high_watermark(Float :: term()) -> ok.
set_procmem_high_watermark(Float) ->
    case param_type(process_memory_high_watermark, Float) of
	true ->
	    os_mon:call(memsup, {set_procmem_high_watermark, Float},
			infinity);
	false ->
	    erlang:error(badarg)
    end.

-doc """
get_sysmem_high_watermark() -> int()

Returns the threshold, in percent, for system memory allocation.
""".
-spec get_sysmem_high_watermark() -> integer().
get_sysmem_high_watermark() ->
    os_mon:call(memsup, get_sysmem_high_watermark, infinity).
-doc """
set_sysmem_high_watermark(Float) -> ok

Changes the threshold, given as a float, for system memory allocation.

The change will take effect during the next periodic memory check and is
non-persistent. That is, in case of a process restart, this value is forgotten
and the default value will be used. See [Configuration](`m:memsup#config`)
above.
""".
-spec set_sysmem_high_watermark(Float :: term()) -> ok.
set_sysmem_high_watermark(Float) ->
    case param_type(system_memory_high_watermark, Float) of
	true ->
	    os_mon:call(memsup, {set_sysmem_high_watermark, Float},
			infinity);
	false ->
	    erlang:error(badarg)
    end.

-doc """
get_helper_timeout() -> Seconds

Returns the timeout value, in seconds, for memory checks.
""".
-spec get_helper_timeout() -> Seconds :: integer().
get_helper_timeout() ->
    os_mon:call(memsup, get_helper_timeout, infinity).
-doc """
set_helper_timeout(Seconds) -> ok

Changes the timeout value, given in seconds, for memory checks.

The change will take effect for the next memory check and is non-persistent.
That is, in the case of a process restart, this value is forgotten and the
default value will be used. See [Configuration](`m:memsup#config`) above.
""".
-spec set_helper_timeout(Seconds :: non_neg_integer()) -> ok.
set_helper_timeout(Seconds) ->
    case param_type(memsup_helper_timeout, Seconds) of
	true ->
	    os_mon:call(memsup, {set_helper_timeout, Seconds}, infinity);
	false ->
	    erlang:error(badarg)
    end.

-doc false.
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

-doc false.
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

-doc false.
param_default(memsup_system_only) -> false;
param_default(memory_check_interval) -> 1;
param_default(memsup_helper_timeout) -> 30;
param_default(system_memory_high_watermark) -> 0.80;
param_default(process_memory_high_watermark) -> 0.05.

%%----------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------

-doc false.
init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),

    OS = os:type(),
    PortMode = case OS of
		   {unix, darwin} -> true;
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

    persistent_term:put(?EXT_MEM_MAP,
                        #{?MEM_SYSTEM_TOTAL => system_total_memory,
                          ?MEM_TOTAL => total_memory,
                          ?MEM_AVAIL => available_memory,
                          ?MEM_FREE => free_memory,
                          ?MEM_BUFFERS => buffered_memory,
                          ?MEM_CACHED => cached_memory,
                          %% When included cached_x is same as cached...
                          ?MEM_CACHED_X => cached_memory, 
                          ?MEM_SHARED => shared_memory,
                          ?MEM_LARGEST_FREE => largest_free,
                          ?MEM_NUMBER_OF_FREE => number_of_free,
                          ?SWAP_TOTAL => total_swap,
                          ?SWAP_FREE => free_swap}),

    %% Initiate first data collection
    self() ! time_to_collect,

    {ok, #state{os=OS, port_mode=PortMode,

		sys_only           = SysOnly,
		timeout            = minutes_to_ms(Timeout),
		helper_timeout     = sec_to_ms(HelperTimeout),
		sys_mem_watermark  = SysMem,
		proc_mem_watermark = ProcMem,

		pid=Pid}}.

-doc false.
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

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% It's time to check memory
-doc false.
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

-doc false.
terminate(_Reason, State) ->
    if
	State#state.port_mode -> State#state.pid ! close;
	true -> ok
    end,
    clear_alarms(),
    ok.

%%----------------------------------------------------------------------
%% Other exports
%%----------------------------------------------------------------------

-doc false.
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
	"ppc64le" -> 64;
	"s390x"   -> 64;
        "aarch64" -> 64;                        %Linux
        "arm64"   -> 64;                        %macOS
	_         -> 32
    end.

clean_string(String) -> lists:flatten(string:lexemes(String,[[$\r,$\n]|"\n\t "])).
    

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

port_shutdown(Port) ->
    Port ! {self(), {command, [?EXIT]}},
    Port ! {self(), close}.

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
	    get_ext_memory_usage(Port, #{}, Memsup);
	cancel ->
	    %% Received after reply already has been delivered...
	    port_idle(Port);
	ext_cancel ->
	    %% Received after reply already has been delivered...
	    port_idle(Port);
	close ->
	    port_shutdown(Port);
	{Port, {data, Data}} ->
	    exit({port_error, Data});
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_shutdown(Port)
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
	    port_shutdown(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_shutdown(Port)
    end.
get_memory_usage_cancelled(Port, Alloc) ->
    receive
	{Port, {data, _Data}} when Alloc==undefined ->
	    get_memory_usage_cancelled(Port, 0);
	{Port, {data, _Data}} ->
	    port_idle(Port);
	close ->
	    port_shutdown(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_shutdown(Port)
    end.

tag2atag(Port, Tag) ->
    Tab = persistent_term:get(?EXT_MEM_MAP),
    try
        maps:get(Tag, Tab)
    catch
        error:_ ->
            exit({memsup_port_error, {Port,[Tag]}})
    end.

get_ext_memory_usage(Port, Accum, Memsup) ->
    receive
	{Port, {data, [?SHOW_SYSTEM_MEM_END]}} ->
	    Memsup ! {collected_ext_sys, maps:to_list(Accum)},
	    port_idle(Port);
	{Port, {data, [Tag]}} ->
            get_ext_memory_usage(tag2atag(Port, Tag), Port, Accum,
                                 Memsup);
	ext_cancel ->
	    get_ext_memory_usage_cancelled(Port);
	close ->
	    port_shutdown(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_shutdown(Port)
    end.
get_ext_memory_usage_cancelled(Port) ->
    receive
	{Port, {data, [?SHOW_SYSTEM_MEM_END]}} ->
	    port_idle(Port);
	{Port, {data, [Tag]}} ->
            get_ext_memory_usage_cancelled(tag2atag(Port, Tag),
                                           Port);
	close ->
	    port_shutdown(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_shutdown(Port)
    end.

get_ext_memory_usage(ATag, Port, Accum0, Memsup) ->
    receive
	{Port, {data, Data}} ->
            Value = erlang:list_to_integer(Data, 16),
            Accum = case maps:get(ATag, Accum0, undefined) of
                        undefined ->
                            maps:put(ATag, Value, Accum0);
                        PrevValue ->
                            maps:put(ATag, Value + PrevValue, Accum0)
                    end,
	    get_ext_memory_usage(Port, Accum, Memsup);
	cancel ->
	    get_ext_memory_usage_cancelled(ATag, Port);
	close ->
	    port_shutdown(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_shutdown(Port)
    end.
get_ext_memory_usage_cancelled(_ATag, Port) ->
    receive
	{Port, {data, _Data}} ->
	    get_ext_memory_usage_cancelled(Port);
	close ->
	    port_shutdown(Port);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason});
	{'EXIT', _Memsup, _Reason} ->
	    port_shutdown(Port)
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
