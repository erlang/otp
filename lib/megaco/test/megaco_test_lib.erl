%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2020. All Rights Reserved.
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
%% Purpose: Lightweight test server
%%----------------------------------------------------------------------
%%

-module(megaco_test_lib).

%% -compile(export_all).

-export([
         log/4,
         error/3,

         sleep/1,
         hours/1, minutes/1, seconds/1,
         formated_timestamp/0, format_timestamp/1,

         skip/3,
         non_pc_tc_maybe_skip/4,
         os_based_skip/1,

         flush/0,
         still_alive/1,

         display_alloc_info/0,
         display_system_info/1, display_system_info/2, display_system_info/3,

         try_tc/6,

         prepare_test_case/5,

         proxy_start/1, proxy_start/2,

         mk_nodes/1,
         start_nodes/3, start_nodes/4,
         start_node/3,  start_node/4,

         stop_nodes/3,
         stop_node/3

        ]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([proxy_init/2]).

-include("megaco_test_lib.hrl").

-record('REASON', {mod, line, desc}).


%% ----------------------------------------------------------------
%% Time related function
%%

sleep(infinity) ->
    receive
    after infinity ->
            ok
    end;
sleep(MSecs) ->
    receive
    after trunc(MSecs) ->
            ok
    end,
    ok.


hours(N)   -> trunc(N * 1000 * 60 * 60).
minutes(N) -> trunc(N * 1000 * 60).
seconds(N) -> trunc(N * 1000).


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp(TS) ->
    megaco:format_timestamp(TS).


%% ----------------------------------------------------------------
%% Conditional skip of testcases
%%

non_pc_tc_maybe_skip(Config, Condition, File, Line) 
  when is_list(Config) andalso is_function(Condition) ->
    %% Check if we shall skip the skip
    case os:getenv("TS_OS_BASED_SKIP") of
	"false" ->
	    ok;
	_ ->
	    case lists:keysearch(ts, 1, Config) of
		{value, {ts, megaco}} ->
		    %% Always run the testcase if we are using our own 
		    %% test-server...
		    ok;
		_ ->
		    case (catch Condition()) of
			true ->
			    skip(non_pc_testcase, File, Line);
			_ ->
			    ok
		    end
	    end
    end.


%% The type and spec'ing is just to increase readability
-type os_family()  :: win32 | unix.
-type os_name()    :: atom().
-type os_version() :: string() | {non_neg_integer(),
                                  non_neg_integer(),
                                  non_neg_integer()}.
-type os_skip_check() :: fun(() -> boolean()) | 
                            fun((os_version()) -> boolean()).
-type skippable() :: any | [os_family() | 
                            {os_family(), os_name() |
                                          [os_name() | {os_name(), 
                                                        os_skip_check()}]}].

-spec os_based_skip(skippable()) -> boolean().

os_based_skip(any) ->
    true;
os_based_skip(Skippable) when is_list(Skippable) ->
    os_base_skip(Skippable, os:type());
os_based_skip(_Crap) ->
    false.

os_base_skip(Skippable, {OsFam, OsName}) ->
    os_base_skip(Skippable, OsFam, OsName);
os_base_skip(Skippable, OsFam) ->
    os_base_skip(Skippable, OsFam, undefined).

os_base_skip(Skippable, OsFam, OsName) -> 
    %% Check if the entire family is to be skipped
    %% Example: [win32, unix]
    case lists:member(OsFam, Skippable) of
        true ->
            true;
        false ->
            %% Example: [{unix, freebsd}] | [{unix, [freebsd, darwin]}]
            case lists:keysearch(OsFam, 1, Skippable) of
                {value, {OsFam, OsName}} ->
                    true;
                {value, {OsFam, OsNames}} when is_list(OsNames) ->
                    %% OsNames is a list of: 
                    %%    [atom()|{atom(), function/0 | function/1}]
                    case lists:member(OsName, OsNames) of
                        true ->
                            true;
                        false ->
                            os_based_skip_check(OsName, OsNames)
                    end;
                _ ->
                    false
            end
    end.



%% Performs a check via a provided fun with arity 0 or 1.
%% The argument is the result of os:version().
os_based_skip_check(OsName, OsNames) ->
    case lists:keysearch(OsName, 1, OsNames) of
        {value, {OsName, Check}} when is_function(Check, 0) ->
            Check();
        {value, {OsName, Check}} when is_function(Check, 1) ->
            Check(os:version());
        _ ->
            false
    end.

    
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluates a test case or test suite
%% Returns a list of failing test cases:
%% 
%%     {Mod, Fun, ExpectedRes, ActualRes}
%%----------------------------------------------------------------------

display_alloc_info() ->
    io:format("Allocator memory information:~n", []),
    AllocInfo = alloc_info(),
    display_alloc_info(AllocInfo).

display_alloc_info([]) ->
    ok;
display_alloc_info([{Alloc, Mem}|AllocInfo]) ->
    io:format("  ~15w: ~10w~n", [Alloc, Mem]),
    display_alloc_info(AllocInfo).
    
alloc_info() ->
    case erlang:system_info(allocator) of
	{_Allocator, _Version, Features, _Settings} ->
	    alloc_info(Features);
	_ ->
	    []
    end.

alloc_info(Allocators) ->
    Allocs = [temp_alloc, sl_alloc, std_alloc, ll_alloc, eheap_alloc, 
	      ets_alloc, binary_alloc, driver_alloc], 
    alloc_info(Allocators, Allocs, []).

alloc_info([], _, Acc) ->
    lists:reverse(Acc);
alloc_info([Allocator | Allocators], Allocs, Acc) ->
    case lists:member(Allocator, Allocs) of
	true ->
	    Instances0 = erlang:system_info({allocator, Allocator}),
	    Instances = 
		if 
		    is_list(Instances0) ->
			[Instance || Instance <- Instances0, 
				     element(1, Instance) =:= instance];
		    true ->
			[]
		end,
	    AllocatorMem = alloc_mem_info(Instances),
	    alloc_info(Allocators, Allocs, [{Allocator, AllocatorMem} | Acc]);

	false ->
	    alloc_info(Allocators, Allocs, Acc)
    end.
    
alloc_mem_info(Instances) ->
    alloc_mem_info(Instances, []).

alloc_mem_info([], Acc) ->
    lists:sum([Mem || {instance, _, Mem} <- Acc]);
alloc_mem_info([{instance, N, Info}|Instances], Acc) ->
    InstanceMemInfo = alloc_instance_mem_info(Info),
    alloc_mem_info(Instances, [{instance, N, InstanceMemInfo} | Acc]).

alloc_instance_mem_info(InstanceInfo) ->
    MBCS = alloc_instance_mem_info(mbcs, InstanceInfo),
    SBCS = alloc_instance_mem_info(sbcs, InstanceInfo),
    MBCS + SBCS.

alloc_instance_mem_info(Key, InstanceInfo) ->
    case lists:keysearch(Key, 1, InstanceInfo) of
	{value, {Key, Info}} ->
	    case lists:keysearch(blocks_size, 1, Info) of
		{value, {blocks_size, Mem, _, _}} ->
		    Mem;
		_ ->
		    0
	    end;
	_ ->
	    0
    end.
		      
    
display_system_info(WhenStr) ->
    display_system_info(WhenStr, undefined, undefined).

display_system_info(WhenStr, undefined, undefined) ->
    display_system_info(WhenStr, "");
display_system_info(WhenStr, Mod, Func) ->
    ModFuncStr = lists:flatten(io_lib:format(" ~w:~w", [Mod, Func])),
    display_system_info(WhenStr, ModFuncStr).

display_system_info(WhenStr, ModFuncStr) ->
    Fun = fun(F) -> case (catch F()) of
			{'EXIT', _} ->
			    undefined;
			Res ->
			    Res
		    end
	  end,
    ProcCount    = Fun(fun() -> erlang:system_info(process_count) end),
    ProcLimit    = Fun(fun() -> erlang:system_info(process_limit) end),
    ProcMemAlloc = Fun(fun() -> erlang:memory(processes) end),
    ProcMemUsed  = Fun(fun() -> erlang:memory(processes_used) end),
    ProcMemBin   = Fun(fun() -> erlang:memory(binary) end),
    ProcMemTot   = Fun(fun() -> erlang:memory(total) end),
    %% error_logger:info_msg(
    io:format("~n"
	      "~n*********************************************"
	      "~n"
	      "System info ~s~s => "
	      "~n   Process count:        ~w"
              "~n   Process limit:        ~w"
              "~n   Process memory alloc: ~w"
              "~n   Process memory used:  ~w"
              "~n   Memory for binaries:  ~w"
              "~n   Memory total:         ~w"
	      "~n"
	      "~n*********************************************"
	      "~n"
	      "~n", [WhenStr, ModFuncStr, 
		     ProcCount, ProcLimit, ProcMemAlloc, ProcMemUsed, 
		     ProcMemBin, ProcMemTot]),
    ok.


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify that the actual result of a test case matches the exected one
%% Returns the actual result
%% Stores the result in the process dictionary if mismatch

error(Actual, Mod, Line) ->
    global:send(megaco_global_logger, {failed, Mod, Line}),
    log("<ERROR> Bad result: ~p~n", [Actual], Mod, Line),
    Label = lists:concat([Mod, "(", Line, ") unexpected result"]),
    megaco:report_event(60, Mod, Mod, Label,
			[{line, Mod, Line}, {error, Actual}]),
    case global:whereis_name(megaco_test_case_sup) of
	undefined -> 
	    ignore;
	Pid -> 
	    Fail = #'REASON'{mod = Mod, line = Line, desc = Actual},
	    Pid ! {fail, self(), Fail}
    end,
    Actual.

log(Format, Args, Mod, Line) ->
    case global:whereis_name(megaco_global_logger) of
	undefined ->
	    io:format(user, "~p~p(~p): " ++ Format, 
		      [self(), Mod, Line] ++ Args);
	Pid ->
	    io:format(Pid, "~p~p(~p): " ++ Format, 
		      [self(), Mod, Line] ++ Args)
    end.

skip(Reason) ->
    exit({skip, Reason}).

skip(Actual, File, Line) ->
    log("Skipping test case: ~p~n", [Actual], File, Line),
    String = f("~p(~p): ~p~n", [File, Line, Actual]),
    skip(String).

fatal_skip(Actual, File, Line) ->
    error(Actual, File, Line),
    skip({fatal, Actual, File, Line}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Flush the message queue and return its messages

flush() ->
    receive
	Msg ->
	    [Msg | flush()]
    after 1000 ->
	    []
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check if process is alive and kicking

still_alive(Pid) ->   
    erlang:is_process_alive(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The proxy process

proxy_start(ProxyId) ->
    spawn_link(?MODULE, proxy_init, [ProxyId, self()]).

proxy_start(Node, ProxyId) ->
    spawn_link(Node, ?MODULE, proxy_init, [ProxyId, self()]).

proxy_init(ProxyId, Controller) ->
    process_flag(trap_exit, true),
    IdStr = proxyid2string(ProxyId),
    put(id, IdStr),
    ?LOG("[~s] proxy started by ~p~n", [IdStr, Controller]),
    proxy_loop(ProxyId, Controller).

proxy_loop(OwnId, Controller) ->
    receive
	{'EXIT', Controller, Reason} ->
	    pprint("proxy_loop -> received exit from controller"
                   "~n   Reason: ~p", [Reason]),
	    exit(Reason);
	{stop, Controller, Reason} ->
	    p("proxy_loop -> received stop from controller"
	      "~n   Reason: ~p"
	      "~n", [Reason]),
	    exit(Reason);
	
	{apply, Fun} ->
            pprint("proxy_loop -> received apply request"),
	    Res = Fun(),
            pprint("proxy_loop -> apply result: "
                   "~n   ~p", [Res]),
	    Controller ! {res, OwnId, Res},
	    proxy_loop(OwnId, Controller);
	OtherMsg ->
	    pprint("proxy_loop -> received unknown message: "
                   "~n  ~p", [OtherMsg]),
	    Controller ! {msg, OwnId, OtherMsg},
	    proxy_loop(OwnId, Controller)
    end.

proxyid2string(Id) when is_list(Id) ->
    Id;
proxyid2string(Id) when is_atom(Id) ->
    atom_to_list(Id);
proxyid2string(Id) ->
    f("~p", [Id]).

pprint(F) ->
    pprint(F, []).

pprint(F, A) ->
    io:format("[~s] ~p ~s " ++ F ++ "~n",
              [get(id), self(), formated_timestamp() | A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks

init_per_suite(Config) ->

    %% We have some crap machines that causes random test case failures
    %% for no obvious reason. So, attempt to identify those without actually
    %% checking for the host name...
    %% We have two "machines" we are checking for. Both are old installations
    %% running on really slow VMs (the host machines are old and tired).
    LinuxVersionVerify =
        fun(V) when (V > {3,6,11}) ->
                false; % OK - No skip
           (V) when (V =:= {3,6,11}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Fedora release 16 " ++ _ -> % Stone age Fedora => Skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V > {2,6,24}) ->
                false; % OK - No skip
           (_) ->
                %% We are specifically checking for
                %% a *really* old gento...
                case string:find(string:strip(os:cmd("uname -a")), "gentoo") of
                    nomatch ->
                        false;
                    _ -> % Stone age gentoo => Skip
                        true
                end
        end,
    DarwinVersionVerify =
        fun(V) when (V > {9, 8, 0}) ->
                %% This version is OK: No Skip
                false;
           (_V) ->
                %% This version is *not* ok: Skip
                true
        end,
    %% We are "looking" for a specific machine (a VM)
    %% which are *old and crappy" and slow, because it
    %% causes a bunch of test cases to fail randomly.
    %% But we don not want to test for the host name...
    %% WinVersionVerify =
    %%     fun(V) when (V =:= {6,2,9200}) ->
    %%             try erlang:system_info(schedulers) of
    %%                 2 ->
    %%                     true;
    %%                 _ ->
    %%                     false
    %%             catch
    %%                 _:_:_ ->
    %%                     true
    %%             end;
    %%        (_) ->
    %%             false
    %% end,
    COND = [
            {unix, [{linux,  LinuxVersionVerify}, 
		    {darwin, DarwinVersionVerify}]}%% ,
            %% {win32, [{nt, WinVersionVerify}]}
           ],
    case os_based_skip(COND) of
        true ->
            {skip, "Unstable host and/or os (or combo thererof)"};
        false ->
            Factor = analyze_and_print_host_info(),
            maybe_start_global_sys_monitor(Config),
            [{megaco_factor, Factor} | Config]
    end.

%% We start the global system monitor unless explicitly disabled
maybe_start_global_sys_monitor(Config) ->
    case lists:keysearch(sysmon, 1, Config) of
        {value, {sysmon, false}} ->
            ok;
        _ ->
            megaco_test_global_sys_monitor:start()
    end.

end_per_suite(Config) when is_list(Config) ->

    case lists:keysearch(sysmon, 1, Config) of
        {value, {sysmon, false}} ->
            ok;
        _ ->
            megaco_test_global_sys_monitor:stop()
    end,

    Config.


init_per_testcase(_Case, Config) ->
    Pid = group_leader(),
    Name = megaco_global_logger,
    case global:whereis_name(Name) of
	undefined ->
	    global:register_name(megaco_global_logger, Pid);
	Pid ->
	    io:format("~w:init_per_testcase -> "
		      "already registered to ~p~n", [?MODULE, Pid]),
	    ok;
	OtherPid when is_pid(OtherPid) ->
	    io:format("~w:init_per_testcase -> "
		      "already registered to other ~p (~p)~n", 
		      [?MODULE, OtherPid, Pid]),
	    exit({already_registered, {megaco_global_logger, OtherPid, Pid}})
    end,
    set_kill_timer(Config).

end_per_testcase(_Case, Config) ->
    Name = megaco_global_logger,
    case global:whereis_name(Name) of
	undefined ->
	    io:format("~w:end_per_testcase -> already un-registered~n", 
		      [?MODULE]),
	    ok;
	Pid when is_pid(Pid) ->
	    global:unregister_name(megaco_global_logger),
	    ok
    end,
    reset_kill_timer(Config).


%% This function prints various host info, which might be usefull
%% when analyzing the test suite (results).
%% It also returns a "factor" that can be used when deciding 
%% the load for some test cases. Such as run time or number of
%% iteraions. This only works for some OSes.
%%
%% We make some calculations on Linux, OpenBSD and FreeBSD.
%% On SunOS we always set the factor to 2 (just to be on the safe side)
%% On all other os:es (mostly windows) we check the number of schedulers,
%% but at least the factor will be 2.
analyze_and_print_host_info() ->
    {OsFam, OsName} = os:type(),
    Version         =
        case os:version() of
            {Maj, Min, Rel} ->
                f("~w.~w.~w", [Maj, Min, Rel]);
            VStr ->
                VStr
        end,
    case {OsFam, OsName} of
        {unix, linux} ->
            analyze_and_print_linux_host_info(Version);
        {unix, openbsd} ->
            analyze_and_print_openbsd_host_info(Version);
        {unix, freebsd} ->
            analyze_and_print_freebsd_host_info(Version);           
        {unix, netbsd} ->
            analyze_and_print_netbsd_host_info(Version);           
        {unix, sunos} ->
            analyze_and_print_solaris_host_info(Version);
        {win32, nt} ->
            analyze_and_print_win_host_info(Version);
        _ ->
            io:format("OS Family: ~p"
                      "~n   OS Type:        ~p"
                      "~n   Version:        ~p"
                      "~n   Num Schedulers: ~s"
                      "~n", [OsFam, OsName, Version, str_num_schedulers()]),
            try erlang:system_info(schedulers) of
                1 ->
                    10;
                2 ->
                    5;
                N when (N =< 6) ->
                    2;
                _ ->
                    1
            catch
                _:_:_ ->
                    10
            end
    end.

str_num_schedulers() ->
    try erlang:system_info(schedulers) of
        N -> f("~w", [N])
    catch
        _:_:_ -> "-"
    end.

    
analyze_and_print_linux_host_info(Version) ->
    case file:read_file_info("/etc/issue") of
        {ok, _} ->
            io:format("Linux: ~s"
                      "~n   ~s"
                      "~n",
                      [Version, string:trim(os:cmd("cat /etc/issue"))]);
        _ ->
            io:format("Linux: ~s"
                      "~n", [Version])
    end,
    Factor =
        case (catch linux_which_cpuinfo()) of
            {ok, {CPU, BogoMIPS}} ->
                io:format("CPU: "
                          "~n   Model:          ~s"
                          "~n   BogoMIPS:       ~s"
                          "~n   Num Schedulers: ~s"
                          "~n", [CPU, BogoMIPS, str_num_schedulers()]),
                %% We first assume its a float, and if not try integer
                try list_to_float(string:trim(BogoMIPS)) of
                    F when F > 4000 ->
                        1;
                    F when F > 1000 ->
                        2;
                    F when F > 500 ->
                        3;
                    _ ->
                        5
                catch
                    _:_:_ ->
                        try list_to_integer(string:trim(BogoMIPS)) of
                            I when I > 4000 ->
                                1;
                            I when I > 1000 ->
                                2;
                            I when I > 500 ->
                                3;
                            _ ->
                                5
                        catch
                            _:_:_ ->
                                5 % Be a "bit" conservative...
                        end
                end;
            {ok, CPU} ->
                io:format("CPU: "
                          "~n   Model:          ~s"
                          "~n   Num Schedulers: ~s"
                          "~n", [CPU, str_num_schedulers()]),
                2; % Be a "bit" conservative...
            _ ->
                5 % Be a "bit" (more) conservative...
        end,
    %% Check if we need to adjust the factor because of the memory
    try linux_which_meminfo() of
        AddFactor ->
            Factor + AddFactor
    catch
        _:_:_ ->
            Factor
    end.

linux_which_cpuinfo() ->
    %% Check for x86 (Intel or AMD)
    CPU =
        try [string:trim(S) || S <- string:tokens(os:cmd("grep \"model name\" /proc/cpuinfo"), [$:,$\n])] of
            ["model name", ModelName | _] ->
                ModelName;
            _ ->
                %% ARM (at least some distros...)
                try [string:trim(S) || S <- string:tokens(os:cmd("grep \"Processor\" /proc/cpuinfo"), [$:,$\n])] of
                    ["Processor", Proc | _] ->
                        Proc;
                    _ ->
                        %% Ok, we give up
                        throw(noinfo)
                catch
                    _:_:_ ->
                        throw(noinfo)
                end
        catch
            _:_:_ ->
                throw(noinfo)
        end,
    try [string:trim(S) || S <- string:tokens(os:cmd("grep -i \"bogomips\" /proc/cpuinfo"), [$:,$\n])] of
        [_, BMips | _] ->
            {ok, {CPU, BMips}};
        _ ->
            {ok, CPU}
    catch
        _:_:_ ->
            {ok, CPU}
    end.

%% We *add* the value this return to the Factor.
linux_which_meminfo() ->
    try [string:trim(S) || S <- string:tokens(os:cmd("grep MemTotal /proc/meminfo"), [$:])] of
        [_, MemTotal] ->
            io:format("Memory:"
                      "~n   ~s"
                      "~n", [MemTotal]),
            case string:tokens(MemTotal, [$ ]) of
                [MemSzStr, MemUnit] ->
                    MemSz2 = list_to_integer(MemSzStr),
                    MemSz3 = 
                        case string:to_lower(MemUnit) of
                            "kb" ->
                                MemSz2;
                            "mb" ->
                                MemSz2*1024;
                            "gb" ->
                                MemSz2*1024*1024;
                            _ ->
                                throw(noinfo)
                        end,
                    if
                        (MemSz3 >= 8388608) ->
                            0;
                        (MemSz3 >= 4194304) ->
                            1;
                        (MemSz3 >= 2097152) ->
                            3;
                        true ->
                            5
                    end;
                _X ->
                    0
            end;
        _ ->
            0
    catch
        _:_:_ ->
            0
    end.


%% Just to be clear: This is ***not*** scientific...
analyze_and_print_openbsd_host_info(Version) ->
    io:format("OpenBSD:"
              "~n   Version: ~p"
              "~n", [Version]),
    Extract =
        fun(Key) -> 
                string:tokens(string:trim(os:cmd("sysctl " ++ Key)), [$=])
        end,
    try
        begin
            CPU =
                case Extract("hw.model") of
                    ["hw.model", Model] ->
                        string:trim(Model);
                    _ ->
                        "-"
                end,
            CPUSpeed =
                case Extract("hw.cpuspeed") of
                    ["hw.cpuspeed", Speed] ->
                        list_to_integer(Speed);
                    _ ->
                        -1
                end,
            NCPU =
                case Extract("hw.ncpufound") of
                    ["hw.ncpufound", N] ->
                        list_to_integer(N);
                    _ ->
                        -1
                end,
            Memory =
                case Extract("hw.physmem") of
                    ["hw.physmem", PhysMem] ->
                        list_to_integer(PhysMem) div 1024;
                    _ ->
                        -1
                end,
            io:format("CPU:"
                      "~n   Model: ~s"
                      "~n   Speed: ~w"
                      "~n   N:     ~w"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n", [CPU, CPUSpeed, NCPU, Memory]),
            CPUFactor =
                if
                    (CPUSpeed =:= -1) ->
                        1;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 4) ->
                                1;
                            (NCPU >= 2) ->
                                2;
                            true ->
                                3
                        end;
                    true ->
                        if
                            (NCPU >= 4) ->
                                2;
                            (NCPU >= 2) ->
                                3;
                            true ->
                                4
                        end
                end,
            MemAddFactor =
                if
                    (Memory =:= -1) ->
                        0;
                    (Memory >= 8388608) ->
                        0;
                    (Memory >= 4194304) ->
                        1;
                    (Memory >= 2097152) ->
                        2;
                    true ->
                        3
                end,
            CPUFactor + MemAddFactor
        end
    catch
        _:_:_ ->
            1
    end.


analyze_and_print_freebsd_host_info(Version) ->
    io:format("FreeBSD:"
              "~n   Version: ~p"
              "~n", [Version]),
    %% This test require that the program 'sysctl' is in the path.
    %% First test with 'which sysctl', if that does not work
    %% try with 'which /sbin/sysctl'. If that does not work either,
    %% we skip the test...
    try
        begin
            SysCtl =
                case string:trim(os:cmd("which sysctl")) of
                    [] ->
                        case string:trim(os:cmd("which /sbin/sysctl")) of
                            [] ->
                                throw(sysctl);
                            SC2 ->
                                SC2
                        end;
                    SC1 ->
                        SC1
                end,
            Extract =
                fun(Key) ->
                        string:tokens(string:trim(os:cmd(SysCtl ++ " " ++ Key)),
                                      [$:])
                end,
            CPU      = analyze_freebsd_cpu(Extract),
            CPUSpeed = analyze_freebsd_cpu_speed(Extract),
            NCPU     = analyze_freebsd_ncpu(Extract),
            Memory   = analyze_freebsd_memory(Extract),
            io:format("CPU:"
                      "~n   Model:          ~s"
                      "~n   Speed:          ~w"
                      "~n   N:              ~w"
                      "~n   Num Schedulers: ~w"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n",
                      [CPU, CPUSpeed, NCPU,
                       erlang:system_info(schedulers), Memory]),
            CPUFactor =
                if
                    (CPUSpeed =:= -1) ->
                        1;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 4) ->
                                1;
                            (NCPU >= 2) ->
                                2;
                            true ->
                                3
                        end;
                    true ->
                        if
                            (NCPU =:= -1) ->
                                1;
                            (NCPU >= 4) ->
                                2;
                            (NCPU >= 2) ->
                                3;
                            true ->
                                4
                        end
                end,
            MemAddFactor =
                if
                    (Memory =:= -1) ->
                        0;
                    (Memory >= 8388608) ->
                        0;
                    (Memory >= 4194304) ->
                        1;
                    (Memory >= 2097152) ->
                        2;
                    true ->
                        3
                end,
            CPUFactor + MemAddFactor
        end
    catch
        _:_:_ ->
            io:format("CPU:"
                      "~n   Num Schedulers: ~w"
                      "~n", [erlang:system_info(schedulers)]),
            case erlang:system_info(schedulers) of
                1 ->
                    10;
                2 ->
                    5;
                _ ->
                    2
            end
    end.

analyze_freebsd_cpu(Extract) ->
    analyze_freebsd_item(Extract, "hw.model", fun(X) -> X end, "-").

analyze_freebsd_cpu_speed(Extract) ->
    analyze_freebsd_item(Extract,
                         "hw.clockrate",
                         fun(X) -> list_to_integer(X) end,
                         -1).

analyze_freebsd_ncpu(Extract) ->
    analyze_freebsd_item(Extract,
                         "hw.ncpu",
                         fun(X) -> list_to_integer(X) end,
                         -1).

analyze_freebsd_memory(Extract) ->
    analyze_freebsd_item(Extract,
                         "hw.physmem",
                         fun(X) -> list_to_integer(X) div 1024 end,
                         -1).

analyze_freebsd_item(Extract, Key, Process, Default) ->
    try
        begin
            case Extract(Key) of
                [Key, Model] ->
                    Process(string:trim(Model));
                _ ->
                    Default
            end
        end
    catch
        _:_:_ ->
            Default
    end.


analyze_and_print_netbsd_host_info(Version) ->
    io:format("NetBSD:"
              "~n   Version: ~p"
              "~n", [Version]),
    %% This test require that the program 'sysctl' is in the path.
    %% First test with 'which sysctl', if that does not work
    %% try with 'which /sbin/sysctl'. If that does not work either,
    %% we skip the test...
    try
        begin
            SysCtl =
                case string:trim(os:cmd("which sysctl")) of
                    [] ->
                        case string:trim(os:cmd("which /sbin/sysctl")) of
                            [] ->
                                throw(sysctl);
                            SC2 ->
                                SC2
                        end;
                    SC1 ->
                        SC1
                end,
            Extract =
                fun(Key) ->
                        [string:trim(S) ||
                            S <-
                                string:tokens(string:trim(os:cmd(SysCtl ++ " " ++ Key)),
                                              [$=])]
                end,
            CPU      = analyze_netbsd_cpu(Extract),
            Machine  = analyze_netbsd_machine(Extract),
            Arch     = analyze_netbsd_machine_arch(Extract),
            CPUSpeed = analyze_netbsd_cpu_speed(Extract),
            NCPU     = analyze_netbsd_ncpu(Extract),
            Memory   = analyze_netbsd_memory(Extract),
            io:format("CPU:"
                      "~n   Model:          ~s (~s, ~s)"
                      "~n   Speed:          ~w MHz"
                      "~n   N:              ~w"
                      "~n   Num Schedulers: ~w"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n",
                      [CPU, Machine, Arch, CPUSpeed, NCPU,
                       erlang:system_info(schedulers), Memory]),
            CPUFactor =
                if
                    (CPUSpeed =:= -1) ->
                        1;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 4) ->
                                1;
                            (NCPU >= 2) ->
                                2;
                            true ->
                                3
                        end;
                    true ->
                        if
                            (NCPU =:= -1) ->
                                1;
                            (NCPU >= 4) ->
                                2;
                            (NCPU >= 2) ->
                                3;
                            true ->
                                4
                        end
                end,
            MemAddFactor =
                if
                    (Memory =:= -1) ->
                        0;
                    (Memory >= 8388608) ->
                        0;
                    (Memory >= 4194304) ->
                        1;
                    (Memory >= 2097152) ->
                        2;
                    true ->
                        3
                end,
            CPUFactor + MemAddFactor
        end
    catch
        _:_:_ ->
            io:format("CPU:"
                      "~n   Num Schedulers: ~w"
                      "~n", [erlang:system_info(schedulers)]),
            case erlang:system_info(schedulers) of
                1 ->
                    10;
                2 ->
                    5;
                _ ->
                    2
            end
    end.

analyze_netbsd_cpu(Extract) ->
    analyze_netbsd_item(Extract, "hw.model", fun(X) -> X end, "-").

analyze_netbsd_machine(Extract) ->
    analyze_netbsd_item(Extract, "hw.machine", fun(X) -> X end, "-").

analyze_netbsd_machine_arch(Extract) ->
    analyze_netbsd_item(Extract, "hw.machine_arch", fun(X) -> X end, "-").

analyze_netbsd_cpu_speed(Extract) ->
    analyze_netbsd_item(Extract, "machdep.dmi.processor-frequency", 
                        fun(X) -> case string:tokens(X, [$\ ]) of
                                      [MHz, "MHz"] ->
                                          list_to_integer(MHz);
                                      _ ->
                                          -1
                                  end
                        end, "-").

analyze_netbsd_ncpu(Extract) ->
    analyze_netbsd_item(Extract,
                        "hw.ncpu",
                        fun(X) -> list_to_integer(X) end,
                        -1).

analyze_netbsd_memory(Extract) ->
    analyze_netbsd_item(Extract,
                        "hw.physmem64",
                        fun(X) -> list_to_integer(X) div 1024 end,
                        -1).

analyze_netbsd_item(Extract, Key, Process, Default) ->
    analyze_freebsd_item(Extract, Key, Process, Default).



analyze_and_print_solaris_host_info(Version) ->
    Release =
        case file:read_file_info("/etc/release") of
            {ok, _} ->
                case [string:trim(S) || S <- string:tokens(os:cmd("cat /etc/release"), [$\n])] of
                    [Rel | _] ->
                        Rel;
                    _ ->
                        "-"
                end;
            _ ->
                "-"
        end,
    %% Display the firmware device tree root properties (prtconf -b)
    Props = [list_to_tuple([string:trim(PS) || PS <- Prop]) ||
                Prop <- [string:tokens(S, [$:]) ||
                            S <- string:tokens(os:cmd("prtconf -b"), [$\n])]],
    BannerName = case lists:keysearch("banner-name", 1, Props) of
                     {value, {_, BN}} ->
                         string:trim(BN);
                     _ ->
                         "-"
                 end,
    InstructionSet =
        case string:trim(os:cmd("isainfo -k")) of
            "Pseudo-terminal will not" ++ _ ->
                "-";
            IS ->
                IS
        end,
    PtrConf = [list_to_tuple([string:trim(S) || S <- Items]) || Items <- [string:tokens(S, [$:]) || S <- string:tokens(os:cmd("prtconf"), [$\n])], length(Items) > 1],
    SysConf =
        case lists:keysearch("System Configuration", 1, PtrConf) of
            {value, {_, SC}} ->
                SC;
            _ ->
                "-"
        end,
    NumPhysProc =
        begin
            NPPStr = string:trim(os:cmd("psrinfo -p")),
            try list_to_integer(NPPStr) of
                _ ->
                    NPPStr
            catch
                _:_:_ ->
                    "-"
            end
        end,
    NumProc = try integer_to_list(length(string:tokens(os:cmd("psrinfo"), [$\n]))) of
                  NPStr ->
                      NPStr
              catch
                  _:_:_ ->
                      "-"
              end,
    MemSz =
        case lists:keysearch("Memory size", 1, PtrConf) of
            {value, {_, MS}} ->
                MS;
            _ ->
                "-"
        end,
    io:format("Solaris: ~s"
              "~n   Release:         ~s"
              "~n   Banner Name:     ~s"
              "~n   Instruction Set: ~s"
              "~n   CPUs:            ~s (~s)"
              "~n   System Config:   ~s"
              "~n   Memory Size:     ~s"
              "~n   Num Schedulers:  ~s"
              "~n~n", [Version, Release, BannerName, InstructionSet,
                       NumPhysProc, NumProc,
                       SysConf, MemSz,
                       str_num_schedulers()]),
    MemFactor =
        try string:tokens(MemSz, [$ ]) of
            [SzStr, "Mega" ++ _] ->
                try list_to_integer(SzStr) of
                    Sz when Sz > 8192 ->
                        0;
                    Sz when Sz > 4096 ->
                        1;
                    Sz when Sz > 2048 ->
                        2;
                    _ -> 
                        5
                catch
                    _:_:_ ->
                        10
                end;
            [SzStr, "Giga" ++ _] ->
                try list_to_integer(SzStr) of
                    Sz when Sz > 8 ->
                        0;
                    Sz when Sz > 4 ->
                        1;
                    Sz when Sz > 2 ->
                        2;
                    _ -> 
                        5
                catch
                    _:_:_ ->
                        10
                end;
            _ ->
                10
        catch
            _:_:_ ->
                10
        end,
    try erlang:system_info(schedulers) of
        1 ->
            10;
        2 ->
            5;
        N when (N =< 6) ->
            2;
        _ ->
            1
    catch
        _:_:_ ->
            10
    end + MemFactor.    


analyze_and_print_win_host_info(Version) ->
    SysInfo    = which_win_system_info(),
    OsName     = win_sys_info_lookup(os_name,             SysInfo),
    OsVersion  = win_sys_info_lookup(os_version,          SysInfo),
    SysMan     = win_sys_info_lookup(system_manufacturer, SysInfo),
    SysMod     = win_sys_info_lookup(system_model,        SysInfo),
    NumProcs   = win_sys_info_lookup(num_processors,      SysInfo),
    TotPhysMem = win_sys_info_lookup(total_phys_memory,   SysInfo),
    io:format("Windows: ~s"
              "~n   OS Version:             ~s (~p)"
              "~n   System Manufacturer:    ~s"
              "~n   System Model:           ~s"
              "~n   Number of Processor(s): ~s"
              "~n   Total Physical Memory:  ~s"
              "~n   Num Schedulers:         ~s"
              "~n", [OsName, OsVersion, Version,
		     SysMan, SysMod, NumProcs, TotPhysMem,
		     str_num_schedulers()]),
    MemFactor =
        try
            begin
                [MStr, MUnit|_] =
                    string:tokens(lists:delete($,, TotPhysMem), [$\ ]),
                case string:to_lower(MUnit) of
                    "gb" ->
                        try list_to_integer(MStr) of
                            M when M > 8 ->
                                0;
                            M when M > 4 ->
                                1;
                            M when M > 2 ->
                                2;
                            _ -> 
                                5
                        catch
                            _:_:_ ->
                                10
                        end;
                    "mb" ->
                        try list_to_integer(MStr) of
                            M when M > 8192 ->
                                0;
                            M when M > 4096 ->
                                1;
                            M when M > 2048 ->
                                2;
                            _ -> 
                                5
                        catch
                            _:_:_ ->
                                10
                        end;
                    _ ->
                        10
                end
            end
        catch
            _:_:_ ->
                10
        end,
    CPUFactor = 
        case erlang:system_info(schedulers) of
            1 ->
                10;
            2 ->
                5;
            _ ->
                2
        end,
    CPUFactor + MemFactor.

win_sys_info_lookup(Key, SysInfo) ->
    win_sys_info_lookup(Key, SysInfo, "-").

win_sys_info_lookup(Key, SysInfo, Def) ->
    case lists:keysearch(Key, 1, SysInfo) of
        {value, {Key, Value}} ->
            Value;
        false ->
            Def
    end.

%% This function only extracts the prop we actually care about!
which_win_system_info() ->
    SysInfo = os:cmd("systeminfo"),
    try process_win_system_info(string:tokens(SysInfo, [$\r, $\n]), [])
    catch
        _:_:_ ->
            io:format("Failed process System info: "
                      "~s~n", [SysInfo]),
            []
    end.

process_win_system_info([], Acc) ->
    Acc;
process_win_system_info([H|T], Acc) ->
    case string:tokens(H, [$:]) of
        [Key, Value] ->
            case string:to_lower(Key) of
                "os name" ->
                    process_win_system_info(T,
                                            [{os_name, string:trim(Value)}|Acc]);
                "os version" ->
                    process_win_system_info(T,
                                            [{os_version, string:trim(Value)}|Acc]);
                "system manufacturer" ->
                    process_win_system_info(T,
                                            [{system_manufacturer, string:trim(Value)}|Acc]);
                "system model" ->
                    process_win_system_info(T,
                                            [{system_model, string:trim(Value)}|Acc]);
                "processor(s)" ->
                    [NumProcStr|_] = string:tokens(Value, [$\ ]),
                    T2 = lists:nthtail(list_to_integer(NumProcStr), T),
                    process_win_system_info(T2,
                                            [{num_processors, NumProcStr}|Acc]);
                "total physical memory" ->
                    process_win_system_info(T,
                                            [{total_phys_memory, string:trim(Value)}|Acc]);
                _ ->
                    process_win_system_info(T, Acc)
            end;
        _ ->
            process_win_system_info(T, Acc)
    end.
                    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set kill timer

set_kill_timer(Config) ->
    case init:get_argument(megaco_test_timeout) of
	{ok, _} -> 
	    Config;
	_ ->
	    Time = 
		case lookup_config(tc_timeout, Config) of
		    [] ->
			timer:minutes(5);
		    ConfigTime when is_integer(ConfigTime) ->
			ConfigTime
		end,
	    Dog = test_server:timetrap(Time),
	    [{kill_timer, Dog}|Config]
		    
	    
    end.

reset_kill_timer(Config) ->
    case lists:keysearch(kill_timer, 1, Config) of
	{value, {kill_timer, Dog}} ->
	    test_server:timetrap_cancel(Dog), 
	    lists:keydelete(kill_timer, 1, Config);
	_ ->
	    Config
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_tc(TCName, Name, Verbosity, Pre, Case, Post)
  when is_function(Pre, 0)  andalso 
       is_function(Case, 1) andalso
       is_function(Post, 1) ->
    process_flag(trap_exit, true),
    put(verbosity, Verbosity),
    put(sname,     Name),
    put(tc,        TCName),
    p("try_tc -> starting: try pre"),
    try Pre() of
        State ->
            p("try_tc -> pre done: try test case"),
            try Case(State) of
                Res ->
                    p("try_tc -> test case done: try post"),
                    (catch Post(State)),
                    p("try_tc -> done"),
                    Res
            catch
                throw:{skip, _} = SKIP:_ ->
                    p("try_tc -> test case (throw) skip: try post"),
                    (catch Post(State)),
                    p("try_tc -> test case (throw) skip: done"),
                    SKIP;
                exit:{skip, _} = SKIP:_ ->
                    p("try_tc -> test case (exit) skip: try post"),
                    (catch Post(State)),
                    p("try_tc -> test case (exit) skip: done"),
                    SKIP;
                C:E:S ->
                    p("try_tc -> test case failed: try post"),
                    (catch Post(State)),
                    case megaco_test_global_sys_monitor:events() of
                        [] ->
                            p("try_tc -> test case failed: done"),
                            exit({case_catched, C, E, S});
                        SysEvs ->
                            p("try_tc -> test case failed with system event(s): "
                              "~n   ~p", [SysEvs]),
                            {skip, "TC failure with system events"}
                    end
            end
    catch
        throw:{skip, _} = SKIP:_ ->
            p("try_tc -> pre (throw) skip"),
            SKIP;
        exit:{skip, _} = SKIP:_ ->
            p("try_tc -> pre (exit) skip"),
            SKIP;
        C:E:S ->
            case megaco_test_global_sys_monitor:events() of
                [] ->
                    p("try_tc -> pre failed: done"),
                    exit({pre_catched, C, E, S});
                SysEvs ->
                    p("try_tc -> pre failed with system event(s): "
                      "~n   ~p", [SysEvs]),
                    {skip, "TC pre failure with system events"}
            end
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_test_case(Actions, N, Config, File, Line) ->
    OrigNodes = lookup_config(nodes, Config),
    TestNodes = lookup_config(nodenames, Config), %% For testserver
    This      = node(),
    SomeNodes = OrigNodes ++ (TestNodes -- OrigNodes),
    AllNodes  = [This | (SomeNodes -- [This])],
    Nodes     = pick_n_nodes(N, AllNodes, File, Line),
    start_nodes(Nodes, File, Line),
    do_prepare_test_case(Actions, Nodes, Config, File, Line).

do_prepare_test_case([init | Actions], Nodes, Config, File, Line) ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush(),
    do_prepare_test_case(Actions, Nodes, Config, File, Line);
do_prepare_test_case([{stop_app, App} | Actions], Nodes, Config, File, Line) ->
    _Res = rpc:multicall(Nodes, application, stop, [App]),
    do_prepare_test_case(Actions, Nodes, Config, File, Line);
do_prepare_test_case([], Nodes, _Config, _File, _Line) ->
    Nodes.

pick_n_nodes(all, AllNodes, _File, _Line) ->
    AllNodes;
pick_n_nodes(N, AllNodes, _File, _Line) 
  when is_integer(N) andalso (length(AllNodes) >= N) ->
    AllNodes -- lists:nthtail(N, AllNodes);
pick_n_nodes(N, AllNodes, File, Line) ->
    fatal_skip({too_few_nodes, N, AllNodes}, File, Line).
   
lookup_config(Key,Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value,{Key,Val}} ->
	    Val;
	_ ->
	    []
    end.

mk_nodes(N) when (N > 0) ->
    mk_nodes(N, []).

mk_nodes(0, Nodes) ->
    Nodes;
mk_nodes(N, []) ->
    mk_nodes(N - 1, [node()]);
mk_nodes(N, Nodes) when N > 0 ->
    Head = hd(Nodes),
    [Name, Host] = node_to_name_and_host(Head),
    Nodes ++ [mk_node(I, Name, Host) || I <- lists:seq(1, N)].

mk_node(N, Name, Host) ->
    list_to_atom(lists:concat([Name ++ integer_to_list(N) ++ "@" ++ Host])).
    
%% Returns [Name, Host]    
node_to_name_and_host(Node) ->
    string:tokens(atom_to_list(Node), [$@]).


start_nodes(Nodes, File, Line) when is_list(Nodes) ->
    start_nodes(Nodes, false, File, Line).

start_nodes(Nodes, Force, File, Line)
  when is_list(Nodes) andalso is_boolean(Force) ->
    start_nodes(Nodes, Force, File, Line, []).

start_nodes([], _Force, _File, _Line, _Started) ->
    ok;
start_nodes([Node|Nodes], Force, File, Line, Started) ->
    try start_node(Node, Force, true, File, Line) of
        ok ->
            start_nodes(Nodes, Force, File, Line, [Node|Started])
    catch
        exit:{skip, _} = SKIP:_ ->
            (catch stop_nodes(lists:reverse(Started), File, Line)),
            exit(SKIP);
        C:E:S ->
            (catch stop_nodes(lists:reverse(Started), File, Line)),
            erlang:raise(C, E, S)
    end.

start_node(Node, File, Line) ->
    start_node(Node, false, false, File, Line).

start_node(Node, Force, File, Line)
  when is_atom(Node) andalso is_boolean(Force) ->
    start_node(Node, Force, false, File, Line).

start_node(Node, Force, Retry, File, Line) ->
    case net_adm:ping(Node) of
        %% Do not require a *new* node
	pong when (Force =:= false) ->
            p("node ~p already running", [Node]),
	    ok;

        %% Do require a *new* node, so kill this one and try again
	pong when ((Force =:= true) andalso (Retry =:= true)) ->
            e("node ~p already running - kill and retry", [Node]),
            case stop_node(Node) of
                ok ->
                    start_node(Node, Force, false, File, Line);
                error ->
                    e("node ~p already running - failed kill (no retry)", [Node]),
                    fatal_skip({node_already_running, Node}, File, Line)
            end;

        %% Do require a *new* node, but no retry so give up and fail
        pong when (Force =:= true) ->
            e("node ~p already running", [Node]),
            fatal_skip({node_already_running, Node}, File, Line);

        % Not (yet) running
        pang ->
	    [Name, Host] = node_to_name_and_host(Node),
            Pa = filename:dirname(code:which(?MODULE)),
            Args = " -pa " ++ Pa ++
                " -s " ++ atom_to_list(megaco_test_sys_monitor) ++ " start" ++ 
                " -s global sync",
            p("try start node ~p", [Node]),
	    case slave:start_link(Host, Name, Args) of
		{ok, NewNode} when NewNode =:= Node ->
                    p("node ~p started - now set path, cwd and sync", [Node]),
		    Path = code:get_path(),
		    {ok, Cwd} = file:get_cwd(),
		    true = rpc:call(Node, code, set_path, [Path]),
		    ok = rpc:call(Node, file, set_cwd, [Cwd]),
		    true = rpc:call(Node, code, set_path, [Path]),
		    {_, []} = rpc:multicall(global, sync, []),
		    ok;
		Other ->
                    e("failed starting node ~p: ~p", [Node, Other]),
		    fatal_skip({cannot_start_node, Node, Other}, File, Line)
	    end
    end.


stop_nodes(Nodes, File, Line) when is_list(Nodes) ->
    stop_nodes(Nodes, [], File, Line).

stop_nodes([], [], _File, _Line) ->
    ok;
stop_nodes([], StillRunning, File, Line) ->
    e("Failed stopping nodes: "
      "~n   ~p", [StillRunning]),
    fatal_skip({failed_stop_nodes, lists:reverse(StillRunning)}, File, Line);
stop_nodes([Node|Nodes], Acc, File, Line) ->
    case stop_node(Node) of
        ok ->
            stop_nodes(Nodes, Acc, File, Line);
        error ->
            stop_nodes(Nodes, [Node|Acc], File, Line)
    end.
    

stop_node(Node, File, Line) when is_atom(Node) ->
    p("try stop node ~p", [Node]),
    case stop_node(Node) of
        ok ->
            ok;
        error ->
            fatal_skip({failed_stop_node, Node}, File, Line)
    end.

stop_node(Node) ->
    p("try stop node ~p", [Node]),
    erlang:monitor_node(Node, true),
    rpc:call(Node, erlang, halt, []),
    receive
        {nodedown, Node} ->
            ok
    after 10000 ->
            e("failed stop node ~p", [Node]),
            error
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

e(F, A) ->
    print("ERROR", F, A).

p(F) ->
    p(F, []).

p(F, A) ->
    print("INFO", F, A).

print(Pre, F, A) ->
    io:format("*** [~s] [~s] ~p " ++ F ++ "~n", [?FTS(), Pre, self() | A]).

