%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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

-compile({no_auto_import, [error/3]}).

-export([
         proxy_call/3,
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

         executor/1, executor/2,
         try_tc/6, try_tc/7,

         prepare_test_case/5,

         proxy_start/1, proxy_start/2,

         mk_nodes/1,
         start_nodes/3, start_nodes/4,
         start_node/3,  start_node/4,

         stop_nodes/3,
         stop_node/3,
         ping/1, ping/2,

	 which_inet_backend/1,
         is_socket_backend/1,
         inet_backend_opts/1,
         explicit_inet_backend/0, test_inet_backends/0,
         open/3,
         listen/3, connect/3,

         megaco_trace/2,
         enable_trace/3

        ]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([proxy_init/2]).

%% Convenient exports...
-export([analyze_and_print_host_info/0]).

-include("megaco_test_lib.hrl").

%% -record('REASON', {mod, line, desc}).


%% ----------------------------------------------------------------
%% Proxy Call
%% This is used when we need to assign a timeout to a call, but the
%% call itself does not provide such an argument.
%%
%% This has nothing to to with the proxy_start and proxy_init
%% functions below.

proxy_call(F, Timeout, Default)
  when is_function(F, 0) andalso
       is_integer(Timeout) andalso (Timeout > 0) andalso
       is_function(Default, 0) ->
    {P, M} = erlang:spawn_monitor(fun() -> exit(F()) end),
    receive
        {'DOWN', M, process, P, Reply} ->
            Reply
    after Timeout ->
            erlang:demonitor(M, [flush]),
            exit(P, kill),
            Default()
    end;
proxy_call(F, Timeout, Default) ->
    proxy_call(F, Timeout, fun() -> Default end).


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
		{value, {OsFam, Check}} when is_function(Check, 0) ->
		    Check();
		{value, {OsFam, Check}} when is_function(Check, 1) ->
		    Check(os:version());
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
    log("<ERROR> Bad result: ~p~n", [Actual], Mod, Line),
    Label = lists:concat([Mod, "(", Line, ") unexpected result"]),
    megaco:report_event(60, Mod, Mod, Label,
			[{line, Mod, Line}, {error, Actual}]),
    exit(Actual).

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

    p("megaco environment: "
      "~n   (megaco) app:  ~p"
      "~n   (all)    init: ~p"
      "~n   (megaco) init: ~p",
      [application:get_all_env(megaco),
       init:get_arguments(),
       case init:get_argument(megaco) of
           {ok, Args} -> Args;
           error -> undefined
       end]),

    ct:timetrap(minutes(3)),

    try analyze_and_print_host_info() of
        {Factor, HostInfo} when is_integer(Factor) ->
            try maybe_skip(HostInfo) of
                true ->
                    {skip, "Unstable host and/or os (or combo thererof)"};
                false ->
                    maybe_start_global_sys_monitor(Config),
                    maybe_disable_trace([{megaco_factor, Factor} | Config])
            catch
                throw:{skip, _} = SKIP ->
                    SKIP
            end
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.


%% For tace to work, we need the 'et' app.
%% Specifically, we need the et_selector module,
%% so check if that module can be found!
maybe_disable_trace(Config) ->
    case code:ensure_loaded(et_selector) of
        {error, _} ->
            [{megaco_trace, disable} | Config];
        _ ->
            Config
    end.

maybe_skip(_HostInfo) ->

    %% We have some crap machines that causes random test case failures
    %% for no obvious reason. So, attempt to identify those without actually
    %% checking for the host name...

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
           (V) when (V =:= {3,4,20}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Wind River Linux 5.0.1.0" ++ _ -> % *Old* Wind River => skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V =:= {2,6,32}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Debian GNU/Linux 6.0 " ++ _ -> % Stone age Debian => Skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V =:= {2,6,16}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    %% Stone age SLES => Skip
                    %% We have atleast one VM that has this version,
                    %% and it causes randome timeout glitches...
                    "Welcome to SUSE Linux Enterprise Server 10 SP1 " ++ _ ->
                        true;
                    _ ->
                        false
                end;
           (V) when (V =:= {2,6,10}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "MontaVista" ++ _ -> % Stone age MontaVista => Skip
                        %% The real problem is that the machine is *very* slow
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
    SkipWindowsOnVirtual =
        %% fun() ->
        %%         SysMan = win_sys_info_lookup(system_manufacturer, HostInfo),
        %%         case string:to_lower(SysMan) of
        %%             "vmware" ++ _ ->
        %%                 true;
        %%             _ ->
        %%                 false
        %%         end
        %% end,
        fun() ->
                %% The host has been replaced and the VM has been reinstalled
                %% so for now we give it a chance...
                false
        end,
    COND = [
            {unix, [{linux,  LinuxVersionVerify}, 
		    {darwin, DarwinVersionVerify}]},
            {win32, SkipWindowsOnVirtual}
           ],
    os_based_skip(COND).

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
        {unix, darwin} ->
            analyze_and_print_darwin_host_info(Version);
        {unix, sunos} ->
            analyze_and_print_solaris_host_info(Version);
        {win32, nt} ->
            analyze_and_print_win_host_info(Version);
        _ ->
            io:format("OS Family: ~p"
                      "~n   OS Type:               ~p"
                      "~n   Version:               ~p"
                      "~n   Num Online Schedulers: ~s"
                      "~n", [OsFam, OsName, Version, str_num_schedulers()]),
            {num_schedulers_to_factor(), []}
    end.

str_num_schedulers() ->
    try erlang:system_info(schedulers_online) of
        N -> f("~w", [N])
    catch
        _:_:_ -> "-"
    end.

num_schedulers_to_factor() ->
    try erlang:system_info(schedulers_online) of
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
    end.
    

ts_extra_platform_label() ->
    case os:getenv("TS_EXTRA_PLATFORM_LABEL") of
        false -> "-";
        Val   -> Val
    end.

ts_scale_factor() ->
    case timetrap_scale_factor() of
        N when is_integer(N) andalso (N > 0) ->
            N - 1;
        _ ->
            0
    end.

simplify_label("Systemtap" ++ _) ->
    {host, systemtap};
simplify_label("Meamax" ++ _) ->
    {host, meamax};
simplify_label("Cover" ++ _) ->
    {host, cover};
simplify_label(Label) ->
    case string:find(string:to_lower(Label), "docker") of
        "docker" ++ _ ->
            docker;
        _ ->
            {host, undefined}
    end.

label2factor(docker) ->
    4;
label2factor({host, meamax}) ->
    2;
label2factor({host, cover}) ->
    6;
label2factor({host, _}) ->
    0.

linux_which_distro(Version) ->
    Label = ts_extra_platform_label(),
    Checks =
        [fun() -> do_linux_which_distro_os_release(Version,     Label) end,
         fun() -> do_linux_which_distro_suse_release(Version,   Label) end,
         fun() -> do_linux_which_distro_fedora_release(Version, Label) end,
         fun() -> do_linux_which_distro_issue(Version,          Label) end],
    try linux_which_distro("", Version, Label, Checks)
    catch
        throw:{distro, Distro} ->
            Distro
    end.

linux_which_distro("", Version, Label, []) ->
    io:format("Linux: ~s"
              "~n   Label:        ~s"
              "~n   Product Name: ~s"
              "~n", [Version, Label,
                     linux_product_name()]),    
    {other, simplify_label(Label)};
linux_which_distro(DestroStr, Version, Label, []) ->
    io:format("Linux: ~s"
              "~n   Distro:       ~s"
              "~n   Label:        ~s"
              "~n   Product Name: ~s"
              "~n", [Version, DestroStr, Label,
                     linux_product_name()]),    
    {other, simplify_label(Label)};
linux_which_distro(Default, Version, Label, [Check|Checks]) ->
    try Check() of
        DistroStr when is_list(DistroStr) ->
            linux_which_distro(DistroStr, Version, Label, Checks);
        retry ->
            linux_which_distro(Default, Version, Label, Checks);
        {error, _Reason} ->
            linux_which_distro(Default, Version, Label, Checks)
    catch
        throw:{error, _Reason} ->
	    linux_which_distro(Default, Version, Label, Checks)
    end.
       

do_linux_which_distro_os_release(Version, Label) ->
    case file:read_file_info("/etc/os-release") of
	{ok, _} ->
            %% We want to 'catch' if our processing is wrong,
            %% that's why we catch and re-throw the distro.
            %% Actual errors will be returned as 'ignore'.
            try
                begin
                    Info = linux_process_os_release(),
                    {value, {_, DistroStr}} = lists:keysearch(name, 1, Info),
                    {value, {_, VersionNo}} = lists:keysearch(version, 1, Info),
                    io:format("Linux: ~s"
                              "~n   Distro:                  ~s"
                              "~n   Distro Version:          ~s"
                              "~n   TS Extra Platform Label: ~s"
                              "~n   Product Name:            ~s"
                              "~n",
                              [Version, DistroStr, VersionNo, Label,
                               linux_product_name()]),
                    throw({distro,
                           {linux_distro_str_to_distro_id(DistroStr),
                            simplify_label(Label)}})
                end
            catch
                throw:{distro, _} = DISTRO ->
                    throw(DISTRO);
                _:_ ->
                    retry
            end;
        _ ->
            retry
    end.
	    

linux_process_os_release() ->
    %% Read the "raw" file
    Raw = os:cmd("cat /etc/os-release"),
    %% Split it into lines
    Lines1 = string:tokens(Raw, [$\n]),
    %% Just in case, skip any lines starting with '#'.
    Lines2 = linux_process_os_release1(Lines1),
    %% Each (remaining) line *should* be: <TAG>=<VALUE>
    %% Both sides will be strings, the value side will be a quoted string...
    %% Convert those into a 2-tuple list: [{Tag, Value}]
    linux_process_os_release2(Lines2).

linux_process_os_release1(Lines) ->
    linux_process_os_release1(Lines, []).

linux_process_os_release1([], Acc) ->
    lists:reverse(Acc);
linux_process_os_release1([H|T], Acc) ->
    case H of
        "#" ++ _ ->
            linux_process_os_release1(T, Acc);
        _ ->
            linux_process_os_release1(T, [H|Acc])
    end.

linux_process_os_release2(Lines) ->
    linux_process_os_release2(Lines, []).

linux_process_os_release2([], Acc) ->
    lists:reverse(Acc);
linux_process_os_release2([H|T], Acc) ->
    case linux_process_os_release3(H) of
        {value, Value} ->
            linux_process_os_release2(T, [Value|Acc]);
        false ->
            linux_process_os_release2(T, Acc)
    end.

linux_process_os_release3(H) ->
    case [string:strip(S) || S <- string:tokens(H, [$=])] of
        [Tag, Value] ->
            Tag2   = list_to_atom(string:to_lower(Tag)),
            Value2 = string:strip(Value, both, $"),
            linux_process_os_release4(Tag2, Value2);
        _ ->
            false
    end.

linux_process_os_release4(name = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(version = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(version_id = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(id = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(pretty_name = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(_Tag, _Value) ->
    false.

linux_distro_str_to_distro_id("Debian" ++ _) ->
    debian;
linux_distro_str_to_distro_id("Fedora" ++ _) ->
    fedora;
linux_distro_str_to_distro_id("Linux Mint" ++ _) ->
    linux_mint;
linux_distro_str_to_distro_id("MontaVista" ++ _) ->
    montavista;
linux_distro_str_to_distro_id("openSUSE" ++ _) ->
    suse;
linux_distro_str_to_distro_id("SLES" ++ _) ->
    sles;
linux_distro_str_to_distro_id("Ubuntu" ++ _) ->
    ubuntu;
linux_distro_str_to_distro_id("Wind River Linux" ++ _) ->
    wind_river;
linux_distro_str_to_distro_id("Yellow Dog" ++ _) ->
    yellow_dog;
linux_distro_str_to_distro_id(X) ->
    X.


do_linux_which_distro_fedora_release(Version, Label) ->
    %% Check if fedora
    case file:read_file_info("/etc/fedora-release") of
        {ok, _} ->
            case [string:trim(S) ||
                     S <- string:tokens(os:cmd("cat /etc/fedora-release"),
                                        [$\n])] of
                [DistroStr | _] ->
                    io:format("Linux: ~s"
                              "~n   Distro:                  ~s"
                              "~n   TS Extra Platform Label: ~s"
                              "~n   Product Name:            ~s"
                              "~n",
                              [Version, DistroStr, Label,
                               linux_product_name()]);
                _ ->
                    io:format("Linux: ~s"
                              "~n   Distro: ~s"
                              "~n   TS Extra Platform Label: ~s"
                              "~n   Product Name:            ~s"
                              "~n",
                              [Version, "Fedora", Label,
                               linux_product_name()])
            end,
            throw({distro, {fedora, simplify_label(Label)}});
        _ ->
            throw({error, not_found})
    end.

do_linux_which_distro_suse_release(Version, Label) ->
    %% Check if its a SuSE
    case file:read_file_info("/etc/SUSE-brand") of
        {ok, _} ->
            case file:read_file_info("/etc/SuSE-release") of
                {ok, _} ->
                    case [string:trim(S) ||
                             S <- string:tokens(os:cmd("cat /etc/SuSE-release"),
                                                [$\n])] of
                        ["SUSE Linux Enterprise Server" ++ _ = DistroStr | _] ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro, {sles, simplify_label(Label)}});
                        [DistroStr | _] ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro, {suse, simplify_label(Label)}});
                        _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, "SuSE", Label,
                                       linux_product_name()]),
                            throw({distro, {suse, simplify_label(Label)}})
                    end;
                _ ->
                    case string:tokens(os:cmd("cat /etc/SUSE-brand"), [$\n]) of
                        ["SLE" = DistroStr, VERSION | _] ->
                            case [string:strip(S) ||
                                     S <- string:tokens(VERSION, [$=])] of
                                ["VERSION", VersionNo] ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   Distro Version:          ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version,
                                               DistroStr, VersionNo,
                                               Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {sles, simplify_label(Label)}});
                                _ ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version, DistroStr, Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {sles, simplify_label(Label)}})
                            end;
                        ["openSUSE" = DistroStr, VERSION | _] ->
                            case [string:strip(S) ||
                                     S <- string:tokens(VERSION, [$=])] of
                                ["VERSION", VersionNo] ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   Distro Version:          ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version,
                                               DistroStr, VersionNo,
                                               Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {suse, simplify_label(Label)}});
                                _ ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version, DistroStr, Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {suse, simplify_label(Label)}})
                            end;
                        _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, "Unknown SUSE", Label,
                                       linux_product_name()]),
                            throw({distro, {suse, simplify_label(Label)}})
                    end
            end;
        _ ->
            throw({error, not_found})
    end.

do_linux_which_distro_issue(Version, Label) ->
    case file:read_file_info("/etc/issue") of
        {ok, _} ->
            case [string:trim(S) ||
                     S <- string:tokens(os:cmd("cat /etc/issue"), [$\n])] of
                [DistroStr | _] ->
                    case DistroStr of
                        "Wind River Linux" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {wind_river, simplify_label(Label)}});
                        "MontaVista" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro, 
                                   {montavista, simplify_label(Label)}});
                        "Yellow Dog" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {yellow_dog, simplify_label(Label)}});
                        "Ubuntu" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {ubuntu, simplify_label(Label)}});
                        "Linux Mint" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {linux_mint, simplify_label(Label)}});
                        "Debian" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {debian, simplify_label(Label)}});
                        _ ->
                            DistroStr
                    end;
                X ->
                    X
            end;
        _ ->
            throw({error, not_found})
    end.
                            

analyze_and_print_linux_host_info(Version) ->
    {Distro, Label} = linux_which_distro(Version),
    %% 'VirtFactor' will be 0 unless virtual
    VirtFactor = linux_virt_factor(),
    Factor =
        case (catch linux_which_cpuinfo(Distro)) of
            {ok, {CPU, BogoMIPS}} ->
                io:format("CPU: "
                          "~n   Model:                 ~s"
                          "~n   BogoMIPS:              ~w"
                          "~n   Num Online Schedulers: ~s"
                          "~n", [CPU, BogoMIPS, str_num_schedulers()]),
                if
                    (BogoMIPS > 50000) ->
                        1;
                    (BogoMIPS > 40000) ->
                        2;
                    (BogoMIPS > 30000) ->
                        3;
                    (BogoMIPS > 20000) ->
                        4;
                    (BogoMIPS > 10000) ->
                        5;
                    (BogoMIPS > 5000) ->
                        8;
                    (BogoMIPS > 3000) ->
                        12;
                    true ->
                        10
                end;
            {ok, "POWER9" ++ _ = CPU} ->
                %% For some reason this host is really slow
                %% Consider the CPU, it really should not be...
                %% But, to not fail a bunch of test cases, we add 5
                case linux_cpuinfo_clock() of
                    Clock when is_integer(Clock) andalso (Clock > 0) ->
                        io:format("CPU: "
                                  "~n   Model:                 ~s"
                                  "~n   CPU Speed:             ~w"
                                  "~n   Num Online Schedulers: ~s"
                                  "~n", [CPU, Clock, str_num_schedulers()]),
                        if
                            (Clock > 2000) ->
                                5 + num_schedulers_to_factor();
                            true ->
                                10 + num_schedulers_to_factor()
                        end;
                    _ ->
                        num_schedulers_to_factor()
                end;
            {ok, CPU} ->
                io:format("CPU: "
                          "~n   Model:                 ~s"
                          "~n   Num Online Schedulers: ~s"
                          "~n", [CPU, str_num_schedulers()]),
                num_schedulers_to_factor();
            _ ->
                5
        end,
    AddLabelFactor = label2factor(Label),
    %% Check if we need to adjust the factor because of the memory
    AddMemFactor = try linux_which_meminfo()
                   catch _:_:_ -> 0
                   end,
    TSScaleFactor = case timetrap_scale_factor() of
                        N when is_integer(N) andalso (N > 0) ->
                            N - 1;
                        _ ->
                            0
                    end,
    io:format("Factor calc:"
              "~n      Base Factor:     ~w"
              "~n      Label Factor:    ~w"
              "~n      Mem Factor:      ~w"
              "~n      Virtual Factor:  ~w"
              "~n      TS Scale Factor: ~w"
              "~n", [Factor, AddLabelFactor, AddMemFactor, VirtFactor,
                     TSScaleFactor]),
    {Factor + AddLabelFactor + AddMemFactor + VirtFactor + TSScaleFactor,
     [{label, Label}]}.


linux_virt_factor() ->
    linux_virt_factor(linux_product_name()).

linux_virt_factor("VMware" ++ _) ->
    2;
linux_virt_factor("VirtualBox" ++ _) ->
    4;
linux_virt_factor(_) ->
    0.


linux_cpuinfo_lookup(Key) when is_list(Key) ->
    linux_info_lookup(Key, "/proc/cpuinfo").

linux_cpuinfo_bogomips() ->
    case linux_cpuinfo_bogomips("bogomips") of
        "-" ->
            linux_cpuinfo_bogomips("BogoMIPS");
        Res ->
            Res
    end.

linux_cpuinfo_bogomips(Key) ->
    case linux_cpuinfo_lookup(Key) of
        [] ->
            "-";
        BMips when is_list(BMips) ->
            try lists:sum([bogomips_to_int(BM) || BM <- BMips])
            catch
                _:_:_ ->
                    "-"
            end;
        _ ->
            "-"
    end.

linux_cpuinfo_total_bogomips() ->
    case linux_cpuinfo_lookup("total bogomips") of
        [TBM] ->
            try bogomips_to_int(TBM)
            catch
                _:_:_ ->
                    "-"
            end;
        _ ->
            "-"
    end.

bogomips_to_int(BM) ->
    try list_to_float(BM) of
        F ->
            floor(F)
    catch
        _:_:_ ->
            try list_to_integer(BM) of
                I ->
                    I
            catch
                _:_:_ ->
                    throw(noinfo)
            end
    end.

linux_cpuinfo_model() ->
    case linux_cpuinfo_lookup("model") of
        [M] ->
            M;
        _ ->
	    %% Note that some distros/platforms,
            %% the first char is Capital, that is: Model...
	    case linux_cpuinfo_lookup("Model") of
		[M] ->
		    M;
		_ ->
		    "-"
	    end
    end.

linux_cpuinfo_platform() ->
    case linux_cpuinfo_lookup("platform") of
        [P] ->
            P;
        _ ->
            "-"
    end.

linux_cpuinfo_model_name() ->
    case linux_cpuinfo_lookup("model name") of
        [P|_] ->
            P;
        _X ->
            "-"
    end.

linux_cpuinfo_cpu() ->
    case linux_cpuinfo_lookup("cpu") of
        [C|_] ->
            C;
        _ ->
            "-"
    end.

linux_cpuinfo_motherboard() ->
    case linux_cpuinfo_lookup("motherboard") of
        [MB] ->
            MB;
        _ ->
            "-"
    end.

linux_cpuinfo_processor() ->
    case linux_cpuinfo_lookup("Processor") of
        [P] ->
            P;
        _ ->
            "-"
    end.

linux_cpuinfo_machine() ->
    case linux_cpuinfo_lookup("machine") of
        [M] ->
            M;
        _ ->
            "-"
    end.

linux_cpuinfo_clock() ->
    %% This is written as: "3783.000000MHz"
    %% So, check unit MHz (handle nothing else).
    %% Also, check for both float and integer
    %% Also,  the freq is per core, and can vary...
    case linux_cpuinfo_lookup("clock") of
        [C|_] when is_list(C) ->
            case lists:reverse(string:to_lower(C)) of
                "zhm" ++ CRev ->
                    try trunc(list_to_float(lists:reverse(CRev))) of
                        I ->
                            I
                    catch
                        _:_:_ ->
                            try list_to_integer(lists:reverse(CRev)) of
                                I ->
                                    I
                            catch
                                _:_:_ ->
                                    0
                            end
                    end;
                _ ->
                    0
            end;
        _ ->
            0
    end.

linux_which_cpuinfo(montavista) ->
    CPU =
        case linux_cpuinfo_cpu() of
            "-" ->
                throw(noinfo);
            Model ->
                case linux_cpuinfo_motherboard() of
                    "-" ->
                        Model;
                    MB ->
                        Model ++ " (" ++ MB ++ ")"
                end
        end,
    case linux_cpuinfo_bogomips() of
        "-" ->
            {ok, CPU};
        BMips ->
            {ok, {CPU, BMips}}
    end;

linux_which_cpuinfo(yellow_dog) ->
    CPU =
        case linux_cpuinfo_cpu() of
            "-" ->
                throw(noinfo);
            Model ->
                case linux_cpuinfo_motherboard() of
                    "-" ->
                        Model;
                    MB ->
                        Model ++ " (" ++ MB ++ ")"
                end
        end,
    {ok, CPU};

linux_which_cpuinfo(wind_river) ->
    CPU =
        case linux_cpuinfo_model() of
            "-" ->
                throw(noinfo);
            Model ->
                case linux_cpuinfo_platform() of
                    "-" ->
                        Model;
                    Platform ->
                        Model ++ " (" ++ Platform ++ ")"
                end
        end,
    case linux_cpuinfo_total_bogomips() of
        "-" ->
            {ok, CPU};
        BMips ->
            {ok, {CPU, BMips}}
    end;

linux_which_cpuinfo(Distro) when (Distro =:= debian) orelse
                                 (Distro =:= fedora) orelse
                                 (Distro =:= linux_mint) orelse
                                 (Distro =:= sles) orelse
                                 (Distro =:= suse) orelse
                                 (Distro =:= ubuntu) orelse
                                 (Distro =:= other) ->
    CPU =
        case linux_cpuinfo_model_name() of
            "-" ->
		%% This is for POWER9
		case linux_cpuinfo_cpu() of
		    "POWER9" ++ _ = PowerCPU ->
			Machine =
			    case linux_cpuinfo_machine() of
				"-" ->
				    "";
				M ->
				    " (" ++ M ++ ")"
			    end,
			PowerCPU ++ Machine;
		    _X ->
			%% ARM (at least some distros...)
			case linux_cpuinfo_processor() of
			    "-" ->
				case linux_cpuinfo_model() of
				    "-" ->
					%% Ok, we give up
					throw(noinfo);
				    Model ->
					Model
				end;
			    Proc ->
				Proc
			end
		end;
	    ModelName ->
                ModelName
        end,
    case linux_cpuinfo_bogomips() of
        "-" ->
            {ok, CPU};
        BMips ->
            {ok, {CPU, BMips}}
    end;

linux_which_cpuinfo(other) ->
    %% Check for x86 (Intel or AMD or Power)
    CPU =
        case linux_cpuinfo_model_name() of
            "-" ->
                %% ARM (at least some distros...)
                case linux_cpuinfo_processor() of
                    "-" ->
                        %% POWER (at least some distros...)
                        case linux_cpuinfo_cpu() of
                            "-" ->
                                %% Ok, we give up
                                throw(noinfo);
                            C ->
                                C
                        end;
                    Proc ->
                        Proc
                end;
            ModelName ->
                ModelName
        end,
    case linux_cpuinfo_bogomips() of
        "-" ->
            {ok, CPU};
        BMips ->
            {ok, {CPU, BMips}}
    end.

linux_meminfo_lookup(Key) when is_list(Key) ->
    linux_info_lookup(Key, "/proc/meminfo").

linux_meminfo_memtotal() ->
    case linux_meminfo_lookup("MemTotal") of
        [X] ->
            X;
        _ ->
            "-"
    end.

%% We *add* the value this return to the Factor.
linux_which_meminfo() ->
    case linux_meminfo_memtotal() of
        "-" ->
            0;
        MemTotal ->
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
                        (MemSz3 >= 16777216) ->
                            0;
                        (MemSz3 >= 8388608) ->
                            1;
                        (MemSz3 >= 4194304) ->
                            3;
                        (MemSz3 >= 2097152) ->
                            5;
                        true ->
                            8
                    end;
                _X ->
                    0
            end
    end.


linux_product_name() ->
    ProductNameFile = "/sys/devices/virtual/dmi/id/product_name",
    case file:read_file_info(ProductNameFile) of
        {ok, _} ->
            case os:cmd("cat " ++ ProductNameFile) of
                false ->
                    "-";
                Info ->
                    string:trim(Info)
            end;
        _ ->
            "-"
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
                    (CPUSpeed >= 3000) ->
                        if
                            (NCPU >= 8) ->
                                1;
                            (NCPU >= 6) ->
                                2;
                            (NCPU >= 4) ->
                                3;
                            (NCPU >= 2) ->
                                4;
                            true ->
                                10
                        end;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 8) ->
                                2;
                            (NCPU >= 6) ->
                                3;
                            (NCPU >= 4) ->
                                4;
                            (NCPU >= 2) ->
                                5;
                            true ->
                                12
                        end;
                    (CPUSpeed >= 1000) ->
                        if
                            (NCPU >= 8) ->
                                3;
                            (NCPU >= 6) ->
                                4;
                            (NCPU >= 4) ->
                                5;
                            (NCPU >= 2) ->
                                6;
                            true ->
                                14
                        end;
                    true ->
                        if
                            (NCPU >= 8) ->
                                4;
                            (NCPU >= 6) ->
                                6;
                            (NCPU >= 4) ->
                                8;
                            (NCPU >= 2) ->
                                10;
                            true ->
                                20
                        end
                end,
            MemAddFactor =
                if
                    (Memory >= 16777216) ->
                        0;
                    (Memory >= 8388608) ->
                        1;
                    (Memory >= 4194304) ->
                        3;
                    (Memory >= 2097152) ->
                        5;
                    true ->
                        10
                end,
            {CPUFactor + MemAddFactor, []}
        end
    catch
        _:_:_ ->
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            {10, []}
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
            {CPUFactor + MemAddFactor, []}
        end
    catch
        _:_:_ ->
            io:format("CPU:"
                      "~n   Num Schedulers: ~w"
                      "~n", [erlang:system_info(schedulers)]),
            Factor = case erlang:system_info(schedulers) of
                         1 ->
                             10;
                         2 ->
                             5;
                         _ ->
                             2
                     end,
            {Factor, []}
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
            {CPUFactor + MemAddFactor, []}
        end
    catch
        _:_:_ ->
            io:format("CPU:"
                      "~n   Num Schedulers: ~w"
                      "~n", [erlang:system_info(schedulers)]),
            Factor = case erlang:system_info(schedulers) of
                         1 ->
                             10;
                         2 ->
                             5;
                         _ ->
                             2
                     end,
            {Factor, []}
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



%% Model Identifier: Macmini7,1
%% Processor Name: Intel Core i5
%% Processor Speed: 2,6 GHz
%% Number of Processors: 1
%% Total Number of Cores: 2
%% L2 Cache (per Core): 256 KB
%% L3 Cache: 3 MB
%% Hyper-Threading Technology: Enabled
%% Memory: 16 GB

analyze_and_print_darwin_host_info(Version) ->
    %% This stuff is for macOS.
    %% If we ever tested on a pure darwin machine,
    %% we need to find some other way to find some info...
    %% Also, I suppose its possible that we for some other
    %% reason *fail* to get the info...
    Label  = ts_extra_platform_label(),
    {BaseFactor, MemFactor} =
        case analyze_darwin_software_info() of
            [] ->
                io:format("Darwin:"
                          "~n   Version:                 ~s"
                          "~n   Num Online Schedulers:   ~s"
                          "~n   TS Extra Platform Label: ~s"
                          "~n", [Version, str_num_schedulers(), Label]),
                {num_schedulers_to_factor(), 1};
            SwInfo when  is_list(SwInfo) ->
                SystemVersion = analyze_darwin_sw_system_version(SwInfo),
                KernelVersion = analyze_darwin_sw_kernel_version(SwInfo),
                HwInfo        = analyze_darwin_hardware_info(),
                ModelName     = analyze_darwin_hw_model_name(HwInfo),
                ModelId       = analyze_darwin_hw_model_identifier(HwInfo),
                {Processor, CPUFactor} = analyze_darwin_hw_processor(HwInfo),
                Memory        = analyze_darwin_hw_memory(HwInfo),
                io:format("Darwin:"
                          "~n   System Version:          ~s"
                          "~n   Kernel Version:          ~s"
                          "~n   Model:                   ~s (~s)"
                          "~n   Processor:               ~s"
                          "~n   Memory:                  ~s"
                          "~n   Num Online Schedulers:   ~s"
                          "~n   TS Extra Platform Label: ~s"
                          "~n~n",
                          [SystemVersion, KernelVersion,
                           ModelName, ModelId,
                           Processor, 
                           Memory,
                           str_num_schedulers(), Label]),
                {CPUFactor, analyze_darwin_memory_to_factor(Memory)}
        end,
    AddLabelFactor = label2factor(simplify_label(Label)),
    AddMemFactor = if
                       (MemFactor > 0) ->
                           MemFactor - 1;
                       true ->
                           0
                   end,
    TSScaleFactor = ts_scale_factor(),
    io:format("Factor calc:"
              "~n      Base Factor:     ~w"
              "~n      Label Factor:    ~w"
              "~n      Mem Factor:      ~w"
              "~n      TS Scale Factor: ~w"
              "~n~n",
              [BaseFactor, AddLabelFactor, AddMemFactor, TSScaleFactor]),
    {BaseFactor + AddLabelFactor + AddMemFactor + TSScaleFactor,
     [{label, Label}]}.

analyze_darwin_sw_system_version(SwInfo) ->
    proplists:get_value("system version", SwInfo, "-").

analyze_darwin_sw_kernel_version(SwInfo) ->
    proplists:get_value("kernel version", SwInfo, "-").

analyze_darwin_software_info() ->
    analyze_darwin_system_profiler("SPSoftwareDataType").

analyze_darwin_hw_chip(HwInfo) ->
    proplists:get_value("chip", HwInfo, "-").

analyze_darwin_hw_model_name(HwInfo) ->
    proplists:get_value("model name", HwInfo, "-").

analyze_darwin_hw_model_identifier(HwInfo) ->
    proplists:get_value("model identifier", HwInfo, "-").

analyze_darwin_hw_processor(HwInfo) ->
    case analyze_darwin_hw_processor_name(HwInfo) of
        "-" -> % Maybe Apple Chip
            case analyze_darwin_hw_chip(HwInfo) of
                "-" ->
                    "-";
                Chip ->
                    NumCores = analyze_darwin_hw_total_number_of_cores(HwInfo),
                    CPUFactor = analyze_darwin_cpu_to_factor(Chip, NumCores),
                    {f("~s [~s]", [Chip, NumCores]), CPUFactor}
            end;
        ProcName ->
            ProcSpeed = analyze_darwin_hw_processor_speed(HwInfo),
            NumProc   = analyze_darwin_hw_number_of_processors(HwInfo),
            NumCores  = analyze_darwin_hw_total_number_of_cores(HwInfo),
            CPUFactor = analyze_darwin_cpu_to_factor(ProcName,
                                                     ProcSpeed,
                                                     NumProc,
                                                     NumCores),
            {f("~s [~s, ~s, ~s]",
               [ProcName, ProcSpeed, NumProc, NumCores]), CPUFactor}
    end.

analyze_darwin_hw_processor_name(HwInfo) ->
    case proplists:get_value("processor name", HwInfo, "-") of
        "-" ->
            proplists:get_value("chip", HwInfo, "-");
        N ->
            N
    end.

analyze_darwin_hw_processor_speed(HwInfo) ->
    proplists:get_value("processor speed", HwInfo, "-").

analyze_darwin_hw_number_of_processors(HwInfo) ->
    case analyze_darwin_hw_processor_name(HwInfo) of
        "Apple M" ++ _ ->
            proplists:get_value("number of processors", HwInfo, "1");
        _ ->
            proplists:get_value("number of processors", HwInfo, "-")
    end.

analyze_darwin_hw_total_number_of_cores(HwInfo) ->
    proplists:get_value("total number of cores", HwInfo, "-").

analyze_darwin_hw_memory(HwInfo) ->
    proplists:get_value("memory", HwInfo, "-").

analyze_darwin_hardware_info() ->
    analyze_darwin_system_profiler("SPHardwareDataType").

%% This basically has the structure: "Key: Value"
%% But could also be (for example):
%%    "Something:" (which we ignore)
%%    "Key: Value1:Value2"
analyze_darwin_system_profiler(DataType) ->
    %% First, make sure the program actually exist:
    case string:trim(os:cmd("which system_profiler")) of
        [] ->
            case string:trim(os:cmd("which /usr/sbin/system_profiler")) of
                [] ->
                    [];
                Cmd1 ->
                    analyze_darwin_system_profiler(Cmd1, DataType)
            end;
        Cmd2 ->
            analyze_darwin_system_profiler(Cmd2, DataType)
    end.

analyze_darwin_system_profiler(Cmd, DataType) ->
    D0 = os:cmd(Cmd ++ " " ++ DataType),
    D1 = string:tokens(D0, [$\n]),
    D2 = [string:trim(S1) || S1 <- D1],
    D3 = [string:tokens(S2, [$:]) || S2 <- D2],
    analyze_darwin_system_profiler2(D3).

analyze_darwin_system_profiler2(L) ->
    analyze_darwin_system_profiler2(L, []).
    
analyze_darwin_system_profiler2([], Acc) ->
    [{string:to_lower(K), V} || {K, V} <- lists:reverse(Acc)];
analyze_darwin_system_profiler2([[_]|T], Acc) ->
    analyze_darwin_system_profiler2(T, Acc);
analyze_darwin_system_profiler2([[H1,H2]|T], Acc) ->
    analyze_darwin_system_profiler2(T, [{H1, string:trim(H2)}|Acc]);
analyze_darwin_system_profiler2([[H|TH0]|T], Acc) ->
    %% Some value parts has ':' in them, so put them together
    TH1 = colonize(TH0),
    analyze_darwin_system_profiler2(T, [{H, string:trim(TH1)}|Acc]).

%% This is only called if the length is at least 2
colonize([L1, L2]) ->
    L1 ++ ":" ++ L2;
colonize([H|T]) ->
    H ++ ":" ++ colonize(T).


%% The memory looks like this "<size> <unit>". Example: "2 GB" 
analyze_darwin_memory_to_factor(Mem) ->
    case [string:to_lower(S) || S <- string:tokens(Mem, [$\ ])] of
        [_SzStr, "tb"] ->
            1;
        [SzStr, "gb"] ->
            try list_to_integer(SzStr) of
                Sz when Sz < 2 ->
                    20;
                Sz when Sz < 4 ->
                    10;
                Sz when Sz < 8 ->
                    5;
                Sz when Sz < 16 ->
                    2;
                _ ->
                    1
            catch
                _:_:_ ->
                    20
            end;
        [_SzStr, "mb"] ->
            20;
        _ ->
            20
    end.


analyze_darwin_cpu_to_factor("Apple" ++ _ = _Chip, _NumCores) ->
    1;
analyze_darwin_cpu_to_factor(_Chip, _NumCores) ->
    8.


%% The speed is a string: "<speed> <unit>"
%% the speed may be a float, which we transforms into an integer of MHz.
%% To calculate a factor based on processor speed, number of procs
%% and number of cores is ... not an exact ... science ...
%%
%% If Apple processor, we don't know the speed, so ignore that for now...
analyze_darwin_cpu_to_factor("Apple M" ++ _ = _ProcName,
                             _ProcSpeedStr, NumProcStr, NumCoresStr) ->
    %% io:format("analyze_darwin_cpu_to_factor(apple) -> entry with"
    %%           "~n  ProcName:     ~p"
    %%           "~n  ProcSpeedStr: ~p"
    %%           "~n  NumProcStr:   ~p"
    %%           "~n  NumCoresStr:  ~p"
    %%           "~n", [_ProcName, _ProcSpeedStr, NumProcStr, NumCoresStr]),
    NumProc = try list_to_integer(NumProcStr) of
                  NumProcI ->
                      NumProcI
              catch
                  _:_:_ ->
                      1
              end,
    %% This is a string that looks like this: X (Y performance and Z efficiency)
    NumCores = try string:tokens(NumCoresStr, [$\ ]) of
                   [NCStr | _] ->
                       try list_to_integer(NCStr) of
                           NumCoresI ->
                               NumCoresI
                       catch
                           _:_:_ ->
                               1
                       end
               catch
                   _:_:_ ->
                       1
               end,
    if
        (NumProc =:= 1) ->
            if
                (NumCores < 2) ->
                    5;
                (NumCores < 4) ->
                    3;
                (NumCores < 6) ->
                    2;
                true ->
                    1
            end;
        true ->
            if
                (NumCores < 4) ->
                    2;
                true ->
                    1
            end
    end;
analyze_darwin_cpu_to_factor(_ProcName,
                             ProcSpeedStr, NumProcStr, NumCoresStr) ->
    %% io:format("analyze_darwin_cpu_to_factor -> entry with"
    %%           "~n  ProcName:     ~p"
    %%           "~n  ProcSpeedStr: ~p"
    %%           "~n  NumProcStr:   ~p"
    %%           "~n  NumCoresStr:  ~p"
    %%           "~n", [_ProcName, ProcSpeedStr, NumProcStr, NumCoresStr]),
    Speed = 
        case [string:to_lower(S) || S <- string:tokens(ProcSpeedStr, [$\ ])] of
            [SpeedStr, "mhz"] ->
                try list_to_integer(SpeedStr) of
                    SpeedI ->
                        SpeedI
                catch
                    _:_:_ ->
                        try list_to_float(SpeedStr) of
                            SpeedF ->
                                trunc(SpeedF)
                        catch
                            _:_:_ ->
                                -1
                        end
                end;
            [SpeedStr, "ghz"] ->
                try list_to_float(SpeedStr) of
                    SpeedF ->
                        trunc(1000*SpeedF)
                catch
                    _:_:_ ->
                        try list_to_integer(SpeedStr) of
                            SpeedI ->
                                1000*SpeedI
                        catch
                            _:_:_ ->
                                -1
                        end
                end;
            _ ->
                -1
        end,
    NumProc = try list_to_integer(NumProcStr) of
                  NumProcI ->
                      NumProcI
              catch
                  _:_:_ ->
                      1
              end,
    NumCores = try list_to_integer(NumCoresStr) of
                   NumCoresI ->
                       NumCoresI
               catch
                   _:_:_ ->
                       1
               end,
    if
        (Speed > 3000) ->
            if
                (NumProc =:= 1) ->
                    if
                        (NumCores < 2) ->
                            5;
                        (NumCores < 4) ->
                            3;
                        (NumCores < 6) ->
                            2;
                        true ->
                            1
                    end;
                true ->
                    if
                        (NumCores < 4) ->
                            2;
                        true ->
                            1
                    end
            end;
        (Speed > 2000) ->
            if
                (NumProc =:= 1) ->
                    if
                        (NumCores < 2) ->
                            8;
                        (NumCores < 4) ->
                            5;
                        (NumCores < 6) ->
                            3;
                        true ->
                            1
                    end;
                true ->
                    if
                        (NumCores < 4) ->
                            5;
                        (NumCores < 8) ->
                            2;
                        true ->
                            1
                    end
            end;
        true ->
            if
                (NumProc =:= 1) ->
                    if
                        (NumCores < 2) ->
                            10;
                        (NumCores < 4) ->
                            7;
                        (NumCores < 6) ->
                            5;
                        (NumCores < 8) ->
                            3;
                        true ->
                            1
                    end;
                true ->
                    if
                        (NumCores < 4) ->
                            8;
                        (NumCores < 8) ->
                            4;
                        true ->
                            1
                    end
            end
    end.
    

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
              "~n   Release:               ~s"
              "~n   Banner Name:           ~s"
              "~n   Instruction Set:       ~s"
              "~n   CPUs:                  ~s (~s)"
              "~n   System Config:         ~s"
              "~n   Memory Size:           ~s"
              "~n   Num Online Schedulers: ~s"
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
    {try erlang:system_info(schedulers) of
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
     end + MemFactor, []}.    


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
              "~n   Total Physical Memory:  ~s (~w)"
              "~n   Num Online Schedulers:  ~s"
              "~n", [OsName, OsVersion, Version,
		     SysMan, SysMod, NumProcs, TotPhysMem, TotPhysMem,
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
    {CPUFactor + MemFactor, SysInfo}.

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
    F = fun() ->
                try
                    begin
                        SysInfo = os:cmd("systeminfo"),
                        process_win_system_info(
                          string:tokens(SysInfo, [$\r, $\n]), [])
                    end
                catch
                    C:E:S ->
                        io:format("Failed get or process System info: "
                                  "   Error Class: ~p"
                                  "   Error:       ~p"
                                  "   Stack:       ~p"
                                  "~n", [C, E, S]),
                        []
                end
        end,
    proxy_call(F, minutes(1),
               fun() -> throw({skip, "System info timeout"}) end).

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
                    

linux_info_lookup(Key, File) ->
    try [string:trim(S) || S <- string:tokens(os:cmd("grep " ++ "\"" ++ Key ++ "\"" ++ " " ++ File), [$:,$\n])] of
        Info ->
            linux_info_lookup_collect(Key, Info, [])
    catch
        _:_:_ ->
            "-"
    end.

linux_info_lookup_collect(_Key, [], Values) ->
    lists:reverse(Values);
linux_info_lookup_collect(Key, [Key, Value|Rest], Values) ->
    linux_info_lookup_collect(Key, Rest, [Value|Values]);
linux_info_lookup_collect(_, _, Values) ->
    lists:reverse(Values).
    

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

executor(Fun) ->
    executor(Fun, infinity).

executor(Fun, Timeout)
  when is_function(Fun, 0) andalso
       ((Timeout =:= infinity) orelse (is_integer(Timeout) andalso (Timeout > 0))) ->
    {Pid, MRef} = erlang:spawn_monitor(Fun),
    receive
        {'DOWN', MRef, process, Pid, Info} ->
            p("executor process terminated (normal) with"
              "~n      ~p", [Info]),
            Info;
        {'EXIT', TCPid, {timetrap_timeout = R, TCTimeout, TCSTack}} ->
            p("received timetrap timeout (~w ms) from ~p => Kill executor process"
              "~n      TC Stack: ~p", [TCTimeout, TCPid, TCSTack]),
            exit(Pid, kill),
            %% We do this in case we get some info about 'where'
            %% the process is hanging...
            receive
                {'DOWN', MRef, process, Pid, Info} ->
                    p("executor process terminated (forced) with"
                      "~n      ~p", [Info]),
                    ok
            after 1000 -> % Give it a second...
                    ok
            end,
            {error, R}
    after Timeout ->
            p("executor process termination timeout - kill executor process"),
            exit(kill, Pid),
            {error, executor_timeout}
    end.
            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_tc(TCName, Name, Verbosity, Pre, Case, Post) ->
    Cond = fun() -> ok end,
    try_tc(TCName, Name, Verbosity, Cond, Pre, Case, Post).

try_tc(TCName, Name, Verbosity, Cond, Pre, Case, Post)
  when is_function(Cond, 0)  andalso 
       is_function(Pre,  0)  andalso 
       is_function(Case, 1) andalso
       is_function(Post, 1) ->
    tc_begin(TCName, Name, Verbosity),
    try Cond() of
        ok ->
            tc_print("starting: try pre"),
            try Pre() of
                State ->
                    tc_print("pre done: try test case"),
                    try
                        begin
                            Res = Case(State),
                            sleep(seconds(1)),
                            tc_print("test case done: try post"),
                            _ = executor(fun() ->
                                                 put(verbosity, Verbosity),
                                                 put(sname,     Name),
                                                 put(tc,        TCName),
                                                 Post(State)
                                         end),
                            tc_end("ok"),
                            Res
                        end
                    catch
                        C:{skip, _} = SKIP:_ when (C =:= throw) orelse
                                                  (C =:= exit) ->
                            tc_print("test case (~w) skip: try post", [C]),
                            _ = executor(fun() ->
                                                 put(verbosity, Verbosity),
                                                 put(sname,     Name),
                                                 put(tc,        TCName),
                                                 Post(State)
                                         end),
                            tc_end( f("skipping(caught,~w,tc)", [C]) ),
                            SKIP;
                        C:E:S ->
                            %% We always check the system events
                            %% before we accept a failure.
                            %% We do *not* run the Post here because it might
                            %% generate sys events itself...
                            p("try_tc -> test case failed: try post"),
                            _ = executor(fun() -> Post(State) end),
                            case megaco_test_global_sys_monitor:events() of
                                [] ->
                                    tc_print("test case failed: try post"),
                                    _ = executor(fun() ->
                                                         put(verbosity,
                                                             Verbosity),
                                                         put(sname,     Name),
                                                         put(tc,        TCName),
                                                         Post(State)
                                                 end),
                                    tc_end( f("failed(caught,~w,tc)", [C]) ),
                                    erlang:raise(C, E, S);
                                SysEvs ->
                                    tc_print("System Events "
                                             "received during tc: "
                                             "~n   ~p"
                                             "~nwhen tc failed:"
                                             "~n   C: ~p"
                                             "~n   E: ~p"
                                             "~n   S: ~p",
                                             [SysEvs, C, E, S]),
                                    _ = executor(fun() ->
                                                         put(verbosity,
                                                             Verbosity),
                                                         put(sname,     Name),
                                                         put(tc,        TCName),
                                                         Post(State)
                                                 end),
                                    tc_end( f("skipping(catched-sysevs,~w,tc)",
                                              [C]) ),
                                    SKIP =
                                        {skip, "TC failure with system events"},
                                    SKIP
                            end
                    end
            catch
                C:{skip, _} = SKIP:_ when (C =:= throw) orelse
                                          (C =:= exit) ->
                    tc_end( f("skipping(caught,~w,tc-pre)", [C]) ),
                    SKIP;
                C:E:S ->
                    case megaco_test_global_sys_monitor:events() of
                        [] ->
                            tc_print("tc-pre failed: auto-skip"
                                     "~n   C: ~p"
                                     "~n   E: ~p"
                                     "~n   S: ~p",
                                     [C, E, S]),
                            tc_end( f("auto-skip(caught,~w,tc-pre)", [C]) ),
                            SKIP = {skip, f("TC-Pre failure (~w)", [C])},
                            SKIP;
                        SysEvs ->
                            tc_print("System Events received: "
                                     "~n   ~p"
                                     "~nwhen tc-pre failed:"
                                     "~n   C: ~p"
                                     "~n   E: ~p"
                                     "~n   S: ~p",
                                     [SysEvs, C, E, S], "", ""),
                            tc_end( f("skipping(catched-sysevs,~w,tc-pre)",
                                      [C]) ),
                            SKIP = {skip, "TC-Pre failure with system events"},
                            SKIP
                    end
            end;
        {skip, _} = SKIP ->
            tc_end("skipping(cond)"),
            SKIP;
        {error, Reason} ->
            tc_end("failed(cond)"),
            exit({tc_cond_failed, Reason})
    catch
        C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
            tc_end( f("skipping(caught,~w,cond)", [C]) ),
            SKIP;
        C:E:S ->
            %% We always check the system events before we accept a failure
            case megaco_test_global_sys_monitor:events() of
                [] ->
                    tc_end( f("failed(caught,~w,cond)", [C]) ),
                    erlang:raise(C, E, S);
                SysEvs ->
                    tc_print("System Events received: "
                             "~n   ~p", [SysEvs], "", ""),
                    tc_end( f("skipping(catched-sysevs,~w,cond)", [C]) ),
                    SKIP = {skip, "TC cond failure with system events"},
                    SKIP
            end
    end.


tc_set_name(N) when is_atom(N) ->
    tc_set_name(atom_to_list(N));
tc_set_name(N) when is_list(N) ->
    put(tc_name, N).

tc_get_name() ->
    get(tc_name).

tc_begin(TC, Name, Verbosity) ->
    OldVal = process_flag(trap_exit, true),
    put(old_trap_exit, OldVal),
    tc_set_name(TC),
    put(sname,     Name),
    put(verbosity, Verbosity),
    tc_print("begin ***",
             "~n----------------------------------------------------~n", "").

tc_end(Result) when is_list(Result) ->
    OldVal = erase(old_trap_exit),
    process_flag(trap_exit, OldVal),
    tc_print("done: ~s", [Result], 
             "", "----------------------------------------------------~n~n"),
    ok.

tc_print(F) ->
    tc_print(F, [], "", "").

tc_print(F, A) ->
    tc_print(F, A, "", "").

tc_print(F, Before, After) ->
    tc_print(F, [], Before, After).

tc_print(F, A, Before, After) ->
    Name = tc_which_name(),
    FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
             [formated_timestamp(), Name, self() | A]),
    io:format(user, Before ++ FStr ++ After, []),
    io:format(standard_io, Before ++ FStr ++ After, []).

tc_which_name() ->
    case tc_get_name() of
        undefined ->
            case get(sname) of
                undefined ->
                    "";
                SName when is_list(SName) ->
                    SName
            end;
        Name when is_list(Name) ->
            Name
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


mk_nodes(1 = _N) ->
    [node()];
mk_nodes(N) when is_integer(N) andalso (N > 1) ->
    OwnNode       = node(),
    [Name0, Host] = node_to_name_and_host(OwnNode),
    Uniq          = erlang:unique_integer([positive]),
    Name          =
        list_to_atom(lists:concat([Name0, "_", integer_to_list(Uniq)])),
    [OwnNode | mk_nodes(N-1, Name, Host, [])].

mk_nodes(0, _BaseName, _Host, Nodes) ->
    Nodes;
mk_nodes(N, BaseName, Host, Nodes) ->
    mk_nodes(N-1, BaseName, Host, [mk_node(N, BaseName, Host)|Nodes]).

mk_node(N, BaseName, Host) ->
    list_to_atom(lists:concat([BaseName, "_", integer_to_list(N), "@", Host])).
    
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
    p("start_node -> check if node ~p already running", [Node]),
    case ping(Node, ?SECS(5)) of
        %% Do not require a *new* node
	pong when (Force =:= false) ->
            p("start_node -> node ~p already running", [Node]),
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
            p("start_node -> node ~p not running - create args", [Node]),
	    [Name, Host] = node_to_name_and_host(Node),
            Pa = filename:dirname(code:which(?MODULE)),
            Args0 = " -pa " ++ Pa ++
                " -s " ++ atom_to_list(megaco_test_sys_monitor) ++ " start" ++ 
                " -s global sync",
            Args = string:tokens(Args0, [$\ ]),
            p("start_node -> try start node ~p", [Node]),
            PeerOpts = #{name => Name,
                         host => Host,
                         args => Args},
	    case peer:start(PeerOpts) of
		{ok, _Peer, NewNode} when NewNode =:= Node ->
                    p("node ~p started - now set path, cwd and sync", [Node]),
		    Path      = code:get_path(),
		    {ok, Cwd} = file:get_cwd(),
		    true      = rpc:call(Node, code, set_path, [Path]),
		    ok        = rpc:call(Node, file, set_cwd, [Cwd]),
		    true      = rpc:call(Node, code, set_path, [Path]),
		    {_, []}   = rpc:multicall(global, sync, []),
		    ok;
		{ok, _Peer, NewNode} ->
                    e("wrong node started: "
                      "~n      Expected: ~p"
                      "~n      Got:      ~p", [Node, NewNode]),
                    stop_node(NewNode),
                    fatal_skip({invalid_node_start, NewNode, Node}, File, Line); 
		Other ->
                    e("failed starting node ~p: ~p", [Node, Other]),
                    fatal_skip({cannot_start_node, Node, Other}, File, Line)
	    end;
        
        timeout ->
            fatal_skip({ping_timeout, Node}, File, Line)
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
    case stop_node(Node) of
        ok ->
            ok;
        error ->
            fatal_skip({failed_stop_node, Node}, File, Line)
    end.

stop_node(Node) ->
    p("try stop node ~p", [Node]),
    erlang:monitor_node(Node, true),
    %% Make sure we do not hang in case 'Node' has problems
    erlang:spawn(fun() -> rpc:call(Node, erlang, halt, []) end),
    receive
        {nodedown, Node} ->
            p("node ~p stopped", [Node]),
            ok
    after 10000 ->
            e("failed stop node ~p", [Node]),
            erlang:monitor_node(Node, false),
            receive
                {nodedown, Node} ->
                    p("node ~p stopped after timeout (race)", [Node]),
                    ok
            after 0 ->
                    error
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ----------------------------------------------------------------
%% Generates a 'megaco_trace' tuple based on Config and a default
%% value.
%%

megaco_trace(Config, Default) ->
    Key = megaco_trace,
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Value}} ->
            p("default megaco-trace ~w", [Value]),
            {Key, Value};
        _ ->
            {Key, Default}
    end.


%% ----------------------------------------------------------------
%% Conditionally enable megaco trace at Level and for Destination.
%%

enable_trace(Config, Level, Destination) ->
    Key = megaco_trace,
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, disable}} ->
            p("megaco-trace disabled => skip enabling trace at: ~w; ~w",
              [Level, Destination]),
            ok;
        _ ->
            megaco:enable_trace(Level, Destination)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timetrap_scale_factor() ->
    case (catch test_server:timetrap_scale_factor()) of
	{'EXIT', _} ->
	    1;
	N ->
	    N
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

e(F) ->
    e(F, []).
e(F, A) ->
    print("ERROR", F, A).

p(F) ->
    p(F, []).

p(F, A) ->
    print("INFO", F, A).

print(Pre, F, A) ->
    io:format("*** [~s] [~s] ~p " ++ F ++ "~n", [?FTS(), Pre, self() | A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

explicit_inet_backend() ->
    %% This is intentional!
    %% This is a kernel flag, which if set disables
    %% our own special handling of the inet_backend
    %% in our test suites.
    case application:get_all_env(kernel) of
        Env when is_list(Env) ->
            case lists:keysearch(inet_backend, 1, Env) of
                {value, {inet_backend, _}} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

test_inet_backends() ->
    case application:get_all_env(megaco) of
        Env when is_list(Env) ->
            case lists:keysearch(test_inet_backends, 1, Env) of
                {value, {test_inet_backends, true}} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false 
    end.

inet_backend_opts(Config) when is_list(Config) ->
    case lists:keysearch(socket_create_opts, 1, Config) of
        {value, {socket_create_opts, InetBackendOpts}} ->
            InetBackendOpts;
        false ->
            []
    end.

which_inet_backend(Config) ->
    case lists:keysearch(socket_create_opts, 1, Config) of
        {value, {socket_create_opts, [{inet_backend, Backend}]}} ->
            Backend;
        _ ->
            default
    end.
    
is_socket_backend(Config) when is_list(Config) ->
    (which_inet_backend(Config) =:= socket).


open(Config, Pid, Opts)
  when is_list(Config) andalso is_pid(Pid) andalso is_list(Opts) ->
    InetBackendOpts = inet_backend_opts(Config),
    megaco_udp:open(Pid, InetBackendOpts ++ Opts).

listen(Config, Pid, Opts)
  when is_list(Config) andalso is_pid(Pid) andalso is_list(Opts) ->
    InetBackendOpts = inet_backend_opts(Config),
    megaco_tcp:listen(Pid, InetBackendOpts ++ Opts).

connect(Config, Ref, Opts)
  when is_list(Config) andalso is_list(Opts) ->
    InetBackendOpts = inet_backend_opts(Config),
    megaco_tcp:connect(Ref, InetBackendOpts ++ Opts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The point of this cludge is to make it possible to specify a 
%% timeout for the ping, since it can actually hang.
ping(Node) ->
    ping(Node, infinity).

ping(Node, Timeout)
  when is_atom(Node) andalso
       ((is_integer(Timeout) andalso (Timeout > 0)) orelse
        (Timeout =:= infinity)) ->
    {Pid, Mon} = erlang:spawn_monitor(fun() -> exit(net_adm:ping(Node)) end),
    receive
        {'DOWN', Mon, process, Pid, Info} when (Info =:= pong) orelse 
                                               (Info =:= pang) ->
            Info;
        {'DOWN', Mon, process, Pid, Info} ->
            e("unexpected ping result: "
              "~n      ~p", [Info]),
            exit({unexpected_ping_result, Info});
        {'EXIT', TCPid, {timetrap_timeout, TCTimeout, TCSTack}} ->
            p("received timetrap timeout (~w ms) from ~p => Kill ping process"
              "~n      TC Stack: ~p", [TCTimeout, TCPid, TCSTack]),
            kill_and_wait(Pid, Mon, "ping"),
            timeout
    after Timeout ->
            e("unexpected ping timeout"),
            kill_and_wait(Pid, Mon, "ping"),
            timeout
    end.
            
                                             
kill_and_wait(Pid, MRef, PStr) ->
    exit(Pid, kill),
    %% We do this in case we get some info about 'where'
    %% the process is hanging...
    receive
        {'DOWN', MRef, process, Pid, Info} ->
            p("~s process terminated (forced) with"
              "~n      ~p", [PStr, Info]),
            ok
    after 100 -> % Give it a second...
            ok
    end.
    
