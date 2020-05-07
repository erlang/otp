%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2020. All Rights Reserved.
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

-module(snmp_test_lib).

-include_lib("kernel/include/file.hrl").


-export([tc_try/2, tc_try/3,
         tc_try/4, tc_try/5]).
-export([proxy_call/3]).
-export([hostname/0, hostname/1, localhost/0, localhost/1, os_type/0, sz/1,
	 display_suite_info/1]).
-export([non_pc_tc_maybe_skip/4,
         os_based_skip/1,
         has_support_ipv6/0
        ]).
-export([init_per_suite/1, end_per_suite/1,
         init_suite_top_dir/2, init_group_top_dir/2, init_testcase_top_dir/2, 
	 fix_data_dir/1, 
	 lookup/2, 
	 replace_config/3, set_config/3, get_config/2, get_config/3]).
-export([fail/3, skip/3]).
-export([hours/1, minutes/1, seconds/1, sleep/1]).
-export([flush_mqueue/0, trap_exit/0, trap_exit/1]).
-export([ping/1, local_nodes/0, nodes_on/1]).
-export([start_node/2, stop_node/1]).
-export([is_app_running/1, 
	 is_crypto_running/0, is_mnesia_running/0, is_snmp_running/0,
         ensure_not_running/3]).
-export([crypto_start/0, crypto_support/0]).
-export([watchdog/3, watchdog_start/1, watchdog_start/2, watchdog_stop/1]).
-export([del_dir/1]).
-export([f/2, formated_timestamp/0]).
-export([p/2, print1/2, print2/2, print/5]).
-export([eprint/2, wprint/2, nprint/2, iprint/2]).


-define(SKIP(R), skip(R, ?MODULE, ?LINE)).


%% ----------------------------------------------------------------------
%% Run test-case
%%

%% *** tc_try/2,3,4,5 ***
%% Case:   Basically the test case name
%% TCCond: A fun that is evaluated before the actual test case
%%         The point of this is that it can performs checks to
%%         see if we shall run the test case at all.
%%         For instance, the test case may only work in specific
%%         conditions.
%% Pre:    A fun that is nominally part of the test case
%%         but is an initiation that must be "undone". This is
%%         done by the Post fun (regardless if the TC is successfull
%%         or not). Example: Starts a couple of nodes,
%% TC:     The test case fun
%% Post:   A fun that undo what was done by the Pre fun.
%%         Example: Stops the nodes created by the Pre function.
tc_try(Case, TC) ->
    tc_try(Case, fun() -> ok end, TC).

tc_try(Case, TCCond, TC0) when is_function(TC0, 0) ->
    Pre  = fun()  -> undefined end,
    TC   = fun(_) -> TC0() end,
    Post = fun(_) -> ok end,
    tc_try(Case, TCCond, Pre, TC, Post).

tc_try(Case, Pre, TC, Post)
  when is_atom(Case) andalso
       is_function(Pre, 0) andalso
       is_function(TC, 1) andalso
       is_function(Post, 1) ->
    TCCond = fun() -> ok end,
    tc_try(Case, TCCond, Pre, TC, Post).

tc_try(Case, TCCond, Pre, TC, Post)
  when is_atom(Case) andalso
       is_function(TCCond, 0) andalso
       is_function(Pre, 0) andalso
       is_function(TC, 1) andalso
       is_function(Post, 1) ->
    tc_begin(Case),
    try TCCond() of
        ok ->
            tc_print("starting: try pre"),
            try Pre() of
                State ->
                    tc_print("pre done: try test case"),
                    try
                        begin
                            TC(State),
                            sleep(seconds(1)),
                            tc_print("test case done: try post"),
                            (catch Post(State)),
                            tc_end("ok")
                        end
                    catch
                        C:{skip, _} = SKIP when (C =:= throw) orelse
                                                (C =:= exit) ->
                            tc_print("test case (~w) skip: try post", [C]),
                            (catch Post(State)),
                            tc_end( f("skipping(catched,~w,tc)", [C]) ),
                            SKIP;
                        C:E:S ->
                            %% We always check the system events
                            %% before we accept a failure.
                            %% We do *not* run the Post here because it might
                            %% generate sys events itself...
                            case snmp_test_global_sys_monitor:events() of
                                [] ->
                                    tc_print("test case failed: try post"),
                                    (catch Post(State)),
                                    tc_end( f("failed(catched,~w,tc)", [C]) ),
                                    erlang:raise(C, E, S);
                                SysEvs ->
                                    tc_print("System Events received during tc: "
                                             "~n   ~p"
                                             "~nwhen tc failed:"
                                             "~n   C: ~p"
                                             "~n   E: ~p"
                                             "~n   S: ~p",
                                             [SysEvs, C, E, S]),
                                    (catch Post(State)),
                                    tc_end( f("skipping(catched-sysevs,~w,tc)",
                                              [C]) ),
                                    SKIP = {skip, "TC failure with system events"},
                                    SKIP
                            end
                    end
            catch
                C:{skip, _} = SKIP when (C =:= throw) orelse
                                        (C =:= exit) ->
                    tc_end( f("skipping(catched,~w,tc-pre)", [C]) ),
                    SKIP;
                C:E:S ->
                    %% We always check the system events
                    %% before we accept a failure
                    case snmp_test_global_sys_monitor:events() of
                        [] ->
                            tc_print("tc-pre failed: auto-skip"
                                     "~n   C: ~p"
                                     "~n   E: ~p"
                                     "~n   S: ~p",
                                     [C, E, S]),
                            tc_end( f("auto-skip(catched,~w,tc-pre)", [C]) ),
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
                            tc_end( f("skipping(catched-sysevs,~w,tc-pre)", [C]) ),
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
            tc_end( f("skipping(catched,~w,cond)", [C]) ),
            SKIP;
        C:E:S ->
            %% We always check the system events before we accept a failure
            case snmp_test_global_sys_monitor:events() of
                [] ->
                    tc_end( f("failed(catched,~w,cond)", [C]) ),
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

tc_begin(TC) ->
    OldVal = process_flag(trap_exit, true),
    put(old_trap_exit, OldVal),
    tc_set_name(TC),
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
    

%% ----------------------------------------------------------------------
%% Misc functions
%%

proxy_call(F, Timeout, Default)
  when is_function(F, 0) andalso is_integer(Timeout) andalso (Timeout > 0) ->
    {P, M} = erlang:spawn_monitor(fun() -> exit(F()) end),
    receive
        {'DOWN', M, process, P, Reply} ->
            Reply
    after Timeout ->
            erlang:demonitor(M, [flush]),
            exit(P, kill),
            Default
    end.


hostname() ->
    hostname(node()).

hostname(Node) ->
    case string:tokens(atom_to_list(Node), [$@]) of
        [_, Host] ->
            Host;
        _ ->
            []
    end.

localhost() ->
    localhost(inet).

localhost(Family) ->
    case inet:getaddr(net_adm:localhost(), Family) of
        {ok, {127, _, _, _}} when (Family =:= inet) ->
            %% Ouch, we need to use something else
            case inet:getifaddrs() of
                {ok, IfList} ->
                    which_addr(Family, IfList);
                {error, Reason1} ->
                    fail({getifaddrs, Reason1}, ?MODULE, ?LINE)
            end;
        {ok, {A1, _, _, _, _, _, _, _}} when (Family =:= inet6) andalso
                                             ((A1 =:= 0) orelse
                                              (A1 =:= 16#fe80)) ->
            %% Ouch, we need to use something else
            case inet:getifaddrs() of
                {ok, IfList} ->
                    which_addr(Family, IfList);
                {error, Reason1} ->
                    fail({getifaddrs, Reason1}, ?MODULE, ?LINE)
            end;
        {ok, Addr} ->
            Addr;
        {error, Reason2} ->
            fail({getaddr, Reason2}, ?MODULE, ?LINE)
    end.

which_addr(_Family, []) ->
    fail(no_valid_addr, ?MODULE, ?LINE);
which_addr(Family, [{"lo", _} | IfList]) ->
    which_addr(Family, IfList);
which_addr(Family, [{"docker" ++ _, _} | IfList]) ->
    which_addr(Family, IfList);
which_addr(Family, [{"br-" ++ _, _} | IfList]) ->
    which_addr(Family, IfList);
which_addr(Family, [{_Name, IfOpts} | IfList]) ->
    case which_addr2(Family, IfOpts) of
        {ok, Addr} ->
            Addr;
        {error, _} ->
            which_addr(Family, IfList)
    end.

which_addr2(_Family, []) ->
    {error, not_found};
which_addr2(Family, [{addr, Addr}|_]) 
  when (Family =:= inet) andalso (size(Addr) =:= 4) ->
    {ok, Addr};
which_addr2(Family, [{addr, Addr}|_]) 
  when (Family =:= inet6) andalso (size(Addr) =:= 8) ->
    {ok, Addr};
which_addr2(Family, [_|IfOpts]) ->
    which_addr2(Family, IfOpts).


sz(L) when is_list(L) ->
    length(L);
sz(B) when is_binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


os_type() ->
    case (catch test_server:os_type()) of
	{'EXIT', _} ->
	    %% Pre-R10 test server does not have this function
	    os:type();
	OsType ->
	    OsType
    end.

display_suite_info(SUITE) when is_atom(SUITE) ->
    (catch do_display_suite_info(SUITE)).

do_display_suite_info(SUITE) ->
    MI = SUITE:module_info(),
    case (catch display_version(MI)) of
	ok ->
	    ok;
	_ ->
	    case (catch display_app_version(MI)) of
		ok ->
		    ok;
		_ ->
		    io:format("No version info available for test suite ~p~n",
			      [?MODULE])
	    end
    end.

display_version(MI) -> 
    {value, {compile, CI}} = lists:keysearch(compile, 1, MI),
    {value, {options, CO}} = lists:keysearch(options, 1, CI),
    Version = version_of_compiler_options(CO),
    io:format("~p version info: "
	      "~n   Version: ~p"
	      "~n", [?MODULE, Version]),
    ok.

version_of_compiler_options([{d, version, Version} | _]) ->
    Version;
version_of_compiler_options([_ | T]) ->
    version_of_compiler_options(T).

display_app_version(MI) ->
    {value, {attributes, Attrs}} = lists:keysearch(attributes, 1, MI),
    {value, {vsn, Vsn}}          = lists:keysearch(vsn, 1, Attrs),
    {value, {app_vsn, AppVsn}}   = lists:keysearch(app_vsn, 1, Attrs),
    io:format("~p version info: "
	      "~n   VSN:     ~p"
	      "~n   App vsn: ~s"
	      "~n", [?MODULE, Vsn, AppVsn]),
    ok.


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
		{value, {ts, snmp}} ->
		    %% Always run the testcase if we are using our own
		    %% test-server...
		    ok;
		_ ->
		    try Condition() of
			true ->
			    skip(non_pc_testcase, File, Line);
			false ->
			    ok
                    catch
                        C:E:S ->
                            skip({condition, C, E, S}, File, Line)
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


%% A modern take on the "Check if our host handle IPv6" question.
%% 
has_support_ipv6() ->
    case os:type() of
        {win32, _} ->
            %% We do not yet have support for windows in the socket nif,
            %% so for windows we need to use the old style...
            old_has_support_ipv6();
        _ ->
            socket:is_supported(ipv6) andalso has_valid_ipv6_address()
    end.

has_valid_ipv6_address() ->
    case net:getifaddrs(fun(#{addr  := #{family := inet6},
                              flags := Flags}) ->
                                not lists:member(loopback, Flags);
                           (_) ->
                                false
                        end) of
        {ok, [#{addr := #{addr := LocalAddr}}|_]} ->
            %% At least one valid address, we pick the first...
            try validate_ipv6_address(LocalAddr)
            catch
                _:_:_ ->
                    false
            end;
        {ok, _} ->
            false;
        {error, _} ->
            false
    end.

validate_ipv6_address(LocalAddr) ->
    Domain = inet6,
    ServerSock =
        case socket:open(Domain, dgram, udp) of
            {ok, SS} ->
                SS;
            {error, R2} ->
                ?SKIP(f("(server) socket open failed: ~p", [R2]))
        end,
    LocalSA = #{family => Domain, addr => LocalAddr},
    ServerPort =
        case socket:bind(ServerSock, LocalSA) of
            {ok, P1} ->
                P1;
            {error, R3} ->
                socket:close(ServerSock),
                ?SKIP(f("(server) socket bind failed: ~p", [R3]))
        end,
    ServerSA = LocalSA#{port => ServerPort},
    ClientSock =
        case socket:open(Domain, dgram, udp) of
            {ok, CS} ->
                CS;
            {error, R4} ->
                ?SKIP(f("(client) socket open failed: ~p", [R4]))
        end,
    case socket:bind(ClientSock, LocalSA) of
        {ok, _} ->
            ok;
        {error, R5} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            ?SKIP(f("(client) socket bind failed: ~p", [R5]))
    end,
    case socket:sendto(ClientSock, <<"hejsan">>, ServerSA) of
        ok ->
            ok;
        {error, R6} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            ?SKIP(f("failed socket sendto test: ~p", [R6]))
    end,
    case socket:recvfrom(ServerSock) of
        {ok, {_, <<"hejsan">>}} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            true;
        {error, R7} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            ?SKIP(f("failed socket recvfrom test: ~p", [R7]))
   end.




old_has_support_ipv6() ->
    case inet:gethostname() of
        {ok, Hostname} ->
            old_has_support_ipv6(Hostname) andalso old_is_ipv6_host(Hostname);
        _ ->
            false
    end.

old_has_support_ipv6(Hostname) ->
    case inet:getaddr(Hostname, inet6) of
        {ok, Addr} when (size(Addr) =:= 8) andalso
                        (element(1, Addr) =/= 0) andalso
                        (element(1, Addr) =/= 16#fe80) ->
            true;
        _ ->
            false
    end.
            
old_is_ipv6_host(Hostname) ->
    case ct:require(ipv6_hosts) of
        ok ->
            lists:member(list_to_atom(Hostname), ct:get_config(ipv6_hosts));
        _ ->
            false
    end.



%% ----------------------------------------------------------------
%% Test suite utility functions
%% 

%% Common suite init function
%% This should be used by "all" suite init functions.

init_per_suite(Config) ->

    ct:timetrap(minutes(2)),

    try analyze_and_print_host_info() of
        {Factor, HostInfo} when is_integer(Factor) ->
            try maybe_skip(HostInfo) of
                true ->
                    {skip, "Unstable host and/or os (or combo thererof)"};
                false ->
                    snmp_test_global_sys_monitor:start(),
                    [{snmp_factor, Factor} | Config]
            catch
                throw:{skip, _} = SKIP ->
                    SKIP
            end
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

maybe_skip(HostInfo) ->

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
           (V) when (V > {2,6,24}) ->
                false; % OK - No skip
           (V) when (V =:= {2,6,10}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "MontaVista" ++ _ -> % Stone age MontaVista => Skip
                        %% The real problem is that the machine is *very* slow
                        true;
                    _ ->
                        false
                end;
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
        fun() ->
                SysMan = win_sys_info_lookup(system_manufacturer, HostInfo),
                case string:to_lower(SysMan) of
                    "vmware" ++ _ ->
                        true;
                    _ ->
                        false
                end
        end,
    COND = [{unix,  [{linux, LinuxVersionVerify}, 
                     {darwin, DarwinVersionVerify}]},
            {win32, SkipWindowsOnVirtual}],
    os_based_skip(COND).


end_per_suite(Config) when is_list(Config) ->

    snmp_test_global_sys_monitor:stop(),

    Config.



fix_data_dir(Config) ->
    DataDir0     = lookup(data_dir, Config),
    DataDir1     = filename:split(filename:absname(DataDir0)),
    [_|DataDir2] = lists:reverse(DataDir1),
    DataDir      = filename:join(lists:reverse(DataDir2) ++ [?snmp_test_data]),
    Config1      = lists:keydelete(data_dir, 1, Config),
    [{data_dir, DataDir ++ "/"} | Config1].


init_suite_top_dir(Suite, Config0) ->
    io:format("~w:init_suite_top_dir -> entry with"
	      "~n   Suite:   ~p"
	      "~n   Config0: ~p"
	      "~n", [?MODULE, Suite, Config0]),
    Dir         = lookup(priv_dir, Config0),
    SuiteTopDir = filename:join(Dir, Suite),
    case file:make_dir(SuiteTopDir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, Reason} ->
            fail({failed_creating_suite_top_dir, SuiteTopDir, Reason}, 
		 ?MODULE, ?LINE)
    end,

    %% This is just in case...
    Config1 = lists:keydelete(snmp_group_top_dir, 1, Config0), 
    Config2 = lists:keydelete(snmp_suite_top_dir, 1, Config1), 
    [{snmp_suite_top_dir, SuiteTopDir} | Config2].


init_group_top_dir(GroupName, Config) ->
    io:format("~w:init_group_top_dir -> entry with"
	      "~n   GroupName: ~p"
	      "~n   Config:    ~p"
	      "~n", [?MODULE, GroupName, Config]),
    case lists:keysearch(snmp_group_top_dir, 1, Config) of
	{value, {_Key, Dir}} ->
	    %% This is a sub-group, so create our dir within Dir
	    GroupTopDir = filename:join(Dir, GroupName),
	    case file:make_dir(GroupTopDir) of
		ok ->
		    ok;
		{error, Reason} ->
		    fail({failed_creating_group_top_dir, GroupTopDir, Reason}, 
			 ?MODULE, ?LINE)
	    end,
	    [{snmp_group_top_dir, GroupTopDir} | Config];

	_ ->
	    case lists:keysearch(snmp_suite_top_dir, 1, Config) of
		{value, {_Key, Dir}} ->
		    GroupTopDir = filename:join(Dir, GroupName),
		    case file:make_dir(GroupTopDir) of
			ok ->
			    ok;
			{error, Reason} ->
			    fail({failed_creating_group_top_dir, 
				  GroupTopDir, Reason}, 
				 ?MODULE, ?LINE)
		    end,
		    [{snmp_group_top_dir, GroupTopDir} | Config];
		_ ->
		    fail(could_not_find_suite_top_dir, ?MODULE, ?LINE)
	    end
    end.


init_testcase_top_dir(Case, Config) ->
    io:format("~w:init_testcase_top_dir -> entry with"
	      "~n   Case:   ~p"
	      "~n   Config: ~p"
	      "~n", [?MODULE, Case, Config]),
    case lists:keysearch(snmp_group_top_dir, 1, Config) of
	{value, {_Key, Dir}} ->
	    CaseTopDir = filename:join(Dir, Case),
	    ok = file:make_dir(CaseTopDir),
	    CaseTopDir;
	false ->
	    case lists:keysearch(snmp_suite_top_dir, 1, Config) of
		{value, {_Key, Dir}} ->
		    CaseTopDir = filename:join(Dir, Case),
		    ok = file:make_dir(CaseTopDir),
		    CaseTopDir;
		false ->
		    fail(failed_creating_case_top_dir, ?MODULE, ?LINE)
	    end
    end.


replace_config(Key, Config, NewValue) ->
    lists:keyreplace(Key, 1, Config, {Key, NewValue}).

set_config(Key, Def, Config) ->
    case get_config(Key, Config) of
        undefined ->
            [{Key, Def}|Config];
        _ ->
            Config
    end.

get_config(Key,C) ->
    get_config(Key,C,undefined).

get_config(Key,C,Default) ->
    case lists:keysearch(Key,1,C) of
        {value,{Key,Val}} ->
            Val;
        _ ->
            Default
    end.

lookup(Key, Config) ->
    {value, {Key, Value}} = lists:keysearch(Key, 1, Config),
    Value.

fail(Reason, Mod, Line) ->
    exit({suite_failed, Reason, Mod, Line}).
    
skip(Reason, Module, Line) ->
    String = lists:flatten(io_lib:format("Skipping ~p(~p): ~p~n", 
					 [Module, Line, Reason])),
    exit({skip, String}).
    

%% This function prints various host info, which might be usefull
%% when analyzing the test suite (results).
%% It also returns a "factor" that can be used when deciding 
%% the load for some test cases. Such as run time or number of
%% iterations. This only works for some OSes.
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
        {unix, darwin} ->
            analyze_and_print_darwin_host_info(Version);
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
            {num_schedulers_to_factor(), []}
    end.

linux_which_distro(Version) ->
    case file:read_file_info("/etc/issue") of
        {ok, _} ->
            case [string:trim(S) ||
                     S <- string:tokens(os:cmd("cat /etc/issue"), [$\n])] of
                [DistroStr|_] ->
                    io:format("Linux: ~s"
                              "~n   ~s"
                              "~n",
                              [Version, DistroStr]),
                    case DistroStr of
                        "Wind River Linux" ++ _ ->
                            wind_river;
                        "MontaVista" ++ _ ->
                            montavista;
                        "Yellow Dog" ++ _ ->
                            yellow_dog;
                        _ ->
                            other
                    end;
                X ->
                    io:format("Linux: ~s"
                              "~n   ~p"
                              "~n",
                              [Version, X]),
                    other
            end;
        _ ->
            io:format("Linux: ~s"
                      "~n", [Version]),
            other
    end.
    
analyze_and_print_linux_host_info(Version) ->
    Distro =
        case file:read_file_info("/etc/issue") of
            {ok, _} ->
                linux_which_distro(Version);
            _ ->
                io:format("Linux: ~s"
                          "~n", [Version]),
                other
        end,
    Factor =
        case (catch linux_which_cpuinfo(Distro)) of
            {ok, {CPU, BogoMIPS}} ->
                io:format("CPU: "
                          "~n   Model:          ~s"
                          "~n   BogoMIPS:       ~w"
                          "~n   Num Schedulers: ~s"
                          "~n", [CPU, BogoMIPS, str_num_schedulers()]),
                if
                    (BogoMIPS > 20000) ->
                        1;
                    (BogoMIPS > 10000) ->
                        2;
                    (BogoMIPS > 5000) ->
                        3;
                    (BogoMIPS > 2000) ->
                        5;
                    (BogoMIPS > 1000) ->
                        8;
                    true ->
                        10
                end;
            {ok, CPU} ->
                io:format("CPU: "
                          "~n   Model:          ~s"
                          "~n   Num Schedulers: ~s"
                          "~n", [CPU, str_num_schedulers()]),
                NumChed = erlang:system_info(schedulers),
                if
                    (NumChed > 2) ->
                        2;
                    true ->
                        5
                end;
            _ ->
                5
        end,
    %% Check if we need to adjust the factor because of the memory
    try linux_which_meminfo() of
        AddFactor ->
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            {Factor + AddFactor, []}
    catch
        _:_:_ ->
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            {Factor, []}
    end.



linux_cpuinfo_lookup(Key) when is_list(Key) ->
    linux_info_lookup(Key, "/proc/cpuinfo").

linux_cpuinfo_cpu() ->
    case linux_cpuinfo_lookup("cpu") of
        [Model] ->
            Model;
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

linux_cpuinfo_bogomips() ->
    case linux_cpuinfo_lookup("bogomips") of
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
            "-"
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

%% Check for x86 (Intel or AMD)
linux_which_cpuinfo(other) ->
    CPU =
        case linux_cpuinfo_model_name() of
            "-" ->
                %% ARM (at least some distros...)
                case linux_cpuinfo_processor() of
                    "-" ->
                        %% Ok, we give up
                        throw(noinfo);
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
            end
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
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
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
            {CPUFactor + MemAddFactor, []}
        end
    catch
        _:_:_ ->
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            {2, []}
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
                      "~n   Num Schedulers: ~s"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n",
                      [CPU, CPUSpeed, NCPU, str_num_schedulers(), Memory]),
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
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
                      "~n   Num Schedulers: ~s"
                      "~n", [str_num_schedulers()]),
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
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
%%       Processor Name: Intel Core i5
%%       Processor Speed: 2,6 GHz
%%       Number of Processors: 1
%%       Total Number of Cores: 2
%%       L2 Cache (per Core): 256 KB
%%       L3 Cache: 3 MB
%%       Hyper-Threading Technology: Enabled
%%       Memory: 16 GB

analyze_and_print_darwin_host_info(Version) ->
    %% This stuff is for macOS.
    %% If we ever tested on a pure darwin machine,
    %% we need to find some other way to find some info...
    %% Also, I suppose its possible that we for some other
    %% reason *fail* to get the info...
    case analyze_darwin_software_info() of
        [] ->
            io:format("Darwin:"
                      "~n   Version:        ~s"
                      "~n   Num Schedulers: ~s"
                      "~n", [Version, str_num_schedulers()]),
            {num_schedulers_to_factor(), []};
        SwInfo when  is_list(SwInfo) ->
            SystemVersion = analyze_darwin_sw_system_version(SwInfo),
            KernelVersion = analyze_darwin_sw_kernel_version(SwInfo),
            HwInfo        = analyze_darwin_hardware_info(),
            ModelName     = analyze_darwin_hw_model_name(HwInfo),
            ModelId       = analyze_darwin_hw_model_identifier(HwInfo),
            ProcName      = analyze_darwin_hw_processor_name(HwInfo),
            ProcSpeed     = analyze_darwin_hw_processor_speed(HwInfo),
            NumProc       = analyze_darwin_hw_number_of_processors(HwInfo),
            NumCores      = analyze_darwin_hw_total_number_of_cores(HwInfo),
            Memory        = analyze_darwin_hw_memory(HwInfo),
            io:format("Darwin:"
                      "~n   System Version: ~s"
                      "~n   Kernel Version: ~s"
                      "~n   Model:          ~s (~s)"
                      "~n   Processor:      ~s (~s, ~s, ~s)"
                      "~n   Memory:         ~s"
                      "~n   Num Schedulers: ~s"
                      "~n", [SystemVersion, KernelVersion,
                             ModelName, ModelId,
                             ProcName, ProcSpeed, NumProc, NumCores, 
                             Memory,
                             str_num_schedulers()]),
            CPUFactor = analyze_darwin_cpu_to_factor(ProcName,
                                                     ProcSpeed,
                                                     NumProc,
                                                     NumCores),
            MemFactor = analyze_darwin_memory_to_factor(Memory),
            if (MemFactor =:= 1) ->
                    {CPUFactor, []};
               true ->
                    {CPUFactor + MemFactor, []}
            end
    end.

analyze_darwin_sw_system_version(SwInfo) ->
    proplists:get_value("system version", SwInfo, "-").

analyze_darwin_sw_kernel_version(SwInfo) ->
    proplists:get_value("kernel version", SwInfo, "-").

analyze_darwin_software_info() ->
    analyze_darwin_system_profiler("SPSoftwareDataType").

analyze_darwin_hw_model_name(HwInfo) ->
    proplists:get_value("model name", HwInfo, "-").

analyze_darwin_hw_model_identifier(HwInfo) ->
    proplists:get_value("model identifier", HwInfo, "-").

analyze_darwin_hw_processor_name(HwInfo) ->
    proplists:get_value("processor name", HwInfo, "-").

analyze_darwin_hw_processor_speed(HwInfo) ->
    proplists:get_value("processor speed", HwInfo, "-").

analyze_darwin_hw_number_of_processors(HwInfo) ->
    proplists:get_value("number of processors", HwInfo, "-").

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
    case os:cmd("which system_profiler") of
        [] ->
            [];
        _ ->
            D0 = os:cmd("system_profiler " ++ DataType),
            D1 = string:tokens(D0, [$\n]),
            D2 = [string:trim(S1) || S1 <- D1],
            D3 = [string:tokens(S2, [$:]) || S2 <- D2],
            analyze_darwin_system_profiler2(D3)
    end.

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


%% The speed is a string: "<speed> <unit>"
%% the speed may be a float, which we transforms into an integer of MHz.
%% To calculate a factor based on processor speed, number of procs
%% and number of cores is ... not an exact ... science ...
analyze_darwin_cpu_to_factor(_ProcName,
                             ProcSpeedStr, NumProcStr, NumCoresStr) ->
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
    %% Because we count the lines of the output (which may contain
    %% any number of extra crap lines) we need to ensure we only
    %% count the "proper" stdout. So send it to a tmp file first
    %% and then count its number of lines...
    NumPhysCPU =
       try
            begin
                File1 = f("/tmp/psrinfo_p.~s.~w", [os:getpid(), os:system_time()]),
                os:cmd("psrinfo -p > " ++ File1),
                string:trim(os:cmd("cat " ++ File1))
            end
        catch
                _:_:_ ->
                    "-"
        end,
    %% Because we count the lines of the output (which may contain
    %% any number of extra crap lines) we need to ensure we only
    %% count the "proper" stdout. So send it to a tmp file first
    %% and then count its number of lines...
    NumVCPU =
        try
            begin
                File2 = f("/tmp/psrinfo.~s.~w", [os:getpid(), os:system_time()]),
                os:cmd("psrinfo > " ++ File2),
                [NumVCPUStr | _] = string:tokens(os:cmd("wc -l " ++ File2), [$\ ]),
                NumVCPUStr
            end
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
                       NumPhysCPU, NumVCPU,
                       SysConf, MemSz,
                       str_num_schedulers()]),
    io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
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
              "~n   Total Physical Memory:  ~s"
              "~n   Num Schedulers:         ~s"
              "~n~n", [OsName, OsVersion, Version,
                       SysMan, SysMod, NumProcs, TotPhysMem,
                       str_num_schedulers()]),
    io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
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
    proxy_call(F, minutes(1), []).

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
                    


str_num_schedulers() ->
    try erlang:system_info(schedulers) of
        N -> f("~w", [N])
    catch
        _:_:_ -> "-"
    end.

num_schedulers_to_factor() ->
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



%% ----------------------------------------------------------------
%% Time related function
%% 

hours(N)   -> trunc(N * 1000 * 60 * 60).
minutes(N) -> trunc(N * 1000 * 60).
seconds(N) -> trunc(N * 1000).


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


%% ----------------------------------------------------------------
%% Process utility function
%%

flush_mqueue() ->
    io:format("~p~n", [lists:reverse(flush_mqueue([]))]).

flush_mqueue(MQ) ->
    receive
        Any ->
            flush_mqueue([Any|MQ])
    after 0 ->
            MQ
    end.

    
trap_exit() -> 
    {trap_exit, Flag} = process_info(self(),trap_exit), Flag.

trap_exit(Flag) -> 
    process_flag(trap_exit, Flag).



%% ----------------------------------------------------------------
%% Node utility functions
%% 

ping(N) ->
    case net_adm:ping(N) of
 	pang ->
 	    error;
 	pong ->
 	    ok
     end.

local_nodes() ->
    nodes_on(net_adm:localhost()).

nodes_on(Host) when is_list(Host) ->
    net_adm:world_list([list_to_atom(Host)]).


start_node(Name, Args) ->
    Opts = [{cleanup, false}, {args, Args}],
    test_server:start_node(Name, slave, Opts).


stop_node(Node) ->
    test_server:stop_node(Node).


%% ----------------------------------------------------------------
%% Application and Crypto utility functions
%% 

is_app_running(App) when is_atom(App) ->
    Apps = application:which_applications(),
    lists:keymember(App,1,Apps).

is_crypto_running() ->
    is_app_running(crypto).

is_mnesia_running() ->
    is_app_running(mnesia).

is_snmp_running() ->
    is_app_running(snmp).

crypto_start() ->
    try crypto:start() of
        ok ->
            ok;
        {error, {already_started,crypto}} ->
            ok
    catch
        exit:{undef, [{crypto, start, [], []} | _]}:_ ->
            {error, no_crypto};
        C:E:S ->
            {error, {C, E, S}}
    end.
 
crypto_support() ->
    crypto_support([md5, sha], []).
 
crypto_support([], []) ->
    yes;
crypto_support([], Acc) ->
    {no, Acc};
crypto_support([Func|Funcs], Acc) ->
    case is_crypto_supported(Func) of
        true ->
            crypto_support(Funcs, Acc);
        false ->
            crypto_support(Funcs, [Func|Acc])
    end.
 
is_crypto_supported(Func) ->
    snmp_misc:is_crypto_supported(Func). 


%% This function ensures that a *named* process on the local node is not running.
%% It does so by:
%%   1) Wait for 'Timeout' msec
%%   2) If 1 did not work, issue 'stop' and then wait 'Timeout' msec
%%   3) And finally, if 2 did not work, issue exit(kill).
ensure_not_running(Name, Stopper, Timeout)
  when is_atom(Name) andalso
       is_function(Stopper, 0) andalso
       is_integer(Timeout) ->
    ensure_not_running(whereis(Name), Name, Stopper, Timeout).

ensure_not_running(Pid, Name, Stopper, Timeout) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    try
        begin
            ensure_not_running_wait(Pid, MRef, Timeout),
            ensure_not_running_stop(Pid, MRef, Stopper, Timeout),
            ensure_not_running_kill(Pid, MRef, Timeout),
            exit({failed_ensure_not_running, Name})
        end
    catch
        throw:ok ->
            sleep(1000),
            ok
    end;
ensure_not_running(_, _, _, _) ->
    iprint("ensure_not_running -> not running", []),
    sleep(1000), % This should not actually be necessary!
    ok.
    
    
ensure_not_running_wait(Pid, MRef, Timeout) ->
    receive
        {'DOWN', MRef, process, Pid, _Info} ->
            iprint("ensure_not_running_wait -> died peacefully", []),
            throw(ok)
    after Timeout ->
            wprint("ensure_not_running_wait -> giving up", []),
            ok
    end.

ensure_not_running_stop(Pid, MRef, Stopper, Timeout) ->
    %% Spawn a stop'er process
    StopPid = spawn(Stopper),
    receive
        {'DOWN', MRef, process, Pid, _Info} ->
            nprint("ensure_not_running_stop -> dead (stopped)", []),
            throw(ok)
    after Timeout ->
            wprint("ensure_not_running_stop -> giving up", []),
            exit(StopPid, kill),
            ok
    end.

ensure_not_running_kill(Pid, MRef, Timeout) ->
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, Pid, _Info} ->
            nprint("ensure_not_running_kill -> dead (killed)", []),
            throw(ok)
    after Timeout ->
            wprint("ensure_not_running_kill -> giving up", []),
            ok
    end.


%% ----------------------------------------------------------------
%% Watchdog functions
%% 

watchdog_start(Timeout) ->
    watchdog_start(unknown, Timeout).

watchdog_start(Case, Timeout) ->
    spawn_link(?MODULE, watchdog, [Case, Timeout, self()]).

watchdog_stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    ok.

watchdog(Case, Timeout0, Pid) ->
    process_flag(priority, max),
    Timeout = timeout(Timeout0),
    receive
    after Timeout ->
	    Mon = erlang:monitor(process, Pid),
	    case erlang:process_info(Pid) of
		undefined ->
		    ok;
		ProcInfo ->
		    Line = 
			case lists:keysearch(dictionary, 1, ProcInfo) of
			    {value, {_, Dict}} when is_list(Dict) ->
				case lists:keysearch(test_server_loc, 1, Dict) of
				    {value, {_, {_Mod, L}}} when is_integer(L) ->
					L;
				    _ ->
					0
				end;
			    _ -> % This borders on paranoia, but...
				0
			end,
		    Trap = {timetrap_timeout, Timeout, Line},
		    exit(Pid, Trap),
		    receive
			{'DOWN', Mon, process, Pid, _} ->
			    ok
		    after 10000 ->
			    warning_msg("Failed stopping "
					"test case ~p process ~p "
					"[~w] after ~w: killing instead", 
				[Case, Pid, Line, Timeout]),
			    exit(Pid, kill)
		    end
	    end
    end.

warning_msg(F, A) ->
    (catch error_logger:warning_msg(F ++ "~n", A)).

timeout(T) ->
    trunc(timeout(T, os:type())).

timeout(T, _) ->
    T * timetrap_scale_factor().
            
timetrap_scale_factor() ->
    case (catch test_server:timetrap_scale_factor()) of
	{'EXIT', _} ->
	    1;
	N ->
	    N
    end.

    
%% ----------------------------------------------------------------------
%% file & dir functions
%%

del_dir(Dir) when is_list(Dir) ->
    (catch do_del_dir(Dir)).

do_del_dir(Dir) ->
    io:format("delete directory ~s~n", [Dir]),
    case file:list_dir(Dir) of
	{ok, Files} -> 
	    Files2 = [filename:join(Dir, File) || File <- Files],
	    del_dir2(Files2),
	    case file:del_dir(Dir) of
		ok ->
		    io:format("directory ~s deleted~n", [Dir]),
		    ok;
		{error, eexist} = Error1 ->
		    io:format("directory not empty: ~n", []),
		    {ok, Files3} = file:list_dir(Dir),
		    io:format("found additional files: ~n~p~n", 
			      [Files3]),
		    throw(Error1);
		{error, Reason2} = Error2 ->
		    io:format("failed deleting directory: ~w~n", [Reason2]),
		    throw(Error2)
	    end;
	Else ->
	    Else
    end.
    
del_dir2([]) ->
    ok;
del_dir2([File|Files]) ->
    del_file_or_dir(File),
    del_dir2(Files).

del_file_or_dir(FileOrDir) ->
    case file:read_file_info(FileOrDir) of
	{ok, #file_info{type = directory}} ->
	    do_del_dir(FileOrDir);
	{ok, _} ->
	    io:format("   delete file ~s~n", [FileOrDir]),
	    case file:delete(FileOrDir) of
		ok ->
		    io:format("   => deleted~n", []),
		    ok;
		{error, Reason} = Error ->
		    io:format("   => failed - ~w~n", [Reason]),
		    throw(Error)
	    end;
		
	_ ->
	    ok
    end.
	    

%% ----------------------------------------------------------------------
%% (debug) Print functions
%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

p(Mod, Case) when is_atom(Mod) andalso is_atom(Case) ->
    case get(test_case) of
	undefined ->
	    put(test_case, Case),
	    p("~n~n************ ~w:~w ************", [Mod, Case]);
	_ ->
	    ok
    end;

p(F, A) when is_list(F) andalso is_list(A) ->
    io:format(user, F ++ "~n", A).

%% This is just a bog standard printout, with a (formatted) timestamp
%% prefix and a newline after.
%% print1 - prints to both standard_io and user.
%% print2 - prints to just standard_io.

print_format(F, A) ->
    FTS = snmp_test_lib:formated_timestamp(),
    io_lib:format("[~s] " ++ F ++ "~n", [FTS | A]).

print1(F, A) ->
    S = print_format(F, A),
    io:format("~s", [S]),
    io:format(user, "~s", [S]).

print2(F, A) ->
    S = print_format(F, A),
    io:format("~s", [S]).


print(Prefix, Module, Line, Format, Args) ->
    io:format("*** [~s] ~s ~p ~p ~p:~p *** " ++ Format ++ "~n", 
	      [formated_timestamp(), 
	       Prefix, node(), self(), Module, Line|Args]).

formated_timestamp() ->
    snmp_misc:formated_timestamp().


%% ----------------------------------------------------------------------
%%
%% General purpose print functions
%% ERROR, WARNING and NOTICE are written both to 'user' and 'standard_io'.
%% INFO only to 'standard_io'.
%%
%% Should we also allow for (optional) a "short name" (sname)?
%%

%% ERROR print (both to user and standard_io)
eprint(F, A) ->
    Str = format_print("ERROR", F, A),
    io:format(user,        "~s~n", [Str]),
    io:format(standard_io, "~s~n", [Str]).

%% WARNING print (both to user and standard_io)
wprint(F, A) ->
    Str = format_print("WARNING", F, A),
    io:format(user,        "~s~n", [Str]),
    io:format(standard_io, "~s~n", [Str]).

%% NOTICE print (both to user and standard_io)
nprint(F, A) ->
    Str = format_print("NOTICE", F, A),
    io:format(user,        "~s~n", [Str]),
    io:format(standard_io, "~s~n", [Str]).

%% INFO print (only to user)
iprint(F, A) -> 
    Str = format_print("INFO", F, A),
    io:format(standard_io, "~s~n", [Str]).

format_print(Prefix, F, A) ->
    format_print(get(tname), Prefix, F, A).

format_print(undefined, Prefix, F, A) ->
    f("*** [~s] ~s ~p ~p *** ~n" ++ F ++ "~n",
      [formated_timestamp(), Prefix, node(), self() | A]);
format_print(TName, Prefix, F, A) when is_atom(TName) ->
    format_print(atom_to_list(TName), Prefix, F, A);
format_print(TName, Prefix, F, A) when is_list(TName) ->
    f("*** [~s] ~s ~s ~p ~p *** ~n" ++ F ++ "~n",
      [formated_timestamp(), Prefix, TName, node(), self() | A]).


