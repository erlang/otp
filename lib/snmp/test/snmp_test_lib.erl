%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2019. All Rights Reserved.
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


-export([tc_try/2, tc_try/3]).
-export([hostname/0, hostname/1, localhost/0, localhost/1, os_type/0, sz/1,
	 display_suite_info/1]).
-export([non_pc_tc_maybe_skip/4, os_based_skip/1,
         has_support_ipv6/0, has_support_ipv6/1,
         is_ipv6_host/0, is_ipv6_host/1]).
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
	 is_crypto_running/0, is_mnesia_running/0, is_snmp_running/0]).
-export([crypto_start/0, crypto_support/0]).
-export([watchdog/3, watchdog_start/1, watchdog_start/2, watchdog_stop/1]).
-export([del_dir/1]).
-export([cover/1]).
-export([f/2, p/2, print1/2, print2/2, print/5, formated_timestamp/0]).


%% ----------------------------------------------------------------------
%% Run test-case
%%

%% *** tc_try/2,3 ***
%% Case:      Basically the test case name
%% TCCondFun: A fun that is evaluated before the actual test case
%%            The point of this is that it can performs checks to
%%            see if we shall run the test case at all.
%%            For instance, the test case may only work in specific
%%            conditions.
%% FCFun:     The test case fun
tc_try(Case, TCFun) ->
    tc_try(Case, fun() -> ok end, TCFun).
                        
tc_try(Case, TCCondFun, TCFun)
  when is_atom(Case) andalso 
       is_function(TCCondFun, 0) andalso 
       is_function(TCFun, 0) ->
    tc_begin(Case),
    try TCCondFun() of
        ok ->
            try 
                begin
                    TCFun(),
                    sleep(seconds(1)),
                    tc_end("ok")
                end
            catch
                C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
                    tc_end( f("skipping(catched,~w,tc)", [C]) ),
                    SKIP;
                C:E:S ->
                    %% We always check the system events before we accept a failure
                    case snmp_test_global_sys_monitor:events() of
                        [] ->
                            tc_end( f("failed(catched,~w,tc)", [C]) ),
                            erlang:raise(C, E, S);
                        SysEvs ->
                            tc_print("System Events received: "
                                     "~n   ~p", [SysEvs], "", ""),
                            tc_end( f("skipping(catched-sysevs,~w,tc)", [C]) ),
                            SKIP = {skip, "TC failure with system events"},
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

tc_print(F, Before, After) ->
    tc_print(F, [], Before, After).

tc_print(F, A, Before, After) ->
    Name = tc_which_name(),
    FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
             [formated_timestamp(),Name,self()|A]),
    io:format(user, Before ++ FStr ++ After, []).

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

hostname() ->
    hostname(node()).

hostname(Node) ->
    case string:tokens(atom_to_list(Node), [$@]) of
        [_, Host] ->
            Host;
        _ ->
            []
    end.

%% localhost() ->
%%     {ok, Ip} = snmp_misc:ip(net_adm:localhost()),
%%     Ip.
%% localhost(Family) ->
%%     {ok, Ip} = snmp_misc:ip(net_adm:localhost(), Family),
%%     Ip.

%% localhost() ->
%%     {ok, Ip} = snmp_misc:ip(net_adm:localhost()),
%%     Ip.
%% localhost(Family) ->
%%     {ok, Ip} = snmp_misc:ip(net_adm:localhost(), Family),
%%     Ip.

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


%% A basic test to check if current host supports IPv6
has_support_ipv6() ->
    case inet:gethostname() of
        {ok, Hostname} ->
            has_support_ipv6(Hostname);
        _ ->
            false
    end.

has_support_ipv6(Hostname) ->
    case inet:getaddr(Hostname, inet6) of
        {ok, Addr} when (size(Addr) =:= 8) andalso
                        (element(1, Addr) =/= 0) andalso
                        (element(1, Addr) =/= 16#fe80) ->
            true;
        {ok, _} ->
            false;
        {error, _} ->
            false
    end.
		

is_ipv6_host() ->
    case inet:gethostname() of
        {ok, Hostname} ->
            is_ipv6_host(Hostname);
        {error, _} ->
            false
    end.

is_ipv6_host(Hostname) ->
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
    COND = [{unix, [{linux, LinuxVersionVerify}]}],
    case os_based_skip(COND) of
        true ->
            {skip, "Unstable host and/or os (or combo thererof)"};
        false ->
            snmp_test_global_sys_monitor:start(),
            Config
    end.


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
    {trap_exit,Flag} = process_info(self(),trap_exit),Flag.

trap_exit(Flag) -> 
    process_flag(trap_exit,Flag).



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
    case (catch crypto:start()) of
        ok ->
            ok;
        {error, {already_started,crypto}} ->
            ok;
	{'EXIT', Reason} ->
	    {error, {exit, Reason}};
        Else ->
            Else
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
%% cover functions
%%

cover([Suite, Case] = Args) when is_atom(Suite) andalso is_atom(Case) ->
    Mods0 = cover:compile_directory("../src"),
    Mods1 = [Mod || {ok, Mod} <- Mods0],
    snmp_test_server:t(Args),
    Files0 = [cover:analyse_to_file(Mod) || Mod <- Mods1],
    [io:format("Cover output: ~s~n", [File]) || {ok, File} <- Files0],
    ok.


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

