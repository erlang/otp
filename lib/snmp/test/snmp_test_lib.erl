%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2015. All Rights Reserved.
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


-export([hostname/0, hostname/1, localhost/0, localhost/1, os_type/0, sz/1,
	 display_suite_info/1]).
-export([non_pc_tc_maybe_skip/4, os_based_skip/1]).
-export([fix_data_dir/1, 
	 init_suite_top_dir/2, init_group_top_dir/2, init_testcase_top_dir/2, 
	 lookup/2, 
	 replace_config/3, set_config/3, get_config/2, get_config/3]).
-export([fail/3, skip/3]).
-export([hours/1, minutes/1, seconds/1, sleep/1]).
-export([flush_mqueue/0, trap_exit/0, trap_exit/1]).
-export([ping/1, local_nodes/0, nodes_on/1]).
-export([start_node/2]).
-export([is_app_running/1, 
	 is_crypto_running/0, is_mnesia_running/0, is_snmp_running/0]).
-export([crypto_start/0, crypto_support/0]).
-export([watchdog/3, watchdog_start/1, watchdog_start/2, watchdog_stop/1]).
-export([del_dir/1]).
-export([cover/1]).
-export([p/2, print/5, formated_timestamp/0]).


%% ----------------------------------------------------------------------
%% Misc functions
%%

hostname() ->
    hostname(node()).

hostname(Node) ->
    from($@, atom_to_list(Node)).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

localhost() ->
    {ok, Ip} = snmp_misc:ip(net_adm:localhost()),
    Ip.
localhost(Family) ->
    {ok, Ip} = snmp_misc:ip(net_adm:localhost(), Family),
    Ip.

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
		    case Condition() of
			true ->
			    skip(non_pc_testcase, File, Line);
			false ->
			    ok
		    end
	    end
    end.


os_based_skip(any) ->
    io:format("os_based_skip(any) -> entry"
	      "~n", []), 
    true;
os_based_skip(Skippable) when is_list(Skippable) ->
    io:format("os_based_skip -> entry with"
	      "~n   Skippable: ~p"
	      "~n", [Skippable]), 
    {OsFam, OsName} =
        case os:type() of
            {_Fam, _Name} = FamAndName ->
                FamAndName;
            Fam ->
                {Fam, undefined}
        end,
    io:format("os_based_skip -> os-type: "
	      "~n   OsFam: ~p"
	      "~n   OsName: ~p"
	      "~n", [OsFam, OsName]), 
    case lists:member(OsFam, Skippable) of
        true ->
            true;
        false ->
            case lists:keysearch(OsFam, 1, Skippable) of
                {value, {OsFam, OsName}} ->
                    true;
                {value, {OsFam, OsNames}} when is_list(OsNames) ->
                    case lists:member(OsName, OsNames) of
			true ->
			    true;
			false ->
			    case lists:keymember(OsName, 1, OsNames) of
				{value, {OsName, Check}} when is_function(Check) ->
				    Check();
				_ ->
				    false
			    end
		    end;
                _ ->
                    false
            end
    end;
os_based_skip(_Crap) ->
    io:format("os_based_skip -> entry with"
	      "~n   _Crap: ~p"
	      "~n", [_Crap]), 
    false.


%% ----------------------------------------------------------------
%% Test suite utility functions
%% 

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
    Opts = [{cleanup,false}, {args,Args}],
    test_server:start_node(Name, slave, Opts).


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

print(Prefix, Module, Line, Format, Args) ->
    io:format("*** [~s] ~s ~p ~p ~p:~p *** " ++ Format ++ "~n", 
	      [formated_timestamp(), 
	       Prefix, node(), self(), Module, Line|Args]).

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w ~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).
