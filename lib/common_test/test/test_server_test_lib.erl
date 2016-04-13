%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(test_server_test_lib).

-export([parse_suite/1]).
-export([init/2, pre_init_per_testcase/3, post_end_per_testcase/4]).

%% for test_server_SUITE when node can not be started as slave
-export([prepare_tester_node/2]).

-include("test_server_test_lib.hrl").

%% The CTH hooks all tests
init(_Id, _Opts) ->
    [].

pre_init_per_testcase(_TC,Config,State) ->
    case os:type() of
	{win32, _} ->
	    %% Extend timeout for windows as starting node
	    %% can take a long time there
	    test_server:timetrap( 120000 * test_server:timetrap_scale_factor());
	_ ->
	    ok
    end,
    {start_slave(Config, 50),State}.

start_slave(Config,_Level) ->
    [_,Host] = string:tokens(atom_to_list(node()), "@"),
    
    ct:log("Trying to start ~s~n", 
	   ["test_server_tester@"++Host]),
    case slave:start(Host, test_server_tester, []) of
	{error,Reason} ->
	    test_server:fail(Reason);
	{ok,Node} ->
	    ct:log("Node ~p started~n", [Node]),
	    IsCover = test_server:is_cover(),
	    if IsCover ->
		    cover:start(Node);
	       true->
		    ok
	    end,
	    prepare_tester_node(Node,Config)
    end.

prepare_tester_node(Node,Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    %% We would normally use priv_dir for temporary data,
    %% but the pathnames gets too long on Windows.
    %% Until the run-time system can support long pathnames,
    %% use the data dir.
    WorkDir = DataDir,

    %% WorkDir as well as directory of Test Server suites
    %% have to be in code path on Test Server node.
    [_ | Parts] = lists:reverse(filename:split(DataDir)),
    TSDir = filename:join(lists:reverse(Parts)),
    AddPathDirs = case proplists:get_value(path_dirs, Config) of
		      undefined -> [];
		      Ds -> Ds
		  end,
    PathDirs = [WorkDir,TSDir | AddPathDirs],
    [true = rpc:call(Node, code, add_patha, [D]) || D <- PathDirs],
    io:format("Dirs added to code path (on ~w):~n",
	      [Node]),
    [io:format("~s~n", [D]) || D <- PathDirs],

    true = rpc:call(Node, os, putenv,
		    ["TEST_SERVER_FRAMEWORK", "undefined"]),

    ok = rpc:call(Node, file, set_cwd, [WorkDir]),
    [{node,Node}, {work_dir,WorkDir} | Config].

post_end_per_testcase(_TC, Config, Return, State) ->
    Node = proplists:get_value(node, Config),
    Cover = test_server:is_cover(),
    if Cover-> cover:flush(Node);
       true -> ok
    end,
    erlang:monitor_node(Node, true),
    slave:stop(Node),
    receive
	{nodedown, Node} ->
	    if Cover -> cover:stop(Node);
	       true -> ok
	    end
    after 5000 ->
	    erlang:monitor_node(Node, false),
	    receive {nodedown, Node} -> ok after 0 -> ok end %flush
    end,
    {Return, State}.

%% Parse an .suite log file
parse_suite(FileName) ->
    
    case file:open(FileName, [read, raw, read_ahead]) of
	{ok, Fd} ->
	    Data = parse_suite(Fd, #suite{ }),
	    file:close(Fd),
	    {ok, Data};
	_ ->
	    error
    end.

fline(Fd) ->
    case prim_file:read_line(Fd) of
	eof -> eof;
	{ok, Line} -> Line
    end.

parse_suite(Fd, S) ->
    _Started                  = fline(Fd),
    _Starting                 = fline(Fd),
    "=cases"        ++ NCases = fline(Fd),
    "=user"         ++ _User  = fline(Fd),
    "=host"         ++ Host   = fline(Fd),
    "=hosts"        ++ _Hosts = fline(Fd),
    "=emulator_vsn" ++ Evsn   = fline(Fd),
    "=emulator"     ++ Emu    = fline(Fd),
    "=otp_release"  ++ OtpRel = fline(Fd),
    "=started"      ++ Start  = fline(Fd),
    NewS = parse_cases(Fd, S#suite{
			     n_cases_expected = list_to_int(clean(NCases)),
			     host             = list_to_binary(clean(Host)),
			     emulator_vsn     = list_to_binary(clean(Evsn)),
			     emulator         = list_to_binary(clean(Emu)),
			     otp_release      = list_to_binary(clean(OtpRel)),
			     started          = list_to_binary(clean(Start))
			    }),
    "=failed"       ++ Failed  = fline(Fd),
    "=successful"   ++ Succ    = fline(Fd),
    "=user_skipped" ++ UsrSkip = fline(Fd),
    "=auto_skipped" ++ AutSkip  = fline(Fd),
    NewS#suite{ n_cases_failed = list_to_int(clean(Failed)),
		n_cases_succ   = list_to_int(clean(Succ)),
		n_cases_user_skip   = list_to_int(clean(UsrSkip)),
		n_cases_auto_skip   = list_to_int(clean(AutSkip)) }.
    

parse_cases(Fd, #suite{ n_cases = N, 
			cases = Cases } = S) ->
    case parse_case(Fd) of
	finished -> S#suite{ log_ok = true };
	{eof, Tc} -> 
	    S#suite{ n_cases = N + 1,
		     cases = [Tc#tc{ result = crashed }|Cases]};
	{ok, Case} ->
	    parse_cases(Fd, S#suite{ n_cases = N + 1, 
				     cases = [Case|Cases]})
    end.

parse_case(Fd) -> parse_case(Fd, #tc{}).
parse_case(Fd, Tc) -> parse_case(fline(Fd), Fd, Tc).

parse_case(eof, _, Tc) -> {eof, Tc};
parse_case("=case" ++ Case, Fd, Tc) ->
    Name = list_to_binary(clean(Case)),
    parse_case(fline(Fd), Fd, Tc#tc{ name = Name });
parse_case("=logfile" ++ File, Fd, Tc) ->
    Log = list_to_binary(clean(File)),
    parse_case(fline(Fd), Fd, Tc#tc{ logfile = Log });
parse_case("=elapsed" ++ Elapsed, Fd, Tc) ->
    {ok, [Time], _} = io_lib:fread("~f", clean(Elapsed)),
    parse_case(fline(Fd), Fd, Tc#tc{ elapsed = Time });
parse_case("=result" ++ Result, _, Tc) ->
    case clean(Result) of
	"ok" ++ _ ->
	    {ok, Tc#tc{ result = ok } };
	"failed" ++ _ ->
	    {ok, Tc#tc{ result = failed } };
	"skipped" ++ _ ->
	    {ok, Tc#tc{ result = skip } };
	"auto_skipped" ++ _ ->
	    {ok, Tc#tc{ result = auto_skip } }
    end;
parse_case("=finished" ++ _ , _Fd, #tc{ name = undefined }) ->
    finished;
parse_case(_, Fd, Tc) ->
    parse_case(fline(Fd), Fd, Tc).

skip([]) -> [];
skip([$ |Ts]) -> skip(Ts);
skip(Ts) -> Ts.

%rmnl(L) -> L.
rmnl([]) -> [];
rmnl([$\n | Ts]) -> rmnl(Ts);
rmnl([T|Ts]) -> [T | rmnl(Ts)].

clean(L) -> 
    rmnl(skip(L)).

list_to_int(L) ->
    try
	list_to_integer(L)
    catch
	_:_ ->
	    0
    end.
