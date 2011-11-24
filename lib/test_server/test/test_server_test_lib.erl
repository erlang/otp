%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(test_server_test_lib).
-export([parse_suite/1]).
-export([init/2, pre_init_per_testcase/3, post_end_per_testcase/4]).

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
	    DataDir = proplists:get_value(data_dir, Config),
	    PrivDir = proplists:get_value(priv_dir, Config),

	    %% PrivDir as well as directory of Test Server suites
	    %% have to be in code path on Test Server node.
	    [_ | Parts] = lists:reverse(filename:split(DataDir)),
	    TSDir = filename:join(lists:reverse(Parts)),
	    AddPathDirs = case proplists:get_value(path_dirs, Config) of
			      undefined -> [];
			      Ds -> Ds
			  end,
	    PathDirs = [PrivDir,TSDir | AddPathDirs],
	    [true = rpc:call(Node, code, add_patha, [D]) || D <- PathDirs],
	    io:format("Dirs added to code path (on ~w):~n",
		      [Node]),
	    [io:format("~s~n", [D]) || D <- PathDirs],

	    true = rpc:call(Node, os, putenv, 
			    ["TEST_SERVER_FRAMEWORK", "undefined"]),

	    ok = rpc:call(Node, file, set_cwd, [PrivDir]),
	    [{node,Node} | Config]     
    end.

post_end_per_testcase(_TC, Config, Return, State) ->
    Node = proplists:get_value(node, Config),
    cover:stop(Node),
    slave:stop(Node),

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
	    {ok, Tc#tc{ result = skip } }
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
