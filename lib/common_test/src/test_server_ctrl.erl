%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2017. All Rights Reserved.
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
-module(test_server_ctrl).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%                      The Erlang Test Server                      %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% MODULE DEPENDENCIES:
%% HARD TO REMOVE: erlang, lists, io_lib, gen_server, file, io, string,
%%                 code, ets, rpc, gen_tcp, inet, erl_tar, sets,
%%                 test_server, test_server_sup, test_server_node
%% EASIER TO REMOVE: filename, filelib, lib, re
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SUPERVISOR INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, start/1, start_link/1, stop/0]).

%%% OPERATOR INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([add_spec/1, add_dir/2, add_dir/3]).
-export([add_module/1, add_module/2,
	 add_conf/3,
	 add_case/2, add_case/3, add_cases/2, add_cases/3]).
-export([add_dir_with_skip/3, add_dir_with_skip/4, add_tests_with_skip/3]).
-export([add_module_with_skip/2, add_module_with_skip/3,
	 add_conf_with_skip/4,
	 add_case_with_skip/3, add_case_with_skip/4,
	 add_cases_with_skip/3, add_cases_with_skip/4]).
-export([jobs/0, run_test/1, wait_finish/0, idle_notify/1,
	 abort_current_testcase/1, abort/0]).
-export([start_get_totals/1, stop_get_totals/0]).
-export([reject_io_reqs/1, get_levels/0, set_levels/3]).
-export([multiply_timetraps/1, scale_timetraps/1, get_timetrap_parameters/0]).
-export([create_priv_dir/1]).
-export([cover/1, cover/2, cover/3,
	 cover_compile/7, cover_analyse/2, cross_cover_analyse/2,
	 trc/1, stop_trace/0]).
-export([testcase_callback/1]).
-export([set_random_seed/1]).
-export([kill_slavenodes/0]).

%%% TEST_SERVER INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([print/2, print/3, print/4, print_timestamp/2]).
-export([start_node/3, stop_node/1, wait_for_node/1, is_release_available/1]).
-export([format/1, format/2, format/3, to_string/1]).
-export([get_target_info/0]).
-export([get_hosts/0]).
-export([node_started/1]).
-export([uri_encode/1,uri_encode/2]).

%%% DEBUGGER INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([i/0, p/1, p/3, pi/2, pi/4, t/0, t/1]).

%%% PRIVATE EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([do_test_cases/4]).
-export([do_spec/2, do_spec_list/2]).
-export([xhtml/2, escape_chars/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("test_server_internal.hrl").
-include_lib("kernel/include/file.hrl").
-define(suite_ext, "_SUITE").
-define(log_ext, ".log.html").
-define(src_listing_ext,  ".src.html").
-define(logdir_ext, ".logs").
-define(data_dir_suffix, "_data/").
-define(suitelog_name, "suite.log").
-define(suitelog_latest_name, "suite.log.latest").
-define(coverlog_name, "cover.html").
-define(raw_coverlog_name, "cover.log").
-define(cross_coverlog_name, "cross_cover.html").
-define(raw_cross_coverlog_name, "cross_cover.log").
-define(cross_cover_info, "cross_cover.info").
-define(cover_total, "total_cover.log").
-define(unexpected_io_log, "unexpected_io.log.html").
-define(last_file, "last_name").
-define(last_link, "last_link").
-define(last_test, "last_test").
-define(html_ext, ".html").
-define(now, os:timestamp()).

-define(void_fun, fun() -> ok end).
-define(mod_result(X), if X == skip -> skipped;
			  X == auto_skip -> skipped;
			  true -> X end).

-define(auto_skip_color, "#FFA64D").
-define(user_skip_color, "#FF8000").
-define(sortable_table_name, "SortableTable").

-record(state,{jobs=[], levels={1,19,10}, reject_io_reqs=false,
	       multiply_timetraps=1, scale_timetraps=true,
	       create_priv_dir=auto_per_run, finish=false,
	       target_info, trc=false, cover=false, wait_for_node=[],
	       testcase_callback=undefined, idle_notify=[],
	       get_totals=false, random_seed=undefined}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OPERATOR INTERFACE

add_dir(Name, Job=[Dir|_Dirs]) when is_list(Dir) ->
    add_job(cast_to_list(Name),
	    lists:map(fun(D)-> {dir,cast_to_list(D)} end, Job));
add_dir(Name, Dir) ->
    add_job(cast_to_list(Name), {dir,cast_to_list(Dir)}).

add_dir(Name, Job=[Dir|_Dirs], Pattern) when is_list(Dir) ->
    add_job(cast_to_list(Name),
	    lists:map(fun(D)-> {dir,cast_to_list(D),
				cast_to_list(Pattern)} end, Job));
add_dir(Name, Dir, Pattern) ->
    add_job(cast_to_list(Name), {dir,cast_to_list(Dir),cast_to_list(Pattern)}).

add_module(Mod) when is_atom(Mod) ->
    add_job(atom_to_list(Mod), {Mod,all}).

add_module(Name, Mods) when is_list(Mods) ->
    add_job(cast_to_list(Name), lists:map(fun(Mod) -> {Mod,all} end, Mods)).

add_conf(Name, Mod, Conf) when is_tuple(Conf) ->
    add_job(cast_to_list(Name), {Mod,[Conf]});

add_conf(Name, Mod, Confs) when is_list(Confs) ->
    add_job(cast_to_list(Name), {Mod,Confs}).

add_case(Mod, Case) when is_atom(Mod), is_atom(Case) ->
    add_job(atom_to_list(Mod), {Mod,Case}).

add_case(Name, Mod, Case) when is_atom(Mod), is_atom(Case) ->
    add_job(Name, {Mod,Case}).

add_cases(Mod, Cases) when is_atom(Mod), is_list(Cases) ->
    add_job(atom_to_list(Mod), {Mod,Cases}).

add_cases(Name, Mod, Cases) when is_atom(Mod), is_list(Cases) ->
    add_job(Name, {Mod,Cases}).

add_spec(Spec) ->
    Name = filename:rootname(Spec, ".spec"),
    case filelib:is_file(Spec) of
	true -> add_job(Name, {spec,Spec});
	false -> {error,nofile}
    end.

%% This version of the interface is to be used if there are
%% suites or cases that should be skipped.

add_dir_with_skip(Name, Job=[Dir|_Dirs], Skip) when is_list(Dir) ->
    add_job(cast_to_list(Name),
	    lists:map(fun(D)-> {dir,cast_to_list(D)} end, Job),
	    Skip);
add_dir_with_skip(Name, Dir, Skip) ->
    add_job(cast_to_list(Name), {dir,cast_to_list(Dir)}, Skip).

add_dir_with_skip(Name, Job=[Dir|_Dirs], Pattern, Skip) when is_list(Dir) ->
    add_job(cast_to_list(Name),
	    lists:map(fun(D)-> {dir,cast_to_list(D),
				cast_to_list(Pattern)} end, Job),
	    Skip);
add_dir_with_skip(Name, Dir, Pattern, Skip) ->
    add_job(cast_to_list(Name),
	    {dir,cast_to_list(Dir),cast_to_list(Pattern)}, Skip).

add_module_with_skip(Mod, Skip) when is_atom(Mod) ->
    add_job(atom_to_list(Mod), {Mod,all}, Skip).

add_module_with_skip(Name, Mods, Skip) when is_list(Mods) ->
    add_job(cast_to_list(Name), lists:map(fun(Mod) -> {Mod,all} end, Mods), Skip).

add_conf_with_skip(Name, Mod, Conf, Skip) when is_tuple(Conf) ->
    add_job(cast_to_list(Name), {Mod,[Conf]}, Skip);

add_conf_with_skip(Name, Mod, Confs, Skip) when is_list(Confs) ->
    add_job(cast_to_list(Name), {Mod,Confs}, Skip).

add_case_with_skip(Mod, Case, Skip) when is_atom(Mod), is_atom(Case) ->
    add_job(atom_to_list(Mod), {Mod,Case}, Skip).

add_case_with_skip(Name, Mod, Case, Skip) when is_atom(Mod), is_atom(Case) ->
    add_job(Name, {Mod,Case}, Skip).

add_cases_with_skip(Mod, Cases, Skip) when is_atom(Mod), is_list(Cases) ->
    add_job(atom_to_list(Mod), {Mod,Cases}, Skip).

add_cases_with_skip(Name, Mod, Cases, Skip) when is_atom(Mod), is_list(Cases) ->
    add_job(Name, {Mod,Cases}, Skip).

add_tests_with_skip(LogDir, Tests, Skip) ->
    add_job(LogDir,
	    lists:map(fun({Dir,all,all}) ->
			      {Dir,{dir,Dir}};
			 ({Dir,Mods,all}) ->
			      {Dir,lists:map(fun(M) -> {M,all} end, Mods)};
			 ({Dir,Mod,Cases}) ->
			      {Dir,{Mod,Cases}}
		      end, Tests),
	    Skip).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COMMAND LINE INTERFACE

parse_cmd_line(Cmds) ->
    parse_cmd_line(Cmds, [], [], local, false, false, undefined).

parse_cmd_line(['SPEC',Spec|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    case file:consult(Spec) of
	{ok, TermList} ->
	    Name = filename:rootname(Spec),
	    parse_cmd_line(Cmds, TermList++SpecList, [Name|Names], Param,
			   Trc, Cov, TCCB);
	{error,Reason} ->
	    io:format("Can't open ~tw: ~tp\n",[Spec, file:format_error(Reason)]),
	    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, Cov, TCCB)
    end;
parse_cmd_line(['NAME',Name|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, SpecList, [{name,atom_to_list(Name)}|Names],
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['SKIPMOD',Mod|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, [{skip,{Mod,"by command line"}}|SpecList], Names,
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['SKIPCASE',Mod,Case|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, [{skip,{Mod,Case,"by command line"}}|SpecList], Names,
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['DIR',Dir|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    Name = filename:basename(Dir),
    parse_cmd_line(Cmds, [{topcase,{dir,Name}}|SpecList], [Name|Names],
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['MODULE',Mod|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds,[{topcase,{Mod,all}}|SpecList],[atom_to_list(Mod)|Names],
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['CASE',Mod,Case|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds,[{topcase,{Mod,Case}}|SpecList],[atom_to_list(Mod)|Names],
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['TRACE',Trc|Cmds], SpecList, Names, Param, _Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, Cov, TCCB);
parse_cmd_line(['COVER',App,CF,Analyse|Cmds], SpecList, Names, Param, Trc, _Cov, TCCB) ->
    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, {{App,CF}, Analyse}, TCCB);
parse_cmd_line(['TESTCASE_CALLBACK',Mod,Func|Cmds], SpecList, Names, Param, Trc, Cov, _) ->
    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, Cov, {Mod,Func});
parse_cmd_line([Obj|_Cmds], _SpecList, _Names, _Param, _Trc, _Cov, _TCCB) ->
    io:format("~w: Bad argument: ~tw\n", [?MODULE,Obj]),
    io:format(" Use the `ts' module to start tests.\n", []),
    io:format(" (If you ARE using `ts', there is a bug in `ts'.)\n", []),
    halt(1);
parse_cmd_line([], SpecList, Names, Param, Trc, Cov, TCCB) ->
    NameList = lists:reverse(Names, ["suite"]),
    Name = case lists:keysearch(name, 1, NameList) of
	       {value,{name,N}} -> N;
	       false -> hd(NameList)
	   end,
    {lists:reverse(SpecList), Name, Param, Trc, Cov, TCCB}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cast_to_list(X) -> string()
%% X = list() | atom() | void()
%% Returns a string representation of whatever was input

cast_to_list(X) when is_list(X) -> X;
cast_to_list(X) when is_atom(X) -> atom_to_list(X);
cast_to_list(X) -> lists:flatten(io_lib:format("~tw", [X])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% START INTERFACE

%% Kept for backwards compatibility
start(_) ->
    start().
start_link(_) ->
    start_link().


start() ->
    case gen_server:start({local,?MODULE}, ?MODULE, [], []) of
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	Other ->
	    Other
    end.

start_link() ->
    case gen_server:start_link({local,?MODULE}, ?MODULE, [], []) of
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	Other ->
	    Other
    end.

run_test(CommandLine) ->
    process_flag(trap_exit,true),
    {SpecList,Name,Param,Trc,Cov,TCCB} = parse_cmd_line(CommandLine),
    {ok,_TSPid} = start_link(Param),
    case Trc of
	false -> ok;
	File -> trc(File)
    end,
    case Cov of
	false -> ok;
	{{App,CoverFile},Analyse} -> cover(App, maybe_file(CoverFile), Analyse)
    end,
    testcase_callback(TCCB),
    add_job(Name, {command_line,SpecList}),

    wait_finish().

%% Converted CoverFile to a string unless it is 'none'
maybe_file(none) ->
    none;
maybe_file(CoverFile) ->
    atom_to_list(CoverFile).

idle_notify(Fun) ->
    {ok, Pid} = controller_call({idle_notify,Fun}),
    Pid.

start_get_totals(Fun) ->
    {ok, Pid} = controller_call({start_get_totals,Fun}),
    Pid.

stop_get_totals() ->
    ok = controller_call(stop_get_totals),
    ok.

wait_finish() ->
    OldTrap = process_flag(trap_exit, true),
    {ok, Pid} = finish(true),
    link(Pid),
    receive
	{'EXIT',Pid,_} ->
	    ok
    end,
    process_flag(trap_exit, OldTrap),
    ok.

abort_current_testcase(Reason) ->
    controller_call({abort_current_testcase,Reason}).

abort() ->
    OldTrap = process_flag(trap_exit, true),
    {ok, Pid} = finish(abort),
    link(Pid),
    receive
	{'EXIT',Pid,_} ->
	    ok
    end,
    process_flag(trap_exit, OldTrap),
    ok.

finish(Abort) ->
    controller_call({finish,Abort}).

stop() ->
    controller_call(stop).

jobs() ->
    controller_call(jobs).

get_levels() ->
    controller_call(get_levels).

set_levels(Show, Major, Minor) ->
    controller_call({set_levels,Show,Major,Minor}).

reject_io_reqs(Bool) ->
    controller_call({reject_io_reqs,Bool}).

multiply_timetraps(N) ->
    controller_call({multiply_timetraps,N}).

scale_timetraps(Bool) ->
    controller_call({scale_timetraps,Bool}).

get_timetrap_parameters() ->
    controller_call(get_timetrap_parameters).

create_priv_dir(Value) ->
    controller_call({create_priv_dir,Value}).

trc(TraceFile) ->
    controller_call({trace,TraceFile}, 2*?ACCEPT_TIMEOUT).

stop_trace() ->
    controller_call(stop_trace).

node_started(Node) ->
    gen_server:cast(?MODULE, {node_started,Node}).

cover(App, Analyse) when is_atom(App) ->
    cover(App, none, Analyse);
cover(CoverFile, Analyse) ->
    cover(none, CoverFile, Analyse).
cover(App, CoverFile, Analyse) ->
    {Excl,Incl,Cross} = read_cover_file(CoverFile),
    CoverInfo = #cover{app=App,
		       file=CoverFile,
		       excl=Excl,
		       incl=Incl,
		       cross=Cross,
		       level=Analyse},
    controller_call({cover,CoverInfo}).

cover(CoverInfo) ->
    controller_call({cover,CoverInfo}).

cover_compile(App,File,Excl,Incl,Cross,Analyse,Stop) ->
    cover_compile(#cover{app=App,
			 file=File,
			 excl=Excl,
			 incl=Incl,
			 cross=Cross,
			 level=Analyse,
			 stop=Stop}).

testcase_callback(ModFunc) ->
    controller_call({testcase_callback,ModFunc}).

set_random_seed(Seed) ->
    controller_call({set_random_seed,Seed}).

kill_slavenodes() ->
    controller_call(kill_slavenodes).

get_hosts() ->
    get(test_server_hosts).

%%--------------------------------------------------------------------

add_job(Name, TopCase) ->
    add_job(Name, TopCase, []).

add_job(Name, TopCase, Skip) ->
    SuiteName =
	case Name of
	    "." -> "current_dir";
	    ".." -> "parent_dir";
	    Other -> Other
	end,
    Dir = filename:absname(SuiteName),
    controller_call({add_job,Dir,SuiteName,TopCase,Skip}).

controller_call(Arg) ->
    case catch gen_server:call(?MODULE, Arg, infinity) of
	{'EXIT',{{badarg,_},{gen_server,call,_}}} ->
	    exit(test_server_ctrl_not_running);
	{'EXIT',Reason} ->
	    exit(Reason);
	Other ->
	    Other
    end.
controller_call(Arg, Timeout) ->
    case catch gen_server:call(?MODULE, Arg, Timeout) of
	{'EXIT',{{badarg,_},{gen_server,call,_}}} ->
	    exit(test_server_ctrl_not_running);
	{'EXIT',Reason} ->
	    exit(Reason);
	Other ->
	    Other
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init([])
%%
%% init() is the init function of the test_server's gen_server.
%%
init([]) ->
    case os:getenv("TEST_SERVER_CALL_TRACE") of
	false ->
	    ok;
	"" ->
	    ok;
	TraceSpec ->
	    test_server_sup:call_trace(TraceSpec)
    end,
    process_flag(trap_exit, true),
    %% copy format_exception setting from init arg to application environment
    case init:get_argument(test_server_format_exception) of
	{ok,[[TSFE]]} ->
	    application:set_env(test_server, format_exception, list_to_atom(TSFE));
	_ ->
	    ok
    end,
    test_server_sup:cleanup_crash_dumps(),
    test_server_sup:util_start(),
    State = #state{jobs=[],finish=false},
    TI0 = test_server:init_target_info(),
    TargetHost = test_server_sup:hoststr(),
    TI = TI0#target_info{host=TargetHost,
			 naming=naming(),
			 master=TargetHost},
    _ = ets:new(slave_tab, [named_table,set,public,{keypos,2}]),
    set_hosts([TI#target_info.host]),
    {ok,State#state{target_info=TI}}.

naming() ->
    case lists:member($., test_server_sup:hoststr()) of
	true -> "-name";
	false -> "-sname"
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(kill_slavenodes, From, State) -> ok
%%
%% Kill all slave nodes that remain after a test case
%% is completed.
%%
handle_call(kill_slavenodes, _From, State) ->
    Nodes = test_server_node:kill_nodes(),
    {reply, Nodes, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({set_hosts, HostList}, From, State) -> ok
%%
%% Set the global hostlist.
%%
handle_call({set_hosts, Hosts}, _From, State) ->
    set_hosts(Hosts),
    {reply, ok, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(get_hosts, From, State) -> [Hosts]
%%
%% Returns the lists of hosts that the test server
%% can use for slave nodes. This is primarily used
%% for nodename generation.
%%
handle_call(get_hosts, _From, State) ->
    Hosts = get_hosts(),
    {reply, Hosts, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({add_job,Dir,Name,TopCase,Skip}, _, State) ->
%%     ok | {error,Reason}
%%
%% Dir = string()
%% Name = string()
%% TopCase = term()
%% Skip = [SkipItem]
%% SkipItem = {Mod,Comment} | {Mod,Case,Comment} | {Mod,Cases,Comment}
%% Mod = Case = atom()
%% Comment = string()
%% Cases = [Case]
%%
%% Adds a job to the job queue. The name of the job is Name. A log directory
%% will be created in Dir/Name.logs. TopCase may be anything that
%% collect_cases/3 accepts, plus the following:
%%
%% {spec,SpecName} executes the named test suite specification file. Commands
%% in the file should be in the format accepted by do_spec_list/1.
%%
%% {command_line,SpecList} executes the list of specification instructions
%% supplied, which should be in the format accepted by do_spec_list/1.

handle_call({add_job,Dir,Name,TopCase,Skip}, _From, State) ->
    LogDir = Dir ++ ?logdir_ext,
    ExtraTools =
	case State#state.cover of
	    false -> [];
	    CoverInfo -> [{cover,CoverInfo}]
	end,
    ExtraTools1 =
	case State#state.random_seed of
	    undefined -> ExtraTools;
	    Seed -> [{random_seed,Seed}|ExtraTools]
	end,
    case lists:keysearch(Name, 1, State#state.jobs) of
	false ->
	    case TopCase of
		{spec,SpecName} ->
		    Pid = spawn_tester(
			    ?MODULE, do_spec,
			    [SpecName,{State#state.multiply_timetraps,
				       State#state.scale_timetraps}],
			    LogDir, Name, State#state.levels,
			    State#state.reject_io_reqs,
			    State#state.create_priv_dir,
			    State#state.testcase_callback, ExtraTools1),
		    NewJobs = [{Name,Pid}|State#state.jobs],
		    {reply, ok, State#state{jobs=NewJobs}};
		{command_line,SpecList} ->
		    Pid = spawn_tester(
			    ?MODULE, do_spec_list,
			    [SpecList,{State#state.multiply_timetraps,
				       State#state.scale_timetraps}],
			    LogDir, Name, State#state.levels,
			    State#state.reject_io_reqs,
			    State#state.create_priv_dir,
			    State#state.testcase_callback, ExtraTools1),
		    NewJobs = [{Name,Pid}|State#state.jobs],
		    {reply, ok, State#state{jobs=NewJobs}};
		TopCase ->
		    case State#state.get_totals of
			{CliPid,Fun} ->
			    Result = count_test_cases(TopCase, Skip),
			    Fun(CliPid, Result),
			    {reply, ok, State};
			_ ->
			    Cfg = make_config([]),
			    Pid = spawn_tester(
				    ?MODULE, do_test_cases,
				    [TopCase,Skip,Cfg,
				     {State#state.multiply_timetraps,
				      State#state.scale_timetraps}],
				    LogDir, Name, State#state.levels,
				    State#state.reject_io_reqs,
				    State#state.create_priv_dir,
				    State#state.testcase_callback, ExtraTools1),
			    NewJobs = [{Name,Pid}|State#state.jobs],
			    {reply, ok, State#state{jobs=NewJobs}}
		    end
	    end;
	_ ->
	    {reply,{error,name_already_in_use},State}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(jobs, _, State) -> JobList
%% JobList = [{Name,Pid}, ...]
%% Name = string()
%% Pid = pid()
%%
%% Return the list of current jobs.

handle_call(jobs, _From, State) ->
    {reply,State#state.jobs,State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({abort_current_testcase,Reason}, _, State) -> Result
%% Reason = term()
%% Result = ok | {error,no_testcase_running}
%%
%% Attempts to abort the test case that's currently running.

handle_call({abort_current_testcase,Reason}, _From, State) ->
    case State#state.jobs of
	[{_,Pid}|_] ->
	    Pid ! {abort_current_testcase,Reason,self()},
	    receive
		{Pid,abort_current_testcase,Result} ->
		    {reply, Result, State}
	    after 10000 ->
		    {reply, {error,no_testcase_running}, State}
	    end;
	_ ->
	    {reply, {error,no_testcase_running}, State}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({finish,Fini}, _, State) -> {ok,Pid}
%% Fini = true | abort
%%
%% Tells the test_server to stop as soon as there are no test suites
%% running. Immediately if none are running. Abort is handled as soon
%% as current test finishes.

handle_call({finish,Fini}, _From, State) ->
    case State#state.jobs of
	[] ->
	    lists:foreach(fun({Cli,Fun}) -> Fun(Cli,Fini) end,
			  State#state.idle_notify),
	    State2 = State#state{finish=false},
	    {stop,shutdown,{ok,self()}, State2};
	_SomeJobs ->
	    State2 = State#state{finish=Fini},
	    {reply, {ok,self()}, State2}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({idle_notify,Fun}, From, State) -> {ok,Pid}
%%
%% Lets a test client subscribe to receive a notification when the
%% test server becomes idle (can be used to syncronize jobs).
%% test_server calls Fun(From) when idle.

handle_call({idle_notify,Fun}, {Cli,_Ref}, State) ->
    case State#state.jobs of
	[] -> self() ! report_idle;
	_  -> ok
    end,
    Subscribed = State#state.idle_notify,
    {reply, {ok,self()}, State#state{idle_notify=[{Cli,Fun}|Subscribed]}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(start_get_totals, From, State) -> {ok,Pid}
%%
%% Switch on the mode where the test server will only
%% report back the number of tests it would execute
%% given some subsequent jobs.

handle_call({start_get_totals,Fun}, {Cli,_Ref}, State) ->
    {reply, {ok,self()}, State#state{get_totals={Cli,Fun}}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(stop_get_totals, From, State) -> ok
%%
%% Lets a test client subscribe to receive a notification when the
%% test server becomes idle (can be used to syncronize jobs).
%% test_server calls Fun(From) when idle.

handle_call(stop_get_totals, {_Cli,_Ref}, State) ->
    {reply, ok, State#state{get_totals=false}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(get_levels, _, State) -> {Show,Major,Minor}
%% Show = integer()
%% Major = integer()
%% Minor = integer()
%%
%% Returns a 3-tuple with the logging thresholds.
%% All output and information from a test suite is tagged with a detail
%% level. Lower values are more "important". Text that is output using
%% io:format or similar is automatically tagged with detail level 50.
%%
%% All output with detail level:
%% less or equal to Show is displayed on the screen (default 1)
%% less or equal to Major is logged in the major log file (default 19)
%% greater or equal to Minor is logged in the minor log files (default 10)

handle_call(get_levels, _From, State) ->
    {reply,State#state.levels,State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({set_levels,Show,Major,Minor}, _, State) -> ok
%% Show = integer()
%% Major = integer()
%% Minor = integer()
%%
%% Sets the logging thresholds, see handle_call(get_levels,...) above.

handle_call({set_levels,Show,Major,Minor}, _From, State) ->
    {reply,ok,State#state{levels={Show,Major,Minor}}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({reject_io_reqs,Bool}, _, State) -> ok
%% Bool = bool()
%%
%% May be used to switch off stdout printouts to the minor log file

handle_call({reject_io_reqs,Bool}, _From, State) ->
    {reply,ok,State#state{reject_io_reqs=Bool}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({multiply_timetraps,N}, _, State) -> ok
%% N = integer() | infinity
%%
%% Multiplies all timetraps set by test cases with N

handle_call({multiply_timetraps,N}, _From, State) ->
    {reply,ok,State#state{multiply_timetraps=N}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({scale_timetraps,Bool}, _, State) -> ok
%% Bool = true | false
%%
%% Specifies if test_server should scale the timetrap value
%% automatically if e.g. cover is running.

handle_call({scale_timetraps,Bool}, _From, State) ->
    {reply,ok,State#state{scale_timetraps=Bool}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(get_timetrap_parameters, _, State) -> {Multiplier,Scale}
%% Multiplier = integer() | infinity
%% Scale = true | false
%%
%% Returns the parameter values that affect timetraps.

handle_call(get_timetrap_parameters, _From, State) ->
    {reply,{State#state.multiply_timetraps,State#state.scale_timetraps},State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({trace,TraceFile}, _, State) -> ok | {error,Reason}
%%
%% Starts a separate node (trace control node) which
%% starts tracing on target and all slave nodes
%%
%% TraceFile is a text file with elements of type
%% {Trace,Mod,TracePattern}.
%% {Trace,Mod,Func,TracePattern}.
%% {Trace,Mod,Func,Arity,TracePattern}.
%%
%% Trace = tp | tpl;  local or global call trace
%% Mod,Func = atom(), Arity=integer(); defines what to trace
%% TracePattern = [] | match_spec()
%%
%% The 'call' trace flag is set on all processes, and then
%% the given trace patterns are set.

handle_call({trace,TraceFile}, _From, State=#state{trc=false}) ->
    TI = State#state.target_info,
    case test_server_node:start_tracer_node(TraceFile, TI) of
	{ok,Tracer} -> {reply,ok,State#state{trc=Tracer}};
	Error -> {reply,Error,State}
    end;
handle_call({trace,_TraceFile}, _From, State) ->
    {reply,{error,already_tracing},State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(stop_trace, _, State) -> ok | {error,Reason}
%%
%% Stops tracing on target and all slave nodes and
%% terminates trace control node

handle_call(stop_trace, _From, State=#state{trc=false}) ->
    {reply,{error,not_tracing},State};
handle_call(stop_trace, _From, State) ->
    R = test_server_node:stop_tracer_node(State#state.trc),
    {reply,R,State#state{trc=false}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({cover,CoverInfo}, _, State) -> ok | {error,Reason}
%%
%% Set specification of cover analysis to be used when running tests
%% (see start_extra_tools/1 and stop_extra_tools/1)

handle_call({cover,CoverInfo}, _From, State) ->
    {reply,ok,State#state{cover=CoverInfo}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({create_priv_dir,Value}, _, State) -> ok | {error,Reason}
%%
%% Set create_priv_dir to either auto_per_run (create common priv dir once
%% per test run), manual_per_tc (the priv dir name will be unique for each
%% test case, but the user has to call test_server:make_priv_dir/0 to create
%% it), or auto_per_tc (unique priv dir created automatically for each test
%% case).

handle_call({create_priv_dir,Value}, _From, State) ->
    {reply,ok,State#state{create_priv_dir=Value}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({testcase_callback,{Mod,Func}}, _, State) -> ok | {error,Reason}
%%
%% Add a callback function that will be called before and after every
%% test case (on the test case process):
%%
%% Mod:Func(Suite,TestCase,InitOrEnd,Config)
%%
%% InitOrEnd = init | 'end'.

handle_call({testcase_callback,ModFunc}, _From, State) ->
    case ModFunc of
	{Mod,Func} ->
	    _ = case code:is_loaded(Mod) of
		{file,_} ->
		    ok;
		false ->
		    code:load_file(Mod)
	    end,
	    case erlang:function_exported(Mod,Func,4) of
		true ->
		    ok;
		false ->
		    io:format(user,
			      "WARNING! Callback function ~w:~tw/4 undefined.~n~n",
			      [Mod,Func])
	    end;
	_ ->
	    ok
    end,
    {reply,ok,State#state{testcase_callback=ModFunc}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({set_random_seed,Seed}, _, State) -> ok | {error,Reason}
%%
%% Let operator set a random seed value to be used e.g. for shuffling
%% test cases.

handle_call({set_random_seed,Seed}, _From, State) ->
    {reply,ok,State#state{random_seed=Seed}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(stop, _, State) -> ok
%%
%% Stops the test server immediately.
%% Some cleanup is done by terminate/2

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call(get_target_info, _, State) -> TI
%%
%% TI = #target_info{}
%%
%% Returns information about target

handle_call(get_target_info, _From, State) ->
    {reply, State#state.target_info, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({start_node,Name,Type,Options}, _, State) ->
%%     ok | {error,Reason}
%%
%% Starts a new node (slave or peer)

handle_call({start_node, Name, Type, Options}, From, State) ->
    %% test_server_ctrl does gen_server:reply/2 explicitly
    test_server_node:start_node(Name, Type, Options, From,
				State#state.target_info),
    {noreply,State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({wait_for_node,Node}, _, State) -> ok
%%
%% Waits for a new node to take contact. Used if
%% node is started with option {wait,false}

handle_call({wait_for_node, Node}, From, State) ->
    NewWaitList =
	case ets:lookup(slave_tab,Node) of
	    [] ->
		[{Node,From}|State#state.wait_for_node];
	    _ ->
		gen_server:reply(From,ok),
		State#state.wait_for_node
	end,
    {noreply,State#state{wait_for_node=NewWaitList}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({stop_node,Name}, _, State) -> ok | {error,Reason}
%%
%% Stops a slave or peer node. This is actually only some cleanup
%% - the node is really stopped by test_server when this returns.

handle_call({stop_node, Name}, _From, State) ->
    R = test_server_node:stop_node(Name),
    {reply, R, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({is_release_available,Name}, _, State) -> ok | {error,Reason}
%%
%% Tests if the release is available.

handle_call({is_release_available, Release}, _From, State) ->
    R = test_server_node:is_release_available(Release),
    {reply, R, State}.

%%--------------------------------------------------------------------
set_hosts(Hosts) ->
    put(test_server_hosts, Hosts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast({node_started,Name}, _, State)
%%
%% Called by test_server_node when a slave/peer node is fully started.

handle_cast({node_started,Node}, State) ->
    case State#state.trc of
	false -> ok;
	Trc -> test_server_node:trace_nodes(Trc, [Node])
    end,
    NewWaitList =
	case lists:keysearch(Node,1,State#state.wait_for_node) of
	    {value,{Node,From}} ->
		gen_server:reply(From, ok),
		lists:keydelete(Node, 1, State#state.wait_for_node);
	    false ->
		State#state.wait_for_node
	end,
    {noreply, State#state{wait_for_node=NewWaitList}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info({'EXIT',Pid,Reason}, State)
%% Pid = pid()
%% Reason = term()
%%
%% Handles exit messages from linked processes. Only test suites are
%% expected to be linked.  When a test suite terminates, it is removed
%% from the job queue.

handle_info(report_idle, State) ->
    Finish = State#state.finish,
    lists:foreach(fun({Cli,Fun}) -> Fun(Cli,Finish) end,
		  State#state.idle_notify),
    {noreply,State#state{idle_notify=[]}};


handle_info({'EXIT',Pid,Reason}, State) ->
    case lists:keysearch(Pid,2,State#state.jobs) of
	false ->
	    %% not our problem
	    {noreply,State};
	{value,{Name,_}} ->
	    NewJobs = lists:keydelete(Pid, 2, State#state.jobs),
	    case Reason of
		normal ->
		    fine;
		killed ->
		    io:format("Suite ~ts was killed\n", [Name]);
		_Other ->
		    io:format("Suite ~ts was killed with reason ~tp\n",
			      [Name,Reason])
	    end,
	    State2 = State#state{jobs=NewJobs},
	    Finish = State2#state.finish,
	    case NewJobs of
		[] ->
		    lists:foreach(fun({Cli,Fun}) -> Fun(Cli,Finish) end,
				  State2#state.idle_notify),
		    case Finish of
			false ->
			    {noreply,State2#state{idle_notify=[]}};
			_ ->			% true | abort
			    %% test_server:finish() has been called and
			    %% there are no jobs in the job queue =>
			    %% stop the test_server_ctrl
			    {stop,shutdown,State2#state{finish=false}}
		    end;
		_ ->				% pending jobs
		    case Finish of
			abort ->		% abort test now!
			    lists:foreach(fun({Cli,Fun}) -> Fun(Cli,Finish) end,
					  State2#state.idle_notify),
			    {stop,shutdown,State2#state{finish=false}};
			_ ->			% true | false
			    {noreply, State2}
		    end
	    end
    end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info({tcp_closed,Sock}, State)
%%
%% A Socket was closed. This indicates that a node died.
%% This can be
%% *Slave or peer node started by a test suite
%% *Trace controll node

handle_info({tcp_closed,Sock}, State=#state{trc=Sock}) ->
    %% Tracer node died - can't really do anything
    %%! Maybe print something???
    {noreply,State#state{trc=false}};
handle_info({tcp_closed,Sock}, State) ->
    test_server_node:nodedown(Sock),
    {noreply,State};
handle_info(_, State) ->
    %% dummy; accept all, do nothing.
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate(Reason, State) -> ok
%% Reason = term()
%%
%% Cleans up when the test_server is terminating. Kills the running
%% test suites (if any) and any possible remainting slave node

terminate(_Reason, State) ->
    test_server_sup:util_stop(),
    case State#state.trc of
	false -> ok;
	Sock -> test_server_node:stop_tracer_node(Sock)
    end,
    ok = kill_all_jobs(State#state.jobs),
    _ = test_server_node:kill_nodes(),
    ok.

kill_all_jobs([{_Name,JobPid}|Jobs]) ->
    exit(JobPid, kill),
    kill_all_jobs(Jobs);
kill_all_jobs([]) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%----------------------- INTERNAL FUNCTIONS -----------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% spawn_tester(Mod, Func, Args, Dir, Name, Levels, RejectIoReqs,
%%              CreatePrivDir, TestCaseCallback, ExtraTools) -> Pid
%% Mod = atom()
%% Func = atom()
%% Args = [term(),...]
%% Dir = string()
%% Name = string()
%% Levels = {integer(),integer(),integer()}
%% RejectIoReqs = bool()
%% CreatePrivDir = auto_per_run | manual_per_tc | auto_per_tc
%% TestCaseCallback = {CBMod,CBFunc} | undefined
%% ExtraTools = [ExtraTool,...]
%% ExtraTool = CoverInfo | TraceInfo | RandomSeed
%%
%% Spawns a test suite execute-process, just an ordinary spawn, except
%% that it will set a lot of dictionary information before starting the
%% named function. Also, the execution is timed and protected by a catch.
%% When the named function is done executing, a summary of the results
%% is printed to the log files.

spawn_tester(Mod, Func, Args, Dir, Name, Levels, RejectIoReqs,
	     CreatePrivDir, TCCallback, ExtraTools) ->
    spawn_link(fun() ->
	      init_tester(Mod, Func, Args, Dir, Name, Levels, RejectIoReqs,
			   CreatePrivDir, TCCallback, ExtraTools)
      end).

init_tester(Mod, Func, Args, Dir, Name, {_,_,MinLev}=Levels,
	    RejectIoReqs, CreatePrivDir, TCCallback, ExtraTools) ->
    process_flag(trap_exit, true),
    _ = test_server_io:start_link(),
    put(app, common_test),
    put(test_server_name, Name),
    put(test_server_dir, Dir),
    put(test_server_total_time, 0),
    put(test_server_ok, 0),
    put(test_server_failed, 0),
    put(test_server_skipped, {0,0}),
    put(test_server_minor_level, MinLev),
    put(test_server_create_priv_dir, CreatePrivDir),
    put(test_server_random_seed, proplists:get_value(random_seed, ExtraTools)),
    put(test_server_testcase_callback, TCCallback),
    case os:getenv("TEST_SERVER_FRAMEWORK") of
	FW when FW =:= false; FW =:= "undefined" ->
	    put(test_server_framework, '$none');	
	FW ->
	    put(test_server_framework_name, list_to_atom(FW)),
	    case os:getenv("TEST_SERVER_FRAMEWORK_NAME") of
		FWName when FWName =:= false; FWName =:= "undefined" ->
		    put(test_server_framework_name, '$none');	
		FWName ->
		    put(test_server_framework_name, list_to_atom(FWName))
	    end
    end,

    %% before first print, read and set logging options
    FWLogDir =
        case test_server_sup:framework_call(get_log_dir, [], []) of
            {ok,FwDir} -> FwDir;
            _          -> filename:dirname(Dir)
        end,
    put(test_server_framework_logdir, FWLogDir),
    LogOpts = test_server_sup:framework_call(get_logopts, [], []),
    put(test_server_logopts, LogOpts),

    StartedExtraTools = start_extra_tools(ExtraTools),

    test_server_io:set_job_name(Name),
    test_server_io:set_gl_props([{levels,Levels},
				 {auto_nl,not lists:member(no_nl, LogOpts)},
				 {reject_io_reqs,RejectIoReqs}]),
    group_leader(test_server_io:get_gl(true), self()),
    {TimeMy,Result} = ts_tc(Mod, Func, Args),
    set_io_buffering(undefined),
    test_server_io:set_job_name(undefined),
    catch stop_extra_tools(StartedExtraTools),
    case Result of
	{'EXIT',test_suites_done} ->
	    ok;
	{'EXIT',_Pid,Reason} ->
	    print(1, "EXIT, reason ~tp", [Reason]);
	{'EXIT',Reason} ->
	    report_severe_error(Reason),
	    print(1, "EXIT, reason ~tp", [Reason])
    end,
    Time = TimeMy/1000000,
    SuccessStr =
	case get(test_server_failed) of
	    0 -> "Ok";
	    _ -> "FAILED"
	end,
    {SkippedN,SkipStr} =
	case get(test_server_skipped) of
	    {0,0} -> 
		{0,""};
	    {USkipped,ASkipped} ->
		Skipped = USkipped+ASkipped,
		{Skipped,io_lib:format(", ~w Skipped", [Skipped])}
	end,
    OkN = get(test_server_ok),
    FailedN = get(test_server_failed),
    print(html,"\n</tbody>\n<tfoot>\n"
	  "<tr><td></td><td><b>TOTAL</b></td><td></td><td></td><td></td>"
	  "<td>~.3fs</td><td><b>~ts</b></td><td>~w Ok, ~w Failed~ts of ~w</td></tr>\n"
	  "</tfoot>\n",
	  [Time,SuccessStr,OkN,FailedN,SkipStr,OkN+FailedN+SkippedN]),

    test_server_io:stop([major,html,unexpected_io]),
    {UnexpectedIoName,UnexpectedIoFooter} = get(test_server_unexpected_footer),
    {ok,UnexpectedIoFd} = open_html_file(UnexpectedIoName, [append]),
    io:put_chars(UnexpectedIoFd, "\n</pre>\n"++UnexpectedIoFooter),
    ok = file:close(UnexpectedIoFd).

report_severe_error(Reason) ->
    test_server_sup:framework_call(report, [severe_error,Reason]).

ts_tc(M,F,A) ->
    Before = erlang:monotonic_time(),
    Result = (catch apply(M, F, A)),
    After   = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(After-Before,
				       native,
				       micro_seconds),
    {Elapsed, Result}.

start_extra_tools(ExtraTools) ->
    start_extra_tools(ExtraTools, []).
start_extra_tools([{cover,CoverInfo} | ExtraTools], Started) ->
    case start_cover(CoverInfo) of
	{ok,NewCoverInfo} ->
	    start_extra_tools(ExtraTools,[{cover,NewCoverInfo}|Started]);
	{error,_} ->
	    start_extra_tools(ExtraTools, Started)
    end;
start_extra_tools([_ | ExtraTools], Started) ->
    start_extra_tools(ExtraTools, Started);
start_extra_tools([], Started) ->
    Started.

stop_extra_tools(ExtraTools) ->
    TestDir = get(test_server_log_dir_base),
    case lists:keymember(cover, 1, ExtraTools) of
	false ->
	    write_default_coverlog(TestDir);
	true ->
	    ok
    end,
    stop_extra_tools(ExtraTools, TestDir).

stop_extra_tools([{cover,CoverInfo}|ExtraTools], TestDir) ->
    stop_cover(CoverInfo,TestDir),
    stop_extra_tools(ExtraTools, TestDir);
%%stop_extra_tools([_ | ExtraTools], TestDir) ->
%%    stop_extra_tools(ExtraTools, TestDir);
stop_extra_tools([], _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% do_spec(SpecName, TimetrapSpec) -> {error,Reason} | exit(Result)
%% SpecName = string()
%% TimetrapSpec = MultiplyTimetrap | {MultiplyTimetrap,ScaleTimetrap}
%% MultiplyTimetrap = integer() | infinity
%% ScaleTimetrap = bool()
%%
%% Reads the named test suite specification file, and executes it.
%%
%% This function is meant to be called by a process created by
%% spawn_tester/10, which sets up some necessary dictionary values.

do_spec(SpecName, TimetrapSpec) when is_list(SpecName) ->
    case file:consult(SpecName) of
	{ok,TermList} ->
	    do_spec_list(TermList,TimetrapSpec);
	{error,Reason} ->
	    io:format("Can't open ~ts: ~tp\n", [SpecName,Reason]),
	    {error,{cant_open_spec,Reason}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% do_spec_list(TermList, TimetrapSpec) -> exit(Result)
%% TermList = [term()|...]
%% TimetrapSpec = MultiplyTimetrap | {MultiplyTimetrap,ScaleTimetrap}
%% MultiplyTimetrap = integer() | infinity
%% ScaleTimetrap = bool()
%%
%% Executes a list of test suite specification commands. The following
%% commands are available, and may occur zero or more times (if several,
%% the contents is appended):
%%
%% {topcase,TopCase} Specifies top level test goals. TopCase has the syntax
%% specified by collect_cases/3.
%%
%% {skip,Skip} Specifies test cases to skip, and lists requirements that
%% cannot be granted during the test run. Skip has the syntax specified
%% by collect_cases/3.
%%
%% {nodes,Nodes} Lists node names avaliable to the test suites. Nodes have
%% the syntax specified by collect_cases/3.
%%
%% {require_nodenames, Num} Specifies how many nodenames the test suite will
%% need. Theese are automaticly generated and inserted into the Config by the
%% test_server. The caller may specify other hosts to run theese nodes by
%% using the {hosts, Hosts} option. If there are no hosts specified, all
%% nodenames will be generated from the local host.
%%
%% {hosts, Hosts} Specifies a list of available hosts on which to start
%% slave nodes. It is used when the {remote, true} option is given to the
%% test_server:start_node/3 function. Also, if {require_nodenames, Num} is
%% contained in the TermList, the generated nodenames will be spread over
%% all hosts given in this Hosts list. The hostnames are given as atoms or
%% strings.
%%
%% {diskless, true}</c></tag> is kept for backwards compatiblilty and
%% should not be used. Use a configuration test case instead.
%%
%% This function is meant to be called by a process created by
%% spawn_tester/10, which sets up some necessary dictionary values.

do_spec_list(TermList0, TimetrapSpec) ->
    Nodes = [],
    TermList =
	case lists:keysearch(hosts, 1, TermList0) of
	    {value, {hosts, Hosts0}} ->
		Hosts = lists:map(fun(H) -> cast_to_list(H) end, Hosts0),
		controller_call({set_hosts, Hosts}),
		lists:keydelete(hosts, 1, TermList0);
	    _ ->
		TermList0
	end,
    DefaultConfig = make_config([{nodes,Nodes}]),
    {TopCases,SkipList,Config} = do_spec_terms(TermList, [], [], DefaultConfig),
    do_test_cases(TopCases, SkipList, Config, TimetrapSpec).

do_spec_terms([], TopCases, SkipList, Config) ->
    {TopCases,SkipList,Config};
do_spec_terms([{topcase,TopCase}|Terms], TopCases, SkipList, Config) ->
    do_spec_terms(Terms,[TopCase|TopCases], SkipList, Config);
do_spec_terms([{skip,Skip}|Terms], TopCases, SkipList, Config) ->
    do_spec_terms(Terms, TopCases, [Skip|SkipList], Config);
do_spec_terms([{nodes,Nodes}|Terms], TopCases, SkipList, Config) ->
    do_spec_terms(Terms, TopCases, SkipList,
		  update_config(Config, {nodes,Nodes}));
do_spec_terms([{diskless,How}|Terms], TopCases, SkipList, Config) ->
    do_spec_terms(Terms, TopCases, SkipList,
		  update_config(Config, {diskless,How}));
do_spec_terms([{config,MoreConfig}|Terms], TopCases, SkipList, Config) ->
    do_spec_terms(Terms, TopCases, SkipList, Config++MoreConfig);
do_spec_terms([{default_timeout,Tmo}|Terms], TopCases, SkipList, Config) ->
    do_spec_terms(Terms, TopCases, SkipList,
		  update_config(Config, {default_timeout,Tmo}));

do_spec_terms([{require_nodenames,NumNames}|Terms], TopCases, SkipList, Config) ->
    NodeNames0=generate_nodenames(NumNames),
    NodeNames=lists:delete([], NodeNames0),
    do_spec_terms(Terms, TopCases, SkipList,
		  update_config(Config, {nodenames,NodeNames}));
do_spec_terms([Other|Terms], TopCases, SkipList, Config) ->
    io:format("** WARNING: Spec file contains unknown directive ~tp\n",
	      [Other]),
    do_spec_terms(Terms, TopCases, SkipList, Config).



generate_nodenames(Num) ->
    Hosts = case controller_call(get_hosts) of
		[] ->
		    TI = controller_call(get_target_info),
		    [TI#target_info.host];
		List ->
		    List
	    end,
    generate_nodenames2(Num, Hosts, []).

generate_nodenames2(0, _Hosts, Acc) ->
    Acc;
generate_nodenames2(N, Hosts, Acc) ->
    Host=lists:nth((N rem (length(Hosts)))+1, Hosts),
    Name=list_to_atom(temp_nodename("nod", []) ++ "@" ++ Host),
    generate_nodenames2(N-1, Hosts, [Name|Acc]).

temp_nodename([], Acc) ->
    lists:flatten(Acc);
temp_nodename([Chr|Base], Acc) ->
    {A,B,C} = ?now,
    New = [Chr | integer_to_list(Chr bxor A bxor B+A bxor C+B)],
    temp_nodename(Base, [New|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% count_test_cases(TopCases, SkipCases) -> {Suites,NoOfCases} | error
%% TopCases = term()      (See collect_cases/3)
%% SkipCases = term()     (See collect_cases/3)
%% Suites = list()
%% NoOfCases = integer() | unknown
%%
%% Counts the test cases that are about to run and returns that number.
%% If there's a conf group in TestSpec with a repeat property, the total number
%% of cases can not be calculated and NoOfCases = unknown.
count_test_cases(TopCases, SkipCases) when is_list(TopCases) ->
    case collect_all_cases(TopCases, SkipCases) of
	{error,_Why} = Error ->
	    Error;
	TestSpec ->
	    {get_suites(TestSpec, []),
	     case remove_conf(TestSpec) of
		 {repeats,_} ->
		     unknown;
		 TestSpec1 ->
		     length(TestSpec1)
	     end}
    end;

count_test_cases(TopCase, SkipCases) ->
    count_test_cases([TopCase], SkipCases).


remove_conf(Cases) ->
    remove_conf(Cases, [], false).

remove_conf([{conf, _Ref, Props, _MF}|Cases], NoConf, Repeats) ->
    case get_repeat(Props) of
	undefined ->
	    remove_conf(Cases, NoConf, Repeats);
	{_RepType,1} ->
	    remove_conf(Cases, NoConf, Repeats);
	_ ->
	    remove_conf(Cases, NoConf, true)
    end;
remove_conf([{make,_Ref,_MF}|Cases], NoConf, Repeats) ->
    remove_conf(Cases, NoConf, Repeats);
remove_conf([{skip_case,{{_M,all},_Cmt},_Mode}|Cases], NoConf, Repeats) ->
    remove_conf(Cases, NoConf, Repeats);
remove_conf([{skip_case,{Type,_Ref,_MF,_Cmt}}|Cases],
	    NoConf, Repeats) when Type==conf;
				   Type==make ->
    remove_conf(Cases, NoConf, Repeats);
remove_conf([{skip_case,{Type,_Ref,_MF,_Cmt},_Mode}|Cases],
	    NoConf, Repeats) when Type==conf;
				  Type==make ->
    remove_conf(Cases, NoConf, Repeats);
remove_conf([C={Mod,error_in_suite,_}|Cases], NoConf, Repeats) ->
    FwMod = get_fw_mod(?MODULE),
    if Mod == FwMod ->
	    remove_conf(Cases, NoConf, Repeats);
       true ->
	    remove_conf(Cases, [C|NoConf], Repeats)
    end;
remove_conf([C|Cases], NoConf, Repeats) ->
    remove_conf(Cases, [C|NoConf], Repeats);
remove_conf([], NoConf, true) ->
    {repeats,lists:reverse(NoConf)};
remove_conf([], NoConf, false) ->
    lists:reverse(NoConf).

get_suites([{skip_case,{{Mod,_F},_Cmt},_Mode}|Tests], Mods) when is_atom(Mod) ->
    case add_mod(Mod, Mods) of
	true ->  get_suites(Tests, [Mod|Mods]);
	false -> get_suites(Tests, Mods)
    end;
get_suites([{Mod,_Case}|Tests], Mods) when is_atom(Mod) ->
    case add_mod(Mod, Mods) of
	true ->  get_suites(Tests, [Mod|Mods]);
	false -> get_suites(Tests, Mods)
    end;
get_suites([{Mod,_Func,_Args}|Tests], Mods) when is_atom(Mod) ->
    case add_mod(Mod, Mods) of
	true ->  get_suites(Tests, [Mod|Mods]);
	false -> get_suites(Tests, Mods)
    end;
get_suites([_|Tests], Mods) ->
    get_suites(Tests, Mods);

get_suites([], Mods) ->
    lists:reverse(Mods).

add_mod(Mod, Mods) ->
    case lists:reverse(atom_to_list(Mod)) of
        "ETIUS_" ++ _ -> % test suite
	     case lists:member(Mod, Mods) of
		 true ->  false;
		 false -> true
	     end;
        _ ->
            false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% do_test_cases(TopCases, SkipCases, Config, TimetrapSpec) ->
%%    exit(Result)
%%
%% TopCases = term()      (See collect_cases/3)
%% SkipCases = term()     (See collect_cases/3)
%% Config = term()        (See collect_cases/3)
%% TimetrapSpec = MultiplyTimetrap | {MultiplyTimetrap,ScaleTimetrap}
%% MultiplyTimetrap = integer() | infinity
%% ScaleTimetrap = bool()
%%
%% Initializes and starts the test run, for "ordinary" test suites.
%% Creates log directories and log files, inserts initial timestamps and
%% configuration information into the log files.
%%
%% This function is meant to be called by a process created by
%% spawn_tester/10, which sets up some necessary dictionary values.
do_test_cases(TopCases, SkipCases,
	      Config, MultiplyTimetrap) when is_integer(MultiplyTimetrap);
					     MultiplyTimetrap == infinity ->
    do_test_cases(TopCases, SkipCases, Config, {MultiplyTimetrap,true});

do_test_cases(TopCases, SkipCases,
	      Config, TimetrapData) when is_list(TopCases),
					 is_tuple(TimetrapData) ->
    {ok,TestDir} = start_log_file(),
    FwMod = get_fw_mod(?MODULE),
    case collect_all_cases(TopCases, SkipCases) of
	{error,Why} ->
	    print(1, "Error starting: ~tp", [Why]),
	    exit(test_suites_done);
	TestSpec0 ->
	    N = case remove_conf(TestSpec0) of
		    {repeats,_} -> unknown;
		    TS -> length(TS)
		end,
	    put(test_server_cases, N),
	    put(test_server_case_num, 0),

	    TestSpec =
		add_init_and_end_per_suite(TestSpec0, undefined, undefined, FwMod),

	    TI = get_target_info(),
	    print(1, "Starting test~ts",
		  [print_if_known(N, {", ~w test cases",[N]},
				  {" (with repeated test cases)",[]})]),
	    Test = get(test_server_name),
	    TestName = 	if is_list(Test) -> 
				lists:flatten(io_lib:format("~ts", [Test]));
			   true ->
				lists:flatten(io_lib:format("~tp", [Test]))
			end,			  
	    TestDescr = "Test " ++ TestName ++ " results",

	    test_server_sup:framework_call(report, [tests_start,{Test,N}]),

	    {Header,Footer} =
		case test_server_sup:framework_call(get_html_wrapper,
						    [TestDescr,true,TestDir,
						    {[],[2,3,4,7,8],[1,6]}], "") of
		    Empty when (Empty == "") ; (element(2,Empty) == "")  ->
			put(basic_html, true),
			{[html_header(TestDescr),
			  "<h2>Results for test ", TestName, "</h2>\n"],
			 "\n</body>\n</html>\n"};
		    {basic_html,Html0,Html1} ->
			put(basic_html, true),
			{Html0++["<h1>Results for <i>",TestName,"</i></h1>\n"],
			 Html1};
		    {xhtml,Html0,Html1} ->
			put(basic_html, false),
			{Html0++["<h1>Results for <i>",TestName,"</i></h1>\n"],
			 Html1}
		end,

	    print(html, Header),

	    print(html, xhtml("<p>", "<h4>")),
	    print_timestamp(html, "Test started at "),
	    print(html, xhtml("</p>", "</h4>")),
	    
	    print(html, xhtml("\n<p><b>Host info:</b><br>\n",
			      "\n<p><b>Host info:</b><br />\n")),
	    print_who(test_server_sup:hoststr(), test_server_sup:get_username()),
	    print(html, xhtml("<br>Used Erlang v~ts in <tt>~ts</tt></p>\n",
			      "<br />Used Erlang v~ts in \"~ts\"</p>\n"),
		  [erlang:system_info(version), code:root_dir()]),
	    
	    if FwMod == ?MODULE ->
		    print(html, xhtml("\n<p><b>Target Info:</b><br>\n",
				      "\n<p><b>Target Info:</b><br />\n")),
		    print_who(TI#target_info.host, TI#target_info.username),
		    print(html,xhtml("<br>Used Erlang v~ts in <tt>~ts</tt></p>\n",
				     "<br />Used Erlang v~ts in \"~ts\"</p>\n"),
			  [TI#target_info.version, TI#target_info.root_dir]);
	       true ->
		    case test_server_sup:framework_call(target_info, []) of
			TargetInfo when is_list(TargetInfo),
			                length(TargetInfo) > 0 ->
			    print(html, xhtml("\n<p><b>Target info:</b><br>\n",
					      "\n<p><b>Target info:</b><br />\n")),
			    print(html, "~ts</p>\n", [TargetInfo]);
			_ ->
			    ok
		    end
	    end,
	    CoverLog =
		case get(test_server_cover_log_dir) of
		    undefined ->
			?coverlog_name;
		    AbsLogDir ->
			AbsLog = filename:join(AbsLogDir,?coverlog_name),
			make_relative(AbsLog, TestDir)
		end,
	    print(html,
		  "<p><ul>\n"
		  "<li><a href=\"~ts\">Full textual log</a></li>\n"
		  "<li><a href=\"~ts\">Coverage log</a></li>\n"
		  "<li><a href=\"~ts\">Unexpected I/O log</a></li>\n</ul></p>\n",
		  [?suitelog_name,CoverLog,?unexpected_io_log]),
	    print(html,
		  "<p>~ts</p>\n" ++
		  xhtml(["<table bgcolor=\"white\" border=\"3\" cellpadding=\"5\">\n",
			 "<thead>\n"],
			["<table id=\"",?sortable_table_name,"\">\n",
			 "<thead>\n"]) ++
		      "<tr><th>Num</th><th>Module</th><th>Group</th>" ++
		      "<th>Case</th><th>Log</th><th>Time</th><th>Result</th>" ++
		      "<th>Comment</th></tr>\n</thead>\n<tbody>\n",
		  [print_if_known(N, {"<i>Executing <b>~w</b> test cases...</i>"
				      ++ xhtml("\n<br>\n", "\n<br />\n"),[N]},
				  {"",[]})]),

	    print(major, "=cases         ~w", [get(test_server_cases)]),
	    print(major, "=user          ~ts", [TI#target_info.username]),
	    print(major, "=host          ~ts", [TI#target_info.host]),

	    %% If there are no hosts specified,use only the local host
	    case controller_call(get_hosts) of
		[] ->
		    print(major, "=hosts         ~ts", [TI#target_info.host]),
		    controller_call({set_hosts, [TI#target_info.host]});
		Hosts ->
		    Str = lists:flatten(lists:map(fun(X) -> [X," "] end, Hosts)),
		    print(major, "=hosts         ~ts", [Str])
	    end,
	    print(major, "=emulator_vsn  ~ts", [TI#target_info.version]),
	    print(major, "=emulator      ~ts", [TI#target_info.emulator]),
	    print(major, "=otp_release   ~ts", [TI#target_info.otp_release]),
	    print(major, "=started       ~s",
		   [lists:flatten(timestamp_get(""))]),

	    test_server_io:set_footer(Footer),

	    run_test_cases(TestSpec, Config, TimetrapData)
    end;

do_test_cases(TopCase, SkipCases, Config, TimetrapSpec) ->
    %% when not list(TopCase)
    do_test_cases([TopCase], SkipCases, Config, TimetrapSpec).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_log_file() -> {ok,TestDirName} | exit({Error,Reason})
%% Stem = string()
%%
%% Creates the log directories, the major log file and the html log file.
%% The log files are initialized with some header information.
%%
%% The name of the log directory will be <Name>.logs/run.<Date>/ where
%% Name is the test suite name and Date is the current date and time.

start_log_file() ->
    Dir  = get(test_server_dir),
    case file:make_dir(Dir) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	MkDirError ->
	    log_file_error(MkDirError, Dir)
    end,
    TestDir = timestamp_filename_get(filename:join(Dir, "run.")),
    TestDir1 =
	case file:make_dir(TestDir) of
	    ok ->
		TestDir;
	    {error,eexist} ->
		timer:sleep(1000),
		%% we need min 1 second between timestamps unfortunately
		TestDirX = timestamp_filename_get(filename:join(Dir, "run.")),
		case file:make_dir(TestDirX) of
		    ok ->
			TestDirX;
		    MkDirError2 ->
			log_file_error(MkDirError2, TestDirX)
		end;
	    MkDirError2 ->
		log_file_error(MkDirError2, TestDir)
	end,
    FilenameMode = file:native_name_encoding(),
    ok = write_file(filename:join(Dir, ?last_file),
		    TestDir1 ++ "\n",
		    FilenameMode),
    ok = write_file(?last_file, TestDir1 ++ "\n", FilenameMode),
    put(test_server_log_dir_base,TestDir1),

    MajorName = filename:join(TestDir1, ?suitelog_name),
    HtmlName = MajorName ++ ?html_ext,
    UnexpectedName = filename:join(TestDir1, ?unexpected_io_log),

    {ok,Major} = open_utf8_file(MajorName),
    {ok,Html}  = open_html_file(HtmlName),

    {UnexpHeader,UnexpFooter} =
	case test_server_sup:framework_call(get_html_wrapper,
					    ["Unexpected I/O log",false,
					     TestDir, undefined],"") of
	    UEmpty when (UEmpty == "") ; (element(2,UEmpty) == "")  ->
		{html_header("Unexpected I/O log"),"\n</body>\n</html>\n"};
	    {basic_html,UH,UF} ->
		{UH,UF};
	    {xhtml,UH,UF} ->
		{UH,UF}
	end,

    {ok,Unexpected} = open_html_file(UnexpectedName),
    io:put_chars(Unexpected, [UnexpHeader,			      
			      xhtml("<br>\n<h2>Unexpected I/O</h2>",
				    "<br />\n<h3>Unexpected I/O</h3>"),
			      "\n<pre>\n"]),
    put(test_server_unexpected_footer,{UnexpectedName,UnexpFooter}),

    test_server_io:set_fd(major, Major),
    test_server_io:set_fd(html, Html),
    test_server_io:set_fd(unexpected_io, Unexpected),

    %% we must assume the redirection file (to the latest suite index) can
    %% be stored on the level above the log directory of the current test
    TopDir = filename:dirname(get(test_server_framework_logdir)),
    RedirectLink = filename:join(TopDir, ?suitelog_latest_name ++ ?html_ext),
    make_html_link(RedirectLink, HtmlName, redirect),

    make_html_link(filename:absname(?last_test ++ ?html_ext),
		   HtmlName, filename:basename(Dir)),
    LinkName = filename:join(Dir, ?last_link),
    make_html_link(LinkName ++ ?html_ext, HtmlName,
		   filename:basename(Dir)),

    PrivDir = filename:join(TestDir1, ?priv_dir),
    ok = file:make_dir(PrivDir),
    put(test_server_priv_dir,PrivDir++"/"),
    print_timestamp(major, "Suite started at "),

    LogInfo = [{topdir,Dir},{rundir,lists:flatten(TestDir1)}],
    test_server_sup:framework_call(report, [loginfo,LogInfo]),
    {ok,TestDir1}.

log_file_error(Error, Dir) ->
    exit({cannot_create_log_dir,{Error,lists:flatten(Dir)}}).

make_html_link(LinkName, Target, Explanation) ->
    %% if possible use a relative reference to Target.
    TargetL = filename:split(Target),
    PwdL = filename:split(filename:dirname(LinkName)),
    Href = case lists:prefix(PwdL, TargetL) of
	       true ->
		   uri_encode(filename:join(lists:nthtail(length(PwdL),TargetL)));
	       false ->
		   "file:" ++ uri_encode(Target)
	   end,
    H = if Explanation == redirect ->
                Meta = ["<meta http-equiv=\"refresh\" "
                        "content=\"0; url=", Href, "\" />\n"],
                [html_header("redirect", Meta), "</html>\n"];
           true ->
                [html_header(Explanation),
                 "<h1>Last test</h1>\n"
                 "<a href=\"",Href,"\">",Explanation,"</a>\n"
                 "</body>\n</html>\n"]
        end,
    ok = write_html_file(LinkName, H).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_minor_log_file(Mod, Func, ParallelTC) -> AbsName
%% Mod = atom()
%% Func = atom()
%% ParallelTC = bool()
%% AbsName = string()
%%
%% Create a minor log file for the test case Mod,Func,Args. The log file
%% will be stored in the log directory under the name <Mod>.<Func>.html.
%% Some header info will also be inserted into the log file. If the test
%% case runs in a parallel group, then to avoid clashing file names if the
%% case is executed more than once, the name <Mod>.<Func>.<Timestamp>.html
%% is used.

start_minor_log_file(Mod, Func, ParallelTC) ->
    MFA = {Mod,Func,1},
    LogDir = get(test_server_log_dir_base),
    Name = minor_log_file_name(Mod,Func),
    AbsName = filename:join(LogDir, Name),
    case (ParallelTC orelse (element(1,file:read_file_info(AbsName))==ok)) of
	false ->                           %% normal case, unique name
	    start_minor_log_file1(Mod, Func, LogDir, AbsName, MFA);
	true ->                            %% special case, duplicate names
	    Tag = test_server_sup:unique_name(),
            Name1 = minor_log_file_name(Mod,Func,[$.|Tag]),
	    AbsName1 = filename:join(LogDir, Name1),
	    start_minor_log_file1(Mod, Func, LogDir, AbsName1, MFA)
    end.

start_minor_log_file1(Mod, Func, LogDir, AbsName, MFA) ->
    {ok,Fd} = open_html_file(AbsName),
    Lev = get(test_server_minor_level)+1000, %% far down in the minor levels
    put(test_server_minor_fd, Fd),
    test_server_gl:set_minor_fd(group_leader(), Fd, MFA),

    TestDescr = io_lib:format("Test ~w:~tw result", [Mod,Func]),
    {Header,Footer} =
	case test_server_sup:framework_call(get_html_wrapper,
					    [TestDescr,false,
					     filename:dirname(AbsName),
					     undefined], "") of
	    Empty when (Empty == "") ; (element(2,Empty) == "")  ->
		put(basic_html, true),
		{html_header(TestDescr), "\n</body>\n</html>\n"};
	    {basic_html,Html0,Html1} ->
		put(basic_html, true),
		{Html0,Html1};
	    {xhtml,Html0,Html1} ->
		put(basic_html, false),
		{Html0,Html1}
	end,
    put(test_server_minor_footer, Footer),
    io:put_chars(Fd, Header),

    io:put_chars(Fd, "<a name=\"top\"></a>"),
    io:put_chars(Fd, "<pre>\n"),

    SrcListing = downcase(atom_to_list(Mod)) ++ ?src_listing_ext,

    case get_fw_mod(?MODULE) of
	Mod when Func == error_in_suite ->
	    ok;
	_ ->
	    {Info,Arity} =
		if Func == init_per_suite; Func == end_per_suite ->
			{"Config function: ", 1};
		   Func == init_per_group; Func == end_per_group ->
			{"Config function: ", 2};
		   true ->
			{"Test case: ", 1}
		end,
	    
	    case {filelib:is_file(filename:join(LogDir, SrcListing)),
		  lists:member(no_src, get(test_server_logopts))} of
		{true,false} ->
		    print(Lev, ["$tc_html",
				Info ++ "<a href=\"~ts#~ts\">~w:~tw/~w</a> "
				"(click for source code)\n"],
			  [uri_encode(SrcListing),
			   uri_encode(atom_to_list(Func)++"-1",utf8),
			   Mod,Func,Arity]);
		_ ->
		    print(Lev, ["$tc_html",Info ++ "~w:~tw/~w\n"], [Mod,Func,Arity])
	    end
    end,

    AbsName.

stop_minor_log_file() ->
    test_server_gl:unset_minor_fd(group_leader()),
    Fd = get(test_server_minor_fd),
    Footer = get(test_server_minor_footer),
    io:put_chars(Fd, "</pre>\n" ++ Footer),
    ok = file:close(Fd),
    put(test_server_minor_fd, undefined).

minor_log_file_name(Mod,Func) ->
    minor_log_file_name(Mod,Func,"").
minor_log_file_name(Mod,Func,Tag) ->
    Name =
        downcase(
          lists:flatten(
            io_lib:format("~w.~tw~s~s", [Mod,Func,Tag,?html_ext]))),
    Ok = file:native_name_encoding()==utf8
        orelse io_lib:printable_latin1_list(Name),
    if Ok -> Name;
       true -> exit({error,unicode_name_on_latin1_file_system})
    end.

downcase(S) -> downcase(S, []).
downcase([Uc|Rest], Result) when $A =< Uc, Uc =< $Z ->
    downcase(Rest, [Uc-$A+$a|Result]);
downcase([C|Rest], Result) ->
    downcase(Rest, [C|Result]);
downcase([], Result) ->
    lists:reverse(Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% html_convert_modules(TestSpec, Config) -> ok
%%  Isolate the modules affected by TestSpec and
%%  make sure they are converted to html.
%%
%%  Errors are silently ignored.

html_convert_modules(TestSpec, _Config, FwMod) ->
    Mods = html_isolate_modules(TestSpec, FwMod),
    html_convert_modules(Mods),
    copy_html_files(get(test_server_dir), get(test_server_log_dir_base)).

%% Retrieve a list of modules out of the test spec.
html_isolate_modules(List, FwMod) ->
    html_isolate_modules(List, sets:new(), FwMod).

html_isolate_modules([], Set, _) -> sets:to_list(Set);
html_isolate_modules([{skip_case,{_Case,_Cmt},_Mode}|Cases], Set, FwMod) ->
    html_isolate_modules(Cases, Set, FwMod);
html_isolate_modules([{conf,_Ref,Props,{FwMod,_Func}}|Cases], Set, FwMod) ->
    Set1 = case proplists:get_value(suite, Props) of
	       undefined -> Set;
	       Mod -> sets:add_element(Mod, Set)
	   end,
    html_isolate_modules(Cases, Set1, FwMod);
html_isolate_modules([{conf,_Ref,_Props,{Mod,_Func}}|Cases], Set, FwMod) ->
    html_isolate_modules(Cases, sets:add_element(Mod, Set), FwMod);
html_isolate_modules([{skip_case,{conf,_Ref,{FwMod,_Func},_Cmt},Mode}|Cases],
		     Set, FwMod) ->
    Set1 = case proplists:get_value(suite, get_props(Mode)) of
	       undefined -> Set;
	       Mod -> sets:add_element(Mod, Set)
	   end,
    html_isolate_modules(Cases, Set1, FwMod);
html_isolate_modules([{skip_case,{conf,_Ref,{Mod,_Func},_Cmt},_Props}|Cases],
		     Set, FwMod) ->
    html_isolate_modules(Cases, sets:add_element(Mod, Set), FwMod);
html_isolate_modules([{Mod,_Case}|Cases], Set, FwMod) ->
    html_isolate_modules(Cases, sets:add_element(Mod, Set), FwMod);
html_isolate_modules([{Mod,_Case,_Args}|Cases], Set, FwMod) ->
    html_isolate_modules(Cases, sets:add_element(Mod, Set), FwMod).

%% Given a list of modules, convert each module's source code to HTML.
html_convert_modules([Mod|Mods]) ->
    case code:which(Mod) of
	Path when is_list(Path) ->
	    SrcFile = filename:rootname(Path) ++ ".erl",
	    FoundSrcFile =
		case file:read_file_info(SrcFile) of
		    {ok,SInfo} ->
			{SrcFile,SInfo};
		    {error,_} ->
			ModInfo = Mod:module_info(compile),
			case proplists:get_value(source, ModInfo) of
			    undefined ->
				undefined;
			    OtherSrcFile ->
				case file:read_file_info(OtherSrcFile) of
				    {ok,SInfo} ->
					{OtherSrcFile,SInfo};
				    {error,_} ->
					undefined
				end
			end
		end,
	    case FoundSrcFile of
		undefined ->
		    html_convert_modules(Mods);
		{SrcFile1,SrcFileInfo} ->
		    DestDir = get(test_server_dir),
		    Name = atom_to_list(Mod),
		    DestFile = filename:join(DestDir,
					     downcase(Name)++?src_listing_ext),
		    _ = html_possibly_convert(SrcFile1, SrcFileInfo, DestFile),
		    html_convert_modules(Mods)
	    end;
	_Other ->
	    html_convert_modules(Mods)
    end;
html_convert_modules([]) -> ok.

%% Convert source code to HTML if possible and needed.
html_possibly_convert(Src, SrcInfo, Dest) ->
    case file:read_file_info(Dest) of
	{ok,DestInfo} when DestInfo#file_info.mtime >= SrcInfo#file_info.mtime ->
	    ok;					% dest file up to date
	_ ->
	    InclPath = case application:get_env(test_server, include) of
			   {ok,Incls} -> Incls;
			   _ -> []
		       end,

	    OutDir = get(test_server_log_dir_base),
	    case test_server_sup:framework_call(get_html_wrapper,
						["Module "++Src,false,
						 OutDir,undefined,
						 encoding(Src)], "") of
		Empty when (Empty == "") ; (element(2,Empty) == "")  ->
		    erl2html2:convert(Src, Dest, InclPath);
		{_,Header,_} ->
		    erl2html2:convert(Src, Dest, InclPath, Header)
	    end
    end.

%% Copy all HTML files in InDir to OutDir.
copy_html_files(InDir, OutDir) ->
    Files = filelib:wildcard(filename:join(InDir, "*" ++ ?src_listing_ext)),
    lists:foreach(fun (Src) -> copy_html_file(Src, OutDir) end, Files).

copy_html_file(Src, DestDir) ->
    Dest = filename:join(DestDir, filename:basename(Src)),
    case file:read_file(Src) of
	{ok,Bin} ->
	    ok = write_binary_file(Dest, Bin);
	{error,_Reason} ->
	    io:format("File ~ts: read failed\n", [Src])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add_init_and_end_per_suite(TestSpec, Mod, Ref, FwMod) -> NewTestSpec
%%
%% Expands TestSpec with an initial init_per_suite, and a final
%% end_per_suite element, per each discovered suite in the list.

add_init_and_end_per_suite([{make,_,_}=Case|Cases], LastMod, LastRef, FwMod) ->
    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef, FwMod)];
add_init_and_end_per_suite([{skip_case,{{Mod,all},_},_}=Case|Cases], LastMod,
			   LastRef, FwMod) when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_end_per_suite_and_skip(LastMod, LastRef, Mod, FwMod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod,
						 NextRef, FwMod)];
add_init_and_end_per_suite([{skip_case,{{Mod,_},_Cmt},_Mode}=Case|Cases],
			   LastMod, LastRef, FwMod) when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod, FwMod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod,
						 NextRef, FwMod)];
add_init_and_end_per_suite([{skip_case,{conf,_,{Mod,_},_},_}=Case|Cases],
			   LastMod, LastRef, FwMod) when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod, FwMod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod,
						 NextRef, FwMod)];
add_init_and_end_per_suite([{skip_case,{conf,_,{Mod,_},_}}=Case|Cases], LastMod,
			   LastRef, FwMod) when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod, FwMod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod,
						 NextRef, FwMod)];
add_init_and_end_per_suite([{conf,Ref,Props,{FwMod,Func}}=Case|Cases], LastMod,
			   LastRef, FwMod) ->
    %% if Mod == FwMod, this conf test is (probably) a test case group where
    %% the init- and end-functions are missing in the suite, and if so,
    %% the suite name should be stored as {suite,Suite} in Props
    case proplists:get_value(suite, Props) of
	Suite when Suite =/= undefined, Suite =/= LastMod ->
	    {PreCases, NextMod, NextRef} =
		do_add_init_and_end_per_suite(LastMod, LastRef, Suite, FwMod),
	    Case1 = {conf,Ref,[{suite,NextMod}|proplists:delete(suite,Props)],
		     {FwMod,Func}},
	    PreCases ++ [Case1|add_init_and_end_per_suite(Cases, NextMod,
							  NextRef, FwMod)];
	_ ->
	    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef, FwMod)]
    end;
add_init_and_end_per_suite([{conf,_,_,{Mod,_}}=Case|Cases], LastMod,
			   LastRef, FwMod) when Mod =/= LastMod, Mod =/= FwMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod, FwMod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod,
						 NextRef, FwMod)];
add_init_and_end_per_suite([SkipCase|Cases], LastMod, LastRef, FwMod)
  when element(1,SkipCase) == skip_case;  element(1,SkipCase) == auto_skip_case->
    [SkipCase|add_init_and_end_per_suite(Cases, LastMod, LastRef, FwMod)];
add_init_and_end_per_suite([{conf,_,_,_}=Case|Cases], LastMod, LastRef, FwMod) ->
    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef, FwMod)];
add_init_and_end_per_suite([{Mod,_}=Case|Cases], LastMod, LastRef, FwMod)
  when Mod =/= LastMod, Mod =/= FwMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod, FwMod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod,
						 NextRef, FwMod)];
add_init_and_end_per_suite([{Mod,_,_}=Case|Cases], LastMod, LastRef, FwMod)
  when Mod =/= LastMod, Mod =/= FwMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod, FwMod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod,
						 NextRef, FwMod)];
add_init_and_end_per_suite([Case|Cases], LastMod, LastRef, FwMod)->
    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef, FwMod)];
add_init_and_end_per_suite([], _LastMod, undefined, _FwMod) ->
    [];
add_init_and_end_per_suite([], _LastMod, skipped_suite, _FwMod) ->
    [];
add_init_and_end_per_suite([], LastMod, LastRef, FwMod) ->
    %% we'll add end_per_suite here even if it's not exported
    %% (and simply let the call fail if it's missing)
    case {erlang:function_exported(LastMod, end_per_suite, 1),
          erlang:function_exported(LastMod, init_per_suite, 1)} of
	{false,false} ->
	    %% let's call a "fake" end_per_suite if it exists			
	    case erlang:function_exported(FwMod, end_per_suite, 1) of
		true ->					
		    [{conf,LastRef,[{suite,LastMod}],{FwMod,end_per_suite}}];
		false ->		
		    [{conf,LastRef,[],{LastMod,end_per_suite}}]
	    end;
	_ ->
            %% If any of these exist, the other should too
            %% (required and documented). If it isn't, it will fail
            %% with reason 'undef'.
	    [{conf,LastRef,[],{LastMod,end_per_suite}}]
    end.    

do_add_init_and_end_per_suite(LastMod, LastRef, Mod, FwMod) ->
    _ = case code:is_loaded(Mod) of
	false -> code:load_file(Mod);
	_ -> ok
    end,
    {Init,NextMod,NextRef} =
	case {erlang:function_exported(Mod, init_per_suite, 1),
              erlang:function_exported(Mod, end_per_suite, 1)} of
	    {false,false} ->
		%% let's call a "fake" init_per_suite if it exists
		case erlang:function_exported(FwMod, init_per_suite, 1) of
		    true ->
			Ref = make_ref(),
			{[{conf,Ref,[{suite,Mod}],
			   {FwMod,init_per_suite}}],Mod,Ref};
		    false ->
			{[],Mod,undefined}
		end;
	    _ ->
                %% If any of these exist, the other should too
                %% (required and documented). If it isn't, it will fail
                %% with reason 'undef'.
		Ref = make_ref(),
		{[{conf,Ref,[],{Mod,init_per_suite}}],Mod,Ref}
	end,
    Cases =
	if LastRef==undefined ->
		Init;
	   LastRef==skipped_suite ->
		Init;
	   true ->
		%% we'll add end_per_suite here even if it's not exported
		%% (and simply let the call fail if it's missing)
		case {erlang:function_exported(LastMod, end_per_suite, 1),
                      erlang:function_exported(LastMod, init_per_suite, 1)} of
		    {false,false} ->
			%% let's call a "fake" end_per_suite if it exists
			case erlang:function_exported(FwMod, end_per_suite, 1) of
			    true ->				
				[{conf,LastRef,[{suite,Mod}],
				  {FwMod,end_per_suite}}|Init];
			    false ->
				[{conf,LastRef,[],{LastMod,end_per_suite}}|Init]
			end;
		    _ ->
                        %% If any of these exist, the other should too
                        %% (required and documented). If it isn't, it will fail
                        %% with reason 'undef'.
			[{conf,LastRef,[],{LastMod,end_per_suite}}|Init]
                end
	end,
    {Cases,NextMod,NextRef}.

do_add_end_per_suite_and_skip(LastMod, LastRef, Mod, FwMod) ->
    case LastRef of
	No when No==undefined ; No==skipped_suite ->
	    {[],Mod,skipped_suite};
	_Ref ->
	    case {erlang:function_exported(LastMod, end_per_suite, 1),
                  erlang:function_exported(LastMod, init_per_suite, 1)} of
		{false,false} ->
		    case erlang:function_exported(FwMod, end_per_suite, 1) of
			true ->				
			    %% let's call "fake" end_per_suite if it exists
			    {[{conf,LastRef,[],{FwMod,end_per_suite}}],
			     Mod,skipped_suite};
			false ->
			    {[{conf,LastRef,[],{LastMod,end_per_suite}}],
			     Mod,skipped_suite}
		    end;
		_ ->
                    %% If any of these exist, the other should too
                    %% (required and documented). If it isn't, it will fail
                    %% with reason 'undef'.
		    {[{conf,LastRef,[],{LastMod,end_per_suite}}],
		     Mod,skipped_suite}
	    end    	    
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_cases(TestSpec, Config, TimetrapData) -> exit(Result)
%%
%% Runs the specified tests, then displays/logs the summary.

run_test_cases(TestSpec, Config, TimetrapData) ->
    test_server:init_valgrind(),
    case lists:member(no_src, get(test_server_logopts)) of
	true ->
	    ok;
	false ->
	    FwMod = get_fw_mod(?MODULE),
	    html_convert_modules(TestSpec, Config, FwMod)
    end,

    run_test_cases_loop(TestSpec, [Config], TimetrapData, [], []),

    {AllSkippedN,UserSkipN,AutoSkipN,SkipStr} =
	case get(test_server_skipped) of
	    {0,0} -> {0,0,0,""};
	    {US,AS} -> {US+AS,US,AS,io_lib:format(", ~w skipped", [US+AS])}
	end,
    OkN = get(test_server_ok),
    FailedN = get(test_server_failed),
    print(1, "TEST COMPLETE, ~w ok, ~w failed~ts of ~w test cases\n",
	  [OkN,FailedN,SkipStr,OkN+FailedN+AllSkippedN]),
    test_server_sup:framework_call(report, [tests_done,
					    {OkN,FailedN,{UserSkipN,AutoSkipN}}]),
    print(major, "=finished      ~s", [lists:flatten(timestamp_get(""))]),
    print(major, "=failed        ~w", [FailedN]),
    print(major, "=successful    ~w", [OkN]),
    print(major, "=user_skipped  ~w", [UserSkipN]),
    print(major, "=auto_skipped  ~w", [AutoSkipN]),
    exit(test_suites_done).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_cases_loop(TestCases, Config, TimetrapData, Mode, Status) -> ok
%% TestCases = [Test,...]
%% Config = [[{Key,Val},...],...]
%% TimetrapData = {MultiplyTimetrap,ScaleTimetrap}
%% MultiplyTimetrap = integer() | infinity
%% ScaleTimetrap = bool()
%% Mode = [{Ref,[Prop,..],StartTime}]
%% Ref = reference()
%% Prop = {name,Name} | sequence | parallel |
%%        shuffle | {shuffle,Seed} |
%%        repeat | {repeat,N} |
%%        repeat_until_all_ok | {repeat_until_all_ok,N} |
%%        repeat_until_any_ok | {repeat_until_any_ok,N} |
%%        repeat_until_any_fail | {repeat_until_any_fail,N} |
%%        repeat_until_all_fail | {repeat_until_all_fail,N}
%% Status = [{Ref,{{Ok,Skipped,Failed},CopiedCases}}]
%% Ok = Skipped = Failed = [Case,...]
%%
%% Execute the TestCases under configuration Config. Config is a list
%% of lists, where hd(Config) holds the config tuples for the current
%% conf case and tl(Config) is the data for the higher level conf cases.
%% Config data is "inherited" from top to nested conf cases, but
%% never the other way around. if length(Config) == 1, Config contains
%% only the initial config data for the suite.
%%
%% Test may be one of the following:
%%
%% {conf,Ref,Props,{Mod,Func}} Mod:Func is a configuration modification
%% function, call it with the current configuration as argument. It will
%% return a new configuration.
%%
%% {make,Ref,{Mod,Func,Args}} Mod:Func is a make function, and it is called
%% with the given arguments.
%%
%% {Mod,Case} This is a normal test case. Determine the correct
%% configuration, and insert {Mod,Case,Config} as head of the list,
%% then reiterate.
%%
%% {Mod,Case,Args} A test case with predefined argument (usually a normal
%% test case which just got a fresh configuration (see above)).
%%
%% {skip_case,{conf,Ref,Case,Comment}} An init conf case gets skipped
%% by the user. This will also cause the end conf case to be skipped.
%% Note that it is not possible to skip an end conf case directly (it
%% can only be skipped indirectly by a skipped init conf case). The
%% comment (which gets printed in the log files) describes why the case
%% was skipped.
%%
%% {skip_case,{Case,Comment},Mode} A normal test case skipped by the user.
%% The comment (which gets printed in the log files) describes why the
%% case was skipped.
%%
%% {auto_skip_case,{conf,Ref,Case,Comment},Mode} This is the result of
%% an end conf case being automatically skipped due to a failing init
%% conf case. It could also be a nested conf case that gets skipped
%% because of a failed or skipped top level conf.
%%
%% {auto_skip_case,{Case,Comment},Mode} This is a normal test case which
%% gets automatically skipped because of a failing init conf case or
%% because of a failing previous test case in a sequence.
%%
%% -------------------------------------------------------------------
%% Description of IO handling during execution of parallel test cases:
%% -------------------------------------------------------------------
%%
%% A conf group can have an associated list of properties. If the
%% parallel property is specified for a group, it means the test cases
%% should be spawned and run in parallel rather than called sequentially
%% (which is always the default mode). Test cases that execute in parallel
%% also write to their respective minor log files in parallel. Printouts
%% to common log files, such as the summary html file and the major log
%% file on text format, still have to be processed sequentially. For this
%% reason, the Mode argument specifies if a parallel group is currently
%% being executed.
%%
%% The low-level mechanism for buffering IO for the common log files
%% is handled by the test_server_io module. Buffering is turned on by
%% test_server_io:start_transaction/0 and off by calling
%% test_server_io:end_transaction/0. The buffered data for the transaction
%% can printed by calling test_server_io:print_buffered/1.
%%
%% This module is responsible for turning on IO buffering and to later
%% test_server_io:print_buffered/1 to print the data. To help with this,
%% two variables in the process dictionary are used:
%% 'test_server_common_io_handler' and 'test_server_queued_io'. The values
%% are set to as following:
%%
%%   Value	Meaning
%%   -----     -------
%%   undefined	No parallel test cases running
%%   {tc,Pid}	Running test cases in a top-level parallel group
%%   {Ref,Pid}	Running sequential test case inside a parallel group
%%
%% FIXME: The Pid is no longer used.
%%
%% If a conf group nested under a parallel group in the test
%% specification should be started, the 'test_server_common_io_handler'
%% value gets set also on the main process.
%%
%% During execution of a parallel group (or of a group nested under a
%% parallel group), *any* new test case being started gets registered
%% in a list saved in the dictionary with 'test_server_queued_io' as key.
%% When the top level parallel group is finished (only then can we be
%% sure all parallel test cases have finished and "reported in"), the
%% list of test cases is traversed in order and test_server_io:print_buffered/1
%% can be called for each test case. See handle_test_case_io_and_status/0
%% for details.
%%
%% To be able to handle nested conf groups with different properties,
%% the Mode argument specifies a list of {Ref,Properties} tuples.
%% The head of the Mode list at any given time identifies the group
%% currently being processed. The tail of the list identifies groups
%% on higher level.
%%
%% -------------------------------------------------------------------
%% Notes on parallel execution of test cases
%% -------------------------------------------------------------------
%%
%% A group nested under a parallel group will start executing in
%% parallel with previous (parallel) test cases (no matter what
%% properties the nested group has). Test cases are however never
%% executed in parallel with the start or end conf case of the same
%% group! Because of this, the test_server_ctrl loop waits at
%% the end conf of a group for all parallel cases to finish
%% before the end conf case actually executes. This has the effect
%% that it's only after a nested group has finished that any
%% remaining parallel cases in the previous group get spawned (*).
%% Example (all parallel cases):
%%
%% group1_init   |---->
%% group1_case1        | --------->
%% group1_case2        | --------------------------------->
%% group2_init         | ---->
%% group2_case1               | ------>
%% group2_case2               | ---------->
%% group2_end                              | --->
%% group1_case3                               (*)| ---->
%% group1_case4                               (*)| -->
%% group1_end                                              | --->
%%

run_test_cases_loop([{SkipTag,CaseData={Type,_Ref,_Case,_Comment}}|Cases],
		    Config, TimetrapData, Mode, Status) when
      ((SkipTag==auto_skip_case) or (SkipTag==skip_case)) and
      ((Type==conf) or (Type==make)) ->
    run_test_cases_loop([{SkipTag,CaseData,Mode}|Cases],
			Config, TimetrapData, Mode, Status);

run_test_cases_loop([{SkipTag,{Type,Ref,Case,Comment},SkipMode}|Cases],
		    Config, TimetrapData, Mode, Status) when
      ((SkipTag==auto_skip_case) or (SkipTag==skip_case)) and
      ((Type==conf) or (Type==make)) ->
    ok = file:set_cwd(filename:dirname(get(test_server_dir))),
    CurrIOHandler = get(test_server_common_io_handler),
    ParentMode = tl(Mode),

    {AutoOrUser,ReportTag} = 
	if SkipTag == auto_skip_case -> {auto,tc_auto_skip};
	   SkipTag == skip_case      -> {user,tc_user_skip}
	end,

    %% check and update the mode for test case execution and io msg handling
    case {curr_ref(Mode),check_props(parallel, Mode)} of
	{Ref,Ref} ->
	    case check_props(parallel, ParentMode) of
		false ->
		    %% this is a skipped end conf for a top level parallel
		    %% group, buffered io can be flushed
		    _ = handle_test_case_io_and_status(),
		    set_io_buffering(undefined),
		    {Mod,Func} = skip_case(AutoOrUser, Ref, 0, Case, Comment,
					   false, SkipMode),
		    ConfData = {Mod,{Func,get_name(SkipMode)},Comment},
		    test_server_sup:framework_call(report,
						   [ReportTag,ConfData]),
		    run_test_cases_loop(Cases, Config, TimetrapData, ParentMode,
					delete_status(Ref, Status));
		_ ->
		    %% this is a skipped end conf for a parallel group nested
		    %% under a parallel group (io buffering is active)
		    _ = wait_for_cases(Ref),
		    {Mod,Func} = skip_case(AutoOrUser, Ref, 0, Case, Comment,
					   true, SkipMode),
		    ConfData = {Mod,{Func,get_name(SkipMode)},Comment},
		    test_server_sup:framework_call(report, [ReportTag,ConfData]),
		    case CurrIOHandler of
			{Ref,_} ->
			    %% current_io_handler was set by start conf of this
			    %% group, so we can unset it now (no more io from main
			    %% process needs to be buffered)
			    set_io_buffering(undefined);
			_ ->
			    ok
		    end,
		    run_test_cases_loop(Cases, Config,
					TimetrapData, ParentMode,
					delete_status(Ref, Status))
	    end;
	{Ref,false} ->
	    %% this is a skipped end conf for a non-parallel group that's not
	    %% nested under a parallel group
	    {Mod,Func} = skip_case(AutoOrUser, Ref, 0, Case, Comment,
				   false, SkipMode),
	    ConfData = {Mod,{Func,get_name(SkipMode)},Comment},
	    test_server_sup:framework_call(report, [ReportTag,ConfData]),

	    %% Check if this group is auto skipped because of error in the
	    %% init conf. If so, check if the parent group is a sequence,
	    %% and if it is, skip all proceeding tests in that group.
	    GrName = get_name(Mode),
	    Cases1 =
		case get_tc_results(Status) of
		    {_,_,Fails} when length(Fails) > 0 ->
			case lists:member({group_result,GrName}, Fails) of
			    true ->
				case check_prop(sequence, ParentMode) of
				    false ->
					Cases;
				    ParentRef ->
					Reason = {group_result,GrName,failed},
					skip_cases_upto(ParentRef, Cases,
							Reason, tc, ParentMode,
							SkipTag)
				end;
			    false ->
				Cases
			end;
		    _ ->
			Cases
		end,
	    run_test_cases_loop(Cases1, Config, TimetrapData, ParentMode,
				delete_status(Ref, Status));
	{Ref,_} ->
	    %% this is a skipped end conf for a non-parallel group nested under
	    %% a parallel group (io buffering is active)
	    {Mod,Func} = skip_case(AutoOrUser, Ref, 0, Case, Comment,
				   true, SkipMode),
	    ConfData = {Mod,{Func,get_name(SkipMode)},Comment},
	    test_server_sup:framework_call(report, [ReportTag,ConfData]),
	    case CurrIOHandler of
		{Ref,_} ->
		    %% current_io_handler was set by start conf of this
		    %% group, so we can unset it now (no more io from main
		    %% process needs to be buffered)
		    set_io_buffering(undefined);
		_ ->
		    ok
	    end,
	    run_test_cases_loop(Cases, Config, TimetrapData, tl(Mode),
				delete_status(Ref, Status));
	{_,false} ->
	    %% this is a skipped start conf for a group which is not nested
	    %% under a parallel group
	    {Mod,Func} = skip_case(AutoOrUser, Ref, 0, Case, Comment,
				   false, SkipMode),
	    ConfData = {Mod,{Func,get_name(SkipMode)},Comment},
	    test_server_sup:framework_call(report, [ReportTag,ConfData]),
	    run_test_cases_loop(Cases, Config, TimetrapData,
				[conf(Ref,[])|Mode], Status);
	{_,Ref0} when is_reference(Ref0) ->
	    %% this is a skipped start conf for a group nested under a parallel
	    %% group and if this is the first nested group, io buffering must
	    %% be activated
	    if CurrIOHandler == undefined ->
		    set_io_buffering({Ref,self()});
	       true ->
		    ok
	    end,
	    {Mod,Func} = skip_case(AutoOrUser, Ref, 0, Case, Comment,
				   true, SkipMode),
	    ConfData = {Mod,{Func,get_name(SkipMode)},Comment},
	    test_server_sup:framework_call(report, [ReportTag,ConfData]),
	    run_test_cases_loop(Cases, Config, TimetrapData,
				[conf(Ref,[])|Mode], Status)
    end;

run_test_cases_loop([{auto_skip_case,{Case,Comment},SkipMode}|Cases],
		    Config, TimetrapData, Mode, Status) ->
    {Mod,Func} = skip_case(auto, undefined, get(test_server_case_num)+1,
			   Case, Comment, is_io_buffered(), SkipMode),
    test_server_sup:framework_call(report, [tc_auto_skip,
					    {Mod,{Func,get_name(SkipMode)},
					     Comment}]),
    run_test_cases_loop(Cases, Config, TimetrapData, Mode,
			update_status(skipped, Mod, Func, Status));

run_test_cases_loop([{skip_case,{{Mod,all}=Case,Comment},SkipMode}|Cases],
		    Config, TimetrapData, Mode, Status) ->
    _ = skip_case(user, undefined, 0, Case, Comment, false, SkipMode),
    test_server_sup:framework_call(report, [tc_user_skip,
					    {Mod,{all,get_name(SkipMode)},
					     Comment}]),
    run_test_cases_loop(Cases, Config, TimetrapData, Mode, Status);

run_test_cases_loop([{skip_case,{Case,Comment},SkipMode}|Cases],
		    Config, TimetrapData, Mode, Status) ->
    {Mod,Func} = skip_case(user, undefined, get(test_server_case_num)+1,
			   Case, Comment, is_io_buffered(), SkipMode),
    test_server_sup:framework_call(report, [tc_user_skip,
					    {Mod,{Func,get_name(SkipMode)},
					     Comment}]),
    run_test_cases_loop(Cases, Config, TimetrapData, Mode,
			update_status(skipped, Mod, Func, Status));

%% a start *or* end conf case, wrapping test cases or other conf cases
run_test_cases_loop([{conf,Ref,Props,{Mod,Func}}|_Cases]=Cs0,
		    Config, TimetrapData, Mode0, Status) ->
    CurrIOHandler = get(test_server_common_io_handler),
    %% check and update the mode for test case execution and io msg handling
    {StartConf,Mode,IOHandler,ConfTime,Status1} =
	case {curr_ref(Mode0),check_props(parallel, Mode0)} of
	    {Ref,Ref} ->
		case check_props(parallel, tl(Mode0)) of
		    false ->
			%% this is an end conf for a top level parallel group,
			%% collect results from the test case processes
			%% and calc total time
			OkSkipFail = handle_test_case_io_and_status(),
			ok = file:set_cwd(filename:dirname(get(test_server_dir))),
			After = ?now,
			Before = get(test_server_parallel_start_time),
			Elapsed = timer:now_diff(After, Before)/1000000,
			put(test_server_total_time, Elapsed),
			{false,tl(Mode0),undefined,Elapsed,
			 update_status(Ref, OkSkipFail, Status)};
		    _ ->
			%% this is an end conf for a parallel group nested under a
			%% parallel group (io buffering is active)
			OkSkipFail = wait_for_cases(Ref),
			queue_test_case_io(Ref, self(), 0, Mod, Func),
			Elapsed = timer:now_diff(?now, conf_start(Ref, Mode0))/1000000,
			case CurrIOHandler of
			    {Ref,_} ->
				%% current_io_handler was set by start conf of this
				%% group, so we can unset it after this case (no
				%% more io from main process needs to be buffered)
				{false,tl(Mode0),undefined,Elapsed,
				 update_status(Ref, OkSkipFail, Status)};
			    _ ->
				{false,tl(Mode0),CurrIOHandler,Elapsed,
				 update_status(Ref, OkSkipFail, Status)}
			end
		end;
	    {Ref,false} ->
		%% this is an end conf for a non-parallel group that's not
		%% nested under a parallel group, so no need to buffer io
		{false,tl(Mode0),undefined,
		 timer:now_diff(?now, conf_start(Ref, Mode0))/1000000, Status};
	    {Ref,_} ->
		%% this is an end conf for a non-parallel group nested under
		%% a parallel group (io buffering is active)
		queue_test_case_io(Ref, self(), 0, Mod, Func),
		Elapsed = timer:now_diff(?now, conf_start(Ref, Mode0))/1000000,
		case CurrIOHandler of
		    {Ref,_} ->
			%% current_io_handler was set by start conf of this
			%% group, so we can unset it after this case (no
			%% more io from main process needs to be buffered)
			{false,tl(Mode0),undefined,Elapsed,Status};
		    _ ->
			{false,tl(Mode0),CurrIOHandler,Elapsed,Status}
		end;
	    {_,false} ->
		%% this is a start conf for a group which is not nested under a
		%% parallel group, check if this case starts a new parallel group
		case lists:member(parallel, Props) of
		    true ->
			%% prepare for execution of parallel group
			put(test_server_parallel_start_time, ?now),
			put(test_server_queued_io, []);
		    false ->
			ok
		end,
		{true,[conf(Ref,Props)|Mode0],undefined,0,Status};
	    {_,_Ref0} ->
		%% this is a start conf for a group nested under a parallel group, the
		%% parallel_start_time and parallel_test_cases values have already been set
		queue_test_case_io(Ref, self(), 0, Mod, Func),
		%% if this is the first nested group under a parallel group, io
		%% buffering must be activated
		IOHandler1 = if CurrIOHandler == undefined ->
				     IOH = {Ref,self()},
				     set_io_buffering(IOH),
				     IOH;
				true ->
				     CurrIOHandler
			     end,
		{true,[conf(Ref,Props)|Mode0],IOHandler1,0,Status}
	end,

    %% if this is a start conf we check if cases should be shuffled
    {[_Conf|Cases1]=Cs1,Shuffle} =
	if StartConf ->
		case get_shuffle(Props) of
		    undefined ->
			{Cs0,undefined};
		    {_,repeated} ->
			%% if group is repeated, a new seed should not be set every
			%% turn - last one is saved in dictionary
			CurrSeed = get(test_server_curr_random_seed),
			{shuffle_cases(Ref, Cs0, CurrSeed),{shuffle,CurrSeed}};
		    {_,Seed} ->
			UseSeed=
			    %% Determine which seed to use by:
			    %% 1. check the TS_RANDOM_SEED env variable
			    %% 2. check random_seed in process state
			    %% 3. use value provided with shuffle option
			    %% 4. use timestamp() values for seed
			    case os:getenv("TS_RANDOM_SEED") of
				Undef when Undef == false ; Undef == "undefined" ->
				    case get(test_server_random_seed) of
					undefined -> Seed;
					TSRS -> TSRS
				    end;
				NumStr ->
				    %% Ex: "123 456 789" or "123,456,789" -> {123,456,789}
				    list_to_tuple([list_to_integer(NS) ||
						   NS <- string:lexemes(NumStr, [$ ,$:,$,])])
			    end,
			{shuffle_cases(Ref, Cs0, UseSeed),{shuffle,UseSeed}}
		end;
	   not StartConf ->
		{Cs0,undefined}
	end,

    %% if this is a start conf we check if Props specifies repeat and if so
    %% we copy the group and carry the copy until the end conf where we
    %% decide to perform the repetition or not
    {Repeating,Status2,Cases,ReportRepeatStop} =
	if StartConf ->
		case get_repeat(Props) of
		    undefined ->
			%% we *must* have a status entry for every conf since we
			%% will continously update status with test case results
			%% without knowing the Ref (but update hd(Status))
			{false,new_status(Ref, Status1),Cases1,?void_fun};
		    {_RepType,N} when N =< 1 ->
			{false,new_status(Ref, Status1),Cases1,?void_fun};
		    _ ->
			{Copied,_} = copy_cases(Ref, make_ref(), Cs1),
			{true,new_status(Ref, Copied, Status1),Cases1,?void_fun}
		end;
	   not StartConf ->
		RepVal = get_repeat(get_props(Mode0)),
		ReportStop =
		    fun() ->
			    print(minor, "~n*** Stopping repeat operation ~w", [RepVal]),
			    print(1, "Stopping repeat operation ~w", [RepVal])
		    end,
		CopiedCases = get_copied_cases(Status1),
		EndStatus = delete_status(Ref, Status1),
		%% check in Mode0 if this is a repeat conf
		case RepVal of
		    undefined ->
			{false,EndStatus,Cases1,?void_fun};
		    {_RepType,N} when N =< 1 ->
			{false,EndStatus,Cases1,?void_fun};
		    {repeat,_} ->
			{true,EndStatus,CopiedCases++Cases1,?void_fun};
		    {repeat_until_all_ok,_} ->
			{RestCs,Fun} = case get_tc_results(Status1) of
					   {_,_,[]} ->
					       {Cases1,ReportStop};
					   _ ->
					       {CopiedCases++Cases1,?void_fun}
				       end,
			{true,EndStatus,RestCs,Fun};
		    {repeat_until_any_ok,_} ->
			{RestCs,Fun} = case get_tc_results(Status1) of
					   {Ok,_,_Fails} when length(Ok) > 0 ->
					       {Cases1,ReportStop};
					   _ ->
					       {CopiedCases++Cases1,?void_fun}
				       end,
			{true,EndStatus,RestCs,Fun};
		    {repeat_until_any_fail,_} ->
			{RestCs,Fun} = case get_tc_results(Status1) of
					   {_,_,Fails} when length(Fails) > 0 ->
					       {Cases1,ReportStop};
					   _ ->
					       {CopiedCases++Cases1,?void_fun}
				 end,
			{true,EndStatus,RestCs,Fun};
		    {repeat_until_all_fail,_} ->
			{RestCs,Fun} = case get_tc_results(Status1) of
					   {[],_,_} ->
					       {Cases1,ReportStop};
					   _ ->
					       {CopiedCases++Cases1,?void_fun}
				       end,
			{true,EndStatus,RestCs,Fun}
		end
	end,

    ReportAbortRepeat = fun(What) when Repeating ->
				print(minor, "~n*** Aborting repeat operation "
				      "(configuration case ~w)", [What]),
				print(1, "Aborting repeat operation "
				      "(configuration case ~w)", [What]);
			   (_) -> ok
			end,
    CfgProps = if StartConf ->
		       if Shuffle == undefined ->
			       [{tc_group_properties,Props}];
			  true ->
			       [{tc_group_properties,
				 [Shuffle|delete_shuffle(Props)]}]
		       end;
		  not StartConf ->
		       {TcOk,TcSkip,TcFail} = get_tc_results(Status1),
		       [{tc_group_properties,get_props(Mode0)},
			{tc_group_result,[{ok,TcOk},
					  {skipped,TcSkip},
					  {failed,TcFail}]}]
	       end,

    SuiteName = proplists:get_value(suite, Props),
    case get(test_server_create_priv_dir) of
	auto_per_run ->				% use common priv_dir
	    TSDirs = [{priv_dir,get(test_server_priv_dir)},
		      {data_dir,get_data_dir(Mod, SuiteName)}];    
	_ ->
	    TSDirs = [{data_dir,get_data_dir(Mod, SuiteName)}]
    end,

    ActualCfg = 
	if not StartConf ->
		update_config(hd(Config), TSDirs ++ CfgProps);
	   true ->
		GroupPath = lists:flatmap(fun({_Ref,[],_T}) -> [];
					     ({_Ref,GrProps,_T}) -> [GrProps]
					  end, Mode0),
		update_config(hd(Config), 
			      TSDirs ++ [{tc_group_path,GroupPath} | CfgProps])
	end,

    CurrMode = curr_mode(Ref, Mode0, Mode),
    ConfCaseResult = run_test_case(Ref, 0, Mod, Func, [ActualCfg], skip_init,
				   TimetrapData, CurrMode),

    case ConfCaseResult of
	{_,NewCfg,_} when Func == init_per_suite, is_list(NewCfg) ->
	    %% check that init_per_suite returned data on correct format
	    case lists:filter(fun({_,_}) -> false;
				 (_) -> true end, NewCfg) of
		[] ->
		    set_io_buffering(IOHandler),
		    stop_minor_log_file(),
		    run_test_cases_loop(Cases, [NewCfg|Config],
					TimetrapData, Mode, Status2);
		Bad ->
		    print(minor,
			  "~n*** ~tw returned bad elements in Config: ~tp.~n",
			  [Func,Bad]),
		    Reason = {failed,{Mod,init_per_suite,bad_return}},
		    Cases2 = skip_cases_upto(Ref, Cases, Reason, conf, CurrMode,
					     auto_skip_case),
		    set_io_buffering(IOHandler),
		    stop_minor_log_file(),
		    run_test_cases_loop(Cases2, Config, TimetrapData, Mode,
					delete_status(Ref, Status2))
	    end;
	{_,NewCfg,_} when StartConf, is_list(NewCfg) ->
	    print_conf_time(ConfTime),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases, [NewCfg|Config], TimetrapData, Mode, Status2);
	{_,{framework_error,{FwMod,FwFunc},Reason},_} ->
	    print(minor, "~n*** ~w failed in ~tw. Reason: ~tp~n",
		  [FwMod,FwFunc,Reason]),
	    print(1, "~w failed in ~tw. Reason: ~tp~n", [FwMod,FwFunc,Reason]),
	    exit(framework_error);
	{_,Fail,_} when element(1,Fail) == 'EXIT';
			element(1,Fail) == timetrap_timeout;
			element(1,Fail) == user_timetrap_error;
			element(1,Fail) == failed ->
	    {Cases2,Config1,Status3} =
		if StartConf ->
			ReportAbortRepeat(failed),
			print(minor, "~n*** ~tw failed.~n"
			      "    Skipping all cases.", [Func]),
			Reason = {failed,{Mod,Func,Fail}},
			{skip_cases_upto(Ref, Cases, Reason, conf, CurrMode,
					 auto_skip_case),
			 Config,
			 update_status(failed, group_result, get_name(Mode),
				       delete_status(Ref, Status2))};
		   not StartConf ->
			ReportRepeatStop(),
			print_conf_time(ConfTime),
			{Cases,tl(Config),delete_status(Ref, Status2)}
		end,
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases2, Config1, TimetrapData, Mode, Status3);

	{_,{auto_skip,SkipReason},_} ->
	    %% this case can only happen if the framework (not the user)
	    %% decides to skip execution of a conf function
	    {Cases2,Config1,Status3} =
		if StartConf ->
			ReportAbortRepeat(auto_skipped),
			print(minor, "~n*** ~tw auto skipped.~n"
			      "    Skipping all cases.", [Func]),
			{skip_cases_upto(Ref, Cases, SkipReason, conf, CurrMode,
					 auto_skip_case),
			 Config,
			 delete_status(Ref, Status2)};
		   not StartConf ->
			ReportRepeatStop(),
			print_conf_time(ConfTime),
			{Cases,tl(Config),delete_status(Ref, Status2)}
		end,
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases2, Config1, TimetrapData, Mode, Status3);

	{_,{Skip,Reason},_} when StartConf and ((Skip==skip) or (Skip==skipped)) ->
	    ReportAbortRepeat(skipped),
	    print(minor, "~n*** ~tw skipped.~n"
		  "    Skipping all cases.", [Func]),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(skip_cases_upto(Ref, Cases, Reason, conf,
						CurrMode, skip_case),
				[hd(Config)|Config], TimetrapData, Mode,
				delete_status(Ref, Status2));
	{_,{skip_and_save,Reason,_SavedConfig},_} when StartConf ->
	    ReportAbortRepeat(skipped),
	    print(minor, "~n*** ~tw skipped.~n"
		  "    Skipping all cases.", [Func]),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(skip_cases_upto(Ref, Cases, Reason, conf,
						CurrMode, skip_case),
				[hd(Config)|Config], TimetrapData, Mode,
				delete_status(Ref, Status2));
	{_,_Other,_} when Func == init_per_suite ->
	    print(minor, "~n*** init_per_suite failed to return a Config list.~n", []),
	    Reason = {failed,{Mod,init_per_suite,bad_return}},
	    Cases2 = skip_cases_upto(Ref, Cases, Reason, conf, CurrMode,
				     auto_skip_case),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases2, Config, TimetrapData, Mode,
				delete_status(Ref, Status2));
	{_,_Other,_} when StartConf ->
	    print_conf_time(ConfTime),
	    set_io_buffering(IOHandler),
	    ReportRepeatStop(),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases, [hd(Config)|Config], TimetrapData,
				Mode, Status2);
	{_,_EndConfRetVal,Opts} ->
	    %% Check if return_group_result is set (ok, skipped or failed) and
	    %% if so:
	    %% 1) *If* the parent group is a sequence, skip all proceeding tests
	    %%    in that group.
	    %% 2) Return the value to the group "above" so that result may be
	    %%    used for evaluating a 'repeat_until_*' property.
	    GrName = get_name(Mode0, Func),
	    {Cases2,Status3} =
		case lists:keysearch(return_group_result, 1, Opts) of
		    {value,{_,failed}} ->
			case {curr_ref(Mode),check_prop(sequence, Mode)} of
			    {ParentRef,ParentRef} ->
				Reason = {group_result,GrName,failed},
				{skip_cases_upto(ParentRef, Cases, Reason, tc,
						 Mode, auto_skip_case),
				 update_status(failed, group_result, GrName,
					       delete_status(Ref, Status2))};
			    _ ->
				{Cases,update_status(failed, group_result, GrName,
						     delete_status(Ref, Status2))}
			end;
		    {value,{_,GroupResult}} ->
			{Cases,update_status(GroupResult, group_result, GrName,
					     delete_status(Ref, Status2))};
		    false ->
			{Cases,update_status(ok, group_result, GrName,
					     delete_status(Ref, Status2))}
		end,
	    print_conf_time(ConfTime),
	    ReportRepeatStop(),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases2, tl(Config), TimetrapData,
				Mode, Status3)
    end;

run_test_cases_loop([{make,Ref,{Mod,Func,Args}}|Cases0], Config, TimetrapData,
		    Mode, Status) ->
    case run_test_case(Ref, 0, Mod, Func, Args, skip_init, TimetrapData) of
	{_,Why={'EXIT',_},_} ->
	    print(minor, "~n*** ~tw failed.~n"
 		  "    Skipping all cases.", [Func]),
	    Reason = {failed,{Mod,Func,Why}},
	    Cases = skip_cases_upto(Ref, Cases0, Reason, conf, Mode,
				    auto_skip_case),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases, Config, TimetrapData, Mode, Status);
	{_,_Whatever,_} ->
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases0, Config, TimetrapData, Mode, Status)
    end;

run_test_cases_loop([{conf,_Ref,_Props,_X}=Conf|_Cases0],
		    Config, _TimetrapData, _Mode, _Status) ->
    erlang:error(badarg, [Conf,Config]);

run_test_cases_loop([{Mod,Case}|Cases], Config, TimetrapData, Mode, Status) ->
    ActualCfg =
	case get(test_server_create_priv_dir) of
	    auto_per_run ->
		update_config(hd(Config), [{priv_dir,get(test_server_priv_dir)},
					   {data_dir,get_data_dir(Mod)}]);
	    _ ->
		update_config(hd(Config), [{data_dir,get_data_dir(Mod)}])
	end,
    run_test_cases_loop([{Mod,Case,[ActualCfg]}|Cases], Config,
			TimetrapData, Mode, Status);

run_test_cases_loop([{Mod,Func,Args}|Cases], Config, TimetrapData, Mode, Status) ->
    {Num,RunInit} =
	case FwMod = get_fw_mod(?MODULE) of
	    Mod when Func == error_in_suite ->
		{-1,skip_init};
	    _ ->
		{put(test_server_case_num, get(test_server_case_num)+1),
		 run_init}
	end,

    %% check the current execution mode and save info about the case if
    %% detected that printouts to common log files is handled later

    case check_prop(parallel, Mode) =:= false andalso is_io_buffered() of
	true ->
	    %% sequential test case nested in a parallel group;
	    %% io is buffered, so we must queue this test case
	    queue_test_case_io(undefined, self(), Num+1, Mod, Func);
	false ->
	    ok
    end,

    case run_test_case(undefined, Num+1, Mod, Func, Args,
		       RunInit, TimetrapData, Mode) of
	%% callback to framework module failed, exit immediately
	{_,{framework_error,{FwMod,FwFunc},Reason},_} ->
	    print(minor, "~n*** ~w failed in ~tw. Reason: ~tp~n",
		  [FwMod,FwFunc,Reason]),
	    print(1, "~w failed in ~tw. Reason: ~tp~n", [FwMod,FwFunc,Reason]),
	    stop_minor_log_file(),
	    exit(framework_error);
	%% sequential execution of test case finished
	{Time,RetVal,_} ->
            RetTag =
                if is_tuple(RetVal) -> element(1,RetVal);
                   true -> undefined
                end,
	    {Failed,Status1} =
                case RetTag of
                    Skip when Skip==skip; Skip==skipped ->
                        {false,update_status(skipped, Mod, Func, Status)};
                    Fail when Fail=='EXIT'; Fail==failed ->
                        {true,update_status(failed, Mod, Func, Status)};
                    _ when Time==died, RetVal=/=ok ->
                        {true,update_status(failed, Mod, Func, Status)};
                    _ ->
                        {false,update_status(ok, Mod, Func, Status)}
                end,
	    case check_prop(sequence, Mode) of
		false ->
		    stop_minor_log_file(),
		    run_test_cases_loop(Cases, Config, TimetrapData, Mode, Status1);
		Ref ->
		    %% the case is in a sequence; we must check the result and
		    %% determine if the following cases should run or be skipped
		    if not Failed ->	      % proceed with next case
			    stop_minor_log_file(),
			    run_test_cases_loop(Cases, Config, TimetrapData, Mode, Status1);
		       true ->	              % skip rest of cases in sequence
			    print(minor, "~n*** ~tw failed.~n"
				  "    Skipping all other cases in sequence.",
				  [Func]),
			    Reason = {failed,{Mod,Func}},
			    Cases2 = skip_cases_upto(Ref, Cases, Reason, tc,
						     Mode, auto_skip_case),
			    stop_minor_log_file(),
			    run_test_cases_loop(Cases2, Config, TimetrapData, Mode, Status1)
		    end
	    end;
	%% the test case is being executed in parallel with the main process (and
	%% other test cases) and Pid is the dedicated process executing the case
	Pid ->
	    %% io from Pid will be buffered by the test_server_io process and
	    %% handled later, so we have to save info about the case
	    queue_test_case_io(undefined, Pid, Num+1, Mod, Func),
	    run_test_cases_loop(Cases, Config, TimetrapData, Mode, Status)
    end;

%% TestSpec processing finished
run_test_cases_loop([], _Config, _TimetrapData, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% various help functions

new_status(Ref, Status) ->
    [{Ref,{{[],[],[]},[]}} | Status].

new_status(Ref, CopiedCases, Status) ->
    [{Ref,{{[],[],[]},CopiedCases}} | Status].

delete_status(Ref, Status) ->
    lists:keydelete(Ref, 1, Status).

update_status(ok, Mod, Func, [{Ref,{{Ok,Skip,Fail},Cs}} | Status]) ->
    [{Ref,{{Ok++[{Mod,Func}],Skip,Fail},Cs}} | Status];

update_status(skipped, Mod, Func, [{Ref,{{Ok,Skip,Fail},Cs}} | Status]) ->
    [{Ref,{{Ok,Skip++[{Mod,Func}],Fail},Cs}} | Status];

update_status(failed, Mod, Func, [{Ref,{{Ok,Skip,Fail},Cs}} | Status]) ->
    [{Ref,{{Ok,Skip,Fail++[{Mod,Func}]},Cs}} | Status];

update_status(_, _, _, []) ->
    [].

update_status(Ref, {Ok,Skip,Fail}, [{Ref,{{Ok0,Skip0,Fail0},Cs}} | Status]) ->
    [{Ref,{{Ok0++Ok,Skip0++Skip,Fail0++Fail},Cs}} | Status].

get_copied_cases([{_,{_,Cases}} | _Status]) ->
    Cases.

get_tc_results([{_,{OkSkipFail,_}} | _Status]) ->
    OkSkipFail;
get_tc_results([]) ->		      % in case init_per_suite crashed
    {[],[],[]}.

conf(Ref, Props) ->
    {Ref,Props,?now}.

curr_ref([{Ref,_Props,_}|_]) ->
    Ref;
curr_ref([]) ->
    undefined.

curr_mode(Ref, Mode0, Mode1) ->
    case curr_ref(Mode1) of
	Ref -> Mode1;
	_   -> Mode0
    end.

get_props([{_,Props,_} | _]) ->
    Props;
get_props([]) ->
    [].

check_prop(_Attrib, []) ->
    false;
check_prop(Attrib, [{Ref,Props,_}|_]) ->
    case lists:member(Attrib, Props) of
	true -> Ref;
	false -> false
    end.

check_props(Attrib, Mode) ->
    case [R || {R,Ps,_} <- Mode, lists:member(Attrib, Ps)] of
	[] -> false;
	[Ref|_] -> Ref
    end.

get_name(Mode, Def) ->
    case get_name(Mode) of
	undefined -> Def;
	Name      -> Name
    end.

get_name([{_Ref,Props,_}|_]) ->
    proplists:get_value(name, Props);
get_name([]) ->
    undefined.

conf_start(Ref, Mode) ->
    case lists:keysearch(Ref, 1, Mode) of
	{value,{_,_,T}} -> T;
	false -> 0
    end.


get_data_dir(Mod) ->
    get_data_dir(Mod, undefined).

get_data_dir(Mod, Suite) ->
    UseMod = if Suite == undefined -> Mod;
		true               -> Suite
	     end,
    case code:which(UseMod) of
	non_existing ->
	    print(12, "The module ~w is not loaded", [Mod]),
	    [];
	cover_compiled ->
	    MainCoverNode = cover:get_main_node(),
	    {file,File} = rpc:call(MainCoverNode,cover,is_compiled,[UseMod]),
	    do_get_data_dir(UseMod,File);
	FullPath ->
	    do_get_data_dir(UseMod,FullPath)
    end.

do_get_data_dir(Mod,File) ->
    filename:dirname(File) ++ "/" ++ atom_to_list(Mod) ++ ?data_dir_suffix.

print_conf_time(0) ->
    ok;
print_conf_time(ConfTime) ->
    print(major, "=group_time    ~.3fs", [ConfTime]),
    print(minor, "~n=== Total execution time of group: ~.3fs~n", [ConfTime]).

print_props([]) ->
    ok;
print_props(Props) ->
    print(major, "=group_props   ~tp", [Props]),
    print(minor, "Group properties: ~tp~n", [Props]).

%% repeat N times:                                  {repeat,N}
%% repeat N times or until all successful:          {repeat_until_all_ok,N}
%% repeat N times or until at least one successful: {repeat_until_any_ok,N}
%% repeat N times or until at least one case fails: {repeat_until_any_fail,N}
%% repeat N times or until all fails:               {repeat_until_all_fail,N}
%% N      = integer() | forever
get_repeat(Props) ->
    get_prop([repeat,repeat_until_all_ok,repeat_until_any_ok,
	      repeat_until_any_fail,repeat_until_all_fail], forever, Props).

update_repeat(Props) ->
    case get_repeat(Props) of
	undefined ->
	    Props;
	{RepType,N} ->
	    Props1 =
		if N == forever ->
			[{RepType,N}|lists:keydelete(RepType, 1, Props)];
		   N < 3 ->
			lists:keydelete(RepType, 1, Props);
		   N >= 3 ->
			[{RepType,N-1}|lists:keydelete(RepType, 1, Props)]
		end,
	    %% if shuffle is used in combination with repeat, a new
	    %% seed shouldn't be set every new turn
	    case get_shuffle(Props1) of
		undefined ->
		    Props1;
		_ ->
		    [{shuffle,repeated}|delete_shuffle(Props1)]
	    end
    end.

get_shuffle(Props) ->
    get_prop([shuffle], ?now, Props).

delete_shuffle(Props) ->
    delete_prop([shuffle], Props).

%% Return {Item,Value} if found, else if Item alone
%% is found, return {Item,Default}
get_prop([Item|Items], Default, Props) ->
    case lists:keysearch(Item, 1, Props) of
	{value,R} ->
	    R;
	false ->
	    case lists:member(Item, Props) of
		true ->
		    {Item,Default};
		false ->
		    get_prop(Items, Default, Props)
	    end
    end;
get_prop([], _Def, _Props) ->
    undefined.

delete_prop([Item|Items], Props) ->
    Props1 = lists:delete(Item, lists:keydelete(Item, 1, Props)),
    delete_prop(Items, Props1);
delete_prop([], Props) ->
    Props.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% shuffle_cases(Ref, Cases, Seed) -> Cases1
%%
%% Shuffles the order of Cases.

shuffle_cases(Ref, Cases, undefined) ->
    shuffle_cases(Ref, Cases, rand:seed_s(exsplus));

shuffle_cases(Ref, [{conf,Ref,_,_}=Start | Cases], Seed0) ->
    {N,CasesToShuffle,Rest} = cases_to_shuffle(Ref, Cases),
    Seed = case Seed0 of
	       {X,Y,Z} when is_integer(X+Y+Z) ->
		   rand:seed(exsplus, Seed0);
	       _ ->
		   Seed0
	   end,
    ShuffledCases = random_order(N, rand:uniform_s(N, Seed), CasesToShuffle, []),
    [Start|ShuffledCases] ++ Rest.

cases_to_shuffle(Ref, Cases) ->
    cases_to_shuffle(Ref, Cases, 1, []).

cases_to_shuffle(Ref, [{conf,Ref,_,_} | _]=Cs, N, Ix) ->          % end
    {N-1,Ix,Cs};
cases_to_shuffle(Ref, [{skip_case,{_,Ref,_,_},_} | _]=Cs, N, Ix) -> % end
    {N-1,Ix,Cs};

cases_to_shuffle(Ref, [{conf,Ref1,_,_}=C | Cs], N, Ix) ->          % nested group
    {Cs1,Rest} = get_subcases(Ref1, Cs, []),
    cases_to_shuffle(Ref, Rest, N+1, [{N,[C|Cs1]} | Ix]);
cases_to_shuffle(Ref, [{skip_case,{_,Ref1,_,_},_}=C | Cs], N, Ix) -> % nested group
    {Cs1,Rest} = get_subcases(Ref1, Cs, []),
    cases_to_shuffle(Ref, Rest, N+1, [{N,[C|Cs1]} | Ix]);

cases_to_shuffle(Ref, [C | Cs], N, Ix) ->
    cases_to_shuffle(Ref, Cs, N+1, [{N,[C]} | Ix]).

get_subcases(SubRef, [{conf,SubRef,_,_}=C | Cs], SubCs) ->
    {lists:reverse([C|SubCs]),Cs};
get_subcases(SubRef, [{skip_case,{_,SubRef,_,_},_}=C | Cs], SubCs) ->
    {lists:reverse([C|SubCs]),Cs};
get_subcases(SubRef, [C|Cs], SubCs) ->
    get_subcases(SubRef, Cs, [C|SubCs]).

random_order(1, {_Pos,Seed}, [{_Ix,CaseOrGroup}], Shuffled) ->
    %% save current seed to be used if test cases are repeated
    put(test_server_curr_random_seed, Seed),
    Shuffled++CaseOrGroup;
random_order(N, {Pos,NewSeed}, IxCases, Shuffled) ->
    {First,[{_Ix,CaseOrGroup}|Rest]} = lists:split(Pos-1, IxCases),
    random_order(N-1, rand:uniform_s(N-1, NewSeed),
		 First++Rest, Shuffled++CaseOrGroup).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% skip_case(Type, Ref, CaseNum, Case, Comment, SendSync) -> {Mod,Func}
%%
%% Prints info about a skipped case in the major and html log files.
%% SendSync determines if start and finished messages must be sent so
%% that the printouts can be buffered and handled in order with io from
%% parallel processes.
skip_case(Type, Ref, CaseNum, Case, Comment, SendSync, Mode) ->
    MF = {Mod,Func} = case Case of
			  {M,F,_A} -> {M,F};
			  {M,F} -> {M,F}
		      end,
    if SendSync ->
	    queue_test_case_io(Ref, self(), CaseNum, Mod, Func),
	    self() ! {started,Ref,self(),CaseNum,Mod,Func},
	    test_server_io:start_transaction(),
	    skip_case1(Type, CaseNum, Mod, Func, Comment, Mode),
	    test_server_io:end_transaction(),
	    self() ! {finished,Ref,self(),CaseNum,Mod,Func,skipped,{0,skipped,[]}};
       not SendSync ->
	    skip_case1(Type, CaseNum, Mod, Func, Comment, Mode)
    end,
    MF.

skip_case1(Type, CaseNum, Mod, Func, Comment, Mode) ->
    {{Col0,Col1},_} = get_font_style((CaseNum > 0), Mode),
    ResultCol = if Type == auto -> ?auto_skip_color;
		   Type == user -> ?user_skip_color
		end,
    print(major, "~n=case          ~w:~tw", [Mod,Func]),
    GroupName =	case get_name(Mode) of
		    undefined ->
			"";
		    GrName ->
			GrName1 = cast_to_list(GrName),
			print(major, "=group_props   ~tp", [[{name,GrName1}]]),
			GrName1
		end,
    print(major, "=started       ~s", [lists:flatten(timestamp_get(""))]),
    Comment1 = reason_to_string(Comment),
    if Type == auto ->
	    print(major, "=result        auto_skipped: ~ts", [Comment1]);
       Type == user ->
	    print(major, "=result        skipped: ~ts", [Comment1])
    end,
    if CaseNum == 0 ->
	    print(2,"*** Skipping ~tw ***", [{Mod,Func}]);
       true ->
	    print(2,"*** Skipping test case #~w ~tw ***", [CaseNum,{Mod,Func}])
    end,
    TR = xhtml("<tr valign=\"top\">", ["<tr class=\"",odd_or_even(),"\">"]),	       
    GroupName =	case get_name(Mode) of
		    undefined -> "";
		    Name      -> cast_to_list(Name)
		end,
    print(html,
	  TR ++ "<td>" ++ Col0 ++ "~ts" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "~w" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "~ts" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "~tw" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "< >" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "0.000s" ++ Col1 ++ "</td>"
	  "<td><font color=\"~ts\">SKIPPED</font></td>"
	  "<td>~ts</td></tr>\n",
	  [num2str(CaseNum),fw_name(Mod),GroupName,Func,ResultCol,Comment1]),

    if CaseNum > 0 ->
	    {US,AS} = get(test_server_skipped),
	    case Type of
		user -> put(test_server_skipped, {US+1,AS});
		auto -> put(test_server_skipped, {US,AS+1})
	    end,
	    put(test_server_case_num, CaseNum);
       true ->					% conf
	    ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% skip_cases_upto(Ref, Cases, Reason, Origin, Mode, SkipType) -> Cases1
%%
%% SkipType = skip_case | auto_skip_case
%% Mark all cases tagged with Ref as skipped.

skip_cases_upto(Ref, Cases, Reason, Origin, Mode, SkipType) ->    
    {_,Modified,Rest} =
	modify_cases_upto(Ref, {skip,Reason,Origin,Mode,SkipType}, Cases),
    Modified++Rest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% copy_cases(OrigRef, NewRef, Cases) -> Cases1
%%
%% Copy the test cases marked with OrigRef and tag the copies with NewRef.
%% The start conf case copy will also get its repeat property updated.

copy_cases(OrigRef, NewRef, Cases) ->
    {Original,Altered,Rest} = modify_cases_upto(OrigRef, {copy,NewRef}, Cases),
    {Altered,Original++Altered++Rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% modify_cases_upto(Ref, ModOp, Cases) -> {Original,Altered,Remaining}
%%
%% ModOp = {skip,Reason,Origin,Mode} | {copy,NewRef}
%% Origin = conf | tc
%%
%% Modifies Cases according to ModOp and returns the original elements,
%% the modified versions of these elements and the remaining (untouched)
%% cases.

modify_cases_upto(Ref, ModOp, Cases) ->
    {Original,Altered,Rest} = modify_cases_upto(Ref, ModOp, Cases, [], []),
    {lists:reverse(Original),lists:reverse(Altered),Rest}.

%% first case of a copy operation is the start conf
modify_cases_upto(Ref, {copy,NewRef}=Op, [{conf,Ref,Props,MF}=C|T], Orig, Alt) ->
    modify_cases_upto(Ref, Op, T, [C|Orig], [{conf,NewRef,update_repeat(Props),MF}|Alt]);

modify_cases_upto(Ref, ModOp, Cases, Orig, Alt) ->
    %% we need to check if there's an end conf case with the
    %% same ref in the list, if not, this *is* an end conf case
    case lists:any(fun({_,R,_,_}) when R == Ref -> true;
		      ({_,R,_})   when R == Ref -> true;
		      ({skip_case,{_,R,_,_},_}) when R == Ref -> true;
		      ({skip_case,{_,R,_,_}}) when R == Ref -> true;
		      (_) -> false
		   end, Cases) of
	true ->
	    modify_cases_upto1(Ref, ModOp, Cases, Orig, Alt);
	false ->
	    {[],[],Cases}
    end.

%% next case is a conf with same ref, must be end conf = we're done
modify_cases_upto1(Ref, {skip,Reason,conf,Mode,skip_case},
		   [{conf,Ref,_Props,MF}|T], Orig, Alt) ->
    {Orig,[{skip_case,{conf,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {skip,Reason,conf,Mode,auto_skip_case},
		   [{conf,Ref,_Props,MF}|T], Orig, Alt) ->
    {Orig,[{auto_skip_case,{conf,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {copy,NewRef}, [{conf,Ref,Props,MF}=C|T], Orig, Alt) ->
    {[C|Orig],[{conf,NewRef,update_repeat(Props),MF}|Alt],T};

%% we've skipped all remaining cases in a sequence
modify_cases_upto1(Ref, {skip,_,tc,_,_},
		   [{conf,Ref,_Props,_MF}|_]=Cs, Orig, Alt) ->
    {Orig,Alt,Cs};

%% next is a make case
modify_cases_upto1(Ref, {skip,Reason,_,Mode,SkipType},
		   [{make,Ref,MF}|T], Orig, Alt) ->
    {Orig,[{SkipType,{make,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {copy,NewRef}, [{make,Ref,MF}=M|T], Orig, Alt) ->
    {[M|Orig],[{make,NewRef,MF}|Alt],T};

%% next case is a user skipped end conf with the same ref = we're done
modify_cases_upto1(Ref, {skip,Reason,_,Mode,SkipType},
		   [{skip_case,{Type,Ref,MF,_Cmt},_}|T], Orig, Alt) ->
    {Orig,[{SkipType,{Type,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {skip,Reason,_,Mode,SkipType},
		   [{skip_case,{Type,Ref,MF,_Cmt}}|T], Orig, Alt) ->
    {Orig,[{SkipType,{Type,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {copy,NewRef},
		   [{skip_case,{Type,Ref,MF,Cmt},Mode}=C|T], Orig, Alt) ->
    {[C|Orig],[{skip_case,{Type,NewRef,MF,Cmt},Mode}|Alt],T};
modify_cases_upto1(Ref, {copy,NewRef},
		   [{skip_case,{Type,Ref,MF,Cmt}}=C|T], Orig, Alt) ->
    {[C|Orig],[{skip_case,{Type,NewRef,MF,Cmt}}|Alt],T};

%% next is a skip_case, could be one test case or 'all' in suite, we must proceed
modify_cases_upto1(Ref, ModOp, [{skip_case,{_F,_Cmt},_Mode}=MF|T], Orig, Alt) ->
    modify_cases_upto1(Ref, ModOp, T, [MF|Orig], [MF|Alt]);

%% next is a normal case (possibly in a sequence), mark as skipped, or copy, and proceed
modify_cases_upto1(Ref, {skip,Reason,_,Mode,skip_case}=Op,
		   [{_M,_F}=MF|T], Orig, Alt) ->
    modify_cases_upto1(Ref, Op, T, Orig, [{skip_case,{MF,Reason},Mode}|Alt]);
modify_cases_upto1(Ref, {skip,Reason,_,Mode,auto_skip_case}=Op,
		   [{_M,_F}=MF|T], Orig, Alt) ->
    modify_cases_upto1(Ref, Op, T, Orig, [{auto_skip_case,{MF,Reason},Mode}|Alt]);
modify_cases_upto1(Ref, CopyOp, [{_M,_F}=MF|T], Orig, Alt) ->
    modify_cases_upto1(Ref, CopyOp, T, [MF|Orig], [MF|Alt]);

%% next is a conf case, modify the Mode arg to keep track of sub groups
modify_cases_upto1(Ref, {skip,Reason,FType,Mode,SkipType},
		   [{conf,OtherRef,Props,_MF}|T], Orig, Alt) ->
    case hd(Mode) of
	{OtherRef,_,_} ->			% end conf
	    modify_cases_upto1(Ref, {skip,Reason,FType,tl(Mode),SkipType},
			       T, Orig, Alt);
	_ ->					% start conf
	    Mode1 = [conf(OtherRef,Props)|Mode],
	    modify_cases_upto1(Ref, {skip,Reason,FType,Mode1,SkipType},
			       T, Orig, Alt)
    end;

%% next is some other case, ignore or copy
modify_cases_upto1(Ref, {skip,_,_,_,_}=Op, [_Other|T], Orig, Alt) ->
    modify_cases_upto1(Ref, Op, T, Orig, Alt);
modify_cases_upto1(Ref, CopyOp, [C|T], Orig, Alt) ->
    modify_cases_upto1(Ref, CopyOp, T, [C|Orig], [C|Alt]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set_io_buffering(IOHandler) -> PrevIOHandler
%%
%% Save info about current process (always the main process) buffering
%% io printout messages from parallel test case processes (*and* possibly
%% also the main process).

set_io_buffering(IOHandler) ->
    put(test_server_common_io_handler, IOHandler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_io_buffered() -> true|false
%%
%% Test whether is being buffered.

is_io_buffered() ->
    get(test_server_common_io_handler) =/= undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% queue_test_case_io(Pid, Num, Mod, Func) -> ok
%%
%% Save info about test case that gets its io buffered. This can
%% be a parallel test case or it can be a test case (conf or normal)
%% that belongs to a group nested under a parallel group. The queue
%% is processed after io buffering is disabled. See run_test_cases_loop/4
%% and handle_test_case_io_and_status/0 for more info.

queue_test_case_io(Ref, Pid, Num, Mod, Func) ->
    Entry = {Ref,Pid,Num,Mod,Func},
    %% the order of the test cases is very important!
    put(test_server_queued_io,
	get(test_server_queued_io)++[Entry]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wait_for_cases(Ref) -> {Ok,Skipped,Failed}
%%
%% At the end of a nested parallel group, we have to wait for the test
%% cases to terminate before we can go on (since test cases never execute
%% in parallel with the end conf case of the group). When a top level
%% parallel group is finished, buffered io messages must be handled and
%% this is taken care of by handle_test_case_io_and_status/0.

wait_for_cases(Ref) ->
    case get(test_server_queued_io) of
	[] ->
	    {[],[],[]};
	Cases ->
	    [_Start|TCs] =
		lists:dropwhile(fun({R,_,_,_,_}) when R == Ref -> false;
				   (_) -> true
				end, Cases),
	    wait_and_resend(Ref, TCs, [],[],[])
    end.

wait_and_resend(Ref, [{OtherRef,_,0,_,_}|Ps],
		Ok,Skip,Fail) when is_reference(OtherRef),
				   OtherRef /= Ref ->
    %% ignore cases that belong to nested group
    Ps1 = rm_cases_upto(OtherRef, Ps),
    wait_and_resend(Ref, Ps1, Ok,Skip,Fail);

wait_and_resend(Ref, [{_,CurrPid,CaseNum,Mod,Func}|Ps] = Cases, Ok,Skip,Fail) ->
    receive
	{finished,_Ref,CurrPid,CaseNum,Mod,Func,Result,_RetVal} = Msg ->
	    %% resend message to main process so that it can be used
	    %% to test_server_io:print_buffered/1 later
	    self() ! Msg,
	    MF = {Mod,Func},
	    {Ok1,Skip1,Fail1} =
		case Result of
		    ok -> {[MF|Ok],Skip,Fail};
		    skipped -> {Ok,[MF|Skip],Fail};
		    failed -> {Ok,Skip,[MF|Fail]}
		end,
	    wait_and_resend(Ref, Ps, Ok1,Skip1,Fail1);
	{'EXIT',CurrPid,Reason} when Reason /= normal ->
	    %% unexpected termination of test case process
	    {value,{_,_,CaseNum,Mod,Func}} = lists:keysearch(CurrPid, 2, Cases),
	    print(1, "Error! Process for test case #~w (~w:~tw) died! Reason: ~tp",
		  [CaseNum, Mod, Func, Reason]),
	    exit({unexpected_termination,{CaseNum,Mod,Func},{CurrPid,Reason}})
    end;

wait_and_resend(_, [], Ok,Skip,Fail) ->
    {lists:reverse(Ok),lists:reverse(Skip),lists:reverse(Fail)}.

rm_cases_upto(Ref, [{Ref,_,0,_,_}|Ps]) ->
    Ps;
rm_cases_upto(Ref, [_|Ps]) ->
    rm_cases_upto(Ref, Ps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_test_case_io_and_status() -> [Ok,Skipped,Failed}
%%
%% Each parallel test case process prints to its own minor log file during
%% execution. The common log files (major, html etc) must however be
%% written to sequentially. This is handled by calling
%% test_server_io:start_transaction/0 to tell the test_server_io process
%% to buffer all print requests.
%%
%% An io session is always started with a
%% {started,Ref,Pid,Num,Mod,Func} message (and
%% test_server_io:start_transaction/0 will be called) and terminated
%% with {finished,Ref,Pid,Num,Mod,Func,Result,RetVal} (and
%% test_server_io:end_transaction/0 will be called).  The result
%% shipped with the finished message from a parallel process is used
%% to update status data of the current test run. An 'EXIT' message
%% from each parallel test case process (after finishing and
%% terminating) is also received and handled here.
%%
%% During execution of a parallel group, any cases (conf or normal)
%% belonging to a nested group will also get its io printouts buffered.
%% This is necessary to get the major and html log files written in
%% correct sequence. This function handles also the print messages
%% generated by nested group cases that have been executed sequentially
%% by the main process (note that these cases do not generate 'EXIT'
%% messages, only 'start' and 'finished' messages).
%%
%% See the header comment for run_test_cases_loop/4 for more
%% info about IO handling.
%%
%% Note: It is important that the type of messages handled here
%% do not get consumed by test_server:run_test_case_msgloop/5
%% during the test case execution (e.g. in the catch clause of
%% the receive)!

handle_test_case_io_and_status() ->
    case get(test_server_queued_io) of
	[] ->
	    {[],[],[]};
	Cases ->
	    %% Cases = [{Ref,Pid,CaseNum,Mod,Func} | ...]
	    Result = handle_io_and_exit_loop([], Cases, [],[],[]),
	    Main = self(),
	    %% flush normal exit messages
	    lists:foreach(fun({_,Pid,_,_,_}) when Pid /= Main ->
				  receive
				      {'EXIT',Pid,normal} -> ok
				  after
				      1000 -> ok
				  end;
			     (_) ->
				  ok
		  end, Cases),
	    Result
    end.

%% Handle cases (without Ref) that belong to the top parallel group (i.e. when Refs = [])
handle_io_and_exit_loop([], [{undefined,CurrPid,CaseNum,Mod,Func}|Ps] = Cases, Ok,Skip,Fail) ->
    %% retrieve the start message for the current io session (= testcase)
    receive
	{started,_,CurrPid,CaseNum,Mod,Func} ->
	    {Ok1,Skip1,Fail1} =
		case handle_io_and_exits(self(), CurrPid, CaseNum, Mod, Func, Cases) of
		    {ok,MF} -> {[MF|Ok],Skip,Fail};
		    {skipped,MF} -> {Ok,[MF|Skip],Fail};
		    {failed,MF} -> {Ok,Skip,[MF|Fail]}
		end,
	    handle_io_and_exit_loop([], Ps, Ok1,Skip1,Fail1)
    after
	1000 ->
	    exit({testcase_failed_to_start,Mod,Func})
    end;

%% Handle cases that belong to groups nested under top parallel group
handle_io_and_exit_loop(Refs, [{Ref,CurrPid,CaseNum,Mod,Func}|Ps] = Cases, Ok,Skip,Fail) ->
    receive
	{started,_,CurrPid,CaseNum,Mod,Func} ->
	    _ = handle_io_and_exits(self(), CurrPid, CaseNum, Mod, Func, Cases),
	    Refs1 =
		case Refs of
		    [Ref|Rs] ->	                % must be end conf case for subgroup
			Rs;
		    _ when is_reference(Ref) -> % must be start of new subgroup
			[Ref|Refs];
		    _ -> 			% must be normal subgroup testcase
			Refs
		end,
	    handle_io_and_exit_loop(Refs1, Ps, Ok,Skip,Fail)
    after
	1000 ->
	    exit({testcase_failed_to_start,Mod,Func})
    end;

handle_io_and_exit_loop(_, [], Ok,Skip,Fail) ->
    {lists:reverse(Ok),lists:reverse(Skip),lists:reverse(Fail)}.

handle_io_and_exits(Main, CurrPid, CaseNum, Mod, Func, Cases) ->
    receive
	{abort_current_testcase=Tag,_Reason,From} ->
	    %% If a parallel group is executing, there is no unique
	    %% current test case, so we must generate an error.
	    From ! {self(),Tag,{error,parallel_group}},
	    handle_io_and_exits(Main, CurrPid, CaseNum, Mod, Func, Cases);
	%% end of io session from test case executed by main process
	{finished,_,Main,CaseNum,Mod,Func,Result,_RetVal} ->
	    test_server_io:print_buffered(CurrPid),
	    {Result,{Mod,Func}};
	%% end of io session from test case executed by parallel process
	{finished,_,CurrPid,CaseNum,Mod,Func,Result,RetVal} ->
	    test_server_io:print_buffered(CurrPid),
	    case Result of
		ok ->
		    put(test_server_ok, get(test_server_ok)+1);
		failed ->
		    put(test_server_failed, get(test_server_failed)+1);
		skipped ->
		    SkipCounters =
			update_skip_counters(RetVal, get(test_server_skipped)),
		    put(test_server_skipped, SkipCounters)
	    end,
	    {Result,{Mod,Func}};

	%% unexpected termination of test case process
	{'EXIT',TCPid,Reason} when Reason /= normal ->
	    test_server_io:print_buffered(CurrPid),
	    {value,{_,_,Num,M,F}} = lists:keysearch(TCPid, 2, Cases),
	    print(1, "Error! Process for test case #~w (~w:~tw) died! Reason: ~tp",
		  [Num, M, F, Reason]),
	    exit({unexpected_termination,{Num,M,F},{TCPid,Reason}})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_case(Ref, Num, Mod, Func, Args, RunInit,
%%               TimetrapData, Mode) -> RetVal
%%
%% Creates the minor log file and inserts some test case specific headers
%% and footers into the log files. Then the test case is executed and the
%% result is printed to the log files (also info about lingering processes
%% & slave nodes in the system is presented).
%%
%% RunInit decides if the per test case init is to be run (true for all
%% but conf cases).
%%
%% Mode specifies if the test case should be executed by a dedicated,
%% parallel, process rather than sequentially by the main process. If
%% the former, the new process is spawned and the dictionary of the main
%% process is copied to the test case process.
%%
%% RetVal is the result of executing the test case. It contains info
%% about the execution time and the return value of the test case function.

run_test_case(Ref, Num, Mod, Func, Args, RunInit, TimetrapData) ->
    ok = file:set_cwd(filename:dirname(get(test_server_dir))),
    run_test_case1(Ref, Num, Mod, Func, Args, RunInit,
		   TimetrapData, [], self()).

run_test_case(Ref, Num, Mod, Func, Args, skip_init, TimetrapData, Mode) ->
    %% a conf case is always executed by the main process
    run_test_case1(Ref, Num, Mod, Func, Args, skip_init,
		   TimetrapData, Mode, self());

run_test_case(Ref, Num, Mod, Func, Args, RunInit, TimetrapData, Mode) ->
    ok = file:set_cwd(filename:dirname(get(test_server_dir))),
    Main = self(),
    case check_prop(parallel, Mode) of
	false ->
	    %% this is a sequential test case
	    run_test_case1(Ref, Num, Mod, Func, Args, RunInit,
			   TimetrapData, Mode, Main);
	_Ref ->
	    %% this a parallel test case, spawn the new process
	    Dictionary = get(),
	    {dictionary,Dictionary} = process_info(self(), dictionary),
	    spawn_link(
	      fun() ->
		      process_flag(trap_exit, true),
                      ct_util:mark_process(),
		      _ = [put(Key, Val) || {Key,Val} <- Dictionary],
		      set_io_buffering({tc,Main}),
		      run_test_case1(Ref, Num, Mod, Func, Args, RunInit,
				     TimetrapData, Mode, Main)
	      end)
    end.

run_test_case1(Ref, Num, Mod, Func, Args, RunInit,
	       TimetrapData, Mode, Main) ->
    group_leader(test_server_io:get_gl(Main == self()), self()),

    %% if io is being buffered, send start io session message
    %% (no matter if case runs on parallel or main process)
    case is_io_buffered() of
	false -> ok;
	true ->
	    test_server_io:start_transaction(),
	    Main ! {started,Ref,self(),Num,Mod,Func},
	    ok
    end,
    TSDir = get(test_server_dir),

    print(major, "=case          ~w:~tw", [Mod, Func]),
    MinorName = start_minor_log_file(Mod, Func, self() /= Main),
    MinorBase = filename:basename(MinorName),
    print(major, "=logfile       ~ts", [filename:basename(MinorName)]),

    UpdatedArgs =
	%% maybe create unique private directory for test case or config func
	case get(test_server_create_priv_dir) of
	    auto_per_run ->
		update_config(hd(Args), [{tc_logfile,MinorName}]);
	    PrivDirMode ->
		%% create unique private directory for test case
		RunDir = filename:dirname(MinorName),
		Ext =
		    if Num == 0 ->
			    Int = erlang:unique_integer([positive,monotonic]),
			    lists:flatten(io_lib:format(".cfg.~w", [Int]));
		       true ->
			    lists:flatten(io_lib:format(".~w", [Num]))
		    end,
		PrivDir = filename:join(RunDir, ?priv_dir) ++ Ext,
		if PrivDirMode == auto_per_tc ->
			ok = file:make_dir(PrivDir);
		   PrivDirMode == manual_per_tc ->
			ok
		end,
		update_config(hd(Args), [{priv_dir,PrivDir++"/"},
					 {tc_logfile,MinorName}])
	end,
    GrName = get_name(Mode),
    test_server_sup:framework_call(report,
				   [tc_start,{{Mod,{Func,GrName}},
					      MinorName}]),

    {ok,Cwd} = file:get_cwd(),
    Args2Print = if is_list(UpdatedArgs) ->
			 lists:keydelete(tc_group_result, 1, UpdatedArgs);
		     true ->
			 UpdatedArgs
		 end,
    if RunInit == skip_init ->
	    print_props(get_props(Mode));
       true ->
	    ok
    end,

    print(minor,
	  escape_chars(io_lib:format("Config value:\n\n    ~tp\n", [Args2Print])),
	  []),
    print(minor, "Current directory is ~tp\n", [Cwd]),

    GrNameStr =	case GrName of
		    undefined -> "";
		    Name      -> cast_to_list(Name)
		end,
    print(major, "=started       ~s", [lists:flatten(timestamp_get(""))]),
    {{Col0,Col1},Style} = get_font_style((RunInit==run_init), Mode),
    TR = xhtml("<tr valign=\"top\">", ["<tr class=\"",odd_or_even(),"\">"]),
    EncMinorBase = uri_encode(MinorBase),
    print(html,	TR ++ "<td>" ++ Col0 ++ "~ts" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "~w" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "~ts" ++ Col1 ++ "</td>"
	  "<td><a href=\"~ts\">~tw</a></td>"
	  "<td><a href=\"~ts#top\">&lt;</a> <a href=\"~ts#end\">&gt;</a></td>",
	  [num2str(Num),fw_name(Mod),GrNameStr,EncMinorBase,Func,
	   EncMinorBase,EncMinorBase]),

    do_unless_parallel(Main, fun erlang:yield/0),

    %% run the test case
    {Result,DetectedFail,ProcsBefore,ProcsAfter} =
	run_test_case_apply(Num, Mod, Func, [UpdatedArgs], GrName,
			    RunInit, TimetrapData),
    {Time,RetVal,Loc,Opts,Comment} =
	case Result of
	    Normal={_Time,_RetVal,_Loc,_Opts,_Comment} -> Normal;
	    {died,DReason,DLoc,DCmt} -> {died,DReason,DLoc,[],DCmt}
	end,

    print(minor, "<a name=\"end\"></a>", [], internal_raw),
    print(minor, "\n", [], internal_raw),
    print_timestamp(minor, "Ended at "),
    print(major, "=ended         ~s", [lists:flatten(timestamp_get(""))]),

    do_unless_parallel(Main, fun() -> file:set_cwd(filename:dirname(TSDir)) end),

    %% call the appropriate progress function clause to print the results to log
    Status =
	case {Time,RetVal} of
	    {died,{timetrap_timeout,TimetrapTimeout}} ->
		progress(failed, Num, Mod, Func, GrName, Loc,
			 timetrap_timeout, TimetrapTimeout, Comment, Style);
	    {died,{Skip,Reason}} when Skip==skip; Skip==skipped ->
                %% died in init_per_testcase
		progress(skip, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {died,Reason} when Reason=/=ok ->
                %% (If Reason==ok it means that process died in
                %% end_per_testcase after successfully completing the
                %% test case itself - then we shall not fail, but a
                %% warning will be issued in the comment field.)
		progress(failed, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',{Skip,Reason}}} when Skip==skip; Skip==skipped;
					    Skip==auto_skip ->
		progress(skip, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',_Pid,{Skip,Reason}}} when Skip==skip; Skip==skipped ->
		progress(skip, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',_Pid,Reason}} ->
		progress(failed, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',Reason}} ->
		progress(failed, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {_,{Fail,Reason}} when Fail =:= fail; Fail =:= failed ->
		progress(failed, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {_,Reason={auto_skip,_Why}} ->
		progress(skip, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);		
	    {_,{Skip,Reason}} when Skip==skip; Skip==skipped ->
		progress(skip, Num, Mod, Func, GrName, Loc, Reason,
			 Time, Comment, Style);
	    {Time,RetVal} ->
		case DetectedFail of
		    [] ->
			progress(ok, Num, Mod, Func, GrName, Loc, RetVal,
				 Time, Comment, Style);

		    Reason ->
			progress(failed, Num, Mod, Func, GrName, Loc, Reason,
				 Time, Comment, Style)
		end
	end,
    %% if the test case was executed sequentially, this updates the
    %% status count on the main process (status of parallel test cases
    %% is updated later by the handle_test_case_io_and_status/0 function)
    case {RunInit,Status} of
	{skip_init,_} ->			% conf doesn't count
	    ok;
	{_,ok} ->
	    put(test_server_ok, get(test_server_ok)+1);
	{_,failed} ->
	    put(test_server_failed, get(test_server_failed)+1);
	{_,skip} ->
	    {US,AS} = get(test_server_skipped),
	    put(test_server_skipped, {US+1,AS});
	{_,auto_skip} ->
	    {US,AS} = get(test_server_skipped),
	    put(test_server_skipped, {US,AS+1})
    end,
    %% only if test case execution is sequential do we care about the
    %% remaining processes and slave nodes count
    case self() of
	Main ->
	    case test_server_sup:framework_call(warn, [processes], true) of
		true ->
		    if ProcsBefore < ProcsAfter ->
			    print(minor,
				  "WARNING: ~w more processes in system after test case",
				  [ProcsAfter-ProcsBefore]);
		       ProcsBefore > ProcsAfter ->
			    print(minor,
				  "WARNING: ~w less processes in system after test case",
				  [ProcsBefore-ProcsAfter]);
		       true -> ok
		    end;
		false ->
		    ok
	    end,
	    case test_server_sup:framework_call(warn, [nodes], true) of
		true ->
		    case catch controller_call(kill_slavenodes) of
			{'EXIT',_} = Exit ->
			    print(minor,
				  "WARNING: There might be slavenodes left in the"
				  " system. I tried to kill them, but I failed: ~tp\n",
				  [Exit]);
			[] -> ok;
			List ->
			    print(minor, "WARNING: ~w slave nodes in system after test"++
				  "case. Tried to killed them.~n"++
				  "         Names:~tp",
				  [length(List),List])
		    end;
		false ->
		    ok
	    end;
	_ ->
	    ok
    end,
    %% if the test case was executed sequentially, this updates the execution
    %% time count on the main process (adding execution time of parallel test
    %% case groups is done in run_test_cases_loop/4)
    if is_number(Time) ->
	    put(test_server_total_time, get(test_server_total_time)+Time);
       true ->
	    ok
    end,
    test_server_sup:check_new_crash_dumps(),

    %% if io is being buffered, send finished message
    %% (no matter if case runs on parallel or main process)
    case is_io_buffered() of
	false ->
	    ok;
	true ->
	    test_server_io:end_transaction(),
	    Main ! {finished,Ref,self(),Num,Mod,Func,
		    ?mod_result(Status),{Time,RetVal,Opts}},
		    ok
    end,
    {Time,RetVal,Opts}.


%%--------------------------------------------------------------------
%% various help functions

%% Call Action if we are running on the main process (not parallel).
do_unless_parallel(Main, Action) when is_function(Action, 0) ->
    case self() of
	Main -> Action();
	_ -> ok
    end.

num2str(0) -> "";
num2str(N) -> integer_to_list(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% progress(Result, CaseNum, Mod, Func, Location, Reason, Time,
%%	    Comment, TimeFormat) -> Result
%%
%% Prints the result of the test case to log file.
%% Note: Strings that are to be written to the minor log must
%% be prefixed with "=== " here, or the indentation will be wrong.

progress(skip, CaseNum, Mod, Func, GrName, Loc, Reason, Time,
	 Comment, {St0,St1}) ->
    {Reason1,{Color,Ret,ReportTag}} = 
	if_auto_skip(Reason,
		     fun() -> {?auto_skip_color,auto_skip,auto_skipped} end,
		     fun() -> {?user_skip_color,skip,skipped} end),
    print(major, "=result        ~w: ~tp", [ReportTag,Reason1]),
    print(1, "*** SKIPPED ~ts ***",
	  [get_info_str(Mod,Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report, [tc_done,{Mod,{Func,GrName},
						     {ReportTag,Reason1}}]),
    TimeStr = io_lib:format(if is_float(Time) -> "~.3fs";
			       true -> "~w"
			    end, [Time]),
    ReasonStr = escape_chars(reason_to_string(Reason1)),
    ReasonStr1 = lists:flatten([string:trim(S,leading,"\s") ||
				S <- string:lexemes(ReasonStr,[$\n])]),
    ReasonLength = string:length(ReasonStr1),
    ReasonStr2 =
	if ReasonLength > 80 ->
		string:slice(ReasonStr1, 0, 77) ++ "...";
	   true ->
		ReasonStr1
	end,
    Comment1 = case Comment of
		   "" -> "";
		   _ -> xhtml("<br>(","<br />(") ++ to_string(Comment) ++ ")"
	       end,
    print(html,
	  "<td>" ++ St0 ++ "~ts" ++ St1 ++ "</td>"
	  "<td><font color=\"~ts\">SKIPPED</font></td>"
	  "<td>~ts~ts</td></tr>\n",
	  [TimeStr,Color,ReasonStr2,Comment1]),
    FormatLoc = test_server_sup:format_loc(Loc),
    print(minor, "=== Location: ~ts", [FormatLoc]),
    print(minor, "=== Reason: ~ts", [ReasonStr1]),
    Ret;

progress(failed, CaseNum, Mod, Func, GrName, Loc, timetrap_timeout, T,
	 Comment0, {St0,St1}) ->
    print(major, "=result        failed: timeout, ~tp", [Loc]),
    print(1, "*** FAILED ~ts ***",
	  [get_info_str(Mod,Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report,
				   [tc_done,{Mod,{Func,GrName},
					     {failed,timetrap_timeout}}]),
    FormatLastLoc = test_server_sup:format_loc(get_last_loc(Loc)),
    ErrorReason = io_lib:format("{timetrap_timeout,~ts}", [FormatLastLoc]),
    Comment =
	case Comment0 of
	    "" -> "<font color=\"red\">" ++ ErrorReason ++ "</font>";
	    _ -> "<font color=\"red\">" ++ ErrorReason ++ 
		 xhtml("</font><br>","</font><br />") ++ to_string(Comment0)
	end,
    print(html,
	  "<td>" ++ St0 ++ "~.3fs" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td>~ts</td></tr>\n",
	  [T/1000,Comment]),
    FormatLoc = test_server_sup:format_loc(Loc),
    print(minor, "=== Location: ~ts", [FormatLoc]),
    print(minor, "=== Reason: timetrap timeout", []),
    failed;

progress(failed, CaseNum, Mod, Func, GrName, Loc, {testcase_aborted,Reason}, _T,
	 Comment0, {St0,St1}) ->
    print(major, "=result        failed: testcase_aborted, ~tp", [Loc]),
    print(1, "*** FAILED ~ts ***",
	  [get_info_str(Mod,Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report,
				   [tc_done,{Mod,{Func,GrName},
					     {failed,testcase_aborted}}]),
    FormatLastLoc = test_server_sup:format_loc(get_last_loc(Loc)),
    ErrorReason = io_lib:format("{testcase_aborted,~ts}", [FormatLastLoc]),
    Comment =
	case Comment0 of
	    "" -> "<font color=\"red\">" ++ ErrorReason ++ "</font>";
	    _ -> "<font color=\"red\">" ++ ErrorReason ++ 
		 xhtml("</font><br>","</font><br />") ++ to_string(Comment0)
	end,
    print(html,
	  "<td>" ++ St0 ++ "died" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td>~ts</td></tr>\n",
	  [Comment]),
    FormatLoc = test_server_sup:format_loc(Loc),
    print(minor, "=== Location: ~ts", [FormatLoc]),
    print(minor,
	  escape_chars(io_lib:format("=== Reason: {testcase_aborted,~tp}",
				     [Reason])),
	  []),
    failed;

progress(failed, CaseNum, Mod, Func, GrName, unknown, Reason, Time,
	 Comment0, {St0,St1}) ->
    print(major, "=result        failed: ~tp, ~w", [Reason,unknown_location]),
    print(1, "*** FAILED ~ts ***",
	  [get_info_str(Mod,Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report, [tc_done,{Mod,{Func,GrName},
						     {failed,Reason}}]),
    TimeStr = io_lib:format(if is_float(Time) -> "~.3fs";
			       true -> "~w"
			    end, [Time]),
    ErrorReason = escape_chars(lists:flatten(io_lib:format("~tp", [Reason]))),
    ErrorReason1 = lists:flatten([string:trim(S,leading,"\s") ||
				  S <- string:lexemes(ErrorReason,[$\n])]),
    ErrorReasonLength = string:length(ErrorReason1),
    ErrorReason2 =
	if ErrorReasonLength > 63 ->
		string:slice(ErrorReason1, 0, 60) ++ "...";
	   true ->
		ErrorReason1
	end,
    Comment =
	case Comment0 of
	    "" -> "<font color=\"red\">" ++ ErrorReason2 ++ "</font>";
	    _ -> "<font color=\"red\">" ++ ErrorReason2 ++ 
		 xhtml("</font><br>","</font><br />") ++
		 to_string(Comment0)
	end,
    print(html,
	  "<td>" ++ St0 ++ "~ts" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td>~ts</td></tr>\n",
	  [TimeStr,Comment]),
    print(minor, "=== Location: ~w", [unknown]),
    {FStr,FormattedReason} = format_exception(Reason),
    print(minor,
	  escape_chars(io_lib:format("=== Reason: " ++ FStr, [FormattedReason])),
	  []),
    failed;

progress(failed, CaseNum, Mod, Func, GrName, Loc, Reason, Time,
	 Comment0, {St0,St1}) ->
    {LocMaj,LocMin} = if Func == error_in_suite ->
			      case get_fw_mod(undefined) of
				  Mod -> {unknown_location,unknown};
				  _   -> {Loc,Loc}
			      end;
			 true -> {Loc,Loc}
		       end,
    print(major, "=result        failed: ~tp, ~tp", [Reason,LocMaj]),
    print(1, "*** FAILED ~ts ***",
	  [get_info_str(Mod,Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report, [tc_done,{Mod,{Func,GrName},
						     {failed,Reason}}]),
    TimeStr = io_lib:format(if is_float(Time) -> "~.3fs";
			       true -> "~w"
			    end, [Time]),
    Comment =
	case Comment0 of
	    "" -> "";
	    _ -> xhtml("<br>","<br />") ++ to_string(Comment0)
	end,
    FormatLastLoc = test_server_sup:format_loc(get_last_loc(LocMaj)),
    print(html,
	  "<td>" ++ St0 ++ "~ts" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td><font color=\"red\">~ts</font>~ts</td></tr>\n",
	  [TimeStr,FormatLastLoc,Comment]),
    FormatLoc = test_server_sup:format_loc(LocMin),
    print(minor, "=== Location: ~ts", [FormatLoc]),
    {FStr,FormattedReason} = format_exception(Reason),
    print(minor, "=== Reason: " ++
	      escape_chars(io_lib:format(FStr, [FormattedReason])), []),
    failed;

progress(ok, _CaseNum, Mod, Func, GrName, _Loc, RetVal, Time,
	 Comment0, {St0,St1}) ->
    print(minor, "successfully completed test case", []),
    test_server_sup:framework_call(report, [tc_done,{Mod,{Func,GrName},ok}]),
    TimeStr = io_lib:format(if is_float(Time) -> "~.3fs";
			       true -> "~w"
			    end, [Time]),
    Comment =
	case RetVal of
	    {comment,RetComment} ->
		String = to_string(RetComment),
		HtmlCmt = test_server_sup:framework_call(format_comment,
							 [String],
							 String),
		print(major, "=result        ok: ~ts", [String]),
		"<td>" ++ HtmlCmt ++ "</td>";
	    _ ->
		print(major, "=result        ok", []),
		case Comment0 of
		    "" -> "<td></td>";
		    _ -> "<td>" ++ to_string(Comment0) ++ "</td>"
		end
	end,
    print(major, "=elapsed       ~p", [Time]),
    print(html,
	  "<td>" ++ St0 ++ "~ts" ++ St1 ++ "</td>"
	  "<td><font color=\"green\">Ok</font></td>"
	  "~ts</tr>\n",
	  [TimeStr,Comment]),
    print(minor,
	  escape_chars(io_lib:format("=== Returned value: ~tp", [RetVal])),
	  []),
    ok.

%%--------------------------------------------------------------------
%% various help functions
escape_chars(Term) when not is_list(Term), not is_binary(Term) ->
    esc_chars_in_list(io_lib:format("~tp", [Term]));
escape_chars(List = [Term | _]) when not is_list(Term), not is_integer(Term) ->
    esc_chars_in_list(io_lib:format("~tp", [List]));
escape_chars(List) ->
    esc_chars_in_list(List).

esc_chars_in_list([Bin | Io]) when is_binary(Bin) ->
    [Bin | esc_chars_in_list(Io)];
esc_chars_in_list([List | Io]) when is_list(List) ->
    [esc_chars_in_list(List) | esc_chars_in_list(Io)];
esc_chars_in_list([$< | Io]) ->
    ["&lt;" | esc_chars_in_list(Io)];
esc_chars_in_list([$> | Io]) ->
    ["&gt;" | esc_chars_in_list(Io)];
esc_chars_in_list([$& | Io]) ->
    ["&amp;" | esc_chars_in_list(Io)];
esc_chars_in_list([Char | Io]) when is_integer(Char) ->
    [Char | esc_chars_in_list(Io)];
esc_chars_in_list([]) ->
    [];
esc_chars_in_list(Bin) ->
    Bin.

get_fw_mod(Mod) ->
    case get(test_server_framework) of
	undefined ->
	    case os:getenv("TEST_SERVER_FRAMEWORK") of
		FW when FW =:= false; FW =:= "undefined" ->
		    Mod;
		FW ->
		    list_to_atom(FW)
	    end;
	'$none' -> Mod;
	FW      -> FW
    end.

fw_name(?MODULE) ->
    test_server;
fw_name(Mod) ->
    case get(test_server_framework_name) of
	undefined ->
	    case get_fw_mod(undefined) of
		undefined ->
		    Mod;
		Mod ->
		    case os:getenv("TEST_SERVER_FRAMEWORK_NAME") of
			FWName when FWName =:= false; FWName =:= "undefined" ->
			    Mod;
			FWName ->
			    list_to_atom(FWName)
		    end;
		_ ->
		    Mod
	    end;
	'$none' ->
	    Mod;
	FWName ->
	    case get_fw_mod(Mod) of
		Mod -> FWName;
		_ -> Mod
	    end	
    end.

if_auto_skip(Reason={failed,{_,init_per_testcase,_}}, True, _False) ->
    {Reason,True()};
if_auto_skip({skip,Reason={failed,{_,init_per_testcase,_}}}, True, _False) ->
    {Reason,True()};
if_auto_skip({auto_skip,Reason}, True, _False) ->
    {Reason,True()};
if_auto_skip(Reason, _True, False) ->
    {Reason,False()}.

update_skip_counters({_T,Pat,_Opts}, {US,AS}) ->
    {_,Result} = if_auto_skip(Pat, fun() -> {US,AS+1} end, fun() -> {US+1,AS} end),
    Result;    
update_skip_counters(Pat, {US,AS}) ->
    {_,Result} = if_auto_skip(Pat, fun() -> {US,AS+1} end, fun() -> {US+1,AS} end),
    Result.

get_info_str(Mod,Func, 0, _Cases) ->
    io_lib:format("~tw", [{Mod,Func}]);
get_info_str(_Mod,_Func, CaseNum, unknown) ->
    "test case " ++ integer_to_list(CaseNum);
get_info_str(_Mod,_Func, CaseNum, Cases) ->
    "test case " ++ integer_to_list(CaseNum) ++
	" of " ++ integer_to_list(Cases).

print_if_known(Known, {SK,AK}, {SU,AU}) ->
    {S,A} = if Known == unknown -> {SU,AU};
	       true -> {SK,AK}
	    end,
    io_lib:format(S, A).

to_string(Term) when is_list(Term) ->
    case (catch io_lib:format("~ts", [Term])) of
	{'EXIT',_} -> lists:flatten(io_lib:format("~tp", [Term]));
	String     -> lists:flatten(String)
    end;
to_string(Term) ->
    lists:flatten(io_lib:format("~tp", [Term])).

get_last_loc(Loc) when is_tuple(Loc) ->
    Loc;
get_last_loc([Loc|_]) when is_tuple(Loc) ->
    [Loc];
get_last_loc(Loc) ->
    Loc.

reason_to_string({failed,{_,FailFunc,bad_return}}) ->
    atom_to_list(FailFunc) ++ " bad return value";
reason_to_string({failed,{_,FailFunc,{timetrap_timeout,_}}}) ->
    atom_to_list(FailFunc) ++ " timed out";
reason_to_string(FWInitFail = {failed,{_CB,init_tc,_Reason}}) ->
    to_string(FWInitFail);
reason_to_string({failed,{_,FailFunc,_}}) ->
    atom_to_list(FailFunc) ++ " failed";
reason_to_string(Other) ->
    to_string(Other).

%get_font_style(Prop) ->
%    {Col,St0,St1} = get_font_style1(Prop),
%    {{"<font color="++Col++">","</font>"},
%     {"<font color="++Col++">"++St0,St1++"</font>"}}.

get_font_style(NormalCase, Mode) ->
    Prop = if not NormalCase ->
		   default;
	      true ->
		   case check_prop(parallel, Mode) of
		       false ->
			   case check_prop(sequence, Mode) of
			       false ->
				   default;
			       _ ->
				   sequence
			   end;
		       _ ->
			   parallel
		   end
	   end,
    {Col,St0,St1} = get_font_style1(Prop),
    {{"<font color="++Col++">","</font>"},
     {"<font color="++Col++">"++St0,St1++"</font>"}}.

get_font_style1(parallel) ->
    {"\"darkslategray\"","<i>","</i>"};
get_font_style1(sequence) ->
%    {"\"darkolivegreen\"","",""};
    {"\"saddlebrown\"","",""};
get_font_style1(default) ->
    {"\"black\"","",""}.
%%get_font_style1(skipped) ->
%%    {"\"lightgray\"","",""}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% format_exception({Error,Stack}) -> {CtrlSeq,Term}
%%
%% The default behaviour is that error information gets formatted
%% (like in the erlang shell) before printed to the minor log file.
%% The framework application can switch this feature off by setting
%% *its* application environment variable 'format_exception' to false.
%% It is also possible to switch formatting off by starting the
%% test_server node with init argument 'test_server_format_exception'
%% set to false.

format_exception(Reason={_Error,Stack}) when is_list(Stack) ->
    case get_fw_mod(undefined) of
	undefined ->
	    case application:get_env(test_server, format_exception) of
		{ok,false} ->
		    {"~tp",Reason};
		_ ->
		    do_format_exception(Reason)
	    end;
	FW ->
	    case application:get_env(FW, format_exception) of
		{ok,false} ->
		    {"~tp",Reason};
		_ ->
		    do_format_exception(Reason)
	    end
    end;
format_exception(Error) ->
    format_exception({Error,[]}).

do_format_exception(Reason={Error,Stack}) ->
    StackFun = fun(_, _, _) -> false end,
    PF = fun(Term, I) ->
		 io_lib:format("~." ++ integer_to_list(I) ++ "tp", [Term])
	 end,
    case catch lib:format_exception(1, error, Error, Stack, StackFun, PF, utf8) of
	{'EXIT',_R} ->
	    {"~tp",Reason};
	Formatted  ->
	    Formatted1 = re:replace(Formatted, "exception error: ", "", [{return,list},unicode]),
	    {"~ts",lists:flatten(Formatted1)}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_case_apply(CaseNum, Mod, Func, Args, Name, RunInit,
%%                     TimetrapData) ->
%%  {{Time,RetVal,Loc,Opts,Comment},DetectedFail,ProcessesBefore,ProcessesAfter} |
%%  {{died,Reason,unknown,Comment},DetectedFail,ProcessesBefore,ProcessesAfter}
%% Name = atom()
%% Time = float()   (seconds)
%% RetVal = term()
%% Loc = term()
%% Comment = string()
%% Reason = term()
%% DetectedFail = [{File,Line}]
%% ProcessesBefore = ProcessesAfter = integer()
%%

run_test_case_apply(CaseNum, Mod, Func, Args, Name, RunInit,
		    TimetrapData) ->
    test_server:run_test_case_apply({CaseNum,Mod,Func,Args,Name,RunInit,
				     TimetrapData}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print(Detail, Format, Args) -> ok
%% Detail = integer()
%% Format = string()
%% Args = [term()]
%%
%% Just like io:format, except that depending on the Detail value, the output
%% is directed to console, major and/or minor log files.

print(Detail, Format) ->
    print(Detail, Format, []).

print(Detail, Format, Args) ->
    print(Detail, Format, Args, internal).

print(Detail, ["$tc_html",Format], Args, Printer) ->
    Msg = io_lib:format(Format, Args),
    print_or_buffer(Detail, ["$tc_html",Msg], Printer);

print(Detail, Format, Args, Printer) ->
    Msg = io_lib:format(Format, Args),
    print_or_buffer(Detail, Msg, Printer).

print_or_buffer(Detail, Msg, Printer) ->
    test_server_gl:print(group_leader(), Detail, Msg, Printer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print_timestamp(Detail, Leader) -> ok
%%
%% Prints Leader followed by a time stamp (date and time). Depending on
%% the Detail value, the output is directed to console, major and/or minor
%% log files.

print_timestamp(Detail, Leader) ->
    print(Detail, timestamp_get(Leader), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print_who(Host, User) -> ok
%%
%% Logs who runs the suite.

print_who(Host, User) ->
    UserStr = case User of
		  "" -> "";
		  _ -> " by " ++ User
	      end,
    print(html, "Run~ts on ~ts", [UserStr,Host]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% format(Format) -> IoLibReturn
%% format(Detail, Format) -> IoLibReturn
%% format(Format, Args) -> IoLibReturn
%% format(Detail, Format, Args) -> IoLibReturn
%%
%% Detail = integer()
%% Format = string()
%% Args = [term(),...]
%% IoLibReturn = term()
%%
%% Logs the Format string and Args, similar to io:format/1/2 etc. If
%% Detail is not specified, the default detail level (which is 50) is used.
%% Which log files the string will be logged in depends on the thresholds
%% set with set_levels/3. Typically with default detail level, only the
%% minor log file is used.

format(Format) ->
    format(minor, Format, []).

format(major, Format) ->
    format(major, Format, []);
format(minor, Format) ->
    format(minor, Format, []);
format(Detail, Format) when is_integer(Detail) ->
    format(Detail, Format, []);
format(Format, Args) ->
    format(minor, Format, Args).

format(Detail, Format, Args) ->
    Str =
	case catch io_lib:format(Format, Args) of
	    {'EXIT',_} ->
		io_lib:format("illegal format; ~tp with args ~tp.\n",
			      [Format,Args]);
	    Valid -> Valid
	end,
    print_or_buffer(Detail, Str, self()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% xhtml(BasicHtml, XHtml) -> BasicHtml | XHtml
%%
xhtml(HTML, XHTML) ->
    case get(basic_html) of
	true -> HTML;
	_ -> XHTML
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% odd_or_even() -> "odd" | "even"
%%
odd_or_even() ->
    case get(odd_or_even) of
	even ->
	    put(odd_or_even, odd),
	    "even";
	_ ->
	    put(odd_or_even, even),
	    "odd"
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timestamp_filename_get(Leader) -> string()
%% Leader = string()
%%
%% Returns a string consisting of Leader concatenated with the current
%% date and time. The resulting string is suitable as a filename.
timestamp_filename_get(Leader) ->
    timestamp_get_internal(Leader,
			   "~ts~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timestamp_get(Leader) -> string()
%% Leader = string()
%%
%% Returns a string consisting of Leader concatenated with the current
%% date and time. The resulting string is suitable for display.
timestamp_get(Leader) ->
    timestamp_get_internal(Leader,
			   "~ts~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w").

timestamp_get_internal(Leader, Format) ->
    {YY,MM,DD,H,M,S} = time_get(),
    io_lib:format(Format, [Leader,YY,MM,DD,H,M,S]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% time_get() -> {YY,MM,DD,H,M,S}
%% YY = integer()
%% MM = integer()
%% DD = integer()
%% H = integer()
%% M = integer()
%% S = integer()
%%
%% Returns the current Year,Month,Day,Hours,Minutes,Seconds.
%% The function checks that the date doesn't wrap while calling
%% getting the time.
time_get() ->
    {YY,MM,DD} = date(),
    {H,M,S} = time(),
    case date() of
	{YY,MM,DD} ->
	    {YY,MM,DD,H,M,S};
	_NewDay ->
	    %% date changed between call to date() and time(), try again
	    time_get()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make_config(Config) -> NewConfig
%% Config = [{Key,Value},...]
%% NewConfig = [{Key,Value},...]
%%
%% Creates a configuration list (currently returns it's input)

make_config(Initial) ->
    Initial.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% update_config(Config, Update) -> NewConfig
%% Config = [{Key,Value},...]
%% Update = [{Key,Value},...] | {Key,Value}
%% NewConfig = [{Key,Value},...]
%%
%% Adds or replaces the key-value pairs in config with those in update.
%% Returns the updated list.

update_config(Config, {Key,Val}) ->
    case lists:keymember(Key, 1, Config) of
	true ->
	    lists:keyreplace(Key, 1, Config, {Key,Val});
	false ->
	    [{Key,Val}|Config]
    end;
update_config(Config, [Assoc|Assocs]) ->
    NewConfig = update_config(Config, Assoc),
    update_config(NewConfig, Assocs);
update_config(Config, []) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% collect_cases(CurMod, TopCase, SkipList) ->
%%     BasicCaseList | {error,Reason}
%%
%% CurMod = atom()
%% TopCase = term()
%% SkipList = [term(),...]
%% BasicCaseList = [term(),...]
%%
%% Parses the given test goal(s) in TopCase, and transforms them to a
%% simple list of test cases to call, when executing the test suite.
%%
%% CurMod is the "current" module, that is, the module the last instruction
%% was read from. May be be set to 'none' initially.
%%
%% SkipList is the list of test cases to skip and requirements to deny.
%%
%% The BasicCaseList is built out of TopCase, which may be any of the
%% following terms:
%%
%% []                        Nothing is added
%% List list()               The list is decomposed, and each element is
%%                           treated according to this table
%% Case atom()               CurMod:Case(suite) is called
%% {module,Case}             CurMod:Case(suite) is called
%% {Module,Case}             Module:Case(suite) is called
%% {module,Module,Case}      Module:Case(suite) is called
%% {module,Module,Case,Args} Module:Case is called with Args as arguments
%% {dir,Dir}                 All modules *_SUITE in the named directory
%%                           are listed, and each Module:all(suite) is called
%% {dir,Dir,Pattern}         All modules <Pattern>_SUITE in the named dir
%%                           are listed, and each Module:all(suite) is called
%% {conf,InitMF,Cases,FinMF}
%% {conf,Props,InitMF,Cases,FinMF}
%%                           InitMF is placed in the BasicCaseList, then
%%                           Cases is treated according to this table, then
%%                           FinMF is placed in the BasicCaseList. InitMF
%%                           and FinMF are configuration manipulation
%%                           functions. See below.
%% {make,InitMFA,Cases,FinMFA}
%%                           InitMFA is placed in the BasicCaseList, then
%%                           Cases is treated according to this table, then
%%                           FinMFA is placed in the BasicCaseList. InitMFA
%%                           and FinMFA are make/unmake functions. If InitMFA
%%                           fails, Cases are not run.
%%
%% When a function is called, above, it means that the function is invoked
%% and the return is expected to be:
%%
%% []                        Leaf case
%% {req,ReqList}             Kept for backwards compatibility - same as []
%% {req,ReqList,Cases}       Kept for backwards compatibility -
%%                           Cases parsed recursively with collect_cases/3
%% Cases (list)              Recursively parsed with collect_cases/3
%%
%% Leaf cases are added to the BasicCaseList as Module:Case(Config). Each
%% case is checked against the SkipList. If present, a skip instruction
%% is inserted instead, which only prints the case name and the reason
%% why the case was skipped in the log files.
%%
%% Configuration manipulation functions are called with the current
%% configuration list as only argument, and are expected to return a new
%% configuration list. Such a pair of function may, for example, start a
%% server and stop it after a serie of test cases.
%%
%% SkipCases is expected to be in the format:
%%
%% Other                     Recursively parsed with collect_cases/3
%% {Mod,Comment}             Skip Mod, with Comment
%% {Mod,Funcs,Comment}       Skip listed functions in Mod with Comment
%% {Mod,Func,Comment}        Skip named function in Mod with Comment
%%
-record(cc, {mod,				% current module
	     skip}).				% skip list

collect_all_cases(Top, Skip) when is_list(Skip) ->
    Result =
	case collect_cases(Top, #cc{mod=[],skip=Skip}, []) of
	    {ok,Cases,_St} -> Cases;
	    Other          -> Other
	end,
    Result.


collect_cases([], St, _) -> {ok,[],St};
collect_cases([Case|Cs0], St0, Mode) ->
    case collect_cases(Case, St0, Mode) of
	{ok,FlatCases1,St1} ->
	    case collect_cases(Cs0, St1, Mode) of
		{ok,FlatCases2,St} ->
		    {ok,FlatCases1 ++ FlatCases2,St};
		{error,_Reason} = Error -> Error
	    end;
	{error,_Reason} = Error -> Error
    end;


collect_cases({module,Case}, St, Mode) when is_atom(Case), is_atom(St#cc.mod) ->
    collect_case({St#cc.mod,Case}, St, Mode);
collect_cases({module,Mod,Case}, St, Mode) ->
    collect_case({Mod,Case}, St, Mode);
collect_cases({module,Mod,Case,Args}, St, Mode) ->
    collect_case({Mod,Case,Args}, St, Mode);

collect_cases({dir,SubDir}, St, Mode) ->
    collect_files(SubDir, "*_SUITE", St, Mode);
collect_cases({dir,SubDir,Pattern}, St, Mode) ->
    collect_files(SubDir, Pattern++"*", St, Mode);

collect_cases({conf,InitF,CaseList,FinMF}, St, Mode) when is_atom(InitF) ->
    collect_cases({conf,[],{St#cc.mod,InitF},CaseList,FinMF}, St, Mode);
collect_cases({conf,InitMF,CaseList,FinF}, St, Mode) when is_atom(FinF) ->
    collect_cases({conf,[],InitMF,CaseList,{St#cc.mod,FinF}}, St, Mode);
collect_cases({conf,InitMF,CaseList,FinMF}, St0, Mode) ->
    collect_cases({conf,[],InitMF,CaseList,FinMF}, St0, Mode);
collect_cases({conf,Props,InitF,CaseList,FinMF}, St, Mode) when is_atom(InitF) ->
    case init_props(Props) of
	{error,_} ->
	    {ok,[],St};
	Props1 ->
	    collect_cases({conf,Props1,{St#cc.mod,InitF},CaseList,FinMF},
			  St, Mode)
    end;
collect_cases({conf,Props,InitMF,CaseList,FinF}, St, Mode) when is_atom(FinF) ->
    case init_props(Props) of
	{error,_} ->
	    {ok,[],St};
	Props1 ->
	    collect_cases({conf,Props1,InitMF,CaseList,{St#cc.mod,FinF}},
			  St, Mode)
    end;
collect_cases({conf,Props,InitMF,CaseList,FinMF} = Conf, St, Mode) ->
    case init_props(Props) of
	{error,_} ->
	    {ok,[],St};
	Props1 ->
	    Ref = make_ref(),
	    Skips = St#cc.skip,
	    Props2 = [{suite,St#cc.mod} | lists:delete(suite,Props1)],
	    Mode1 = [{Ref,Props2,undefined} | Mode],
	    case in_skip_list({St#cc.mod,Conf}, Skips) of
		{true,Comment} ->	    	           % conf init skipped
		    {ok,[{skip_case,{conf,Ref,InitMF,Comment},Mode1} |
			 [] ++ [{conf,Ref,[],FinMF}]],St};
		{true,Name,Comment} when is_atom(Name) ->  % all cases skipped
		    case collect_cases(CaseList, St, Mode1) of
			{ok,[],_St} = Empty ->
			    Empty;
			{ok,FlatCases,St1} ->
			    Cases2Skip = FlatCases ++ [{conf,Ref,
							keep_name(Props1),
							FinMF}],
			    Skipped = skip_cases_upto(Ref, Cases2Skip, Comment,
						      conf, Mode1, skip_case),
			    {ok,[{skip_case,{conf,Ref,InitMF,Comment},Mode1} |
				 Skipped],St1};
			{error,_Reason} = Error ->
			    Error
		    end;
		{true,ToSkip,_} when is_list(ToSkip) ->    % some cases skipped
		    case collect_cases(CaseList,
				       St#cc{skip=ToSkip++Skips}, Mode1) of
			{ok,[],_St} = Empty ->
			    Empty;
			{ok,FlatCases,St1} ->
			    {ok,[{conf,Ref,Props1,InitMF} |
				 FlatCases ++ [{conf,Ref,
						keep_name(Props1),
						FinMF}]],St1#cc{skip=Skips}};
			{error,_Reason} = Error ->
			    Error
		    end;
		false ->
		    case collect_cases(CaseList, St, Mode1) of
			{ok,[],_St} = Empty ->
			    Empty;
			{ok,FlatCases,St1} ->
			    {ok,[{conf,Ref,Props1,InitMF} |
				 FlatCases ++ [{conf,Ref,
						keep_name(Props1),
						FinMF}]],St1};
			{error,_Reason} = Error ->
			    Error
		    end
	    end
    end;

collect_cases({make,InitMFA,CaseList,FinMFA}, St0, Mode) ->
    case collect_cases(CaseList, St0, Mode) of
	{ok,[],_St} = Empty -> Empty;
	{ok,FlatCases,St} ->
	    Ref = make_ref(),
	    {ok,[{make,Ref,InitMFA}|FlatCases ++
		 [{make,Ref,FinMFA}]],St};
	{error,_Reason} = Error -> Error
    end;

collect_cases({Module, Cases}, St, Mode) when is_list(Cases)  ->
    case (catch collect_case(Cases, St#cc{mod=Module}, [], Mode)) of
	Result = {ok,_,_} ->
 	    Result;
 	Other ->
	    {error,Other}
     end;

collect_cases({_Mod,_Case}=Spec, St, Mode) ->
    collect_case(Spec, St, Mode);

collect_cases({_Mod,_Case,_Args}=Spec, St, Mode) ->
    collect_case(Spec, St, Mode);
collect_cases(Case, St, Mode) when is_atom(Case), is_atom(St#cc.mod) ->
    collect_case({St#cc.mod,Case}, St, Mode);
collect_cases(Other, St, _Mode) ->
    {error,{bad_subtest_spec,St#cc.mod,Other}}.

collect_case({Mod,{conf,_,_,_,_}=Conf}, St, Mode) ->
    collect_case_invoke(Mod, Conf, [], St, Mode);

collect_case(MFA, St, Mode) ->
    case in_skip_list(MFA, St#cc.skip) of
	{true,Comment} when Comment /= make_failed ->
	    {ok,[{skip_case,{MFA,Comment},Mode}],St};
	_ ->
	    case MFA of
		{Mod,Case} -> collect_case_invoke(Mod, Case, MFA, St, Mode);
		{_Mod,_Case,_Args} -> {ok,[MFA],St}
	    end
    end.

collect_case([], St, Acc, _Mode) ->
    {ok, Acc, St};

collect_case([Case | Cases], St, Acc, Mode) ->
    {ok, FlatCases, NewSt}  = collect_case({St#cc.mod, Case}, St, Mode),
    collect_case(Cases, NewSt, Acc ++ FlatCases, Mode).

collect_case_invoke(Mod, Case, MFA, St, Mode) ->
    case get_fw_mod(undefined) of
	undefined ->
	    case catch apply(Mod, Case, [suite]) of
		{'EXIT',_} ->
		    {ok,[MFA],St};
		Suite ->
		    collect_subcases(Mod, Case, MFA, St, Suite, Mode)
	    end;
	_ ->
	    Suite = test_server_sup:framework_call(get_suite,
						   [Mod,Case],
						   []),
	    collect_subcases(Mod, Case, MFA, St, Suite, Mode)
    end.

collect_subcases(Mod, Case, MFA, St, Suite, Mode) ->
    case Suite of
	[] when Case == all -> {ok,[],St};
	[] when element(1, Case) == conf -> {ok,[],St};
	[] -> {ok,[MFA],St};
%%%! --- START Kept for backwards compatibility ---
%%%! Requirements are not used
	{req,ReqList} ->
	    collect_case_deny(Mod, Case, MFA, ReqList, [], St, Mode);
	{req,ReqList,SubCases} ->
	    collect_case_deny(Mod, Case, MFA, ReqList, SubCases, St, Mode);
%%%! --- END Kept for backwards compatibility ---
	{Skip,Reason} when Skip==skip; Skip==skipped ->
	    {ok,[{skip_case,{MFA,Reason},Mode}],St};
	{error,Reason} ->
	    throw(Reason);
	SubCases ->
	    collect_case_subcases(Mod, Case, SubCases, St, Mode)
    end.

collect_case_subcases(Mod, Case, SubCases, St0, Mode) ->
    OldMod = St0#cc.mod,
    case collect_cases(SubCases, St0#cc{mod=Mod}, Mode) of
	{ok,FlatCases,St} ->
	    {ok,FlatCases,St#cc{mod=OldMod}};
	{error,Reason} ->
	    {error,{{Mod,Case},Reason}}
    end.

collect_files(Dir, Pattern, St, Mode) ->
    {ok,Cwd} = file:get_cwd(),
    Dir1 = filename:join(Cwd, Dir),
    Wc = filename:join([Dir1,Pattern++"{.erl,"++code:objfile_extension()++"}"]),
    case catch filelib:wildcard(Wc) of
	{'EXIT', Reason} ->
	    io:format("Could not collect files: ~tp~n", [Reason]),
	    {error,{collect_fail,Dir,Pattern}};
	Files ->
	    %% convert to module names and remove duplicates
	    Mods = lists:foldl(fun(File, Acc) ->
				       Mod = fullname_to_mod(File),
				       case lists:member(Mod, Acc) of
					   true  -> Acc;
					   false -> [Mod | Acc]
				       end
			       end, [], Files),
	    Tests = [{Mod,all} || Mod <- lists:sort(Mods)],
	    collect_cases(Tests, St, Mode)
    end.

fullname_to_mod(Path) when is_list(Path) ->
    %% If this is called with a binary, then we are probably in +fnu
    %% mode and have found a beam file with name encoded as latin1. We
    %% will let this crash since it can not work to load such a module
    %% anyway. It should be removed or renamed!
    list_to_atom(filename:rootname(filename:basename(Path))).

collect_case_deny(Mod, Case, MFA, ReqList, SubCases, St, Mode) ->
    case {check_deny(ReqList, St#cc.skip),SubCases} of
	{{denied,Comment},_SubCases} ->
	    {ok,[{skip_case,{MFA,Comment},Mode}],St};
	{granted,[]} ->
	    {ok,[MFA],St};
	{granted,SubCases} ->
	    collect_case_subcases(Mod, Case, SubCases, St, Mode)
    end.

check_deny([Req|Reqs], DenyList) ->
    case check_deny_req(Req, DenyList) of
	{denied,_Comment}=Denied -> Denied;
	granted -> check_deny(Reqs, DenyList)
    end;
check_deny([], _DenyList) -> granted;
check_deny(Req, DenyList) -> check_deny([Req], DenyList).

check_deny_req({Req,Val}, DenyList) ->
    %%io:format("ValCheck ~p=~p in ~p\n", [Req,Val,DenyList]),
    case lists:keysearch(Req, 1, DenyList) of
	{value,{_Req,DenyVal}} when Val >= DenyVal ->
	    {denied,io_lib:format("Requirement ~tp=~tp", [Req,Val])};
	_ ->
	    check_deny_req(Req, DenyList)
    end;
check_deny_req(Req, DenyList) ->
    case lists:member(Req, DenyList) of
	true -> {denied,io_lib:format("Requirement ~tp", [Req])};
	false -> granted
    end.

in_skip_list({Mod,{conf,Props,InitMF,_CaseList,_FinMF}}, SkipList) ->
    case in_skip_list(InitMF, SkipList) of
	{true,_} = Yes ->
	    Yes;
	_ ->
	    case proplists:get_value(name, Props) of
		undefined ->
		    false;
		Name ->
		    ToSkip =
			lists:flatmap(
			  fun({M,{conf,SProps,_,SCaseList,_},Cmt}) when
				    M == Mod ->
				  case proplists:get_value(name, SProps) of
				      all ->
					  [{M,all,Cmt}];
				      Name ->
					  case SCaseList of
					      all ->
						  [{M,all,Cmt}];
					      _ ->
						  [{M,F,Cmt} || F <- SCaseList]
					  end;
				      _ ->
					  []
				  end;
			     (_) ->
				  []
			  end, SkipList),
		    case ToSkip of
			[] ->
			    false;
			_ ->
			    case lists:keysearch(all, 2, ToSkip) of
				{value,{_,_,Cmt}} -> {true,Name,Cmt};
				_                 -> {true,ToSkip,""}
			    end
		    end
	    end
    end;

in_skip_list({Mod,Func,_Args}, SkipList) ->
    in_skip_list({Mod,Func}, SkipList);
in_skip_list({Mod,Func}, [{Mod,Funcs,Comment}|SkipList]) when is_list(Funcs) ->
    case lists:member(Func, Funcs) of
	true ->
	    {true,Comment};
	_ ->
	    in_skip_list({Mod,Func}, SkipList)
    end;
in_skip_list({Mod,Func}, [{Mod,Func,Comment}|_SkipList]) ->
    {true,Comment};
in_skip_list({Mod,_Func}, [{Mod,Comment}|_SkipList]) ->
    {true,Comment};
in_skip_list({Mod,Func}, [_|SkipList]) ->
    in_skip_list({Mod,Func}, SkipList);
in_skip_list(_, []) ->
    false.

%% remove unnecessary properties
init_props(Props) ->
    case get_repeat(Props) of
	Repeat = {_RepType,N} when N < 2 ->
	    if N == 0 ->
		    {error,{invalid_property,Repeat}};
	       true ->
		    lists:delete(Repeat, Props)
	    end;
	_ ->
	    Props
    end.

keep_name(Props) ->
    lists:filter(fun({name,_}) -> true;
		    ({suite,_}) -> true;
		    (_) -> false end, Props).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 Node handling functions                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_target_info() -> #target_info
%%
%% Returns a record containing system information for target

get_target_info() ->
    controller_call(get_target_info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_node(SlaveName, Type, Options) ->
%%     {ok, Slave} | {error, Reason}
%%
%% Called by test_server. See test_server:start_node/3 for details

start_node(Name, Type, Options) ->
    T = 10 * ?ACCEPT_TIMEOUT * test_server:timetrap_scale_factor(),
    format(minor, "Attempt to start ~w node ~tp with options ~tp",
	   [Type, Name, Options]),
    case controller_call({start_node,Name,Type,Options}, T) of
	{{ok,Nodename}, Host, Cmd, Info, Warning} ->
	    format(minor,
		   "Successfully started node ~w on ~tp with command: ~ts",
		   [Nodename, Host, Cmd]),
	    format(major, "=node_start    ~w", [Nodename]),
	    case Info of
		[] -> ok;
		_ -> format(minor, Info)
	    end,
	    case Warning of
		[] -> ok;
		_ ->
		    format(1, Warning),
		    format(minor, Warning)
	    end,
	    {ok, Nodename};
	{fail,{Ret, Host, Cmd}}  ->
	    format(minor,
		   "Failed to start node ~tp on ~tp with command: ~ts~n"
		   "Reason: ~tp",
		   [Name, Host, Cmd, Ret]),
	    {fail,Ret};
	{Ret, undefined, undefined} ->
	    format(minor, "Failed to start node ~tp: ~tp", [Name,Ret]),
	    Ret;
	{Ret, Host, Cmd} ->
	    format(minor,
		   "Failed to start node ~tp on ~tp with command: ~ts~n"
		   "Reason: ~tp",
		   [Name, Host, Cmd, Ret]),
	    Ret
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wait_for_node(Node) -> ok | {error,timeout}
%%
%% Wait for a slave/peer node which has been started with
%% the option {wait,false}. This function returns when
%% when the new node has contacted test_server_ctrl again

wait_for_node(Slave) ->
    T = 10000 * test_server:timetrap_scale_factor(),
    case catch controller_call({wait_for_node,Slave},T) of
	{'EXIT',{timeout,_}} -> {error,timeout};
	ok -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_release_available(Release) -> true | false
%% Release -> string()
%%
%% Test if a release (such as "r10b") is available to be
%% started using start_node/3.

is_release_available(Release) ->
    controller_call({is_release_available,Release}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stop_node(Name) -> ok | {error,Reason}
%%
%% Clean up - test_server will stop this node

stop_node(Slave) ->
    controller_call({stop_node,Slave}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        DEBUGGER INTERFACE                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i() ->
    hformat("Pid", "Initial Call", "Current Function", "Reducts", "Msgs"),
    Line=lists:duplicate(27, "-"),
    hformat(Line, Line, Line, Line, Line),
    display_info(processes(), 0, 0).

p(A,B,C) ->
    pinfo(ts_pid(A,B,C)).
p(X) when is_atom(X) ->
    pinfo(whereis(X));
p({A,B,C}) ->
    pinfo(ts_pid(A,B,C));
p(X) ->
    pinfo(X).

t() ->
    t(wall_clock).
t(X) ->
    element(1, statistics(X)).

pi(Item,X) ->
    lists:keysearch(Item,1,p(X)).
pi(Item,A,B,C) ->
    lists:keysearch(Item,1,p(A,B,C)).

%% c:pid/3
ts_pid(X,Y,Z) when is_integer(X), is_integer(Y), is_integer(Z) ->
    list_to_pid("<" ++ integer_to_list(X) ++ "." ++
		integer_to_list(Y) ++ "." ++
		integer_to_list(Z) ++ ">").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% display_info(Pids, Reductions, Messages) -> void
%% Pids = [pid(),...]
%% Reductions = integer()
%% Messaged = integer()
%%
%% Displays info, similar to c:i() about the processes in the list Pids.
%% Also counts the total number of reductions and msgs for the listed
%% processes, if called with Reductions = Messages = 0.

display_info([Pid|T], R, M) ->
    case pinfo(Pid) of
	undefined ->
	    display_info(T, R, M);
	Info ->
	    Call = fetch(initial_call, Info),
	    Curr = case fetch(current_function, Info) of
		       {Mod,F,Args} when is_list(Args) ->
			   {Mod,F,length(Args)};
		       Other ->
			   Other
		   end,
	    Reds  = fetch(reductions, Info),
	    LM = fetch(message_queue_len, Info),
	    pformat(io_lib:format("~w", [Pid]),
		    io_lib:format("~tw", [Call]),
		    io_lib:format("~tw", [Curr]), Reds, LM),
	    display_info(T, R+Reds, M + LM)
    end;
display_info([], R, M) ->
    Line=lists:duplicate(27, "-"),
    hformat(Line, Line, Line, Line, Line),
    pformat("Total", "", "", R, M).

hformat(A1, A2, A3, A4, A5) ->
    io:format("~-10s ~-27s ~-27s ~8s ~4s~n", [A1,A2,A3,A4,A5]).

pformat(A1, A2, A3, A4, A5) ->
    io:format("~-10s ~-27s ~-27s ~8w ~4w~n", [A1,A2,A3,A4,A5]).

fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {_, Val}} ->
	    Val;
	_ ->
	    0
    end.

pinfo(P) ->
    Node = node(),
    case node(P) of
	Node ->
	    process_info(P);
	_ ->
	    rpc:call(node(P),erlang,process_info,[P])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Support functions for COVER                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% A module is included in the cover analysis if
%% - it belongs to the tested application and is not listed in the
%%   {exclude,List} part of the App.cover file
%% - it does not belong to the application, but is listed in the
%%   {include,List} part of the App.cover file
%% - it does not belong to the application, but is listed in the
%%   {cross,[{Tag,List}]} part of the App.cover file
%%
%% The modules listed in the 'cross' part of the cover file are
%% modules that are heavily used by other tests than the one where
%% they are explicitly tested. They should then be listed as 'cross'
%% in the cover file for the test where they are used but do not
%% belong.
%%
%% After all tests are completed, the these modules can be analysed
%% with coverage data from all tests where they are compiled - see
%% cross_cover_analyse/2. The result is stored in a file called
%% cross_cover.html in the run.<timestamp> directory of the
%% test the modules belong to.
%%
%% Example:
%% If the module m1 belongs to system s1 but is heavily used also in
%% the tests for another system s2, then the cover files for the two
%% systems could be like this:
%%
%% s1.cover:
%%  {include,[m1]}.
%%
%% s2.cover:
%%  {include,[....]}. % modules belonging to system s2
%%  {cross,[{s1,[m1]}]}.
%%
%% When the tests for both s1 and s2 are completed, run
%% cross_cover_analyse(Level,[{s1,S1LogDir},{s2,S2LogDir}]), and
%% the accumulated cover data for m1 will be written to
%% S1LogDir/[run.<timestamp>/]cross_cover.html
%%
%% S1LogDir and S2LogDir are either the run.<timestamp> directories
%% for the two tests, or the parent directory of these, in which case
%% the latest run.<timestamp> directory will be chosen.
%%
%% Note that the m1 module will also be presented in the normal
%% coverage log for s1 (due to the include statement in s1.cover), but
%% that only includes the coverage achieved by the s1 test itself.
%%
%% The Tag in the 'cross' statement in the cover file has no other
%% purpose than mapping the list of modules ([m1] in the example
%% above) to the correct log directory where it should be included in
%% the cross_cover.html file (S1LogDir in the example above).
%% I.e. the value of the Tag has no meaning, it could be foo as well
%% as s1 above, as long as the same Tag is used in the cover file and
%% in the call to cross_cover_analyse/2.


%% Cover compilation
%% The compilation is executed on the target node
start_cover(#cover{}=CoverInfo) ->
    cover_compile(CoverInfo);
start_cover({log,CoverLogDir}=CoverInfo) ->
    %% Cover is controlled by the framework - here's the log
    put(test_server_cover_log_dir,CoverLogDir),
    {ok,CoverInfo}.

cover_compile(CoverInfo) ->
    test_server:cover_compile(CoverInfo).

%% Read the coverfile for an application and return a list of modules
%% that are members of the application but shall not be compiled
%% (Exclude), and a list of modules that are not members of the
%% application but shall be compiled (Include).
read_cover_file(none) ->
    {[],[],[]};
read_cover_file(CoverFile) ->
    case file:consult(CoverFile) of
	{ok,List} ->
	    case check_cover_file(List, [], [], []) of
		{ok,Exclude,Include,Cross} -> {Exclude,Include,Cross};
		error ->
		    io:fwrite("Faulty format of CoverFile ~tp\n", [CoverFile]),
		    {[],[],[]}
	    end;
	{error,Reason} ->
	    io:fwrite("Can't read CoverFile ~ts\nReason: ~tp\n",
		      [CoverFile,Reason]),
	    {[],[],[]}
    end.

check_cover_file([{exclude,all}|Rest], _, Include, Cross) ->
    check_cover_file(Rest, all, Include, Cross);
check_cover_file([{exclude,Exclude}|Rest], _, Include, Cross) ->
    case lists:all(fun(M) -> is_atom(M) end, Exclude) of
	true ->
	    check_cover_file(Rest, Exclude, Include, Cross);
	false ->
	    error
    end;
check_cover_file([{include,Include}|Rest], Exclude, _, Cross) ->
    case lists:all(fun(M) -> is_atom(M) end, Include) of
	true ->
	    check_cover_file(Rest, Exclude, Include, Cross);
	false ->
	    error
    end;
check_cover_file([{cross,Cross}|Rest], Exclude, Include, _) ->
    case check_cross(Cross) of
	true ->
	    check_cover_file(Rest, Exclude, Include, Cross);
	false ->
	    error
    end;
check_cover_file([], Exclude, Include, Cross) ->
    {ok,Exclude,Include,Cross}.

check_cross([{Tag,Modules}|Rest]) ->
    case lists:all(fun(M) -> is_atom(M) end, [Tag|Modules]) of
	true ->
	    check_cross(Rest);
	false ->
	    false
    end;
check_cross([]) ->
    true.


%% Cover analysis, per application
%% This analysis is executed on the target node once the test is
%% completed for an application. This is not the same as the cross
%% cover analysis, which can be executed on any node after the tests
%% are finshed.
%%
%% This per application analysis writes the file cover.html in the
%% application's run.<timestamp> directory.
stop_cover(#cover{}=CoverInfo, TestDir) ->
    cover_analyse(CoverInfo, TestDir),
    ok;
stop_cover(_CoverInfo, _TestDir) ->
    %% Cover is probably controlled by the framework
    ok.

make_relative(AbsDir, VsDir) ->
    DirTokens = filename:split(AbsDir),
    VsTokens = filename:split(VsDir),
    filename:join(make_relative1(DirTokens, VsTokens)).

make_relative1([T | DirTs], [T | VsTs]) ->
    make_relative1(DirTs, VsTs);
make_relative1(Last = [_File], []) ->
    Last;
make_relative1(Last = [_File], VsTs) ->
    Ups = ["../" || _ <- VsTs],
    Ups ++ Last;
make_relative1(DirTs, []) ->
    DirTs;
make_relative1(DirTs, VsTs) ->
    Ups = ["../" || _ <- VsTs],
    Ups ++ DirTs.


cover_analyse(CoverInfo, TestDir) ->
    write_default_cross_coverlog(TestDir),

    {ok,CoverLog} = open_html_file(filename:join(TestDir, ?coverlog_name)),
    write_coverlog_header(CoverLog),
    #cover{app=App,
	   file=CoverFile,
	   excl=Excluded,
	   cross=Cross} = CoverInfo,
    io:fwrite(CoverLog, "<h1>Coverage for application '~w'</h1>\n", [App]),
    io:fwrite(CoverLog,
	      "<p><a href=\"~ts\">Coverdata collected over all tests</a></p>",
	      [?cross_coverlog_name]),

    io:fwrite(CoverLog, "<p>CoverFile: <code>~tp</code>\n", [CoverFile]),
    ok = write_cross_cover_info(TestDir,Cross),

    case length(cover:imported_modules()) of
	Imps when Imps > 0 ->
	    io:fwrite(CoverLog,
		      "<p>Analysis includes data from ~w imported module(s).\n",
		      [Imps]);
	_ ->
	    ok
    end,

    io:fwrite(CoverLog, "<p>Excluded module(s): <code>~tp</code>\n", [Excluded]),

    Coverage = test_server:cover_analyse(TestDir, CoverInfo),
    ok = write_binary_file(filename:join(TestDir,?raw_coverlog_name),
		      term_to_binary(Coverage)),

    case lists:filter(fun({_M,{_,_,_}}) -> false;
			 (_) -> true
		      end, Coverage) of
	[] ->
	    ok;
	Bad ->
	    io:fwrite(CoverLog, "<p>Analysis failed for ~w module(s): "
		      "<code>~w</code>\n",
		      [length(Bad),[BadM || {BadM,{_,_Why}} <- Bad]])
    end,

    TotPercent = write_cover_result_table(CoverLog, Coverage),
    ok = write_binary_file(filename:join(TestDir, ?cover_total),
			   term_to_binary(TotPercent)).

%% Cover analysis - accumulated over multiple tests
%% This can be executed on any node after all tests are finished.
%% Analyse = overview | details
%% TagDirs = [{Tag,Dir}]
%%   Tag = atom(), identifier
%%   Dir = string(), the log directory for Tag, it can be a
%%         run.<timestamp> directory or the parent directory of
%%         such (in which case the latest run.<timestamp> directory
%%         is used)
cross_cover_analyse(Analyse, TagDirs0) ->
    TagDirs = get_latest_run_dirs(TagDirs0),
    TagMods = get_all_cross_info(TagDirs,[]),
    TagDirMods = add_cross_modules(TagMods,TagDirs),
    CoverdataFiles = get_coverdata_files(TagDirMods),
    lists:foreach(fun(CDF) -> cover:import(CDF) end, CoverdataFiles),
    io:fwrite("Cover analysing...\n", []),
    DetailsFun =
	case Analyse of
	    details ->
		fun(Dir,M) ->
			OutFile = filename:join(Dir,
						atom_to_list(M) ++
						".CROSS_COVER.html"),
			case cover:analyse_to_file(M, OutFile, [html]) of
			    {ok,_} ->
				{file,OutFile};
			    Error ->
				Error
			end
		end;
	    _ ->
		fun(_,_) -> undefined end
	end,
    Coverage = analyse_tests(TagDirMods, DetailsFun, []),
    cover:stop(),
    write_cross_cover_logs(Coverage,TagDirMods).

write_cross_cover_info(_Dir,[]) ->
    ok;
write_cross_cover_info(Dir,Cross) ->
    write_binary_file(filename:join(Dir,?cross_cover_info),
		      term_to_binary(Cross)).

%% For each test from which there are cross cover analysed
%% modules, write a cross cover log (cross_cover.html).
write_cross_cover_logs([{Tag,Coverage}|T],TagDirMods) ->
    case lists:keyfind(Tag,1,TagDirMods) of
	{_,Dir,Mods} when Mods=/=[] ->
	    ok = write_binary_file(filename:join(Dir,?raw_cross_coverlog_name),
			      term_to_binary(Coverage)),
	    CoverLogName = filename:join(Dir,?cross_coverlog_name),
	    {ok,CoverLog} = open_html_file(CoverLogName),
	    write_coverlog_header(CoverLog),
	    io:fwrite(CoverLog,
		      "<h1>Coverage results for \'~w\' from all tests</h1>\n",
		      [Tag]),
	    write_cover_result_table(CoverLog, Coverage),
	    io:fwrite("Written file ~tp\n", [CoverLogName]);
	_ ->
	    ok
    end,
    write_cross_cover_logs(T,TagDirMods);
write_cross_cover_logs([],_) ->
    io:fwrite("done\n", []).

%% Get the latest run.<timestamp> directories
get_latest_run_dirs([{Tag,Dir}|Rest]) ->
    [{Tag,get_latest_run_dir(Dir)} | get_latest_run_dirs(Rest)];
get_latest_run_dirs([]) ->
    [].

get_latest_run_dir(Dir) ->
    case filelib:wildcard(filename:join(Dir,"run.[1-2]*")) of
	[] ->
	    Dir;
	[H|T] ->
	    get_latest_dir(T,H)
    end.

get_latest_dir([H|T],Latest) when H>Latest ->
    get_latest_dir(T,H);
get_latest_dir([_|T],Latest) ->
    get_latest_dir(T,Latest);
get_latest_dir([],Latest) ->
    Latest.

get_all_cross_info([{_Tag,Dir}|Rest],Acc) ->
    case file:read_file(filename:join(Dir,?cross_cover_info)) of
	{ok,Bin} ->
	    TagMods = binary_to_term(Bin),
	    get_all_cross_info(Rest,TagMods++Acc);
	_ ->
	    get_all_cross_info(Rest,Acc)
    end;
get_all_cross_info([],Acc) ->
    Acc.

%% Associate the cross cover modules with their log directories
add_cross_modules(TagMods,TagDirs)->
    do_add_cross_modules(TagMods,[{Tag,Dir,[]} || {Tag,Dir} <- TagDirs]).
do_add_cross_modules([{Tag,Mods1}|TagMods],TagDirMods)->
    NewTagDirMods =
	case lists:keytake(Tag,1,TagDirMods) of
	    {value,{Tag,Dir,Mods},Rest} ->
		[{Tag,Dir,lists:umerge(lists:sort(Mods1),Mods)}|Rest];
	    false ->
		TagDirMods
	end,
    do_add_cross_modules(TagMods,NewTagDirMods);
do_add_cross_modules([],TagDirMods) ->
    %% Just to get the modules in the same order as in the normal cover log
    [{Tag,Dir,lists:reverse(Mods)} || {Tag,Dir,Mods} <- TagDirMods].

%% Find all exported coverdata files.
get_coverdata_files(TagDirMods) ->
    lists:flatmap(
      fun({_,LatestDir,_}) ->
	      filelib:wildcard(filename:join(LatestDir,"all.coverdata"))
      end,
      TagDirMods).


%% For each test, analyse all modules
%% Used for cross cover analysis.
analyse_tests([{Tag,LastTest,Modules}|T], DetailsFun, Acc) ->
    Cov = analyse_modules(LastTest, Modules, DetailsFun, []),
    analyse_tests(T, DetailsFun, [{Tag,Cov}|Acc]);
analyse_tests([], _DetailsFun, Acc) ->
    Acc.

%% Analyse each module
%% Used for cross cover analysis.
analyse_modules(Dir, [M|Modules], DetailsFun, Acc) ->
    {ok,{M,{Cov,NotCov}}} = cover:analyse(M, module),
    Acc1 = [{M,{Cov,NotCov,DetailsFun(Dir,M)}}|Acc],
    analyse_modules(Dir, Modules, DetailsFun, Acc1);
analyse_modules(_Dir, [], _DetailsFun, Acc) ->
    Acc.


%% Support functions for writing the cover logs (both cross and normal)
write_coverlog_header(CoverLog) ->
    case catch io:put_chars(CoverLog,html_header("Coverage results")) of
	{'EXIT',Reason} ->
	    io:format("\n\nERROR: Could not write normal heading in coverlog.\n"
		      "CoverLog: ~tw\n"
		      "Reason: ~tp\n",
		      [CoverLog,Reason]),
	    io:format(CoverLog,"<html><body>\n", []);
	_ ->
	    ok
    end.


format_analyse(M,Cov,NotCov,undefined) ->
    io_lib:fwrite("<tr><td>~w</td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>\n",
		  [M,pc(Cov,NotCov),Cov,NotCov]);
format_analyse(M,Cov,NotCov,{file,File}) ->
    io_lib:fwrite("<tr><td><a href=\"~ts\">~w</a></td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>\n",
		  [uri_encode(filename:basename(File)),
		   M,pc(Cov,NotCov),Cov,NotCov]);
format_analyse(M,Cov,NotCov,{lines,Lines}) ->
    CoverOutName = atom_to_list(M)++".COVER.html",
    {ok,CoverOut} = open_html_file(CoverOutName),
    write_not_covered(CoverOut,M,Lines),
    ok = file:close(CoverOut),
    io_lib:fwrite("<tr><td><a href=\"~ts\">~w</a></td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>\n",
		  [uri_encode(CoverOutName),M,pc(Cov,NotCov),Cov,NotCov]);
format_analyse(M,Cov,NotCov,{error,_}) ->
    io_lib:fwrite("<tr><td>~w</td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>\n",
		  [M,pc(Cov,NotCov),Cov,NotCov]).


pc(0,0) ->
    0;
pc(Cov,NotCov) ->
    round(Cov/(Cov+NotCov)*100).


write_not_covered(CoverOut,M,Lines) ->
    io:put_chars(CoverOut,html_header("Coverage results for "++atom_to_list(M))),
    io:fwrite(CoverOut,
	      "The following lines in module ~w are not covered:\n"
	      "<table border=3 cellpadding=5>\n"
	      "<th>Line Number</th>\n",
	      [M]),
    lists:foreach(fun({{_M,Line},{0,1}}) ->
			  io:fwrite(CoverOut,"<tr><td>~w</td></tr>\n", [Line]);
		     (_) ->
			  ok
		  end,
		  Lines),
    io:put_chars(CoverOut,"</table>\n</body>\n</html>\n").


write_default_coverlog(TestDir) ->
    {ok,CoverLog} = open_html_file(filename:join(TestDir,?coverlog_name)),
    write_coverlog_header(CoverLog),
    io:put_chars(CoverLog,"Cover tool is not used\n</body></html>\n"),
    ok = file:close(CoverLog).

write_default_cross_coverlog(TestDir) ->
    {ok,CrossCoverLog} =
	open_html_file(filename:join(TestDir,?cross_coverlog_name)),
    write_coverlog_header(CrossCoverLog),
    io:put_chars(CrossCoverLog,
		 ["No cross cover modules exist for this application,",
		  xhtml("<br>","<br />"),
		  "or cross cover analysis is not completed.\n"
		  "</body></html>\n"]),
    ok = file:close(CrossCoverLog).

write_cover_result_table(CoverLog,Coverage) ->
    io:fwrite(CoverLog,
	      "<p><table border=3 cellpadding=5>\n"
	      "<tr><th>Module</th><th>Covered (%)</th><th>Covered (Lines)</th>"
	      "<th>Not covered (Lines)</th>\n",
	      []),
    {TotCov,TotNotCov} =
	lists:foldl(fun({M,{Cov,NotCov,Details}},{AccCov,AccNotCov}) ->
			    Str = format_analyse(M,Cov,NotCov,Details),
			    io:fwrite(CoverLog,"~ts", [Str]),
			    {AccCov+Cov,AccNotCov+NotCov};
		       ({_M,{error,_Reason}},{AccCov,AccNotCov}) ->
			    {AccCov,AccNotCov}
		    end,
		    {0,0},
		    Coverage),
    TotPercent = pc(TotCov,TotNotCov),
    io:fwrite(CoverLog,
	      "<tr><th align=left>Total</th><th align=right>~w %</th>"
	      "<th align=right>~w</th><th align=right>~w</th></tr>\n"
	      "</table>\n"
	      "</body>\n"
	      "</html>\n",
	      [TotPercent,TotCov,TotNotCov]),
    ok = file:close(CoverLog),
    TotPercent.


%%%-----------------------------------------------------------------
%%% Support functions for writing files

%% HTML files are always written with utf8 encoding
html_header(Title) ->
    ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
     "<!-- autogenerated by '", atom_to_list(?MODULE), "'. -->\n"
     "<html>\n"
     "<head>\n"
     "<title>", Title, "</title>\n"
     "<meta http-equiv=\"cache-control\" content=\"no-cache\"></meta>\n"
     "<meta http-equiv=\"content-type\" content=\"text/html; "
            "charset=utf-8\"></meta>\n"
     "</head>\n"
     "<body bgcolor=\"white\" text=\"black\" "
     "link=\"blue\" vlink=\"purple\" alink=\"red\">\n"].

html_header(Title, Meta) ->
    ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
     "<!-- autogenerated by '", atom_to_list(?MODULE), "'. -->\n"
     "<html>\n"
     "<head>\n"
     "<title>", Title, "</title>\n"] ++ Meta ++ ["</head>\n"].

open_html_file(File) ->
    open_utf8_file(File).

open_html_file(File,Opts) ->
    open_utf8_file(File,Opts).

write_html_file(File,Content) ->
    write_file(File,Content,utf8).

%% The 'major' log file, which is a pure text file is also written
%% with utf8 encoding
open_utf8_file(File) ->
    case file:open(File,AllOpts=[write,{encoding,utf8}]) of
	{error,Reason} -> {error,{Reason,{File,AllOpts}}};
	Result         -> Result
    end.

open_utf8_file(File,Opts) ->
    case file:open(File,AllOpts=[{encoding,utf8}|Opts]) of
	{error,Reason} -> {error,{Reason,{File,AllOpts}}};
	Result         -> Result
    end.

%% Write a file with specified encoding
write_file(File,Content,latin1) ->
    file:write_file(File,Content);
write_file(File,Content,utf8) ->
    write_binary_file(File,unicode:characters_to_binary(Content)).

%% Write a file with only binary data
write_binary_file(File,Content) ->
    file:write_file(File,Content).

%% Encoding of hyperlinks in HTML files
uri_encode(File) ->
    Encoding = file:native_name_encoding(),
    uri_encode(File,Encoding).

uri_encode(File,Encoding) ->
    Components = filename:split(File),
    filename:join([uri_encode_comp(C,Encoding) || C <- Components]).

%% Encode the reference to a "filename of the given encoding" so it
%% can be inserted in a utf8 encoded HTML file.
%% This does almost the same as http_uri:encode/1, except
%% 1. it does not convert @, : and / (in order to preserve nodename and c:/)
%% 2. if the file name is in latin1, it also encodes all
%%    characters >127 - i.e. latin1 but not ASCII.
uri_encode_comp([Char|Chars],Encoding) ->
    Reserved = sets:is_element(Char, reserved()),
    case (Char>127 andalso Encoding==latin1) orelse Reserved of
	true ->
	    [ $% | integer_to_list(Char, 16)] ++
		uri_encode_comp(Chars,Encoding);
	false ->
	    [Char | uri_encode_comp(Chars,Encoding)]
    end;
uri_encode_comp([],_) ->
    [].

%% Copied from http_uri.erl, but slightly modified
%% (not converting @, : and /)
reserved() ->
    sets:from_list([$;, $&, $=, $+, $,, $?,
		    $#, $[, $], $<, $>, $\", ${, $}, $|,
                    $\\, $', $^, $%, $ ]).

encoding(File) ->
    case epp:read_encoding(File) of
	none ->
	    epp:default_encoding();
	E ->
	    E
    end.
