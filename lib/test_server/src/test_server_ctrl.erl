%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARCHITECTURE
%%
%% The Erlang Test Server can be run on the target machine (local target)
%% or towards a remote target. The execution flow is mainly the same in
%% both cases, but with a remote target the test cases are (obviously)
%% executed on the target machine. Host and target communicates over
%% socket connections because the host should not be introduced as an
%% additional node in the distributed erlang system in which the test
%% cases are run.
%%
%%
%% Local Target:
%% =============
%%
%%   -----
%%   |   |  test_server_ctrl ({global,test_server})
%%   ----- (test_server_ctrl.erl)
%%     |
%%     |
%%   -----
%%   |   | JobProc
%%   ----- (test_server_ctrl.erl and test_server.erl)
%%     |
%%     |
%%   -----
%%   |   | CaseProc
%%   ----- (test_server.erl)
%%
%%
%%
%% test_server_ctrl is the main process in the system. It is a registered
%% process, and it will always be alive when testing is ongoing.
%% test_server_ctrl initiates testing and monitors JobProc(s).
%%
%% When target is local, and Test Server is *not* being used by a framework
%% application (where it might cause duplicate name problems in a distributed
%% test environment), the process is globally registered as 'test_server'
%% to be able to simulate the {global,test_server} process on a remote target.
%%
%% JobProc is spawned for each 'job' added to the test_server_ctrl.
%% A job can mean one test case, one test suite or one spec.
%% JobProc creates and writes logs and presents results from testing.
%% JobProc is the group leader for CaseProc.
%%
%% CaseProc is spawned for each test case. It runs the test case and
%% sends results and any other information to its group leader - JobProc.
%%
%%
%%
%% Remote Target:
%% ==============
%%
%% 		HOST				TARGET
%%
%% 	                   -----  MainSock   -----
%%        test_server_ctrl |   |- - - - - - -|   | {global,test_server}
%%  (test_server_ctrl.erl) -----	     ----- (test_server.erl)
%% 		             |		       |
%% 			     |		       |
%% 		           -----  JobSock    -----
%% 	          JobProcH |   |- - - - - - -|   | JobProcT
%%  (test_server_ctrl.erl) -----	     ----- (test_server.erl)
%% 					       |	
%% 					       |
%% 					     -----
%% 					     |   | CaseProc
%% 					     ----- (test_server.erl)
%%
%%
%%
%%
%% A separate test_server process only exists when target is remote. It
%% is then the main process on target. It is started when test_server_ctrl
%% is started, and a socket connection is established between
%% test_server_ctrl and test_server. The following information can be sent
%% over MainSock:
%%
%% HOST			TARGET
%%  -> {target_info, TargetInfo}     (during initiation)
%%  <- {job_proc_killed,Name,Reason} (if a JobProcT dies unexpectedly)
%%  -> {job,Port,Name}               (to start a new JobProcT)
%%
%%
%% When target is remote, JobProc is split into to processes: JobProcH
%% executing on Host and JobProcT executing on Target. (The two processes
%% execute the same code as JobProc does when target is local.) JobProcH
%% and JobProcT communicates over a socket connection. The following
%% information can be sent over JobSock:
%%
%% HOST			TARGET
%%  -> {test_case, Case}          To start a new test case
%%  -> {beam,Mod}                 .beam file as binary to be loaded
%% 				  on target, e.g. a test suite
%%  -> {datadir,Tarfile}          Content of the datadir for a test suite
%%  <- {apply,MFA}                MFA to be applied on host, ignore return;
%% 				  (apply is used for printing information in
%% 				  log or console)
%%  <- {sync_apply,MFA}           MFA to be applied on host, wait for return
%% 				  (used for starting and stopping slave nodes)
%%  -> {sync_apply,MFA}           MFA to be applied on target, wait for return
%% 				  (used for cover compiling and analysing)
%% <-> {sync_result,Result}       Return value from sync_apply
%%  <- {test_case_result,Result}  When a test case is finished
%%  <- {crash_dumps,Tarfile}      When a test case is finished
%%  -> job_done			  When a job is finished
%%  <- {privdir,Privdir}          When a job is finished
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%% SUPERVISOR INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, start/1, start_link/1, stop/0]).

%%% OPERATOR INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([add_spec/1, add_dir/2, add_dir/3]).
-export([add_module/1, add_module/2, add_case/2, add_case/3, add_cases/2,
	 add_cases/3]).
-export([add_dir_with_skip/3, add_dir_with_skip/4, add_tests_with_skip/3]).
-export([add_module_with_skip/2, add_module_with_skip/3,
	 add_case_with_skip/3, add_case_with_skip/4,
	 add_cases_with_skip/3, add_cases_with_skip/4]).
-export([jobs/0, run_test/1, wait_finish/0, idle_notify/1,
	 abort_current_testcase/1, abort/0]).
-export([start_get_totals/1, stop_get_totals/0]).
-export([get_levels/0, set_levels/3]).
-export([multiply_timetraps/1, scale_timetraps/1, get_timetrap_parameters/0]).
-export([cover/2, cover/3, cover/7,
	 cross_cover_analyse/1, cross_cover_analyse/2, trc/1, stop_trace/0]).
-export([testcase_callback/1]).
-export([set_random_seed/1]).

%%% TEST_SERVER INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([output/2, print/2, print/3, print_timestamp/2]).
-export([start_node/3, stop_node/1, wait_for_node/1, is_release_available/1]).
-export([format/1, format/2, format/3]).
-export([get_target_info/0]).
-export([get_hosts/0]).
-export([get_target_os_type/0]).
-export([node_started/1]).

%%% DEBUGGER INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([i/0, p/1, p/3, pi/2, pi/4, t/0, t/1]).

%%% PRIVATE EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([do_test_cases/4]).
-export([do_spec/2, do_spec_list/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("test_server_internal.hrl").
-include_lib("kernel/include/file.hrl").
-define(suite_ext, "_SUITE").
-define(log_ext, ".log.html").
-define(src_listing_ext,  ".src.html").
-define(logdir_ext, ".logs").
-define(data_dir_suffix, "_data/").
-define(suitelog_name, "suite.log").
-define(coverlog_name, "cover.html").
-define(cross_coverlog_name, "cross_cover.html").
-define(cover_total, "total_cover.log").
-define(last_file, "last_name").
-define(last_link, "last_link").
-define(last_test, "last_test").
-define(html_ext, ".html").
-define(cross_cover_file, "cross.cover").
-define(now, erlang:now()).

-define(pl2a(M), test_server_sup:package_atom(M)).
-define(void_fun, fun() -> ok end).
-define(mod_result(X), if X == skip -> skipped;
			  X == auto_skip -> skipped;
			  true -> X end).

-record(state,{jobs=[],levels={1,19,10},
	       multiply_timetraps=1,scale_timetraps=true,
	       finish=false,
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
	    io:format("Can't open ~s: ~p\n",
		      [cast_to_list(Spec), file:format_error(Reason)]),
	    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, Cov, TCCB)
    end;
parse_cmd_line(['NAME',Name|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, SpecList, [{name,Name}|Names], Param, Trc, Cov, TCCB);
parse_cmd_line(['SKIPMOD',Mod|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, [{skip,{Mod,"by command line"}}|SpecList], Names,
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['SKIPCASE',Mod,Case|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, [{skip,{Mod,Case,"by command line"}}|SpecList], Names,
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['DIR',Dir|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    Name = cast_to_list(filename:basename(Dir)),
    parse_cmd_line(Cmds, [{topcase,{dir,Name}}|SpecList], [Name|Names],
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['MODULE',Mod|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, [{topcase,{Mod,all}}|SpecList], [Mod|Names],
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['CASE',Mod,Case|Cmds], SpecList, Names, Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, [{topcase,{Mod,Case}}|SpecList], [Mod|Names],
		   Param, Trc, Cov, TCCB);
parse_cmd_line(['PARAMETERS',Param|Cmds], SpecList, Names, _Param, Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, Cov, TCCB);
parse_cmd_line(['TRACE',Trc|Cmds], SpecList, Names, Param, _Trc, Cov, TCCB) ->
    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, Cov, TCCB);
parse_cmd_line(['COVER',App,CF,Analyse|Cmds], SpecList, Names, Param, Trc, _Cov, TCCB) ->
    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, {{App,CF}, Analyse}, TCCB);
parse_cmd_line(['TESTCASE_CALLBACK',Mod,Func|Cmds], SpecList, Names, Param, Trc, Cov, _) ->
    parse_cmd_line(Cmds, SpecList, Names, Param, Trc, Cov, {Mod,Func});
parse_cmd_line([Obj|_Cmds], _SpecList, _Names, _Param, _Trc, _Cov, _TCCB) ->
    io:format("~p: Bad argument: ~p\n", [?MODULE,Obj]),
    io:format(" Use the `ts' module to start tests.\n", []),
    io:format(" (If you ARE using `ts', there is a bug in `ts'.)\n", []),
    halt(1);
parse_cmd_line([], SpecList, Names, Param, Trc, Cov, TCCB) ->
    NameList = lists:reverse(Names, [suite]),
    Name = case lists:keysearch(name, 1, NameList) of
	       {value,{name,N}} -> N;
	       false -> hd(NameList)
	   end,
    {lists:reverse(SpecList), cast_to_list(Name), Param, Trc, Cov, TCCB}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cast_to_list(X) -> string()
%% X = list() | atom() | void()
%% Returns a string representation of whatever was input

cast_to_list(X) when is_list(X) -> X;
cast_to_list(X) when is_atom(X) -> atom_to_list(X);
cast_to_list(X) -> lists:flatten(io_lib:format("~w", [X])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% START INTERFACE

start() ->
    start(local).

start(Param) ->
    case gen_server:start({local,?MODULE}, ?MODULE, [Param], []) of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    Other
    end.

start_link(Param) ->
    case gen_server:start_link({local,?MODULE}, ?MODULE, [Param], []) of
	{ok, Pid} ->
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

    %% adding of jobs involves file i/o which may take long time
    %% when running a nfs mounted file system (VxWorks).
    case controller_call(get_target_info) of
	#target_info{os_family=vxworks} ->
	    receive after 30000 -> ready_to_wait end;
	_ ->
	    wait_now
    end,
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
    controller_call({abort_current_testcase,Reason}),
    ok.

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

multiply_timetraps(N) ->
    controller_call({multiply_timetraps,N}).

scale_timetraps(Bool) ->
    controller_call({scale_timetraps,Bool}).

get_timetrap_parameters() ->
    controller_call(get_timetrap_parameters).

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
    controller_call({cover,{App,CoverFile},Analyse}).
cover(App, CoverFile, Exclude, Include, Cross, Export, Analyse) ->
    controller_call({cover,{App,{CoverFile,Exclude,Include,Cross,Export}},Analyse}).

testcase_callback(ModFunc) ->
    controller_call({testcase_callback,ModFunc}).

set_random_seed(Seed) ->
    controller_call({set_random_seed,Seed}).

get_hosts() ->
    get(test_server_hosts).

get_target_os_type() ->
    case whereis(?MODULE) of
	undefined ->
	    %% This is probably called on the target node
	    os:type();
	_pid ->
	    %% This is called on the controller, e.g. from a
	    %% specification clause of a test case
	    #target_info{os_type=OsType} = controller_call(get_target_info),
	    OsType
    end.

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
%% init([Mode])
%% Mode = lazy | error_logger
%% StateFile = string()
%% ReadMode = ignore_errors | halt_on_errors
%%
%% init() is the init function of the test_server's gen_server.
%% When Mode=error_logger: The init function of the test_server's gen_event
%% event handler used as a replacement error_logger when running test_suites.
%%
%% The init function reads the test server state file, to see what test
%% suites were running when the test server was last running, and which
%% flags that were in effect. If no state file is found, or there are
%% errors in it, defaults are used.
%%
%% Mode 'lazy' ignores (and resets to []) any jobs in the state file
%%

init([Param]) ->
    case os:getenv("TEST_SERVER_CALL_TRACE") of
	false ->
	    ok;
	"" ->
	    ok;
	TraceSpec ->
	    test_server_sup:call_trace(TraceSpec)
    end,
    process_flag(trap_exit, true),
    case lists:keysearch(sasl, 1, application:which_applications()) of
	{value,_} ->
	    test_server_h:install();
	false ->
	    ok
    end,
    %% copy format_exception setting from init arg to application environment
    case init:get_argument(test_server_format_exception) of
	{ok,[[TSFE]]} ->
	    application:set_env(test_server, format_exception, list_to_atom(TSFE));
	_ ->
	    ok
    end,
    test_server_sup:cleanup_crash_dumps(),
    State = #state{jobs=[],finish=false},
    put(test_server_free_targets,[]),
    case contact_main_target(Param) of
	{ok,TI} ->
	    ets:new(slave_tab, [named_table,set,public,{keypos,2}]),
	    set_hosts([TI#target_info.host]),
	    {ok,State#state{target_info=TI}};
	{error,Reason} ->
	    {stop,Reason}
    end.
	

%% If the test is to be run at a remote target, this function sets up
%% a socket communication with the target.
contact_main_target(local) ->
    %% When used by a general framework, global registration of
    %% test_server should not be required.
    case os:getenv("TEST_SERVER_FRAMEWORK") of
	false ->
	    %% Local target! The global test_server process implemented by
	    %% test_server.erl will not be started, so we simulate it by
	    %% globally registering this process instead.
	    global:sync(),
	    case global:whereis_name(test_server) of
		undefined ->
		    global:register_name(test_server, self());
		Pid ->
		    case node() of
			N when N == node(Pid) ->
			    io:format(user, "Warning: test_server already running!\n", []),
			    global:re_register_name(test_server,self());
			_ ->
			    ok
		    end
	    end;
	_ ->
	    ok
    end,
    TI = test_server:init_target_info(),
    TargetHost = test_server_sup:hoststr(),
    {ok,TI#target_info{where=local,
		       host=TargetHost,
		       naming=naming(),
		       master=TargetHost}};

contact_main_target(ParameterFile) ->
    case read_parameters(ParameterFile) of
	{ok,Par} ->
	    case test_server_node:start_remote_main_target(Par) of
		{ok,TI} ->
		    {ok,TI};
		{error,Error} ->
		    {error,{could_not_start_main_target,Error}}
	    end;
	{error,Error} ->
	    {error,{could_not_read_parameterfile,Error}}
    end.

read_parameters(File) ->
    case file:consult(File) of
	{ok,Data} ->
	    read_parameters(lists:flatten(Data), #par{naming=naming()});
	Error ->
	    Error
    end.
read_parameters([{type,Type}|Data], Par) -> % mandatory
    read_parameters(Data, Par#par{type=Type});
read_parameters([{target,Target}|Data], Par) -> % mandatory
    read_parameters(Data, Par#par{target=cast_to_list(Target)});
read_parameters([{slavetargets,SlaveTargets}|Data], Par) ->
    read_parameters(Data, Par#par{slave_targets=SlaveTargets});
read_parameters([{longnames,Bool}|Data], Par) ->
    Naming = if Bool->"-name"; true->"-sname" end,
    read_parameters(Data, Par#par{naming=Naming});
read_parameters([{master,{Node,Cookie}}|Data], Par) ->
    read_parameters(Data, Par#par{master=cast_to_list(Node),
				 cookie=cast_to_list(Cookie)});
read_parameters([Other|_Data], _Par) ->
    {error,{illegal_parameter,Other}};
read_parameters([], Par) when Par#par.type==undefined ->
    {error, {missing_mandatory_parameter,type}};
read_parameters([], Par) when Par#par.target==undefined ->
    {error, {missing_mandatory_parameter,target}};
read_parameters([], Par0) ->
    Par =
	case {Par0#par.type, Par0#par.master} of
	    {ose, undefined} ->
		%% Use this node as master and bootserver for target
		%% and slave nodes
		Par0#par{master = atom_to_list(node()),
			 cookie = atom_to_list(erlang:get_cookie())};
	    {ose, _Master} ->
		%% Master for target and slave nodes was defined in parameterfile
		Par0;
	    _ ->
		%% Use target as master for slave nodes,
		%% (No master is used for target)
		Par0#par{master="test_server@" ++ Par0#par.target}
	end,
    {ok,Par}.

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
    Nodes = test_server_node:kill_nodes(State#state.target_info),
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
	    {App,Analyse} -> [{cover,App,Analyse}]
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
			    State#state.testcase_callback, ExtraTools1),
		    NewJobs = [{Name,Pid}|State#state.jobs],
		    {reply, ok, State#state{jobs=NewJobs}};
		{command_line,SpecList} ->
		    Pid = spawn_tester(
			    ?MODULE, do_spec_list,
			    [SpecList,{State#state.multiply_timetraps,
				       State#state.scale_timetraps}],
			    LogDir, Name, State#state.levels,
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
	    lists:foreach(fun({Cli,Fun}) -> Fun(Cli) end,
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
	[] ->
	    Fun(Cli),
	    {reply, {ok,self()}, State};
	_ ->
	    Subscribed = State#state.idle_notify,
	    {reply, {ok,self()},
	     State#state{idle_notify=[{Cli,Fun}|Subscribed]}}
    end;

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
%% handle_call({cover,App,Analyse}, _, State) -> ok | {error,Reason}
%%
%% All modules inn application App are cover compiled
%% Analyse indicates on which level the coverage should be analysed

handle_call({cover,App,Analyse}, _From, State) ->
    {reply,ok,State#state{cover={App,Analyse}}};

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
	    case code:is_loaded(Mod) of
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
			      "WARNING! Callback function ~w:~w/4 undefined.~n~n",
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
    R = test_server_node:stop_node(Name, State#state.target_info),
    {reply, R, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call({stop_node,Name}, _, State) -> ok | {error,Reason}
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
%% Handles exit messages from linked processes. Only test suites and
%% possibly a target client are expected to be linked.
%% When a test suite terminates, it is removed from the job queue.
%% If a target client terminates it means that we lost contact with
%% target. The test_server_ctrl process is terminated, and teminate/2
%% will do the cleanup

handle_info({'EXIT',Pid,Reason}, State) ->
    case lists:keysearch(Pid,2,State#state.jobs) of
	false ->
	    TI = State#state.target_info,
	    case TI#target_info.target_client of
		Pid ->
		    %% The target client died - lost contact with target
		    {stop,{lost_contact_with_target,Reason},State};
		_other ->
		    %% not our problem
		    {noreply,State}
	    end;
	{value,{Name,_}} ->
	    NewJobs = lists:keydelete(Pid, 2, State#state.jobs),
	    case Reason of
		normal ->
		    fine;
		killed ->
		    io:format("Suite ~s was killed\n", [Name]);
		_Other ->
		    io:format("Suite ~s was killed with reason ~p\n",
			      [Name,Reason])
	    end,
	    State2 = State#state{jobs=NewJobs},
	    case NewJobs of
		[] ->
		    lists:foreach(fun({Cli,Fun}) -> Fun(Cli) end,
				  State2#state.idle_notify),
		    case State2#state.finish of
			false ->
			    {noreply,State2#state{idle_notify=[]}};
			_ ->			% true | abort
			    %% test_server:finish() has been called and
			    %% there are no jobs in the job queue =>
			    %% stop the test_server_ctrl
			    {stop,shutdown,State2#state{finish=false}}
		    end;
		_ ->				% pending jobs
		    case State2#state.finish of
			abort ->		% abort test now!
			    lists:foreach(fun({Cli,Fun}) -> Fun(Cli) end,
					  State2#state.idle_notify),
			    {stop,shutdown,State2#state{finish=false}};
			_ ->			% true | false
			    {noreply, State2}
		    end
	    end
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info({tcp,Sock,Bin}, State)
%%
%% Message from remote main target process
%% Only valid message is 'job_proc_killed', which indicates
%% that a process running a test suite was killed

handle_info({tcp,_MainSock,<<1,Request/binary>>}, State) ->
    case binary_to_term(Request) of
	{job_proc_killed,Name,Reason} ->
	    %% The only purpose of this is to inform the user about what
	    %% happened on target.
	    %% The local job proc will soon be killed by the closed socket or
	    %% because the job is finished. Then the above clause ('EXIT') will
	    %% handle the problem.
	    io:format("Suite ~s was killed on remote target with reason"
		      " ~p\n", [Name,Reason]);
	_ ->
	    ignore
    end,
    {noreply,State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info({tcp_closed,Sock}, State)
%%
%% A Socket was closed. This indicates that a node died.
%% This can be
%% *Target node (if remote)
%% *Slave or peer node started by a test suite
%% *Trace controll node

handle_info({tcp_closed,Sock}, State=#state{trc=Sock}) ->
    %% Tracer node died - can't really do anything
    %%! Maybe print something???
    {noreply,State#state{trc=false}};
handle_info({tcp_closed,Sock}, State) ->
    case test_server_node:nodedown(Sock,State#state.target_info) of
	target_died ->
	    %% terminate/2 will do the cleanup
	    {stop,target_died,State};
	_ ->
	    {noreply,State}
    end;

handle_info(_, State) ->
    %% dummy; accept all, do nothing.
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate(Reason, State) -> ok
%% Reason = term()
%%
%% Cleans up when the test_server is terminating. Kills the running
%% test suites (if any) and terminates the remote target (if is exists)

terminate(_Reason, State) ->
    case State#state.trc of
	false -> ok;
	Sock -> test_server_node:stop_tracer_node(Sock)
    end,
    kill_all_jobs(State#state.jobs),
    test_server_node:stop(State#state.target_info),
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
%% spawn_tester(Mod, Func, Args, Dir, Name, Levels,
%%              TestCaseCallback, ExtraTools) -> Pid
%% Mod = atom()
%% Func = atom()
%% Args = [term(),...]
%% Dir = string()
%% Name = string()
%% Levels = {integer(),integer(),integer()}
%% TestCaseCallback = {CBMod,CBFunc} | undefined
%% ExtraTools = [ExtraTool,...]
%% ExtraTool = CoverInfo | TraceInfo | RandomSeed
%%
%% Spawns a test suite execute-process, just an ordinary spawn, except
%% that it will set a lot of dictionary information before starting the
%% named function. Also, the execution is timed and protected by a catch.
%% When the named function is done executing, a summary of the results
%% is printed to the log files.

spawn_tester(Mod, Func, Args, Dir, Name, Levels, TCCallback, ExtraTools) ->
    spawn_link(
      fun() -> init_tester(Mod, Func, Args, Dir, Name, Levels,
			   TCCallback, ExtraTools)
      end).

init_tester(Mod, Func, Args, Dir, Name, {SumLev,MajLev,MinLev},
	    TCCallback, ExtraTools) ->
    process_flag(trap_exit, true),
    put(test_server_name, Name),
    put(test_server_dir, Dir),
    put(test_server_total_time, 0),
    put(test_server_ok, 0),
    put(test_server_failed, 0),
    put(test_server_skipped, {0,0}),
    put(test_server_summary_level, SumLev),
    put(test_server_major_level, MajLev),
    put(test_server_minor_level, MinLev),
    put(test_server_random_seed, proplists:get_value(random_seed, ExtraTools)),
    put(test_server_testcase_callback, TCCallback),
    StartedExtraTools = start_extra_tools(ExtraTools),
    {TimeMy,Result} = ts_tc(Mod, Func, Args),
    put(test_server_common_io_handler, undefined),
    stop_extra_tools(StartedExtraTools),
    case Result of
	{'EXIT',test_suites_done} ->
	    print(25, "DONE, normal exit", []);
	{'EXIT',_Pid,Reason} ->
	    print(1, "EXIT, reason ~p", [Reason]);
	{'EXIT',Reason} ->
	    print(1, "EXIT, reason ~p", [Reason]);
	_Other ->
	    print(25, "DONE", [])
    end,
    Time = TimeMy/1000000,
    SuccessStr =
	case get(test_server_failed) of
	    0 -> "Ok";
	    _ -> "FAILED"
	end,
    {SkippedN,SkipStr} =
	case get(test_server_skipped) of
	    {0,_} -> {0,""};
	    {Skipped,_} -> {Skipped,io_lib:format(", ~p Skipped", [Skipped])}
	end,
    OkN = get(test_server_ok),
    FailedN = get(test_server_failed),
    print(html,"<tr><td></td><td><b>TOTAL</b></td><td></td><td></td>"
	  "<td>~.3fs</td><td><b>~s</b></td><td>~p Ok, ~p Failed~s of ~p</td></tr>\n",
	  [Time,SuccessStr,OkN,FailedN,SkipStr,OkN+FailedN+SkippedN]).

%% timer:tc/3
ts_tc(M, F, A) ->
    Before = ?now,
    Val = (catch apply(M, F, A)),
    After = ?now,
    Elapsed = elapsed_time(Before, After),
    {Elapsed,Val}.

elapsed_time(Before, After) ->
    (element(1,After)*1000000000000 +
     element(2,After)*1000000 + element(3,After)) -
    (element(1,Before)*1000000000000 +
     element(2,Before)*1000000 + element(3,Before)).

start_extra_tools(ExtraTools) ->
    start_extra_tools(ExtraTools, []).
start_extra_tools([{cover,App,Analyse} | ExtraTools], Started) ->
    case cover_compile(App) of
	{ok,AnalyseMods} ->
	    start_extra_tools(ExtraTools,
			      [{cover,App,Analyse,AnalyseMods}|Started]);
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

stop_extra_tools([{cover,App,Analyse,AnalyseMods}|ExtraTools], TestDir) ->
    cover_analyse(App, Analyse, AnalyseMods, TestDir),
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
%% spawn_tester/7, which sets up some necessary dictionary values.

do_spec(SpecName, TimetrapSpec) when is_list(SpecName) ->
    case file:consult(SpecName) of
	{ok,TermList} ->
	    do_spec_list(TermList,TimetrapSpec);
	{error,Reason} ->
	    io:format("Can't open ~s: ~p\n", [SpecName,Reason]),
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
%% spawn_tester/7, which sets up some necessary dictionary values.

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
    io:format("** WARNING: Spec file contains unknown directive ~p\n",
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
    Host=cast_to_list(lists:nth((N rem (length(Hosts)))+1, Hosts)),
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
	{error,_} ->
	    error;
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
remove_conf([{skip_case,{Type,_Ref,_MF,_Cmt}}|Cases],
	    NoConf, Repeats) when Type==conf;
				   Type==make ->
    remove_conf(Cases, NoConf, Repeats);
remove_conf([C|Cases], NoConf, Repeats) ->
    remove_conf(Cases, [C|NoConf], Repeats);
remove_conf([], NoConf, true) ->
    {repeats,lists:reverse(NoConf)};
remove_conf([], NoConf, false) ->
    lists:reverse(NoConf).

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
    case string:rstr(atom_to_list(Mod), "_SUITE") of
	0 -> false;
	_ ->					% test suite
	     case lists:member(Mod, Mods) of
		 true ->  false;
		 false -> true
	     end
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
%% spawn_tester/7, which sets up some necessary dictionary values.
do_test_cases(TopCases, SkipCases,
	      Config, MultiplyTimetrap) when is_integer(MultiplyTimetrap);
					     MultiplyTimetrap == infinity ->
    do_test_cases(TopCases, SkipCases, Config, {MultiplyTimetrap,true});

do_test_cases(TopCases, SkipCases,
	      Config, TimetrapData) when is_list(TopCases),
					 is_tuple(TimetrapData) ->
    start_log_file(),
    case collect_all_cases(TopCases, SkipCases) of
	{error,Why} ->
	    print(1, "Error starting: ~p", [Why]),
	    exit(test_suites_done);
	TestSpec0 ->
	    N = case remove_conf(TestSpec0) of
		    {repeats,_} -> unknown;
		    TS -> length(TS)
		end,
	    put(test_server_cases, N),
	    put(test_server_case_num, 0),
	    TestSpec =
		add_init_and_end_per_suite(TestSpec0, undefined, undefined),
	    TI = get_target_info(),
	    print(1, "Starting test~s", [print_if_known(N, {", ~w test cases",[N]},
							{" (with repeated test cases)",[]})]),
	    test_server_sup:framework_call(report, [tests_start,
						    {get(test_server_name),N}]),
	    print(html,
		  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
		  "<!-- autogenerated by '"++atom_to_list(?MODULE)++"'. -->\n"
		  "<html>\n"
		  "<head><title>Test ~p results</title>\n"
		  "<meta http-equiv=\"cache-control\" content=\"no-cache\">\n"
		  "</head>\n"
		  "<body bgcolor=\"white\" text=\"black\" "
		  "link=\"blue\" vlink=\"purple\" alink=\"red\">"
		  "<h2>Results from test ~p</h2>\n",
		  [get(test_server_name),get(test_server_name)]),
	    print_timestamp(html, "Test started at "),

	    print(html, "<p>Host:<br>\n"),
	    print_who(test_server_sup:hoststr(), test_server_sup:get_username()),
	    print(html, "<br>Used Erlang ~s in <tt>~s</tt>.\n",
		  [erlang:system_info(version), code:root_dir()]),

	    case os:getenv("TEST_SERVER_FRAMEWORK") of
		false ->
		    print(html, "<p>Target:<br>\n"),
		    print_who(TI#target_info.host, TI#target_info.username),
		    print(html, "<br>Used Erlang ~s in <tt>~s</tt>.\n",
			  [TI#target_info.version, TI#target_info.root_dir]);
		_ ->
		    case test_server_sup:framework_call(target_info, []) of
			TargetInfo when is_list(TargetInfo),
			                length(TargetInfo) > 0 ->
			    print(html, "<p>Target:<br>\n"),
			    print(html, "~s\n", [TargetInfo]);
			_ ->
			    ok
		    end
	    end,

	    print(html,
		  "<p><a href=\"~s\">Full textual log</a>\n"
		  "<br><a href=\"~s\">Coverage log</a>\n",
		  [?suitelog_name,?coverlog_name]),
	    print(html,"<p>~s"
		  "<p>\n"
		  "<table border=3 cellpadding=5>"
		  "<tr><th>Num</th><th>Module</th><th>Case</th><th>Log</th>"
		  "<th>Time</th><th>Result</th><th>Comment</th></tr>\n",
		  [print_if_known(N, {"Suite contains ~p test cases.\n",[N]},
				  {"",[]})]),
	    print(major, "=cases         ~p", [get(test_server_cases)]),
	    print(major, "=user          ~s", [TI#target_info.username]),
	    print(major, "=host          ~s", [TI#target_info.host]),

	    %% If there are no hosts specified,use only the local host
	    case controller_call(get_hosts) of
		[] ->
		    print(major, "=hosts         ~s", [TI#target_info.host]),
		    controller_call({set_hosts, [TI#target_info.host]});
		Hosts ->
		    Str = lists:flatten(lists:map(fun(X) -> [X," "] end, Hosts)),
		    print(major, "=hosts         ~s", [Str])
	    end,
	    print(major, "=emulator_vsn  ~s", [TI#target_info.version]),
	    print(major, "=emulator      ~s", [TI#target_info.emulator]),
	    print(major, "=otp_release   ~s", [TI#target_info.otp_release]),
	    print(major, "=started       ~s",
		   [lists:flatten(timestamp_get(""))]),
	    run_test_cases(TestSpec, Config, TimetrapData)
    end;

do_test_cases(TopCase, SkipCases, Config, TimetrapSpec) ->
    %% when not list(TopCase)
    do_test_cases([TopCase], SkipCases, Config, TimetrapSpec).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_log_file() -> ok | exit({Error,Reason})
%% Stem = string()
%%
%% Creates the log directories, the major log file and the html log file.
%% The log files are initialized with some header information.
%%
%% The name of the log directory will be <Name>.LOGS/run.<Date>/ where
%% Name is the test suite name and Date is the current date and time.

start_log_file() ->
    Dir  = get(test_server_dir),
    case file:make_dir(Dir) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	MkDirError ->
	    exit({cant_create_log_dir,{MkDirError,Dir}})
    end,
    TestDir = timestamp_filename_get(filename:join(Dir, "run.")),
    case file:make_dir(TestDir) of
	ok ->
	    ok;
	MkDirError2 ->
	    exit({cant_create_log_dir,{MkDirError2,TestDir}})
    end,

    ok = file:write_file(filename:join(Dir, ?last_file), TestDir ++ "\n"),
    ok = file:write_file(?last_file, TestDir ++ "\n"),

    put(test_server_log_dir_base,TestDir),
    MajorName = filename:join(TestDir, ?suitelog_name),
    HtmlName = MajorName ++ ?html_ext,
    {ok,Major} = file:open(MajorName, [write]),
    {ok,Html}  = file:open(HtmlName,  [write]),
    put(test_server_major_fd,Major),
    put(test_server_html_fd,Html),

    make_html_link(filename:absname(?last_test ++ ?html_ext),
		   HtmlName, filename:basename(Dir)),
    LinkName = filename:join(Dir, ?last_link),
    make_html_link(LinkName ++ ?html_ext, HtmlName,
		   filename:basename(Dir)),

    PrivDir = filename:join(TestDir, ?priv_dir),
    ok = file:make_dir(PrivDir),
    put(test_server_priv_dir,PrivDir++"/"),
    print_timestamp(13,"Suite started at "),
    ok.

make_html_link(LinkName, Target, Explanation) ->
    %% if possible use a relative reference to Target.
    TargetL = filename:split(Target),
    PwdL = filename:split(filename:dirname(LinkName)),
    Href = case lists:prefix(PwdL, TargetL) of
	       true ->
		   filename:join(lists:nthtail(length(PwdL), TargetL));
	       false ->
		   "file:" ++ Target
	   end,
    H = io_lib:format("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
		      "<!-- autogenerated by '"++atom_to_list(?MODULE)++"'. -->\n"
		      "<html>\n"
		      "<head><title>~s</title></head>\n"
		      "<body bgcolor=\"white\" text=\"black\""
		      " link=\"blue\" vlink=\"purple\" alink=\"red\">\n"
		      "<h1>Last test</h1>\n"
		      "<a href=\"~s\">~s</a>~n"
		      "</body>\n</html>\n",
		      [Explanation,Href,Explanation]),
    ok = file:write_file(LinkName, H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_minor_log_file(Mod, Func) -> AbsName
%% Mod = atom()
%% Func = atom()
%% AbsName = string()
%%
%% Create a minor log file for the test case Mod,Func,Args. The log file
%% will be stored in the log directory under the name <Mod>.<Func>.log.
%% Some header info will also be inserted into the log file.

start_minor_log_file(Mod, Func) ->
    LogDir = get(test_server_log_dir_base),
    Name0 = lists:flatten(io_lib:format("~s.~s~s", [Mod,Func,?html_ext])),
    Name = downcase(Name0),
    AbsName = filename:join(LogDir, Name),
    case file:read_file_info(AbsName) of
	{error,_} ->                         %% normal case, unique name
	    start_minor_log_file1(Mod, Func, LogDir, AbsName);
	{ok,_} ->                            %% special case, duplicate names
	    {_,S,Us} = now(),
	    Name1_0 =
		lists:flatten(io_lib:format("~s.~s.~w.~w~s", [Mod,Func,S,
							     trunc(Us/1000),
							     ?html_ext])),
	    Name1 = downcase(Name1_0),
	    AbsName1 = filename:join(LogDir, Name1),
	    start_minor_log_file1(Mod, Func, LogDir, AbsName1)
    end.

start_minor_log_file1(Mod, Func, LogDir, AbsName) ->
    {ok,Fd} = file:open(AbsName, [write]),
    Lev = get(test_server_minor_level)+1000, %% far down in the minor levels
    put(test_server_minor_fd, Fd),
    io:fwrite(Fd,
	      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
	      "<!-- autogenerated by '"++atom_to_list(?MODULE)++"'. -->\n"
	      "<html>\n"
	      "<head><title>"++cast_to_list(Mod)++"</title>\n"
	      "<meta http-equiv=\"cache-control\" content=\"no-cache\">\n"
	      "</head>\n"
	      "<body bgcolor=\"white\" text=\"black\""
	      " link=\"blue\" vlink=\"purple\" alink=\"red\">\n",
	      []),

    SrcListing = downcase(cast_to_list(Mod)) ++ ?src_listing_ext,
    case filelib:is_file(filename:join(LogDir, SrcListing)) of
	true ->
	    print(Lev, "<a href=\"~s#~s\">source code for ~p:~p/1</a>\n",
		  [SrcListing,Func,Mod,Func]);
	false -> ok
    end,

    io:fwrite(Fd, "<pre>\n", []),

% Stupid BUG!
%    case catch apply(Mod, Func, [doc]) of
%	{'EXIT', _Why} -> ok;
%	Comment -> print(Lev, "Comment: ~s~n<br>", [Comment])
%    end,

    AbsName.

stop_minor_log_file() ->
    Fd = get(test_server_minor_fd),
    io:fwrite(Fd, "</pre>\n</body>\n</html>\n", []),
    file:close(Fd),
    put(test_server_minor_fd, undefined).

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

html_convert_modules(TestSpec, _Config) ->
    Mods = html_isolate_modules(TestSpec),
    html_convert_modules(Mods),
    copy_html_files(get(test_server_dir), get(test_server_log_dir_base)).

%% Retrieve a list of modules out of the test spec.

html_isolate_modules(List) -> html_isolate_modules(List, sets:new()).

html_isolate_modules([], Set) -> sets:to_list(Set);
html_isolate_modules([{skip_case,_}|Cases], Set) ->
    html_isolate_modules(Cases, Set);
html_isolate_modules([{conf,_Ref,_Props,{Mod,_Func}}|Cases], Set) ->
    html_isolate_modules(Cases, sets:add_element(Mod, Set));
html_isolate_modules([{Mod,_Case}|Cases], Set) ->
    html_isolate_modules(Cases, sets:add_element(Mod, Set));
html_isolate_modules([{Mod,_Case,_Args}|Cases], Set) ->
    html_isolate_modules(Cases, sets:add_element(Mod, Set)).

%% Given a list of modules, convert each module's source code to HTML.

html_convert_modules([Mod|Mods]) ->
    case code:which(Mod) of
	Path when is_list(Path) ->
	    SrcFile = filename:rootname(Path) ++ ".erl",
	    DestDir = get(test_server_dir),
	    Name = atom_to_list(Mod),
	    DestFile = filename:join(DestDir, downcase(Name) ++ ?src_listing_ext),
	    html_possibly_convert(SrcFile, DestFile),
	    html_convert_modules(Mods);
	_Other -> ok
    end;
html_convert_modules([]) -> ok.

%% Convert source code to HTML if possible and needed.

html_possibly_convert(Src, Dest) ->
    case file:read_file_info(Src) of
	{ok,SInfo} ->
	    case file:read_file_info(Dest) of
		{error,_Reason} ->		% no dest file
		    erl2html2:convert(Src, Dest);
		{ok,DInfo} when DInfo#file_info.mtime < SInfo#file_info.mtime ->
		    erl2html2:convert(Src, Dest);
		{ok,_DInfo} -> ok		% dest file up to date
	    end;
	{error,_Reason} -> ok			% no source code found
    end.

%% Copy all HTML files in InDir to OutDir.

copy_html_files(InDir, OutDir) ->
    Files = filelib:wildcard(filename:join(InDir, "*" ++ ?src_listing_ext)),
    lists:foreach(fun (Src) -> copy_html_file(Src, OutDir) end, Files).

copy_html_file(Src, DestDir) ->
    Dest = filename:join(DestDir, filename:basename(Src)),
    case file:read_file(Src) of
	{ok,Bin} ->
	    ok = file:write_file(Dest, Bin);
	{error,_Reason} ->
	    io:format("File ~p: read failed\n", [Src])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add_init_and_end_per_suite(TestSpec, Mod, Ref) -> NewTestSpec
%%
%% Expands TestSpec with an initial init_per_suite, and a final
%% end_per_suite element, per each discovered suite in the list.

add_init_and_end_per_suite([{make,_,_}=Case|Cases], LastMod, LastRef) ->
    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef)];
add_init_and_end_per_suite([{skip_case,{{Mod,all},_}}=Case|Cases], LastMod, LastRef)
  when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_end_per_suite_and_skip(LastMod, LastRef, Mod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod, NextRef)];
add_init_and_end_per_suite([{skip_case,{{Mod,_},_}}=Case|Cases], LastMod, LastRef)
  when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod, NextRef)];
add_init_and_end_per_suite([{skip_case,{conf,_,{Mod,_},_}}=Case|Cases], LastMod, LastRef)
  when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod, NextRef)];
add_init_and_end_per_suite([{skip_case,_}=Case|Cases], LastMod, LastRef) ->
    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef)];
add_init_and_end_per_suite([{conf,_,_,{Mod,_}}=Case|Cases], LastMod, LastRef)
  when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod, NextRef)];
add_init_and_end_per_suite([{conf,_,_,_}=Case|Cases], LastMod, LastRef) ->
    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef)];
add_init_and_end_per_suite([{Mod,_}=Case|Cases], LastMod, LastRef)
  when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod, NextRef)];
add_init_and_end_per_suite([{Mod,_,_}=Case|Cases], LastMod, LastRef)
  when Mod =/= LastMod ->
    {PreCases, NextMod, NextRef} =
	do_add_init_and_end_per_suite(LastMod, LastRef, Mod),
    PreCases ++ [Case|add_init_and_end_per_suite(Cases, NextMod, NextRef)];
add_init_and_end_per_suite([Case|Cases], LastMod, LastRef)->
    [Case|add_init_and_end_per_suite(Cases, LastMod, LastRef)];
add_init_and_end_per_suite([], _LastMod, undefined) ->
    [];
add_init_and_end_per_suite([], _LastMod, skipped_suite) ->
    [];
add_init_and_end_per_suite([], LastMod, LastRef) ->
    [{conf,LastRef,[],{LastMod,end_per_suite}}].

do_add_init_and_end_per_suite(LastMod, LastRef, Mod) ->
    case code:is_loaded(Mod) of
	false -> code:load_file(Mod);
	_ -> ok
    end,
    {Init,NextMod,NextRef} =
	case erlang:function_exported(Mod, init_per_suite, 1) of
	    true ->
		Ref = make_ref(),
		{[{conf,Ref,[],{Mod,init_per_suite}}],Mod,Ref};
	    false ->
		{[],Mod,undefined}
	end,
    Cases =
	if LastRef==undefined ->
		Init;
	   LastRef==skipped_suite ->
		Init;
	   true ->
		%% Adding end_per_suite here without checking if the
		%% function is actually exported. This is because a
		%% conf case must have an end case - so if it doesn't
		%% exist, it will only fail...
		[{conf,LastRef,[],{LastMod,end_per_suite}}|Init]
	end,
    {Cases,NextMod,NextRef}.

do_add_end_per_suite_and_skip(LastMod, LastRef, Mod) ->
    case LastRef of
	No when No==undefined ; No==skipped_suite ->
	    {[],Mod,skipped_suite};
	_Ref ->
	    {[{conf,LastRef,[],{LastMod,end_per_suite}}],Mod,skipped_suite}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_cases(TestSpec, Config, TimetrapData) -> exit(Result)
%%
%% If remote target, a socket connection is established.
%% Runs the specified tests, then displays/logs the summary.

run_test_cases(TestSpec, Config, TimetrapData) ->

    maybe_open_job_sock(),

    html_convert_modules(TestSpec, Config),

    %%! For readable tracing...
    %%! Config1 = [{data_dir,""},{priv_dir,""},{nodes,[]}],
    %%! run_test_cases_loop(TestSpec, [[]], TimetrapData, [], []),

    run_test_cases_loop(TestSpec, [Config], TimetrapData, [], []),

    maybe_get_privdir(),

    {AllSkippedN,UserSkipN,AutoSkipN,SkipStr} =
	case get(test_server_skipped) of
	    {0,0} -> {0,0,0,""};
	    {US,AS} -> {US+AS,US,AS,io_lib:format(", ~w skipped", [US+AS])}
	end,
    OkN = get(test_server_ok),
    FailedN = get(test_server_failed),
    print(1, "TEST COMPLETE, ~w ok, ~w failed~s of ~w test cases\n",
	  [OkN,FailedN,SkipStr,OkN+FailedN+AllSkippedN]),
    test_server_sup:framework_call(report, [tests_done,
					    {OkN,FailedN,{UserSkipN,AutoSkipN}}]),
    print(major, "=finished      ~s", [lists:flatten(timestamp_get(""))]),
    print(major, "=failed        ~p", [FailedN]),
    print(major, "=successful    ~p", [OkN]),
    print(major, "=user_skipped  ~p", [UserSkipN]),
    print(major, "=auto_skipped  ~p", [AutoSkipN]),
    exit(test_suites_done).

%% If the test is run at a remote target, this function sets up a socket
%% communication with the target for handling this particular job.
maybe_open_job_sock() ->
    TI = get_target_info(),
    case TI#target_info.where of
	local ->
	    %% local target
	    test_server:init_purify();
	MainSock ->
	    %% remote target
	    {ok,LSock} = gen_tcp:listen(0, [binary,
					    {reuseaddr,true},
					    {packet,4},
					    {active,false}]),
	    {ok,Port} = inet:port(LSock),
	    request(MainSock, {job,Port,get(test_server_name)}),
	    case gen_tcp:accept(LSock, ?ACCEPT_TIMEOUT) of
		{ok,Sock} -> put(test_server_ctrl_job_sock, Sock);
		{error,Reason} -> exit({no_contact,Reason})
	    end
    end.

%% If the test is run at a remote target, this function waits for a
%% tar packet containing the privdir created by the test case.
maybe_get_privdir() ->
    case get(test_server_ctrl_job_sock) of
	undefined ->
	    %% local target
	    ok;
	Sock ->
	    %% remote target
	    request(Sock, job_done),
	    gen_tcp:close(Sock)
    end.


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
%% with the given arguments. This function will *always* be called on the host
%% - not on target.
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
%% {skip_case,{Case,Comment}} A normal test case skipped by the user.
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
%% A parallel test case process will always set the dictionary value
%% 'test_server_common_io_handler' to the pid of the main (starting)
%% process. With this value set, the print/3 function will send print
%% messages to the main process instead of writing the data to file
%% (only true for printouts to common log files).
%%
%% If a conf group nested under a parallel group in the test
%% specification should be started, the 'test_server_common_io_handler'
%% value gets set also on the main process. This causes all printouts
%% to common files - both from parallel test cases and from cases
%% executed by the main process - to all end up as messages in the
%% inbox of the main process.
%%
%% During execution of a parallel group (or of a group nested under a
%% parallel group), *any* new test case being started gets registered
%% in a list saved in the dictionary with 'test_server_queued_io' as key.
%% When the top level parallel group is finished (only then can we be
%% sure all parallel test cases have finished and "reported in"), the
%% list of test cases is traversed in order and printout messages from
%% each process - including the main process - are handled in turn. See
%% handle_test_case_io_and_status/0 for details.
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

run_test_cases_loop([{auto_skip_case,{Type,Ref,Case,Comment},SkipMode}|Cases],
		    Config, TimetrapData, Mode, Status) when Type==conf;
							     Type==make ->

    file:set_cwd(filename:dirname(get(test_server_dir))),
    CurrIOHandler = get(test_server_common_io_handler),
    %% check and update the mode for test case execution and io msg handling
    case {curr_ref(Mode),check_props(parallel, Mode)} of
	{Ref,Ref} ->
	    case check_props(parallel, tl(Mode)) of
		false ->
		    %% this is a skipped end conf for a top level parallel group,
		    %% buffered io can be flushed
		    handle_test_case_io_and_status(),
		    set_io_buffering(undefined),
		    {Mod,Func} = skip_case(auto, Ref, 0, Case, Comment, false, SkipMode),
		    test_server_sup:framework_call(report, [tc_auto_skip,{?pl2a(Mod),Func,Comment}]),
		    run_test_cases_loop(Cases, Config, TimetrapData, tl(Mode),
					delete_status(Ref, Status));
		_ ->
		    %% this is a skipped end conf for a parallel group nested under a
		    %% parallel group (io buffering is active)
		    wait_for_cases(Ref),
		    {Mod,Func} = skip_case(auto, Ref, 0, Case, Comment, true, SkipMode),
		    test_server_sup:framework_call(report, [tc_auto_skip,{?pl2a(Mod),Func,Comment}]),
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
					delete_status(Ref, Status))
	    end;
	{Ref,false} ->
	    %% this is a skipped end conf for a non-parallel group that's not
	    %% nested under a parallel group
	    {Mod,Func} = skip_case(auto, Ref, 0, Case, Comment, false, SkipMode),
	    test_server_sup:framework_call(report, [tc_auto_skip,{?pl2a(Mod),Func,Comment}]),
	    run_test_cases_loop(Cases, Config, TimetrapData, tl(Mode),
				delete_status(Ref, Status));
	{Ref,_} ->
	    %% this is a skipped end conf for a non-parallel group nested under
	    %% a parallel group (io buffering is active)
	    {Mod,Func} = skip_case(auto, Ref, 0, Case, Comment, true, SkipMode),
	    test_server_sup:framework_call(report, [tc_auto_skip,{?pl2a(Mod),Func,Comment}]),
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
	    {Mod,Func} = skip_case(auto, Ref, 0, Case, Comment, false, SkipMode),
	    test_server_sup:framework_call(report, [tc_auto_skip,{?pl2a(Mod),Func,Comment}]),
	    run_test_cases_loop(Cases, Config, TimetrapData, [conf(Ref,[])|Mode], Status);
	{_,Ref0} when is_reference(Ref0) ->
	    %% this is a skipped start conf for a group nested under a parallel group
	    %% and if this is the first nested group, io buffering must be activated
	    if CurrIOHandler == undefined ->
		    set_io_buffering({Ref,self()});
	       true ->
		    ok
	    end,
	    {Mod,Func} = skip_case(auto, Ref, 0, Case, Comment, true, SkipMode),
	    test_server_sup:framework_call(report, [tc_auto_skip,{?pl2a(Mod),Func,Comment}]),
	    run_test_cases_loop(Cases, Config, TimetrapData, [conf(Ref,[])|Mode], Status)
    end;

run_test_cases_loop([{auto_skip_case,{Case,Comment},SkipMode}|Cases],
		    Config, TimetrapData, Mode, Status) ->
    {Mod,Func} = skip_case(auto, undefined, get(test_server_case_num)+1, Case, Comment,
			   (undefined /= get(test_server_common_io_handler)), SkipMode),
    test_server_sup:framework_call(report, [tc_auto_skip,{?pl2a(Mod),Func,Comment}]),
    run_test_cases_loop(Cases, Config, TimetrapData, Mode,
			update_status(skipped, Mod, Func, Status));

run_test_cases_loop([{skip_case,{conf,Ref,Case,Comment}}|Cases0],
		    Config, TimetrapData, Mode, Status) ->
    {Mod,Func} = skip_case(user, Ref, 0, Case, Comment,
			   (undefined /= get(test_server_common_io_handler))),
    {Cases,Config1} =
	case curr_ref(Mode) of
	    Ref ->
		%% skipped end conf
		{Cases0,tl(Config)};
	    _ ->
		%% skipped start conf
		{skip_cases_upto(Ref, Cases0, Comment, conf, Mode),Config}
	end,
    test_server_sup:framework_call(report, [tc_user_skip,{?pl2a(Mod),Func,Comment}]),
    run_test_cases_loop(Cases, Config1, TimetrapData, Mode,
			update_status(skipped, Mod, Func, Status));

run_test_cases_loop([{skip_case,{Case,Comment}}|Cases],
		    Config, TimetrapData, Mode, Status) ->
    {Mod,Func} = skip_case(user, undefined, get(test_server_case_num)+1, Case, Comment,
			   (undefined /= get(test_server_common_io_handler))),
    test_server_sup:framework_call(report, [tc_user_skip,{?pl2a(Mod),Func,Comment}]),
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
			%% this is an end conf for a top level parallel group, collect
			%% results from the test case processes and calc total time
			OkSkipFail = handle_test_case_io_and_status(),
			file:set_cwd(filename:dirname(get(test_server_dir))),
			After = ?now,
			Before = get(test_server_parallel_start_time),
			Elapsed = elapsed_time(Before, After)/1000000,
			put(test_server_total_time, Elapsed),
			{false,tl(Mode0),undefined,Elapsed,
			 update_status(Ref, OkSkipFail, Status)};
		    _ ->
			%% this is an end conf for a parallel group nested under a
			%% parallel group (io buffering is active)
			OkSkipFail = wait_for_cases(Ref),
			queue_test_case_io(Ref, self(), 0, Mod, Func),
			Elapsed = elapsed_time(conf_start(Ref, Mode0),?now)/1000000,
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
		 elapsed_time(conf_start(Ref, Mode0),?now)/1000000, Status};
	    {Ref,_} ->
		%% this is an end conf for a non-parallel group nested under
		%% a parallel group (io buffering is active)
		queue_test_case_io(Ref, self(), 0, Mod, Func),
		Elapsed = elapsed_time(conf_start(Ref, Mode0),?now)/1000000,
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
			    %% 4. use now() values for seed
			    case os:getenv("TS_RANDOM_SEED") of
				Undef when Undef == false ; Undef == "undefined" ->
				    case get(test_server_random_seed) of
					undefined -> Seed;
					TSRS -> TSRS
				    end;
				NumStr ->
				    %% Ex: "123 456 789" or "123,456,789" -> {123,456,789}
				    list_to_tuple([list_to_integer(NS) ||
						   NS <- string:tokens(NumStr, [$ ,$:,$,])])
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
					   {Ok,_,_} when length(Ok) > 0 ->
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
			       [{tc_group_properties,[Shuffle|delete_shuffle(Props)]}]
		       end;
		  not StartConf ->
		       {TcOk,TcSkip,TcFail} = get_tc_results(Status1),
		       [{tc_group_properties,get_props(Mode0)},
			{tc_group_result,[{ok,TcOk},{skipped,TcSkip},{failed,TcFail}]}]
	       end,
    ActualCfg =
	update_config(hd(Config), [{priv_dir,get(test_server_priv_dir)},
				   {data_dir,get_data_dir(Mod)}] ++ CfgProps),
    CurrMode = curr_mode(Ref, Mode0, Mode),

    ConfCaseResult = run_test_case(Ref, 0, Mod, Func, [ActualCfg], skip_init, target,
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
		    print(minor, "~n*** ~p returned bad elements in Config: ~p.~n",
			  [Func,Bad]),
		    Reason = {failed,{Mod,init_per_suite,bad_return}},
		    Cases2 = skip_cases_upto(Ref, Cases, Reason, conf, CurrMode),
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
	    print(minor, "~n*** ~p failed in ~p. Reason: ~p~n", [FwMod,FwFunc,Reason]),
	    print(1, "~p failed in ~p. Reason: ~p~n", [FwMod,FwFunc,Reason]),
	    exit(framework_error);
	{_,Fail,_} when element(1,Fail) == 'EXIT';
			element(1,Fail) == timetrap_timeout;
			element(1,Fail) == failed ->
	    {Cases2,Config1} =
		if StartConf ->
			ReportAbortRepeat(failed),
			print(minor, "~n*** ~p failed.~n"
			      "    Skipping all cases.", [Func]),
			Reason = {failed,{Mod,Func,Fail}},
			{skip_cases_upto(Ref, Cases, Reason, conf, CurrMode),Config};
		   not StartConf ->
			ReportRepeatStop(),
			print_conf_time(ConfTime),
			{Cases,tl(Config)}
		end,
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases2, Config1, TimetrapData, Mode,
				delete_status(Ref, Status2));
	{died,Why,_} when Func == init_per_suite ->
	    print(minor, "~n*** Unexpected exit during init_per_suite.~n", []),
	    Reason = {failed,{Mod,init_per_suite,Why}},
	    Cases2 = skip_cases_upto(Ref, Cases, Reason, conf, CurrMode),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases2, Config, TimetrapData, Mode,
				delete_status(Ref, Status2));
	{_,{Skip,Reason},_} when StartConf and ((Skip==skip) or (Skip==skipped)) ->
	    ReportAbortRepeat(skipped),
	    print(minor, "~n*** ~p skipped.~n"
		  "    Skipping all cases.", [Func]),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(skip_cases_upto(Ref, Cases, Reason, conf, CurrMode),
				Config, TimetrapData, Mode,
				delete_status(Ref, Status2));
	{_,{skip_and_save,Reason,_SavedConfig},_} when StartConf ->
	    ReportAbortRepeat(skipped),
	    print(minor, "~n*** ~p skipped.~n"
		  "    Skipping all cases.", [Func]),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(skip_cases_upto(Ref, Cases, Reason, conf, CurrMode),
				Config, TimetrapData, Mode,
				delete_status(Ref, Status2));
	{_,_Other,_} when Func == init_per_suite ->
	    print(minor, "~n*** init_per_suite failed to return a Config list.~n", []),
	    Reason = {failed,{Mod,init_per_suite,bad_return}},
	    Cases2 = skip_cases_upto(Ref, Cases, Reason, conf, CurrMode),
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
	    %% check if return_group_result is set (ok, skipped or failed) and
	    %% if so return the value to the group "above" so that result may be
	    %% used for evaluating repeat_until_*
	    Status3 =
		case lists:keysearch(return_group_result, 1, Opts) of
		    {value,{_,GroupResult}} ->
			update_status(GroupResult, group_result, Func,
				      delete_status(Ref, Status2));
		    false ->
			delete_status(Ref, Status2)
		end,
	    print_conf_time(ConfTime),
	    ReportRepeatStop(),
	    set_io_buffering(IOHandler),
	    stop_minor_log_file(),
	    run_test_cases_loop(Cases, tl(Config), TimetrapData, Mode, Status3)
    end;

run_test_cases_loop([{make,Ref,{Mod,Func,Args}}|Cases0], Config, TimetrapData, Mode, Status) ->
    case run_test_case(Ref, 0, Mod, Func, Args, skip_init, host, TimetrapData) of
	{_,Why={'EXIT',_},_} ->
 	    print(minor, "~n*** ~p failed.~n"
 		  "    Skipping all cases.", [Func]),
	    Reason = {failed,{Mod,Func,Why}},
	    Cases = skip_cases_upto(Ref, Cases0, Reason, conf, Mode),
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
	update_config(hd(Config), [{priv_dir,get(test_server_priv_dir)},
				   {data_dir,get_data_dir(Mod)}]),
    run_test_cases_loop([{Mod,Case,[ActualCfg]}|Cases], Config,
			TimetrapData, Mode, Status);

run_test_cases_loop([{Mod,Func,Args}|Cases], Config, TimetrapData, Mode, Status) ->
    Num = put(test_server_case_num, get(test_server_case_num)+1),
    %% check the current execution mode and save info about the case if
    %% detected that printouts to common log files is handled later
    case check_prop(parallel, Mode) of
	false ->
	    case get(test_server_common_io_handler) of
		undefined ->
		    %% io printouts are written to straight to file
		    ok;
		_ ->
		    %% io messages are buffered, put test case in queue
		    queue_test_case_io(undefined, self(), Num+1, Mod, Func)
	    end;
	_ ->
	    ok
    end,
    case run_test_case(undefined, Num+1, Mod, Func, Args,
		       run_init, target, TimetrapData, Mode) of
	%% callback to framework module failed, exit immediately
	{_,{framework_error,{FwMod,FwFunc},Reason},_} ->
	    print(minor, "~n*** ~p failed in ~p. Reason: ~p~n", [FwMod,FwFunc,Reason]),
	    print(1, "~p failed in ~p. Reason: ~p~n", [FwMod,FwFunc,Reason]),
	    stop_minor_log_file(),
	    exit(framework_error);
	%% sequential execution of test case finished
	{Time,RetVal,_} ->
	    {Failed,Status1} =
		case Time of
		    died ->
			{true,update_status(failed, Mod, Func, Status)};
		    _ when is_tuple(RetVal) ->
			case element(1, RetVal) of
			    R when R=='EXIT'; R==failed ->
				{true,update_status(failed, Mod, Func, Status)};
			    R when R==skip; R==skipped ->
				{false,update_status(skipped, Mod, Func, Status)};
			    _ ->
				{false,update_status(ok, Mod, Func, Status)}
			end;
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
			    print(minor, "~n*** ~p failed.~n"
				  "    Skipping all other cases in sequence.", [Func]),
			    Reason = {failed,{Mod,Func}},
			    Cases2 = skip_cases_upto(Ref, Cases, Reason, tc, Mode),
			    stop_minor_log_file(),
			    run_test_cases_loop(Cases2, Config, TimetrapData, Mode, Status1)
		    end
	    end;
	%% the test case is being executed in parallel with the main process (and
	%% other test cases) and Pid is the dedicated process executing the case
	Pid ->
	    %% io from Pid will be buffered in the main process inbox and handled
	    %% later, so we have to save info about the case
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
    OkSkipFail.

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
    case code:which(Mod) of
	non_existing ->
	    print(12, "The module ~p is not loaded", [Mod]),
	    [];
	FullPath ->
	    filename:dirname(FullPath) ++ "/" ++ cast_to_list(Mod) ++
		?data_dir_suffix
    end.

print_conf_time(0) ->
    ok;
print_conf_time(ConfTime) ->
    print(major, "=group_time    ~.3fs", [ConfTime]),
    print(minor, "~n=== Total execution time of group: ~.3fs~n", [ConfTime]).

print_props(_, []) ->
    ok;
print_props(true, Props) ->
    print(major, "=group_props   ~p", [Props]),
    print(minor, "Group properties: ~p~n", [Props]);
print_props(_, _) ->
    ok.

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
    shuffle_cases(Ref, Cases, ?now);

shuffle_cases(Ref, [{conf,Ref,_,_}=Start | Cases], Seed) ->
    {N,CasesToShuffle,Rest} = cases_to_shuffle(Ref, Cases),
    ShuffledCases = random_order(N, random:uniform_s(N, Seed), CasesToShuffle, []),
    [Start|ShuffledCases] ++ Rest.

cases_to_shuffle(Ref, Cases) ->
    cases_to_shuffle(Ref, Cases, 1, []).

cases_to_shuffle(Ref, [{conf,Ref,_,_} | _]=Cs, N, Ix) ->          % end
    {N-1,Ix,Cs};
cases_to_shuffle(Ref, [{skip_case,{_,Ref,_,_}} | _]=Cs, N, Ix) -> % end
    {N-1,Ix,Cs};

cases_to_shuffle(Ref, [{conf,Ref1,_,_}=C | Cs], N, Ix) ->          % nested group
    {Cs1,Rest} = get_subcases(Ref1, Cs, []),
    cases_to_shuffle(Ref, Rest, N+1, [{N,[C|Cs1]} | Ix]);
cases_to_shuffle(Ref, [{skip_case,{_,Ref1,_,_}}=C | Cs], N, Ix) -> % nested group
    {Cs1,Rest} = get_subcases(Ref1, Cs, []),
    cases_to_shuffle(Ref, Rest, N+1, [{N,[C|Cs1]} | Ix]);

cases_to_shuffle(Ref, [C | Cs], N, Ix) ->
    cases_to_shuffle(Ref, Cs, N+1, [{N,[C]} | Ix]).

get_subcases(SubRef, [{conf,SubRef,_,_}=C | Cs], SubCs) ->
    {lists:reverse([C|SubCs]),Cs};
get_subcases(SubRef, [{skip_case,{_,SubRef,_,_}}=C | Cs], SubCs) ->
    {lists:reverse([C|SubCs]),Cs};
get_subcases(SubRef, [C|Cs], SubCs) ->
    get_subcases(SubRef, Cs, [C|SubCs]).

random_order(1, {_Pos,Seed}, [{_Ix,CaseOrGroup}], Shuffled) ->
    %% save current seed to be used if test cases are repeated
    put(test_server_curr_random_seed, Seed),
    Shuffled++CaseOrGroup;
random_order(N, {Pos,NewSeed}, IxCases, Shuffled) ->
    {First,[{_Ix,CaseOrGroup}|Rest]} = lists:split(Pos-1, IxCases),
    random_order(N-1, random:uniform_s(N-1, NewSeed),
		 First++Rest, Shuffled++CaseOrGroup).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% skip_case(Type, Ref, CaseNum, Case, Comment, SendSync) -> {Mod,Func}
%%
%% Prints info about a skipped case in the major and html log files.
%% SendSync determines if start and finished messages must be sent so
%% that the printouts can be buffered and handled in order with io from
%% parallel processes.

skip_case(Type, Ref, CaseNum, Case, Comment, SendSync) ->
    skip_case(Type, Ref, CaseNum, Case, Comment, SendSync, []).

skip_case(Type, Ref, CaseNum, Case, Comment, SendSync, Mode) ->
    MF = {Mod,Func} = case Case of
			  {M,F,_A} -> {M,F};
			  {M,F} -> {M,F}
		      end,
    if SendSync ->
	    queue_test_case_io(Ref, self(), CaseNum, Mod, Func),
	    self() ! {started,Ref,self(),CaseNum,Mod,Func},
	    skip_case1(Type, CaseNum, Mod, Func, Comment, Mode),
	    self() ! {finished,Ref,self(),CaseNum,Mod,Func,skipped,{0,skipped,[]}};
       not SendSync ->
	    skip_case1(Type, CaseNum, Mod, Func, Comment, Mode)
    end,
    MF.

skip_case1(Type, CaseNum, Mod, Func, Comment, Mode) ->
    {{Col0,Col1},_} = get_font_style((CaseNum > 0), Mode),
    ResultCol = if Type == auto -> "#ffcc99";
		   Type == user -> "#ff9933"
		end,

    Comment1 = reason_to_string(Comment),

    print(major, "~n=case          ~p:~p", [Mod,Func]),
    print(major, "=started         ~s", [lists:flatten(timestamp_get(""))]),
    print(major, "=result          skipped: ~s", [Comment1]),
    print(2,"*** Skipping test case #~w ~p ***", [CaseNum,{Mod,Func}]),
    print(html,
	  "<tr valign=top>"
	  "<td>" ++ Col0 ++ "~s" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "~p" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "~p" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "< >" ++ Col1 ++ "</td>"
	  "<td>" ++ Col0 ++ "0.000s" ++ Col1 ++ "</td>"
	  "<td><font color=\"~s\">SKIPPED</font></td>"
	  "<td>~s</td></tr>\n",
	  [num2str(CaseNum),Mod,Func,ResultCol,Comment1]),
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
%% skip_cases_upto(Ref, Cases, Reason, Origin, Mode) -> Cases1
%%
%% Mark all cases tagged with Ref as skipped.

skip_cases_upto(Ref, Cases, Reason, Origin, Mode) ->
    {_,Modified,Rest} = modify_cases_upto(Ref, {skip,Reason,Origin,Mode}, Cases),
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
		      ({skip_case,{_,R,_,_}}) when R == Ref -> true;
		      (_) -> false
		   end, Cases) of
	true ->
	    modify_cases_upto1(Ref, ModOp, Cases, Orig, Alt);
	false ->
	    {[],[],Cases}
    end.

%% next case is a conf with same ref, must be end conf = we're done
modify_cases_upto1(Ref, {skip,Reason,conf,Mode}, [{conf,Ref,_Props,MF}|T], Orig, Alt) ->
    {Orig,[{auto_skip_case,{conf,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {copy,NewRef}, [{conf,Ref,Props,MF}=C|T], Orig, Alt) ->
    {[C|Orig],[{conf,NewRef,update_repeat(Props),MF}|Alt],T};

%% we've skipped all remaining cases in a sequence
modify_cases_upto1(Ref, {skip,_,tc,_}, [{conf,Ref,_Props,_MF}|_]=Cs, Orig, Alt) ->
    {Orig,Alt,Cs};

%% next is a make case
modify_cases_upto1(Ref, {skip,Reason,_,Mode}, [{make,Ref,MF}|T], Orig, Alt) ->
    {Orig,[{auto_skip_case,{make,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {copy,NewRef}, [{make,Ref,MF}=M|T], Orig, Alt) ->
    {[M|Orig],[{make,NewRef,MF}|Alt],T};

%% next case is a user skipped end conf with the same ref = we're done
modify_cases_upto1(Ref, {skip,Reason,_,Mode}, [{skip_case,{Type,Ref,MF,_Cmt}}|T], Orig, Alt) ->
    {Orig,[{auto_skip_case,{Type,Ref,MF,Reason},Mode}|Alt],T};
modify_cases_upto1(Ref, {copy,NewRef}, [{skip_case,{Type,Ref,MF,Cmt}}=C|T], Orig, Alt) ->
    {[C|Orig],[{skip_case,{Type,NewRef,MF,Cmt}}|Alt],T};

%% next is a skip_case, could be one test case or 'all' in suite, we must proceed
modify_cases_upto1(Ref, ModOp, [{skip_case,{_F,_Cmt}}=MF|T], Orig, Alt) ->
    modify_cases_upto1(Ref, ModOp, T, [MF|Orig], [MF|Alt]);

%% next is a normal case (possibly in a sequence), mark as skipped, or copy, and proceed
modify_cases_upto1(Ref, {skip,Reason,_,Mode}=Op, [{_M,_F}=MF|T], Orig, Alt) ->
    modify_cases_upto1(Ref, Op, T, Orig, [{auto_skip_case,{MF,Reason},Mode}|Alt]);
modify_cases_upto1(Ref, CopyOp, [{_M,_F}=MF|T], Orig, Alt) ->
    modify_cases_upto1(Ref, CopyOp, T, [MF|Orig], [MF|Alt]);

%% next is some other case, ignore or copy
modify_cases_upto1(Ref, {skip,_,_,_}=Op, [_|T], Orig, Alt) ->
    modify_cases_upto1(Ref, Op, T, Orig, Alt);
modify_cases_upto1(Ref, CopyOp, [C|T], Orig, Alt) ->
    modify_cases_upto1(Ref, CopyOp, T, [C|Orig], [C|Alt]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set_io_buffering(IOHandler) -> PrevIOHandler
%%
%% Save info about current process (always the main process) buffering
%% io printout messages from parallel test case processes (*and* possibly
%% also the main process). If the value is the default 'undefined',
%% io is not buffered but printed directly to file (see print/3).

set_io_buffering(IOHandler) ->
    put(test_server_common_io_handler, IOHandler).

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
	    %% to handle buffered io messages later
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
	    print(1, "Error! Process for test case #~p (~p:~p) died! Reason: ~p",
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
%% written to sequentially. The test case processes send print requests
%% to the main (starting) process (the same process executing
%% run_test_cases_loop/4), which handles these requests in the same
%% order that the test case processes were started.
%%
%% An io session is always started with a {started,Ref,Pid,Num,Mod,Func}
%% message and terminated with {finished,Ref,Pid,Num,Mod,Func,Result,RetVal}.
%% The result shipped with the finished message from a parallel process
%% is used to update status data of the current test run. An 'EXIT'
%% message from each parallel test case process (after finishing and
%% terminating) is also received and handled here.
%%
%% During execution of a parallel group, any cases (conf or normal)
%% belonging to a nested group will also get its io printouts buffered.
%% This is necessary to get the major and html log files written in
%% correct sequence. This function handles also the print messages
%% generated by nested group cases that have been executed sequentially
%% by the main process (note that these cases do not generate 'EXIT'
%% messages, only 'start', 'print' and 'finished' messages).
%%
%% See the header comment for run_test_cases_loop/4 for more
%% info about IO handling.
%%
%% Note: It is important that the type of messages handled here
%% do not get consumated by test_server:run_test_case_msgloop/5
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
    %% retreive the start message for the current io session (= testcase)
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
	    handle_io_and_exits(self(), CurrPid, CaseNum, Mod, Func, Cases),
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
	%% end of io session from test case executed by main process
	{finished,_,Main,CaseNum,Mod,Func,Result,_RetVal} ->
	    {Result,{Mod,Func}};
	%% end of io session from test case executed by parallel process
	{finished,_,CurrPid,CaseNum,Mod,Func,Result,RetVal} ->
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

	%% print to common log file
	{print,CurrPid,Detail,Msg} ->
	    output({Detail,Msg}, internal),
	    handle_io_and_exits(Main, CurrPid, CaseNum, Mod, Func, Cases);

	%% unexpected termination of test case process
	{'EXIT',TCPid,Reason} when Reason /= normal ->
	    {value,{_,_,Num,M,F}} = lists:keysearch(TCPid, 2, Cases),
	    print(1, "Error! Process for test case #~p (~p:~p) died! Reason: ~p",
		  [Num, M, F, Reason]),
	    exit({unexpected_termination,{Num,M,F},{TCPid,Reason}})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_case(Ref, Num, Mod, Func, Args, RunInit,
%%               Where, TimetrapData, Mode) -> RetVal
%%
%% Creates the minor log file and inserts some test case specific headers
%% and footers into the log files. If a remote target is used, the test
%% suite (binary) and the content of data_dir is sent. Then the test case
%% is executed and the result is printed to the log files (also info
%% about lingering processes & slave nodes in the system is presented).
%%
%% RunInit decides if the per test case init is to be run (true for all
%% but conf cases).
%%
%% Where specifies if the test case should run on target or on the host.
%% (Note that 'make' test cases always run on host).
%%
%% Mode specifies if the test case should be executed by a dedicated,
%% parallel, process rather than sequentially by the main process. If
%% the former, the new process is spawned and the dictionary of the main
%% process is copied to the test case process.
%%
%% RetVal is the result of executing the test case. It contains info
%% about the execution time and the return value of the test case function.

run_test_case(Ref, Num, Mod, Func, Args, RunInit, Where, TimetrapData) ->
    file:set_cwd(filename:dirname(get(test_server_dir))),
    run_test_case1(Ref, Num, Mod, Func, Args, RunInit, Where,
		   TimetrapData, [], [], self()).

run_test_case(Ref, Num, Mod, Func, Args, skip_init, Where, TimetrapData, Mode) ->
    %% a conf case is always executed by the main process
    run_test_case1(Ref, Num, Mod, Func, Args, skip_init, Where,
		   TimetrapData, [], Mode, self());

run_test_case(Ref, Num, Mod, Func, Args, RunInit, Where, TimetrapData, Mode) ->
    file:set_cwd(filename:dirname(get(test_server_dir))),
    case check_prop(parallel, Mode) of
	false ->
	    %% this is a sequential test case
	    run_test_case1(Ref, Num, Mod, Func, Args, RunInit, Where,
			   TimetrapData, [], Mode, self());
	_Ref ->
	    %% this a parallel test case, spawn the new process
	    Main = self(),
	    {dictionary,State} = process_info(self(), dictionary),
	    spawn_link(fun() ->
			   run_test_case1(Ref, Num, Mod, Func, Args, RunInit, Where,
					  TimetrapData, State, Mode, Main)
		       end)
    end.

run_test_case1(Ref, Num, Mod, Func, Args, RunInit, Where,
	       TimetrapData, State, Mode, Main) ->
    %% if this runs on a parallel test case process,
    %% copy the dictionary from the main process
    do_if_parallel(Main, fun() -> process_flag(trap_exit, true) end, ok),
    CopyDict = fun() -> lists:foreach(fun({Key,Val}) -> put(Key, Val) end, State) end,
    do_if_parallel(Main, CopyDict, ok),
    do_if_parallel(Main, fun() -> put(test_server_common_io_handler, {tc,Main}) end, ok),
    %% if io is being buffered, send start io session message
    %% (no matter if case runs on parallel or main process)
    case get(test_server_common_io_handler) of
	undefined -> ok;
	_ -> Main ! {started,Ref,self(),Num,Mod,Func}
    end,
    TSDir = get(test_server_dir),
    case Where of
	target ->
	    maybe_send_beam_and_datadir(Mod);
	host ->
	    ok
    end,
    test_server_sup:framework_call(report, [tc_start,{?pl2a(Mod),Func}]),
    print(major, "=case          ~p:~p", [Mod, Func]),
    MinorName = start_minor_log_file(Mod, Func),
    print(minor, "<a name=top></a>", []),
    MinorBase = filename:basename(MinorName),
    print(major, "=logfile       ~s", [filename:basename(MinorName)]),
    print_props((RunInit==skip_init), get_props(Mode)),
    print(major, "=started       ~s", [lists:flatten(timestamp_get(""))]),
    {{Col0,Col1},Style} = get_font_style((RunInit==run_init), Mode),
    print(html,	  "<tr valign=top><td>" ++ Col0 ++ "~s" ++ Col1 ++ "</td>"
		  "<td>" ++ Col0 ++ "~p" ++ Col1 ++ "</td>"
		  "<td><a href=\"~s\">~p</a></td>"
		  "<td><a href=\"~s#top\"><</a> <a href=\"~s#end\">></a></td>",
		  [num2str(Num),Mod,MinorBase,Func,MinorBase,MinorBase]),

    do_if_parallel(Main, ok, fun erlang:yield/0),
    %% run the test case
    {Result,DetectedFail,ProcsBefore,ProcsAfter} =
	run_test_case_apply(Num, Mod, Func, Args, get_name(Mode),
			    RunInit, Where, TimetrapData),
    {Time,RetVal,Loc,Opts,Comment} =
	case Result of
	    Normal={_Time,_RetVal,_Loc,_Opts,_Comment} -> Normal;
	    {died,DReason,DLoc,DCmt} -> {died,DReason,DLoc,[],DCmt}
	end,

    print(minor, "<a name=end></a>", []),
    print_timestamp(minor, "Ended at "),
    print(major, "=ended         ~s", [lists:flatten(timestamp_get(""))]),

    do_if_parallel(Main, ok, fun() -> file:set_cwd(filename:dirname(TSDir)) end),

    %% call the appropriate progress function clause to print the results to log
    Status =
	case {Time,RetVal} of
	    {died,{timetrap_timeout,TimetrapTimeout}} ->
		progress(failed, Num, Mod, Func, Loc,
			 timetrap_timeout, TimetrapTimeout, Comment, Style);
	    {died,Reason} ->
		progress(failed, Num, Mod, Func, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',{Skip,Reason}}} when Skip==skip; Skip==skipped ->
		progress(skip, Num, Mod, Func, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',_Pid,{Skip,Reason}}} when Skip==skip; Skip==skipped ->
		progress(skip, Num, Mod, Func, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',_Pid,Reason}} ->
		progress(failed, Num, Mod, Func, Loc, Reason,
			 Time, Comment, Style);
	    {_,{'EXIT',Reason}} ->
		progress(failed, Num, Mod, Func, Loc, Reason,
			 Time, Comment, Style);
	    {_, {failed, Reason}} ->
		progress(failed, Num, Mod, Func, Loc, Reason,
			 Time, Comment, Style);
	    {_, {Skip, Reason}} when Skip==skip; Skip==skipped ->
		progress(skip, Num, Mod, Func, Loc, Reason,
			 Time, Comment, Style);
	    {Time,RetVal} ->
		case DetectedFail of
		    [] ->
			progress(ok, Num, Mod, Func, Loc, RetVal,
				 Time, Comment, Style);

		    Reason ->
			progress(failed, Num, Mod, Func, Loc, Reason,
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
			{'EXIT',_}=Exit ->
			    print(minor,
				  "WARNING: There might be slavenodes left in the"
				  " system. I tried to kill them, but I failed: ~p\n",
				  [Exit]);
			[] -> ok;
			List ->
			    print(minor, "WARNING: ~w slave nodes in system after test"++
				  "case. Tried to killed them.~n"++
				  "         Names:~p",
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
    check_new_crash_dumps(Where),

    %% if io is being buffered, send finished message
    %% (no matter if case runs on parallel or main process)
    case get(test_server_common_io_handler) of
	undefined -> ok;
	_ -> Main ! {finished,Ref,self(),Num,Mod,Func,
		     ?mod_result(Status),{Time,RetVal,Opts}}
    end,
    {Time,RetVal,Opts}.


%%--------------------------------------------------------------------
%% various help functions

%% Call If() if we're on parallel process, or
%% call Else() if we're on main process
do_if_parallel(Pid, If, Else) ->
    case self() of
	Pid ->
	    if is_function(Else) -> Else();
	       true -> Else
	    end;
	_ ->
	    if is_function(If) -> If();
	       true -> If
	    end
    end.

num2str(0) -> "";
num2str(N) -> integer_to_list(N).

%% If remote target, this function sends the test suite (if not already sent)
%% and the content of datadir til target.
maybe_send_beam_and_datadir(Mod) ->
    case get(test_server_ctrl_job_sock) of
	undefined ->
	    %% local target
	    ok;
	JobSock ->
	    %% remote target
	    case get(test_server_downloaded_suites) of
		undefined ->
		    send_beam_and_datadir(Mod, JobSock),
		    put(test_server_downloaded_suites, [Mod]);
		Suites ->
		    case lists:member(Mod, Suites) of
			false ->
			    send_beam_and_datadir(Mod, JobSock),
			    put(test_server_downloaded_suites, [Mod|Suites]);
			true ->
			    ok
		    end
	    end
    end.

send_beam_and_datadir(Mod, JobSock) ->
    case code:which(Mod) of
	non_existing ->
	    io:format("** WARNING: Suite ~w could not be found on host\n",
		      [Mod]);
	BeamFile ->
	    send_beam(JobSock, Mod, BeamFile)
    end,
    DataDir = get_data_dir(Mod),
    case file:read_file_info(DataDir) of
	{ok,_I} ->
	    {ok,All} = file:list_dir(DataDir),
	    AddTarFiles =
		case controller_call(get_target_info) of
		    #target_info{os_family=ose} ->
			ObjExt = code:objfile_extension(),
			Wc = filename:join(DataDir, "*" ++ ObjExt),
			ModsInDatadir = filelib:wildcard(Wc),
			SendBeamFun = fun(X) -> send_beam(JobSock, X) end,
			lists:foreach(SendBeamFun, ModsInDatadir),
			%% No need to send C code or makefiles since
			%% no compilation can be done on target anyway.
			%% Compiled C code must exist on target.
			%% Beam files are already sent as binaries.
			%% Erlang source are sent in case the test case
			%% is to compile it.
			Filter = fun("Makefile") -> false;
				    ("Makefile.src") -> false;
				    (Y) ->
					 case filename:extension(Y) of
					     ".c"   -> false;
					     ObjExt -> false;
					     _      -> true
					 end
				 end,
			lists:filter(Filter, All);
		    _ ->
			All
		end,
	    Tarfile =  "data_dir.tar.gz",
	    {ok,Tar} = erl_tar:open(Tarfile, [write,compressed]),
	    ShortDataDir = filename:basename(DataDir),
	    AddTarFun =
		fun(File) ->
			Long = filename:join(DataDir, File),
			Short = filename:join(ShortDataDir, File),
			ok = erl_tar:add(Tar, Long, Short, [])
		end,
	    lists:foreach(AddTarFun, AddTarFiles),
	    ok = erl_tar:close(Tar),
	    {ok,TarBin} = file:read_file(Tarfile),
	    file:delete(Tarfile),
	    request(JobSock, {{datadir,Tarfile}, TarBin});
	{error,_R} ->
	    ok
    end.

send_beam(JobSock, BeamFile) ->
    Mod=filename:rootname(filename:basename(BeamFile), code:objfile_extension()),
    send_beam(JobSock, list_to_atom(Mod), BeamFile).
send_beam(JobSock, Mod, BeamFile) ->
    {ok,BeamBin} = file:read_file(BeamFile),
    request(JobSock, {{beam,Mod,BeamFile}, BeamBin}).

check_new_crash_dumps(Where) ->
    case Where of
	target ->
	    case get(test_server_ctrl_job_sock) of
		undefined ->
		    ok;
		Socket ->
		    read_job_sock_loop(Socket)
	    end;
	_ ->
	    ok
    end,
    test_server_sup:check_new_crash_dumps().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% progress(Result, CaseNum, Mod, Func, Location, Reason, Time,
%%	    Comment, TimeFormat) -> Result
%%
%% Prints the result of the test case to log file.
%% Note: Strings that are to be written to the minor log must
%% be prefixed with "=== " here, or the indentation will be wrong.

progress(skip, CaseNum, Mod, Func, Loc, Reason, Time,
	 Comment, {St0,St1}) ->
    {Reason1,{Color,Ret}} = if_auto_skip(Reason,
					 fun() -> {"#ffcc99",auto_skip} end,
					 fun() -> {"#ff9933",skip} end),
    print(major, "=result        skipped", []),
    print(1, "*** SKIPPED *** ~s",
	  [get_info_str(Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report, [tc_done,{?pl2a(Mod),Func,
						     {skipped,Reason1}}]),
    ReasonStr = reason_to_string(Reason1),
    ReasonStr1 = lists:flatten([string:strip(S,left) ||
				S <- string:tokens(ReasonStr,[$\n])]),
    ReasonStr2 =
	if length(ReasonStr1) > 80 ->
		string:substr(ReasonStr1, 1, 77) ++ "...";
	   true ->
		ReasonStr1
	end,
    Comment1 = case Comment of
		   "" -> "";
		   _ -> "<br>(" ++ to_string(Comment) ++ ")"
	       end,
    print(html,
	  "<td>" ++ St0 ++ "~.3fs" ++ St1 ++ "</td>"
	  "<td><font color=\"~s\">SKIPPED</font></td>"
	  "<td>~s~s</td></tr>\n",
	  [Time,Color,ReasonStr2,Comment1]),
    FormatLoc = test_server_sup:format_loc(Loc),
    print(minor, "=== location ~s", [FormatLoc]),
    print(minor, "=== reason = ~s", [ReasonStr1]),
    Ret;

progress(failed, CaseNum, Mod, Func, Loc, timetrap_timeout, T,
	 Comment0, {St0,St1}) ->
    print(major, "=result        failed: timeout, ~p", [Loc]),
    print(1, "*** FAILED *** ~s",
	  [get_info_str(Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report,
				   [tc_done,{?pl2a(Mod),Func,
					     {failed,timetrap_timeout}}]),
    FormatLastLoc = test_server_sup:format_loc(get_last_loc(Loc)),
    ErrorReason = io_lib:format("{timetrap_timeout,~s}", [FormatLastLoc]),
    Comment =
	case Comment0 of
	    "" -> "<font color=\"red\">" ++ ErrorReason ++ "</font>";
	    _ -> "<font color=\"red\">" ++ ErrorReason ++ "</font><br>" ++
                 to_string(Comment0)
	end,
    print(html,
	  "<td>" ++ St0 ++ "~.3fs" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td>~s</td></tr>\n",
	  [T/1000,Comment]),
    FormatLoc = test_server_sup:format_loc(Loc),
    print(minor, "=== location ~s", [FormatLoc]),
    print(minor, "=== reason = timetrap timeout", []),
    failed;

progress(failed, CaseNum, Mod, Func, Loc, {testcase_aborted,Reason}, _T,
	 Comment0, {St0,St1}) ->
    print(major, "=result        failed: testcase_aborted, ~p", [Loc]),
    print(1, "*** FAILED *** ~s",
	  [get_info_str(Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report,
				   [tc_done,{?pl2a(Mod),Func,
					     {failed,testcase_aborted}}]),
    FormatLastLoc = test_server_sup:format_loc(get_last_loc(Loc)),
    ErrorReason = io_lib:format("{testcase_aborted,~s}", [FormatLastLoc]),
    Comment =
	case Comment0 of
	    "" -> "<font color=\"red\">" ++ ErrorReason ++ "</font>";
	    _ -> "<font color=\"red\">" ++ ErrorReason ++ "</font><br>" ++
                 to_string(Comment0)
	end,
    print(html,
	  "<td>" ++ St0 ++ "died" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td>~s</td></tr>\n",
	  [Comment]),
    FormatLoc = test_server_sup:format_loc(Loc),
    print(minor, "=== location ~s", [FormatLoc]),
    print(minor, "=== reason = {testcase_aborted,~p}", [Reason]),
    failed;

progress(failed, CaseNum, Mod, Func, unknown, Reason, Time,
	 Comment0, {St0,St1}) ->
    print(major, "=result        failed: ~p, ~p", [Reason,unknown]),
    print(1, "*** FAILED *** ~s",
	  [get_info_str(Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report, [tc_done,{?pl2a(Mod),Func,
						     {failed,Reason}}]),
    TimeStr = io_lib:format(if is_float(Time) -> "~.3fs";
			       true -> "~w"
			    end, [Time]),
    ErrorReason = lists:flatten(io_lib:format("~p", [Reason])),
    ErrorReason1 = lists:flatten([string:strip(S,left) ||
				  S <- string:tokens(ErrorReason,[$\n])]),
    ErrorReason2 =
	if length(ErrorReason1) > 63 ->
		string:substr(ErrorReason1, 1, 60) ++ "...";
	   true ->
		ErrorReason1
	end,
    Comment =
	case Comment0 of
	    "" -> "<font color=\"red\">" ++ ErrorReason2 ++ "</font>";
	    _ -> "<font color=\"red\">" ++ ErrorReason2 ++ "</font><br>" ++
		 to_string(Comment0)
	end,
    print(html,
	  "<td>" ++ St0 ++ "~s" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td>~s</td></tr>\n",
	  [TimeStr,Comment]),
    print(minor, "=== location ~s", [unknown]),
    {FStr,FormattedReason} = format_exception(Reason),
    print(minor, "=== reason = "++FStr, [FormattedReason]),
    failed;

progress(failed, CaseNum, Mod, Func, Loc, Reason, Time,
	 Comment0, {St0,St1}) ->
    print(major, "=result        failed: ~p, ~p", [Reason,Loc]),
    print(1, "*** FAILED *** ~s",
	  [get_info_str(Func, CaseNum, get(test_server_cases))]),
    test_server_sup:framework_call(report, [tc_done,{?pl2a(Mod),Func,
						     {failed,Reason}}]),
    TimeStr = io_lib:format(if is_float(Time) -> "~.3fs";
			       true -> "~w"
			    end, [Time]),
    Comment =
	case Comment0 of
	    "" -> "";
	    _ -> "<br>" ++ to_string(Comment0)
	end,
    FormatLastLoc = test_server_sup:format_loc(get_last_loc(Loc)),
    print(html,
	  "<td>" ++ St0 ++ "~s" ++ St1 ++ "</td>"
	  "<td><font color=\"red\">FAILED</font></td>"
	  "<td><font color=\"red\">~s</font>~s</td></tr>\n",
	  [TimeStr,FormatLastLoc,Comment]),
    FormatLoc = test_server_sup:format_loc(Loc),
    print(minor, "=== location ~s", [FormatLoc]),
    {FStr,FormattedReason} = format_exception(Reason),
    print(minor, "=== reason = "++FStr, [FormattedReason]),
    failed;

progress(ok, _CaseNum, Mod, Func, _Loc, RetVal, Time,
	 Comment0, {St0,St1}) ->
    print(minor, "successfully completed test case", []),
    test_server_sup:framework_call(report, [tc_done,{?pl2a(Mod),Func,ok}]),
    Comment =
	case RetVal of
	    {comment,RetComment} ->
		String = to_string(RetComment),
		print(major, "=result        ok: ~s", [String]),
		"<td>" ++ String ++ "</td>";
	    _ ->
		print(major, "=result        ok", []),
		case Comment0 of
		    "" -> "";
		    _ -> "<td>" ++ to_string(Comment0) ++ "</td>"
		end
	end,
    print(major, "=elapsed       ~p", [Time]),
    print(html,
	  "<td>" ++ St0 ++ "~.3fs" ++ St1 ++ "</td>"
	  "<td><font color=\"green\">Ok</font></td>"
	  "~s</tr>\n",
	  [Time,Comment]),
    print(minor, "=== returned value = ~p", [RetVal]),
    ok.

%%--------------------------------------------------------------------
%% various help functions

if_auto_skip(Reason={failed,{_,init_per_testcase,_}}, True, _False) ->
    {Reason,True()};
if_auto_skip({_T,{skip,Reason={failed,{_,init_per_testcase,_}}},_Opts}, True, _False) ->
    {Reason,True()};
if_auto_skip({fw_auto_skip,Reason}, True, _False) ->
    {Reason,True()};
if_auto_skip({_T,{skip,{fw_auto_skip,Reason}},_Opts}, True, _False) ->
    {Reason,True()};
if_auto_skip(Reason, _True, False) ->
    {Reason,False()}.

update_skip_counters(RetVal, {US,AS}) ->
    {_,Result} = if_auto_skip(RetVal, fun() -> {US,AS+1} end, fun() -> {US+1,AS} end),
    Result.

get_info_str(Func, 0, _Cases) ->
    atom_to_list(Func);
get_info_str(_Func, CaseNum, unknown) ->
    "test case " ++ integer_to_list(CaseNum);
get_info_str(_Func, CaseNum, Cases) ->
    "test case " ++ integer_to_list(CaseNum) ++
	" of " ++ integer_to_list(Cases).

print_if_known(Known, {SK,AK}, {SU,AU}) ->
    {S,A} = if Known == unknown -> {SU,AU};
	       true -> {SK,AK}
	    end,
    io_lib:format(S, A).

to_string(Term) when is_list(Term) ->
    case (catch io_lib:format("~s", [Term])) of
	{'EXIT',_} -> io_lib:format("~p", [Term]);
	String     -> lists:flatten(String)
    end;
to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

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
    case os:getenv("TEST_SERVER_FRAMEWORK") of
	false ->
	    case application:get_env(test_server, format_exception) of
		{ok,false} ->
		    {"~p",Reason};
		_ ->
		    do_format_exception(Reason)
	    end;
	FW ->
	    case application:get_env(list_to_atom(FW), format_exception) of
		{ok,false} ->
		    {"~p",Reason};
		_ ->
		    do_format_exception(Reason)
	    end
    end;
format_exception(Error) ->
    format_exception({Error,[]}).

do_format_exception(Reason={Error,Stack}) ->
    StackFun = fun(_, _, _) -> false end,
    PF = fun(Term, I) ->
		 io_lib:format("~." ++ integer_to_list(I) ++ "p", [Term])
	 end,
    case catch lib:format_exception(1, error, Error, Stack, StackFun, PF) of
	{'EXIT',_} ->
	    {"~p",Reason};
	Formatted  ->
	    Formatted1 = re:replace(Formatted, "exception error: ", "", [{return,list}]),
	    {"~s",lists:flatten(Formatted1)}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_case_apply(CaseNum, Mod, Func, Args, Name, RunInit,
%%                     Where, TimetrapData) ->
%%  {{Time,RetVal,Loc,Opts,Comment},DetectedFail,ProcessesBefore,ProcessesAfter} |
%%  {{died,Reason,unknown,Comment},DetectedFail,ProcessesBefore,ProcessesAfter}
%% Name = atom()
%% Where = target | host
%% Time = float()   (seconds)
%% RetVal = term()
%% Loc = term()
%% Comment = string()
%% Reason = term()
%% DetectedFail = [{File,Line}]
%% ProcessesBefore = ProcessesAfter = integer()
%%
%% Where indicates if the test should run on target or always on the host.
%%
%% If test is to be run on target, and target is remote the request is
%% sent over socket to target, and test_server runs the case and sends the
%% result back over the socket. Else test_server runs the case directly on host.

run_test_case_apply(CaseNum, Mod, Func, Args, Name, RunInit, host, TimetrapData) ->
    test_server:run_test_case_apply({CaseNum,Mod,Func,Args,Name,RunInit,
				     TimetrapData});
run_test_case_apply(CaseNum, Mod, Func, Args, Name, RunInit, target, TimetrapData) ->
    case get(test_server_ctrl_job_sock) of
	undefined ->
	    %% local target
	    test_server:run_test_case_apply({CaseNum,Mod,Func,Args,Name,RunInit,
					     TimetrapData});
	JobSock ->
	    %% remote target
	    request(JobSock, {test_case,{CaseNum,Mod,Func,Args,Name,RunInit,
					 TimetrapData}}),
	    read_job_sock_loop(JobSock)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print(Detail, Format, Args) -> ok
%% Detail = integer()
%% Format = string()
%% Args = [term()]
%%
%% Just like io:format, except that depending on the Detail value, the output
%% is directed to console, major and/or minor log files.
%%
%% To handle printouts to common (not minor) log files from parallel test
%% case processes, the test_server_common_io_handler value is checked. If
%% set, the data is sent to the main controlling process. Note that test
%% cases that belong to a conf group nested under a parallel group will also
%% get its io data sent to main rather than immediately printed out, even
%% if the test cases are executed by the same, main, process (ie the main
%% process sends messages to itself then).
%%
%% Buffered io is handled by the handle_test_case_io_and_status/0 function.

print(Detail, Format) ->
    print(Detail, Format, []).

print(Detail, Format, Args) ->
    print(Detail, Format, Args, internal).

print(Detail, Format, Args, Printer) ->
    Msg = io_lib:format(Format, Args),
    print_or_buffer(Detail, Msg, Printer).

print_or_buffer(Detail, Msg, Printer) ->
    case get(test_server_minor_level) of
	_ when Detail == minor ->
	    output({Detail,Msg}, Printer);
	MinLevel when is_number(Detail), Detail >= MinLevel ->
	    output({Detail,Msg}, Printer);
	_ ->					% Detail < Minor | major | html
	    case get(test_server_common_io_handler) of
		undefined ->
		    output({Detail,Msg}, Printer);
		{_,MainPid} ->
		    MainPid ! {print,self(),Detail,Msg}
	    end
    end.

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
    print(html, "Run~s on ~s", [UserStr,Host]).

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
		io_lib:format("illegal format; ~p with args ~p.\n",
			      [Format,Args]);
	    Valid -> Valid
	end,
    print_or_buffer(Detail, Str, self()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% output({Level,Message}, Sender) -> ok
%% Level = integer() | minor | major | html
%% Message = string() | [integer()]
%% Sender = string() | internal
%%
%% Outputs the message on the channels indicated by Level. If Level is an
%% atom, only the corresponding channel receives the output. When Level is
%% an integer console, major and/or minor log file will receive output
%% depending on the user set thresholds (see get_levels/0, set_levels/3)
%%
%% When printing on the console, the message is prefixed with the test
%% suite's name. In case a name is not set (yet), Sender is used.
%%
%% When not outputting to the console, and the Sender is 'internal',
%% the message is prefixed with "=== ", so that it will be apparent that
%% the message comes from the test server and not the test suite itself.

output({Level,Msg}, Sender) when is_integer(Level) ->
    SumLev = get(test_server_summary_level),
    if  Level =< SumLev ->
	    output_to_fd(stdout, Msg, Sender);
	true ->
	    ok
    end,
    MajLev = get(test_server_major_level),
    if  Level =< MajLev ->
	    output_to_fd(get(test_server_major_fd), Msg, Sender);
	true ->
	    ok
    end,
    MinLev = get(test_server_minor_level),
    if  Level >= MinLev ->
	    output_to_fd(get(test_server_minor_fd), Msg, Sender);
	true ->
	    ok
    end;
output({minor,Bytes}, Sender) when is_list(Bytes) ->
    output_to_fd(get(test_server_minor_fd), Bytes, Sender);
output({major,Bytes}, Sender) when is_list(Bytes) ->
    output_to_fd(get(test_server_major_fd), Bytes, Sender);
output({minor,Bytes}, Sender) when is_binary(Bytes) ->
    output_to_fd(get(test_server_minor_fd),binary_to_list(Bytes), Sender);
output({major,Bytes}, Sender) when is_binary(Bytes) ->
    output_to_fd(get(test_server_major_fd),binary_to_list(Bytes), Sender);
output({html,Msg}, _Sender) ->
    case get(test_server_html_fd) of
	undefined ->
	    ok;
	Fd ->
	    io:put_chars(Fd,Msg),
	    case file:position(Fd, {cur, 0}) of
		{ok, Pos} ->
		    %% We are writing to a seekable file.  Finalise so
		    %% we get complete valid (and viewable) HTML code.
		    %% Then rewind to overwrite the finalising code.
		    io:put_chars(Fd, "\n</table>\n</body>\n</html>\n"),
		    file:position(Fd, Pos);
		{error, epipe} ->
		    %% The file is not seekable.  We cannot erase what
		    %% we've already written --- so the reader will
		    %% have to wait until we're done.
		    ok
	    end
    end;
output({minor,Data}, Sender) ->
    output_to_fd(get(test_server_minor_fd),
		 lists:flatten(io_lib:format(
				 "Unexpected output: ~p~n", [Data])),Sender);
output({major,Data}, Sender) ->
    output_to_fd(get(test_server_major_fd),
		 lists:flatten(io_lib:format(
				 "Unexpected output: ~p~n", [Data])),Sender).

output_to_fd(stdout, Msg, Sender) ->
    Name =
	case get(test_server_name) of
	    undefined -> Sender;
	    Other -> Other
	end,
    io:format("Testing ~s: ~s\n", [Name, lists:flatten(Msg)]);
output_to_fd(undefined, _Msg, _Sender) ->
    ok;
output_to_fd(Fd, [$=|Msg], internal) ->
    io:put_chars(Fd, [$=]),
    io:put_chars(Fd, Msg),
    io:put_chars(Fd, "\n");
output_to_fd(Fd, Msg, internal) ->
    io:put_chars(Fd, [$=,$=,$=,$ ]),
    io:put_chars(Fd, Msg),
    io:put_chars(Fd, "\n");
output_to_fd(Fd, Msg, _Sender) ->
    io:put_chars(Fd, Msg),
    io:put_chars(Fd, "\n").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timestamp_filename_get(Leader) -> string()
%% Leader = string()
%%
%% Returns a string consisting of Leader concatenated with the current
%% date and time. The resulting string is suitable as a filename.
timestamp_filename_get(Leader) ->
    timestamp_get_internal(Leader,
			   "~s~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timestamp_get(Leader) -> string()
%% Leader = string()
%%
%% Returns a string consisting of Leader concatenated with the current
%% date and time. The resulting string is suitable for display.
timestamp_get(Leader) ->
    timestamp_get_internal(Leader,
			   "~s~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w").

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
%%                           fails, Cases are not run. InitMFA and FinMFA are
%%                           always run on the host - not on target.
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
	case collect_cases(Top, #cc{mod=[],skip=Skip}) of
	    {ok,Cases,_St} -> Cases;
	    Other -> Other
	end,
    Result.


collect_cases([], St) -> {ok,[],St};
collect_cases([Case|Cs0], St0) ->
    case collect_cases(Case, St0) of
	{ok,FlatCases1,St1} ->
	    case collect_cases(Cs0, St1) of
		{ok,FlatCases2,St} ->
		    {ok,FlatCases1 ++ FlatCases2,St};
		{error,_Reason}=Error -> Error
	    end;
	{error,_Reason}=Error -> Error
    end;


collect_cases({module,Case}, St) when is_atom(Case), is_atom(St#cc.mod) ->
    collect_case({St#cc.mod,Case}, St);
collect_cases({module,Mod,Case}, St) ->
    collect_case({Mod,Case}, St);
collect_cases({module,Mod,Case,Args}, St) ->
    collect_case({Mod,Case,Args}, St);

collect_cases({dir,SubDir}, St) ->
    collect_files(SubDir, "*_SUITE", St);
collect_cases({dir,SubDir,Pattern}, St) ->
    collect_files(SubDir, Pattern++"*", St);

collect_cases({conf,InitF,CaseList,FinMF}, St) when is_atom(InitF) ->
    collect_cases({conf,[],{St#cc.mod,InitF},CaseList,FinMF}, St);
collect_cases({conf,InitMF,CaseList,FinF}, St) when is_atom(FinF) ->
    collect_cases({conf,[],InitMF,CaseList,{St#cc.mod,FinF}}, St);
collect_cases({conf,InitMF,CaseList,FinMF}, St0) ->
    collect_cases({conf,[],InitMF,CaseList,FinMF}, St0);
collect_cases({conf,Props,InitF,CaseList,FinMF}, St) when is_atom(InitF) ->
    case init_props(Props) of
	{error,_} ->
	    {ok,[],St};
	Props1 ->
	    collect_cases({conf,Props1,{St#cc.mod,InitF},CaseList,FinMF}, St)
    end;
collect_cases({conf,Props,InitMF,CaseList,FinF}, St) when is_atom(FinF) ->
    case init_props(Props) of
	{error,_} ->
	    {ok,[],St};
	Props1 ->
	    collect_cases({conf,Props1,InitMF,CaseList,{St#cc.mod,FinF}}, St)
    end;
collect_cases({conf,Props,InitMF,CaseList,FinMF}, St0) ->
    case collect_cases(CaseList, St0) of
	{ok,[],_St}=Empty ->
	    Empty;
	{ok,FlatCases,St} ->
	    Ref = make_ref(),
	    case in_skip_list(InitMF, St#cc.skip) of
		{true,Comment} ->
		    {ok,[{skip_case,{conf,Ref,InitMF,Comment}} |
			 FlatCases ++ [{conf,Ref,[],FinMF}]],St};
		false ->
		    case init_props(Props) of
			{error,_} ->
			    {ok,[],St};
			Props1 ->
			    {ok,[{conf,Ref,Props1,InitMF} |
				 FlatCases ++ [{conf,Ref,
						keep_name(Props1),
						FinMF}]],St}
		    end
	    end;
	{error,_Reason}=Error ->
	    Error
    end;

collect_cases({make,InitMFA,CaseList,FinMFA}, St0) ->
    case collect_cases(CaseList, St0) of
	{ok,[],_St}=Empty -> Empty;
	{ok,FlatCases,St} ->
	    Ref = make_ref(),
	    {ok,[{make,Ref,InitMFA}|FlatCases ++
		 [{make,Ref,FinMFA}]],St};
	{error,_Reason}=Error -> Error
    end;

collect_cases({Module, Cases}, St) when is_list(Cases)  ->
    case (catch collect_case(Cases, St#cc{mod=Module}, [])) of
	{ok, NewCases, NewSt} ->
 	    {ok, NewCases, NewSt};
 	Other ->
	    {error, Other}
     end;

collect_cases({_Mod,_Case}=Spec, St) ->
    collect_case(Spec, St);

collect_cases({_Mod,_Case,_Args}=Spec, St) ->
    collect_case(Spec, St);
collect_cases(Case, St) when is_atom(Case), is_atom(St#cc.mod) ->
    collect_case({St#cc.mod,Case}, St);
collect_cases(Other, _St) ->
    {error,{bad_subtest_spec,Other}}.

collect_case(MFA, St) ->
    case in_skip_list(MFA, St#cc.skip) of
	{true,Comment} ->
	    {ok,[{skip_case,{MFA,Comment}}],St};
	false ->
	    case MFA of
		{Mod,Case} -> collect_case_invoke(Mod, Case, MFA, St);
		{_Mod,_Case,_Args} -> {ok,[MFA],St}
	    end
    end.

collect_case([], St, Acc) ->
    {ok, Acc, St};

collect_case([Case | Cases], St, Acc) ->
    {ok, FlatCases, NewSt}  = collect_case({St#cc.mod, Case}, St),
    collect_case(Cases, NewSt, Acc ++ FlatCases).

collect_case_invoke(Mod, Case, MFA, St) ->
    case os:getenv("TEST_SERVER_FRAMEWORK") of
	false ->
	    case catch apply(Mod, Case, [suite]) of
		{'EXIT',_} ->
		    {ok,[MFA],St};
		Suite ->
		    collect_subcases(Mod, Case, MFA, St, Suite)
	    end;
	_ ->
	    Suite = test_server_sup:framework_call(get_suite, [?pl2a(Mod),Case],[]),
	    collect_subcases(Mod, Case, MFA, St, Suite)
    end.

collect_subcases(Mod, Case, MFA, St, Suite) ->
    case Suite of
	[] when Case == all -> {ok,[],St};
	[] -> {ok,[MFA],St};
%%%! --- START Kept for backwards compatibilty ---
%%%! Requirements are not used
	{req,ReqList} ->
	    collect_case_deny(Mod, Case, MFA, ReqList, [], St);
	{req,ReqList,SubCases} ->
	    collect_case_deny(Mod, Case, MFA, ReqList, SubCases, St);
%%%! --- END Kept for backwards compatibilty ---
	{Skip,Reason} when Skip==skip; Skip==skipped ->
	    {ok,[{skip_case,{MFA,Reason}}],St};
	SubCases ->
	    collect_case_subcases(Mod, Case, SubCases, St)
    end.

collect_case_subcases(Mod, Case, SubCases, St0) ->
    OldMod = St0#cc.mod,
    case collect_cases(SubCases, St0#cc{mod=Mod}) of
	{ok,FlatCases,St} ->
	    {ok,FlatCases,St#cc{mod=OldMod}};
	{error,Reason} ->
	    {error,{{Mod,Case},Reason}}
    end.

collect_files(Dir, Pattern, St) ->
    {ok,Cwd} = file:get_cwd(),
    Dir1 = filename:join(Cwd, Dir),
    Wc = filename:join([Dir1,Pattern++code:objfile_extension()]),
    case catch filelib:wildcard(Wc) of
	{'EXIT', Reason} ->
	    io:format("Could not collect files: ~p~n", [Reason]),
	    {error,{collect_fail,Dir,Pattern}};
	Mods0 ->
	    Mods = [{path_to_module(Mod),all} || Mod <- lists:sort(Mods0)],
	    collect_cases(Mods, St)
    end.

path_to_module(Path) ->
    list_to_atom(filename:rootname(filename:basename(Path))).

collect_case_deny(Mod, Case, MFA, ReqList, SubCases, St) ->
    case {check_deny(ReqList, St#cc.skip),SubCases} of
	{{denied,Comment},_SubCases} ->
	    {ok,[{skip_case,{MFA,Comment}}],St};
	{granted,[]} ->
	    {ok,[MFA],St};
	{granted,SubCases} ->
	    collect_case_subcases(Mod, Case, SubCases, St)
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
	    {denied,io_lib:format("Requirement ~p=~p", [Req,Val])};
	_ ->
	    check_deny_req(Req, DenyList)
    end;
check_deny_req(Req, DenyList) ->
    case lists:member(Req, DenyList) of
	true -> {denied,io_lib:format("Requirement ~p", [Req])};
	false -> granted
    end.

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
    lists:filter(fun({name,_}) -> true; (_) -> false end, Props).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 Target node handling functions                   %%
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
    T = 10 * ?ACCEPT_TIMEOUT, % give some extra time
    format(minor, "Attempt to start ~w node ~p with options ~p",
	   [Type, Name, Options]),
    case controller_call({start_node,Name,Type,Options}, T) of
	{{ok,Nodename}, Host, Cmd, Info, Warning} ->
	    format(minor,
		   "Successfully started node ~p on ~p with command: ~p",
		   [Nodename, Host, Cmd]),
	    format(major, "=node_start    ~p", [Nodename]),
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
		   "Failed to start node ~p on ~p with command: ~p~n"
		   "Reason: ~p",
		   [Name, Host, Cmd, Ret]),
	    {fail,Ret};
	{Ret, undefined, undefined} ->
	    format(minor, "Failed to start node ~p: ~p", [Name,Ret]),
	    Ret;
	{Ret, Host, Cmd} ->
	    format(minor,
		   "Failed to start node ~p on ~p with command: ~p~n"
		   "Reason: ~p",
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
    case catch controller_call({wait_for_node,Slave},10000) of
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


%%--------------------------------------------------------------------
%% Functions handling target communication over socket

%% Generic send function for communication with target
request(Sock,Request) ->
    gen_tcp:send(Sock,<<1,(term_to_binary(Request))/binary>>).

%% Receive and decode request on job specific socket
%% Used when test is running on a remote target
read_job_sock_loop(Sock) ->
    case gen_tcp:recv(Sock,0) of
	{error,Reason} ->
	    gen_tcp:close(Sock),
	    exit({controller,connection_lost,Reason});
	{ok,<<1,Request/binary>>} ->
	    case decode(binary_to_term(Request)) of
		ok ->
		    read_job_sock_loop(Sock);
		{stop,Result} ->
		    Result
	    end
    end.

decode({apply,{M,F,A}}) ->
    apply(M,F,A),
    ok;
decode({sync_apply,{M,F,A}}) ->
    R = apply(M,F,A),
    request(get(test_server_ctrl_job_sock),{sync_result,R}),
    ok;
decode({sync_result,Result}) ->
    {stop,Result};
decode({test_case_result,Result}) ->
    {stop,Result};
decode({privdir,empty_priv_dir}) ->
    {stop,ok};
decode({{privdir,PrivDirTar},TarBin}) ->
    Root = get(test_server_log_dir_base),
    unpack_tar(Root,PrivDirTar,TarBin),
    {stop,ok};
decode({crash_dumps,no_crash_dumps}) ->
    {stop,ok};
decode({{crash_dumps,CrashDumpTar},TarBin}) ->
    Dir = test_server_sup:crash_dump_dir(),
    unpack_tar(Dir,CrashDumpTar,TarBin),
    {stop,ok}.

unpack_tar(Dir,TarFileName0,TarBin) ->
    TarFileName = filename:join(Dir,TarFileName0),
    ok = file:write_file(TarFileName,TarBin),
    ok = erl_tar:extract(TarFileName,[compressed,{cwd,Dir}]),
    ok = file:delete(TarFileName).


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
	    LM = length(fetch(messages, Info)),
	    pformat(io_lib:format("~w", [Pid]),
		    io_lib:format("~w", [Call]),
		    io_lib:format("~w", [Curr]), Reds, LM),
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
%%   cross.cover file (in the test_server application) under 'all'
%%   or under the tested application.
%%
%% The modules listed in the cross.cover file are modules that are
%% hevily used by other applications than the one they belong
%% to. After all tests are completed, these modules can be analysed
%% with coverage data from all tests - see cross_cover_analyse/1. The
%% result is stored in a file called cross_cover.html in the
%% run.<timestamp> directory of the application the modules belong
%% to.
%%
%% For example, the lists module is listed in cross.cover to be
%% included in all tests. lists belongs to the stdlib
%% application. cross_cover_analyse/1 will create a file named
%% cross_cover.html under the newest stdlib.logs/run.xxx directory,
%% where the coverage result for the lists module from all tests is
%% presented.
%%
%% The lists module is also presented in the normal coverage log
%% for stdlib, but that only includes the coverage achieved by
%% the stdlib tests themselves.
%%
%% The Cross cover file cross.cover contains elements like this:
%% {App,Modules}.
%% where App can be an application name or the atom all. The
%% application (or all applications) shall cover compile the listed
%% Modules.


%% Cover compilation
%% The compilation is executed on the target node
cover_compile({App,{_File,Exclude,Include,Cross,_Export}}) ->
    cover_compile1({App,Exclude,Include,Cross});

cover_compile({App,CoverFile}) ->
    Cross = get_cross_modules(App),
    {Exclude,Include} = read_cover_file(CoverFile),
    cover_compile1({App,Exclude,Include,Cross}).

cover_compile1(What) ->
    case get(test_server_ctrl_job_sock) of
	undefined ->
	    %% local target
	    test_server:cover_compile(What);
	JobSock ->
	    %% remote target
	    request(JobSock, {sync_apply,{test_server,cover_compile,[What]}}),
	    read_job_sock_loop(JobSock)
    end.


%% Read the coverfile for an application and return a list of modules
%% that are members of the application but shall not be compiled
%% (Exclude), and a list of modules that are not members of the
%% application but shall be compiled (Include).
read_cover_file(none) ->
    {[],[]};
read_cover_file(CoverFile) ->
    case file:consult(CoverFile) of
	{ok,List} ->
	    case check_cover_file(List, [], []) of
		{ok,Exclude,Include} -> {Exclude,Include};
		error ->
		    io:fwrite("Faulty format of CoverFile ~p\n", [CoverFile]),
		    {[],[]}
	    end;
	{error,Reason} ->
	    io:fwrite("Can't read CoverFile ~p\nReason: ~p\n",
		      [CoverFile,Reason]),
	    {[],[]}
    end.

check_cover_file([{exclude,all}|Rest], _, Include) ->
    check_cover_file(Rest, all, Include);
check_cover_file([{exclude,Exclude}|Rest], _, Include) ->
    case lists:all(fun(M) -> is_atom(M) end, Exclude) of
	true ->
	    check_cover_file(Rest, Exclude, Include);
	false ->
	    error
    end;
check_cover_file([{include,Include}|Rest], Exclude, _) ->
    case lists:all(fun(M) -> is_atom(M) end, Include) of
	true ->
	    check_cover_file(Rest, Exclude, Include);
	false ->
	    error
    end;
check_cover_file([], Exclude, Include) ->
    {ok,Exclude,Include}.



%% Cover analysis, per application
%% This analysis is executed on the target node once the test is
%% completed for an application. This is not the same as the cross
%% cover analysis, which can be executed on any node after the tests
%% are finshed.
%%
%% This per application analysis writes the file cover.html in the
%% application's run.<timestamp> directory.
cover_analyse({App,CoverInfo}, Analyse, AnalyseMods, TestDir) ->
    write_default_cross_coverlog(TestDir),

    {ok,CoverLog} = file:open(filename:join(TestDir, ?coverlog_name), [write]),
    write_coverlog_header(CoverLog),
    io:fwrite(CoverLog, "<h1>Coverage for application '~w'</h1>\n", [App]),
    io:fwrite(CoverLog,
	      "<p><a href=\"~s\">Coverdata collected over all tests</a></p>",
	      [?cross_coverlog_name]),

    {CoverFile,_Included,Excluded} =
	case CoverInfo of
	    {File,Excl,Incl,_Cross,Export} ->
		cover:export(Export),
		{File,Incl,Excl};
	    File ->
		{Excl,Incl} = read_cover_file(File),
		{File,Incl,Excl}
	end,
    io:fwrite(CoverLog, "<p>CoverFile: <code>~p</code>\n", [CoverFile]),

    case length(cover:imported_modules()) of
	Imps when Imps > 0 ->
	    io:fwrite(CoverLog, "<p>Analysis includes data from ~w imported module(s).\n",
		      [Imps]);
	_ ->
	    ok
    end,

    io:fwrite(CoverLog, "<p>Excluded module(s): <code>~p</code>\n", [Excluded]),

    Coverage = cover_analyse(Analyse, AnalyseMods),

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
    file:write_file(filename:join(TestDir, ?cover_total),
		    term_to_binary(TotPercent)).

cover_analyse(Analyse, AnalyseMods) ->
    TestDir = get(test_server_log_dir_base),
    case get(test_server_ctrl_job_sock) of
	undefined ->
	    %% local target
	    test_server:cover_analyse({Analyse,TestDir}, AnalyseMods);
	JobSock ->
	    %% remote target
	    request(JobSock, {sync_apply,{test_server,
					  cover_analyse,
					  [Analyse,AnalyseMods]}}),
	    read_job_sock_loop(JobSock)
    end.


%% Cover analysis, cross application
%% This can be executed on any node after all tests are finished.
%% The node's current directory must be the same as when the tests
%% were run.
cross_cover_analyse(Analyse) ->
    cross_cover_analyse(Analyse, undefined).

cross_cover_analyse(Analyse, CrossModules) ->
    CoverdataFiles = get_coverdata_files(),
    lists:foreach(fun(CDF) -> cover:import(CDF) end, CoverdataFiles),
    io:fwrite("Cover analysing... ", []),
    DetailsFun =
	case Analyse of
	    details ->
		fun(Dir,M) ->
			OutFile = filename:join(Dir,
						atom_to_list(M) ++
						".CROSS_COVER.html"),
			cover:analyse_to_file(M, OutFile, [html]),
			{file,OutFile}
		end;
	    _ ->
		fun(_,_) -> undefined end
	end,
    SortedModules =
	case CrossModules of
	    undefined ->
		sort_modules([Mod || Mod <- get_all_cross_modules(),
				     lists:member(Mod, cover:imported_modules())], []);
	    _ ->
		sort_modules(CrossModules, [])
	end,
    Coverage = analyse_apps(SortedModules, DetailsFun, []),
    cover:stop(),
    write_cross_cover_logs(Coverage).

%% For each application from which there are modules listed in the
%% cross.cover, write a cross cover log (cross_cover.html).
write_cross_cover_logs([{App,Coverage}|T]) ->
    case last_test_for_app(App) of
	false ->
	    ok;
	Dir ->
	    CoverLogName = filename:join(Dir,?cross_coverlog_name),
	    {ok,CoverLog} = file:open(CoverLogName, [write]),
	    write_coverlog_header(CoverLog),
	    io:fwrite(CoverLog,
		      "<h1>Coverage results for \'~w\' from all tests</h1>\n",
		      [App]),
	    write_cover_result_table(CoverLog, Coverage),
	    io:fwrite("Written file ~p\n", [CoverLogName])
    end,
    write_cross_cover_logs(T);
write_cross_cover_logs([]) ->
    io:fwrite("done\n", []).

%% Find all exported coverdata files. First find all the latest
%% run.<timestamp> directories, and the check if there is a file named
%% all.coverdata.
get_coverdata_files() ->
    PossibleFiles = [last_coverdata_file(Dir) ||
			Dir <- filelib:wildcard([$*|?logdir_ext]),
			filelib:is_dir(Dir)],
    [File || File <- PossibleFiles, filelib:is_file(File)].

last_coverdata_file(Dir) ->
    LastDir = last_test(filelib:wildcard(filename:join(Dir,"run.[1-2]*")),false),
    filename:join(LastDir,"all.coverdata").


%% Find the latest run.<timestamp> directory for the given application.
last_test_for_app(App) ->
    AppLogDir = atom_to_list(App)++?logdir_ext,
    last_test(filelib:wildcard(filename:join(AppLogDir,"run.[1-2]*")),false).

last_test([Run|Rest], false) ->
    last_test(Rest, Run);
last_test([Run|Rest], Latest) when Run > Latest ->
    last_test(Rest, Run);
last_test([_|Rest], Latest) ->
    last_test(Rest, Latest);
last_test([], Latest) ->
    Latest.

%% Sort modules according to the application they belong to.
%% Return [{App,LastTestDir,ModuleList}]
sort_modules([M|Modules], Acc) ->
    App = get_app(M),
    Acc1 =
	case lists:keysearch(App, 1, Acc) of
	    {value,{App,LastTest,List}} ->
		lists:keyreplace(App, 1, Acc, {App,LastTest,[M|List]});
	    false ->
		[{App,last_test_for_app(App),[M]}|Acc]
	end,
    sort_modules(Modules, Acc1);
sort_modules([], Acc) ->
    Acc.

get_app(Module) ->
    Beam = code:which(Module),
    AppDir = filename:basename(filename:dirname(filename:dirname(Beam))),
    [AppStr|_] = string:tokens(AppDir,"-"),
    list_to_atom(AppStr).


%% For each application, analyse all modules
%% Used for cross cover analysis.
analyse_apps([{App,LastTest,Modules}|T], DetailsFun, Acc) ->
    Cov = analyse_modules(LastTest, Modules, DetailsFun, []),
    analyse_apps(T, DetailsFun, [{App,Cov}|Acc]);
analyse_apps([], _DetailsFun, Acc) ->
    Acc.

%% Analyse each module
%% Used for cross cover analysis.
analyse_modules(Dir, [M|Modules], DetailsFun, Acc) ->
    {ok,{M,{Cov,NotCov}}} = cover:analyse(M, module),
    Acc1 = [{M,{Cov,NotCov,DetailsFun(Dir,M)}}|Acc],
    analyse_modules(Dir, Modules, DetailsFun, Acc1);
analyse_modules(_Dir, [], _DetailsFun, Acc) ->
    Acc.


%% Read the cross cover file (cross.cover)
get_all_cross_modules() ->
    get_cross_modules(all).
get_cross_modules(App) ->
    case file:consult(?cross_cover_file) of
	{ok,List} ->
	    get_cross_modules(App, List, []);
	_X ->
	    []
    end.

get_cross_modules(App, [{_To,Modules}|T], Acc) when App==all->
    get_cross_modules(App, T, Acc ++ Modules);
get_cross_modules(App, [{To,Modules}|T], Acc) when To==App; To==all->
    get_cross_modules(App, T, Acc ++ Modules);
get_cross_modules(App, [_H|T], Acc) ->
    get_cross_modules(App, T, Acc);
get_cross_modules(_App, [], Acc) ->
    Acc.


%% Support functions for writing the cover logs (both cross and normal)
write_coverlog_header(CoverLog) ->
    case catch
	io:fwrite(CoverLog,
		  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
		  "<!-- autogenerated by '~w'. -->\n"
		  "<html>\n"
		  "<head><title>Coverage results</title></head>\n"
		  "<body bgcolor=\"white\" text=\"black\" "
		  "link=\"blue\" vlink=\"purple\" alink=\"red\">",
		  [?MODULE]) of
	{'EXIT',Reason} ->
	    io:format("\n\nERROR: Could not write normal heading in coverlog.\n"
		      "CoverLog: ~w\n"
		      "Reason: ~p\n",
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
    io_lib:fwrite("<tr><td><a href=\"~s\">~w</a></td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>\n",
		  [filename:basename(File),M,pc(Cov,NotCov),Cov,NotCov]);
format_analyse(M,Cov,NotCov,{lines,Lines}) ->
    CoverOutName = atom_to_list(M)++".COVER.html",
    {ok,CoverOut} = file:open(CoverOutName, [write]),
    write_not_covered(CoverOut,M,Lines),
    io_lib:fwrite("<tr><td><a href=\"~s\">~w</a></td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>\n",
		  [CoverOutName,M,pc(Cov,NotCov),Cov,NotCov]);
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
    io:fwrite(CoverOut,
	      "<html>\n"
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
    io:fwrite(CoverOut,"</table>\n</html>\n", []).


write_default_coverlog(TestDir) ->
    {ok,CoverLog} = file:open(filename:join(TestDir,?coverlog_name), [write]),
    write_coverlog_header(CoverLog),
    io:fwrite(CoverLog,"Cover tool is not used\n</body></html>\n", []),
    file:close(CoverLog).

write_default_cross_coverlog(TestDir) ->
    {ok,CrossCoverLog} =
	file:open(filename:join(TestDir,?cross_coverlog_name), [write]),
    write_coverlog_header(CrossCoverLog),
    io:fwrite(CrossCoverLog,
	      "No cross cover modules exist for this application,<br>"
	      "or cross cover analysis is not completed.\n"
	      "</body></html>\n", []),
    file:close(CrossCoverLog).

write_cover_result_table(CoverLog,Coverage) ->
    io:fwrite(CoverLog,
	      "<p><table border=3 cellpadding=5>\n"
	      "<tr><th>Module</th><th>Covered (%)</th><th>Covered (Lines)</th>"
	      "<th>Not covered (Lines)</th>\n",
	      []),
    {TotCov,TotNotCov} =
	lists:foldl(fun({M,{Cov,NotCov,Details}},{AccCov,AccNotCov}) ->
			    Str = format_analyse(M,Cov,NotCov,Details),
			    io:fwrite(CoverLog,"~s", [Str]),
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
    file:close(CoverLog),
    TotPercent.
