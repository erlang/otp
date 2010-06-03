%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(test_server).

-define(DEFAULT_TIMETRAP_SECS, 60).

%%% START %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/1,start/2]).

%%% TEST_SERVER_CTRL INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([run_test_case_apply/1,init_target_info/0,init_purify/0]).
-export([cover_compile/1,cover_analyse/2]).

%%% TEST_SERVER_SUP INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([get_loc/1]).

%%% TEST SUITE INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([lookup_config/2]).
-export([fail/0,fail/1,format/1,format/2,format/3]).
-export([capture_start/0,capture_stop/0,capture_get/0]).
-export([messages_get/0]).
-export([hours/1,minutes/1,seconds/1,sleep/1,adjusted_sleep/1,timecall/3]).
-export([timetrap_scale_factor/0,timetrap/1,timetrap_cancel/1]).
-export([m_out_of_n/3,do_times/4,do_times/2]).
-export([call_crash/3,call_crash/4,call_crash/5]).
-export([temp_name/1]).
-export([start_node/3, stop_node/1, wait_for_node/1, is_release_available/1]).
-export([app_test/1, app_test/2]).
-export([is_native/1]).
-export([comment/1]).
-export([os_type/0]).
-export([run_on_shielded_node/2]).
-export([is_cover/0,is_debug/0,is_commercial/0]).

-export([break/1,continue/0]).

%%% DEBUGGER INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([purify_new_leaks/0, purify_format/2, purify_new_fds_inuse/0,
	 purify_is_running/0]).

%%% PRIVATE EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{controller,jobs=[]}).

-include("test_server_internal.hrl").
-include_lib("kernel/include/file.hrl").

-define(pl2a(M), test_server_sup:package_atom(M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% **** START *** CODE FOR REMOTE TARGET ONLY ***
%%
%% test_server
%% This process is started only if the test is to be run on a remote target
%% The process is then started on target
%% A socket connection is established with the test_server_ctrl process
%% on host, and information about target is sent to host.
start([ControllerHost]) when is_atom(ControllerHost) ->
    start(atom_to_list(ControllerHost));
start(ControllerHost) when is_list(ControllerHost) ->
    start(ControllerHost,?MAIN_PORT).
start(ControllerHost,ControllerPort) ->
    S = self(),
    Pid = spawn(fun() -> init(ControllerHost,ControllerPort,S) end),
    receive {Pid,started} -> {ok,Pid};
	    {Pid,Error} -> Error
    end.

init(Host,Port,Starter) ->
    global:register_name(?MODULE,self()),
    process_flag(trap_exit,true),
    test_server_sup:cleanup_crash_dumps(),
    case gen_tcp:connect(Host,Port, [binary,
				     {reuseaddr,true},
				     {packet,2}]) of
	{ok,MainSock} ->
	    Starter ! {self(),started},
	    request(MainSock,{target_info,init_target_info()}),
	    loop(#state{controller={Host,MainSock}});
	Error ->
	    Starter ! {self(),{error,
			       {could_not_contact_controller,Error}}}
    end.

init_target_info() ->
    [$.|Emu] = code:objfile_extension(),
    {_, OTPRel} = init:script_id(),
    TestServerDir = filename:absname(filename:dirname(code:which(?MODULE))),
    #target_info{os_family=test_server_sup:get_os_family(),
		 os_type=os:type(),
		 version=erlang:system_info(version),
		 system_version=erlang:system_info(system_version),
		 root_dir=code:root_dir(),
		 test_server_dir=TestServerDir,
		 emulator=Emu,
		 otp_release=OTPRel,
		 username=test_server_sup:get_username(),
		 cookie=atom_to_list(erlang:get_cookie())}.


loop(#state{controller={_,MainSock}} = State) ->
    receive
	{tcp, MainSock, <<1,Request/binary>>} ->
	    State1 = decode_main(binary_to_term(Request),State),
	    loop(State1);
	{tcp_closed, MainSock} ->
	    gen_tcp:close(MainSock),
	    halt();
	{'EXIT',Pid,Reason} ->
	    case lists:keysearch(Pid,1,State#state.jobs) of
		{value,{Pid,Name}} ->
		    case Reason of
			normal -> ignore;
			_other -> request(MainSock,{job_proc_killed,Name,Reason})
		    end,
		    NewJobs = lists:keydelete(Pid,1,State#state.jobs),
		    loop(State#state{jobs = NewJobs});
		false ->
		    loop(State)
	    end
    end.

%% Decode request on main socket
decode_main({job,Port,Name},#state{controller={Host,_},jobs=Jobs}=State) ->
    S = self(),
    NewJob = spawn_link(fun() -> job(Host,Port,S) end),
    receive {NewJob,started} -> State#state{jobs=[{NewJob,Name}|Jobs]};
	    {NewJob,_Error} -> State
    end.

init_purify() ->
    purify_new_leaks().


%% Temporary job process on target
%% This process will live while all test cases in the job are executed.
%% A socket connection is established with the job process on host.
job(Host,Port,Starter) ->
    process_flag(trap_exit,true),
    init_purify(),
    case gen_tcp:connect(Host,Port, [binary,
				     {reuseaddr,true},
				     {packet,4},
				     {active,false}]) of
	{ok,JobSock} ->
	    Starter ! {self(),started},
	    job(JobSock);
	Error ->
	    Starter ! {self(),{error,
			       {could_not_contact_controller,Error}}}
    end.

job(JobSock) ->
    JobDir = get_jobdir(),
    ok = file:make_dir(JobDir),
    ok = file:make_dir(filename:join(JobDir,?priv_dir)),
    put(test_server_job_sock,JobSock),
    put(test_server_job_dir,JobDir),
    {ok,Cwd} = file:get_cwd(),
    job_loop(JobSock),
    ok = file:set_cwd(Cwd),
    send_privdir(JobDir,JobSock), % also recursively removes jobdir
    ok.


get_jobdir() ->
    Now = now(),
    {{Y,M,D},{H,Mi,S}} = calendar:now_to_local_time(Now),
    Basename = io_lib:format("~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w_~w",
			     [Y,M,D,H,Mi,S,element(3,Now)]),
    %% if target has a file master, don't use prim_file to look up cwd
    case lists:keymember(master,1,init:get_arguments()) of
	true ->
	    {ok,Cwd} = file:get_cwd(),
	    Cwd ++ "/" ++ Basename;
	false ->
	    filename:absname(Basename)
    end.

send_privdir(JobDir,JobSock) ->
    LocalPrivDir = filename:join(JobDir,?priv_dir),
    case file:list_dir(LocalPrivDir) of
	{ok,List} when List/=[] ->
	    Tarfile0 = ?priv_dir ++ ".tar.gz",
	    Tarfile = filename:join(JobDir,Tarfile0),
	    {ok,Tar} = erl_tar:open(Tarfile,[write,compressed,cooked]),
	    ok = erl_tar:add(Tar,LocalPrivDir,?priv_dir,[]),
	    ok = erl_tar:close(Tar),
	    {ok,TarBin} = file:read_file(Tarfile),
	    file:delete(Tarfile),
	    ok = del_dir(JobDir),
	    request(JobSock,{{privdir,Tarfile0},TarBin});
	_ ->
	    ok = del_dir(JobDir),
	    request(JobSock,{privdir,empty_priv_dir})
    end.

del_dir(Dir) ->
    case file:read_file_info(Dir) of
	{ok,#file_info{type=directory}} ->
	    {ok,Cont} = file:list_dir(Dir),
	    lists:foreach(fun(F) -> del_dir(filename:join(Dir,F)) end, Cont),
	    ok = file:del_dir(Dir);
	{ok,#file_info{}} ->
	    ok = file:delete(Dir);
	_r ->
	    %% This might be a symlink - let's try to delete it!
	    catch file:delete(Dir),
	    ok
    end.

%%
%% Receive and decode request on job socket
%%
job_loop(JobSock) ->
    Request = recv(JobSock),
    case decode_job(Request) of
	ok -> job_loop(JobSock);
	{stop,R} -> R
    end.

decode_job({{beam,Mod,Which},Beam}) ->
    % FIXME, shared directory structure on host and target required,
    % "Library beams" are not loaded from HOST... /Patrik
    code:add_patha(filename:dirname(Which)),
    % End of Patriks uglyness...
    {module,Mod} = code:load_binary(Mod,Which,Beam),
    ok;
decode_job({{datadir,Tarfile0},Archive}) ->
    JobDir = get(test_server_job_dir),
    Tarfile = filename:join(JobDir,Tarfile0),
    ok = file:write_file(Tarfile,Archive),
    % Cooked is temporary removed/broken
    % ok = erl_tar:extract(Tarfile,[compressed,{cwd,JobDir},cooked]),
    ok = erl_tar:extract(Tarfile,[compressed,{cwd,JobDir}]),
    ok = file:delete(Tarfile),
    ok;
decode_job({test_case,Case}) ->
    Result = run_test_case_apply(Case),
    JobSock = get(test_server_job_sock),
    request(JobSock,{test_case_result,Result}),
    case test_server_sup:tar_crash_dumps() of
	{error,no_crash_dumps} -> request(JobSock,{crash_dumps,no_crash_dumps});
	{ok,TarFile} ->
	    {ok,TarBin} = file:read_file(TarFile),
	    file:delete(TarFile),
	    request(JobSock,{{crash_dumps,filename:basename(TarFile)},TarBin})
    end,
    ok;
decode_job({sync_apply,{M,F,A}}) ->
    R = apply(M,F,A),
    request(get(test_server_job_sock),{sync_result,R}),
    ok;
decode_job(job_done) ->
    {stop,stopped}.

%%
%% **** STOP *** CODE FOR REMOTE TARGET ONLY ***
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cover_compile({App,Include,Exclude,Cross}) ->
%%                                          {ok,AnalyseModules} | {error,Reason}
%%
%% App = atom() , name of application to be compiled
%% Exclude = [atom()], list of modules to exclude
%% Include = [atom()], list of modules outside of App that should be included
%%                 in the cover compilation
%% Cross = [atoms()], list of modules outside of App shat should be included
%%                 in the cover compilation, but that shall not be part of
%%                 the cover analysis for this application.
%%
%% Cover compile the given application. Return {ok,AnalyseMods} if application
%% is found, else {error,application_not_found}.

cover_compile({none,_Exclude,Include,Cross}) ->
    CompileMods = Include++Cross,
    case length(CompileMods) of
	0 ->
	    io:fwrite("WARNING: No modules to cover compile!\n\n",[]),
	    cover:start(),			% start cover server anyway
	    {ok,[]};
	N ->
	    io:fwrite("Cover compiling ~w modules - "
		      "this may take some time... ",[N]),
	    do_cover_compile(CompileMods),
	    io:fwrite("done\n\n",[]),
	    {ok,Include}
    end;
cover_compile({App,all,Include,Cross}) ->
    CompileMods = Include++Cross,
    case length(CompileMods) of
	0 ->
	    io:fwrite("WARNING: No modules to cover compile!\n\n",[]),
	    cover:start(),			% start cover server anyway
	    {ok,[]};
	N ->
	    io:fwrite("Cover compiling '~w' (~w files) - "
		      "this may take some time... ",[App,N]),
	    io:format("\nWARNING: All modules in \'~w\' are excluded\n"
		      "Only cover compiling modules in include list "
		      "and the modules\nin the cross cover file:\n"
		      "~p\n", [App,CompileMods]),
	    do_cover_compile(CompileMods),
	    io:fwrite("done\n\n",[]),
	    {ok,Include}
    end;
cover_compile({App,Exclude,Include,Cross}) ->
    case code:lib_dir(App) of
	{error,bad_name} ->
	    case Include++Cross of
		[] ->
		    io:format("\nWARNING: Can't find lib_dir for \'~w\'\n"
			      "Not cover compiling!\n\n",[App]),
		    {error,application_not_found};
		CompileMods ->
		    io:fwrite("Cover compiling '~w' (~w files) - "
			      "this may take some time... ",
			      [App,length(CompileMods)]),
		    io:format("\nWARNING: Can't find lib_dir for \'~w\'\n"
			      "Only cover compiling modules in include list: "
			      "~p\n", [App,Include]),
		    do_cover_compile(CompileMods),
		    io:fwrite("done\n\n",[]),
		    {ok,Include}
	    end;
	LibDir ->
	    EbinDir = filename:join([LibDir,"ebin"]),
	    WC = filename:join(EbinDir,"*.beam"),
	    AllMods = module_names(filelib:wildcard(WC)),
	    AnalyseMods = (AllMods ++ Include) -- Exclude,
	    CompileMods = AnalyseMods ++ Cross,
	    case length(CompileMods) of
		0 ->
		    io:fwrite("WARNING: No modules to cover compile!\n\n",[]),
		    cover:start(),		% start cover server anyway
		    {ok,[]};
		N ->
		    io:fwrite("Cover compiling '~w' (~w files) - "
			      "this may take some time... ",[App,N]),
		    do_cover_compile(CompileMods),
		    io:fwrite("done\n\n",[]),
		    {ok,AnalyseMods}
	    end
    end.


module_names(Beams) ->
    [list_to_atom(filename:basename(filename:rootname(Beam))) || Beam <- Beams].


do_cover_compile(Modules) ->
    do_cover_compile1(lists:usort(Modules)). % remove duplicates

do_cover_compile1([Dont|Rest]) when Dont=:=cover;
				    Dont=:=test_server;
				    Dont=:=test_server_ctrl ->
    do_cover_compile1(Rest);
do_cover_compile1([M|Rest]) ->
    case {code:is_sticky(M),code:is_loaded(M)} of
	{true,_} ->
	    code:unstick_mod(M),
	    case cover:compile_beam(M) of
		{ok,_} ->
		    ok;
		Error ->
		    io:fwrite("\nWARNING: Could not cover compile ~w: ~p\n",
			      [M,Error])
	    end,
	    code:stick_mod(M),
	    do_cover_compile1(Rest);
	{false,false} ->
	    case code:load_file(M) of
		{module,_} ->
		    do_cover_compile1([M|Rest]);
		Error ->
		    io:fwrite("\nWARNING: Could not load ~w: ~p\n",[M,Error]),
		    do_cover_compile1(Rest)
	    end;
	{false,_} ->
	    case cover:compile_beam(M) of
		{ok,_} ->
		    ok;
		Error ->
		    io:fwrite("\nWARNING: Could not cover compile ~w: ~p\n",
			      [M,Error])
	    end,
	    do_cover_compile1(Rest)
    end;
do_cover_compile1([]) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cover_analyse(Analyse,Modules) -> [{M,{Cov,NotCov,Details}}]
%%
%% Analyse = {details,Dir} | details | {overview,void()} | overview
%% Modules = [atom()], the modules to analyse
%%
%% Cover analysis. If this is a remote target, analyse_to_file can not be used.
%% In that case the analyse level 'line' is used instead if Analyse==details.
%%
%% If this is a local target, the test directory is given
%% (Analyse=={details,Dir}) and analyse_to_file can be used directly.
%%
%% If Analyse==overview | {overview,Dir} analyse_to_file is not used, only
%% an overview containing the number of covered/not covered lines in each module.
%%
%% Also, if a Dir exists, cover data will be exported to a file called
%% all.coverdata in that directory.
cover_analyse(Analyse,Modules) ->
    io:fwrite("Cover analysing...\n",[]),
    DetailsFun =
	case Analyse of
	    {details,Dir} ->
		case cover:export(filename:join(Dir,"all.coverdata")) of
		    ok ->
			fun(M) ->
				OutFile = filename:join(Dir,
							atom_to_list(M) ++
							".COVER.html"),
				case cover:analyse_to_file(M,OutFile,[html]) of
				    {ok,_} ->
					{file,OutFile};
				    Error ->
					Error
				end
			end;
		    Error ->
			fun(_) -> Error end
		end;
	    details ->
		fun(M) ->
			case cover:analyse(M,line) of
			    {ok,Lines} ->
				{lines,Lines};
			    Error ->
				Error
			end
		end;
	    {overview,Dir} ->
		case cover:export(filename:join(Dir,"all.coverdata")) of
		    ok ->
			fun(_) -> undefined end;
		    Error ->
			fun(_) -> Error end
		end;
	    overview ->
		fun(_) -> undefined end
	end,
    R = lists:map(
	  fun(M) ->
		  case cover:analyse(M,module) of
		      {ok,{M,{Cov,NotCov}}} ->
			  {M,{Cov,NotCov,DetailsFun(M)}};
		      Err ->
			  io:fwrite("WARNING: Analysis failed for ~w. Reason: ~p\n",
				    [M,Err]),
			  {M,Err}
		  end
	  end, Modules),
    Sticky = unstick_all_sticky(node()),
    cover:stop(),
    stick_all_sticky(node(),Sticky),
    R.


unstick_all_sticky(Node) ->
    lists:filter(
      fun(M) ->
	      case code:is_sticky(M) of
		  true ->
		      rpc:call(Node,code,unstick_mod,[M]),
		      true;
		  false ->
		      false
	      end
      end,
      cover:modules()).

stick_all_sticky(Node,Sticky) ->
    lists:foreach(
      fun(M) ->
	      rpc:call(Node,code,stick_mod,[M])
      end,
      Sticky).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_test_case_apply(Mod,Func,Args,Name,RunInit,TimetrapData) ->
%%               {Time,Value,Loc,Opts,Comment} | {died,Reason,unknown,Comment}
%%
%% Time = float()   (seconds)
%% Value = term()
%% Loc = term()
%% Comment = string()
%% Reason = term()
%%
%% Spawns off a process (case process) that actually runs the test suite.
%% The case process will have the job process as group leader, which makes
%% it possible to capture all it's output from io:format/2, etc.
%%
%% The job process then sits down and waits for news from the case process.
%% This might be io requests (which are redirected to the log files).
%%
%% Returns a tuple with the time spent (in seconds) in the test case,
%% the return value from the test case or an {'EXIT',Reason} if the case
%% failed, Loc points out where the test case crashed (if it did). Loc
%% is either the name of the function, or {<Module>,<Line>} of the last
%% line executed that had a ?line macro. If the test case did execute
%% erase/0 or similar, it may be empty. Comment is the last comment added
%% by test_server:comment/1, the reason if test_server:fail has been
%% called or the comment given by the return value {comment,Comment} from
%% a test case.
%%
%% {died,Reason,unknown,Comment} is returned if the test case was killed
%% by some other process. Reason is the kill reason provided.
%%
%% TimetrapData = {MultiplyTimetrap,ScaleTimetrap}, which indicates a
%% possible extension of all timetraps. Timetraps will be multiplied by
%% MultiplyTimetrap. If it is infinity, no timetraps will be started at all.
%% ScaleTimetrap indicates if test_server should attemp to automatically
%% compensate timetraps for runtime delays introduced by e.g. tools like
%% cover.

run_test_case_apply({CaseNum,Mod,Func,Args,Name,RunInit,TimetrapData}) ->
    purify_format("Test case #~w ~w:~w/1", [CaseNum, Mod, Func]),
    case os:getenv("TS_RUN_VALGRIND") of
	false ->
	    ok;
	_ ->
	    os:putenv("VALGRIND_LOGFILE_INFIX",atom_to_list(Mod)++"."++
		      atom_to_list(Func)++"-")
    end,
    test_server_h:testcase({Mod,Func,1}),
    ProcBef = erlang:system_info(process_count),
    Result = run_test_case_apply(Mod, Func, Args, Name, RunInit, TimetrapData),
    ProcAft = erlang:system_info(process_count),
    purify_new_leaks(),
    DetFail = get(test_server_detected_fail),
    {Result,DetFail,ProcBef,ProcAft}.

run_test_case_apply(Mod, Func, Args, Name, RunInit, TimetrapData) ->
    case get(test_server_job_dir) of
	undefined ->
	    %% i'm a local target
	    do_run_test_case_apply(Mod, Func, Args, Name, RunInit, TimetrapData);
	JobDir ->
	    %% i'm a remote target
	    case Args of
		[Config] when is_list(Config) ->
		    {value,{data_dir,HostDataDir}} =
			lists:keysearch(data_dir, 1, Config),
		    DataBase = filename:basename(HostDataDir),
		    TargetDataDir = filename:join(JobDir, DataBase),
		    Config1 = lists:keyreplace(data_dir, 1, Config,
					       {data_dir,TargetDataDir}),
		    TargetPrivDir = filename:join(JobDir, ?priv_dir),
		    Config2 = lists:keyreplace(priv_dir, 1, Config1,
					       {priv_dir,TargetPrivDir}),
		    do_run_test_case_apply(Mod, Func, [Config2], Name, RunInit,
					   TimetrapData);
		_other ->
		    do_run_test_case_apply(Mod, Func, Args, Name, RunInit,
					   TimetrapData)
	    end
    end.
do_run_test_case_apply(Mod, Func, Args, Name, RunInit, TimetrapData) ->
    {ok,Cwd} = file:get_cwd(),
    Args2Print = case Args of
		     [Args1] when is_list(Args1) ->
			 lists:keydelete(tc_group_result, 1, Args1);
		     _ ->
			 Args
		 end,
    print(minor, "Test case started with:\n~s:~s(~p)\n", [Mod,Func,Args2Print]),
    print(minor, "Current directory is ~p\n", [Cwd]),
    print_timestamp(minor,"Started at "),
    TCCallback = get(test_server_testcase_callback),
    Ref = make_ref(),
    OldGLeader = group_leader(),
    %% Set ourself to group leader for the spawned process
    group_leader(self(),self()),
    Pid =
	spawn_link(
	  fun() ->
		  run_test_case_eval(Mod, Func, Args, Name, Ref,
				     RunInit, TimetrapData,
				     TCCallback)
	  end),
    group_leader(OldGLeader, self()),
    put(test_server_detected_fail, []),
    run_test_case_msgloop(Ref, Pid, false, false, "").

%% Ugly bug (pre R5A):
%% If this process (group leader of the test case) terminates before
%% all messages have been replied back to the io server, the io server
%% hangs. Fixed by the 20 milli timeout check here, and by using monitor in
%% io.erl (livrem OCH hangslen mao :)
%%
%% A test case is known to have failed if it returns {'EXIT', _} tuple,
%% or sends a message {failed, File, Line} to it's group_leader
%%
run_test_case_msgloop(Ref, Pid, CaptureStdout, Terminate, Comment) ->
    %% NOTE: Keep job_proxy_msgloop/0 up to date when changes
    %%       are made in this function!
    {Timeout,ReturnValue} =
	case Terminate of
	    {true, ReturnVal} ->
		{20, ReturnVal};
	    false ->
		{infinity, should_never_appear}
	end,
    receive
	{abort_current_testcase,Reason,From} ->
	    Line = get_loc(Pid),
	    Mon = erlang:monitor(process, Pid),
	    exit(Pid,{testcase_aborted,Reason,Line}),
	    erlang:yield(),
	    From ! {self(),abort_current_testcase,ok},
	    NewComment =
		receive
		    {'DOWN', Mon, process, Pid, _} ->
			Comment
		    after 10000 ->
			    %% Pid is probably trapping exits, hit it harder...
			    exit(Pid, kill),
			    %% here's the only place we know Reason, so we save
			    %% it as a comment, potentially replacing user data
			    Error = lists:flatten(io_lib:format("Aborted: ~p",[Reason])),
			    Error1 = lists:flatten([string:strip(S,left) ||
						    S <- string:tokens(Error,[$\n])]),
			    if length(Error1) > 63 ->
				    string:substr(Error1,1,60) ++ "...";
			       true ->
				    Error1
			    end
		    end,
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,NewComment);
        {io_request,From,ReplyAs,{put_chars,io_lib,Func,[Format,Args]}}
	when is_list(Format) ->
	    Msg = (catch io_lib:Func(Format,Args)),
	    run_test_case_msgloop_io(ReplyAs,CaptureStdout,Msg,From,Func),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,io_lib,Func,[Format,Args]}}
	when is_atom(Format) ->
	    Msg = (catch io_lib:Func(Format,Args)),
	    run_test_case_msgloop_io(ReplyAs,CaptureStdout,Msg,From,Func),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,Bytes}} ->
	    run_test_case_msgloop_io(
	      ReplyAs,CaptureStdout,Bytes,From,put_chars),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,unicode,io_lib,Func,[Format,Args]}}
	when is_list(Format) ->
	    Msg = unicode_to_latin1(catch io_lib:Func(Format,Args)),
	    run_test_case_msgloop_io(ReplyAs,CaptureStdout,Msg,From,Func),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,latin1,io_lib,Func,[Format,Args]}}
	when is_list(Format) ->
	    Msg = (catch io_lib:Func(Format,Args)),
	    run_test_case_msgloop_io(ReplyAs,CaptureStdout,Msg,From,Func),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,unicode,io_lib,Func,[Format,Args]}}
	when is_atom(Format) ->
	    Msg = unicode_to_latin1(catch io_lib:Func(Format,Args)),
	    run_test_case_msgloop_io(ReplyAs,CaptureStdout,Msg,From,Func),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,latin1,io_lib,Func,[Format,Args]}}
	when is_atom(Format) ->
	    Msg = (catch io_lib:Func(Format,Args)),
	    run_test_case_msgloop_io(ReplyAs,CaptureStdout,Msg,From,Func),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,unicode,Bytes}} ->
	    run_test_case_msgloop_io(
	      ReplyAs,CaptureStdout,unicode_to_latin1(Bytes),From,put_chars),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        {io_request,From,ReplyAs,{put_chars,latin1,Bytes}} ->
	    run_test_case_msgloop_io(
	      ReplyAs,CaptureStdout,Bytes,From,put_chars),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
        IoReq when element(1, IoReq) == io_request ->
	    %% something else, just pass it on
            group_leader() ! IoReq,
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
	{structured_io,ClientPid,Msg} ->
	    output(Msg, ClientPid),
            run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
	{capture,NewCapture} ->
            run_test_case_msgloop(Ref,Pid,NewCapture,Terminate,Comment);
	{sync_apply,From,MFA} ->
	    sync_local_or_remote_apply(false,From,MFA),
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
	{sync_apply_proxy,Proxy,From,MFA} ->
	    sync_local_or_remote_apply(Proxy,From,MFA),
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
	{printout,Detail,Format,Args} ->
	    print(Detail,Format,Args),
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
	{comment,NewComment} ->
	    Terminate1 =
		case Terminate of
		    {true,{Time,Value,Loc,Opts,_OldComment}} ->
			{true,{Time,Value,mod_loc(Loc),Opts,NewComment}};
		    Other ->
			Other
		end,
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate1,NewComment);
	{'EXIT',Pid,{Ref,Time,Value,Loc,Opts}} ->
	    RetVal = {Time/1000000,Value,mod_loc(Loc),Opts,Comment},
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,{true,RetVal},Comment);
	{'EXIT',Pid,Reason} ->
	    case Reason of
		{timetrap_timeout,TVal,Loc} ->
		    %% convert Loc to form that can be formatted
		    Loc1 = mod_loc(Loc),
		    {Mod,Func} = get_mf(Loc1),
		    %% The framework functions mustn't execute on this
		    %% group leader process or io will cause deadlock,
		    %% so we spawn a dedicated process for the operation
		    %% and let the group leader go back to handle io.
		    spawn_fw_call(Mod,Func,Pid,{timetrap_timeout,TVal},
				  Loc1,self(),Comment),
		    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
		{timetrap_timeout,TVal,Loc,InitOrEnd} ->
		    Loc1 = mod_loc(Loc),
		    {Mod,_Func} = get_mf(Loc1),
		    spawn_fw_call(Mod,InitOrEnd,Pid,{timetrap_timeout,TVal},
				  Loc1,self(),Comment),
		    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
		{testcase_aborted,Reason,Loc} ->
		    Loc1 = mod_loc(Loc),
		    {Mod,Func} = get_mf(Loc1),
		    spawn_fw_call(Mod,Func,Pid,{testcase_aborted,Reason},
				  Loc1,self(),Comment),
		    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
		killed ->
		    %% result of an exit(TestCase,kill) call, which is the
		    %% only way to abort a testcase process that traps exits
		    %% (see abort_current_testcase)
		    spawn_fw_call(undefined,undefined,Pid,testcase_aborted_or_killed,
				  unknown,self(),Comment),
		    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
		{fw_error,{FwMod,FwFunc,FwError}} ->
		    spawn_fw_call(FwMod,FwFunc,Pid,{framework_error,FwError},
				  unknown,self(),Comment),
		    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
		_ ->
		    %% the testcase has terminated because of Reason (e.g. an exit
		    %% because a linked process failed)
		    spawn_fw_call(undefined,undefined,Pid,Reason,
				  unknown,self(),Comment),
		    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment)
	    end;
	{_FwCallPid,fw_notify_done,RetVal} ->
	    %% the framework has been notified, we're finished
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,{true,RetVal},Comment);
 	{'EXIT',_FwCallPid,{fw_notify_done,Func,Error}} ->
	    %% a framework function failed
	    CB = os:getenv("TEST_SERVER_FRAMEWORK"),
	    Loc = case CB of
		      false ->
			  {test_server,Func};
		      _ ->
			  {list_to_atom(CB),Func}
		  end,
	    RetVal = {died,{framework_error,Loc,Error},Loc,"Framework error"},
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,{true,RetVal},Comment);
	{failed,File,Line} ->
	    put(test_server_detected_fail,
		[{File, Line}| get(test_server_detected_fail)]),
       	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
	_Other when not is_tuple(_Other) ->
	    %% ignore anything not generated by test server
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment);
	_Other when element(1, _Other) /= 'EXIT',
		    element(1, _Other) /= started,
		    element(1, _Other) /= finished,
		    element(1, _Other) /= print ->
	    %% ignore anything not generated by test server
	    run_test_case_msgloop(Ref,Pid,CaptureStdout,Terminate,Comment)
    after Timeout ->
	    ReturnValue
    end.

run_test_case_msgloop_io(ReplyAs,CaptureStdout,Msg,From,Func) ->
    case Msg of
	{'EXIT',_} ->
	    From ! {io_reply,ReplyAs,{error,Func}};
	_ ->
	    From ! {io_reply,ReplyAs,ok}
    end,
    if  CaptureStdout /= false ->
	    CaptureStdout ! {captured,Msg};
	true ->
	    ok
    end,
    output({minor,Msg},From).

output(Msg,Sender) ->
    local_or_remote_apply({test_server_ctrl,output,[Msg,Sender]}).

spawn_fw_call(Mod,{init_per_testcase,Func},Pid,{timetrap_timeout,TVal}=Why,
	      Loc,SendTo,Comment) ->
    FwCall =
	fun() ->
	    Skip = {skip,{failed,{Mod,init_per_testcase,Why}}},
	    %% if init_per_testcase fails, the test case
	    %% should be skipped
	    case catch test_server_sup:framework_call(
			 end_tc,[?pl2a(Mod),Func,{Pid,Skip,[[]]}]) of
		{'EXIT',FwEndTCErr} ->
		    exit({fw_notify_done,end_tc,FwEndTCErr});
		_ ->
		    ok
	    end,
	    %% finished, report back
	    SendTo ! {self(),fw_notify_done,
		      {TVal/1000,Skip,Loc,[],Comment}}
	end,
    spawn_link(FwCall);
spawn_fw_call(Mod,{end_per_testcase,Func},Pid,{timetrap_timeout,TVal}=Why,
	      Loc,SendTo,_Comment) ->
    FwCall =
	fun() ->
	    Conf = [{tc_status,ok}],
	    %% if end_per_testcase fails, the test case should be
	    %% reported successful with a warning printed as comment
	    case catch test_server_sup:framework_call(end_tc,
						      [?pl2a(Mod),Func,
						       {Pid,
							{failed,{Mod,end_per_testcase,Why}},
							[Conf]}]) of
		{'EXIT',FwEndTCErr} ->
		    exit({fw_notify_done,end_tc,FwEndTCErr});
		_ ->
		    ok
	    end,
	    %% finished, report back
	    SendTo ! {self(),fw_notify_done,
		      {TVal/1000,{error,{Mod,end_per_testcase,Why}},Loc,[],
		       ["<font color=\"red\">"
			"WARNING: end_per_testcase timed out!"
			"</font>"]}}
	end,
    spawn_link(FwCall);

spawn_fw_call(FwMod,FwFunc,_Pid,{framework_error,FwError},_,SendTo,_Comment) ->
    FwCall =
	fun() ->
		test_server_sup:framework_call(report, [framework_error,
							{{FwMod,FwFunc},FwError}]),
		Comment =
		    lists:flatten(
		      io_lib:format("<font color=\"red\">"
				    "WARNING! ~w:~w failed!</font>", [FwMod,FwFunc])),
	    %% finished, report back
	    SendTo ! {self(),fw_notify_done,
		      {died,{error,{FwMod,FwFunc,FwError}},{FwMod,FwFunc},[],Comment}}
	end,
    spawn_link(FwCall);

spawn_fw_call(Mod,Func,Pid,Error,Loc,SendTo,Comment) ->
    FwCall =
	fun() ->
		case catch fw_error_notify(Mod,Func,[],
					   Error,Loc) of
		    {'EXIT',FwErrorNotifyErr} ->
			exit({fw_notify_done,error_notification,
			      FwErrorNotifyErr});
		    _ ->
			ok
		end,
		Conf = [{tc_status,{failed,timetrap_timeout}}],
		case catch test_server_sup:framework_call(end_tc,
							  [?pl2a(Mod),Func,
							   {Pid,Error,[Conf]}]) of
		    {'EXIT',FwEndTCErr} ->
			exit({fw_notify_done,end_tc,FwEndTCErr});
		    _ ->
			ok
		end,
		%% finished, report back
		SendTo ! {self(),fw_notify_done,{died,Error,Loc,Comment}}
	end,
    spawn_link(FwCall).

%% The job proxy process forwards messages between the test case
%% process on a shielded node (and its descendants) and the job process.
%%
%% The job proxy process have to be started by the test-case process
%% on the shielded node!
start_job_proxy() ->
    group_leader(spawn(fun () -> job_proxy_msgloop() end), self()), ok.

%% The io_reply_proxy is not the most satisfying solution but it works...
io_reply_proxy(ReplyTo) ->
    receive
	IoReply when is_tuple(IoReply),
		     element(1, IoReply) == io_reply ->
	    ReplyTo ! IoReply;
	_ ->
	    io_reply_proxy(ReplyTo)
    end.

job_proxy_msgloop() ->
    receive

	%%
	%% Messages that need intervention by proxy...
	%%

	%% io stuff ...
	IoReq when tuple_size(IoReq) >= 2,
	           element(1, IoReq) == io_request ->

	    ReplyProxy = spawn(fun () -> io_reply_proxy(element(2, IoReq)) end),
	    group_leader() ! setelement(2, IoReq, ReplyProxy);

	%% test_server stuff...
	{sync_apply, From, MFA} ->
	    group_leader() ! {sync_apply_proxy, self(), From, MFA};
	{sync_result_proxy, To, Result} ->
	    To ! {sync_result, Result};

	%%
	%% Messages that need no intervention by proxy...
	%%
        Msg ->
	    group_leader() ! Msg
    end,
    job_proxy_msgloop().

%% A test case is known to have failed if it returns {'EXIT', _} tuple,
%% or sends a message {failed, File, Line} to it's group_leader

run_test_case_eval(Mod, Func, Args0, Name, Ref, RunInit,
		   TimetrapData, TCCallback) ->

    put(test_server_multiply_timetraps,TimetrapData),

    {{Time,Value},Loc,Opts} =
	case test_server_sup:framework_call(init_tc,[?pl2a(Mod),Func,Args0],
					    {ok,Args0}) of
	    {ok,Args} ->
		run_test_case_eval1(Mod, Func, Args, Name, RunInit, TCCallback);
	    Error = {error,_Reason} ->
		test_server_sup:framework_call(end_tc,[?pl2a(Mod),Func,{Error,Args0}]),
		{{0,{skip,{failed,Error}}},{Mod,Func},[]};
	    {fail,Reason} ->
		[Conf] = Args0,
		Conf1 = [{tc_status,{failed,Reason}} | Conf],
		fw_error_notify(Mod, Func, Conf, Reason),
		test_server_sup:framework_call(end_tc,[?pl2a(Mod),Func,
						       {{error,Reason},[Conf1]}]),
		{{0,{failed,Reason}},{Mod,Func},[]};
	    Skip = {skip,_Reason} ->
		test_server_sup:framework_call(end_tc,[?pl2a(Mod),Func,{Skip,Args0}]),
		{{0,Skip},{Mod,Func},[]};
	    {auto_skip,Reason} ->
		test_server_sup:framework_call(end_tc,[?pl2a(Mod),
						       Func,
						       {{skip,Reason},Args0}]),
		{{0,{skip,{fw_auto_skip,Reason}}},{Mod,Func},[]}
	end,
    exit({Ref,Time,Value,Loc,Opts}).

run_test_case_eval1(Mod, Func, Args, Name, RunInit, TCCallback) ->
    case RunInit of
	run_init ->
	    put(test_server_init_or_end_conf,{init_per_testcase,Func}),
	    put(test_server_loc, {Mod,{init_per_testcase,Func}}),
	    ensure_timetrap(Args),
	    case init_per_testcase(Mod, Func, Args) of
		Skip = {skip,Reason} ->
		    Line = get_loc(),
		    Conf = [{tc_status,{skipped,Reason}}],
		    test_server_sup:framework_call(end_tc,[?pl2a(Mod),Func,{Skip,[Conf]}]),
		    {{0,{skip,Reason}},Line,[]};
		{skip_and_save,Reason,SaveCfg} ->
		    Line = get_loc(),
		    Conf = [{tc_status,{skipped,Reason}},{save_config,SaveCfg}],
		    test_server_sup:framework_call(end_tc,[?pl2a(Mod),Func,
							   {{skip,Reason},[Conf]}]),
		    {{0,{skip,Reason}},Line,[]};
		{ok,NewConf} ->
		    put(test_server_init_or_end_conf,undefined),
		    %% call user callback function if defined
		    NewConf1 = user_callback(TCCallback, Mod, Func, init, NewConf),
		    put(test_server_loc, {Mod,Func}),
		    %% execute the test case
		    {{T,Return},Loc} = {ts_tc(Mod, Func, [NewConf1]),get_loc()},
		    {EndConf,TSReturn,FWReturn} =
			case Return of
			    {E,TCError} when E=='EXIT' ; E==failed ->
				fw_error_notify(Mod, Func, NewConf1,
						TCError, mod_loc(Loc)),
				{[{tc_status,{failed,TCError}}|NewConf1],
				 Return,{error,TCError}};
			    SaveCfg={save_config,_} ->
				{[{tc_status,ok},SaveCfg|NewConf1],Return,ok};
			    {skip_and_save,Why,SaveCfg} ->
				Skip = {skip,Why},
				{[{tc_status,{skipped,Why}},{save_config,SaveCfg}|NewConf1],
				 Skip,Skip};
			    {skip,Why} ->
				{[{tc_status,{skipped,Why}}|NewConf1],Return,Return};
			    _ ->
				{[{tc_status,ok}|NewConf1],Return,ok}
			end,
		    %% call user callback function if defined
		    EndConf1 = user_callback(TCCallback, Mod, Func, 'end', EndConf),
		    {FWReturn1,TSReturn1,EndConf2} =
			case end_per_testcase(Mod, Func, EndConf1) of
			    SaveCfg1={save_config,_} ->
				{FWReturn,TSReturn,[SaveCfg1|lists:keydelete(save_config, 1, EndConf1)]};
			    {fail,ReasonToFail} ->                       % user has failed the testcase
				fw_error_notify(Mod, Func, EndConf1, ReasonToFail),
				{{error,ReasonToFail},{failed,ReasonToFail},EndConf1};
			    {failed,{_,end_per_testcase,_}} = Failure -> % unexpected termination
				{Failure,TSReturn,EndConf1};
			    _ ->
				{FWReturn,TSReturn,EndConf1}
			end,
		    case test_server_sup:framework_call(end_tc, [?pl2a(Mod), Func,
								 {FWReturn1,[EndConf2]}]) of
			{fail,Reason} ->
			    fw_error_notify(Mod, Func, EndConf2, Reason),
			    {{T,{failed,Reason}},{Mod,Func},[]};
			_ ->
			    {{T,TSReturn1},Loc,[]}
		    end
	    end;
	skip_init ->
	    %% call user callback function if defined
	    Args1 = user_callback(TCCallback, Mod, Func, init, Args),
	    ensure_timetrap(Args1),
	    %% ts_tc does a catch
	    put(test_server_loc, {Mod,Func}),
	    %% if this is a named conf group, the test case (init or end conf)
	    %% should be called with the name as the first argument
	    Args2 = if Name == undefined -> Args1;
		       true -> [Name | Args1]
		    end,
	    %% execute the conf test case
	    {{T,Return},Loc} = {ts_tc(Mod, Func, Args2),get_loc()},
	    %% call user callback function if defined
	    Return1 = user_callback(TCCallback, Mod, Func, 'end', Return),
	    {Return2,Opts} = process_return_val([Return1], Mod,Func,Args1, Loc, Return1),
	    {{T,Return2},Loc,Opts}
    end.

%% the return value is a list and we have to check if it contains
%% the result of an end conf case or if it's a Config list
process_return_val([Return], M,F,A, Loc, Final) when is_list(Return) ->
    ReturnTags = [skip,skip_and_save,save_config,comment,return_group_result],
    %% check if all elements in the list are valid end conf return value tuples
    case lists:all(fun(Val) when is_tuple(Val) ->
			   lists:any(fun(T) -> T == element(1, Val) end, ReturnTags);
		      (ok) ->
			   true;
		      (_) ->
			   false
		   end, Return) of
	true ->		     % must be return value from end conf case
	    process_return_val1(Return, M,F,A, Loc, Final, []);
	false ->	     % must be Config value from init conf case
	    test_server_sup:framework_call(end_tc, [?pl2a(M),F,{ok,A}]),
	    {Return,[]}
    end;
%% the return value is not a list, so it's the return value from an
%% end conf case or it's a dummy value that can be ignored
process_return_val(Return, M,F,A, Loc, Final) ->
    process_return_val1(Return, M,F,A, Loc, Final, []).

process_return_val1([Failed={E,TCError}|_], M,F,A=[Args], Loc, _, SaveOpts) when E=='EXIT';
										 E==failed ->
    fw_error_notify(M,F,A, TCError, mod_loc(Loc)),
    test_server_sup:framework_call(end_tc,
				   [?pl2a(M),F,{{error,TCError},
						[[{tc_status,{failed,TCError}}|Args]]}]),
    {Failed,SaveOpts};
process_return_val1([SaveCfg={save_config,_}|Opts], M,F,[Args], Loc, Final, SaveOpts) ->
    process_return_val1(Opts, M,F,[[SaveCfg|Args]], Loc, Final, SaveOpts);
process_return_val1([{skip_and_save,Why,SaveCfg}|Opts], M,F,[Args], Loc, _, SaveOpts) ->
    process_return_val1(Opts, M,F,[[{save_config,SaveCfg}|Args]], Loc, {skip,Why}, SaveOpts);
process_return_val1([GR={return_group_result,_}|Opts], M,F,A, Loc, Final, SaveOpts) ->
    process_return_val1(Opts, M,F,A, Loc, Final, [GR|SaveOpts]);
process_return_val1([RetVal={Tag,_}|Opts], M,F,A, Loc, _, SaveOpts) when Tag==skip;
									 Tag==comment ->
    process_return_val1(Opts, M,F,A, Loc, RetVal, SaveOpts);
process_return_val1([_|Opts], M,F,A, Loc, Final, SaveOpts) ->
    process_return_val1(Opts, M,F,A, Loc, Final, SaveOpts);
process_return_val1([], M,F,A, _Loc, Final, SaveOpts) ->
    test_server_sup:framework_call(end_tc, [?pl2a(M),F,{Final,A}]),
    {Final,lists:reverse(SaveOpts)}.

user_callback(undefined, _, _, _, Args) ->
    Args;
user_callback({CBMod,CBFunc}, Mod, Func, InitOrEnd, [Args]) when is_list(Args) ->
    case catch apply(CBMod, CBFunc, [InitOrEnd,Mod,Func,Args]) of
	Args1 when is_list(Args1) ->
	    [Args1];
	_ ->
	    [Args]
    end;
user_callback({CBMod,CBFunc}, Mod, Func, InitOrEnd, Args) ->
    case catch apply(CBMod, CBFunc, [InitOrEnd,Mod,Func,Args]) of
	Args1 when is_list(Args1) ->
	    Args1;
	_ ->
	    Args
    end.

init_per_testcase(Mod, Func, Args) ->
    case code:is_loaded(Mod) of
	false -> code:load_file(Mod);
	_ -> ok
    end,
    %% init_per_testcase defined, returns new configuration
    case erlang:function_exported(Mod,init_per_testcase,2) of
	true ->
	    case catch my_apply(Mod, init_per_testcase, [Func|Args]) of
		{'$test_server_ok',{Skip,Reason}} when Skip==skip;
						       Skip==skipped ->
		    {skip,Reason};
		{'$test_server_ok',Res={skip_and_save,_,_}} ->
		    Res;
		{'$test_server_ok',NewConf} when is_list(NewConf) ->
		    case lists:filter(fun(T) when is_tuple(T) -> false;
					 (_) -> true end, NewConf) of
			[] ->
			    {ok,NewConf};
			Bad ->
			    group_leader() ! {printout,12,
					      "ERROR! init_per_testcase has returned "
					      "bad elements in Config: ~p\n",[Bad]},
			    {skip,{failed,{Mod,init_per_testcase,bad_return}}}
		    end;
		{'$test_server_ok',_Other} ->
		    group_leader() ! {printout,12,
				      "ERROR! init_per_testcase did not return "
				      "a Config list.\n",[]},
		    {skip,{failed,{Mod,init_per_testcase,bad_return}}};
		{'EXIT',Reason} ->
		    Line = get_loc(),
		    FormattedLoc = test_server_sup:format_loc(mod_loc(Line)),
		    group_leader() ! {printout,12,
				      "ERROR! init_per_testcase crashed!\n"
				      "\tLocation: ~s\n\tReason: ~p\n",
				      [FormattedLoc,Reason]},
		    {skip,{failed,{Mod,init_per_testcase,Reason}}};
		Other ->
		    Line = get_loc(),
		    FormattedLoc = test_server_sup:format_loc(mod_loc(Line)),
		    group_leader() ! {printout,12,
				      "ERROR! init_per_testcase thrown!\n"
				      "\tLocation: ~s\n\tReason: ~p\n",
				      [FormattedLoc, Other]},
		    {skip,{failed,{Mod,init_per_testcase,Other}}}
	    end;
	false ->
	    %% Optional init_per_testcase not defined
	    %% keep quiet.
	    [Config] = Args,
	    {ok, Config}
    end.

end_per_testcase(Mod, Func, Conf) ->
    case erlang:function_exported(Mod,end_per_testcase,2) of
	true ->
	    do_end_per_testcase(Mod,end_per_testcase,Func,Conf);
	false ->
	    %% Backwards compatibility!
	    case erlang:function_exported(Mod,fin_per_testcase,2) of
		true ->
		    do_end_per_testcase(Mod,fin_per_testcase,Func,Conf);
		false ->
		    ok
	    end
    end.

do_end_per_testcase(Mod,EndFunc,Func,Conf) ->
    put(test_server_init_or_end_conf,{EndFunc,Func}),
    put(test_server_loc, {Mod,{EndFunc,Func}}),
    case catch my_apply(Mod, EndFunc, [Func,Conf]) of
	{'$test_server_ok',SaveCfg={save_config,_}} ->
	    SaveCfg;
	{'$test_server_ok',{fail,_}=Fail} ->
	    Fail;
	{'$test_server_ok',_} ->
	    ok;
	{'EXIT',Reason} = Why ->
	    comment(io_lib:format("<font color=\"red\">"
				  "WARNING: ~w crashed!"
				  "</font>\n",[EndFunc])),
	    group_leader() ! {printout,12,
			      "WARNING: ~w crashed!\n"
			      "Reason: ~p\n"
			      "Line: ~s\n",
			      [EndFunc, Reason,
			       test_server_sup:format_loc(
				 mod_loc(get_loc()))]},
	    {failed,{Mod,end_per_testcase,Why}};
	Other ->
	    comment(io_lib:format("<font color=\"red\">"
				  "WARNING: ~w thrown!"
				  "</font>\n",[EndFunc])),
	    group_leader() ! {printout,12,
			      "WARNING: ~w thrown!\n"
			      "Reason: ~p\n"
			      "Line: ~s\n",
			      [EndFunc, Other,
			       test_server_sup:format_loc(
				 mod_loc(get_loc()))]},
	    {failed,{Mod,end_per_testcase,Other}}
    end.

get_loc() ->
    case catch test_server_line:get_lines() of
	[] ->
	    get(test_server_loc);
	{'EXIT',_} ->
	    get(test_server_loc);
	Loc ->
	    Loc
    end.

get_loc(Pid) ->
    {dictionary,Dict} = process_info(Pid, dictionary),
    lists:foreach(fun({Key,Val}) -> put(Key,Val) end,Dict),
    get_loc().

get_mf([{M,F,_}|_]) -> {M,F};
get_mf([{M,F}|_])   -> {M,F};
get_mf(_)           -> {undefined,undefined}.

mod_loc(Loc) ->
    %% handle diff line num versions
    case Loc of
	[{{_M,_F},_L}|_] ->
	    [{?pl2a(M),F,L} || {{M,F},L} <- Loc];
	[{_M,_F}|_] ->
	    [{?pl2a(M),F} || {M,F} <- Loc];
	{{M,F},L} ->
	    [{?pl2a(M),F,L}];
	{M,ForL} ->
	    [{?pl2a(M),ForL}];
	_ ->
	    Loc
    end.


fw_error_notify(Mod, Func, Args, Error) ->
    test_server_sup:framework_call(error_notification,
				   [?pl2a(Mod),Func,[Args],
				    {Error,unknown}]).
fw_error_notify(Mod, Func, Args, Error, Loc) ->
    test_server_sup:framework_call(error_notification,
				   [?pl2a(Mod),Func,[Args],
				    {Error,Loc}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print(Detail,Format,Args) -> ok
%% Detail = integer()
%% Format = string()
%% Args = [term()]
%%
%% Just like io:format, except that depending on the Detail value, the output
%% is directed to console, major and/or minor log files.

print(Detail,Format,Args) ->
    local_or_remote_apply({test_server_ctrl,print,[Detail,Format,Args]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print_timsteamp(Detail,Leader) -> ok
%%
%% Prints Leader followed by a time stamp (date and time). Depending on
%% the Detail value, the output is directed to console, major and/or minor
%% log files.

print_timestamp(Detail,Leader) ->
    local_or_remote_apply({test_server_ctrl,print_timestamp,[Detail,Leader]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lookup_config(Key,Config) -> {value,{Key,Value}} | undefined
%% Key = term()
%% Value = term()
%% Config = [{Key,Value},...]
%%
%% Looks up a specific key in the config list, and returns the value
%% of the associated key, or undefined if the key doesn't exist.

lookup_config(Key,Config) ->
    case lists:keysearch(Key,1,Config) of
	{value,{Key,Val}} ->
	    Val;
	_ ->
	    io:format("Could not find element ~p in Config.~n",[Key]),
	    undefined
    end.

%% timer:tc/3
ts_tc(M, F, A) ->
    Before = erlang:now(),
    Val = (catch my_apply(M, F, A)),
    After = erlang:now(),
    Result = case Val of
		 {'$test_server_ok', R} ->
		     R; % test case ok
		 {'EXIT',_Reason} = R ->
		     R; % test case crashed
		 Other ->
		     {failed, {thrown,Other}} % test case was thrown
	  end,
    Elapsed =
	(element(1,After)*1000000000000
	 +element(2,After)*1000000+element(3,After)) -
	(element(1,Before)*1000000000000
	 +element(2,Before)*1000000+element(3,Before)),
    {Elapsed, Result}.

my_apply(M, F, A) ->
    {'$test_server_ok',apply(M, F, A)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                TEST SUITE SUPPORT FUNCTIONS                      %%
%%                                                                  %%
%% Note: Some of these functions have been moved to test_server_sup %%
%%       in an attempt to keep this modules small (yeah, right!)    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unicode_to_latin1(Chars) when is_list(Chars); is_binary(Chars) ->
    lists:flatten(
      [ case X of
	    High when High > 255 ->
		io_lib:format("\\{~.8B}",[X]);
	    Low ->
		Low
	end || X <- unicode:characters_to_list(Chars,unicode) ]);
unicode_to_latin1(Garbage) ->
    Garbage.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% format(Format) -> IoLibReturn
%% format(Detail,Format) -> IoLibReturn
%% format(Format,Args) -> IoLibReturn
%% format(Detail,Format,Args) -> IoLibReturn
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
	case catch io_lib:format(Format,Args) of
	    {'EXIT',_} ->
		io_lib:format("illegal format; ~p with args ~p.\n",
			      [Format,Args]);
	    Valid -> Valid
	end,
    log({Detail, Str}).

log(Msg) ->
    group_leader() ! {structured_io, self(), Msg},
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% capture_start() -> ok
%% capture_stop() -> ok
%%
%% Starts/stops capturing all output from io:format, and similar. Capturing
%% output doesn't stop output from happening. It just makes it possible
%% to retrieve the output using capture_get/0.
%% Starting and stopping capture doesn't affect already captured output.
%% All output is stored as messages in the message queue until retrieved

capture_start() ->
    group_leader() ! {capture,self()},
    ok.

capture_stop() ->
    group_leader() ! {capture,false},
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% capture_get() -> Output
%% Output = [string(),...]
%%
%% Retrieves all the captured output since last call to capture_get/0.
%% Note that since output arrive as messages to the process, it takes
%% a short while from the call to io:format until all output is available
%% by capture_get/0. It is not necessary to call capture_stop/0 before
%% retreiving the output.
capture_get() ->
    test_server_sup:capture_get([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% messages_get() -> Messages
%% Messages = [term(),...]
%%
%% Returns all messages in the message queue.
messages_get() ->
    test_server_sup:messages_get([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sleep(Time) -> ok
%% Time = integer() | float() | infinity
%%
%% Sleeps the specified number of milliseconds. This sleep also accepts
%% floating point numbers (which are truncated) and the atom 'infinity'.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% adjusted_sleep(Time) -> ok
%% Time = integer() | float() | infinity
%%
%% Sleeps the specified number of milliseconds, multiplied by the
%% 'multiply_timetraps' value (if set) and possibly also automatically scaled
%% up if 'scale_timetraps' is set to true (which is default).
%% This function also accepts floating point numbers (which are truncated) and
%% the atom 'infinity'.
adjusted_sleep(infinity) ->
    receive
    after infinity ->
	    ok
    end;
adjusted_sleep(MSecs) ->
    {Multiplier,ScaleFactor} =
	case test_server_ctrl:get_timetrap_parameters() of
	    {undefined,undefined} ->
		{1,1};
	    {undefined,false} ->
		{1,1};
	    {undefined,true} ->
		{1,timetrap_scale_factor()};
	    {infinity,_} ->
		{infinity,1};
	    {Mult,undefined} ->
		{Mult,1};
	    {Mult,false} ->
		{Mult,1};
	    {Mult,true} ->
		{Mult,timetrap_scale_factor()}
	end,
    receive
    after trunc(MSecs*Multiplier*ScaleFactor) ->
	    ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fail(Reason) -> exit({suite_failed,Reason})
%%
%% Immediately calls exit. Included because test suites are easier
%% to read when using this function, rather than exit directly.
fail(Reason) ->
    comment(cast_to_list(Reason)),
    exit({suite_failed,Reason}).

cast_to_list(X) when is_list(X) -> X;
cast_to_list(X) when is_atom(X) -> atom_to_list(X);
cast_to_list(X) -> lists:flatten(io_lib:format("~p", [X])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fail() -> exit(suite_failed)
%%
%% Immediately calls exit. Included because test suites are easier
%% to read when using this function, rather than exit directly.
fail() ->
    exit(suite_failed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% break(Comment) -> ok
%%
%% Break a test case so part of the test can be done manually.
%% Use continue/0 to continue.
break(Comment) ->
    case erase(test_server_timetraps) of
	undefined -> ok;
	List -> lists:foreach(fun(Ref) -> timetrap_cancel(Ref) end,List)
    end,
    io:format(user,
	      "\n\n\n--- SEMIAUTOMATIC TESTING ---"
	      "\nThe test case executes on process ~w"
	      "\n\n\n~s"
	      "\n\n\n-----------------------------\n\n"
	      "Continue with --> test_server:continue().\n",
	      [self(),Comment]),
    case whereis(test_server_break_process) of
	undefined ->
	    spawn_break_process(self());
	OldBreakProcess ->
	    OldBreakProcess ! cancel,
	    spawn_break_process(self())
    end,
    receive continue -> ok end.

spawn_break_process(Pid) ->
    spawn(fun() ->
		  register(test_server_break_process,self()),
		  receive
		      continue -> continue(Pid);
		      cancel -> ok
		  end
	  end).

continue() ->
    case whereis(test_server_break_process) of
	undefined ->
	     ok;
	BreakProcess ->
	    BreakProcess ! continue
    end.

continue(Pid) ->
    Pid ! continue.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap_scale_factor() -> Factor
%%
%% Returns the amount to scale timetraps with.

timetrap_scale_factor() ->
    F0 = case test_server:purify_is_running() of
	    true -> 5;
	    false -> 1
	end,
    F1 = case {is_debug(), has_lock_checking()} of
	     {true,_} -> 6 * F0;
	     {false,true} -> 2 * F0;
	     {false,false} -> F0
	 end,
    F2 = case has_superfluous_schedulers() of
	     true -> 3*F1;
	     false -> F1
	 end,
    F = case test_server_sup:get_os_family() of
	    vxworks -> 5 * F2;
	    _ -> F2
	end,
    case test_server:is_cover() of
	true -> 10 * F;
	false -> F
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap(Timeout) -> Handle
%% Handle = term()
%%
%% Creates a time trap, that will kill the calling process if the
%% trap is not cancelled with timetrap_cancel/1, within Timeout milliseconds.

timetrap(Timeout0) ->
    Timeout = time_ms(Timeout0),
    cancel_default_timetrap(),
    case get(test_server_multiply_timetraps) of
	undefined -> timetrap1(Timeout, true);
	{infinity,_} -> infinity;
	{Int,Scale} -> timetrap1(Timeout*Int, Scale)
    end.

timetrap1(Timeout, Scale) ->
    Ref = spawn_link(test_server_sup,timetrap,[Timeout,Scale,self()]),
    case get(test_server_timetraps) of
	undefined -> put(test_server_timetraps,[Ref]);
	List -> put(test_server_timetraps,[Ref|List])
    end,
    Ref.

ensure_timetrap(Config) ->
    case get(test_server_timetraps) of
	[_|_] ->
	    ok;
	_ ->
	    case get(test_server_default_timetrap) of
		undefined -> ok;
		Garbage ->
		    erase(test_server_default_timetrap),
		    format("=== WARNING: garbage in test_server_default_timetrap: ~p~n",
			   [Garbage])
	    end,
	    DTmo = case lists:keysearch(default_timeout,1,Config) of
		       {value,{default_timeout,Tmo}} -> Tmo;
		       _ -> ?DEFAULT_TIMETRAP_SECS
		   end,
	    format("=== test_server setting default timetrap of ~p seconds~n",
		   [DTmo]),
	    put(test_server_default_timetrap, timetrap(seconds(DTmo)))
    end.

cancel_default_timetrap() ->
    case get(test_server_default_timetrap) of
	undefined ->
	    ok;
	TimeTrap when is_pid(TimeTrap) ->
	    timetrap_cancel(TimeTrap),
	    erase(test_server_default_timetrap),
	    format("=== test_server canceled default timetrap since another timetrap was set~n"),
	    ok;
	Garbage ->
	    erase(test_server_default_timetrap),
	    format("=== WARNING: garbage in test_server_default_timetrap: ~p~n",
		   [Garbage]),
	    error
    end.


time_ms({hours,N}) -> hours(N);
time_ms({minutes,N}) -> minutes(N);
time_ms({seconds,N}) -> seconds(N);
time_ms({Other,_N}) ->
    format("=== ERROR: Invalid time specification: ~p. "
	   "Should be seconds, minutes, or hours.~n", [Other]),
    exit({invalid_time_spec,Other});
time_ms(Ms) when is_integer(Ms) -> Ms;
time_ms(Other) -> exit({invalid_time_spec,Other}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap_cancel(Handle) -> ok
%% Handle = term()
%%
%% Cancels a time trap.
timetrap_cancel(infinity) ->
    ok;
timetrap_cancel(Handle) ->
    case get(test_server_timetraps) of
	undefined -> ok;
	[Handle] -> erase(test_server_timetraps);
	List -> put(test_server_timetraps,lists:delete(Handle,List))
    end,
    test_server_sup:timetrap_cancel(Handle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hours(N) -> Milliseconds
%% minutes(N) -> Milliseconds
%% seconds(N) -> Milliseconds
%% N = integer() | float()
%% Milliseconds = integer()
%%
%% Transforms the named units to milliseconds. Fractions in the input
%% are accepted. The output is an integer.
hours(N)   -> trunc(N * 1000 * 60 * 60).
minutes(N) -> trunc(N * 1000 * 60).
seconds(N) -> trunc(N * 1000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timecall(M,F,A) -> {Time,Val}
%% Time = float()
%%
%% Measures the time spent evaluating MFA. The measurement is done with
%% erlang:now/0, and should have pretty good accuracy on most platforms.
%% The function is not evaluated in a catch context.
timecall(M, F, A) ->
    test_server_sup:timecall(M,F,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% do_times(N,M,F,A) -> ok
%% do_times(N,Fun) ->
%% N = integer()
%% Fun = fun() -> void()
%%
%% Evaluates MFA or Fun N times, and returns ok.
do_times(N,M,F,A) when N>0 ->
    apply(M,F,A),
    do_times(N-1,M,F,A);
do_times(0,_,_,_) ->
    ok.

do_times(N,Fun) when N>0 ->
    Fun(),
    do_times(N-1,Fun);
do_times(0,_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% m_out_of_n(M,N,Fun) -> ok | exit({m_out_of_n_failed,{R,left_to_do}})
%% M = integer()
%% N = integer()
%% Fun = fun() -> void()
%% R = integer()
%%
%% Repeats evaluating the given function until it succeeded (didn't crash)
%% M times. If, after N times, M successful attempts have not been
%% accomplished, the process crashes with reason {m_out_of_n_failed
%% {R,left_to_do}}, where R indicates how many cases that remained to be
%% successfully completed.
%%
%% For example:
%% m_out_of_n(1,4,fun() -> tricky_test_case() end)
%%                           Tries to run tricky_test_case() up to 4 times,
%%                           and is happy if it succeeds once.
%%
%% m_out_of_n(7,8,fun() -> clock_sanity_check() end)
%%                         Tries running clock_sanity_check() up to 8
%%                         times and allows the function to fail once.
%%                         This might be useful if clock_sanity_check/0
%%                         is known to fail if the clock crosses an hour
%%                         boundary during the test (and the up to 8
%%                         test runs could never cross 2 boundaries)
m_out_of_n(0,_,_) ->
    ok;
m_out_of_n(M,0,_) ->
    exit({m_out_of_n_failed,{M,left_to_do}});
m_out_of_n(M,N,Fun) ->
    case catch Fun() of
	{'EXIT',_} ->
	    m_out_of_n(M,N-1,Fun);
	_Other ->
	    m_out_of_n(M-1,N-1,Fun)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%call_crash(M,F,A)
%%call_crash(Time,M,F,A)
%%call_crash(Time,Crash,M,F,A)
%%	M     - atom()
%%	F     - atom()
%%	A     - [term()]
%%	Time  - integer() in milliseconds.
%%	Crash - term()
%%
%%	Spaws a new process that calls MFA. The call is considered
%%      successful if the call crashes with the given reason (Crash),
%%      or any other reason if Crash is not specified.
%%	** The call must terminate withing the given Time (defaults
%%      to infinity), or it is considered a failure (exit with reason
%%      'call_crash_timeout' is generated).

call_crash(M,F,A) ->
    call_crash(infinity,M,F,A).
call_crash(Time,M,F,A) ->
    call_crash(Time,any,M,F,A).
call_crash(Time,Crash,M,F,A) ->
    test_server_sup:call_crash(Time,Crash,M,F,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_node(SlaveName, Type, Options) ->
%%                   {ok, Slave} | {error, Reason}
%%
%% SlaveName = string(), atom().
%% Type = slave | peer
%% Options = [{tuple(), term()}]
%%
%% OptionList is a tuplelist wich may contain one
%% or more of these members:
%%
%% Slave and Peer:
%% {remote, true}         - Start the node on a remote host. If not specified,
%%                          the node will be started on the local host (with
%%                          some exceptions, for instance VxWorks,
%%                          where all nodes are started on a remote host).
%% {args, Arguments}      - Arguments passed directly to the node.
%% {cleanup, false}       - Nodes started with this option will not be killed
%%                          by the test server after completion of the test case
%%                          Therefore it is IMPORTANT that the USER terminates
%%                          the node!!
%% {erl, ReleaseList}     - Use an Erlang emulator determined by ReleaseList
%%                          when starting nodes, instead of the same emulator
%%                          as the test server is running. ReleaseList is a list
%%                          of specifiers, where a specifier is either
%%                          {release, Rel}, {prog, Prog}, or 'this'. Rel is
%%                          either the name of a release, e.g., "r7a" or
%%                          'latest'. 'this' means using the same emulator as
%%                          the test server. Prog is the name of an emulator
%%                          executable.  If the list has more than one element,
%%                          one of them is picked randomly. (Only
%%                          works on Solaris and Linux, and the test
%%                          server gives warnings when it notices that
%%                          nodes are not of the same version as
%%                          itself.)
%%
%% Peer only:
%% {wait, false}	  - Don't wait for the node to be started.
%% {fail_on_error, false} - Returns {error, Reason} rather than failing
%%			    the test case. This option can only be used with
%%                          peer nodes.
%%                          Note that slave nodes always act as if they had
%%                          fail_on_error==false.
%%

start_node(Name, Type, Options) ->
    lists:foreach(
      fun(N) ->
	      case firstname(N) of
		  Name ->
		      format("=== WARNING: Trying to start node \'~w\' when node"
			     " with same first name exists: ~w", [Name, N]);
		  _other -> ok
	      end
      end,
      nodes()),

    group_leader() ! {sync_apply,
		      self(),
		      {test_server_ctrl,start_node,[Name,Type,Options]}},
    Result = receive {sync_result,R} -> R end,

    case Result of
	{ok,Node} ->

            %% Cannot run cover on shielded node or on a node started
            %% by a shielded node.
            Cover = case is_cover() of
                        true ->
                            not is_shielded(Name) andalso same_version(Node);
                        false ->
                            false
                    end,

	    net_adm:ping(Node),
	    case Cover of
		true ->
		    Sticky = unstick_all_sticky(Node),
		    cover:start(Node),
		    stick_all_sticky(Node,Sticky);
		_ ->
		    ok
	    end,
	    {ok,Node};
	{fail,Reason} -> fail(Reason);
	Error -> Error
    end.

firstname(N) ->
    list_to_atom(upto($@,atom_to_list(N))).

%% This should!!! crash if H is not member in list.
upto(H, [H | _T]) -> [];
upto(H, [X | T]) -> [X | upto(H,T)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wait_for_node(Name) -> ok | {error,timeout}
%%
%% If a node is started with the options {wait,false}, this function
%% can be used to wait for the node to come up from the
%% test server point of view (i.e. wait until it has contacted
%% the test server controller after startup)
wait_for_node(Slave) ->
    group_leader() ! {sync_apply,
		      self(),
		      {test_server_ctrl,wait_for_node,[Slave]}},
    receive {sync_result,R} -> R end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stop_node(Name) -> true|false
%%
%% Kills a (remote) node.
%% Also inform test_server_ctrl so it can clean up!
stop_node(Slave) ->
    Nocover = is_shielded(Slave) orelse not same_version(Slave),
    case is_cover() of
	true when not Nocover ->
	    Sticky = unstick_all_sticky(Slave),
	    cover:stop(Slave),
	    stick_all_sticky(Slave,Sticky);
	_ ->
	    ok
    end,
    group_leader() ! {sync_apply,self(),{test_server_ctrl,stop_node,[Slave]}},
    Result = receive {sync_result,R} -> R end,
    case Result of
	ok ->
	    erlang:monitor_node(Slave, true),
	    slave:stop(Slave),
	    receive
		{nodedown, Slave} ->
		    format(minor, "Stopped slave node: ~p", [Slave]),
		    format(major, "=node_stop     ~p", [Slave]),
		    true
	    after 30000 ->
		    format("=== WARNING: Node ~p does not seem to terminate.",
			   [Slave]),
		    false
	    end;
	{error, _Reason} ->
	    %% Either, the node is already dead or it was started
	    %% with the {cleanup,false} option, or it was started
	    %% in some other way than test_server:start_node/3
	    format("=== WARNING: Attempt to stop a nonexisting slavenode (~p)~n"
		   "===          Trying to kill it anyway!!!",
		   [Slave]),
	    case net_adm:ping(Slave)of
		pong ->
		    slave:stop(Slave),
		    true;
		pang ->
		    false
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_release_available(Release) -> true | false
%% Release -> string()
%%
%% Test if a release (such as "r10b") is available to be
%% started using start_node/3.

is_release_available(Release) ->
    group_leader() ! {sync_apply,
		      self(),
		      {test_server_ctrl,is_release_available,[Release]}},
    receive {sync_result,R} -> R end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run_on_shielded_node(Fun, CArgs) -> term()
%% Fun -> function()
%% CArg -> list()
%%
%%
%% Fun is executed in a process on a temporarily created
%% hidden node. Communication with the job process goes
%% via a job proxy process on the hidden node, i.e. the
%% group leader of the test case process is the job proxy
%% process. This makes it possible to start nodes from the
%% hidden node that are unaware of the test server node.
%% Without the job proxy process all processes would have
%% a process residing on the test_server node as group_leader.
%%
%% Fun    -  Function to execute
%% CArg   -  Extra command line arguments to use when starting
%%           the shielded node.
%%
%% If Fun is successfully executed, the result is returned.
%%

run_on_shielded_node(Fun, CArgs) when is_function(Fun), is_list(CArgs) ->
    {A,B,C} = now(),
    Name = "shielded_node-" ++ integer_to_list(A) ++ "-" ++ integer_to_list(B)
	++ "-" ++ integer_to_list(C),
    Node = case start_node(Name, slave, [{args, "-hidden " ++ CArgs}]) of
	       {ok, N} -> N;
	       Err -> fail({failed_to_start_shielded_node, Err})
	   end,
    Master = self(),
    Ref = make_ref(),
    Slave = spawn(Node,
		  fun () ->
			  start_job_proxy(),
			  receive
			      Ref ->
				  Master ! {Ref, Fun()}
			  end,
			  receive after infinity -> infinity end
		  end),
    MRef = erlang:monitor(process, Slave),
    Slave ! Ref,
    receive
	{'DOWN', MRef, _, _, Info} ->
	    stop_node(Node),
	    fail(Info);
	{Ref, Res} ->
	    stop_node(Node),
	    receive
		{'DOWN', MRef, _, _, _} ->
		    Res
	    end
    end.

%% Return true if Name or node() is a shielded node
is_shielded(Name) ->
    case {cast_to_list(Name),atom_to_list(node())} of
	{"shielded_node"++_,_} -> true;
	{_,"shielded_node"++_} -> true;
	_ -> false
    end.

same_version(Name) ->
    ThisVersion = erlang:system_info(version),
    OtherVersion = rpc:call(Name, erlang, system_info, [version]),
    ThisVersion =:= OtherVersion.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% temp_name(Stem) -> string()
%% Stem = string()
%%
%% Create a unique file name, based on (starting with) Stem.
%% A filename of the form <Stem><Number> is generated, and the
%% function checks that that file doesn't already exist.
temp_name(Stem) ->
    {A,B,C} = erlang:now(),
    RandomNum = A bxor B bxor C,
    RandomName = Stem ++ integer_to_list(RandomNum),
    {ok,Files} = file:list_dir(filename:dirname(Stem)),
    case lists:member(RandomName,Files) of
	true ->
	    %% oh, already exists - bad luck. Try again.
	    temp_name(Stem); %% recursively try again
	false ->
	    RandomName
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% app_test/1
%%
app_test(App) ->
    app_test(App, pedantic).
app_test(App, Mode) ->
    test_server_sup:app_test(App, Mode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_native(Mod) -> true | false
%%
%% Checks wether the module is natively compiled or not.

is_native(Mod) ->
    case catch Mod:module_info(native_addresses) of
	[_|_] -> true;
	_Other -> false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% comment(String) -> ok
%%
%% The given String will occur in the comment field
%% of the table on the test suite result page. If
%% called several times, only the last comment is
%% printed.
%% comment/1 is also overwritten by the return value
%% {comment,Comment} or fail/1 (which prints Reason
%% as a comment).
comment(String) ->
    group_leader() ! {comment,String},
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% os_type() -> OsType
%%
%% Returns the OsType of the target node. OsType is
%% the same as returned from os:type()
os_type() ->
    test_server_ctrl:get_target_os_type().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_cover() -> boolean()
%%
%% Returns true if cover is running, else false
is_cover() ->
    case whereis(cover_server) of
	undefined -> false;
	_ -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_debug() -> boolean()
%%
%% Returns true if the emulator is debug-compiled, false otherwise.
is_debug() ->
    case catch erlang:system_info(debug_compiled) of
	{'EXIT', _} ->
	    case string:str(erlang:system_info(system_version), "debug") of
		Int when is_integer(Int), Int > 0 -> true;
		_ -> false
	    end;
	Res ->
	    Res
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% has_lock_checking() -> boolean()
%%
%% Returns true if the emulator has lock checking enabled, false otherwise.
has_lock_checking() ->
    case catch erlang:system_info(lock_checking) of
	{'EXIT', _} -> false;
	Res -> Res
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% has_superfluous_schedulers() -> boolean()
%%
%% Returns true if the emulator has more scheduler threads than logical
%% processors, false otherwise.
has_superfluous_schedulers() ->
    case catch {erlang:system_info(schedulers),
		erlang:system_info(logical_processors)} of
	{S, P} when is_integer(S), is_integer(P), S > P -> true;
	_ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_commercial_build() -> boolean()
%%
%% Returns true if the current emulator is commercially supported.
%% (The emulator will not have "[source]" in its start-up message.)
%% We might want to do more tests on a commercial platform, for instance
%% ensuring that all applications have documentation).
is_commercial() ->
    case string:str(erlang:system_info(system_version), "source") of
	Int when is_integer(Int), Int > 0 -> false;
	_ -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     DEBUGGER INTERFACE                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% purify_is_running() -> false|true
%%
%% Tests if Purify is currently running.

purify_is_running() ->
    case catch erlang:system_info({error_checker, running}) of
	{'EXIT', _} -> false;
	Res -> Res
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% purify_new_leaks() -> false|BytesLeaked
%% BytesLeaked = integer()
%%
%% Checks for new memory leaks if Purify is active.
%% Returns the number of bytes leaked, or false if Purify
%% is not running.
purify_new_leaks() ->
    case catch erlang:system_info({error_checker, memory}) of
	{'EXIT', _} -> false;
	Leaked when is_integer(Leaked) -> Leaked
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% purify_new_fds_inuse() -> false|FdsInuse
%% FdsInuse = integer()
%%
%% Checks for new file descriptors in use.
%% Returns the number of new file descriptors in use, or false
%% if Purify is not running.
purify_new_fds_inuse() ->
    case catch erlang:system_info({error_checker, fd}) of
	{'EXIT', _} -> false;
	Inuse when is_integer(Inuse) -> Inuse
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% purify_format(Format, Args) -> ok
%% Format = string()
%% Args = lists()
%%
%% Outputs the formatted string to Purify's logfile,if Purify is active.
purify_format(Format, Args) ->
    (catch erlang:system_info({error_checker, io_lib:format(Format, Args)})),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Generic send functions for communication with host
%%
sync_local_or_remote_apply(Proxy,From,{M,F,A} = MFA) ->
    case get(test_server_job_sock) of
	undefined ->
	    %% i'm a local target
	    Result = apply(M,F,A),
	    if is_pid(Proxy) -> Proxy ! {sync_result_proxy,From,Result};
	       true -> From ! {sync_result,Result}
	    end;
	JobSock ->
	    %% i'm a remote target
	    request(JobSock,{sync_apply,MFA}),
	    {sync_result,Result} = recv(JobSock),
	    if is_pid(Proxy) -> Proxy ! {sync_result_proxy,From,Result};
	       true -> From ! {sync_result,Result}
	    end
    end.
local_or_remote_apply({M,F,A} = MFA) ->
    case get(test_server_job_sock) of
	undefined ->
	    %% i'm a local target
	    apply(M,F,A),
	    ok;
	JobSock ->
	    %% i'm a remote target
	    request(JobSock,{apply,MFA}),
	    ok
    end.

request(Sock,Request) ->
    gen_tcp:send(Sock,<<1,(term_to_binary(Request))/binary>>).

%%
%% Generic receive function for communication with host
%%
recv(Sock) ->
    case gen_tcp:recv(Sock,0) of
	{error,closed} ->
	    gen_tcp:close(Sock),
	    exit(connection_lost);
	{ok,<<1,Request/binary>>} ->
	    binary_to_term(Request);
	{ok,<<0,B/binary>>} ->
	    B
    end.
