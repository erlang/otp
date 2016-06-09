%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(test_server).

-define(DEFAULT_TIMETRAP_SECS, 60).

%%% TEST_SERVER_CTRL INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([run_test_case_apply/1,init_target_info/0]).
-export([cover_compile/1,cover_analyse/2]).

%%% TEST_SERVER_SUP INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([get_loc/1,set_tc_state/1]).

%%% TEST SUITE INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([lookup_config/2]).
-export([fail/0,fail/1,format/1,format/2,format/3]).
-export([capture_start/0,capture_stop/0,capture_get/0]).
-export([messages_get/0]).
-export([permit_io/2]).
-export([hours/1,minutes/1,seconds/1,sleep/1,adjusted_sleep/1,timecall/3]).
-export([timetrap_scale_factor/0,timetrap/1,get_timetrap_info/0,
	 timetrap_cancel/1,timetrap_cancel/0]).
-export([m_out_of_n/3,do_times/4,do_times/2]).
-export([call_crash/3,call_crash/4,call_crash/5]).
-export([temp_name/1]).
-export([start_node/3, stop_node/1, wait_for_node/1, is_release_available/1]).
-export([app_test/1, app_test/2, appup_test/1]).
-export([is_native/1]).
-export([comment/1, make_priv_dir/0]).
-export([os_type/0]).
-export([run_on_shielded_node/2]).
-export([is_cover/0,is_debug/0,is_commercial/0]).

-export([break/1,break/2,break/3,continue/0,continue/1]).

%%% PRIVATE EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("test_server_internal.hrl").
-include_lib("kernel/include/file.hrl").

init_target_info() ->
    [$.|Emu] = code:objfile_extension(),
    {_, OTPRel} = init:script_id(),
    #target_info{os_family=test_server_sup:get_os_family(),
		 os_type=os:type(),
		 version=erlang:system_info(version),
		 system_version=erlang:system_info(system_version),
		 root_dir=code:root_dir(),
		 emulator=Emu,
		 otp_release=OTPRel,
		 username=test_server_sup:get_username(),
		 cookie=atom_to_list(erlang:get_cookie())}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cover_compile(#cover{app=App,incl=Include,excl=Exclude,cross=Cross}) ->
%%        {ok,#cover{mods=AnalyseModules}} | {error,Reason}
%%
%% App = atom() , name of application to be compiled
%% Exclude = [atom()], list of modules to exclude
%% Include = [atom()], list of modules outside of App that should be included
%%                 in the cover compilation
%% Cross = [atoms()], list of modules outside of App shat should be included
%%                 in the cover compilation, but that shall not be part of
%%                 the cover analysis for this application.
%% AnalyseModules = [atom()], list of successfully compiled modules
%%
%% Cover compile the given application. Return {ok,CoverInfo} if
%% compilation succeeds, else (if application is not found and there
%% are no modules to compile) {error,application_not_found}.

cover_compile(CoverInfo=#cover{app=none,incl=Include,cross=Cross}) ->
    CrossMods = lists:flatmap(fun({_,M}) -> M end,Cross),
    CompileMods = Include++CrossMods,
    case length(CompileMods) of
	0 ->
	    io:fwrite("WARNING: No modules to cover compile!\n\n",[]),
	    {ok, _} = start_cover(),			% start cover server anyway
	    {ok,CoverInfo#cover{mods=[]}};
	N ->
	    io:fwrite("Cover compiling ~w modules - "
		      "this may take some time... ",[N]),
	    do_cover_compile(CompileMods),
	    io:fwrite("done\n\n",[]),
	    {ok,CoverInfo#cover{mods=Include}}
    end;
cover_compile(CoverInfo=#cover{app=App,excl=all,incl=Include,cross=Cross}) ->
    CrossMods = lists:flatmap(fun({_,M}) -> M end,Cross),
    CompileMods = Include++CrossMods,
    case length(CompileMods) of
	0 ->
	    io:fwrite("WARNING: No modules to cover compile!\n\n",[]),
	    {ok, _} = start_cover(),			% start cover server anyway
	    {ok,CoverInfo#cover{mods=[]}};
	N ->
	    io:fwrite("Cover compiling '~w' (~w files) - "
		      "this may take some time... ",[App,N]),
	    io:format("\nWARNING: All modules in \'~w\' are excluded\n"
		      "Only cover compiling modules in include list "
		      "and the modules\nin the cross cover file:\n"
		      "~tp\n", [App,CompileMods]),
	    do_cover_compile(CompileMods),
	    io:fwrite("done\n\n",[]),
	    {ok,CoverInfo#cover{mods=Include}}
    end;
cover_compile(CoverInfo=#cover{app=App,excl=Exclude,
			       incl=Include,cross=Cross}) ->
    CrossMods = lists:flatmap(fun({_,M}) -> M end,Cross),
    case code:lib_dir(App) of
	{error,bad_name} ->
	    case Include++CrossMods of
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
			      "~tp\n", [App,Include]),
		    do_cover_compile(CompileMods),
		    io:fwrite("done\n\n",[]),
		    {ok,CoverInfo#cover{mods=Include}}
	    end;
	LibDir ->
	    EbinDir = filename:join([LibDir,"ebin"]),
	    WC = filename:join(EbinDir,"*.beam"),
	    AllMods = module_names(filelib:wildcard(WC)),
	    AnalyseMods = (AllMods ++ Include) -- Exclude,
	    CompileMods = AnalyseMods ++ CrossMods,
	    case length(CompileMods) of
		0 ->
		    io:fwrite("WARNING: No modules to cover compile!\n\n",[]),
		    {ok, _} = start_cover(),		% start cover server anyway
		    {ok,CoverInfo#cover{mods=[]}};
		N ->
		    io:fwrite("Cover compiling '~w' (~w files) - "
			      "this may take some time... ",[App,N]),
		    do_cover_compile(CompileMods),
		    io:fwrite("done\n\n",[]),
		    {ok,CoverInfo#cover{mods=AnalyseMods}}
	    end
    end.


module_names(Beams) ->
    [list_to_atom(filename:basename(filename:rootname(Beam))) || Beam <- Beams].


do_cover_compile(Modules) ->
    {ok, _} = start_cover(),
    Sticky = prepare_cover_compile(Modules,[]),
    R = cover:compile_beam(Modules),
    _ = [warn_compile(Error) || Error <- R,element(1,Error)=/=ok],
    _ = [code:stick_mod(M) || M <- Sticky],
    ok.

warn_compile({error,{Reason,Module}}) ->
    io:fwrite("\nWARNING: Could not cover compile ~ts: ~p\n",
	      [Module,{error,Reason}]).

%% Make sure all modules are loaded and unstick if sticky
prepare_cover_compile([M|Ms],Sticky) ->
    case {code:is_sticky(M),code:is_loaded(M)} of
	{true,_} ->
	    code:unstick_mod(M),
	    prepare_cover_compile(Ms,[M|Sticky]);
	{false,false} ->
	    case code:load_file(M) of
		{module,_} ->
		    prepare_cover_compile([M|Ms],Sticky);
		Error ->
		    io:fwrite("\nWARNING: Could not load ~w: ~p\n",[M,Error]),
		    prepare_cover_compile(Ms,Sticky)
	    end;
	{false,_} ->
	    prepare_cover_compile(Ms,Sticky)
    end;
prepare_cover_compile([],Sticky) ->
    Sticky.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cover_analyse(Dir,#cover{level=Analyse,mods=Modules,stop=Stop) ->
%%            [{M,{Cov,NotCov,Details}}]
%%
%% Dir = string()
%% Analyse = details | overview
%% Modules = [atom()], the modules to analyse
%%
%% Cover analysis. If Analyse==details analyse_to_file is used.
%%
%% If Analyse==overview analyse_to_file is not used, only an overview
%% containing the number of covered/not covered lines in each module.
%%
%% Also, cover data will be exported to a file called all.coverdata in
%% the given directory.
%%
%% Finally, if Stop==true, then cover will be stopped after the
%% analysis is completed. Stopping cover causes the original (non
%% cover compiled) modules to be loaded back in. If a process at this
%% point is still running old code of any of the cover compiled
%% modules, meaning that is has not done any fully qualified function
%% call after the cover compilation, the process will now be
%% killed. To avoid this scenario, it is possible to set Stop=false,
%% which means that the modules will stay cover compiled. Note that
%% this is only recommended if the erlang node is being terminated
%% after the test is completed.
cover_analyse(Dir,#cover{level=Analyse,mods=Modules,stop=Stop}) ->
    io:fwrite(user, "Cover analysing... ", []),
    {ATFOk,ATFFail} =
	case Analyse of
	    details ->
		case cover:export(filename:join(Dir,"all.coverdata")) of
		    ok ->
			{result,Ok1,Fail1} =
			    cover:analyse_to_file(Modules,[{outdir,Dir},html]),
			{lists:map(fun(OutFile) ->
					   M = list_to_atom(
						 filename:basename(
						   filename:rootname(OutFile,
								     ".COVER.html")
						  )
						),
					   {M,{file,OutFile}}
				   end, Ok1),
			lists:map(fun({Reason,M}) ->
					  {M,{error,Reason}}
				  end, Fail1)};
		    Error ->
			{[],lists:map(fun(M) -> {M,Error} end, Modules)}
		end;
	    overview ->
		case cover:export(filename:join(Dir,"all.coverdata")) of
		    ok ->
			{[],lists:map(fun(M) -> {M,undefined} end, Modules)};
		    Error ->
			{[],lists:map(fun(M) -> {M,Error} end, Modules)}
		end
	end,
    {result,AOk,AFail} = cover:analyse(Modules,module),
    R0 = merge_analysis_results(AOk,ATFOk++ATFFail,[]) ++
	[{M,{error,Reason}} || {Reason,M} <- AFail],
    R = lists:sort(R0),
    io:fwrite(user, "done\n\n", []),

    case Stop of
	true ->
	    Sticky = unstick_all_sticky(node()),
	    cover:stop(),
	    stick_all_sticky(node(),Sticky);
	false ->
	    ok
    end,
    R.

merge_analysis_results([{M,{Cov,NotCov}}|T],ATF,Acc) ->
    case lists:keytake(M,1,ATF) of
	{value,{_,R},ATF1} ->
	    merge_analysis_results(T,ATF1,[{M,{Cov,NotCov,R}}|Acc]);
	false ->
	    merge_analysis_results(T,ATF,Acc)
    end;
merge_analysis_results([],_,Acc) ->
    Acc.

do_cover_for_node(Node,CoverFunc) ->
    do_cover_for_node(Node,CoverFunc,true).
do_cover_for_node(Node,CoverFunc,StickUnstick) ->
    %% In case a slave node is starting another slave node! I.e. this
    %% function is executed on a slave node - then the cover function
    %% must be executed on the master node. This is for instance the
    %% case in test_server's own tests.
    MainCoverNode = cover:get_main_node(),
    Sticky =
	if StickUnstick -> unstick_all_sticky(MainCoverNode,Node);
	   true -> ok
	end,
    rpc:call(MainCoverNode,cover,CoverFunc,[Node]),
    if StickUnstick -> stick_all_sticky(Node,Sticky);
       true -> ok
    end.

unstick_all_sticky(Node) ->
    unstick_all_sticky(node(),Node).
unstick_all_sticky(MainCoverNode,Node) ->
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
      rpc:call(MainCoverNode,cover,modules,[])).

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

run_test_case_apply({Mod,Func,Args,Name,RunInit,TimetrapData}) ->
    case os:getenv("TS_RUN_VALGRIND") of
	false ->
	    ok;
	_ ->
	    os:putenv("VALGRIND_LOGFILE_INFIX",atom_to_list(Mod)++"."++
		      atom_to_list(Func)++"-")
    end,
    ProcBef = erlang:system_info(process_count),
    Result = run_test_case_apply(Mod, Func, Args, Name, RunInit,
				 TimetrapData),
    ProcAft = erlang:system_info(process_count),
    DetFail = get(test_server_detected_fail),
    {Result,DetFail,ProcBef,ProcAft}.

-type tc_status() :: 'starting' | 'running' | 'init_per_testcase' |
		     'end_per_testcase' | {'framework',atom(),atom()} |
		     'tc'.
-record(st,
	{
	  ref :: reference(),
	  pid :: pid(),
	  mf :: {atom(),atom()},
	  last_known_loc :: term(),
	  status :: tc_status() | 'undefined',
	  ret_val :: term(),
	  comment :: list(char()),
	  timeout :: non_neg_integer() | 'infinity',
	  config :: list() | 'undefined',
	  end_conf_pid :: pid() | 'undefined'
	}).

run_test_case_apply(Mod, Func, Args, Name, RunInit, TimetrapData) ->
    print_timestamp(minor,"Started at "),
    print(minor, "", [], internal_raw),
    TCCallback = get(test_server_testcase_callback),
    LogOpts = get(test_server_logopts),
    Ref = make_ref(),
    Pid =
	spawn_link(
          run_test_case_eval_fun(Mod, Func, Args, Name, Ref,
                                 RunInit, TimetrapData,
                                 LogOpts, TCCallback)),
    put(test_server_detected_fail, []),
    St = #st{ref=Ref,pid=Pid,mf={Mod,Func},last_known_loc=unknown,
	     status=starting,ret_val=[],comment="",timeout=infinity,
	     config=hd(Args)},
    run_test_case_msgloop(St).

%% Ugly bug (pre R5A):
%% If this process (group leader of the test case) terminates before
%% all messages have been replied back to the io server, the io server
%% hangs. Fixed by the 20 milli timeout check here, and by using monitor in
%% io.erl.
%%
%% A test case is known to have failed if it returns {'EXIT', _} tuple,
%% or sends a message {failed, File, Line} to it's group_leader
%%
run_test_case_msgloop(#st{ref=Ref,pid=Pid,end_conf_pid=EndConfPid0}=St0) ->
    receive
	{set_tc_state=Tag,From,{Status,Config0}} ->
	    Config = case Config0 of
			 unknown -> St0#st.config;
			 _ -> Config0
		     end,
	    St = St0#st{status=Status,config=Config},
	    From ! {self(),Tag,ok},
	    run_test_case_msgloop(St);
	{abort_current_testcase,_,_}=Abort when St0#st.status =:= starting ->
	    %% we're in init phase, must must postpone this operation
	    %% until test case execution is in progress (or FW:init_tc
	    %% gets killed)
	    self() ! Abort,
	    erlang:yield(),
	    run_test_case_msgloop(St0);
	{abort_current_testcase,Reason,From} ->
	    Line = case is_process_alive(Pid) of
		       true -> get_loc(Pid);
		       false -> unknown
		   end,
	    Mon = erlang:monitor(process, Pid),
	    exit(Pid,{testcase_aborted,Reason,Line}),
	    erlang:yield(),
	    From ! {self(),abort_current_testcase,ok},
	    St = receive
		     {'DOWN', Mon, process, Pid, _} ->
			 St0
		 after 10000 ->
			 %% Pid is probably trapping exits, hit it harder...
			 exit(Pid, kill),
			 %% here's the only place we know Reason, so we save
			 %% it as a comment, potentially replacing user data
			 Error = lists:flatten(io_lib:format("Aborted: ~p",
							     [Reason])),
			 Error1 = lists:flatten([string:strip(S,left) ||
						    S <- string:tokens(Error,
								       [$\n])]),
			 Comment = if length(Error1) > 63 ->
					   string:substr(Error1,1,60) ++ "...";
				      true ->
					   Error1
				   end,
			 St0#st{comment=Comment}
		 end,
	    run_test_case_msgloop(St);
	{sync_apply,From,MFA} ->
	    do_sync_apply(false,From,MFA),
	    run_test_case_msgloop(St0);
	{sync_apply_proxy,Proxy,From,MFA} ->
	    do_sync_apply(Proxy,From,MFA),
	    run_test_case_msgloop(St0);
	{comment,NewComment0} ->
	    NewComment1 = test_server_ctrl:to_string(NewComment0),
	    NewComment = test_server_sup:framework_call(format_comment,
							[NewComment1],
							NewComment1),
	    run_test_case_msgloop(St0#st{comment=NewComment});
	{read_comment,From} ->
	    From ! {self(),read_comment,St0#st.comment},
	    run_test_case_msgloop(St0);
	{make_priv_dir,From} ->
	    Config = case St0#st.config of
			 undefined -> [];
			 Config0 -> Config0
		     end,
	    Result =
		case proplists:get_value(priv_dir, Config) of
		    undefined ->
			{error,no_priv_dir_in_config};
		    PrivDir ->
			case file:make_dir(PrivDir) of
			    ok ->
				ok;
			    {error, eexist} ->
				ok;
			    MkDirError ->
				{error,{MkDirError,PrivDir}}
			end
		end,
	    From ! {self(),make_priv_dir,Result},
	    run_test_case_msgloop(St0);
	{'EXIT',Pid,{Ref,Time,Value,Loc,Opts}} ->
	    RetVal = {Time/1000000,Value,Loc,Opts},
	    St = setup_termination(RetVal, St0#st{config=undefined}),
	    run_test_case_msgloop(St);
	{'EXIT',Pid,Reason} ->
	    %% This exit typically happens when an unknown external process
	    %% has caused a test case process to terminate (e.g. if a linked
	    %% process has crashed).
	    St =
		case Reason of
		    {What,[Loc0={_M,_F,A,[{file,_}|_]}|_]} when 
			  is_integer(A) ->
			Loc = rewrite_loc_item(Loc0),
			handle_tc_exit(What, St0#st{last_known_loc=[Loc]});
		    {What,[Details,Loc0={_M,_F,A,[{file,_}|_]}|_]} when
			  is_integer(A) ->
			Loc = rewrite_loc_item(Loc0),
			handle_tc_exit({What,Details}, St0#st{last_known_loc=[Loc]});
		    _ ->
			handle_tc_exit(Reason, St0)
		end,
	    run_test_case_msgloop(St);
	{EndConfPid0,{call_end_conf,Data,_Result}} ->
	    #st{mf={Mod,Func},config=CurrConf} = St0,
	    case CurrConf of
		_ when is_list(CurrConf) ->
		    {_Mod,_Func,TCPid,TCExitReason,Loc} = Data,
		    spawn_fw_call(Mod,Func,CurrConf,TCPid,
				  TCExitReason,Loc,self()),
		    St = St0#st{config=undefined,end_conf_pid=undefined},
		    run_test_case_msgloop(St);
		_ ->
		    run_test_case_msgloop(St0)
	    end;
	{_FwCallPid,fw_notify_done,{T,Value,Loc,Opts,AddToComment}} ->
	    %% the framework has been notified, we're finished
	    RetVal = {T,Value,Loc,Opts},
	    Comment0 = St0#st.comment,
	    Comment = case AddToComment of
			  undefined ->
			      Comment0;
			  _ ->
			      if Comment0 =:= "" ->
				      AddToComment;
				 true -> 
				      Comment0 ++
					  test_server_ctrl:xhtml("<br>",
								 "<br />") ++
					  AddToComment
			      end
		      end,
	    St = setup_termination(RetVal, St0#st{comment=Comment,
						  config=undefined}),
	    run_test_case_msgloop(St);
 	{'EXIT',_FwCallPid,{fw_notify_done,Func,Error}} ->
	    %% a framework function failed
	    CB = os:getenv("TEST_SERVER_FRAMEWORK"),
	    Loc = case CB of
		      FW when FW =:= false; FW =:= "undefined" ->
			  [{test_server,Func}];
		      _ ->
			  [{list_to_atom(CB),Func}]
		  end,
	    RetVal = {died,{framework_error,Loc,Error},Loc},
	    St = setup_termination(RetVal, St0#st{comment="Framework error",
						 config=undefined}),
	    run_test_case_msgloop(St);
	{failed,File,Line} ->
	    put(test_server_detected_fail,
		[{File, Line}| get(test_server_detected_fail)]),
	    run_test_case_msgloop(St0);

	{user_timetrap,Pid,_TrapTime,StartTime,E={user_timetrap_error,_},_} ->
	    case update_user_timetraps(Pid, StartTime) of
		proceed ->
		    self() ! {abort_current_testcase,E,Pid},
		    ok;
		ignore ->
		    ok
	    end,
	    run_test_case_msgloop(St0);
	{user_timetrap,Pid,TrapTime,StartTime,ElapsedTime,Scale} ->
	    %% a user timetrap is triggered, ignore it if new
	    %% timetrap has been started since
	    case update_user_timetraps(Pid, StartTime) of
		proceed ->
		    TotalTime = if is_integer(TrapTime) -> 
					TrapTime + ElapsedTime;
				   true -> 
					TrapTime
				end,
		    _ = timetrap(TrapTime, TotalTime, Pid, Scale),
		    ok;
		ignore ->
		    ok
	    end,
	    run_test_case_msgloop(St0);
	{timetrap_cancel_one,Handle,_From} ->
	    timetrap_cancel_one(Handle, false),
	    run_test_case_msgloop(St0);
	{timetrap_cancel_all,TCPid,_From} ->
	    timetrap_cancel_all(TCPid, false),
	    run_test_case_msgloop(St0);
	{get_timetrap_info,From,TCPid} ->
	    Info = get_timetrap_info(TCPid, false),
	    From ! {self(),get_timetrap_info,Info},
	    run_test_case_msgloop(St0);
	_Other when not is_tuple(_Other) ->
	    %% ignore anything not generated by test server
	    run_test_case_msgloop(St0);
	_Other when element(1, _Other) /= 'EXIT',
		    element(1, _Other) /= started,
		    element(1, _Other) /= finished,
		    element(1, _Other) /= print ->
	    %% ignore anything not generated by test server
	    run_test_case_msgloop(St0)
    after St0#st.timeout ->
	    #st{ret_val=RetVal,comment=Comment} = St0,
	    erlang:append_element(RetVal, Comment)
    end.

setup_termination(RetVal, #st{pid=Pid}=St) ->
    timetrap_cancel_all(Pid, false),
    St#st{ret_val=RetVal,timeout=20}.

set_tc_state(State) ->
    set_tc_state(State,unknown).
set_tc_state(State, Config) ->
    tc_supervisor_req(set_tc_state, {State,Config}).

handle_tc_exit(killed, St) ->
    %% probably the result of an exit(TestCase,kill) call, which is the
    %% only way to abort a testcase process that traps exits
    %% (see abort_current_testcase).
    #st{config=Config,mf={Mod,Func},pid=Pid} = St,
    Msg = testcase_aborted_or_killed,
    spawn_fw_call(Mod, Func, Config, Pid, Msg, unknown, self()),
    St;
handle_tc_exit({testcase_aborted,{user_timetrap_error,_}=Msg,_}, St) ->
    #st{config=Config,mf={Mod,Func},pid=Pid} = St,
    spawn_fw_call(Mod, Func, Config, Pid, Msg, unknown, self()),
    St;
handle_tc_exit(Reason, #st{status={framework,FwMod,FwFunc},
			   config=Config,pid=Pid}=St) ->
    R = case Reason of
	    {timetrap_timeout,TVal,_} ->
		{timetrap,TVal};
	    {testcase_aborted=E,AbortReason,_} ->
		{E,AbortReason};
	    {fw_error,{FwMod,FwFunc,FwError}} ->
		FwError;
	    Other ->
		Other
	end,
    Error = {framework_error,R},
    spawn_fw_call(FwMod, FwFunc, Config, Pid, Error, unknown, self()),
    St;
handle_tc_exit(Reason, #st{status=tc,config=Config0,mf={Mod,Func},pid=Pid}=St)
  when is_list(Config0) ->
    {R,Loc1,F} = case Reason of
		     {timetrap_timeout=E,TVal,Loc0} ->
			 {{E,TVal},Loc0,E};
		     {testcase_aborted=E,AbortReason,Loc0} ->
			 Msg = {E,AbortReason},
			 {Msg,Loc0,Msg};
		     Other ->
			 {{'EXIT',Other},unknown,Other}
		 end,
    Timeout = end_conf_timeout(Reason, St),
    Config = [{tc_status,{failed,F}}|Config0],
    EndConfPid = call_end_conf(Mod, Func, Pid, R, Loc1, Config, Timeout),
    St#st{end_conf_pid=EndConfPid};
handle_tc_exit(Reason, #st{config=Config,mf={Mod,Func0},pid=Pid,
			   status=Status}=St) ->
    {R,Loc1} = case Reason of
		   {timetrap_timeout=E,TVal,Loc0} ->
		       {{E,TVal},Loc0};
		   {testcase_aborted=E,AbortReason,Loc0} ->
		       {{E,AbortReason},Loc0};
		   Other ->
		       {{'EXIT',Other},St#st.last_known_loc}
	       end,
    Func = case Status of
	       init_per_testcase=F -> {F,Func0};
	       end_per_testcase=F -> {F,Func0};
	       _ -> Func0
	   end,
    spawn_fw_call(Mod, Func, Config, Pid, R, Loc1, self()),
    St.

end_conf_timeout({timetrap_timeout,Timeout,_}, _) ->
    Timeout;
end_conf_timeout(_, #st{config=Config}) when is_list(Config) ->
    proplists:get_value(default_timeout, Config, ?DEFAULT_TIMETRAP_SECS*1000);
end_conf_timeout(_, _) ->
    ?DEFAULT_TIMETRAP_SECS*1000.

call_end_conf(Mod,Func,TCPid,TCExitReason,Loc,Conf,TVal) ->
    Starter = self(),
    Data = {Mod,Func,TCPid,TCExitReason,Loc},
    case erlang:function_exported(Mod,end_per_testcase,2) of
	false ->
	    spawn_link(fun() ->
			       Starter ! {self(),{call_end_conf,Data,ok}}
		       end);
	true ->
	    do_call_end_conf(Starter,Mod,Func,Data,TCExitReason,Conf,TVal)
    end.

do_call_end_conf(Starter,Mod,Func,Data,TCExitReason,Conf,TVal) ->
    EndConfProc =
	fun() ->
		process_flag(trap_exit,true), % to catch timetraps
		Supervisor = self(),
		EndConfApply =
		    fun() ->
			    _ = timetrap(TVal),
			    %% We can't handle fails or skips here
			    %% (neither input nor output). The error can
			    %% be read from Conf though (tc_status).
			    EndConf =
				case do_init_tc_call(Mod,{end_per_testcase,Func},
						     [Conf],
						     {TCExitReason,[Conf]}) of
				    {_,[EPTCInit]} when is_list(EPTCInit) ->
					EPTCInit;
				    _ ->
					Conf
				end,
			    try apply(Mod,end_per_testcase,[Func,EndConf]) of
				_ -> ok
			    catch
				_:Error ->
				    timer:sleep(1),
				    print_end_conf_result(Mod,Func,Conf,
							  "crashed",Error)
			    end,
			    Supervisor ! {self(),end_conf}
		    end,
		Pid = spawn_link(EndConfApply),
		receive
		    {Pid,end_conf} ->
			Starter ! {self(),{call_end_conf,Data,ok}};
		    {'EXIT',Pid,Reason} ->
			print_end_conf_result(Mod,Func,Conf,"failed",Reason),
			Starter ! {self(),{call_end_conf,Data,{error,Reason}}};
		    {'EXIT',_OtherPid,Reason} ->
			%% Probably the parent - not much to do about that
			exit(Reason)
		end
	end,
    spawn_link(EndConfProc).

print_end_conf_result(Mod,Func,Conf,Cause,Error) ->
    Str2Print =
	fun(NoHTML) when NoHTML == stdout; NoHTML == major ->
		io_lib:format("WARNING! "
			      "~w:end_per_testcase(~w, ~tp)"
			      " ~s!\n\tReason: ~tp\n",
			      [Mod,Func,Conf,Cause,Error]);		    
	   (minor) ->
		ErrorStr = test_server_ctrl:escape_chars(Error),
		io_lib:format("WARNING! "
			      "~w:end_per_testcase(~w, ~tp)"
			      " ~s!\n\tReason: ~ts\n",
			      [Mod,Func,Conf,Cause,ErrorStr])
	end,
    group_leader() ! {printout,12,Str2Print},
    ok.


spawn_fw_call(Mod,IPTC={init_per_testcase,Func},CurrConf,Pid,
	      Why,Loc,SendTo) ->
    FwCall =
	fun() ->
		Skip = {skip,{failed,{Mod,init_per_testcase,Why}}},
		%% if init_per_testcase fails, the test case
		%% should be skipped
		try begin do_end_tc_call(Mod,IPTC, {Pid,Skip,[CurrConf]}, Why),
			  do_init_tc_call(Mod,{end_per_testcase,Func},
					  [CurrConf],{ok,[CurrConf]}),
			  do_end_tc_call(Mod,{end_per_testcase,Func}, 
					 {Pid,Skip,[CurrConf]}, Why) end of
		    _ -> ok
		catch
		    _:FwEndTCErr ->
			exit({fw_notify_done,end_tc,FwEndTCErr})
		end,
		Time = case Why of
			   {timetrap_timeout,TVal} -> TVal/1000;
			   _                       -> died
		       end,
		group_leader() ! {printout,12,
				  "ERROR! ~w:init_per_testcase(~w, ~p)"
				  " failed!\n\tReason: ~tp\n",
				 [Mod,Func,CurrConf,Why]},
		%% finished, report back
		SendTo ! {self(),fw_notify_done,{Time,Skip,Loc,[],undefined}}
	end,
    spawn_link(FwCall);

spawn_fw_call(Mod,EPTC={end_per_testcase,Func},EndConf,Pid,
	      Why,_Loc,SendTo) ->
    FwCall =
	fun() ->
		{RetVal,Report} =
		    case proplists:get_value(tc_status, EndConf) of
			undefined ->
			    E = {failed,{Mod,end_per_testcase,Why}},
			    {E,E};
			E = {failed,Reason} ->
			    {E,{error,Reason}};
			Result ->
			    E = {failed,{Mod,end_per_testcase,Why}},
			    {Result,E}
		    end,
		{Time,Warn} =
		    case Why of
			{timetrap_timeout,TVal} ->
			    group_leader() !
				{printout,12,
				 "WARNING! ~w:end_per_testcase(~w, ~p)"
				 " failed!\n\tReason: timetrap timeout"
				 " after ~w ms!\n", [Mod,Func,EndConf,TVal]},
			    W = "<font color=\"red\">"
				"WARNING: end_per_testcase timed out!</font>",
			    {TVal/1000,W};
			_ ->
			    group_leader() !
				{printout,12,
				 "WARNING! ~w:end_per_testcase(~w, ~p)"
				 " failed!\n\tReason: ~tp\n",
				 [Mod,Func,EndConf,Why]},
			    W = "<font color=\"red\">"
				"WARNING: end_per_testcase failed!</font>",
			    {died,W}
		    end,
		try do_end_tc_call(Mod,EPTC,{Pid,Report,[EndConf]}, Why) of
		    _ -> ok
		catch
		    _:FwEndTCErr ->
			exit({fw_notify_done,end_tc,FwEndTCErr})
		end,
		FailLoc = proplists:get_value(tc_fail_loc, EndConf),
		%% finished, report back (if end_per_testcase fails, a warning
		%% should be printed as part of the comment)
		SendTo ! {self(),fw_notify_done,
			  {Time,RetVal,FailLoc,[],Warn}}
	end,
    spawn_link(FwCall);

spawn_fw_call(FwMod,FwFunc,_,_Pid,{framework_error,FwError},_,SendTo) ->
    FwCall =
	fun() ->
		test_server_sup:framework_call(report, [framework_error,
							{{FwMod,FwFunc},
							 FwError}]),
		Comment =
		    lists:flatten(
		      io_lib:format("<font color=\"red\">"
				    "WARNING! ~w:~w failed!</font>",
				    [FwMod,FwFunc])),
	    %% finished, report back
	    SendTo ! {self(),fw_notify_done,
		      {died,{error,{FwMod,FwFunc,FwError}},
		       {FwMod,FwFunc},[],Comment}}
	end,
    spawn_link(FwCall);

spawn_fw_call(Mod,Func,CurrConf,Pid,Error,Loc,SendTo) ->
    {Func1,EndTCFunc} = case Func of
			    CF when CF == init_per_suite; CF == end_per_suite;
				    CF == init_per_group; CF == end_per_group ->
				{CF,CF};
			    TC -> {TC,{end_per_testcase,TC}}
			end,
    FwCall =
	fun() ->
		try fw_error_notify(Mod,Func1,[],
				    Error,Loc) of
		    _ -> ok
		catch
		    _:FwErrorNotifyErr ->
			exit({fw_notify_done,error_notification,
			      FwErrorNotifyErr})
		end,
		Conf = [{tc_status,{failed,Error}}|CurrConf],
		try do_end_tc_call(Mod,EndTCFunc,{Pid,Error,[Conf]},Error) of
		    _ -> ok
		catch
		    _:FwEndTCErr ->
			exit({fw_notify_done,end_tc,FwEndTCErr})
		end,
		%% finished, report back
		SendTo ! {self(),fw_notify_done,{died,Error,Loc,[],undefined}}
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

-spec run_test_case_eval_fun(_, _, _, _, _, _, _, _, _) ->
                                    fun(() -> no_return()).
run_test_case_eval_fun(Mod, Func, Args, Name, Ref, RunInit,
                       TimetrapData, LogOpts, TCCallback) ->
    fun () ->
            run_test_case_eval(Mod, Func, Args, Name, Ref,
                               RunInit, TimetrapData,
                               LogOpts, TCCallback)
    end.

%% A test case is known to have failed if it returns {'EXIT', _} tuple,
%% or sends a message {failed, File, Line} to it's group_leader

run_test_case_eval(Mod, Func, Args0, Name, Ref, RunInit,
		   TimetrapData, LogOpts, TCCallback) ->
    put(test_server_multiply_timetraps, TimetrapData),
    put(test_server_logopts, LogOpts),
    Where = [{Mod,Func}],
    put(test_server_loc, Where),

    FWInitFunc = case RunInit of
		     run_init -> {init_per_testcase,Func};
		     _        -> Func
		 end,

    FWInitResult0 = do_init_tc_call(Mod,FWInitFunc,Args0,{ok,Args0}),

    set_tc_state(running),
    {{Time,Value},Loc,Opts} =
	case FWInitResult0 of
	    {ok,Args} ->
		run_test_case_eval1(Mod, Func, Args, Name, RunInit, TCCallback);
	    Error = {error,_Reason} ->
		NewResult = do_end_tc_call(Mod,FWInitFunc, {Error,Args0},
					   {auto_skip,{failed,Error}}),
		{{0,NewResult},Where,[]};
	    {fail,Reason} ->
		Conf = [{tc_status,{failed,Reason}} | hd(Args0)],		
		fw_error_notify(Mod, Func, Conf, Reason),
		NewResult = do_end_tc_call(Mod,FWInitFunc,
					   {{error,Reason},[Conf]},
					   {fail,Reason}),
		{{0,NewResult},Where,[]};
	    Skip = {SkipType,_Reason} when SkipType == skip;
					   SkipType == skipped ->
		NewResult = do_end_tc_call(Mod,FWInitFunc,
					   {Skip,Args0}, Skip),
		{{0,NewResult},Where,[]};
	    AutoSkip = {auto_skip,_Reason} ->
		%% special case where a conf case "pretends" to be skipped
		NewResult =
		    do_end_tc_call(Mod,FWInitFunc, {AutoSkip,Args0}, AutoSkip),
		{{0,NewResult},Where,[]}
	end,
    exit({Ref,Time,Value,Loc,Opts}).

run_test_case_eval1(Mod, Func, Args, Name, RunInit, TCCallback) ->
    case RunInit of
	run_init ->
	    set_tc_state(init_per_testcase, hd(Args)),
	    ensure_timetrap(Args),
	    case init_per_testcase(Mod, Func, Args) of
		Skip = {SkipType,Reason} when SkipType == skip;
					      SkipType == skipped ->
		    Line = get_loc(),
		    Conf = [{tc_status,{skipped,Reason}}|hd(Args)],
		    NewRes = do_end_tc_call(Mod,{init_per_testcase,Func},
					    {Skip,[Conf]}, Skip),
		    {{0,NewRes},Line,[]};
		{skip_and_save,Reason,SaveCfg} ->
		    Line = get_loc(),
		    Conf = [{tc_status,{skipped,Reason}},
			    {save_config,SaveCfg}|hd(Args)],
		    NewRes = do_end_tc_call(Mod,{init_per_testcase,Func},
					    {{skip,Reason},[Conf]},
					    {skip,Reason}),
		    {{0,NewRes},Line,[]};
		FailTC = {fail,Reason} ->       % user fails the testcase
		    EndConf = [{tc_status,{failed,Reason}} | hd(Args)],
		    fw_error_notify(Mod, Func, EndConf, Reason),
		    NewRes = do_end_tc_call(Mod,{init_per_testcase,Func},
					    {{error,Reason},[EndConf]},
					    FailTC),
		    {{0,NewRes},[{Mod,Func}],[]};
		{ok,NewConf} ->
		    IPTCEndRes = do_end_tc_call(Mod,{init_per_testcase,Func},
						{ok,[NewConf]}, NewConf),
		    {{T,Return},Loc,NewConf1} =
			if not is_list(IPTCEndRes) ->
				%% received skip or fail, not config
				{{0,IPTCEndRes},undefined,NewConf};
			   true ->
				%% call user callback function if defined
				NewConfUC =
				    user_callback(TCCallback, Mod, Func,
						  init, IPTCEndRes),
				%% save current state in controller loop
				set_tc_state(tc, NewConfUC),
				%% execute the test case
				{ts_tc(Mod,Func,[NewConfUC]),get_loc(),NewConfUC}
			end,
		    {EndConf,TSReturn,FWReturn} =
			case Return of
			    {E,TCError} when E=='EXIT' ; E==failed ->
				fw_error_notify(Mod, Func, NewConf1,
						TCError, Loc),
				{[{tc_status,{failed,TCError}},
				  {tc_fail_loc,Loc}|NewConf1],
				 Return,{error,TCError}};
			    SaveCfg={save_config,_} ->
				{[{tc_status,ok},SaveCfg|NewConf1],Return,ok};
			    {skip_and_save,Why,SaveCfg} ->
				Skip = {skip,Why},
				{[{tc_status,{skipped,Why}},
				  {save_config,SaveCfg}|NewConf1],
				 Skip,Skip};
			    {SkipType,Why} when SkipType == skip;
						SkipType == skipped ->
				{[{tc_status,{skipped,Why}}|NewConf1],Return,
				 Return};
			    _ ->
				{[{tc_status,ok}|NewConf1],Return,ok}
			end,
		    %% call user callback function if defined
		    EndConf1 =
			user_callback(TCCallback, Mod, Func, 'end', EndConf),

		    %% We can't handle fails or skips here
		    EndConf2 =
			case do_init_tc_call(Mod,{end_per_testcase,Func},
					     [EndConf1],{ok,[EndConf1]}) of
			    {ok,[EPTCInitRes]} when is_list(EPTCInitRes) ->
				EPTCInitRes;
			    _ ->
				EndConf1
			end,

		    %% update current state in controller loop
		    {FWReturn1,TSReturn1,EndConf3} =
			case end_per_testcase(Mod, Func, EndConf2) of
			    SaveCfg1={save_config,_} ->
				{FWReturn,TSReturn,
				 [SaveCfg1|lists:keydelete(save_config,1,
							   EndConf2)]};
			    {fail,ReasonToFail} ->
				%% user has failed the testcase
				fw_error_notify(Mod, Func, EndConf2,
						ReasonToFail),
				{{error,ReasonToFail},
				 {failed,ReasonToFail},
				 EndConf2};
			    {failed,{_,end_per_testcase,_}} = Failure when
				  FWReturn == ok ->
				%% unexpected termination in end_per_testcase
				%% report this as the result to the framework
				{Failure,TSReturn,EndConf2};
			    _ ->
				%% test case result should be reported to
				%% framework no matter the status of
				%% end_per_testcase
				{FWReturn,TSReturn,EndConf2}
			end,
		    %% clear current state in controller loop
		    case do_end_tc_call(Mod,{end_per_testcase,Func},
					{FWReturn1,[EndConf3]}, TSReturn1) of
			{failed,Reason} = NewReturn ->
			    fw_error_notify(Mod,Func,EndConf3, Reason),
			    {{T,NewReturn},[{Mod,Func}],[]};
			NewReturn ->
			    {{T,NewReturn},Loc,[]}
		    end
	    end;
	skip_init ->
	    set_tc_state(running, hd(Args)),
	    %% call user callback function if defined
	    Args1 = user_callback(TCCallback, Mod, Func, init, Args),
	    ensure_timetrap(Args1),
	    %% ts_tc does a catch
	    %% if this is a named conf group, the test case (init or end conf)
	    %% should be called with the name as the first argument
	    Args2 = if Name == undefined -> Args1;
		       true -> [Name | Args1]
		    end,
	    %% execute the conf test case
	    {{T,Return},Loc} = {ts_tc(Mod, Func, Args2),get_loc()},
	    %% call user callback function if defined
	    Return1 = user_callback(TCCallback, Mod, Func, 'end', Return),
	    {Return2,Opts} = process_return_val([Return1], Mod, Func,
						Args1, [{Mod,Func}], Return1),
	    {{T,Return2},Loc,Opts}
    end.

do_init_tc_call(Mod, Func, Res, Return) ->
    test_server_sup:framework_call(init_tc,[Mod,Func,Res],Return).

do_end_tc_call(Mod, IPTC={init_per_testcase,Func}, Res, Return) ->
     case Return of
	 {NOk,_} when NOk == auto_skip; NOk == fail;
		      NOk == skip ; NOk == skipped ->
	     {_,Args} = Res,
	     IPTCEndRes =
		 case do_end_tc_call1(Mod, IPTC, Res, Return) of
		     IPTCEndConfig when is_list(IPTCEndConfig) ->
			 IPTCEndConfig;
		     _ ->
			 Args
		 end,
	     EPTCInitRes =
		 case do_init_tc_call(Mod,{end_per_testcase,Func},
				      IPTCEndRes,Return) of
		     {ok,EPTCInitConfig} when is_list(EPTCInitConfig) ->
			 {Return,EPTCInitConfig};
		     _ ->
			 Return
		 end,
	     do_end_tc_call1(Mod, {end_per_testcase,Func},
			     EPTCInitRes, Return);
	 _Ok ->
	     do_end_tc_call1(Mod, IPTC, Res, Return)
     end;
do_end_tc_call(Mod, Func, Res, Return) ->
    do_end_tc_call1(Mod, Func, Res, Return).

do_end_tc_call1(Mod, Func, Res, Return) ->
    FwMod = os:getenv("TEST_SERVER_FRAMEWORK"),
    Ref = make_ref(),
    if FwMod == "ct_framework" ; FwMod == "undefined"; FwMod == false ->
	    case test_server_sup:framework_call(
		   end_tc, [Mod,Func,Res, Return], ok) of
		{fail,FWReason} ->
		    {failed,FWReason};
		ok ->
		    case Return of
			{fail,Reason} ->
			    {failed,Reason};
			Return ->
			    Return
		    end;
		NewReturn ->
		    NewReturn
	    end;
       true ->
	    case test_server_sup:framework_call(FwMod, end_tc,
						[Mod,Func,Res], Ref) of
		{fail,FWReason} ->
		    {failed,FWReason};
		_Else ->
		    Return
	    end
    end.

%% the return value is a list and we have to check if it contains
%% the result of an end conf case or if it's a Config list
process_return_val([Return], M,F,A, Loc, Final) when is_list(Return) ->
    ReturnTags = [skip,skip_and_save,save_config,comment,return_group_result],
    %% check if all elements in the list are valid end conf return value tuples
    case lists:all(fun(Val) when is_tuple(Val) ->
			   lists:any(fun(T) -> T == element(1, Val) end,
				     ReturnTags);
		      (ok) ->
			   true;
		      (_) ->
			   false
		   end, Return) of
	true ->		     % must be return value from end conf case
	    process_return_val1(Return, M,F,A, Loc, Final, []);
	false -> % must be Config value from init conf case
	    case do_end_tc_call(M, F, {ok,A}, Return) of
		{failed, FWReason} = Failed ->
		    fw_error_notify(M,F,A, FWReason),
		    {Failed, []};
		NewReturn ->
		    {NewReturn, []}
	    end
    end;
%% the return value is not a list, so it's the return value from an
%% end conf case or it's a dummy value that can be ignored
process_return_val(Return, M,F,A, Loc, Final) ->
    process_return_val1(Return, M,F,A, Loc, Final, []).

process_return_val1([Failed={E,TCError}|_], M,F,A=[Args], Loc, _, SaveOpts)
  when E=='EXIT';
       E==failed ->
    fw_error_notify(M,F,A, TCError, Loc),
    case do_end_tc_call(M,F, {{error,TCError},
			      [[{tc_status,{failed,TCError}}|Args]]},
			Failed) of
	{failed,FWReason} ->
	    {{failed,FWReason},SaveOpts};
	NewReturn ->
	    {NewReturn,SaveOpts}
    end;
process_return_val1([SaveCfg={save_config,_}|Opts], M,F,[Args],
		    Loc, Final, SaveOpts) ->
    process_return_val1(Opts, M,F,[[SaveCfg|Args]], Loc, Final, SaveOpts);
process_return_val1([{skip_and_save,Why,SaveCfg}|Opts], M,F,[Args],
		    Loc, _, SaveOpts) ->
    process_return_val1(Opts, M,F,[[{save_config,SaveCfg}|Args]],
			Loc, {skip,Why}, SaveOpts);
process_return_val1([GR={return_group_result,_}|Opts], M,F,A,
		    Loc, Final, SaveOpts) ->
    process_return_val1(Opts, M,F,A, Loc, Final, [GR|SaveOpts]);
process_return_val1([RetVal={Tag,_}|Opts], M,F,A,
		    Loc, _, SaveOpts) when Tag==skip;
					   Tag==comment ->
    process_return_val1(Opts, M,F,A, Loc, RetVal, SaveOpts);
process_return_val1([_|Opts], M,F,A, Loc, Final, SaveOpts) ->
    process_return_val1(Opts, M,F,A, Loc, Final, SaveOpts);
process_return_val1([], M,F,A, _Loc, Final, SaveOpts) ->
    case do_end_tc_call(M,F, {Final,A}, Final) of
	{failed,FWReason} ->
	    {{failed,FWReason},SaveOpts};
	NewReturn ->
	    {NewReturn,lists:reverse(SaveOpts)}
    end.

user_callback(undefined, _, _, _, Args) ->
    Args;
user_callback({CBMod,CBFunc}, Mod, Func, InitOrEnd,
	      [Args]) when is_list(Args) ->
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
	false ->
	    _ = code:load_file(Mod),
	    ok;
	_ -> ok
    end,
    case erlang:function_exported(Mod, init_per_testcase, 2) of
	true ->
	    do_init_per_testcase(Mod, [Func|Args]);
	false ->
	    %% Optional init_per_testcase is not defined -- keep quiet.
	    [Config] = Args,
	    {ok, Config}
    end.

do_init_per_testcase(Mod, Args) ->
    try	apply(Mod, init_per_testcase, Args) of
	{Skip,Reason} when Skip =:= skip; Skip =:= skipped ->
	    {skip,Reason};
	{skip_and_save,_,_}=Res ->
	    Res;
	NewConf when is_list(NewConf) ->
	    case lists:filter(fun(T) when is_tuple(T) -> false;
				 (_) -> true end, NewConf) of
		[] ->
		    {ok,NewConf};
		Bad ->
		    group_leader() ! {printout,12,
				      "ERROR! init_per_testcase has returned "
				      "bad elements in Config: ~tp\n",[Bad]},
		    {skip,{failed,{Mod,init_per_testcase,bad_return}}}
	    end;
	{fail,_Reason}=Res ->
	    Res;
	_Other ->
	    group_leader() ! {printout,12,
			      "ERROR! init_per_testcase did not return "
			      "a Config list.\n",[]},
	    {skip,{failed,{Mod,init_per_testcase,bad_return}}}
    catch
	throw:{Skip,Reason} when Skip =:= skip; Skip =:= skipped ->
	    {skip,Reason};
	exit:{Skip,Reason} when Skip =:= skip; Skip =:= skipped ->
	    {skip,Reason};
	throw:Other ->
	    set_loc(erlang:get_stacktrace()),
	    Line = get_loc(),
	    print_init_conf_result(Line,"thrown",Other),
	    {skip,{failed,{Mod,init_per_testcase,Other}}};
	_:Reason0 ->
	    Stk = erlang:get_stacktrace(),
	    Reason = {Reason0,Stk},
	    set_loc(Stk),
	    Line = get_loc(),
	    print_init_conf_result(Line,"crashed",Reason),
	    {skip,{failed,{Mod,init_per_testcase,Reason}}}
    end.

print_init_conf_result(Line,Cause,Reason) ->
    FormattedLoc = test_server_sup:format_loc(Line),
    Str2Print =
	fun(NoHTML) when NoHTML == stdout; NoHTML == major ->
		io_lib:format("ERROR! init_per_testcase ~s!\n"
				      "\tLocation: ~p\n\tReason: ~tp\n",
				      [Cause,Line,Reason]);
	   (minor) ->
		ReasonStr = test_server_ctrl:escape_chars(Reason),
		io_lib:format("ERROR! init_per_testcase ~s!\n"
			      "\tLocation: ~ts\n\tReason: ~ts\n",
			      [Cause,FormattedLoc,ReasonStr])
	end,
    group_leader() ! {printout,12,Str2Print},
    ok.


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
    set_tc_state(end_per_testcase, Conf),
    try Mod:EndFunc(Func, Conf) of
	{save_config,_}=SaveCfg ->
	    SaveCfg;
	{fail,_}=Fail ->
	    Fail;
	_ ->
	    ok
    catch
	throw:Other ->
	    Comment0 = case read_comment() of
			   ""  -> "";
			   Cmt -> Cmt ++ test_server_ctrl:xhtml("<br>",
								"<br />")
		       end,
	    set_loc(erlang:get_stacktrace()),
	    comment(io_lib:format("~ts<font color=\"red\">"
				  "WARNING: ~w thrown!"
				  "</font>\n",[Comment0,EndFunc])),
	    print_end_tc_warning(EndFunc,Other,"thrown",get_loc()),
	    {failed,{Mod,end_per_testcase,Other}};
	  Class:Reason ->
	    Stk = erlang:get_stacktrace(),
	    set_loc(Stk),
	    Why = case Class of
		      exit -> {'EXIT',Reason};
		      error -> {'EXIT',{Reason,Stk}}
		  end,
	    Comment0 = case read_comment() of
			   ""  -> "";
			   Cmt -> Cmt ++ test_server_ctrl:xhtml("<br>",
								"<br />")
		       end,
	    comment(io_lib:format("~ts<font color=\"red\">"
				  "WARNING: ~w crashed!"
				  "</font>\n",[Comment0,EndFunc])),
	    print_end_tc_warning(EndFunc,Reason,"crashed",get_loc()),
	    {failed,{Mod,end_per_testcase,Why}}
    end.

print_end_tc_warning(EndFunc,Reason,Cause,Loc) ->
    FormattedLoc = test_server_sup:format_loc(Loc),
    Str2Print =
	fun(NoHTML) when NoHTML == stdout; NoHTML == major ->
		io_lib:format("WARNING: ~w ~s!\n"
			      "Reason: ~tp\nLine: ~p\n",
			      [EndFunc,Cause,Reason,Loc]);
	   (minor) ->
		ReasonStr = test_server_ctrl:escape_chars(Reason),
		io_lib:format("WARNING: ~w ~s!\n"
			      "Reason: ~ts\nLine: ~ts\n",
			      [EndFunc,Cause,ReasonStr,FormattedLoc])
	end,
    group_leader() ! {printout,12,Str2Print},
    ok.

get_loc() ->
    get(test_server_loc).

get_loc(Pid) ->
    [{current_stacktrace,Stk0},{dictionary,Dict}] =
	process_info(Pid, [current_stacktrace,dictionary]),
    lists:foreach(fun({Key,Val}) -> put(Key, Val) end, Dict),
    Stk = [rewrite_loc_item(Loc) || Loc <- Stk0],
    case get(test_server_loc) of
	[{Suite,Case}] ->
	    %% Location info unknown, check if {Suite,Case,Line}
	    %% is available in stacktrace and if so, use stacktrace
	    %% instead of current test_server_loc.
	    %% If location is the last expression in a test case
	    %% function, the info is not available due to tail call
	    %% elimination. We need to check if the test case has been
	    %% called by ts_tc/3 and, if so, insert the test case info
	    %% at that position.
	    case [match || {S,C,_L} <- Stk, S == Suite, C == Case] of
		[match|_] ->
		    put(test_server_loc, Stk);
		_ ->
		    {PreTC,PostTC} =
			lists:splitwith(fun({test_server,ts_tc,_}) ->
						false;
					   (_) ->
						true
					end, Stk),
		    if PostTC == [] ->
			    ok;
		       true ->
			    put(test_server_loc,
				PreTC++[{Suite,Case,last_expr} | PostTC])
		    end
	    end;
	_ ->
	    put(test_server_loc, Stk)
    end,
    get_loc().

fw_error_notify(Mod, Func, Args, Error) ->
    test_server_sup:framework_call(error_notification,
				   [Mod,Func,[Args],
				    {Error,unknown}]).
fw_error_notify(Mod, Func, Args, Error, Loc) ->
    test_server_sup:framework_call(error_notification,
				   [Mod,Func,[Args],
				    {Error,Loc}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print(Detail,Format,Args,Printer) -> ok
%% Detail = integer()
%% Format = string()
%% Args = [term()]
%%
%% Just like io:format, except that depending on the Detail value, the output
%% is directed to console, major and/or minor log files.

%% print(Detail,Format,Args) ->
%%    test_server_ctrl:print(Detail, Format, Args).

print(Detail,Format,Args,Printer) ->
    test_server_ctrl:print(Detail, Format, Args, Printer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print_timsteamp(Detail,Leader) -> ok
%%
%% Prints Leader followed by a time stamp (date and time). Depending on
%% the Detail value, the output is directed to console, major and/or minor
%% log files.

print_timestamp(Detail,Leader) ->
    test_server_ctrl:print_timestamp(Detail, Leader).


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

%%
%% IMPORTANT: get_loc/1 uses the name of this function when analysing
%% stack traces. If the name changes, get_loc/1 must be updated!
%%
ts_tc(M, F, A) ->
    Before = erlang:monotonic_time(),
    Result = try
		 apply(M, F, A)
	     catch
		 throw:{skip, Reason} -> {skip, Reason};
		 throw:{skipped, Reason} -> {skip, Reason};
		 exit:{skip, Reason} -> {skip, Reason};
		 exit:{skipped, Reason} -> {skip, Reason};
		 Type:Reason ->
		     Stk = erlang:get_stacktrace(),
		     set_loc(Stk),
		     case Type of
			 throw ->
			     {failed,{thrown,Reason}};
			 error ->
			     {'EXIT',{Reason,Stk}};
			 exit ->
			     {'EXIT',Reason}
		     end
	     end,
    After   = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(After-Before, native, micro_seconds),
    {Elapsed, Result}.

set_loc(Stk) ->
    Loc = case [rewrite_loc_item(I) || {_,_,_,_}=I <- Stk] of
	      [{M,F,0}|Stack] ->
		  [{M,F}|Stack];
	      Other ->
		  Other
	  end,
    put(test_server_loc, Loc).

rewrite_loc_item({M,F,_,Loc}) ->
    {M,F,proplists:get_value(line, Loc, 0)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                TEST SUITE SUPPORT FUNCTIONS                      %%
%%                                                                  %%
%% Note: Some of these functions have been moved to test_server_sup %%
%%       in an attempt to keep this modules small (yeah, right!)    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
%% permit_io(GroupLeader, FromPid) -> ok
%%
%% Make sure proceeding IO from FromPid won't get rejected
permit_io(GroupLeader, FromPid) ->
    GroupLeader ! {permit_io,FromPid},
    ok.

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
    try
	exit({suite_failed,Reason})
    catch
	Class:R ->
	    case erlang:get_stacktrace() of
		[{?MODULE,fail,1,_}|Stk] -> ok;
		Stk -> ok
	    end,
	    erlang:raise(Class, R, Stk)
    end.

cast_to_list(X) when is_list(X) -> X;
cast_to_list(X) when is_atom(X) -> atom_to_list(X);
cast_to_list(X) -> lists:flatten(io_lib:format("~p", [X])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fail() -> exit(suite_failed)
%%
%% Immediately calls exit. Included because test suites are easier
%% to read when using this function, rather than exit directly.
fail() ->
    try
	exit(suite_failed)
    catch
	Class:R ->
	    case erlang:get_stacktrace() of
		[{?MODULE,fail,0,_}|Stk] -> ok;
		Stk -> ok
	    end,
	    erlang:raise(Class, R, Stk)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% break(Comment) -> ok
%%
%% Break a test case so part of the test can be done manually.
%% Use continue/0 to continue.
break(Comment) ->
    break(?MODULE, Comment).

break(CBM, Comment) ->
    break(CBM, '', Comment).

break(CBM, TestCase, Comment) ->
    timetrap_cancel(),
    {TCName,CntArg,PName} =
	if TestCase == '' -> 
		{"", "", test_server_break_process};
	   true ->
		Str = atom_to_list(TestCase),
		{[32 | Str], Str,
		 list_to_atom("test_server_break_process_" ++ Str)}
	end,
    io:format(user,
	      "\n\n\n--- SEMIAUTOMATIC TESTING ---"
	      "\nThe test case~ts executes on process ~w"
	      "\n\n\n~ts"
	      "\n\n\n-----------------------------\n\n"
	      "Continue with --> ~w:continue(~ts).\n",
	      [TCName,self(),Comment,CBM,CntArg]),
    case whereis(PName) of
	undefined ->
	    spawn_break_process(self(), PName);
	OldBreakProcess ->
	    OldBreakProcess ! cancel,
	    spawn_break_process(self(), PName)
    end,
    receive continue -> ok end.

spawn_break_process(Pid, PName) ->
    spawn(fun() ->
		  register(PName, self()),
		  receive
		      continue -> continue(Pid);
		      cancel -> ok
		  end
	  end).

continue() ->
    case whereis(test_server_break_process) of
	undefined    -> ok;
	BreakProcess -> BreakProcess ! continue
    end.

continue(TestCase) when is_atom(TestCase) ->
    PName = list_to_atom("test_server_break_process_" ++
			 atom_to_list(TestCase)),
    case whereis(PName) of
	undefined    -> ok;
	BreakProcess -> BreakProcess ! continue
    end;	

continue(Pid) when is_pid(Pid) ->
    Pid ! continue.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap_scale_factor() -> Factor
%%
%% Returns the amount to scale timetraps with.

%% {X, fun() -> check() end} <- multiply scale with X if Fun() is true
timetrap_scale_factor() ->
    timetrap_scale_factor([
	{ 2, fun() -> has_lock_checking() end},
	{ 3, fun() -> has_superfluous_schedulers() end},
	{ 6, fun() -> is_debug() end},
	{10, fun() -> is_cover() end}
    ]).

timetrap_scale_factor(Scales) ->
    %% The fun in {S, Fun} a filter input to the list comprehension
    lists:foldl(fun(S,O) -> O*S end, 1, [ S || {S,F} <- Scales, F()]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap(Timeout) -> Handle
%% Handle = term()
%%
%% Creates a time trap, that will kill the calling process if the
%% trap is not cancelled with timetrap_cancel/1, within Timeout milliseconds.
timetrap(Timeout) ->
    MultAndScale =
	case get(test_server_multiply_timetraps) of
	    undefined -> {fun(T) -> T end, true};
	    {undefined,false} -> {fun(T) -> T end, false};
	    {undefined,_} -> {fun(T) -> T end, true};
	    {infinity,_} -> {fun(_) -> infinity end, false};
	    {Int,Scale} -> {fun(infinity) -> infinity;
			       (T) -> T*Int end, Scale}
	end,	    
    timetrap(Timeout, Timeout, self(), MultAndScale).

%% when the function is called from different process than
%% the test case, the test_server_multiply_timetraps data
%% is unknown and must be passed as argument
timetrap(Timeout, TCPid, MultAndScale) ->
    timetrap(Timeout, Timeout, TCPid, MultAndScale).

timetrap(Timeout0, TimeToReport0, TCPid, MultAndScale = {Multiplier,Scale}) ->
    %% the time_ms call will either convert Timeout to ms or spawn a
    %% user timetrap which sends the result to the IO server process
    Timeout = time_ms(Timeout0, TCPid, MultAndScale),
    Timeout1 = Multiplier(Timeout),
    TimeToReport = if Timeout0 == TimeToReport0 ->
			   Timeout1;
		      true ->
			   %% only convert to ms, don't start a
			   %% user timetrap
			   time_ms_check(TimeToReport0)
		   end,
    cancel_default_timetrap(self() == TCPid),
    Handle = case Timeout1 of
		 infinity ->
		     infinity;
		 _ ->
		     spawn_link(test_server_sup,timetrap,[Timeout1,TimeToReport,
							  Scale,TCPid])
	     end,

    %% ERROR! This sets dict on IO process instead of testcase process
    %% if Timeout is return value from previous user timetrap!!

    case get(test_server_timetraps) of
	undefined ->
	    put(test_server_timetraps,[{Handle,TCPid,{TimeToReport,Scale}}]);
	List ->
	    List1 = lists:delete({infinity,TCPid,{infinity,false}}, List),
	    put(test_server_timetraps,[{Handle,TCPid,
					{TimeToReport,Scale}}|List1])
    end,
    Handle.

ensure_timetrap(Config) ->
    case get(test_server_timetraps) of
	[_|_] ->
	    ok;
	_ ->
	    case get(test_server_default_timetrap) of
		undefined -> ok;
		Garbage ->
		    erase(test_server_default_timetrap),
		    format("=== WARNING: garbage in "
			   "test_server_default_timetrap: ~p~n",
			   [Garbage])
	    end,
	    DTmo = case lists:keysearch(default_timeout,1,Config) of
		       {value,{default_timeout,Tmo}} -> Tmo;
		       _ -> ?DEFAULT_TIMETRAP_SECS
		   end,
	    format("=== test_server setting default "
		   "timetrap of ~p seconds~n",
		   [DTmo]),
	    put(test_server_default_timetrap, timetrap(seconds(DTmo)))
    end.

%% executing on IO process, no default timetrap ever set here
cancel_default_timetrap(false) ->
    ok;
cancel_default_timetrap(true) ->
    case get(test_server_default_timetrap) of
	undefined ->
	    ok;
	TimeTrap when is_pid(TimeTrap) ->
	    timetrap_cancel(TimeTrap),
	    erase(test_server_default_timetrap),
	    format("=== test_server canceled default timetrap "
		   "since another timetrap was set~n"),
	    ok;
	Garbage ->
	    erase(test_server_default_timetrap),
	    format("=== WARNING: garbage in "
		   "test_server_default_timetrap: ~p~n",
		   [Garbage]),
	    error
    end.

time_ms({hours,N}, _, _) -> hours(N);
time_ms({minutes,N}, _, _) -> minutes(N);
time_ms({seconds,N}, _, _) -> seconds(N);
time_ms({Other,_N}, _, _) ->
    format("=== ERROR: Invalid time specification: ~p. "
	   "Should be seconds, minutes, or hours.~n", [Other]),
    exit({invalid_time_format,Other});
time_ms(Ms, _, _) when is_integer(Ms) -> Ms;
time_ms(infinity, _, _) -> infinity;
time_ms(Fun, TCPid, MultAndScale) when is_function(Fun) ->
    time_ms_apply(Fun, TCPid, MultAndScale);
time_ms({M,F,A}=MFA, TCPid, MultAndScale) when is_atom(M),
					       is_atom(F),
					       is_list(A) ->
    time_ms_apply(MFA, TCPid, MultAndScale);
time_ms(Other, _, _) -> exit({invalid_time_format,Other}).

time_ms_check(MFA = {M,F,A}) when is_atom(M), is_atom(F), is_list(A) ->
    MFA;
time_ms_check(Fun) when is_function(Fun) ->
    Fun;
time_ms_check(Other) ->
    time_ms(Other, undefined, undefined).

time_ms_apply(Func, TCPid, MultAndScale) ->
    {_,GL} = process_info(TCPid, group_leader),
    WhoAmI = self(),				% either TC or IO server
    T0 = erlang:monotonic_time(),
    UserTTSup = 
	spawn(fun() -> 
		      user_timetrap_supervisor(Func, WhoAmI, TCPid,
					       GL, T0, MultAndScale)
	      end),
    receive
	{UserTTSup,infinity} ->
	    %% remember the user timetrap so that it can be cancelled
	    save_user_timetrap(TCPid, UserTTSup, T0),
	    %% we need to make sure the user timetrap function
	    %% gets time to execute and return
	    timetrap(infinity, TCPid, MultAndScale)
    after 5000 ->
	    exit(UserTTSup, kill),
	    if WhoAmI /= GL ->
		    exit({user_timetrap_error,time_ms_apply});
	       true ->
		    format("=== ERROR: User timetrap execution failed!", []),
		    ignore
	    end
    end.

user_timetrap_supervisor(Func, Spawner, TCPid, GL, T0, MultAndScale) ->
    process_flag(trap_exit, true),
    Spawner ! {self(),infinity},
    MonRef = monitor(process, TCPid),
    UserTTSup = self(),
    group_leader(GL, UserTTSup),
    UserTT = spawn_link(fun() -> call_user_timetrap(Func, UserTTSup) end),
    receive
	{UserTT,Result} ->
	    demonitor(MonRef, [flush]),
	    T1 = erlang:monotonic_time(),
	    Elapsed = erlang:convert_time_unit(T1-T0, native, milli_seconds),
	    try time_ms_check(Result) of
		TimeVal ->
		    %% this is the new timetrap value to set (return value
		    %% from a fun or an MFA)
		    GL ! {user_timetrap,TCPid,TimeVal,T0,Elapsed,MultAndScale}
	    catch _:_ ->		    
		    %% when other than a legal timetrap value is returned
		    %% which will be the normal case for user timetraps
		    GL ! {user_timetrap,TCPid,0,T0,Elapsed,MultAndScale}
	    end;
	{'EXIT',UserTT,Error} when Error /= normal ->
	    demonitor(MonRef, [flush]),
	    GL ! {user_timetrap,TCPid,0,T0,{user_timetrap_error,Error},
		  MultAndScale};
	{'DOWN',MonRef,_,_,_} ->
	    demonitor(MonRef, [flush]),
	    exit(UserTT, kill)
    end.

call_user_timetrap(Func, Sup) when is_function(Func) ->
    try Func() of
	Result -> 
	    Sup ! {self(),Result}
    catch _:Error ->
	    exit({Error,erlang:get_stacktrace()})
    end;
call_user_timetrap({M,F,A}, Sup) ->
    try apply(M,F,A) of
	Result -> 
	    Sup ! {self(),Result}
    catch _:Error ->
	    exit({Error,erlang:get_stacktrace()})
    end.

save_user_timetrap(TCPid, UserTTSup, StartTime) ->
    %% save pid of user timetrap supervisor process so that
    %% it may be stopped even before the timetrap func has returned
    NewUserTT = {TCPid,{UserTTSup,StartTime}},
    case get(test_server_user_timetrap) of
	undefined ->
	    put(test_server_user_timetrap, [NewUserTT]);
	UserTTSups ->
	    case proplists:get_value(TCPid, UserTTSups) of
		undefined ->
		    put(test_server_user_timetrap,
			[NewUserTT | UserTTSups]);
		PrevTTSup ->
		    %% remove prev user timetrap
		    remove_user_timetrap(PrevTTSup),
		    put(test_server_user_timetrap,
			[NewUserTT | proplists:delete(TCPid,
						      UserTTSups)])
	    end
    end.
    
update_user_timetraps(TCPid, StartTime) ->
    %% called when a user timetrap is triggered
    case get(test_server_user_timetrap) of
	undefined ->
	    proceed;
	UserTTs ->
	    case proplists:get_value(TCPid, UserTTs) of
		{_UserTTSup,StartTime} ->	% same timetrap
		    put(test_server_user_timetrap,
			proplists:delete(TCPid, UserTTs)),
		    proceed;
		{OtherUserTTSup,OtherStartTime} ->
		    case OtherStartTime - StartTime of
			Diff when Diff >= 0 ->
			    ignore;
			_ ->
			    exit(OtherUserTTSup, kill),
			    put(test_server_user_timetrap,
				proplists:delete(TCPid, UserTTs)),
			    proceed
		    end;
		undefined ->
		    proceed
	    end
    end.

remove_user_timetrap(TTSup) ->
    exit(TTSup, kill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap_cancel(Handle) -> ok
%% Handle = term()
%%
%% Cancels a time trap.
timetrap_cancel(Handle) ->
    timetrap_cancel_one(Handle, true).

timetrap_cancel_one(infinity, _SendToServer) ->
    ok;
timetrap_cancel_one(Handle, SendToServer) ->
    case get(test_server_timetraps) of
	undefined ->
	    ok;
	[{Handle,_,_}] ->
	    erase(test_server_timetraps);
	Timers ->
	    case lists:keysearch(Handle, 1, Timers) of
		{value,_} ->
		    put(test_server_timetraps,
			lists:keydelete(Handle, 1, Timers));
		false when SendToServer == true ->
		    group_leader() ! {timetrap_cancel_one,Handle,self()};
		false ->
		    ok
	    end
    end,
    test_server_sup:timetrap_cancel(Handle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap_cancel() -> ok
%%
%% Cancels timetrap for current test case.
timetrap_cancel() ->
    timetrap_cancel_all(self(), true).

timetrap_cancel_all(TCPid, SendToServer) ->
    case get(test_server_timetraps) of
	undefined ->
	    ok;
	Timers ->
	    [timetrap_cancel_one(Handle, false) ||
		{Handle,Pid,_} <- Timers, Pid == TCPid],
	    ok
    end,
    case get(test_server_user_timetrap) of
	undefined ->
	    ok;
	UserTTs ->
	    case proplists:get_value(TCPid, UserTTs) of
		{UserTTSup,_StartTime} ->
		    remove_user_timetrap(UserTTSup),
		    put(test_server_user_timetrap,
			proplists:delete(TCPid, UserTTs)),
			ok;
		undefined ->
		    ok
	    end
    end,
    if SendToServer == true ->
	    group_leader() ! {timetrap_cancel_all,TCPid,self()},
	    ok;
       true ->
	    ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_timetrap_info() -> {Timeout,Scale} | undefined
%%
%% Read timetrap info for current test case
get_timetrap_info() ->
    get_timetrap_info(self(), true).

get_timetrap_info(TCPid, SendToServer) ->
    case get(test_server_timetraps) of
	undefined ->
	    undefined;
	Timers ->
	    case [Info || {Handle,Pid,Info} <- Timers, 
			  Pid == TCPid, Handle /= infinity] of
		[{TVal,true}|_] ->
		    {TVal,{true,test_server:timetrap_scale_factor()}};
		[{TVal,false}|_] ->
		    {TVal,{false,1}};
		[] when SendToServer == true ->
		    case tc_supervisor_req({get_timetrap_info,TCPid}) of
			{TVal,true} ->
			    {TVal,{true,test_server:timetrap_scale_factor()}};
			{TVal,false} ->
			    {TVal,{false,1}};
			Error ->
			    Error
		    end;
		[] ->
		    undefined
	    end
    end.

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
%% tc_supervisor_req(Tag) -> Result
%% tc_supervisor_req(Tag, Msg) -> Result
%%

tc_supervisor_req(Tag) ->
    Pid = test_server_gl:get_tc_supervisor(group_leader()),
    Pid ! {Tag,self()},
    receive
	{Pid,Tag,Result} ->
	    Result
    after 5000 ->
	    error(no_answer_from_tc_supervisor)
    end.

tc_supervisor_req(Tag, Msg) ->
    Pid = test_server_gl:get_tc_supervisor(group_leader()),
    Pid ! {Tag,self(),Msg},
    receive
	{Pid,Tag,Result} ->
	    Result
    after 5000 ->
	    error(no_answer_from_tc_supervisor)
    end.

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
            Cover = case is_cover(Node) of
                        true ->
			    proplists:get_value(start_cover,Options,true);
                        false ->
                            false
                    end,

	    net_adm:ping(Node),
	    case Cover of
		true ->
		    do_cover_for_node(Node,start);
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
    Result = receive {sync_result,R} -> R end,
    case Result of
	ok ->
	    net_adm:ping(Slave),
	    case is_cover(Slave) of
		true ->
		    do_cover_for_node(Slave,start);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    Result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stop_node(Name) -> true|false
%%
%% Kills a (remote) node.
%% Also inform test_server_ctrl so it can clean up!
stop_node(Slave) ->
    Cover = is_cover(Slave),
    if Cover -> do_cover_for_node(Slave,flush,false);
       true -> ok
    end,
    group_leader() ! {sync_apply,self(),{test_server_ctrl,stop_node,[Slave]}},
    Result = receive {sync_result,R} -> R end,
    case Result of
	ok ->
	    erlang:monitor_node(Slave, true),
	    slave:stop(Slave),
	    receive
		{nodedown, Slave} ->
		    format(minor, "Stopped slave node: ~w", [Slave]),
		    format(major, "=node_stop     ~w", [Slave]),
		    if Cover -> do_cover_for_node(Slave,stop,false);
		       true -> ok
		    end,
		    true
	    after 30000 ->
		    format("=== WARNING: Node ~w does not seem to terminate.",
			   [Slave]),
		    erlang:monitor_node(Slave, false),
		    receive {nodedown, Slave} -> ok after 0 -> ok end,
		    false
	    end;
	{error, _Reason} ->
	    %% Either, the node is already dead or it was started
	    %% with the {cleanup,false} option, or it was started
	    %% in some other way than test_server:start_node/3
	    format("=== WARNING: Attempt to stop a nonexisting slavenode (~w)~n"
		   "===          Trying to kill it anyway!!!",
		   [Slave]),
	    case net_adm:ping(Slave)of
		pong ->
		    erlang:monitor_node(Slave, true),
		    slave:stop(Slave),
		    receive
			{nodedown, Slave} ->
			    format(minor, "Stopped slave node: ~w", [Slave]),
			    format(major, "=node_stop     ~w", [Slave]),
			    if Cover -> do_cover_for_node(Slave,stop,false);
			       true -> ok
			    end,
			    true
		    after 30000 ->
			    format("=== WARNING: Node ~w does not seem to terminate.",
				   [Slave]),
			    erlang:monitor_node(Slave, false),
			    receive {nodedown, Slave} -> ok after 0 -> ok end,
			    false
		    end;
		pang ->
		    if Cover -> do_cover_for_node(Slave,stop,false);
		       true -> ok
		    end,
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
    Nr = erlang:unique_integer([positive]),
    Name = "shielded_node-" ++ integer_to_list(Nr),
    Node = case start_node(Name, slave, [{args, "-hidden " ++ CArgs}]) of
	       {ok, N} -> N;
	       Err -> fail({failed_to_start_shielded_node, Err})
	   end,
    Master = self(),
    Ref = make_ref(),
    Slave = spawn(Node, start_job_proxy_fun(Master, Fun)),
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

-spec start_job_proxy_fun(_, _) -> fun(() -> no_return()).
start_job_proxy_fun(Master, Fun) ->
    fun () ->
            _ = start_job_proxy(),
            receive
                Ref ->
                    Master ! {Ref, Fun()},
                    ok
            end,
            receive after infinity -> infinity end
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

is_cover(Name) ->
    case is_cover() of
	true ->
	    not is_shielded(Name) andalso same_version(Name);
	false ->
	    false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% temp_name(Stem) -> string()
%% Stem = string()
%%
%% Create a unique file name, based on (starting with) Stem.
%% A filename of the form <Stem><Number> is generated, and the
%% function checks that that file doesn't already exist.
temp_name(Stem) ->
    Num = erlang:unique_integer([positive]),
    RandomName = Stem ++ integer_to_list(Num),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% appup_test/1
%%
appup_test(App) ->
    test_server_sup:appup_test(App).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_native(Mod) -> true | false
%%
%% Checks wether the module is natively compiled or not.

is_native(Mod) ->
    (catch Mod:module_info(native)) =:= true.

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
%% read_comment() -> string()
%%
%% Read the current comment string stored in
%% state during test case execution.
read_comment() ->
    tc_supervisor_req(read_comment).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make_priv_dir() -> ok
%%
%% Order test server to create the private directory
%% for the current test case.
make_priv_dir() ->
    tc_supervisor_req(make_priv_dir).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% os_type() -> OsType
%%
%% Returns the OsType of the target node. OsType is
%% the same as returned from os:type()
os_type() ->
    os:type().


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Apply given function and reply to caller or proxy.
%%
do_sync_apply(Proxy, From, {M,F,A}) ->
    Result = apply(M, F, A),
    if  is_pid(Proxy) ->
	    Proxy ! {sync_result_proxy,From,Result},
	    ok;
	true ->
	    From ! {sync_result,Result},
	    ok
    end.

start_cover() ->
    case cover:start() of
       {error, {already_started, Pid}} ->
           {ok, Pid};
       Else ->
           Else
   end.

