%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%% @doc Common Test Framework test execution control module.
%%%
%%% <p>This module exports functions for installing and running tests
%%% withing the Common Test Framework.</p>

-module(ct_run).

%% Script interface
-export([script_start/0,script_usage/0]).

%% User interface
-export([install/1,install/2,run/1,run/2,run/3,run_test/1,
	 run_testspec/1,step/3,step/4,refresh_logs/1]).


%% Exported for VTS
-export([run_make/3,do_run/4,tests/1,tests/2,tests/3]).


%% Misc internal functions
-export([variables_file_name/1,script_start1/2,run_test2/1]).

-include("ct.hrl").
-include("ct_event.hrl").
-include("ct_util.hrl").

-define(abs(Name), filename:absname(Name)).
-define(testdir(Name, Suite), ct_util:get_testdir(Name, Suite)).

-define(EXIT_STATUS_TEST_SUCCESSFUL, 0).
-define(EXIT_STATUS_TEST_CASE_FAILED, 1).
-define(EXIT_STATUS_TEST_RUN_FAILED, 2).

-define(default_verbosity, [{default,?MAX_VERBOSITY},
			    {'$unspecified',?MAX_VERBOSITY}]).

-record(opts, {label,
	       profile,
	       vts,
	       shell,
	       cover,
	       cover_stop,
	       coverspec,
	       step,
	       logdir,
	       logopts = [],
	       basic_html,
	       esc_chars = true,
	       verbosity = [],
	       config = [],
	       event_handlers = [],
	       ct_hooks = [],
	       enable_builtin_hooks,
	       include = [],
	       auto_compile,
	       abort_if_missing_suites,
	       silent_connections = [],
	       stylesheet,
	       multiply_timetraps,
	       scale_timetraps,
	       create_priv_dir,
	       testspec_files = [],
	       current_testspec,
	       tests,
	       starter}).

%%%-----------------------------------------------------------------
%%% @spec script_start() -> term()
%%%
%%% @doc Start tests via the ct_run program or script.
%%%
%%% <p>Example:<br/><code>./ct_run -config config.ctc -dir
%%% $TEST_DIR</code></p>
%%%
%%% <p>Example:<br/><code>./ct_run -config config.ctc -suite
%%% $SUITE_PATH/$SUITE_NAME [-case $CASE_NAME]</code></p>
%%%
script_start() ->
    process_flag(trap_exit, true),
    Init = init:get_arguments(),
    CtArgs = lists:takewhile(fun({ct_erl_args,_}) -> false;
				(_) -> true end, Init),

    %% convert relative dirs added with pa or pz (pre erl_args on
    %% the ct_run command line) to absolute so that app modules
    %% can be found even after CT changes CWD to logdir
    rel_to_abs(CtArgs),

    Args =
	case application:get_env(common_test, run_test_start_opts) of
	    {ok,EnvStartOpts} ->
		FlagFilter = fun(Flags) ->
				     lists:filter(fun({root,_}) -> false;
						     ({progname,_}) -> false;
						     ({home,_}) -> false;
						     ({noshell,_}) -> false;
						     ({noinput,_}) -> false;
						     (_) -> true
						  end, Flags)
			     end,
		%% used for purpose of testing the run_test interface
		io:format(user, "~n-------------------- START ARGS "
			  "--------------------~n", []),
		io:format(user, "--- Init args:~n~p~n", [FlagFilter(Init)]),
		io:format(user, "--- CT args:~n~p~n", [FlagFilter(CtArgs)]),
		EnvArgs = opts2args(EnvStartOpts),
		io:format(user, "--- Env opts -> args:~n~p~n   =>~n~p~n",
			  [EnvStartOpts,EnvArgs]),
		Merged = merge_arguments(CtArgs ++ EnvArgs),
		io:format(user, "--- Merged args:~n~p~n", [FlagFilter(Merged)]),
		io:format(user, "-----------------------------------"
			  "-----------------~n~n", []),
		Merged;
	    _ ->
		merge_arguments(CtArgs)
	end,
    case proplists:get_value(help, Args) of
	undefined -> script_start(Args);
	_ -> script_usage()
    end.

script_start(Args) ->
    Tracing = start_trace(Args),
    case ct_repeat:loop_test(script, Args) of
	false ->
	    {ok,Cwd} = file:get_cwd(),
	    CTVsn =
		case filename:basename(code:lib_dir(common_test)) of
		    CTBase when is_list(CTBase) ->
			case string:tokens(CTBase, "-") of
			    ["common_test",Vsn] -> " v"++Vsn;
			    _ -> ""
			end
		end,
	    io:format("~nCommon Test~s starting (cwd is ~ts)~n~n",
	              [CTVsn,Cwd]),
	    Self = self(),
	    Pid = spawn_link(fun() -> script_start1(Self, Args) end),
	    receive
		{'EXIT',Pid,Reason} ->
		    case Reason of
			{user_error,What} ->
			    io:format("\nTest run failed!\nReason: ~p\n\n\n",
                                      [What]),
			    finish(Tracing, ?EXIT_STATUS_TEST_RUN_FAILED, Args);
			_ ->
			    io:format("Test run crashed! "
                                      "This could be an internal error "
				      "- please report!\n\n"
				      "~p\n\n\n", [Reason]),
			    finish(Tracing, ?EXIT_STATUS_TEST_RUN_FAILED, Args)
		    end;
		{Pid,{error,Reason}} ->
		    io:format("\nTest run failed! Reason:\n~p\n\n\n",[Reason]),
		    finish(Tracing, ?EXIT_STATUS_TEST_RUN_FAILED, Args);
		{Pid,Result} ->
		    io:nl(),
		    finish(Tracing, analyze_test_result(Result, Args), Args)
	    end;
	{error,_LoopReason} ->
	    finish(Tracing, ?EXIT_STATUS_TEST_RUN_FAILED, Args);
	Result ->
	    io:nl(),
	    finish(Tracing, analyze_test_result(Result, Args), Args)
    end.

%% analyze the result of one test run, or many (in case of looped test)
analyze_test_result(ok, _) ->
    ?EXIT_STATUS_TEST_SUCCESSFUL;
analyze_test_result({error,_Reason}, _) ->
    ?EXIT_STATUS_TEST_RUN_FAILED;
analyze_test_result({_Ok,Failed,{_UserSkipped,AutoSkipped}}, Args) ->
    if Failed > 0 ->
	    ?EXIT_STATUS_TEST_CASE_FAILED;
       true ->
	    case AutoSkipped of
		0 ->
		    ?EXIT_STATUS_TEST_SUCCESSFUL;
		_ ->
		    case get_start_opt(exit_status,
				       fun([ExitOpt]) -> ExitOpt end,
				       Args) of
			undefined ->
			    ?EXIT_STATUS_TEST_CASE_FAILED;
			"ignore_config" ->
		    	    ?EXIT_STATUS_TEST_SUCCESSFUL
		    end
	    end
    end;
analyze_test_result([Result|Rs], Args) ->
    case analyze_test_result(Result, Args) of
	?EXIT_STATUS_TEST_SUCCESSFUL ->
	    analyze_test_result(Rs, Args);
	Other ->
	    Other
    end;
analyze_test_result([], _) ->
    ?EXIT_STATUS_TEST_SUCCESSFUL;
analyze_test_result(interactive_mode, _) ->
    interactive_mode;
analyze_test_result(Unknown, _) ->
    io:format("\nTest run failed! Reason:\n~p\n\n\n",[Unknown]),
    ?EXIT_STATUS_TEST_RUN_FAILED.

finish(Tracing, ExitStatus, Args) ->
    stop_trace(Tracing),
    timer:sleep(1000),
    if ExitStatus == interactive_mode ->
	    interactive_mode;
       true ->
	    case get_start_opt(vts, true, Args) of
		true ->
		    %% VTS mode, don't halt the node
		    ok;
		_ ->
		    %% it's possible to tell CT to finish execution with a call
		    %% to a different function than the normal halt/1 BIF
		    %% (meant to be used mainly for reading the CT exit status)
		    case get_start_opt(halt_with,
				       fun([HaltMod,HaltFunc]) -> 
					       {list_to_atom(HaltMod),
						list_to_atom(HaltFunc)} end,
				       Args) of
			undefined ->
			    halt(ExitStatus);
			{M,F} ->
			    apply(M, F, [ExitStatus])
		    end
	    end
    end.

script_start1(Parent, Args) ->
    %% read general start flags
    Label = get_start_opt(label, fun([Lbl]) -> Lbl end, Args),
    Profile = get_start_opt(profile, fun([Prof]) -> Prof end, Args),
    Vts = get_start_opt(vts, true, undefined, Args),
    Shell = get_start_opt(shell, true, Args),
    Cover = get_start_opt(cover, fun([CoverFile]) -> ?abs(CoverFile) end, Args),
    CoverStop = get_start_opt(cover_stop, 
			      fun([CS]) -> list_to_atom(CS) end, Args),
    LogDir = get_start_opt(logdir, fun([LogD]) -> LogD end, Args),
    LogOpts = get_start_opt(logopts,
			    fun(Os) -> [list_to_atom(O) || O <- Os] end,
			    [], Args),
    Verbosity = verbosity_args2opts(Args),
    MultTT = get_start_opt(multiply_timetraps,
			   fun([MT]) -> list_to_integer(MT) end, Args),
    ScaleTT = get_start_opt(scale_timetraps,
			    fun([CT]) -> list_to_atom(CT);
			       ([]) -> true
			    end, Args),
    CreatePrivDir = get_start_opt(create_priv_dir,
				  fun([PD]) -> list_to_atom(PD);
				     ([]) -> auto_per_tc
				  end, Args),
    EvHandlers = event_handler_args2opts(Args),
    CTHooks = ct_hooks_args2opts(Args),
    EnableBuiltinHooks = get_start_opt(enable_builtin_hooks,
				       fun([CT]) -> list_to_atom(CT);
					  ([]) -> undefined
				       end, undefined, Args),

    %% check flags and set corresponding application env variables

    %% ct_decrypt_key | ct_decrypt_file
    case proplists:get_value(ct_decrypt_key, Args) of
	[DecryptKey] ->
	    application:set_env(common_test, decrypt, {key,DecryptKey});
	undefined ->
	    case proplists:get_value(ct_decrypt_file, Args) of
		[DecryptFile] ->
		    application:set_env(common_test, decrypt,
					{file,?abs(DecryptFile)});
		undefined ->
		    application:unset_env(common_test, decrypt)
	    end
    end,
    %% no_auto_compile + include
    {AutoCompile,IncludeDirs} =
	case proplists:get_value(no_auto_compile, Args) of
	    undefined ->
		application:set_env(common_test, auto_compile, true),
		InclDirs =
		    case proplists:get_value(include, Args) of
			Incls when is_list(hd(Incls)) ->
			    [filename:absname(IDir) || IDir <- Incls];
			Incl when is_list(Incl) ->
			    [filename:absname(Incl)];
			undefined ->
			    []
		    end,
		case os:getenv("CT_INCLUDE_PATH") of
		    false ->
			application:set_env(common_test, include, InclDirs),
			{undefined,InclDirs};
		    CtInclPath ->
			AllInclDirs =
			    string:tokens(CtInclPath,[$:,$ ,$,]) ++ InclDirs,
			application:set_env(common_test, include, AllInclDirs),
			{undefined,AllInclDirs}
		end;
	    _ ->
		application:set_env(common_test, auto_compile, false),
		{false,[]}
	end,

    %% abort test run if some suites can't be compiled
    AbortIfMissing = get_start_opt(abort_if_missing_suites,
				   fun([]) -> true;
				      ([Bool]) -> list_to_atom(Bool)
				   end, false, Args),
    %% silent connections
    SilentConns =
	get_start_opt(silent_connections,
		      fun(["all"]) -> [all];
			 (Conns) -> [list_to_atom(Conn) || Conn <- Conns]
		      end, [], Args),
    %% stylesheet
    Stylesheet = get_start_opt(stylesheet,
			       fun([SS]) -> ?abs(SS) end, Args),
    %% basic_html - used by ct_logs
    BasicHtml = case {Vts,proplists:get_value(basic_html, Args)} of
		    {undefined,undefined} ->
			application:set_env(common_test, basic_html, false),
			undefined;
		    _ ->
			application:set_env(common_test, basic_html, true),
			true
		end,
    %% esc_chars - used by ct_logs
    EscChars = case proplists:get_value(no_esc_chars, Args) of
		   undefined ->
		       application:set_env(common_test, esc_chars, true),
		       undefined;
		   _ ->
		       application:set_env(common_test, esc_chars, false),
		       false
	       end,
    %% disable_log_cache - used by ct_logs
    case proplists:get_value(disable_log_cache, Args) of
	undefined ->
	    application:set_env(common_test, disable_log_cache, false);
	_ ->
	    application:set_env(common_test, disable_log_cache, true)
    end,
    %% log_cleanup - used by ct_logs
    KeepLogs = get_start_opt(keep_logs,
                             fun ct_logs:parse_keep_logs/1,
                             all,
                             Args),
    application:set_env(common_test, keep_logs, KeepLogs),

    Opts = #opts{label = Label, profile = Profile,
		 vts = Vts, shell = Shell,
		 cover = Cover, cover_stop = CoverStop,
		 logdir = LogDir, logopts = LogOpts,
		 basic_html = BasicHtml,
		 esc_chars = EscChars,
		 verbosity = Verbosity,
		 event_handlers = EvHandlers,
		 ct_hooks = CTHooks,
		 enable_builtin_hooks = EnableBuiltinHooks,
		 auto_compile = AutoCompile,
		 abort_if_missing_suites = AbortIfMissing,
		 include = IncludeDirs,
		 silent_connections = SilentConns,
		 stylesheet = Stylesheet,
		 multiply_timetraps = MultTT,
		 scale_timetraps = ScaleTT,
		 create_priv_dir = CreatePrivDir,
		 starter = script},

    %% check if log files should be refreshed or go on to run tests...
    Result = run_or_refresh(Opts, Args),

    %% send final results to starting process waiting in script_start/0
    Parent ! {self(), Result}.

run_or_refresh(Opts = #opts{logdir = LogDir}, Args) ->
    case proplists:get_value(refresh_logs, Args) of
	undefined ->
	    script_start2(Opts, Args);
	Refresh ->
	    LogDir1 = case Refresh of
			  [] -> which(logdir,LogDir);
			  [RefreshDir] -> ?abs(RefreshDir)
		      end,
	    {ok,Cwd} = file:get_cwd(),
	    ok = file:set_cwd(LogDir1),
	    %% give the shell time to print version etc
	    timer:sleep(500),
	    io:nl(),
	    case catch ct_logs:make_all_runs_index(refresh) of
		{'EXIT',ARReason} ->
		    ok = file:set_cwd(Cwd),
		    {error,{all_runs_index,ARReason}};
		_ ->
		    case catch ct_logs:make_all_suites_index(refresh) of
			{'EXIT',ASReason} ->
			    ok = file:set_cwd(Cwd),
			    {error,{all_suites_index,ASReason}};
			_ ->
			    ok = file:set_cwd(Cwd),
			    io:format("Logs in ~ts refreshed!~n~n",
				      [LogDir1]),
			    timer:sleep(500), % time to flush io before quitting
			    ok
		    end
	    end
    end.

script_start2(Opts = #opts{vts = undefined,
			   shell = undefined}, Args) ->
    case proplists:get_value(spec, Args) of
	Specs when Specs =/= [], Specs =/= undefined ->
	    Specs1 = get_start_opt(join_specs, [Specs], Specs, Args),
	    %% using testspec as input for test
	    Relaxed = get_start_opt(allow_user_terms, true, false, Args),
	    case catch ct_testspec:collect_tests_from_file(Specs1, Relaxed) of
		{E,Reason} when E == error ; E == 'EXIT' ->
		    StackTrace = erlang:get_stacktrace(),
		    {error,{invalid_testspec,{Reason,StackTrace}}};
		TestSpecData ->
		    execute_all_specs(TestSpecData, Opts, Args, [])
	    end;
	[] ->
	    {error,no_testspec_specified};
	_ ->	    % no testspec used
	    %% read config/userconfig from start flags
	    InitConfig = ct_config:prepare_config_list(Args),
	    TheLogDir = which(logdir, Opts#opts.logdir),
	    case check_and_install_configfiles(InitConfig,
					       TheLogDir,
					       Opts) of
		ok ->      % go on read tests from start flags
		    script_start3(Opts#opts{config=InitConfig,
					    logdir=TheLogDir}, Args);
		Error ->
		    Error
	    end
    end;

script_start2(Opts, Args) ->
    %% read config/userconfig from start flags
    InitConfig = ct_config:prepare_config_list(Args),
    LogDir = which(logdir, Opts#opts.logdir),
    case check_and_install_configfiles(InitConfig, LogDir, Opts) of
	ok ->      % go on read tests from start flags
	    script_start3(Opts#opts{config=InitConfig,
				    logdir=LogDir}, Args);
	Error ->
	    Error
    end.

execute_all_specs([], _, _, Result) ->
    Result1 = lists:reverse(Result),
    case lists:keysearch('EXIT', 1, Result1) of
	{value,{_,_,ExitReason}} ->
	    exit(ExitReason);
	false ->
	    case lists:keysearch(error, 1, Result1) of
		{value,Error} ->
		    Error;
		false ->
		    lists:foldl(fun({Ok,Fail,{UserSkip,AutoSkip}},
				    {Ok1,Fail1,{UserSkip1,AutoSkip1}}) ->
					{Ok1+Ok,Fail1+Fail,
					 {UserSkip1+UserSkip,
					  AutoSkip1+AutoSkip}}
				end, {0,0,{0,0}}, Result1)
	    end
    end;

execute_all_specs([{Specs,TS} | TSs], Opts, Args, Result) ->    
    CombinedOpts = combine_test_opts(TS, Specs, Opts),
    try execute_one_spec(TS, CombinedOpts, Args) of
	ExecResult ->
	    execute_all_specs(TSs, Opts, Args, [ExecResult|Result])
    catch
	_ : ExitReason ->
	    execute_all_specs(TSs, Opts, Args,
			      [{'EXIT',self(),ExitReason}|Result])
    end.

execute_one_spec(TS, Opts, Args) ->
    %% read config/userconfig from start flags
    InitConfig = ct_config:prepare_config_list(Args),
    TheLogDir = which(logdir, Opts#opts.logdir),
    %% merge config from start flags with config from testspec
    AllConfig = merge_vals([InitConfig, Opts#opts.config]),
    case check_and_install_configfiles(AllConfig, TheLogDir, Opts) of
	ok ->      % read tests from spec
	    {Run,Skip} = ct_testspec:prepare_tests(TS, node()),
	    Result = do_run(Run, Skip, Opts#opts{config=AllConfig,
						 logdir=TheLogDir,
						 current_testspec=TS}, Args),
	    ct_util:delete_testdata(testspec),
	    Result;
	Error ->
	    Error
    end.

combine_test_opts(TS, Specs, Opts) ->
    TSOpts = get_data_for_node(TS, node()),

    Label = choose_val(Opts#opts.label,
		       TSOpts#opts.label),

    Profile = choose_val(Opts#opts.profile,
			 TSOpts#opts.profile),

    LogDir = choose_val(Opts#opts.logdir,
			TSOpts#opts.logdir),

    AllLogOpts = merge_vals([Opts#opts.logopts,
			     TSOpts#opts.logopts]),
    AllVerbosity =
	merge_keyvals([Opts#opts.verbosity,
		       TSOpts#opts.verbosity]),
    AllSilentConns =
	merge_vals([Opts#opts.silent_connections,
		    TSOpts#opts.silent_connections]),
    Cover =
	choose_val(Opts#opts.cover,
		   TSOpts#opts.cover),
    CoverStop =
	choose_val(Opts#opts.cover_stop,
		   TSOpts#opts.cover_stop),
    MultTT =
	choose_val(Opts#opts.multiply_timetraps,
		   TSOpts#opts.multiply_timetraps),
    ScaleTT =
	choose_val(Opts#opts.scale_timetraps,
		   TSOpts#opts.scale_timetraps),

    CreatePrivDir =
	choose_val(Opts#opts.create_priv_dir,
		   TSOpts#opts.create_priv_dir),

    AllEvHs = 
	merge_vals([Opts#opts.event_handlers,
		    TSOpts#opts.event_handlers]),

    AllCTHooks = merge_vals(
		   [Opts#opts.ct_hooks,
		    TSOpts#opts.ct_hooks]),

    EnableBuiltinHooks =
	choose_val(
	  Opts#opts.enable_builtin_hooks,
	  TSOpts#opts.enable_builtin_hooks),

    Stylesheet =
	choose_val(Opts#opts.stylesheet,
		   TSOpts#opts.stylesheet),

    AllInclude = merge_vals([Opts#opts.include,
			     TSOpts#opts.include]),
    application:set_env(common_test, include, AllInclude),

    AutoCompile =
	case choose_val(Opts#opts.auto_compile,
			TSOpts#opts.auto_compile) of
	    undefined ->
		true;
	    ACBool ->
		application:set_env(common_test,
				    auto_compile,
				    ACBool),
		ACBool
	end,

    AbortIfMissing = choose_val(Opts#opts.abort_if_missing_suites,
				TSOpts#opts.abort_if_missing_suites),

    BasicHtml =
	case choose_val(Opts#opts.basic_html,
			TSOpts#opts.basic_html) of
	    undefined ->
		false;
	    BHBool ->
		application:set_env(common_test, basic_html, 
				    BHBool),
		BHBool
	end,

    EscChars =
	case choose_val(Opts#opts.esc_chars,
			TSOpts#opts.esc_chars) of
	    undefined ->
		true;
	    ECBool ->
		application:set_env(common_test, esc_chars,
				    ECBool),
		ECBool
	end,

    Opts#opts{label = Label,
	      profile = Profile,
	      testspec_files = Specs,
	      cover = Cover,
	      cover_stop = CoverStop,
	      logdir = which(logdir, LogDir),
	      logopts = AllLogOpts,
	      basic_html = BasicHtml,
	      esc_chars = EscChars,
	      verbosity = AllVerbosity,
	      silent_connections = AllSilentConns,
	      config = TSOpts#opts.config,
	      event_handlers = AllEvHs,
	      ct_hooks = AllCTHooks,
	      enable_builtin_hooks = EnableBuiltinHooks,
	      stylesheet = Stylesheet,
	      auto_compile = AutoCompile,
	      abort_if_missing_suites = AbortIfMissing,
	      include = AllInclude,
	      multiply_timetraps = MultTT,
	      scale_timetraps = ScaleTT,
	      create_priv_dir = CreatePrivDir}.

check_and_install_configfiles(
  Configs, LogDir, #opts{
	     event_handlers = EvHandlers,
	     ct_hooks = CTHooks,
	     enable_builtin_hooks = EnableBuiltinHooks} ) ->
    case ct_config:check_config_files(Configs) of
	false ->
	    install([{config,Configs},
		     {event_handler,EvHandlers},
		     {ct_hooks,CTHooks},
		     {enable_builtin_hooks,EnableBuiltinHooks}], LogDir);
	{value,{error,{nofile,File}}} ->
	    {error,{cant_read_config_file,File}};
	{value,{error,{wrong_config,Message}}}->
	    {error,{wrong_config,Message}};
	{value,{error,{callback,Info}}} ->
	    {error,{cant_load_callback_module,Info}}
    end.

script_start3(Opts, Args) ->
    Opts1 = get_start_opt(step,
			  fun(Step) ->
				  Opts#opts{step = Step,
					    cover = undefined}
			  end, Opts, Args),
    case {proplists:get_value(dir, Args),
	  proplists:get_value(suite, Args),
	  groups_and_cases(proplists:get_value(group, Args),
			   proplists:get_value(testcase, Args))} of
	%% flag specified without data
	{_,_,Error={error,_}} ->
	    Error;
	{_,[],_} ->
	    {error,no_suite_specified};
	{[],_,_} ->
	    {error,no_dir_specified};

	{Dirs,undefined,[]} when is_list(Dirs) ->
	    script_start4(Opts#opts{tests = tests(Dirs)}, Args);

	{undefined,Suites,[]} when is_list(Suites) ->
	    Ts = tests([suite_to_test(S) || S <- Suites]),
	    script_start4(Opts1#opts{tests = Ts}, Args);

	{undefined,Suite,GsAndCs} when is_list(Suite) ->
	    case [suite_to_test(S) || S <- Suite] of
		DirMods = [_] ->
		    Ts = tests(DirMods, GsAndCs),
		    script_start4(Opts1#opts{tests = Ts}, Args);
		[_,_|_] ->
		    {error,multiple_suites_and_cases};
		_ ->
		    {error,incorrect_start_options}
	    end;

	{[_,_|_],Suite,[]} when is_list(Suite) ->
	    {error,multiple_dirs_and_suites};

	{[Dir],Suite,GsAndCs} when is_list(Dir), is_list(Suite) ->
	    case [suite_to_test(Dir,S) || S <- Suite] of
		DirMods when GsAndCs == [] ->
		    Ts = tests(DirMods),
		    script_start4(Opts1#opts{tests = Ts}, Args);
		DirMods = [_] when GsAndCs /= [] ->
		    Ts = tests(DirMods, GsAndCs),
		    script_start4(Opts1#opts{tests = Ts}, Args);
		[_,_|_] when GsAndCs /= [] ->
		    {error,multiple_suites_and_cases};
		_ ->
		    {error,incorrect_start_options}
	    end;

	{undefined,undefined,GsAndCs} when GsAndCs /= [] ->
	    {error,incorrect_start_options};

	{undefined,undefined,_} ->
	    if Opts#opts.vts ; Opts#opts.shell ->
		    script_start4(Opts#opts{tests = []}, Args);
	       true ->
		    %% no start options, use default "-dir ./"
		    {ok,Dir} = file:get_cwd(),
		    io:format("ct_run -dir ~ts~n~n", [Dir]),
		    script_start4(Opts#opts{tests = tests([Dir])}, Args)
	    end
    end.

script_start4(#opts{vts = true, config = Config, event_handlers = EvHandlers,
		    tests = Tests, logdir = LogDir, logopts = LogOpts}, _Args) ->
    ConfigFiles =
	lists:foldl(fun({ct_config_plain,CfgFiles}, AllFiles) when
			      is_list(hd(CfgFiles)) ->
			    AllFiles ++ CfgFiles;
		       ({ct_config_plain,CfgFile}, AllFiles) when
			      is_integer(hd(CfgFile)) ->
			    AllFiles ++ [CfgFile];
		       (_, AllFiles) ->
			    AllFiles
		    end, [], Config),
    vts:init_data(ConfigFiles, EvHandlers, ?abs(LogDir), LogOpts, Tests);

script_start4(#opts{label = Label, profile = Profile,
		    shell = true, config = Config,
		    event_handlers = EvHandlers,
		    ct_hooks = CTHooks,
		    logopts = LogOpts,
		    verbosity = Verbosity,
		    enable_builtin_hooks = EnableBuiltinHooks,
		    logdir = LogDir, testspec_files = Specs}, _Args) ->

    %% label - used by ct_logs
    application:set_env(common_test, test_label, Label),

    %% profile - used in ct_util
    application:set_env(common_test, profile, Profile),

    if Config == [] ->
	    ok;
       true ->
	    io:format("\nInstalling: ~p\n\n", [Config])
    end,
    case install([{config,Config},{event_handler,EvHandlers},
		  {ct_hooks, CTHooks},
		  {enable_builtin_hooks,EnableBuiltinHooks}]) of
	ok ->
	    _ = ct_util:start(interactive, LogDir,
			      add_verbosity_defaults(Verbosity)),
	    ct_util:set_testdata({logopts, LogOpts}),
	    log_ts_names(Specs),
	    io:nl(),
	    interactive_mode;
	Error ->
	    Error
    end;

script_start4(#opts{vts = true, cover = Cover}, _) ->
    case Cover of
	undefined ->
	    script_usage();
	_ ->
	    %% Add support later (maybe).
	    io:format("\nCan't run cover in vts mode.\n\n", [])
    end,
    {error,no_cover_in_vts_mode};

script_start4(#opts{shell = true, cover = Cover}, _) ->
    case Cover of
	undefined ->
	    script_usage();
	_ ->
	    %% Add support later (maybe).
	    io:format("\nCan't run cover in interactive mode.\n\n", [])
    end,
    {error,no_cover_in_interactive_mode};

script_start4(Opts = #opts{tests = Tests}, Args) ->
    do_run(Tests, [], Opts, Args).

%%%-----------------------------------------------------------------
%%% @spec script_usage() -> ok
%%% @doc Print usage information for <code>ct_run</code>.
script_usage() ->
    io:format("\nUsage:\n\n"),
    io:format("Run tests from command line:\n\n"
	      "\tct_run -dir TestDir1 TestDir2 .. TestDirN |"
	      "\n\t  [-dir TestDir] -suite Suite1 Suite2 .. SuiteN"
	      "\n\t   [-group Group1 Group2 .. GroupN] [-case Case1 Case2 .. CaseN]"
	      "\n\t [-step [config | keep_inactive]]"
	      "\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t [-userconfig CallbackModule ConfigFile1 .. ConfigFileN]"
	      "\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]"
	      "\n\t [-logdir LogDir]"
	      "\n\t [-logopts LogOpt1 LogOpt2 .. LogOptN]"
	      "\n\t [-verbosity GenVLvl | [CategoryVLvl1 .. CategoryVLvlN]]"
	      "\n\t [-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]"
	      "\n\t [-stylesheet CSSFile]"	     
	      "\n\t [-cover CoverCfgFile]"
	      "\n\t [-cover_stop Bool]"
	      "\n\t [-event_handler EvHandler1 EvHandler2 .. EvHandlerN]"
	      "\n\t [-ct_hooks CTHook1 CTHook2 .. CTHookN]"
	      "\n\t [-include InclDir1 InclDir2 .. InclDirN]"
	      "\n\t [-no_auto_compile]"
	      "\n\t [-abort_if_missing_suites]"
	      "\n\t [-multiply_timetraps N]"
	      "\n\t [-scale_timetraps]"
	      "\n\t [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]"
	      "\n\t [-basic_html]"
	      "\n\t [-no_esc_chars]"
	      "\n\t [-repeat N] |"
	      "\n\t [-duration HHMMSS [-force_stop [skip_rest]]] |"
	      "\n\t [-until [YYMoMoDD]HHMMSS [-force_stop [skip_rest]]]"
	      "\n\t [-exit_status ignore_config]"
	      "\n\t [-help]\n\n"),
    io:format("Run tests using test specification:\n\n"
	      "\tct_run -spec TestSpec1 TestSpec2 .. TestSpecN"
	      "\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]"
	      "\n\t [-logdir LogDir]"
	      "\n\t [-logopts LogOpt1 LogOpt2 .. LogOptN]"
	      "\n\t [-verbosity GenVLvl | [CategoryVLvl1 .. CategoryVLvlN]]"
	      "\n\t [-allow_user_terms]"
	      "\n\t [-join_specs]"
	      "\n\t [-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]"
	      "\n\t [-stylesheet CSSFile]"
	      "\n\t [-cover CoverCfgFile]"
	      "\n\t [-cover_stop Bool]"
	      "\n\t [-event_handler EvHandler1 EvHandler2 .. EvHandlerN]"
	      "\n\t [-ct_hooks CTHook1 CTHook2 .. CTHookN]"
	      "\n\t [-include InclDir1 InclDir2 .. InclDirN]"
	      "\n\t [-no_auto_compile]"
	      "\n\t [-abort_if_missing_suites]"
	      "\n\t [-multiply_timetraps N]"
	      "\n\t [-scale_timetraps]"
	      "\n\t [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]"
	      "\n\t [-basic_html]"
	      "\n\t [-no_esc_chars]"
	      "\n\t [-repeat N] |"
	      "\n\t [-duration HHMMSS [-force_stop [skip_rest]]] |"
	      "\n\t [-until [YYMoMoDD]HHMMSS [-force_stop [skip_rest]]]\n\n"),
    io:format("Refresh the HTML index files:\n\n"
	      "\tct_run -refresh_logs [LogDir]"
	      " [-logdir LogDir] "
	      " [-basic_html]\n\n"),
    io:format("Run CT in interactive mode:\n\n"
	      "\tct_run -shell"
	      "\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]\n\n"),
    io:format("Run tests in web based GUI:\n\n"
	      "\tct_run -vts [-browser Browser]"
	      "\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]"
	      "\n\t [-dir TestDir1 TestDir2 .. TestDirN] |"
	      "\n\t [-suite Suite [-case Case]]"
	      "\n\t [-logopts LogOpt1 LogOpt2 .. LogOptN]"
	      "\n\t [-verbosity GenVLvl | [CategoryVLvl1 .. CategoryVLvlN]]"
	      "\n\t [-include InclDir1 InclDir2 .. InclDirN]"
	      "\n\t [-no_auto_compile]"
	      "\n\t [-abort_if_missing_suites]"
	      "\n\t [-multiply_timetraps N]"
	      "\n\t [-scale_timetraps]"
	      "\n\t [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]"
	      "\n\t [-basic_html]"
	      "\n\t [-no_esc_chars]\n\n").

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:install/1
install(Opts) ->
    install(Opts, ".").

install(Opts, LogDir) ->

    ConfOpts = ct_config:add_default_callback(Opts),

    case application:get_env(common_test, decrypt) of
	{ok,_} ->
	    ok;
	_ ->
	    case lists:keysearch(decrypt, 1, Opts) of
		{value,{_,KeyOrFile}} ->
		    application:set_env(common_test, decrypt, KeyOrFile);
		false ->
		    application:unset_env(common_test, decrypt)
	    end
    end,
    case whereis(ct_util_server) of
	undefined ->
	    VarFile = variables_file_name(LogDir),
	    case file:open(VarFile, [write]) of
		{ok,Fd} ->
		    _ = [io:format(Fd, "~p.\n", [Opt]) || Opt <- ConfOpts],
		    ok = file:close(Fd);
		{error,Reason} ->
		    io:format("CT failed to install configuration data. Please "
			      "verify that the log directory exists and that "
			      "write permission is set.\n\n", []),
		    {error,{VarFile,Reason}}
	    end;
	_ ->
	    io:format("It is not possible to install CT while running "
		      "in interactive mode.\n"
		      "To exit this mode, run ct:stop_interactive().\n"
		      "To enter the interactive mode again, "
		      "run ct:start_interactive()\n\n", []),
	    {error,interactive_mode}
    end.

variables_file_name(Dir) ->
    filename:join(Dir, "variables-"++atom_to_list(node())).

%%%-----------------------------------------------------------------
%%% @spec run_test(Opts) -> Result
%%%   Opts = [tuple()]
%%%   Result = [TestResult] | {error,Reason}
%%%
%%% @doc Start tests from the erlang shell or from an erlang program.
%%% @equiv ct:run_test/1
%%%-----------------------------------------------------------------

run_test(StartOpt) when is_tuple(StartOpt) ->
    run_test([StartOpt]);

run_test(StartOpts) when is_list(StartOpts) ->
    CTPid = spawn(run_test1_fun(StartOpts)),
    Ref = monitor(process, CTPid),
    receive
	{'DOWN',Ref,process,CTPid,{user_error,Error}} ->
		    {error,Error};
	{'DOWN',Ref,process,CTPid,Other} ->
		    Other
    end.

-spec run_test1_fun(_) -> fun(() -> no_return()).

run_test1_fun(StartOpts) ->
    fun() -> run_test1(StartOpts) end.

run_test1(StartOpts) when is_list(StartOpts) ->
    case proplists:get_value(refresh_logs, StartOpts) of
	undefined ->
	    Tracing = start_trace(StartOpts),
	    {ok,Cwd} = file:get_cwd(),
	    io:format("~nCommon Test starting (cwd is ~ts)~n~n", [Cwd]),
	    Res =
		case ct_repeat:loop_test(func, StartOpts) of
		    false ->
			case catch run_test2(StartOpts) of
			    {'EXIT',Reason} ->
				ok = file:set_cwd(Cwd),
				{error,Reason};
			    Result ->
				Result
			end;
		    Result ->
			Result
		end,
	    stop_trace(Tracing),
	    exit(Res);
	RefreshDir ->
            %% log_cleanup - used by ct_logs
            KeepLogs = get_start_opt(keep_logs,
                                     fun ct_logs:parse_keep_logs/1,
                                     all,
                                     StartOpts),
            application:set_env(common_test, keep_logs, KeepLogs),
	    ok = refresh_logs(?abs(RefreshDir)),
	    exit(done)
    end.

run_test2(StartOpts) ->
    %% label
    Label = get_start_opt(label, fun(Lbl) when is_list(Lbl) -> Lbl;
				    (Lbl) when is_atom(Lbl) -> atom_to_list(Lbl)
				 end, StartOpts),
    %% profile
    Profile = get_start_opt(profile, fun(Prof) when is_list(Prof) ->
					     Prof;
					(Prof) when is_atom(Prof) ->
					     atom_to_list(Prof)
				     end, StartOpts),
    %% logdir
    LogDir = get_start_opt(logdir, fun(LD) when is_list(LD) -> LD end,
			   StartOpts),
    %% logopts
    LogOpts = get_start_opt(logopts, value, [], StartOpts),

    %% verbosity
    Verbosity =
	get_start_opt(verbosity,
		      fun(VLvls) when is_list(VLvls) ->
			      lists:map(fun(VLvl = {_Cat,_Lvl}) ->
						VLvl;
					   (Lvl) ->
						{'$unspecified',Lvl}
					end, VLvls);
			 (VLvl) when is_integer(VLvl) ->
			      [{'$unspecified',VLvl}]
		      end, [], StartOpts),

    %% config & userconfig
    CfgFiles = ct_config:get_config_file_list(StartOpts),

    %% event handlers
    EvHandlers =
	case proplists:get_value(event_handler, StartOpts) of
	    undefined ->
		[];
	    H when is_atom(H) ->
		[{H,[]}];
	    H ->
		Hs =
		    if is_tuple(H) -> [H];
		       is_list(H) -> H;
		       true -> []
		    end,
		lists:flatten(
		  lists:map(fun(EH) when is_atom(EH) ->
				    {EH,[]};
			       ({HL,Args}) when is_list(HL) ->
				    [{EH,Args} || EH <- HL];
			       ({EH,Args}) when is_atom(EH) ->
				    {EH,Args};
			       (_) ->
				    []
			    end, Hs))
	end,

    %% CT Hooks
    CTHooks = get_start_opt(ct_hooks, value, [], StartOpts),
    EnableBuiltinHooks = get_start_opt(enable_builtin_hooks,
				       fun(EBH) when EBH == true;
						     EBH == false ->
					       EBH
				       end, undefined, StartOpts),

    %% silent connections
    SilentConns = get_start_opt(silent_connections,
				fun(all) -> [all];
				   (Conns) -> Conns
				end, [], StartOpts),
    %% stylesheet
    Stylesheet = get_start_opt(stylesheet,
			       fun(SS) -> ?abs(SS) end,
			       StartOpts),
    %% code coverage
    Cover = get_start_opt(cover,
			  fun(CoverFile) -> ?abs(CoverFile) end, StartOpts),
    CoverStop = get_start_opt(cover_stop, value, StartOpts),

    %% timetrap manipulation
    MultiplyTT = get_start_opt(multiply_timetraps, value, StartOpts),
    ScaleTT = get_start_opt(scale_timetraps, value, StartOpts),

    %% create unique priv dir names
    CreatePrivDir = get_start_opt(create_priv_dir, value, StartOpts),

    %% auto compile & include files
    {AutoCompile,Include} =
	case proplists:get_value(auto_compile, StartOpts) of
	    undefined ->
		application:set_env(common_test, auto_compile, true),		
		InclDirs =
		    case proplists:get_value(include, StartOpts) of
			undefined ->
			    [];
			Incls when is_list(hd(Incls)) ->
			    [filename:absname(IDir) || IDir <- Incls];
			Incl when is_list(Incl) ->
			    [filename:absname(Incl)]
		    end,
		case os:getenv("CT_INCLUDE_PATH") of
		    false ->
			application:set_env(common_test, include, InclDirs),
			{undefined,InclDirs};
		    CtInclPath ->
			InclDirs1 = string:tokens(CtInclPath, [$:,$ ,$,]),
			AllInclDirs = InclDirs1++InclDirs,
			application:set_env(common_test, include, AllInclDirs),
			{undefined,AllInclDirs}
		end;
	    ACBool ->
		application:set_env(common_test, auto_compile, ACBool),
		{ACBool,[]}
	end,

    %% abort test run if some suites can't be compiled
    AbortIfMissing = get_start_opt(abort_if_missing_suites, value, false,
				   StartOpts),

    %% decrypt config file
    case proplists:get_value(decrypt, StartOpts) of
	undefined ->
	    application:unset_env(common_test, decrypt);
	Key={key,_} ->
	    application:set_env(common_test, decrypt, Key);
	{file,KeyFile} ->
	    application:set_env(common_test, decrypt, {file,?abs(KeyFile)})
    end,

    %% basic html - used by ct_logs
    BasicHtml =
	case proplists:get_value(basic_html, StartOpts) of
	    undefined ->
		application:set_env(common_test, basic_html, false),
		undefined;
	    BasicHtmlBool ->
		application:set_env(common_test, basic_html, BasicHtmlBool),
		BasicHtmlBool		
    end,
    %% esc_chars - used by ct_logs
    EscChars =
	case proplists:get_value(esc_chars, StartOpts) of
	    undefined ->
		application:set_env(common_test, esc_chars, true),
		undefined;
	    EscCharsBool ->
		application:set_env(common_test, esc_chars, EscCharsBool),
		EscCharsBool		
    end,
    %% disable_log_cache - used by ct_logs
    case proplists:get_value(disable_log_cache, StartOpts) of
	undefined ->
	    application:set_env(common_test, disable_log_cache, false);
	DisableCacheBool ->
	    application:set_env(common_test, disable_log_cache, DisableCacheBool)
    end,
    %% log_cleanup - used by ct_logs
    KeepLogs = get_start_opt(keep_logs,
                             fun ct_logs:parse_keep_logs/1,
                             all,
                             StartOpts),
    application:set_env(common_test, keep_logs, KeepLogs),

    %% stepped execution
    Step = get_start_opt(step, value, StartOpts),

    Opts = #opts{label = Label, profile = Profile,
		 cover = Cover, cover_stop = CoverStop,
		 step = Step, logdir = LogDir,
		 logopts = LogOpts, basic_html = BasicHtml,
		 esc_chars = EscChars,
		 config = CfgFiles,
		 verbosity = Verbosity,
		 event_handlers = EvHandlers,
		 ct_hooks = CTHooks,
		 enable_builtin_hooks = EnableBuiltinHooks,
		 auto_compile = AutoCompile,
		 abort_if_missing_suites = AbortIfMissing,
		 include = Include,
		 silent_connections = SilentConns,
		 stylesheet = Stylesheet,
		 multiply_timetraps = MultiplyTT,
		 scale_timetraps = ScaleTT,
		 create_priv_dir = CreatePrivDir,
		 starter = ct},

    %% test specification
    case proplists:get_value(spec, StartOpts) of
	undefined ->
	    case lists:keysearch(prepared_tests, 1, StartOpts) of
		{value,{_,{Run,Skip},Specs}} ->	% use prepared tests
		    run_prepared(Run, Skip, Opts#opts{testspec_files = Specs},
				 StartOpts);
		false ->
		    run_dir(Opts, StartOpts)
	    end;
	Specs ->
	    Relaxed = get_start_opt(allow_user_terms, value, false, StartOpts),
	    %% using testspec(s) as input for test
	    run_spec_file(Relaxed, Opts#opts{testspec_files = Specs}, StartOpts)
    end.

run_spec_file(Relaxed,
	      Opts = #opts{testspec_files = Specs},
	      StartOpts) ->
    Specs1 = case Specs of
		 [X|_] when is_integer(X) -> [Specs];
		 _ -> Specs
	     end,
    AbsSpecs = lists:map(fun(SF) -> ?abs(SF) end, Specs1),
    AbsSpecs1 = get_start_opt(join_specs, [AbsSpecs], AbsSpecs, StartOpts),
    case catch ct_testspec:collect_tests_from_file(AbsSpecs1, Relaxed) of
	{Error,CTReason} when Error == error ; Error == 'EXIT' ->
	    StackTrace = erlang:get_stacktrace(),
	    exit({error,{invalid_testspec,{CTReason,StackTrace}}});
	TestSpecData ->
	    run_all_specs(TestSpecData, Opts, StartOpts, [])
    end.

run_all_specs([], _, _, TotResult) ->
    TotResult1 = lists:reverse(TotResult),
    case lists:keysearch('EXIT', 1, TotResult1) of
	{value,{_,_,ExitReason}} ->
	    exit(ExitReason);
	false ->
	    case lists:keysearch(error, 1, TotResult1) of
		{value,Error} ->
		    Error;
		false ->
		    lists:foldl(fun({Ok,Fail,{UserSkip,AutoSkip}},
				    {Ok1,Fail1,{UserSkip1,AutoSkip1}}) ->
					{Ok1+Ok,Fail1+Fail,
					 {UserSkip1+UserSkip,
					  AutoSkip1+AutoSkip}}
				end, {0,0,{0,0}}, TotResult1)
	    end
    end;

run_all_specs([{Specs,TS} | TSs], Opts, StartOpts, TotResult) ->
    Combined = #opts{config = TSConfig} = combine_test_opts(TS, Specs, Opts),
    AllConfig = merge_vals([Opts#opts.config, TSConfig]),
    try run_one_spec(TS, 
		     Combined#opts{config = AllConfig,
				   current_testspec=TS},
		     StartOpts) of
	Result ->
	    run_all_specs(TSs, Opts, StartOpts, [Result | TotResult])		
    catch
	_ : Reason ->
	    run_all_specs(TSs, Opts, StartOpts, [{error,Reason} | TotResult])
    end.

run_one_spec(TS, CombinedOpts, StartOpts) ->
    #opts{logdir = Logdir, config = Config} = CombinedOpts,
    case check_and_install_configfiles(Config, Logdir, CombinedOpts) of
	ok ->
	    {Run,Skip} = ct_testspec:prepare_tests(TS, node()),
	    reformat_result(catch do_run(Run, Skip, CombinedOpts, StartOpts));
	Error ->
	    Error
    end.

run_prepared(Run, Skip, Opts = #opts{logdir = LogDir,
				     config = CfgFiles},
	     StartOpts) ->
    LogDir1 = which(logdir, LogDir),
    case check_and_install_configfiles(CfgFiles, LogDir1, Opts) of
	ok ->
	    reformat_result(catch do_run(Run, Skip, Opts#opts{logdir = LogDir1},
					 StartOpts));
	{error,_Reason} = Error ->
	    exit(Error)
    end.

check_config_file(Callback, File)->
    case code:is_loaded(Callback) of
	false ->
	    case code:load_file(Callback) of
		{module,_} -> ok;
		{error,Why} -> exit({error,{cant_load_callback_module,Why}})
	    end;
	_ ->
	    ok
    end,
    case Callback:check_parameter(File) of
	{ok,{file,File}}->
	    ?abs(File);
	{ok,{config,_}}->
	    File;
	{error,{wrong_config,Message}}->
	    exit({error,{wrong_config,{Callback,Message}}});
	{error,{nofile,File}}->
	    exit({error,{no_such_file,?abs(File)}})
    end.

run_dir(Opts = #opts{logdir = LogDir,
		     config = CfgFiles,
		     event_handlers = EvHandlers,
		     ct_hooks = CTHook,
		     enable_builtin_hooks = EnableBuiltinHooks},
	StartOpts) ->
    LogDir1 = which(logdir, LogDir),
    Opts1 = Opts#opts{logdir = LogDir1},
    AbsCfgFiles =
	lists:map(fun({Callback,FileList})->
			  case code:is_loaded(Callback) of
			      {file,_Path}->
				  ok;
			      false ->
				  case code:load_file(Callback) of
				      {module,Callback}->
					  ok;
				      {error,_}->
					  exit({error,{no_such_module,
						       Callback}})
				  end
			  end,
			  {Callback,
			   lists:map(fun(File)->
					     check_config_file(Callback, File)
				     end, FileList)}
		  end, CfgFiles),
    case install([{config,AbsCfgFiles},
		  {event_handler,EvHandlers},
		  {ct_hooks, CTHook},
		  {enable_builtin_hooks,EnableBuiltinHooks}], LogDir1) of
	ok -> ok;
	{error,_IReason} = IError -> exit(IError)
    end,
    case {proplists:get_value(dir, StartOpts),
	  proplists:get_value(suite, StartOpts),
	  groups_and_cases(proplists:get_value(group, StartOpts),
			   proplists:get_value(testcase, StartOpts))} of
	%% flag specified without data
	{_,_,Error={error,_}} ->
	    Error;
	{_,[],_} ->
	    {error,no_suite_specified};
	{[],_,_} ->
	    {error,no_dir_specified};

	{Dirs=[Hd|_],undefined,[]} when is_list(Dirs), not is_integer(Hd) ->
	    Dirs1 = [if is_atom(D) -> atom_to_list(D);
			true -> D end || D <- Dirs],
	    reformat_result(catch do_run(tests(Dirs1), [], Opts1, StartOpts));

	{Dir=[Hd|_],undefined,[]} when is_list(Dir) and is_integer(Hd) ->
	    reformat_result(catch do_run(tests(Dir), [], Opts1, StartOpts));

	{Dir,undefined,[]} when is_atom(Dir) and (Dir /= undefined) ->
	    reformat_result(catch do_run(tests(atom_to_list(Dir)),
					 [], Opts1, StartOpts));

	{undefined,Suites=[Hd|_],[]} when not is_integer(Hd) ->
	    Suites1 = [suite_to_test(S) || S <- Suites],
	    reformat_result(catch do_run(tests(Suites1), [], Opts1, StartOpts));

	{undefined,Suite,[]} when is_atom(Suite) and
				  (Suite /= undefined) ->
	    {Dir,Mod} = suite_to_test(Suite),
	    reformat_result(catch do_run(tests(Dir, Mod), [], Opts1, StartOpts));

	{undefined,Suite,GsAndCs} when is_atom(Suite) and
				       (Suite /= undefined) ->
	    {Dir,Mod} = suite_to_test(Suite),
	    reformat_result(catch do_run(tests(Dir, Mod, GsAndCs),
					 [], Opts1, StartOpts));

	{undefined,[Hd,_|_],_GsAndCs} when not is_integer(Hd) ->
	    exit({error,multiple_suites_and_cases});

	{undefined,Suite=[Hd|Tl],GsAndCs} when is_integer(Hd) ;
					       (is_list(Hd) and	(Tl == [])) ;
					       (is_atom(Hd) and	(Tl == [])) ->
	    {Dir,Mod} = suite_to_test(Suite),
	    reformat_result(catch do_run(tests(Dir, Mod, GsAndCs),
					 [], Opts1, StartOpts));

	{[Hd,_|_],_Suites,[]} when is_list(Hd) ; not is_integer(Hd) ->
	    exit({error,multiple_dirs_and_suites});

	{undefined,undefined,GsAndCs} when GsAndCs /= [] ->
	    exit({error,incorrect_start_options});

	{Dir,Suite,GsAndCs} when is_integer(hd(Dir)) ;
				 (is_atom(Dir) and (Dir /= undefined)) ;
				 ((length(Dir) == 1) and is_atom(hd(Dir))) ;
				 ((length(Dir) == 1) and is_list(hd(Dir))) ->
	    Dir1 = if is_atom(Dir) -> atom_to_list(Dir);
		      true -> Dir end,
	    if Suite == undefined ->
		  exit({error,incorrect_start_options});

	       is_integer(hd(Suite)) ;
	       (is_atom(Suite) and (Suite /= undefined)) ;
	       ((length(Suite) == 1) and is_atom(hd(Suite))) ;
	       ((length(Suite) == 1) and is_list(hd(Suite))) ->
		    {Dir2,Mod} = suite_to_test(Dir1, Suite),
		    case GsAndCs of
			[] ->
			    reformat_result(catch do_run(tests(Dir2, Mod),
							 [], Opts1, StartOpts));
			_ ->
			    reformat_result(catch do_run(tests(Dir2, Mod,
							       GsAndCs),
							 [], Opts1, StartOpts))
		    end;

		is_list(Suite) ->		% multiple suites
		    case [suite_to_test(Dir1, S) || S <- Suite] of
			[_,_|_] when GsAndCs /= [] ->
			    exit({error,multiple_suites_and_cases});
			[{Dir2,Mod}] when GsAndCs /= [] ->
			    reformat_result(catch do_run(tests(Dir2, Mod,
							       GsAndCs),
							 [], Opts1, StartOpts));
			DirMods ->
			    reformat_result(catch do_run(tests(DirMods),
							 [], Opts1, StartOpts))
		    end
	    end;

	{undefined,undefined,[]} ->
	    {ok,Dir} = file:get_cwd(),
	    %% No start options, use default {dir,CWD}
	    reformat_result(catch do_run(tests(Dir), [], Opts1, StartOpts));

	{Dir,Suite,GsAndCs} ->
	    exit({error,{incorrect_start_options,{Dir,Suite,GsAndCs}}})
    end.

%%%-----------------------------------------------------------------
%%% @spec run_testspec(TestSpec) -> Result
%%%   TestSpec = [term()]
%%%
%%% @doc Run test specified by <code>TestSpec</code>. The terms are
%%% the same as those used in test specification files.
%%% @equiv ct:run_testspec/1
%%%-----------------------------------------------------------------
run_testspec(TestSpec) ->
    CTPid = spawn(run_testspec1_fun(TestSpec)),
    Ref = monitor(process, CTPid),
    receive
	{'DOWN',Ref,process,CTPid,{user_error,Error}} ->
		    Error;
	{'DOWN',Ref,process,CTPid,Other} ->
		    Other
    end.

-spec run_testspec1_fun(_) -> fun(() -> no_return()).

run_testspec1_fun(TestSpec) ->
    fun() -> run_testspec1(TestSpec) end.

run_testspec1(TestSpec) ->
    {ok,Cwd} = file:get_cwd(),
    io:format("~nCommon Test starting (cwd is ~ts)~n~n", [Cwd]),
    case catch run_testspec2(TestSpec) of
	{'EXIT',Reason} ->
	    ok = file:set_cwd(Cwd),
	    exit({error,Reason});
	Result ->
	    exit(Result)
    end.

run_testspec2(File) when is_list(File), is_integer(hd(File)) ->
    case file:read_file_info(File) of
	{ok,_} ->
	    exit("Bad argument, "
		 "use ct:run_test([{spec," ++ File ++ "}])");
	_ ->
	    exit("Bad argument, list of tuples expected, "
		 "use ct:run_test/1 for test specification files")
    end;

run_testspec2(TestSpec) ->
    case catch ct_testspec:collect_tests_from_list(TestSpec, false) of
	{E,CTReason}  when E == error ; E == 'EXIT' ->
	    exit({error,CTReason});
	TS ->
	    Opts = get_data_for_node(TS, node()),

	    AllInclude =
		case os:getenv("CT_INCLUDE_PATH") of
		    false ->
			Opts#opts.include;
		    CtInclPath ->
			EnvInclude = string:tokens(CtInclPath, [$:,$ ,$,]),
			EnvInclude++Opts#opts.include
		end,
	    application:set_env(common_test, include, AllInclude),

	    LogDir1 = which(logdir,Opts#opts.logdir),
	    case check_and_install_configfiles(
		   Opts#opts.config, LogDir1, Opts) of
		ok ->
		    Opts1 = Opts#opts{testspec_files = [],
				      logdir = LogDir1,
				      include = AllInclude},
		    {Run,Skip} = ct_testspec:prepare_tests(TS, node()),
		    reformat_result(catch do_run(Run, Skip, Opts1, []));
		{error,_GCFReason} = GCFError ->
		    exit(GCFError)
	    end
    end.

get_data_for_node(#testspec{label = Labels,
			    profile = Profiles,
			    logdir = LogDirs,
			    logopts = LogOptsList,
			    basic_html = BHs,
			    esc_chars = EscChs,
			    stylesheet = SSs,
			    verbosity = VLvls,
			    silent_connections = SilentConnsList,
			    cover = CoverFs,
			    cover_stop = CoverStops,
			    config = Cfgs,
			    userconfig = UsrCfgs,
			    event_handler = EvHs,
			    ct_hooks = CTHooks,
			    enable_builtin_hooks = EnableBuiltinHooks,
			    auto_compile = ACs,
			    abort_if_missing_suites = AiMSs,
			    include = Incl,
			    multiply_timetraps = MTs,
			    scale_timetraps = STs,
			    create_priv_dir = PDs}, Node) ->
    Label = proplists:get_value(Node, Labels),
    Profile = proplists:get_value(Node, Profiles),
    LogDir = case proplists:get_value(Node, LogDirs) of
		 undefined -> ".";
		 Dir -> Dir
	     end,
    LogOpts = case proplists:get_value(Node, LogOptsList) of
		  undefined -> [];
		  LOs -> LOs
	      end,
    BasicHtml = proplists:get_value(Node, BHs),
    EscChars = proplists:get_value(Node, EscChs),
    Stylesheet = proplists:get_value(Node, SSs),
    Verbosity = case proplists:get_value(Node, VLvls) of
		    undefined -> [];
		    Lvls -> Lvls
		end,
    SilentConns = case proplists:get_value(Node, SilentConnsList) of
		      undefined -> [];
		      SCs -> SCs
		  end,
    Cover = proplists:get_value(Node, CoverFs),
    CoverStop = proplists:get_value(Node, CoverStops),
    MT = proplists:get_value(Node, MTs),
    ST = proplists:get_value(Node, STs),
    CreatePrivDir = proplists:get_value(Node, PDs),
    ConfigFiles = [{?ct_config_txt,F} || {N,F} <- Cfgs, N==Node] ++
	[CBF || {N,CBF} <- UsrCfgs, N==Node],
    EvHandlers =  [{H,A} || {N,H,A} <- EvHs, N==Node],
    FiltCTHooks = [Hook || {N,Hook} <- CTHooks, N==Node],
    AutoCompile = proplists:get_value(Node, ACs),
    AbortIfMissing = proplists:get_value(Node, AiMSs),
    Include =  [I || {N,I} <- Incl, N==Node],
    #opts{label = Label,
	  profile = Profile,
	  logdir = LogDir,
	  logopts = LogOpts,
	  basic_html = BasicHtml,
	  esc_chars = EscChars,
	  stylesheet = Stylesheet,
	  verbosity = Verbosity,
	  silent_connections = SilentConns,
	  cover = Cover,
	  cover_stop = CoverStop,
	  config = ConfigFiles,
	  event_handlers = EvHandlers,
	  ct_hooks = FiltCTHooks,
	  enable_builtin_hooks = EnableBuiltinHooks,
	  auto_compile = AutoCompile,
	  abort_if_missing_suites = AbortIfMissing,
	  include = Include,
	  multiply_timetraps = MT,
	  scale_timetraps = ST,
	  create_priv_dir = CreatePrivDir}.

refresh_logs(LogDir) ->
    {ok,Cwd} = file:get_cwd(),
    case file:set_cwd(LogDir) of
	E = {error,_Reason} ->
	    E;
	_ ->
	    case catch ct_logs:make_all_suites_index(refresh) of
		{'EXIT',ASReason} ->
		    ok = file:set_cwd(Cwd),
		    {error,{all_suites_index,ASReason}};
		_ ->
		    case catch ct_logs:make_all_runs_index(refresh) of
			{'EXIT',ARReason} ->
			    ok = file:set_cwd(Cwd),
			    {error,{all_runs_index,ARReason}};
			_ ->
			    ok = file:set_cwd(Cwd),
			    io:format("Logs in ~ts refreshed!~n",[LogDir]),
			    ok
		    end
	    end
    end.

which(logdir, undefined) ->
    ".";
which(logdir, Dir) ->
    Dir.

choose_val(undefined, V1) ->
    V1;
choose_val(V0, _V1) ->
    V0.

merge_vals(Vs) ->
    lists:append(Vs).

merge_keyvals(Vs) ->
    make_unique(lists:append(Vs)).

make_unique([Elem={Key,_} | Elems]) ->
    [Elem | make_unique(proplists:delete(Key, Elems))];
make_unique([]) ->
    [].

listify([C|_]=Str) when is_integer(C) -> [Str];
listify(L) when is_list(L) -> L;
listify(E) -> [E].

delistify([E]) -> E;
delistify(E)   -> E.


%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:run/3
run(TestDir, Suite, Cases) ->
    case install([]) of
	ok ->
	    reformat_result(catch do_run(tests(TestDir, Suite, Cases), []));
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:run/2
run(TestDir, Suite) when is_list(TestDir), is_integer(hd(TestDir)) ->
    case install([]) of
	ok ->
	    reformat_result(catch do_run(tests(TestDir, Suite), []));
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:run/1
run(TestDirs) ->
    case install([]) of
	ok ->
	    reformat_result(catch do_run(tests(TestDirs), []));
	Error ->
	    Error
    end.

reformat_result({'EXIT',{user_error,Reason}}) ->
    {error,Reason};
reformat_result({user_error,Reason}) ->
    {error,Reason};
reformat_result(Result) ->
    Result.

suite_to_test(Suite) when is_atom(Suite) ->
    suite_to_test(atom_to_list(Suite));

suite_to_test(Suite) when is_list(Suite) ->
    {filename:dirname(Suite),
     list_to_atom(filename:rootname(filename:basename(Suite)))}.

suite_to_test(Dir, Suite) when is_atom(Suite) ->
    suite_to_test(Dir, atom_to_list(Suite));

suite_to_test(Dir, Suite) when is_list(Suite) ->
    case filename:dirname(Suite) of
	"." ->
	    {Dir,list_to_atom(filename:rootname(Suite))};
	DirName ->				% ignore Dir
	    File = filename:basename(Suite),
	    {DirName,list_to_atom(filename:rootname(File))}
    end.

groups_and_cases(Gs, Cs) when ((Gs == undefined) or (Gs == [])) and
			      ((Cs == undefined) or (Cs == [])) ->
    [];
groups_and_cases(Gs, Cs) when Gs == undefined ; Gs == [] ->
    if (Cs == all) or (Cs == [all]) or (Cs == ["all"]) -> all;
       true -> [ensure_atom(C) || C <- listify(Cs)]
    end;
groups_and_cases(GOrGs, Cs) when (is_atom(GOrGs) orelse
				  (is_list(GOrGs) andalso
				   (is_atom(hd(GOrGs)) orelse
				    (is_list(hd(GOrGs)) andalso
				     is_atom(hd(hd(GOrGs))))))) ->
    if (Cs == undefined) or (Cs == []) or
       (Cs == all) or (Cs == [all]) or (Cs == ["all"]) ->
	    [{GOrGs,all}];
       true ->
	    [{GOrGs,[ensure_atom(C) || C <- listify(Cs)]}]
    end;
groups_and_cases(Gs, Cs) when is_integer(hd(hd(Gs))) ->
    %% if list of strings, this comes from 'ct_run -group G1 G2 ...' and
    %% we need to parse the strings
    Gs1 = 
	if (Gs == [all]) or (Gs == ["all"]) ->
		all;
	   true ->
		lists:map(fun(G) ->
				  {ok,Ts,_} = erl_scan:string(G++"."),
				  {ok,Term} = erl_parse:parse_term(Ts),
				  Term
			  end, Gs)
	end,
    groups_and_cases(Gs1, Cs);
groups_and_cases(Gs, Cs) ->
    {error,{incorrect_group_or_case_option,Gs,Cs}}.

tests(TestDir, Suites, []) when is_list(TestDir), is_integer(hd(TestDir)) ->
    [{?testdir(TestDir,Suites),ensure_atom(Suites),all}];
tests(TestDir, Suite, Cases) when is_list(TestDir), is_integer(hd(TestDir)) ->
    [{?testdir(TestDir,Suite),ensure_atom(Suite),Cases}];
tests([TestDir], Suite, Cases) when is_list(TestDir), is_integer(hd(TestDir)) ->
    [{?testdir(TestDir,Suite),ensure_atom(Suite),Cases}].
tests([{Dir,Suite}],Cases) ->
    [{?testdir(Dir,Suite),ensure_atom(Suite),Cases}];
tests(TestDir, Suite) when is_list(TestDir), is_integer(hd(TestDir)) ->
    tests(TestDir, ensure_atom(Suite), all);
tests([TestDir], Suite) when is_list(TestDir), is_integer(hd(TestDir)) ->
     tests(TestDir, ensure_atom(Suite), all).
tests(DirSuites) when is_list(DirSuites), is_tuple(hd(DirSuites)) ->
    [{?testdir(Dir,Suite),ensure_atom(Suite),all} || {Dir,Suite} <- DirSuites];
tests(TestDir) when is_list(TestDir), is_integer(hd(TestDir)) ->
    tests([TestDir]);
tests(TestDirs) when is_list(TestDirs), is_list(hd(TestDirs)) ->
    [{?testdir(TestDir,all),all,all} || TestDir <- TestDirs].

do_run(Tests, Misc) when is_list(Misc) ->
    do_run(Tests, Misc, ".", []).

do_run(Tests, Misc, LogDir, LogOpts) when is_list(Misc),
					  is_list(LogDir),
					  is_list(LogOpts) ->
    Opts =
	case proplists:get_value(step, Misc) of
	    undefined ->
		#opts{};
	    StepOpts ->
		#opts{step = StepOpts}
	end,
    do_run(Tests, [], Opts#opts{logdir = LogDir}, []);

do_run(Tests, Skip, Opts, Args) when is_record(Opts, opts) ->
    #opts{label = Label, profile = Profile,
	  verbosity = VLvls} = Opts,
    %% label - used by ct_logs
    TestLabel =
	if Label == undefined -> undefined;
	   is_atom(Label)     -> atom_to_list(Label);
	   is_list(Label)     -> Label;
	   true               -> undefined
	end,
    application:set_env(common_test, test_label, TestLabel),

    %% profile - used in ct_util
    TestProfile =
	if Profile == undefined -> undefined;
	   is_atom(Profile)     -> atom_to_list(Profile);
	   is_list(Profile)     -> Profile;
	   true                 -> undefined
	end,
    application:set_env(common_test, profile, TestProfile),

    case code:which(test_server) of
	non_existing ->
	    {error,no_path_to_test_server};
	_ ->
	    %% This env variable is used by test_server to determine
	    %% which framework it runs under.
	    case os:getenv("TEST_SERVER_FRAMEWORK") of
		false ->
		    os:putenv("TEST_SERVER_FRAMEWORK", "ct_framework"),
		    os:putenv("TEST_SERVER_FRAMEWORK_NAME", "common_test");
		"ct_framework" ->
		    ok;
		Other ->
		    erlang:display(
		      list_to_atom(
			"Note: TEST_SERVER_FRAMEWORK = " ++ Other))
	    end,
	    Verbosity = add_verbosity_defaults(VLvls),
	    case ct_util:start(Opts#opts.logdir, Verbosity) of
		{error,interactive_mode} ->
		    io:format("CT is started in interactive mode. "
			      "To exit this mode, "
			      "run ct:stop_interactive().\n"
			      "To enter the interactive mode again, "
			      "run ct:start_interactive()\n\n",[]),
		    {error,interactive_mode};
		_Pid ->
		    ct_util:set_testdata({starter,Opts#opts.starter}),
		    compile_and_run(Tests, Skip,
                                    Opts#opts{verbosity=Verbosity}, Args)
	    end
    end.

compile_and_run(Tests, Skip, Opts, Args) ->
    %% save stylesheet info
    ct_util:set_testdata({stylesheet,Opts#opts.stylesheet}),
    %% save logopts
    ct_util:set_testdata({logopts,Opts#opts.logopts}),
    %% save info about current testspec (testspec record or undefined)
    ct_util:set_testdata({testspec,Opts#opts.current_testspec}),

    %% enable silent connections
    case Opts#opts.silent_connections of
	[] ->
	    ok;
	Conns ->
	    case lists:member(all, Conns) of
		true ->
		    Conns1 = ct_util:override_silence_all_connections(),
		    ct_logs:log("Silent connections", "~p", [Conns1]);
		false ->
		    ct_util:override_silence_connections(Conns),
		    ct_logs:log("Silent connections", "~p", [Conns])
	    end
    end,
    log_ts_names(Opts#opts.testspec_files),
    TestSuites = suite_tuples(Tests),
    
    {_TestSuites1,SuiteMakeErrors,AllMakeErrors} =
	case application:get_env(common_test, auto_compile) of
	    {ok,false} ->
		{TestSuites1,SuitesNotFound} =
		    verify_suites(TestSuites),
		{TestSuites1,SuitesNotFound,SuitesNotFound};
	    _ ->
		{SuiteErrs,HelpErrs} = auto_compile(TestSuites),
		{TestSuites,SuiteErrs,SuiteErrs++HelpErrs}
	end,

    case continue(AllMakeErrors, Opts#opts.abort_if_missing_suites) of
	true ->
	    SavedErrors = save_make_errors(SuiteMakeErrors),
	    ct_repeat:log_loop_info(Args),
	    
	    try final_tests(Tests,Skip,SavedErrors) of
		{Tests1,Skip1} ->	    
		    ReleaseSh = proplists:get_value(release_shell, Args),
		    ct_util:set_testdata({release_shell,ReleaseSh}),
		    TestResult = 
			possibly_spawn(ReleaseSh == true, Tests1, Skip1, Opts),
		    case TestResult of
			{Ok,Errors,Skipped} ->
			    NoOfMakeErrors =
				lists:foldl(fun({_,BadMods}, X) ->
						    X + length(BadMods)
					    end, 0, SuiteMakeErrors),
			    {Ok,Errors+NoOfMakeErrors,Skipped};
			ErrorResult ->
			    ErrorResult
		    end
	    catch
		_:BadFormat ->
		    {error,BadFormat}
	    end;
	false ->
	    io:nl(),
	    ct_util:stop(clean),
	    BadMods =
		lists:foldr(
		  fun({{_,_},Ms}, Acc) ->
			  Ms ++ lists:foldl(
				  fun(M, Acc1) ->
					  lists:delete(M, Acc1)
				  end, Acc, Ms)
		  end, [], AllMakeErrors),
	    {error,{make_failed,BadMods}}
    end.

%% keep the shell as the top controlling process
possibly_spawn(false, Tests, Skip, Opts) ->	
    TestResult = (catch do_run_test(Tests, Skip, Opts)),
    case TestResult of
	{EType,_} = Error when EType == user_error;
			       EType == error ->
	    ct_util:stop(clean),
	    exit(Error);
	_ ->
	    ct_util:stop(normal),
	    TestResult
    end;

%% we must return control to the shell now, so we spawn
%% a test supervisor process to keep an eye on the test run 
possibly_spawn(true, Tests, Skip, Opts) ->
    CTUtilSrv = whereis(ct_util_server),
    Supervisor = 
	fun() ->
		process_flag(trap_exit, true),
		link(CTUtilSrv),
		TestRun =
		    fun() ->
			    TestResult = (catch do_run_test(Tests, Skip, Opts)),
			    case TestResult of
				{EType,_} = Error when EType == user_error;
						       EType == error ->
				    ct_util:stop(clean),
				    exit(Error);
				_ ->
				    ct_util:stop(normal),
				    exit({ok,TestResult})
			    end
		    end,
		TestRunPid = spawn_link(TestRun),
		receive
		    {'EXIT',TestRunPid,{ok,TestResult}} ->
			io:format(user, "~nCommon Test returned ~p~n~n",
				  [TestResult]);
		    {'EXIT',TestRunPid,Error} ->
			exit(Error)				
		end
	end,
    unlink(CTUtilSrv),
    SupPid = spawn(Supervisor),
    io:format(user, "~nTest control handed over to process ~w~n~n",
	      [SupPid]),
    SupPid.

%% attempt to compile the modules specified in TestSuites
auto_compile(TestSuites) ->
    io:format("~nCommon Test: Running make in test directories...~n"),
    UserInclude =
	case application:get_env(common_test, include) of
	    {ok,UserInclDirs} when length(UserInclDirs) > 0 ->
		io:format("Including the following directories:~n"),
		[begin io:format("~p~n",[UserInclDir]), {i,UserInclDir} end ||
		 UserInclDir <- UserInclDirs];
	    _ ->
		[]
	end,
    SuiteMakeErrors =
	lists:flatmap(fun({TestDir,Suite} = TS) ->
			      case run_make(suites, TestDir, 
					    Suite, UserInclude) of
				  {error,{make_failed,Bad}} ->
				      [{TS,Bad}];
				  {error,_} ->
				      [{TS,[filename:join(TestDir,
							  "*_SUITE")]}];
				  _ ->
				      []
			      end
		      end, TestSuites),

    %% try to compile other modules than SUITEs in the test directories
    {_,HelpMakeErrors} =
	lists:foldl(
	  fun({Dir,Suite}, {Done,Failed}) ->
		  case lists:member(Dir, Done) of
		      false ->
			  Failed1 =
			      case run_make(helpmods, Dir, Suite, UserInclude) of
				  {error,{make_failed,BadMods}} ->
				      [{{Dir,all},BadMods}|Failed];
				  {error,_} ->
				      [{{Dir,all},[Dir]}|Failed];
				  _ ->
				      Failed
			      end,
			  {[Dir|Done],Failed1};
		      true ->		    % already visited
			  {Done,Failed}
		  end
	  end, {[],[]}, TestSuites),
    {SuiteMakeErrors,lists:reverse(HelpMakeErrors)}.

%% verify that specified test suites exist (if auto compile is disabled)
verify_suites(TestSuites) ->
    io:nl(),
    Verify =
	fun({Dir,Suite}=DS,{Found,NotFound}) ->
		case locate_test_dir(Dir, Suite) of
		    {ok,TestDir} ->
			if Suite == all ->
				{[DS|Found],NotFound};
			   true ->
				Beam = filename:join(TestDir,
						     atom_to_list(Suite)++
							 ".beam"),
				case filelib:is_regular(Beam) of
				    true  ->
					{[DS|Found],NotFound};
				    false ->
					case code:is_loaded(Suite) of
					    {file,SuiteFile} ->
						%% test suite is already
						%% loaded and since
						%% auto_compile == false,
						%% let's assume the user has
						%% loaded the beam file
						%% explicitly
						ActualDir = 
						    filename:dirname(SuiteFile),
						{[{ActualDir,Suite}|Found],
						 NotFound};
					    false ->
						Name =
						    filename:join(TestDir,
								  atom_to_list(
								    Suite)),
						io:format(user,
							  "Suite ~w not found "
							  "in directory ~ts~n",
							  [Suite,TestDir]),
						{Found,[{DS,[Name]}|NotFound]}
					end
				end
			end;
		    {error,_Reason} ->
			case code:is_loaded(Suite) of
			    {file,SuiteFile} ->
				%% test suite is already loaded and since
				%% auto_compile == false, let's assume the
				%% user has loaded the beam file explicitly
				ActualDir = filename:dirname(SuiteFile),
				{[{ActualDir,Suite}|Found],NotFound};
			    false ->
				io:format(user, "Directory ~ts is "
					  "invalid~n", [Dir]),
				Name = filename:join(Dir, atom_to_list(Suite)),
				{Found,[{DS,[Name]}|NotFound]}
			end
		end
	end,
    {ActualFound,Missing} = lists:foldl(Verify, {[],[]}, TestSuites),
    {lists:reverse(ActualFound),lists:reverse(Missing)}.

save_make_errors([]) ->
    [];
save_make_errors(Errors) ->
    Suites = get_bad_suites(Errors,[]),
    ct_logs:log("MAKE RESULTS",
		"Error compiling or locating the "
		"following suites: ~n~p",[Suites]),
    %% save the info for logger
    ok = file:write_file(?missing_suites_info,term_to_binary(Errors)),
    Errors.

get_bad_suites([{{_TestDir,_Suite},Failed}|Errors], BadSuites) ->
    get_bad_suites(Errors,BadSuites++Failed);
get_bad_suites([], BadSuites) ->
    BadSuites.



%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:step/3
step(TestDir, Suite, Case) ->
    step(TestDir, Suite, Case, []).

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:step/4
step(TestDir, Suite, Case, Opts) when is_list(TestDir),
				      is_atom(Suite), is_atom(Case),
				      Suite =/= all, Case =/= all ->
    do_run([{TestDir,Suite,Case}], [{step,Opts}]).


%%%-----------------------------------------------------------------
%%% Internal
suite_tuples([{TestDir,Suites,_} | Tests]) when is_list(Suites) ->
    lists:map(fun(S) -> {TestDir,S} end, Suites) ++ suite_tuples(Tests);
suite_tuples([{TestDir,Suite,_} | Tests]) when is_atom(Suite) ->
    [{TestDir,Suite} | suite_tuples(Tests)];
suite_tuples([]) ->
    [].

final_tests(Tests, Skip, Bad) ->
    {Tests1,Skip1} = final_tests1(Tests, [], Skip, Bad),
    Skip2 = final_skip(Skip1, []),
    {Tests1,Skip2}.

final_tests1([{TestDir,Suites,_}|Tests], Final, Skip, Bad) when
      is_list(Suites), is_atom(hd(Suites)) ->
    Skip1 = [{TD,S,make_failed} || {{TD,S},_} <- Bad, S1 <- Suites,
				     S == S1, TD == TestDir],
    Final1 = [{TestDir,S,all} || S <- Suites],
    final_tests1(Tests, lists:reverse(Final1)++Final, Skip++Skip1, Bad);

final_tests1([{TestDir,all,all}|Tests], Final, Skip, Bad) ->
    MissingSuites =
	case lists:keysearch({TestDir,all}, 1, Bad) of
	    {value,{_,Failed}} ->
		[list_to_atom(filename:basename(F)) || F <- Failed];
	    false ->
		[]
	end,
    Missing = [{TestDir,S,make_failed} || S <- MissingSuites],
    Final1 = [{TestDir,all,all}|Final],
    final_tests1(Tests, Final1, Skip++Missing, Bad);

final_tests1([{TestDir,Suite,Cases}|Tests], Final, Skip, Bad) when
      Cases==[]; Cases==all  ->
    final_tests1([{TestDir,[Suite],all}|Tests], Final, Skip, Bad);

final_tests1([{TestDir,Suite,GrsOrCs}|Tests], Final, Skip, Bad) when
      is_list(GrsOrCs) ->
    case lists:keymember({TestDir,Suite}, 1, Bad) of
	true ->
	    Skip1 = Skip ++ [{TestDir,Suite,all,make_failed}],
	    final_tests1(Tests, [{TestDir,Suite,all}|Final], Skip1, Bad);
	false ->
	    GrsOrCs1 =
		lists:flatmap(
		  %% for now, only flat group defs are allowed as
		  %% start options and test spec terms
		  fun({all,all}) ->
			  [ct_groups:make_conf(TestDir, Suite, all, [], all)];
		     ({skipped,Group,TCs}) ->
			  [ct_groups:make_conf(TestDir, Suite,
					       Group, [skipped], TCs)];
		     ({skipped,TC}) ->
			  case lists:member(TC, GrsOrCs) of
			      true ->
				  [];
			      false ->
				  [TC]
			  end;
		     ({GrSpec = {GroupName,_},TCs}) ->
			  Props = [{override,GrSpec}],
			  [ct_groups:make_conf(TestDir, Suite,
					       GroupName, Props, TCs)];
		     ({GrSpec = {GroupName,_,_},TCs}) ->
			  Props = [{override,GrSpec}],
			  [ct_groups:make_conf(TestDir, Suite,
					       GroupName, Props, TCs)];
		     ({GroupOrGroups,TCs}) ->
			  [ct_groups:make_conf(TestDir, Suite,
					       GroupOrGroups, [], TCs)];
		     (TC) ->
			  [TC]
		  end, GrsOrCs),
	    Do = {TestDir,Suite,GrsOrCs1},
	    final_tests1(Tests, [Do|Final], Skip, Bad)
    end;

final_tests1([], Final, Skip, _Bad) ->
    {lists:reverse(Final),Skip}.

final_skip([{TestDir,Suite,{all,all},Reason}|Skips], Final) ->
    SkipConf = ct_groups:make_conf(TestDir, Suite, all, [], all),
    Skip = {TestDir,Suite,SkipConf,Reason},
    final_skip(Skips, [Skip|Final]);

final_skip([{TestDir,Suite,{Group,TCs},Reason}|Skips], Final) ->
    Conf =  ct_groups:make_conf(TestDir, Suite, Group, [], TCs),
    Skip = {TestDir,Suite,Conf,Reason},
    final_skip(Skips, [Skip|Final]);

final_skip([Skip|Skips], Final) ->
    final_skip(Skips, [Skip|Final]);

final_skip([], Final) ->
    lists:reverse(Final).

continue([], _) ->
    true;
continue(_MakeErrors, true) ->
    false;
continue(_MakeErrors, _AbortIfMissingSuites) ->
    io:nl(),
    OldGL = group_leader(),
    case set_group_leader_same_as_shell(OldGL) of
	true ->
	    S = self(),
	    io:format("Failed to compile or locate one "
		      "or more test suites\n"
		      "Press \'c\' to continue or \'a\' to abort.\n"
		      "Will continue in 15 seconds if no "
		      "answer is given!\n"),
	    Pid = spawn(fun() ->
				case io:get_line('(c/a) ') of
				    "c\n" ->
					S ! true;
				    _ ->
					S ! false
				end
			end),
	    group_leader(OldGL, self()),
	    receive R when R==true; R==false ->
		    R
	    after 15000 ->
		    exit(Pid, kill),
		    io:format("... timeout - continuing!!\n"),
		    true
	    end;
	false ->				% no shell process to use
	    true
    end.

set_group_leader_same_as_shell(OldGL) ->
    %% find the group leader process on the node in a dirty fashion
    %% (check initial function call and look in the process dictionary)
    GS2or3 = fun(P) ->
    		     case process_info(P,initial_call) of
    			 {initial_call,{group,server,X}} when X == 2 ; X == 3 ->
    			     true;
    			 _ ->
    			     false
    		     end
    	     end,	
    case [P || P <- processes(), GS2or3(P),
    	       true == lists:keymember(shell,1,
    				       element(2,process_info(P,dictionary)))] of
    	[GL|_] ->
            %% check if started from remote node (skip interaction)
            if node(OldGL) /= node(GL) -> false;
               true -> group_leader(GL, self())
            end;
    	[] ->
    	    false
    end.

check_and_add([{TestDir0,M,_} | Tests], Added, PA) ->
    case locate_test_dir(TestDir0, M) of
	{ok,TestDir} ->
	    case lists:member(TestDir, Added) of
		true ->
		    check_and_add(Tests, Added, PA);
		false ->
		    case lists:member(rm_trailing_slash(TestDir),
				      code:get_path()) of
			false ->
			    true = code:add_patha(TestDir),
			    check_and_add(Tests, [TestDir|Added], [TestDir|PA]);
			true ->
			    check_and_add(Tests, [TestDir|Added], PA)
		    end
	    end;
	{error,_} ->
	    {error,{invalid_directory,TestDir0}}
    end;
check_and_add([], _, PA) ->
    {ok,PA}.

do_run_test(Tests, Skip, Opts0) ->
    case check_and_add(Tests, [], []) of
	{ok,AddedToPath} ->
	    ct_util:set_testdata({stats,{0,0,{0,0}}}),

	    %% test_server needs to know the include path too
	    InclPath = case application:get_env(common_test, include) of
			   {ok,Incls} -> Incls;
			   _          -> []
		       end,
	    application:set_env(test_server, include, InclPath),

	    %% copy the escape characters setting to test_server
	    EscChars =
		case application:get_env(common_test, esc_chars) of
		    {ok,ECBool} -> ECBool;
		    _           -> true
		end,
	    application:set_env(test_server, esc_chars, EscChars),

	    {ok, _} = test_server_ctrl:start_link(local),

	    %% let test_server expand the test tuples and count no of cases
	    {Suites,NoOfCases} = count_test_cases(Tests, Skip),
	    Suites1 = delete_dups(Suites),
	    NoOfTests = length(Tests),
	    NoOfSuites = length(Suites1),
	    ct_util:warn_duplicates(Suites1),
	    {ok,Cwd} = file:get_cwd(),
	    io:format("~nCWD set to: ~p~n", [Cwd]),
	    if NoOfCases == unknown ->
		    io:format("~nTEST INFO: ~w test(s), ~w suite(s)~n~n",
			      [NoOfTests,NoOfSuites]),
		    ct_logs:log("TEST INFO","~w test(s), ~w suite(s)",
				[NoOfTests,NoOfSuites]);
	       true ->
		    io:format("~nTEST INFO: ~w test(s), ~w case(s) "
			      "in ~w suite(s)~n~n",
			      [NoOfTests,NoOfCases,NoOfSuites]),
		    ct_logs:log("TEST INFO","~w test(s), ~w case(s) "
				"in ~w suite(s)",
				[NoOfTests,NoOfCases,NoOfSuites])
	    end,
	    %% if the verbosity level is set lower than ?STD_IMPORTANCE, tell
	    %% test_server to ignore stdout printouts to the test case log file
	    case proplists:get_value(default, Opts0#opts.verbosity) of
		VLvl when is_integer(VLvl), (?STD_IMPORTANCE < (100-VLvl)) ->
		    test_server_ctrl:reject_io_reqs(true);
		_Lower ->
		    ok
	    end,

            case Opts0#opts.multiply_timetraps of
                undefined -> MultTT = 1;
                MultTT    -> MultTT
            end,
            case Opts0#opts.scale_timetraps of
                undefined -> ScaleTT = false;
                ScaleTT   -> ScaleTT
            end,
            ct_logs:log("TEST INFO","Timetrap time multiplier = ~w~n"
                        "Timetrap scaling enabled = ~w", [MultTT,ScaleTT]),
            test_server_ctrl:multiply_timetraps(MultTT),
	    test_server_ctrl:scale_timetraps(ScaleTT),

	    test_server_ctrl:create_priv_dir(choose_val(
					       Opts0#opts.create_priv_dir,
					       auto_per_run)),

	    {ok,LogDir} = ct_logs:get_log_dir(true),
	    {TsCoverInfo,Opts} = maybe_start_cover(Opts0, LogDir),

	    ct_event:notify(#event{name=start_info,
				   node=node(),
				   data={NoOfTests,NoOfSuites,NoOfCases}}),
	    CleanUp = add_jobs(Tests, Skip, Opts, []),
	    unlink(whereis(test_server_ctrl)),
	    catch test_server_ctrl:wait_finish(),

	    maybe_stop_cover(Opts, TsCoverInfo, LogDir),

	    %% check if last testcase has left a "dead" trace window
	    %% behind, and if so, kill it
	    case ct_util:get_testdata(interpret) of
		{_What,kill,{TCPid,AttPid}} ->
		    ct_util:kill_attached(TCPid, AttPid);
		_ ->
		    ok
	    end,
	    lists:foreach(fun(Suite) ->
				  maybe_cleanup_interpret(Suite, Opts#opts.step)
			  end, CleanUp),
	    _ = [code:del_path(Dir) || Dir <- AddedToPath],

	    %% If a severe error has occurred in the test_server,
	    %% we will generate an exception here.
	    case ct_util:get_testdata(severe_error) of
		undefined -> ok;
		SevereError ->
		    ct_logs:log("SEVERE ERROR", "~p\n", [SevereError]),
		    exit(SevereError)
	    end,

	    case ct_util:get_testdata(stats) of
		Stats = {_Ok,_Failed,{_UserSkipped,_AutoSkipped}} ->
		    Stats;
		_ ->
		    {error,test_result_unknown}
	    end;
	Error ->
	    exit(Error)
    end.

maybe_start_cover(Opts=#opts{cover=Cover,cover_stop=CoverStop0},LogDir) ->
    if Cover == undefined ->
	    {undefined,Opts};
       true ->
	    case ct_cover:get_spec(Cover) of
		{error,Reason} ->
		    exit({error,Reason});
		CoverSpec ->
		    CoverStop =
			case CoverStop0 of
			    undefined -> true;
			    Stop -> Stop
			end,
		    start_cover(Opts#opts{coverspec=CoverSpec,
					  cover_stop=CoverStop},
				LogDir)
	    end
    end.

start_cover(Opts=#opts{coverspec=CovData,cover_stop=CovStop},LogDir) ->
    {CovFile,
     CovNodes,
     CovImport,
     _CovExport,
     #cover{app        = CovApp,
	    level      = CovLevel,
	    excl_mods  = CovExcl,
	    incl_mods  = CovIncl,
	    cross      = CovCross,
	    src        = _CovSrc}} = CovData,
    ct_logs:log("COVER INFO",
		"Using cover specification file: ~ts~n"
		"App: ~w~n"
		"Cross cover: ~w~n"
		"Including ~w modules~n"
		"Excluding ~w modules",
		[CovFile,CovApp,CovCross,
		 length(CovIncl),length(CovExcl)]),

    %% Tell test_server to print a link in its coverlog
    %% pointing to the real coverlog which will be written in
    %% maybe_stop_cover/2
    test_server_ctrl:cover({log,LogDir}),

    %% Cover compile all modules
    {ok,TsCoverInfo} = test_server_ctrl:cover_compile(CovApp,CovFile,
						      CovExcl,CovIncl,
						      CovCross,CovLevel,
						      CovStop),
    ct_logs:log("COVER INFO",
		"Compilation completed - test_server cover info: ~tp",
		[TsCoverInfo]),

    %% start cover on specified nodes
    if (CovNodes /= []) and (CovNodes /= undefined) ->
	    ct_logs:log("COVER INFO",
			"Nodes included in cover "
			"session: ~w",
			[CovNodes]),
	    cover:start(CovNodes);
       true ->
	    ok
    end,
    lists:foreach(
      fun(Imp) ->
	      case cover:import(Imp) of
		  ok ->
		      ok;
		  {error,Reason} ->
		      ct_logs:log("COVER INFO",
				  "Importing cover data from: ~ts fails! "
				  "Reason: ~p", [Imp,Reason])
	      end
      end, CovImport),
    {TsCoverInfo,Opts}.

maybe_stop_cover(_,undefined,_) ->
    ok;
maybe_stop_cover(#opts{coverspec=CovData},TsCoverInfo,LogDir) ->
    {_CovFile,
     _CovNodes,
     _CovImport,
     CovExport,
     _AppData} = CovData,
    case CovExport of
	undefined -> ok;
	_ ->
	    ct_logs:log("COVER INFO","Exporting cover data to ~tp",[CovExport]),
	    cover:export(CovExport)
    end,
    ct_logs:log("COVER INFO","Analysing cover data to ~tp",[LogDir]),
    test_server_ctrl:cover_analyse(TsCoverInfo,LogDir),
    ct_logs:log("COVER INFO","Analysis completed.",[]),
    ok.


delete_dups([S | Suites]) ->
    Suites1 = lists:delete(S, Suites),
    [S | delete_dups(Suites1)];
delete_dups([]) ->
    [].

count_test_cases(Tests, Skip) ->
    SendResult = fun(Me, Result) -> Me ! {no_of_cases,Result} end,
    TSPid = test_server_ctrl:start_get_totals(SendResult),
    Ref = erlang:monitor(process, TSPid),
    _ = add_jobs(Tests, Skip, #opts{}, []),
    Counted = (catch count_test_cases1(length(Tests), 0, [], Ref)),
    erlang:demonitor(Ref, [flush]),
    case Counted of
	{error,{test_server_died}} = Error ->
	    throw(Error);
	{error,Reason} ->
	    unlink(whereis(test_server_ctrl)),
	    test_server_ctrl:stop(),
	    throw({user_error,Reason});
	Result ->
	    test_server_ctrl:stop_get_totals(),
	    Result
    end.

count_test_cases1(0, N, Suites, _) ->
    {lists:flatten(Suites), N};
count_test_cases1(Jobs, N, Suites, Ref) ->
    receive
	{_,{error,_Reason} = Error} ->
	    throw(Error);
	{no_of_cases,{Ss,N1}} ->
	    count_test_cases1(Jobs-1, add_known(N,N1), [Ss|Suites], Ref);
	{'DOWN', Ref, _, _, Info} ->
	    throw({error,{test_server_died,Info}})
    end.

add_known(unknown, _) ->
    unknown;
add_known(_, unknown) ->
    unknown;
add_known(N, N1) ->
    N+N1.

add_jobs([{TestDir,all,_}|Tests], Skip, Opts, CleanUp) ->
    Name = get_name(TestDir),
    case catch test_server_ctrl:add_dir_with_skip(Name, TestDir,
						  skiplist(TestDir,Skip)) of
	{'EXIT',_} ->
	    CleanUp;
	_ ->
	    case wait_for_idle() of
		ok ->
		    add_jobs(Tests, Skip, Opts, CleanUp);
		_ ->
		    CleanUp
	    end
    end;
add_jobs([{TestDir,[Suite],all}|Tests], Skip,
	 Opts, CleanUp) when is_atom(Suite) ->
    add_jobs([{TestDir,Suite,all}|Tests], Skip, Opts, CleanUp);
add_jobs([{TestDir,Suites,all}|Tests], Skip,
	 Opts, CleanUp) when is_list(Suites) ->
    Name = get_name(TestDir) ++ ".suites",
    case catch test_server_ctrl:add_module_with_skip(Name, Suites,
						     skiplist(TestDir,Skip)) of
	{'EXIT',_} ->
	    CleanUp;
	_ ->
	    case wait_for_idle() of
		ok ->
		    add_jobs(Tests, Skip, Opts, CleanUp);
		_ ->
		    CleanUp
	    end
    end;
add_jobs([{TestDir,Suite,all}|Tests], Skip, Opts, CleanUp) ->
    case maybe_interpret(Suite, all, Opts) of
	ok ->
	    Name =  get_name(TestDir) ++ "." ++ atom_to_list(Suite),
	    case catch test_server_ctrl:add_module_with_skip(Name, [Suite],
							     skiplist(TestDir,
								      Skip)) of
		{'EXIT',_} ->
		    CleanUp;
		_ ->
		    case wait_for_idle() of
			ok ->
			    add_jobs(Tests, Skip, Opts, [Suite|CleanUp]);
			_ ->
			    CleanUp
		    end
	    end;
	Error ->
	    Error
    end;

%% group (= conf case in test_server)
add_jobs([{TestDir,Suite,Confs}|Tests], Skip, Opts, CleanUp) when
      element(1, hd(Confs)) == conf ->
    Group = fun(Conf) -> proplists:get_value(name, element(2, Conf)) end,
    TestCases = fun(Conf) -> element(4, Conf) end,
    TCTestName = fun(all) -> "";
		    ([C]) when is_atom(C) -> "." ++ atom_to_list(C);
		    (Cs) when is_list(Cs) -> ".cases"
		 end,
    GrTestName =
	case Confs of
	    [Conf] ->
		case Group(Conf) of
		    GrName when is_atom(GrName) ->
			"." ++ atom_to_list(GrName) ++
			    TCTestName(TestCases(Conf));
		    _ ->
			".groups" ++ TCTestName(TestCases(Conf))
		end;
	    _ ->
		".groups"
	end,
    TestName = get_name(TestDir) ++ "." ++ atom_to_list(Suite) ++ GrTestName,
    case maybe_interpret(Suite, init_per_group, Opts) of
	ok ->
	    case catch test_server_ctrl:add_conf_with_skip(TestName,
							   Suite,
							   Confs,
							   skiplist(TestDir,
								    Skip)) of
		{'EXIT',_} ->
		    CleanUp;
		_ ->
		    case wait_for_idle() of
			ok ->
			    add_jobs(Tests, Skip, Opts, [Suite|CleanUp]);
			_ ->
			    CleanUp
		    end
	    end;
	Error ->
	    Error
    end;

%% test case
add_jobs([{TestDir,Suite,[Case]}|Tests],
	 Skip, Opts, CleanUp) when is_atom(Case) ->
    add_jobs([{TestDir,Suite,Case}|Tests], Skip, Opts, CleanUp);

add_jobs([{TestDir,Suite,Cases}|Tests],
	 Skip, Opts, CleanUp) when is_list(Cases) ->
    Cases1 = lists:map(fun({GroupName,_}) when is_atom(GroupName) -> GroupName;
			  (Case) -> Case
		       end, Cases),
    case maybe_interpret(Suite, Cases1, Opts) of
	ok ->
	    Name =  get_name(TestDir) ++ "." ++ atom_to_list(Suite) ++ ".cases",
	    case catch test_server_ctrl:add_cases_with_skip(Name, Suite, Cases1,
							    skiplist(TestDir,
								     Skip)) of
		{'EXIT',_} ->
		    CleanUp;
		_ ->
		    case wait_for_idle() of
			ok ->
			    add_jobs(Tests, Skip, Opts, [Suite|CleanUp]);
			_ ->
			    CleanUp
		    end
	    end;
	Error ->
	    Error
    end;
add_jobs([{TestDir,Suite,Case}|Tests], Skip, Opts, CleanUp) when is_atom(Case) ->
    case maybe_interpret(Suite, Case, Opts) of
	ok ->
	    Name = get_name(TestDir) ++	"." ++ atom_to_list(Suite) ++ "." ++
		atom_to_list(Case),
	    case catch test_server_ctrl:add_case_with_skip(Name, Suite, Case,
							   skiplist(TestDir,
								    Skip)) of
		{'EXIT',_} ->
		    CleanUp;
		_ ->
		    case wait_for_idle() of
			ok ->
			    add_jobs(Tests, Skip, Opts, [Suite|CleanUp]);
			_ ->
			    CleanUp
		    end
	    end;
	Error ->
	    Error
    end;
add_jobs([], _, _, CleanUp) ->
    CleanUp.

wait_for_idle() ->
    ct_util:update_last_run_index(),
    Notify = fun(Me,IdleState) -> Me ! {idle,IdleState},
				  receive
				      {Me,proceed} -> ok
				  after
				      30000 -> ok
				  end
	     end,
    case catch test_server_ctrl:idle_notify(Notify) of
	{'EXIT',_} ->
	    error;
	TSPid ->
	    %% so we don't hang forever if test_server dies
	    Ref = erlang:monitor(process, TSPid),
	    Result = receive
			 {idle,abort}           -> aborted;
			 {idle,_}               -> ok;
			 {'DOWN', Ref, _, _, _} -> error
		     end,
	    erlang:demonitor(Ref, [flush]),
	    ct_util:update_last_run_index(),
	    %% let test_server_ctrl proceed (and possibly shut down) now
	    TSPid ! {self(),proceed},
	    Result
    end.

skiplist(Dir, [{Dir,all,Cmt}|Skip]) ->
    %% we need to turn 'all' into list of modules since
    %% test_server doesn't do skips on Dir level
    Ss = filelib:wildcard(filename:join(Dir, "*_SUITE.beam")),
    [{list_to_atom(filename:basename(S,".beam")),Cmt} || S <- Ss] ++
	skiplist(Dir,Skip);
skiplist(Dir, [{Dir,S,Cmt}|Skip]) ->
    [{S,Cmt} | skiplist(Dir, Skip)];
skiplist(Dir, [{Dir,S,C,Cmt}|Skip]) ->
    [{S,C,Cmt} | skiplist(Dir, Skip)];
skiplist(Dir, [_|Skip]) ->
    skiplist(Dir, Skip);
skiplist(_Dir, []) ->
    [].

get_name(Dir) ->
    TestDir =
	case filename:basename(Dir) of
	    "test" ->
		filename:dirname(Dir);
	    _ ->
		Dir
	end,
    Base = filename:basename(TestDir),
    case filename:basename(filename:dirname(TestDir)) of
	"" ->
	    Base;
	TopDir ->
	    TopDir ++ "." ++ Base
    end.


run_make(TestDir, Mod, UserInclude) ->
    run_make(suites, TestDir, Mod, UserInclude).

run_make(Targets, TestDir0, Mod, UserInclude) when is_list(Mod) ->
    run_make(Targets, TestDir0, list_to_atom(Mod), UserInclude);

run_make(Targets, TestDir0, Mod, UserInclude) ->
    case locate_test_dir(TestDir0, Mod) of
	{ok,TestDir} ->
	    %% send a start_make notification which may suspend
	    %% the process if some other node is compiling files
	    %% in the same directory
	    ct_event:sync_notify(#event{name=start_make,
					node=node(),
					data=TestDir}),
	    {ok,Cwd} = file:get_cwd(),
	    ok = file:set_cwd(TestDir),
	    CtInclude = get_dir(common_test, "include"),
	    XmerlInclude = get_dir(xmerl, "include"),
	    ErlFlags = UserInclude ++ [{i,CtInclude},
				       {i,XmerlInclude},
				       debug_info],
	    Result =
		if Mod == all ; Targets == helpmods ->
			case (catch ct_make:all([noexec|ErlFlags])) of
			    {'EXIT',_} = Failure ->
				Failure;
			    MakeInfo ->
				FileTest = fun(F, suites) -> is_suite(F);
					      (F, helpmods) -> not is_suite(F)
					   end,
				Files =
				    lists:flatmap(fun({F,out_of_date}) ->
							  case FileTest(F,
									Targets) of
								  true -> [F];
								  false -> []
							      end;
							 (_) ->
							      []
						      end, MakeInfo),
				(catch ct_make:files(Files, [load|ErlFlags]))
			end;
		   true ->
			(catch ct_make:files([Mod], [load|ErlFlags]))
		end,

	    ok = file:set_cwd(Cwd),
	    %% send finished_make notification
	    ct_event:notify(#event{name=finished_make,
				   node=node(),
				   data=TestDir}),
	    case Result of
		{up_to_date,_} ->
		    ok;
		{'EXIT',Reason} ->
		    io:format("{error,{make_crashed,~p}\n", [Reason]),
		    {error,{make_crashed,TestDir,Reason}};
		{error,ModInfo} ->
		    io:format("{error,make_failed}\n", []),
		    Bad = [filename:join(TestDir, M) || {M,R} <- ModInfo,
							R == error],
		    {error,{make_failed,Bad}}
	    end;
	{error,_} ->
	    io:format("{error,{invalid_directory,~p}}\n", [TestDir0]),
	    {error,{invalid_directory,TestDir0}}
    end.

get_dir(App, Dir) ->
    filename:join(code:lib_dir(App), Dir).

maybe_interpret(Suite, Cases, #opts{step = StepOpts}) when StepOpts =/= undefined ->
    %% if other suite has run before this one, check if last testcase
    %% has left a "dead" trace window behind, and if so, kill it
    case ct_util:get_testdata(interpret) of
	{_What,kill,{TCPid,AttPid}} ->
	    ct_util:kill_attached(TCPid, AttPid);
	_ ->
	    ok
    end,
    maybe_interpret1(Suite, Cases, StepOpts);
maybe_interpret(_, _, _) ->
    ok.

maybe_interpret1(Suite, all, StepOpts) ->
    case i:ii(Suite) of
	{module,_} ->
	    i:iaa([break]),
	    case get_all_testcases(Suite) of
		{error,_} ->
		    {error,no_testcases_found};
		Cases ->
		    maybe_interpret2(Suite, Cases, StepOpts)
	    end;
	error ->
	    {error,could_not_interpret_module}
    end;
maybe_interpret1(Suite, Case, StepOpts) when is_atom(Case) ->
    maybe_interpret1(Suite, [Case], StepOpts);
maybe_interpret1(Suite, Cases, StepOpts) when is_list(Cases) ->
    case i:ii(Suite) of
	{module,_} ->
	    i:iaa([break]),
	    maybe_interpret2(Suite, Cases, StepOpts);
	error ->
	    {error,could_not_interpret_module}
    end.

maybe_interpret2(Suite, Cases, StepOpts) ->
    set_break_on_config(Suite, StepOpts),
    _ = [begin try i:ib(Suite, Case, 1) of
	       _ -> ok
	   catch
	       _:_Error ->
		   io:format(user, "Invalid breakpoint: ~w:~w/1~n",
			     [Suite,Case])
	   end
 	 end || Case <- Cases, is_atom(Case)],
    test_server_ctrl:multiply_timetraps(infinity),
    WinOp = case lists:member(keep_inactive, ensure_atom(StepOpts)) of
		true -> no_kill;
		false -> kill
	    end,
    ct_util:set_testdata({interpret,{{Suite,Cases},WinOp,
				     {undefined,undefined}}}),
    ok.

set_break_on_config(Suite, StepOpts) ->
    case lists:member(config, ensure_atom(StepOpts)) of
	true ->
	    SetBPIfExists = fun(F,A) ->
				    case erlang:function_exported(Suite, F, A) of
					true -> i:ib(Suite, F, A);
					false -> ok
				    end
			    end,
	    ok = SetBPIfExists(init_per_suite, 1),
	    ok = SetBPIfExists(init_per_group, 2),
	    ok = SetBPIfExists(init_per_testcase, 2),
	    ok = SetBPIfExists(end_per_testcase, 2),
	    ok = SetBPIfExists(end_per_group, 2),
	    ok = SetBPIfExists(end_per_suite, 1);
	false ->
	    ok
    end.

maybe_cleanup_interpret(_, undefined) ->
    ok;
maybe_cleanup_interpret(Suite, _) ->
    i:iq(Suite).

log_ts_names([]) ->
    ok;
log_ts_names(Specs) ->
    List = lists:map(fun(Name) ->
			     Name ++ " "
		     end, Specs),
    ct_logs:log("Test Specification file(s)", "~ts",
		[lists:flatten(List)]).

merge_arguments(Args) ->
    merge_arguments(Args, []).

merge_arguments([LogDir={logdir,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(replace, LogDir, Merged));

merge_arguments([CoverFile={cover,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(replace, CoverFile, Merged));

merge_arguments([CoverStop={cover_stop,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(replace, CoverStop, Merged));

merge_arguments([{'case',TC}|Args], Merged) ->
    merge_arguments(Args, handle_arg(merge, {testcase,TC}, Merged));

merge_arguments([Arg|Args], Merged) ->
    merge_arguments(Args, handle_arg(merge, Arg, Merged));

merge_arguments([], Merged) ->
    Merged.

handle_arg(replace, {Key,Elems}, [{Key,_}|Merged]) ->
    [{Key,Elems}|Merged];
handle_arg(merge, {event_handler_init,Elems}, [{event_handler_init,PrevElems}|Merged]) ->
    [{event_handler_init,PrevElems++["add"|Elems]}|Merged];
handle_arg(merge, {userconfig,Elems}, [{userconfig,PrevElems}|Merged]) ->
    [{userconfig,PrevElems++["add"|Elems]}|Merged];
handle_arg(merge, {Key,Elems}, [{Key,PrevElems}|Merged]) ->
    [{Key,PrevElems++Elems}|Merged];
handle_arg(Op, Arg, [Other|Merged]) ->
    [Other|handle_arg(Op, Arg, Merged)];
handle_arg(_,Arg,[]) ->
    [Arg].

get_start_opt(Key, IfExists, Args) ->
    get_start_opt(Key, IfExists, undefined, Args).

get_start_opt(Key, IfExists, IfNotExists, Args) ->
    try try_get_start_opt(Key, IfExists, IfNotExists, Args) of
	Result ->
	    Result
    catch
	error:_ ->
	    exit({user_error,{bad_argument,Key}})
    end.

try_get_start_opt(Key, IfExists, IfNotExists, Args) ->
    case lists:keysearch(Key, 1, Args) of
	{value,{Key,Val}} when is_function(IfExists) ->
	    IfExists(Val);
	{value,{Key,Val}} when IfExists == value ->
	    Val;
	{value,{Key,_Val}} ->
	    IfExists;
	_ ->
	    IfNotExists
    end.

ct_hooks_args2opts(Args) ->
    lists:foldl(fun({ct_hooks,Hooks}, Acc) ->
			ct_hooks_args2opts(Hooks,Acc);
		   (_,Acc) ->
			Acc
		end,[],Args).

ct_hooks_args2opts([CTH,Arg,Prio,"and"| Rest],Acc) when Arg /= "and" ->
    ct_hooks_args2opts(Rest,[{list_to_atom(CTH),
			      parse_cth_args(Arg),
			      parse_cth_args(Prio)}|Acc]);
ct_hooks_args2opts([CTH,Arg,"and"| Rest],Acc) ->
    ct_hooks_args2opts(Rest,[{list_to_atom(CTH),
			      parse_cth_args(Arg)}|Acc]);
ct_hooks_args2opts([CTH], Acc) ->
    ct_hooks_args2opts([CTH,"and"],Acc);
ct_hooks_args2opts([CTH, "and" | Rest], Acc) ->
    ct_hooks_args2opts(Rest,[list_to_atom(CTH)|Acc]);
ct_hooks_args2opts([CTH, Args], Acc) ->
    ct_hooks_args2opts([CTH, Args, "and"],Acc);
ct_hooks_args2opts([CTH, Args, Prio], Acc) ->
    ct_hooks_args2opts([CTH, Args, Prio, "and"],Acc);
ct_hooks_args2opts([],Acc) ->
    lists:reverse(Acc).

parse_cth_args(String) ->
    try
	true = io_lib:printable_list(String),
	{ok,Toks,_} = erl_scan:string(String++"."),
	{ok, Args} = erl_parse:parse_term(Toks),
	Args
    catch _:_ ->
	    String
    end.

event_handler_args2opts(Args) ->
    case proplists:get_value(event_handler, Args) of
	undefined ->
	    event_handler_args2opts([], Args);
	EHs ->
	    event_handler_args2opts([{list_to_atom(EH),[]} || EH <- EHs], Args)
    end.
event_handler_args2opts(Default, Args) ->
    case proplists:get_value(event_handler_init, Args) of
	undefined ->
	    Default;
	EHs ->
	    event_handler_init_args2opts(EHs)
    end.
event_handler_init_args2opts([EH, Arg, "and" | EHs]) ->
    [{list_to_atom(EH),lists:flatten(io_lib:format("~ts",[Arg]))} |
     event_handler_init_args2opts(EHs)];
event_handler_init_args2opts([EH, Arg]) ->
    [{list_to_atom(EH),lists:flatten(io_lib:format("~ts",[Arg]))}];
event_handler_init_args2opts([]) ->
    [].

verbosity_args2opts(Args) ->
    case proplists:get_value(verbosity, Args) of
	undefined ->
	    [];
	VArgs ->	
	    GetVLvls =
		fun("and", {new,SoFar}) when is_list(SoFar) ->
			{new,SoFar};
		   ("and", {Lvl,SoFar}) when is_list(SoFar) -> 
			{new,[{'$unspecified',list_to_integer(Lvl)} | SoFar]};
		   (CatOrLvl, {new,SoFar}) when is_list(SoFar) -> 
			{CatOrLvl,SoFar};
		   (Lvl, {Cat,SoFar}) ->
			{new,[{list_to_atom(Cat),list_to_integer(Lvl)} | SoFar]}
		end,
		case lists:foldl(GetVLvls, {new,[]}, VArgs) of
		    {new,Parsed} ->
			Parsed;
		    {Lvl,Parsed} ->
			[{'$unspecified',list_to_integer(Lvl)} | Parsed]
		end
    end.

add_verbosity_defaults(VLvls) ->
    case {proplists:get_value('$unspecified', VLvls),
	  proplists:get_value(default, VLvls)} of
	{undefined,undefined} ->	    
	    ?default_verbosity ++ VLvls;
	{Lvl,undefined} ->
	    [{default,Lvl} | VLvls];
	{undefined,_Lvl} ->
	    [{'$unspecified',?MAX_VERBOSITY} | VLvls];
	_ ->
	    VLvls
    end.

%% This function reads pa and pz arguments, converts dirs from relative
%% to absolute, and re-inserts them in the code path. The order of the
%% dirs in the code path remain the same. Note however that since this
%% function is only used for arguments "pre run_test erl_args", the order
%% relative dirs "post run_test erl_args" is not kept!
rel_to_abs(CtArgs) ->
    {PA,PZ} = get_pa_pz(CtArgs, [], []),
    _ = [begin
	 Dir = rm_trailing_slash(D),
	 Abs = make_abs(Dir),
	 _ = if Dir /= Abs ->
		 _ = code:del_path(Dir),
		 _ = code:del_path(Abs),		 
		 io:format(user, "Converting ~p to ~p and re-inserting "
			   "with add_pathz/1~n",
			   [Dir, Abs]);
	    true ->
		 _ = code:del_path(Dir)
	 end,
	 code:add_pathz(Abs)	 
     end || D <- PZ],
    _ = [begin
	 Dir = rm_trailing_slash(D),
	 Abs = make_abs(Dir),
	 _ = if Dir /= Abs ->
		 _ = code:del_path(Dir),
		 _ = code:del_path(Abs),		 
		 io:format(user, "Converting ~p to ~p and re-inserting "
			   "with add_patha/1~n",
			   [Dir, Abs]);
	    true ->
		 _ = code:del_path(Dir)
	 end,
	 code:add_patha(Abs)
     end || D <- PA],
    io:format(user, "~n", []).	    

rm_trailing_slash(Dir) ->
    filename:join(filename:split(Dir)).

get_pa_pz([{pa,Dirs} | Args], PA, PZ) ->
    get_pa_pz(Args, PA ++ Dirs, PZ);
get_pa_pz([{pz,Dirs} | Args], PA, PZ) ->
    get_pa_pz(Args, PA, PZ ++ Dirs);
get_pa_pz([_ | Args], PA, PZ) ->
    get_pa_pz(Args, PA, PZ);
get_pa_pz([], PA, PZ) ->
    {PA,PZ}.

make_abs(RelDir) ->
    Tokens = filename:split(filename:absname(RelDir)),
    filename:join(lists:reverse(make_abs1(Tokens, []))).

make_abs1([".."|Dirs], [_Dir|Path]) ->
    make_abs1(Dirs, Path);
make_abs1(["."|Dirs], Path) ->
    make_abs1(Dirs, Path);
make_abs1([Dir|Dirs], Path) ->
    make_abs1(Dirs, [Dir|Path]);
make_abs1([], Path) ->
    Path.

%% This function translates ct:run_test/1 start options
%% to ct_run start arguments (on the init arguments format) -
%% this is useful mainly for testing the ct_run start functions.
opts2args(EnvStartOpts) ->
    lists:flatmap(fun({exit_status,ExitStatusOpt}) when is_atom(ExitStatusOpt) ->
			  [{exit_status,[atom_to_list(ExitStatusOpt)]}];
		     ({halt_with,{HaltM,HaltF}}) ->
			  [{halt_with,[atom_to_list(HaltM),
				       atom_to_list(HaltF)]}];
		     ({interactive_mode,true}) ->
			  [{shell,[]}];
		     ({config,CfgFile}) when is_integer(hd(CfgFile)) ->
			  [{ct_config,[CfgFile]}];
		     ({config,CfgFiles}) when is_list(hd(CfgFiles)) ->
			  [{ct_config,CfgFiles}];
		     ({userconfig,{CBM,CfgStr=[X|_]}}) when is_integer(X) ->
			  [{userconfig,[atom_to_list(CBM),CfgStr]}];
		     ({userconfig,{CBM,CfgStrs}}) when is_list(CfgStrs) ->
			  [{userconfig,[atom_to_list(CBM) | CfgStrs]}];
		     ({userconfig,UserCfg}) when is_list(UserCfg) ->
			  Strs =
			      lists:map(fun({CBM,CfgStr=[X|_]})
					      when is_integer(X) ->
						[atom_to_list(CBM),
						 CfgStr,"and"];
					   ({CBM,CfgStrs})
					      when is_list(CfgStrs) ->
						[atom_to_list(CBM) | CfgStrs] ++
						    ["and"]
					end, UserCfg),
			  [_LastAnd|StrsR] = lists:reverse(lists:flatten(Strs)),
			  [{userconfig,lists:reverse(StrsR)}];
		     ({group,G}) when is_atom(G) ->
			  [{group,[atom_to_list(G)]}];
		     ({group,Gs}) when is_list(Gs) ->
			  LOfGStrs = [lists:flatten(io_lib:format("~w",[G])) ||
					 G <- Gs],
			  [{group,LOfGStrs}];
		     ({testcase,Case}) when is_atom(Case) ->
			  [{'case',[atom_to_list(Case)]}];
		     ({testcase,Cases}) ->
			  [{'case',[atom_to_list(C) || C <- Cases]}];
		     ({'case',Cases}) ->
			  [{'case',[atom_to_list(C) || C <- Cases]}];
		     ({allow_user_terms,true}) ->
			  [{allow_user_terms,[]}];
		     ({allow_user_terms,false}) ->
			  [];
		     ({join_specs,true}) ->
			  [{join_specs,[]}];
		     ({join_specs,false}) ->
			  [];
		     ({auto_compile,false}) ->
			  [{no_auto_compile,[]}];
		     ({auto_compile,true}) ->
			  [];
		     ({scale_timetraps,true}) ->
			  [{scale_timetraps,[]}];
		     ({scale_timetraps,false}) ->
			  [];
		     ({create_priv_dir,auto_per_run}) ->
			  [];
		     ({create_priv_dir,PD}) when is_atom(PD) ->
			  [{create_priv_dir,[atom_to_list(PD)]}];
		     ({force_stop,skip_rest}) ->
			  [{force_stop,["skip_rest"]}];
		     ({force_stop,true}) ->
			  [{force_stop,[]}];
		     ({force_stop,false}) ->
			  [];
		     ({decrypt,{key,Key}}) ->
			  [{ct_decrypt_key,[Key]}];
		     ({decrypt,{file,File}}) ->
			  [{ct_decrypt_file,[File]}];
		     ({basic_html,true}) ->
			  [{basic_html,[]}];
		     ({basic_html,false}) ->
			  [];
		     ({esc_chars,false}) ->
			  [{no_esc_chars,[]}];
		     ({esc_chars,true}) ->
			  [];
		     ({event_handler,EH}) when is_atom(EH) ->
			  [{event_handler,[atom_to_list(EH)]}];
		     ({event_handler,EHs}) when is_list(EHs) ->
			  [{event_handler,[atom_to_list(EH) || EH <- EHs]}];
		     ({event_handler,{EH,Arg}}) when is_atom(EH) ->
			  ArgStr = lists:flatten(io_lib:format("~p", [Arg])),
			  [{event_handler_init,[atom_to_list(EH),ArgStr]}];
		     ({event_handler,{EHs,Arg}}) when is_list(EHs) ->
			  ArgStr = lists:flatten(io_lib:format("~p", [Arg])),
			  Strs = lists:flatmap(fun(EH) ->
						       [atom_to_list(EH),
							ArgStr,"and"]
					       end, EHs),
			  [_LastAnd | StrsR] = lists:reverse(Strs),
			  [{event_handler_init,lists:reverse(StrsR)}];
		     ({logopts,LOs}) when is_list(LOs) ->
			  [{logopts,[atom_to_list(LO) || LO <- LOs]}];
		     ({verbosity,?default_verbosity}) ->
			  [];
		     ({verbosity,VLvl}) when is_integer(VLvl) ->
			  [{verbosity,[integer_to_list(VLvl)]}];
		     ({verbosity,VLvls}) when is_list(VLvls) ->
			  VLvlArgs =
			      lists:flatmap(fun({'$unspecified',Lvl}) ->
						    [integer_to_list(Lvl),
						     "and"];
					       ({Cat,Lvl}) ->
						    [atom_to_list(Cat),
						     integer_to_list(Lvl),
						     "and"];
					       (Lvl) ->
						    [integer_to_list(Lvl),
						     "and"]
					    end, VLvls),
			  [_LastAnd|VLvlArgsR] = lists:reverse(VLvlArgs),
			  [{verbosity,lists:reverse(VLvlArgsR)}];
		     ({ct_hooks,[]}) ->
			  [];
		     ({ct_hooks,CTHs}) when is_list(CTHs) ->
			  io:format(user,"ct_hooks: ~p",[CTHs]),
			  Strs = lists:flatmap(
				   fun({CTH,Arg,Prio}) ->
					   [atom_to_list(CTH),
					    lists:flatten(
					      io_lib:format("~p",[Arg])),
					    lists:flatten(
					      io_lib:format("~p",[Prio])),
					    "and"];
				       ({CTH,Arg}) ->
					   [atom_to_list(CTH),
					    lists:flatten(
					      io_lib:format("~p",[Arg])),
					    "and"];
				      (CTH) when is_atom(CTH) ->
					   [atom_to_list(CTH),"and"]
				   end,CTHs),
			  [_LastAnd|StrsR] = lists:reverse(Strs),
			  io:format(user,"return: ~p",[lists:reverse(StrsR)]),
			  [{ct_hooks,lists:reverse(StrsR)}];
		     ({Opt,As=[A|_]}) when is_atom(A) ->
			  [{Opt,[atom_to_list(Atom) || Atom <- As]}];
		     ({Opt,Strs=[S|_]}) when is_list(S) ->
			  [{Opt,Strs}];
		     ({Opt,A}) when is_atom(A) ->
			  [{Opt,[atom_to_list(A)]}];
		     ({Opt,I}) when is_integer(I) ->
			  [{Opt,[integer_to_list(I)]}];
		     ({Opt,S}) when is_list(S) ->
			  [{Opt,[S]}];
		     (Opt) ->
			  Opt
		  end, EnvStartOpts).

locate_test_dir(Dir, Suite) ->
    TestDir = case ct_util:is_test_dir(Dir) of
		  true  -> Dir;
		  false -> ct_util:get_testdir(Dir, Suite)
	      end,
    case filelib:is_dir(TestDir) of
	true  -> {ok,TestDir};
	false -> {error,invalid}
    end.

is_suite(Mod) when is_atom(Mod) ->
    is_suite(atom_to_list(Mod));
is_suite(ModOrFile) when is_list(ModOrFile) ->
    case lists:reverse(filename:basename(ModOrFile, ".erl")) of
	[$E,$T,$I,$U,$S,$_|_] ->
	    true;
	_ ->
	    case lists:reverse(filename:basename(ModOrFile, ".beam")) of
		[$E,$T,$I,$U,$S,$_|_] ->
		    true;
		_ ->
		    false
	    end
    end.

get_all_testcases(Suite) ->
    try ct_framework:get_all_cases(Suite) of
	{error,_Reason} = Error ->
	    Error;
	SuiteCases ->
	    Cases = [C || {_S,C} <- SuiteCases],
	    try Suite:sequences() of
		[] ->
		    Cases;
		Seqs ->
		    TCs1 = lists:flatten([TCs || {_,TCs} <- Seqs]),
		    lists:reverse(
		      lists:foldl(fun(TC, Acc) ->
					  case lists:member(TC, Acc) of
					      true  -> Acc;
					      false -> [TC | Acc]
					  end
				  end, [], Cases ++ TCs1))
	    catch
		_:_ ->
		    Cases
	    end
    catch
	_:Error ->
	    {error,Error}
    end.

%% Internal tracing support. If {ct_trace,TraceSpec} is present, the
%% TraceSpec file will be consulted and dbg used to trace function
%% calls during test run. Expected terms in TraceSpec:
%% {m,Mod} or {f,Mod,Func}.
start_trace(Args) ->
    case lists:keysearch(ct_trace,1,Args) of
	{value,{ct_trace,File}} ->
	    TraceSpec = delistify(File),
	    case file:consult(TraceSpec) of
		{ok,Terms} ->
		    case catch do_trace(Terms) of
			ok ->
			    true;
			{_,Error} ->
			    io:format("Warning! Tracing not started. Reason: ~p~n~n",
				      [Error]),
			    false
		    end;
		{_,Error} ->
		    io:format("Warning! Tracing not started. Reason: ~ts~n~n",
			      [file:format_error(Error)]),
		    false
	    end;
	false ->
	    false		
    end.

do_trace(Terms) ->
    dbg:tracer(),
    dbg:p(self(), [sos,call]),
    lists:foreach(fun({m,M}) ->
			  case dbg:tpl(M,x) of
			      {error,What} -> exit({error,{tracing_failed,What}});
			      _ -> ok
			  end;
		     ({me,M}) ->
			  case dbg:tp(M,[{'_',[],[{exception_trace},
						  {message,{caller}}]}]) of
			      {error,What} -> exit({error,{tracing_failed,What}});
			      _ -> ok
			  end;
		     ({f,M,F}) ->
			  case dbg:tpl(M,F,[{'_',[],[{exception_trace},
						     {message,{caller}}]}]) of
			      {error,What} -> exit({error,{tracing_failed,What}});
			      _ -> ok
			  end;
		     (Huh) ->
			  exit({error,{unrecognized_trace_term,Huh}})
		  end, Terms),
    ok.

stop_trace(true) ->
    dbg:stop_clear();
stop_trace(false) ->
    ok.

ensure_atom(Atom) when is_atom(Atom) ->
    Atom;
ensure_atom(String) when is_list(String), is_integer(hd(String)) ->
    list_to_atom(String);
ensure_atom(List) when is_list(List) ->
    [ensure_atom(Item) || Item <- List];
ensure_atom(Other) ->				
    Other.
