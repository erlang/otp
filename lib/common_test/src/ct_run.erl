%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
-export([run_make/3,do_run/3,tests/1,tests/2,tests/3]).


%% Misc internal functions
-export([variables_file_name/1,script_start1/2,run_test1/1]).

-include("ct_event.hrl").
-include("ct_util.hrl").

-define(abs(Name), filename:absname(Name)).
-define(testdir(Name, Suite), ct_util:get_testdir(Name, Suite)).

-record(opts, {vts,
	       shell,
	       cover,
	       coverspec,
	       step,
	       logdir,
	       config = [],
	       event_handlers = [],
	       include = [],
	       silent_connections,
	       multiply_timetraps = 1,
	       scale_timetraps = false,
	       testspecs = [],
	       tests}).

%%%-----------------------------------------------------------------
%%% @spec script_start() -> void()
%%%
%%% @doc Start tests via the run_test program or script.
%%%
%%% <p>Example:<br/><code>./run_test -config config.ctc -dir
%%% $TEST_DIR</code></p>
%%%
%%% <p>Example:<br/><code>./run_test -config config.ctc -suite
%%% $SUITE_PATH/$SUITE_NAME [-case $CASE_NAME]</code></p>
%%%
script_start() ->
    process_flag(trap_exit, true),
    Init = init:get_arguments(),
    CtArgs = lists:takewhile(fun({ct_erl_args,_}) -> false;
				(_) -> true end, Init),
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
		io:format(user, "~n--------------- START ARGS ---------------~n", []),
		io:format(user, "--- Init args:~n~p~n", [FlagFilter(Init)]),
		io:format(user, "--- CT args:~n~p~n", [FlagFilter(CtArgs)]),
		EnvArgs = opts2args(EnvStartOpts),
		io:format(user, "--- Env opts -> args:~n~p~n   =>~n~p~n",
			  [EnvStartOpts,EnvArgs]),
		Merged = merge_arguments(CtArgs ++ EnvArgs),
		io:format(user, "--- Merged args:~n~p~n", [FlagFilter(Merged)]),
		io:format(user, "------------------------------------------~n~n", []),
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
    Res =
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
		io:format("~nCommon Test~s starting (cwd is ~s)~n~n", [CTVsn,Cwd]),
		Self = self(),
		Pid = spawn_link(fun() -> script_start1(Self, Args) end),
	        receive
		    {'EXIT',Pid,Reason} ->
			case Reason of
			    {user_error,What} ->
				io:format("\nTest run failed!\nReason: ~p\n\n", [What]),
				{error,What};
			    _ ->
				io:format("Test run crashed! This could be an internal error "
					  "- please report!\n\n"
					  "~p\n\n", [Reason]),
				{error,Reason}				
			end;
		    {Pid,{error,Reason}} ->
			io:format("\nTest run failed! Reason:\n~p\n\n",[Reason]),
			{error,Reason};
		    {Pid,Result} ->
			Result
		end;
	    Result ->
		Result
	end,
    stop_trace(Tracing),
    Res.

script_start1(Parent, Args) ->
    %% read general start flags
    Vts = get_start_opt(vts, true, Args),
    Shell = get_start_opt(shell, true, Args),
    Cover = get_start_opt(cover, fun(CoverFile) -> ?abs(CoverFile) end, Args),
    LogDir = get_start_opt(logdir, fun([LogD]) -> LogD end, Args),
    MultTT = get_start_opt(multiply_timetraps, fun(MT) -> MT end, 1, Args),
    ScaleTT = get_start_opt(scale_timetraps, fun(CT) -> CT end, false, Args),
    EvHandlers = event_handler_args2opts(Args),

    %% check flags and set corresponding application env variables

    %% ct_decrypt_key | ct_decrypt_file
    case proplists:get_value(ct_decrypt_key, Args) of
	[DecryptKey] ->
	    application:set_env(common_test, decrypt, {key,DecryptKey});
	undefined ->
	    case proplists:get_value(ct_decrypt_file, Args) of
		[DecryptFile] ->
		    application:set_env(common_test, decrypt,
					{file,filename:absname(DecryptFile)});
		undefined ->
		    application:unset_env(common_test, decrypt)
	    end
    end,
    %% no_auto_compile + include
    IncludeDirs =
	case proplists:get_value(no_auto_compile, Args) of
	    undefined ->
		application:set_env(common_test, auto_compile, true),
		InclDirs =
		    case proplists:get_value(include, Args) of
			{include,Incl} when is_list(hd(Incl)) ->
			    Incl;
			{include,Incl} when is_list(Incl) ->
			    [Incl];
			undefined ->
			    []
		    end,
		case os:getenv("CT_INCLUDE_PATH") of
		    false ->
			application:set_env(common_test, include, InclDirs),
			InclDirs;
		    CtInclPath ->
			AllInclDirs =
			    string:tokens(CtInclPath,[$:,$ ,$,]) ++ InclDirs,
			application:set_env(common_test, include, AllInclDirs),
			AllInclDirs
		end;
	    _ ->
		application:set_env(common_test, auto_compile, false),
		[]
	end,
    %% basic_html - used by ct_logs
    case proplists:get_value(basic_html, Args) of
	undefined ->
	    application:set_env(common_test, basic_html, false);
	_ ->
	    application:set_env(common_test, basic_html, true)
    end,

   StartOpts = #opts{vts = Vts, shell = Shell, cover = Cover,
		     logdir = LogDir, event_handlers = EvHandlers,
		     include = IncludeDirs,
		     multiply_timetraps = MultTT,
		     scale_timetraps = ScaleTT},

    %% check if log files should be refreshed or go on to run tests...
    Result = run_or_refresh(StartOpts, Args),
    %% send final results to starting process waiting in script_start/0
    Parent ! {self(), Result}.

run_or_refresh(StartOpts = #opts{logdir = LogDir}, Args) ->
    case proplists:get_value(refresh_logs, Args) of
	undefined ->
	    script_start2(StartOpts, Args);
	Refresh ->
	    LogDir1 = case Refresh of
			  [] -> which(logdir,LogDir);
			  [RefreshDir] -> ?abs(RefreshDir)
		      end,
	    {ok,Cwd} = file:get_cwd(),
	    file:set_cwd(LogDir1),
	    %% give the shell time to print version etc
	    timer:sleep(500),
	    io:nl(),
	    case catch ct_logs:make_all_suites_index(refresh) of
		{'EXIT',ASReason} ->
		    file:set_cwd(Cwd),
		    {error,{all_suites_index,ASReason}};
		_ ->
		    case catch ct_logs:make_all_runs_index(refresh) of
			{'EXIT',ARReason} ->
			    file:set_cwd(Cwd),
			    {error,{all_runs_index,ARReason}};
			_ ->
			    file:set_cwd(Cwd),
			    io:format("Logs in ~s refreshed!~n~n", [LogDir1]),
			    timer:sleep(500), % time to flush io before quitting
			    ok
		    end
	    end
    end.

script_start2(StartOpts = #opts{vts = undefined,
				shell = undefined}, Args) ->
    TestSpec = proplists:get_value(spec, Args),
    {Terms,Opts} =
	case TestSpec of
	    Specs when Specs =/= [], Specs =/= undefined ->
		%% using testspec as input for test
		Relaxed = get_start_opt(allow_user_terms, true, false, Args),
		case catch ct_testspec:collect_tests_from_file(Specs, Relaxed) of
		    {E,Reason} when E == error ; E == 'EXIT' ->
			{{error,Reason},StartOpts};
		    TS ->
			SpecStartOpts = get_data_for_node(TS, node()),

			LogDir = choose_val(StartOpts#opts.logdir,
					    SpecStartOpts#opts.logdir),

			Cover = choose_val(StartOpts#opts.cover,
					   SpecStartOpts#opts.cover),
			MultTT = choose_val(StartOpts#opts.multiply_timetraps,
					    SpecStartOpts#opts.multiply_timetraps),
			ScaleTT = choose_val(StartOpts#opts.scale_timetraps,
					     SpecStartOpts#opts.scale_timetraps),
			AllEvHs = merge_vals([StartOpts#opts.event_handlers,
					      SpecStartOpts#opts.event_handlers]),
			AllInclude = merge_vals([StartOpts#opts.include,
						 SpecStartOpts#opts.include]),
			application:set_env(common_test, include, AllInclude),

			{TS,StartOpts#opts{testspecs = Specs,
					   cover = Cover,
					   logdir = LogDir,
					   config = SpecStartOpts#opts.config,
					   event_handlers = AllEvHs,
					   include = AllInclude,
					   multiply_timetraps = MultTT,
					   scale_timetraps = ScaleTT}}
		end;
	    _ ->
		{undefined,StartOpts}
	end,
    %% read config/userconfig from start flags
    InitConfig = ct_config:prepare_config_list(Args),
    case {TestSpec,Terms} of
	{_,{error,_}=Error} ->
	    Error;
	{[],_} ->
	    {error,no_testspec_specified};
	{undefined,_} ->   % no testspec used
	    case check_and_install_configfiles(InitConfig,
					       which(logdir,Opts#opts.logdir),
					       Opts#opts.event_handlers) of
		ok ->      % go on read tests from start flags
		    script_start3(Opts#opts{config=InitConfig}, Args);
		Error ->
		    Error
	    end;
	{_,_} ->           % testspec used
	    %% merge config from start flags with config from testspec
	    AllConfig = merge_vals([InitConfig, Opts#opts.config]),
	    case check_and_install_configfiles(AllConfig,
					       which(logdir,Opts#opts.logdir),
					       Opts#opts.event_handlers) of
		ok ->      % read tests from spec
		    {Run,Skip} = ct_testspec:prepare_tests(Terms, node()),
		    do_run(Run, Skip, Opts#opts{config=AllConfig}, Args);
		Error ->
		    Error
	    end
    end;

script_start2(StartOpts, Args) ->
    script_start3(StartOpts, Args).

check_and_install_configfiles(Configs, LogDir, EvHandlers) ->
    case ct_config:check_config_files(Configs) of
	false ->
	    install([{config,Configs},
		     {event_handler,EvHandlers}], LogDir);
	{value,{error,{nofile,File}}} ->
	    {error,{cant_read_config_file,File}};
	{value,{error,{wrong_config,Message}}}->
	    {error,{wrong_config,Message}};
	{value,{error,{callback,File}}} ->
	    {error,{cant_load_callback_module,File}}
    end.

script_start3(StartOpts, Args) ->
    case proplists:get_value(dir, Args) of
	[] ->
	    {error,no_dir_specified};
	Dirs when is_list(Dirs) ->
	    script_start4(StartOpts#opts{tests = tests(Dirs)}, Args);
	undefined ->
	    case proplists:get_value(suite, Args) of
		[] ->
		    {error,no_suite_specified};
		Suites when is_list(Suites) ->
		    StartOpts1 =
			get_start_opt(step,
				      fun(Step) ->
					      StartOpts#opts{step = Step,
							     cover = undefined}
				      end, StartOpts, Args),
		    DirMods = [suite_to_test(S) || S <- Suites],
		    case groups_and_cases(proplists:get_value(group, Args),
					  proplists:get_value(testcase, Args)) of
			Error = {error,_} ->
			    Error;
			[] when DirMods =/= [] ->
			    Ts = tests(DirMods),
			    script_start4(StartOpts1#opts{tests = Ts}, Args);
			GroupsAndCases when length(DirMods) == 1 ->
			    Ts = tests(DirMods, GroupsAndCases),
			    script_start4(StartOpts1#opts{tests = Ts}, Args);
			[_,_|_] when length(DirMods) > 1 ->
			    {error,multiple_suites_and_cases};
			_ ->
			    {error,incorrect_suite_option}
		    end;
		undefined ->
		    if StartOpts#opts.vts ; StartOpts#opts.shell ->
			    script_start4(StartOpts#opts{tests = []}, Args);
		       true ->
			    script_usage(),
			    {error,incorrect_usage}
		    end
	    end
    end.

script_start4(#opts{vts = true, config = Config, event_handlers = EvHandlers,
		    tests = Tests, logdir = LogDir}, _Args) ->
    vts:init_data(Config, EvHandlers, ?abs(LogDir), Tests);

script_start4(#opts{shell = true, config = Config, event_handlers = EvHandlers,
		    logdir = LogDir, testspecs = Specs}, _Args) ->
    InstallOpts = [{config,Config},{event_handler,EvHandlers}],
    if Config == [] ->
	    ok;
       true ->
	    io:format("\nInstalling: ~p\n\n", [Config])
    end,
    case install(InstallOpts) of
	ok ->
	    ct_util:start(interactive, LogDir),
	    log_ts_names(Specs),
	    io:nl(),
	    ok;
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
    erlang:halt();

script_start4(#opts{shell = true, cover = Cover}, _) ->
    case Cover of
	undefined ->
	    script_usage();
	_ ->
	    %% Add support later (maybe).
	    io:format("\nCan't run cover in interactive mode.\n\n", [])
    end;

script_start4(Opts = #opts{tests = Tests}, Args) ->
    do_run(Tests, [], Opts, Args).

%%%-----------------------------------------------------------------
%%% @spec script_usage() -> ok
%%% @doc Print usage information for <code>run_test</code>.
script_usage() ->
    io:format("\n\nUsage:\n\n"),
    io:format("Run tests in web based GUI:\n\n"
	      "\trun_test -vts [-browser Browser]"
	      "\n\t[-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t[-decrypt_key Key] | [-decrypt_file KeyFile]"
	      "\n\t[-dir TestDir1 TestDir2 .. TestDirN] |"
	      "\n\t[-suite Suite [-case Case]]"
	      "\n\t[-include InclDir1 InclDir2 .. InclDirN]"
	      "\n\t[-no_auto_compile]"
	      "\n\t[-multiply_timetraps]"
	      "\n\t[-scale_timetraps]"
	      "\n\t[-basic_html]\n\n"),
    io:format("Run tests from command line:\n\n"
	      "\trun_test [-dir TestDir1 TestDir2 .. TestDirN] |"
	      "\n\t[-suite Suite1 Suite2 .. SuiteN [-case Case1 Case2 .. CaseN]]"
	      "\n\t[-step [config | keep_inactive]]"
	      "\n\t[-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t[-userconfig CallbackModule ConfigFile1 .. ConfigFileN]"
	      "\n\t[-decrypt_key Key] | [-decrypt_file KeyFile]"
	      "\n\t[-logdir LogDir]"
	      "\n\t[-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]"
	      "\n\t[-stylesheet CSSFile]"
	      "\n\t[-cover CoverCfgFile]"
	      "\n\t[-event_handler EvHandler1 EvHandler2 .. EvHandlerN]"
	      "\n\t[-include InclDir1 InclDir2 .. InclDirN]"
	      "\n\t[-no_auto_compile]"
	      "\n\t[-multiply_timetraps]"
	      "\n\t[-scale_timetraps]"
	      "\n\t[-basic_html]"
	      "\n\t[-repeat N [-force_stop]] |"
	      "\n\t[-duration HHMMSS [-force_stop]] |"
	      "\n\t[-until [YYMoMoDD]HHMMSS [-force_stop]]\n\n"),
    io:format("Run tests using test specification:\n\n"
	      "\trun_test -spec TestSpec1 TestSpec2 .. TestSpecN"
	      "\n\t[-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t[-decrypt_key Key] | [-decrypt_file KeyFile]"
	      "\n\t[-logdir LogDir]"
	      "\n\t[-allow_user_terms]"
	      "\n\t[-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]"
	      "\n\t[-stylesheet CSSFile]"
	      "\n\t[-cover CoverCfgFile]"
	      "\n\t[-event_handler EvHandler1 EvHandler2 .. EvHandlerN]"
	      "\n\t[-include InclDir1 InclDir2 .. InclDirN]"
	      "\n\t[-no_auto_compile]"
	      "\n\t[-multiply_timetraps]"
	      "\n\t[-scale_timetraps]"
	      "\n\t[-basic_html]"
	      "\n\t[-repeat N [-force_stop]] |"
	      "\n\t[-duration HHMMSS [-force_stop]] |"
	      "\n\t[-until [YYMoMoDD]HHMMSS [-force_stop]]\n\n"),
    io:format("Refresh the HTML index files:\n\n"
	      "\trun_test -refresh_logs [LogDir]"
	      "[-logdir LogDir] "
	      "[-basic_html]\n\n"),
    io:format("Run CT in interactive mode:\n\n"
	      "\trun_test -shell"
	      "\n\t[-config ConfigFile1 ConfigFile2 .. ConfigFileN]"
	      "\n\t[-decrypt_key Key] | [-decrypt_file KeyFile]\n\n").

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:install/1
install(Opts) ->
    install(Opts, ".").

install(Opts, LogDir) ->
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

	    %%! --- Tue Jun  1 00:20:33 2010 --- peppe was here!
	    io:format("Varfile = ~p~n", [VarFile]),

	    case file:open(VarFile, [write]) of
		{ok,Fd} ->
		    [io:format(Fd, "~p.\n", [Opt]) || Opt <- Opts],
		    file:close(Fd),
		    ok;
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
    case proplists:get_value(refresh_logs, StartOpts) of
	undefined ->
	    Tracing = start_trace(StartOpts),
	    {ok,Cwd} = file:get_cwd(),
	    io:format("~nCommon Test starting (cwd is ~s)~n~n", [Cwd]),
	    Res =
		case ct_repeat:loop_test(func, StartOpts) of
		    false ->
			case catch run_test1(StartOpts) of
			    {'EXIT',Reason} ->
				file:set_cwd(Cwd),
				{error,Reason};
			    Result ->
				Result
			end;
		    Result ->
			Result
		end,
	    stop_trace(Tracing),
	    Res;
	RefreshDir ->
	    refresh_logs(?abs(RefreshDir)),
	    ok
    end.

run_test1(StartOpts) ->
    %% logdir
    LogDir = get_start_opt(logdir, fun(LD) when is_list(LD) -> LD end,
			   ".", StartOpts),
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

    %% silent connections
    SilentConns = get_start_opt(silent_connections,
				fun(all) -> [];
				   (Conns) -> Conns
				end, StartOpts),
    %% code coverage
    Cover = get_start_opt(cover, fun(CoverFile) -> ?abs(CoverFile) end, StartOpts),

    %% timetrap manipulation
    MultiplyTT = get_start_opt(multiply_timetraps, value, 1, StartOpts),
    ScaleTT = get_start_opt(scale_timetraps, value, false, StartOpts),

    %% auto compile & include files
    Include =
	case proplists:get_value(auto_compile, StartOpts) of
	    undefined ->
		application:set_env(common_test, auto_compile, true),		
		InclDirs =
		    case proplists:get_value(include, StartOpts) of
			undefined ->
			    [];
			Incl when is_list(hd(Incl)) ->
			    Incl;
			Incl when is_list(Incl) ->
			    [Incl]
		    end,
		case os:getenv("CT_INCLUDE_PATH") of
		    false ->
			application:set_env(common_test, include, InclDirs),
			InclDirs;
		    CtInclPath ->
			InclDirs1 = string:tokens(CtInclPath, [$:,$ ,$,]),
			AllInclDirs = InclDirs1++InclDirs,
			application:set_env(common_test, include, AllInclDirs),
			AllInclDirs
		end;
	    ACBool ->
		application:set_env(common_test, auto_compile, ACBool),
		[]
	end,

    %% decrypt config file
    case proplists:get_value(decrypt, StartOpts) of
	undefined ->
	    application:unset_env(common_test, decrypt);
	Key={key,_} ->
	    application:set_env(common_test, decrypt, Key);
	{file,KeyFile} ->
	    application:set_env(common_test, decrypt, {file,filename:absname(KeyFile)})
    end,

    %% basic html - used by ct_logs
    case proplists:get_value(basic_html, StartOpts) of
	undefined ->
	    application:set_env(common_test, basic_html, false);
	BasicHtmlBool ->
	    application:set_env(common_test, basic_html, BasicHtmlBool)
    end,

    %% stepped execution
    Step = get_start_opt(step, value, StartOpts),

    Opts = #opts{cover = Cover, step = Step, logdir = LogDir, config = CfgFiles,
		 event_handlers = EvHandlers, include = Include,
		 silent_connections = SilentConns, multiply_timetraps = MultiplyTT,
		 scale_timetraps = ScaleTT},

    %% test specification
    case proplists:get_value(spec, StartOpts) of
	undefined ->
	    case proplists:get_value(prepared_tests, StartOpts) of
		undefined ->            % use dir|suite|case
		    run_dir(Opts, StartOpts);
		{{Run,Skip},Specs} ->	% use prepared tests
		    run_prepared(Run, Skip, Opts#opts{testspecs = Specs}, StartOpts)
	    end;
	Specs ->
	    Relaxed = get_start_opt(allow_user_terms, value, false, StartOpts),
	    %% using testspec(s) as input for test
	    run_spec_file(Relaxed, Opts#opts{testspecs = Specs}, StartOpts)
    end.

run_spec_file(Relaxed,
	      Opts = #opts{testspecs = Specs, config = CfgFiles},
	      StartOpts) ->
    Specs1 = case Specs of
		 [X|_] when is_integer(X) -> [Specs];
		 _ -> Specs
	     end,
    AbsSpecs = lists:map(fun(SF) -> ?abs(SF) end, Specs1),
    log_ts_names(AbsSpecs),
    case catch ct_testspec:collect_tests_from_file(AbsSpecs, Relaxed) of
	{Error,CTReason} when Error == error ; Error == 'EXIT' ->
	    exit(CTReason);
	TS ->
	    SpecOpts = get_data_for_node(TS, node()),
	    LogDir = choose_val(Opts#opts.logdir,
				SpecOpts#opts.logdir),
	    AllConfig = merge_vals([CfgFiles, SpecOpts#opts.config]),
	    Cover = choose_val(Opts#opts.cover,
			       SpecOpts#opts.cover),
	    MultTT = choose_val(Opts#opts.multiply_timetraps,
				SpecOpts#opts.multiply_timetraps),
	    ScaleTT = choose_val(Opts#opts.scale_timetraps,
				 SpecOpts#opts.scale_timetraps),
	    AllEvHs = merge_vals([Opts#opts.event_handlers,
				  SpecOpts#opts.event_handlers]),
	    AllInclude = merge_vals([Opts#opts.include,
				     SpecOpts#opts.include]),
	    application:set_env(common_test, include, AllInclude),

	    case check_and_install_configfiles(AllConfig,
					       which(logdir,LogDir),
					       AllEvHs) of
		ok ->
		    Opts1 = Opts#opts{cover = Cover,
				      logdir = LogDir,
				      config = AllConfig,
				      event_handlers = AllEvHs,
				      include = AllInclude,
				      testspecs = AbsSpecs,
				      multiply_timetraps = MultTT,
				      scale_timetraps = ScaleTT},
		    {Run,Skip} = ct_testspec:prepare_tests(TS, node()),
		    do_run(Run, Skip, Opts1, StartOpts);
		{error,GCFReason} ->
		    exit(GCFReason)
	    end
    end.

run_prepared(Run, Skip, Opts = #opts{logdir = LogDir,
				     config = CfgFiles,
				     event_handlers = EvHandlers},
	     StartOpts) ->
    case check_and_install_configfiles(CfgFiles, LogDir, EvHandlers) of
	ok ->
	    do_run(Run, Skip, Opts, StartOpts);
	{error,Reason} ->
	    exit(Reason)
    end.

check_config_file(Callback, File)->
    case Callback:check_parameter(File) of
	{ok,{file,File}}->
	    ?abs(File);
	{ok,{config,_}}->
	    File;
	{nok,{wrong_config,Message}}->
	    exit({wrong_config,{Callback,Message}});
	{nok,{nofile,File}}->
	    exit({no_such_file,?abs(File)})
    end.

run_dir(Opts = #opts{logdir = LogDir,
		     config = CfgFiles,
		     event_handlers = EvHandlers}, StartOpts) ->
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
					  exit({no_such_module,Callback})
				  end
			  end,
			  {Callback,
			   lists:map(fun(File)->
					     check_config_file(Callback, File)
				     end, FileList)}
		  end, CfgFiles),
    case install([{config,AbsCfgFiles},{event_handler,EvHandlers}], LogDir) of
	ok -> ok;
	{error,IReason} -> exit(IReason)
    end,
    case lists:keysearch(dir, 1, StartOpts) of
	{value,{_,Dirs=[Dir|_]}} when not is_integer(Dir),
	                              length(Dirs)>1 ->
	    %% multiple dirs (no suite)
	    do_run(tests(Dirs), [], Opts, StartOpts);
	false ->				% no dir
	    %% fun for converting suite name to {Dir,Mod} tuple
	    S2M = fun(S) when is_list(S) ->
			  {filename:dirname(S),
			   list_to_atom(filename:rootname(filename:basename(S)))};
		     (A) ->
			  {".",A}
		  end,
	    case lists:keysearch(suite, 1, StartOpts) of
		{value,{_,Suite}} when is_integer(hd(Suite)) ; is_atom(Suite) ->
		    {Dir,Mod} = S2M(Suite),
		    case listify(proplists:get_value(group, StartOpts, [])) ++
			 listify(proplists:get_value(testcase, StartOpts, [])) of
			[] ->
			    do_run(tests(Dir, listify(Mod)), [], Opts, StartOpts);
			GsAndCs ->
			    do_run(tests(Dir, Mod, GsAndCs), [], Opts, StartOpts)
		    end;
		{value,{_,Suites}} ->
		    do_run(tests(lists:map(S2M, Suites)), [], Opts, StartOpts);
		_ ->
		    exit(no_tests_specified)
	    end;
	{value,{_,Dir}} ->
	    case lists:keysearch(suite, 1, StartOpts) of
		{value,{_,Suite}} when is_integer(hd(Suite)) ; is_atom(Suite) ->
		    Mod = if is_atom(Suite) -> Suite;
			     true -> list_to_atom(Suite)
			  end,
		    case listify(proplists:get_value(group, StartOpts, [])) ++
			 listify(proplists:get_value(testcase, StartOpts, [])) of
			[] ->
			    do_run(tests(Dir, listify(Mod)), [], Opts, StartOpts);
			GsAndCs ->
			    do_run(tests(Dir, Mod, GsAndCs), [], Opts, StartOpts)
		    end;
		{value,{_,Suites=[Suite|_]}} when is_list(Suite) ->
		    Mods = lists:map(fun(Str) -> list_to_atom(Str) end, Suites),
		    do_run(tests(delistify(Dir), Mods), [], Opts, StartOpts);
		{value,{_,Suites}} ->
		    do_run(tests(delistify(Dir), Suites), [], Opts, StartOpts);
	        false ->			% no suite, only dir
		    do_run(tests(listify(Dir)), [], Opts, StartOpts)
	    end
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
    {ok,Cwd} = file:get_cwd(),
    io:format("~nCommon Test starting (cwd is ~s)~n~n", [Cwd]),
    case catch run_testspec1(TestSpec) of
	{'EXIT',Reason} ->
	    file:set_cwd(Cwd),
	    {error,Reason};
	Result ->
	    Result
    end.

run_testspec1(TestSpec) ->
    case catch ct_testspec:collect_tests_from_list(TestSpec, false) of
	{E,CTReason}  when E == error ; E == 'EXIT' ->
	    exit(CTReason);
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

	    case check_and_install_configfiles(Opts#opts.config,
					       which(logdir,Opts#opts.logdir),
					       Opts#opts.event_handlers) of
		ok ->
		    Opts1 = Opts#opts{testspecs = [TestSpec],
				      include = AllInclude},
		    {Run,Skip} = ct_testspec:prepare_tests(TS, node()),
		    do_run(Run, Skip, Opts1, []);
		{error,GCFReason} ->
		    exit(GCFReason)
	    end
    end.

get_data_for_node(#testspec{logdir=LogDirs,
			    cover=CoverFs,
			    config=Cfgs,
			    userconfig=UsrCfgs,
			    event_handler=EvHs,
			    include=Incl,
			    multiply_timetraps=MTs,
			    scale_timetraps=STs}, Node) ->
    LogDir = case proplists:get_value(Node, LogDirs) of
		 undefined -> ".";
		 Dir -> Dir
	     end,
    Cover = proplists:get_value(Node, CoverFs),
    MT = proplists:get_value(Node, MTs),
    ST = proplists:get_value(Node, STs),
    ConfigFiles = [{?ct_config_txt,F} || {N,F} <- Cfgs, N==Node] ++
	[CBF || {N,CBF} <- UsrCfgs, N==Node],
    EvHandlers =  [{H,A} || {N,H,A} <- EvHs, N==Node],
    Include =  [I || {N,I} <- Incl, N==Node],
    #opts{logdir = LogDir,
	  cover = Cover,
	  config = ConfigFiles,
	  event_handlers = EvHandlers,
	  include = Include,
	  multiply_timetraps = MT,
	  scale_timetraps = ST}.

refresh_logs(LogDir) ->
    {ok,Cwd} = file:get_cwd(),
    case file:set_cwd(LogDir) of
	E = {error,_Reason} ->
	    E;
	_ ->
	    case catch ct_logs:make_all_suites_index(refresh) of
		{'EXIT',ASReason} ->
		    file:set_cwd(Cwd),
		    {error,{all_suites_index,ASReason}};
		_ ->
		    case catch ct_logs:make_all_runs_index(refresh) of
			{'EXIT',ARReason} ->
			    file:set_cwd(Cwd),
			    {error,{all_runs_index,ARReason}};
			_ ->
			    file:set_cwd(Cwd),
			    io:format("Logs in ~s refreshed!~n",[LogDir]),
			    ok
		    end
	    end
    end.

which(logdir, undefined) ->
    ".";
which(logdir, Dir) ->
    Dir;
which(multiply_timetraps, undefined) ->
    1;
which(multiply_timetraps, MT) ->
    MT;
which(scale_timetraps, undefined) ->
    false;
which(scale_timetraps, ST) ->
    ST.

choose_val(undefined, V1) ->
    V1;
choose_val(V0, _V1) ->
    V0.

merge_vals(Vs) ->
    lists:append(Vs).

listify([C|_]=Str) when is_integer(C) -> [Str];
listify(L) when is_list(L) -> L;
listify(E) -> [E].

delistify([E]) -> E;
delistify(E)   -> E.


%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:run/3
run(TestDir, Suite, Cases) ->
    install([]),
    do_run(tests(TestDir, Suite, Cases), []).

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:run/2
run(TestDir, Suite) when is_list(TestDir), is_integer(hd(TestDir)) ->
    install([]),
    do_run(tests(TestDir, Suite), []).

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:run/1
run(TestDirs) ->
    install([]),
    do_run(tests(TestDirs), []).

suite_to_test(Suite) ->
    {filename:dirname(Suite),list_to_atom(filename:rootname(filename:basename(Suite)))}.

groups_and_cases(Gs, Cs) when ((Gs == undefined) or (Gs == [])) and
			      ((Cs == undefined) or (Cs == [])) ->
    [];
groups_and_cases(Gs, Cs) when Gs == undefined ; Gs == [] ->
    [list_to_atom(C) || C <- Cs];
groups_and_cases(Gs, Cs) when Cs == undefined ; Cs == [] ->
    [{list_to_atom(G),all} || G <- Gs];
groups_and_cases([G], Cs) ->
    [{list_to_atom(G),[list_to_atom(C) || C <- Cs]}];
groups_and_cases([_,_|_] , Cs) when Cs =/= [] ->
    {error,multiple_groups_and_cases};
groups_and_cases(_Gs, _Cs) ->
    {error,incorrect_group_or_case_option}.

tests(TestDir, Suites, []) when is_list(TestDir), is_integer(hd(TestDir)) ->
    [{?testdir(TestDir,Suites),ensure_atom(Suites),all}];
tests(TestDir, Suite, Cases) when is_list(TestDir), is_integer(hd(TestDir)) ->
    [{?testdir(TestDir,Suite),ensure_atom(Suite),Cases}].
tests([{Dir,Suite}],Cases) ->
    [{?testdir(Dir,Suite),ensure_atom(Suite),Cases}];
tests(TestDir, Suite) when is_list(TestDir), is_integer(hd(TestDir)) ->
    tests(TestDir, ensure_atom(Suite), all).
tests(DirSuites) when is_list(DirSuites), is_tuple(hd(DirSuites)) ->
    [{?testdir(Dir,Suite),ensure_atom(Suite),all} || {Dir,Suite} <- DirSuites];
tests(TestDir) when is_list(TestDir), is_integer(hd(TestDir)) ->
    tests([TestDir]);
tests(TestDirs) when is_list(TestDirs), is_list(hd(TestDirs)) ->
    [{?testdir(TestDir,all),all,all} || TestDir <- TestDirs].

do_run(Tests, Misc) when is_list(Misc) ->
    do_run(Tests, Misc, ".").

do_run(Tests, Misc, LogDir) when is_list(Misc) ->
    Opts =
	case proplists:get_value(step, Misc) of
	    undefined ->
		#opts{};
	    StepOpts ->
		#opts{step = StepOpts}
	end,
    Opts1 =
	case proplists:get_value(cover, Misc) of
	    undefined ->
		Opts;
	    CoverFile ->
		Opts#opts{cover = CoverFile}
	end,
    do_run(Tests, [], Opts1#opts{logdir = LogDir}, []).

do_run(Tests, Skip, Opts, Args) ->
    #opts{cover = Cover} = Opts,
    case code:which(test_server) of
	non_existing ->
	    exit({error,no_path_to_test_server});
	_ ->
	    Opts1 = if Cover == undefined ->
			    Opts;
		       true ->
			    case ct_cover:get_spec(Cover) of
				{error,Reason} ->
				    exit({error,Reason});
				CoverSpec ->
				    Opts#opts{coverspec = CoverSpec}
			    end
		    end,
	    %% This env variable is used by test_server to determine
	    %% which framework it runs under.
	    case os:getenv("TEST_SERVER_FRAMEWORK") of
		false ->
		    os:putenv("TEST_SERVER_FRAMEWORK", "ct_framework");
		"ct_framework" ->
		    ok;
		Other ->
		    erlang:display(list_to_atom("Note: TEST_SERVER_FRAMEWORK = " ++ Other))
	    end,
	    case ct_util:start(Opts#opts.logdir) of
		{error,interactive_mode} ->
		    io:format("CT is started in interactive mode. "
			      "To exit this mode, run ct:stop_interactive().\n"
			      "To enter the interactive mode again, "
			      "run ct:start_interactive()\n\n",[]),
		    {error,interactive_mode};
		_Pid ->
		    %% save style sheet info
		    case lists:keysearch(stylesheet, 1, Args) of
			{value,{_,SSFile}} ->
			    ct_util:set_testdata({stylesheet,SSFile});
			_ ->
			    ct_util:set_testdata({stylesheet,undefined})
		    end,

		    case lists:keysearch(silent_connections, 1, Args) of
			{value,{silent_connections,undefined}} ->
			    ok;
			{value,{silent_connections,[]}} ->
			    Conns = ct_util:override_silence_all_connections(),
			    ct_logs:log("Silent connections", "~p", [Conns]);
			{value,{silent_connections,Cs}} ->
			    Conns = lists:map(fun(S) when is_list(S) ->
						      list_to_atom(S);
						 (A) -> A
					      end, Cs),
			    ct_util:override_silence_connections(Conns),
			    ct_logs:log("Silent connections", "~p", [Conns]);
			_ ->
			    ok
		    end,
		    log_ts_names(Opts1#opts.testspecs),
		    TestSuites = suite_tuples(Tests),

		    {SuiteMakeErrors,AllMakeErrors} =
			case application:get_env(common_test, auto_compile) of
			    {ok,false} ->
				SuitesNotFound = verify_suites(TestSuites),
				{SuitesNotFound,SuitesNotFound};
			    _ ->
				{SuiteErrs,HelpErrs} = auto_compile(TestSuites),
				{SuiteErrs,SuiteErrs++HelpErrs}
			end,

		    case continue(AllMakeErrors) of
			true ->
			    SavedErrors = save_make_errors(SuiteMakeErrors),
			    ct_repeat:log_loop_info(Args),
			    {Tests1,Skip1} = final_tests(Tests,[],Skip,SavedErrors),
			    R = do_run_test(Tests1, Skip1, Opts1),
			    ct_util:stop(normal),
			    R;
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
		    end
	    end
    end.

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
			      case run_make(suites, TestDir, Suite, UserInclude) of
				  {error,{make_failed,Bad}} ->
				      [{TS,Bad}];
				  {error,_} ->
				      [{TS,[filename:join(TestDir,"*_SUITE")]}];
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
	fun({Dir,Suite},NotFound) ->
		case locate_test_dir(Dir, Suite) of
		    {ok,TestDir} ->
			if Suite == all ->
				NotFound;
			   true ->
				Beam = filename:join(TestDir, atom_to_list(Suite)++".beam"),
				case filelib:is_regular(Beam) of
				    true  ->
					NotFound;
				    false ->
					Name = filename:join(TestDir, atom_to_list(Suite)),
					io:format("Suite ~w not found in directory ~s~n",
						  [Suite,TestDir]),
					[{{Dir,Suite},[Name]} | NotFound]
				end
			end;
		    {error,_Reason} ->
			io:format("Directory ~s is invalid~n", [Dir]),
			Name = filename:join(Dir, atom_to_list(Suite)),
			[{{Dir,Suite},[Name]} | NotFound]
		end
	end,
    lists:reverse(lists:foldl(Verify, [], TestSuites)).

save_make_errors([]) ->
    [];
save_make_errors(Errors) ->
    Suites = get_bad_suites(Errors,[]),
    ct_logs:log("MAKE RESULTS",
		"Error compiling or locating the following suites: ~n~p",[Suites]),
    %% save the info for logger
    file:write_file(?missing_suites_info,term_to_binary(Errors)),
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
step(TestDir, Suite, Case, Opts) when is_list(TestDir), is_atom(Suite), is_atom(Case),
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

final_tests([{TestDir,Suites,_}|Tests],
	    Final, Skip, Bad) when is_list(Suites), is_atom(hd(Suites)) ->
%     Separate =
% 	fun(S,{DoSuite,Dont}) ->		
% 		case lists:keymember({TestDir,S},1,Bad) of
% 		    false ->	
% 			{[S|DoSuite],Dont};
% 		    true ->	
% 			SkipIt = {TestDir,S,"Make failed"},
% 			{DoSuite,Dont++[SkipIt]}
% 		end
% 	end,
	
%     {DoSuites,Skip1} =
% 	lists:foldl(Separate,{[],Skip},Suites),
%     Do = {TestDir,lists:reverse(DoSuites),all},

    Skip1 = [{TD,S,"Make failed"} || {{TD,S},_} <- Bad, S1 <- Suites,
				     S == S1, TD == TestDir],
    Final1 = [{TestDir,S,all} || S <- Suites],
    final_tests(Tests, lists:reverse(Final1)++Final, Skip++Skip1, Bad);

final_tests([{TestDir,all,all}|Tests], Final, Skip, Bad) ->
    MissingSuites =
	case lists:keysearch({TestDir,all}, 1, Bad) of
	    {value,{_,Failed}} ->
		[list_to_atom(filename:basename(F)) || F <- Failed];
	    false ->
		[]
	end,
    Missing = [{TestDir,S,"Make failed"} || S <- MissingSuites],
    Final1 = [{TestDir,all,all}|Final],
    final_tests(Tests, Final1, Skip++Missing, Bad);

final_tests([{TestDir,Suite,Cases}|Tests],
	    Final, Skip, Bad) when Cases==[]; Cases==all  ->
    final_tests([{TestDir,[Suite],all}|Tests], Final, Skip, Bad);

final_tests([{TestDir,Suite,Cases}|Tests], Final, Skip, Bad) ->
    case lists:keymember({TestDir,Suite}, 1, Bad) of
	false ->
	    Do = {TestDir,Suite,Cases},
	    final_tests(Tests, [Do|Final], Skip, Bad);
	true ->
	    Do = {TestDir,Suite,Cases},
	    Skip1 = Skip ++ [{TestDir,Suite,Cases,"Make failed"}],
	    final_tests(Tests, [Do|Final], Skip1, Bad)
    end;

final_tests([], Final, Skip, _Bad) ->
    {lists:reverse(Final),Skip}.

continue([]) ->
    true;
continue(_MakeErrors) ->
    io:nl(),
    OldGl = group_leader(),
    case set_group_leader_same_as_shell() of
	true ->
	    S = self(),
	    io:format("Failed to compile or locate one or more test suites\n"
		      "Press \'c\' to continue or \'a\' to abort.\n"
		      "Will continue in 15 seconds if no answer is given!\n"),
	    Pid = spawn(fun() ->
				case io:get_line('(c/a) ') of
				    "c\n" ->
					S ! true;
				    _ ->
					S ! false
				end
			end),
	    group_leader(OldGl, self()),
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

set_group_leader_same_as_shell() ->
    %%! Locate the shell process... UGLY!!!
    GS2or3 = fun(P) ->
		     case process_info(P,initial_call) of
			 {initial_call,{group,server,X}} when X == 2 ; X == 3 ->
			     true;
			 _ ->
			     false
		     end
	     end,	
    case [P || P <- processes(), GS2or3(P),
	       true == lists:keymember(shell,1,element(2,process_info(P,dictionary)))] of
	[GL|_] ->
	    group_leader(GL, self());
	[] ->
	    false
    end.

check_and_add([{TestDir0,M,_} | Tests], Added) ->
    case locate_test_dir(TestDir0, M) of
	{ok,TestDir} ->
	    case lists:member(TestDir, Added) of
		true ->
		    check_and_add(Tests, Added);
		false ->
		    true = code:add_patha(TestDir),
		    check_and_add(Tests, [TestDir|Added])
	    end;
	{error,_} ->
	    {error,{invalid_directory,TestDir0}}
    end;
check_and_add([], _) ->
    ok.

do_run_test(Tests, Skip, Opts) ->
    case check_and_add(Tests, []) of
	ok ->
	    ct_util:set_testdata({stats,{0,0,{0,0}}}),
	    ct_util:set_testdata({cover,undefined}),
	    test_server_ctrl:start_link(local),
	    case Opts#opts.coverspec of
		CovData={CovFile,
			 CovNodes,
			 _CovImport,
			 CovExport,
			 #cover{app        = CovApp,
				level      = CovLevel,
				excl_mods  = CovExcl,
				incl_mods  = CovIncl,
				cross      = CovCross,
				src        = _CovSrc}} ->
		    ct_logs:log("COVER INFO","Using cover specification file: ~s~n"
				"App: ~w~n"
				"Cross cover: ~w~n"
				"Including ~w modules~n"
				"Excluding ~w modules",
				[CovFile,CovApp,CovCross,length(CovIncl),length(CovExcl)]),

		    %% cover export file will be used for export and import
		    %% between tests so make sure it doesn't exist initially
		    case filelib:is_file(CovExport) of
			true ->
			    DelResult = file:delete(CovExport),
			    ct_logs:log("COVER INFO",
					"Warning! Export file ~s already exists. "
					"Deleting with result: ~p",
					[CovExport,DelResult]);
			false ->
			    ok
		    end,

		    %% tell test_server which modules should be cover compiled
		    %% note that actual compilation is done when tests start
		    test_server_ctrl:cover(CovApp, CovFile, CovExcl, CovIncl,
					   CovCross, CovExport, CovLevel),
		    %% save cover data (used e.g. to add nodes dynamically)
		    ct_util:set_testdata({cover,CovData}),
		    %% start cover on specified nodes
		    if (CovNodes /= []) and (CovNodes /= undefined) ->
			    ct_logs:log("COVER INFO",
					"Nodes included in cover session: ~w",
					[CovNodes]),
			    cover:start(CovNodes);
		       true ->
			    ok
		    end,
		    true;
		_ ->
		    false
	    end,
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
		    io:format("~nTEST INFO: ~w test(s), ~w case(s) in ~w suite(s)~n~n",
			      [NoOfTests,NoOfCases,NoOfSuites]),
		    ct_logs:log("TEST INFO","~w test(s), ~w case(s) in ~w suite(s)",
				[NoOfTests,NoOfCases,NoOfSuites])
	    end,

	    test_server_ctrl:multiply_timetraps(Opts#opts.multiply_timetraps),
	    test_server_ctrl:scale_timetraps(Opts#opts.scale_timetraps),

	    ct_event:notify(#event{name=start_info,
				   node=node(),
				   data={NoOfTests,NoOfSuites,NoOfCases}}),
	    CleanUp = add_jobs(Tests, Skip, Opts, []),
	    unlink(whereis(test_server_ctrl)),
	    catch test_server_ctrl:wait_finish(),
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
			  end, CleanUp);
	Error ->
	    Error
    end.

delete_dups([S | Suites]) ->
    Suites1 = lists:delete(S, Suites),
    [S | delete_dups(Suites1)];
delete_dups([]) ->
    [].

count_test_cases(Tests, Skip) ->
    SendResult = fun(Me, Result) -> Me ! {no_of_cases,Result} end,
    TSPid = test_server_ctrl:start_get_totals(SendResult),
    Ref = erlang:monitor(process, TSPid),
    add_jobs(Tests, Skip, #opts{}, []),
    {Suites,NoOfCases} = count_test_cases1(length(Tests), 0, [], Ref),
    erlang:demonitor(Ref),
    test_server_ctrl:stop_get_totals(),
    {Suites,NoOfCases}.

count_test_cases1(0, N, Suites, _) ->
    {lists:flatten(Suites), N};
count_test_cases1(Jobs, N, Suites, Ref) ->
    receive
	{no_of_cases,{Ss,N1}} ->
	    count_test_cases1(Jobs-1, add_known(N,N1), [Ss|Suites], Ref);
	{'DOWN', Ref, _, _, _} ->
	    {[],0}
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
	    wait_for_idle(),
	    add_jobs(Tests, Skip, Opts, CleanUp)
    end;
add_jobs([{TestDir,[Suite],all}|Tests], Skip, Opts, CleanUp) when is_atom(Suite) ->
    add_jobs([{TestDir,Suite,all}|Tests], Skip, Opts, CleanUp);
add_jobs([{TestDir,Suites,all}|Tests], Skip, Opts, CleanUp) when is_list(Suites) ->
    Name = get_name(TestDir) ++ ".suites",
    case catch test_server_ctrl:add_module_with_skip(Name, Suites,
						     skiplist(TestDir,Skip)) of
	{'EXIT',_} ->
	    CleanUp;
	_ ->
	    wait_for_idle(),
	    add_jobs(Tests, Skip, Opts, CleanUp)
    end;
add_jobs([{TestDir,Suite,all}|Tests], Skip, Opts, CleanUp) ->
    case maybe_interpret(Suite, all, Opts) of
	ok ->
	    Name =  get_name(TestDir) ++ "." ++ atom_to_list(Suite),
	    case catch test_server_ctrl:add_module_with_skip(Name, [Suite],
							     skiplist(TestDir,Skip)) of
		{'EXIT',_} ->
		    CleanUp;
		_ ->
		    wait_for_idle(),
		    add_jobs(Tests, Skip, Opts, [Suite|CleanUp])
	    end;
	Error ->
	    Error
    end;
add_jobs([{TestDir,Suite,[Case]}|Tests], Skip, Opts, CleanUp) when is_atom(Case) ->
    add_jobs([{TestDir,Suite,Case}|Tests], Skip, Opts, CleanUp);
add_jobs([{TestDir,Suite,Cases}|Tests], Skip, Opts, CleanUp) when is_list(Cases) ->
    case maybe_interpret(Suite, Cases, Opts) of
	ok ->
	    Name =  get_name(TestDir) ++ "." ++	atom_to_list(Suite) ++ ".cases",
	    case catch test_server_ctrl:add_cases_with_skip(Name, Suite, Cases,
							    skiplist(TestDir,Skip)) of
		{'EXIT',_} ->
		    CleanUp;
		_ ->
		    wait_for_idle(),
		    add_jobs(Tests, Skip, Opts, [Suite|CleanUp])
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
							   skiplist(TestDir,Skip)) of
		{'EXIT',_} ->
		    CleanUp;
		_ ->
		    wait_for_idle(),
		    add_jobs(Tests, Skip, Opts, [Suite|CleanUp])
	    end;
	Error ->
	    Error
    end;
add_jobs([], _, _, CleanUp) ->
    CleanUp.

wait_for_idle() ->
    ct_util:update_last_run_index(),
    Notify = fun(Me) -> Me ! idle end,
    case catch test_server_ctrl:idle_notify(Notify) of
	{'EXIT',_} ->
	    error;
	TSPid ->
	    %% so we don't hang forever if test_server dies
	    Ref = erlang:monitor(process, TSPid),
	    Result = receive
			 idle -> ok;
			 {'DOWN', Ref, _, _, _} -> error
		     end,
	    erlang:demonitor(Ref),
	    ct_util:update_last_run_index(),
	    Result
    end.

skiplist(Dir, [{Dir,all,Cmt}|Skip]) ->
    %% we need to turn 'all' into list of modules since
    %% test_server doesn't do skips on Dir level
    Ss = filelib:wildcard(filename:join(Dir, "*_SUITE.beam")),
    [{list_to_atom(filename:basename(S,".beam")),Cmt} || S <- Ss] ++ skiplist(Dir,Skip);
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
	    TestServerInclude = get_dir(test_server, "include"),
	    CtInclude = get_dir(common_test, "include"),
	    XmerlInclude = get_dir(xmerl, "include"),
	    ErlFlags = UserInclude ++ [{i,TestServerInclude},
				       {i,CtInclude},
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
				Files = lists:flatmap(fun({F,out_of_date}) ->
							      case FileTest(F, Targets) of
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
    [i:ib(Suite, Case, 1) || Case <- Cases],
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
	    i:ib(Suite, init_per_suite, 1),
	    i:ib(Suite, init_per_testcase, 2),
	    i:ib(Suite, end_per_testcase, 2),
	    i:ib(Suite, end_per_suite, 1);
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
    ct_logs:log("Test Specification file(s)", "~s",
		[lists:flatten(List)]).

merge_arguments(Args) ->
    merge_arguments(Args, []).

merge_arguments([LogDir={logdir,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(replace, LogDir, Merged));

merge_arguments([CoverFile={cover,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(replace, CoverFile, Merged));

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
    case lists:keysearch(Key, 1, Args) of
	{value,{Key,Val}} when is_function(IfExists) ->
	    IfExists(Val);
	{value,{Key,Val}} when IfExists == value ->
	    Val;
	{value,{Key,_Val}} ->
	    IfExists;
	_ when is_function(IfNotExists) ->
	    IfNotExists();
	_ ->
	    IfNotExists
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
    [{list_to_atom(EH),lists:flatten(io_lib:format("~s",[Arg]))} |
     event_handler_init_args2opts(EHs)];
event_handler_init_args2opts([EH, Arg]) ->
    [{list_to_atom(EH),lists:flatten(io_lib:format("~s",[Arg]))}];
event_handler_init_args2opts([]) ->
    [].

%% this function translates ct:run_test/1 start options
%% to run_test start arguments (on the init arguments format) -
%% this is useful mainly for testing the ct_run start functions
opts2args(EnvStartOpts) ->
    lists:flatmap(fun({config,CfgFiles}) ->
			  [{ct_config,[CfgFiles]}];
		     ({userconfig,{CBM,CfgStr=[X|_]}}) when is_integer(X) ->
			  [{userconfig,[atom_to_list(CBM),CfgStr]}];
		     ({userconfig,{CBM,CfgStrs}}) when is_list(CfgStrs) ->
			  [{userconfig,[atom_to_list(CBM) | CfgStrs]}];
		     ({userconfig,UserCfg}) when is_list(UserCfg) ->
			  Strs =
			      lists:map(fun({CBM,CfgStr=[X|_]}) when is_integer(X) ->
						[atom_to_list(CBM),CfgStr,"and"];
					   ({CBM,CfgStrs}) when is_list(CfgStrs) ->
						[atom_to_list(CBM) | CfgStrs] ++ ["and"]
					end, UserCfg),
			  [_LastAnd|StrsR] = lists:reverse(lists:flatten(Strs)),
			  [{userconfig,lists:reverse(StrsR)}];
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
		     ({auto_compile,false}) ->
			  [{no_auto_compile,[]}];
		     ({auto_compile,true}) ->
			  [];
		     ({scale_timetraps,true}) ->
			  [{scale_timetraps,[]}];
		     ({scale_timetraps,false}) ->
			  [];
		     ({force_stop,true}) ->
			  [{force_stop,[]}];
		     ({force_stop,false}) ->
			  [];
		     ({decrypt,{key,Key}}) ->
			  [{ct_decrypt_key,[Key]}];
		     ({decrypt,{file,File}}) ->
			  [{ct_decrypt_file,[File]}];
		     ({basic_html,true}) ->
			  ({basic_html,[]});
		     ({basic_html,false}) ->
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
			  Strs = lists:map(fun(EH) ->
						   [atom_to_list(EH),ArgStr,"and"]
					   end, EHs),
			  [_LastAnd|StrsR] = lists:reverse(lists:flatten(Strs)),
			  [{event_handler_init,lists:reverse(StrsR)}];
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
    %%! this needs to be updated to handle testcase groups later!!
    case catch Suite:all() of
	{'EXIT',Why} ->
	    {error,Why};
	{skip,_} ->
	    [];
	Cases ->
	    AllCases =
		lists:foldl(fun({sequence,SeqName}, All) ->
				    case catch Suite:sequences() of
					{'EXIT',_} ->
					    All;
					Seqs ->
					    case proplists:get_value(SeqName, Seqs) of
						undefined ->
						    All;
						SeqCases ->
						    lists:reverse(SeqCases) ++ All
					    end
				    end;
			       (Case,All) ->
				    [Case|All]
			    end, [], Cases),
	    lists:reverse(AllCases)
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
		    io:format("Warning! Tracing not started. Reason: ~p~n~n",
			      [Error]),
		    false
	    end;
	false ->
	    false		
    end.

do_trace(Terms) ->
    dbg:tracer(),
    dbg:p(self(), [sos,call]),
    lists:foreach(fun({m,M}) ->
			  case dbg:tpl(M,[{'_',[],[{return_trace}]}]) of
			      {error,What} -> exit({error,{tracing_failed,What}});
			      _ -> ok
			  end;
		     ({f,M,F}) ->
			  case dbg:tpl(M,F,[{'_',[],[{return_trace}]}]) of
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
