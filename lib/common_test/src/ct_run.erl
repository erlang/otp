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

%%%-----------------------------------------------------------------
%%% @spec script_start() -> void()
%%%
%%% @doc Start tests via the run_test script.
%%% 
%%% <p>Example:<br/><code>./run_test -config config.ctc -dir
%%% $TEST_DIR</code></p>
%%%
%%% <p>Example:<br/><code>./run_test -config config.ctc -suite
%%% $SUITE_PATH/$SUITE_NAME [-case $CASE_NAME]</code></p>
%%%
script_start() ->
    process_flag(trap_exit, true),
    Args = merge_arguments(init:get_arguments()),
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
    case lists:keymember(preload, 1, Args) of
	true -> preload();
	false -> ok
    end,

    VtsOrShell = 
	case lists:keymember(vts, 1, Args) of
	    true -> 
		vts;
	    false ->
		case lists:keymember(shell, 1, Args) of
		    true -> shell;
		    false -> false
		end
	end,
    LogDir =
	case lists:keysearch(logdir, 1, Args) of
	    {value,{logdir,[LogD]}} -> LogD;
	    false -> "."
	end,
    EvHandlers =	
	case lists:keysearch(event_handler, 1, Args) of
	    {value,{event_handler,Handlers}} -> 
		lists:map(fun(H) -> {list_to_atom(H),[]} end, Handlers);
	    false -> 
		[]
	end,
    Cover = 
	case lists:keysearch(cover, 1, Args) of
	    {value,{cover,CoverFile}} -> 
		{cover,?abs(CoverFile)};
	    false -> 
		false
	end,

    case lists:keysearch(ct_decrypt_key, 1, Args) of
	{value,{_,[DecryptKey]}} ->
	    application:set_env(common_test, decrypt, {key,DecryptKey});
	false ->
	    case lists:keysearch(ct_decrypt_file, 1, Args) of
		{value,{_,[DecryptFile]}} ->
		    application:set_env(common_test, decrypt, 
					{file,filename:absname(DecryptFile)});
		false ->
		    application:unset_env(common_test, decrypt)
	    end
    end,

    case lists:keysearch(no_auto_compile, 1, Args) of
	{value,_} ->
	    application:set_env(common_test, auto_compile, false);
	false ->
	    application:set_env(common_test, auto_compile, true),

	    InclDirs =
		case lists:keysearch(include,1,Args) of
		    {value,{include,Incl}} when is_list(hd(Incl)) ->
			Incl;		    
		    {value,{include,Incl}} when is_list(Incl) ->
			[Incl];
		    false ->
			[]
		end,
	    case os:getenv("CT_INCLUDE_PATH") of
		false ->
		    application:set_env(common_test, include, InclDirs);
		CtInclPath ->
		    InclDirs1 = string:tokens(CtInclPath,[$:,$ ,$,]),
		    application:set_env(common_test, include, InclDirs1++InclDirs)
	    end
    end,

    case lists:keysearch(basic_html, 1, Args) of
	{value,_} ->
	    application:set_env(common_test, basic_html, true);
	false ->
	    application:set_env(common_test, basic_html, false)
    end,

    Result =
	case lists:keysearch(refresh_logs, 1, Args) of
	    {value,{refresh_logs,Refresh}} ->
		LogDir1 = case Refresh of
			      [] -> LogDir;
			      [RefreshDir] -> ?abs(RefreshDir)
			  end,
		{ok,Cwd} = file:get_cwd(),
		file:set_cwd(LogDir1),
		timer:sleep(500),		% give the shell time to print version etc
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
		end;
	    false ->    
		ConfigFiles = case lists:keysearch(ct_config, 1, Args) of
		    {value,{ct_config,Files}}->
			[{ct_config_plain, Files}];
		    false->
			[]
		end,
		UserConfigs = case lists:keysearch(userconfig, 1, Args) of
		    {value,{userconfig,UserConfigFiles}}->
			prepare_user_configs(UserConfigFiles, [], new);
		    false->
			[]
		end,
		Config = ConfigFiles ++ UserConfigs,
		case Config of
		    [] ->
			case install([{config,[]},
				      {event_handler,EvHandlers}],
				     LogDir) of
			    ok ->
				script_start2(VtsOrShell, [], EvHandlers,
					      Args, LogDir, Cover);
			    Error ->
				Error
			end;
		    Config ->
			case lists:keysearch(spec, 1, Args) of
			    false ->
				case check_and_install_configfiles(Config,
						LogDir, EvHandlers) of
				    ok ->
					script_start2(VtsOrShell, Config,
						      EvHandlers, Args, LogDir,
						      Cover);
				    Error ->
					Error
				end;
			    _ ->
				script_start2(VtsOrShell, Config,
					      EvHandlers, Args, LogDir, Cover)
			end
		end
	end,
    Parent ! {self(), Result}.

prepare_user_configs([ConfigString|UserConfigs], Acc, new)->
    prepare_user_configs(UserConfigs,
			 [{list_to_atom(ConfigString), []}|Acc],
			 cur);
prepare_user_configs(["and"|UserConfigs], Acc, _)->
    prepare_user_configs(UserConfigs, Acc, new);
prepare_user_configs([ConfigString|UserConfigs], [{LastMod, LastList}|Acc], cur)->
    prepare_user_configs(UserConfigs,
			 [{LastMod, [ConfigString|LastList]}|Acc],
			 cur);
prepare_user_configs([], Acc, _)->
    Acc.

check_and_install_configfiles(Configs, LogDir, EvHandlers) ->
    % Configs is list of tuples such as {Callback=atom(), Files=list()}
    % The ugly code below checks:
    % 1. that all config files are present
    % 2. thar all callback modules are loadable
    case lists:keysearch(nok, 1,
	    lists:flatten(
		lists:map(fun({Callback, Files})->
		    case code:load_file(Callback) of
			{module, Callback}->
			    lists:map(fun(File)->
				case filelib:is_file(File) of
				    true->
					{ok, File};
				    false->
					{nok, {config, File}}
				end
			    end,
			    Files);
			{error, _}->
			    {nok, {callback, Callback}}
		    end
		end,
		Configs))) of
    false->
	install([{config,Configs},
		 {event_handler,EvHandlers}], LogDir);
    {value, {nok, {config, File}}} ->
	{error,{cant_read_config_file,File}};
    {value, {nok, {callback, File}}} ->
	{error,{cant_load_callback_module,File}}
    end.

script_start2(false, ConfigFiles, EvHandlers, Args, LogDir, Cover) ->
    case lists:keysearch(spec, 1, Args) of
	{value,{spec,[]}} ->
	    {error,no_testspec_specified};
	{value,{spec,Specs}} ->
	    Relaxed = lists:keymember(allow_user_terms, 1, Args),
	    %% using testspec as input for test
	    case catch ct_testspec:collect_tests_from_file(Specs, Relaxed) of
		{error,Reason} ->
		    {error,Reason};
		TS ->
		    {LogDir1,TSCoverFile,ConfigFiles1,EvHandlers1,Include1} = 
			get_data_for_node(TS,node()),
		    UserInclude =
			case application:get_env(common_test, include) of
			    {ok,Include} -> Include++Include1;
			    _ -> Include1
			end,
		    application:set_env(common_test, include, UserInclude),
		    LogDir2 = which_logdir(LogDir,LogDir1),
		    CoverOpt = case {Cover,TSCoverFile} of
				   {false,undef} -> [];
				   {_,undef} ->     [Cover];
				   {false,_} ->     [{cover,TSCoverFile}]
			       end,
		    case check_and_install_configfiles(
					 ConfigFiles++ConfigFiles1, LogDir2,
					 EvHandlers++EvHandlers1) of
			ok ->
			    {Run,Skip} = ct_testspec:prepare_tests(TS, node()),
			    do_run(Run, Skip, CoverOpt, Args, LogDir2);
			Error ->
			    Error
		    end
	    end;
	false ->
	    script_start3(false, ConfigFiles, EvHandlers, Args, LogDir, Cover)
    end;
script_start2(VtsOrShell, ConfigFiles, EvHandlers, Args, LogDir, Cover) ->
    script_start3(VtsOrShell, ConfigFiles, EvHandlers, Args, LogDir, Cover).

script_start3(VtsOrShell, ConfigFiles, EvHandlers, Args, LogDir, Cover) ->
    case lists:keysearch(dir, 1, Args) of
	{value,{dir,[]}} ->
	    {error,no_dir_specified};
	{value,{dir,Dirs}} ->
	    script_start4(VtsOrShell, ConfigFiles, EvHandlers, tests(Dirs),
			  Cover, Args, LogDir);
	false ->
	    case lists:keysearch(suite, 1, Args) of
		{value,{suite,[]}} ->
		    {error,no_suite_specified};
		{value,{suite,Suites}} ->
		    StepOrCover =
			case lists:keysearch(step, 1, Args) of
			    {value,Step} -> Step;
			    false -> Cover
			end,
		    S2M = fun(S) ->
				  {filename:dirname(S),
				   list_to_atom(
				     filename:rootname(filename:basename(S)))}
			  end,
		    DirMods = lists:map(S2M, Suites),
		    {Specified,GroupsAndCases} =
			case {lists:keysearch(group, 1, Args),
			      lists:keysearch('case', 1, Args)} of
			    {{value,{_,Gs}},{value,{_,Cs}}} -> {true,Gs++Cs};
			    {{value,{_,Gs}},_}              -> {true,Gs};
			    {_,{value,{_,Cs}}}              -> {true,Cs};
			    _                               -> {false,[]}
			end,
		    if Specified, length(GroupsAndCases) == 0 ->
			    {error,no_case_or_group_specified};
		       Specified, length(DirMods) > 1 ->
			    {error,multiple_suites_and_cases};
		       length(GroupsAndCases) > 0, length(DirMods) == 1 ->
			    GsAndCs = lists:map(fun(C) -> list_to_atom(C) end, 
						GroupsAndCases),
			    script_start4(VtsOrShell, ConfigFiles, EvHandlers,
					  tests(DirMods, GsAndCs),
					  StepOrCover, Args, LogDir);
		       not Specified, length(DirMods) > 0 ->
			    script_start4(VtsOrShell, ConfigFiles, EvHandlers,
					  tests(DirMods), 
					  StepOrCover, Args, LogDir);
		       true ->
			    {error,incorrect_suite_and_case_options}
		    end;
		false when VtsOrShell=/=false ->
		    script_start4(VtsOrShell, ConfigFiles, EvHandlers,
				  [], Cover, Args, LogDir);
		false ->
		    script_usage(),
		    {error,incorrect_usage}
	    end
    end.

script_start4(vts, ConfigFiles, EvHandlers, Tests, false, _Args, LogDir) ->
    vts:init_data(ConfigFiles, EvHandlers, ?abs(LogDir), Tests);
script_start4(shell, ConfigFiles, EvHandlers, _Tests, false, Args, LogDir) ->
    Opts = [{config,ConfigFiles},{event_handler,EvHandlers}],
    if ConfigFiles == [] ->
	    ok;
       true ->
	    io:format("\nInstalling: ~p\n\n", [ConfigFiles])
    end,
    case install(Opts) of
	ok ->
	    ct_util:start(interactive, LogDir),
	    log_ts_names(Args),
	    io:nl(),
	    ok;
	Error ->
	    Error
    end;
script_start4(vts, _CfgFs, _EvHs, _Tests, _Cover={cover,_}, _Args, _LogDir) ->
    %% Add support later (maybe).
    script_usage(),
    erlang:halt();
script_start4(shell, _CfgFs, _EvHs, _Tests, _Cover={cover,_}, _Args, _LogDir) ->
    %% Add support later (maybe).
    script_usage();
script_start4(false, _CfgFs, _EvHs, Tests, Cover={cover,_}, Args, LogDir) ->
    do_run(Tests, [], [Cover], Args, LogDir);
script_start4(false, _ConfigFiles, _EvHandlers, Tests, false, Args, LogDir) ->
    do_run(Tests, [], [], Args, LogDir);
script_start4(false, _ConfigFiles, _EvHandlers, Test, Step, Args, LogDir) ->
    do_run(Test, [], [Step], Args, LogDir);
script_start4(vts, _ConfigFiles, _EvHandlers, _Test, _Step, _Args, _LogDir) ->
    script_usage(),
    erlang:halt();
script_start4(shell, _ConfigFiles, _EvHandlers, _Test, _Step, _Args, _LogDir) ->
    script_usage().

%%%-----------------------------------------------------------------
%%% @spec script_usage() -> ok
%%% @doc Print script usage information for <code>run_test</code>.
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
%%% @hidden
%%% @equiv ct:run_test/1

%%   Opts = [OptTuples]
%%   OptTuples = {config,CfgFiles} | {dir,TestDirs} | {suite,Suites} |
%%               {testcase,Cases} | {spec,TestSpecs} | {allow_user_terms,Bool} |
%%               {logdir,LogDir} | {cover,CoverSpecFile} | {step,StepOpts} |
%%               {silent_connections,Conns} | {event_handler,EventHandlers} | 
%%               {include,InclDirs} | {auto_compile,Bool} | 
%%               {repeat,N} | {duration,DurTime} | {until,StopTime} | {force_stop,Bool} |
%%               {decrypt,KeyOrFile}

run_test(Opt) when is_tuple(Opt) ->
    run_test([Opt]);

run_test(Opts) when is_list(Opts) ->
    case lists:keysearch(refresh_logs, 1, Opts) of
	{value,{_,RefreshDir}} ->
	    refresh_logs(?abs(RefreshDir)),
	    ok;
	false ->
	    Tracing = start_trace(Opts),
	    {ok,Cwd} = file:get_cwd(),
	    io:format("~nCommon Test starting (cwd is ~s)~n~n", [Cwd]),	    
	    Res =
		case ct_repeat:loop_test(func, Opts) of
		    false ->
			case catch run_test1(Opts) of
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
	    Res
    end.

run_test1(Opts) ->
    LogDir =
	case lists:keysearch(logdir, 1, Opts) of
	    {value,{_,LD}} when is_list(LD) -> LD;
	    false -> "."
	end,
    CfgFiles = ct_config:get_config_file_list(Opts),
    EvHandlers =
	case lists:keysearch(event_handler, 1, Opts) of
	    {value,{_,H}} when is_atom(H) ->
		[{H,[]}];
	    {value,{_,H}} ->
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
			    end, Hs));
	    _ ->
		[]
	end,
    SilentConns =
	case lists:keysearch(silent_connections, 1, Opts) of
	    {value,{_,all}} ->
		[];
	    {value,{_,Conns}} ->
		Conns;
	    _ ->
		undefined
	end,
    Cover = 
	case lists:keysearch(cover, 1, Opts) of
	    {value,{_,CoverFile}} ->
		[{cover,?abs(CoverFile)}];
	    _ ->
		[]
	end,
    Include =
	case lists:keysearch(auto_compile, 1, Opts) of
	    {value,{auto_compile,ACBool}} ->
		application:set_env(common_test, auto_compile, ACBool),
		[];
	    _ ->
		application:set_env(common_test, auto_compile, true),		
		InclDirs =
		    case lists:keysearch(include, 1, Opts) of
			{value,{include,Incl}}  when is_list(hd(Incl)) ->
			    Incl;		    
			{value,{include,Incl}} when is_list(Incl) ->
			    [Incl];
			false ->
			    []
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
		end
	end,

    case lists:keysearch(decrypt, 1, Opts) of
	{value,{_,Key={key,_}}} ->
	    application:set_env(common_test, decrypt, Key);
	{value,{_,{file,KeyFile}}} ->
	    application:set_env(common_test, decrypt, {file,filename:absname(KeyFile)});
	false ->
	    application:unset_env(common_test, decrypt)
    end,

    case lists:keysearch(basic_html, 1, Opts) of
	{value,{basic_html,BasicHtmlBool}} ->
	    application:set_env(common_test, basic_html, BasicHtmlBool);
	_ ->
	    application:set_env(common_test, basic_html, false)
    end,

    case lists:keysearch(spec, 1, Opts) of
	{value,{_,Specs}} ->
	    Relaxed =
		case lists:keysearch(allow_user_terms, 1, Opts) of
		    {value,{_,true}} -> true;
		    _ -> false
		end,	    
	    %% using testspec(s) as input for test
	    run_spec_file(LogDir, CfgFiles, EvHandlers, Include, Specs, Relaxed, Cover,
			 replace_opt([{silent_connections,SilentConns}], Opts));
	false ->
	    case lists:keysearch(prepared_tests, 1, Opts) of
		{value,{_,{Run,Skip},Specs}} ->	% use prepared tests
		    run_prepared(LogDir, CfgFiles, EvHandlers,
				 Run, Skip, Cover,
				 replace_opt([{silent_connections,SilentConns},
					      {spec,Specs}],Opts));
		false ->		% use dir|suite|case
		    StepOrCover =
			case lists:keysearch(step, 1, Opts) of
			    {value,Step} -> [Step];
			    false -> Cover
			end,
		    run_dir(LogDir, CfgFiles, EvHandlers, StepOrCover,
			    replace_opt([{silent_connections,SilentConns}], Opts))
	    end
    end.

replace_opt([O={Key,_Val}|Os], Opts) ->
    [O | replace_opt(Os, lists:keydelete(Key, 1, Opts))];
replace_opt([], Opts) ->
    Opts.

run_spec_file(LogDir, CfgFiles, EvHandlers, Include, Specs, Relaxed, Cover, Opts) ->
    Specs1 = case Specs of
		 [X|_] when is_integer(X) -> [Specs];
		 _ -> Specs
	     end,
    AbsSpecs = lists:map(fun(SF) -> ?abs(SF) end, Specs1), 
    log_ts_names(AbsSpecs),
    case catch ct_testspec:collect_tests_from_file(AbsSpecs, Relaxed) of
	{error,CTReason} ->
	    exit(CTReason);
	TS ->
	    {LogDir1,TSCoverFile,CfgFiles1,EvHandlers1,Include1} = 
		get_data_for_node(TS, node()),
	    application:set_env(common_test, include, Include++Include1),
	    LogDir2 = which_logdir(LogDir, LogDir1),
	    CoverOpt = case {Cover,TSCoverFile} of
			   {[],undef} -> [];
			   {_,undef} ->  Cover;
			   {[],_} ->     [{cover,TSCoverFile}]
		       end,
	    case check_and_install_configfiles(CfgFiles++CfgFiles1, LogDir2,
				 EvHandlers++EvHandlers1) of
		ok ->
		    {Run,Skip} = ct_testspec:prepare_tests(TS, node()),
		    do_run(Run, Skip, CoverOpt,
			   replace_opt([{spec,AbsSpecs}], Opts),
			   LogDir2);
		{error,GCFReason} ->
		    exit(GCFReason)
	    end
    end.

run_prepared(LogDir, CfgFiles, EvHandlers, Run, Skip, Cover, Opts) ->
    case check_and_install_configfiles(CfgFiles, LogDir, EvHandlers) of
	ok ->
	    do_run(Run, Skip, Cover, Opts, LogDir);
	{error,Reason} ->
	    exit(Reason)
    end.    

check_config_file(File)->
    AbsName = ?abs(File),
    case filelib:is_file(AbsName) of
	true -> AbsName;
	false -> exit({no_such_file,AbsName})
    end.

run_dir(LogDir, CfgFiles, EvHandlers, StepOrCover, Opts) ->
    AbsCfgFiles = 
	lists:map(fun({Callback, FileList})->
	    case code:is_loaded(Callback) of
		{file, _Path}->
		    ok;
		false->
		    case code:load_file(Callback) of
			{module, Callback}->
			    ok;
			{error, _}->
			    exit({no_such_module, Callback})
		    end
	    end,
	    {Callback,
		lists:map(fun(File)->
		    check_config_file(File)
		end, FileList)}
	    end,
	CfgFiles),
    case install([{config,AbsCfgFiles},{event_handler,EvHandlers}], LogDir) of
	ok -> ok;
	{error,IReason} -> exit(IReason)
    end,
    case lists:keysearch(dir,1,Opts) of
	{value,{_,Dirs=[Dir|_]}} when not is_integer(Dir),
	                              length(Dirs)>1 ->
	    %% multiple dirs (no suite)
	    do_run(tests(Dirs), [], StepOrCover, Opts, LogDir);
	false ->				% no dir
	    %% fun for converting suite name to {Dir,Mod} tuple
	    S2M = fun(S) when is_list(S) ->
			  {filename:dirname(S),
			   list_to_atom(filename:rootname(filename:basename(S)))};
		     (A) ->
			  {".",A}
		  end,
	    case lists:keysearch(suite, 1, Opts) of
		{value,{_,Suite}} when is_integer(hd(Suite)) ; is_atom(Suite) ->
		    {Dir,Mod} = S2M(Suite),
		    case listify(proplists:get_value(group, Opts, [])) ++
			 listify(proplists:get_value(testcase, Opts, [])) of
			[] ->
			    do_run(tests(Dir, listify(Mod)), [], StepOrCover, Opts, LogDir);
			GsAndCs ->
			    do_run(tests(Dir, Mod, GsAndCs), [], StepOrCover, Opts, LogDir)
		    end;
		{value,{_,Suites}} ->
		    do_run(tests(lists:map(S2M, Suites)), [], StepOrCover, Opts, LogDir);
		_ ->
		    exit(no_tests_specified)
	    end;		   
	{value,{_,Dir}} ->
	    case lists:keysearch(suite, 1, Opts) of
		{value,{_,Suite}} when is_integer(hd(Suite)) ; is_atom(Suite) ->
		    Mod = if is_atom(Suite) -> Suite; 
			     true -> list_to_atom(Suite) 
			  end,
		    case listify(proplists:get_value(group, Opts, [])) ++
			 listify(proplists:get_value(testcase, Opts, [])) of
			[] ->
			    do_run(tests(Dir, listify(Mod)), [], StepOrCover, Opts, LogDir);
			GsAndCs ->
			    do_run(tests(Dir, Mod, GsAndCs), [], StepOrCover, Opts, LogDir)
		    end;
		{value,{_,Suites=[Suite|_]}} when is_list(Suite) ->
		    Mods = lists:map(fun(Str) -> list_to_atom(Str) end, Suites),
		    do_run(tests(delistify(Dir), Mods), [], StepOrCover, Opts, LogDir);		    
		{value,{_,Suites}} ->
		    do_run(tests(delistify(Dir), Suites), [], StepOrCover, Opts, LogDir);		    
	        false ->			% no suite, only dir
		    do_run(tests(listify(Dir)), [], StepOrCover, Opts, LogDir)
	    end   
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%%

%% using testspec(s) as input for test
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
    case ct_testspec:collect_tests_from_list(TestSpec,false) of
	{error,CTReason} ->
	    exit(CTReason);
	TS ->
	    {LogDir,TSCoverFile,CfgFiles,EvHandlers,Include} = 
		get_data_for_node(TS,node()),
	    case os:getenv("CT_INCLUDE_PATH") of
		false ->
		    application:set_env(common_test, include, Include);
		CtInclPath ->
		    EnvInclude = string:tokens(CtInclPath, [$:,$ ,$,]),
		    application:set_env(common_test, include, EnvInclude++Include)
	    end,
	    CoverOpt = if TSCoverFile == undef -> [];
			  true -> [{cover,TSCoverFile}]
		       end,
	    case check_and_install_configfiles(CfgFiles,LogDir,EvHandlers) of
		ok ->
		    {Run,Skip} = ct_testspec:prepare_tests(TS,node()),
		    do_run(Run,Skip,CoverOpt,[],LogDir);
		{error,GCFReason} ->
		    exit(GCFReason)
	    end
    end.


get_data_for_node(#testspec{logdir=LogDirs,
			    cover=CoverFs,
			    config=Cfgs,
			    event_handler=EvHs,
			    include=Incl}, Node) ->
    LogDir = case lists:keysearch(Node,1,LogDirs) of
		 {value,{Node,Dir}} -> Dir;
		 false -> "."
	     end,
    Cover = case lists:keysearch(Node,1,CoverFs) of
		{value,{Node,CovFile}} -> CovFile;
		false -> undef
	    end,
    ConfigFiles = [F || {N,F} <- Cfgs, N==Node],
    EvHandlers =  [{H,A} || {N,H,A} <- EvHs, N==Node],
    Include =  [I || {N,I} <- Incl, N==Node],
    {LogDir,Cover,ConfigFiles,EvHandlers,Include}.


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

which_logdir(".",Dir) ->
    Dir;
which_logdir(Dir,_) ->
    Dir.
	   
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

do_run(Tests, Opt) ->
    do_run(Tests, [], Opt, [], ".").

do_run(Tests, Opt, LogDir) ->
    do_run(Tests, [], Opt, [], LogDir).

do_run(Tests, Skip, Opt, Args, LogDir) ->
    case code:which(test_server) of
	non_existing ->
	    exit({error,no_path_to_test_server});
	_ ->
	    Opt1 = 
		case lists:keysearch(cover, 1, Opt) of
		    {value,{_,CoverFile}} ->
			case ct_cover:get_spec(CoverFile) of
			    {error,Reason} ->
				exit({error,Reason});
			    Spec ->
				[{cover_spec,Spec} |
				 lists:keydelete(cover, 1, Opt)]
			end;
		    _ ->
			Opt
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
	    case ct_util:start(LogDir) of
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
		    log_ts_names(Args),
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
			    R = do_run_test(Tests1, Skip1, Opt1),
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

do_run_test(Tests, Skip, Opt) ->
    case check_and_add(Tests, []) of
	ok ->
	    ct_util:set_testdata({stats,{0,0,{0,0}}}),
	    ct_util:set_testdata({cover,undefined}),
	    test_server_ctrl:start_link(local),
	    case lists:keysearch(cover_spec, 1, Opt) of
		{value,{_,CovData={CovFile,
				   CovNodes,
				   _CovImport,
				   CovExport,
				   #cover{app        = CovApp,
					  level      = CovLevel,
					  excl_mods  = CovExcl,
					  incl_mods  = CovIncl,
					  cross      = CovCross,
					  src        = _CovSrc}}}} ->
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
	    ct_event:notify(#event{name=start_info,
				   node=node(),
				   data={NoOfTests,NoOfSuites,NoOfCases}}),
	    CleanUp = add_jobs(Tests, Skip, Opt, []),
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
				  maybe_cleanup_interpret(Suite, Opt) 
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
    add_jobs(Tests, Skip, [], []),
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

add_jobs([{TestDir,all,_}|Tests], Skip, Opt, CleanUp) ->
    Name = get_name(TestDir),
    case catch test_server_ctrl:add_dir_with_skip(Name, TestDir,
						  skiplist(TestDir,Skip)) of
	{'EXIT',_} -> 
	    CleanUp;
	_ ->
	    wait_for_idle(),
	    add_jobs(Tests, Skip, Opt, CleanUp)
    end;
add_jobs([{TestDir,[Suite],all}|Tests], Skip, Opt, CleanUp) when is_atom(Suite) ->
    add_jobs([{TestDir,Suite,all}|Tests], Skip, Opt, CleanUp);
add_jobs([{TestDir,Suites,all}|Tests], Skip, Opt, CleanUp) when is_list(Suites) ->
    Name = get_name(TestDir) ++ ".suites",
    case catch test_server_ctrl:add_module_with_skip(Name, Suites,
						     skiplist(TestDir,Skip)) of
	{'EXIT',_} -> 
	    CleanUp;
	_ ->
	    wait_for_idle(),
	    add_jobs(Tests, Skip, Opt, CleanUp)
    end;
add_jobs([{TestDir,Suite,all}|Tests], Skip, Opt, CleanUp) ->
    case maybe_interpret(Suite, all, Opt) of
	ok ->
	    Name =  get_name(TestDir) ++ "." ++ atom_to_list(Suite),
	    case catch test_server_ctrl:add_module_with_skip(Name, [Suite],
							     skiplist(TestDir,Skip)) of
		{'EXIT',_} -> 
		    CleanUp;
		_ ->
		    wait_for_idle(),
		    add_jobs(Tests, Skip, Opt, [Suite|CleanUp])
	    end;
	Error ->
	    Error
    end;
add_jobs([{TestDir,Suite,[Case]}|Tests], Skip, Opt, CleanUp) when is_atom(Case) ->
    add_jobs([{TestDir,Suite,Case}|Tests], Skip, Opt, CleanUp);
add_jobs([{TestDir,Suite,Cases}|Tests], Skip, Opt, CleanUp) when is_list(Cases) ->
    case maybe_interpret(Suite, Cases, Opt) of
	ok ->
	    Name =  get_name(TestDir) ++ "." ++	atom_to_list(Suite) ++ ".cases",
	    case catch test_server_ctrl:add_cases_with_skip(Name, Suite, Cases,
							    skiplist(TestDir,Skip)) of
		{'EXIT',_} -> 
		    CleanUp;
		_ ->
		    wait_for_idle(),
		    add_jobs(Tests, Skip, Opt, [Suite|CleanUp])
	    end;
	Error ->
	    Error
    end;
add_jobs([{TestDir,Suite,Case}|Tests], Skip, Opt, CleanUp) when is_atom(Case) ->
    case maybe_interpret(Suite, Case, Opt) of
	ok ->
	    Name = get_name(TestDir) ++	"." ++ atom_to_list(Suite) ++ "." ++ 
		atom_to_list(Case),
	    case catch test_server_ctrl:add_case_with_skip(Name, Suite, Case,
							   skiplist(TestDir,Skip)) of
		{'EXIT',_} -> 
		    CleanUp;
		_ ->
		    wait_for_idle(),
		    add_jobs(Tests, Skip, Opt, [Suite|CleanUp])
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
					      (F, helpmods) -> not is_suite(F);
					      (_, _) -> true end, 
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

maybe_interpret(Suite, Cases, [{step,StepOpts}]) ->
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

maybe_cleanup_interpret(Suite, [{step,_}]) ->
    i:iq(Suite);
maybe_cleanup_interpret(_, _) ->
    ok.

log_ts_names(Args) ->
    case lists:keysearch(spec, 1, Args) of
	{value,{_,Specs}} ->
	    List = lists:map(fun(Name) ->
				     Name ++ " "
			     end, Specs),
	    ct_logs:log("Test Specification file(s)", "~s",
			[lists:flatten(List)]);
	_ ->
	    ok
    end.
			  
merge_arguments(Args) ->
    merge_arguments(Args, []).

merge_arguments([LogDir={logdir,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(replace, LogDir, Merged));
merge_arguments([CoverFile={cover,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(replace, CoverFile, Merged));
merge_arguments([Arg={_,_}|Args], Merged) ->
    merge_arguments(Args, handle_arg(merge, Arg, Merged));
merge_arguments([], Merged) ->
    Merged.

handle_arg(replace, {Key,Elems}, [{Key,_}|Merged]) ->
    [{Key,Elems}|Merged];
handle_arg(merge, {Key,Elems}, [{Key,PrevElems}|Merged]) ->
    [{Key,PrevElems++Elems}|Merged];
handle_arg(Op, Arg, [Other|Merged]) ->
    [Other|handle_arg(Op, Arg, Merged)];
handle_arg(_,Arg,[]) ->
    [Arg].

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

preload() ->
    io:format("~nLoading Common Test and Test Server modules...~n~n"),
    preload_mod([ct_logs,
		 ct_make,
		 ct_telnet,
		 ct,
		 ct_master,
		 ct_testspec,
		 ct_cover,
		 ct_master_event,
		 ct_util,
		 ct_event,           
		 ct_master_logs,
		 ct_framework,
		 teln,
		 ct_ftp,
		 ct_rpc,
		 unix_telnet,
		 ct_gen_conn,
		 ct_line,
		 ct_snmp,
		 test_server_sup,
		 test_server,
		 test_server_ctrl,
		 test_server_h,
		 test_server_line,
		 test_server_node]).

preload_mod([M|Ms]) ->
    case code:is_loaded(M) of
	false ->
	    {module,M} = code:load_file(M),
	    preload_mod(Ms);
	_ ->
	    ok
    end;
preload_mod([]) ->
    ok.
    
ensure_atom(Atom) when is_atom(Atom) ->
    Atom;
ensure_atom(String) when is_list(String), is_integer(hd(String)) ->
    list_to_atom(String);
ensure_atom(List) when is_list(List) ->
    [ensure_atom(Item) || Item <- List];
ensure_atom(Other) ->				
    Other.
		  
