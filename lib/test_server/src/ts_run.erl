%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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

%%% Purpose : Supervises running of test cases.

-module(ts_run).

-export([run/4]).

-define(DEFAULT_MAKE_TIMETRAP_MINUTES, 60).
-define(DEFAULT_UNMAKE_TIMETRAP_MINUTES, 15).

-include("ts.hrl").

-import(lists, [map/2,member/2,filter/2,reverse/1]).

-record(state,
	{file,					% File given.
	 mod,					% Module to run.
	 test_server_args,			% Arguments to test server.
	 command,				% Command to run.
	 test_dir,				% Directory for test suite.
	 makefiles,				% List of all makefiles.
	 makefile,				% Current makefile.
	 batch,					% Are we running in batch mode?
	 data_wc,				% Wildcard for data dirs.
	 topcase,				% Top case specification.
	 all					% Set if we have all_SUITE_data
	}).

-define(tracefile,"traceinfo").

%% Options is a slightly modified version of the options given to
%% ts:run. Vars0 are from the variables file.
run(File, Args0, Options, Vars0) ->
    Vars=
	case lists:keysearch(vars, 1, Options) of
	    {value, {vars, Vars1}} ->
		Vars1++Vars0;
	    _ ->
		Vars0
	end,
    {Batch,Runner}  = 
	case {member(interactive, Options), member(batch, Options)} of
	    {false, true} ->
		{true, fun run_batch/3};
	    _ ->
		{false, fun run_interactive/3}
	end,
    HandleTopcase = case member(keep_topcase, Options) of
			true -> [fun copy_topcase/3];
			false -> [fun remove_original_topcase/3,
				  fun init_topcase/3]
		    end,
    MakefileHooks = [fun make_make/3,
		     fun add_make_testcase/3],
    MakeLoop = fun(V, Sp, St) -> make_loop(MakefileHooks, V, Sp, St) end,
    Hooks = [fun init_state/3,
	     fun read_spec_file/3] ++
	     HandleTopcase ++
             [fun run_preinits/3,
	     fun find_makefiles/3,
	     MakeLoop,
	     fun make_test_suite/3,
	     fun add_topcase_to_spec/3,
	     fun write_spec_file/3,
	     fun make_command/3,
	     Runner],
    Args = make_test_server_args(Args0,Options,Vars),
    St = #state{file=File,test_server_args=Args,batch=Batch},
    R = execute(Hooks, Vars, [], St),
    case Batch of
	true -> ts_reports:make_index();
	false -> ok % ts_reports:make_index() is run on the test_server node
    end,
    case R of
	{ok,_,_,_} -> ok;
	Error -> Error
    end.

make_loop(Hooks, Vars0, Spec0, St0) ->
    case St0#state.makefiles of
	[Makefile|Rest] ->
	    case execute(Hooks, Vars0, Spec0, St0#state{makefile=Makefile}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Vars, Spec, St} ->
		    make_loop(Hooks, Vars, Spec, St#state{makefiles=Rest})
	    end;
	[] ->
	    {ok, Vars0, Spec0, St0}
    end.

execute([Hook|Rest], Vars0, Spec0, St0) ->
    case Hook(Vars0, Spec0, St0) of
	ok ->
	    execute(Rest, Vars0, Spec0, St0);
	{ok, Vars, Spec, St} ->
	    execute(Rest, Vars, Spec, St);
	Error ->
	    Error
    end;
execute([], Vars, Spec, St) ->
    {ok, Vars, Spec, St}.

%%
%% Deletes File from Files when File is on the form .../<SUITE>_data/<file>
%% when all of <SUITE> has been skipped in Spec, i.e. there
%% exists a {skip, {<SUITE>, _}} tuple in Spec.
%%
del_skipped_suite_data_dir(Files, Spec) ->
    SkipDirNames = lists:foldl(fun ({skip, {SS, _C}}, SSs) ->
				       [atom_to_list(SS) ++ "_data" | SSs];
				   (_, SSs) ->
				       SSs
			       end,
			       [],
			       Spec),
    filter(fun (File) ->
		   not member(filename:basename(filename:dirname(File)),
			      SkipDirNames)
	   end,
	   Files).

%% Initialize our internal state.

init_state(Vars, [], St0) ->
    {FileBase,Wc0,Mod} =
	case St0#state.file of
	    {Fil,Mod0} -> {Fil, atom_to_list(Mod0) ++ "*_data",Mod0};
	    Fil -> {Fil,"*_SUITE_data",[]}
	end,
    {ok,Cwd} = file:get_cwd(),
    TestDir = filename:join(filename:dirname(Cwd), FileBase++"_test"),
    case filelib:is_dir(TestDir) of
	true ->
	    Wc = filename:join(TestDir, Wc0),
	    {ok,Vars,[],St0#state{file=FileBase,mod=Mod,
				  test_dir=TestDir,data_wc=Wc}};
	false ->
	    {error,{no_test_directory,TestDir}}
    end.
    
%% Read the spec file for the test suite.

read_spec_file(Vars, _, St) ->
    TestDir = St#state.test_dir,
    File = St#state.file,
    {SpecFile,Res} = get_spec_filename(Vars, TestDir, File),
    case Res of
	{ok,Spec} ->
	    {ok,Vars,Spec,St};
	{error,Atom} when is_atom(Atom) ->
	    {error,{no_spec,SpecFile}};
	{error,Reason} ->
	    {error,{bad_spec,lists:flatten(file:format_error(Reason))}}
    end.

get_spec_filename(Vars, TestDir, File) ->
    DynSpec = filename:join(TestDir, File ++ ".dynspec"),
    case filelib:is_file(DynSpec) of
	true ->
	    Bs0 = erl_eval:new_bindings(),
	    Bs1 = erl_eval:add_binding('Target', ts_lib:var(target, Vars), Bs0),
	    Bs2 = erl_eval:add_binding('Os', ts_lib:var(os, Vars), Bs1),
	    TCCStr = ts_lib:var(test_c_compiler, Vars),
	    TCC = try
		      {ok, Toks, _} = erl_scan:string(TCCStr ++ "."),
		      {ok, Tcc} = erl_parse:parse_term(Toks),
		      Tcc
		  catch
		      _:_ -> undefined
		  end,
	    Bs = erl_eval:add_binding('TestCCompiler', TCC, Bs2),
	    {DynSpec,file:script(DynSpec, Bs)};
	false ->
	    SpecFile = get_spec_filename_1(Vars, TestDir, File),
	    {SpecFile,file:consult(SpecFile)}
    end.

get_spec_filename_1(Vars, TestDir, File) ->
    case ts_lib:var(os, Vars) of
	"VxWorks" ->
	    check_spec_filename(TestDir, File, ".spec.vxworks");
	"Windows"++_ ->
	    check_spec_filename(TestDir, File, ".spec.win");
	_Other ->
	    filename:join(TestDir, File ++ ".spec")
    end.

check_spec_filename(TestDir, File, Ext) ->
    Spec = filename:join(TestDir, File ++ Ext),
    case filelib:is_file(Spec) of
	true -> Spec;
	false -> filename:join(TestDir, File ++ ".spec")
    end.

%% Remove the top case from the spec file. We will add our own
%% top case later.

remove_original_topcase(Vars, Spec, St) ->
    {ok,Vars,filter(fun ({topcase,_}) -> false;
			(_Other) -> true end, Spec),St}.

%% Initialize our new top case. We'll keep in it the state to be
%%  able to add more to it.

init_topcase(Vars, Spec, St) ->
    TestDir = St#state.test_dir,
    TopCase = 
	case St#state.mod of
	    Mod when is_atom(Mod) ->
		ModStr = atom_to_list(Mod),
		case filelib:is_file(filename:join(TestDir,ModStr++".erl")) of
		    true -> [{Mod,all}];
		    false ->
			Wc = filename:join(TestDir, ModStr ++ "*_SUITE.erl"),
			[{list_to_atom(filename:basename(M, ".erl")),all} ||
			    M <- filelib:wildcard(Wc)]
		end;
	    _Other ->
		%% Here we used to return {dir,TestDir}. Now we instead
		%% list all suites in TestDir, so we can add make testcases
		%% around it later (see add_make_testcase) without getting
		%% duplicates of the suite. (test_server_ctrl does no longer
		%% check for duplicates of testcases)
		Wc = filename:join(TestDir, "*_SUITE.erl"),
		[{list_to_atom(filename:basename(M, ".erl")),all} ||
		    M <- filelib:wildcard(Wc)]
	end,
    {ok,Vars,Spec,St#state{topcase=TopCase}}.

%% Or if option keep_topcase was given, eh... keep the topcase
copy_topcase(Vars, Spec, St) ->
    {value,{topcase,Tc}} = lists:keysearch(topcase,1,Spec),
    {ok, Vars, lists:keydelete(topcase,1,Spec),St#state{topcase=Tc}}.


%% Run any "Makefile.first" files first.
%%  XXX We should fake a failing test case if the make fails.

run_preinits(Vars, Spec, St) ->
    Wc = filename:join(St#state.data_wc, "Makefile.first"),
    run_pre_makefiles(del_skipped_suite_data_dir(filelib:wildcard(Wc), Spec),
		      Vars, Spec, St),
    {ok,Vars,Spec,St}.

run_pre_makefiles([Makefile|Ms], Vars0, Spec0, St0) ->
    Hooks = [fun run_pre_makefile/3],
    case execute(Hooks, Vars0, Spec0, St0#state{makefile=Makefile}) of
	{error,_Reason}=Error -> Error;
	{ok,Vars,Spec,St} -> run_pre_makefiles(Ms, Vars, Spec, St)
    end;
run_pre_makefiles([], Vars, Spec, St) -> {ok,Vars,Spec,St}.

run_pre_makefile(Vars, Spec, St) ->
    Makefile = St#state.makefile,
    Shortname = filename:basename(Makefile),
    DataDir = filename:dirname(Makefile),
    Make = ts_lib:var(make_command, Vars),
    case ts_make:make(Make,DataDir, Shortname) of
	ok -> {ok,Vars,Spec,St};
	{error,_Reason}=Error -> Error
    end.

%% Search for `Makefile.src' in each *_SUITE_data directory.

find_makefiles(Vars, Spec, St) ->
    Wc = filename:join(St#state.data_wc, "Makefile.src"),
    Makefiles = reverse(del_skipped_suite_data_dir(filelib:wildcard(Wc), Spec)),
    {ok,Vars,Spec,St#state{makefiles=Makefiles}}.
    
%% Create "Makefile" from "Makefile.src".

make_make(Vars, Spec, State) ->
    Src = State#state.makefile,
    Dest = filename:rootname(Src),
    ts_lib:progress(Vars, 1, "Making ~s...\n", [Dest]),
    case ts_lib:subst_file(Src, Dest, Vars) of
	ok ->
	    {ok, Vars, Spec, State#state{makefile=Dest}};
	{error, Reason} ->
	    {error, {Src, Reason}}
    end.

%% Add a testcase which will do the making of the stuff in the data directory.

add_make_testcase(Vars, Spec, St) ->
    Makefile = St#state.makefile,
    Dir = filename:dirname(Makefile),
    Shortname = filename:basename(Makefile),
    Suite = filename:basename(Dir, "_data"),
    Config = [{data_dir,Dir},{makefile,Shortname}],
    MakeModule = Suite ++ "_make",
    MakeModuleSrc = filename:join(filename:dirname(Dir),
				  MakeModule ++ ".erl"),
    MakeMod = list_to_atom(MakeModule),
    case filelib:is_file(MakeModuleSrc) of
	true -> ok;
	false -> generate_make_module(ts_lib:var(make_command, Vars),
				      MakeModuleSrc,
				      MakeModule)
    end,
    case Suite of
	"all_SUITE" ->
	    {ok,Vars,Spec,St#state{all={MakeMod,Config}}};
	_ ->
	    %% Avoid duplicates of testcases. There is no longer
	    %% a check for this in test_server_ctrl.
	    TestCase = {list_to_atom(Suite),all},
	    TopCase0 = case St#state.topcase of
			   List when is_list(List) ->
			       List -- [TestCase];
			   Top ->
			       [Top] -- [TestCase]
		       end,
	    TopCase = [{make,{MakeMod,make,[Config]},
			TestCase,
			{MakeMod,unmake,[Config]}}|TopCase0],
	    {ok,Vars,Spec,St#state{topcase=TopCase}}
    end.

generate_make_module(MakeCmd, Name, ModuleString) ->
    {ok,Host} = inet:gethostname(),
    file:write_file(Name,
		    ["-module(",ModuleString,").\n",
		     "\n",
		     "-export([make/1,unmake/1]).\n",
		     "\n",
		     "make(Config) when is_list(Config) ->\n",
		     "    Mins = " ++ integer_to_list(?DEFAULT_MAKE_TIMETRAP_MINUTES) ++ ",\n"
		     "    test_server:format(\"=== Setting timetrap to ~p minutes ===~n\", [Mins]),\n"
		     "    TimeTrap = test_server:timetrap(test_server:minutes(Mins)),\n"
		     "    Res = ts_make:make([{make_command, \""++MakeCmd++"\"},{cross_node,\'ts@" ++ Host ++ "\'}|Config]),\n",
		     "    test_server:timetrap_cancel(TimeTrap),\n"
		     "    Res.\n"
		     "\n",
		     "unmake(Config) when is_list(Config) ->\n",
		     "    Mins = " ++ integer_to_list(?DEFAULT_UNMAKE_TIMETRAP_MINUTES) ++ ",\n"
		     "    test_server:format(\"=== Setting timetrap to ~p minutes ===~n\", [Mins]),\n"
		     "    TimeTrap = test_server:timetrap(test_server:minutes(Mins)),\n"
		     "    Res = ts_make:unmake([{make_command, \""++MakeCmd++"\"}|Config]),\n"
		     "    test_server:timetrap_cancel(TimeTrap),\n"
		     "    Res.\n"
		     "\n"]).
			   

make_test_suite(Vars, _Spec, State) ->
    TestDir = State#state.test_dir,

    Erl_flags=[{i, "../test_server"}|ts_lib:var(erl_flags,Vars)],

    case code:is_loaded(test_server_line) of
        false -> code:load_file(test_server_line);
	_ -> ok
    end,

    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(TestDir),
    Result = (catch make_all(Erl_flags)),
    ok = file:set_cwd(Cwd),
    case Result of
	up_to_date ->
	    ok;
	{'EXIT', Reason} ->
	    %% If I return an error here, the test will be stopped
	    %% and it will not show up in the top index page. Instead
	    %% I return ok - the test will run for all existing suites.
	    %% It might be that there are old suites that are run, but
	    %% at least one suite is missing, and that is reported on the
	    %% top index page.
	    io:format("~s: {error,{make_crashed,~p}\n",
		      [State#state.file,Reason]),
	    ok;
	error ->
	    %% See comment above
	    io:format("~s: {error,make_of_test_suite_failed}\n",
		      [State#state.file]),
	    ok
    end.

%% Add topcase to spec.

add_topcase_to_spec(Vars, Spec, St) ->
    Tc = case St#state.all of
	     {MakeMod,Config} ->
		 [{make,{MakeMod,make,[Config]},
		   St#state.topcase,
		   {MakeMod,unmake,[Config]}}];
	     undefined -> St#state.topcase
	 end,
    {ok,Vars,Spec++[{topcase,Tc}],St}.

%% Writes the (possibly transformed) spec file.

write_spec_file(Vars, Spec, _State) ->
    F = fun(Term) -> io_lib:format("~p.~n", [Term]) end,
    SpecFile = map(F, Spec),
    Hosts = 
	case lists:keysearch(hosts, 1, Vars) of
	    false ->
		[];
	    {value, {hosts, HostList}} ->
		io_lib:format("{hosts,~p}.~n",[HostList])
	end,
    DiskLess =
	case lists:keysearch(diskless, 1, Vars) of
	    false ->
		[];
	    {value, {diskless, How}} ->
		io_lib:format("{diskless, ~p}.~n",[How])
	end,
    Conf = consult_config(),
    MoreConfig = io_lib:format("~p.\n", [{config,Conf}]),
    file:write_file("current.spec", [DiskLess,Hosts,MoreConfig,SpecFile]).

consult_config() ->
    {ok,Conf} = file:consult("ts.config"),
    case os:type() of
	{unix,_} -> consult_config("ts.unix.config", Conf);
	{win32,_} -> consult_config("ts.win32.config", Conf);
	vxworks -> consult_config("ts.vxworks.config", Conf);
	_ -> Conf
    end.

consult_config(File, Conf0) ->
    case file:consult(File) of
	{ok,Conf} -> Conf++Conf0;
	{error,enoent} -> Conf0
    end.

%% Makes the command to start up the Erlang node to run the tests.

backslashify([$\\, $" | T]) ->
    [$\\, $" | backslashify(T)];
backslashify([$" | T]) ->
    [$\\, $" | backslashify(T)];
backslashify([H | T]) ->
    [H | backslashify(T)];
backslashify([]) ->
    [].

make_command(Vars, Spec, State) ->
    TestDir = State#state.test_dir,
    TestPath = filename:nativename(TestDir),
    Erl = case os:getenv("TS_RUN_VALGRIND") of
	      false ->
		  atom_to_list(lib:progname());
	      _ ->
		  case State#state.file of
		      Dir when is_list(Dir) ->
			  os:putenv("VALGRIND_LOGFILE_PREFIX", Dir++"-");
		      _ ->
			  ok
		  end,
		  "cerl -valgrind" ++
		      case erlang:system_info(smp_support) of
			  true -> " -smp";
			  false -> ""
		      end
	  end,
    Naming =
	case ts_lib:var(longnames, Vars) of
	    true ->
		" -name ";
	    false ->
		" -sname "
	end,
    ExtraArgs = 
	case lists:keysearch(erl_start_args,1,Vars) of
	    {value,{erl_start_args,Args}} -> Args;
	    false -> ""
	end,
    CrashFile = State#state.file ++ "_erl_crash.dump",
    case filelib:is_file(CrashFile) of
	true -> 
	    io:format("ts_run: Deleting dump: ~s\n",[CrashFile]),
	    file:delete(CrashFile);
	false -> 
	    ok
    end,
    Cmd = [Erl, Naming, "test_server -pa ", $", TestPath, $",
	   " -rsh ", ts_lib:var(rsh_name, Vars),
	   " -env PATH \"",
	   backslashify(lists:flatten([TestPath, path_separator(),
			  remove_path_spaces()])), 
	   "\"",
	   " -env ERL_CRASH_DUMP ", CrashFile,
	   %% uncomment the line below to disable exception formatting 
	   %%	   " -test_server_format_exception false",
	   " -boot start_sasl -sasl errlog_type error",
	   " -s test_server_ctrl run_test ", State#state.test_server_args,
	   " ",
	   ExtraArgs],
    {ok, Vars, Spec, State#state{command=lists:flatten(Cmd)}}.

run_batch(Vars, _Spec, State) ->
    process_flag(trap_exit, true),
    Command = State#state.command ++ " -noinput -s erlang halt",
    ts_lib:progress(Vars, 1, "Command: ~s~n", [Command]),
    Port = open_port({spawn, Command}, [stream, in, eof]),
    tricky_print_data(Port).

tricky_print_data(Port) ->
    receive
	{Port, {data, Bytes}} ->
	    io:put_chars(Bytes),
	    tricky_print_data(Port);
	{Port, eof} ->
	    Port ! {self(), close}, 
	    receive
		{Port, closed} ->
		    true
	    end, 
	    receive
		{'EXIT',  Port,  _} -> 
		    ok
	    after 1 ->				% force context switch
		    ok
	    end
    after 30000 ->
	    case erl_epmd:names() of
		{ok,Names} ->
		    case is_testnode_dead(Names) of
			true ->
			    io:put_chars("WARNING: No EOF, but "
					 "test_server node is down!\n");
			false ->
			    tricky_print_data(Port)
		    end;
		_ ->
		    tricky_print_data(Port)
	    end
    end.

is_testnode_dead([]) -> true;
is_testnode_dead([{"test_server",_}|_]) -> false;
is_testnode_dead([_|T]) -> is_testnode_dead(T).

run_interactive(Vars, _Spec, State) ->
    Command = State#state.command ++ " -s ts_reports make_index",
    ts_lib:progress(Vars, 1, "Command: ~s~n", [Command]),
    case ts_lib:var(os, Vars) of
	"Windows 95" ->
	    %% Windows 95 strikes again!  We must redirect standard
	    %% input and output for the `start' command, to force
	    %% standard input and output to the Erlang shell to be
	    %% connected to the newly started console.
	    %% Without these redirections, the Erlang shell would be
	    %% connected to the pipes provided by the port program
	    %% and there would be an inactive console window.
	    os:cmd("start < nul > nul w" ++ Command),
	    ok;
	"Windows 98" ->
	    os:cmd("start < nul > nul w" ++ Command),
	    ok;
	"Windows"++_ ->
	    os:cmd("start w" ++ Command),
	    ok;
	_Other ->
	    %% Assuming ts and controller always run on solaris
	    start_xterm(Command)
    end.

start_xterm(Command) ->
    case os:find_executable("xterm") of
	false ->
	    io:format("The `xterm' program was not found.\n"),
	    {error, no_xterm};
	_Xterm ->
	    case os:getenv("DISPLAY") of
		false ->
		    io:format("DISPLAY is not set.\n"),
		    {error, display_not_set};
		Display ->
		    io:format("Starting xterm (DISPLAY=~s)...\n",
			      [Display]),
		    os:cmd("xterm -sl 10000 -e " ++ Command ++ "&"),
		    ok
	    end
    end.

path_separator() ->
    case os:type() of
	{win32, _} -> ";";
	{unix, _}  -> ":";
	vxworks ->    ":"
    end.


make_test_server_args(Args0,Options,Vars) ->
    Parameters = 
	case ts_lib:var(os, Vars) of
	    "VxWorks" ->
		F = write_parameterfile(vxworks,Vars),
		" PARAMETERS " ++ F;
	    _ ->
		""
	end,
    Trace = 
	case lists:keysearch(trace,1,Options) of
	    {value,{trace,TI}} when is_tuple(TI); is_tuple(hd(TI)) ->
		ok = file:write_file(?tracefile,io_lib:format("~p.~n",[TI])),
		" TRACE " ++ ?tracefile;
	    {value,{trace,TIFile}} when is_atom(TIFile) ->
		" TRACE " ++ atom_to_list(TIFile);
	    {value,{trace,TIFile}} ->
		" TRACE " ++ TIFile;
	    false ->
		""
	end,
    Cover = 
	case lists:keysearch(cover,1,Options) of
	    {value,{cover,App,File,Analyse}} -> 
		" COVER " ++ to_list(App) ++ " " ++ to_list(File) ++ " " ++ 
		    to_list(Analyse);
	    false -> 
		""
	end,
    TCCallback =
	case ts_lib:var(ts_testcase_callback, Vars) of
	    "" -> 
		"";
	    {Mod,Func} ->
		io:format("Function ~w:~w/4 will be called before and "
			  "after each test case.\n", [Mod,Func]),
		" TESTCASE_CALLBACK " ++ to_list(Mod) ++ " " ++ to_list(Func);	    
	    ModFunc when is_list(ModFunc) ->
		[Mod,Func]=string:tokens(ModFunc," "),
		io:format("Function ~s:~s/4 will be called before and "
			  "after each test case.\n", [Mod,Func]),			
		" TESTCASE_CALLBACK " ++ ModFunc;		    
	    _ ->
		""
	end,
    Args0 ++ Parameters ++ Trace ++ Cover ++ TCCallback.

to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

write_parameterfile(Type,Vars) ->
    Cross_host = ts_lib:var(target_host, Vars),
    SlaveTargets = case lists:keysearch(slavetargets,1,Vars) of
		       {value, ST} ->
			   [ST];
		       _ ->
			   []
		   end,
    Master = case lists:keysearch(master,1,Vars) of
		 {value,M} -> [M];
		 false -> []
	     end,
    ToWrite = [{type,Type},
	       {target, list_to_atom(Cross_host)}] ++ SlaveTargets ++ Master,

    Crossfile = atom_to_list(Type) ++ "parameters" ++ os:getpid(),
    ok = file:write_file(Crossfile,io_lib:format("~p.~n", [ToWrite])),
    Crossfile.

%%
%% Paths and spaces handling for w2k and XP
%%
remove_path_spaces() ->
    Path = os:getenv("PATH"),
    case os:type() of
	{win32,nt} ->
	    remove_path_spaces(Path);
	_ ->
	    Path
    end.

remove_path_spaces(Path) ->
    SPath = split_path(Path),
    [NSHead|NSTail] = lists:map(fun(X) -> filename:nativename(
					    filename:join(
					      translate_path(split_one(X)))) 
				end,
				SPath),
    NSHead ++ lists:flatten([[$;|X] || X <- NSTail]).

translate_path(PList) ->
    %io:format("translate_path([~p|~p]~n",[Base,PList]),
    translate_path(PList,[]).


translate_path([],_) ->
    [];
translate_path([PC | T],BaseList) ->
    FullPath = filename:nativename(filename:join(BaseList ++ [PC])),
    NewPC = case catch file:altname(FullPath) of
		{ok,X} ->
		    X;
		_ ->
		    PC
	    end,
    %io:format("NewPC:~s, DirList:~p~n",[NewPC,DirList]),
    NewBase = BaseList ++ [NewPC],
    [NewPC | translate_path(T,NewBase)].

split_one(Path) ->
    filename:split(Path).

split_path(Path) ->
    string:tokens(Path,";").

%%
%% Run make:all/1 if the test suite seems to be designed
%% to be built/re-built by ts.
%%
make_all(Flags) ->
    case filelib:is_regular("Emakefile") of
	false ->
	    make_all_no_emakefile(Flags);
	true ->
	    make:all(Flags)
    end.

make_all_no_emakefile(Flags) ->
    case filelib:wildcard("*.beam") of
	[] ->
	    %% Since there are no *.beam files, we will assume
	    %% that this test suite was designed to be built and
	    %% re-built by ts. Create an Emakefile so that
	    %% make:all/1 will be run the next time too
	    %% (in case a test suite is being interactively
	    %% developed).
	    create_emakefile(Flags, "*.erl");
	[_|_] ->
	    %% There is no Emakefile and there already are
	    %% some *.beam files here. Assume that this test
	    %% suite was not designed to be re-built by ts.
	    %% Only create a Emakefile that will compile
	    %% generated *_SUITE_make files (if any).
	    create_emakefile(Flags, "*_SUITE_make.erl")
    end.

create_emakefile(Flags, Wc) ->
    case filelib:wildcard(Wc) of
	[] ->
	    %% There are no files to be built (i.e. not even any
	    %% generated *_SUITE_make.erl files). We must handle
	    %% this case specially, because make:all/1 will crash
	    %% on Emakefile with an empty list of modules.
	    io:put_chars("No Emakefile found - not running make:all/1\n"),
	    up_to_date;
	[_|_]=Ms0 ->
	    io:format("Creating an Emakefile for compiling files matching ~s\n",
		      [Wc]),
	    Ms = [list_to_atom(filename:rootname(M, ".erl")) || M <- Ms0],
	    Make0 = {Ms,Flags},
	    Make = io_lib:format("~p. \n", [Make0]),
	    ok = file:write_file("Emakefile", Make),
	    make:all(Flags)
    end.
