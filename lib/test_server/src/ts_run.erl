%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-import(lists, [member/2,filter/2]).

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
    Hooks = [fun init_state/3,
             fun run_preinits/3,
	     fun make_command/3,
	     Runner],
    Args = make_common_test_args(Args0,Options,Vars),
    St = #state{file=File,test_server_args=Args,batch=Batch},
    R = execute(Hooks, Vars, [], St),
    case R of
	{ok,_,_,_} -> ok;
	Error -> Error
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

get_config_files() ->
    TSConfig = "ts.config",
    [TSConfig | case os:type() of
		    {unix,_} -> ["ts.unix.config"];
		    {win32,_} -> ["ts.win32.config"];
		    vxworks -> ["ts.vxworks.config"];
		    _ -> []
		end].

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
    {ok,Cwd} = file:get_cwd(),
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
    CrashFile = filename:join(Cwd,State#state.file ++ "_erl_crash.dump"),
    case filelib:is_file(CrashFile) of
	true -> 
	    io:format("ts_run: Deleting dump: ~s\n",[CrashFile]),
	    file:delete(CrashFile);
	false -> 
	    ok
    end,

    %% If Common Test specific variables are needed, add them here
    %% on form: "{key1,value1}" "{key2,value2}" ...
    NetDir = ts_lib:var(ts_net_dir, Vars),
    TestVars = [ "\"{net_dir,\\\"",NetDir,"\\\"}\"" ],

    %% NOTE: Do not use ' in these commands as it wont work on windows
    Cmd = [Erl, Naming, "test_server"
	   " -rsh ", ts_lib:var(rsh_name, Vars),
	   " -env PATH \"",
	   backslashify(lists:flatten([TestPath, path_separator(),
			  remove_path_spaces()])), 
	   "\"",
	   " -env ERL_CRASH_DUMP ", CrashFile,
	   %% uncomment the line below to disable exception formatting 
	   %%	   " -test_server_format_exception false",
	   " -boot start_sasl -sasl errlog_type error",
	   " -pz ",Cwd,
	   " -ct_test_vars ",TestVars,
	   " -eval \"file:set_cwd(\\\"",TestDir,"\\\")\" "
	   " -eval \"ct:run_test(", 
	   backslashify(lists:flatten(State#state.test_server_args)),")\""
	   " ",
	   ExtraArgs],
    {ok, Vars, Spec, State#state{command=lists:flatten(Cmd)}}.
    

run_batch(Vars, _Spec, State) ->
    process_flag(trap_exit, true),
    Command = State#state.command ++ " -noinput -s erlang halt",
    ts_lib:progress(Vars, 1, "Command: ~s~n", [Command]),
    io:format(user, "Command: ~s~n",[Command]),
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
    Command = State#state.command,
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


make_common_test_args(Args0, Options, _Vars) ->
    Trace = 
	case lists:keysearch(trace,1,Options) of
	    {value,{trace,TI}} when is_tuple(TI); is_tuple(hd(TI)) ->
		ok = file:write_file(?tracefile,io_lib:format("~p.~n",[TI])),
		[{ct_trace,?tracefile}];
	    {value,{trace,TIFile}} when is_atom(TIFile) ->
		[{ct_trace,atom_to_list(TIFile)}];
	    {value,{trace,TIFile}} ->
		[{ct_trace,TIFile}];
	    false ->
		[]
	end,
    Cover = 
	case lists:keysearch(cover,1,Options) of
	    {value,{cover, App, none, _Analyse}} ->
		io:format("No cover file found for ~p~n",[App]),
		[];
	    {value,{cover,_App,File,_Analyse}} -> 
		[{cover,to_list(File)}];
	    false -> 
		[]
	end,

    Logdir = case lists:keysearch(logdir, 1, Options) of
		  {value,{logdir, _}} ->
		      [];
		  false ->
		      [{logdir,"../test_server"}]
	     end,

    TimeTrap = case test_server:timetrap_scale_factor() of
		   1 ->
		       [];
		   Scale ->
		       [{multiply_timetraps, Scale},
			{scale_timetraps, true}]
	       end,

    ConfigPath = case {os:getenv("TEST_CONFIG_PATH"),
		       lists:keysearch(config, 1, Options)} of
		     {false,{value, {config, Path}}} ->
			 Path;
		     {false,false} ->
			 "../test_server";
		     {Path,_} ->
			 Path
		 end,
    ConfigFiles = [{config,[filename:join(ConfigPath,File)
			    || File <- get_config_files()]}],
    io_lib:format("~100000p",[Args0++Trace++Cover++Logdir++
				  ConfigFiles++Options++TimeTrap]).

to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

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
