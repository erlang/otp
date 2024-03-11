%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

-module(z_SUITE).

%%
%% This suite expects to be run as the last suite of all suites.
%%

-include_lib("kernel/include/file.hrl").
	    
-record(core_search_conf, {db_top_dir,
                           search_dir,
			   extra_search_dir,
			   cerl,
			   file,
			   run_by_ts}).

-export([all/0, suite/0]).

-export([search_for_core_files/1, core_files/1]).

-include_lib("common_test/include/ct.hrl").
    
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() -> 
    [core_files].

core_files(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
            win32_search(true, os:getenv("OTP_DAILY_BUILD_TOP_DIR"));
	{unix, darwin} ->
	    core_file_search(
	      core_search_conf(true,
			       os:getenv("OTP_DAILY_BUILD_TOP_DIR"),
			       "/cores"));
	_ ->
	    core_file_search(
	      core_search_conf(true,
			       os:getenv("OTP_DAILY_BUILD_TOP_DIR")))
    end.

search_for_core_files(Dir) ->
    case os:type() of
	{win32, _} ->
            win32_search(false, Dir);
	{unix, darwin} ->
	    core_file_search(core_search_conf(false, Dir, "/cores"));
	_ ->
	    core_file_search(core_search_conf(false, Dir))
    end.
    
find_cerl(false) ->
    case os:getenv("ERL_TOP") of
	false -> false;
	ETop ->
	    Cerl = filename:join([ETop, "bin", "cerl"]),
	    case filelib:is_regular(Cerl) of
		true -> Cerl;
		_ -> false
	    end
    end;
find_cerl(DBTop) ->
    case catch filelib:wildcard(filename:join([DBTop,
					       "otp_src_*",
					       "bin",
					       "cerl"])) of
	[Cerl | _ ] ->
	    case filelib:is_regular(Cerl) of
		true -> Cerl;
		_ -> find_cerl(false)
	    end;
	_ ->
	    find_cerl(false)
    end.

is_dir(false) ->
    false;
is_dir(Dir) ->
    filelib:is_dir(Dir).

core_search_conf(RunByTS, DBTop) ->
    core_search_conf(RunByTS, DBTop, false).

core_search_conf(RunByTS, DBTop, XDir) ->
    SearchDir = search_dir(DBTop),
    XSearchDir = case is_dir(XDir) of
		     false ->
			 false;
		     true ->
			 case SearchDir == XDir of
			     true -> false;
			     _ -> XDir
			 end
		 end,
    #core_search_conf{db_top_dir = DBTop,
                      search_dir = SearchDir,
		      extra_search_dir = XSearchDir,
		      cerl = find_cerl(DBTop),
		      file = os:find_executable("file"),
		      run_by_ts = RunByTS}.

search_dir(DBTop) ->
    case is_dir(DBTop) of
        false ->
            case code:which(test_server) of
                non_existing ->
                    {ok, CWD} = file:get_cwd(),
                    CWD;
                TS ->
                    filename:dirname(filename:dirname(TS))
            end;
        true ->
            DBTop
    end.

file_inspect(#core_search_conf{file = File}, Core) ->
    FRes0 = os:cmd(File ++ " " ++ Core),
    FRes = case string:split(FRes0, Core) of
	       [S1] -> S1;
	       [S1,S2] -> lists:flatten(S1 ++ " " ++ S2)
	   end,
    case re:run(FRes, "text|ascii", [caseless,{capture,none}]) of
	match ->
	    not_a_core;
	nomatch ->
	    probably_a_core
    end.

mk_readable(F) ->    
    try
	{ok, Old} = file:read_file_info(F),
	file:write_file_info(F, Old#file_info{mode = 8#00444})
    catch	
	_:_ -> io:format("Failed to \"chmod\" core file ~p\n", [F])
    end.

ignore_core(C) ->
    filelib:is_regular(filename:join([filename:dirname(C),
				      "ignore_core_files"])).

core_cand(#core_search_conf{file = false}, C, Cs) ->
    %% Guess that it is a core file; make it readable by anyone and save it
    mk_readable(C),
    [C|Cs];
core_cand(Conf, C, Cs) ->
    case file_inspect(Conf, C) of
	not_a_core -> Cs;
	_ ->
	    %% Probably a core file; make it readable by anyone and save it
	    mk_readable(C),
	    case ignore_core(C) of
		true -> [{ignore, C}|Cs];
		_ -> [C|Cs]
	    end
    end.

time_fstr() ->
    "(~w-~.2.0w-~.2.0w ~w.~.2.0w:~.2.0w)".
mod_time_list(F) ->
    case catch filelib:last_modified(F) of
	{{Y,Mo,D},{H,Mi,S}} ->
	    [Y,Mo,D,H,Mi,S];
	_ ->
	    [0,0,0,0,0,0]
    end.

dump_core(#core_search_conf{ cerl = false }, _) ->
    ok;
dump_core(_, {ignore, _Core}) ->
    ok;
dump_core(#core_search_conf{ cerl = Cerl }, Core) ->
    Dump = case erlang:system_info(build_type) of
               opt ->
                   os:cmd(Cerl ++ " -dump " ++ Core);
               Type ->
		   os:cmd(lists:concat([Cerl," -",Type," -dump ",Core]))
           end,
    ct:log("~ts~n~n~ts",[Core,Dump]).

format_core(Conf, {ignore, Core}) ->
    format_core(Conf, Core, "[ignored] ");
format_core(Conf, Core) ->
    format_core(Conf, Core, ""),

    %% Try print (log dir) name of offending application
    CoreDir = filename:dirname(Core),
    lists:foreach(fun(TestDir) ->
                          FullTestDir = filename:join(CoreDir,TestDir),
			  case filelib:is_dir(FullTestDir) of
			      true ->
				  io:format("  from ~s\n", [TestDir]),
                                  print_last_testcase(FullTestDir);
			      false ->
				  no
			  end
		  end,
		  filelib:wildcard("*.logs", CoreDir)).

print_last_testcase(FullTestDir) ->
    lists:foreach(fun(RunDir) ->
                          SuiteLog = filename:join([FullTestDir,RunDir,"suite.log"]),
                          try last_testcase(SuiteLog) of
                              TestCase ->
                                  io:format("       ~s\n", [TestCase])
                          catch
                              error:Reason ->
                                  io:format("  could not find last test case: ~p\n", [Reason])
                          end
		  end,
		  filelib:wildcard("run.*", FullTestDir)).

last_testcase(SuiteLog) ->
    {ok, Bin} = file:read_file(SuiteLog),
    case string:find(Bin, "\n=case", trailing) of
        TailBin when is_binary(TailBin) ->
            {match, [TestCase]} = re:run(TailBin, "=case\\s+(.+)",
                                         [{capture,all_but_first,binary}]),
            TestCase
    end.

format_core(#core_search_conf{file = false}, Core, Ignore) ->
    io:format("  ~s~s " ++ time_fstr() ++ "~s~n",
	      [Ignore, Core] ++ mod_time_list(Core));
format_core(#core_search_conf{file = File}, Core, Ignore) ->
    FRes = string:trim(os:cmd(File ++ " " ++ Core)),
    case catch re:run(FRes, Core, [caseless,{capture,none}]) of
	match ->
	    io:format("  ~s~s " ++ time_fstr() ++ "~n",
		      [Ignore, FRes] ++ mod_time_list(Core));
	_ ->
	    io:format("  ~s~s: ~s " ++ time_fstr() ++ "~n",
		      [Ignore, Core, FRes] ++ mod_time_list(Core))
    end.

core_file_search(#core_search_conf{search_dir = Base,
				   extra_search_dir = XBase,
				   cerl = Cerl,
				   run_by_ts = RunByTS} = Conf) ->
    case {Cerl,erlang:system_info(build_type)} of
	{false,_} -> ok;
	{_,opt} ->
	    catch io:format("A cerl script that probably can be used for "
			    "inspection of emulator cores:~n  ~s~n",
			    [Cerl]);
	{_,Type} ->
	    catch io:format("A cerl script that probably can be used for "
			    "inspection of emulator cores:~n  ~s -emu_type ~p~n",
			    [Cerl,Type])
    end,

    case os:getenv("DOCKER_BUILD_INFO") of
        false -> ok;
        Info ->
            io:format(Info)
    end,

    io:format("Searching for core-files in: ~s~s~n",
	      [case XBase of
		   false -> "";
		   _ -> XBase ++ " and "
	       end,
	       Base]),
    Filter = fun (Core, Cores) ->
		     case filelib:is_regular(Core) of
			 true ->
			     case filename:basename(Core) of
				 "core" ->
				     core_cand(Conf, Core, Cores);
				 "core." ++ _ ->
				     core_cand(Conf, Core, Cores);
				 "vgcore." ++ _ -> % valgrind
				     core_cand(Conf, Core, Cores);
				 Bin when is_binary(Bin) -> %Icky filename; ignore
				     Cores;
				 BName ->
				     case lists:suffix(".core", BName) of
					 true -> core_cand(Conf, Core, Cores);
					 _ -> Cores
				     end
			     end;
			 _ ->
			     Cores
		     end
	     end,
    case case XBase of
	     false -> [];
	     _ -> filelib:fold_files(XBase, "core", true, Filter, [])
	 end ++ filelib:fold_files(Base, "core", true, Filter, []) of
	[] ->
	    io:format("No core-files found.~n", []),
	    ok;
	Cores ->
	    io:format("Found core files:~n",[]),
	    lists:foreach(fun (C) -> format_core(Conf, C) end, Cores),
	    {ICores, FCores} = lists:foldl(fun ({ignore, IC}, {ICs, FCs}) ->
						   {[" "++IC|ICs], FCs};
					       (FC, {ICs, FCs}) ->
						   {ICs, [" "++FC|FCs]}
					   end,
					   {[],[]},
					   Cores),
	    ICoresComment =
		"Core-files marked with [ignored] were found in directories~n"
		"containing an ignore_core_files file, i.e., the testcase~n"
		"writer has decided that core-files dumped there should be~n"
		"ignored. This testcase won't fail on ignored core-files~n"
		"found.~n",
	    Res = lists:flatten([case FCores of
				     [] ->
					 [];
				     _ ->
					 ["Core-files found:",
					  lists:reverse(FCores)]
				 end,
				 case {FCores, ICores} of
				     {[], []} -> [];
				     {_, []} -> [];
				     {[], _} -> [];
				     _ -> " "
				 end,
				 case ICores of
				     [] -> [];
				     _ ->
					 io:format(ICoresComment, []),
					 ["Ignored core-files found:",
					  lists:reverse(ICores)]
				 end]),

	    lists:foreach(fun(C) -> dump_core(Conf,C) end, Cores),
	    case {RunByTS, ICores, FCores} of
		{true, [], []} -> ok;
		{true, _, []} -> {comment, Res};
		{true, _, _} ->
                    docker_export_otp_src(Conf),
                    ct:fail(Res);
		_ ->
                    Res
	    end
    end.

docker_export_otp_src(#core_search_conf{db_top_dir = DbTop}) ->
    %% If this is a docker run, export the otp_src directory
    %% to not get lost when the docker image is purged.
    try
        case {is_dir(DbTop), is_dir("/daily_build/otp_src/erts")}  of
            {true, true} ->
                %% Stolen from get_otp_src script.
                %% Basically it's a recursive copy of otp_src dir
                %% with preserved permissions, etc.
                run("cd /daily_build && "
                    "tar -cf - otp_src | (cd "++DbTop++" && tar -xpf -)"),
                OtpSrc = DbTop ++ "/otp_src",
                run("cd " ++ OtpSrc ++ "/erts && "
                    "ERL_TOP=" ++ OtpSrc ++ " make local_setup"),
                io:format("otp_src directory exported from docker image");
            _ ->
                ok
        end
    catch
        C:E:S ->
            io:format("Failed to export otp_src directory:"
                      "Exception: ~p\nReason: ~p\nStack: ~p\n",
                      [C, E, S])
    end.

run(Cmd) ->
    Options = [binary, exit_status,stderr_to_stdout,{line,4096}],
    Port = open_port({spawn,"sh -c \"" ++ Cmd ++ "\""}, Options),
    run_loop(Cmd, Port, []).

run_loop(Cmd, Port, Output) ->
    receive
        {Port, {exit_status,0}} ->
            lists:reverse(Output);
        {Port, {exit_status,Status}} ->
            io:format("Failed command (~p): ~p\nOutput: ~p\n", [Status, Cmd, Output]),
            error(bailout);
        {Port, {data,{eol,Bin}}} ->
            run_loop(Cmd, Port, [Bin|Output]);
        Msg ->
            io:format("Unexpected message: ~p\nCommand was: ~p\n", [Msg, Cmd]),
            error(bailout)
    end.


win32_search(RunByTS, DBTop) ->
    case os:getenv("WSLENV") of
        false when RunByTS ->
            {skipped, "No idea searching for core-files on old windows"};
        false ->
            io:format("No idea searching for core-files on old windows");
        _ ->
            win32_search_2(RunByTS, DBTop)
    end.

win32_search_2(true, DBTop0) ->
    DBTop = search_dir(DBTop0),
    Dir = "c:/ldisk/daily_build",
    io:format("Find and move 'dmp' files in: ~s to ~s~n",[Dir, DBTop]),
    case filelib:wildcard("*.dmp", Dir) of
        [] -> ok;
        Dumps ->
            %% We move the "daily" dmp files to this test-run
            Str = lists:flatten(["Core-files found:", lists:join($\s, lists:reverse(Dumps))]),
            Rename = fun(File) ->
                             FP = filename:join(Dir, File),
                             _ = file:rename(FP, filename:join(DBTop, File))
                     end,
            [Rename(File) || File <- Dumps],
            ct:fail(Str)
    end;
win32_search_2(false, _DBTop0) ->
    DBTop = search_dir("c:/ldisk/daily_build"),
    io:format("Search for 'dmp' files in: ~s~n",[DBTop]),
    case filelib:wildcard("*.dmp", DBTop) of
        [] -> "Core-files found: Ignored core-files found:";
        Dumps ->
            io:format("The dmp files must be removed manually\n", []),
            lists:flatten(["Core-files found:", lists:join($\s, lists:reverse(Dumps))])
    end.
