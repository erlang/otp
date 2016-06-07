%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
	    
-record(core_search_conf, {search_dir,
			   extra_search_dir,
			   cerl,
			   file,
			   run_by_ts}).

-export([all/0, suite/0]).

-export([search_for_core_files/1, core_files/1]).

-include_lib("common_test/include/ct.hrl").
    
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 5}}].

all() -> 
    [core_files].

core_files(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    {skipped, "No idea searching for core-files on windows"};
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
	    io:format("No idea searching for core-files on windows");
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
		_ -> false
	    end;
	_ ->
	    false
    end.

is_dir(false) ->
    false;
is_dir(Dir) ->
    filelib:is_dir(Dir).

core_search_conf(RunByTS, DBTop) ->
    core_search_conf(RunByTS, DBTop, false).

core_search_conf(RunByTS, DBTop, XDir) ->
    SearchDir = case is_dir(DBTop) of
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
		end,
    XSearchDir = case is_dir(XDir) of
		     false ->
			 false;
		     true ->
			 case SearchDir == XDir of
			     true -> false;
			     _ -> XDir
			 end
		 end,
    #core_search_conf{search_dir = SearchDir,
		      extra_search_dir = XSearchDir,
		      cerl = find_cerl(DBTop),
		      file = os:find_executable("file"),
		      run_by_ts = RunByTS}.

file_inspect(#core_search_conf{file = File}, Core) ->
    FRes0 = os:cmd(File ++ " " ++ Core),
    FRes = case string:str(FRes0, Core) of
	       0 ->
		   FRes0;
	       S ->
		   L = length(FRes0),
		   E = length(Core),
		   case S of
		       1 ->
			   lists:sublist(FRes0, E+1, L+1);
		       _ ->
			   lists:sublist(FRes0, 1, S-1)
			       ++
			       " "
			       ++
			       lists:sublist(FRes0, E+1, L+1)
		   end
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

str_strip(S) ->
    string:strip(string:strip(string:strip(S), both, $\n), both, $\r).

dump_core(#core_search_conf{ cerl = false }, _) ->
    ok;
dump_core(_, {ignore, _Core}) ->
    ok;
dump_core(#core_search_conf{ cerl = Cerl }, Core) ->
    Dump = case test_server:is_debug() of
	       true ->
		   os:cmd(Cerl ++ " -debug -dump " ++ Core);
	       _ ->
		   os:cmd(Cerl ++ " -dump " ++ Core)
	   end,
    ct:log("~ts~n~n~ts",[Core,Dump]).


format_core(Conf, {ignore, Core}) ->
    format_core(Conf, Core, "[ignored] ");
format_core(Conf, Core) ->
    format_core(Conf, Core, "").

format_core(#core_search_conf{file = false}, Core, Ignore) ->
    io:format("  ~s~s " ++ time_fstr() ++ "~s~n",
	      [Ignore, Core] ++ mod_time_list(Core));
format_core(#core_search_conf{file = File}, Core, Ignore) ->
    FRes = str_strip(os:cmd(File ++ " " ++ Core)),
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
    case {Cerl,test_server:is_debug()} of
	{false,_} -> ok;
	{_,true} ->
	    catch io:format("A cerl script that probably can be used for "
			    "inspection of emulator cores:~n  ~s -debug~n",
			    [Cerl]);
	_ ->
	    catch io:format("A cerl script that probably can be used for "
			    "inspection of emulator cores:~n  ~s~n",
			    [Cerl])
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
		{true, _, _} -> ct:fail(Res);
		_ -> Res
	    end
    end.
