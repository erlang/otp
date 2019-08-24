#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-include_lib("reltool/src/reltool.hrl").

main(Args) ->
    process_flag(trap_exit, true),
    try
	Tokens = scan_args(Args, [], []),
	{Options, Actions} = parse_args(Tokens, []),
	case invoke(Options, Actions) of
	    ok ->
		safe_stop(0);
	    {error, ReasonString} ->
		fatal_error(ReasonString, 2)
	end
    catch
	throw:usage ->
	    usage(),
	    safe_stop(1);
	exit:Reason ->
	    String = lists:flatten(io_lib:format("EXIT: ~p", [Reason])),
	    fatal_error(String, 3)
    end.

usage() ->
    Usage =
	[
	 "[Config] [--window]",
	 "[Config] --create_config [-defaults] [-derived] [ConfigFile]",
	 "[Config] --create_rel RelName [RelFile]",
	 "[Config] --create_script RelName [ScriptFile]",
	 "[Config] --create_target TargetDir",
	 "[Config] --create_target_spec [SpecFile]",
	 "[Config] --eval_target_spec Spec TargetDir RootDir"
	],
    Script = script_name(),
    String = lists:flatten([[Script, " ", U, "\n"] || U <- Usage]),
    io:format("Erlang/OTP release management tool\n\n"
	      "~s\nConfig = ConfigFile | '{sys, [sys()]}'\n"
	      "Spec   = SpecFile   | '{spec, [target_spec()}']\n\n"
	      "See User's guide and Reference manual for more info.\n",
	      [String]).

safe_stop(Code) ->
    init:stop(Code),
    timer:sleep(infinity).

invoke(Options, Actions) ->
    case Actions of
	[] ->
	    invoke(Options, [["--window"]]);
	[["--window"]] ->
	    start_window(Options);
	[["--create_config" | OptArgs]] ->
	    DefArg = "-defaults",
	    DerivArg = "-derived",
	    InclDef = lists:member(DefArg, OptArgs),
	    InclDeriv = lists:member(DerivArg, OptArgs),
	    case reltool:get_config(Options, InclDef, InclDeriv) of
		{ok, Config} ->
		    String = pretty("config", Config),
		    case OptArgs -- [DefArg, DerivArg] of
			[] ->
			    format("~s", [String]);
			[ConfigFile] ->
			    write_file(ConfigFile, String);
			_ ->
			    throw(usage)
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end;
	[["--create_rel", RelName | OptArgs]] ->
	    case reltool:get_rel(Options, RelName) of
		{ok, Rel} ->
		    String = pretty("rel", Rel),
		    case OptArgs of
			[] ->
			    format("~s", [String]);
			[RelFile] ->
			    write_file(RelFile, String);
			_ ->
			    throw(usage)
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end;
	[["--create_script", RelName | OptArgs]] ->
	    case reltool:get_script(Options, RelName) of
		{ok, Script} ->
		    String = pretty("script", Script),
		    case OptArgs of
			[] ->
			    format("~s", [String]);
			[ScriptFile] ->
			    write_file(ScriptFile, String);
			_ ->
			    throw(usage)
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end;
	[["--create_target", TargetDir]] ->
	    reltool:create_target(Options, TargetDir);
	[["--create_target_spec" | OptArgs]] ->
	    case reltool:get_target_spec(Options) of
		{ok, Script} ->
		    String = pretty("target_spec", Script),
		    case OptArgs of
			[] ->
			    format("~s", [String]);
			[SpecFile] ->
			    write_file(SpecFile, String);
			_ ->
			    throw(usage)
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end;
	[["--eval_target_spec", TargetSpec, TargetDir, RootDir]] ->
	    try
		{ok, Tokens, _} = erl_scan:string(TargetSpec ++ ". "),
		{ok, {spec, Spec}} = erl_parse:parse_term(Tokens),
		reltool:eval_target_spec(Spec, TargetDir, RootDir)
	    catch
		error:{badmatch, _} ->
		    case file:consult(TargetSpec) of
			{ok, Spec2} ->
			    reltool:eval_target_spec(Spec2, TargetDir, RootDir);
			{error, Reason} ->
			    Text = file:format_error(Reason),
			    {error, TargetSpec ++ ": " ++ Text}
		    end
	    end;
	_ ->
	    throw(usage)
    end.

start_window(Options) ->
    case reltool:start_link(Options) of
	{ok, WinPid} ->
	    receive
		{'EXIT', WinPid, shutdown} ->
		    ok;
		{'EXIT', WinPid, normal} ->
		    ok;
		{'EXIT', WinPid, Reason} ->
		    exit(Reason)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

script_name() ->
    filename:basename(escript:script_name(), ".escript").

fatal_error(String, Code) ->
    io:format(standard_error, "~s: ~s\n", [script_name(), String]),
    safe_stop(Code).

write_file(File, IoList) ->
    case file:write_file(File, IoList) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error, file:format_error(Reason)}
    end.

format(Format, Args) ->
    io:format(Format, Args),
    %% Wait a while for the I/O to be processed
    timer:sleep(timer:seconds(1)).

pretty(Tag, Term) ->
    lists:flatten(io_lib:format("%% ~s generated at ~w ~w\n~p.\n\n",
				[Tag, date(), time(), Term])).

scan_args([H | T], Single, Multi) ->
    case H of
	"--" ++ _ when Single =:= [] ->
	    scan_args(T, [H], Multi);
	"--" ++ _ ->
	    scan_args(T, [H], [lists:reverse(Single) | Multi]);
	_ ->
	    scan_args(T, [H | Single], Multi)
    end;
scan_args([], [], Multi) ->
    lists:reverse(Multi);
scan_args([], Single, Multi) ->
    lists:reverse([lists:reverse(Single) | Multi]).

parse_args([H | T] = Args, Options) ->
    case H of
	["--wx_debug" | Levels] ->
	    Dbg =
		fun(L) ->
			case catch list_to_integer(L) of
			    {'EXIT', _} ->
				case catch list_to_atom(L) of
				    {'EXIT', _} ->
					exit("Illegal wx debug level: " ++ L);
				    Atom ->
						Atom
				end;
			    Int ->
				Int
			end
		end,
	    Levels2 = lists:map(Dbg, Levels),
	    parse_args(T, [{wx_debug, Levels2} | Options]);
	["--" ++ _ | _] ->
	    %% No more options
	    {lists:reverse(Options), Args};
	[Config] ->
	    try
		{ok, Tokens, _} = erl_scan:string(Config ++ ". "),
		{ok, {sys, _} = Sys} = erl_parse:parse_term(Tokens),
		parse_args(T, [{config, Sys} | Options])
	    catch
		error:{badmatch, _} ->
		    parse_args(T, [{config, Config} | Options]);
		X:Y ->
		    io:format("\n\n~p\n\n", [{X, Y}])
	    end
    end;
parse_args([], Options) ->
    {lists:reverse(Options), []}.
