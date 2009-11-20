%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(erl_compile).

-include("erl_compile.hrl").
-include("file.hrl").

-export([compile_cmdline/1]).

%% Mapping from extension to {M,F} to run the correct compiler.

compiler(".erl") ->    {compile,         compile};
compiler(".S") ->      {compile,         compile_asm};
compiler(".beam") ->   {compile,         compile_beam};
compiler(".core") ->   {compile,         compile_core};
compiler(".mib") ->    {snmpc,           compile};
compiler(".bin") ->    {snmpc,           mib_to_hrl};
compiler(".xrl") ->    {leex,            compile};
compiler(".yrl") ->    {yecc,            compile};
compiler(".script") -> {systools,        script2boot};
compiler(".rel") ->    {systools,        compile_rel};
compiler(".idl") ->    {ic,              compile};
compiler(".asn1") ->   {asn1ct,          compile_asn1};
compiler(".asn") ->    {asn1ct,          compile_asn};
compiler(".py") ->     {asn1ct,          compile_py};
compiler(".xml") ->    {xmerl_scan,      process};
compiler(_) ->         no.

%% Entry from command line.

-type cmd_line_arg() :: atom() | string().

-spec compile_cmdline([cmd_line_arg()]) -> no_return().

compile_cmdline(List) ->
    case compile(List) of
	ok -> my_halt(0);
	error -> my_halt(1);
	_ -> my_halt(2)
    end.

my_halt(Reason) ->
    case process_info(group_leader(), status) of
	{_,waiting} ->
	    %% Now all output data is down in the driver.
	    %% Give the driver some extra time before halting.
	    receive after 1 -> ok end,
	    halt(Reason);
	_ ->
	    %% Probably still processing I/O requests.
	    erlang:yield(),
	    my_halt(Reason)
    end.

%% Run the the compiler in a separate process, trapping EXITs.

compile(List) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> compiler_runner(List) end),
    receive
	{'EXIT', Pid, {compiler_result, Result}} ->
	    Result;
	{'EXIT', Pid, Reason} ->
	    io:format("Runtime error: ~p~n", [Reason]),
	    error
    end.

-spec compiler_runner([cmd_line_arg()]) -> no_return().

compiler_runner(List) ->
    %% We don't want the current directory in the code path.
    %% Remove it.
    Path = [D || D <- code:get_path(), D =/= "."],
    true = code:set_path(Path),
    exit({compiler_result, compile1(List)}).

%% Parses the first part of the option list.

compile1(['@cwd', Cwd|Rest]) ->
    CwdL = atom_to_list(Cwd),
    compile1(Rest, CwdL, #options{outdir=CwdL, cwd=CwdL});
compile1(Args) ->
    %% From R13B02, the @cwd argument is optional.
    {ok, Cwd} = file:get_cwd(),
    compile1(Args, Cwd, #options{outdir=Cwd, cwd=Cwd}).

%% Parses all options.

compile1(['@i', Dir|Rest], Cwd, Opts) ->
    AbsDir = filename:absname(Dir, Cwd),
    compile1(Rest, Cwd, Opts#options{includes=[AbsDir|Opts#options.includes]});
compile1(['@outdir', Dir|Rest], Cwd, Opts) ->
    AbsName = filename:absname(Dir, Cwd),
    case file_or_directory(AbsName) of
	file ->
	    compile1(Rest, Cwd, Opts#options{outfile=AbsName});
	directory ->
	    compile1(Rest, Cwd, Opts#options{outdir=AbsName})
    end;
compile1(['@d', Name|Rest], Cwd, Opts) ->
    Defines = Opts#options.defines,
    compile1(Rest, Cwd, Opts#options{defines=[Name|Defines]});
compile1(['@dv', Name, Term|Rest], Cwd, Opts) ->
    Defines = Opts#options.defines,
    Value = make_term(atom_to_list(Term)),
    compile1(Rest, Cwd, Opts#options{defines=[{Name, Value}|Defines]});
compile1(['@warn', Level0|Rest], Cwd, Opts) ->
    case catch list_to_integer(atom_to_list(Level0)) of
	Level when is_integer(Level) ->
	    compile1(Rest, Cwd, Opts#options{warning=Level});
	_ ->
	    compile1(Rest, Cwd, Opts)
    end;
compile1(['@verbose', false|Rest], Cwd, Opts) ->
    compile1(Rest, Cwd, Opts#options{verbose=false});
compile1(['@verbose', true|Rest], Cwd, Opts) ->
    compile1(Rest, Cwd, Opts#options{verbose=true});
compile1(['@optimize', Atom|Rest], Cwd, Opts) ->
    Term = make_term(atom_to_list(Atom)),
    compile1(Rest, Cwd, Opts#options{optimize=Term});
compile1(['@option', Atom|Rest], Cwd, Opts) ->
    Term = make_term(atom_to_list(Atom)),
    Specific = Opts#options.specific,
    compile1(Rest, Cwd, Opts#options{specific=[Term|Specific]});
compile1(['@output_type', OutputType|Rest], Cwd, Opts) ->
    compile1(Rest, Cwd, Opts#options{output_type=OutputType});
compile1(['@files'|Rest], Cwd, Opts) ->
    Includes = lists:reverse(Opts#options.includes),
    compile2(Rest, Cwd, Opts#options{includes=Includes}).

compile2(Files, Cwd, Opts) ->
    case {Opts#options.outfile, length(Files)} of
	{"", _} ->
	    compile3(Files, Cwd, Opts);
	{[_|_], 1} ->
	    compile3(Files, Cwd, Opts);
	{[_|_], _N} ->
	    io:format("Output file name given, but more than one input file.~n"),
	    error
    end.

%% Compiles the list of files, until done or compilation fails.

compile3([File|Rest], Cwd, Options) ->
    Ext = filename:extension(File),
    Root = filename:rootname(File),
    InFile = filename:absname(Root, Cwd),
    OutFile =
	case Options#options.outfile of
	    "" ->
		filename:join(Options#options.outdir, filename:basename(Root));
	    Outfile ->
		filename:rootname(Outfile)
	end,
    case compile_file(Ext, InFile, OutFile, Options) of
	ok ->
	    compile3(Rest, Cwd, Options);
	Other ->
	    Other
    end;
compile3([], _Cwd, _Options) -> ok.

%% Invokes the appropriate compiler, depending on the file extension.

compile_file("", Input, _Output, _Options) ->
    io:format("File has no extension: ~s~n", [Input]),
    error;
compile_file(Ext, Input, Output, Options) ->
    case compiler(Ext) of
	no ->
	    io:format("Unknown extension: '~s'\n", [Ext]),
	    error;
	{M, F} ->
	    case catch M:F(Input, Output, Options) of
		ok -> ok;
		error -> error;
		{'EXIT',Reason} ->
		    io:format("Compiler function ~w:~w/3 failed:\n~p~n",
			      [M,F,Reason]),
		    error;
		Other ->
		    io:format("Compiler function ~w:~w/3 returned:\n~p~n",
			      [M,F,Other]),
		    error
	    end
    end.

%% Guesses if a give name refers to a file or a directory.

file_or_directory(Name) ->
    case file:read_file_info(Name) of
	{ok, #file_info{type=regular}} ->
	    file;
	{ok, _} ->
	    directory;
	{error, _} ->
	    case filename:extension(Name) of
		[] -> directory;
		_Other -> file
	    end
    end.

%% Makes an Erlang term given a string.

make_term(Str) -> 
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->		  
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok, Term} -> Term;
		{error, {_,_,Reason}} ->
		    io:format("~s: ~s~n", [Reason, Str]),
		    throw(error)
	    end;
	{error, {_,_,Reason}, _} ->
	    io:format("~s: ~s~n", [Reason, Str]),
	    throw(error)
    end.
