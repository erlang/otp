%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(erl_compile).

-include("erl_compile.hrl").
-include("file.hrl").

-export([compile_cmdline/0]).

-export_type([cmd_line_arg/0]).

-define(STDERR, standard_error).		%Macro to avoid misspellings.

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
compiler(_) ->         no.

%% Entry from command line.

-type cmd_line_arg() :: atom() | string().

-spec compile_cmdline() -> no_return().

compile_cmdline() ->
    List = init:get_plain_arguments(),
    case compile(List) of
	ok -> my_halt(0);
	error -> my_halt(1);
	_ -> my_halt(2)
    end.

-spec my_halt(_) -> no_return().
my_halt(Reason) ->
    erlang:halt(Reason).

%% Run the the compiler in a separate process, trapping EXITs.

compile(List) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(compiler_runner(List)),
    receive
	{'EXIT', Pid, {compiler_result, Result}} ->
	    Result;
	{'EXIT', Pid, {compiler_error, Error}} ->
	    io:put_chars(?STDERR, Error),
	    io:nl(?STDERR),
	    error;
	{'EXIT', Pid, Reason} ->
	    io:format(?STDERR, "Runtime error: ~tp~n", [Reason]),
	    error
    end.

-spec compiler_runner([cmd_line_arg()]) -> fun(() -> no_return()).

compiler_runner(List) ->
    fun() ->
            %% We don't want the current directory in the code path.
            %% Remove it.
            Path = [D || D <- code:get_path(), D =/= "."],
            true = code:set_path(Path),
            exit({compiler_result, compile1(List)})
    end.

%% Parses the first part of the option list.

compile1(Args) ->
    {ok, Cwd} = file:get_cwd(),
    compile1(Args, #options{outdir=Cwd,cwd=Cwd}).

%% Parses all options.

compile1(["--"|Files], Opts) ->
    compile2(Files, Opts);
compile1(["-"++Option|T], Opts) ->
    parse_generic_option(Option, T, Opts);
compile1(["+"++Option|Rest], Opts) ->
    Term = make_term(Option),
    Specific = Opts#options.specific,
    compile1(Rest, Opts#options{specific=[Term|Specific]});
compile1(Files, Opts) ->
    compile2(Files, Opts).

parse_generic_option("b"++Opt, T0, Opts) ->
    {OutputType,T} = get_option("b", Opt, T0),
    compile1(T, Opts#options{output_type=list_to_atom(OutputType)});
parse_generic_option("D"++Opt, T0, #options{defines=Defs}=Opts) ->
    {Val0,T} = get_option("D", Opt, T0),
    {Key0,Val1} = split_at_equals(Val0, []),
    Key = list_to_atom(Key0),
    case Val1 of
	[] ->
	    compile1(T, Opts#options{defines=[Key|Defs]});
	Val2 ->
	    Val = make_term(Val2),
	    compile1(T, Opts#options{defines=[{Key,Val}|Defs]})
    end;
parse_generic_option("help", _, _Opts) ->
    usage();
parse_generic_option("I"++Opt, T0, #options{cwd=Cwd}=Opts) ->
    {Dir,T} = get_option("I", Opt, T0),
    AbsDir = filename:absname(Dir, Cwd),
    compile1(T, Opts#options{includes=[AbsDir|Opts#options.includes]});
parse_generic_option("M"++Opt, T0, #options{specific=Spec}=Opts) ->
    case parse_dep_option(Opt, T0) of
	error ->
	    error;
	{SpecOpts,T} ->
	    compile1(T, Opts#options{specific=SpecOpts++Spec})
    end;
parse_generic_option("o"++Opt, T0, #options{cwd=Cwd}=Opts) ->
    {Dir,T} = get_option("o", Opt, T0),
    AbsName = filename:absname(Dir, Cwd),
    case file_or_directory(AbsName) of
	file ->
	    compile1(T, Opts#options{outfile=AbsName});
	directory ->
	    compile1(T, Opts#options{outdir=AbsName})
    end;
parse_generic_option("O"++Opt, T, Opts) ->
    case Opt of
	"" ->
	    compile1(T, Opts#options{optimize=1});
	_ ->
	    Term = make_term(Opt),
	    compile1(T, Opts#options{optimize=Term})
    end;
parse_generic_option("v", T, Opts) ->
    compile1(T, Opts#options{verbose=true});
parse_generic_option("W"++Warn, T, #options{specific=Spec}=Opts) ->
    case Warn of
	"all" ->
	    compile1(T, Opts#options{warning=999});
	"error" ->
	    compile1(T, Opts#options{specific=[warnings_as_errors|Spec]});
	"" ->
	    compile1(T, Opts#options{warning=1});
	_ ->
	    try	list_to_integer(Warn) of
		Level ->
		    compile1(T, Opts#options{warning=Level})
	    catch
		error:badarg ->
		    usage()
	    end
    end;
parse_generic_option("E", T, #options{specific=Spec}=Opts) ->
    compile1(T, Opts#options{specific=['E'|Spec]});
parse_generic_option("P", T, #options{specific=Spec}=Opts) ->
    compile1(T, Opts#options{specific=['P'|Spec]});
parse_generic_option("S", T, #options{specific=Spec}=Opts) ->
    compile1(T, Opts#options{specific=['S'|Spec]});
parse_generic_option(Option, _T, _Opts) ->
    io:format(?STDERR, "Unknown option: -~s\n", [Option]),
    usage().

parse_dep_option("", T) ->
    {[makedep,{makedep_output,standard_io}],T};
parse_dep_option("D", T) ->
    {[makedep],T};
parse_dep_option("F"++Opt, T0) ->
    {File,T} = get_option("MF", Opt, T0),
    {[makedep,{makedep_output,File}],T};
parse_dep_option("G", T) ->
    {[makedep_add_missing],T};
parse_dep_option("P", T) ->
    {[makedep_phony],T};
parse_dep_option("Q"++Opt, T0) ->
    {Target,T} = get_option("MT", Opt, T0),
    {[makedep_quote_target,{makedep_target,Target}],T};
parse_dep_option("T"++Opt, T0) ->
    {Target,T} = get_option("MT", Opt, T0),
    {[{makedep_target,Target}],T};
parse_dep_option(Opt, _T) ->
    io:format(?STDERR, "Unknown option: -M~s\n", [Opt]),
    usage().

usage() ->
    H = [{"-b type","type of output file (e.g. beam)"},
	 {"-d","turn on debugging of erlc itself"},
	 {"-Dname","define name"},
	 {"-Dname=value","define name to have value"},
	 {"-help","shows this help text"},
	 {"-I path","where to search for include files"},
	 {"-M","generate a rule for make(1) describing the dependencies"},
	 {"-MF file","write the dependencies to 'file'"},
	 {"-MT target","change the target of the rule emitted by dependency "
	  "generation"},
	 {"-MQ target","same as -MT but quote characters special to make(1)"},
	 {"-MG","consider missing headers as generated files and add them to "
	  "the dependencies"},
	 {"-MP","add a phony target for each dependency"},
	 {"-MD","same as -M -MT file (with default 'file')"},
	 {"-o name","name output directory or file"},
	 {"-pa path","add path to the front of Erlang's code path"},
	 {"-pz path","add path to the end of Erlang's code path"},
	 {"-smp","compile using SMP emulator"},
	 {"-v","verbose compiler output"},
	 {"-Werror","make all warnings into errors"},
	 {"-W0","disable warnings"},
	 {"-Wnumber","set warning level to number"},
	 {"-Wall","enable all warnings"},
	 {"-W","enable warnings (default; same as -W1)"},
	 {"-E","generate listing of expanded code (Erlang compiler)"},
	 {"-S","generate assembly listing (Erlang compiler)"},
	 {"-P","generate listing of preprocessed code (Erlang compiler)"},
	 {"+term","pass the Erlang term unchanged to the compiler"}],
    io:put_chars(?STDERR,
		 ["Usage: erlc [Options] file.ext ...\n",
		  "Options:\n",
		  [io_lib:format("~-14s ~s\n", [K,D]) || {K,D} <- H]]),
    error.

get_option(_Name, [], [[C|_]=Option|T]) when C =/= $- ->
    {Option,T};
get_option(_Name, [_|_]=Option, T) ->
    {Option,T};
get_option(Name, _, _) ->
    exit({compiler_error,"No value given to -"++Name++" option"}).

split_at_equals([$=|T], Acc) ->
    {lists:reverse(Acc),T};
split_at_equals([H|T], Acc) ->
    split_at_equals(T, [H|Acc]);
split_at_equals([], Acc) ->
    {lists:reverse(Acc),[]}.

compile2(Files, #options{cwd=Cwd,includes=Incl,outfile=Outfile}=Opts0) ->
    Opts = Opts0#options{includes=lists:reverse(Incl)},
    case {Outfile,length(Files)} of
	{"", _} ->
	    compile3(Files, Cwd, Opts);
	{[_|_], 1} ->
	    compile3(Files, Cwd, Opts);
	{[_|_], _N} ->
	    io:put_chars(?STDERR,
			 "Output file name given, "
			 "but more than one input file.\n"),
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
    io:format(?STDERR, "File has no extension: ~ts~n", [Input]),
    error;
compile_file(Ext, Input, Output, Options) ->
    case compiler(Ext) of
	no ->
	    io:format(?STDERR, "Unknown extension: '~ts'\n", [Ext]),
	    error;
	{M, F} ->
	    case catch M:F(Input, Output, Options) of
		ok -> ok;
		error -> error;
		{'EXIT',Reason} ->
		    io:format(?STDERR,
			      "Compiler function ~w:~w/3 failed:\n~p~n",
			      [M,F,Reason]),
		    error;
		Other ->
		    io:format(?STDERR,
			      "Compiler function ~w:~w/3 returned:\n~p~n",
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
		    io:format(?STDERR, "~ts: ~ts~n", [Reason, Str]),
		    throw(error)
	    end;
	{error, {_,_,Reason}, _} ->
	    io:format(?STDERR, "~ts: ~ts~n", [Reason, Str]),
	    throw(error)
    end.
