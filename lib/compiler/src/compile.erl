%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
%% Purpose: Run the Erlang compiler.

-module(compile).

%% High-level interface.
-export([file/1,file/2,noenv_file/2,format_error/1,iofile/1]).
-export([forms/1,forms/2,noenv_forms/2]).
-export([output_generated/1,noenv_output_generated/1]).
-export([options/0]).

%% Erlc interface.
-export([compile/3,compile_beam/3,compile_asm/3,compile_core/3]).

-export_type([option/0]).

-include("erl_compile.hrl").
-include("core_parse.hrl").

-import(lists, [member/2,reverse/1,reverse/2,keyfind/3,last/1,
		map/2,flatmap/2,foreach/2,foldr/3,any/2]).

%%----------------------------------------------------------------------

-type option() :: atom() | {atom(), term()} | {'d', atom(), term()}.

-type err_info() :: {erl_anno:line() | 'none',
		     module(), term()}. %% ErrorDescriptor
-type errors()   :: [{file:filename(), [err_info()]}].
-type warnings() :: [{file:filename(), [err_info()]}].
-type mod_ret()  :: {'ok', module()}
                  | {'ok', module(), cerl:c_module()} %% with option 'to_core'
                  | {'ok', module(), warnings()}.
-type bin_ret()  :: {'ok', module(), binary()}
                  | {'ok', module(), binary(), warnings()}.
-type err_ret()  :: 'error' | {'error', errors(), warnings()}.
-type comp_ret() :: mod_ret() | bin_ret() | err_ret().

%%----------------------------------------------------------------------

%%
%%  Exported functions
%%


%% file(FileName)
%% file(FileName, Options)
%%  Compile the module in file FileName.

-define(DEFAULT_OPTIONS, [verbose,report_errors,report_warnings]).

-spec file(module() | file:filename()) -> comp_ret().

file(File) -> file(File, ?DEFAULT_OPTIONS).

-spec file(module() | file:filename(), [option()] | option()) -> comp_ret().

file(File, Opts) when is_list(Opts) ->
    do_compile({file,File}, Opts++env_default_opts());
file(File, Opt) ->
    file(File, [Opt|?DEFAULT_OPTIONS]).

forms(File) -> forms(File, ?DEFAULT_OPTIONS).

forms(Forms, Opts) when is_list(Opts) ->
    do_compile({forms,Forms}, [binary|Opts++env_default_opts()]);
forms(Forms, Opt) when is_atom(Opt) ->
    forms(Forms, [Opt|?DEFAULT_OPTIONS]).

%% Given a list of compilation options, returns true if compile:file/2
%% would have generated a Beam file, false otherwise (if only a binary or a
%% listing file would have been generated).

-spec output_generated([option()]) -> boolean().

output_generated(Opts) ->
    noenv_output_generated(Opts++env_default_opts()).

%%
%% Variants of the same function that don't consult ERL_COMPILER_OPTIONS
%% for default options.
%%

-spec noenv_file(module() | file:filename(), [option()] | option()) -> comp_ret().

noenv_file(File, Opts) when is_list(Opts) ->
    do_compile({file,File}, Opts);
noenv_file(File, Opt) ->
    noenv_file(File, [Opt|?DEFAULT_OPTIONS]).

noenv_forms(Forms, Opts) when is_list(Opts) ->
    do_compile({forms,Forms}, [binary|Opts]);
noenv_forms(Forms, Opt) when is_atom(Opt) ->
    noenv_forms(Forms, [Opt|?DEFAULT_OPTIONS]).

-spec noenv_output_generated([option()]) -> boolean().

noenv_output_generated(Opts) ->
    {_,Passes} = passes(file, expand_opts(Opts)),
    any(fun ({save_binary,_T,_F}) -> true;
	    (_Other) -> false
	end, Passes).

%%
%%  Local functions
%%

-define(pass(P), {P,fun P/1}).
-define(pass(P,T), {P,fun T/1,fun P/1}).

env_default_opts() ->
    Key = "ERL_COMPILER_OPTIONS",
    case os:getenv(Key) of
	false -> [];
	Str when is_list(Str) ->
	    case erl_scan:string(Str) of
		{ok,Tokens,_} ->
                    Dot = {dot, erl_anno:new(1)},
		    case erl_parse:parse_term(Tokens ++ [Dot]) of
			{ok,List} when is_list(List) -> List;
			{ok,Term} -> [Term];
			{error,_Reason} ->
			    io:format("Ignoring bad term in ~s\n", [Key]),
			    []
		    end;
		{error, {_,_,_Reason}, _} ->
		    io:format("Ignoring bad term in ~s\n", [Key]),
		    []
	    end
    end.

do_compile(Input, Opts0) ->
    Opts = expand_opts(Opts0),
    {Pid,Ref} =
	spawn_monitor(fun() ->
			      exit(try
				       internal(Input, Opts)
				   catch
				       error:Reason ->
					   {error,Reason}
				   end)
		      end),
    receive
	{'DOWN',Ref,process,Pid,Rep} -> Rep
    end.

expand_opts(Opts0) ->
    %% {debug_info_key,Key} implies debug_info.
    Opts = case {proplists:get_value(debug_info_key, Opts0),
		 proplists:get_value(encrypt_debug_info, Opts0),
		 proplists:get_bool(debug_info, Opts0)} of
	       {undefined,undefined,_} -> Opts0;
	       {_,_,false} -> [debug_info|Opts0];
	       {_,_,_} -> Opts0
	   end,
    foldr(fun expand_opt/2, [], Opts).

expand_opt(basic_validation, Os) ->
    [no_code_generation,to_pp,binary|Os];
expand_opt(strong_validation, Os) ->
    [no_code_generation,to_kernel,binary|Os];
expand_opt(report, Os) ->
    [report_errors,report_warnings|Os];
expand_opt(return, Os) ->
    [return_errors,return_warnings|Os];
expand_opt(r12, Os) ->
    [no_recv_opt,no_line_info|Os];
expand_opt(r13, Os) ->
    [no_recv_opt,no_line_info|Os];
expand_opt(r14, Os) ->
    [no_line_info|Os];
expand_opt({debug_info_key,_}=O, Os) ->
    [encrypt_debug_info,O|Os];
expand_opt(no_float_opt, Os) ->
    %%Turn off the entire type optimization pass.
    [no_topt|Os];
expand_opt(O, Os) -> [O|Os].

%% format_error(ErrorDescriptor) -> string()

format_error(no_native_support) ->
    "this system is not configured for native-code compilation.";
format_error(no_crypto) ->
    "this system is not configured with crypto support.";
format_error(bad_crypto_key) ->
    "invalid crypto key.";
format_error(no_crypto_key) ->
    "no crypto key supplied.";
format_error({native, E}) ->
    io_lib:fwrite("native-code compilation failed with reason: ~tP.",
		  [E, 25]);
format_error({native_crash,E,Stk}) ->
    io_lib:fwrite("native-code compilation crashed with reason: ~tP.\n~tP\n",
		  [E,25,Stk,25]);
format_error({open,E}) ->
    io_lib:format("open error '~ts'", [file:format_error(E)]);
format_error({epp,E}) ->
    epp:format_error(E);
format_error(write_error) ->
    "error writing file";
format_error({rename,From,To,Error}) ->
    io_lib:format("failed to rename ~ts to ~ts: ~ts",
		  [From,To,file:format_error(Error)]);
format_error({delete,File,Error}) ->
    io_lib:format("failed to delete file ~ts: ~ts",
		  [File,file:format_error(Error)]);
format_error({delete_temp,File,Error}) ->
    io_lib:format("failed to delete temporary file ~ts: ~ts",
		  [File,file:format_error(Error)]);
format_error({parse_transform,M,R}) ->
    io_lib:format("error in parse transform '~s': ~tp", [M, R]);
format_error({undef_parse_transform,M}) ->
    io_lib:format("undefined parse transform '~s'", [M]);
format_error({core_transform,M,R}) ->
    io_lib:format("error in core transform '~s': ~tp", [M, R]);
format_error({crash,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\ncrash reason: ~ts", [Pass,format_error_reason(Reason)]);
format_error({bad_return,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\nbad return value: ~ts", [Pass,format_error_reason(Reason)]);
format_error({module_name,Mod,Filename}) ->
    io_lib:format("Module name '~s' does not match file name '~ts'", [Mod,Filename]);
format_error(reparsing_invalid_unicode) ->
    "Non-UTF-8 character(s) detected, but no encoding declared. Encode the file in UTF-8 or add \"%% coding: latin-1\" at the beginning of the file. Retrying with latin-1 encoding.".

format_error_reason({Reason, Stack}) when is_list(Stack) ->
    StackFun = fun
	(escript, run,      2) -> true;
	(escript, start,    1) -> true;
	(init,    start_it, 1) -> true;
	(init,    start_em, 1) -> true;
	(_Mod, _Fun, _Arity)   -> false
    end,
    FormatFun = fun (Term, _) -> io_lib:format("~tp", [Term]) end,
    [io_lib:format("~tp", [Reason]),"\n\n",
     lib:format_stacktrace(1, Stack, StackFun, FormatFun)];
format_error_reason(Reason) ->
    io_lib:format("~tp", [Reason]).

%% The compile state record.
-record(compile, {filename="" :: file:filename(),
		  dir=""      :: file:filename(),
		  base=""     :: file:filename(),
		  ifile=""    :: file:filename(),
		  ofile=""    :: file:filename(),
		  module=[],
		  code=[],
		  core_code=[],
		  abstract_code=[],		%Abstract code for debugger.
		  options=[]  :: [option()],	%Options for compilation
		  mod_options=[]  :: [option()], %Options for module_info
                  encoding=none :: none | epp:source_encoding(),
		  errors=[],
		  warnings=[]}).

internal({forms,Forms}, Opts0) ->
    {_,Ps} = passes(forms, Opts0),
    Source = proplists:get_value(source, Opts0, ""),
    Opts1 = proplists:delete(source, Opts0),
    Compile = #compile{code=Forms,options=Opts1,mod_options=Opts1},
    internal_comp(Ps, Source, "", Compile);
internal({file,File}, Opts) ->
    {Ext,Ps} = passes(file, Opts),
    Compile = #compile{options=Opts,mod_options=Opts},
    internal_comp(Ps, File, Ext, Compile).

internal_comp(Passes, File, Suffix, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    St1 = St0#compile{filename=File, dir=Dir, base=Base,
		      ifile=erlfile(Dir, Base, Suffix),
		      ofile=objfile(Base, St0)},
    Opts = St1#compile.options,
    Run0 = case member(time, Opts) of
	       true  ->
		   io:format("Compiling ~tp\n", [File]),
		   fun run_tc/2;
	       false -> fun({_Name,Fun}, St) -> catch Fun(St) end
	   end,
    Run = case keyfind(eprof, 1, Opts) of
	      {eprof,EprofPass} ->
		  fun(P, St) ->
			  run_eprof(P, EprofPass, St)
		  end;
	      false ->
		  Run0
	  end,
    case fold_comp(Passes, Run, St1) of
	{ok,St2} -> comp_ret_ok(St2);
	{error,St2} -> comp_ret_err(St2)
    end.

fold_comp([{delay,Ps0}|Passes], Run, #compile{options=Opts}=St) ->
    Ps = select_passes(Ps0, Opts) ++ Passes,
    fold_comp(Ps, Run, St);
fold_comp([{Name,Test,Pass}|Ps], Run, St) ->
    case Test(St) of
	false ->				%Pass is not needed.
	    fold_comp(Ps, Run, St);
	true ->					%Run pass in the usual way.
	    fold_comp([{Name,Pass}|Ps], Run, St)
    end;
fold_comp([{Name,Pass}|Ps], Run, St0) ->
    case Run({Name,Pass}, St0) of
	{ok,St1} -> fold_comp(Ps, Run, St1);
	{error,_St1} = Error -> Error;
	{'EXIT',Reason} ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{crash,Name,Reason}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}};
	Other ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{bad_return,Name,Other}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}}
    end;
fold_comp([], _Run, St) -> {ok,St}.

run_tc({Name,Fun}, St) ->
    T1 = erlang:monotonic_time(),
    Val = (catch Fun(St)),
    T2 = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(T2 - T1, native, milli_seconds),
    Mem0 = erts_debug:flat_size(Val)*erlang:system_info(wordsize),
    Mem = lists:flatten(io_lib:format("~.1f kB", [Mem0/1024])),
    io:format(" ~-30s: ~10.3f s ~12s\n",
	      [Name,Elapsed/1000,Mem]),
    Val.

run_eprof({Name,Fun}, Name, St) ->
    io:format("~p: Running eprof\n", [Name]),
    c:appcall(tools, eprof, start_profiling, [[self()]]),
    Val = (catch Fun(St)),
    c:appcall(tools, eprof, stop_profiling, []),
    c:appcall(tools, eprof, analyze, []),
    Val;
run_eprof({_,Fun}, _, St) ->
    catch Fun(St).

comp_ret_ok(#compile{code=Code,warnings=Warn0,module=Mod,options=Opts}=St) ->
    case werror(St) of
        true ->
            case member(report_warnings, Opts) of
                true ->
		    io:format("~p: warnings being treated as errors\n",
			      [?MODULE]);
                false ->
		    ok
            end,
            comp_ret_err(St);
        false ->
            Warn = messages_per_file(Warn0),
            report_warnings(St#compile{warnings = Warn}),
            Ret1 = case member(binary, Opts) andalso
		       not member(no_code_generation, Opts) of
                       true -> [Code];
                       false -> []
                   end,
            Ret2 = case member(return_warnings, Opts) of
                       true -> Ret1 ++ [Warn];
                       false -> Ret1
                   end,
            list_to_tuple([ok,Mod|Ret2])
    end.

comp_ret_err(#compile{warnings=Warn0,errors=Err0,options=Opts}=St) ->
    Warn = messages_per_file(Warn0),
    Err = messages_per_file(Err0),
    report_errors(St#compile{errors=Err}),
    report_warnings(St#compile{warnings=Warn}),
    case member(return_errors, Opts) of
	true -> {error,Err,Warn};
	false -> error
    end.

not_werror(St) -> not werror(St).

werror(#compile{options=Opts,warnings=Ws}) ->
    Ws =/= [] andalso member(warnings_as_errors, Opts).

%% messages_per_file([{File,[Message]}]) -> [{File,[Message]}]
messages_per_file(Ms) ->
    T = lists:sort([{File,M} || {File,Messages} <- Ms, M <- Messages]),
    PrioMs = [erl_scan, epp, erl_parse],
    {Prio0, Rest} =
        lists:mapfoldl(fun(M, A) ->
                               lists:partition(fun({_,{_,Mod,_}}) -> Mod =:= M;
                                                  (_) -> false
                                               end, A)
                       end, T, PrioMs),
    Prio = lists:sort(fun({_,{L1,_,_}}, {_,{L2,_,_}}) -> L1 =< L2 end,
                      lists:append(Prio0)),
    flatmap(fun mpf/1, [Prio, Rest]).

mpf(Ms) ->
    [{File,[M || {F,M} <- Ms, F =:= File]} ||
	File <- lists:usort([F || {F,_} <- Ms])].

%% passes(forms|file, [Option]) -> {Extension,[{Name,PassFun}]}
%%  Figure out the extension of the input file and which passes
%%  that need to be run.

passes(Type, Opts) ->
    {Ext,Passes0} = passes_1(Opts),
    Passes1 = case Type of
		  file -> Passes0;
		  forms -> tl(Passes0)
	      end,
    Passes = select_passes(Passes1, Opts),

    %% If the last pass saves the resulting binary to a file,
    %% insert a first pass to remove the file (unless the
    %% source file is a BEAM file).
    {Ext,case last(Passes) of
	     {save_binary,_TestFun,_Fun} ->
		 case Passes of
		     [{read_beam_file,_}|_] ->
			 %% The BEAM is both input and output.
			 %% Don't remove it.
			 Passes;
		     _ ->
			 [?pass(remove_file)|Passes]
		 end;
	     _ ->
		 Passes
	 end}.

passes_1([Opt|Opts]) ->
    case pass(Opt) of
	{_,_}=Res -> Res;
	none -> passes_1(Opts)
    end;
passes_1([]) ->
    {".erl",[?pass(parse_module)|standard_passes()]}.

pass(from_core) ->
    {".core",[?pass(parse_core)|core_passes()]};
pass(from_asm) ->
    {".S",[?pass(beam_consult_asm)|asm_passes()]};
pass(from_beam) ->
    {".beam",[?pass(read_beam_file)|binary_passes()]};
pass(_) -> none.

%% select_passes([Command], Opts) -> [{Name,Function}]
%%  Interpret the lists of commands to return a pure list of passes.
%%
%%  Command can be one of:
%%
%%    {pass,Mod}	Will be expanded to a call to the external
%%			function Mod:module(Code, Options).  This
%%			function must transform the code and return
%%			{ok,NewCode} or {error,Term}.
%%			Example: {pass,beam_codegen}
%%
%%    {Name,Fun}	Name is an atom giving the name of the pass.
%%    			Fun is an 'fun' taking one argument: a compile record.
%%			The fun should return {ok,NewCompileRecord} or
%%			{error,NewCompileRecord}.
%%			Note: ?pass(Name) is equvivalent to {Name,fun Name/1}.
%%			Example: ?pass(parse_module)
%%
%%    {Name,Test,Fun}	Like {Name,Fun} above, but the pass will be run
%%			(and listed by the `time' option) only if Test(St)
%%			returns true.
%%
%%    {src_listing,Ext}	Produces an Erlang source listing with the
%%			the file extension Ext.  (Ext should not contain
%%			a period.)  No more passes will be run.
%%
%%    {listing,Ext}	Produce an listing of the terms in the internal
%%			representation.  The extension of the listing
%%			file will be Ext.  (Ext should not contain
%%			a period.)   No more passes will be run.
%%
%%    done              End compilation at this point.
%%
%%    {done,Ext}        End compilation at this point. Produce a listing
%%                      as with {listing,Ext}, unless 'binary' is
%%                      specified, in which case the current
%%                      representation of the code is returned without
%%                      creating an output file.
%%
%%    {iff,Flag,Cmd}	If the given Flag is given in the option list,
%%			Cmd will be interpreted as a command.
%%			Otherwise, Cmd will be ignored.
%%			Example: {iff,dcg,{listing,"codegen}}
%%
%%    {unless,Flag,Cmd}	If the given Flag is NOT given in the option list,
%%			Cmd will be interpreted as a command.
%%			Otherwise, Cmd will be ignored.
%%			Example: {unless,no_kernopt,{pass,sys_kernopt}}
%%

select_passes([{pass,Mod}|Ps], Opts) ->
    F = fun(St) ->
		case catch Mod:module(St#compile.code, St#compile.options) of
		    {ok,Code} ->
			{ok,St#compile{code=Code}};
		    {ok,Code,Ws} ->
			{ok,St#compile{code=Code,warnings=St#compile.warnings++Ws}};
		    {error,Es} ->
			{error,St#compile{errors=St#compile.errors ++ Es}}
		end
	end,
    [{Mod,F}|select_passes(Ps, Opts)];
select_passes([{src_listing,Ext}|_], _Opts) ->
    [{listing,fun (St) -> src_listing(Ext, St) end}];
select_passes([{listing,Ext}|_], _Opts) ->
    [{listing,fun (St) -> listing(Ext, St) end}];
select_passes([done|_], _Opts) ->
    [];
select_passes([{done,Ext}|_], Opts) ->
    select_passes([{unless,binary,{listing,Ext}}], Opts);
select_passes([{iff,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, true, Pass, Ps, Opts);
select_passes([{unless,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, false, Pass, Ps, Opts);
select_passes([{_,Fun}=P|Ps], Opts) when is_function(Fun) ->
    [P|select_passes(Ps, Opts)];
select_passes([{delay,Passes0}|Ps], Opts) when is_list(Passes0) ->
    %% Delay evaluation of compiler options and which compiler passes to run.
    %% Since we must know beforehand whether a listing will be produced, we
    %% will go through the list of passes and evaluate all conditions that
    %% select a list pass.
    case select_list_passes(Passes0, Opts) of
	{done,Passes} ->
	    [{delay,Passes}];
	{not_done,Passes} ->
	    [{delay,Passes}|select_passes(Ps, Opts)]
    end;
select_passes([{_,Test,Fun}=P|Ps], Opts) when is_function(Test),
					      is_function(Fun) ->
    [P|select_passes(Ps, Opts)];
select_passes([], _Opts) ->
    [];
select_passes([List|Ps], Opts) when is_list(List) ->
    case select_passes(List, Opts) of
	[] -> select_passes(Ps, Opts);
	Nested ->
	    case last(Nested) of
		{listing,_Fun} -> Nested;
		_Other         -> Nested ++ select_passes(Ps, Opts)
	    end
    end.

select_cond(Flag, ShouldBe, Pass, Ps, Opts) ->
    ShouldNotBe = not ShouldBe,
    case member(Flag, Opts) of
	ShouldBe    -> select_passes([Pass|Ps], Opts);
	ShouldNotBe -> select_passes(Ps, Opts)
    end.

%% select_list_passes([Pass], Opts) -> {done,[Pass]} | {not_done,[Pass]}
%%  Evaluate all conditions having to do with listings in the list of
%%  passes.

select_list_passes(Ps, Opts) ->
    select_list_passes_1(Ps, Opts, []).

select_list_passes_1([{iff,Flag,{listing,_}=Listing}|Ps], Opts, Acc) ->
    case member(Flag, Opts) of
	true -> {done,reverse(Acc, [Listing])};
	false -> select_list_passes_1(Ps, Opts, Acc)
    end;
select_list_passes_1([{iff,Flag,{done,Ext}}|Ps], Opts, Acc) ->
    case member(Flag, Opts) of
	false ->
	    select_list_passes_1(Ps, Opts, Acc);
	true ->
	    {done,case member(binary, Opts) of
		      false -> reverse(Acc, [{listing,Ext}]);
		      true -> reverse(Acc)
		  end}
    end;
select_list_passes_1([{iff=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
	{done,List} -> {done,reverse(Acc) ++ List};
	{not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
    end;
select_list_passes_1([{unless=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
	{done,List} -> {done,reverse(Acc) ++ List};
	{not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
    end;
select_list_passes_1([P|Ps], Opts, Acc) ->
    select_list_passes_1(Ps, Opts, [P|Acc]);
select_list_passes_1([], _, Acc) ->
    {not_done,reverse(Acc)}.

%% The standard passes (almost) always run.

standard_passes() ->
    [?pass(transform_module),

     {iff,makedep,[
	 ?pass(makedep),
	 {unless,binary,?pass(makedep_output)}
       ]},
     {iff,makedep,done},

     {iff,'dpp',{listing,"pp"}},
     ?pass(lint_module),
     {iff,'P',{src_listing,"P"}},
     {iff,'to_pp',{done,"P"}},

     {iff,'dabstr',{listing,"abstr"}},
     {iff,debug_info,?pass(save_abstract_code)},

     ?pass(expand_module),
     {iff,'dexp',{listing,"expand"}},
     {iff,'E',{src_listing,"E"}},
     {iff,'to_exp',{done,"E"}},

     %% Conversion to Core Erlang.
     {pass,v3_core},
     {iff,'dcore',{listing,"core"}},
     {iff,'to_core0',{done,"core"}}
     | core_passes()].

core_passes() ->
    %% Optimization and transforms of Core Erlang code.
    [{iff,clint0,?pass(core_lint_module)},
     {delay,
      [{unless,no_copt,
       [{core_old_inliner,fun test_old_inliner/1,fun core_old_inliner/1},
	{iff,doldinline,{listing,"oldinline"}},
	{pass,sys_core_fold},
	{iff,dcorefold,{listing,"corefold"}},
	{core_inline_module,fun test_core_inliner/1,fun core_inline_module/1},
	{iff,dinline,{listing,"inline"}},
        {core_fold_after_inlining,fun test_any_inliner/1,
         fun core_fold_module_after_inlining/1},
	?pass(core_transforms)]},
       {iff,dcopt,{listing,"copt"}},
       {iff,'to_core',{done,"core"}}]}
     | kernel_passes()].

kernel_passes() ->
    %% Destructive setelement/3 optimization and core lint.
    [{pass,sys_core_dsetel},
     {iff,dsetel,{listing,"dsetel"}},

     {iff,clint,?pass(core_lint_module)},
     {iff,core,?pass(save_core_code)},

     %% Kernel Erlang and code generation.
     {pass,v3_kernel},
     {iff,dkern,{listing,"kernel"}},
     {iff,'to_kernel',{done,"kernel"}},
     {pass,v3_life},
     {iff,dlife,{listing,"life"}},
     {pass,v3_codegen},
     {iff,dcg,{listing,"codegen"}}
     | asm_passes()].

asm_passes() ->
    %% Assembly level optimisations.
    [{delay,
      [{pass,beam_a},
       {iff,da,{listing,"a"}},
       {unless,no_postopt,
	[{unless,no_reorder,{pass,beam_reorder}},
	 {iff,dre,{listing,"reorder"}},
	 {pass,beam_block},
	 {iff,dblk,{listing,"block"}},
	 {unless,no_except,{pass,beam_except}},
	 {iff,dexcept,{listing,"except"}},
	 {unless,no_bs_opt,{pass,beam_bs}},
	 {iff,dbs,{listing,"bs"}},
	 {unless,no_bopt,{pass,beam_bool}},
	 {iff,dbool,{listing,"bool"}},
	 {unless,no_topt,{pass,beam_type}},
	 {iff,dtype,{listing,"type"}},
	 {pass,beam_split},
	 {iff,dsplit,{listing,"split"}},
	 {unless,no_dead,{pass,beam_dead}},
	 {iff,ddead,{listing,"dead"}},
	 {unless,no_jopt,{pass,beam_jump}},
	 {iff,djmp,{listing,"jump"}},
	 {unless,no_peep_opt,{pass,beam_peep}},
	 {iff,dpeep,{listing,"peep"}},
	 {pass,beam_clean},
	 {iff,dclean,{listing,"clean"}},
	 {unless,no_bsm_opt,{pass,beam_bsm}},
	 {iff,dbsm,{listing,"bsm"}},
	 {unless,no_recv_opt,{pass,beam_receive}},
	 {iff,drecv,{listing,"recv"}},
	 {unless,no_stack_trimming,{pass,beam_trim}},
	 {iff,dtrim,{listing,"trim"}},
	 {pass,beam_flatten}]},

       %% If post optimizations are turned off, we still
       %% need to do a few clean-ups to code.
       {iff,no_postopt,[{pass,beam_clean}]},

       {pass,beam_z},
       {iff,dz,{listing,"z"}},
       {iff,dopt,{listing,"optimize"}},
       {iff,'S',{listing,"S"}},
       {iff,'to_asm',{done,"S"}}]},
     {pass,beam_validator},
     ?pass(beam_asm)
     | binary_passes()].

binary_passes() ->
    [{native_compile,fun test_native/1,fun native_compile/1},
     {unless,binary,?pass(save_binary,not_werror)}].

%%%
%%% Compiler passes.
%%%

%% Remove the target file so we don't have an old one if the compilation fail.
remove_file(St) ->
    _ = file:delete(St#compile.ofile),
    {ok,St}.

-record(asm_module, {module,
		     exports,
		     labels,
		     functions=[],
		     cfun,
		     code,
		     attributes=[]}).

preprocess_asm_forms(Forms) ->
    R = #asm_module{},
    R1 = collect_asm(Forms, R),
    {R1#asm_module.module,
     {R1#asm_module.module,
      R1#asm_module.exports,
      R1#asm_module.attributes,
      R1#asm_module.functions,
      R1#asm_module.labels}}.

collect_asm([], R) ->
    case R#asm_module.cfun of
	undefined ->
	    R;
	{A,B,C} ->
	    R#asm_module{functions=R#asm_module.functions++
			 [{function,A,B,C,R#asm_module.code}]}
    end;
collect_asm([{module,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{module=M});
collect_asm([{exports,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{exports=M});
collect_asm([{labels,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{labels=M});
collect_asm([{function,A,B,C} | Rest], R) ->
    R1 = case R#asm_module.cfun of
	     undefined ->
		 R;
	     {A0,B0,C0} ->
		 R#asm_module{functions=R#asm_module.functions++
			      [{function,A0,B0,C0,R#asm_module.code}]}
	 end,
    collect_asm(Rest, R1#asm_module{cfun={A,B,C}, code=[]});
collect_asm([{attributes, Attr} | Rest], R) ->
    collect_asm(Rest, R#asm_module{attributes=Attr});
collect_asm([X | Rest], R) ->
    collect_asm(Rest, R#asm_module{code=R#asm_module.code++[X]}).

beam_consult_asm(St) ->
    case file:consult(St#compile.ifile) of
	{ok, Forms0} ->
            Encoding = epp:read_encoding(St#compile.ifile),
	    {Module, Forms} = preprocess_asm_forms(Forms0),
	    {ok,St#compile{module=Module, code=Forms, encoding=Encoding}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

read_beam_file(St) ->
    case file:read_file(St#compile.ifile) of
	{ok,Beam} ->
	    Infile = St#compile.ifile,
	    case no_native_compilation(Infile, St) of
		true ->
		    {ok,St#compile{module=none,code=none}};
		false ->
		    Mod0 = filename:rootname(filename:basename(Infile)),
		    Mod = list_to_atom(Mod0),
		    {ok,St#compile{module=Mod,code=Beam,ofile=Infile}}
	    end;
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

no_native_compilation(BeamFile, #compile{options=Opts0}) ->
    case beam_lib:chunks(BeamFile, ["CInf"]) of
	{ok,{_,[{"CInf",Term0}]}} ->
	    Term = binary_to_term(Term0),

	    %% Compiler options in the beam file will override
	    %% options passed to the compiler.
	    Opts = proplists:get_value(options, Term, []) ++ Opts0,
	    member(no_new_funs, Opts) orelse not is_native_enabled(Opts);
	_ -> false
    end.

parse_module(St0) ->
    case do_parse_module(utf8, St0) of
	{ok,_}=Ret ->
	    Ret;
	{error,_}=Ret ->
	    Ret;
	{invalid_unicode,File,Line} ->
	    case do_parse_module(latin1, St0) of
		{ok,St} ->
		    Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
		    {ok,St#compile{warnings=Es++St#compile.warnings}};
		{error,St} ->
		    Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
		    {error,St#compile{errors=Es++St#compile.errors}}
	    end
    end.

do_parse_module(DefEncoding, #compile{ifile=File,options=Opts,dir=Dir}=St) ->
    R = epp:parse_file(File,
		       [{includes,[".",Dir|inc_paths(Opts)]},
			{macros,pre_defs(Opts)},
			{default_encoding,DefEncoding},
			extra]),
    case R of
	{ok,Forms,Extra} ->
	    Encoding = proplists:get_value(encoding, Extra),
	    case find_invalid_unicode(Forms, File) of
		none ->
		    {ok,St#compile{code=Forms,encoding=Encoding}};
		{invalid_unicode,_,_}=Ret ->
		    case Encoding of
			none ->
			    Ret;
			_ ->
			    {ok,St#compile{code=Forms,encoding=Encoding}}
		    end
	    end;
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{epp,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

find_invalid_unicode([H|T], File0) ->
    case H of
	{attribute,_,file,{File,_}} ->
	    find_invalid_unicode(T, File);
	{error,{Line,file_io_server,invalid_unicode}} ->
	    {invalid_unicode,File0,Line};
	_Other ->
	    find_invalid_unicode(T, File0)
    end;
find_invalid_unicode([], _) -> none.

parse_core(St) ->
    case file:read_file(St#compile.ifile) of
	{ok,Bin} ->
	    case core_scan:string(binary_to_list(Bin)) of
		{ok,Toks,_} ->
		    case core_parse:parse(Toks) of
			{ok,Mod} ->
			    Name = (Mod#c_module.name)#c_literal.val,
			    {ok,St#compile{module=Name,code=Mod}};
			{error,E} ->
			    Es = [{St#compile.ifile,[E]}],
			    {error,St#compile{errors=St#compile.errors ++ Es}}
		    end;
		{error,E,_} ->
		    Es = [{St#compile.ifile,[E]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,compile,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

compile_options([{attribute,_L,compile,C}|Fs]) when is_list(C) ->
    C ++ compile_options(Fs);
compile_options([{attribute,_L,compile,C}|Fs]) ->
    [C|compile_options(Fs)];
compile_options([_F|Fs]) -> compile_options(Fs);
compile_options([]) -> [].

clean_parse_transforms(Fs) ->
    clean_parse_transforms_1(Fs, []).

clean_parse_transforms_1([{attribute,L,compile,C0}|Fs], Acc) when is_list(C0) ->
    C = lists:filter(fun({parse_transform,_}) -> false;
			(_) -> true
		     end, C0),
    clean_parse_transforms_1(Fs, [{attribute,L,compile,C}|Acc]);
clean_parse_transforms_1([{attribute,_,compile,{parse_transform,_}}|Fs], Acc) ->
    clean_parse_transforms_1(Fs, Acc);
clean_parse_transforms_1([F|Fs], Acc) ->
    clean_parse_transforms_1(Fs, [F|Acc]);
clean_parse_transforms_1([], Acc) -> reverse(Acc).

transforms(Os) -> [ M || {parse_transform,M} <- Os ].

transform_module(#compile{options=Opt,code=Code0}=St0) ->
    %% Extract compile options from code into options field.
    case transforms(Opt ++ compile_options(Code0)) of
	[] -> {ok,St0};				%No parse transforms.
	Ts ->
	    %% Remove parse_transform attributes from the abstract code to
	    %% prevent parse transforms to be run more than once.
	    Code = clean_parse_transforms(Code0),
	    St = St0#compile{code=Code},
	    foldl_transform(St, Ts)
    end.

foldl_transform(St, [T|Ts]) ->
    Name = "transform " ++ atom_to_list(T),
    case code:ensure_loaded(T) =:= {module,T} andalso
                erlang:function_exported(T, parse_transform, 2) of
        true ->
            Fun = fun(S) ->
                         T:parse_transform(S#compile.code, S#compile.options)
                  end,
            Run = case member(time, St#compile.options) of
                      true  -> fun run_tc/2;
                      false -> fun({_Name,F}, S) -> catch F(S) end
                  end,
            case Run({Name, Fun}, St) of
                {error,Es,Ws} ->
                    {error,St#compile{warnings=St#compile.warnings ++ Ws,
                                      errors=St#compile.errors ++ Es}};
                {'EXIT',R} ->
                    Es = [{St#compile.ifile,[{none,compile,
                                              {parse_transform,T,R}}]}],
                    {error,St#compile{errors=St#compile.errors ++ Es}};
                {warning, Forms, Ws} ->
                    foldl_transform(
                      St#compile{code=Forms,
                                 warnings=St#compile.warnings ++ Ws}, Ts);
                Forms ->
                    foldl_transform(St#compile{code=Forms}, Ts)
            end;
        false ->
            Es = [{St#compile.ifile,[{none,compile,
                                      {undef_parse_transform,T}}]}],
            {error,St#compile{errors=St#compile.errors ++ Es}}
    end;
foldl_transform(St, []) -> {ok,St}.

get_core_transforms(Opts) -> [M || {core_transform,M} <- Opts].

core_transforms(St) ->
    %% The options field holds the complete list of options at this
    Ts = get_core_transforms(St#compile.options),
    foldl_core_transforms(St, Ts).

foldl_core_transforms(St, [T|Ts]) ->
    Name = "core transform " ++ atom_to_list(T),
    Fun = fun(S) -> T:core_transform(S#compile.code, S#compile.options) end,
    Run = case member(time, St#compile.options) of
	      true  -> fun run_tc/2;
	      false -> fun({_Name,F}, S) -> catch F(S) end
	  end,
    case Run({Name, Fun}, St) of
	{'EXIT',R} ->
	    Es = [{St#compile.ifile,[{none,compile,{core_transform,T,R}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}};
	Forms ->
	    foldl_core_transforms(St#compile{code=Forms}, Ts)
    end;
foldl_core_transforms(St, []) -> {ok,St}.

%%% Fetches the module name from a list of forms. The module attribute must
%%% be present.
get_module([{attribute,_,module,M} | _]) -> M;
get_module([_ | Rest]) ->
    get_module(Rest).

%%% A #compile state is returned, where St.base has been filled in
%%% with the module name from Forms, as a string, in case it wasn't
%%% set in St (i.e., it was "").
add_default_base(St, Forms) ->
    F = St#compile.filename,
    case F of
	"" ->
 	    M = get_module(Forms),
	    St#compile{base=atom_to_list(M)};
	_ ->
	    St
    end.

lint_module(St) ->
    case erl_lint:module(St#compile.code,
			 St#compile.ifile, St#compile.options) of
	{ok,Ws} ->
	    %% Insert name of module as base name, if needed. This is
	    %% for compile:forms to work with listing files.
	    St1 = add_default_base(St, St#compile.code),
	    {ok,St1#compile{warnings=St1#compile.warnings ++ Ws}};
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}}
    end.

core_lint_module(St) ->
    case core_lint:module(St#compile.code, St#compile.options) of
	{ok,Ws} ->
	    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}}
    end.

makedep(#compile{code=Code,options=Opts}=St) ->
    Ifile = St#compile.ifile,
    Ofile = St#compile.ofile,

    %% Get the target of the Makefile rule.
    Target0 =
	case proplists:get_value(makedep_target, Opts) of
	    undefined ->
		%% The target is derived from the output filename: possibly
		%% remove the current working directory to obtain a relative
		%% path.
		shorten_filename(Ofile);
	    T ->
		%% The caller specified one.
		T
	end,

    %% Quote the target is the called asked for this.
    Target1 = case proplists:get_value(makedep_quote_target, Opts) of
		  true ->
		      %% For now, only "$" is replaced by "$$".
		      Fun = fun
				($$) -> "$$";
				(C)  -> C
			    end,
		      map(Fun, Target0);
		  _ ->
		      Target0
	      end,
    Target = Target1 ++ ":",

    %% List the dependencies (includes) for this target.
    {MainRule,PhonyRules} = makedep_add_headers(
      Ifile,          % The input file name.
      Code,           % The parsed source.
      [],             % The list of dependencies already added.
      length(Target), % The current line length.
      Target,         % The target.
      "",             % Phony targets.
      Opts),

    %% Prepare the content of the Makefile. For instance:
    %%   hello.erl: hello.hrl common.hrl
    %%
    %% Or if phony targets are enabled:
    %%   hello.erl: hello.hrl common.hrl
    %%
    %%   hello.hrl:
    %%
    %%   common.hrl:
    Makefile = case proplists:get_value(makedep_phony, Opts) of
		   true -> MainRule ++ PhonyRules;
		   _ -> MainRule
	       end,
    {ok,St#compile{code=iolist_to_binary([Makefile,"\n"])}}.

makedep_add_headers(Ifile, [{attribute,_,file,{File,_}}|Rest],
		    Included, LineLen, MainTarget, Phony, Opts) ->
    %% The header "File" exists, add it to the dependencies.
    {Included1,LineLen1,MainTarget1,Phony1} =
	makedep_add_header(Ifile, Included, LineLen, MainTarget, Phony, File),
    makedep_add_headers(Ifile, Rest, Included1, LineLen1,
			MainTarget1, Phony1, Opts);
makedep_add_headers(Ifile, [{error,{_,epp,{include,file,File}}}|Rest],
		    Included, LineLen, MainTarget, Phony, Opts) ->
    %% The header "File" doesn't exist, do we add it to the dependencies?
    case proplists:get_value(makedep_add_missing, Opts) of
        true ->
            {Included1,LineLen1,MainTarget1,Phony1} =
		makedep_add_header(Ifile, Included, LineLen, MainTarget,
				   Phony, File),
            makedep_add_headers(Ifile, Rest, Included1, LineLen1,
				MainTarget1, Phony1, Opts);
        _ ->
            makedep_add_headers(Ifile, Rest, Included, LineLen,
				MainTarget, Phony, Opts)
    end;
makedep_add_headers(Ifile, [_|Rest], Included, LineLen,
		    MainTarget, Phony, Opts) ->
    makedep_add_headers(Ifile, Rest, Included,
			LineLen, MainTarget, Phony, Opts);
makedep_add_headers(_Ifile, [], _Included, _LineLen,
		    MainTarget, Phony, _Opts) ->
    {MainTarget,Phony}.

makedep_add_header(Ifile, Included, LineLen, MainTarget, Phony, File) ->
    case member(File, Included) of
	true ->
	    %% This file was already listed in the dependencies, skip it.
            {Included,LineLen,MainTarget,Phony};
	false ->
            Included1 = [File|Included],

	    %% Remove "./" in front of the dependency filename.
	    File1 = case File of
			"./" ++ File0 -> File0;
			_ -> File
	    end,

	    %% Prepare the phony target name.
	    Phony1 = case File of
			 Ifile -> Phony;
			 _     -> Phony ++ "\n\n" ++ File1 ++ ":"
	    end,

	    %% Add the file to the dependencies. Lines longer than 76 columns
	    %% are splitted.
	    if
		LineLen + 1 + length(File1) > 76 ->
                    LineLen1 = 2 + length(File1),
                    MainTarget1 = MainTarget ++ " \\\n  " ++ File1,
                    {Included1,LineLen1,MainTarget1,Phony1};
		true ->
                    LineLen1 = LineLen + 1 + length(File1),
                    MainTarget1 = MainTarget ++ " " ++ File1,
                    {Included1,LineLen1,MainTarget1,Phony1}
	    end
    end.

makedep_output(#compile{code=Code,options=Opts,ofile=Ofile}=St) ->
    %% Write this Makefile (Code) to the selected output.
    %% If no output is specified, the default is to write to a file named after
    %% the output file.
    Output0 = case proplists:get_value(makedep_output, Opts) of
		  undefined ->
		      %% Prepare the default filename.
		      outfile(filename:basename(Ofile, ".beam"), "Pbeam", Opts);
		  O ->
		      O
	      end,

    %% If the caller specified an io_device(), there's nothing to do. If he
    %% specified a filename, we must create it. Furthermore, this created file
    %% must be closed before returning.
    Ret = case Output0 of
	      _ when is_list(Output0) ->
		  case file:delete(Output0) of
		      Ret2 when Ret2 =:= ok; Ret2 =:= {error,enoent} ->
			  case file:open(Output0, [write]) of
			      {ok,IODev} ->
				  {ok,IODev,true};
			      {error,Reason2} ->
				  {error,open,Reason2}
			  end;
		      {error,Reason1} ->
			  {error,delete,Reason1}
		  end;
	      _ ->
		  {ok,Output0,false}
	  end,

    case Ret of
	{ok,Output1,CloseOutput} ->
	    try
		%% Write the Makefile.
		io:fwrite(Output1, "~ts", [Code]),
		%% Close the file if relevant.
		if
		    CloseOutput -> ok = file:close(Output1);
		    true -> ok
		end,
		{ok,St}
	    catch
		exit:_ ->
		    %% Couldn't write to output Makefile.
		    Err = {St#compile.ifile,[{none,?MODULE,write_error}]},
		    {error,St#compile{errors=St#compile.errors++[Err]}}
	    end;
	{error,open,Reason} ->
	    %% Couldn't open output Makefile.
	    Err = {St#compile.ifile,[{none,?MODULE,{open,Reason}}]},
	    {error,St#compile{errors=St#compile.errors++[Err]}};
	{error,delete,Reason} ->
	    %% Couldn't open output Makefile.
	    Err = {St#compile.ifile,[{none,?MODULE,{delete,Output0,Reason}}]},
	    {error,St#compile{errors=St#compile.errors++[Err]}}
    end.

%% expand_module(State) -> State'
%%  Do the common preprocessing of the input forms.

expand_module(#compile{code=Code,options=Opts0}=St0) ->
    {Mod,Exp,Forms,Opts1} = sys_pre_expand:module(Code, Opts0),
    Opts = expand_opts(Opts1),
    {ok,St0#compile{module=Mod,options=Opts,code={Mod,Exp,Forms}}}.

core_fold_module_after_inlining(#compile{code=Code0,options=Opts}=St) ->
    %% Inlining may produce code that generates spurious warnings.
    %% Ignore all warnings.
    {ok,Code,_Ws} = sys_core_fold:module(Code0, Opts),
    {ok,St#compile{code=Code}}.

test_old_inliner(#compile{options=Opts}) ->
    %% The point of this test is to avoid loading the old inliner
    %% if we know that it will not be used.
    any(fun({inline,_}) -> true;
	   (_) -> false
	end, Opts).

test_core_inliner(#compile{options=Opts}) ->
    case any(fun(no_inline) -> true;
		(_) -> false
	     end, Opts) of
	true -> false;
	false ->
	    any(fun(inline) -> true;
		   (_) -> false
		end, Opts)
    end.

test_any_inliner(St) ->
    test_old_inliner(St) orelse test_core_inliner(St).

core_old_inliner(#compile{code=Code0,options=Opts}=St) ->
    {ok,Code} = sys_core_inline:module(Code0, Opts),
    {ok,St#compile{code=Code}}.

core_inline_module(#compile{code=Code0,options=Opts}=St) ->
    Code = cerl_inline:core_transform(Code0, Opts),
    {ok,St#compile{code=Code}}.

save_abstract_code(#compile{ifile=File}=St) ->
    case abstract_code(St) of
	{ok,Code} ->
	    {ok,St#compile{abstract_code=Code}};
	{error,Es} ->
	    {error,St#compile{errors=St#compile.errors ++ [{File,Es}]}}
    end.

abstract_code(#compile{code=Code0,options=Opts,ofile=OFile}) ->
    Code = erl_parse:anno_to_term(Code0),
    Abstr = erlang:term_to_binary({raw_abstract_v1,Code}, [compressed]),
    case member(encrypt_debug_info, Opts) of
	true ->
	    case keyfind(debug_info_key, 1, Opts) of
		{_,Key} ->
		    encrypt_abs_code(Abstr, Key);
		false ->
		    %% Note: #compile.module has not been set yet.
		    %% Here is an approximation that should work for
		    %% all valid cases.
		    Module = list_to_atom(filename:rootname(filename:basename(OFile))),
		    Mode = proplists:get_value(crypto_mode, Opts, des3_cbc),
		    case beam_lib:get_crypto_key({debug_info, Mode, Module, OFile}) of
			error ->
			    {error, [{none,?MODULE,no_crypto_key}]};
			Key ->
			    encrypt_abs_code(Abstr, {Mode, Key})
		    end
	    end;
	false ->
	    {ok, Abstr}
    end.

encrypt_abs_code(Abstr, Key0) ->
    try
	RealKey = generate_key(Key0),
	case start_crypto() of
	    ok -> {ok,encrypt(RealKey, Abstr)};
	    {error,_}=E -> E
	end
    catch
	error:_ ->
	    {error,[{none,?MODULE,bad_crypto_key}]}
    end.

start_crypto() ->
    try crypto:start() of
	{error,{already_started,crypto}} -> ok;
	ok -> ok
    catch
	error:_ ->
	    {error,[{none,?MODULE,no_crypto}]}
    end.

generate_key({Type,String}) when is_atom(Type), is_list(String) ->
    beam_lib:make_crypto_key(Type, String);
generate_key(String) when is_list(String) ->
    generate_key({des3_cbc,String}).

encrypt({des3_cbc=Type,Key,IVec,BlockSize}, Bin0) ->
    Bin1 = case byte_size(Bin0) rem BlockSize of
	       0 -> Bin0;
	       N -> list_to_binary([Bin0,random_bytes(BlockSize-N)])
	   end,
    Bin = crypto:block_encrypt(Type, Key, IVec, Bin1),
    TypeString = atom_to_list(Type),
    list_to_binary([0,length(TypeString),TypeString,Bin]).

random_bytes(N) ->
    _ = random:seed(erlang:time_offset(),
		    erlang:monotonic_time(),
		    erlang:unique_integer()),
    random_bytes_1(N, []).

random_bytes_1(0, Acc) -> Acc;
random_bytes_1(N, Acc) -> random_bytes_1(N-1, [random:uniform(255)|Acc]).

save_core_code(St) ->
    {ok,St#compile{core_code=cerl:from_records(St#compile.code)}}.

beam_asm(#compile{ifile=File,code=Code0,
		  abstract_code=Abst,mod_options=Opts0}=St) ->
    Source = filename:absname(File),
    Opts1 = lists:map(fun({debug_info_key,_}) -> {debug_info_key,'********'};
			 (Other) -> Other
		      end, Opts0),
    Opts2 = [O || O <- Opts1, effects_code_generation(O)],
    case beam_asm:module(Code0, Abst, Source, Opts2) of
	{ok,Code} -> {ok,St#compile{code=Code,abstract_code=[]}}
    end.

test_native(#compile{options=Opts}) ->
    %% This test is done late, in case some other option has turned off native.
    %% 'native' given on the command line can be overridden by
    %% 'no_native' in the module itself.
    is_native_enabled(Opts).

is_native_enabled([native|_]) -> true;
is_native_enabled([no_native|_]) -> false;
is_native_enabled([_|Opts]) -> is_native_enabled(Opts);
is_native_enabled([]) -> false.

native_compile(#compile{code=none}=St) -> {ok,St};
native_compile(St) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    Ws = [{St#compile.ifile,[{none,compile,no_native_support}]}],
	    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
	_ ->
	    native_compile_1(St)
    end.

native_compile_1(St) ->
    Opts0 = St#compile.options,
    IgnoreErrors = member(ignore_native_errors, Opts0),
    Opts = case keyfind(hipe, 1, Opts0) of
	       {hipe,L} when is_list(L) -> L;
	       {hipe,X} -> [X];
	       _ -> []
	   end,
    try hipe:compile(St#compile.module,
		     St#compile.core_code,
		     St#compile.code,
		     Opts) of
	{ok,{_Type,Bin}=T} when is_binary(Bin) ->
	    {ok,embed_native_code(St, T)};
	{error,R} ->
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,[{none,?MODULE,{native,R}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    Es = [{St#compile.ifile,[{none,?MODULE,{native,R}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end
    catch
	Class:R ->
	    Stk = erlang:get_stacktrace(),
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,
			   [{none,?MODULE,{native_crash,R,Stk}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    erlang:raise(Class, R, Stk)
	    end
    end.

embed_native_code(St, {Architecture,NativeCode}) ->
    {ok, _, Chunks0} = beam_lib:all_chunks(St#compile.code),
    ChunkName = hipe_unified_loader:chunk_name(Architecture),
    Chunks1 = lists:keydelete(ChunkName, 1, Chunks0),
    Chunks = Chunks1 ++ [{ChunkName,NativeCode}],
    {ok, BeamPlusNative} = beam_lib:build_module(Chunks),
    St#compile{code=BeamPlusNative}.

%% effects_code_generation(Option) -> true|false.
%%  Determine whether the option could have any effect on the
%%  generated code in the BEAM file (as opposed to how
%%  errors will be reported).

effects_code_generation(Option) ->
    case Option of 
	beam -> false;
	report_warnings -> false;
	report_errors -> false;
	return_errors-> false;
	return_warnings-> false;
	binary -> false;
	verbose -> false;
	{cwd,_} -> false;
	_ -> true
    end.

save_binary(#compile{code=none}=St) -> {ok,St};
save_binary(#compile{module=Mod,ofile=Outfile,
		     options=Opts}=St) ->
    %% Test that the module name and output file name match.
    case member(no_error_module_mismatch, Opts) of
	true ->
	    save_binary_1(St);
	false ->
	    Base = filename:rootname(filename:basename(Outfile)),
	    case atom_to_list(Mod) of
		Base ->
		    save_binary_1(St);
		_ ->
		    Es = [{St#compile.ofile,
			   [{none,?MODULE,{module_name,Mod,Base}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end
    end.

save_binary_1(St) ->
    Ofile = St#compile.ofile,
    Tfile = tmpfile(Ofile),		%Temp working file
    case write_binary(Tfile, St#compile.code, St) of
	ok ->
	    case file:rename(Tfile, Ofile) of
		ok ->
		    {ok,St};
		{error,RenameError} ->
		    Es0 = [{Ofile,[{none,?MODULE,{rename,Tfile,Ofile,
						  RenameError}}]}],
		    Es = case file:delete(Tfile) of
			     ok -> Es0;
			     {error,DeleteError} ->
				 Es0 ++
				     [{Ofile,
				       [{none,?MODULE,{delete_temp,Tfile,
						       DeleteError}}]}]
			 end,
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{error,_Error} ->
	    Es = [{Tfile,[{none,compile,write_error}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

write_binary(Name, Bin, St) ->
    Opts = case member(compressed, St#compile.options) of
	       true -> [compressed];
	       false -> []
	   end,
    case file:write_file(Name, Bin, Opts) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

%% report_errors(State) -> ok
%% report_warnings(State) -> ok

report_errors(#compile{options=Opts,errors=Errors}) ->
    case member(report_errors, Opts) of
	true ->
	    foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
			({F,Eds}) -> list_errors(F, Eds) end,
		    Errors);
	false -> ok
    end.

report_warnings(#compile{options=Opts,warnings=Ws0}) ->
    Werror = member(warnings_as_errors, Opts),
    P = case Werror of
	    true -> "";
	    false -> "Warning: "
	end,
    ReportWerror = Werror andalso member(report_errors, Opts),
    case member(report_warnings, Opts) orelse ReportWerror of
	true ->
	    Ws1 = flatmap(fun({{F,_L},Eds}) -> format_message(F, P, Eds);
			     ({F,Eds}) -> format_message(F, P, Eds) end,
			  Ws0),
	    Ws = lists:sort(Ws1),
	    foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws);
	false -> ok
    end.

format_message(F, P, [{none,Mod,E}|Es]) ->
    M = {none,io_lib:format("~ts: ~s~ts\n", [F,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(F, P, [{{Line,Column}=Loc,Mod,E}|Es]) ->
    M = {{F,Loc},io_lib:format("~ts:~w:~w ~s~ts\n",
                                [F,Line,Column,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(F, P, [{Line,Mod,E}|Es]) ->
    M = {{F,{Line,0}},io_lib:format("~ts:~w: ~s~ts\n",
                                [F,Line,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(F, P, [{Mod,E}|Es]) ->
    %% Not documented and not expected to be used any more, but
    %% keep a while just in case.
    M = {none,io_lib:format("~ts: ~s~ts\n", [F,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(_, _, []) -> [].

%% list_errors(File, ErrorDescriptors) -> ok

list_errors(F, [{none,Mod,E}|Es]) ->
    io:fwrite("~ts: ~ts\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{{Line,Column},Mod,E}|Es]) ->
    io:fwrite("~ts:~w:~w: ~ts\n", [F,Line,Column,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~ts:~w: ~ts\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    %% Not documented and not expected to be used any more, but
    %% keep a while just in case.
    io:fwrite("~ts: ~ts\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.

%% erlfile(Dir, Base) -> ErlFile
%% outfile(Base, Extension, Options) -> OutputFile
%% objfile(Base, Target, Options) -> ObjFile
%% tmpfile(ObjFile) -> TmpFile
%%  Work out the correct input and output file names.

iofile(File) when is_atom(File) ->
    iofile(atom_to_list(File));
iofile(File) ->
    {filename:dirname(File), filename:basename(File, ".erl")}.

erlfile(".", Base, Suffix) ->
    Base ++ Suffix;
erlfile(Dir, Base, Suffix) ->
    filename:join(Dir, Base ++ Suffix).

outfile(Base, Ext, Opts) when is_atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case keyfind(outdir, 1, Opts) of
		{outdir, Odir} -> filename:join(Odir, Base);
		_Other -> Base			% Not found or bad format
	    end,
    Obase ++ "." ++ Ext.

objfile(Base, St) ->
    outfile(Base, "beam", St#compile.options).

tmpfile(Ofile) ->
    reverse([$#|tl(reverse(Ofile))]).

%% pre_defs(Options)
%% inc_paths(Options)
%%  Extract the predefined macros and include paths from the option list.

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [ P || {i,P} <- Opts, is_list(P) ].

src_listing(Ext, St) ->
    listing(fun (Lf, {_Mod,_Exp,Fs}) -> do_src_listing(Lf, Fs);
		(Lf, Fs) -> do_src_listing(Lf, Fs) end,
	    Ext, St).

do_src_listing(Lf, Fs) ->
    Opts = [lists:keyfind(encoding, 1, io:getopts(Lf))],
    foreach(fun (F) -> io:put_chars(Lf, [erl_pp:form(F, Opts),"\n"]) end,
	    Fs).

listing(Ext, St0) ->
    St = St0#compile{encoding = none},
    listing(fun(Lf, Fs) -> beam_listing:module(Lf, Fs) end, Ext, St).

listing(LFun, Ext, St) ->
    Lfile = outfile(St#compile.base, Ext, St#compile.options),
    case file:open(Lfile, [write,delayed_write]) of
	{ok,Lf} ->
            Code = restore_expanded_types(Ext, St#compile.code),
            output_encoding(Lf, St),
	    LFun(Lf, Code),
	    ok = file:close(Lf),
	    {ok,St};
	{error,_Error} ->
	    Es = [{Lfile,[{none,compile,write_error}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

output_encoding(F, #compile{encoding = none}) ->
    ok = io:setopts(F, [{encoding, epp:default_encoding()}]);
output_encoding(F, #compile{encoding = Encoding}) ->
    ok = io:setopts(F, [{encoding, Encoding}]),
    ok = io:fwrite(F, <<"%% ~s\n">>, [epp:encoding_to_string(Encoding)]).

restore_expanded_types("P", Fs) ->
    epp:restore_typed_record_fields(Fs);
restore_expanded_types("E", {M,I,Fs0}) ->
    Fs1 = restore_expand_module(Fs0),
    Fs = epp:restore_typed_record_fields(Fs1),
    {M,I,Fs};
restore_expanded_types(_Ext, Code) -> Code.

restore_expand_module([{attribute,Line,type,[Type]}|Fs]) ->
    [{attribute,Line,type,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,opaque,[Type]}|Fs]) ->
    [{attribute,Line,opaque,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,spec,[Arg]}|Fs]) ->
    [{attribute,Line,spec,Arg}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,callback,[Arg]}|Fs]) ->
    [{attribute,Line,callback,Arg}|restore_expand_module(Fs)];
restore_expand_module([F|Fs]) ->
    [F|restore_expand_module(Fs)];
restore_expand_module([]) -> [].


-spec options() -> 'ok'.

options() ->
    help(standard_passes()).

help([{delay,Ps}|T]) ->
    help(Ps),
    help(T);
help([{iff,Flag,{src_listing,Ext}}|T]) ->
    io:fwrite("~p - Generate .~s source listing file\n", [Flag,Ext]),
    help(T);
help([{iff,Flag,{listing,Ext}}|T]) ->
    io:fwrite("~p - Generate .~s file\n", [Flag,Ext]),
    help(T);
help([{iff,Flag,{Name,Fun}}|T]) when is_function(Fun) ->
    io:fwrite("~p - Run ~s\n", [Flag,Name]),
    help(T);
help([{iff,_Flag,Action}|T]) ->
    help(Action),
    help(T);
help([{unless,Flag,{pass,Pass}}|T]) ->
    io:fwrite("~p - Skip the ~s pass\n", [Flag,Pass]),
    help(T);
help([{unless,no_postopt=Flag,List}|T]) when is_list(List) ->
    %% Hard-coded knowledge here.
    io:fwrite("~p - Skip all post optimisation\n", [Flag]),
    help(List),
    help(T);
help([{unless,_Flag,Action}|T]) ->
    help(Action),
    help(T);
help([_|T]) ->
    help(T);
help(_) ->
    ok.


%% compile(AbsFileName, Outfilename, Options)
%%   Compile entry point for erl_compile.

compile(File0, _OutFile, Options) ->
    File = shorten_filename(File0),
    case file(File, make_erl_options(Options)) of
	{ok,_Mod} -> ok;
	Other -> Other
    end.

compile_beam(File0, _OutFile, Opts) ->
    File = shorten_filename(File0),
    case file(File, [from_beam|make_erl_options(Opts)]) of
	{ok,_Mod} -> ok;
	Other -> Other
    end.

compile_asm(File0, _OutFile, Opts) ->
    File = shorten_filename(File0),
    case file(File, [from_asm|make_erl_options(Opts)]) of
	{ok,_Mod} -> ok;
	Other -> Other
    end.

compile_core(File0, _OutFile, Opts) ->
    File = shorten_filename(File0),
    case file(File, [from_core|make_erl_options(Opts)]) of
	{ok,_Mod} -> ok;
	Other -> Other
    end.

shorten_filename(Name0) ->
    {ok,Cwd} = file:get_cwd(),
    case lists:prefix(Cwd, Name0) of
	false -> Name0;
	true ->
	    case lists:nthtail(length(Cwd), Name0) of
		"/"++N -> N;
		N -> N
	    end
    end.

%% Converts generic compiler options to specific options.

make_erl_options(Opts) ->
    #options{includes=Includes,
	     defines=Defines,
	     outdir=Outdir,
	     warning=Warning,
	     verbose=Verbose,
	     specific=Specific,
	     output_type=OutputType,
	     cwd=Cwd} = Opts,
    Options = [verbose || Verbose] ++
	[report_warnings || Warning =/= 0] ++
	map(fun ({Name,Value}) ->
		    {d,Name,Value};
		(Name) ->
		    {d,Name}
	    end, Defines) ++
	case OutputType of
	    undefined -> [];
	    jam -> [jam];
	    beam -> [beam];
	    native -> [native]
	end,
    Options ++ [report_errors, {cwd, Cwd}, {outdir, Outdir}|
	        [{i, Dir} || Dir <- Includes]] ++ Specific.
