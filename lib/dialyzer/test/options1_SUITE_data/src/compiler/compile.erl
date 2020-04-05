%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: compile.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%% Purpose: Run the Erlang compiler.

-module(compile).
-include("erl_compile.hrl").
-include("core_parse.hrl").

%% High-level interface.
-export([file/1,file/2,format_error/1,iofile/1]).
-export([forms/1,forms/2]).
-export([output_generated/1]).
-export([options/0]).

%% Erlc interface.
-export([compile/3,compile_beam/3,compile_asm/3,compile_core/3]).


-import(lists, [member/2,reverse/1,keysearch/3,last/1,
		map/2,flatmap/2,foreach/2,foldr/3,any/2,filter/2]).

%% file(FileName)
%% file(FileName, Options)
%%  Compile the module in file FileName.

-define(DEFAULT_OPTIONS, [verbose,report_errors,report_warnings]).

-define(pass(P), {P,fun P/1}).

file(File) -> file(File, ?DEFAULT_OPTIONS).

file(File, Opts) when list(Opts) ->
    do_compile({file,File}, Opts++env_default_opts());
file(File, Opt) ->
    file(File, [Opt|?DEFAULT_OPTIONS]).

forms(File) -> forms(File, ?DEFAULT_OPTIONS).

forms(Forms, Opts) when list(Opts) ->
    do_compile({forms,Forms}, [binary|Opts++env_default_opts()]);
forms(Forms, Opts) when atom(Opts) ->
    forms(Forms, [Opts|?DEFAULT_OPTIONS]).

env_default_opts() ->
    Key = "ERL_COMPILER_OPTIONS",
    case os:getenv(Key) of
	false -> [];
	Str when list(Str) ->
	    case erl_scan:string(Str) of
		{ok,Tokens,_} ->
		    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
			{ok,List} when list(List) -> List;
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
    Self = self(),
    Serv = spawn_link(fun() -> internal(Self, Input, Opts) end),
    receive
	{Serv,Rep} -> Rep
    end.

%% Given a list of compilation options, returns true if compile:file/2
%% would have generated a Beam file, false otherwise (if only a binary or a
%% listing file would have been generated).

output_generated(Opts) ->
    any(fun ({save_binary,_F}) -> true;
	    (_Other) -> false
	end, passes(file, expand_opts(Opts))).

expand_opts(Opts) ->
    foldr(fun expand_opt/2, [], Opts).

expand_opt(basic_validation, Os) ->
    [no_code_generation,to_pp,binary|Os];
expand_opt(strong_validation, Os) ->
    [no_code_generation,to_kernel,binary|Os];
expand_opt(report, Os) ->
    [report_errors,report_warnings|Os];
expand_opt(return, Os) ->
    [return_errors,return_warnings|Os];
expand_opt(r7, Os) ->
    [no_float_opt,no_new_funs,no_new_binaries,no_new_apply|Os];
expand_opt(O, Os) -> [O|Os].

filter_opts(Opts0) ->
    %% Native code generation is not supported if no_new_funs is given.
    case member(no_new_funs, Opts0) of
	false -> Opts0;
	true -> Opts0 -- [native]
    end.

%% format_error(ErrorDescriptor) -> string()

format_error(no_native_support) ->
    "this system is not configured for native-code compilation.";
format_error({native, E}) ->
    io_lib:fwrite("native-code compilation failed with reason: ~P.",
		  [E, 25]);
format_error({native_crash, E}) ->
    io_lib:fwrite("native-code compilation crashed with reason: ~P.",
		  [E, 25]);
format_error({open,E}) ->
    io_lib:format("open error '~s'", [file:format_error(E)]);
format_error({epp,E}) ->
    epp:format_error(E);
format_error(write_error) ->
    "error writing file";
format_error({rename,S}) ->
    io_lib:format("error renaming ~s", [S]);
format_error({parse_transform,M,R}) ->
    io_lib:format("error in parse transform '~s': ~p", [M, R]);
format_error({core_transform,M,R}) ->
    io_lib:format("error in core transform '~s': ~p", [M, R]);
format_error({crash,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\ncrash reason: ~p", [Pass,Reason]);
format_error({bad_return,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\nbad return value: ~p", [Pass,Reason]).

%% The compile state record.
-record(compile, {filename="",
		  dir="",
		  base="",
		  ifile="",
		  ofile="",
		  module=[],
		  code=[],
		  core_code=[],
		  abstract_code=[],		%Abstract code for debugger.
		  options=[],
		  errors=[],
		  warnings=[]}).

internal(Master, Input, Opts) ->
    Master ! {self(),
	      case catch internal(Input, Opts) of
		  {'EXIT', Reason} ->
		      {error, Reason};
		  Other ->
		      Other
	      end}.

internal({forms,Forms}, Opts) ->
    Ps = passes(forms, Opts),
    internal_comp(Ps, "", "", #compile{code=Forms,options=Opts});
internal({file,File}, Opts) ->
    Ps = passes(file, Opts),
    Compile = #compile{options=Opts},
    case member(from_core, Opts) of
	true -> internal_comp(Ps, File, ".core", Compile);
	false ->
	    case member(from_beam, Opts) of
		true ->
		    internal_comp(Ps, File, ".beam", Compile);
		false ->
		    case member(from_asm, Opts) orelse member(asm, Opts) of
			true ->
			    internal_comp(Ps, File, ".S", Compile);
			false ->
			    internal_comp(Ps, File, ".erl", Compile)
		    end
	    end
    end.

internal_comp(Passes, File, Suffix, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    St1 = St0#compile{filename=File, dir=Dir, base=Base,
		      ifile=erlfile(Dir, Base, Suffix),
		      ofile=objfile(Base, St0)},
    Run = case member(time, St1#compile.options) of
	      true  ->
		  io:format("Compiling ~p\n", [File]),
		  fun run_tc/2;
	      false -> fun({_Name,Fun}, St) -> catch Fun(St) end
	  end,
    case fold_comp(Passes, Run, St1) of
	{ok,St2} -> comp_ret_ok(St2);
	{error,St2} -> comp_ret_err(St2)
    end.

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
	{error,St1} -> {error,St1};
	{'EXIT',Reason} ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{crash,Name,Reason}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}};
	Other ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{bad_return,Name,Other}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}}
    end;
fold_comp([], _Run, St) -> {ok,St}.

os_process_size() ->
    case os:type() of
	{unix, sunos} ->
	    Size = os:cmd("ps -o vsz -p " ++ os:getpid() ++ " | tail -1"),
	    list_to_integer(nonl(Size));
	_ ->
	    0
    end.

nonl([$\n]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].

run_tc({Name,Fun}, St) ->
    Before0 = statistics(runtime),
    Val = (catch Fun(St)),
    After0 = statistics(runtime),
    {Before_c, _} = Before0,
    {After_c, _} = After0,
    io:format(" ~-30s: ~10.3f s (~w k)\n",
	      [Name, (After_c-Before_c) / 1000, os_process_size()]),
    Val.

comp_ret_ok(#compile{code=Code,warnings=Warn,module=Mod,options=Opts}=St) ->
    report_warnings(St),
    Ret1 = case member(binary, Opts) andalso not member(no_code_generation, Opts) of
	       true -> [Code];
	       false -> []
	   end,
    Ret2 = case member(return_warnings, Opts) of
	       true -> Ret1 ++ [Warn];
	       false -> Ret1
	   end,
    list_to_tuple([ok,Mod|Ret2]).

comp_ret_err(St) ->
    report_errors(St),
    report_warnings(St),
    case member(return_errors, St#compile.options) of
	true -> {error,St#compile.errors,St#compile.warnings};
	false -> error
    end.

%% passes(form|file, [Option]) -> [{Name,PassFun}]
%%  Figure out which passes that need to be run.

passes(forms, Opts) ->
    select_passes(standard_passes(), Opts);
passes(file, Opts) ->
    case member(from_beam, Opts) of
	true ->
	    Ps = [?pass(read_beam_file)|binary_passes()],
	    select_passes(Ps, Opts);
	false ->
	    Ps = case member(from_asm, Opts) orelse member(asm, Opts) of
		     true ->
			 [?pass(beam_consult_asm)|asm_passes()];
		     false ->
			 case member(from_core, Opts) of
			     true ->
				 [?pass(parse_core)|core_passes()];
			     false ->
				 [?pass(parse_module)|standard_passes()]
			 end
		 end,
	    Fs = select_passes(Ps, Opts),

	    %% If the last pass saves the resulting binary to a file,
	    %% insert a first pass to remove the file.
	    case last(Fs)  of
		{save_binary,_Fun} -> [?pass(remove_file)|Fs];
		_Other -> Fs
	    end
    end.

%% select_passes([Command], Opts) ->  [{Name,Function}]
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
		    {error,Es} ->
			{error,St#compile{errors=St#compile.errors ++ Es}}
		end
	end,
    [{Mod,F}|select_passes(Ps, Opts)];
select_passes([{src_listing,Ext}|_], _Opts) ->
    [{listing,fun (St) -> src_listing(Ext, St) end}];
select_passes([{listing,Ext}|_], _Opts) ->
    [{listing,fun (St) -> listing(Ext, St) end}];
select_passes([{done,Ext}|_], Opts) ->
    select_passes([{unless,binary,{listing,Ext}}], Opts);
select_passes([{iff,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, true, Pass, Ps, Opts);
select_passes([{unless,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, false, Pass, Ps, Opts);
select_passes([{_,Fun}=P|Ps], Opts) when is_function(Fun) ->
    [P|select_passes(Ps, Opts)];
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

%% The standard passes (almost) always run.

standard_passes() ->
    [?pass(transform_module),
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
     ?pass(core_module),
     {iff,'dcore',{listing,"core"}},
     {iff,'to_core0',{done,"core"}}
     | core_passes()].

core_passes() ->
    %% Optimization and transforms of Core Erlang code.
    [{unless,no_copt,
      [{core_old_inliner,fun test_old_inliner/1,fun core_old_inliner/1},
       ?pass(core_fold_module),
       {core_inline_module,fun test_core_inliner/1,fun core_inline_module/1},
       {core_fold_after_inline,fun test_core_inliner/1,fun core_fold_module/1},
       ?pass(core_transforms)]},
     {iff,dcopt,{listing,"copt"}},
     {iff,'to_core',{done,"core"}}
     | kernel_passes()].

kernel_passes() ->
    %% Destructive setelement/3 optimization and core lint.
    [?pass(core_dsetel_module),
     {iff,clint,?pass(core_lint_module)},
     {iff,core,?pass(save_core_code)},

     %% Kernel Erlang and code generation.
     ?pass(kernel_module),
     {iff,dkern,{listing,"kernel"}},
     {iff,'to_kernel',{done,"kernel"}},
     {pass,v3_life},
     {iff,dlife,{listing,"life"}},
     {pass,v3_codegen},
     {iff,dcg,{listing,"codegen"}}
     | asm_passes()].

asm_passes() ->
    %% Assembly level optimisations.
    [{unless,no_postopt,
      [{pass,beam_block},
       {iff,dblk,{listing,"block"}},
       {unless,no_bopt,{pass,beam_bool}},
       {iff,dbool,{listing,"bool"}},
       {unless,no_topt,{pass,beam_type}},
       {iff,dtype,{listing,"type"}},
       {pass,beam_dead},	      %Must always run since it splits blocks.
       {iff,ddead,{listing,"dead"}},
       {unless,no_jopt,{pass,beam_jump}},
       {iff,djmp,{listing,"jump"}},
       {pass,beam_clean},
       {iff,dclean,{listing,"clean"}},
       {pass,beam_flatten}]},

     %% If post optimizations are turned off, we still coalesce
     %% adjacent labels and remove unused labels to keep the
     %% HiPE compiler happy.
     {iff,no_postopt,
      [?pass(beam_unused_labels),
       {pass,beam_clean}]},

     {iff,dopt,{listing,"optimize"}},
     {iff,'S',{listing,"S"}},
     {iff,'to_asm',{done,"S"}},

     {pass,beam_validator},
     ?pass(beam_asm)
     | binary_passes()].

binary_passes() ->
    [{native_compile,fun test_native/1,fun native_compile/1},
     {unless,binary,?pass(save_binary)}].

%%%
%%% Compiler passes.
%%%

%% Remove the target file so we don't have an old one if the compilation fail.
remove_file(St) ->
    file:delete(St#compile.ofile),
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
	    {Module, Forms} = preprocess_asm_forms(Forms0),
	    {ok,St#compile{module=Module, code=Forms}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

read_beam_file(St) ->
    case file:read_file(St#compile.ifile) of
	{ok,Beam} ->
	    Infile = St#compile.ifile,
	    case is_too_old(Infile) of
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

is_too_old(BeamFile) ->
    case beam_lib:chunks(BeamFile, ["CInf"]) of
	{ok,{_,[{"CInf",Term0}]}} ->
	    Term = binary_to_term(Term0),
	    Opts = proplists:get_value(options, Term, []),
	    lists:member(no_new_funs, Opts);
	_ -> false
    end.

parse_module(St) ->
    Opts = St#compile.options,
    Cwd = ".",
    IncludePath = [Cwd, St#compile.dir|inc_paths(Opts)],
    Tab = ets:new(compiler__tab, [protected,named_table]),
    ets:insert(Tab, {compiler_options,Opts}),
    R =  epp:parse_file(St#compile.ifile, IncludePath, pre_defs(Opts)),
    ets:delete(Tab),
    case R of
	{ok,Forms} ->
	    {ok,St#compile{code=Forms}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{epp,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

parse_core(St) ->
    case file:read_file(St#compile.ifile) of
	{ok,Bin} ->
	    case core_scan:string(binary_to_list(Bin)) of
		{ok,Toks,_} ->
		    case core_parse:parse(Toks) of
			{ok,Mod} ->
			    Name = (Mod#c_module.name)#c_atom.val,
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

transforms(Os) -> [ M || {parse_transform,M} <- Os ].

transform_module(St) ->
    %% Extract compile options from code into options field.
    Ts = transforms(St#compile.options ++ compile_options(St#compile.code)),
    foldl_transform(St, Ts).

foldl_transform(St, [T|Ts]) ->
    Name = "transform " ++ atom_to_list(T),
    Fun = fun(S) -> T:parse_transform(S#compile.code, S#compile.options) end,
    Run = case member(time, St#compile.options) of
	      true  -> fun run_tc/2;
	      false -> fun({_Name,F}, S) -> catch F(S) end
	  end,
    case Run({Name, Fun}, St) of
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}};
	{'EXIT',R} ->
	    Es = [{St#compile.ifile,[{none,compile,{parse_transform,T,R}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}};
	Forms ->
	    foldl_transform(St#compile{code=Forms}, Ts)
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
get_module([{attribute,_,module,{M,_As}} | _]) -> M;
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
	    St#compile{base = atom_to_list(M)};
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

%% expand_module(State) -> State'
%%  Do the common preprocessing of the input forms.

expand_module(#compile{code=Code,options=Opts0}=St0) ->
    {Mod,Exp,Forms,Opts1} = sys_pre_expand:module(Code, Opts0),
    Opts2 = expand_opts(Opts1),
    Opts = filter_opts(Opts2),
    {ok,St0#compile{module=Mod,options=Opts,code={Mod,Exp,Forms}}}.

core_module(#compile{code=Code0,options=Opts,ifile=File}=St) ->
    {ok,Code,Ws} = v3_core:module(Code0, Opts),
    {ok,St#compile{code=Code,warnings=St#compile.warnings ++ [{File,Ws}]}}.

core_fold_module(#compile{code=Code0,options=Opts,ifile=File}=St) ->
    {ok,Code,Ws} = sys_core_fold:module(Code0, Opts),
    {ok,St#compile{code=Code,warnings=St#compile.warnings ++ [{File,Ws}]}}.

test_old_inliner(#compile{options=Opts}) ->
    %% The point of this test is to avoid loading the old inliner
    %% if we know that it will not be used.
    case any(fun(no_inline) -> true;
		(_) -> false
	     end, Opts) of
      true -> false;
      false ->
	any(fun({inline,_}) -> true;
	       (_) -> false
	    end, Opts)
    end.

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

core_old_inliner(#compile{code=Code0,options=Opts}=St) ->
    case catch sys_core_inline:module(Code0, Opts) of
	{ok,Code} ->
	    {ok,St#compile{code=Code}};
	{error,Es} ->
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

core_inline_module(#compile{code=Code0,options=Opts}=St) ->
    Code = cerl_inline:core_transform(Code0, Opts),
    {ok,St#compile{code=Code}}.

core_dsetel_module(#compile{code=Code0,options=Opts}=St) ->
    {ok,Code} = sys_core_dsetel:module(Code0, Opts),
    {ok,St#compile{code=Code}}.

kernel_module(#compile{code=Code0,options=Opts,ifile=File}=St) ->
    {ok,Code,Ws} = v3_kernel:module(Code0, Opts),
    {ok,St#compile{code=Code,warnings=St#compile.warnings ++ [{File,Ws}]}}.

save_abstract_code(St) ->
    {ok,St#compile{abstract_code=abstract_code(St)}}.

abstract_code(#compile{code=Code}) ->
    Abstr = {raw_abstract_v1,Code},
    case catch erlang:term_to_binary(Abstr, [compressed]) of
	{'EXIT',_} -> term_to_binary(Abstr);
	Other -> Other
    end.

save_core_code(St) ->
    {ok,St#compile{core_code=cerl:from_records(St#compile.code)}}.

beam_unused_labels(#compile{code=Code0}=St) ->
    Code = beam_jump:module_labels(Code0),
    {ok,St#compile{code=Code}}.

beam_asm(#compile{ifile=File,code=Code0,abstract_code=Abst,options=Opts0}=St) ->
    Source = filename:absname(File),
    Opts = filter(fun is_informative_option/1, Opts0),
    case beam_asm:module(Code0, Abst, Source, Opts) of
	{ok,Code} -> {ok,St#compile{code=Code,abstract_code=[]}};
	{error,Es} -> {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

test_native(#compile{options=Opts}) ->
    %% This test must be made late, because the r7 or no_new_funs options
    %% will turn off the native option.
    member(native, Opts).

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
    Opts0 = [no_new_binaries|St#compile.options],
    IgnoreErrors = member(ignore_native_errors, Opts0),
    Opts = case keysearch(hipe, 1, Opts0) of
	       {value,{hipe,L}} when list(L) -> L;
	       {value,{hipe,X}} -> [X];
	       _ -> []
	   end,
    case catch hipe:compile(St#compile.module,
			    St#compile.core_code,
			    St#compile.code,
			    Opts) of
	{ok, {Type,Bin}} when binary(Bin) ->
	    {ok, embed_native_code(St, {Type,Bin})};
	{error, R} ->
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,[{none,?MODULE,{native,R}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    Es = [{St#compile.ifile,[{none,?MODULE,{native,R}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{'EXIT',R} ->
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,[{none,?MODULE,{native_crash,R}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    exit(R)
	    end
    end.

embed_native_code(St, {Architecture,NativeCode}) ->
    {ok, _, Chunks0} = beam_lib:all_chunks(St#compile.code),
    ChunkName = hipe_unified_loader:chunk_name(Architecture),
    Chunks1 = lists:keydelete(ChunkName, 1, Chunks0),
    Chunks = Chunks1 ++ [{ChunkName,NativeCode}],
    {ok, BeamPlusNative} = beam_lib:build_module(Chunks),
    St#compile{code=BeamPlusNative}.

%% Returns true if the option is informative and therefore should be included
%% in the option list of the compiled module.

is_informative_option(beam) -> false;
is_informative_option(report_warnings) -> false;
is_informative_option(report_errors) -> false;
is_informative_option(binary) -> false;
is_informative_option(verbose) -> false;
is_informative_option(_) -> true.

save_binary(#compile{code=none}=St) -> {ok,St};
save_binary(St) ->
    Tfile = tmpfile(St#compile.ofile),		%Temp working file
    case write_binary(Tfile, St#compile.code, St) of
	ok ->
	    case file:rename(Tfile, St#compile.ofile) of
		ok ->
		    {ok,St};
		{error,_Error} ->
		    file:delete(Tfile),
		    Es = [{St#compile.ofile,[{none,?MODULE,{rename,Tfile}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{error,_Error} ->
	    Es = [{Tfile,[{compile,write_error}]}],
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

report_errors(St) ->
    case member(report_errors, St#compile.options) of
	true ->
	    foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
			({F,Eds}) -> list_errors(F, Eds) end,
		    St#compile.errors);
	false -> ok
    end.

report_warnings(#compile{options=Opts,warnings=Ws0}) ->
    case member(report_warnings, Opts) of
	true ->
	    Ws1 = flatmap(fun({{F,_L},Eds}) -> format_message(F, Eds);
			     ({F,Eds}) -> format_message(F, Eds) end,
		     Ws0),
	    Ws = ordsets:from_list(Ws1),
	    foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws);
	false -> ok
    end.

format_message(F, [{Line,Mod,E}|Es]) ->
    M = {Line,io_lib:format("~s:~w: Warning: ~s\n", [F,Line,Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(F, [{Mod,E}|Es]) ->
    M = {none,io_lib:format("~s: Warning: ~s\n", [F,Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(_, []) -> [].

%% list_errors(File, ErrorDescriptors) -> ok

list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~s:~w: ~s\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    io:fwrite("~s: ~s\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.

%% erlfile(Dir, Base) -> ErlFile
%% outfile(Base, Extension, Options) -> OutputFile
%% objfile(Base, Target, Options) -> ObjFile
%% tmpfile(ObjFile) -> TmpFile
%%  Work out the correct input and output file names.

iofile(File) when atom(File) ->
    iofile(atom_to_list(File));
iofile(File) ->
    {filename:dirname(File), filename:basename(File, ".erl")}.

erlfile(Dir, Base, Suffix) ->
    filename:join(Dir, Base++Suffix).

outfile(Base, Ext, Opts) when atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case keysearch(outdir, 1, Opts) of
		{value, {outdir, Odir}} -> filename:join(Odir, Base);
		_Other -> Base			% Not found or bad format
	    end,
    Obase++"."++Ext.

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
    [ P || {i,P} <- Opts, list(P) ].

src_listing(Ext, St) ->
    listing(fun (Lf, {_Mod,_Exp,Fs}) -> do_src_listing(Lf, Fs);
		(Lf, Fs) -> do_src_listing(Lf, Fs) end,
	    Ext, St).

do_src_listing(Lf, Fs) ->
    foreach(fun (F) -> io:put_chars(Lf, [erl_pp:form(F),"\n"]) end,
	    Fs).

listing(Ext, St) ->
    listing(fun(Lf, Fs) -> beam_listing:module(Lf, Fs) end, Ext, St).

listing(LFun, Ext, St) ->
    Lfile = outfile(St#compile.base, Ext, St#compile.options),
    case file:open(Lfile, [write,delayed_write]) of
	{ok,Lf} ->
	    LFun(Lf, St#compile.code),
	    ok = file:close(Lf),
	    {ok,St};
	{error,_Error} ->
	    Es = [{Lfile,[{none,compile,write_error}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

options() ->
    help(standard_passes()).

help([{iff,Flag,{src_listing,Ext}}|T]) ->
    io:fwrite("~p - Generate .~s source listing file\n", [Flag,Ext]),
    help(T);
help([{iff,Flag,{listing,Ext}}|T]) ->
    io:fwrite("~p - Generate .~s file\n", [Flag,Ext]),
    help(T);
help([{iff,Flag,{Name,Fun}}|T]) when function(Fun) ->
    io:fwrite("~p - Run ~s\n", [Flag,Name]),
    help(T);
help([{iff,_Flag,Action}|T]) ->
    help(Action),
    help(T);
help([{unless,Flag,{pass,Pass}}|T]) ->
    io:fwrite("~p - Skip the ~s pass\n", [Flag,Pass]),
    help(T);
help([{unless,no_postopt=Flag,List}|T]) when list(List) ->
    %% Hard-coded knowledgde here.
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
    case file(File, [asm|make_erl_options(Opts)]) of
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
	    Name = case lists:nthtail(length(Cwd), Name0) of
		       "/"++N -> N;
		       N -> N
		   end,
	    Name
    end.

%% Converts generic compiler options to specific options.

make_erl_options(Opts) ->

    %% This way of extracting will work even if the record passed
    %% has more fields than known during compilation.

    Includes = Opts#options.includes,
    Defines = Opts#options.defines,
    Outdir = Opts#options.outdir,
    Warning = Opts#options.warning,
    Verbose = Opts#options.verbose,
    Specific = Opts#options.specific,
    OutputType = Opts#options.output_type,
    Cwd = Opts#options.cwd,

    Options =
	case Verbose of
	    true ->  [verbose];
	    false -> []
	end ++
	case Warning of
	    0 -> [];
	    _ -> [report_warnings]
	end ++
	map(
	  fun ({Name, Value}) ->
		  {d, Name, Value};
	      (Name) ->
		  {d, Name}
	  end,
	  Defines) ++
	case OutputType of
	    undefined -> [];
	    jam -> [jam];
	    beam -> [beam];
	    native -> [native]
	end,

    Options++[report_errors, {cwd, Cwd}, {outdir, Outdir}|
	      map(fun(Dir) -> {i, Dir} end, Includes)]++Specific.
