%% -*- erlang-indent-level: 2 -*-
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
%% ====================================================================
%% Copyright (c) 1998 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : hipe.erl
%%  Module   : hipe
%%  Purpose  :  
%%  Notes    : 
%%  History  : * 1998-01-28 Erik Johansson (happi@it.uu.se): Created.
%% ====================================================================
%% @doc This is the direct interface to the HiPE compiler.
%%
%% <h3>Normal use</h3>
%%
%% <p>The normal way to native-compile an Erlang module using HiPE is to
%% include the atom <code>native</code> in the Erlang compiler options,
%% as in:
%%
%% <pre>    1> c(my_module, [native]).</pre></p>
%%
%% <p>Options to the HiPE compiler are then passed as follows:
%%
%% <pre>    1> c(my_module, [native,{hipe,Options}]).</pre></p>
%%
%% <p>For on-line help in the Erlang shell, call <a
%% href="#help-0"><code>hipe:help()</code></a>. Details on HiPE compiler
%% options are given by <a
%% href="#help_options-0"><code>hipe:help_options()</code></a>.</p>
%%
%% <h3>Using the direct interface - for advanced users only</h3>
%%
%% To compile a module to native code and automatically load the code
%% into memory, call <a href="#c-1"><code>hipe:c(Module)</code></a> or <a
%% href="#c-2"><code>hipe:c(Module, Options)</code></a>. Note that all
%% options are specific to the HiPE compiler. See the <a
%% href="#index">function index</a> for other compiler functions.
%%
%% <h3>Main Options</h3>
%%
%% Options are processed in the order they appear in the list; an
%% early option will shadow a later one.
%% <dl>
%%   <dt><code>o0, 'O0', o1, 'O1', o2, 'O2', o3, 'O3'</code></dt>
%%     <dd>Set optimization level (default 2).</dd>
%%
%%   <dt><code>load</code></dt>
%%     <dd>Automatically load the code into memory after compiling.</dd>
%%
%%   <dt><code>time</code></dt>
%%     <dd>Reports the compilation times for the different stages
%%     of the compiler. Call <a
%%     href="#help_option-1"><code>hipe:help_option(time)</code></a> for
%%     details.</dd>
%%
%%   <dt><code>{timeout, Time}</code></dt>
%%     <dd>Sets the time the compiler is allowed to use for the
%%     compilation. <code>Time</code> is time in ms or the atom
%%     <code>infinity</code> (the default).</dd>
%%
%%   <dt><code>verbose</code></dt>
%%     <dd>Make the HiPE compiler output information about what it is
%%     being done.</dd>
%% </dl>
%% 
%% <h3>Advanced Options</h3>
%%
%% Note: You can also specify <code>{Option, false}</code> to turn a
%% particular option off, or <code>{Option, true}</code> to force it on.
%% Boolean-valued (<code>true</code>/<code>false</code>) options also
%% have negative-form aliases, e.g. <code>no_load</code> = <code>{load,
%% false}</code>.
%%
%% <p><dl>
%%   <dt><code>debug</code></dt>
%%     <dd>Outputs internal debugging information during
%%     compilation.</dd>
%%
%%   <dt><code>icode_ssa_copy_prop</code></dt>
%%     <dd>Performs copy propagation on the SSA form on the Icode
%%     level.</dd>
%%
%%   <dt><code>icode_ssa_const_prop</code></dt>
%%     <dd>Performs sparse conditional constant propagation on the SSA
%%     form on the Icode level.</dd>
%%
%%   <dt><code>icode_ssa_struct_reuse</code></dt>
%%     <dd>Tries to factor out identical tuple and list constructions
%%     on the Icode level.</dd>
%%
%%   <dt><code>icode_type</code></dt>
%%     <dd>Simplifies the code by employing type analysis and propagation
%%     on the Icode level.</dd>
%%
%%   <dt><code>icode_range</code></dt>
%%     <dd>Performs integer range analysis on the Icode level.</dd>
%%
%%   <dt><code>pp_all</code></dt>
%%     <dd>Equivalent to <code>[pp_beam, pp_icode, pp_rtl,
%%     pp_native]</code>.</dd>
%%
%%   <dt><code>pp_asm</code></dt>
%%     <dd>Prints the assembly listing with addresses and bytecode.
%%     Currently available for x86 only.</dd>
%%
%%   <dt><code>pp_beam, {pp_beam, {file, File}}</code></dt>
%%     <dd>Display the input Beam code to stdout or file.</dd>
%%
%%   <dt><code>pp_icode, {pp_icode, {file, File}},
%%       {pp_icode, {only, Functions}}</code></dt>
%%     <dd>Pretty-print Icode intermediate code to stdout or file.</dd>
%%
%%   <dt><code>pp_native, {pp_native, {file, File}},
%%       {pp_native, {only, Functions}}</code></dt>
%%     <dd>Pretty-print native code to stdout or file.</dd>
%%
%%   <dt><code>pp_opt_icode, {pp_opt_icode, {file, File}},
%%       {pp_opt_icode, {only, Functions}}</code></dt>
%%     <dd>Pretty-print optimized Icode to stdout or file.</dd>
%%
%%   <dt><code>pp_rtl, {pp_rtl, {file, File}},
%%       {pp_rtl, {only, Functions}}</code></dt>
%%     <dd>Pretty-print RTL intermediate code to stdout or file.</dd>
%%
%%   <dt><code>regalloc</code></dt>
%%     <dd>Select register allocation algorithm. Used as
%%     <code>{regalloc, Method}</code>.
%%
%%     <p><code>Method</code> is one of the following:
%%     <ul>
%%       <li><code>naive</code>: spills everything (for debugging and
%%       testing only).</li>
%%       <li><code>linear_scan</code>: fast compilation; not so good if
%%       only few registers available.</li>
%%       <li><code>graph_color</code>: slower, but gives better
%%       performance.</li>
%%       <li><code>coalescing</code>: tries hard to use registers; can be
%%	 very slow, but typically results in code with best performance.</li>
%%     </ul></p></dd>
%%
%%   <dt><code>remove_comments</code></dt>
%%     <dd>Remove comments from intermediate code.</dd>
%%
%%   <dt><code>rtl_ssa_const_prop</code></dt>
%%     <dd>Performs sparse conditional constant propagation on the SSA
%%     form on the RTL level. </dd>
%%
%%   <dt><code>rtl_lcm</code></dt>
%%     <dd>Lazy Code Motion on RTL.</dd>
%%
%%   <dt><code>rtl_ssapre</code></dt>
%%     <dd>Lazy Partial Redundancy Elimination on RTL (SSA level).</dd>
%%
%%   <dt><code>use_indexing</code></dt>
%%     <dd>Use indexing for multiple-choice branch selection.</dd>
%%
%%   <dt><code>use_callgraph</code></dt>
%%     <dd>Use a static call graph for determining the order in which
%%         the functions of a module should be compiled (in reversed 
%%         topological sort order).</dd>
%% </dl></p>
%%
%% <h3>Debugging Options</h3>
%% (May require that some modules have been
%% compiled with the <code>DEBUG</code> flag.)
%% <dl>
%%   <dt><code>rtl_show_translation</code></dt>
%%     <dd>Prints each step in the translation from Icode to RTL</dd>
%% </dl>
%%
%% @end
%% ====================================================================

-module(hipe).

-export([c/1,
	 c/2,
 	 f/1,
 	 f/2,
	 compile/1,
	 compile/2,
	 compile/4,
	 compile_core/4,
 	 file/1,
	 file/2,
	 get_llvm_version/0,
	 erllvm_is_supported/0,
	 load/1,
	 help/0,
	 help_hiper/0,
	 help_options/0,
	 help_option/1,
	 help_debug_options/0,
	 version/0,
	 erts_checksum/0]).

-ifndef(DEBUG).
-define(DEBUG,true).
-endif.

-include("hipe.hrl").
-include("../../compiler/src/beam_disasm.hrl").
-include("../rtl/hipe_literals.hrl").

%%-------------------------------------------------------------------
%% Basic type declaration for exported functions of the 'hipe' module
%%-------------------------------------------------------------------

-type mod() :: module().
-type file_or_bin() :: file:filename() | binary().
-type ret_rtl() :: [_].
-type c_ret() :: {'ok', mod()} | {'error', term()} |
                 {'ok', mod(), ret_rtl()}. %% The last for debugging only
-type compile_ret() :: {hipe_architecture(), binary()} | list().

%%-------------------------------------------------------------------

-define(COMPILE_DEFAULTS, [o2]).
-define(DEFAULT_TIMEOUT, infinity).

%%-------------------------------------------------------------------

%% @spec load(Module) -> {module, Module} | {error, Reason}
%%     Module = mod()
%%     Reason = term()
%% 
%% @doc Like load/2, but tries to locate a BEAM file automatically.
%%
%% @see load/2

-spec load(Module) -> {'module', Module} | {'error', Reason :: term()}
                   when Module :: mod().

load(Module) ->
  load(Module, beam_file(Module)).

%% @spec load(Module, BeamFileName) -> {module, Module} | {error, Reason}
%%     Module = mod()
%%     BeamFileName = file:filename()
%%     Reason = term()
%%
%% @type mod() = module(). A module name.
%% 
%% @doc User interface for loading code into memory. The code can be
%% given as a native code binary or as the file name of a BEAM file
%% which should contain a native-code chunk. If only the module name is
%% given (see <code>load/1</code>), the BEAM file is located
%% automatically.
%%
%% @see load/1

-spec load(Module, file:filename()) -> {'module', Module} | {'error', term()}
				    when Module :: mod().

load(Mod, BeamFileName) when is_list(BeamFileName) ->
  Architecture = erlang:system_info(hipe_architecture),
  ChunkName = hipe_unified_loader:chunk_name(Architecture),
  case beam_lib:chunks(BeamFileName, [ChunkName]) of
    {ok,{_,[{_,Bin}]}} when is_binary(Bin) -> do_load(Mod, Bin, BeamFileName);
    Error -> {error, Error}
  end.

%% @spec c(Mod) -> {ok, Mod} | {error, Reason}
%%       Mod = mod()
%%       Reason = term()
%%
%% @equiv c(Mod, [])

-spec c(mod()) -> c_ret().

c(Mod) ->
  c(Mod, []).

%% @spec c(Module, options()) -> {ok, Module} | {error, Reason}
%%     Module = mod()
%%     options() = [option()]
%%     option() = term()
%%     Reason = term()
%% 
%% @doc User-friendly native code compiler interface. Reads BEAM code
%% from the corresponding "Module<code>.beam</code>" file in the
%% system path, and compiles the whole module to native code. By
%% default, the compiled code is loaded directly. See above for
%% documentation of options.
%%
%% @see c/1
%% @see c/3
%% @see f/2
%% @see compile/2

-spec c(mod(), comp_options()) -> c_ret().

c(Module, Options) ->
  c(Module, beam_file(Module), Options).

%% @spec c(Module, File, options()) -> {ok, Module} | {error, Reason}
%%     Module = mod()
%%     File = file:filename() | binary()
%%     Reason = term()
%%
%% @doc Like <code>c/2</code>, but reads BEAM code from the specified
%% <code>File</code>.
%%
%% @see c/2
%% @see f/2

c(Module, File, Opts) ->
  Opts1 = user_compile_opts(Opts),
  case compile(Module, File, Opts1) of
    {ok, Res} ->
      case proplists:get_bool(to_rtl, Opts1) of
	true  -> {ok, Module, Res};
	false -> {ok, Module}
      end;
    Other ->
      Other
  end.

%% @spec f(File) -> {ok, Name} | {error, Reason}
%%     File = file:filename() | binary()
%%     Name = mod()
%%     Reason = term()
%%
%% @equiv f(File, [])

-spec f(file_or_bin()) -> {'ok', mod()} | {'error', term()}.

f(File) ->
  f(File, []).

%% @spec f(File, options()) -> {ok, Name} | {error, Reason}
%%     File = file:filename() | binary()
%%     Name = mod()
%%     Reason = term()
%%
%% @doc Like <code>c/3</code>, but takes the module name from the
%% specified <code>File</code>.
%%
%% @see c/3

-spec f(file_or_bin(), comp_options()) -> {'ok', mod()} | {'error', term()}.

f(File, Opts) ->
  case file(File, user_compile_opts(Opts)) of
    {ok, Name, _} ->
      {ok, Name};
    Other ->
      Other
  end.

-define(USER_DEFAULTS, [load]).

user_compile_opts(Opts) ->
  Opts ++ ?USER_DEFAULTS.


%% @spec compile(Module) -> {ok, {Target,Binary}} | {error, Reason}
%%       Module = mod()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @equiv compile(Module, [])

-spec compile(mod()) -> {'ok', compile_ret()} | {'error', term()}.

compile(Module) ->
  compile(Module, []).

%% @spec compile(Module, options()) -> {ok, {Target,Binary}} | {error, Reason}
%%       Module = mod()
%%       Binary = binary()
%%       Reason = term()
%%
%% @doc Direct compiler interface, for advanced use. This just
%% compiles the module, reading BEAM code from the corresponding
%% "Module<code>.beam</code>" file in the system path.  Returns
%% <code>{ok, Binary}</code> if successful, or <code>{error,
%% Reason}</code> otherwise. By default, it does <em>not</em> load the
%% binary to memory (the <code>load</code> option can be used to
%% activate automatic loading). <code>File</code> can be either a file
%% name or a binary containing the BEAM code for the module.
%%
%% @see c/2
%% @see compile/1
%% @see compile/3
%% @see file/2
%% @see load/2

-spec compile(mod(), comp_options()) -> {'ok', compile_ret()} | {'error', term()}.

compile(Module, Options) ->
  compile(Module, beam_file(Module), Options).

-spec beam_file(mod()) -> file:filename().

beam_file(Module) when is_atom(Module) ->
  case code:which(Module) of
    non_existing ->
      ?error_msg("Cannot find ~w.beam file.", [Module]),
      ?EXIT({cant_find_beam_file,Module});
    File when is_list(File) ->
      File
  end.

%% @spec compile(Name, File, options()) ->
%%           {ok, {Target, Binary}} | {error, Reason}
%%       Name = mod()
%%       File = file:filename() | binary()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @doc Like <code>compile/2</code>, but reads BEAM code from the
%% specified <code>File</code>.
%%
%% @see compile/2

-spec compile(mod(), file_or_bin(), comp_options()) ->
	 {'ok', compile_ret()} | {'error', term()}.

compile(Name, File, Opts0) when is_atom(Name) ->
  Opts = expand_kt2(Opts0),
  case proplists:get_value(core, Opts) of
    true when is_binary(File) ->
      ?error_msg("Cannot get Core Erlang code from BEAM binary.",[]),
      ?EXIT({cant_compile_core_from_binary});
    true ->
      case filelib:find_source(filename:rootname(File,".beam") ++ ".beam") of
	{error, _} ->
	  ?error_msg("Cannot find source code for ~p.", [File]),
	  ?EXIT({cant_find_source_code});
	{Source, CompOpts} ->
	  CoreOpts = [X || X = {core_transform, _} <- Opts],
	  %% io:format("Using: ~w\n", [CoreOpts]),
	  case compile:file(Source, CoreOpts ++ [to_core, binary|CompOpts]) of
	    {ok, _, Core} ->
	      compile_core(Name, Core, File, Opts);
	    Error ->
	      ?error_msg("Error compiling ~p:\n~p.", [File, Error]),
	      ?EXIT({cant_compile_source_code})
	  end
      end;
    {src_file, Source} ->
      CoreOpts1 = [X || X = {core_transform, _} <- Opts],
      CoreOpts2 = [report_errors, to_core, binary, {i,"../include"}|CoreOpts1],
      %% io:format("Using: ~w\n", [CoreOpts2]),
      case compile:file(Source, CoreOpts2) of
	{ok, _, Core} ->
	  compile_core(Name, Core, File, Opts);
	Error ->
	  ?error_msg("Error compiling ~p:\n~p\n", [Source, Error]),
	  ?EXIT({cant_compile_source_code, Error})
      end;
    Other when Other =:= false; Other =:= undefined ->
      DisasmFun = fun (_) -> disasm(File) end,
      IcodeFun = fun (Code, Opts_) ->
		     get_beam_icode(Name, Code, File, Opts_)
		 end,
      run_compiler(Name, DisasmFun, IcodeFun, Opts)
  end.

-spec compile_core(mod(), cerl:c_module(), file_or_bin(), comp_options()) ->
	 {'ok', compile_ret()} | {'error', term()}.

compile_core(Name, Core0, File, Opts) ->
  Core = cerl:from_records(Core0),
  compile(Name, Core, File, Opts).

%% @spec compile(Module, Core, File, options()) ->
%%           {ok, {Target, Binary}} | {error, Reason}
%%       Module = mod()
%%       Core = coreErlang() | []
%%       File = file:filename() | binary()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @doc Like <code>compile/3</code>, but unless <code>Core</code> is
%% <code>[]</code>, low-level code is generated from the given Core
%% Erlang code instead of from the BEAM code.
%%
%% <p>Note that only whole modules can be compiled with this
%% function.</p>
%%
%% @see compile/3

-spec compile(mod(), cerl:c_module() | [], file_or_bin(), comp_options()) ->
	 {'ok', compile_ret()} | {'error', term()}.

compile(Name, [], File, Opts) ->
  compile(Name, File, Opts);
compile(Name, Core, File, Opts) when is_atom(Name) ->
  DisasmFun = fun (_) -> {false, []} end,
  IcodeFun = fun (_, Opts) ->
		 get_core_icode(Name, Core, File, Opts)
	     end,
  run_compiler(Name, DisasmFun, IcodeFun, Opts).

%% @spec file(File) -> {ok, Mod, {Target, Binary}} | {error, Reason}
%%       File = file:filename()
%%       Mod = mod()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @equiv file(File, [])

-spec file(file:filename()) -> {'ok', mod(), compile_ret()} | {'error', term()}.

file(File) ->
  file(File, []).

%% @spec file(File, options()) -> {ok, Mod, {Target, Binary}} | {error, Reason}
%%       File = file:filename()
%%       Mod = mod()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @doc Like <code>compile/2</code>, but takes the module name from the
%% specified <code>File</code>. Returns both the module name and the final
%% binary if successful.
%%
%% @see file/1
%% @see compile/2

-spec file(file:filename(), comp_options()) -> {'ok', mod(), compile_ret()}
				             | {'error', Reason :: term()}.
file(File, Options) when is_list(File) ->
  case beam_lib:info(File) of
    L when is_list(L) ->
      {module, Mod} = lists:keyfind(module, 1, L),
      case compile(Mod, File, Options) of
	{ok, CompRet} ->
	  {ok, Mod, CompRet};
	Other ->
	  Other
      end;
    Error ->
      Error
  end.


%%-----------------------------------------------------------------------
%% The rest are internal functions:
%%-----------------------------------------------------------------------

%% @doc
%% Get BEAM code from `.beam' files or directly from binaries.
%%   File is either a file name or a binary containing the BEAM code.

disasm(File) ->
  case beam_disasm:file(File) of
    #beam_file{labeled_exports = LabeledExports,
	       compile_info = CompInfo, code = BeamCode} ->
      CompOpts = proplists:get_value(options, CompInfo, []),
      HCompOpts = case lists:keyfind(hipe, 1, CompOpts) of
		    {hipe, L} when is_list(L) -> L;
		    {hipe, X} -> [X];
		    _ -> []
		  end,
      Exports = fix_beam_exports(LabeledExports),
      {{BeamCode, Exports}, HCompOpts};
    {error, _Mod, Error} ->
      io:format("~s\n", [beam_lib:format_error(Error)]),
      ?EXIT(no_beam_code)
  end.

fix_beam_exports(BeamExports) ->
  fix_beam_exports(BeamExports, []).

fix_beam_exports([{F,A,_}|BeamExports], Exports) ->
  fix_beam_exports(BeamExports, [{F,A} | Exports]);
fix_beam_exports([], Exports) ->
  Exports.

get_beam_icode(Mod, {BeamCode, Exports}, File, Options) ->
  Icode = ?option_time(hipe_beam_to_icode:module(BeamCode, Options),
                       "BEAM-to-Icode", Options),
  BeamBin = get_beam_code(File),
  {{Mod, Exports, Icode}, BeamBin}.

get_core_icode(Mod, Core, File, Options) ->
  {ok, Icode} =
    ?option_time((catch {ok, cerl_to_icode:module(Core, Options)}),
		 "BEAM-to-Icode", Options),
  NeedBeamCode = not proplists:get_bool(load, Options),
  BeamBin = 
    case NeedBeamCode of
      true -> [];
      false -> get_beam_code(File)
    end,
  Exports = [cerl:var_name(V) || V <- cerl:module_exports(Core)],
  {{Mod, Exports, Icode}, BeamBin}.

get_beam_code(Bin) when is_binary(Bin) -> Bin;
get_beam_code(FileName) ->
  case erl_prim_loader:get_file(FileName) of
    {ok, Bin, _} ->
      Bin;
    error ->
      ?EXIT(no_beam_file)
  end.


%% ---------------------------------------------------------------------
%% All compilations go through this function. Note that it receives only
%% "basic" options. Name is just used for verbosity. The DisasmFun and
%% IcodeFun only collect the Icode; most of the real work is done in the
%% 'finalize' function.

run_compiler(Name, DisasmFun, IcodeFun, Opts0) ->
  Opts = expand_basic_options(Opts0 ++ ?COMPILE_DEFAULTS),
  ?when_option(verbose, Opts, ?debug_msg("Compiling: ~p\n",[Name])),
  ?option_start_time("Compile", Opts),
  Res = run_compiler_1(Name, DisasmFun, IcodeFun, Opts),
  ?option_stop_time("Compile", Opts),
  Res.

run_compiler_1(Name, DisasmFun, IcodeFun, Options) ->
  Parent = self(),
  {trap_exit,TrapExit} = process_info(Parent, trap_exit),
  %% Spawn a compilation process CompProc. In case this process gets
  %% killed, the trap_exit flag is restored to that of the Parent process.
  process_flag(trap_exit, true),
  CompProc =
    spawn_link(
      fun () ->
	  try
	    %% Compiler process
	    set_architecture(Options),
	    pre_init(Options),
	    %% The full option expansion is not done
	    %% until the DisasmFun returns.
	    {Code, CompOpts} = DisasmFun(Options),
	    Opts0 = expand_options(Options ++ CompOpts,
				   get(hipe_target_arch)),
	    Opts =
	      case proplists:get_bool(to_llvm, Opts0) andalso
		not llvm_version_is_OK() of
		true ->
		  ?error_msg("No LLVM version 3.9 or greater "
			     "found in $PATH; aborting "
			     "native code compilation.\n", []),
		  ?EXIT(cant_find_required_llvm_version);
		false ->
		  Opts0
	      end,
	    check_options(Opts),
	    ?when_option(verbose, Options,
			 ?debug_msg("Options: ~p.\n",[Opts])),
	    init(Opts),
	    {Icode, WholeModule} = IcodeFun(Code, Opts),
	    CompRes = compile_finish(Icode, WholeModule, Opts),
	    compiler_return(CompRes, Parent)
	  catch
            error:Error:StackTrace ->
	      print_crash_message(Name, Error, StackTrace),
	      exit(Error);
            throw:{unimplemented_instruction,_Instruction}=Error ->
              exit(Error)
	  end
      end),
  Timeout = case proplists:get_value(timeout, Options) of
	      N when is_integer(N), N >= 0 -> N;
	      undefined -> ?DEFAULT_TIMEOUT;
	      infinity -> infinity;
	      Other ->
		?WARNING_MSG("Bad timeout value: ~P\n"
			     "Using default timeout limit.\n",	
			     [Other, 5]),
		?DEFAULT_TIMEOUT
	    end,
  receive
    {'EXIT', CompProc, normal} -> ok;
    {'EXIT', CompProc, Reason} -> exit(Reason)
  after Timeout ->
      %% Kill the compilation process
      exit(CompProc, kill),
      receive {'EXIT', CompProc, _} -> ok end,
      flush(),
      ?error_msg("ERROR: Compilation of ~w timed out.\n",[Name]),
      exit(timed_out)
  end,
  Result = receive {CompProc, Res} -> Res end,
  process_flag(trap_exit, TrapExit),
  Result.

flush() ->
  receive
    _ -> flush()
  after 0 ->
      ok
  end.

compiler_return(Res, Client) ->
  Client ! {self(), Res}.

compile_finish({Mod, Exports, Icode}, WholeModule, Options) ->
  Res = finalize(Icode, Mod, Exports, WholeModule, Options),
  post(Res, Icode, Options).


%% -------------------------------------------------------------------------
%% finalize/5
%% compiles, assembles, and optionally loads a list of `{MFA, Icode}' pairs,
%% and returns `{ok, {TargetArch, Binary}}' or `{error, Reason, Stack}'.

finalize(OrigList, Mod, Exports, WholeModule, Opts) ->
  List = icode_multret(OrigList, Mod, Opts, Exports),
  {T1Compile,_} = erlang:statistics(runtime),
  CompiledCode =
    case proplists:get_value(use_callgraph, Opts) of
      true -> 
	%% Compiling the functions bottom-up by using a call graph
	CallGraph = hipe_icode_callgraph:construct(List),
	OrdList = hipe_icode_callgraph:to_list(CallGraph),
	finalize_fun(OrdList, Exports, Opts);
      _ -> 
	%% Compiling the functions bottom-up by reversing the list
	OrdList = lists:reverse(List),
	finalize_fun(OrdList, Exports, Opts)
    end,
  {T2Compile,_} = erlang:statistics(runtime),
  ?when_option(verbose, Opts,
	       ?debug_msg("Compiled ~p in ~.2f s\n",
			  [Mod,(T2Compile-T1Compile)/1000])),
  case proplists:get_bool(to_rtl, Opts) of
    true ->
      {ok, CompiledCode};
    false ->
      Closures =
	[MFA || {MFA, Icode} <- List,
		hipe_icode:icode_is_closure(Icode)],
      {T1,_} = erlang:statistics(runtime),
      ?when_option(verbose, Opts, ?debug_msg("Assembling ~w",[Mod])),
      try assemble(CompiledCode, Closures, Exports, Opts) of
	Bin ->
	  {T2,_} = erlang:statistics(runtime),
	  ?when_option(verbose, Opts,
		       ?debug_untagged_msg(" in ~.2f s\n",
					   [(T2-T1)/1000])),
	  {module,Mod} = maybe_load(Mod, Bin, WholeModule, Opts),
	  TargetArch = get(hipe_target_arch),
	  {ok, {TargetArch,Bin}}
      catch
	error:Error:StackTrace ->
	  {error,Error,StackTrace}
      end
  end.

finalize_fun(MfaIcodeList, Exports, Opts) ->
  case proplists:get_value(concurrent_comp, Opts) of
    FalseVal when (FalseVal =:= undefined) orelse (FalseVal =:= false) ->
      NoServers = #comp_servers{pp_server = none, range = none, type = none},
      [finalize_fun_sequential(MFAIcode, Opts, NoServers)
       || {_MFA, _Icode} = MFAIcode <- MfaIcodeList];
    TrueVal when (TrueVal =:= true) orelse (TrueVal =:= debug) ->
      finalize_fun_concurrent(MfaIcodeList, Exports, Opts)
  end.

finalize_fun_concurrent(MfaIcodeList, Exports, Opts) ->
  Self = self(),
  case MfaIcodeList of
    [{{M,_,_},_}|_] ->
      CallGraph = hipe_icode_callgraph:construct_callgraph(MfaIcodeList),
      Exported = [{M, F, A} || {F, A} <- Exports],
      Closures = [MFA || {MFA, Icode} <- MfaIcodeList,
			 hipe_icode:icode_is_closure(Icode)],
      %% In principle, a function could both be exported and used as a
      %% closure so make sure to add it only once in Escaping below
      Escaping = ordsets:from_list(Exported ++ Closures),
      NonEscaping = [MFA || {{_M, F, A} = MFA, Icode} <- MfaIcodeList, 
			    not lists:member({F, A}, Exports),
			    not hipe_icode:icode_is_closure(Icode)],
      TypeServerFun =
	fun() ->
	    hipe_icode_coordinator:coordinate(CallGraph, Escaping,
					      NonEscaping, hipe_icode_type)
	end,
      TypeServer = spawn_link(TypeServerFun),
      PPServerFun =
	fun() ->
	    pp_server_start(Opts)
	end,
      PPServer = spawn_link(PPServerFun),
      RangeServerFun =
	fun() ->
	    hipe_icode_coordinator:coordinate(CallGraph, Escaping,
					      NonEscaping, hipe_icode_range)
	end,
      RangeServer = spawn_link(RangeServerFun),
      Servers = #comp_servers{pp_server = PPServer,
			      range = RangeServer,
			      type = TypeServer},
      CompFuns =
	[fun() ->
	     set_architecture(Opts),
	     pre_init(Opts),
	     init(Opts),
	     Self ! finalize_fun_sequential(IcodeFun, Opts, Servers)
	 end || IcodeFun <- MfaIcodeList],
      lists:foreach(fun (F) -> spawn_link(F) end, CompFuns),
      Final = [receive Res when element(1, Res) =:= MFA -> Res end
	       || {MFA, _} <- MfaIcodeList],
      lists:foreach(fun (Pid) -> stop_and_wait(Pid) end,
		    [PPServer, TypeServer, RangeServer]),
      Final;
    [] ->
      []
  end.

stop_and_wait(Pid) ->
  Pid ! {stop, self()},
  receive
    _ -> ok
  end.

finalize_fun_sequential({MFA, Icode}, Opts, Servers) ->
  {T1, _} = erlang:statistics(runtime),
  ?when_option(verbose, Opts, ?debug_msg("Compiling ~w~n", [MFA])),
  try hipe_main:compile_icode(MFA, Icode, Opts, Servers) of
    {native, _Platform, {unprofiled, Code}} ->
      {T2, _} = erlang:statistics(runtime),
      ?when_option(verbose, Opts,
		   ?debug_msg("Compiled ~w in ~.2f s\n", [MFA,(T2-T1)/1000])),
      {MFA, Code};
    {rtl, LinearRtl} ->
      {MFA, LinearRtl};
    {llvm_binary, Binary} ->
      {MFA, Binary}
  catch
    error:Error:StackTrace ->
      ?when_option(verbose, Opts, ?debug_untagged_msg("\n", [])),
      print_crash_message(MFA, Error, StackTrace),
      exit(Error)
  end.

print_crash_message(What, Error, StackTrace) ->
  StackFun = fun(_,_,_) -> false end,
  FormatFun = fun (Term, _) -> io_lib:format("~p", [Term]) end,
  StackTraceS = erl_error:format_stacktrace(1, StackTrace,
                                            StackFun, FormatFun),
  WhatS = case What of
	    {M,F,A} -> io_lib:format("~w:~w/~w", [M,F,A]);
	    Mod -> io_lib:format("~w", [Mod])
	  end,
  ?error_msg("INTERNAL ERROR~n"
	     "while compiling ~s~n"
	     "crash reason: ~p~n"
	     "~s~n",
	     [WhatS, Error, StackTraceS]).

pp_server_start(Opts) ->
  set_architecture(Opts),
  garbage_collect(),
  pp_server().

pp_server() ->
  receive
    {print, Fun} ->
      Fun(), pp_server();
    {stop, Pid} ->
      Pid ! {done, self()};
    _ -> 
      pp_server()
  end.

icode_multret(List, Mod, Opts, Exports) ->
  case proplists:get_bool(icode_multret, Opts) of
    true ->
      hipe_icode_mulret:mult_ret(List, Mod, Opts, Exports);
    false ->
      List
  end.

maybe_load(Mod, Bin, WholeModule, Opts) ->
  case proplists:get_bool(load, Opts) of
    false ->
      {module, Mod};
    true ->
      ?when_option(verbose, Opts, ?debug_msg("Loading/linking\n", [])),
      do_load(Mod, Bin, WholeModule)
  end.

do_load(Mod, Bin, BeamBinOrPath) when is_binary(BeamBinOrPath);
				      is_list(BeamBinOrPath) ->
  HostArch = get(hipe_host_arch),
  TargetArch = get(hipe_target_arch),
  %% Make sure we can do the load.
  if HostArch =/= TargetArch ->
    ?EXIT({host_and_target_arch_differ, HostArch, TargetArch});
    true -> ok
  end,
  case code:is_sticky(Mod) of
    true ->
      %% We unpack and repack the Beam binary as a workaround to
      %% ensure that it is not compressed.
      {ok, _, Chunks} = beam_lib:all_chunks(BeamBinOrPath),
      {ok, Beam} = beam_lib:build_module(Chunks),
      %% Don't purge or register sticky mods; just load native.
      code:load_native_sticky(Mod, Bin, Beam);
    false ->
      %% Normal loading of a whole module
      ChunkName = hipe_unified_loader:chunk_name(HostArch),
      {ok, _, Chunks0} = beam_lib:all_chunks(BeamBinOrPath),
      Chunks = [{ChunkName, Bin}|lists:keydelete(ChunkName, 1, Chunks0)],
      {ok, BeamPlusNative} = beam_lib:build_module(Chunks),
      code:load_binary(Mod, code:which(Mod), BeamPlusNative)
  end.

assemble(CompiledCode, Closures, Exports, Options) ->
  case proplists:get_bool(to_llvm, Options) of
    false ->
      case get(hipe_target_arch) of
        ultrasparc ->
          hipe_sparc_assemble:assemble(CompiledCode, Closures, Exports, Options);
        powerpc ->
          hipe_ppc_assemble:assemble(CompiledCode, Closures, Exports, Options);
        ppc64 ->
          hipe_ppc_assemble:assemble(CompiledCode, Closures, Exports, Options);
        arm ->
          hipe_arm_assemble:assemble(CompiledCode, Closures, Exports, Options);
        x86 ->
          hipe_x86_assemble:assemble(CompiledCode, Closures, Exports, Options);
        amd64 ->
          hipe_amd64_assemble:assemble(CompiledCode, Closures, Exports, Options);
        Arch ->
          ?EXIT({executing_on_an_unsupported_architecture, Arch})
      end;
    true ->
      %% Merge already compiled code (per MFA) to a single binary.
      hipe_llvm_merge:finalize(CompiledCode, Closures, Exports)
  end.

%% --------------------------------------------------------------------

%% Initialise host and target architectures. Target defaults to host,
%% but can be overridden by passing an option {target, Target}.

set_architecture(Options) ->
  HostArch = erlang:system_info(hipe_architecture),
  put(hipe_host_arch, HostArch),
  put(hipe_target_arch, proplists:get_value(target, Options, HostArch)),
  ok.

%% This sets up some globally accessed stuff that are needed by the
%% compiler process before it even gets the full list of options.
%% Therefore, this expands the current set of options for local use.

pre_init(Opts) ->
  Options = expand_options(Opts, get(hipe_target_arch)),
  %% Initialise some counters used for measurements and benchmarking. If
  %% the option 'measure_regalloc' is given the compilation will return
  %% a keylist with the counter values.
  put(hipe_time,
      case proplists:get_value(time, Options, false) of
	true -> [hipe, hipe_main];
	OptTime -> OptTime
      end),
  lists:foreach(fun (T) -> ?set_hipe_timer_val(T, 0) end, hipe_timers()),
  lists:foreach(fun (Counter) ->
		    case Counter of
		      {CounterName, InitVal} -> put(CounterName, InitVal);
		      CounterName -> put(CounterName, 0)
		    end
		end,
		proplists:get_value(counters, Options, [])),
  put(hipe_debug,     proplists:get_bool(debug, Options)),
  put(hipe_inline_fp, proplists:get_bool(inline_fp, Options)),
  ok.

%% Prepare the compiler process by setting up variables which are
%% accessed globally. Options have been fully expanded at ths point.

init(_Options) ->
  put(callersavetime, 0),
  put(totalspill, {0,0}),
  put(spilledtemps, 0),
  put(pre_ra_instrs, 0),
  put(post_ra_instrs, 0),
  put(pre_ra_temps, 0),
  put(post_ra_temps, 0),
  put(noregs, 0),
  put(bbs, 0),
  ok.

%% --------------------------------------------------------------------

post(Res, Icode, Options) ->
  TimerVals = 
    case proplists:get_value(timers, Options) of
      Timers when is_list(Timers) ->
	[{Timer, ?get_hipe_timer_val(Timer)} || Timer <- Timers];
      _ -> []
    end,
  CounterVals = 
    case proplists:get_value(counters, Options) of
      Counters when is_list(Counters) ->
	[case Counter of
	   {CounterName, _InitVal} -> {CounterName, get(CounterName)};
	   CounterName -> {CounterName, get(CounterName)}
	 end
	 || Counter <- Counters];
      _ -> []
    end,
  Measures = 
    case proplists:get_bool(measure_regalloc, Options) of
      true ->
	get();  % return whole process dictionary list (simplest way...)
      false -> []
    end,
  Info = TimerVals ++ CounterVals ++ Measures,
  case proplists:get_bool(get_called_modules, Options) of
    true ->
      CalledMods = hipe_icode_callgraph:get_called_modules(Icode),
      case Info of
	[] ->
	  {Res, {called_modules, CalledMods}};
	_ ->
	  {Res, {info, Info}, {called_modules, CalledMods}}
      end;
    false ->
      case Info of
	[] ->
	  Res;
	_ ->
	  {Res, {info, Info}}
      end
  end.

%% --------------------------------------------------------------------

%% @doc Returns the current HiPE version as a string().
-spec version() -> nonempty_string().

version() ->
  ?VERSION_STRING().

%% @doc Returns checksum identifying the target runtime system.
-spec erts_checksum() -> integer().

erts_checksum() ->
  ?HIPE_ERTS_CHECKSUM.

%% --------------------------------------------------------------------
%% D O C U M E N T A T I O N   -   H E L P 
%% --------------------------------------------------------------------

%% @doc Prints on-line documentation to the standard output.
-spec help() -> 'ok'.

help() ->
  M =
    "The HiPE Compiler (Version " ++ ?VERSION_STRING() ++ ")\n" ++
    "\n" ++
    " The normal way to native-compile Erlang code using HiPE is to\n" ++
    " include `native' in the Erlang compiler options, as in:\n" ++
    "     1> c(my_module, [native]).\n" ++
    " Options to the HiPE compiler must then be passed as follows:\n" ++
    "     1> c(my_module, [native,{hipe,Options}]).\n" ++
    " Use `help_options()' for details.\n" ++
    "\n" ++
    " Utility functions:\n" ++
    "   help()\n" ++
    "     Prints this message.\n" ++
    "   help_options()\n" ++
    "     Prints a description of options recognized by the\n" ++
    "     HiPE compiler.\n" ++
    "   help_option(Option)\n" ++
    "     Prints a description of that option.\n" ++
    "   help_debug_options()\n" ++
    "     Prints a description of debug options.\n" ++
    "   version() ->\n" ++
    "     Returns the HiPE version as a string'.\n" ++
    "   erts_checksum() ->\n" ++
    "     Returns a checksum identifying the target runtime system.\n" ++
    "\n" ++
    " For HiPE developers only:\n" ++
    "  Use `help_hiper()' for information about HiPE's low-level interface\n",
  io:put_chars(M),
  ok.

-spec help_hiper() -> 'ok'.

help_hiper() ->
  M =
    " This interface is supposed to be used by HiPE-developers only!\n" ++
    " Note that all options are specific to the HiPE compiler.\n" ++
    "   c(Name,Options)\n" ++ 
    "     Compiles the module or function Name and loads it\n" ++
    "     to memory. Name is an atom or a tuple {M,F,A}.\n" ++
    "   c(Name)\n" ++
    "     As above, but using only default options.\n" ++
    "   f(File,Options)\n" ++ 
    "     As c(Name,File,Options), but taking the module name\n" ++
    "     from File.\n" ++
    "   f(File)\n" ++ 
    "     As above, but using only default options.\n" ++
    "   compile(Name,Options)\n" ++
    "     Compiles the module or function Name to a binary.\n" ++
    "     By default, this does not load to memory.\n" ++
    "   compile(Name)\n" ++ 
    "     As above, but using only default options.\n" ++
    "   file(File,Options)\n" ++ 
    "     As compile(Name,File,Options), but taking the\n" ++
    "     module name from File.\n" ++
    "   file(File)\n" ++ 
    "     As above, but using only default options.\n" ++
    "   load(Module)\n" ++
    "     Loads the named module into memory.\n",
  io:put_chars(M),
  ok.

%% TODO: it should be possible to specify the target somehow when asking
%% for available options. Right now, you only see host machine options.

%% @doc Prints documentation about options to the standard output.
-spec help_options() -> 'ok'.

help_options() ->
  HostArch = erlang:system_info(hipe_architecture),
  O0 = expand_options([o0] ++ ?COMPILE_DEFAULTS, HostArch),
  O1 = expand_options([o1] ++ ?COMPILE_DEFAULTS, HostArch),
  O2 = expand_options([o2] ++ ?COMPILE_DEFAULTS, HostArch),
  O3 = expand_options([o3] ++ ?COMPILE_DEFAULTS, HostArch),
  io:format("HiPE Compiler Options\n" ++
	    " Boolean-valued options generally have corresponding " ++
	    "aliases `no_...',\n" ++
	    " and can also be specified as `{Option, true}' " ++
	    "or `{Option, false}.\n\n" ++
	    " General boolean options:\n" ++
	    "   ~p.\n\n" ++
	    " Non-boolean options:\n" ++
	    "   o#, where 0 =< # =< 3:\n" ++
	    "     Select optimization level (the default is 2).\n\n" ++
	    " Further options can be found below; " ++
	    "use `hipe:help_option(Name)' for details.\n\n" ++
	    " Aliases:\n" ++
	    "   pp_all = ~p,\n" ++
	    "   pp_sparc = pp_native,\n" ++
	    "   pp_x86 = pp_native,\n" ++
	    "   pp_amd64 = pp_native,\n" ++
	    "   pp_ppc = pp_native,\n" ++
	    "   o0 = ~p,\n" ++
	    "   o1 = ~p ++ o0,\n" ++
	    "   o2 = ~p ++ o1,\n" ++
	    "   o3 = ~p ++ o2.\n",
	    [ordsets:from_list([verbose, debug, time, load, pp_beam,
				pp_icode, pp_rtl, pp_native, pp_asm,
				timeout]),
	     expand_options([pp_all], HostArch),
	     O0 -- [o0],
	     (O1 -- O0) -- [o1],
	     (O2 -- O1) -- [o2],
	     (O3 -- O2) -- [o3]]),
  ok.

%% Documentation of the individual options.
%% If you add an option, please add help-text here.

-spec option_text(atom()) -> string().

option_text('O') ->
  "Specify optimization level. Used as o1, o2, o3.\n" ++
  "    At the moment levels 0 - 3 are implemented.\n" ++
  "    Aliases: 'O1', 'O2', O3'.";
option_text(caller_save_spill_restore) ->
  "Activates caller save register spills and restores";
option_text(debug) ->
  "Outputs internal debugging information during compilation";
option_text(icode_call_elim) ->
  "Performs call elimination of BIFs that are side-effect free\n" ++
  "only on some argument types";
option_text(icode_range) ->
  "Performs integer range analysis on the Icode level";
option_text(icode_ssa_check) ->
  "Checks whether Icode is on SSA form or not";
option_text(icode_ssa_copy_prop) ->
  "Performs copy propagation on Icode SSA";
option_text(icode_ssa_const_prop) ->
  "Performs sparse conditional constant propagation on Icode SSA";
option_text(icode_ssa_struct_reuse) ->
  "Factors out common tuple and list constructions on Icode SSA";
option_text(icode_type) ->
  "Performs type analysis on the Icode level\n" ++
  "and then simplifies the code based on the results of this analysis";
option_text(load) ->
  "Automatically load the produced native code into memory";
option_text(peephole) ->
  "Enables peephole optimizations";
option_text(pmatch) ->
  "Enables pattern matching compilation when compiling from Core;\n" ++
  "has no effect when compiling from BEAM bytecode";
option_text(pp_asm) ->
  "Displays assembly listing with addresses and bytecode\n" ++
  "Currently available for x86 only";
option_text(pp_beam) ->
  "Display the input BEAM code";
option_text(pp_icode) ->
  "Display the intermediate HiPE-ICode";
option_text(pp_rtl) ->
  "Display the intermediate HiPE-RTL code";
option_text(pp_rtl_lcm) ->
  "Display the intermediate HiPE-RTL lazy code motion sets";
option_text(pp_rtl_ssapre) ->
  "Display the intermediate HiPE-RTL A-SSAPRE sets";
option_text(pp_native) ->
  "Display the generated (back-end specific) native code";
option_text(regalloc) ->
  "Select register allocation algorithm. Used as {regalloc, METHOD}.\n" ++
  "  Currently available methods:\n" ++
  "    naive - spills everything (for debugging and testing)\n" ++
  "    linear_scan - fast; not so good if few registers available\n" ++
  "    graph_color - slow, but gives OK performance\n" ++
  "    coalescing - slower, tries hard to use registers\n" ++
  "    optimistic - another variant of a coalescing allocator";
option_text(remove_comments) ->
  "Strip comments from intermediate code";
option_text(ra_range_split) ->
  "Split live ranges of temporaries live over call instructions\n"
  "before performing register allocation.\n"
  "Heuristically tries to move stack accesses to the cold path of function.\n"
  "This range splitter is more sophisticated than 'ra_restore_reuse', but has\n"
  "a significantly larger impact on compile time.\n"
  "Should only be used with move coalescing register allocators.";
option_text(ra_restore_reuse) ->
  "Split live ranges of temporaries such that straight-line\n"
  "code will not need to contain multiple restores from the same stack\n"
  "location.\n"
  "Should only be used with move coalescing register allocators.";
option_text(rtl_ssa) ->
  "Perform SSA conversion on the RTL level -- default starting at O2";
option_text(rtl_ssa_const_prop) ->
  "Performs sparse conditional constant propagation on RTL SSA";
option_text(rtl_lcm) ->
  "Perform Lazy Code Motion on RTL";
option_text(rtl_ssapre) ->
  "Perform A-SSAPRE on RTL";
option_text(time) ->
  "Reports the compilation times for the different stages\n" ++
  "of the compiler.\n" ++
  "    {time, Module}       reports timings for the module Module.\n" ++
  "    {time, [M1, M2, M3]} reports timings for the specified modules.\n" ++
  "    {time, all}          reports timings all modules.\n" ++
  "    time                 reports timings for the main module.\n";
option_text(timeout) ->
  "Specify compilation time limit in ms. Used as {timeout, LIMIT}.\n" ++
  "    The limit must be a non-negative integer or the atom 'infinity'.\n" ++
  "    The current default limit is 15 minutes (900000 ms).";
option_text(use_indexing) ->
  "Use indexing for multiple-choice branch selection";
option_text(use_callgraph) ->
  "Compile the functions in a module according to a reversed topological\n" ++
  "sorted order to gain more information when using a persistent lookup\n" ++
  "table for storing intra-modular type information";
option_text(verbose) ->
  "Output information about what is being done";
option_text(Opt) when is_atom(Opt) ->
  "".

%% @doc Prints documentation about a specific option to the standard output.
-spec help_option(comp_option()) -> 'ok'.

help_option(Opt) ->
  HostArch = erlang:system_info(hipe_architecture),
  case expand_options([Opt], HostArch) of
    [Opt] ->
      Name = if is_atom(Opt) -> Opt;
		tuple_size(Opt) =:= 2 -> element(1, Opt)
	     end,
      case option_text(Name) of
	"" ->  
	  case lists:member(Name, opt_keys()) of
	    true ->
	      io:format("~w - Sorry, this option is not documented yet.\n",
			[Name]);
	    _ -> 
	      io:format("Unknown option ~p.\n", [Name])
	  end;
	Txt ->
	  io:fwrite("~w - ~s\n", [Name, Txt])
      end;
    Opts ->
      io:fwrite("This is an alias for: ~p.\n", [Opts])
  end,
  ok.

%% @doc Prints documentation about debugging options to the standard
%% output.
-spec help_debug_options() -> 'ok'.

help_debug_options() ->
  io:format("HiPE compiler debug options:\n" ++
	    "  Might require that some modules have been compiled " ++ 
	    "with the debug flag.\n" ++
	    "    rtl_show_translation - Prints each step in the\n" ++
	    "                           translation from Icode to RTL\n",
	    []),
  ok.

hipe_timers() ->
  [time_ra].

%% ____________________________________________________________________
%% 
%% Option expansion

%% These are currently in use, but not documented:
%%
%%     count_instrs:
%%     icode_type:
%%     icode_range:
%%     {ls_order, Order}:
%%     {regalloc, Algorithm}:
%%     remove_comments
%%     timeregalloc:
%%     timers
%%     use_indexing

%% Valid option keys. (Don't list aliases or negations - the check is
%% done after the options have been expanded to normal form.)

opt_keys() ->
    [
     binary_opt,
     bitlevel_binaries,
     caller_save_spill_restore,
     concurrent_comp,
     core,
     core_transform,
     counters,
     count_instrs,
     count_spills,
     count_temps,
     debug,
     get_called_modules,
     split_arith,
     split_arith_unsafe,
     icode_call_elim,
     icode_inline_bifs,
     icode_ssa_check,
     icode_ssa_copy_prop,
     icode_ssa_const_prop,
     icode_ssa_struct_reuse,
     icode_type,
     icode_range,
     icode_multret,
     inline_fp,
     ls_order,
     load,
     measure_regalloc,
     peephole,
     pmatch,
     pp_asm,
     pp_beam,
     pp_icode,
     pp_icode_ssa,
     pp_icode_split_arith,
     pp_opt_icode,
     pp_range_icode,
     pp_typed_icode,
     pp_icode_liveness,
     pp_native,
     pp_rtl,
     pp_rtl_liveness,
     pp_rtl_ssa,
     pp_rtl_lcm,
     pp_rtl_ssapre,
     pp_rtl_linear,
     ra_partitioned,
     ra_prespill,
     ra_range_split,
     ra_restore_reuse,
     range_split_min_gain,
     range_split_mode1_fudge,
     range_split_weight_power,
     range_split_weights,
     regalloc,
     remove_comments,
     rtl_ssa,
     rtl_ssa_const_prop,
     rtl_lcm,
     rtl_ssapre,
     rtl_show_translation,
     spillmin_color,
     target,
     time,
     timeout,
     timeregalloc,
     timers,
     to_rtl,
     to_llvm, % Use the LLVM backend for compilation.
     llvm_save_temps, % Save the LLVM intermediate files in the current
                      % directory; useful for debugging.
     llvm_llc, % Specify llc optimization-level: o1, o2, o3, undefined.
     llvm_opt, % Specify opt optimization-level: o1, o2, o3, undefined.
     use_indexing,
     use_inline_atom_search,
     use_callgraph,
     use_clusters,
     use_jumptable,
     verbose,
     verify_gcsafe,
     %% verbose_spills,
     x87].

%% Definitions:

o0_opts(_TargetArch) ->
  [concurrent_comp, {regalloc,linear_scan}].

o1_opts(TargetArch) ->
  Common = [inline_fp, pmatch, peephole, ra_prespill, ra_partitioned,
	    icode_ssa_const_prop, icode_ssa_copy_prop, icode_inline_bifs,
	    rtl_ssa, rtl_ssa_const_prop, rtl_ssapre,
	    spillmin_color, use_indexing, remove_comments,
	    binary_opt, {regalloc,coalescing}, ra_restore_reuse
	    | o0_opts(TargetArch)],
  case TargetArch of
    ultrasparc ->
      Common;
    powerpc ->
      Common;
    ppc64 ->
      Common;
    arm ->
      Common -- [inline_fp]; % Pointless optimising for absent hardware
    x86 ->
      [x87 | Common];        % XXX: Temporary until x86 has sse2
    amd64 ->
      Common;
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

o2_opts(TargetArch) ->
  Common = [icode_type, icode_call_elim, % icode_ssa_struct_reuse,
	    ra_range_split, range_split_weights, % XXX: Having defaults here is ugly
	    rtl_lcm | (o1_opts(TargetArch) -- [rtl_ssapre, ra_restore_reuse])],
  case TargetArch of
    T when T =:= amd64 orelse T =:= ppc64 -> % 64-bit targets
      [icode_range | Common];
    _ ->      % T \in [arm, powerpc, ultrasparc, x86]
      Common  % [rtl_ssapre | Common];
    end.

o3_opts(TargetArch) ->
  %% no point checking for target architecture since this is checked in 'o1'
  [icode_range | o2_opts(TargetArch)].

%% Note that in general, the normal form for options should be positive.
%% This is a good programming convention, so that tests in the code say
%% "if 'x' ..." instead of "if not 'no_x' ...".

opt_negations() ->
  [{no_binary_opt, binary_opt},
   {no_bitlevel_binaries, bitlevel_binaries},
   {no_core, core},
   {no_debug, debug},
   {no_get_called_modules, get_called_modules},
   {no_split_arith, split_arith},
   {no_concurrent_comp, concurrent_comp},
   {no_icode_inline_bifs, icode_inline_bifs},
   {no_icode_range, icode_range},
   {no_icode_split_arith, icode_split_arith},
   {no_icode_call_elim, icode_call_elim},
   {no_icode_ssa_check, icode_ssa_check},
   {no_icode_ssa_copy_prop, icode_ssa_copy_prop},
   {no_icode_ssa_const_prop, icode_ssa_const_prop},
   {no_icode_ssa_struct_reuse, icode_ssa_struct_reuse},
   {no_icode_type, icode_type},
   {no_inline_fp, inline_fp},
   {no_load, load},
   {no_peephole, peephole},
   {no_pmatch, pmatch},
   {no_pp_beam, pp_beam},
   {no_pp_icode, pp_icode},
   {no_pp_icode_ssa, pp_icode_ssa},
   {no_pp_opt_icode, pp_opt_icode},
   {no_pp_typed_icode, pp_typed_icode},
   {no_pp_rtl, pp_rtl},
   {no_pp_native, pp_native},
   {no_pp_rtl_lcm, pp_rtl_lcm},
   {no_pp_rtl_ssapre, pp_rtl_ssapre},
   {no_ra_partitioned, ra_partitioned},
   {no_ra_prespill, ra_prespill},
   {no_ra_range_split, ra_range_split},
   {no_ra_restore_reuse, ra_restore_reuse},
   {no_range_split_weights, range_split_weights},
   {no_remove_comments, remove_comments},
   {no_rtl_ssa, rtl_ssa},
   {no_rtl_ssa_const_prop, rtl_ssa_const_prop},
   {no_rtl_lcm, rtl_lcm},
   {no_rtl_ssapre, rtl_ssapre},
   {no_rtl_show_translation, rtl_show_translation},
   {no_time, time},
   {no_use_callgraph, use_callgraph},
   {no_use_clusters, use_clusters},
   {no_use_inline_atom_search, use_inline_atom_search},
   {no_use_indexing, use_indexing},
   {no_verify_gcsafe, verify_gcsafe}].

%% Don't use negative forms in right-hand sides of aliases and expansions!
%% We only expand negations once, before the other expansions are done.

opt_aliases() ->
  [{'O0', o0},
   {'O1', o1},
   {'O2', o2},
   {'O3', o3},
   {pp_sparc, pp_native},
   {pp_x86, pp_native},
   {pp_amd64, pp_native},
   {pp_ppc, pp_native}].

opt_basic_expansions() ->
  [{pp_all, [pp_beam, pp_icode, pp_rtl, pp_native]}].

opt_expansions(TargetArch) ->
  [{o0, o0_opts(TargetArch)},
   {o1, o1_opts(TargetArch)},
   {o2, o2_opts(TargetArch)},
   {o3, o3_opts(TargetArch)},
   {to_llvm, llvm_opts(o3, TargetArch)},
   {{to_llvm, o0}, llvm_opts(o0, TargetArch)},
   {{to_llvm, o1}, llvm_opts(o1, TargetArch)},
   {{to_llvm, o2}, llvm_opts(o2, TargetArch)},
   {{to_llvm, o3}, llvm_opts(o3, TargetArch)},
   {x87, [x87, inline_fp]},
   {inline_fp, case TargetArch of  %% XXX: Temporary until x86 has sse2
		 x86 -> [x87, inline_fp];
		 _ -> [inline_fp] end}].

llvm_opts(O, TargetArch) ->
  Base = [to_llvm, {llvm_opt, O}, {llvm_llc, O}],
  case TargetArch of
    %% A llvm bug present in 3.4 through (at least) 3.8 miscompiles x86
    %% functions that have floats are spilled to stack by clobbering the process
    %% pointer (ebp) trying to realign the stack pointer.
    x86 -> [no_inline_fp | Base];
      _ -> Base
  end.

%% This expands "basic" options, which may be tested early and cannot be
%% in conflict with options found in the source code.

-spec expand_basic_options(comp_options()) -> comp_options().

expand_basic_options(Opts) ->
  proplists:normalize(Opts, [{negations, opt_negations()},
			     {aliases, opt_aliases()},
			     {expand, opt_basic_expansions()}]).

-spec expand_kt2(comp_options()) -> comp_options().

expand_kt2(Opts) -> 
  proplists:normalize(Opts, [{expand, [{kt2_type,
					[{use_callgraph, fixpoint}, core, 
					 {core_transform, cerl_typean}]}]}]).

%% Note that the given
%% list should contain the total set of options, since things like 'o2'
%% are expanded here. Basic expansions are processed here also, since
%% this function is called from the help functions.

-spec expand_options(comp_options(), hipe_architecture()) -> comp_options().

expand_options(Opts0, TargetArch) ->
  Opts1 = proplists:normalize(Opts0, [{aliases, opt_aliases()}]),
  Opts = normalise_opt_options(Opts1),
  proplists:normalize(Opts, [{negations, opt_negations()},
			     {expand, opt_basic_expansions()},
			     {expand, opt_expansions(TargetArch)},
			     {negations, opt_negations()}]).

normalise_opt_options([o0|Opts]) -> [o0] ++ (Opts -- [o0, o1, o2, o3]);
normalise_opt_options([o1|Opts]) -> [o1] ++ (Opts -- [o0, o1, o2, o3]);
normalise_opt_options([o2|Opts]) -> [o2] ++ (Opts -- [o0, o1, o2, o3]);
normalise_opt_options([o3|Opts]) -> [o3] ++ (Opts -- [o0, o1, o2, o3]);
normalise_opt_options([O|Opts]) -> [O|normalise_opt_options(Opts)];
normalise_opt_options([]) -> [].

-spec check_options(comp_options()) -> 'ok'.

check_options(Opts) ->
  Keys = ordsets:from_list(opt_keys()),
  Used = ordsets:from_list(proplists:get_keys(Opts)),
  case ordsets:subtract(Used, Keys) of
    [] ->
      ok;
    L ->
      ?WARNING_MSG("Unknown options: ~p.\n", [L]),
      ok
  end.

-spec erllvm_is_supported() -> boolean().
erllvm_is_supported() ->
  %% XXX: The test should really check the _target_ architecture,
  %%      (hipe_target_arch), but there's no guarantee it's set.
  Arch = erlang:system_info(hipe_architecture),
  lists:member(Arch, [amd64, x86]) andalso llvm_version_is_OK().

-spec llvm_version_is_OK() -> boolean().
llvm_version_is_OK() ->
  get_llvm_version() >= {3,9}.

-type llvm_version() :: {Major :: integer(), Minor :: integer()}.

-spec get_llvm_version() -> llvm_version() | {0, 0}.
get_llvm_version() ->
  OptStr = os:cmd("opt -version"),
  SubStr = "LLVM version ", N = length(SubStr),
  case string:find(OptStr, SubStr) of
     nomatch -> % No opt available
       {0, 0};
     S ->
       case string:lexemes(string:slice(S, N), ".") of
	 [MajorS, MinorS | _] ->
	   case {string:to_integer(MajorS), string:to_integer(MinorS)} of
	     {{Major, ""}, {Minor, _}}
	       when is_integer(Major), is_integer(Minor) ->
	       {Major, Minor};
	     _ -> {0, 0}
	   end;
	 _ -> {0, 0} %XXX: Assumes no revision numbers in versioning
       end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
