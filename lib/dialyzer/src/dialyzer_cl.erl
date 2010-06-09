%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_cl.erl
%%% Authors : Tobias Lindahl <tobiasl@it.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : The command line interface for the Dialyzer tool.
%%%
%%% Created : 27 Apr 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer_cl).

-export([start/1]).

-include("dialyzer.hrl").
-include_lib("kernel/include/file.hrl").  % needed for #file_info{}

-record(cl_state,
	{backend_pid                      :: pid(),
	 erlang_mode     = false          :: boolean(),
	 external_calls  = []             :: [mfa()],
         external_types  = []             :: [mfa()],
	 legal_warnings  = ordsets:new()  :: [dial_warn_tag()],
	 mod_deps        = dict:new()     :: dict(),
	 output          = standard_io	  :: io:device(),
	 output_format   = formatted      :: 'raw' | 'formatted',
	 output_plt      = none           :: 'none' | file:filename(),
	 plt_info        = none           :: 'none' | dialyzer_plt:plt_info(),
	 report_mode     = normal         :: rep_mode(),
	 return_status= ?RET_NOTHING_SUSPICIOUS	:: dial_ret(),
	 stored_warnings = []             :: [dial_warning()],
	 unknown_behaviours = []          :: [dialyzer_behaviours:behaviour()]
	}).

%%--------------------------------------------------------------------

-spec start(#options{}) -> {dial_ret(), [dial_warning()]}.

start(#options{analysis_type = AnalysisType} = Options) ->
  process_flag(trap_exit, true),
  case AnalysisType of
    plt_check  -> check_plt(Options);
    plt_build  -> build_plt(Options);
    plt_add    -> add_to_plt(Options);
    plt_remove -> remove_from_plt(Options);
    succ_typings -> do_analysis(Options)
  end.

%%--------------------------------------------------------------------

build_plt(Opts) ->
  Opts1 = init_opts_for_build(Opts),
  Files = get_files_from_opts(Opts1),
  Md5 = dialyzer_plt:compute_md5_from_files(Files),
  PltInfo = {Md5, dict:new()},
  do_analysis(Files, Opts1, dialyzer_plt:new(), PltInfo).

init_opts_for_build(Opts) ->
  case Opts#options.output_plt =:= none of
    true ->
      case Opts#options.init_plt of
	none -> Opts#options{init_plt = none, output_plt = get_default_plt()};
	Plt  -> Opts#options{init_plt = none, output_plt = Plt}
      end;
    false -> Opts#options{init_plt = none}
  end.

%%--------------------------------------------------------------------

add_to_plt(Opts) ->
  Opts1 = init_opts_for_add(Opts),
  AddFiles = get_files_from_opts(Opts1),
  plt_common(Opts1, [], AddFiles).

init_opts_for_add(Opts) ->
  case Opts#options.output_plt =:= none of
    true ->
      case Opts#options.init_plt of
	none -> Opts#options{output_plt = get_default_plt(),
			     init_plt = get_default_plt()};
	Plt  -> Opts#options{output_plt = Plt}
      end;
    false ->
      case Opts#options.init_plt =:= none of
	true  -> Opts#options{init_plt = get_default_plt()};
	false -> Opts
      end
  end.

%%--------------------------------------------------------------------

check_plt(Opts) ->
  Opts1 = init_opts_for_check(Opts),
  report_check(Opts),
  plt_common(Opts1, [], []).

init_opts_for_check(Opts) ->
  Plt =
    case Opts#options.init_plt of
      none -> get_default_plt();
      Plt0 -> Plt0
    end,
  Opts#options{files         = [],
	       files_rec     = [],
	       analysis_type = plt_check,
	       defines       = [],
	       from          = byte_code,
	       init_plt      = Plt,
	       include_dirs  = [],
	       output_plt    = Plt,
	       use_contracts = true
	      }.

%%--------------------------------------------------------------------

remove_from_plt(Opts) ->
  Opts1 = init_opts_for_remove(Opts),
  Files = get_files_from_opts(Opts1),
  plt_common(Opts1, Files, []).

init_opts_for_remove(Opts) ->
  case Opts#options.output_plt =:= none of
    true ->
      case Opts#options.init_plt of
	none -> Opts#options{output_plt = get_default_plt(),
			     init_plt = get_default_plt()};
	Plt  -> Opts#options{output_plt = Plt}
      end;
    false ->
      case Opts#options.init_plt =:= none of
	true  -> Opts#options{init_plt = get_default_plt()};
	false -> Opts
      end
  end.

%%--------------------------------------------------------------------

plt_common(Opts, RemoveFiles, AddFiles) ->
  case check_plt(Opts, RemoveFiles, AddFiles) of
    ok ->
      case Opts#options.report_mode of
	quiet -> ok;
	_ -> io:put_chars(" yes\n")
      end,
      {?RET_NOTHING_SUSPICIOUS, []};
    {old_version, Md5} ->
      PltInfo = {Md5, dict:new()},
      Files = [F || {F, _} <- Md5],
      do_analysis(Files, Opts, dialyzer_plt:new(), PltInfo);
    {differ, Md5, DiffMd5, ModDeps} ->
      report_failed_plt_check(Opts, DiffMd5),
      {AnalFiles, RemovedMods, ModDeps1} = 
	expand_dependent_modules(Md5, DiffMd5, ModDeps),
      Plt = clean_plt(Opts#options.init_plt, RemovedMods),
      case AnalFiles =:= [] of
	true ->
	  %% Only removed stuff. Just write the PLT.
	  dialyzer_plt:to_file(Opts#options.output_plt, Plt, ModDeps, 
			       {Md5, ModDeps}),
	  {?RET_NOTHING_SUSPICIOUS, []};
	false ->
	  do_analysis(AnalFiles, Opts, Plt, {Md5, ModDeps1})
      end;
    {error, no_such_file} ->
      Msg = io_lib:format("Could not find the PLT: ~s\n~s",
			  [Opts#options.init_plt, default_plt_error_msg()]),
      error(Msg);
    {error, not_valid} ->
      Msg = io_lib:format("The file: ~s is not a valid PLT file\n~s",
			  [Opts#options.init_plt, default_plt_error_msg()]),
      error(Msg);
    {error, read_error} ->
      Msg = io_lib:format("Could not read the PLT: ~s\n~s",
			  [Opts#options.init_plt, default_plt_error_msg()]),
      error(Msg);
    {error, {no_file_to_remove, F}} ->
      Msg = io_lib:format("Could not remove the file ~s from the PLT: ~s\n",
			  [F, Opts#options.init_plt]),
      error(Msg)
  end.

default_plt_error_msg() ->
  "Use the options:\n"
    "   --build_plt   to build a new PLT; or\n"
    "   --add_to_plt  to add to an existing PLT\n"
    "\n"
    "For example, use a command like the following:\n"
    "   dialyzer --build_plt --apps erts kernel stdlib mnesia\n"
    "Note that building a PLT such as the above may take 20 mins or so\n"
    "\n"
    "If you later need information about other applications, say crypto,\n"
    "you can extend the PLT by the command:\n"
    "  dialyzer --add_to_plt --apps crypto\n"
    "For applications that are not in Erlang/OTP use an absolute file name.\n".

%%--------------------------------------------------------------------

check_plt(Opts, RemoveFiles, AddFiles) ->
  Plt = Opts#options.init_plt,
  case dialyzer_plt:check_plt(Plt, RemoveFiles, AddFiles) of
    {old_version, _MD5} = OldVersion ->
      report_old_version(Opts),
      OldVersion;
    {differ, _MD5, _DiffMd5, _ModDeps} = Differ ->
      Differ;
    ok ->
      ok;
    {error, _Reason} = Error ->
      Error
  end.

%%--------------------------------------------------------------------

report_check(#options{report_mode = ReportMode, init_plt = InitPlt}) ->
  case ReportMode of
    quiet -> ok;
    _ ->
      io:format("  Checking whether the PLT ~s is up-to-date...", [InitPlt])
  end.

report_old_version(#options{report_mode = ReportMode, init_plt = InitPlt}) ->
  case ReportMode of
    quiet -> ok;
    _ ->
      io:put_chars(" no\n"),
      io:format("    (the PLT ~s was built with an old version of Dialyzer)\n",
		[InitPlt])
  end.

report_failed_plt_check(#options{analysis_type = AnalType, 
				 report_mode = ReportMode}, DiffMd5) ->
  case AnalType =:= plt_check of
    true ->
      case ReportMode of
	quiet -> ok;
	normal -> io:format(" no\n", []);
	verbose -> report_md5_diff(DiffMd5)
      end;
    false -> ok
  end.

report_analysis_start(#options{analysis_type = Type,
			       report_mode = ReportMode,
			       init_plt = InitPlt, 
			       output_plt = OutputPlt}) ->
  case ReportMode of
    quiet -> ok;
    _ ->
      io:format("  "),
      case Type of
	plt_add ->
	  case InitPlt =:= OutputPlt of
	    true -> io:format("Adding information to ~s...", [OutputPlt]);
	    false -> io:format("Adding information from ~s to ~s...", 
			       [InitPlt, OutputPlt])
	  end;
	plt_build ->
	  io:format("Creating PLT ~s ...", [OutputPlt]);
	plt_check ->
	  io:format("Rebuilding the information in ~s...", [OutputPlt]);
	plt_remove ->
	  case InitPlt =:= OutputPlt of
	    true -> io:format("Removing information from ~s...", [OutputPlt]);
	    false -> io:format("Removing information from ~s to ~s...", 
			       [InitPlt, OutputPlt])
	  end;
	succ_typings -> io:format("Proceeding with analysis...")
      end
  end.

report_native_comp(#options{report_mode = ReportMode}) ->
  case ReportMode of
    quiet -> ok;
    _ -> io:format("  Compiling some key modules to native code...")
  end.

report_elapsed_time(T1, T2, #options{report_mode = ReportMode}) ->
  case ReportMode of
    quiet -> ok;
    _ ->
      ElapsedTime = T2 - T1,
      Mins = ElapsedTime div 60000,
      Secs = (ElapsedTime rem 60000) / 1000,
      io:format(" done in ~wm~.2fs\n", [Mins, Secs])
  end.

report_md5_diff(List) ->
  io:format("    The PLT information is not up to date:\n", []),
  case [Mod || {removed, Mod} <- List] of
    [] -> ok;
    RemovedMods -> io:format("    Removed modules: ~p\n", [RemovedMods])
  end,
  case [Mod || {differ, Mod} <- List] of
    [] -> ok;
    ChangedMods -> io:format("    Changed modules: ~p\n", [ChangedMods])
  end.

%%--------------------------------------------------------------------

get_default_plt() ->
  dialyzer_plt:get_default_plt().

%%--------------------------------------------------------------------

do_analysis(Options) ->
  Files = get_files_from_opts(Options),
  case Options#options.init_plt of
    none -> do_analysis(Files, Options, dialyzer_plt:new(), none);
    File -> do_analysis(Files, Options, dialyzer_plt:from_file(File), none)
  end.
  
do_analysis(Files, Options, Plt, PltInfo) ->
  assert_writable(Options#options.output_plt),
  hipe_compile(Files, Options),
  report_analysis_start(Options),
  State0 = new_state(),
  State1 = init_output(State0, Options),
  State2 = State1#cl_state{legal_warnings = Options#options.legal_warnings,
			   output_plt = Options#options.output_plt,
			   plt_info = PltInfo,
			   erlang_mode = Options#options.erlang_mode,
			   report_mode = Options#options.report_mode},
  AnalysisType = convert_analysis_type(Options#options.analysis_type,
				       Options#options.get_warnings),
  InitAnalysis = #analysis{type = AnalysisType,
			   defines = Options#options.defines,
			   include_dirs = Options#options.include_dirs,
			   files = Files,
			   start_from = Options#options.from, 
			   plt = Plt,
			   use_contracts = Options#options.use_contracts,
			   callgraph_file = Options#options.callgraph_file},
  State3 = start_analysis(State2, InitAnalysis),
  {T1, _} = statistics(wall_clock),
  Return = cl_loop(State3),
  {T2, _} = statistics(wall_clock),
  report_elapsed_time(T1, T2, Options),
  Return.

convert_analysis_type(plt_check, true)   -> succ_typings;
convert_analysis_type(plt_check, false)  -> plt_build;
convert_analysis_type(plt_add, true)     -> succ_typings;
convert_analysis_type(plt_add, false)    -> plt_build;
convert_analysis_type(plt_build, true)   -> succ_typings;
convert_analysis_type(plt_build, false)  -> plt_build;
convert_analysis_type(plt_remove, true)  -> succ_typings;
convert_analysis_type(plt_remove, false) -> plt_build;
convert_analysis_type(succ_typings, _)   -> succ_typings.

%%--------------------------------------------------------------------

assert_writable(none) ->
  ok;
assert_writable(PltFile) ->
  case check_if_writable(PltFile) of
    true -> ok;
    false ->
      Msg = io_lib:format("    The PLT file ~s is not writable", [PltFile]),
      error(Msg)
  end.

check_if_writable(PltFile) ->
  case filelib:is_regular(PltFile) of
    true -> is_writable_file_or_dir(PltFile);
    false ->
      case filelib:is_dir(PltFile) of
	true -> false;
	false ->
	  DirName = filename:dirname(PltFile),
	  filelib:is_dir(DirName) andalso is_writable_file_or_dir(DirName)
      end
  end.

is_writable_file_or_dir(PltFile) ->
  case file:read_file_info(PltFile) of
    {ok, #file_info{access = A}} ->
      (A =:= write) orelse (A =:= read_write);
    {error, _} ->
      false
  end.

%%--------------------------------------------------------------------

clean_plt(PltFile, RemovedMods) ->
  %% Clean the plt from the removed modules.
  Plt = dialyzer_plt:from_file(PltFile),
  sets:fold(fun(M, AccPlt) -> dialyzer_plt:delete_module(AccPlt, M) end,
	    Plt, RemovedMods).

expand_dependent_modules(Md5, DiffMd5, ModDeps) ->
  ChangedMods = sets:from_list([M || {differ, M} <- DiffMd5]),
  RemovedMods = sets:from_list([M || {removed, M} <- DiffMd5]),
  BigSet = sets:union(ChangedMods, RemovedMods),
  BigList = sets:to_list(BigSet),
  ExpandedSet = expand_dependent_modules_1(BigList, BigSet, ModDeps),
  NewModDeps = dialyzer_callgraph:strip_module_deps(ModDeps, BigSet),
  AnalyzeMods = sets:subtract(ExpandedSet, RemovedMods),  
  FilterFun = fun(File) ->
		  Mod = list_to_atom(filename:basename(File, ".beam")),
		  sets:is_element(Mod, AnalyzeMods)
	      end,
  {[F || {F, _} <- Md5, FilterFun(F)], RemovedMods, NewModDeps}.

expand_dependent_modules_1([Mod|Mods], Included, ModDeps) ->
  case dict:find(Mod, ModDeps) of
    {ok, Deps} ->
      NewDeps = sets:subtract(sets:from_list(Deps), Included), 
      case sets:size(NewDeps) =:= 0 of
	true -> expand_dependent_modules_1(Mods, Included, ModDeps);
	false -> 
	  NewIncluded = sets:union(Included, NewDeps),
	  expand_dependent_modules_1(sets:to_list(NewDeps) ++ Mods, 
				     NewIncluded, ModDeps)
      end;
    error ->
      expand_dependent_modules_1(Mods, Included, ModDeps)
  end;
expand_dependent_modules_1([], Included, _ModDeps) ->
  Included.

-define(MIN_FILES_FOR_NATIVE_COMPILE, 20).

-spec hipe_compile([file:filename()], #options{}) -> 'ok'.

hipe_compile(Files, #options{erlang_mode = ErlangMode} = Options) ->
  NoNative = (get(dialyzer_options_native) =:= false),
  FewFiles = (length(Files) < ?MIN_FILES_FOR_NATIVE_COMPILE),
  case NoNative orelse FewFiles orelse ErlangMode of
    true -> ok;
    false ->
      case erlang:system_info(hipe_architecture) of
	undefined -> ok;
	_ ->
	  Mods = [lists, dict, gb_sets, gb_trees, ordsets, sets,
		  cerl, cerl_trees, erl_types, erl_bif_types,
		  dialyzer_analysis_callgraph, dialyzer_codeserver,
		  dialyzer_dataflow, dialyzer_dep, dialyzer_plt,
		  dialyzer_succ_typings, dialyzer_typesig],
	  report_native_comp(Options),
	  {T1, _} = statistics(wall_clock),
	  native_compile(Mods),
	  {T2, _} = statistics(wall_clock),
	  report_elapsed_time(T1, T2, Options)
      end
  end.

native_compile(Mods) ->
  case erlang:system_info(schedulers) of
    %% N when N > 1 ->
    %%   Parent = self(),
    %%   Pids = [spawn(fun () -> Parent ! {self(), hc(M)} end) || M <- Mods],
    %%   lists:foreach(fun (Pid) -> receive {Pid, Res} -> Res end end, Pids);
    _ -> % 1 ->
      lists:foreach(fun (Mod) -> hc(Mod) end, Mods)
  end.

hc(Mod) ->
  case code:ensure_loaded(Mod) of
    {module, Mod} -> ok;
    {error, sticky_directory} -> ok
  end,
  case code:is_module_native(Mod) of
    true -> ok;
    false ->
      {ok, Mod} = hipe:c(Mod),
      ok
  end.

new_state() ->
  #cl_state{}.

init_output(State0, #options{output_file = OutFile, output_format = OutFormat}) ->
  State = State0#cl_state{output_format = OutFormat},
  case OutFile =:= none of
    true ->
      State;
    false ->
      case file:open(OutFile, [write]) of
	{ok, File} ->
	  State#cl_state{output = File};
	{error, Reason} ->
	  Msg = io_lib:format("Could not open output file ~p, Reason: ~p\n",
			      [OutFile, Reason]),
	  error(State, lists:flatten(Msg))
      end
  end.

-spec maybe_close_output_file(#cl_state{}) -> 'ok'.

maybe_close_output_file(State) ->
  case State#cl_state.output of
    standard_io -> ok;
    File -> ok = file:close(File)
  end.

%% ----------------------------------------------------------------
%%
%%  Main Loop
%%

-define(LOG_CACHE_SIZE, 10).

%%-spec cl_loop(#cl_state{}) -> 
cl_loop(State) ->
  cl_loop(State, []).

cl_loop(State, LogCache) ->
  BackendPid = State#cl_state.backend_pid,
  receive
    {BackendPid, log, LogMsg} ->
      %%io:format(State#cl_state.output ,"Log: ~s\n", [LogMsg]),
      cl_loop(State, lists:sublist([LogMsg|LogCache], ?LOG_CACHE_SIZE));
    {BackendPid, warnings, Warnings} ->
      NewState = store_warnings(State, Warnings),
      cl_loop(NewState, LogCache);
    {BackendPid, unknown_behaviours, Behaviours} ->
      NewState = store_unknown_behaviours(State, Behaviours),
      cl_loop(NewState, LogCache);
    {BackendPid, done, NewPlt, _NewDocPlt} ->
      return_value(State, NewPlt);
    {BackendPid, ext_calls, ExtCalls} ->
      cl_loop(State#cl_state{external_calls = ExtCalls}, LogCache);
    {BackendPid, ext_types, ExtTypes} ->
      cl_loop(State#cl_state{external_types = ExtTypes}, LogCache);
    {BackendPid, mod_deps, ModDeps} ->
      NewState = State#cl_state{mod_deps = ModDeps},
      cl_loop(NewState, LogCache);
    {'EXIT', BackendPid, {error, Reason}} ->
      Msg = failed_anal_msg(Reason, LogCache),
      error(State, Msg);
    {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
      Msg = failed_anal_msg(io_lib:format("~P", [Reason, 12]), LogCache),
      error(State, Msg);
    _Other ->
      %% io:format("Received ~p\n", [_Other]),
      cl_loop(State, LogCache)
  end.

-spec failed_anal_msg(string(), [_]) -> string().

failed_anal_msg(Reason, LogCache) ->
  Msg = "Analysis failed with error: " ++ Reason ++ "\n",
  case LogCache =:= [] of
    true -> Msg;
    false ->
      Msg ++ "Last messages in the log cache:\n  " ++ format_log_cache(LogCache)
  end.

%%
%% formats the log cache (treating it as a string) for pretty-printing
%%
format_log_cache(LogCache) ->
  Str = lists:append(lists:reverse(LogCache)),
  string:join(string:tokens(Str, "\n"), "\n  ").

-spec store_warnings(#cl_state{}, [dial_warning()]) -> #cl_state{}.

store_warnings(#cl_state{stored_warnings = StoredWarnings} = St, Warnings) ->
  St#cl_state{stored_warnings = StoredWarnings ++ Warnings}.

-spec store_unknown_behaviours(#cl_state{}, [dialyzer_behaviours:behaviour()]) -> #cl_state{}.

store_unknown_behaviours(#cl_state{unknown_behaviours = Behs} = St, Beh) ->
  St#cl_state{unknown_behaviours = Beh ++ Behs}.

-spec error(string()) -> no_return().

error(Msg) ->
  throw({dialyzer_error, Msg}).

-spec error(#cl_state{}, string()) -> no_return().

error(State, Msg) ->
  case State#cl_state.output of
    standard_io -> ok;
    Outfile -> io:format(Outfile, "\n~s\n", [Msg])
  end,
  maybe_close_output_file(State),
  throw({dialyzer_error, Msg}).

return_value(State = #cl_state{erlang_mode = ErlangMode,
			       mod_deps = ModDeps,
			       output_plt = OutputPlt,
			       plt_info = PltInfo,
			       stored_warnings = StoredWarnings},
	     Plt) ->
  case OutputPlt =:= none of
    true -> ok;
    false -> dialyzer_plt:to_file(OutputPlt, Plt, ModDeps, PltInfo)
  end,
  RetValue =
    case StoredWarnings =:= [] of
      true -> ?RET_NOTHING_SUSPICIOUS;
      false -> ?RET_DISCREPANCIES
    end,
  case ErlangMode of
    false ->
      print_warnings(State),
      print_ext_calls(State),
      print_ext_types(State),
      print_unknown_behaviours(State),
      maybe_close_output_file(State),
      {RetValue, []};
    true -> 
      {RetValue, process_warnings(StoredWarnings)}
  end.

print_ext_calls(#cl_state{report_mode = quiet}) ->
  ok;
print_ext_calls(#cl_state{output = Output,
			  external_calls = Calls,
			  stored_warnings = Warnings,
			  output_format = Format}) ->
  case Calls =:= [] of
    true -> ok;
    false ->
      case Warnings =:= [] of
	true -> io:nl(Output); %% Need to do a newline first
	false -> ok
      end,
      case Format of
	formatted ->
	  io:put_chars(Output, "Unknown functions:\n"),
	  do_print_ext_calls(Output, Calls, "  ");
	raw ->
	  io:put_chars(Output, "%% Unknown functions:\n"),
	  do_print_ext_calls(Output, Calls, "%%  ")
      end
  end.

do_print_ext_calls(Output, [{M,F,A}|T], Before) ->
  io:format(Output, "~s~p:~p/~p\n", [Before,M,F,A]),
  do_print_ext_calls(Output, T, Before);
do_print_ext_calls(_, [], _) ->
  ok.

print_ext_types(#cl_state{report_mode = quiet}) ->
  ok;
print_ext_types(#cl_state{output = Output,
                          external_calls = Calls,
                          external_types = Types,
                          stored_warnings = Warnings,
                          output_format = Format}) ->
  case Types =:= [] of
    true -> ok;
    false ->
      case Warnings =:= [] andalso Calls =:= [] of
        true -> io:nl(Output); %% Need to do a newline first
        false -> ok
      end,
      case Format of
        formatted ->
          io:put_chars(Output, "Unknown types:\n"),
          do_print_ext_types(Output, Types, "  ");
        raw ->
          io:put_chars(Output, "%% Unknown types:\n"),
          do_print_ext_types(Output, Types, "%%  ")
      end
  end.

do_print_ext_types(Output, [{M,F,A}|T], Before) ->
  io:format(Output, "~s~p:~p/~p\n", [Before,M,F,A]),
  do_print_ext_types(Output, T, Before);
do_print_ext_types(_, [], _) ->
  ok.

%%print_unknown_behaviours(#cl_state{report_mode = quiet}) ->
%%  ok;
print_unknown_behaviours(#cl_state{output = Output,
				   external_calls = Calls,
				   external_types = Types,
				   stored_warnings = Warnings,
				   unknown_behaviours = DupBehaviours,
				   legal_warnings = LegalWarnings,
				   output_format = Format}) ->
  case ordsets:is_element(?WARN_BEHAVIOUR, LegalWarnings)
    andalso DupBehaviours =/= [] of
    false -> ok;
    true ->
      Behaviours = lists:usort(DupBehaviours),
      case Warnings =:= [] andalso Calls =:= [] andalso Types =:= [] of
	true -> io:nl(Output); %% Need to do a newline first
	false -> ok
      end,
      case Format of
	formatted ->
	  io:put_chars(Output, "Unknown behaviours (behaviour_info(callbacks)"
		       " does not return any specs):\n"),
	  do_print_unknown_behaviours(Output, Behaviours, "  ");
	raw ->
	  io:put_chars(Output, "%% Unknown behaviours:\n"),
	  do_print_unknown_behaviours(Output, Behaviours, "%%  ")
      end
  end.

do_print_unknown_behaviours(Output, [B|T], Before) ->
  io:format(Output, "~s~p\n", [Before,B]),
  do_print_unknown_behaviours(Output, T, Before);
do_print_unknown_behaviours(_, [], _) ->
  ok.

print_warnings(#cl_state{stored_warnings = []}) ->
  ok;
print_warnings(#cl_state{output = Output,
			 output_format = Format,
			 stored_warnings = Warnings}) ->
  PrWarnings = process_warnings(Warnings),
  case PrWarnings of
    [] -> ok;
    [_|_] ->
      S = case Format of
	    formatted ->
	      [dialyzer:format_warning(W) || W <- PrWarnings];
	    raw ->
	      [io_lib:format("~p. \n", [W]) || W <- PrWarnings]
	  end,
      io:format(Output, "\n~s", [S])
  end.

-spec process_warnings([dial_warning()]) -> [dial_warning()].
  
process_warnings(Warnings) ->
  Warnings1 = lists:keysort(2, Warnings), %% Sort on file/line
  remove_duplicate_warnings(Warnings1, []).

remove_duplicate_warnings([Duplicate, Duplicate|Left], Acc) ->
  remove_duplicate_warnings([Duplicate|Left], Acc);
remove_duplicate_warnings([NotDuplicate|Left], Acc) ->
  remove_duplicate_warnings(Left, [NotDuplicate|Acc]);
remove_duplicate_warnings([], Acc) ->
  lists:reverse(Acc).

get_files_from_opts(Options) ->
  From = Options#options.from,
  Files1 = add_files(Options#options.files, From),
  Files2 = add_files_rec(Options#options.files_rec, From),
  ordsets:union(Files1, Files2).

add_files_rec(Files, From) ->
  add_files(Files, From, true).

add_files(Files, From) ->
  add_files(Files, From, false).

add_files(Files, From, Rec) ->
  Files1 = [filename:absname(F) || F <- Files],
  Files2 = ordsets:from_list(Files1), 
  Dirs = ordsets:filter(fun(X) -> filelib:is_dir(X) end, Files2),
  Files3 = ordsets:subtract(Files2, Dirs),
  Extension = case From of
		byte_code -> ".beam";
		src_code -> ".erl"
	      end,
  Fun = add_file_fun(Extension),
  lists:foldl(fun(Dir, Acc) ->
		  filelib:fold_files(Dir, Extension, Rec, Fun, Acc)
	      end, Files3, Dirs).

add_file_fun(Extension) ->
  fun(File, AccFiles) ->
      case filename:extension(File) =:= Extension of
	true ->
	  AbsName = filename:absname(File),
	  ordsets:add_element(AbsName, AccFiles);
	false -> AccFiles
      end
  end.

-spec start_analysis(#cl_state{}, #analysis{}) -> #cl_state{}.

start_analysis(State, Analysis) ->
  Self = self(),
  LegalWarnings = State#cl_state.legal_warnings,
  Fun = fun() -> 
	    dialyzer_analysis_callgraph:start(Self, LegalWarnings, Analysis)
	end,
  BackendPid = spawn_link(Fun),
  State#cl_state{backend_pid = BackendPid}.

