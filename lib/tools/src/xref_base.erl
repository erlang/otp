%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2017. All Rights Reserved.
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

-module(xref_base).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([new/0, new/1, delete/1,
	 add_directory/2, add_directory/3,
	 add_module/2, add_module/3,
	 add_application/2, add_application/3,
	 replace_module/3, replace_module/4,
	 replace_application/3, replace_application/4,
	 remove_module/2, remove_application/2, remove_release/2,
	 add_release/2, add_release/3,
	 get_library_path/1, set_library_path/2, set_library_path/3,
	 set_up/1, set_up/2,
	 q/2, q/3, info/1, info/2, info/3, update/1, update/2,
	 forget/1, forget/2, variables/1, variables/2,
	 analyze/2, analyze/3, analysis/1,
	 get_default/2, set_default/3,
	 get_default/1, set_default/2]).

-export([format_error/1]).

%% The following functions are exported for testing purposes only:
-export([do_add_module/4, do_add_application/2, do_add_release/2,
	 do_remove_module/2]).

-import(lists,
	[filter/2, flatten/1, foldl/3, foreach/2, keysearch/3, map/2,
         mapfoldl/3, member/2, reverse/1, sort/1, usort/1]).

-import(sofs,
	[constant_function/2, converse/1, difference/2, domain/1,
         empty_set/0, family/1, family_difference/2, intersection/2,
         family_projection/2, family_to_relation/1, family_union/1,
         family_union/2, from_sets/1, from_term/1, a_function/1,
         image/2, family_intersection/2, inverse/1, is_empty_set/1,
         multiple_relative_product/2, no_elements/1,
         partition_family/2, projection/2, range/1, relation/1,
         relation_to_family/1, relative_product1/2, restriction/2,
         restriction/3, set/1, specification/2, substitution/2,
         to_external/1, union/1, union/2, union_of_family/1]).

-include("xref.hrl").

-define(Suffix, ".beam").

%-define(debug, true).

-ifdef(debug).
-define(FORMAT(P, A), io:format(P, A)).
-else.
-define(FORMAT(P, A), ok).
-endif.

%%
%%  Exported functions
%%

new() ->
    new([]).

%% -> {ok, InitialState}
new(Options) ->
    Modes = [functions,modules,function,module],
    case xref_utils:options(Options, [{xref_mode,Modes}]) of
	{[[function]], []} ->
	    {ok, #xref{mode = functions}};
	{[[module]], []} ->
	    {ok, #xref{mode = modules}};
	{[[OM]], []} ->
	    {ok, #xref{mode = OM}};
	_ ->
	    error({invalid_options, Options})
    end.

%% -> ok
%% Need not be called by the server.
delete(State) when State#xref.variables =:= not_set_up ->
    ok;
delete(State) ->
    Fun = fun({X, _}) ->
		  case catch digraph:info(X) of
		      Info when is_list(Info) ->
			  true = digraph:delete(X);
		      _Else ->
			  ok
		  end
	     end,
    foreach(Fun, dict:to_list(State#xref.variables)),
    ok.

add_directory(State, Dir) ->
    add_directory(State, Dir, []).

%% -> {ok, Modules, NewState} | Error
add_directory(State, Dir, Options) ->
    ValOptions = option_values([builtins, recurse, verbose, warnings], State),
    case xref_utils:options(Options, ValOptions) of
	{[[OB], [OR], [OV], [OW]], []} ->
	    catch do_add_directory(Dir, [], OB, OR, OV, OW, State);
	_ ->
	    error({invalid_options, Options})
    end.

add_module(State, File) ->
    add_module(State, File, []).

%% -> {ok, Module, NewState} | Error
add_module(State, File, Options) ->
    ValOptions = option_values([builtins, verbose, warnings], State),
    case xref_utils:options(Options, ValOptions) of
	{[[OB], [OV], [OW]], []} ->
	    case catch do_add_a_module(File, [], OB, OV, OW, State) of
		{ok, [Module], NewState} ->
		    {ok, Module, NewState};
		{ok, [], _NewState} ->
		    error({no_debug_info, File});
		Error ->
		    Error
	    end;
	_ ->
	    error({invalid_options, Options})
    end.

add_application(State, AppDir) ->
    add_application(State, AppDir, []).

%% -> {ok, AppName, NewState} | Error
add_application(State, AppDir, Options) ->
    OptVals = option_values([builtins, verbose, warnings], State),
    ValidOptions = [{name, ["", fun check_name/1]} | OptVals],
    case xref_utils:options(Options, ValidOptions) of
	{[ApplName, [OB], [OV], [OW]], []} ->
	    catch do_add_application(AppDir, [], ApplName, OB, OV, OW, State);
	_ ->
	    error({invalid_options, Options})
    end.

replace_module(State, Module, File) ->
    replace_module(State, Module, File, []).

%% -> {ok, Module, NewState} | Error
replace_module(State, Module, File, Options) ->
    ValidOptions = option_values([verbose, warnings], State),
    case xref_utils:options(Options, ValidOptions) of
	{[[OV], [OW]], []} ->
	    catch do_replace_module(Module, File, OV, OW, State);
	_ ->
	    error({invalid_options, Options})
    end.

replace_application(State, Appl, Dir) ->
    replace_application(State, Appl, Dir, []).

%% -> {ok, AppName, NewState} | Error
replace_application(State, Appl, Dir, Options) ->
    ValidOptions = option_values([builtins, verbose, warnings], State),
    case xref_utils:options(Options, ValidOptions) of
	{[[OB], [OV], [OW]], []} ->
	    catch do_replace_application(Appl, Dir, OB, OV, OW, State);
	_ ->
	    error({invalid_options, Options})
    end.

%% -> {ok, NewState} | Error
remove_module(State, Mod) when is_atom(Mod) ->
    remove_module(State, [Mod]);
remove_module(State, [Mod | Mods]) ->
    case catch do_remove_module(State, Mod) of
	{ok, _OldXMod, NewState} ->
	    remove_module(NewState, Mods);
	Error ->
	    Error
    end;
remove_module(State, []) ->
    {ok, State}.

%% -> {ok, NewState} | Error
remove_application(State, Appl) when is_atom(Appl) ->
    remove_application(State, [Appl]);
remove_application(State, [Appl | Appls]) ->
    case catch do_remove_application(State, Appl) of
	{ok, _OldXApp, NewState} ->
	    remove_application(NewState, Appls);
	Error ->
	    Error
    end;
remove_application(State, []) ->
    {ok, State}.

%% -> {ok, NewState} | Error
remove_release(State, Rel) when is_atom(Rel) ->
    remove_release(State, [Rel]);
remove_release(State, [Rel | Rels]) ->
    case catch do_remove_release(State, Rel) of
	{ok, _OldXRel, NewState} ->
	    remove_release(NewState, Rels);
	Error ->
	    Error
    end;
remove_release(State, []) ->
    {ok, State}.

add_release(State, RelDir) ->
    add_release(State, RelDir, []).

%% -> {ok, ReleaseName, NewState} | Error
add_release(State, RelDir, Options) ->
    ValidOptions0 = option_values([builtins, verbose, warnings], State),
    ValidOptions = [{name, ["", fun check_name/1]} | ValidOptions0],
    case xref_utils:options(Options, ValidOptions) of
	{[RelName, [OB], [OV], [OW]], []} ->
	    catch do_add_release(RelDir, RelName, OB, OV, OW, State);
	_ ->
	    error({invalid_options, Options})
    end.

get_library_path(State) ->
    {ok, State#xref.library_path}.

set_library_path(State, Path) ->
    set_library_path(State, Path, []).

%% -> {ok, NewState} | Error
set_library_path(State, code_path, _Options) ->
    S1 = State#xref{library_path = code_path, libraries = dict:new()},
    {ok, take_down(S1)};
set_library_path(State, Path, Options) ->
    case xref_utils:is_path(Path) of
	true ->
	    ValidOptions = option_values([verbose], State),
	    case xref_utils:options(Options, ValidOptions) of
		{[[OV]], []} ->
		    do_add_libraries(Path, OV, State);
		_ ->
		    error({invalid_options, Options})
	    end;
	false ->
	    error({invalid_path, Path})
    end.

set_up(State) ->
    set_up(State, []).

%% -> {ok, NewState} | Error
set_up(State, Options) ->
    ValidOptions = option_values([verbose], State),
    case xref_utils:options(Options, ValidOptions) of
	{[[Verbose]], []} ->
	    do_set_up(State, Verbose);
	_ ->
	    error({invalid_options, Options})
    end.

q(S, Q) ->
    q(S, Q, []).

%% -> {{ok, Answer}, NewState} | {Error, NewState}
q(S, Q, Options) when is_atom(Q) ->
    q(S, atom_to_list(Q), Options);
q(S, Q, Options) ->
    case xref_utils:is_string(Q, 1) of
	true ->
	    case set_up(S, Options) of
		{ok, S1} ->
		    case xref_compiler:compile(Q, S1#xref.variables) of
			{NewT, Ans} ->
			    {{ok, Ans}, S1#xref{variables = NewT}};
			Error ->
			    {Error, S1}
		    end;
		Error ->
		    {Error, S}
	    end;
	false ->
	    {error({invalid_query, Q}), S}
    end.

%% -> InfoList
info(State) ->
    D0 = sort(dict:to_list(State#xref.modules)),
    D = map(fun({_M, XMod}) -> XMod end, D0),
    NoApps = length(dict:to_list(State#xref.applications)),
    NoRels = length(dict:to_list(State#xref.releases)),
    No = no_sum(State, D),
    [{library_path, State#xref.library_path}, {mode, State#xref.mode},
     {no_releases, NoRels}, {no_applications, NoApps}] ++ No.

info(State, What) ->
    do_info(State, What).

%% -> [{what(), InfoList}]
info(State, What, Qual) ->
    catch do_info(State, What, Qual).

update(State) ->
    update(State, []).

%% -> {ok, NewState, Modules} | Error
update(State, Options) ->
    ValidOptions = option_values([verbose, warnings], State),
    case xref_utils:options(Options, ValidOptions) of
	{[[OV],[OW]], []} ->
	    catch do_update(OV, OW, State);
	_ ->
	    error({invalid_options, Options})
    end.

%% -> {ok, NewState}
forget(State) ->
    {U, _P} = do_variables(State),
    {ok, foldl(fun(V, S) -> {ok, NS} = forget(S, V), NS end, State, U)}.

%% -> {ok, NewState} | Error
forget(State, Variable) when State#xref.variables =:= not_set_up ->
    error({not_user_variable, Variable});
forget(State, Variable) when is_atom(Variable) ->
    forget(State, [Variable]);
forget(State, Variables) ->
    Vars = State#xref.variables,
    do_forget(Variables, Vars, Variables, State).

variables(State) ->
    variables(State, [user]).

%% -> {{ok, Answer}, NewState} | {Error, NewState}
%% Answer = [{vartype(), [VariableName]}]
variables(State, Options) ->
    ValidOptions = option_values([verbose], State),
    case xref_utils:options(Options, [user, predefined | ValidOptions]) of
	{[User,Predef,[OV]],[]} ->
	    case do_set_up(State, OV) of
		{ok, NewState} ->
		    {U, P} = do_variables(NewState),
		    R1 = if User -> [{user, U}]; true -> [] end,
		    R = if
			    Predef -> [{predefined,P} | R1];
			    true -> R1
			end,
		    {{ok, R}, NewState};
		Error ->
		    {Error, State}
	    end;
	_ ->
	    {error({invalid_options, Options}), State}
    end.

analyze(State, Analysis) ->
    analyze(State, Analysis, []).

%% -> {{ok, Answer}, NewState} | {Error, NewState}
analyze(State, Analysis, Options) ->
    case analysis(Analysis, State#xref.mode) of
	P when is_list(P) ->
	    q(State, P, Options);
	error ->
	    R = case analysis(Analysis, functions) of
		    error -> unknown_analysis;
		    P when is_list(P) -> unavailable_analysis
		end,
	    Error = error({R, Analysis}),
	    {Error, State}
    end.

analysis(Analysis) ->
    analysis(Analysis, functions).

%% -> string() | Error
analysis(undefined_function_calls, functions) ->
    "(XC - UC) || (XU - X - B)";
analysis(undefined_functions, modules) ->
    %% "XU * (L + U)" is equivalent, but the following works when L is
    %% not available.
    "XU - X - B";
analysis(undefined_functions, functions) ->
    %% "XU * ((L + U) - range UC)" is equivalent.
    "XU - range UC - X - B";
analysis(locals_not_used, functions) ->
    %% The Inter Call Graph is used to get local functions that are not
    %% used (indirectly) from any export: "(domain EE + range EE) * L".
    %% But then we only get locals that make some calls, so we add
    %% locals that are not used at all: "L * (UU + XU - LU)".
    %% We also need to exclude functions with the -on_load() attribute:
    %% (L - OL) is used rather than just L.
    "(L - OL) * ((UU + XU - LU) + domain EE + range EE)";
analysis(exports_not_used, _) ->
    %% Local calls are not considered here. "X * UU" would do otherwise.
    "X - XU";
analysis({call, F}, functions) ->
    make_query("range (E | ~tw : Fun)", [F]);
analysis({use, F}, functions) ->
    make_query("domain (E || ~tw : Fun)", [F]);
analysis({module_call, M}, _) ->
    make_query("range (ME | ~tw : Mod)", [M]);
analysis({module_use, M}, _) ->
    make_query("domain (ME || ~tw : Mod)", [M]);
analysis({application_call, A}, _) ->
    make_query("range (AE | ~tw : App)", [A]);
analysis({application_use, A}, _) ->
    make_query("domain (AE || ~tw : App)", [A]);
analysis({release_call, R}, _) ->
    make_query("range (RE | ~tw : Rel)", [R]);
analysis({release_use, R}, _) ->
    make_query("domain (RE || ~tw : Rel)", [R]);
analysis(deprecated_function_calls, functions) ->
    "XC || DF";
analysis({deprecated_function_calls,Flag}, functions) ->
    case deprecated_flag(Flag) of
        undefined -> error;
        I -> make_query("XC || DF_~w", [I])
    end;
analysis(deprecated_functions, _) ->
    "XU * DF";
analysis({deprecated_functions,Flag}, _) ->
    case deprecated_flag(Flag) of
        undefined -> error;
        I -> make_query("XU * DF_~w", [I])
    end;
analysis(_, _) ->
    error.

%% -> {ok, OldValue, NewState} | Error
set_default(State, Option, Value) ->
    case get_default(State, Option) of
	{ok, OldValue} ->
	    Values = option_values([Option], State),
	    case xref_utils:options([{Option,Value}], Values) of
		{_, []} ->
		    NewState = set_def(Option, Value, State),
		    {ok, OldValue, NewState};
		{_, Unknown} ->
		    error({invalid_options, Unknown})
	    end;
	Error ->
	    Error
    end.

%% -> {ok, Value} | Error
get_default(State, Option) ->
    case catch current_default(State, Option) of
	{'EXIT', _} ->
	    error({invalid_options, [Option]});
	Value ->
	    {ok, Value}
    end.

%% -> [{Option, Value}]
get_default(State) ->
    Fun = fun(O) -> V = current_default(State, O), {O, V} end,
    map(Fun, [builtins, recurse, verbose, warnings]).

%% -> {ok, NewState} -> Error
set_default(State, Options) ->
    Opts = [builtins, recurse, verbose, warnings],
    ValidOptions = option_values(Opts, State),
    case xref_utils:options(Options, ValidOptions) of
	{Values = [[_], [_], [_], [_]], []} ->
	    {ok, set_defaults(Opts, Values, State)};
	_ ->
	    error({invalid_options, Options})
    end.

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error({invalid_options, Options}) ->
    io_lib:format("Unknown option(s) or invalid option value(s): ~tp~n",
		  [Options]);
format_error({invalid_filename, Term}) ->
    io_lib:format("A file name (a string) was expected: ~tp~n", [Term]);
format_error({no_debug_info, FileName}) ->
    io_lib:format("The BEAM file ~tp has no debug info~n", [FileName]);
format_error({invalid_path, Term}) ->
    io_lib:format("A path (a list of strings) was expected: ~tp~n", [Term]);
format_error({invalid_query, Term}) ->
    io_lib:format("A query (a string or an atom) was expected: ~tp~n", [Term]);
format_error({not_user_variable, Variable}) ->
    io_lib:format("~tp is not a user variable~n", [Variable]);
format_error({unknown_analysis, Term}) ->
    io_lib:format("~tp is not a predefined analysis~n", [Term]);
format_error({module_mismatch, Module, ReadModule}) ->
    io_lib:format("Name of read module ~tp does not match analyzed module ~tp~n",
		  [ReadModule, Module]);
format_error({release_clash, {Release, Dir, OldDir}}) ->
    io_lib:format("The release ~tp read from ~tp clashes with release "
		  "already read from ~tp~n", [Release, Dir, OldDir]);
format_error({application_clash, {Application, Dir, OldDir}}) ->
    io_lib:format("The application ~tp read from ~tp clashes with application "
		  "already read from ~tp~n", [Application, Dir, OldDir]);
format_error({module_clash, {Module, Dir, OldDir}}) ->
    io_lib:format("The module ~tp read from ~tp clashes with module "
		  "already read from ~tp~n", [Module, Dir, OldDir]);
format_error({no_such_release, Name}) ->
    io_lib:format("There is no analyzed release ~tp~n", [Name]);
format_error({no_such_application, Name}) ->
    io_lib:format("There is no analyzed application ~tp~n", [Name]);
format_error({no_such_module, Name}) ->
    io_lib:format("There is no analyzed module ~tp~n", [Name]);
format_error({no_such_info, Term}) ->
    io_lib:format("~tp is not one of 'modules', 'applications', "
		  "'releases' and 'libraries'~n", [Term]);
format_error(E) ->
    io_lib:format("~tp~n", [E]).

%%
%%  Local functions
%%

check_name([N]) when is_atom(N) -> true;
check_name(_) -> false.

do_update(OV, OW, State) ->
    Changed = updated_modules(State),
    Fun = fun({Mod,File}, S) ->
		  {ok, _M, NS} = do_replace_module(Mod, File, OV, OW, S),
		  NS
	  end,
    NewState = foldl(Fun, State, Changed),
    {ok, NewState, to_external(domain(a_function(Changed)))}.

%% -> [{Module, File}]
updated_modules(State) ->
    Fun = fun({M,XMod}, L) ->
		  RTime = XMod#xref_mod.mtime,
		  File = module_file(XMod),
		  case xref_utils:file_info(File) of
		      {ok, {_, file, readable, MTime}} when MTime =/= RTime ->
			  [{M,File} | L];
		      _Else ->
			  L
		  end
	  end,
    foldl(Fun, [], dict:to_list(State#xref.modules)).

do_forget([Variable | Variables], Vars, Vs, State) ->
    case dict:find(Variable, Vars) of
	{ok, #xref_var{vtype = user}} ->
	    do_forget(Variables, Vars, Vs, State);
	_ ->
	    error({not_user_variable, Variable})
    end;
do_forget([], Vars, Vs, State) ->
    Fun = fun(V, VT) ->
		  {ok, #xref_var{value = Value}} = dict:find(V, VT),
		  VT1 = xref_compiler:update_graph_counter(Value, -1, VT),
		  dict:erase(V, VT1)
	  end,
    NewVars = foldl(Fun, Vars, Vs),
    NewState = State#xref{variables = NewVars},
    {ok, NewState}.

%% -> {ok, Module, State} | throw(Error)
do_replace_module(Module, File, OV, OW, State) ->
    {ok, OldXMod, State1} = do_remove_module(State, Module),
    OldApp = OldXMod#xref_mod.app_name,
    OB = OldXMod#xref_mod.builtins,
    case do_add_a_module(File, OldApp, OB, OV, OW, State1) of
	{ok, [Module], NewState} ->
	    {ok, Module, NewState};
	{ok, [ReadModule], _State} ->
	    throw_error({module_mismatch, Module, ReadModule});
	{ok, [], _NewState} ->
	    throw_error({no_debug_info, File})
    end.

do_replace_application(Appl, Dir, OB, OV, OW, State) ->
    {ok, OldXApp, State1} = do_remove_application(State, Appl),
    Rel = OldXApp#xref_app.rel_name,
    N = OldXApp#xref_app.name,
    %% The application name is kept; the name of Dir is not used
    %% as source for a "new" application name.
    do_add_application(Dir, Rel, [N], OB, OV, OW, State1).

%% -> {ok, ReleaseName, NewState} | throw(Error)
do_add_release(Dir, RelName, OB, OV, OW, State) ->
    ok = is_filename(Dir),
    case xref_utils:release_directory(Dir, true, "ebin") of
	{ok, ReleaseDirName, ApplDir, Dirs} ->
	    ApplDirs = xref_utils:select_last_application_version(Dirs),
	    Release = case RelName of
			  [[]] -> ReleaseDirName;
			  [Name] -> Name
		      end,
	    XRel = #xref_rel{name = Release, dir = ApplDir},
	    NewState = do_add_release(State, XRel),
	    add_rel_appls(ApplDirs, [Release], OB, OV, OW, NewState);
	Error ->
	    throw(Error)
    end.

do_add_release(S, XRel) ->
    Release = XRel#xref_rel.name,
    case dict:find(Release, S#xref.releases) of
	{ok, OldXRel} ->
	    Dir = XRel#xref_rel.dir,
	    OldDir = OldXRel#xref_rel.dir,
	    throw_error({release_clash, {Release, Dir, OldDir}});
	error ->
	    D1 = dict:store(Release, XRel, S#xref.releases),
	    S#xref{releases = D1}
    end.

add_rel_appls([ApplDir | ApplDirs], Release, OB, OV, OW, State) ->
    {ok, _AppName,  NewState} =
	add_appldir(ApplDir, Release, [[]], OB, OV, OW, State),
    add_rel_appls(ApplDirs, Release, OB, OV, OW, NewState);
add_rel_appls([], [Release], _OB, _OV, _OW, NewState) ->
    {ok, Release, NewState}.

do_add_application(Dir0, Release, Name, OB, OV, OW, State) ->
    ok = is_filename(Dir0),
    case xref_utils:select_application_directories([Dir0], "ebin") of
	{ok, [ApplD]} ->
	    add_appldir(ApplD, Release, Name, OB, OV, OW, State);
	Error ->
	    throw(Error)
    end.

%% -> {ok, AppName, NewState} | throw(Error)
add_appldir(ApplDir, Release, Name, OB, OV, OW, OldState) ->
    {AppName0, Vsn, Dir} = ApplDir,
    AppName = case Name of
		  [[]] -> AppName0;
		  [N] -> N
	      end,
    AppInfo = #xref_app{name = AppName, rel_name = Release,
			vsn = Vsn, dir = Dir},
    State1 = do_add_application(OldState, AppInfo),
    {ok, _Modules, NewState} =
	do_add_directory(Dir, [AppName], OB, false, OV, OW, State1),
    {ok, AppName, NewState}.

%% -> State | throw(Error)
do_add_application(S, XApp) ->
    Application = XApp#xref_app.name,
    case dict:find(Application, S#xref.applications) of
	{ok, OldXApp} ->
	    Dir = XApp#xref_app.dir,
	    OldDir = OldXApp#xref_app.dir,
	    throw_error({application_clash, {Application, Dir, OldDir}});
	error ->
	    D1 = dict:store(Application, XApp, S#xref.applications),
	    S#xref{applications = D1}
    end.

%% -> {ok, Modules, NewState} | throw(Error)
do_add_directory(Dir, AppName, Bui, Rec, Ver, War, State) ->
    ok = is_filename(Dir),
    {FileNames, Errors, Jams, Unreadable} =
	xref_utils:scan_directory(Dir, Rec, [?Suffix], [".jam"]),
    warnings(War, jam, Jams),
    warnings(War, unreadable, Unreadable),
    case Errors of
	[] ->
	    do_add_modules(FileNames, AppName, Bui, Ver, War, State);
	[Error | _] ->
	    throw(Error)
    end.

do_add_modules(Files, AppName, OB, OV, OW, State0) ->
    NFiles = length(Files),
    Reader = fun(SplitName, State) ->
                     _Pid = read_module(SplitName, AppName, OB, OV, OW, State)
             end,
    N = parallelism(),
    Files1 = start_readers(Files, Reader, State0, N),
    %% Increase the number of readers towards the end to decrease the
    %% waiting time for the collecting process:
    Nx = N,
    add_mods(Files1, Reader, State0, [], NFiles, Nx).

add_mods(_, _ReaderFun, State, Modules, 0, _Nx) ->
    {ok, sort(Modules), State};
add_mods(Files, ReaderFun, State, Modules, N, Nx) ->
    {I, Nx1} = case Nx > 0 of
                   false -> {1, Nx};
                   true -> {2, Nx - 1}
               end,
    Files1 = start_readers(Files, ReaderFun, State, I),
    {ok, M, NewState} = process_module(State),
    add_mods(Files1, ReaderFun, NewState, M ++ Modules, N - 1, Nx1).

start_readers([SplitName|Files], ReaderFun, State, N) when N > 0 ->
    _Pid = ReaderFun(SplitName, State),
    start_readers(Files, ReaderFun, State, N - 1);
start_readers(Files, _ReaderFun, _State, _) ->
    Files.

parallelism() ->
    case erlang:system_info(multi_scheduling) of
        enabled -> erlang:system_info(schedulers_online);
        _ -> 1
    end.

%% -> {ok, Module, State} | throw(Error)
do_add_a_module(File, AppName, Builtins, Verbose, Warnings, State) ->
    case xref_utils:split_filename(File, ?Suffix) of
	false ->
	    throw_error({invalid_filename, File});
	Splitname ->
	    do_add_module(Splitname, AppName, Builtins, Verbose,
			  Warnings, State)
    end.

%% -> {ok, Module, State} | throw(Error)
%% Options: verbose, warnings, builtins
do_add_module(SplitName, AppName, Builtins, Verbose, Warnings, State) ->
    _Pid = read_module(SplitName, AppName, Builtins, Verbose, Warnings, State),
    process_module(State).

read_module(SplitName, AppName, Builtins, Verbose, Warnings, State) ->
    Me = self(),
    #xref{mode = Mode} = State,
    Fun =
        fun() ->
                Me ! {?MODULE,
                      read_a_module(SplitName, AppName, Builtins, Verbose,
                                    Warnings, Mode)}
        end,
    spawn_opt(Fun, [link, {min_heap_size, 1000000}, {priority, high}]).

read_a_module({Dir, BaseName}, AppName, Builtins, Verbose, Warnings, Mode) ->
    File = filename:join(Dir, BaseName),
    case abst(File, Builtins, Mode) of
	{ok, _M, no_abstract_code} when Verbose ->
	    message(Verbose, no_debug_info, [File]),
	    no;
	{ok, _M, no_abstract_code} when not Verbose ->
	    message(Warnings, no_debug_info, [File]),
	    no;
	{ok, M, Data, UnresCalls0}  ->
	    message(Verbose, done_file, [File]),
            %% Remove duplicates. Identical unresolved calls on the
            %% same line are counted as _one_ unresolved call.
            UnresCalls = usort(UnresCalls0),
            NoUnresCalls = length(UnresCalls),
            case NoUnresCalls of
                0 -> ok;
                1 -> warnings(Warnings, unresolved_summary1, [[M]]);
                N -> warnings(Warnings, unresolved_summary, [[M, N]])
            end,
            case xref_utils:file_info(File) of
                {ok, {_, _, _, Time}} ->
                    XMod = #xref_mod{name = M, app_name = AppName,
                                     dir = Dir, mtime = Time,
                                     builtins = Builtins,
                                     no_unresolved = NoUnresCalls},
                    {ok, PrepMod, Bad} =
                        prepare_module(Mode, XMod, UnresCalls, Data),
                    foreach(fun({Tag,B}) ->
                                    warnings(Warnings, Tag,
                                             [[File,B]])
                            end, Bad),
                    {ok, PrepMod};
                Error -> Error
            end;
	Error ->
	    message(Verbose, error, []),
            Error
    end.

process_module(State) ->
    receive
        {?MODULE, Reply} ->
            case Reply of
                no ->
                    {ok, [], State};
                {ok, PrepMod} ->
                    finish_module(PrepMod, State);
                Error ->
                    throw(Error)
            end
    end.

abst(File, Builtins, _Mode = functions) ->
    case beam_lib:chunks(File, [abstract_code, exports, attributes]) of
	{ok, {M,[{abstract_code,NoA},_X,_A]}} when NoA =:= no_abstract_code ->
	    {ok, M, NoA};
	{ok, {M, [{abstract_code, {abstract_v1, Forms}},
                  {exports,X0}, {attributes,A}]}} ->
	    %% R7.
	    X = xref_utils:fa_to_mfa(X0, M),
            D = deprecated(A, X, M),
	    xref_reader:module(M, Forms, Builtins, X, D);
	{ok, {M, [{abstract_code, {abstract_v2, Forms}},
                  {exports,X0}, {attributes,A}]}} ->
	    %% R8-R9B.
	    X = xref_utils:fa_to_mfa(X0, M),
            D = deprecated(A, X, M),
	    xref_reader:module(M, Forms, Builtins, X, D);
	{ok, {M, [{abstract_code, {raw_abstract_v1, Code}},
                  {exports,X0}, {attributes,A}]}} ->
	    %% R9C-
            Forms0 = epp:interpret_file_attribute(Code),
	    Forms1 = erl_expand_records:module(Forms0, []),
	    Forms = erl_internal:add_predefined_functions(Forms1),
	    X = mfa_exports(X0, A, M),
            D = deprecated(A, X, M),
	    xref_reader:module(M, Forms, Builtins, X, D);
	Error when element(1, Error) =:= error ->
	    Error
    end;
abst(File, Builtins, _Mode = modules) ->
    case beam_lib:chunks(File, [exports, imports, attributes]) of
	{ok, {Mod, [{exports,X0}, {imports,I0}, {attributes,At}]}} ->
	    X1 = mfa_exports(X0, At, Mod),
	    X = filter(fun(MFA) -> not (predef_fun())(MFA) end, X1),
            D = deprecated(At, X, Mod),
	    I = case Builtins of
		    true ->
			I0;
		    false ->
			Fun = fun({M,F,A}) ->
				      not xref_utils:is_builtin(M, F, A)
			      end,
			filter(Fun, I0)
		end,
	    {ok, Mod, {X, I, D}, []};
	Error when element(1, Error) =:= error ->
	    Error
    end.

mfa_exports(X0, Attributes, M) ->
    %% Adjust arities for abstract modules.
    X1 = case xref_utils:is_abstract_module(Attributes) of
             true ->
                 [{F,adjust_arity(F,A)} || {F,A} <- X0];
             false ->
                 X0
         end,
    xref_utils:fa_to_mfa(X1, M).

adjust_arity(F, A) ->
    case xref_utils:is_static_function(F, A) of
        true -> A;
        false -> A - 1
    end.

deprecated(A, X, M) ->
    DF = {[],[],[],[]},
    case keysearch(deprecated, 1, A) of
        {value, {deprecated, D0}} ->
            depr(D0, M, DF, X, []);
        false ->
            {DF,[]}
    end.

depr([D | Depr], M, DF, X, Bad) ->
    case depr_cat(D, M, X) of
        {I,Dt} ->
            NDF = setelement(I, DF, Dt ++ element(I, DF)),
            depr(Depr, M, NDF, X, Bad);
        undefined ->
            depr(Depr, M, DF, X, [D | Bad])
    end;
depr([], _M, DF, _X, Bad) ->
    {DF, reverse(Bad)}.

depr_cat({F, A, Flg}, M, X) ->
    case deprecated_flag(Flg) of
        undefined -> undefined;
        I -> depr_fa(F, A, X, M, I)
    end;
depr_cat({F, A}, M, X) ->
    depr_fa(F, A, X, M, 4);
depr_cat(module, M, X) ->
    depr_fa('_', '_', X, M, 4);
depr_cat(_D, _M, _X) ->
    undefined.

depr_fa('_', '_', X, _M, I) ->
    {I, X};
depr_fa(F, '_', X, _M, I) when is_atom(F) ->
    {I, filter(fun({_,F1,_}) -> F1 =:= F end, X)};
depr_fa(F, A, _X, M, I) when is_atom(F), is_integer(A), A >= 0 ->
    {I, [{M,F,A}]};
depr_fa(_F, _A, _X, _M, _I) ->
    undefined.

%% deprecated_flag(Flag) -> integer() | undefined
%% Maps symbolic flags for deprecated functions to integers.

%deprecated_flag(1) -> 1;
%deprecated_flag(2) -> 2;
%deprecated_flag(3) -> 3;
deprecated_flag(next_version) -> 1;
deprecated_flag(next_major_release) -> 2;
deprecated_flag(eventually) -> 3;
deprecated_flag(_) -> undefined.

%% -> {ok, Module, Bad, State} | throw(Error)
%% Assumes:
%% L U X is a subset of dom DefAt
%% dom CallAt = LC U XC
%% Attrs is collected from the attribute 'xref' (experimental).
do_add_module(S, XMod, Unres, Data) ->
    #xref{mode = Mode} = S,
    Mode = S#xref.mode,
    {ok, PrepMod, Bad} = prepare_module(Mode, XMod, Unres, Data),
    {ok, Ms, NS} = finish_module(PrepMod, S),
    {ok, Ms, Bad, NS}.

prepare_module(_Mode = functions, XMod, Unres0, Data) ->
    {DefAt0, LPreCAt0, XPreCAt0, LC0, XC0, X0, Attrs, Depr, OL0} = Data,
    %% Bad is a list of bad values of 'xref' attributes.
    {ALC0,AXC0,Bad0} = Attrs,
    FT = [tspec(func)],
    FET = [tspec(fun_edge)],
    PCA = [tspec(pre_call_at)],

    XPreCAt1 = xref_utils:xset(XPreCAt0, PCA),
    LPreCAt1 = xref_utils:xset(LPreCAt0, PCA),
    DefAt = xref_utils:xset(DefAt0, [tspec(def_at)]),
    X1 = xref_utils:xset(X0, FT),
    XC1 = xref_utils:xset(XC0, FET),
    LC1 = xref_utils:xset(LC0, FET),
    AXC1 = xref_utils:xset(AXC0, PCA),
    ALC1 = xref_utils:xset(ALC0, PCA),
    UnresCalls = xref_utils:xset(Unres0, PCA),
    Unres = domain(UnresCalls),
    OL1 = xref_utils:xset(OL0, FT),

    DefinedFuns = domain(DefAt),
    {AXC, ALC, Bad1, LPreCAt2, XPreCAt2} =
	extra_edges(AXC1, ALC1, Bad0, DefinedFuns),
    Bad = map(fun(B) -> {xref_attr, B} end, Bad1),
    LPreCAt = union(LPreCAt1, LPreCAt2),
    XPreCAt = union(XPreCAt1, XPreCAt2),
    NoCalls = no_elements(LPreCAt) + no_elements(XPreCAt),
    LCallAt = relation_to_family(LPreCAt),
    XCallAt = relation_to_family(XPreCAt),
    CallAt = family_union(LCallAt, XCallAt),
    %% Local and exported functions with no definitions are removed.
    L = difference(DefinedFuns, X1),
    X = difference(DefinedFuns, L),
    XC = union(XC1, AXC),
    LC = union(LC1, ALC),

    {DF1,DF_11,DF_21,DF_31,DBad} = depr_mod(Depr, X),
    {EE, ECallAt} = inter_graph(X, L, LC, XC, CallAt),
    {ok, {functions, XMod, [DefAt,L,X,LCallAt,XCallAt,CallAt,LC,XC,EE,ECallAt,
                            OL1,DF1,DF_11,DF_21,DF_31], NoCalls, Unres},
     DBad++Bad};
prepare_module(_Mode = modules, XMod, _Unres, Data) ->
    {X0, I0, Depr} = Data,
    X1 = xref_utils:xset(X0, [tspec(func)]),
    I1 = xref_utils:xset(I0, [tspec(func)]),
    {DF1,DF_11,DF_21,DF_31,DBad} = depr_mod(Depr, X1),
    {ok, {modules, XMod, [X1,I1,DF1,DF_11,DF_21,DF_31]}, DBad}.

finish_module({functions, XMod, List, NoCalls, Unres}, S) ->
    ok  = check_module(XMod, S),
    [DefAt2,L2,X2,LCallAt2,XCallAt2,CallAt2,LC2,XC2,EE2,ECallAt2,
     OL2,DF2,DF_12,DF_22,DF_32] = pack(List),

    LU = range(LC2),

    LPredefined = predefined_funs(LU),

    M = XMod#xref_mod.name,
    MS = xref_utils:xset(M, atom),
    T = from_sets({MS,DefAt2,L2,X2,LCallAt2,XCallAt2,CallAt2,
		   LC2,XC2,LU,EE2,ECallAt2,Unres,LPredefined,OL2,
                   DF2,DF_12,DF_22,DF_32}),

    NoUnres = XMod#xref_mod.no_unresolved,
    Info = no_info(X2, L2, LC2, XC2, EE2, Unres, NoCalls, NoUnres),

    XMod1 = XMod#xref_mod{data = T, info = Info},
    S1 = S#xref{modules = dict:store(M, XMod1, S#xref.modules)},
    {ok, [M], take_down(S1)};
finish_module({modules, XMod, List}, S) ->
    ok = check_module(XMod, S),
    [X2,I2,DF2,DF_12,DF_22,DF_32] = pack(List),
    M = XMod#xref_mod.name,
    MS = xref_utils:xset(M, atom),
    T = from_sets({MS, X2, I2, DF2, DF_12, DF_22, DF_32}),
    Info = [],
    XMod1 = XMod#xref_mod{data = T, info = Info},
    S1 = S#xref{modules = dict:store(M, XMod1, S#xref.modules)},
    {ok, [M], take_down(S1)}.

check_module(XMod, State) ->
    M = XMod#xref_mod.name,
    case dict:find(M, State#xref.modules) of
	{ok, OldXMod}  ->
	    BF2 = module_file(XMod),
	    BF1 = module_file(OldXMod),
	    throw_error({module_clash, {M, BF1, BF2}});
        error ->
            ok
    end.

depr_mod({Depr,Bad0}, X) ->
    %% Bad0 are badly formed deprecated attributes.
    %% Here deprecated functions that are neither BIFs nor exported
    %% are deemed bad. do_set_up filters away BIFs if necessary.
    {DF_10,DF_20,DF_30,DF0} = Depr,
    FT = [tspec(func)],
    DF1 = xref_utils:xset(DF0, FT),
    DF_11 = xref_utils:xset(DF_10, FT),
    DF_21 = xref_utils:xset(DF_20, FT),
    DF_31 = xref_utils:xset(DF_30, FT),

    All = union(from_sets([DF1,DF_11,DF_21,DF_31])),
    Fun = {external, fun({M,F,A}) -> xref_utils:is_builtin(M, F, A) end},
    XB = union(X, specification(Fun, All)),
    DF_1 = intersection(DF_11, XB),
    DF_2 = union(intersection(DF_21, XB), DF_1),
    DF_3 = union(intersection(DF_31, XB), DF_2),
    DF = union(intersection(DF1, XB), DF_3),

    Bad1 = difference(All, XB),
    Bad2 = to_external(difference(Bad1, predefined_funs(Bad1))),
    Bad = map(fun(B) -> {depr_attr, B} end, usort(Bad2++Bad0)),
    {DF,DF_1,DF_2,DF_3,Bad}.

%% Extra edges gathered from the attribute 'xref' (experimental)
extra_edges(CAX, CAL, Bad0, F) ->
    AXC0 = domain(CAX),
    ALC0 = domain(CAL),
    AXC = restriction(AXC0, F),
    ALC = restriction(2, restriction(ALC0, F), F),
    LPreCAt2 = restriction(CAL, ALC),
    XPreCAt2 = restriction(CAX, AXC),
    Bad = Bad0 ++ to_external(difference(AXC0, AXC))
	       ++ to_external(difference(ALC0, ALC)),
    {AXC, ALC, Bad, LPreCAt2, XPreCAt2}.

no_info(X, L, LC, XC, EE, Unres, NoCalls, NoUnresCalls) ->
    NoUnres = no_elements(Unres),
    [{no_calls, {NoCalls-NoUnresCalls, NoUnresCalls}},
     {no_function_calls, {no_elements(LC), no_elements(XC)-NoUnres, NoUnres}},
     {no_functions, {no_elements(L), no_elements(X)}},
     %% Note: this is overwritten in do_set_up():
     {no_inter_function_calls, no_elements(EE)}].

%% Inter Call Graph.
%inter_graph(_X, _L, _LC, _XC, _CallAt) ->
%    {empty_set(), empty_set()};
inter_graph(X, L, LC, XC, CallAt) ->
    G = xref_utils:relation_to_graph(LC),

    Reachable0 = digraph_utils:reachable_neighbours(to_external(X), G),
    Reachable = xref_utils:xset(Reachable0, [tspec(func)]),
    % XL includes exports and locals that are not used by any exports
    % (the locals are tacitly ignored in the comments below).
    XL = union(difference(L, Reachable), X),

    % Immediate local calls between the module's own exports are qualified.
    LEs = restriction(restriction(2, LC, XL), XL),
    % External calls to the module's exports are qualified.
    XEs = restriction(XC, XL),
    Es = union(LEs, XEs),

    E1 = to_external(restriction(difference(LC, LEs), XL)),
    R0 = xref_utils:xset(reachable(E1, G, []),
			 [{tspec(func), tspec(fun_edge)}]),
    true = digraph:delete(G),

    % RL is a set of indirect local calls to exports.
    RL = restriction(R0, XL),
    % RX is a set of indirect external calls to exports.
    RX = relative_product1(R0, XC),
    R = union(RL, converse(RX)),

    EE0 = projection({external, fun({Ee2,{Ee1,_L}}) -> {Ee1,Ee2} end}, R),
    EE = union(Es, EE0),

    % The first call in each chain, {e1,l}, contributes with the line
    % number(s) l.
    SFun = {external, fun({Ee2,{Ee1,Ls}}) -> {{Ee1,Ls},{Ee1,Ee2}} end},
    ECallAt1 = relative_product1(projection(SFun, R), CallAt),
    ECallAt2 = union(ECallAt1, restriction(CallAt, Es)),
    ECallAt = family_union(relation_to_family(ECallAt2)),

    ?FORMAT("XL=~p~nXEs=~p~nLEs=~p~nE1=~p~nR0=~p~nRL=~p~nRX=~p~nR=~p~n"
	    "EE=~p~nECallAt1=~p~nECallAt2=~p~nECallAt=~p~n~n",
	    [XL, XEs, LEs, E1, R0, RL, RX, R, EE,
	     ECallAt1, ECallAt2, ECallAt]),
    {EE, ECallAt}.

%% -> set of {V2,{V1,L1}}
reachable([E = {_X, L} | Xs], G, R) ->
    Ns = digraph_utils:reachable([L], G),
    reachable(Xs, G, reach(Ns, E, R));
reachable([], _G, R) ->
    R.

reach([N | Ns], E, L) ->
    reach(Ns, E, [{N, E} | L]);
reach([], _E, L) ->
    L.

tspec(func)        -> {atom, atom, atom};
tspec(fun_edge)    -> {tspec(func), tspec(func)};
tspec(def_at)      -> {tspec(func), atom};
tspec(pre_call_at) -> {tspec(fun_edge), atom}.

%% -> {ok, OldXrefRel, NewState} | throw(Error)
do_remove_release(S, RelName) ->
    case dict:find(RelName, S#xref.releases) of
	error ->
	    throw_error({no_such_release, RelName});
	{ok, XRel} ->
	    S1 = take_down(S),
	    S2 = remove_rel(S1, RelName),
	    {ok, XRel, S2}
    end.

%% -> {ok, OldXrefApp, NewState} | throw(Error)
do_remove_application(S, AppName) ->
    case dict:find(AppName, S#xref.applications) of
	error ->
	    throw_error({no_such_application, AppName});
	{ok, XApp} ->
	    S1 = take_down(S),
	    S2 = remove_apps(S1, [AppName]),
	    {ok, XApp, S2}
    end.

%% -> {ok, OldXMod, NewState} | throw(Error)
do_remove_module(S, Module) ->
    case dict:find(Module, S#xref.modules) of
	error ->
	    throw_error({no_such_module, Module});
	{ok, XMod} ->
	    S1 = take_down(S),
	    {ok, XMod, remove_modules(S1, [Module])}
    end.

remove_rel(S, RelName) ->
    Rels = [RelName],
    Fun = fun({A,XApp}, L) when XApp#xref_app.rel_name =:= Rels ->
		  [A | L];
	     (_, L) -> L
	  end,
    Apps = foldl(Fun, [], dict:to_list(S#xref.applications)),
    S1 = remove_apps(S, Apps),
    NewReleases = remove_erase(Rels, S1#xref.releases),
    S1#xref{releases = NewReleases}.

remove_apps(S, Apps) ->
    Fun = fun({M,XMod}, L) ->
		  case XMod#xref_mod.app_name of
		      [] -> L;
		      [AppName] -> [{AppName,M} | L]
		  end
	  end,
    Ms = foldl(Fun, [], dict:to_list(S#xref.modules)),
    Modules = to_external(image(relation(Ms), set(Apps))),
    S1 = remove_modules(S, Modules),
    NewApplications = remove_erase(Apps, S1#xref.applications),
    S1#xref{applications = NewApplications}.

remove_modules(S, Modules) ->
    NewModules = remove_erase(Modules, S#xref.modules),
    S#xref{modules = NewModules}.

remove_erase([K | Ks], D) ->
    remove_erase(Ks, dict:erase(K, D));
remove_erase([], D) ->
    D.

do_add_libraries(Path, Verbose, State) ->
    message(Verbose, lib_search, []),
    {C, E} = xref_utils:list_path(Path, [?Suffix]),
    message(Verbose, done, []),
    MDs = to_external(relation_to_family(relation(C))),
    %% message(Verbose, lib_check, []),
    Reply = check_file(MDs, [], E, Path, State),
    %% message(Verbose, done, []),
    Reply.

%%check_file([{_M, [{_N, Dir, File} | _]} | MDs], L, E, Path, State) ->
%%    case beam_lib:version(filename:join(Dir, File)) of
%%	{ok, {Module, _Version}} ->
%%	    XLib = #xref_lib{name = Module, dir = Dir},
%%	    check_file(MDs, [{Module,XLib} | L], E, Path, State);
%%	Error ->
%%	    check_file(MDs, L, [Error | E], Path, State)
%%    end;
check_file([{Module, [{_N, Dir, _File} | _]} | MDs], L, E, Path, State) ->
    XLib = #xref_lib{name = Module, dir = Dir},
    check_file(MDs, [{Module,XLib} | L], E, Path, State);
check_file([], L, [], Path, State) ->
    D = dict:from_list(L),
    State1 = State#xref{library_path = Path, libraries = D},
    %% Take down everything, that's simplest.
    NewState = take_down(State1),
    {ok, NewState};
check_file([], _L, [E | _], _Path, _State) ->
    E.

%% -> {ok, NewState} | Error
%% Finding libraries may fail.
do_set_up(S, _VerboseOpt) when S#xref.variables =/= not_set_up ->
    {ok, S};
do_set_up(S, VerboseOpt) ->
    message(VerboseOpt, set_up, []),
    Reply = (catch do_set_up(S)),
    message(VerboseOpt, done, []),
    Reply.

%% If data has been supplied using add_module/9 (and that is the only
%% sanctioned way), then DefAt, L, X, LCallAt, XCallAt, CallAt, XC, LC,
%% LU and OL are  guaranteed to be functions (with all supplied
%% modules as domain (disregarding unknown modules, that is, modules
%% not supplied but hosting unknown functions)).
%% As a consequence, V and E are also functions. V is defined for unknown
%% modules also.
%% UU is also a function (thanks to sofs:family_difference/2...).
%% XU on the other hand can be a partial function (that is, not defined
%% for all modules). U is derived from XU, so U is also partial.
%% The inverse variables - LC_1, XC_1, E_1 and EE_1 - are all partial.
%% B is also partial.
do_set_up(S) when S#xref.mode =:= functions ->
    ModDictList = dict:to_list(S#xref.modules),
    [DefAt0, L, X0, LCallAt, XCallAt, CallAt, LC, XC, LU,
     EE0, ECallAt, UC, LPredefined, OL,
     Mod_DF,Mod_DF_1,Mod_DF_2,Mod_DF_3] = make_families(ModDictList, 19),

    {XC_1, XU, XPredefined} = do_set_up_1(XC),
    LC_1 = user_family(union_of_family(LC)),
    E_1 = family_union(XC_1, LC_1),
    Predefined = family_union(XPredefined, LPredefined),

    %% Add "hidden" functions to the exports.
    X1 = family_union(X0, Predefined),

    F1 = family_union(L, X1),
    V = family_union(F1, XU),
    E = family_union(LC, XC),

    M = domain(V),
    M2A = make_M2A(ModDictList),
    {A2R,A} = make_A2R(S#xref.applications),
    R = set(dict:fetch_keys(S#xref.releases)),

    %% Converting from edges of functions to edges of modules.
    VEs = union_of_family(E),
    Fun = {external, fun({{M1,_F1,_A1},{M2,_F2,_A2}}) -> {M1,M2} end},
    ME = projection(Fun, VEs),
    ME2AE = multiple_relative_product({M2A, M2A}, ME),

    AE = range(ME2AE),
    AE2RE = multiple_relative_product({A2R, A2R}, AE),
    RE = range(AE2RE),

    AM = domain(F1),
    %% Undef is the union of U0 and Lib:
    {Undef, U0, Lib, Lib_DF, Lib_DF_1, Lib_DF_2, Lib_DF_3} =
	make_libs(XU, F1, AM, S#xref.library_path, S#xref.libraries),
    {B, U} = make_builtins(U0),
    X1_B = family_union(X1, B),
    F = family_union(F1, Lib),
    DF = family_union(family_intersection(Mod_DF, X1_B), Lib_DF),
    DF_1 = family_union(family_intersection(Mod_DF_1, X1_B), Lib_DF_1),
    DF_2 = family_union(family_intersection(Mod_DF_2, X1_B), Lib_DF_2),
    DF_3 = family_union(family_intersection(Mod_DF_3, X1_B), Lib_DF_3),
    % If we have 'used' too, then there will be a set LU U XU...
    UU = family_difference(family_difference(F1, LU), XU),
    DefAt = make_defat(Undef, DefAt0),

    LM = domain(Lib),
    UM = difference(difference(domain(U), AM), LM),
    X = family_union(X1, Lib),

    %% Inter Call Graph. Calls to exported functions (library
    %% functions inclusive) as well as calls within modules. This is a
    %% way to discard calls to local functions in other modules.
    EE_conv = converse(union_of_family(EE0)),
    EE_exported = restriction(EE_conv, union_of_family(X)),
    EE_local =
      specification({external, fun({{M1,_,_},{M2,_,_}}) -> M1 =:= M2 end},
                    EE_conv),
    EE_0 = converse(union(EE_local, EE_exported)),
    EE_1 = user_family(EE_0),
    EE1 = partition_family({external, fun({{M1,_,_}, _MFA2}) -> M1 end},
                           EE_0),
    %% Make sure EE is defined for all modules:
    EE = family_union(family_difference(EE0, EE0), EE1),
    IFun =
       fun({Mod,EE_M}, XMods) ->
               IMFun =
                 fun(XrefMod) ->
                         [NoCalls, NoFunctionCalls,
                          NoFunctions,  _NoInter] = XrefMod#xref_mod.info,
                         NewInfo = [NoCalls, NoFunctionCalls, NoFunctions,
                                    {no_inter_function_calls,length(EE_M)}],
                         XrefMod#xref_mod{info = NewInfo}
                 end,
               dict:update(Mod, IMFun,XMods)
       end,
    XrefMods1 = foldl(IFun, S#xref.modules, to_external(EE)),
    S1 = S#xref{modules = XrefMods1},

    UC_1 = user_family(union_of_family(UC)),

    ?FORMAT("DefAt ~p~n", [DefAt]),
    ?FORMAT("U=~p~nLib=~p~nB=~p~nLU=~p~nXU=~p~nUU=~p~nOL=~p~n",
            [U,Lib,B,LU,XU,UU,OL]),
    ?FORMAT("E_1=~p~nLC_1=~p~nXC_1=~p~n", [E_1,LC_1,XC_1]),
    ?FORMAT("EE=~p~nEE_1=~p~nECallAt=~p~n", [EE, EE_1, ECallAt]),
    ?FORMAT("DF=~p~nDF_1=~p~nDF_2=~p~nDF_3=~p~n", [DF, DF_1, DF_2, DF_3]),

    Vs = [{'L',L}, {'X',X},{'F',F},{'U',U},{'B',B},{'UU',UU},
	  {'XU',XU},{'LU',LU},{'V',V},{v,V},{'OL',OL},
	  {'LC',{LC,LC_1}},{'XC',{XC,XC_1}},{'E',{E,E_1}},{e,{E,E_1}},
	  {'EE',{EE,EE_1}},{'UC',{UC,UC_1}},
	  {'M',M},{'A',A},{'R',R},
	  {'AM',AM},{'UM',UM},{'LM',LM},
	  {'ME',ME},{'AE',AE},{'RE',RE},
          {'DF',DF},{'DF_1',DF_1},{'DF_2',DF_2},{'DF_3',DF_3},
	  {me2ae, ME2AE},{ae, AE2RE},{m2a, M2A},{a2r, A2R},
	  {def_at, DefAt}, {call_at, CallAt}, {e_call_at, ECallAt},
	  {l_call_at, LCallAt}, {x_call_at, XCallAt}],
    finish_set_up(S1, Vs);
do_set_up(S) when S#xref.mode =:= modules ->
    ModDictList = dict:to_list(S#xref.modules),
    [X0, I0, Mod_DF, Mod_DF_1, Mod_DF_2, Mod_DF_3] =
        make_families(ModDictList, 7),
    I = union_of_family(I0),
    AM = domain(X0),

    {XU, Predefined} = make_predefined(I, AM),
    %% Add "hidden" functions to the exports.
    X1 = family_union(X0, Predefined),
    V = family_union(X1, XU),

    M = union(AM, domain(XU)),
    M2A = make_M2A(ModDictList),
    {A2R,A} = make_A2R(S#xref.applications),
    R = set(dict:fetch_keys(S#xref.releases)),

    ME = projection({external, fun({M1,{M2,_F2,_A2}}) -> {M1,M2} end},
		    family_to_relation(I0)),
    ME2AE = multiple_relative_product({M2A, M2A}, ME),

    AE = range(ME2AE),
    AE2RE = multiple_relative_product({A2R, A2R}, AE),
    RE = range(AE2RE),

    %% Undef is the union of U0 and Lib:
    {_Undef, U0, Lib, Lib_DF, Lib_DF_1, Lib_DF_2, Lib_DF_3} =
	make_libs(XU, X1, AM, S#xref.library_path, S#xref.libraries),
    {B, U} = make_builtins(U0),
    X1_B = family_union(X1, B),
    DF = family_union(family_intersection(Mod_DF, X1_B), Lib_DF),
    DF_1 = family_union(family_intersection(Mod_DF_1, X1_B), Lib_DF_1),
    DF_2 = family_union(family_intersection(Mod_DF_2, X1_B), Lib_DF_2),
    DF_3 = family_union(family_intersection(Mod_DF_3, X1_B), Lib_DF_3),

    LM = domain(Lib),
    UM = difference(difference(domain(U), AM), LM),
    X = family_union(X1, Lib),

    Empty = empty_set(),
    Vs = [{'X',X},{'U',U},{'B',B},{'XU',XU},{v,V},
	  {e,{Empty,Empty}},
	  {'M',M},{'A',A},{'R',R},
	  {'AM',AM},{'UM',UM},{'LM',LM},
	  {'ME',ME},{'AE',AE},{'RE',RE},
          {'DF',DF},{'DF_1',DF_1},{'DF_2',DF_2},{'DF_3',DF_3},
	  {me2ae, ME2AE},{ae, AE2RE},{m2a, M2A},{a2r, A2R},
	  {def_at, Empty}, {call_at, Empty}, {e_call_at, Empty},
	  {l_call_at, Empty}, {x_call_at, Empty}],
    finish_set_up(S, Vs).

finish_set_up(S, Vs) ->
    T = do_finish_set_up(Vs, dict:new()),
    S1 = S#xref{variables = T},
    %% io:format("~p <= state <= ~p~n", [pack:lsize(S), pack:usize(S)]),
    {ok, S1}.

do_finish_set_up([{Key, Value} | Vs], T) ->
    {Type, OType} = var_type(Key),
    Val = #xref_var{name = Key, value = Value, vtype = predef,
		    otype = OType, type = Type},
    T1 = dict:store(Key, Val, T),
    do_finish_set_up(Vs, T1);
do_finish_set_up([], T) ->
    T.

var_type('B')    -> {function, vertex};
var_type('F')    -> {function, vertex};
var_type('L')    -> {function, vertex};
var_type('LU')   -> {function, vertex};
var_type('U')    -> {function, vertex};
var_type('UU')   -> {function, vertex};
var_type('V')    -> {function, vertex};
var_type('X')    -> {function, vertex};
var_type('OL')   -> {function, vertex};
var_type('XU')   -> {function, vertex};
var_type('DF')   -> {function, vertex};
var_type('DF_1') -> {function, vertex};
var_type('DF_2') -> {function, vertex};
var_type('DF_3') -> {function, vertex};
var_type('A')    -> {application, vertex};
var_type('AM')   -> {module, vertex};
var_type('LM')   -> {module, vertex};
var_type('M')    -> {module, vertex};
var_type('UM')   -> {module, vertex};
var_type('R')    -> {release, vertex};
var_type('E')    -> {function, edge};
var_type('EE')   -> {function, edge};
var_type('LC')   -> {function, edge};
var_type('UC')   -> {function, edge};
var_type('XC')   -> {function, edge};
var_type('AE')   -> {application, edge};
var_type('ME')   -> {module, edge};
var_type('RE')   -> {release, edge};
var_type(_)      -> {foo, bar}.

make_families(ModDictList, N) ->
    Fun1 = fun({_,XMod}) -> XMod#xref_mod.data end,
    Ss = from_sets(map(Fun1, ModDictList)),
    %% io:format("~n~p <= module data <= ~p~n",
    %%           [pack:lsize(Ss), pack:usize(Ss)]),
    make_fams(N, Ss, []).

make_fams(1, _Ss, L) ->
    L;
make_fams(I, Ss, L) ->
    Fun = {external, fun(R) -> {element(1, R), element(I, R)} end},
    make_fams(I-1, Ss, [projection(Fun, Ss) | L]).

make_M2A(ModDictList) ->
    Fun = fun({M,XMod}) -> {M, XMod#xref_mod.app_name} end,
    Mod0 = family(map(Fun, ModDictList)),
    Mod = family_to_relation(Mod0),
    Mod.

make_A2R(ApplDict) ->
    AppDict = dict:to_list(ApplDict),
    Fun = fun({A,XApp}) -> {A, XApp#xref_app.rel_name} end,
    Appl0 = family(map(Fun, AppDict)),
    AllApps = domain(Appl0),
    Appl = family_to_relation(Appl0),
    {Appl, AllApps}.

do_set_up_1(XC) ->
    %% Call Graph cross reference...
    XCp = union_of_family(XC),
    XC_1 = user_family(XCp),

    %% I - functions used externally from some module
    %% XU  - functions used externally per module.
    I = range(XCp),

    {XU, XPredefined} = make_predefined(I, domain(XC)),
    {XC_1, XU, XPredefined}.

make_predefined(I, CallingModules) ->
    XPredefined0 = predefined_funs(I),
    XPredefined1 = converse(substitution(1, XPredefined0)),
    %% predefined funs in undefined modules are still undefined...
    XPredefined2 = restriction(XPredefined1, CallingModules),
    XPredefined = relation_to_family(XPredefined2),
    XU = partition_family(1, I),
    {XU, XPredefined}.

predefined_funs(Functions) ->
    specification({external, predef_fun()}, Functions).

predef_fun() ->
    PredefinedFuns = xref_utils:predefined_functions(),
    fun({_M,F,A}) -> member({F,A}, PredefinedFuns) end.

make_defat(Undef, DefAt0) ->
    % Complete DefAt with unknown functions:
    Zero = from_term(0),
    DAL = family_projection(fun(S) -> constant_function(S, Zero) end, Undef),
    family_union(DefAt0, DAL).

%% -> {Unknown U Lib, Unknown, Lib} | throw(Error)
make_libs(XU, F, AM, LibPath, LibDict) ->
    Undef = family_difference(XU, F),
    UM = difference(domain(family_to_relation(Undef)), AM),
    Fs = case is_empty_set(UM) of
	     true ->
		 [];
	     false when LibPath =:= code_path ->
		 BFun = fun(M, A) -> case xref_utils:find_beam(M) of
					 {ok, File} -> [File | A];
					 _ -> A
				     end
			end,
		 foldl(BFun, [], to_external(UM));
	     false ->
		 Libraries = dict:to_list(LibDict),
		 Lb = restriction(a_function(Libraries), UM),
		 MFun = fun({M,XLib}) ->
				#xref_lib{dir = Dir} = XLib,
				xref_utils:module_filename(Dir, M)
			end,
		 map(MFun, to_external(Lb))
	     end,
    Fun = fun(FileName, Deprs) ->
		  case beam_lib:chunks(FileName, [exports, attributes]) of
		      {ok, {M, [{exports,X}, {attributes,A}]}} ->
			  Exports = mfa_exports(X, A, M),
                          %% No warnings for bad attributes...
                          {Deprecated,_Bad} = deprecated(A, Exports, M),
                          {{M,Exports}, [{M,Deprecated} | Deprs]};
		      Error ->
			  throw(Error)
		  end
	  end,
    {XL, DL} = mapfoldl(Fun, [], Fs),
    LF = from_term(XL),
    %% Undef is the first argument to make sure that the whole of LF
    %% becomes garbage:
    Lib = family_intersection(Undef, LF),
    {B,_} = make_builtins(Undef),
    DLib = family_union(Lib, B),
    [DF_1,DF_21,DF_31,DF1] = depr_lib(4, DL, DL, [], [], DLib),
    DF_2 = family_union(DF_21, DF_1),
    DF_3 = family_union(DF_31, DF_2),
    DF = family_union(DF1, DF_3),
    U = family_difference(Undef, Lib),
    {Undef, U, Lib, DF, DF_1, DF_2, DF_3}.

depr_lib(0, _, _, LL, [], _Lib) ->
    LL;
depr_lib(I, [], DL, LL, L, Lib) ->
    DT = family_intersection(Lib, from_term(L)),
    depr_lib(I-1, DL, DL, [DT | LL], [], Lib);
depr_lib(I, [{M,D} | Ds], DL, LL, L, Lib) ->
    depr_lib(I, Ds, DL, LL, [{M,element(I, D)} | L], Lib).

make_builtins(U0) ->
    Tmp = family_to_relation(U0),
    Fun2 = {external, fun({_M,{M,F,A}}) -> xref_utils:is_builtin(M, F, A) end},
    B = relation_to_family(specification(Fun2, Tmp)),
    U = family_difference(U0, B),
    {B, U}.

% Returns a family that may not be defined for all modules.
user_family(R) ->
    partition_family({external, fun({_MFA1, {M2,_,_}}) -> M2 end}, R).

do_variables(State) ->
    Fun = fun({Name, #xref_var{vtype = user}}, {P,U}) ->
		  {P,[Name | U]};
	     ({Name, #xref_var{vtype = predef}}, A={P,U}) ->
		  case atom_to_list(Name) of
		      [H|_] when H>= $a, H=<$z -> A;
		      _Else -> {[Name | P], U}
		  end;
	     ({{tmp, V}, _}, A) ->
		  io:format("Bug in ~tp: temporary ~tp~n", [?MODULE, V]), A;
	     (_V, A) -> A
	  end,
    {U,P} = foldl(Fun, {[],[]}, dict:to_list(State#xref.variables)),
    {sort(P), sort(U)}.

%% Throws away the variables derived from raw data.
take_down(S) when S#xref.variables =:= not_set_up ->
    S;
take_down(S) ->
    S#xref{variables = not_set_up}.

make_query(Format, Args) ->
    flatten(io_lib:format(Format, Args)).

set_defaults([O | Os], [[V] | Vs], State) ->
    NewState = set_def(O, V, State),
    set_defaults(Os, Vs, NewState);
set_defaults([], [], State) ->
    State.

set_def(builtins, Value, State) ->
    State#xref{builtins_default = Value};
set_def(recurse, Value, State) ->
    State#xref{recurse_default = Value};
set_def(verbose, Value, State) ->
    State#xref{verbose_default = Value};
set_def(warnings, Value, State) ->
    State#xref{warnings_default = Value}.

option_values([Option | Options], State) ->
    Default = current_default(State, Option),
    [{Option, [Default,true,false]} | option_values(Options, State)];
option_values([], _State) ->
    [].

current_default(State, builtins) ->
    State#xref.builtins_default;
current_default(State, recurse) ->
    State#xref.recurse_default;
current_default(State, verbose) ->
    State#xref.verbose_default;
current_default(State, warnings) ->
    State#xref.warnings_default.

%% sets are used here to avoid long execution times
do_info(S, modules) ->
    D = sort(dict:to_list(S#xref.modules)),
    map(fun({_M,XMod}) -> mod_info(XMod) end, D);
do_info(S, applications) ->
    AppMods = to_external(relation_to_family(relation(app_mods(S)))),
    Sum = sum_mods(S, AppMods),
    map(fun(AppSum) -> app_info(AppSum, S) end, Sum);
do_info(S, releases) ->
    {RA, RRA} = rel_apps(S),
    rel_apps_sums(RA, RRA, S);
do_info(S, libraries) ->
    D = sort(dict:to_list(S#xref.libraries)),
    map(fun({_L,XLib}) -> lib_info(XLib) end, D);
do_info(_S, I) ->
    error({no_such_info, I}).

do_info(S, Type, E) when is_atom(E) ->
    do_info(S, Type, [E]);
do_info(S, modules, Modules0) when is_list(Modules0) ->
    Modules = to_external(set(Modules0)),
    XMods = find_info(Modules, S#xref.modules, no_such_module),
    map(fun(XMod) -> mod_info(XMod) end, XMods);
do_info(S, applications, Applications) when is_list(Applications) ->
    _XA = find_info(Applications, S#xref.applications, no_such_application),
    AM = relation(app_mods(S)),
    App = set(Applications),
    AppMods_S = relation_to_family(restriction(AM, App)),
    AppSums = sum_mods(S, to_external(AppMods_S)),
    map(fun(AppSum) -> app_info(AppSum, S) end, AppSums);
do_info(S, releases, Releases) when is_list(Releases) ->
    _XR = find_info(Releases, S#xref.releases, no_such_release),
    {AR, RRA} = rel_apps(S),
    AR_S = restriction(2, relation(AR), set(Releases)),
    rel_apps_sums(to_external(AR_S), RRA, S);
do_info(S, libraries, Libraries0) when is_list(Libraries0) ->
    Libraries = to_external(set(Libraries0)),
    XLibs = find_info(Libraries, S#xref.libraries, no_such_library),
    map(fun(XLib) -> lib_info(XLib) end, XLibs);
do_info(_S, I, J) when is_list(J) ->
    throw_error({no_such_info, I}).

find_info([E | Es], Dict, Error) ->
    case dict:find(E, Dict) of
	error ->
	    throw_error({Error, E});
	{ok, X} ->
	    [X | find_info(Es, Dict, Error)]
    end;
find_info([], _Dict, _Error) ->
    [].

%% -> {[{AppName, RelName}], [{RelName, XApp}]}
rel_apps(S) ->
    D = sort(dict:to_list(S#xref.applications)),
    Fun = fun({_A, XApp}, Acc={AR, RRA}) ->
		  case XApp#xref_app.rel_name of
		      [] -> Acc;
		      [R] ->
			  AppName = XApp#xref_app.name,
			  {[{AppName, R} | AR], [{R, XApp} | RRA]}
		  end
	  end,
    foldl(Fun, {[], []}, D).

%% -> [{{RelName, [XApp]}, Sums}]
rel_apps_sums(AR, RRA0, S) ->
    AppMods = app_mods(S), % [{AppName, XMod}]
    RRA1 = relation_to_family(relation(RRA0)),
    RRA = inverse(substitution(1, RRA1)),
    %% RRA is [{RelName,{RelName,[XApp]}}]
    RelMods = relative_product1(relation(AR), relation(AppMods)),
    RelAppsMods = relative_product1(RRA, RelMods),
    RelsAppsMods = to_external(relation_to_family(RelAppsMods)),
    %% [{{RelName, [XApp]}, [XMod]}]
    Sum = sum_mods(S, RelsAppsMods),
    map(fun(RelAppsSums) -> rel_info(RelAppsSums, S) end, Sum).

%% -> [{AppName, XMod}]
app_mods(S) ->
    D = sort(dict:to_list(S#xref.modules)),
    Fun = fun({_M,XMod}, Acc) ->
		  case XMod#xref_mod.app_name of
		      [] -> Acc;
		      [AppName] -> [{AppName, XMod} | Acc]
		  end
	  end,
    foldl(Fun, [], D).

mod_info(XMod) ->
    #xref_mod{name = M, app_name = AppName, builtins = BuiltIns,
	       dir = Dir, info = Info} = XMod,
    App = sup_info(AppName),
    {M, [{application, App}, {builtins, BuiltIns}, {directory, Dir} | Info]}.

app_info({AppName, ModSums}, S) ->
    XApp = dict:fetch(AppName, S#xref.applications),
    #xref_app{rel_name = RelName, vsn = Vsn, dir = Dir} = XApp,
    Release = sup_info(RelName),
    {AppName, [{directory,Dir}, {release, Release}, {version,Vsn} | ModSums]}.

rel_info({{RelName, XApps}, ModSums}, S) ->
    NoApps = length(XApps),
    XRel = dict:fetch(RelName, S#xref.releases),
    Dir = XRel#xref_rel.dir,
    {RelName, [{directory, Dir}, {no_applications, NoApps} | ModSums]}.

lib_info(XLib) ->
    #xref_lib{name = LibName, dir = Dir} = XLib,
    {LibName, [{directory,Dir}]}.

sup_info([]) -> [];
sup_info([Name]) ->
    [Name].

sum_mods(S, AppsMods) ->
    sum_mods(S, AppsMods, []).

sum_mods(S, [{N, XMods} | NX], L) ->
    sum_mods(S, NX, [{N, no_sum(S, XMods)} | L]);
sum_mods(_S, [], L) ->
    reverse(L).

no_sum(S, L) when S#xref.mode =:= functions ->
    no_sum(L, 0, 0, 0, 0, 0, 0, 0, 0, length(L));
no_sum(S, L) when S#xref.mode =:= modules ->
    [{no_analyzed_modules, length(L)}].

no_sum([XMod | D], C0, UC0, LC0, XC0, UFC0, L0, X0, EV0, NoM) ->
    [{no_calls, {C,UC}},
     {no_function_calls, {LC,XC,UFC}},
     {no_functions, {L,X}},
     {no_inter_function_calls, EV}] = XMod#xref_mod.info,
    no_sum(D, C0+C, UC0+UC, LC0+LC, XC0+XC, UFC0+UFC, L0+L, X0+X, EV0+EV, NoM);
no_sum([], C, UC, LC, XC, UFC, L, X, EV, NoM) ->
    [{no_analyzed_modules, NoM},
     {no_calls, {C,UC}},
     {no_function_calls, {LC,XC,UFC}},
     {no_functions, {L,X}},
     {no_inter_function_calls, EV}].

%% -> ok | throw(Error)
is_filename(F) when is_atom(F) ->
    ok;
is_filename(F) ->
    case xref_utils:is_string(F, 31) of
	true ->
	    ok;
	false ->
	    throw_error({invalid_filename, F})
    end.

module_file(XMod) ->
    xref_utils:module_filename(XMod#xref_mod.dir, XMod#xref_mod.name).

warnings(_Flag, _Message, []) -> true;
warnings(Flag, Message, [F | Fs]) ->
    message(Flag, Message, F),
    warnings(Flag, Message, Fs).

%% pack(term()) -> term()
%%
%% The identify function. The returned term does not use more heap
%% than the given term. Tuples that are equal (=:=/2) are made
%% "the same".
%%
%% The process dictionary is used because it seems to be faster than
%% anything else right now...
%%
%pack(T) -> T;
pack(T) ->
    PD = erase(),
    NT = pack1(T),
    %% true = T =:= NT,
    %% io:format("erasing ~p elements...~n", [length(erase())]),
    _ = erase(), % wasting heap (and time)...
    foreach(fun({K,V}) -> put(K, V) end, PD),
    NT.

pack1(C) when not is_tuple(C), not is_list(C) ->
    C;
pack1([T | Ts]) ->
    %% don't store conscells...
    [pack1(T) | pack1(Ts)];
%% Optimization.
pack1(T={Mod,Fun,_}) when is_atom(Mod), is_atom(Fun) -> % MFA
    case get(T) of
	undefined -> put(T, T), T;
	NT -> NT
    end;
pack1({C, L}) when is_list(L) -> % CallAt
    {pack1(C), L};
pack1({MFA, L}) when is_integer(L) -> % DefAt
    {pack1(MFA), L};
%% End optimization.
pack1([]) ->
    [];
pack1(T) -> % when is_tuple(T)
    case get(T) of
	undefined ->
	    NT = tpack(T, tuple_size(T), []),
	    put(NT, NT),
	    NT;
	NT ->
	    NT
    end.

tpack(_T, 0, L) ->
    list_to_tuple(L);
tpack(T, I, L) ->
    tpack(T, I-1, [pack1(element(I, T)) | L]).

message(true, What, Arg) ->
    case What of
	no_debug_info ->
	    io:format("Skipping ~ts (no debug information)~n", Arg);
	unresolved_summary1 ->
	    io:format("~tp: 1 unresolved call~n", Arg);
	unresolved_summary ->
	    io:format("~tp: ~tp unresolved calls~n", Arg);
	jam ->
	    io:format("Skipping ~ts (probably JAM file)~n", [Arg]);
	unreadable ->
	    io:format("Skipping ~ts (unreadable)~n", [Arg]);
	xref_attr ->
	    io:format("~ts: Skipping 'xref' attribute ~tw~n", Arg);
        depr_attr ->
            io:format("~ts: Skipping 'deprecated' attribute ~tw~n", Arg);
	lib_search ->
	    io:format("Scanning library path for BEAM files... ", []);
	lib_check ->
	    io:format("Checking library files... ", []);
	set_up ->
	    io:format("Setting up...", Arg);
	done ->
	    io:format("done~n", Arg);
	done_file ->
	    io:format("done reading ~ts~n", Arg);
	error ->
	    io:format("error~n", Arg);
	Else ->
	    io:format("~tp~n", [{Else,Arg}])
    end;
message(_, _, _) ->
    true.

throw_error(Reason) ->
    throw(error(Reason)).

error(Reason) ->
    {error, ?MODULE, Reason}.
