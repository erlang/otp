%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(systools_rc).
-export([translate_scripts/3, translate_scripts/4, format_error/1]).

-include("systools.hrl").

%%-----------------------------------------------------------------
%% High-level
%% ==========
%% mnesia_backup (not yet implemented)
%% {update, Mod, Change, PrePurge, PostPurge, [Mod]}
%% {update, Mod, Timeout, Change, PrePurge, PostPurge, [Mod]}
%% {update, Mod, ModType, , Change, PrePurge, PostPurge, [Mod]}
%% {update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, [Mod]}
%% {load_module, Mod, PrePurge, PostPurge, [Mod]}
%% {add_module, Mod}
%% {add_module, Mod, [Mod]}
%% {restart_application, Appl}
%% {add_application, Appl, Type}
%% {remove_application, Appl}
%%
%% Low-level
%% =========
%% {load_object_code, {Lib, LibVsn, Mods}}
%% point_of_no_return
%% {load, {Mod, PrePurge, PostPurge}}
%% {remove, {Mod, PrePurge, PostPurge}}
%% {purge, Mods}
%% {suspend, Mods}
%% {resume, Mods}
%% {code_change, [{Mod, Extra}]}
%% {code_change, Mode, [{Mod, Extra}]}
%% {stop, Mods}
%% {start, Mods}
%% {sync_nodes, Id, {M, F, A}}
%% {sync_nodes, Id, Nodes}
%% {apply, {M, F, A}}
%% restart_new_emulator
%% restart_emulator
%%-----------------------------------------------------------------

%% High-level instructions that contain dependencies
%%
-define(DEP_INSTRS, [update, load_module, add_module, delete_module]).

%%-----------------------------------------------------------------
%% translate_scripts(Scripts, Appls, PreAppls) -> Res
%% Mode = up | dn
%% Scripts = [AppupScript]
%% Appls = PreAppls = [#application]
%% Res = {ok, LowLevelScript} | {error, ?MODULE, Reason}
%%-----------------------------------------------------------------
translate_scripts(Scripts, Appls, PreAppls) ->
    translate_scripts(up, Scripts, Appls, PreAppls).

translate_scripts(Mode, Scripts, Appls, PreAppls) ->
    Scripts2 = expand_scripts(Scripts),
    case catch do_translate_scripts(Mode, Scripts2, Appls, PreAppls) of
	{ok, NewScript} -> {ok, NewScript};
	{error, Reason} -> {error, ?MODULE, Reason};
	{'EXIT', Reason} -> {error, ?MODULE, Reason}
    end.

expand_scripts([Script|Scripts]) ->
    [expand_script(Script)|expand_scripts(Scripts)];
expand_scripts([]) ->
    [].

expand_script([I|Script]) ->
    I2 = case I of
	     {load_module, Mod} ->
		 {load_module, Mod, brutal_purge, brutal_purge, []};
	     {load_module, Mod, Mods} when is_list(Mods) ->
		 {load_module, Mod, brutal_purge, brutal_purge, Mods};
	     {update, Mod} ->
		 {update, Mod, soft, brutal_purge, brutal_purge, []};
	     {update, Mod, supervisor} ->
		 {update, Mod, static, default, {advanced,[]},
		  brutal_purge, brutal_purge, []};
	     {update, Mod, Change} when is_tuple(Change) ->
		 {update, Mod, Change, brutal_purge, brutal_purge, []};
	     {update, Mod, Change} when Change==soft ->
		 {update, Mod, Change, brutal_purge, brutal_purge, []};
	     {update, Mod, Mods} when is_list(Mods) ->
		 {update, Mod, soft, brutal_purge, brutal_purge, Mods};
	     {update, Mod, Change, Mods} when is_tuple(Change),
					      is_list(Mods) ->
		 {update, Mod, Change, brutal_purge,brutal_purge, Mods};
	     {update, Mod, Change, Mods} when Change==soft,
					      is_list(Mods) ->
		 {update, Mod, Change, brutal_purge,brutal_purge, Mods};
	     {add_application, Application} ->
		 {add_application, Application, permanent};
	     _ ->
		 I
	 end,
    if
	is_list(I2) ->
	    I2 ++ expand_script(Script);
	true ->
	    [I2|expand_script(Script)]
    end;
expand_script([]) ->
    [].

do_translate_scripts(Mode, Scripts, Appls, PreAppls) ->
    MergedScript = merge_scripts(Scripts),
    translate_merged_script(Mode, MergedScript, Appls, PreAppls).

%%-----------------------------------------------------------------
%% All check_ functions performs checks, and throws {error, Reason}
%% (or fails) in case of error.  Called functions may throw error or
%% fail. The script is split into instructions before and after
%% point_of_no_return.  In Before, only load_object_code and apply are
%% allowed.
%% %%-----------------------------------------------------------------
translate_merged_script(Mode, Script, Appls, PreAppls) ->
    check_syntax(Script),
    Script1 = normalize_instrs(Script),
    {Before, After} = split_script(Script1),
    check_script(Before, After),

    {Before1, After1} = translate_independent_instrs(Before, After, Appls, PreAppls),
    {Before2, After2} = translate_dependent_instrs(Mode, Before1, After1, 
						  Appls),
    Before3 = merge_load_object_code(Before2),

    {Before4,After4} = sort_emulator_restart(Mode,Before3,After2),
    NewScript = Before4 ++ [point_of_no_return | After4],

    check_syntax(NewScript),
    {ok, NewScript}.

%%-----------------------------------------------------------------
%% SPLIT AND MERGE
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% merge_scripts(Scripts) -> Script
%%
%% Splits each script into before and after, and merges the before and
%% after parts.
%%-----------------------------------------------------------------
merge_scripts(Scripts) ->
    {Before, After} =
	lists:foldl(
	  fun(Script, {B1, A1}) ->
		  {B2, A2} = split_script(Script),
		  {B1 ++ B2, A1 ++ A2}
	  end, {[], []},Scripts),
    Before ++ [point_of_no_return | After].

%%-----------------------------------------------------------------
%% split_script(Script) -> {Before, After}
%%
%% Splits the script into instructions before and after
%% point_of_no_return. Puts all load_object_code instructions in
%% Before.  Checks that there is at most one point_of_no_return.
%% Makes sure that if there was a point_of_no_return, only apply and
%% load_object_code are before the point_of_no_return.
%%-----------------------------------------------------------------
split_script(Script) ->
    {Before, After} = split_instrs(Script),
    lists:foreach(
      fun({load_object_code, _}) -> ok;
	 ({apply, _}) -> ok;
	 (Instruction) ->
	      throw({error, {bad_op_before_point_of_no_return,
			     Instruction}})
      end, Before),
    {Found, Rest} = split(fun({load_object_code, _}) -> true;
			     (_) -> false
			  end, After),
    {Before ++ Found, Rest}.

%% split_instrs(Script) -> {Before, After} Split the
%% instructions into the set of those that appear before
%% point_of_no_return, and the set of those that appear after. If
%% there is no point_of_no_return instruction {[], Script} is
%% returned.
split_instrs(Script) ->
    split_instrs(Script, []).
split_instrs([point_of_no_return | T], Before) ->
    case lists:member(point_of_no_return, T) of
	true -> throw({error, too_many_point_of_no_return});
	false -> {lists:reverse(Before), T}
    end;
split_instrs([H | T], Before) ->
    split_instrs(T, [H | Before]);
split_instrs([], Before) ->
    {[], lists:reverse(Before)}.

%%-----------------------------------------------------------------
%% CHECKS
%%-----------------------------------------------------------------

check_script(Before, After) ->
    check_load(Before, After),
    check_suspend_resume(After),
    check_start_stop(After).

%%-----------------------------------------------------------------
%% Checks that each load has a corresponding load_object_code.
%%-----------------------------------------------------------------
check_load(Before, After) ->
    lists:foreach(
      fun({load, {Mod, _, _}}) ->
	      case find_object_code(Mod, Before) of
		  true -> ok;
		  false -> throw({error, {no_object_code, Mod}})
	      end;
	 (_) -> ok
      end, After).

find_object_code(Mod, [{load_object_code, {_, _, Mods}} | T]) ->
    case lists:member(Mod, Mods) of
	true -> true;
	false -> find_object_code(Mod, T)
    end;
find_object_code(Mod, [_|T]) ->
    find_object_code(Mod, T);
find_object_code(_Mod, []) ->
    false.

%%-----------------------------------------------------------------
%% Checks that all suspended Mods are resumed, and that none are
%% resumed/code_changed but not suspended.
%%-----------------------------------------------------------------
check_suspend_resume(Script) ->
    Suspended   = lists:map(fun({Mod, _Timeout}) -> Mod;
			       (Mod) -> Mod
			    end,
			    lists:flatten([X || {suspend, X} <- Script])),
    Resumed = lists:flatten([X || {resume, X} <- Script]),
    CodeChanged = lists:flatten([X || {code_change, _, {X, _}} <- Script]),
    case difference(Suspended, Resumed) of
	[] -> ok;
	S2 -> throw({error, {suspended_not_resumed, S2}})
    end,
    case difference(Resumed, Suspended) of
	[] -> ok;
	R2 -> throw({error, {resumed_not_suspended, R2}})
    end,
    case difference(CodeChanged, Suspended) of
	[] -> ok;
	C2 -> throw({error, {code_change_not_suspended, C2}})
    end.

%%-----------------------------------------------------------------
%% Checks that all stops are started, and that all starts are
%% stopped.
%%-----------------------------------------------------------------
check_start_stop(Script) ->
    Start = lists:flatten([X || {start, X} <- Script]),
    Stop  = lists:flatten([X || {stop, X}  <- Script]),
    case difference(Start, Stop) of
	[] -> ok;
	S2 -> throw({error, {start_not_stop, S2}})
    end,
    case difference(Stop, Start) of
	[] -> ok;
	S3 -> throw({error, {stop_not_start, S3}})
    end.


%%-----------------------------------------------------------------
%% NORMALISATION
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Normalize those instructions that have variants (update and
%% add_module).
%%-----------------------------------------------------------------
normalize_instrs(Script) ->
    lists:map(fun({update, Mod, Change, PrePurge, PostPurge, Mods}) ->
		      {update, Mod, dynamic, default, Change, PrePurge, 
		       PostPurge, Mods};
		 ({update, Mod, Timeout, Change, PrePurge, PostPurge, 
		   Mods}) ->
		      {update, Mod, dynamic, Timeout, Change, PrePurge, 
		       PostPurge, Mods};
		 ({add_module, Mod}) ->
		      {add_module, Mod, []};
		 ({delete_module, Mod}) ->
		      {delete_module, Mod, []};
		 (I) ->
		      I
	      end, Script).

%%-----------------------------------------------------------------
%% TRANSLATION OF INDEPENDENT INSTRUCTIONS
%%-----------------------------------------------------------------

%% translate_independent_instrs(Before, After, Appls, PreAppls) -> 
%%						{NBefore, NAfter}
%%
translate_independent_instrs(Before, After, Appls, PreAppls) -> 
    After1 = translate_application_instrs(After, Appls, PreAppls),
    translate_add_module_instrs(Before, After1).

%%-----------------------------------------------------------------
%% Translates add_application, remove_application  and restart_application
%% into add_module, remove, purge and apply.
%%-----------------------------------------------------------------
translate_application_instrs(Script, Appls, PreAppls) ->
    %% io:format("Appls ~n~p~n",[Appls]),
    L = lists:map(
	  fun({add_application, Appl, Type}) ->
		  case lists:keysearch(Appl, #application.name, Appls) of
		      {value, Application} ->
			  Mods = Application#application.modules,
			  ApplyL = case Type of
			      none -> [];
			      load -> [{apply, {application, load, [Appl]}}];
			      _ -> [{apply, {application, start, [Appl, Type]}}]
			  end,
			  [{add_module, M, []} || M <- Mods] ++
			      ApplyL;
		      false ->
			  throw({error, {no_such_application, Appl}})
		  end;

	     ({remove_application, Appl}) ->
		  case lists:keysearch(Appl, #application.name, Appls) of
		      {value, _Application} ->
			  throw({error, {removed_application_present,
					 Appl}});
		      false ->
			  ignore
		  end,
		  case lists:keysearch(Appl, #application.name, PreAppls) of
		      {value, RemApplication} ->
			  Mods = RemApplication#application.modules,

			  [{apply, {application, stop, [Appl]}}] ++
			      [{remove, {M, brutal_purge, brutal_purge}}
			       || M <- Mods] ++
			      [{purge, Mods},
			       {apply, {application, unload, [Appl]}}];
		      false ->
			  throw({error, {no_such_application, Appl}})
		  end;
	     ({restart_application, Appl}) ->
		  case lists:keysearch(Appl, #application.name, PreAppls) of
		      {value, PreApplication} ->
			  PreMods = PreApplication#application.modules,
			  case lists:keysearch(Appl, #application.name, Appls) of
			      {value, PostApplication} ->
				  PostMods = PostApplication#application.modules,
				  Type = PostApplication#application.type,
				  Apply =
				      case Type of
					  none -> [];
					  load -> [{apply, {application, load,
							    [Appl]}}];
					  _ -> [{apply, {application, start,
							 [Appl, Type]}}]
				      end,

				  [{apply, {application, stop, [Appl]}}] ++
				      [{remove, {M, brutal_purge, brutal_purge}}
				       || M <- PreMods] ++
				      [{purge, PreMods}] ++
				      [{add_module, M, []} || M <- PostMods] ++
				      Apply;
			      false ->
				  throw({error, {no_such_application, Appl}})
			  end;

		      false ->
			  throw({error, {no_such_application, Appl}})
		  end;
	     (X) -> X
	  end, Script),
    lists:flatten(L).

%%-----------------------------------------------------------------
%% Translates add_module into load_module (high-level transformation)
%%-----------------------------------------------------------------
translate_add_module_instrs(Before, After) ->
    NAfter = lists:map(
	       fun({add_module, Mod, Mods}) -> 
		       %% Purge method really doesn't matter. Module
		       %% is new.
		       {load_module, Mod, brutal_purge, brutal_purge, Mods};
		  (I) ->
		       I
	       end, After),
    {Before, NAfter}.
		

%%-----------------------------------------------------------------
%% TRANSLATION OF INSTRUCTIONS WITH DEPENDENCIES
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Translates update, load_module and delete_module, and reorder the
%% instructions according to dependencies. Leaves other instructions
%% unchanged.
%%-----------------------------------------------------------------
translate_dependent_instrs(Mode, Before, After, Appls) ->
    %% G is the total dependency graph, WCs is the decomposition of
    %% the vertices (lists of vertices) of G. 
    G = make_dependency_graph(After),
    WCs = digraph_utils:components(G),
    {NBefore, NAfter} = translate_dep_loop(G, WCs, After, Appls, 
					   [], [], Mode),
    digraph:delete(G),
    {Before ++ NBefore, NAfter}.

translate_dep_loop(G, WCs, [I| Is], Appls, Before, After, Mode) 
  when is_tuple(I), size(I) > 1 ->
    IName = element(1, I),
    case lists:member(IName, ?DEP_INSTRS) of 
	true ->
	    Mod = element(2, I),
	    DepIs = get_dependent_instructions(G, WCs, Mod),
	    {B2, A2} = translate_dep_to_low(Mode, DepIs, Appls),
	    RemIs = difference([I| Is], DepIs),
	    translate_dep_loop(G, WCs, RemIs, Appls, Before ++ B2, 
			       After ++ A2, Mode);
	false ->
	    translate_dep_loop(G, WCs, Is, Appls, Before, 
			       After ++ [I], Mode)  % hmm
    end;
translate_dep_loop(G, WCs, [I| Is], Appls, Before, After, Mode) ->
    translate_dep_loop(G, WCs, Is, Appls, Before, After ++ [I], Mode);  % hmm
translate_dep_loop(_G, _WCs, [], _Appls, Before, After, _Mode) ->
    {Before, After}.


%%-----------------------------------------------------------------
%% make_dependency_graph(Instructions) -> graph()
%%
%% The return value is a digraph graph(). A vertex is a module name
%% Mod, and the associated data is {N, I} where I is the corresponding
%% instruction, and N numbers the instruction in the order given at
%% input. Only instructions that have dependencies are considered.
%% %%-----------------------------------------------------------------
make_dependency_graph(Instructions) ->
    %% Filter out dependent instructions
    DepIs = lists:filter(fun(I) when is_tuple(I) ->
			       IName = element(1, I),
			       lists:member(IName, ?DEP_INSTRS);
			    (_) ->
				 false
			 end, Instructions),
    {VDs, _} = lists:mapfoldl(
			     fun(I, N) ->
				     Mod = element(2, I),
				     Mods = element(size(I), I),
				     {{Mod, Mods, {N, I}}, N+1}
			     end, 1, DepIs),
    G = digraph:new(),
    %% Add vertices
    lists:foreach(
      fun({Mod, _Mods, Data}) ->
	      case digraph:vertex(G, Mod) of
		  false ->
		      digraph:add_vertex(G, Mod, Data);
		  _  ->
		      throw({error, {muldef_module, Mod}})
	      end
      end, VDs),
    %% Add edges
    lists:foreach(
      fun({Mod, Mods, _Data}) ->
	      lists:foreach(
		fun(M) ->
			case digraph:add_edge(G, Mod, M) of
			    {error, _Reason} ->
				throw({error, {undef_module, M}});
			    _ ->
				ok
			end
		end, Mods)
      end, VDs),
    G.

%% get_dependent_instructions(G, WCs, Mod) -> DepIs
%%  
%% G is the global dependency graph, WCs are the weak components
%% (lists of vertices) of G, and Mod is the module for which we will
%% pick up all instructions that Mod depends on, or that depend on
%% Mod.
%%
get_dependent_instructions(G, WCs, Mod) ->
    case lists:filter(fun(C) -> lists:member(Mod, C) end, WCs) of
	[WC] ->
	    %% restrict G to WC
	    H = restriction(WC, G),
	    %% vertices of S are strong components of H
	    S = condensation(H),
	    Ts = digraph_utils:topsort(S),
	    DepIss = lists:map(
		       fun(T) -> 
			       NIs = lists:map(
				       fun(V) ->
					       {_, Data} = 
						   digraph:vertex(H, V),
					       Data
				       end, T),
			       %% NIs = [{N, I}]
			       SortedNIs = lists:keysort(1, NIs),
			       lists:map(fun({_N, I}) -> I end, SortedNIs)
		       end, Ts),
	    DepIs = lists:flatten(DepIss),	% XXX One level flat only
	    digraph:delete(H),
	    digraph:delete(S),
	    DepIs;
	[] ->
	    throw({error, {undef_module, Mod}});
	_ -> 
	    throw({error, {muldef_module, Mod}})
    end.
			       
%% translate_dep_to_low(Mode, Instructions, Appls) -> {Before, After}
%%
%% Mode = up | dn
%% Instructions are in order of dependency.
%% Appls = [#application]
%%
%% Instructions translated are: update, load_module, and delete_module
%%
%% Before =	[{load_object_code, ...}]
%% After = 	[{suspend, ...}] ++ CodeInstrs ++ [{resume, ...}]
%% CodeInstrs = [{load, ...}] ++ [{code_change, ...}]  (Mode == up)
%% 	      = [{code_change, ...}] ++ [{load, ...}] ++
%% 		[{code_change, ...}]  (Mode == dn)
%%
translate_dep_to_low(Mode, Instructions, Appls) ->
    UpdateMods = 
	filtermap(fun({update, Mod, _, default, _, _, _, _}) ->
			  {true, Mod};
		     ({update, Mod, _, T, _, _, _, _}) ->
			  {true, {Mod, T}};
		     (_) ->
			  false
		  end,
		  Instructions),
    RevUpdateMods = lists:reverse(UpdateMods),

    %% Processes are suspended in the order of dependency.
    SuspendInstrs =
	if
	    UpdateMods == [] -> [];
	    true -> [{suspend, UpdateMods}]
	end,


    %% Processes are resumed in the reversed order of dependency.
    ResumeInstrs =
	if
	    UpdateMods == [] -> [];
	    true -> [{resume,
		      lists:map(fun({Mod, _T}) -> Mod;
				   (Mod) -> Mod
				end, RevUpdateMods)}]
	end,

    LoadRemoveInstrs0 =
	filtermap(fun({update, Mod, _, _, _, PreP, PostP, _}) ->
			  {true, {load, {Mod, PreP, PostP}}};
		     ({load_module, Mod, PreP, PostP, _}) ->
			  {true, {load, {Mod, PreP, PostP}}};
		     ({delete_module, Mod, _}) ->
			  {true,[{remove, {Mod, brutal_purge, brutal_purge}},
				 {purge, [Mod]}]};
		     (_) -> false
		  end,
		  Instructions),
    LoadRemoveInstrs = lists:flatten(LoadRemoveInstrs0),
    RevLoadRemoveInstrs = lists:flatten(lists:reverse(LoadRemoveInstrs0)),

    %% The order of loading object code is unimportant. The order
    %% chosen is the order of dependency.
    LoadObjCodeInstrs = 
	filtermap(fun({load, {Mod, _, _}}) ->
		    {Lib, LibVsn} = get_lib(Mod, Appls),
		    {true, {load_object_code, {Lib, LibVsn, [Mod]}}};
		     (_) -> false
	    end, LoadRemoveInstrs),
    if
	Mode == up ->
	    %% The order of changing code is unimportant (processes
	    %% are suspended). The order chosen is the order of
	    %% dependency.
	    CodeChangeMods = 
		filtermap(fun({update, Mod, _, _,
			       {advanced, Extra}, _, _, _}) ->
				  {true, {Mod, Extra}};
			     (_) ->
				  false
			  end, Instructions),
	    CodeChangeInstrs = 
		if
		    CodeChangeMods == [] -> [];
		    true -> [{code_change, up, CodeChangeMods}]
		end,
	    %% RevLoadRemoveInstrs: When upgrading modules are loaded
	    %% in the reversed order of dependency.
	    {LoadObjCodeInstrs, 
	     SuspendInstrs ++ RevLoadRemoveInstrs ++ CodeChangeInstrs ++ 
	     ResumeInstrs};

	Mode == dn ->
	    %% PreCodeChangeMods is the list of all modules that have
	    %% to change code *before* the code is loaded (when
	    %% downgrading). The order is not important (processes are
	    %% suspended). The order chosen is the order of
	    %% dependency.
	    PreCodeChangeMods = 
		[{Mod, Extra} ||
		    {update, Mod, dynamic, _, {advanced, Extra}, _, _, _}
			<- Instructions],
	    PreCodeChangeInstrs = 
		if
		    PreCodeChangeMods == [] -> [];
		    true -> [{code_change, down, PreCodeChangeMods}]
		end,
	    %% PostCodeChangeMods is the list of all modules that have
	    %% to change code *after* the code is loaded (when
	    %% downgrading). The order is not important (processes are
	    %% suspended). The order chosen is the order of
	    %% dependency.
	    PostCodeChangeMods =
		[{Mod, Extra} ||
		    {update, Mod, static, _, {advanced, Extra}, _, _, _}
			<- Instructions],
	    PostCodeChangeInstrs = 
		if
		    PostCodeChangeMods == [] -> [];
		    true -> [{code_change, down, PostCodeChangeMods}]
		end,
	    %% LoadRemoveInstrs: When downgrading modules are loaded
	    %% in the order of dependency.
	    {LoadObjCodeInstrs, 
	     SuspendInstrs ++ PreCodeChangeInstrs ++
	     LoadRemoveInstrs ++ PostCodeChangeInstrs ++ ResumeInstrs}
    end.

get_lib(Mod, [#application{name = Name, vsn = Vsn, modules = Modules} | T]) ->
    case lists:member(Mod, Modules) of
	true -> {Name, Vsn};
	false ->   get_lib(Mod, T)
    end;
get_lib(Mod, []) ->
    throw({error, {no_such_module, Mod}}).

%%-----------------------------------------------------------------
%% MERGE LOAD_OBJECT_CODE
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Merge load_object_code instructions into one load_object_code 
%% instruction per lib (optimization). Order is preserved. 
%%-----------------------------------------------------------------
merge_load_object_code(Before) ->
    {Found, Rest} = split(fun({load_object_code, _}) -> true;
			     (_) -> false
			  end, Before),
    mlo(Found) ++ Rest.

mlo([{load_object_code, {Lib, LibVsn, Mods}} | T]) ->
    {Same, Other} = split(fun({load_object_code, {Lib2, LibVsn2, _Mods2}})
			       when Lib == Lib2, LibVsn == LibVsn2 -> true;
			     ({load_object_code, {Lib2, LibVsn2, _Mods2}})
			       when Lib == Lib2 ->
				  throw({error, {conflicting_versions,
						 Lib, LibVsn, LibVsn2}});
			     (_) -> false
			  end, T),
    %% io:format("Same = ~p, Other = ~p~n", [Same, Other]),
    %% foldr to preserver order.
    OCode0 = lists:foldr(fun({load_object_code, {_, _, Ms}}, Res) ->
				 U = union(Ms, Res),
				 %% io:format("Ms = ~p, Res = ~p, U = ~p~n",
				 %% [Ms, Res, U]),
				 U
			 end, [], Same),
    OCode1 = union(Mods, OCode0),		% preserve order
    %% io:format("OCode0 = ~p, OCode1 = ~p~n", [OCode0, OCode1]),
    [{load_object_code, {Lib, LibVsn, OCode1}} | mlo(Other)];
mlo([]) -> [].

%%-----------------------------------------------------------------
%% RESTART EMULATOR
%% -----------------------------------------------------------------
%% -----------------------------------------------------------------
%% Check if there are any 'restart_new_emulator' instructions (i.e. if
%% the emulator or core application version is changed). If so, this
%% must be done first for upgrade and last for downgrade.
%% Check if there are any 'restart_emulator' instructions, if so
%% remove all and place one the end.
%% -----------------------------------------------------------------
sort_emulator_restart(Mode,Before,After) ->
    {Before1,After1} =
	case filter_out(restart_new_emulator, After) of
	    After ->
		{Before,After};
	    A1 when Mode==up ->
		{[restart_new_emulator|Before],A1};
	    A1 when Mode==dn ->
		{Before,A1++[restart_emulator]}
	end,
    After2 =
	case filter_out(restart_emulator, After1) of
	    After1 ->
		After1;
	    A2 ->
		A2++[restart_emulator]
	end,
    {Before1,After2}.


filter_out(What,List) ->
    lists:filter(fun(X) when X=:=What -> false; (_) -> true end, List).

%%-----------------------------------------------------------------
%% SYNTAX CHECK
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Checks the syntax of all instructions.
%%-----------------------------------------------------------------
check_syntax([H|T]) ->
    check_op(H),
    check_syntax(T);
check_syntax([]) -> ok.

check_op(mnesia_backup) ->
    throw({error, {not_yet_implemented, mnesia_backup}});
check_op({update, Mod, Change, PrePurge, PostPurge, Mods}) ->
    check_mod(Mod),
    check_change(Change),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({update, Mod, Timeout, Change, PrePurge, PostPurge, Mods}) ->
    check_mod(Mod),
    check_timeout(Timeout),
    check_change(Change),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, 
	  Mods}) ->
    check_mod(Mod),
    check_mod_type(ModType),
    check_timeout(Timeout),
    check_change(Change),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({load_module, Mod, PrePurge, PostPurge, Mods}) ->
    check_mod(Mod),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({add_module, Mod}) ->
    check_mod(Mod);
check_op({add_module, Mod, Mods}) ->
    check_mod(Mod),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({delete_module, Mod}) ->
    check_mod(Mod);
check_op({delete_module, Mod, Mods}) ->
    check_mod(Mod),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({remove_application, Appl}) ->
    check_appl(Appl);
check_op({add_application, Appl, Type}) ->
    check_appl(Appl),
    check_start_type(Type);
check_op({restart_application, Appl}) ->
    check_appl(Appl);
check_op(restart) -> ok;
check_op(reboot) -> ok;
check_op({load_object_code, {Lib, LibVsn, Mods}}) ->
    check_lib(Lib),
    check_lib_vsn(LibVsn),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op(point_of_no_return) -> ok;
check_op({load, {Mod, PrePurge, PostPurge}}) ->
    check_mod(Mod),
    check_purge(PrePurge),
    check_purge(PostPurge);
check_op({remove, {Mod, PrePurge, PostPurge}}) ->
    check_mod(Mod),
    check_purge(PrePurge),
    check_purge(PostPurge);
check_op({purge, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({suspend, Mods}) ->
    check_list(Mods),
    lists:foreach(fun({M,T}) -> check_mod(M), check_timeout(T);
		     (M) -> check_mod(M)
		  end, Mods);
check_op({resume, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({code_change, Mods}) ->
    check_list(Mods),
    lists:foreach(fun({M, _Extra}) -> check_mod(M);
		     (X) -> throw({error, {bad_code_change, X}})
		  end, Mods);
check_op({code_change, Mode, Mods}) ->
    check_list(Mods),
    check_mode(Mode),
    lists:foreach(fun({M, _Extra}) -> check_mod(M);
		     (X) -> throw({error, {bad_code_change, X}})
		  end, Mods);
check_op({stop, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({start, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({sync_nodes, _Id, {M, F, A}}) ->
    check_mod(M),
    check_func(F),
    check_args(A);
check_op({sync_nodes, _Id, Nodes}) ->
    check_list(Nodes),
    lists:foreach(fun(Node) -> check_node(Node) end, Nodes);
check_op({apply, {M, F, A}}) ->
    check_mod(M),
    check_func(F),
    check_args(A);
check_op(restart_new_emulator) -> ok;
check_op(restart_emulator) -> ok;
check_op(X) -> throw({error, {bad_instruction, X}}).

check_mod(Mod) when is_atom(Mod) -> ok;
check_mod(Mod) -> throw({error, {bad_module, Mod}}).

check_change(soft) -> ok;
check_change({advanced, _}) -> ok;
check_change(Change) -> throw({error, {bad_change, Change}}).

check_mod_type(static) -> ok;
check_mod_type(dynamic) -> ok;
check_mod_type(ModType) -> throw({error, {bad_mod_type, ModType}}).

check_purge(soft_purge) -> ok;
check_purge(brutal_purge) -> ok;
check_purge(Purge) -> throw({error, {bad_purge_method, Purge}}).

check_list(List) when is_list(List) -> ok;
check_list(List) -> throw({error, {bad_list, List}}).

check_args(Args) when is_list(Args) -> ok;
check_args(Args) -> throw({error, {bad_args_list, Args}}).

check_node(Node) when is_atom(Node) -> ok;
check_node(Node) -> throw({error, {bad_node, Node}}).

check_appl(Appl) when is_atom(Appl) -> ok;
check_appl(Appl) -> throw({error, {bad_application, Appl}}).

check_start_type(none) -> ok;
check_start_type(load) -> ok;
check_start_type(temporary) -> ok;
check_start_type(transient) -> ok;
check_start_type(permanent) -> ok;
check_start_type(T) -> throw({error, {bad_start_type, T}}).

check_func(Func) when is_atom(Func) -> ok;
check_func(Func) -> throw({error, {bad_func, Func}}).

check_lib(Lib) when is_atom(Lib) -> ok;
check_lib(Lib) -> throw({error, {bad_lib, Lib}}).

check_lib_vsn(LibVsn) when is_list(LibVsn) -> ok;
check_lib_vsn(LibVsn) -> throw({error, {bad_lib_vsn, LibVsn}}).

check_timeout(default) -> ok;
check_timeout(infinity) -> ok;
check_timeout(Int) when is_integer(Int), Int > 0 -> ok;
check_timeout(T) -> throw({error, {bad_timeout, T}}).

check_mode(up) -> ok;
check_mode(down) -> ok;
check_mode(Mode) -> throw({error, {bad_mode, Mode}}).

%%-----------------------------------------------------------------
%% Format error
%%-----------------------------------------------------------------
format_error({bad_op_before_point_of_no_return, Instruction}) ->
    io_lib:format("Bad instruction ~p~nbefore point_of_no_return~n",
		  [Instruction]);
format_error({no_object_code, Mod}) ->
    io_lib:format("No load_object_code found for module: ~w~n", [Mod]);
format_error({suspended_not_resumed, Mods}) ->
    io_lib:format("Suspended but not resumed: ~p~n", [Mods]);
format_error({resumed_not_suspended, Mods}) ->
    io_lib:format("Resumed but not suspended: ~p~n", [Mods]);
format_error({code_change_not_suspended, Mods}) ->
    io_lib:format("Code changed but not suspended: ~p~n", [Mods]);
format_error({start_not_stop, Mods}) ->
    io_lib:format("Started but not stopped: ~p~n", [Mods]);
format_error({stop_not_start, Mods}) ->
    io_lib:format("Stopped but not started: ~p~n", [Mods]);
format_error({no_such_application, App}) ->
    io_lib:format("Started undefined application: ~w~n", [App]);
format_error({removed_application_present, App}) ->
    io_lib:format("Removed application present: ~w~n", [App]);
format_error(dup_mnesia_backup) ->
    io_lib:format("Duplicate mnesia_backup~n", []);
format_error(bad_mnesia_backup) ->
    io_lib:format("mnesia_backup in bad position~n", []);
format_error({conflicting_versions, Lib, V1, V2}) ->
    io_lib:format("Conflicting versions for ~w, ~ts and ~ts~n", [Lib, V1, V2]);
format_error({no_appl_vsn, Appl}) ->
    io_lib:format("No version specified for application: ~w~n", [Appl]);
format_error({no_such_module, Mod}) ->
    io_lib:format("No such module: ~w~n", [Mod]);
format_error(too_many_point_of_no_return) ->
    io_lib:format("Too many point_of_no_return~n", []);

format_error({bad_instruction, X}) ->
    io_lib:format("Bad instruction: ~tp~n", [X]);
format_error({bad_module, X}) ->
    io_lib:format("Bad module: ~tp(should be atom())~n", [X]);
format_error({bad_code_change, X}) ->
    io_lib:format("Bad code_change: ~tp(should be {Mod, Extra})~n", [X]);
format_error({bad_change, X}) ->
    io_lib:format("Bad change spec: ~tp(should be soft | {advanced, E})~n", [X]);
format_error({bad_mod_type, X}) ->
    io_lib:format("Bad module type: ~tp(should be static | dynamic)~n", [X]);
format_error({bad_purge_method, X}) ->
    io_lib:format("Bad purge method: ~tp(should be soft_purge | brutal_purge)~n",
		  [X]);
format_error({bad_list, X}) ->
    io_lib:format("Bad list: ~tp~n", [X]);
format_error({bad_args_list, X}) ->
    io_lib:format("Bad argument list: ~tp~n", [X]);
format_error({bad_node, X}) ->
    io_lib:format("Bad node: ~tp(should be atom())~n", [X]);
format_error({bad_application, X}) ->
    io_lib:format("Bad application: ~tp(should be atom())~n", [X]);
format_error({bad_func, X}) ->
    io_lib:format("Bad function: ~tp(should be atom())~n", [X]);
format_error({bad_lib, X}) ->
    io_lib:format("Bad library: ~tp(should be atom())~n", [X]);
format_error({bad_lib_vsn, X}) ->
    io_lib:format("Bad library version: ~tp(should be string())~n", [X]);
format_error({bad_timeout, X}) ->
    io_lib:format("Bad timeout: ~tp(should be infinity | int() > 0)~n", [X]);

format_error({undef_module, Mod}) ->
    io_lib:format("Undefined module: ~p~n", [Mod]);
format_error({muldef_module, Mod}) ->
    io_lib:format("Multiply defined module: ~p~n", [Mod]);
format_error(E) ->
    io_lib:format("~tp~n",[E]).


%%-----------------------------------------------------------------
%% MISC SUPPORT
%%-----------------------------------------------------------------

%% filtermap(F, List1) -> List2
%% F(H) -> false | true | {true, Val}
filtermap(F, List) ->
    lists:zf(F, List).

%% split(F, List1) -> {List2, List3}
%% F(H) -> true | false. Preserves order. 
split(Fun, [H | T]) ->
    {Found, Rest} = split(Fun, T),
    case Fun(H) of
	true -> {[H | Found], Rest};
	false -> {Found, [H | Rest]}
    end;
split(_Fun, []) ->
    {[], []}.

union([H|T], L) ->
    case lists:member(H, L) of
	true -> union(T,L);
	false -> [H | union(T, L)]
    end;
union([], L) -> L.

difference([H | T], L) ->
    case lists:member(H, L) of
	true -> difference(T, L);
	false -> [H | difference(T, L)]
    end;
difference([], _) -> [].


%%-----------------------------------------------------------------
%% GRAPHS
%%-----------------------------------------------------------------

%% Additions to digraph and digraph utils.
%% XXX Should be removed in future versions.

%% This function should be included in digraph_utils.

%% condensation(G) -> graph()
%% 
%% Given a graph G, returns a new graph H where each vertex V in H is
%% a strong component of G, and where there is an edge from V1 to V2
%% in H if there are members of v1 and v2 of V1 and V2, respectively,
%% such that there is an edge from v1 to v2 in G.
%%
condensation(G) ->
    H = digraph:new(),
    HVs = digraph_utils:strong_components(G),
    %% Add all vertices
    lists:foreach(fun(HV) -> digraph:add_vertex(H, HV) end, HVs),
    %% Add edges
    lists:foreach(
      fun(HV1) ->
	      GRs = digraph_utils:reachable(HV1, G),
	      lists:foreach(
		fun(HV2) ->
			if
			    HV1 /= HV2 ->
				case lists:member(hd(HV2), GRs) of
				    true ->
					digraph:add_edge(H, HV1, HV2);
				    _  ->
					ok
				end;
			    true  ->
				ok 
			end
		end, HVs)
      end,  HVs),
    H.


%% This function should be included in digraph.

%% restriction(Rs, G) -> graph()
%% 
%% Given a graph G, returns a new graph H that is the restriction of
%% G to the vertices Rs. 
%%
restriction(Rs, G) ->
    H = digraph:new(),
    %% Add vertices
    lists:foreach(
      fun(R) ->
	      case digraph:vertex(G, R) of
		  {R, Data} ->
		      digraph:add_vertex(H, R, Data);
		  _  ->
		      ok
	      end
      end, Rs),
    %% Add edges
    GEs = digraph:edges(G),
    lists:foreach(
      fun(GE) ->
	      {_, GV1, GV2, GData} = digraph:edge(G, GE),
	      case {digraph:vertex(H, GV1), digraph:vertex(H, GV2)} of
		  {{GV1, _}, {GV2, _}} ->
		      digraph:add_edge(H, GE, GV1, GV2, GData);
		  _  ->
		      ok
	      end
      end, GEs),
    H.


