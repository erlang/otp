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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_plt.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : Interface to display information in the persistent
%%%               lookup tables.
%%%
%%% Created : 23 Jul 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_plt).

-export([check_plt/3,
	 compute_md5_from_files/1,
	 contains_mfa/2,
	 all_modules/1,
	 delete_list/2,
	 delete_module/2,
	 included_files/1,
	 from_file/1,
	 get_default_plt/0,
         get_module_types/2,
         get_exported_types/1,
	 insert_list/2,
	 insert_contract_list/2,
	 insert_callbacks/2,
	 insert_types/2,
         insert_exported_types/2,
	 lookup/2,
         is_contract/2,
	 lookup_contract/2,
	 lookup_callbacks/2,
	 lookup_module/2,
	 merge_plts/1,
         merge_plts_or_report_conflicts/2,
	 new/0,
	 plt_and_info_from_file/1,
	 get_specs/1,
	 get_specs/4,
	 to_file/4,
         delete/1
	]).

%% Debug utilities
-export([pp_non_returning/0, pp_mod/1]).

-export_type([plt/0, plt_info/0]).

-include_lib("stdlib/include/ms_transform.hrl").

%%----------------------------------------------------------------------

-type mod_deps() :: dialyzer_callgraph:mod_deps().

-type deep_string() :: string() | [deep_string()].

%% The following are used for searching the PLT when using the GUI
%% (e.g. in show or search PLT contents). The user might be searching
%% with a partial specification, in which case the missing items
%% default to '_'
-type arity_patt() :: '_' | arity().
-type mfa_patt()   :: {module(), atom(), arity_patt()}.

%%----------------------------------------------------------------------

-record(plt, {info      :: ets:tid(), %% {mfa() | integer(), ret_args_types()}
              types     :: ets:tid(), %% {module(), erl_types:type_table()}
              contracts :: ets:tid(), %% {mfa(), #contract{}}
              callbacks :: ets:tid(), %% {module(),
                                      %%  [{mfa(),
                                      %%  dialyzer_contracts:file_contract()}]
              exported_types :: ets:tid() %% {module(), sets:set()}
             }).

-opaque plt() :: #plt{}.

-include("dialyzer.hrl").

-type file_md5() :: {file:filename(), binary()}.
-type plt_info() :: {[file_md5()], dict:dict()}.

-record(file_plt, {version = ""                :: string(),
		   file_md5_list = []          :: [file_md5()],
		   info = dict:new()           :: dict:dict(),
		   contracts = dict:new()      :: dict:dict(),
		   callbacks = dict:new()      :: dict:dict(),
		   types = dict:new()          :: dict:dict(),
                   exported_types = sets:new() :: sets:set(),
		   mod_deps                    :: mod_deps(),
		   implementation_md5 = []     :: [file_md5()]}).

%%----------------------------------------------------------------------

-spec new() -> plt().

new() ->
  [ETSInfo, ETSContracts] =
    [ets:new(Name, [public]) ||
      Name <- [plt_info, plt_contracts]],
  [ETSTypes, ETSCallbacks, ETSExpTypes] =
    [ets:new(Name, [compressed, public]) ||
      Name <- [plt_types, plt_callbacks, plt_exported_types]],
  #plt{info = ETSInfo,
       types = ETSTypes,
       contracts = ETSContracts,
       callbacks = ETSCallbacks,
       exported_types = ETSExpTypes}.

-spec delete_module(plt(), atom()) -> plt().

delete_module(#plt{info = Info, types = Types,
		   contracts = Contracts,
		   callbacks = Callbacks,
                   exported_types = ExpTypes}, Mod) ->
  #plt{info = table_delete_module(Info, Mod),
       types = table_delete_module2(Types, Mod),
       contracts = table_delete_module(Contracts, Mod),
       callbacks = table_delete_module2(Callbacks, Mod),
       exported_types = table_delete_module1(ExpTypes, Mod)}.

-spec delete_list(plt(), [mfa() | integer()]) -> plt().

delete_list(#plt{info = Info,
                 contracts = Contracts}=Plt, List) ->
  Plt#plt{info = ets_table_delete_list(Info, List),
          contracts = ets_table_delete_list(Contracts, List)}.

-spec insert_contract_list(plt(), dialyzer_contracts:plt_contracts()) -> plt().

insert_contract_list(#plt{contracts = Contracts} = PLT, List) ->
  true = ets:insert(Contracts, List),
  PLT.

-spec insert_callbacks(plt(), dialyzer_codeserver:codeserver()) -> plt().

insert_callbacks(#plt{callbacks = Callbacks} = Plt, Codeserver) ->
  CallbacksList = dialyzer_codeserver:get_callbacks(Codeserver),
  CallbacksByModule =
    [{M, [Cb || {{M1,_,_},_} = Cb <- CallbacksList, M1 =:= M]} ||
      M <- lists:usort([M || {{M,_,_},_} <- CallbacksList])],
  true = ets:insert(Callbacks, CallbacksByModule),
  Plt.

-spec is_contract(plt(), mfa()) -> boolean().

is_contract(#plt{contracts = ETSContracts},
            {M, F, _} = MFA) when is_atom(M), is_atom(F) ->
  ets:member(ETSContracts, MFA).

-spec lookup_contract(plt(), mfa_patt()) -> 'none' | {'value', #contract{}}.

lookup_contract(#plt{contracts = ETSContracts},
		{M, F, _} = MFA) when is_atom(M), is_atom(F) ->
  ets_table_lookup(ETSContracts, MFA).

-spec lookup_callbacks(plt(), module()) ->
	 'none' | {'value', [{mfa(), dialyzer_contracts:file_contract()}]}.

lookup_callbacks(#plt{callbacks = ETSCallbacks}, Mod) when is_atom(Mod) ->
  ets_table_lookup(ETSCallbacks, Mod).

-type ret_args_types() :: {erl_types:erl_type(), [erl_types:erl_type()]}.

-spec insert_list(plt(), [{mfa() | integer(), ret_args_types()}]) -> plt().

insert_list(#plt{info = Info} = PLT, List) ->
  true = ets:insert(Info, List),
  PLT.

-spec lookup(plt(), integer() | mfa_patt()) ->
        'none' | {'value', ret_args_types()}.

lookup(Plt, {M, F, _} = MFA) when is_atom(M), is_atom(F) ->
  lookup_1(Plt, MFA);
lookup(Plt, Label) when is_integer(Label) ->
  lookup_1(Plt, Label).

lookup_1(#plt{info = Info}, MFAorLabel) ->
  ets_table_lookup(Info, MFAorLabel).

-spec insert_types(plt(), ets:tid()) -> plt().

insert_types(PLT, Records) ->
  ok = dialyzer_utils:ets_move(Records, PLT#plt.types),
  PLT.

-spec insert_exported_types(plt(), ets:tid()) -> plt().

insert_exported_types(PLT, ExpTypes) ->
  ok = dialyzer_utils:ets_move(ExpTypes, PLT#plt.exported_types),
  PLT.

-spec get_module_types(plt(), atom()) ->
                          'none' | {'value', erl_types:type_table()}.

get_module_types(#plt{types = Types}, M) when is_atom(M) ->
  ets_table_lookup(Types, M).

-spec get_exported_types(plt()) -> sets:set().

get_exported_types(#plt{exported_types = ETSExpTypes}) ->
  sets:from_list([E || {E} <- table_to_list(ETSExpTypes)]).

-type mfa_types() :: {mfa(), erl_types:erl_type(), [erl_types:erl_type()]}.

-spec lookup_module(plt(), atom()) -> 'none' | {'value', [mfa_types()]}.

lookup_module(#plt{info = Info}, M) when is_atom(M) ->
  table_lookup_module(Info, M).

-spec all_modules(plt()) -> sets:set().

all_modules(#plt{info = Info, contracts = Cs}) ->
  sets:union(table_all_modules(Info), table_all_modules(Cs)).

-spec contains_mfa(plt(), mfa()) -> boolean().

contains_mfa(#plt{info = Info, contracts = Contracts}, MFA) ->
  ets:member(Info, MFA) orelse ets:member(Contracts, MFA).

-spec get_default_plt() -> file:filename().

get_default_plt() ->
  case os:getenv("DIALYZER_PLT") of
    false ->
      {ok,[[HomeDir]]} = init:get_argument(home),
      filename:join(HomeDir, ".dialyzer_plt");
    UserSpecPlt -> UserSpecPlt
  end.

-spec plt_and_info_from_file(file:filename()) -> {plt(), plt_info()}.

plt_and_info_from_file(FileName) ->
  from_file(FileName, true).

-spec from_file(file:filename()) -> plt().

from_file(FileName) ->
  from_file(FileName, false).

from_file(FileName, ReturnInfo) ->
  Plt = new(),
  Fun = fun() -> from_file1(Plt, FileName, ReturnInfo) end,
  case subproc(Fun) of
    {ok, Return} ->
      Return;
    {error, Msg} ->
      delete(Plt),
      plt_error(Msg)
  end.

from_file1(Plt, FileName, ReturnInfo) ->
  case get_record_from_file(FileName) of
    {ok, Rec} ->
      case check_version(Rec) of
	error ->
	  Msg = io_lib:format("Old PLT file ~ts\n", [FileName]),
          {error, Msg};
	ok ->
          #file_plt{info = FileInfo,
                    contracts = FileContracts,
                    callbacks = FileCallbacks,
                    types = FileTypes,
                    exported_types = FileExpTypes} = Rec,
          Types = [{Mod, maps:from_list(dict:to_list(Types))} ||
                    {Mod, Types} <- dict:to_list(FileTypes)],
          CallbacksList = dict:to_list(FileCallbacks),
          CallbacksByModule =
            [{M, [Cb || {{M1,_,_},_} = Cb <- CallbacksList, M1 =:= M]} ||
              M <- lists:usort([M || {{M,_,_},_} <- CallbacksList])],
          #plt{info = ETSInfo,
               types = ETSTypes,
               contracts = ETSContracts,
               callbacks = ETSCallbacks,
               exported_types = ETSExpTypes} = Plt,
          [true, true, true] =
            [ets:insert(ETS, Data) ||
              {ETS, Data} <- [{ETSInfo, dict:to_list(FileInfo)},
                              {ETSTypes, Types},
                              {ETSContracts, dict:to_list(FileContracts)}]],
          true = ets:insert(ETSCallbacks, CallbacksByModule),
          true = ets:insert(ETSExpTypes, [{ET} ||
                                           ET <- sets:to_list(FileExpTypes)]),
	  case ReturnInfo of
	    false -> {ok, Plt};
	    true ->
	      PltInfo = {Rec#file_plt.file_md5_list,
			 Rec#file_plt.mod_deps},
	      {ok, {Plt, PltInfo}}
	  end
      end;
    {error, Reason} ->
      Msg = io_lib:format("Could not read PLT file ~ts: ~p\n",
			  [FileName, Reason]),
      {error, Msg}
  end.

-type err_rsn() :: 'not_valid' | 'no_such_file' | 'read_error'.

-spec included_files(file:filename()) -> {'ok', [file:filename()]}
				      |  {'error', err_rsn()}.

included_files(FileName) ->
  Fun = fun() -> included_files1(FileName) end,
  subproc(Fun).

included_files1(FileName) ->
  case get_record_from_file(FileName) of
    {ok, #file_plt{file_md5_list = Md5}} ->
      {ok, [File || {File, _} <- Md5]};
    {error, _What} = Error ->
      Error
  end.

check_version(#file_plt{version = ?VSN, implementation_md5 = ImplMd5}) ->
  case compute_new_md5(ImplMd5, [], []) of
    ok -> ok;
    {differ, _, _} -> error;
    {error, _} -> error
  end;
check_version(#file_plt{}) -> error.

get_record_from_file(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      try binary_to_term(Bin) of
	  #file_plt{} = FilePLT -> {ok, FilePLT};
	  _ -> {error, not_valid}
      catch
	_:_ -> {error, not_valid}
      end;
    {error, enoent} ->
      {error, no_such_file};
    {error, _} ->
      {error, read_error}
  end.

-spec merge_plts([plt()]) -> plt().

%% One of the PLTs of the list is augmented with the contents of the
%% other PLTs, and returned. The other PLTs are deleted.

merge_plts(List) ->
  {InfoList, TypesList, ExpTypesList, ContractsList, CallbacksList} =
    group_fields(List),
  #plt{info = table_merge(InfoList),
       types = table_merge(TypesList),
       exported_types = sets_merge(ExpTypesList),
       contracts = table_merge(ContractsList),
       callbacks = table_merge(CallbacksList)
      }.

-spec merge_disj_plts([plt()]) -> plt().

%% One of the PLTs of the list is augmented with the contents of the
%% other PLTs, and returned. The other PLTs are deleted.
%%
%% The keys are compared when checking for disjointness. Sometimes the
%% key is a module(), sometimes an mfa(). It boils down to checking if
%% any module occurs more than once.
merge_disj_plts(List) ->
  {InfoList, TypesList, ExpTypesList, ContractsList, CallbacksList} =
    group_fields(List),
  #plt{info = table_disj_merge(InfoList),
       types = table_disj_merge(TypesList),
       exported_types = sets_disj_merge(ExpTypesList),
       contracts = table_disj_merge(ContractsList),
       callbacks = table_disj_merge(CallbacksList)
      }.

group_fields(List) ->
  InfoList = [Info || #plt{info = Info} <- List],
  TypesList = [Types || #plt{types = Types} <- List],
  ExpTypesList = [ExpTypes || #plt{exported_types = ExpTypes} <- List],
  ContractsList = [Contracts || #plt{contracts = Contracts} <- List],
  CallbacksList = [Callbacks || #plt{callbacks = Callbacks} <- List],
  {InfoList, TypesList, ExpTypesList, ContractsList, CallbacksList}.

-spec merge_plts_or_report_conflicts([file:filename()], [plt()]) -> plt().

merge_plts_or_report_conflicts(PltFiles, Plts) ->
  try
    merge_disj_plts(Plts)
  catch throw:{dialyzer_error, not_disjoint_plts} ->
      IncFiles = lists:append([begin {ok, Fs} = included_files(F), Fs end
                               || F <- PltFiles]),
      ConfFiles = find_duplicates(IncFiles),
      Msg = io_lib:format("Could not merge PLTs since they are not disjoint\n"
                          "The following files are included in more than one "
                          "PLTs:\n~tp\n", [ConfFiles]),
      plt_error(Msg)
  end.

find_duplicates(List) ->
  ModList = [filename:basename(E) || E <- List],
  SortedList = lists:usort(ModList),
  lists:usort(ModList -- SortedList).
  
-spec to_file(file:filename(), plt(), mod_deps(), {[file_md5()], mod_deps()}) -> 'ok'.

%% Write the PLT to file, and deletes the PLT.
to_file(FileName, Plt, ModDeps, MD5_OldModDeps) ->
  Fun = fun() -> to_file1(FileName, Plt, ModDeps, MD5_OldModDeps) end,
  Return = subproc(Fun),
  delete(Plt),
  case Return of
    ok -> ok;
    Thrown -> throw(Thrown)
  end.

to_file1(FileName,
	#plt{info = ETSInfo, types = ETSTypes, contracts = ETSContracts,
	     callbacks = ETSCallbacks, exported_types = ETSExpTypes},
	ModDeps, {MD5, OldModDeps}) ->
  NewModDeps = dict:merge(fun(_Key, OldVal, NewVal) ->
			      ordsets:union(OldVal, NewVal)
			  end,
			  OldModDeps, ModDeps),
  ImplMd5 = compute_implementation_md5(),
  CallbacksList =
      [Cb ||
        {_M, Cbs} <- tab2list(ETSCallbacks),
        Cb <- Cbs],
  Callbacks = dict:from_list(CallbacksList),
  Info = dict:from_list(tab2list(ETSInfo)),
  Types = tab2list(ETSTypes),
  Contracts = dict:from_list(tab2list(ETSContracts)),
  ExpTypes = sets:from_list([E || {E} <- tab2list(ETSExpTypes)]),
  FileTypes = dict:from_list([{Mod, dict:from_list(maps:to_list(MTypes))} ||
                               {Mod, MTypes} <- Types]),
  Record = #file_plt{version = ?VSN,
		     file_md5_list = MD5,
		     info = Info,
		     contracts = Contracts,
		     callbacks = Callbacks,
		     types = FileTypes,
                     exported_types = ExpTypes,
		     mod_deps = NewModDeps,
		     implementation_md5 = ImplMd5},
  Bin = term_to_binary(Record, [compressed]),
  case file:write_file(FileName, Bin) of
    ok -> ok;
    {error, Reason} ->
      Msg = io_lib:format("Could not write PLT file ~ts: ~w\n",
			  [FileName, Reason]),
      {dialyzer_error, Msg}
  end.

-type md5_diff()    :: [{'differ', atom()} | {'removed', atom()}].
-type check_error() :: err_rsn() | {'no_file_to_remove', file:filename()}.

-spec check_plt(file:filename(), [file:filename()], [file:filename()]) ->
	 'ok'
       | {'error', check_error()}
       | {'differ', [file_md5()], md5_diff(), mod_deps()}
       | {'old_version', [file_md5()]}.

check_plt(FileName, RemoveFiles, AddFiles) ->
  Fun = fun() -> check_plt1(FileName, RemoveFiles, AddFiles) end,
  subproc(Fun).

check_plt1(FileName, RemoveFiles, AddFiles) ->
  case get_record_from_file(FileName) of
    {ok, #file_plt{file_md5_list = Md5, mod_deps = ModDeps} = Rec} ->
      case check_version(Rec) of
	ok ->
	  case compute_new_md5(Md5, RemoveFiles, AddFiles) of
	    ok -> ok;
	    {differ, NewMd5, DiffMd5} -> {differ, NewMd5, DiffMd5, ModDeps};
	    {error, _What} = Err -> Err
	  end;
	error ->
	  case compute_new_md5(Md5, RemoveFiles, AddFiles) of
	    ok -> {old_version, Md5};
	    {differ, NewMd5, _DiffMd5} -> {old_version, NewMd5};
	    {error, _What} = Err -> Err
	  end
      end;
    Error -> Error
  end.

compute_new_md5(Md5, [], []) ->
  compute_new_md5_1(Md5, [], []);
compute_new_md5(Md5, RemoveFiles0, AddFiles0) ->
  %% Assume that files are first removed and then added. Files that
  %% are both removed and added will be checked for consistency in the
  %% normal way. If they have moved, we assume that they differ.
  RemoveFiles = RemoveFiles0 -- AddFiles0,
  AddFiles = AddFiles0 -- RemoveFiles0,
  InitDiffList = init_diff_list(RemoveFiles, AddFiles),
  case init_md5_list(Md5, RemoveFiles, AddFiles) of
    {ok, NewMd5} -> compute_new_md5_1(NewMd5, [], InitDiffList);
    {error, _What} = Error -> Error
  end.

compute_new_md5_1([{File, Md5} = Entry|Entries], NewList, Diff) ->
  case compute_md5_from_file(File) of
    Md5 -> compute_new_md5_1(Entries, [Entry|NewList], Diff);
    NewMd5 ->
      ModName = beam_file_to_module(File),
      compute_new_md5_1(Entries, [{File, NewMd5}|NewList], [{differ, ModName}|Diff])
  end;
compute_new_md5_1([], _NewList, []) ->
  ok;
compute_new_md5_1([], NewList, Diff) ->
  {differ, lists:keysort(1, NewList), Diff}.

-spec compute_implementation_md5() -> [file_md5()].

compute_implementation_md5() ->
  Dir = code:lib_dir(hipe),
  Files1 = ["erl_bif_types.beam", "erl_types.beam"],
  Files2 = [filename:join([Dir, "ebin", F]) || F <- Files1],
  compute_md5_from_files(Files2).

-spec compute_md5_from_files([file:filename()]) -> [file_md5()].

compute_md5_from_files(Files) ->
  lists:keysort(1, [{F, compute_md5_from_file(F)} || F <- Files]).

compute_md5_from_file(File) ->
  case beam_lib:all_chunks(File) of
    {ok, _, Chunks} ->
      %% We cannot use beam_lib:md5 because it does not consider
      %% the debug_info chunk, where typespecs are likely stored.
      %% So we consider almost all chunks except the useless ones.
      Filtered = [[ID, Chunk] || {ID, Chunk} <- Chunks, ID =/= "CInf", ID =/= "Docs"],
      erlang:md5(lists:sort(Filtered));
    {error, beam_lib, {file_error, _, enoent}} ->
      Msg = io_lib:format("File not found: ~ts\n", [File]),
      throw({dialyzer_error, Msg});
    {error, beam_lib, _} ->
      Msg = io_lib:format("Could not compute MD5 for .beam: ~ts\n", [File]),
      throw({dialyzer_error, Msg})
  end.

init_diff_list(RemoveFiles, AddFiles) ->
  RemoveSet0 = sets:from_list([beam_file_to_module(F) || F <- RemoveFiles]),
  AddSet0 = sets:from_list([beam_file_to_module(F) || F <- AddFiles]),
  DiffSet = sets:intersection(AddSet0, RemoveSet0),
  RemoveSet = sets:subtract(RemoveSet0, DiffSet),
  %% Added files and diff files will appear as diff files from the md5 check.
  [{removed, F} || F <- sets:to_list(RemoveSet)].

init_md5_list(Md5, RemoveFiles, AddFiles) ->
  Files = [{remove, F} || F <- RemoveFiles] ++ [{add, F} || F <- AddFiles],
  DiffFiles = lists:keysort(2, Files),
  Md5Sorted = lists:keysort(1, Md5),
  init_md5_list_1(Md5Sorted, DiffFiles, []).

init_md5_list_1([{File, _Md5}|Md5Left], [{remove, File}|DiffLeft], Acc) ->
  init_md5_list_1(Md5Left, DiffLeft, Acc);
init_md5_list_1([{File, _Md5} = Entry|Md5Left], [{add, File}|DiffLeft], Acc) ->
  init_md5_list_1(Md5Left, DiffLeft, [Entry|Acc]);
init_md5_list_1([{File1, _Md5} = Entry|Md5Left] = Md5List,
		[{Tag, File2}|DiffLeft] = DiffList, Acc) ->
  case File1 < File2 of
    true -> init_md5_list_1(Md5Left, DiffList, [Entry|Acc]);
    false ->
      %% Just an assert.
      true = File1 > File2,
      case Tag of
	add -> init_md5_list_1(Md5List, DiffLeft, [{File2, <<>>}|Acc]);
	remove -> {error, {no_file_to_remove, File2}}
      end
  end;
init_md5_list_1([], DiffList, Acc) ->
  AddFiles = [{F, <<>>} || {add, F} <- DiffList],
  {ok, lists:reverse(Acc, AddFiles)};
init_md5_list_1(Md5List, [], Acc) ->
  {ok, lists:reverse(Acc, Md5List)}.

-spec delete(plt()) -> 'ok'.

delete(#plt{info = ETSInfo,
            types = ETSTypes,
            contracts = ETSContracts,
            callbacks = ETSCallbacks,
            exported_types = ETSExpTypes}) ->
  true = ets:delete(ETSContracts),
  true = ets:delete(ETSTypes),
  true = ets:delete(ETSInfo),
  true = ets:delete(ETSCallbacks),
  true = ets:delete(ETSExpTypes),
  ok.

tab2list(Tab) ->
  dialyzer_utils:ets_tab2list(Tab).

subproc(Fun) ->
  F = fun() -> exit(Fun()) end,
  {Pid, Ref} = erlang:spawn_monitor(F),
  receive {'DOWN', Ref, process, Pid, Return} ->
      Return
  end.

%%---------------------------------------------------------------------------
%% Edoc

-spec get_specs(plt()) -> string().

get_specs(#plt{info = Info}) ->
  %% TODO: Should print contracts as well.
  L = lists:sort([{MFA, Val} ||
                   {{_,_,_} = MFA, Val} <- table_to_list(Info)]),
  lists:flatten(create_specs(L, [])).

beam_file_to_module(Filename) ->
  list_to_atom(filename:basename(Filename, ".beam")).

-spec get_specs(plt(), atom(), atom(), arity_patt()) -> 'none' | string().

get_specs(#plt{info = Info}, M, F, A) when is_atom(M), is_atom(F) ->
  MFA = {M, F, A},
  case ets_table_lookup(Info, MFA) of
    none -> none;
    {value, Val} -> lists:flatten(create_specs([{MFA, Val}], []))
  end.

create_specs([{{M, F, _A}, {Ret, Args}}|Left], M) ->
  [io_lib:format("-spec ~tw(~ts) -> ~ts\n",
		 [F, expand_args(Args), erl_types:t_to_string(Ret)])
   | create_specs(Left, M)];
create_specs(List = [{{M, _F, _A}, {_Ret, _Args}}| _], _M) ->
  [io_lib:format("\n\n%% ------- Module: ~w -------\n\n", [M])
   | create_specs(List, M)];
create_specs([], _) ->
  [].

expand_args([]) ->
  [];
expand_args([ArgType]) ->
  case erl_types:t_is_any(ArgType) of
    true -> ["_"];
    false -> [erl_types:t_to_string(ArgType)]
  end;
expand_args([ArgType|Left]) ->
  [case erl_types:t_is_any(ArgType) of
     true -> "_";
     false -> erl_types:t_to_string(ArgType)
   end ++
   ","|expand_args(Left)].

-spec plt_error(deep_string()) -> no_return().

plt_error(Msg) ->
  throw({dialyzer_error, lists:flatten(Msg)}).

%%---------------------------------------------------------------------------
%% Ets table

table_to_list(Plt) ->
  ets:tab2list(Plt).

table_delete_module(Tab, Mod) ->
  MS = ets:fun2ms(fun({{M, _F, _A}, _Val}) -> M =:= Mod;
                     ({_, _}) -> false
                  end),
  _NumDeleted = ets:select_delete(Tab, MS),
  Tab.

table_delete_module1(Tab, Mod) ->
  MS = ets:fun2ms(fun({{M, _F, _A}}) -> M =:= Mod end),
  _NumDeleted = ets:select_delete(Tab, MS),
  Tab.

table_delete_module2(Tab, Mod) ->
  true = ets:delete(Tab, Mod),
  Tab.

ets_table_delete_list(Tab, [H|T]) ->
  ets:delete(Tab, H),
  ets_table_delete_list(Tab, T);
ets_table_delete_list(Tab, []) ->
  Tab.

ets_table_lookup(Plt, Obj) ->
  try ets:lookup_element(Plt, Obj, 2) of
      Val -> {value, Val}
  catch
    _:_ -> none
  end.

table_lookup_module(Tab, Mod) ->
  MS = ets:fun2ms(fun({{M, F, A}, V}) when M =:= Mod ->
                      {{M, F, A}, V} end),
  List = [begin
            {V1, V2} = V,
            {MFA, V1, V2}
          end || {MFA, V} <- ets:select(Tab, MS)],
  case List =:= [] of
    true -> none;
    false -> {value, List}
  end.

table_all_modules(Tab) ->
  Ks = ets:match(Tab, {'$1', '_'}, 100),
  all_mods(Ks, sets:new()).

all_mods('$end_of_table', S) ->
  S;
all_mods({ListsOfKeys, Cont}, S) ->
  S1 = lists:foldl(fun([{M, _F, _A}], S0) -> sets:add_element(M, S0)
                   end, S, ListsOfKeys),
  all_mods(ets:match(Cont), S1).

table_merge([H|T]) ->
  table_merge(T, H).

table_merge([], Acc) ->
  Acc;
table_merge([Plt|Plts], Acc) ->
  NewAcc = merge_tables(Plt, Acc),
  table_merge(Plts, NewAcc).

table_disj_merge([H|T]) ->
  table_disj_merge(T, H).

table_disj_merge([], Acc) ->
  Acc;
table_disj_merge([Plt|Plts], Acc) ->
  case table_is_disjoint(Plt, Acc) of
    true ->
      NewAcc = merge_tables(Plt, Acc),
      table_disj_merge(Plts, NewAcc);
    false -> throw({dialyzer_error, not_disjoint_plts})
  end.

sets_merge([H|T]) ->
  sets_merge(T, H).

sets_merge([], Acc) ->
  Acc;
sets_merge([Plt|Plts], Acc) ->
  NewAcc = merge_tables(Plt, Acc),
  sets_merge(Plts, NewAcc).

sets_disj_merge([H|T]) ->
  sets_disj_merge(T, H).

sets_disj_merge([], Acc) ->
  Acc;
sets_disj_merge([Plt|Plts], Acc) ->
  case table_is_disjoint(Plt, Acc) of
    true ->
      NewAcc = merge_tables(Plt, Acc),
      sets_disj_merge(Plts, NewAcc);
    false -> throw({dialyzer_error, not_disjoint_plts})
  end.

table_is_disjoint(T1, T2) ->
  tab_is_disj(ets:first(T1), T1, T2).

tab_is_disj('$end_of_table', _T1, _T2) ->
  true;
tab_is_disj(K1, T1, T2) ->
  case ets:member(T2, K1) of
    false ->
      tab_is_disj(ets:next(T1, K1), T1, T2);
    true ->
      false
  end.

merge_tables(T1, T2) ->
  tab_merge(ets:first(T1), T1, T2).

tab_merge('$end_of_table', T1, T2) ->
  case ets:first(T1) of % no safe_fixtable()...
    '$end_of_table' ->
      true = ets:delete(T1),
      T2;
    Key ->
      tab_merge(Key, T1, T2)
  end;
tab_merge(K1, T1, T2) ->
  Vs = ets:lookup(T1, K1),
  NextK1 = ets:next(T1, K1),
  true = ets:delete(T1, K1),
  true = ets:insert(T2, Vs),
  tab_merge(NextK1, T1, T2).

%%---------------------------------------------------------------------------
%% Debug utilities.

-spec pp_non_returning() -> 'ok'.

pp_non_returning() ->
  PltFile = get_default_plt(),
  Plt = from_file(PltFile),
  List = table_to_list(Plt#plt.info),
  Unit = [{MFA, erl_types:t_fun(Args, Ret)} || {MFA, {Ret, Args}} <- List,
						erl_types:t_is_unit(Ret)],
  None = [{MFA, erl_types:t_fun(Args, Ret)} || {MFA, {Ret, Args}} <- List,
						erl_types:t_is_none(Ret)],
  io:format("=========================================\n"),
  io:format("=                Loops                  =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M, F, _}, Type}) ->
		    io:format("~w:~tw~ts.\n",
			      [M, F, dialyzer_utils:format_sig(Type)])
		end, lists:sort(Unit)),
  io:format("\n"),
  io:format("=========================================\n"),
  io:format("=                Errors                 =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M, F, _}, Type}) ->
		    io:format("~w:~w~s.\n",
			      [M, F, dialyzer_utils:format_sig(Type)])
		end, lists:sort(None)),
  delete(Plt).

-spec pp_mod(atom()) -> 'ok'.

pp_mod(Mod) when is_atom(Mod) ->
  PltFile = get_default_plt(),
  Plt = from_file(PltFile),
  case lookup_module(Plt, Mod) of
    {value, List} ->
      lists:foreach(fun({{_, F, _}, Ret, Args}) ->
			T = erl_types:t_fun(Args, Ret),
			S = dialyzer_utils:format_sig(T),
			io:format("-spec ~tw~ts.\n", [F, S])
		    end, lists:sort(List));
    none ->
      io:format("dialyzer: Found no module named '~s' in the PLT\n", [Mod])
  end,
  delete(Plt).
