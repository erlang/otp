%% -*- erlang-indent-level: 2 -*-
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
%% =======================================================================
%%  Filename : 	hipe_unified_loader.erl
%%  Module   :	hipe_unified_loader
%%  Purpose  :  To load code into memory and link it to the system.
%%  Notes    :  See hipe_ext_format.hrl for description of the external 
%%              format.
%% =======================================================================
%% TODO:
%%   Problems with the order in which things are done.
%%   export_funs should atomically patch references to make fe and
%%   make beam stubs. !!
%%
%%   Each function should have two proper databases.
%%   Describe the patch algorithm:
%%     For each function MFA that is (re)compiled to Address:
%%     1.  For the old MFA 
%%         a. RefsTo = MFA->refers_to
%%         b. for each {F,Adr} in RefsTo: remove Adr from F->is_referred
%%         c. RefsFrom = MFA->is_referred
%%         d. For each {Adr,Type} in RefsFrom: 
%%                update instr at Adr to refer to Address instead.
%%     2.  For the new MFA
%%         a. MFA->is_referred=RefsFrom 
%%     3.  For each function F referenced in the code at Offset:
%%                 add {Address+Offset,Type} to F->is_referred
%%                 add {F,Address+Offset} to MFA->refers_to
%%     4.  Make Address the entrypoint for MFA
%%
%%   Add exporting of exported constants.
%%   Add freeing of old code. 
%%   Inline hipe_sparc_ext_format somehow.
%% =======================================================================

-module(hipe_unified_loader).

-compile(no_native).
% 'no_native' is a workaround to avoid "The code server called unloaded module"
% caused by Mod:module_info(exports) in patch_to_emu_step1() called by post_beam_load.
% Reproducable with hipelibs and asn1_SUITE.
% I think the real solution would be to let BIF erlang:load_module/2 redirect all
% hipe calls to the module and thereby remove post_beam_load.

% SVERK: Can we remove -compile(no_native) now when post_beam_load is gone?

-export([chunk_name/1,
	 %% Only the code and code_server modules may call the entries below!
	 load_native_code/3,
	 load_module/4,
	 load/3]).

%%-define(DEBUG,true).
-define(DO_ASSERT,true).
-define(HIPE_LOGGING,true).

-include("../../hipe/main/hipe.hrl").
-include("hipe_ext_format.hrl").

%% Currently, there is no need to expose these to the outside world.
-define(HS8P_TAG,"HS8P").
-define(HPPC_TAG,"HPPC").
-define(HP64_TAG,"HP64").
-define(HARM_TAG,"HARM").
-define(HX86_TAG,"HX86").
-define(HA64_TAG,"HA64").

%%========================================================================

-spec chunk_name(hipe_architecture()) -> string().
%% @doc
%%    Returns the native code chunk name of the Architecture.
%%    (On which presumably we are running.)

chunk_name(Architecture) ->
  case Architecture of
    amd64 ->      ?HA64_TAG; %% HiPE, x86_64, (implicit: 64-bit, Unix)
    arm ->	  ?HARM_TAG; %% HiPE, arm, v5 (implicit: 32-bit, Linux)
    powerpc ->    ?HPPC_TAG; %% HiPE, PowerPC (implicit: 32-bit, Linux)
    ppc64 ->	  ?HP64_TAG; %% HiPE, ppc64 (implicit: 64-bit, Linux)
    ultrasparc -> ?HS8P_TAG; %% HiPE, SPARC, V8+ (implicit: 32-bit)
    x86 ->        ?HX86_TAG  %% HiPE, x86, (implicit: Unix)
    %% Future:     HSV9      %% HiPE, SPARC, V9 (implicit: 64-bit)
    %%             HW32      %% HiPE, x86, Win32 
  end.

word_size(Architecture) ->
  case Architecture of
    amd64 -> 8;
    ppc64 -> 8;
    _ -> 4
  end.

%%========================================================================

-spec load_native_code(Mod, binary(), hipe_architecture()) ->
                          'no_native' | {'module', Mod} when Mod :: atom().
%% @doc
%%    Loads the native code of a module Mod.
%%    Returns {module,Mod} on success (for compatibility with
%%    code:load_file/1) and the atom `no_native' on failure.

load_native_code(_Mod, _Bin, undefined) ->
  no_native;
load_native_code(Mod, Bin, Architecture) when is_atom(Mod), is_binary(Bin) ->
  case code:get_chunk(Bin, chunk_name(Architecture)) of
    undefined -> no_native;
    NativeCode when is_binary(NativeCode) ->
      erlang:system_flag(multi_scheduling, block_normal),
      try
        put(hipe_patch_closures, false),
        case load_common(Mod, NativeCode, Bin, Architecture) of
          bad_crc -> no_native;
          Result -> Result
        end
      after
        erlang:system_flag(multi_scheduling, unblock_normal)
      end
  end.

%%========================================================================

version_check(Version, Mod) when is_atom(Mod) ->
  Ver = ?VERSION_STRING(),
  case Version < Ver of
    true -> 
      ?msg("WARNING: Module ~w was compiled with HiPE version ~s\n",
	   [Mod, Version]);
    _ -> ok
  end.

%%========================================================================

-spec load_module(Mod, binary(), _, hipe_architecture()) ->
                     'bad_crc' | {'module', Mod} when Mod :: atom().

load_module(Mod, Bin, Beam, Architecture) ->
  erlang:system_flag(multi_scheduling, block_normal),
  try
    put(hipe_patch_closures, false),
    load_common(Mod, Bin, Beam, Architecture)
  after
    erlang:system_flag(multi_scheduling, unblock_normal)
  end.


%%========================================================================

-spec load(Mod, binary(), hipe_architecture()) ->
              'bad_crc' | {'module', Mod} when Mod :: atom().

load(Mod, Bin, Architecture) ->
  erlang:system_flag(multi_scheduling, block_normal),
  try
    ?debug_msg("********* Loading funs in module ~w *********\n",[Mod]),
    %% Loading just some functions in a module; patch closures separately.
    put(hipe_patch_closures, true),
    load_common(Mod, Bin, [], Architecture)
  after
    erlang:system_flag(multi_scheduling, unblock_normal)
  end.

%%------------------------------------------------------------------------

load_common(Mod, Bin, Beam, Architecture) ->
  %% Unpack the binary.
  [{Version, CheckSum},
   ConstAlign, ConstSize, ConstMap, LabelMap, ExportMap,
   CodeSize,  CodeBinary,  Refs,
   0,[] % ColdSize, CRrefs
  ] = binary_to_term(Bin),
  MD5 = erlang:md5(Bin), % use md5 of actual running code for module_info
  ?debug_msg("***** ErLLVM *****~nVersion: ~s~nCheckSum: ~w~nConstAlign: ~w~n" ++
    "ConstSize: ~w~nConstMap: ~w~nLabelMap: ~w~nExportMap ~w~nRefs ~w~n",
    [Version, CheckSum, ConstAlign, ConstSize, ConstMap, LabelMap, ExportMap,
      Refs]),
  %% Write HiPE binary code to a file in the current directory in order to
  %% debug by disassembling.
  %% file:write_file("erl.o", CodeBinary, [binary]),
  %% Check that we are loading up-to-date code.
  version_check(Version, Mod),
  case hipe_bifs:check_crc(CheckSum) of
    false ->
      ?msg("Warning: not loading native code for module ~w: "
	   "it was compiled for an incompatible runtime system; "
	   "please regenerate native code for this runtime system\n", [Mod]),
      bad_crc;
    true ->
      put(closures_to_patch, []),
      WordSize = word_size(Architecture),
      WriteWord = write_word_fun(WordSize),
      LoaderState = hipe_bifs:alloc_loader_state(Mod),
      put(hipe_loader_state, LoaderState),
      %% Create data segment
      {ConstAddr,ConstMap2} =
	create_data_segment(ConstAlign, ConstSize, ConstMap, WriteWord,
			    LoaderState),
      %% Find callees for which we may need trampolines.
      CalleeMFAs = find_callee_mfas(Refs, Architecture),
      %% Write the code to memory.
      {CodeAddress,Trampolines} =
	enter_code(CodeSize, CodeBinary, CalleeMFAs, LoaderState),
      %% Construct CalleeMFA-to-trampoline mapping.
      TrampolineMap = mk_trampoline_map(CalleeMFAs, Trampolines,
                                        Architecture),
      %% Patch references to code labels in data seg.
      ok = patch_consts(LabelMap, ConstAddr, CodeAddress, WriteWord),

      %% Find out which functions are being loaded (and where).
      %% Note: FunDefs are sorted descending address order.
      FunDefs = exports(ExportMap, CodeAddress),

      %% Patch all dynamic references in the code.
      %%  Function calls, Atoms, Constants, System calls

      ok = patch(Refs, CodeAddress, ConstMap2, FunDefs, TrampolineMap),

      %% Tell the system where the loaded funs are. 
      %%  (patches the BEAM code to redirect to native.)
      case Beam of
	[] ->
	  %% This module was previously loaded as BEAM code during system
	  %% start-up before the code server has started (-enable-native-libs
	  %% is active), so we must now patch the pre-existing entries in the
	  %% fun table with the native code addresses for all closures.
	  lists:foreach(fun({FE, DestAddress}) ->
			    hipe_bifs:set_native_address_in_fe(FE, DestAddress)
			end, erase(closures_to_patch)),
	  set_beam_call_traps(FunDefs),
	  export_funs(FunDefs),
          ok = hipe_bifs:commit_patch_load(LoaderState),
          ok;
	BeamBinary when is_binary(BeamBinary) ->
	  %% Find all closures in the code.
	  [] = erase(closures_to_patch),	%Clean up, assertion.
	  ClosurePatches = find_closure_patches(Refs),
	  AddressesOfClosuresToPatch =
	    calculate_addresses(ClosurePatches, CodeAddress, FunDefs),
	  export_funs(FunDefs),
	  make_beam_stub(Mod, LoaderState, MD5, BeamBinary, FunDefs,
                         AddressesOfClosuresToPatch)
      end,

      %% Final clean up.
      _ = erase(hipe_loader_state),
      _ = erase(hipe_patch_closures),
      _ = erase(hipe_assert_code_area),
      ?debug_msg("****************Loader Finished****************\n", []),
      {module,Mod}  % for compatibility with code:load_file/1
  end.

%%----------------------------------------------------------------
%% Scan the list of patches and build a set (returned as a tuple)
%% of the callees for which we may need trampolines.
%%
find_callee_mfas(Patches, Architecture) when is_list(Patches) ->
  case needs_trampolines(Architecture) of
    true -> find_callee_mfas(Patches, gb_sets:empty(),
                             no_erts_trampolines(Architecture));
    _ -> []
  end.

needs_trampolines(Architecture) ->
  case Architecture of
    arm -> true;
    powerpc -> true;
    ppc64 -> true;
    amd64 -> true;
    _ -> false
  end.

no_erts_trampolines(Architecture) ->
  case Architecture of
    powerpc -> true;
    ppc64 -> true;
    _ -> false
  end.

find_callee_mfas([{Type,Data}|Patches], MFAs, SkipErtsSyms) ->
  NewMFAs =
    case ?EXT2PATCH_TYPE(Type) of
      call_local -> add_callee_mfas(Data, MFAs, SkipErtsSyms);
      call_remote -> add_callee_mfas(Data, MFAs, SkipErtsSyms);
      %% load_address(function) deliberately ignored
      _ -> MFAs
    end,
  find_callee_mfas(Patches, NewMFAs, SkipErtsSyms);
find_callee_mfas([], MFAs, _SkipErtsSyms) ->
  list_to_tuple(gb_sets:to_list(MFAs)).

add_callee_mfas([{DestMFA,_Offsets}|Refs], MFAs, SkipErtsSyms) ->
  NewMFAs =
    case SkipErtsSyms of
      true ->
	%% On PowerPC we put the runtime system below the
	%% 32M boundary, which allows BIFs and primops to
	%% be called with ba/bla instructions. Hence we do
	%% not need trampolines for BIFs or primops.
	case bif_address(DestMFA) of
	  false -> gb_sets:add_element(DestMFA, MFAs);
	  BifAddress when is_integer(BifAddress) -> MFAs
	end;
      false ->
	%% On ARM we also need trampolines for BIFs and primops.
	gb_sets:add_element(DestMFA, MFAs)
    end,
  add_callee_mfas(Refs, NewMFAs, SkipErtsSyms);
add_callee_mfas([], MFAs, _SkipErtsSyms) -> MFAs.

%%----------------------------------------------------------------
%%
mk_trampoline_map([], [], _) -> []; % archs not using trampolines
mk_trampoline_map(CalleeMFAs, Trampolines, Architecture) ->
  SizeofLong = word_size(Architecture),
  mk_trampoline_map(tuple_size(CalleeMFAs), CalleeMFAs,
		    Trampolines, SizeofLong, gb_trees:empty()).

mk_trampoline_map(I, CalleeMFAs, Trampolines, SizeofLong, Map) when I >= 1 ->
  MFA = element(I, CalleeMFAs),
  %% Trampoline = element(I, Trampolines),
  Skip = (I-1)*SizeofLong,
  <<_:Skip/binary-unit:8,
    Trampoline:SizeofLong/integer-unsigned-native-unit:8,
    _/binary>> = Trampolines,
  NewMap = gb_trees:insert(MFA, Trampoline, Map),
  mk_trampoline_map(I-1, CalleeMFAs, Trampolines, SizeofLong, NewMap);
mk_trampoline_map(0, _, _, _, Map) -> Map.

%%----------------------------------------------------------------
%%
trampoline_map_get(_, []) -> []; % archs not using trampolines
trampoline_map_get(MFA, Map) -> gb_trees:get(MFA, Map).

trampoline_map_lookup(_, []) -> []; % archs not using trampolines
trampoline_map_lookup(Primop, Map) ->
  case gb_trees:lookup(Primop, Map) of
    {value, X} -> X;
    _ -> []
  end.

%%------------------------------------------------------------------------

-record(fundef, {address     :: integer(),
		 mfa         :: mfa(),
		 is_closure  :: boolean(),
		 is_exported :: boolean()}).

exports(ExportMap, BaseAddress) ->
  exports(ExportMap, BaseAddress, []).

exports([Offset,M,F,A,IsClosure,IsExported|Rest], BaseAddress, FunDefs) ->
  case IsExported andalso erlang:is_builtin(M, F, A) of
    true ->
      exports(Rest, BaseAddress, FunDefs);
    _false ->
      MFA = {M,F,A},
      Address = BaseAddress + Offset,
      FunDef = #fundef{address=Address, mfa=MFA, is_closure=IsClosure,
		       is_exported=IsExported},
      exports(Rest, BaseAddress, [FunDef|FunDefs])
  end;
exports([], _, FunDefs) ->
  FunDefs.

mod({M,_F,_A}) -> M.

%%------------------------------------------------------------------------

calculate_addresses(PatchOffsets, Base, FunDefs) ->
  RemoteOrLocal = local, % closure code refs are local
  [{Data,
    offsets_to_addresses(Offsets, Base),
    get_native_address(DestMFA, FunDefs, RemoteOrLocal)} ||
    {{DestMFA,_,_}=Data,Offsets} <- PatchOffsets].

offsets_to_addresses(Os, Base) ->
  [{O+Base,load_fe} || O <- Os].

%%------------------------------------------------------------------------

find_closure_patches([{Type,Refs} | Rest]) ->
  case ?EXT2PATCH_TYPE(Type) of 
    load_address -> 
      find_closure_refs(Refs, Rest);
    _ ->
      find_closure_patches(Rest)
  end;
find_closure_patches([]) -> [].

find_closure_refs([{Dest,Offsets} | Rest], Refs) ->
  case Dest of
    {closure,Data} ->
      [{Data,Offsets}|find_closure_refs(Rest,Refs)];
    _ ->
      find_closure_refs(Rest,Refs)
  end;
find_closure_refs([], Refs) ->
  find_closure_patches(Refs).

%%------------------------------------------------------------------------

set_beam_call_traps([FunDef | FunDefs]) ->
  #fundef{address=Address, mfa=MFA, is_closure=IsClosure,
	  is_exported=_IsExported} = FunDef,
  ?IF_DEBUG({M,F,A} = MFA, no_debug),
  ?IF_DEBUG(
     case IsClosure of
       false ->
	 ?debug_msg("LINKING: ~w:~w/~w to (0x~.16b)\n",
		    [M,F,A, Address]);
       true ->
	 ?debug_msg("LINKING: ~w:~w/~w to closure (0x~.16b)\n",
		    [M,F,A, Address])
     end, no_debug),
  hipe_bifs:set_native_address(MFA, Address, IsClosure),
  set_beam_call_traps(FunDefs);
set_beam_call_traps([]) ->
  ok.

export_funs([FunDef | FunDefs]) ->
  #fundef{address=Address, mfa=MFA, is_closure=_IsClosure,
	  is_exported=IsExported} = FunDef,
  ?IF_DEBUG({M,F,A} = MFA, no_debug),
  ?IF_DEBUG(
     case _IsClosure of
       false ->
	 ?debug_msg("LINKING: ~w:~w/~w to (0x~.16b)\n",
		    [M,F,A, Address]);
       true ->
	 ?debug_msg("LINKING: ~w:~w/~w to closure (0x~.16b)\n",
		    [M,F,A, Address])
     end, no_debug),
  hipe_bifs:set_funinfo_native_address(MFA, Address, IsExported),
  export_funs(FunDefs);
export_funs([]) ->
  ok.

make_beam_stub(Mod, LoaderState, MD5, Beam, FunDefs, ClosuresToPatch) ->
  Fs = [{F,A,Address} || #fundef{address=Address, mfa={_M,F,A}} <- FunDefs],
  Mod = code:make_stub_module(LoaderState, Beam, {Fs,ClosuresToPatch,MD5}),
  ok.

%%========================================================================
%% Patching 
%%  @spec patch(refs(), BaseAddress::integer(), ConstAndZone::term(),
%%              FunDefs::term(), TrampolineMap::term()) -> 'ok'.
%%   @type refs()=[{RefType::integer(), Reflist::reflist()} | refs()]
%%
%%   @type reflist()=   [{Data::term(), Offsets::offests()}|reflist()]
%%   @type offsets()=   [Offset::integer() | offsets()]
%% @doc
%%  The patchlist is a list of lists of patches of a type.
%%  For each type the list of references is sorted so that several
%%  references to the same type of data come after each other
%%  (we use this to look up the address of a referred function only once).
%%

patch([{Type,SortedRefs}|Rest], CodeAddress, ConstMap2, FunDefs, TrampolineMap) ->
  ?debug_msg("Patching ~w at [~w+offset] with ~w\n",
	     [Type,CodeAddress,SortedRefs]),
  case ?EXT2PATCH_TYPE(Type) of 
    call_local -> 
      patch_call(SortedRefs, CodeAddress, FunDefs, 'local', TrampolineMap);
    call_remote ->
      patch_call(SortedRefs, CodeAddress, FunDefs, 'remote', TrampolineMap);
    Other -> 
      patch_all(Other, SortedRefs, CodeAddress, {ConstMap2,CodeAddress}, FunDefs)
  end,
  patch(Rest, CodeAddress, ConstMap2, FunDefs, TrampolineMap);
patch([], _, _, _, _) -> ok.

%%----------------------------------------------------------------
%% Handle a 'call_local' or 'call_remote' patch.
%%
patch_call([{DestMFA,Offsets}|SortedRefs], BaseAddress, FunDefs, RemoteOrLocal, TrampolineMap) ->
  case bif_address(DestMFA) of
    false ->
      %% Previous code used mfa_to_address(DestMFA, FunDefs)
      %% here for local calls. That is wrong because even local
      %% destinations may not be present in FunDefs: they may
      %% not have been compiled yet, or they may be BEAM-only
      %% functions (e.g. module_info).
      DestAddress = get_native_address(DestMFA, FunDefs, RemoteOrLocal),
      Trampoline = trampoline_map_get(DestMFA, TrampolineMap),
      patch_mfa_call_list(Offsets, BaseAddress, DestMFA, DestAddress, FunDefs, RemoteOrLocal, Trampoline);
    BifAddress when is_integer(BifAddress) ->
      Trampoline = trampoline_map_lookup(DestMFA, TrampolineMap),
      patch_bif_call_list(Offsets, BaseAddress, BifAddress, Trampoline)
  end,
  patch_call(SortedRefs, BaseAddress, FunDefs, RemoteOrLocal, TrampolineMap);
patch_call([], _, _, _, _) ->
  ok.

patch_bif_call_list([Offset|Offsets], BaseAddress, BifAddress, Trampoline) ->
  CallAddress = BaseAddress+Offset,
  ?ASSERT(assert_local_patch(CallAddress)),
  patch_call_insn(CallAddress, BifAddress, Trampoline),
  patch_bif_call_list(Offsets, BaseAddress, BifAddress, Trampoline);
patch_bif_call_list([], _, _, _) -> ok.

patch_mfa_call_list([Offset|Offsets], BaseAddress, DestMFA, DestAddress, FunDefs, RemoteOrLocal, Trampoline) ->
  CallAddress = BaseAddress+Offset,
  add_ref(DestMFA, CallAddress, FunDefs, 'call', Trampoline, RemoteOrLocal),
  ?ASSERT(assert_local_patch(CallAddress)),
  patch_call_insn(CallAddress, DestAddress, Trampoline),
  patch_mfa_call_list(Offsets, BaseAddress, DestMFA, DestAddress, FunDefs, RemoteOrLocal, Trampoline);
patch_mfa_call_list([], _, _, _, _, _, _) -> ok.

patch_call_insn(CallAddress, DestAddress, Trampoline) ->
  ?ASSERT(assert_local_patch(CallAddress)),
  hipe_bifs:patch_call(CallAddress, DestAddress, Trampoline).

%% ____________________________________________________________________
%% 

patch_all(Type, [{Dest,Offsets}|Rest], BaseAddress, ConstAndZone, FunDefs)->
  patch_all_offsets(Type, Dest, Offsets, BaseAddress, ConstAndZone, FunDefs),
  patch_all(Type, Rest, BaseAddress, ConstAndZone, FunDefs);
patch_all(_, [], _, _, _) -> ok.

patch_all_offsets(Type, Data, [Offset|Offsets], BaseAddress,
		  ConstAndZone, FunDefs) ->
  ?debug_msg("Patching ~w at [~w+~w] with ~w\n",
	     [Type,BaseAddress,Offset, Data]),
  Address = BaseAddress + Offset,
  patch_offset(Type, Data, Address, ConstAndZone, FunDefs),
  ?debug_msg("Patching done\n",[]),
  patch_all_offsets(Type, Data, Offsets, BaseAddress, ConstAndZone, FunDefs);
patch_all_offsets(_, _, [], _, _, _) -> ok.

%%----------------------------------------------------------------
%% Handle any patch type except 'call_local' or 'call_remote'.
%%
patch_offset(Type, Data, Address, ConstAndZone, FunDefs) ->
  case Type of
    load_address ->
      patch_load_address(Data, Address, ConstAndZone, FunDefs);
    load_atom ->
      Atom = Data,
      patch_atom(Address, Atom);
    sdesc ->
      patch_sdesc(Data, Address, ConstAndZone, FunDefs);
    x86_abs_pcrel ->
      patch_instr(Address, Data, x86_abs_pcrel)
    %% _ ->
    %%   ?error_msg("Unknown ref ~w ~w ~w\n", [Type, Address, Data]),
    %%   exit({unknown_reference, Type, Address, Data})
  end.

patch_atom(Address, Atom) ->
  ?ASSERT(assert_local_patch(Address)),
  patch_instr(Address, hipe_bifs:atom_to_word(Atom), atom).

patch_sdesc(?STACK_DESC(SymExnRA, FSize, Arity, Live),
	    Address, {_ConstMap2,CodeAddress}, FunDefs) ->
  ExnRA =
    case SymExnRA of
      [] -> 0; % No catch
      LabelOffset -> CodeAddress + LabelOffset
    end,
  ?ASSERT(assert_local_patch(Address)),
  MFA = address_to_mfa_lth(Address, FunDefs),
  hipe_bifs:enter_sdesc({Address, ExnRA, FSize, Arity, Live, MFA},
		       get(hipe_loader_state)).


%%----------------------------------------------------------------
%% Handle a 'load_address'-type patch.
%%
patch_load_address(Data, Address, ConstAndZone, FunDefs) ->
  case Data of
    {local_function,DestMFA} ->
      patch_load_mfa(Address, DestMFA, FunDefs, 'local');
    {remote_function,DestMFA} ->
      patch_load_mfa(Address, DestMFA, FunDefs, 'remote');
    {constant,Name} ->
      {ConstMap2,_CodeAddress} = ConstAndZone,
      ConstAddress = find_const(Name, ConstMap2),
      patch_instr(Address, ConstAddress, constant);
    {closure,{DestMFA,Uniq,Index}} ->
      patch_closure(DestMFA, Uniq, Index, Address, FunDefs);
    {c_const,CConst} ->
      patch_instr(Address, bif_address(CConst), c_const)
  end.

patch_closure(DestMFA, Uniq, Index, Address, FunDefs) ->
  case get(hipe_patch_closures) of
    false ->
      []; % This is taken care of when registering the module.
    true ->
      %% We are replacing a previosly loaded BEAM module with native code,
      %% so we must reference the pre-existing entries in the fun table
      %% from the native code. We must delay actually patching the native
      %% address into the fun entry to ensure that the native code cannot
      %% be called until it has been completely fixed up.
      RemoteOrLocal = local, % closure code refs are local
      DestAddress = get_native_address(DestMFA, FunDefs, RemoteOrLocal),
      BEAMAddress = hipe_bifs:fun_to_address(DestMFA),
      FE = hipe_bifs:get_fe(mod(DestMFA), {Uniq, Index, BEAMAddress}),
      put(closures_to_patch, [{FE,DestAddress}|get(closures_to_patch)]),
      ?debug_msg("Patch FE(~w) to 0x~.16b->0x~.16b (emu:0x~.16b)\n",
		 [DestMFA, FE, DestAddress, BEAMAddress]),
      ?ASSERT(assert_local_patch(Address)),
      patch_instr(Address, FE, closure) 
  end.

%%----------------------------------------------------------------
%% Patch an instruction loading the address of an MFA.
%% RemoteOrLocal ::= 'remote' | 'local'
%%
patch_load_mfa(CodeAddress, DestMFA, FunDefs, RemoteOrLocal) ->
  ?ASSERT(assert_local_patch(CodeAddress)),
  DestAddress =
    case bif_address(DestMFA) of
      false ->
	NativeAddress = get_native_address(DestMFA, FunDefs, RemoteOrLocal),
	add_ref(DestMFA, CodeAddress, FunDefs, 'load_mfa', [], RemoteOrLocal),
	NativeAddress;
      BifAddress when is_integer(BifAddress) ->
	BifAddress
    end,
  patch_instr(CodeAddress, DestAddress, 'load_mfa').

%%----------------------------------------------------------------
%% Patch references to code labels in the data segment.
%%
patch_consts(Labels, DataAddress, CodeAddress, WriteWord) ->
  lists:foreach(fun (L) ->
		    patch_label_or_labels(L, DataAddress, CodeAddress,
                                          WriteWord)
		end, Labels).

patch_label_or_labels({Pos,Offset}, DataAddress, CodeAddress, WriteWord) ->
  ?ASSERT(assert_local_patch(CodeAddress+Offset)),
  WriteWord(DataAddress+Pos, CodeAddress+Offset);
patch_label_or_labels({sorted,Base,UnOrderdList}, DataAddress, CodeAddress,
                      WriteWord) ->
  sort_and_write(UnOrderdList, Base, DataAddress, CodeAddress, WriteWord).

sort_and_write(UnOrderdList, Base, DataAddress, CodeAddress, WriteWord) ->
  WriteAndInc =
    fun ({_, Offset}, DataPos) ->
	?ASSERT(assert_local_patch(CodeAddress+Offset)),
	WriteWord(DataPos, CodeAddress+Offset)
    end,
  lists:foldl(WriteAndInc, DataAddress+Base, sort_on_representation(UnOrderdList)).

sort_on_representation(List) ->
  lists:sort([{hipe_bifs:term_to_word(Term), Offset} ||
	       {Term, Offset} <- List]).

%%--------------------------------------------------------------------
%% Update an instruction to refer to a value of a given type.
%%
%% Type ::= 'call' | 'load_mfa' | 'x86_abs_pcrel' | 'atom'
%%	  | 'constant' | 'c_const' | 'closure'
%%
%% Note: the values of this Type are hard-coded in file erl_bif_types.erl
%%
patch_instr(Address, Value, Type) ->
  hipe_bifs:patch_insn(Address, Value, Type).

%%--------------------------------------------------------------------
%% Write a data word of the machine's natural word size.
%% Returns the address of the next word.
%%
%% XXX: It appears this is used for inserting both code addresses
%% and other data. In HiPE, code addresses are still 32-bit on
%% some 64-bit machines.
write_word_fun(WordSize) ->
  case WordSize of
    8 ->
      fun (DataAddress, DataWord) ->
          hipe_bifs:write_u64(DataAddress, DataWord),
          DataAddress+8
      end;
    4 ->
      fun (DataAddress, DataWord) ->
          hipe_bifs:write_u32(DataAddress, DataWord),
          DataAddress+4
      end
  end.

%%--------------------------------------------------------------------

bif_address({M,F,A}) ->
  hipe_bifs:bif_address(M,F,A);
bif_address(Name) when is_atom(Name) ->
  hipe_bifs:primop_address(Name).

%%--------------------------------------------------------------------
%% create_data_segment/3 takes an object file ConstMap, as produced by
%% hipe_pack_constants:slim_constmap/1, loads the constants into
%% memory, and produces a ConstMap2 mapping each constant's ConstNo to
%% its runtime address, tagged if the constant is a term.
%%
create_data_segment(DataAlign, DataSize, DataList, WriteWord, LoaderState) ->
  %%io:format("create_data_segment: \nDataAlign: ~p\nDataSize: ~p\nDataList: ~p\n",[DataAlign,DataSize,DataList]),
  DataAddress = hipe_bifs:alloc_data(DataAlign, DataSize, LoaderState),
  enter_data(DataList, [], DataAddress, DataSize, WriteWord).

enter_data(List, ConstMap2, DataAddress, DataSize, WriteWord) ->
  case List of
    [ConstNo,Offset,Type,Data|Rest] when is_integer(Offset) ->
      %%?msg("Const ~w\n",[[ConstNo,Offset,Type,Data]]),
      ?ASSERT((Offset >= 0) and (Offset =< DataSize)),
      Res = enter_datum(Type, Data, DataAddress+Offset, WriteWord),
      enter_data(Rest, [{ConstNo,Res}|ConstMap2], DataAddress, DataSize,
                 WriteWord);
    [] ->
      {DataAddress, ConstMap2}
  end.

enter_datum(Type, Data, Address, WriteWord) ->
  case ?EXT2CONST_TYPE(Type) of
    term ->
      %% Address is unused for terms
      hipe_bifs:term_to_word(hipe_bifs:merge_term(Data));
    sorted_block ->
      L = lists:sort([hipe_bifs:term_to_word(Term) || Term <- Data]),
      write_words(L, Address, WriteWord),
      Address;
    block ->
      case Data of
	{Lbls, []} ->
	  write_bytes(Lbls, Address);
	{Lbls, SortOrder} ->
	  SortedLbls = [Lbl || {_,Lbl} <- lists:sort(group(Lbls, SortOrder))],
	  write_words(SortedLbls, Address, WriteWord);
	Lbls ->
	  write_bytes(Lbls, Address)
      end,
      Address
  end.

group([], []) ->
  [];
group([B1,B2,B3,B4|Ls], [O|Os]) -> 
  [{hipe_bifs:term_to_word(O),bytes_to_32(B4,B3,B2,B1)}|group(Ls,Os)].

bytes_to_32(B4,B3,B2,B1) ->
  (B4 bsl 24) bor (B3 bsl 16) bor (B2 bsl 8) bor B1.

write_words([W|Rest], Addr, WriteWord) ->
  write_words(Rest, WriteWord(Addr, W), WriteWord);
write_words([], Addr, _) when is_integer(Addr) -> true.

write_bytes([B|Rest], Addr) ->
  hipe_bifs:write_u8(Addr, B),
  write_bytes(Rest, Addr+1);
write_bytes([], Addr) when is_integer(Addr) -> true.

%%% lists:keysearch/3 conses a useless wrapper around the found tuple :-(
%%% otherwise it would have been a good replacement for this loop
find_const(ConstNo, [{ConstNo,Addr}|_ConstMap2]) ->
  Addr;
find_const(ConstNo, [_|ConstMap2]) ->
  find_const(ConstNo, ConstMap2);
find_const(ConstNo, []) ->
  ?error_msg("Constant not found ~w\n",[ConstNo]),
  exit({constant_not_found,ConstNo}).


%%----------------------------------------------------------------
%% Record that the code at address 'Address' has a reference
%% of type 'RefType' ('call' or 'load_mfa') to 'CalleeMFA'.
%% 'FunDefs' must be an address-descending list from exports/2.
%%
%% If 'RefType' is 'call', then 'Trampoline' may be the address
%% of a stub branching to 'CalleeMFA', where the stub is reachable
%% from 'Address' via a normal call or tailcall instruction.
%%
%% RemoteOrLocal ::= 'remote' | 'local'.
%%

add_ref(CalleeMFA, Address, FunDefs, RefType, Trampoline, RemoteOrLocal) ->
  CallerMFA = address_to_mfa_lth(Address, FunDefs),
  case RemoteOrLocal of
    local ->
      %% assert that the callee and caller are from the same module
      {M,_,_} = CalleeMFA,
      {M,_,_} = CallerMFA,
      ok;
    remote ->
      hipe_bifs:add_ref(CalleeMFA, {CallerMFA,Address,RefType,Trampoline,
				    get(hipe_loader_state)})
  end.

%% For FunDefs sorted from low to high addresses
address_to_mfa_lth(Address, FunDefs) ->
  case address_to_mfa_lth(Address, FunDefs, false) of
    false ->
      ?error_msg("Local adddress not found ~w\n",[Address]),
      exit({?MODULE, local_address_not_found});
    MFA ->
      MFA
  end.

address_to_mfa_lth(Address, [#fundef{address=Adr, mfa=MFA}|Rest], Prev) ->
  if Address < Adr -> 
	  Prev;
     true -> 
	  address_to_mfa_lth(Address, Rest, MFA)
  end;
address_to_mfa_lth(_Address, [], Prev) -> 
    Prev.

%% For FunDefs sorted from high to low addresses
%% address_to_mfa_htl(Address, [#fundef{address=Adr, mfa=MFA}|_Rest]) when Address >= Adr -> MFA;
%% address_to_mfa_htl(Address, [_ | Rest]) -> address_to_mfa_htl(Address, Rest);
%% address_to_mfa_htl(Address, []) -> 
%%   ?error_msg("Local adddress not found ~w\n",[Address]),
%%   exit({?MODULE, local_address_not_found}).


%%--------------------------------------------------------------------

%% To find the native code of an MFA we need to look in 3 places:
%%  1. If it is compiled now look in the FunDefs data structure.
%%  2. Then look in native_addresses from module info. 
%%  3. Then (the function might have been singled compiled) look in
%%      hipe_funinfo
%%  If all else fails create a native stub for the MFA 
get_native_address(MFA, FunDefs, RemoteOrLocal) ->
  case mfa_to_address(MFA, FunDefs, RemoteOrLocal) of
    Adr when is_integer(Adr) -> Adr;
    false ->
	case RemoteOrLocal of
	  remote ->
	    hipe_bifs:find_na_or_make_stub(MFA);
	  local ->
	    ?error_msg("Local function ~p not found\n",[MFA]),
	    exit({function_not_found,MFA})
	end
  end.

mfa_to_address(MFA, [#fundef{address=Adr, mfa=MFA,
			     is_exported=IsExported}|_Rest], RemoteOrLocal) ->
  case RemoteOrLocal of
    local ->
      Adr;
    remote ->
      case IsExported of
	true ->
	  Adr;
	false ->
	  false
      end
  end;
mfa_to_address(MFA, [_|Rest], RemoteOrLocal) ->
  mfa_to_address(MFA, Rest, RemoteOrLocal);
mfa_to_address(_, [], _) -> false.

%% ____________________________________________________________________
%% 

-ifdef(DO_ASSERT).

-define(init_assert_patch(Base, Size), put(hipe_assert_code_area,{Base,Base+Size})).

assert_local_patch(Address) when is_integer(Address) ->
  {First,Last} = get(hipe_assert_code_area),
  Address >= First andalso Address < (Last).

-else.

-define(init_assert_patch(Base, Size), ok).

-endif.

%% ____________________________________________________________________
%% 

enter_code(CodeSize, CodeBinary, CalleeMFAs, LoaderState) ->
  true = byte_size(CodeBinary) =:= CodeSize,
  {CodeAddress,Trampolines} = hipe_bifs:enter_code(CodeBinary, CalleeMFAs,
						   LoaderState),
  ?init_assert_patch(CodeAddress, byte_size(CodeBinary)),
  {CodeAddress,Trampolines}.

