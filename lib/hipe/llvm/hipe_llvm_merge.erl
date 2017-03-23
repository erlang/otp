%%% -*- erlang-indent-level: 2 -*-
-module(hipe_llvm_merge).

-export([finalize/3]).

-include("hipe_llvm_arch.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-include("../main/hipe.hrl").

finalize(CompiledCode, Closures, Exports) ->
  CompiledCode1 = [CodePack || {_, CodePack} <- CompiledCode],
  Code = [{MFA, [], ConstTab}
	  || {MFA, _, _ , ConstTab, _, _} <- CompiledCode1],
  {ConstAlign, ConstSize, ConstMap, RefsFromConsts} =
    hipe_pack_constants:pack_constants(Code),
  %% Compute total code size separately as a sanity check for alignment
  CodeSize = compute_code_size(CompiledCode1, 0),
  %% io:format("Code Size (pre-computed): ~w~n", [CodeSize]),
  {CodeBinary, ExportMap} = merge_mfas(CompiledCode1, 0, <<>>, []),
  %% io:format("Code Size (post-computed): ~w~n", [byte_size(CodeBinary)]),
  ?VERBOSE_ASSERT(CodeSize =:= byte_size(CodeBinary)),
  AccRefs = merge_refs(CompiledCode1, ConstMap, 0, []),
  %% Bring CompiledCode to a combine_label_maps-acceptable form.
  LabelMap = combine_label_maps(CompiledCode1, 0, gb_trees:empty()),
  SC = hipe_pack_constants:slim_constmap(ConstMap),
  DataRelocs = hipe_pack_constants:mk_data_relocs(RefsFromConsts, LabelMap),
  SSE = hipe_pack_constants:slim_sorted_exportmap(ExportMap, Closures, Exports),
  SlimRefs = hipe_pack_constants:slim_refs(AccRefs),
  term_to_binary([{?VERSION_STRING(),?HIPE_ERTS_CHECKSUM},
		  ConstAlign, ConstSize,
		  SC,  % ConstMap
		  DataRelocs, % LabelMap
		  SSE, % ExportMap
		  CodeSize, CodeBinary, SlimRefs,
		  0,[] % ColdCodeSize, SlimColdRefs
		 ]).

%% Copied from hipe_x86_assemble.erl
nr_pad_bytes(Address) ->
  (4 - (Address rem 4)) rem 4. % XXX: 16 or 32 instead?

align_entry(Address) ->
  Address + nr_pad_bytes(Address).

compute_code_size([{_MFA, _BinaryCode, CodeSize, _, _, _}|Code], Size) ->
  compute_code_size(Code, align_entry(Size+CodeSize));
compute_code_size([], Size) -> Size.

combine_label_maps([{MFA, _, CodeSize, _, _, LabelMap}|Code], Address, CLM) ->
  NewCLM = merge_label_map(gb_trees:to_list(LabelMap), MFA, Address, CLM),
  combine_label_maps(Code, align_entry(Address+CodeSize), NewCLM);
combine_label_maps([], _Address, CLM) -> CLM.

merge_label_map([{Label,Offset}|Rest], MFA, Address, CLM) ->
  NewCLM = gb_trees:insert({MFA,Label}, Address+Offset, CLM),
  merge_label_map(Rest, MFA, Address, NewCLM);
merge_label_map([], _MFA, _Address, CLM) -> CLM.

%% @doc Merge the MFAs' binary code to one continuous binary and compute the
%%      size of this binary. At the same time create an exportmap in a form
%%      of {Address, M, F, A}.
%% XXX: Is alignment correct/optimal for X86/AMD64?
merge_mfas([{{M,F,A}, CodeBinary, CodeSize, _, _, _}|Code],
           Address, AccCode, AccExportMap) ->
  ?VERBOSE_ASSERT(CodeSize =:= byte_size(CodeBinary)),
  {Address1, Code1} =
    case nr_pad_bytes(Address + CodeSize) of
      0 -> %% Retains alignment:
        {Address + CodeSize, CodeBinary};
      NrPadBytes -> %% Needs padding!
        Padding = list_to_binary(lists:duplicate(NrPadBytes, 0)),
        {Address + CodeSize + NrPadBytes, % =:= align_entry(Address+CodeSize)
         <<CodeBinary/binary, Padding/binary>>}
    end,
  ?VERBOSE_ASSERT(Address1 =:=
        align_entry(Address + CodeSize)), %XXX: Should address be aligned?
  AccCode1 = <<AccCode/binary, Code1/binary>>,
  merge_mfas(Code, Address1, AccCode1, [{Address, M, F, A}|AccExportMap]);
merge_mfas([], _Address, AccCode, AccExportMap) ->
  {AccCode, AccExportMap}.

%% @doc Merge the references of relocatable symbols in the binary code. The
%%      offsets must be updated because of the merging of the code binaries!
merge_refs([], _ConstMap, _Addr, AccRefs) -> AccRefs;
merge_refs([{MFA, _, CodeSize, _, Refs, _}|Rest], ConstMap, Address, AccRefs) ->
  %% Important!: The hipe_pack_constants:pack_constants/2 function assignes
  %% unique numbers to constants (ConstNo). This numbers are used from now on,
  %% instead of labels that were used before. So, in order to be compatible, we
  %% must change all the constant labels in the Refs to the corresponding
  %% ConstNo, that can be found in the ConstMap (#pcm_entry{}).
  UpdatedRefs = [update_ref(label_to_constno(Ref, MFA, ConstMap), Address)
                 || Ref <- Refs],
  merge_refs(Rest, ConstMap, align_entry(Address+CodeSize),
             UpdatedRefs++AccRefs).

label_to_constno({Type, Offset, {constant, Label}}, MFA, ConstMap) ->
  ConstNo = hipe_pack_constants:find_const({MFA, Label}, ConstMap),
  {Type, Offset, {constant, ConstNo}};
label_to_constno(Other, _MFA, _ConstMap) ->
  Other.

%% @doc Update offset to a reference. In case of stack descriptors we must check
%%      if there exists an exception handler, because it must also be updated.
update_ref({?SDESC, Offset, SDesc}, CodeAddr) ->
  NewRefAddr = Offset+CodeAddr,
  case SDesc of
    {[], _, _, _} -> % No handler; only update offset
      {?SDESC, NewRefAddr, SDesc};
    {ExnHandler, FrameSize, StackArity, Roots} -> % Update exception handler
      {?SDESC, NewRefAddr, {ExnHandler+CodeAddr, FrameSize, StackArity, Roots}}
  end;
update_ref({Type, Offset, Term}, CodeAddr) ->
  {Type, Offset+CodeAddr, Term}.
