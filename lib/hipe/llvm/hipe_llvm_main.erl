%% -*- erlang-indent-level: 2 -*-
-module(hipe_llvm_main).

-export([rtl_to_native/4]).

-include("../../kernel/src/hipe_ext_format.hrl").
-include("hipe_llvm_arch.hrl").
-include("elf_format.hrl").

%% @doc Translation of RTL to a loadable object. This function takes the RTL
%%      code and calls hipe_rtl_to_llvm:translate/2 to translate the RTL code to
%%      LLVM code. After this, LLVM asm is printed to a file and the LLVM tool
%%      chain is invoked in order to produce an object file.
rtl_to_native(MFA, RTL, Roots, Options) ->
  %% Compile to LLVM and get Instruction List (along with infos)
  {LLVMCode, RelocsDict0, ConstTab0} =
    hipe_rtl_to_llvm:translate(RTL, Roots),
  %% Fix function name to an acceptable LLVM identifier (needed for closures)
  {_Module, Fun, Arity} = hipe_rtl_to_llvm:fix_mfa_name(MFA),
  %% Write LLVM Assembly to intermediate file (on disk)
  {ok, Dir, ObjectFile} =
    compile_with_llvm(Fun, Arity, LLVMCode, Options, false),
  %%
  %% Extract information from object file
  %%
  ObjBin = open_object_file(ObjectFile),
  Obj = elf_format:read(ObjBin),
  %% Get labels info (for switches and jump tables)
  Labels = elf_format:extract_rela(Obj, ?RODATA),
  Tables = get_tables(Obj),
  %% Associate Labels with Switches and Closures with stack args
  {SwitchInfos, ExposedClosures} = correlate_labels(Tables, Labels),
  %% SwitchInfos:     [{"table_50", [Labels]}]
  %% ExposedClosures: [{"table_closures", [Labels]}]

  %% Labelmap contains the offsets of the labels in the code that are
  %% used for switch's jump tables
  LabelMap = create_labelmap(MFA, SwitchInfos, RelocsDict0),
  {RelocsDict, ConstTab} = extract_constants(RelocsDict0, ConstTab0, Obj),
  %% Get relocation info
  TextRelocs = elf_format:extract_rela(Obj, ?TEXT),
  %% AccRefs contains the offsets of all references to relocatable symbols in
  %% the code:
  AccRefs = fix_relocations(TextRelocs, RelocsDict, MFA),
  %% Get stack descriptors
  SDescs = get_sdescs(Obj),
  %% FixedSDescs are the stack descriptors after correcting calls that have
  %% arguments in the stack
  FixedSDescs =
    fix_stack_descriptors(RelocsDict, AccRefs, SDescs, ExposedClosures),
  Refs = AccRefs ++ FixedSDescs,
  %% Get binary code from object file
  BinCode = elf_format:extract_text(Obj),
  %% Remove temp files (if needed)
  ok = remove_temp_folder(Dir, Options),
  %% Return the code together with information that will be used in the
  %% hipe_llvm_merge module to produce the final binary that will be loaded
  %% by the hipe unified loader.
  {MFA, BinCode, byte_size(BinCode), ConstTab, Refs, LabelMap}.

%%------------------------------------------------------------------------------
%% LLVM tool chain
%%------------------------------------------------------------------------------

%% @doc Compile function FunName/Arity to LLVM. Return Dir (in order to remove
%%      it if we do not want to store temporary files) and ObjectFile name that
%%      is created by the LLVM tools.
compile_with_llvm(FunName, Arity, LLVMCode, Options, UseBuffer) ->
  Filename = atom_to_list(FunName) ++ "_" ++ integer_to_list(Arity),
  %% Save temp files in a unique folder
  Dir = unique_folder(FunName, Arity, Options),
  ok = file:make_dir(Dir),
  %% Print LLVM assembly to file
  OpenOpts = [append, raw] ++
    case UseBuffer of
      %% true  -> [delayed_write]; % Use delayed_write!
      false -> []
    end,
  {ok, File_llvm} = file:open(Dir ++ Filename ++ ".ll", OpenOpts),
  Ver = hipe:get_llvm_version(), %% Should probably cache this
  hipe_llvm:pp_ins_list(File_llvm, Ver, LLVMCode),
  %% delayed_write can cause file:close not to do a close, hence the two calls
  ok = file:close(File_llvm),
  __ = file:close(File_llvm),
  %% Invoke LLVM compiler tools to produce an object file
  llvm_opt(Dir, Filename, Options),
  llvm_llc(Dir, Filename, Ver, Options),
  compile(Dir, Filename, "gcc"), %%FIXME: use llc -filetype=obj and skip this!
  {ok, Dir, Dir ++ Filename ++ ".o"}.

%% @doc Invoke opt tool to optimize the bitcode (_name.ll -> _name.bc).
llvm_opt(Dir, Filename, Options) ->
  Source   = Dir ++ Filename ++ ".ll",
  Dest     = Dir ++ Filename ++ ".bc",
  OptLevel = trans_optlev_flag(opt, Options),
  OptFlags = [OptLevel, "-mem2reg", "-strip"],
  Command  = "opt " ++ fix_opts(OptFlags) ++ " " ++ Source ++ " -o " ++ Dest,
  %% io:format("OPT: ~s~n", [Command]),
  case os:cmd(Command) of
    "" -> ok;
    Error -> exit({?MODULE, opt, Error})
  end.

%% @doc Invoke llc tool to compile the bitcode to object file
%%      (_name.bc -> _name.o).
llvm_llc(Dir, Filename, Ver, Options) ->
  Source   = Dir ++ Filename ++ ".bc",
  OptLevel = trans_optlev_flag(llc, Options),
  VerFlags = llc_ver_flags(Ver),
  Align    = find_stack_alignment(),
  Target   = llc_target_opt(),
  LlcFlags = [OptLevel, "-code-model=medium", "-stack-alignment=" ++ Align
             , "-tailcallopt", "-filetype=asm" %FIXME
             , Target
             | VerFlags],
  Command  = "llc " ++ fix_opts(LlcFlags) ++ " " ++ Source,
  %% io:format("LLC: ~s~n", [Command]),
  case os:cmd(Command) of
    "" -> ok;
    Error -> exit({?MODULE, llc, Error})
  end.

%% @doc Invoke the compiler tool ("gcc", "llvmc", etc.) to generate an object
%%      file from native assembly.
compile(Dir, Fun_Name, Compiler) ->
  Source  = Dir ++ Fun_Name ++ ".s",
  Dest    = Dir ++ Fun_Name ++ ".o",
  Target  = compiler_target_opt(),
  Command = Compiler ++ " " ++ Target ++ " -c " ++ Source ++ " -o " ++ Dest,
  %% io:format("~s: ~s~n", [Compiler, Command]),
  case os:cmd(Command) of
    "" -> ok;
    Error -> exit({?MODULE, cc, Error})
  end.

find_stack_alignment() ->
  case get(hipe_target_arch) of
    x86 -> "4";
    amd64 -> "8";
    _ -> exit({?MODULE, find_stack_alignment, "Unimplemented architecture"})
  end.

llc_target_opt() ->
  case get(hipe_target_arch) of
    x86 -> "-march=x86";
    amd64 -> "-march=x86-64"
  end.

compiler_target_opt() ->
  case get(hipe_target_arch) of
    x86 -> "-m32";
    amd64 -> "-m64"
  end.

%% @doc Join options.
fix_opts(Opts) ->
  lists:flatten(lists:join(" ", Opts)).

%% @doc Translate optimization-level flag (default is "O2").
trans_optlev_flag(Tool, Options) ->
  Flag = case Tool of
	   opt -> llvm_opt;
	   llc -> llvm_llc
         end,
  case proplists:get_value(Flag, Options) of
    o0 -> ""; % "-O0" does not exist in opt tool
    o1 -> "-O1";
    o2 -> "-O2";
    o3 -> "-O3";
    undefined -> "-O2"
  end.

llc_ver_flags(Ver = {_, _}) when Ver >= {3,9} ->
  %% Works around a bug in the x86-call-frame-opt pass (as of LLVM 3.9) that
  %% break the garbage collection stack descriptors.
  ["-no-x86-call-frame-opt"];
llc_ver_flags({_, _}) -> [].

%%------------------------------------------------------------------------------
%% Functions to manage Relocations
%%------------------------------------------------------------------------------

%% @doc Get switch table and closure table.
-spec get_tables(elf_format:elf()) -> [elf_sym()].
get_tables(Elf) ->
  %% Search Symbol Table for entries where name is prefixed with "table_":
  [S || S=#elf_sym{name="table_" ++ _} <- elf_format:elf_symbols(Elf)].

%% @doc This function associates symbols who point to some table of labels with
%%      the corresponding offsets of the labels in the code. These tables can
%%      either be jump tables for switches or a table which contains the labels
%%      of blocks that contain closure calls with more than ?NR_ARG_REGS.
correlate_labels([], _L) -> {[], []};
correlate_labels(Tables, Labels) ->
  %% Assumes that the relocations are sorted
  RelocTree = gb_trees:from_orddict(
		[{Rel#elf_rel.offset, Rel#elf_rel.addend} || Rel <- Labels]),
  %% Lookup all relocations pertaining to each symbol
  NamesValues = [{Name, lookup_range(Value, Value+Size, RelocTree)}
		 || #elf_sym{name=Name, value=Value, size=Size} <- Tables],
  case lists:keytake("table_closures", 1, NamesValues) of
    false ->  %% No closures in the code, no closure table
      {NamesValues, []};
    {value, ClosureTableNV, SwitchesNV} ->
      {SwitchesNV, ClosureTableNV}
  end.

%% Fetches all values with a key in [Low, Hi)
-spec lookup_range(_::K, _::K, gb_trees:tree(K,V)) -> [_::V].
lookup_range(Low, Hi, Tree) ->
  lookup_range_1(Hi, gb_trees:iterator_from(Low, Tree)).

lookup_range_1(Hi, Iter0) ->
  case gb_trees:next(Iter0) of
    {Key, Value, Iter} when Key < Hi -> [Value | lookup_range_1(Hi, Iter)];
    _ -> []
  end.

%% @doc Create a gb_tree which contains information about the labels that used
%%      for switch's jump tables. The keys of the gb_tree are of the form
%%      {MFA, Label} and the values are the actual Offsets.
create_labelmap(MFA, SwitchInfos, RelocsDict) ->
  create_labelmap(MFA, SwitchInfos, RelocsDict, gb_trees:empty()).

create_labelmap(_, [], _, LabelMap) -> LabelMap;
create_labelmap(MFA, [{Name, Offsets} | Rest], RelocsDict, LabelMap) ->
  case dict:fetch(Name, RelocsDict) of
    {switch, {_TableType, LabelList, _NrLabels, _SortOrder}, _JTabLab} ->
      KVDict = lists:ukeysort(1, lists:zip(LabelList, Offsets)),
      NewLabelMap = insert_to_labelmap(KVDict, LabelMap),
      create_labelmap(MFA, Rest, RelocsDict, NewLabelMap);
    _ ->
      exit({?MODULE, create_labelmap, "Not a jump table!"})
  end.

%% @doc Insert a list of [{Key,Value}] to a LabelMap (gb_tree).
insert_to_labelmap([], LabelMap) -> LabelMap;
insert_to_labelmap([{Key, Value}|Rest], LabelMap) ->
  case gb_trees:lookup(Key, LabelMap) of
    none ->
      insert_to_labelmap(Rest, gb_trees:insert(Key, Value, LabelMap));
    {value, Value} -> %% Exists with the *exact* same Value.
      insert_to_labelmap(Rest, LabelMap)
  end.

%% Find any LLVM-generated constants and add them to the constant table
extract_constants(RelocsDict0, ConstTab0, Obj) ->
  TextRelocs = elf_format:extract_rela(Obj, ?TEXT),
  AnonConstSections =
    lists:usort([{Sec, Offset}
		 || #elf_rel{symbol=#elf_sym{type=section, section=Sec},
			     addend=Offset} <- TextRelocs]),
  lists:foldl(
    fun({#elf_shdr{name=Name, type=progbits, addralign=Align, entsize=EntSize,
		   size=Size} = Section, Offset}, {RelocsDict1, ConstTab1})
	when EntSize > 0, 0 =:= Size rem EntSize, 0 =:= Offset rem EntSize ->
	SectionBin = elf_format:section_contents(Section, Obj),
	Constant = binary:part(SectionBin, Offset, EntSize),
	{ConstTab, ConstLbl} =
	  hipe_consttab:insert_binary_const(ConstTab1, Align, Constant),
	{dict:store({anon, Name, Offset}, {constant, ConstLbl}, RelocsDict1),
	 ConstTab}
    end, {RelocsDict0, ConstTab0}, AnonConstSections).

%% @doc Correlate object file relocation symbols with info from translation to
%%      llvm code.
fix_relocations(Relocs, RelocsDict, MFA) ->
  lists:map(fun(Reloc) -> fix_reloc(Reloc, RelocsDict, MFA) end, Relocs).

%% Relocation types and expected addends for x86 and amd64
-define(PCREL_T, 'pc32').
-define(PCREL_A, -4). %% Hard-coded in hipe_x86.c and hipe_amd64.c
-ifdef(BIT32).
-define(ABS_T, '32').
-define(ABS_A, _). %% We support any addend
-else.
-define(ABS_T, '64').
-define(ABS_A, 0).
-endif.

fix_reloc(#elf_rel{symbol=#elf_sym{name=Name, section=undefined, type=notype},
		   offset=Offset, type=?PCREL_T, addend=?PCREL_A},
	  RelocsDict, {_,_,_}) when Name =/= "" ->
  case dict:fetch(Name, RelocsDict) of
    {call, _, {bif, BifName, _}} -> {?CALL_LOCAL, Offset, BifName};
    {call, not_remote,  CallMFA} -> {?CALL_LOCAL,  Offset, CallMFA};
    {call, remote, CallMFA} -> {?CALL_REMOTE, Offset, CallMFA}
  end;
fix_reloc(#elf_rel{symbol=#elf_sym{name=Name, section=undefined, type=notype},
		   offset=Offset, type=?ABS_T, addend=?ABS_A},
	  RelocsDict, _) when Name =/= "" ->
  case dict:fetch(Name, RelocsDict) of
    {atom, AtomName}     -> {?LOAD_ATOM,    Offset, AtomName};
    {constant, Label}    -> {?LOAD_ADDRESS, Offset, {constant, Label}};
    {closure, _}=Closure -> {?LOAD_ADDRESS, Offset, Closure}
  end;
fix_reloc(#elf_rel{symbol=#elf_sym{name=Name, section=#elf_shdr{name=?TEXT},
				   type=func},
		   offset=Offset, type=?PCREL_T, addend=?PCREL_A},
	  RelocsDict, MFA) when Name =/= "" ->
  case dict:fetch(Name, RelocsDict) of
    {call, not_remote, MFA} -> {?CALL_LOCAL, Offset, MFA}
  end;
fix_reloc(#elf_rel{symbol=#elf_sym{name=Name, section=#elf_shdr{name=?RODATA},
				   type=object},
		   offset=Offset, type=?ABS_T, addend=?ABS_A},
	  RelocsDict, _) when Name =/= "" ->
  case dict:fetch(Name, RelocsDict) of
    {switch, _, JTabLab} -> %% Treat switch exactly as constant
      {?LOAD_ADDRESS, Offset, {constant, JTabLab}}
  end;
fix_reloc(#elf_rel{symbol=#elf_sym{type=section, section=#elf_shdr{name=Name}},
		   offset=Offset, type=?ABS_T, addend=Addend}, RelocsDict, _) ->
  case dict:fetch({anon, Name, Addend}, RelocsDict) of
    {constant, Label} -> {?LOAD_ADDRESS, Offset, {constant, Label}}
  end.

%%------------------------------------------------------------------------------
%% Functions to manage Stack Descriptors
%%------------------------------------------------------------------------------

%% @doc This function takes an ELF Object File binary and returns a proper sdesc
%%      list for Erlang/OTP System's loader. The return value should be of the
%%      form:
%%        {
%%          4, Safepoint Address,
%%          {ExnLabel OR [], FrameSize, StackArity, {Liveroot stack frame indexes}},
%%        }
get_sdescs(Elf) ->
  case elf_format:extract_note(Elf, ?NOTE_ERLGC_NAME) of
    <<>> -> % Object file has no ".note.gc" section!
      [];
    NoteGC_bin ->
      %% Get safe point addresses (stored in ".rela.note.gc" section):
      RelaNoteGC = elf_format:extract_rela(Elf, ?NOTE(?NOTE_ERLGC_NAME)),
      SPCount = length(RelaNoteGC),
      T = SPCount * ?SP_ADDR_SIZE,
      %% Pattern match fields of ".note.gc":
      <<SPCount:(?bits(?SP_COUNT_SIZE))/integer-little, % Sanity check!
	_SPAddrs:T/binary, % NOTE: In 64bit they are relocs!
        StkFrameSize:(?bits(?SP_STKFRAME_SIZE))/integer-little,
        StkArity:(?bits(?SP_STKARITY_SIZE))/integer-little,
        _LiveRootCount:(?bits(?SP_LIVEROOTCNT_SIZE))/integer-little, % Skip
        Roots/binary>> = NoteGC_bin,
      LiveRoots = get_liveroots(Roots, []),
      %% Extract the safe point offsets:
      SPOffs = [A || #elf_rel{addend=A} <- RelaNoteGC],
      %% Extract Exception Handler labels:
      ExnHandlers = elf_format:get_exn_handlers(Elf),
      %% Combine ExnHandlers and Safe point addresses (return addresses):
      ExnAndSPOffs = combine_ras_and_exns(ExnHandlers, SPOffs, []),
      create_sdesc_list(ExnAndSPOffs, StkFrameSize, StkArity, LiveRoots, [])
  end.

%% @doc Extracts a bunch of integers (live roots) from a binary. Returns a tuple
%%      as need for stack descriptors.
get_liveroots(<<>>, Acc) ->
  list_to_tuple(Acc);
get_liveroots(<<Root:?bits(?LR_STKINDEX_SIZE)/integer-little,
                MoreRoots/binary>>, Acc) ->
  get_liveroots(MoreRoots, [Root | Acc]).

combine_ras_and_exns(_, [], Acc) ->
  lists:reverse(Acc);
combine_ras_and_exns(ExnHandlers, [RA | MoreRAs], Acc) ->
  %% FIXME: do something better than O(n^2) by taking advantage of the property
  %% ||ExnHandlers|| <= ||RAs||
  Handler = find_exn_handler(RA, ExnHandlers),
  combine_ras_and_exns(ExnHandlers, MoreRAs, [{Handler, RA} | Acc]).

find_exn_handler(_, []) ->
  [];
find_exn_handler(RA, [{Start, End, Handler} | MoreExnHandlers]) ->
  case (RA >= Start andalso RA =< End) of
    true ->
      Handler;
    false ->
      find_exn_handler(RA, MoreExnHandlers)
  end.

create_sdesc_list([], _, _, _, Acc) ->
  lists:reverse(Acc);
create_sdesc_list([{ExnLbl, SPOff} | MoreExnAndSPOffs],
		  StkFrameSize, StkArity, LiveRoots, Acc) ->
  Hdlr = case ExnLbl of
           0 -> [];
           N -> N
         end,
  create_sdesc_list(MoreExnAndSPOffs, StkFrameSize, StkArity, LiveRoots,
                    [{?SDESC, SPOff, {Hdlr, StkFrameSize, StkArity, LiveRoots}}
                     | Acc]).

%% @doc This function is responsible for correcting the stack descriptors of
%%      the calls that are found in the code and have more than NR_ARG_REGS
%%      (thus, some of their arguments are passed to the stack). Because of the
%%      Reserved Call Frame feature that the LLVM uses, the stack descriptors
%%      are not correct since at the point of call the frame size is reduced
%%      by the number of arguments that are passed on the stack. Also, the
%%      offsets of the roots need to be re-adjusted.
fix_stack_descriptors(_, _, [], _) ->
  [];
fix_stack_descriptors(RelocsDict, Relocs, SDescs, ExposedClosures) ->
  %% NamedCalls are MFA and BIF calls that need fix
  NamedCalls       = calls_with_stack_args(RelocsDict),
  NamedCallsOffs   = calls_offsets_arity(Relocs, NamedCalls),
  ExposedClosures1 =
    case dict:is_key("table_closures", RelocsDict) of
      true -> %% A Table with closures exists
        {table_closures, ArityList} = dict:fetch("table_closures", RelocsDict),
        case ExposedClosures of
          {_,  Offsets} ->
            lists:zip(Offsets, ArityList);
          _ ->
            exit({?MODULE, fix_stack_descriptors,
                 {"Wrong exposed closures", ExposedClosures}})
        end;
      false ->
        []
    end,
  ClosuresOffs = closures_offsets_arity(ExposedClosures1, SDescs),
  fix_sdescs(NamedCallsOffs ++ ClosuresOffs, SDescs).

%% @doc This function takes as argument the relocation dictionary as produced by
%%      the translation of RTL code to LLVM and finds the names of the calls
%%      (MFA and BIF calls) that have more than NR_ARG_REGS.
calls_with_stack_args(Dict) ->
  calls_with_stack_args(dict:to_list(Dict), []).

calls_with_stack_args([], Calls) -> Calls;
calls_with_stack_args([ {_Name, {call, _, {M, F, A}}} | Rest], Calls)
  when A > ?NR_ARG_REGS ->
  Call =
    case M of
      bif -> {F,A};
      _ -> {M,F,A}
    end,
  calls_with_stack_args(Rest, [Call|Calls]);
calls_with_stack_args([_|Rest], Calls) ->
  calls_with_stack_args(Rest, Calls).

%% @doc This function extracts the stack arity and the offset in the code of
%%      the named calls (MFAs, BIFs) that have stack arguments.
calls_offsets_arity(AccRefs, CallsWithStackArgs) ->
  calls_offsets_arity(AccRefs, CallsWithStackArgs, []).

calls_offsets_arity([], _, Acc) -> Acc;
calls_offsets_arity([{Type, Offset, Term} | Rest], CallsWithStackArgs, Acc)
  when Type =:= ?CALL_REMOTE orelse Type =:= ?CALL_LOCAL ->
  case lists:member(Term, CallsWithStackArgs) of
    true ->
      Arity =
        case Term of
          {_M, _F, A} -> A;
          {_F, A} -> A
        end,
      calls_offsets_arity(Rest, CallsWithStackArgs,
                          [{Offset + 4, Arity - ?NR_ARG_REGS} | Acc]);
    false ->
      calls_offsets_arity(Rest, CallsWithStackArgs, Acc)
  end;
calls_offsets_arity([_|Rest], CallsWithStackArgs, Acc) ->
  calls_offsets_arity(Rest, CallsWithStackArgs, Acc).

%% @doc This function extracts the stack arity and the offsets of closures that
%%      have stack arity. The Closures argument represents the
%%      hipe_bifs:llvm_exposure_closure/0 calls in the code. The actual closure
%%      is the next call in the code, so the offset of the next call must be
%%      calculated from the stack descriptors.
closures_offsets_arity([], _) ->
  [];
closures_offsets_arity(ExposedClosures, SDescs) ->
  Offsets = [Offset || {_, Offset, _} <- SDescs],
  %% Offsets and closures must be sorted in order for find_offsets/3 to work
  SortedOffsets = lists:sort(Offsets),
  SortedExposedClosures = lists:keysort(1, ExposedClosures),
  find_offsets(SortedExposedClosures, SortedOffsets, []).

find_offsets([], _, Acc) -> Acc;
find_offsets([{Off,Arity}|Rest], Offsets, Acc) ->
  [I | RestOffsets] = lists:dropwhile(fun (Y) -> Y<Off end, Offsets),
  find_offsets(Rest, RestOffsets, [{I, Arity}|Acc]).

%% The function below corrects the stack descriptors of calls with arguments
%% that are passed on the stack (more than NR_ARG_REGS) by subtracting the
%% number of stacked arguments from the frame size and from the offset of the
%% roots.
fix_sdescs([], SDescs) -> SDescs;
fix_sdescs([{Offset, Arity} | Rest], SDescs) ->
  case lists:keyfind(Offset, 2, SDescs) of
    false ->
      fix_sdescs(Rest, SDescs);
    {?SDESC, Offset, {ExnHandler, FrameSize, StkArity, Roots}} ->
      FixedRoots = list_to_tuple([Ri - Arity || Ri <- tuple_to_list(Roots)]),
      FixedSDesc =
        {?SDESC, Offset, {ExnHandler, FrameSize - Arity, StkArity, FixedRoots}},
      fix_sdescs(Rest, [FixedSDesc | lists:keydelete(Offset, 2, SDescs)])
  end.

%%------------------------------------------------------------------------------
%% Miscellaneous functions
%%------------------------------------------------------------------------------

%% @doc A function that opens a file as binary. The function takes as argument
%%      the name of the file and returns an Erlang binary.
-spec open_object_file(string()) -> binary().
open_object_file(ObjFile) ->
  case file:read_file(ObjFile) of
    {ok, Binary} ->
      Binary;
    {error, Reason} ->
      exit({?MODULE, open_file, Reason})
  end.

remove_temp_folder(Dir, Options) ->
  case proplists:get_bool(llvm_save_temps, Options) of
    true -> ok;
    false -> spawn(fun () -> "" = os:cmd("rm -rf " ++ Dir) end), ok
  end.

unique_id(FunName, Arity) ->
  integer_to_list(erlang:phash2({FunName, Arity, erlang:unique_integer()})).

unique_folder(FunName, Arity, Options) ->
  DirName = "llvm_" ++ unique_id(FunName, Arity) ++ "/",
  Dir =
    case proplists:get_bool(llvm_save_temps, Options) of
      true ->  %% Store folder in current directory
        DirName;
      false -> %% Temporarily store folder in tempfs (/dev/shm/)
        "/dev/shm/" ++ DirName
    end,
  %% Make sure it does not exist
  case dir_exists(Dir) of
    true -> %% Dir already exists! Generate again.
      unique_folder(FunName, Arity, Options);
    false ->
      Dir
  end.

%% @doc Function that checks that a given Filename is an existing Directory
%%      Name (from http://rosettacode.org/wiki/Ensure_that_a_file_exists#Erlang)
dir_exists(Filename) ->
  {Flag, Info} = file:read_file_info(Filename),
  (Flag =:= ok) andalso (element(3, Info) =:= directory).
