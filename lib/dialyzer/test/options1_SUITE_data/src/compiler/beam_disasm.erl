%% -*- erlang-indent-level: 4 -*-
%%=======================================================================
%% File        : beam_disasm.erl
%% Author      : Kostis Sagonas
%% Description : Disassembles an R5-R10 .beam file into symbolic BEAM code
%%=======================================================================
%% $Id: beam_disasm.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%%=======================================================================
%% Notes:
%%   1. It does NOT work for .beam files of previous BEAM versions.
%%   2. If handling of new BEAM instructions is needed, this should be
%%      inserted at the end of function resolve_inst().
%%=======================================================================

-module(beam_disasm).

-export([file/1, format_error/1]).

-author("Kostis Sagonas").

-include("beam_opcodes.hrl").

%%-----------------------------------------------------------------------

-define(NO_DEBUG(Str,Xs),ok).
-define(DEBUG(Str,Xs),io:format(Str,Xs)).
-define(exit(Reason),exit({?MODULE,?LINE,Reason})).

%%-----------------------------------------------------------------------
%% Error information

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error({internal, Error}) ->
    io_lib:format("~p: disassembly failed with reason ~P.",
		  [?MODULE, Error, 25]).

%%-----------------------------------------------------------------------
%% The main exported function
%%   File is either a file name or a binary containing the code.
%%   Returns `{beam_file, [...]}' or `{error, Module, Reason}'.
%%   Call `format_error({error, Module, Reason})' for an error string.
%%-----------------------------------------------------------------------

file(File) ->
    case beam_lib:info(File) of
	Info when list(Info) ->
	    {value,{chunks,Chunks}} = lists:keysearch(chunks,1,Info),
	    case catch process_chunks(File, Chunks) of
		{'EXIT', Error} ->
		    {error, ?MODULE, {internal, Error}};
		Result ->
		    Result
	    end;
	Error ->
	    Error
    end.

%%-----------------------------------------------------------------------
%% Interface might need to be revised -- do not depend on it.
%%-----------------------------------------------------------------------

process_chunks(F,ChunkInfoList) ->
    {ok,{_,Chunks}} = beam_lib:chunks(F, ["Atom","Code","StrT","ImpT","ExpT"]),
    [{"Atom",AtomBin},{"Code",CodeBin},{"StrT",StrBin},
     {"ImpT",ImpBin},{"ExpT",ExpBin}] = Chunks,
    LambdaBin = optional_chunk(F, "FunT", ChunkInfoList),
    LocBin = optional_chunk(F, "LocT", ChunkInfoList),
    AttrBin = optional_chunk(F, "Attr", ChunkInfoList),
    CompBin = optional_chunk(F, "CInf", ChunkInfoList),
    Atoms = beam_disasm_atoms(AtomBin),
    Exports = beam_disasm_exports(ExpBin, Atoms),
    Imports = beam_disasm_imports(ImpBin, Atoms),
    LocFuns = beam_disasm_exports(LocBin, Atoms),
    Lambdas = beam_disasm_lambdas(LambdaBin, Atoms),
    Str = beam_disasm_strings(StrBin),
    Str1 = binary_to_list(Str),  %% for debugging -- use Str as far as poss.
    Sym_Code = beam_disasm_code(CodeBin,Atoms,Imports,Str,Lambdas),
    Attributes = beam_disasm_attributes(AttrBin),
    CompInfo = beam_disasm_compilation_info(CompBin),
    All = [{exports,Exports},
	   {imports,Imports},
	   {code,Sym_Code},
	   {atoms,Atoms},
	   {local_funs,LocFuns},
	   {strings,Str1},
	   {attributes,Attributes},
	   {comp_info,CompInfo}],
    {beam_file,[Item || {_Key,Data}=Item <- All, Data =/= none]}.

%%-----------------------------------------------------------------------
%% Retrieve an optional chunk or none if the chunk doesn't exist.
%%-----------------------------------------------------------------------

optional_chunk(F, ChunkTag, ChunkInfo) ->
    case lists:keymember(ChunkTag, 1, ChunkInfo) of
	true ->
	    {ok,{_,[{ChunkTag,Chunk}]}} = beam_lib:chunks(F, [ChunkTag]),
	    Chunk;
	false -> none
    end.

%%-----------------------------------------------------------------------
%% UTILITIES -- these actually exist in file "beam_lib"
%%           -- they should be moved into a common utils file.
%%-----------------------------------------------------------------------

i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

get_int(B) ->
    {I, B1} = split_binary(B, 4),
    {i32(binary_to_list(I)), B1}.

%%-----------------------------------------------------------------------
%% Disassembles the atom table of a BEAM file.
%% - atoms are stored in order 1 ... N (N = Num_atoms, in fact),
%% - each atom name consists of a length byte, followed by that many
%%   bytes of name
%% (nb: atom names max 255 chars?!)
%%-----------------------------------------------------------------------

beam_disasm_atoms(AtomTabBin) ->
    {_NumAtoms,B} = get_int(AtomTabBin),
    disasm_atoms(B).

disasm_atoms(AtomBin) ->
    disasm_atoms(binary_to_list(AtomBin),1).

disasm_atoms([Len|Xs],N) ->
    {AtomName,Rest} = get_atom_name(Len,Xs),
    [{N,list_to_atom(AtomName)}|disasm_atoms(Rest,N+1)];
disasm_atoms([],_) ->
    [].

get_atom_name(Len,Xs) ->
    get_atom_name(Len,Xs,[]).

get_atom_name(N,[X|Xs],RevName) when N > 0 ->
    get_atom_name(N-1,Xs,[X|RevName]);
get_atom_name(0,Xs,RevName) ->
    { lists:reverse(RevName), Xs }.

%%-----------------------------------------------------------------------
%% Disassembles the export table of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_exports(none, _) -> none;
beam_disasm_exports(ExpTabBin, Atoms) ->
    {_NumAtoms,B} = get_int(ExpTabBin),
    disasm_exports(B,Atoms).

disasm_exports(Bin,Atoms) ->
    resolve_exports(collect_exports(binary_to_list(Bin)),Atoms).

collect_exports([F3,F2,F1,F0,A3,A2,A1,A0,L3,L2,L1,L0|Exps]) ->
    [{i32([F3,F2,F1,F0]),  % F = function (atom ID)
      i32([A3,A2,A1,A0]),  % A = arity (int)
      i32([L3,L2,L1,L0])}  % L = label (int)
     |collect_exports(Exps)];
collect_exports([]) ->
    [].

resolve_exports(Exps,Atoms) ->
    [ {lookup_key(F,Atoms), A, L} || {F,A,L} <- Exps ].

%%-----------------------------------------------------------------------
%% Disassembles the import table of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_imports(ExpTabBin,Atoms) ->
    {_NumAtoms,B} = get_int(ExpTabBin),
    disasm_imports(B,Atoms).

disasm_imports(Bin,Atoms) ->
    resolve_imports(collect_imports(binary_to_list(Bin)),Atoms).

collect_imports([M3,M2,M1,M0,F3,F2,F1,F0,A3,A2,A1,A0|Exps]) ->
    [{i32([M3,M2,M1,M0]),  % M = module (atom ID)
      i32([F3,F2,F1,F0]),  % F = function (atom ID)
      i32([A3,A2,A1,A0])}  % A = arity (int)
     |collect_imports(Exps)];
collect_imports([]) ->
    [].

resolve_imports(Exps,Atoms) ->
    [{extfunc,lookup_key(M,Atoms),lookup_key(F,Atoms),A} || {M,F,A} <- Exps ].

%%-----------------------------------------------------------------------
%% Disassembles the lambda (fun) table of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_lambdas(none, _) -> none;
beam_disasm_lambdas(<<_:32,Tab/binary>>, Atoms) ->
    disasm_lambdas(Tab, Atoms, 0).

disasm_lambdas(<<F:32,A:32,Lbl:32,Index:32,NumFree:32,OldUniq:32,More/binary>>,
	       Atoms, OldIndex) ->
    Info = {lookup_key(F, Atoms),A,Lbl,Index,NumFree,OldUniq},
    [{OldIndex,Info}|disasm_lambdas(More, Atoms, OldIndex+1)];
disasm_lambdas(<<>>, _, _) -> [].

%%-----------------------------------------------------------------------
%% Disassembles the code chunk of a BEAM file:
%%   - The code is first disassembled into a long list of instructions.
%%   - This list is then split into functions and all names are resolved.
%%-----------------------------------------------------------------------

beam_disasm_code(CodeBin,Atoms,Imports,Str,Lambdas) ->
    [_SS3,_SS2,_SS1,_SS0,  % Sub-Size (length of information before code)
     _IS3,_IS2,_IS1,_IS0,  % Instruction Set Identifier (always 0)
     _OM3,_OM2,_OM1,_OM0,  % Opcode Max
     _L3,_L2,_L1,_L0,_F3,_F2,_F1,_F0|Code] = binary_to_list(CodeBin),
    case catch disasm_code(Code, Atoms) of
	{'EXIT',Rsn} ->
	    ?NO_DEBUG('code disasm failed: ~p~n',[Rsn]),
	    ?exit(Rsn);
	DisasmCode ->
	    Functions = get_function_chunks(DisasmCode),
	    LocLabels = local_labels(Functions),
	    [resolve_names(F,Imports,Str,LocLabels,Lambdas) || F <- Functions]
    end.

%%-----------------------------------------------------------------------

disasm_code([B|Bs], Atoms) ->
    {Instr,RestBs} = disasm_instr(B, Bs, Atoms),
    [Instr|disasm_code(RestBs, Atoms)];
disasm_code([], _) -> [].

%%-----------------------------------------------------------------------
%% Splits the code stream into chunks representing the code of functions.
%%
%% NOTE: code actually looks like
%%   label L1: ... label Ln:
%%     func_info ...
%%   label entry:
%%     ...
%%     <on failure, use label Li to show where things died>
%%     ...
%% So the labels before each func_info should be included as well.
%% Ideally, only one such label is needed, but the BEAM compiler
%% before R8 didn't care to remove the redundant ones.
%%-----------------------------------------------------------------------

get_function_chunks([I|Code]) ->
    {LastI,RestCode,Labs} = split_head_labels(I,Code,[]),
    get_funs(LastI,RestCode,Labs,[]);
get_function_chunks([]) ->
    ?exit(empty_code_segment).

get_funs(PrevI,[I|Is],RevF,RevFs) ->
    case I of
	{func_info,_Info} ->
	    [H|T] = RevF,
	    {Last,Fun,TrailingLabels} = split_head_labels(H,T,[]),
	    get_funs(I, Is, [PrevI|TrailingLabels], add_funs([Last|Fun],RevFs));
	_ ->
	    get_funs(I, Is, [PrevI|RevF], RevFs)
    end;
get_funs(PrevI,[],RevF,RevFs) ->
    case PrevI of
	{int_code_end,[]} ->
	    emit_funs(add_fun(RevF,RevFs));
	_ ->
	    ?DEBUG('warning: code segment did not end with int_code_end~n',[]),
            emit_funs(add_funs([PrevI|RevF],RevFs))
    end.

split_head_labels({label,L},[I|Code],Labs) ->
    split_head_labels(I,Code,[{label,L}|Labs]);
split_head_labels(I,Code,Labs) ->
    {I,Code,Labs}.

add_fun([],Fs) ->
    Fs;
add_fun(F,Fs) ->
    add_funs(F,Fs).

add_funs(F,Fs) ->
    [ lists:reverse(F) | Fs ].

emit_funs(Fs) ->
    lists:reverse(Fs).

%%-----------------------------------------------------------------------
%% Collects local labels -- I am not sure this is 100% what is needed.
%%-----------------------------------------------------------------------

local_labels(Funs) ->
    [local_label(Fun) || Fun <- Funs].

%% The first clause below attempts to provide some (limited form of)
%% backwards compatibility; it is not needed for .beam files generated
%% by the R8 compiler.  The clause should one fine day be taken out.
local_label([{label,_},{label,L}|Code]) ->
    local_label([{label,L}|Code]);
local_label([{label,_},
	     {func_info,[M0,F0,{u,A}]},
	     {label,[{u,L1}]}|_]) ->
    {atom,M} = resolve_arg(M0),
    {atom,F} = resolve_arg(F0),
    {L1, {M, F, A}};
local_label(Code) ->
    io:format('beam_disasm: no label in ~p~n', [Code]),
    {-666,{none,none,0}}.

%%-----------------------------------------------------------------------
%% Disassembles a single BEAM instruction; most instructions are handled
%% in a generic way; indexing instructions are handled separately.
%%-----------------------------------------------------------------------

disasm_instr(B, Bs, Atoms) ->
    {SymOp,Arity} = beam_opcodes:opname(B),
    case SymOp of
	select_val ->
	    disasm_select_inst(select_val, Bs, Atoms);
	select_tuple_arity ->
	    disasm_select_inst(select_tuple_arity, Bs, Atoms);
	_ ->
	    case catch decode_n_args(Arity, Bs, Atoms) of
		{'EXIT',Rsn} ->
		    ?NO_DEBUG("decode_n_args(~p,~p) failed~n",[Arity,Bs]),
		    {{'EXIT',{SymOp,Arity,Rsn}},[]};
		{Args,RestBs} ->
		    ?NO_DEBUG("instr ~p~n",[{SymOp,Args}]),
		    {{SymOp,Args}, RestBs}
	    end
    end.

%%-----------------------------------------------------------------------
%% Disassembles a BEAM select_* instruction used for indexing.
%%   Currently handles {select_val,3} and {select_tuple_arity,3} insts.
%%
%%   The arruments of a "select"-type instruction look as follows:
%%       <reg>, {f,FailLabel}, {list, <num cases>, [<case1> ... <caseN>]}
%%   where each case is of the form [symbol,{f,Label}].
%%-----------------------------------------------------------------------

disasm_select_inst(Inst, Bs, Atoms) ->
    {X, Bs1} = decode_arg(Bs, Atoms),
    {F, Bs2} = decode_arg(Bs1, Atoms),
    {Z, Bs3} = decode_arg(Bs2, Atoms),
    {U, Bs4} = decode_arg(Bs3, Atoms),
    {u,Len} = U,
    {List, RestBs} = decode_n_args(Len, Bs4, Atoms),
    {{Inst,[X,F,{Z,U,List}]},RestBs}.

%%-----------------------------------------------------------------------
%% decode_arg([Byte]) -> { Arg, [Byte] }
%%
%% - an arg can have variable length, so we must return arg + remaining bytes
%% - decodes an argument into its 'raw' form: { Tag, Value }
%%   several types map to a single tag, so the byte code instr must then
%%   assign a type to it
%%-----------------------------------------------------------------------

decode_arg([B|Bs]) ->
    Tag = decode_tag(B band 2#111),
    ?NO_DEBUG('Tag = ~p, B = ~p, Bs = ~p~n',[Tag,B,Bs]),
    case Tag of
	z ->
	    decode_z_tagged(Tag, B, Bs);
	_ ->
	    %% all other cases are handled as if they were integers
	    decode_int(Tag, B, Bs)
    end.

decode_arg([B|Bs0], Atoms) ->
    Tag = decode_tag(B band 2#111),
    ?NO_DEBUG('Tag = ~p, B = ~p, Bs = ~p~n',[Tag,B,Bs]),
    case Tag of
	z ->
	    decode_z_tagged(Tag, B, Bs0);
	a ->
	    %% atom or nil
	    case decode_int(Tag, B, Bs0) of
		{{a,0},Bs} -> {nil,Bs};
		{{a,I},Bs} -> {{atom,lookup_key(I, Atoms)},Bs}
	    end;
	_ ->
	    %% all other cases are handled as if they were integers
	    decode_int(Tag, B, Bs0)
    end.

%%-----------------------------------------------------------------------
%% Decodes an integer value.  Handles positives, negatives, and bignums.
%%
%% Tries to do the opposite of:
%%   beam_asm:encode(1, 5) =            [81]
%%   beam_asm:encode(1, 1000) =         [105,232]
%%   beam_asm:encode(1, 2047) =         [233,255]
%%   beam_asm:encode(1, 2048) =         [25,8,0]
%%   beam_asm:encode(1,-1) =            [25,255,255]
%%   beam_asm:encode(1,-4294967295) =   [121,255,0,0,0,1]
%%   beam_asm:encode(1, 4294967295) =   [121,0,255,255,255,255]
%%   beam_asm:encode(1, 429496729501) = [121,99,255,255,255,157]
%%-----------------------------------------------------------------------

decode_int(Tag,B,Bs) when (B band 16#08) == 0 ->
    %% N < 16 = 4 bits, NNNN:0:TTT
    N = B bsr 4,
    {{Tag,N},Bs};
decode_int(Tag,B,Bs) when (B band 16#10) == 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    [B1|Bs1] = Bs,
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3) bor B1,
    ?NO_DEBUG('NNN:01:TTT, NNNNNNNN = ~n~p:01:~p, ~p = ~p~n', [Val0,Tag,B,N]),
    {{Tag,N},Bs1};
decode_int(Tag,B,Bs) ->
    {Len,Bs1} = decode_int_length(B,Bs),
    {IntBs,RemBs} = take_bytes(Len,Bs1),
    N = build_arg(IntBs),
    [F|_] = IntBs,
    Num = if F > 127, Tag == i -> decode_negative(N,Len);
	     true -> N
	  end,
    ?NO_DEBUG('Len = ~p, IntBs = ~p, Num = ~p~n', [Len,IntBs,Num]),
    {{Tag,Num},RemBs}.

decode_int_length(B,Bs) ->
    %% The following imitates get_erlang_integer() in beam_load.c
    %% Len is the size of the integer value in bytes
    case B bsr 5 of
	7 ->
	    {Arg,ArgBs} = decode_arg(Bs),
	    case Arg of
		{u,L} ->
		    {L+9,ArgBs};  % 9 stands for 7+2
		_ ->
		    ?exit({decode_int,weird_bignum_sublength,Arg})
	    end;
	L ->
	    {L+2,Bs}
    end.

decode_negative(N,Len) ->
    N - (1 bsl (Len*8)). % 8 is number of bits in a byte

%%-----------------------------------------------------------------------
%% Decodes lists and floating point numbers.
%%-----------------------------------------------------------------------

decode_z_tagged(Tag,B,Bs) when (B band 16#08) == 0 ->
    N = B bsr 4,
    case N of
	0 -> % float
	    decode_float(Bs);
	1 -> % list
	    {{Tag,N},Bs};
	2 -> % fr
	    decode_fr(Bs);
	3 -> % allocation list
	    decode_alloc_list(Bs);
	_ ->
	    ?exit({decode_z_tagged,{invalid_extended_tag,N}})
    end;
decode_z_tagged(_,B,_) ->
    ?exit({decode_z_tagged,{weird_value,B}}).

decode_float(Bs) ->
    {FL,RestBs} = take_bytes(8,Bs),
    <<Float:64/float>> = list_to_binary(FL),
    {{float,Float},RestBs}.

decode_fr(Bs) ->
    {{u,Fr},RestBs} = decode_arg(Bs),
    {{fr,Fr},RestBs}.

decode_alloc_list(Bs) ->
    {{u,N},RestBs} = decode_arg(Bs),
    decode_alloc_list_1(N, RestBs, []).

decode_alloc_list_1(0, RestBs, Acc) ->
    {{u,{alloc,lists:reverse(Acc)}},RestBs};
decode_alloc_list_1(N, Bs0, Acc) ->
    {{u,Type},Bs1} = decode_arg(Bs0),
    {{u,Val},Bs} = decode_arg(Bs1),
    case Type of
	0 ->
	    decode_alloc_list_1(N-1, Bs, [{words,Val}|Acc]);
	1 ->
	    decode_alloc_list_1(N-1, Bs, [{floats,Val}|Acc])
    end.

%%-----------------------------------------------------------------------
%% take N bytes from a stream, return { Taken_bytes, Remaining_bytes }
%%-----------------------------------------------------------------------

take_bytes(N,Bs) ->
    take_bytes(N,Bs,[]).

take_bytes(N,[B|Bs],Acc) when N > 0 ->
    take_bytes(N-1,Bs,[B|Acc]);
take_bytes(0,Bs,Acc) ->
    { lists:reverse(Acc), Bs }.

%%-----------------------------------------------------------------------
%% from a list of bytes Bn,Bn-1,...,B1,B0
%% build  (Bn << 8*n) bor ... bor B1 << 8 bor B0 << 0
%%-----------------------------------------------------------------------

build_arg(Bs) ->
    build_arg(Bs,0).

build_arg([B|Bs],N) ->
    build_arg(Bs, (N bsl 8) bor B);
build_arg([],N) ->
    N.

%%-----------------------------------------------------------------------
%% Decodes a bunch of arguments and returns them in a list
%%-----------------------------------------------------------------------

decode_n_args(N, Bs, Atoms) when N >= 0 ->
    decode_n_args(N, [], Bs, Atoms).

decode_n_args(N, Acc, Bs0, Atoms) when N > 0 ->
    {A1,Bs} = decode_arg(Bs0, Atoms),
    decode_n_args(N-1, [A1|Acc], Bs, Atoms);
decode_n_args(0, Acc, Bs, _) ->
    {lists:reverse(Acc),Bs}.

%%-----------------------------------------------------------------------
%% Convert a numeric tag value into a symbolic one
%%-----------------------------------------------------------------------

decode_tag(?tag_u) -> u;
decode_tag(?tag_i) -> i;
decode_tag(?tag_a) -> a;
decode_tag(?tag_x) -> x;
decode_tag(?tag_y) -> y;
decode_tag(?tag_f) -> f;
decode_tag(?tag_h) -> h;
decode_tag(?tag_z) -> z;
decode_tag(X) -> ?exit({unknown_tag,X}).

%%-----------------------------------------------------------------------
%% - replace all references {a,I} with the atom with index I (or {atom,A})
%% - replace all references to {i,K} in an external call position with
%%    the proper MFA (position in list, first elt = 0, yields MFA to use)
%% - resolve strings, represented as <offset, length>, into their
%%   actual values by using string table
%%    (note: string table should be passed as a BINARY so that we can
%%    use binary_to_list/3!)
%% - convert instruction to its readable form ...
%%
%% Currently, only the first three are done (systematically, at least).
%%
%% Note: It MAY be premature to remove the lists of args, since that
%%  representation means it is simpler to iterate over all args, etc.
%%-----------------------------------------------------------------------

resolve_names(Fun, Imports, Str, Lbls, Lambdas) ->
    [resolve_inst(Instr, Imports, Str, Lbls, Lambdas) || Instr <- Fun].

%%
%% New make_fun2/4 instruction added in August 2001 (R8).
%% We handle it specially here to avoid adding an argument to
%% the clause for every instruction.
%%

resolve_inst({make_fun2,Args},_,_,Lbls,Lambdas) ->
    [OldIndex] = resolve_args(Args),
    {value,{OldIndex,{F,A,_Lbl,_Index,NumFree,OldUniq}}} =
	lists:keysearch(OldIndex, 1, Lambdas),
    [{_,{M,_,_}}|_] = Lbls,			% Slightly kludgy.
    {make_fun2,{M,F,A},OldIndex,OldUniq,NumFree};
resolve_inst(Instr, Imports, Str, Lbls, _Lambdas) ->
    resolve_inst(Instr, Imports, Str, Lbls).

resolve_inst({label,[{u,L}]},_,_,_) ->
    {label,L};
resolve_inst({func_info,RawMFA},_,_,_) ->
    {func_info,resolve_args(RawMFA)};
% resolve_inst(int_code_end,_,_,_,_) ->  % instruction already handled
%    int_code_end;                       % should not really be handled here
resolve_inst({call,[{u,N},{f,L}]},_,_,Lbls) ->
    {call,N,catch lookup_key(L,Lbls)};
resolve_inst({call_last,[{u,N},{f,L},{u,U}]},_,_,Lbls) ->
    {call_last,N,catch lookup_key(L,Lbls),U};
resolve_inst({call_only,[{u,N},{f,L}]},_,_,Lbls) ->
    {call_only,N,catch lookup_key(L,Lbls)};
resolve_inst({call_ext,[{u,N},{u,MFAix}]},Imports,_,_) ->
    {call_ext,N,catch lists:nth(MFAix+1,Imports)};
resolve_inst({call_ext_last,[{u,N},{u,MFAix},{u,X}]},Imports,_,_) ->
    {call_ext_last,N,catch lists:nth(MFAix+1,Imports),X};
resolve_inst({bif0,Args},Imports,_,_) ->
    [Bif,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lists:nth(Bif+1,Imports),
    %?NO_DEBUG('bif0(~p, ~p)~n',[BifName,Reg]),
    {bif,BifName,nofail,[],Reg};
resolve_inst({bif1,Args},Imports,_,_) ->
    [F,Bif,A1,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lists:nth(Bif+1,Imports),
    %?NO_DEBUG('bif1(~p, ~p, ~p, ~p, ~p)~n',[Bif,BifName,F,[A1],Reg]),
    {bif,BifName,F,[A1],Reg};
resolve_inst({bif2,Args},Imports,_,_) ->
    [F,Bif,A1,A2,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lists:nth(Bif+1,Imports),
    %?NO_DEBUG('bif2(~p, ~p, ~p, ~p, ~p)~n',[Bif,BifName,F,[A1,A2],Reg]),
    {bif,BifName,F,[A1,A2],Reg};
resolve_inst({allocate,[{u,X0},{u,X1}]},_,_,_) ->
    {allocate,X0,X1};
resolve_inst({allocate_heap,[{u,X0},{u,X1},{u,X2}]},_,_,_) ->
    {allocate_heap,X0,X1,X2};
resolve_inst({allocate_zero,[{u,X0},{u,X1}]},_,_,_) ->
    {allocate_zero,X0,X1};
resolve_inst({allocate_heap_zero,[{u,X0},{u,X1},{u,X2}]},_,_,_) ->
    {allocate_heap_zero,X0,X1,X2};
resolve_inst({test_heap,[{u,X0},{u,X1}]},_,_,_) ->
    {test_heap,X0,X1};
resolve_inst({init,[Dst]},_,_,_) ->
    {init,Dst};
resolve_inst({deallocate,[{u,L}]},_,_,_) ->
    {deallocate,L};
resolve_inst({return,[]},_,_,_) ->
    return;
resolve_inst({send,[]},_,_,_) ->
    send;
resolve_inst({remove_message,[]},_,_,_) ->
    remove_message;
resolve_inst({timeout,[]},_,_,_) ->
    timeout;
resolve_inst({loop_rec,[Lbl,Dst]},_,_,_) ->
    {loop_rec,Lbl,Dst};
resolve_inst({loop_rec_end,[Lbl]},_,_,_) ->
    {loop_rec_end,Lbl};
resolve_inst({wait,[Lbl]},_,_,_) ->
    {wait,Lbl};
resolve_inst({wait_timeout,[Lbl,Int]},_,_,_) ->
    {wait_timeout,Lbl,resolve_arg(Int)};
resolve_inst({m_plus,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'+',W,[SrcR1,SrcR2],DstR};
resolve_inst({m_minus,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'-',W,[SrcR1,SrcR2],DstR};
resolve_inst({m_times,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'*',W,[SrcR1,SrcR2],DstR};
resolve_inst({m_div,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'/',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_div,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'div',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_rem,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'rem',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_band,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'band',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_bor,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'bor',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_bxor,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'bxor',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_bsl,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'bsl',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_bsr,Args},_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args),
    {arithbif,'bsr',W,[SrcR1,SrcR2],DstR};
resolve_inst({int_bnot,Args},_,_,_) ->
    [W,SrcR,DstR] = resolve_args(Args),
    {arithbif,'bnot',W,[SrcR],DstR};
resolve_inst({is_lt=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_ge=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_eq=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_ne=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_eq_exact=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_ne_exact=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_integer=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_float=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_number=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_atom=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_pid=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_reference=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_port=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_nil=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_binary=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_constant=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_list=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_nonempty_list=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({is_tuple=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({test_arity=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({select_val,Args},_,_,_) ->
    [Reg,FLbl,{{z,1},{u,_Len},List0}] = Args,
    List = resolve_args(List0),
    {select_val,Reg,FLbl,{list,List}};
resolve_inst({select_tuple_arity,Args},_,_,_) ->
    [Reg,FLbl,{{z,1},{u,_Len},List0}] = Args,
    List = resolve_args(List0),
    {select_tuple_arity,Reg,FLbl,{list,List}};
resolve_inst({jump,[Lbl]},_,_,_) ->
    {jump,Lbl};
resolve_inst({'catch',[Dst,Lbl]},_,_,_) ->
    {'catch',Dst,Lbl};
resolve_inst({catch_end,[Dst]},_,_,_) ->
    {catch_end,Dst};
resolve_inst({move,[Src,Dst]},_,_,_) ->
    {move,resolve_arg(Src),Dst};
resolve_inst({get_list,[Src,Dst1,Dst2]},_,_,_) ->
    {get_list,Src,Dst1,Dst2};
resolve_inst({get_tuple_element,[Src,{u,Off},Dst]},_,_,_) ->
    {get_tuple_element,resolve_arg(Src),Off,resolve_arg(Dst)};
resolve_inst({set_tuple_element,[Src,Dst,{u,Off}]},_,_,_) ->
    {set_tuple_element,resolve_arg(Src),resolve_arg(Dst),Off};
resolve_inst({put_string,[{u,Len},{u,Off},Dst]},_,Strings,_) ->
    String = if Len > 0 -> binary_to_list(Strings, Off+1, Off+Len);
		true -> ""
	     end,
?NO_DEBUG('put_string(~p, {string,~p}, ~p)~n',[Len,String,Dst]),
    {put_string,Len,{string,String},Dst};
resolve_inst({put_list,[Src1,Src2,Dst]},_,_,_) ->
    {put_list,resolve_arg(Src1),resolve_arg(Src2),Dst};
resolve_inst({put_tuple,[{u,Arity},Dst]},_,_,_) ->
    {put_tuple,Arity,Dst};
resolve_inst({put,[Src]},_,_,_) ->
    {put,resolve_arg(Src)};
resolve_inst({badmatch,[X]},_,_,_) ->
    {badmatch,resolve_arg(X)};
resolve_inst({if_end,[]},_,_,_) ->
    if_end;
resolve_inst({case_end,[X]},_,_,_) ->
    {case_end,resolve_arg(X)};
resolve_inst({call_fun,[{u,N}]},_,_,_) ->
    {call_fun,N};
resolve_inst({make_fun,Args},_,_,Lbls) ->
    [{f,L},Magic,FreeVars] = resolve_args(Args),
    {make_fun,catch lookup_key(L,Lbls),Magic,FreeVars};
resolve_inst({is_function=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({call_ext_only,[{u,N},{u,MFAix}]},Imports,_,_) ->
    {call_ext_only,N,catch lists:nth(MFAix+1,Imports)};
%%
%% Instructions for handling binaries added in R7A & R7B
%%
resolve_inst({bs_start_match,[F,Reg]},_,_,_) ->
    {bs_start_match,F,Reg};
resolve_inst({bs_get_integer=I,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {test,I,Lbl,[A2,N,decode_field_flags(U),A5]};
resolve_inst({bs_get_float=I,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {test,I,Lbl,[A2,N,decode_field_flags(U),A5]};
resolve_inst({bs_get_binary=I,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {test,I,Lbl,[A2,N,decode_field_flags(U),A5]};
resolve_inst({bs_skip_bits,[Lbl,Arg2,{u,N},{u,U}]},_,_,_) ->
    [A2] = resolve_args([Arg2]),
    {test,bs_skip_bits,Lbl,[A2,N,decode_field_flags(U)]};
resolve_inst({bs_test_tail,[F,{u,N}]},_,_,_) ->
    {test,bs_test_tail,F,[N]};
resolve_inst({bs_save,[{u,N}]},_,_,_) ->
    {bs_save,N};
resolve_inst({bs_restore,[{u,N}]},_,_,_) ->
    {bs_restore,N};
resolve_inst({bs_init,[{u,N},{u,U}]},_,_,_) ->
    {bs_init,N,decode_field_flags(U)};
resolve_inst({bs_final,[F,X]},_,_,_) ->
    {bs_final,F,X};
resolve_inst({bs_put_integer,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {bs_put_integer,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_binary,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    ?NO_DEBUG('bs_put_binary(~p,~p,~p,~p,~p})~n',[Lbl,A2,N,U,A5]),
    {bs_put_binary,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_float,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    ?NO_DEBUG('bs_put_float(~p,~p,~p,~p,~p})~n',[Lbl,A2,N,U,A5]),
    {bs_put_float,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_string,[{u,Len},{u,Off}]},_,Strings,_) ->
    String = if Len > 0 -> binary_to_list(Strings, Off+1, Off+Len);
		true -> ""
	     end,
    ?NO_DEBUG('bs_put_string(~p, {string,~p})~n',[Len,String]),
    {bs_put_string,Len,{string,String}};
resolve_inst({bs_need_buf,[{u,N}]},_,_,_) ->
    {bs_need_buf,N};

%%
%% Instructions for handling floating point numbers added in June 2001 (R8).
%%
resolve_inst({fclearerror,[]},_,_,_) ->
    fclearerror;
resolve_inst({fcheckerror,Args},_,_,_) ->
    [Fail] = resolve_args(Args),
    {fcheckerror,Fail};
resolve_inst({fmove,Args},_,_,_) ->
    [FR,Reg] = resolve_args(Args),
    {fmove,FR,Reg};
resolve_inst({fconv,Args},_,_,_) ->
    [Reg,FR] = resolve_args(Args),
    {fconv,Reg,FR};
resolve_inst({fadd=I,Args},_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args),
    {arithfbif,I,F,[A1,A2],Reg};
resolve_inst({fsub=I,Args},_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args),
    {arithfbif,I,F,[A1,A2],Reg};
resolve_inst({fmul=I,Args},_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args),
    {arithfbif,I,F,[A1,A2],Reg};
resolve_inst({fdiv=I,Args},_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args),
    {arithfbif,I,F,[A1,A2],Reg};
resolve_inst({fnegate,Args},_,_,_) ->
    [F,Arg,Reg] = resolve_args(Args),
    {arithfbif,fnegate,F,[Arg],Reg};

%%
%% Instructions for try expressions added in January 2003 (R10).
%%

resolve_inst({'try',[Reg,Lbl]},_,_,_) -> % analogous to 'catch'
    {'try',Reg,Lbl};
resolve_inst({try_end,[Reg]},_,_,_) ->   % analogous to 'catch_end'
    {try_end,Reg};
resolve_inst({try_case,[Reg]},_,_,_) ->   % analogous to 'catch_end'
    {try_case,Reg};
resolve_inst({try_case_end,[Reg]},_,_,_) ->
    {try_case_end,Reg};
resolve_inst({raise,[Reg1,Reg2]},_,_,_) ->
    {bif,raise,{f,0},[Reg1,Reg2],{x,0}};

%%
%% New bit syntax instructions added in February 2004 (R10B).
%%

resolve_inst({bs_init2,[Lbl,Arg2,{u,W},{u,R},{u,F},Arg6]},_,_,_) ->
    [A2,A6] = resolve_args([Arg2,Arg6]),
    {bs_init2,Lbl,A2,W,R,decode_field_flags(F),A6};
resolve_inst({bs_bits_to_bytes,[Lbl,Arg2,Arg3]},_,_,_) ->
    [A2,A3] = resolve_args([Arg2,Arg3]),
    {bs_bits_to_bytes,Lbl,A2,A3};
resolve_inst({bs_add=I,[Lbl,Arg2,Arg3,Arg4,Arg5]},_,_,_) ->
    [A2,A3,A4,A5] = resolve_args([Arg2,Arg3,Arg4,Arg5]),
    {I,Lbl,[A2,A3,A4],A5};

%%
%% New apply instructions added in April 2004 (R10B).
%%
resolve_inst({apply,[{u,Arity}]},_,_,_) ->
    {apply,Arity};
resolve_inst({apply_last,[{u,Arity},{u,D}]},_,_,_) ->
    {apply_last,Arity,D};

%%
%% New test instruction added in April 2004 (R10B).
%%
resolve_inst({is_boolean=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};

%%
%% Catches instructions that are not yet handled.
%%

resolve_inst(X,_,_,_) -> ?exit({resolve_inst,X}).

%%-----------------------------------------------------------------------
%% Resolves arguments in a generic way.
%%-----------------------------------------------------------------------

resolve_args(Args) -> [resolve_arg(A) || A <- Args].

resolve_arg({u,N}) -> N;
resolve_arg({i,N}) -> {integer,N};
resolve_arg({atom,Atom}=A) when is_atom(Atom) -> A;
resolve_arg(nil) -> nil;
resolve_arg(Arg) -> Arg.

%%-----------------------------------------------------------------------
%% The purpose of the following is just to add a hook for future changes.
%% Currently, field flags are numbers 1-2-4-8 and only two of these
%% numbers (BSF_LITTLE 2 -- BSF_SIGNED 4) have a semantic significance;
%% others are just hints for speeding up the execution; see "erl_bits.h".
%%-----------------------------------------------------------------------

decode_field_flags(FF) ->
    {field_flags,FF}.

%%-----------------------------------------------------------------------
%% Each string is denoted in the assembled code by its offset into this
%% binary.  This binary contains all strings concatenated together.
%%-----------------------------------------------------------------------

beam_disasm_strings(Bin) ->
    Bin.

%%-----------------------------------------------------------------------
%% Disassembles the attributes of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_attributes(none) -> none;
beam_disasm_attributes(AttrBin) -> binary_to_term(AttrBin).

%%-----------------------------------------------------------------------
%% Disassembles the compilation information of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_compilation_info(none) -> none;
beam_disasm_compilation_info(Bin) ->  binary_to_term(Bin).

%%-----------------------------------------------------------------------
%% Private Utilities
%%-----------------------------------------------------------------------

%%-----------------------------------------------------------------------

lookup_key(Key,[{Key,Val}|_]) ->
    Val;
lookup_key(Key,[_|KVs]) ->
    lookup_key(Key,KVs);
lookup_key(Key,[]) ->
    ?exit({lookup_key,{key_not_found,Key}}).

%%-----------------------------------------------------------------------
