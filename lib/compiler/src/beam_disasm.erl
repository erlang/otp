%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2018. All Rights Reserved.
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
%%=======================================================================
%% Notes:
%%   1. It does NOT work for .beam files of previous BEAM versions.
%%   2. If handling of new BEAM instructions is needed, this should be 
%%      inserted at the end of function resolve_inst().
%%=======================================================================

-module(beam_disasm).

-export([file/1]). %% the main function
-export([function__code/1, format_error/1]).
-ifdef(DEBUG_DISASM).
-export([dfs/1, df/1, files/1, pp/1, pp/2]).
-endif.

-author("Kostis Sagonas").

-include("beam_opcodes.hrl").
-include("beam_disasm.hrl").

%%-----------------------------------------------------------------------

-type index()        :: non_neg_integer().
-type literals()     :: 'none' | gb_trees:tree(index(), term()).
-type symbolic_tag() :: 'a' | 'f' | 'h' | 'i' | 'u' | 'x' | 'y' | 'z'.
-type disasm_tag()   :: symbolic_tag() | 'fr' | 'atom' | 'float' | 'literal'.
-type disasm_term()  :: 'nil' | {disasm_tag(), _}.

%%-----------------------------------------------------------------------

-define(NO_DEBUG(Str,Xs), ok).
-define(DEBUG(Str,Xs), io:format(Str,Xs)).
-define(exit(Reason), exit({?MODULE,?LINE,Reason})).

%%-----------------------------------------------------------------------
%% Utility functions to get/set their fields. (Uncomment and export
%% them when/if they get used in other files.)
%%-----------------------------------------------------------------------

%% -spec function__name(#function{}) -> atom().
%% function__name(#function{name = N}) -> N.
%% -spec function__arity(#function{}) -> arity().
%% function__arity(#function{arity = A}) -> A.
%% function__entry(#function{entry = E}) -> E.

-spec function__code(#function{}) -> [beam_instr()].
function__code(#function{code = Code}) -> Code.

-spec function__code_update(#function{}, [beam_instr()]) -> #function{}.
function__code_update(Function, NewCode) ->
  Function#function{code = NewCode}.

%%-----------------------------------------------------------------------
%% Error information

-spec format_error({'internal',term()} | {'error',atom(),term()}) -> string().

format_error({internal,Error}) ->
    io_lib:format("~p: disassembly failed with reason ~P.",
		  [?MODULE, Error, 25]);
format_error({error,Module,Error}) ->
    lists:flatten(Module:format_error(Error)).

%%-----------------------------------------------------------------------
%% User comfort functions to directly disassemble to file or to
%% stream, pretty-printed, and to just pretty-print, also commented.
%%-----------------------------------------------------------------------

-ifdef(DEBUG_DISASM).

dfs(Files) when is_list(Files) ->
    lists:foreach(fun df/1, Files).

df(Module) when is_atom(Module) ->
    case code:which(Module) of
	File when is_list(File) ->
	    df(File);
	Reason when is_atom(Reason) ->
	    {error,?MODULE,Reason}
    end;
df(File) when is_list(File) ->
    file(File, filename:rootname(File, ".beam")++".dis").

files(Files) when is_list(Files) ->
    lists:foreach(fun (File) -> file(File, group_leader()) end, Files).

file(File, Dest) ->
    case file(File) of
	#beam_file{code = DisasmCode} ->
	    pp(Dest, [{file,File}, {code,DisasmCode}]);
	Error -> Error
    end.

-spec pp([_]) -> 'ok' | {'error', atom()}.

pp(Disasm) ->
    pp(group_leader(), Disasm).

-spec pp(pid() | file:filename(), [_]) -> 'ok' | {'error', atom()}.

pp(Stream, Disasm) when is_pid(Stream), is_list(Disasm) ->
    NL = io_lib:nl(),
    lists:foreach(
      fun ({code,Code}) ->
	      lists:foreach(
		fun (#function{name=F,arity=A,entry=E,code=C}) ->
			io:format(Stream, "~p.~n", [{function,F,A,E}]),
			lists:foreach(
			  fun (I) -> 
				  io:put_chars(Stream, [pp_instr(I)|NL])
			  end, C),
			io:nl(Stream)
		end, Code);
	  (Item) ->
	      io:format(Stream, "~p.~n~n", [Item])
      end, Disasm),
    ok;
pp(File, Disasm) when is_list(Disasm) ->
    case file:open(File, [write]) of
	{ok,F} ->
	    Result = pp(F, Disasm),
	    ok = file:close(F),
	    Result;
	{error,_Reason} = Error -> Error
    end.

pp_instr({comment,I,Comment}) ->
    [pp_instr(I)|" % "++Comment];
pp_instr({comment,Comment}) ->
    ["%% "++Comment];
pp_instr({label,_}=I) ->
    io_lib:format("  ~p.", [I]);
pp_instr(I) ->
    io_lib:format("    ~p.", [I]).

-endif.

%%-----------------------------------------------------------------------
%% The main exported function
%%   File is either a file name or a binary containing the code.
%%   Call `format_error({error, Module, Reason})' for an error string.
%%-----------------------------------------------------------------------

-spec file(file:filename() | binary()) -> #beam_file{} | {'error',atom(),_}.

file(File) ->
    try process_chunks(File)
    catch error:Reason:Stack ->
            {error,?MODULE,{internal,{Reason,Stack}}}
    end.

%%-----------------------------------------------------------------------
%% Interface might need to be revised -- do not depend on it.
%%-----------------------------------------------------------------------

process_chunks(F) ->
    case beam_lib:chunks(F, [atoms,"Code","StrT",
			     indexed_imports,labeled_exports]) of
	{ok,{Module,
	     [{atoms,AtomsList},{"Code",CodeBin},{"StrT",StrBin},
	      {indexed_imports,ImportsList},{labeled_exports,Exports}]}} ->
	    Atoms = mk_atoms(AtomsList),
	    LambdaBin = optional_chunk(F, "FunT"),
	    Lambdas = beam_disasm_lambdas(LambdaBin, Atoms),
	    LiteralBin = optional_chunk(F, "LitT"),
	    Literals = beam_disasm_literals(LiteralBin),
	    Code = beam_disasm_code(CodeBin, Atoms, mk_imports(ImportsList),
				    StrBin, Lambdas, Literals, Module),
	    Attributes =
		case optional_chunk(F, attributes) of
		    none -> [];
		    Atts when is_list(Atts) -> Atts
		end,
	    CompInfo = 
		case optional_chunk(F, "CInf") of
		    none -> [];
		    CompInfoBin when is_binary(CompInfoBin) ->
			binary_to_term(CompInfoBin)
		end,
	    #beam_file{module = Module,
		       labeled_exports = Exports,
		       attributes = Attributes,
		       compile_info = CompInfo,
		       code = Code};
	Error -> Error
    end.

%%-----------------------------------------------------------------------
%% Retrieve an optional chunk or return 'none' if the chunk doesn't exist.
%%-----------------------------------------------------------------------

optional_chunk(F, ChunkTag) ->
    case beam_lib:chunks(F, [ChunkTag]) of
	{ok,{_Module,[{ChunkTag,Chunk}]}} -> Chunk;
	{error,beam_lib,{missing_chunk,_,_}} -> none
    end.

%%-----------------------------------------------------------------------
%% Disassembles the lambda (fun) table of a BEAM file.
%%-----------------------------------------------------------------------

-type l_info() :: {non_neg_integer(), {_,_,_,_,_,_}}.
-spec beam_disasm_lambdas('none' | binary(), gb_trees:tree(index(), _)) ->
        'none' | [l_info()].

beam_disasm_lambdas(none, _) -> none;
beam_disasm_lambdas(<<_:32,Tab/binary>>, Atoms) ->
    disasm_lambdas(Tab, Atoms, 0).

disasm_lambdas(<<F:32,A:32,Lbl:32,Index:32,NumFree:32,OldUniq:32,More/binary>>,
	       Atoms, OldIndex) ->
    Info = {lookup(F, Atoms),A,Lbl,Index,NumFree,OldUniq},
    [{OldIndex,Info}|disasm_lambdas(More, Atoms, OldIndex+1)];
disasm_lambdas(<<>>, _, _) -> [].

%%-----------------------------------------------------------------------
%% Disassembles the literal table (constant pool) of a BEAM file.
%%-----------------------------------------------------------------------

-spec beam_disasm_literals('none' | binary()) -> literals().

beam_disasm_literals(none) -> none;
beam_disasm_literals(<<_:32,Compressed/binary>>) ->
    <<_:32,Tab/binary>> = zlib:uncompress(Compressed),
    gb_trees:from_orddict(disasm_literals(Tab, 0)).

disasm_literals(<<Sz:32,Ext:Sz/binary,T/binary>>, Index) ->
    [{Index,binary_to_term(Ext)}|disasm_literals(T, Index+1)];
disasm_literals(<<>>, _) -> [].

%%-----------------------------------------------------------------------
%% Disassembles the code chunk of a BEAM file:
%%   - The code is first disassembled into a long list of instructions.
%%   - This list is then split into functions and all names are resolved.
%%-----------------------------------------------------------------------

beam_disasm_code(<<_SS:32, % Sub-Size (length of information before code)
		  _IS:32,  % Instruction Set Identifier (always 0)
		  _OM:32,  % Opcode Max
		  _L:32,_F:32,
		  CodeBin/binary>>, Atoms, Imports,
		 Str, Lambdas, Literals, M) ->
    Code = binary_to_list(CodeBin),
    try disasm_code(Code, Atoms, Literals) of
	DisasmCode ->
	    Functions = get_function_chunks(DisasmCode),
	    Labels = mk_labels(local_labels(Functions)),
	    [function__code_update(Function,
				   resolve_names(Is, Imports, Str,
						 Labels, Lambdas, Literals, M))
	     || Function = #function{code=Is} <- Functions]
    catch
	error:Rsn ->
	    ?NO_DEBUG('code disassembling failed: ~p~n', [Rsn]),
	    ?exit(Rsn)
    end.

%%-----------------------------------------------------------------------

disasm_code([B|Bs], Atoms, Literals) ->
    {Instr,RestBs} = disasm_instr(B, Bs, Atoms, Literals),
    [Instr|disasm_code(RestBs, Atoms, Literals)];
disasm_code([], _, _) -> [].

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

get_function_chunks([]) ->
    ?exit(empty_code_segment);
get_function_chunks(Code) ->
    get_funs(labels_r(Code, [])).

labels_r([], R) -> {R, []};
labels_r([{label,_}=I|Is], R) ->
    labels_r(Is, [I|R]);
labels_r([{line,_}=I|Is], R) ->
    labels_r(Is, [I|R]);
labels_r(Is, R) -> {R, Is}.

get_funs({[],[]}) -> [];
get_funs({_,[]}) ->
    ?exit(no_func_info_in_code_segment);
get_funs({LsR0,[{func_info,[{atom,M}=AtomM,{atom,F}=AtomF,ArityArg]}|Code0]})
  when is_atom(M), is_atom(F) ->
    Arity = resolve_arg_unsigned(ArityArg),
    {LsR,Code,RestCode} = get_fun(Code0, []),
    [{label,[{u,Entry}]}|_] = Code,
    [#function{name=F,
	       arity=Arity,
	       entry=Entry,
	       code=lists:reverse(LsR0, [{func_info,AtomM,AtomF,Arity}|Code])}
     |get_funs({LsR,RestCode})].

get_fun([{func_info,_}|_]=Is, R0) ->
    {LsR,R} = labels_r(R0, []),
    {LsR,lists:reverse(R),Is};
get_fun([{int_code_end,[]}], R) ->
    {[],lists:reverse(R),[]};
get_fun([I|Is], R) ->
    get_fun(Is, [I|R]);
get_fun([], R) ->
    ?DEBUG('warning: code segment did not end with int_code_end~n',[]),
    {[],lists:reverse(R),[]}.

%%-----------------------------------------------------------------------
%% Collects local labels -- I am not sure this is 100% what is needed.
%%-----------------------------------------------------------------------

local_labels(Funs) ->
    lists:sort(lists:foldl(fun (F, R) ->
				   local_labels_1(function__code(F), R)
			   end, [], Funs)).

local_labels_1(Code0, R) ->
    Code1 = lists:dropwhile(fun({label,_}) -> true;
			       ({line,_}) -> true;
			       ({func_info,_,_,_}) -> false
			    end, Code0),
    [{func_info,{atom,M},{atom,F},A}|Code] = Code1,
    local_labels_2(Code, R, {M,F,A}).

local_labels_2([{label,[{u,L}]}|Code], R, MFA) ->
    local_labels_2(Code, [{L,MFA}|R], MFA);
local_labels_2(_, R, _) -> R.

%%-----------------------------------------------------------------------
%% Disassembles a single BEAM instruction; most instructions are handled
%% in a generic way; indexing instructions are handled separately.
%%-----------------------------------------------------------------------

disasm_instr(B, Bs, Atoms, Literals) ->
    {SymOp, Arity} = beam_opcodes:opname(B),
    case SymOp of
	select_val ->
	    disasm_select_inst(select_val, Bs, Atoms, Literals);
	select_tuple_arity ->
	    disasm_select_inst(select_tuple_arity, Bs, Atoms, Literals);
	put_map_assoc ->
	    disasm_map_inst(put_map_assoc, Arity, Bs, Atoms, Literals);
	put_map_exact ->
	    disasm_map_inst(put_map_exact, Arity, Bs, Atoms, Literals);
	get_map_elements ->
	    disasm_map_inst(get_map_elements, Arity, Bs, Atoms, Literals);
	has_map_fields ->
	    disasm_map_inst(has_map_fields, Arity, Bs, Atoms, Literals);
	put_tuple2 ->
	    disasm_put_tuple2(Bs, Atoms, Literals);
	_ ->
	    try decode_n_args(Arity, Bs, Atoms, Literals) of
		{Args, RestBs} ->
		    ?NO_DEBUG("instr ~p~n", [{SymOp, Args}]),
		    {{SymOp, Args}, RestBs}
	    catch
		error:Rsn ->
		    ?NO_DEBUG("decode_n_args(~p,~p) failed~n", [Arity, Bs]),
		    ?exit({cannot_disasm_instr, {SymOp, Arity, Rsn}})
	    end
    end.

%%-----------------------------------------------------------------------
%% Disassembles a BEAM select_* instruction used for indexing.
%%   Currently handles {select_val,3} and {select_tuple_arity,3} insts.
%%
%%   The arguments of a "select"-type instruction look as follows:
%%       <reg>, {f,FailLabel}, {list, <num cases>, [<case1> ... <caseN>]}
%%   where each case is of the form [symbol,{f,Label}].
%%-----------------------------------------------------------------------

disasm_select_inst(Inst, Bs, Atoms, Literals) ->
    {X, Bs1} = decode_arg(Bs, Atoms, Literals),
    {F, Bs2} = decode_arg(Bs1, Atoms, Literals),
    {Z, Bs3} = decode_arg(Bs2, Atoms, Literals),
    {U, Bs4} = decode_arg(Bs3, Atoms, Literals),
    {u, Len} = U,
    {List, RestBs} = decode_n_args(Len, Bs4, Atoms, Literals),
    {{Inst, [X,F,{Z,U,List}]}, RestBs}.

disasm_map_inst(Inst, Arity, Bs0, Atoms, Literals) ->
    {Args0,Bs1} = decode_n_args(Arity, Bs0, Atoms, Literals),
    %% no droplast ..
    [Z|Args1]  = lists:reverse(Args0),
    Args       = lists:reverse(Args1),
    {U, Bs2}   = decode_arg(Bs1, Atoms, Literals),
    {u, Len}   = U,
    {List, RestBs} = decode_n_args(Len, Bs2, Atoms, Literals),
    {{Inst, Args ++ [{Z,U,List}]}, RestBs}.

disasm_put_tuple2(Bs, Atoms, Literals) ->
    {X, Bs1} = decode_arg(Bs, Atoms, Literals),
    {Z, Bs2} = decode_arg(Bs1, Atoms, Literals),
    {U, Bs3} = decode_arg(Bs2, Atoms, Literals),
    {u, Len} = U,
    {List, RestBs} = decode_n_args(Len, Bs3, Atoms, Literals),
    {{put_tuple2, [X,{Z,U,List}]}, RestBs}.

%%-----------------------------------------------------------------------
%% decode_arg([Byte]) -> {Arg, [Byte]}
%%
%% - an arg can have variable length, so we must return arg + remaining bytes
%% - decodes an argument into its 'raw' form: { Tag, Value }
%%   several types map to a single tag, so the byte code instr must then
%%   assign a type to it
%%-----------------------------------------------------------------------

-spec decode_arg([byte(),...]) -> {{disasm_tag(),_}, [byte()]}.

decode_arg([B|Bs]) ->
    Tag = decode_tag(B band 2#111),
    ?NO_DEBUG('Tag = ~p, B = ~p, Bs = ~p~n', [Tag, B, Bs]),
    case Tag of
	z ->
	    decode_z_tagged(Tag, B, Bs, no_literals);
	_ ->
	    %% all other cases are handled as if they were integers
	    decode_int(Tag, B, Bs)
    end.

-spec decode_arg([byte(),...], gb_trees:tree(index(), _), literals()) ->
        {disasm_term(), [byte()]}.

decode_arg([B|Bs0], Atoms, Literals) ->
    Tag = decode_tag(B band 2#111),
    ?NO_DEBUG('Tag = ~p, B = ~p, Bs = ~p~n', [Tag, B, Bs0]),
    case Tag of
	z ->
	    decode_z_tagged(Tag, B, Bs0, Literals);
	a ->
	    %% atom or nil
	    case decode_int(Tag, B, Bs0) of
		{{a,0},Bs} -> {nil,Bs};
		{{a,I},Bs} -> {{atom,lookup(I, Atoms)},Bs}
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

decode_int(Tag,B,Bs) when (B band 16#08) =:= 0 ->
    %% N < 16 = 4 bits, NNNN:0:TTT
    N = B bsr 4,
    {{Tag,N},Bs};
decode_int(Tag,B,Bs) when (B band 16#10) =:= 0 ->
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
    Num = if F > 127, Tag =:= i -> decode_negative(N,Len);
	     true -> N
	  end,
    ?NO_DEBUG('Len = ~p, IntBs = ~p, Num = ~p~n', [Len,IntBs,Num]),
    {{Tag,Num},RemBs}.

-spec decode_int_length(integer(), [byte()]) -> {integer(), [byte()]}.

decode_int_length(B, Bs) ->
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
    
-spec decode_negative(non_neg_integer(), non_neg_integer()) -> neg_integer().

decode_negative(N, Len) ->
    N - (1 bsl (Len*8)). % 8 is number of bits in a byte

%%-----------------------------------------------------------------------
%% Decodes lists and floating point numbers.
%%-----------------------------------------------------------------------

decode_z_tagged(Tag,B,Bs,Literals) when (B band 16#08) =:= 0 ->
    N = B bsr 4,
    case N of
	0 -> % float
	    decode_float(Bs);
	1 -> % list
	    {{Tag,N},Bs};
	2 -> % fr
	    decode_fr(Bs);
	3 -> % allocation list
	    decode_alloc_list(Bs, Literals);
	4 -> % literal
	    {{u,LitIndex},RestBs} = decode_arg(Bs),
	    case gb_trees:get(LitIndex, Literals) of
		Float when is_float(Float) ->
		    {{float,Float},RestBs};
		Literal ->
		    {{literal,Literal},RestBs}
	    end;
	_ ->
	    ?exit({decode_z_tagged,{invalid_extended_tag,N}})
    end;
decode_z_tagged(_,B,_,_) ->
    ?exit({decode_z_tagged,{weird_value,B}}).

-spec decode_float([byte(),...]) -> {{'float', float()}, [byte()]}.

decode_float(Bs) ->
    {FL,RestBs} = take_bytes(8,Bs),
    <<Float:64/float>> = list_to_binary(FL),
    {{float,Float},RestBs}.

-spec decode_fr([byte(),...]) -> {{'fr', non_neg_integer()}, [byte()]}.

decode_fr(Bs) ->
    {{u,Fr},RestBs} = decode_arg(Bs),
    {{fr,Fr},RestBs}.

decode_alloc_list(Bs, Literals) ->
    {{u,N},RestBs} = decode_arg(Bs),
    decode_alloc_list_1(N, Literals, RestBs, []).

decode_alloc_list_1(0, _Literals, RestBs, Acc) ->
    {{u,{alloc,lists:reverse(Acc)}},RestBs};
decode_alloc_list_1(N, Literals, Bs0, Acc) ->
    {{u,Type},Bs1} = decode_arg(Bs0),
    {{u,Val},Bs} = decode_arg(Bs1),
    Res = case Type of
	      0 -> {words,Val};
	      1 -> {floats,Val};
	      2 -> {literal,gb_trees:get(Val, Literals)}
	  end,
    decode_alloc_list_1(N-1, Literals, Bs, [Res|Acc]).

%%-----------------------------------------------------------------------
%% take N bytes from a stream, return {Taken_bytes, Remaining_bytes}
%%-----------------------------------------------------------------------

-spec take_bytes(non_neg_integer(), [byte()]) -> {[byte()], [byte()]}.

take_bytes(N, Bs) ->
    take_bytes(N, Bs, []).

take_bytes(N, [B|Bs], Acc) when N > 0 ->
    take_bytes(N-1, Bs, [B|Acc]);
take_bytes(0, Bs, Acc) ->
    {lists:reverse(Acc), Bs}.

%%-----------------------------------------------------------------------
%% from a list of bytes Bn,Bn-1,...,B1,B0
%% build  (Bn << 8*n) bor ... bor (B1 << 8) bor (B0 << 0)
%%-----------------------------------------------------------------------

build_arg(Bs) ->
    build_arg(Bs, 0).

build_arg([B|Bs], N) ->
    build_arg(Bs, (N bsl 8) bor B);
build_arg([], N) ->
    N.

%%-----------------------------------------------------------------------
%% Decodes a bunch of arguments and returns them in a list
%%-----------------------------------------------------------------------

decode_n_args(N, Bs, Atoms, Literals) when N >= 0 ->
    decode_n_args(N, [], Bs, Atoms, Literals).

decode_n_args(N, Acc, Bs0, Atoms, Literals) when N > 0 ->
    {A1,Bs} = decode_arg(Bs0, Atoms, Literals),
    decode_n_args(N-1, [A1|Acc], Bs, Atoms, Literals);
decode_n_args(0, Acc, Bs, _, _) ->
    {lists:reverse(Acc),Bs}.

%%-----------------------------------------------------------------------
%% Convert a numeric tag value into a symbolic one
%%-----------------------------------------------------------------------

-spec decode_tag(0..7) -> symbolic_tag().

decode_tag(?tag_u) -> u;
decode_tag(?tag_i) -> i;
decode_tag(?tag_a) -> a;
decode_tag(?tag_x) -> x;
decode_tag(?tag_y) -> y;
decode_tag(?tag_f) -> f;
decode_tag(?tag_h) -> h;
decode_tag(?tag_z) -> z.

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

resolve_names(Fun, Imports, Str, Lbls, Lambdas, Literals, M) ->
    [resolve_inst(Instr, Imports, Str, Lbls, Lambdas, Literals, M) || Instr <- Fun].

%%
%% New make_fun2/4 instruction added in August 2001 (R8).
%% We handle it specially here to avoid adding an argument to
%% the clause for every instruction.
%%

resolve_inst({make_fun2,Args}, _, _, _, Lambdas, _, M) ->
    [OldIndex] = resolve_args(Args),
    {OldIndex,{F,A,_Lbl,_Index,NumFree,OldUniq}} =
	lists:keyfind(OldIndex, 1, Lambdas),
    {make_fun2,{M,F,A},OldIndex,OldUniq,NumFree};
resolve_inst(Instr, Imports, Str, Lbls, _Lambdas, _Literals, _M) ->
    %% io:format(?MODULE_STRING":resolve_inst ~p.~n", [Instr]),
    resolve_inst(Instr, Imports, Str, Lbls).

resolve_inst({label,[{u,L}]},_,_,_) ->
    {label,L};
resolve_inst(FuncInfo,_,_,_) when element(1, FuncInfo) =:= func_info -> 
    FuncInfo; % already resolved
%% resolve_inst(int_code_end,_,_,_,_) ->  % instruction already handled
%%    int_code_end;                       % should not really be handled here
resolve_inst({call,[{u,N},{f,L}]},_,_,Lbls) ->
    {call,N,lookup(L,Lbls)};
resolve_inst({call_last,[{u,N},{f,L},{u,U}]},_,_,Lbls) ->
    {call_last,N,lookup(L,Lbls),U};
resolve_inst({call_only,[{u,N},{f,L}]},_,_,Lbls) ->
    {call_only,N,lookup(L,Lbls)};
resolve_inst({call_ext,[{u,N},{u,MFAix}]},Imports,_,_) ->
    {call_ext,N,lookup(MFAix+1,Imports)};
resolve_inst({call_ext_last,[{u,N},{u,MFAix},{u,X}]},Imports,_,_) ->
    {call_ext_last,N,lookup(MFAix+1,Imports),X};
resolve_inst({bif0,Args},Imports,_,_) ->
    [Bif,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lookup(Bif+1,Imports),
    {bif,BifName,nofail,[],Reg};
resolve_inst({bif1,Args},Imports,_,_) ->
    [F,Bif,A1,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lookup(Bif+1,Imports),
    {bif,BifName,F,[A1],Reg};
resolve_inst({bif2,Args},Imports,_,_) ->
    [F,Bif,A1,A2,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lookup(Bif+1,Imports),
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
resolve_inst({is_tagged_tuple=I,Args0},_,_,_) ->
    [F|Args] = resolve_args(Args0),
    {test,I,F,Args};
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
resolve_inst({is_function=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};
resolve_inst({call_ext_only,[{u,N},{u,MFAix}]},Imports,_,_) ->
    {call_ext_only,N,lookup(MFAix+1,Imports)};
%%
%% Instructions for handling binaries added in R7A & R7B
%%
resolve_inst({bs_put_integer,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {bs_put_integer,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_binary,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {bs_put_binary,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_float,[Lbl,Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {bs_put_float,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_string,[{u,Len},{u,Off}]},_,Strings,_) ->
    String = if Len > 0 -> binary_to_list(Strings, Off+1, Off+Len);
		true -> ""
	     end,
    {bs_put_string,Len,{string,String}};

%%
%% Instructions for handling floating point numbers added in June 2001 (R8).
%%
resolve_inst({fclearerror,[]},_,_,_) ->
    fclearerror;
resolve_inst({fcheckerror,[Arg]},_,_,_) ->
    {fcheckerror,resolve_arg(Arg)};
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
resolve_inst({try_case,[Reg]},_,_,_) ->  % analogous to 'catch_end'
    {try_case,Reg};
resolve_inst({try_case_end,[Arg]},_,_,_) ->
    {try_case_end,resolve_arg(Arg)};
resolve_inst({raise,[_Reg1,_Reg2]=Regs},_,_,_) ->
    {raise,{f,0},Regs,{x,0}};		 % do NOT wrap this as a 'bif'
					 % as there is no raise/2 bif!

%%
%% New bit syntax instructions added in February 2004 (R10B).
%%
resolve_inst({bs_init2,[Lbl,Arg2,{u,W},{u,R},{u,F},Arg6]},_,_,_) ->
    [A2,A6] = resolve_args([Arg2,Arg6]),
    {bs_init2,Lbl,A2,W,R,decode_field_flags(F),A6};
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
%% New instruction added in June 2005.
%%
resolve_inst({is_function2=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};

%%
%% New bit syntax matching added in Dec 2005 (R11B).
%%
resolve_inst({bs_start_match2=I,[F,Reg,{u,Live},{u,Max},Ms]},_,_,_) ->
    {test,I,F,[Reg,Live,Max,Ms]};
resolve_inst({bs_get_integer2=I,[Lbl,Ms,{u,Live},Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {test,I,Lbl,[Ms, Live,A2,N,decode_field_flags(U),A5]};
resolve_inst({bs_get_binary2=I,[Lbl,Ms,{u,Live},Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {test,I,Lbl,[Ms, Live,A2,N,decode_field_flags(U),A5]};
resolve_inst({bs_get_float2=I,[Lbl,Ms,{u,Live},Arg2,{u,N},{u,U},Arg5]},_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5]),
    {test,I,Lbl,[Ms, Live,A2,N,decode_field_flags(U),A5]};
resolve_inst({bs_skip_bits2=I,[Lbl,Ms,Arg2,{u,N},{u,U}]},_,_,_) ->
    A2 = resolve_arg(Arg2),
    {test,I,Lbl,[Ms,A2,N,decode_field_flags(U)]};
resolve_inst({bs_test_tail2=I,[F,Ms,{u,N}]},_,_,_) ->
    {test,I,F,[Ms,N]};
resolve_inst({bs_save2=I,[Ms,{u,N}]},_,_,_) ->
    {I,Ms,N};
resolve_inst({bs_restore2=I,[Ms,{u,N}]},_,_,_) ->
    {I,Ms,N};
resolve_inst({bs_save2=I,[Ms,{atom,_}=Atom]},_,_,_) ->
    %% New operand type in R12B.
    {I,Ms,Atom};
resolve_inst({bs_restore2=I,[Ms,{atom,_}=Atom]},_,_,_) ->
    %% New operand type in R12B.
    {I,Ms,Atom};

%%
%% New instructions for guard BIFs that may GC. Added in Jan 2006 (R11B).
%%
resolve_inst({gc_bif1,Args},Imports,_,_) ->
    [F,Live,Bif,A1,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lookup(Bif+1,Imports),
    {gc_bif,BifName,F,Live,[A1],Reg};
resolve_inst({gc_bif2,Args},Imports,_,_) ->
    [F,Live,Bif,A1,A2,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lookup(Bif+1,Imports),
    {gc_bif,BifName,F,Live,[A1,A2],Reg};

%%
%% New instruction in R14, gc_bif with 3 arguments
%%
resolve_inst({gc_bif3,Args},Imports,_,_) ->
    [F,Live,Bif,A1,A2,A3,Reg] = resolve_args(Args),
    {extfunc,_Mod,BifName,_Arity} = lookup(Bif+1,Imports),
    {gc_bif,BifName,F,Live,[A1,A2,A3],Reg};

%%
%% R11B-5.
%% 
resolve_inst({is_bitstr=I,Args0},_,_,_) ->
    [L|Args] = resolve_args(Args0),
    {test,I,L,Args};

%%
%% R12B.
%%
resolve_inst({bs_context_to_binary=I,[Reg0]},_,_,_) ->
    Reg = resolve_arg(Reg0),
    {I,Reg};
resolve_inst({bs_test_unit=I,[F,Ms,{u,N}]},_,_,_) ->
    {test,I,F,[Ms,N]};
resolve_inst({bs_match_string=I,[F,Ms,{u,Bits},{u,Off}]},_,Strings,_) ->
    Len = (Bits+7) div 8,
    String = if
		 Len > 0 -> 
		     <<_:Off/binary,Bin:Len/binary,_/binary>> = Strings,
		     Bin;
		 true -> <<>>
	     end,
    {test,I,F,[Ms,Bits,String]};
resolve_inst({bs_init_writable=I,[]},_,_,_) ->
    I;
resolve_inst({bs_append=I,[Lbl,Arg2,{u,W},{u,R},{u,U},Arg6,{u,F},Arg8]},_,_,_) ->
    [A2,A6,A8] = resolve_args([Arg2,Arg6,Arg8]),
    {I,Lbl,A2,W,R,U,A6,decode_field_flags(F),A8};
resolve_inst({bs_private_append=I,[Lbl,Arg2,{u,U},Arg4,{u,F},Arg6]},_,_,_) ->
    [A2,A4,A6] = resolve_args([Arg2,Arg4,Arg6]),
    {I,Lbl,A2,U,A4,decode_field_flags(F),A6};
resolve_inst({trim=I,[{u,N},{u,Remaining}]},_,_,_) ->
    {I,N,Remaining};
resolve_inst({bs_init_bits,[Lbl,Arg2,{u,W},{u,R},{u,F},Arg6]},_,_,_) ->
    [A2,A6] = resolve_args([Arg2,Arg6]),
    {bs_init_bits,Lbl,A2,W,R,decode_field_flags(F),A6};

%%
%% R12B-5.
%%
resolve_inst({bs_get_utf8=I,[Lbl,Arg2,Arg3,{u,U},Arg4]},_,_,_) ->
    [A2,A3,A4] = resolve_args([Arg2,Arg3,Arg4]),
    {test,I,Lbl,[A2,A3,decode_field_flags(U),A4]};
resolve_inst({bs_skip_utf8=I,[Lbl,Arg2,Arg3,{u,U}]},_,_,_) ->
    [A2,A3] = resolve_args([Arg2,Arg3]),
    {test,I,Lbl,[A2,A3,decode_field_flags(U)]};
resolve_inst({bs_get_utf16=I,[Lbl,Arg2,Arg3,{u,U},Arg4]},_,_,_) ->
    [A2,A3,A4] = resolve_args([Arg2,Arg3,Arg4]),
    {test,I,Lbl,[A2,A3,decode_field_flags(U),A4]};
resolve_inst({bs_skip_utf16=I,[Lbl,Arg2,Arg3,{u,U}]},_,_,_) ->
    [A2,A3] = resolve_args([Arg2,Arg3]),
    {test,I,Lbl,[A2,A3,decode_field_flags(U)]};
resolve_inst({bs_get_utf32=I,[Lbl,Arg2,Arg3,{u,U},Arg4]},_,_,_) ->
    [A2,A3,A4] = resolve_args([Arg2,Arg3,Arg4]),
    {test,I,Lbl,[A2,A3,decode_field_flags(U),A4]};
resolve_inst({bs_skip_utf32=I,[Lbl,Arg2,Arg3,{u,U}]},_,_,_) ->
    [A2,A3] = resolve_args([Arg2,Arg3]),
    {test,I,Lbl,[A2,A3,decode_field_flags(U)]};
resolve_inst({bs_utf8_size=I,[Lbl,Arg2,Arg3]},_,_,_) ->
    [A2,A3] = resolve_args([Arg2,Arg3]),
    {I,Lbl,A2,A3};
resolve_inst({bs_put_utf8=I,[Lbl,{u,U},Arg3]},_,_,_) ->
    A3 = resolve_arg(Arg3),
    {I,Lbl,decode_field_flags(U),A3};
resolve_inst({bs_utf16_size=I,[Lbl,Arg2,Arg3]},_,_,_) ->
    [A2,A3] = resolve_args([Arg2,Arg3]),
    {I,Lbl,A2,A3};
resolve_inst({bs_put_utf16=I,[Lbl,{u,U},Arg3]},_,_,_) ->
    A3 = resolve_arg(Arg3),
    {I,Lbl,decode_field_flags(U),A3};
resolve_inst({bs_put_utf32=I,[Lbl,{u,U},Arg3]},_,_,_) ->
    A3 = resolve_arg(Arg3),
    {I,Lbl,decode_field_flags(U),A3};

%%
%% R13B03.
%%
resolve_inst({on_load,[]},_,_,_) ->
    on_load;

%%
%% R14A.
%%
resolve_inst({recv_mark,[Lbl]},_,_,_) ->
    {recv_mark,Lbl};
resolve_inst({recv_set,[Lbl]},_,_,_) ->
    {recv_set,Lbl};

%%
%% R15A.
%%
resolve_inst({line,[Index]},_,_,_) ->
    {line,resolve_arg(Index)};

%%
%% 17.0
%%
resolve_inst({put_map_assoc,Args},_,_,_) ->
    [FLbl,Src,Dst,{u,N},{{z,1},{u,_Len},List0}] = Args,
    List = resolve_args(List0),
    {put_map_assoc,FLbl,Src,Dst,N,{list,List}};
resolve_inst({put_map_exact,Args},_,_,_) ->
    [FLbl,Src,Dst,{u,N},{{z,1},{u,_Len},List0}] = Args,
    List = resolve_args(List0),
    {put_map_exact,FLbl,Src,Dst,N,{list,List}};
resolve_inst({is_map=I,Args0},_,_,_) ->
    [FLbl|Args] = resolve_args(Args0),
    {test,I,FLbl,Args};
resolve_inst({has_map_fields,Args0},_,_,_) ->
    [FLbl,Src,{{z,1},{u,_Len},List0}] = Args0,
    List = resolve_args(List0),
    {test,has_map_fields,FLbl,Src,{list,List}};
resolve_inst({get_map_elements,Args0},_,_,_) ->
    [FLbl,Src,{{z,1},{u,_Len},List0}] = Args0,
    List = resolve_args(List0),
    {get_map_elements,FLbl,Src,{list,List}};

%%
%% OTP 21.
%%

resolve_inst({build_stacktrace,[]},_,_,_) ->
    build_stacktrace;
resolve_inst({raw_raise,[]},_,_,_) ->
    raw_raise;
resolve_inst({get_hd,[Src,Dst]},_,_,_) ->
    {get_hd,Src,Dst};
resolve_inst({get_tl,[Src,Dst]},_,_,_) ->
    {get_tl,Src,Dst};

%% OTP 22
resolve_inst({bs_start_match3,[Fail,Bin,Live,Dst]},_,_,_) ->
    {bs_start_match3,Fail,Bin,Live,Dst};
resolve_inst({bs_get_tail,[Src,Dst,Live]},_,_,_) ->
    {bs_get_tail,Src,Dst,Live};
resolve_inst({bs_get_position,[Src,Dst,Live]},_,_,_) ->
    {bs_get_position,Src,Dst,Live};
resolve_inst({bs_set_position,[Src,Dst]},_,_,_) ->
    {bs_set_position,Src,Dst};

%%
%% OTP 22.
%%
resolve_inst({put_tuple2,[Dst,{{z,1},{u,_},List0}]},_,_,_) ->
    List = resolve_args(List0),
    {put_tuple2,Dst,{list,List}};

%%
%% Catches instructions that are not yet handled.
%%
resolve_inst(X,_,_,_) -> ?exit({resolve_inst,X}).

%%-----------------------------------------------------------------------
%% Resolves arguments in a generic way.
%%-----------------------------------------------------------------------

resolve_args(Args) -> [resolve_arg(A) || A <- Args].

resolve_arg({x,N} = Arg) when is_integer(N), N >= 0 -> Arg;
resolve_arg({y,N} = Arg) when is_integer(N), N >= 0 -> Arg;
resolve_arg({fr,N} = Arg) when is_integer(N), N >= 0 -> Arg;
resolve_arg({f,N} = Arg) when is_integer(N), N >= 0 -> Arg;
resolve_arg({u,_} = Arg) -> resolve_arg_unsigned(Arg);
resolve_arg({i,_} = Arg) -> resolve_arg_integer(Arg);
resolve_arg({atom,Atom} = Arg) when is_atom(Atom) -> Arg;
resolve_arg({float,F} = Arg) when is_float(F) -> Arg;
resolve_arg({literal,_} = Arg) -> Arg;
resolve_arg(nil) -> nil.

resolve_arg_unsigned({u,N}) when is_integer(N), N >= 0 -> N.

resolve_arg_integer({i,N}) when is_integer(N) -> {integer,N}.

%%-----------------------------------------------------------------------
%% The purpose of the following is just to add a hook for future changes.
%% Currently, field flags are numbers 1-2-4-8 and only two of these
%% numbers (BSF_LITTLE 2 -- BSF_SIGNED 4) have a semantic significance;
%% others are just hints for speeding up the execution; see "erl_bits.h".
%%-----------------------------------------------------------------------

decode_field_flags(FF) ->
    {field_flags,FF}.

%%-----------------------------------------------------------------------
%% Private Utilities
%%-----------------------------------------------------------------------

mk_imports(ImportList) ->
    gb_trees:from_orddict([{I,{extfunc,M,F,A}} || {I,M,F,A} <- ImportList]).

mk_atoms(AtomList) ->
    gb_trees:from_orddict(AtomList).

mk_labels(LabelList) ->
    gb_trees:from_orddict(LabelList).

lookup(I, Imports) ->
    gb_trees:get(I, Imports).
