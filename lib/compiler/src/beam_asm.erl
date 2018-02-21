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
%% Purpose : Assembler for threaded Beam.

-module(beam_asm).

-export([module/4]).
-export([encode/2]).

-export_type([fail/0,label/0,reg/0,reg_num/0,src/0,module_code/0,function_name/0]).

-import(lists, [map/2,member/2,keymember/3,duplicate/2,splitwith/2]).
-include("beam_opcodes.hrl").

%% Common types for describing operands for BEAM instructions.
-type reg_num() :: 0..1023.
-type reg() :: {'x',reg_num()} | {'y',reg_num()}.
-type src() :: reg() |
	       {'literal',term()} |
	       {'atom',atom()} |
	       {'integer',integer()} |
	       'nil' |
	       {'float',float()}.
-type label() :: pos_integer().
-type fail() :: {'f',label() | 0}.

%% asm_instruction() describes only the instructions that
%% are used in BEAM files (as opposed to internal instructions
%% used only during optimization).

-type asm_instruction() :: atom() | tuple().

-type function_name() :: atom().

-type asm_function() ::
        {'function',function_name(),arity(),label(),[asm_instruction()]}.

-type module_code() ::
        {module(),[_],[_],[asm_function()],pos_integer()}.

-spec module(module_code(), [{binary(), binary()}], [{atom(),term()}], [compile:option()]) ->
                    {'ok',binary()}.

module(Code, ExtraChunks, CompileInfo, CompilerOpts) ->
    {ok,assemble(Code, ExtraChunks, CompileInfo, CompilerOpts)}.

assemble({Mod,Exp0,Attr0,Asm0,NumLabels}, ExtraChunks, CompileInfo, CompilerOpts) ->
    {1,Dict0} = beam_dict:atom(Mod, beam_dict:new()),
    {0,Dict1} = beam_dict:fname(atom_to_list(Mod) ++ ".erl", Dict0),
    NumFuncs = length(Asm0),
    {Asm,Attr} = on_load(Asm0, Attr0),
    Exp = cerl_sets:from_list(Exp0),
    {Code,Dict2} = assemble_1(Asm, Exp, Dict1, []),
    build_file(Code, Attr, Dict2, NumLabels, NumFuncs, ExtraChunks, CompileInfo, CompilerOpts).

on_load(Fs0, Attr0) ->
    case proplists:get_value(on_load, Attr0) of
	undefined ->
	    {Fs0,Attr0};
	[{Name,0}] ->
	    Fs = map(fun({function,N,0,Entry,Is0}) when N =:= Name ->
			     Is = insert_on_load_instruction(Is0, Entry),
			     {function,N,0,Entry,Is};
			(F) ->
			     F
		     end, Fs0),
	    Attr = proplists:delete(on_load, Attr0),
	    {Fs,Attr}
    end.

insert_on_load_instruction(Is0, Entry) ->
    {Bef,[{label,Entry}=El|Is]} =
	splitwith(fun({label,L}) when L =:= Entry -> false;
		     (_) -> true
		  end, Is0),
    Bef ++ [El,on_load|Is].

assemble_1([{function,Name,Arity,Entry,Asm}|T], Exp, Dict0, Acc) ->
    Dict1 = case cerl_sets:is_element({Name,Arity}, Exp) of
		true ->
		    beam_dict:export(Name, Arity, Entry, Dict0);
		false ->
		    beam_dict:local(Name, Arity, Entry, Dict0)
	    end,
    {Code, Dict2} = assemble_function(Asm, Acc, Dict1),
    assemble_1(T, Exp, Dict2, Code);
assemble_1([], _Exp, Dict0, Acc) ->
    {IntCodeEnd,Dict1} = make_op(int_code_end, Dict0),
    {list_to_binary(lists:reverse(Acc, [IntCodeEnd])),Dict1}.

assemble_function([H|T], Acc, Dict0) ->
    {Code, Dict} = make_op(H, Dict0),
    assemble_function(T, [Code| Acc], Dict);
assemble_function([], Code, Dict) ->
    {Code, Dict}.

build_file(Code, Attr, Dict, NumLabels, NumFuncs, ExtraChunks, CompileInfo, CompilerOpts) ->
    %% Create the code chunk.

    CodeChunk = chunk(<<"Code">>,
		      <<16:32,
		       (beam_opcodes:format_number()):32,
		       (beam_dict:highest_opcode(Dict)):32,
		       NumLabels:32,
		       NumFuncs:32>>,
		      Code),

    %% Create the atom table chunk.
    AtomEncoding = atom_encoding(CompilerOpts),
    {NumAtoms, AtomTab} = beam_dict:atom_table(Dict, AtomEncoding),
    AtomChunk = chunk(atom_chunk_name(AtomEncoding), <<NumAtoms:32>>, AtomTab),

    %% Create the import table chunk.

    {NumImps, ImpTab0} = beam_dict:import_table(Dict),
    Imp = flatten_imports(ImpTab0),
    ImportChunk = chunk(<<"ImpT">>, <<NumImps:32>>, Imp),

    %% Create the export table chunk.

    {NumExps, ExpTab0} = beam_dict:export_table(Dict),
    Exp = flatten_exports(ExpTab0),
    ExpChunk = chunk(<<"ExpT">>, <<NumExps:32>>, Exp),

    %% Create the local function table chunk.

    {NumLocals, Locals} = beam_dict:local_table(Dict),
    Loc = flatten_exports(Locals),
    LocChunk = chunk(<<"LocT">>, <<NumLocals:32>>, Loc),

    %% Create the string table chunk.

    {_,StringTab} = beam_dict:string_table(Dict),
    StringChunk = chunk(<<"StrT">>, StringTab),

    %% Create the fun table chunk. It is important not to build an empty chunk,
    %% as that would change the MD5.

    LambdaChunk = case beam_dict:lambda_table(Dict) of
		      {0,[]} -> [];
		      {NumLambdas,LambdaTab} ->
			  chunk(<<"FunT">>, <<NumLambdas:32>>, LambdaTab)
		  end,

    %% Create the literal table chunk. It is important not to build an empty chunk,
    %% as that would change the MD5.

    LiteralChunk = case beam_dict:literal_table(Dict) of
		       {0,[]} -> [];
		       {NumLiterals,LitTab0} ->
			   LitTab1 = [<<NumLiterals:32>>,LitTab0],
			   LitTab = zlib:compress(LitTab1),
			   chunk(<<"LitT">>, <<(iolist_size(LitTab1)):32>>,
				 LitTab)
		   end,

    %% Create the line chunk.
    
    LineChunk = chunk(<<"Line">>, build_line_table(Dict)),

    %% Create the attributes and compile info chunks.

    Essentials0 = [AtomChunk,CodeChunk,StringChunk,ImportChunk,
		   ExpChunk,LambdaChunk,LiteralChunk],
    Essentials1 = [iolist_to_binary(C) || C <- Essentials0],
    MD5 = module_md5(Essentials1),
    Essentials = finalize_fun_table(Essentials1, MD5),
    {Attributes,Compile} = build_attributes(Attr, CompileInfo, MD5),
    AttrChunk = chunk(<<"Attr">>, Attributes),
    CompileChunk = chunk(<<"CInf">>, Compile),

    %% Compile all extra chunks.

    CheckedChunks = [chunk(Key, Value) || {Key, Value} <- ExtraChunks],

    %% Create IFF chunk.

    Chunks = case member(slim, CompilerOpts) of
		 true ->
		     [Essentials,AttrChunk];
		 false ->
		     [Essentials,LocChunk,AttrChunk,
		      CompileChunk,CheckedChunks,LineChunk]
	     end,
    build_form(<<"BEAM">>, Chunks).

atom_encoding(Opts) ->
    case proplists:get_bool(no_utf8_atoms, Opts) of
	false -> utf8;
	true -> latin1
    end.

atom_chunk_name(utf8) -> <<"AtU8">>;
atom_chunk_name(latin1) -> <<"Atom">>.

%% finalize_fun_table(Essentials, MD5) -> FinalizedEssentials
%%  Update the 'old_uniq' field in the entry for each fun in the
%%  'FunT' chunk. We'll use part of the MD5 for the module as a
%%  unique value.

finalize_fun_table(Essentials, MD5) ->
    [finalize_fun_table_1(E, MD5) || E <- Essentials].

finalize_fun_table_1(<<"FunT",Keep:8/binary,Table0/binary>>, MD5) ->
    <<Uniq:27,_:101/bits>> = MD5,
    Table = finalize_fun_table_2(Table0, Uniq, <<>>),
    <<"FunT",Keep/binary,Table/binary>>;
finalize_fun_table_1(Chunk, _) -> Chunk.

finalize_fun_table_2(<<Keep:20/binary,0:32,T/binary>>, Uniq, Acc) ->
    finalize_fun_table_2(T, Uniq, <<Acc/binary,Keep/binary,Uniq:32>>);
finalize_fun_table_2(<<>>, _, Acc) -> Acc.

%% Build an IFF form.

build_form(Id, Chunks0) when byte_size(Id) =:= 4, is_list(Chunks0) ->
    Chunks = list_to_binary(Chunks0),
    Size = byte_size(Chunks),
    0 = Size rem 4,				% Assertion: correct padding?
    <<"FOR1",(Size+4):32,Id/binary,Chunks/binary>>.

%% Build a correctly padded chunk (with no sub-header).

chunk(Id, Contents) when byte_size(Id) =:= 4, is_binary(Contents) ->
    Size = byte_size(Contents),
    [<<Id/binary,Size:32>>,Contents|pad(Size)].

%% Build a correctly padded chunk (with a sub-header).

chunk(Id, Head, Contents) when byte_size(Id) =:= 4, is_binary(Head), is_binary(Contents) ->
    Size = byte_size(Head)+byte_size(Contents),
    [<<Id/binary,Size:32,Head/binary>>,Contents|pad(Size)];
chunk(Id, Head, Contents) when is_list(Contents) ->
    chunk(Id, Head, list_to_binary(Contents)).

pad(Size) ->
    case Size rem 4 of
	0 -> [];
	Rem -> duplicate(4 - Rem, 0)
    end.

flatten_exports(Exps) ->
    list_to_binary(map(fun({F,A,L}) -> <<F:32,A:32,L:32>> end, Exps)).

flatten_imports(Imps) ->
    list_to_binary(map(fun({M,F,A}) -> <<M:32,F:32,A:32>> end, Imps)).

build_attributes(Attr, Compile, MD5) ->
    AttrBinary = term_to_binary(set_vsn_attribute(Attr, MD5)),
    CompileBinary = term_to_binary([{version,?COMPILER_VSN}|Compile]),
    {AttrBinary,CompileBinary}.

build_line_table(Dict) ->
    {NumLineInstrs,NumFnames0,Fnames0,NumLines,Lines0} =
	beam_dict:line_table(Dict),
    NumFnames = NumFnames0 - 1,
    [_|Fnames1] = Fnames0,
    Fnames2 = [unicode:characters_to_binary(F) || F <- Fnames1],
    Fnames = << <<(byte_size(F)):16,F/binary>> || F <- Fnames2 >>,
    Lines1 = encode_line_items(Lines0, 0),
    Lines = iolist_to_binary(Lines1),
    Ver = 0,
    Bits = 0,
    <<Ver:32,Bits:32,NumLineInstrs:32,NumLines:32,NumFnames:32,
     Lines/binary,Fnames/binary>>.

%% encode_line_items([{FnameIndex,Line}], PrevFnameIndex)
%%  Encode the line items compactly. Tag the FnameIndex with
%%  an 'a' tag (atom) and only include it when it has changed.
%%  Tag the line numbers with an 'i' (integer) tag.

encode_line_items([{F,L}|T], F) ->
    [encode(?tag_i, L)|encode_line_items(T, F)];
encode_line_items([{F,L}|T], _) ->
    [encode(?tag_a, F),encode(?tag_i, L)|encode_line_items(T, F)];
encode_line_items([], _) -> [].

%%
%% If the attributes contains no 'vsn' attribute, we'll insert one
%% with an MD5 "checksum" calculated on the code as its value.
%% We'll not change an existing 'vsn' attribute.
%%

set_vsn_attribute(Attr, MD5) ->
    case keymember(vsn, 1, Attr) of
	true -> Attr;
	false ->
	    <<Number:128>> = MD5,
	    [{vsn,[Number]}|Attr]
    end.

module_md5(Essentials0) ->
    Essentials = filter_essentials(Essentials0),
    erlang:md5(Essentials).

%% filter_essentials([Chunk]) -> [Chunk']
%%  Filter essentials so that we obtain the same MD5 as code:module_md5/1 and
%%  beam_lib:md5/1 would calculate for this module.  Note that at this
%%  point, the 'old_uniq' entry for each fun in the 'FunT' chunk is zeroed,
%%  so there is no need to go through the 'FunT' chunk.

filter_essentials([<<_Tag:4/binary,Sz:32,Data:Sz/binary,_Padding/binary>>|T]) ->
    [Data|filter_essentials(T)];
filter_essentials([<<>>|T]) ->
    filter_essentials(T);
filter_essentials([]) -> [].

bif_type(fnegate, 1) -> {op,fnegate};
bif_type(fadd, 2)   -> {op,fadd};
bif_type(fsub, 2)   -> {op,fsub};
bif_type(fmul, 2)   -> {op,fmul};
bif_type(fdiv, 2)   -> {op,fdiv};
bif_type(_, 1)      -> bif1;
bif_type(_, 2)      -> bif2.

make_op({'%',_}, Dict) ->
    {[],Dict};
make_op({line,Location}, Dict0) ->
    {Index,Dict} = beam_dict:line(Location, Dict0),
    encode_op(line, [Index], Dict);
make_op({bif, Bif, {f,_}, [], Dest}, Dict) ->
    %% BIFs without arguments cannot fail.
    encode_op(bif0, [{extfunc, erlang, Bif, 0}, Dest], Dict);
make_op({bif, raise, _Fail, [_A1,_A2] = Args, _Dest}, Dict) ->
    encode_op(raise, Args, Dict);
make_op({bif,Bif,Fail,Args,Dest}, Dict) ->
    Arity = length(Args),
    case bif_type(Bif, Arity) of
	{op,Op} ->
	    make_op(list_to_tuple([Op,Fail|Args++[Dest]]), Dict);
	BifOp when is_atom(BifOp) ->
	    encode_op(BifOp, [Fail,{extfunc,erlang,Bif,Arity}|Args++[Dest]],
		      Dict)
    end;
make_op({gc_bif,Bif,Fail,Live,Args,Dest}, Dict) ->
    Arity = length(Args),
    BifOp = case Arity of
		1 -> gc_bif1;
		2 -> gc_bif2;
		3 -> gc_bif3
	    end,
    encode_op(BifOp, [Fail,Live,{extfunc,erlang,Bif,Arity}|Args++[Dest]],Dict);
make_op({bs_add=Op,Fail,[Src1,Src2,Unit],Dest}, Dict) ->
    encode_op(Op, [Fail,Src1,Src2,Unit,Dest], Dict);
make_op({test,Cond,Fail,Src,{list,_}=Ops}, Dict) ->
    encode_op(Cond, [Fail,Src,Ops], Dict);
make_op({test,Cond,Fail,Ops}, Dict) when is_list(Ops) ->
    encode_op(Cond, [Fail|Ops], Dict);
make_op({test,Cond,Fail,Live,[Op|Ops],Dst}, Dict) when is_list(Ops) ->
    encode_op(Cond, [Fail,Op,Live|Ops++[Dst]], Dict);
make_op({make_fun2,{f,Lbl},_Index,_OldUniq,NumFree}, Dict0) ->
    {Fun,Dict} = beam_dict:lambda(Lbl, NumFree, Dict0),
    make_op({make_fun2,Fun}, Dict);
make_op({kill,Y}, Dict) ->
    make_op({init,Y}, Dict);
make_op({Name,Arg1}, Dict) ->
    encode_op(Name, [Arg1], Dict);
make_op({Name,Arg1,Arg2}, Dict) ->
    encode_op(Name, [Arg1,Arg2], Dict);
make_op({Name,Arg1,Arg2,Arg3}, Dict) ->
    encode_op(Name, [Arg1,Arg2,Arg3], Dict);
make_op({Name,Arg1,Arg2,Arg3,Arg4}, Dict) ->
    encode_op(Name, [Arg1,Arg2,Arg3,Arg4], Dict);
make_op({Name,Arg1,Arg2,Arg3,Arg4,Arg5}, Dict) ->
    encode_op(Name, [Arg1,Arg2,Arg3,Arg4,Arg5], Dict);
make_op({Name,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6}, Dict) ->
    encode_op(Name, [Arg1,Arg2,Arg3,Arg4,Arg5,Arg6], Dict);
%% make_op({Name,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7}, Dict) ->
%%     encode_op(Name, [Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7], Dict);
make_op({Name,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8}, Dict) ->
    encode_op(Name, [Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8], Dict);
make_op(Op, Dict) when is_atom(Op) ->
    encode_op(Op, [], Dict).

encode_op(Name, Args, Dict0) when is_atom(Name) ->
    Op = beam_opcodes:opcode(Name, length(Args)),
    Dict = beam_dict:opcode(Op, Dict0),
    encode_op_1(Args, Dict, Op).

encode_op_1([A0|As], Dict0, Acc) ->
    {A,Dict} = encode_arg(A0, Dict0),
    encode_op_1(As, Dict, [Acc,A]);
encode_op_1([], Dict, Acc) -> {Acc,Dict}.

encode_arg({x, X}, Dict) when is_integer(X), X >= 0 ->
    {encode(?tag_x, X), Dict};
encode_arg({y, Y}, Dict) when is_integer(Y), Y >= 0 ->
    {encode(?tag_y, Y), Dict};
encode_arg({atom, Atom}, Dict0) when is_atom(Atom) ->
    {Index, Dict} = beam_dict:atom(Atom, Dict0),
    {encode(?tag_a, Index), Dict};
encode_arg({integer, N}, Dict) ->
    %% Conservatily assume that all integers whose absolute
    %% value is greater than 1 bsl 128 will be bignums in
    %% the runtime system.
    if
        N >= 1 bsl 128 ->
            encode_arg({literal, N}, Dict);
        N =< -(1 bsl 128) ->
            encode_arg({literal, N}, Dict);
        true ->
            {encode(?tag_i, N), Dict}
    end;
encode_arg(nil, Dict) ->
    {encode(?tag_a, 0), Dict};
encode_arg({f, W}, Dict) ->
    {encode(?tag_f, W), Dict};
%% encode_arg({'char', C}, Dict) ->
%%     {encode(?tag_h, C), Dict};
encode_arg({string, String}, Dict0) ->
    {Offset, Dict} = beam_dict:string(String, Dict0),
    {encode(?tag_u, Offset), Dict};
encode_arg({extfunc, M, F, A}, Dict0) ->
    {Index, Dict} = beam_dict:import(M, F, A, Dict0),
    {encode(?tag_u, Index), Dict};
encode_arg({list, List}, Dict0) ->
    {L, Dict} = encode_list(List, Dict0, []),
    {[encode(?tag_z, 1), encode(?tag_u, length(List))|L], Dict};
encode_arg({float, Float}, Dict) when is_float(Float) ->
    encode_arg({literal,Float}, Dict);
encode_arg({fr,Fr}, Dict) ->
    {[encode(?tag_z, 2),encode(?tag_u, Fr)], Dict};
encode_arg({field_flags,Flags0}, Dict) ->
    Flags = lists:foldl(fun (F, S) -> S bor flag_to_bit(F) end, 0, Flags0),
    {encode(?tag_u, Flags), Dict};
encode_arg({alloc,List}, Dict) ->
    encode_alloc_list(List, Dict);
encode_arg({literal,Lit}, Dict0) ->
    {Index,Dict} = beam_dict:literal(Lit, Dict0),
    {[encode(?tag_z, 4),encode(?tag_u, Index)],Dict};
encode_arg(Int, Dict) when is_integer(Int) ->
    {encode(?tag_u, Int),Dict}.

%%flag_to_bit(aligned) -> 16#01; %% No longer useful.
flag_to_bit(little)  -> 16#02;
flag_to_bit(big)     -> 16#00;
flag_to_bit(signed)  -> 16#04;
flag_to_bit(unsigned)-> 16#00;
%%flag_to_bit(exact)   -> 16#08;
flag_to_bit(native)  -> 16#10;
flag_to_bit({anno,_}) -> 0.
    
encode_list([H|T], Dict0, Acc) when not is_list(H) ->
    {Enc,Dict} = encode_arg(H, Dict0),
    encode_list(T, Dict, [Acc,Enc]);
encode_list([], Dict, Acc) -> {Acc,Dict}.

encode_alloc_list(L0, Dict0) ->
    {Bin,Dict} = encode_alloc_list_1(L0, Dict0, []),
    {[encode(?tag_z, 3),encode(?tag_u, length(L0)),Bin],Dict}.

encode_alloc_list_1([{words,Words}|T], Dict, Acc0) ->
    Acc = [Acc0,encode(?tag_u, 0),encode(?tag_u, Words)],
    encode_alloc_list_1(T, Dict, Acc);
encode_alloc_list_1([{floats,Floats}|T], Dict, Acc0) ->
    Acc = [Acc0,encode(?tag_u, 1),encode(?tag_u, Floats)],
    encode_alloc_list_1(T, Dict, Acc);
encode_alloc_list_1([], Dict, Acc) ->
    {iolist_to_binary(Acc),Dict}.

-spec encode(non_neg_integer(), integer()) -> iodata().

encode(Tag, N) when N < 0 ->
    encode1(Tag, negative_to_bytes(N));
encode(Tag, N) when N < 16 ->
    (N bsl 4) bor Tag;
encode(Tag, N) when N < 16#800  ->
    [((N bsr 3) band 2#11100000) bor Tag bor 2#00001000, N band 16#ff];
encode(Tag, N) ->
    encode1(Tag, to_bytes(N)).

encode1(Tag, Bytes) ->
    case iolist_size(Bytes) of
	Num when 2 =< Num, Num =< 8 ->
	    [((Num-2) bsl 5) bor 2#00011000 bor Tag| Bytes];
	Num when 8 < Num ->
	    [2#11111000 bor Tag, encode(?tag_u, Num-9)| Bytes]
    end.

to_bytes(N) ->
    Bin = binary:encode_unsigned(N),
    case Bin of
	<<0:1,_/bits>> -> Bin;
	<<1:1,_/bits>> -> [0,Bin]
    end.

negative_to_bytes(N) when N >= -16#8000 ->
    <<N:16>>;
negative_to_bytes(N) ->
    Bytes = byte_size(binary:encode_unsigned(-N)),
    Bin = <<N:Bytes/unit:8>>,
    case Bin of
	<<0:1,_/bits>> -> [16#ff,Bin];
	<<1:1,_/bits>> -> Bin
    end.
