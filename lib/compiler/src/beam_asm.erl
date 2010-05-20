%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
%% Purpose : Assembler for threaded Beam.

-module(beam_asm).

-export([module/4]).
-export([encode/2]).

-import(lists, [map/2,member/2,keymember/3,duplicate/2]).
-include("beam_opcodes.hrl").

module(Code, Abst, SourceFile, Opts) ->
    {ok,assemble(Code, Abst, SourceFile, Opts)}.

assemble({Mod,Exp,Attr0,Asm0,NumLabels}, Abst, SourceFile, Opts) ->
    {1,Dict0} = beam_dict:atom(Mod, beam_dict:new()),
    NumFuncs = length(Asm0),
    {Asm,Attr} = on_load(Asm0, Attr0),
    {Code,Dict1} = assemble_1(Asm, Exp, Dict0, []),
    build_file(Code, Attr, Dict1, NumLabels, NumFuncs, Abst, SourceFile, Opts).

on_load(Fs0, Attr0) ->
    case proplists:get_value(on_load, Attr0) of
	undefined ->
	    {Fs0,Attr0};
	[{Name,0}] ->
	    Fs = map(fun({function,N,0,Entry,Asm0}) when N =:= Name ->
			     [{label,_}=L,
			      {func_info,_,_,_}=Fi,
			      {label,_}=E|Asm1] = Asm0,
			     Asm = [L,Fi,E,on_load|Asm1],
			     {function,N,0,Entry,Asm};
			(F) ->
			     F
		     end, Fs0),
	    Attr = proplists:delete(on_load, Attr0),
	    {Fs,Attr}
    end.

assemble_1([{function,Name,Arity,Entry,Asm}|T], Exp, Dict0, Acc) ->
    Dict1 = case member({Name,Arity}, Exp) of
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

build_file(Code, Attr, Dict, NumLabels, NumFuncs, Abst, SourceFile, Opts) ->
    %% Create the code chunk.

    CodeChunk = chunk(<<"Code">>,
		      <<16:32,
		       (beam_opcodes:format_number()):32,
		       (beam_dict:highest_opcode(Dict)):32,
		       NumLabels:32,
		       NumFuncs:32>>,
		      Code),

    %% Create the atom table chunk.

    {NumAtoms, AtomTab} = beam_dict:atom_table(Dict),
    AtomChunk = chunk(<<"Atom">>, <<NumAtoms:32>>, AtomTab),

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
			   LitTab1 = iolist_to_binary(LitTab0),
			   LitTab2 = <<NumLiterals:32,LitTab1/binary>>,
			   LitTab = iolist_to_binary(zlib:compress(LitTab2)),
			   chunk(<<"LitT">>, <<(byte_size(LitTab2)):32>>, LitTab)
		   end,
    

    %% Create the attributes and compile info chunks.

    Essentials0 = [AtomChunk,CodeChunk,StringChunk,ImportChunk,
		   ExpChunk,LambdaChunk,LiteralChunk],
    Essentials = [iolist_to_binary(C) || C <- Essentials0],
    {Attributes,Compile} = build_attributes(Opts, SourceFile, Attr, Essentials),
    AttrChunk = chunk(<<"Attr">>, Attributes),
    CompileChunk = chunk(<<"CInf">>, Compile),

    %% Create the abstract code chunk.

    AbstChunk = chunk(<<"Abst">>, Abst),

    %% Create IFF chunk.

    Chunks = case member(slim, Opts) of
		 true -> [Essentials,AttrChunk,AbstChunk];
		 false -> [Essentials,LocChunk,AttrChunk,CompileChunk,AbstChunk]
	     end,
    build_form(<<"BEAM">>, Chunks).

%% Build an IFF form.

build_form(Id, Chunks0) when byte_size(Id) =:= 4, is_list(Chunks0) ->
    Chunks = list_to_binary(Chunks0),
    Size = byte_size(Chunks),
    0 = Size rem 4,				% Assertion: correct padding?
    <<"FOR1",(Size+4):32,Id/binary,Chunks/binary>>.

%% Build a correctly padded chunk (with no sub-header).

chunk(Id, Contents) when byte_size(Id) =:= 4, is_binary(Contents) ->
    Size = byte_size(Contents),
    [<<Id/binary,Size:32>>,Contents|pad(Size)];
chunk(Id, Contents) when is_list(Contents) ->
    chunk(Id, list_to_binary(Contents)).

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

build_attributes(Opts, SourceFile, Attr, Essentials) ->
    Misc = case member(slim, Opts) of
	       false ->
		   {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
		   [{time,{Y,Mo,D,H,Mi,S}},{source,SourceFile}];
	       true -> []
	   end,
    Compile = [{options,Opts},{version,?COMPILER_VSN}|Misc],
    {term_to_binary(calc_vsn(Attr, Essentials)),term_to_binary(Compile)}.

%%
%% If the attributes contains no 'vsn' attribute, we'll insert one
%% with an MD5 "checksum" calculated on the code as its value.
%% We'll not change an existing 'vsn' attribute.
%%

calc_vsn(Attr, Essentials0) ->
    case keymember(vsn, 1, Attr) of
	true -> Attr;
	false ->
	    Essentials = filter_essentials(Essentials0),
	    <<Number:128>> = erlang:md5(Essentials),
	    [{vsn,[Number]}|Attr]
    end.

%% filter_essentials([Chunk]) -> [Chunk']
%%  Filter essentials so that we obtain the same MD5 as code:module_md5/1 and
%%  beam_lib:md5/1 would calculate for this module.

filter_essentials([<<"FunT",_Sz:4/binary,Entries:4/binary,Table0/binary>>|T]) ->
    Table = filter_funtab(Table0, <<0:32>>),
    [Entries,Table|filter_essentials(T)];
filter_essentials([<<_Tag:4/binary,Sz:32,Data:Sz/binary,_Padding/binary>>|T]) ->
    [Data|filter_essentials(T)];
filter_essentials([<<>>|T]) ->
    filter_essentials(T);
filter_essentials([]) -> [].

filter_funtab(<<Important:20/binary,_OldUniq:4/binary,T/binary>>, Zero) ->
    [Important,Zero|filter_funtab(T, Zero)];
filter_funtab(<<>>, _) -> [].

bif_type(fnegate, 1) -> {op,fnegate};
bif_type(fadd, 2)   -> {op,fadd};
bif_type(fsub, 2)   -> {op,fsub};
bif_type(fmul, 2)   -> {op,fmul};
bif_type(fdiv, 2)   -> {op,fdiv};
bif_type(_, 1)      -> bif1;
bif_type(_, 2)      -> bif2.

make_op({'%',_}, Dict) ->
    {[],Dict};
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
make_op({test,Cond,Fail,Ops}, Dict) when is_list(Ops) ->
    encode_op(Cond, [Fail|Ops], Dict);
make_op({test,Cond,Fail,Live,[Op|Ops],Dst}, Dict) when is_list(Ops) ->
    encode_op(Cond, [Fail,Op,Live|Ops++[Dst]], Dict);
make_op({make_fun2,{f,Lbl},Index,OldUniq,NumFree}, Dict0) ->
    {Fun,Dict} = beam_dict:lambda(Lbl, Index, OldUniq, NumFree, Dict0),
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
    {encode(?tag_i, N), Dict};
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
    {[encode(?tag_z, 0),<<Float:64/float>>], Dict};
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

encode(Tag, N) when N < 0 ->
    encode1(Tag, negative_to_bytes(N, []));
encode(Tag, N) when N < 16 ->
    (N bsl 4) bor Tag;
encode(Tag, N) when N < 16#800  ->
    [((N bsr 3) band 2#11100000) bor Tag bor 2#00001000, N band 16#ff];
encode(Tag, N) ->
    encode1(Tag, to_bytes(N, [])).

encode1(Tag, Bytes) ->
    case length(Bytes) of
	Num when 2 =< Num, Num =< 8 ->
	    [((Num-2) bsl 5) bor 2#00011000 bor Tag| Bytes];
	Num when 8 < Num ->
	    [2#11111000 bor Tag, encode(?tag_u, Num-9)| Bytes]
    end.


to_bytes(N0, Acc) ->
    Bits = 3*128,
    case N0 bsr Bits of
	0 ->
	    to_bytes_1(N0, Acc);
	N ->
	    to_bytes(N, binary_to_list(<<N0:Bits>>) ++ Acc)
    end.
    
to_bytes_1(0, [B|_]=Done) when B < 128 -> Done;
to_bytes_1(N, Acc) -> to_bytes(N bsr 8, [N band 16#ff|Acc]).

negative_to_bytes(N0, Acc) ->
    Bits = 3*128,
    case N0 bsr Bits of
	-1 ->
	    negative_to_bytes_1(N0, Acc);
	N ->
	    negative_to_bytes_1(N, binary_to_list(<<N0:Bits>>) ++ Acc)
    end.

negative_to_bytes_1(-1, [B1,_B2|_]=Done) when B1 > 127 ->
    Done;
negative_to_bytes_1(N, Acc) ->
    negative_to_bytes_1(N bsr 8, [N band 16#ff|Acc]).
