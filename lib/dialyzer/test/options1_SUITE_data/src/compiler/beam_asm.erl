%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: beam_asm.erl,v 1.1 2008/12/17 09:53:40 mikpe Exp $
%% Purpose : Assembler for threaded Beam.

-module(beam_asm).

-export([module/4,format_error/1]).
-export([encode/2]).

-import(lists, [map/2,member/2,keymember/3,duplicate/2]).
-include("beam_opcodes.hrl").

-define(bs_aligned, 1).

module(Code, Abst, SourceFile, Opts) ->
    case assemble(Code, Abst, SourceFile, Opts) of
	{error, Error} ->
	    {error, [{none, ?MODULE, Error}]};
	Bin when binary(Bin) ->
	    {ok, Bin}
    end.

format_error({crashed, Why}) ->
    io_lib:format("beam_asm_int: EXIT: ~p", [Why]).

assemble({Mod,Exp,Attr,Asm,NumLabels}, Abst, SourceFile, Opts) ->
    {1,Dict0} = beam_dict:atom(Mod, beam_dict:new()),
    NumFuncs = length(Asm),
    {Code,Dict1} = assemble_1(Asm, Exp, Dict0, []),
    build_file(Code, Attr, Dict1, NumLabels, NumFuncs, Abst, SourceFile, Opts).

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

    %% Create the attributes and compile info chunks.

    Essentials = [AtomChunk,CodeChunk,StringChunk,ImportChunk,ExpChunk,LambdaChunk],
    {Attributes,Compile} = build_attributes(Opts, SourceFile, Attr, Essentials),
    AttrChunk = chunk(<<"Attr">>, Attributes),
    CompileChunk = chunk(<<"CInf">>, Compile),

    %% Create the abstract code chunk.

    AbstChunk = chunk(<<"Abst">>, Abst),

    %% Create IFF chunk.

    Chunks = case member(slim, Opts) of
		 true -> [Essentials,AttrChunk,CompileChunk,AbstChunk];
		 false -> [Essentials,LocChunk,AttrChunk,CompileChunk,AbstChunk]
	     end,
    build_form(<<"BEAM">>, Chunks).

%% Build an IFF form.

build_form(Id, Chunks0) when size(Id) == 4, list(Chunks0) ->
    Chunks = list_to_binary(Chunks0),
    Size = size(Chunks),
    0 = Size rem 4,				% Assertion: correct padding?
    <<"FOR1",(Size+4):32,Id/binary,Chunks/binary>>.

%% Build a correctly padded chunk (with no sub-header).

chunk(Id, Contents) when size(Id) == 4, binary(Contents) ->
    Size = size(Contents),
    [<<Id/binary,Size:32>>,Contents|pad(Size)];
chunk(Id, Contents) when list(Contents) ->
    chunk(Id, list_to_binary(Contents)).

%% Build a correctly padded chunk (with a sub-header).

chunk(Id, Head, Contents) when size(Id) == 4, is_binary(Head), is_binary(Contents) ->
    Size = size(Head)+size(Contents),
    [<<Id/binary,Size:32,Head/binary>>,Contents|pad(Size)];
chunk(Id, Head, Contents) when list(Contents) ->
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

calc_vsn(Attr, Essentials) ->
    case keymember(vsn, 1, Attr) of
	true -> Attr;
	false ->
	    <<Number:128>> = erlang:md5(Essentials),
	    [{vsn,[Number]}|Attr]
    end.

bif_type('-', 1)    -> negate;
bif_type('+', 2)    -> {op, m_plus};
bif_type('-', 2)    -> {op, m_minus};
bif_type('*', 2)    -> {op, m_times};
bif_type('/', 2)    -> {op, m_div};
bif_type('div', 2)  -> {op, int_div};
bif_type('rem', 2)  -> {op, int_rem};
bif_type('band', 2) -> {op, int_band};
bif_type('bor', 2)  -> {op, int_bor};
bif_type('bxor', 2) -> {op, int_bxor};
bif_type('bsl', 2)  -> {op, int_bsl};
bif_type('bsr', 2)  -> {op, int_bsr};
bif_type('bnot', 1) -> {op, int_bnot};
bif_type(fnegate, 1)   -> {op, fnegate};
bif_type(fadd, 2)   -> {op, fadd};
bif_type(fsub, 2)   -> {op, fsub};
bif_type(fmul, 2)   -> {op, fmul};
bif_type(fdiv, 2)   -> {op, fdiv};
bif_type(_, _)      -> bif.

make_op(Comment, Dict) when element(1, Comment) == '%' ->
    {[],Dict};
make_op({'%live',_R}, Dict) ->
    {[],Dict};
make_op({bif, Bif, nofail, [], Dest}, Dict) ->
    encode_op(bif0, [{extfunc, erlang, Bif, 0}, Dest], Dict);
make_op({bif, raise, _Fail, [A1,A2], _Dest}, Dict) ->
    encode_op(raise, [A1,A2], Dict);
make_op({bif, Bif, Fail, Args, Dest}, Dict) ->
    Arity = length(Args),
    case bif_type(Bif, Arity) of
	{op, Op} ->
	    make_op(list_to_tuple([Op, Fail|Args++[Dest]]), Dict);
	negate ->
	    %% Fake negation operator.
	    make_op({m_minus, Fail, {integer,0}, hd(Args), Dest}, Dict);
	bif ->
	    BifOp = list_to_atom(lists:concat([bif, Arity])),
	    encode_op(BifOp, [Fail, {extfunc, erlang, Bif, Arity}|Args++[Dest]],
		      Dict)
    end;
make_op({bs_add=Op,Fail,[Src1,Src2,Unit],Dest}, Dict) ->
    encode_op(Op, [Fail,Src1,Src2,Unit,Dest], Dict);
make_op({test,Cond,Fail,Ops}, Dict) when list(Ops) ->
    encode_op(Cond, [Fail|Ops], Dict);
make_op({make_fun2,{f,Lbl},Index,OldUniq,NumFree}, Dict0) ->
    {Fun,Dict} = beam_dict:lambda(Lbl, Index, OldUniq, NumFree, Dict0),
    make_op({make_fun2,Fun}, Dict);
make_op(Op, Dict) when atom(Op) ->
    encode_op(Op, [], Dict);
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
    encode_op(Name, [Arg1,Arg2,Arg3,Arg4,Arg5,Arg6], Dict).

encode_op(Name, Args, Dict0) when atom(Name) ->
    {EncArgs,Dict1} = encode_args(Args, Dict0),
    Op = beam_opcodes:opcode(Name, length(Args)),
    Dict2 = beam_dict:opcode(Op, Dict1),
    {list_to_binary([Op|EncArgs]),Dict2}.

encode_args([Arg| T], Dict0) ->
    {EncArg, Dict1} = encode_arg(Arg, Dict0),
    {EncTail, Dict2} = encode_args(T, Dict1),
    {[EncArg| EncTail], Dict2};
encode_args([], Dict) ->
    {[], Dict}.

encode_arg({x, X}, Dict) when X >= 0 ->
    {encode(?tag_x, X), Dict};
encode_arg({y, Y}, Dict) when Y >= 0 ->
    {encode(?tag_y, Y), Dict};
encode_arg({atom, Atom}, Dict0) when atom(Atom) ->
    {Index, Dict} = beam_dict:atom(Atom, Dict0),
    {encode(?tag_a, Index), Dict};
encode_arg({integer, N}, Dict) ->
    {encode(?tag_i, N), Dict};
encode_arg(nil, Dict) ->
    {encode(?tag_a, 0), Dict};
encode_arg({f, W}, Dict) ->
    {encode(?tag_f, W), Dict};
encode_arg({'char', C}, Dict) ->
    {encode(?tag_h, C), Dict};
encode_arg({string, String}, Dict0) ->
    {Offset, Dict} = beam_dict:string(String, Dict0),
    {encode(?tag_u, Offset), Dict};
encode_arg({extfunc, M, F, A}, Dict0) ->
    {Index, Dict} = beam_dict:import(M, F, A, Dict0),
    {encode(?tag_u, Index), Dict};
encode_arg({list, List}, Dict0) ->
    {L, Dict} = encode_list(List, Dict0, []),
    {[encode(?tag_z, 1), encode(?tag_u, length(List))|L], Dict};
encode_arg({float, Float}, Dict) when float(Float) ->
    {[encode(?tag_z, 0)|<<Float:64/float>>], Dict};
encode_arg({fr,Fr}, Dict) ->
    {[encode(?tag_z, 2),encode(?tag_u,Fr)], Dict};
encode_arg({field_flags,Flags0}, Dict) ->
    Flags = lists:foldl(fun (F, S) -> S bor flag_to_bit(F) end, 0, Flags0),
    {encode(?tag_u, Flags), Dict};
encode_arg({alloc,List}, Dict) ->
    {encode_alloc_list(List),Dict};
encode_arg(Int, Dict) when is_integer(Int) ->
    {encode(?tag_u, Int),Dict}.

flag_to_bit(aligned) -> 16#01;
flag_to_bit(little)  -> 16#02;
flag_to_bit(big)     -> 16#00;
flag_to_bit(signed)  -> 16#04;
flag_to_bit(unsigned)-> 16#00;
flag_to_bit(exact)   -> 16#08;
flag_to_bit(native) ->  16#10.

encode_list([H|T], _Dict, _Acc) when is_list(H) ->
    exit({illegal_nested_list,encode_arg,[H|T]});
encode_list([H|T], Dict0, Acc) ->
    {Enc,Dict} = encode_arg(H, Dict0),
    encode_list(T, Dict, [Enc|Acc]);
encode_list([], Dict, Acc) ->
    {lists:reverse(Acc), Dict}.

encode_alloc_list(L0) ->
    L = encode_alloc_list_1(L0),
    [encode(?tag_z, 3),encode(?tag_u, length(L0))|L].

encode_alloc_list_1([{words,Words}|T]) ->
    [encode(?tag_u, 0),encode(?tag_u, Words)|encode_alloc_list_1(T)];
encode_alloc_list_1([{floats,Floats}|T]) ->
    [encode(?tag_u, 1),encode(?tag_u, Floats)|encode_alloc_list_1(T)];
encode_alloc_list_1([]) -> [].

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

to_bytes(0, [B|Acc]) when B < 128 ->
    [B|Acc];
to_bytes(N, Acc) ->
    to_bytes(N bsr 8, [N band 16#ff| Acc]).

negative_to_bytes(-1, [B1, B2|T]) when B1 > 127 ->
    [B1, B2|T];
negative_to_bytes(N, Acc) ->
    negative_to_bytes(N bsr 8, [N band 16#ff|Acc]).
