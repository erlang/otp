%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% Purpose : Partitions assembly instructions into basic blocks and
%% optimizes them.

-module(beam_bs).

-export([module/2]).
-import(lists, [mapfoldl/3,reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc0}, _Opt) ->
    {Fs,Lc} = mapfoldl(fun function/2, Lc0, Fs0),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}, Lc0) ->
    try
	Is1 = bs_put_opt(Is0),
	{Is,Lc} = bsm_opt(Is1, Lc0),
	{{function,Name,Arity,CLabel,Is},Lc}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%%%
%%% Evaluation of constant bit fields.
%%%

bs_put_opt([{bs_put,_,_,_}=I|Is0]) ->
    {BsPuts0,Is} = collect_bs_puts(Is0, [I]),
    BsPuts = opt_bs_puts(BsPuts0),
    BsPuts ++ bs_put_opt(Is);
bs_put_opt([I|Is]) ->
    [I|bs_put_opt(Is)];
bs_put_opt([]) -> [].

collect_bs_puts([{bs_put,_,_,_}=I|Is], Acc) ->
    collect_bs_puts(Is, [I|Acc]);
collect_bs_puts([_|_]=Is, Acc) ->
    {reverse(Acc),Is}.

opt_bs_puts(Is) ->
    opt_bs_1(Is, []).

opt_bs_1([{bs_put,Fail,
	   {bs_put_float,1,Flags0},[{integer,Sz},Src]}=I0|Is], Acc) ->
    try eval_put_float(Src, Sz, Flags0) of
	<<Int:Sz>> ->
	    Flags = force_big(Flags0),
	    I = {bs_put,Fail,{bs_put_integer,1,Flags},
		 [{integer,Sz},{integer,Int}]},
	    opt_bs_1([I|Is], Acc)
    catch
	error:_ ->
	    opt_bs_1(Is, [I0|Acc])
    end;
opt_bs_1([{bs_put,_,{bs_put_integer,1,_},[{integer,8},{integer,_}]}|_]=IsAll,
	 Acc0) ->
    {Is,Acc} = bs_collect_string(IsAll, Acc0),
    opt_bs_1(Is, Acc);
opt_bs_1([{bs_put,Fail,{bs_put_integer,1,F},[{integer,Sz},{integer,N}]}=I|Is0],
	 Acc) when Sz > 8 ->
    case field_endian(F) of
	big ->
	    %% We can do this optimization for any field size without
	    %% risk for code explosion.
	    case bs_split_int(N, Sz, Fail, Is0) of
		no_split -> opt_bs_1(Is0, [I|Acc]);
		Is -> opt_bs_1(Is, Acc)
	    end;
	little when Sz < 128 ->
	    %% We only try to optimize relatively small fields, to
	    %% avoid an explosion in code size.
	    <<Int:Sz>> = <<N:Sz/little>>,
	    Flags = force_big(F),
	    Is = [{bs_put,Fail,{bs_put_integer,1,Flags},
		   [{integer,Sz},{integer,Int}]}|Is0],
	    opt_bs_1(Is, Acc);
	_ ->			      %native or too wide little field
	    opt_bs_1(Is0, [I|Acc])
    end;
opt_bs_1([{bs_put,Fail,{Op,U,F},[{integer,Sz},Src]}|Is], Acc) when U > 1 ->
    opt_bs_1([{bs_put,Fail,{Op,1,F},[{integer,U*Sz},Src]}|Is], Acc);
opt_bs_1([I|Is], Acc) ->
    opt_bs_1(Is, [I|Acc]);
opt_bs_1([], Acc) -> reverse(Acc).

eval_put_float(Src, Sz, Flags) when Sz =< 256 ->
    %%Only evaluate if Sz is reasonable.
    Val = value(Src),
    case field_endian(Flags) of
	little -> <<Val:Sz/little-float-unit:1>>;
	big -> <<Val:Sz/big-float-unit:1>>
        %% native intentionally not handled here - we can't optimize
        %% it.
    end.

value({integer,I}) -> I;
value({float,F}) -> F.

bs_collect_string(Is, [{bs_put,_,{bs_put_string,Len,{string,Str}},[]}|Acc]) ->
    bs_coll_str_1(Is, Len, reverse(Str), Acc);
bs_collect_string(Is, Acc) ->
    bs_coll_str_1(Is, 0, [], Acc).

bs_coll_str_1([{bs_put,_,{bs_put_integer,U,_},[{integer,Sz},{integer,V}]}|Is],
	      Len, StrAcc, IsAcc) when U*Sz =:= 8 ->
    Byte = V band 16#FF,
    bs_coll_str_1(Is, Len+1, [Byte|StrAcc], IsAcc);
bs_coll_str_1(Is, Len, StrAcc, IsAcc) ->
    {Is,[{bs_put,{f,0},{bs_put_string,Len,{string,reverse(StrAcc)}},[]}|IsAcc]}.

field_endian({field_flags,F}) -> field_endian_1(F).

field_endian_1([big=E|_]) -> E;
field_endian_1([little=E|_]) -> E;
field_endian_1([native=E|_]) -> E;
field_endian_1([_|Fs]) -> field_endian_1(Fs).

force_big({field_flags,F}) ->
    {field_flags,force_big_1(F)}.

force_big_1([big|_]=Fs) -> Fs;
force_big_1([little|Fs]) -> [big|Fs];
force_big_1([F|Fs]) -> [F|force_big_1(Fs)].

bs_split_int(0, Sz, _, _) when Sz > 64 ->
    %% We don't want to split in this case because the
    %% string will consist of only zeroes.
    no_split;
bs_split_int(-1, Sz, _, _) when Sz > 64 ->
    %% We don't want to split in this case because the
    %% string will consist of only 255 bytes.
    no_split;
bs_split_int(N, Sz, Fail, Acc) ->
    FirstByteSz = case Sz rem 8 of
		      0 -> 8;
		      Rem -> Rem
		  end,
    bs_split_int_1(N, FirstByteSz, Sz, Fail, Acc).

bs_split_int_1(-1, _, Sz, Fail, Acc) when Sz > 64 ->
    I = {bs_put,Fail,{bs_put_integer,1,{field_flags,[big]}},
	 [{integer,Sz},{integer,-1}]},
    [I|Acc];
bs_split_int_1(0, _, Sz, Fail, Acc) when Sz > 64 ->
    I = {bs_put,Fail,{bs_put_integer,1,{field_flags,[big]}},
	 [{integer,Sz},{integer,0}]},
    [I|Acc];
bs_split_int_1(N, ByteSz, Sz, Fail, Acc) when Sz > 0 ->
    Mask = (1 bsl ByteSz) - 1,
    I = {bs_put,Fail,{bs_put_integer,1,{field_flags,[big]}},
	 [{integer,ByteSz},{integer,N band Mask}]},
    bs_split_int_1(N bsr ByteSz, 8, Sz-ByteSz, Fail, [I|Acc]);
bs_split_int_1(_, _, _, _, Acc) -> Acc.

%%%
%%% Optimization of bit syntax matching: get rid
%%% of redundant bs_restore2/2 instructions across select_val
%%% instructions, as well as a few other simple peep-hole
%%% optimizations.
%%%

bsm_opt(Is0, Lc0) ->
    {Is1,D0,Lc} = bsm_scan(Is0, [], Lc0, []),
    Is2 = case D0 of
	      [] ->
		  %% No bit syntax matching in this function.
		  Is1;
	      [_|_] ->
		  %% Optimize the bit syntax matching.
		  D = gb_trees:from_orddict(orddict:from_list(D0)),
		  bsm_reroute(Is1, D, none, [])
	 end,
    Is = beam_clean:bs_clean_saves(Is2),
    {bsm_opt_2(Is, []),Lc}.

bsm_scan([{label,L}=Lbl,{bs_restore2,_,Save}=R|Is], D0, Lc, Acc0) ->
    D = [{{L,Save},Lc}|D0],
    Acc = [{label,Lc},R,Lbl|Acc0],
    bsm_scan(Is, D, Lc+1, Acc);
bsm_scan([I|Is], D, Lc, Acc) ->
    bsm_scan(Is, D, Lc, [I|Acc]);
bsm_scan([], D, Lc, Acc) ->
    {reverse(Acc),D,Lc}.

bsm_reroute([{bs_save2,Reg,Save}=I|Is], D, _, Acc) ->
    bsm_reroute(Is, D, {Reg,Save}, [I|Acc]);
bsm_reroute([{bs_restore2,Reg,Save}=I|Is], D, _, Acc) ->
    bsm_reroute(Is, D, {Reg,Save}, [I|Acc]);
bsm_reroute([{label,_}=I|Is], D, S, Acc) ->
    bsm_reroute(Is, D, S, [I|Acc]);
bsm_reroute([{select,select_val,Reg,F0,Lbls0}|Is], D, {_,Save}=S, Acc0) ->
    [F|Lbls] = bsm_subst_labels([F0|Lbls0], Save, D),
    Acc = [{select,select_val,Reg,F,Lbls}|Acc0],
    bsm_reroute(Is, D, S, Acc);
bsm_reroute([{test,TestOp,F0,TestArgs}=I|Is], D, {_,Save}=S, Acc0) ->
    F = bsm_subst_label(F0, Save, D),
    Acc = [{test,TestOp,F,TestArgs}|Acc0],
    case bsm_not_bs_test(I) of
	true ->
	    %% The test instruction will not update the bit offset for
	    %% the binary being matched. Therefore the save position
	    %% can be kept.
	    bsm_reroute(Is, D, S, Acc);
	false ->
	    %% The test instruction might update the bit offset. Kill
	    %% our remembered Save position.
	    bsm_reroute(Is, D, none, Acc)
    end;
bsm_reroute([{test,TestOp,F0,Live,TestArgs,Dst}|Is], D, {_,Save}, Acc0) ->
    F = bsm_subst_label(F0, Save, D),
    Acc = [{test,TestOp,F,Live,TestArgs,Dst}|Acc0],
    %% The test instruction will update the bit offset. Kill our
    %% remembered Save position.
    bsm_reroute(Is, D, none, Acc);
bsm_reroute([{block,[{set,[],[],{alloc,_,_}}]}=Bl,
	     {bs_context_to_binary,_}=I|Is], D, S, Acc) ->
    %% To help further bit syntax optimizations.
    bsm_reroute([I,Bl|Is], D, S, Acc);
bsm_reroute([I|Is], D, _, Acc) ->
    bsm_reroute(Is, D, none, [I|Acc]);
bsm_reroute([], _, _, Acc) -> reverse(Acc).

bsm_opt_2([{test,bs_test_tail2,F,[Ctx,Bits]}|Is],
	  [{test,bs_skip_bits2,F,[Ctx,{integer,I},Unit,_Flags]}|Acc]) ->
    bsm_opt_2(Is, [{test,bs_test_tail2,F,[Ctx,Bits+I*Unit]}|Acc]);
bsm_opt_2([{test,bs_skip_bits2,F,[Ctx,{integer,I1},Unit1,_]}|Is],
	  [{test,bs_skip_bits2,F,[Ctx,{integer,I2},Unit2,Flags]}|Acc]) ->
    bsm_opt_2(Is, [{test,bs_skip_bits2,F,
		    [Ctx,{integer,I1*Unit1+I2*Unit2},1,Flags]}|Acc]);
bsm_opt_2([I|Is], Acc) ->
    bsm_opt_2(Is, [I|Acc]);
bsm_opt_2([], Acc) -> reverse(Acc).

%% bsm_not_bs_test({test,Name,_,Operands}) -> true|false.
%%  Test whether is the test is a "safe", i.e. does not move the
%%  bit offset for a binary.
%%
%%  'true' means that the test is safe, 'false' that we don't know or
%%  that the test moves the offset (e.g. bs_get_integer2).

bsm_not_bs_test({test,bs_test_tail2,_,[_,_]}) -> true;
bsm_not_bs_test(Test) -> beam_utils:is_pure_test(Test).

bsm_subst_labels(Fs, Save, D) ->
    bsm_subst_labels_1(Fs, Save, D, []).

bsm_subst_labels_1([F|Fs], Save, D, Acc) ->
    bsm_subst_labels_1(Fs, Save, D, [bsm_subst_label(F, Save, D)|Acc]);
bsm_subst_labels_1([], _, _, Acc) ->
    reverse(Acc).

bsm_subst_label({f,Lbl0}=F, Save, D) ->
    case gb_trees:lookup({Lbl0,Save}, D) of
	{value,Lbl} -> {f,Lbl};
	none -> F
    end;
bsm_subst_label(Other, _, _) -> Other.
