%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
%% Purpose: Peephole optimization of binary syntax instructions.

-module(beam_bs).

-export([module/2]).
-import(lists, [reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
	Is = bs_opt(Is0),
	{function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%%%
%%% Evaluate construction of constant bit fields.
%%%

bs_opt([{bs_put,_,_,_}=I|Is0]) ->
    {BsPuts0,Is} = collect_bs_puts(Is0, [I]),
    BsPuts = opt_bs_puts(BsPuts0),
    BsPuts ++ bs_opt(Is);
bs_opt([I|Is]) ->
    [I|bs_opt(Is)];
bs_opt([]) -> [].

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
