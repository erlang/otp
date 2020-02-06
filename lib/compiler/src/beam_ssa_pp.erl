%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(beam_ssa_pp).

-export([format_function/1,format_instr/1,format_var/1]).

-include("beam_ssa.hrl").

-spec format_function(beam_ssa:b_function()) -> iolist().

format_function(#b_function{anno=Anno0,args=Args,
                            bs=Blocks,cnt=Counter}) ->
    #{func_info:={M,F,_}} = Anno0,
    Anno = maps:without([func_info,location,live_intervals,registers], Anno0),
    FuncAnno = case Anno0 of
                   #{live_intervals:=Intervals} ->
                       Anno0#{live_intervals:=maps:from_list(Intervals)};
                   #{} ->
                       Anno0
               end,
    ReachableBlocks = beam_ssa:rpo(Blocks),
    All = maps:keys(Blocks),
    Unreachable = ordsets:subtract(ordsets:from_list(All),
                                   ordsets:from_list(ReachableBlocks)),
    [case Anno0 of
         #{location:={Filename,Line}} ->
             io_lib:format("%% ~ts:~p\n", [Filename,Line]);
         #{} ->
             []
     end,
     io_lib:format("%% Counter = ~p\n", [Counter]),
     [format_anno(Key, Value) ||
         {Key,Value} <- lists:sort(maps:to_list(Anno))],
     io_lib:format("function `~p`:`~p`(~ts) {\n",
                   [M, F, format_args(Args, FuncAnno)]),
     [format_live_interval(Var, FuncAnno) || Var <- Args],
     format_blocks(ReachableBlocks, Blocks, FuncAnno),
     case Unreachable of
         [] ->
             [];
         [_|_] ->
             ["\n%% Unreachable blocks\n\n",
              format_blocks(Unreachable, Blocks, FuncAnno)]
     end,

     "}\n"].


-spec format_instr(beam_ssa:b_set()) -> iolist().

format_instr(#b_set{}=I) ->
    Cs = lists:flatten(format_instr(I#b_set{anno=#{}}, #{}, true)),
    string:trim(Cs, leading);
format_instr(I0) ->
    I = setelement(2, I0, #{}),
    Cs = lists:flatten(format_terminator(I, #{})),
    string:trim(Cs, both).

-spec format_var(beam_ssa:b_var()) -> iolist().

format_var(V) ->
    Cs = lists:flatten(format_var(V, #{})),
    string:trim(Cs, leading).

%%%
%%% Local functions.
%%%

format_anno(Key, Map) when is_map(Map) ->
    Sorted = lists:sort(maps:to_list(Map)),
    [io_lib:format("%% ~s:\n", [Key]),
     [io_lib:format("%%    ~w => ~w\n", [K,V]) || {K,V} <- Sorted]];
format_anno(Key, Value) ->
    io_lib:format("%% ~s: ~p\n", [Key,Value]).

format_blocks(Ls, Blocks, Anno) ->
    PP = [format_block(L, Blocks, Anno) || L <- Ls],
    lists:join($\n, PP).

format_block(L, Blocks, FuncAnno) ->
    #b_blk{anno=Anno,is=Is,last=Last} = maps:get(L, Blocks),
    [case map_size(Anno) of
         0 -> [];
         _ -> io_lib:format("%% ~p\n", [Anno])
     end,
     io_lib:format("~p:", [L]),
     format_instrs(Is, FuncAnno, true),
     $\n,
     format_terminator(Last, FuncAnno)].

format_instrs([I|Is], FuncAnno, First) ->
    [$\n,
     format_instr(I, FuncAnno, First),
     format_instrs(Is, FuncAnno, false)];
format_instrs([], _FuncAnno, _First) ->
    [].

format_instr(#b_set{anno=Anno,op=Op,dst=Dst,args=Args},
             FuncAnno, First) ->
    AnnoStr = format_anno(Anno),
    LiveIntervalStr = format_live_interval(Dst, FuncAnno),
    [if
         First ->
             [];
         AnnoStr =/= []; LiveIntervalStr =/= [] ->
             $\n;
         true ->
             []
     end,
     AnnoStr,
     LiveIntervalStr,
     io_lib:format("  ~s~ts = ~ts", [format_i_number(Anno),
                                     format_var(Dst, FuncAnno),
                                     format_op(Op)]),
     case Args of
         [] ->
             [];
         [_|_] ->
             io_lib:format(" ~ts", [format_args(Args, FuncAnno)])
     end].

format_i_number(#{n:=N}) ->
    io_lib:format("[~p] ", [N]);
format_i_number(#{}) -> [].

format_terminator(#b_br{anno=A,bool=#b_literal{val=true},succ=Lbl}, _) ->
    io_lib:format("  ~sbr ~ts\n", [format_i_number(A),format_label(Lbl)]);
format_terminator(#b_br{anno=A,bool=#b_literal{val=false},fail=Lbl}, _) ->
    io_lib:format("  ~sbr ~ts\n", [format_i_number(A),format_label(Lbl)]);
format_terminator(#b_br{anno=A,bool=Bool,succ=Succ,fail=Fail}, FuncAnno) ->
    io_lib:format("  ~sbr ~ts, ~ts, ~ts\n",
                  [format_i_number(A),
                   format_arg(Bool, FuncAnno),
                   format_label(Succ),
                   format_label(Fail)]);
format_terminator(#b_switch{anno=A,arg=Arg,fail=Fail,list=List}, FuncAnno) ->
    io_lib:format("  ~sswitch ~ts, ~ts, ~ts\n",
                  [format_i_number(A),format_arg(Arg, FuncAnno),
                   format_label(Fail),
                   format_switch_list(List, FuncAnno)]);
format_terminator(#b_ret{anno=A,arg=Arg}, FuncAnno) ->
    io_lib:format("  ~sret ~ts\n", [format_i_number(A),format_arg(Arg, FuncAnno)]).

format_op({Prefix,Name}) ->
    io_lib:format("~p:~p", [Prefix,Name]);
format_op(Name) ->
    io_lib:format("~p", [Name]).

format_register(#b_var{}=V, #{registers:=Regs}) ->
    {Tag,N} = maps:get(V, Regs),
    io_lib:format("~p~p", [Tag,N]);
format_register(_, #{}) -> "".

format_var(Var, FuncAnno) ->
    VarString = format_var_1(Var),
    case format_register(Var, FuncAnno) of
        [] -> VarString;
        [_|_]=Reg -> [Reg,$/,VarString]
    end.

format_var_1(#b_var{name={Name,Uniq}}) ->
    if
        is_atom(Name) ->
            io_lib:format("~ts:~p", [Name,Uniq]);
        is_integer(Name) ->
            io_lib:format("_~p:~p", [Name,Uniq])
    end;
format_var_1(#b_var{name=Name}) when is_atom(Name) ->
    atom_to_list(Name);
format_var_1(#b_var{name=Name}) when is_integer(Name) ->
    "_"++integer_to_list(Name).

format_args(Args, FuncAnno) ->
    Ss = [format_arg(Arg, FuncAnno) || Arg <- Args],
    lists:join(", ", Ss).

format_arg(#b_var{}=Arg, FuncAnno) ->
    format_var(Arg, FuncAnno);
format_arg(#b_literal{val=Val}, _FuncAnno) ->
    io_lib:format("`~p`", [Val]);
format_arg(#b_remote{mod=Mod,name=Name,arity=Arity}, FuncAnno) ->
    io_lib:format("(~ts:~ts/~p)",
                  [format_arg(Mod, FuncAnno),format_arg(Name, FuncAnno),Arity]);
format_arg(#b_local{name=Name,arity=Arity}, FuncAnno) ->
    io_lib:format("(~ts/~p)", [format_arg(Name, FuncAnno),Arity]);
format_arg({Value,Label}, FuncAnno) when is_integer(Label) ->
    io_lib:format("{ ~ts, ~ts }", [format_arg(Value, FuncAnno),
                                   format_label(Label)]);
format_arg(Other, _) ->
    io_lib:format("*** ~p ***", [Other]).

format_switch_list(List, FuncAnno) ->
    Ss = [io_lib:format("{ ~ts, ~ts }", [format_arg(Val, FuncAnno),
                                         format_label(L)]) || {Val,L} <- List],
    io_lib:format("[\n    ~ts\n  ]", [lists:join(",\n    ", Ss)]).

format_label(L) ->
    io_lib:format("^~w", [L]).

format_anno(#{n:=_}=Anno) ->
    format_anno(maps:remove(n, Anno));
format_anno(#{location:={File,Line}}=Anno0) ->
    Anno = maps:remove(location, Anno0),
    [io_lib:format("  %% ~ts:~p\n", [File,Line])|format_anno_1(Anno)];
format_anno(Anno) ->
    format_anno_1(Anno).

format_anno_1(Anno) ->
    case map_size(Anno) of
        0 ->
            [];
        _ ->
            [io_lib:format("  %% Anno: ~p\n", [Anno])]
    end.

format_live_interval(#b_var{}=Dst, #{live_intervals:=Intervals}) ->
    case Intervals of
        #{Dst:=Rs0} ->
            Rs1 = [io_lib:format("~p..~p", [Start,End]) ||
                      {Start,End} <- Rs0],
            Rs = lists:join(" ", Rs1),
            io_lib:format("  %% ~ts: ~s\n", [format_var_1(Dst),Rs]);
        #{} ->
            []
    end;
format_live_interval(_, _) -> [].

