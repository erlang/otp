%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
-moduledoc false.

-export([format_function/1,format_instr/1,format_var/1,format_type/1]).

-include("beam_ssa.hrl").
-include("beam_types.hrl").

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
         Key := Value <- maps:iterator(Anno, ordered)],
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

format_anno(parameter_info, Map) when is_map(Map) ->
    case map_size(Map) of
        0 ->
            [];
        _ ->
            Params = lists:sort(maps:to_list(Map)),
            Break = "\n%%     ",
            [io_lib:format("%% Parameters\n", []),
             [io_lib:format("%%    ~s =>~s~s\n",
                            [format_var(V),
                             Break,
                             format_param_info(I, Break)]) ||
                 {V,I} <- Params]]
    end;
format_anno(Key, Map) when is_map(Map) ->
    Sorted = maps:to_list(maps:iterator(Map, ordered)),
    [io_lib:format("%% ~s:\n", [Key]),
     [io_lib:format("%%    ~kw => ~kw\n", [K,V]) || {K,V} <- Sorted]];
format_anno(Key, Value) ->
    io_lib:format("%% ~s: ~kp\n", [Key,Value]).

format_param_info([{type, T} | Infos], Break) ->
    [format_type(T, Break) |
     format_param_info(Infos, Break)];
format_param_info([Info | Infos], Break) ->
    [io_lib:format("~s~kp", [Break, Info]) |
     format_param_info(Infos, Break)];
format_param_info([], _Break) ->
    [].

format_type(T, Break) ->
    %% Gross hack, but it's short and simple.
    Indented = unicode:characters_to_list(format_type(T)),
    string:replace(Indented, [$\n], Break, all).

format_blocks(Ls, Blocks, Anno) ->
    PP = [format_block(L, Blocks, Anno) || L <- Ls],
    lists:join($\n, PP).

format_block(L, Blocks, FuncAnno) ->
    #b_blk{anno=Anno,is=Is,last=Last} = maps:get(L, Blocks),
    [case map_size(Anno) of
         0 -> [];
         _ -> io_lib:format("%% ~kp\n", [Anno])
     end,
     io_lib:format("~kp:", [L]),
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
    AnnoStr = format_instr_anno(Anno, FuncAnno, Args),
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

format_terminator(#b_br{anno=A,bool=#b_literal{val=true},
                        succ=Same,fail=Same}, _) ->
    io_lib:format("~s  ~sbr ~ts\n",
                  [format_terminator_anno(A),
                   format_i_number(A),
                   format_label(Same)]);
format_terminator(#b_br{anno=A,bool=Bool,succ=Succ,fail=Fail}, FuncAnno) ->
    io_lib:format("~s  ~sbr ~ts, ~ts, ~ts\n",
                  [format_terminator_anno(A),
                   format_i_number(A),
                   format_arg(Bool, FuncAnno),
                   format_label(Succ),
                   format_label(Fail)]);
format_terminator(#b_switch{anno=A,arg=Arg,fail=Fail,list=List}, FuncAnno) ->
    io_lib:format("~s  ~sswitch ~ts, ~ts, ~ts\n",
                  [format_terminator_anno(A),
                   format_i_number(A),format_arg(Arg, FuncAnno),
                   format_label(Fail),
                   format_switch_list(List, FuncAnno)]);
format_terminator(#b_ret{anno=A,arg=Arg}, FuncAnno) ->
    io_lib:format("~s  ~sret ~ts\n",
                  [format_terminator_anno(A),
                   format_i_number(A),
                   format_arg(Arg, FuncAnno)]).

format_terminator_anno(Anno) ->
    format_instr_anno(Anno, #{}, []).

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
    io_lib:format("`~kp`", [Val]);
format_arg(#b_remote{mod=Mod,name=Name,arity=Arity}, FuncAnno) ->
    io_lib:format("(~ts:~ts/~p)",
                  [format_arg(Mod, FuncAnno),format_arg(Name, FuncAnno),Arity]);
format_arg(#b_local{name=Name,arity=Arity}, FuncAnno) ->
    io_lib:format("(~ts/~p)", [format_arg(Name, FuncAnno),Arity]);
format_arg({Value,Label}, FuncAnno) when is_integer(Label) ->
    io_lib:format("{ ~ts, ~ts }", [format_arg(Value, FuncAnno),
                                   format_label(Label)]);
format_arg(Other, _) ->
    io_lib:format("*** ~kp ***", [Other]).

format_switch_list(List, FuncAnno) ->
    Ss = [io_lib:format("{ ~ts, ~ts }", [format_arg(Val, FuncAnno),
                                         format_label(L)]) || {Val,L} <- List],
    io_lib:format("[\n    ~ts\n  ]", [lists:join(",\n    ", Ss)]).

format_label(L) ->
    io_lib:format("^~w", [L]).

format_instr_anno(#{n:=_}=Anno, FuncAnno, Args) ->
    format_instr_anno(maps:remove(n, Anno), FuncAnno, Args);
format_instr_anno(#{location:={File,Line}}=Anno0, FuncAnno, Args) ->
    Anno = maps:remove(location, Anno0),
    [io_lib:format("  %% ~ts:~p\n", [File,Line]) |
     format_instr_anno(Anno, FuncAnno, Args)];
format_instr_anno(#{result_type:=T}=Anno0, FuncAnno, Args) ->
    Anno = maps:remove(result_type, Anno0),
    Break = "\n  %%    ",
    [io_lib:format("  %% Result type:~s~s\n",
                   [Break, format_type(T, Break)]) |
                    format_instr_anno(Anno, FuncAnno, Args)];
format_instr_anno(#{arg_types:=Ts}=Anno0, FuncAnno, Args) ->
    Anno = maps:remove(arg_types, Anno0),

    Break = "\n  %%    ",

    Iota = lists:seq(0, length(Args) - 1),
    Formatted0 = [[format_arg(Arg, FuncAnno), " => ",
                   format_type(map_get(Idx, Ts),
                   Break)]
                  || {Idx, Arg} <- lists:zip(Iota, Args), is_map_key(Idx, Ts)],
    Formatted = lists:join(Break, Formatted0),

    [io_lib:format("  %% Argument types:~s~ts\n",
                   [Break, unicode:characters_to_list(Formatted)]) |
     format_instr_anno(Anno, FuncAnno, Args)];
format_instr_anno(#{aliased:=As}=Anno, FuncAnno, Args) ->
    Break = "\n  %%    ",
    ["  %% Aliased:",
     string:join([[Break, format_var(V)] || V <- As], ", "), "\n",
     format_instr_anno(maps:remove(aliased, Anno), FuncAnno, Args)];
format_instr_anno(#{unique:=Us}=Anno, FuncAnno, Args) ->
    Break = "\n  %%    ",
    ["  %% Unique:",
     string:join([[Break, format_var(V)] || V <- Us], ", "), "\n",
     format_instr_anno(maps:remove(unique, Anno), FuncAnno, Args)];
format_instr_anno(Anno, _FuncAnno, _Args) ->
    format_instr_anno_1(Anno).

format_instr_anno_1(Anno) ->
    case map_size(Anno) of
        0 ->
            [];
        _ ->
            [io_lib:format("  %% Anno: ~kp\n", [Anno])]
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

-spec format_type(type()) -> iolist().

format_type(any) ->
    "any()";
format_type(#t_atom{elements=any}) ->
    "atom()";
format_type(#t_atom{elements=Es}) ->
    string:join([io_lib:format("'~p'", [E])
                 || E <- ordsets:to_list(Es)], " | ");
format_type(#t_bs_matchable{tail_unit=U}) ->
    io_lib:format("bs_matchable(~p)", [U]);
format_type(#t_bitstring{size_unit=S,appendable=true}) ->
    io_lib:format("bitstring(~p,appendable)", [S]);
format_type(#t_bitstring{size_unit=S}) ->
    io_lib:format("bitstring(~p)", [S]);
format_type(#t_bs_context{tail_unit=U}) ->
    io_lib:format("bs_context(~p)", [U]);
format_type(#t_fun{arity=any,type=any}) ->
    "fun()";
format_type(#t_fun{arity=any,type=T}) ->
    ["fun((...) -> ", format_type(T), ")"];
format_type(#t_fun{arity=A,type=any}) ->
    ["fun((", format_fun_args(A), "))"];
format_type(#t_fun{arity=A,type=T}) ->
    ["fun((", format_fun_args(A), ") -> ", format_type(T), ")"];
format_type(#t_map{super_key=any,super_value=any}) ->
    "map()";
format_type(#t_map{super_key=none,super_value=none}) ->
    "#{}";
format_type(#t_map{super_key=K,super_value=V}) ->
    ["#{", format_type(K), "=>", format_type(V), "}"];
format_type(number) ->
    "number()";
format_type(#t_float{elements=any}) ->
    "float()";
format_type(#t_float{elements={X,X}}) ->
    io_lib:format("~p", [X]);
format_type(#t_float{elements={Low,High}}) ->
    io_lib:format("~p..~p", [Low,High]);
format_type(#t_integer{elements=any}) ->
    "integer()";
format_type(#t_integer{elements={X,X}}) ->
    io_lib:format("~p", [X]);
format_type(#t_integer{elements={Low,High}}) ->
    io_lib:format("~p..~p", [Low,High]);
format_type(#t_number{elements=any}) ->
    "number()";
format_type(#t_number{elements={X,X}}) ->
    io_lib:format("number(~p)", [X]);
format_type(#t_number{elements={Low,High}}) ->
    io_lib:format("number(~p, ~p)", [Low,High]);
format_type(#t_list{type=ET,terminator=nil}) ->
    ["list(", format_type(ET), ")"];
format_type(#t_list{type=ET,terminator=TT}) ->
    ["maybe_improper_list(", format_type(ET), ", ", format_type(TT), ")"];
format_type(#t_cons{type=ET,terminator=nil}) ->
    ["nonempty_list(", format_type(ET), ")"];
format_type(#t_cons{type=ET,terminator=TT}) ->
    ["nonempty_improper_list(", format_type(ET), ", ", format_type(TT), ")"];
format_type(nil) ->
    "nil()";
format_type(#t_tuple{elements=Es,exact=Ex,size=S}) ->
    ["{",
     string:join(format_tuple_elems(S, Ex, Es, 1), ", "),
     "}"];
format_type(other) ->
    "other()";
format_type(pid) ->
    "pid()";
format_type(port) ->
    "pid()";
format_type(reference) ->
    "reference()";
format_type(identifier) ->
    "identifier()";
format_type(none) ->
    "none()";
format_type(#t_union{atom=A,list=L,number=N,tuple_set=Ts,other=O}) ->
    Es = case A of
             none -> [];
             _ -> [format_type(A)]
         end
        ++ case L of
               none -> [];
               _ -> [format_type(L)]
           end
        ++ case N of
               none -> [];
               _ -> [format_type(N)]
           end
        ++ case Ts of
               none -> [];
               _ -> [format_tuple_set(Ts)]
           end
        ++ case O of
               none -> [];
               _ -> [format_type(O)]
           end,
    string:join(Es, " | ").

format_fun_args(A) ->
    string:join(lists:duplicate(A, "_"), ", ").

format_tuple_elems(Size, true, _Elems, Idx) when Idx > Size ->
    [];
format_tuple_elems(Size, false, _Elems, Idx) when Idx > Size ->
    ["..."];
format_tuple_elems(Size, Exact, Elems, Idx) ->
    T = case Elems of
            #{ Idx := Ty} -> Ty;
            _ -> any
        end,
    [format_type(T)|format_tuple_elems(Size, Exact, Elems, Idx + 1)].

format_tuple_set(#t_tuple{}=T) ->
    format_type(T);
format_tuple_set(RecordSet) ->
    string:join([format_tuple_set_1(T) || T <- ordsets:to_list(RecordSet)],
                " | ").

format_tuple_set_1({{Arity,Key},#t_tuple{size=Arity,elements=Elems}=Tuple}) ->
    false = none =:= beam_types:meet(Key, map_get(1, Elems)), % Assertion
    format_type(Tuple).
