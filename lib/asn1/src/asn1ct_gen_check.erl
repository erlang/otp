%% vim: tabstop=8:shiftwidth=4
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2017. All Rights Reserved.
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
%%

-module(asn1ct_gen_check).
-export([emit/4]).

-import(asn1ct_gen, [emit/1]).
-include("asn1_records.hrl").

emit(Gen, Type, Default, Value) ->
    Key = {Type,Default},
    DoGen = fun(Fd, Name) ->
                    file:write(Fd, gen(Gen, Name, Type, Default))
            end,
    emit(" case "),
    asn1ct_func:call_gen("is_default_", Key, DoGen, [Value]),
    emit([" of",nl,
	  "true -> {[],0};",nl,
	  "false ->",nl]).

gen(#gen{pack=Pack}=Gen, Name, #type{def=T}, Default) ->
    DefMarker = case Pack of
                    record -> "asn1_DEFAULT";
                    map -> atom_to_list(?MISSING_IN_MAP)
                end,
    NameStr = atom_to_list(Name),
    [NameStr,"(",DefMarker,") ->\n",
     "true;\n"|
     case do_gen(Gen, T, Default) of
         {literal,Literal} ->
             [NameStr,"(Def) when Def =:= ",term2str(Literal)," ->\n",
              "true;\n",
              NameStr,"(_) ->\n","false.\n\n"];
         {exception,Func,Args} ->
             [NameStr,"(Value) ->\n",
              "try ",Func,"(Value",arg2str(Args),") of\n",
              "_ -> true\n"
              "catch throw:false -> false\n"
              "end.\n\n"]
     end].

do_gen(_Gen, _, asn1_NOVALUE) ->
    {literal,asn1_NOVALUE};
do_gen(Gen, #'Externaltypereference'{module=M,type=T}, Default) ->
    #typedef{typespec=#type{def=Td}} = asn1_db:dbget(M, T),
    do_gen(Gen, Td, Default);
do_gen(_Gen, 'BOOLEAN', Default) ->
    {literal,Default};
do_gen(_Gen, {'BIT STRING',[]}, Default) ->
    true = is_bitstring(Default),		%Assertion.
    case asn1ct:use_legacy_types() of
	false ->
	    {literal,Default};
	true ->
	    {exception,need(check_legacy_bitstring, 2),[Default]}
    end;
do_gen(_Gen, {'BIT STRING',[_|_]=NBL}, Default) ->
    do_named_bitstring(NBL, Default);
do_gen(_Gen, {'ENUMERATED',_}, Default) ->
    {literal,Default};
do_gen(_Gen, 'INTEGER', Default) ->
    {literal,Default};
do_gen(_Gen, {'INTEGER',NNL}, Default) ->
    {exception,need(check_int, 3),[Default,NNL]};
do_gen(_Gen, 'NULL', Default) ->
    {literal,Default};
do_gen(_Gen, 'OCTET STRING', Default) ->
    true = is_binary(Default),			%Assertion.
    case asn1ct:use_legacy_types() of
	false ->
	    {literal,Default};
	true ->
	    {exception,need(check_octetstring, 2),[Default]}
    end;
do_gen(_Gen, 'OBJECT IDENTIFIER', Default0) ->
    Default = pre_process_oid(Default0),
    {exception,need(check_objectidentifier, 2),[Default]};
do_gen(Gen, {'CHOICE',Cs}, Default) ->
    {Tag,Value} = Default,
    [Type] = [Type || #'ComponentType'{name=T,typespec=Type} <- Cs,
		      T =:= Tag],
    case do_gen(Gen, Type#type.def, Value) of
	{literal,Lit} ->
	    {literal,{Tag,Lit}};
	{exception,Func0,Args} ->
	    Key = {Tag,Func0,Args},
	    DoGen = fun(Fd, Name) ->
                            S = gen_choice(Name, Tag, Func0, Args),
                            ok = file:write(Fd, S)
		  end,
	    Func = asn1ct_func:call_gen("is_default_choice", Key, DoGen),
	    {exception,atom_to_list(Func),[]}
    end;
do_gen(Gen, #'SEQUENCE'{components=Cs}, Default) ->
    do_seq_set(Gen, Cs, Default);
do_gen(Gen, {'SEQUENCE OF',Type}, Default) ->
    do_sof(Gen, Type, Default);
do_gen(Gen, #'SET'{components=Cs}, Default) ->
    do_seq_set(Gen, Cs, Default);
do_gen(Gen, {'SET OF',Type}, Default) ->
    do_sof(Gen, Type, Default);
do_gen(_Gen, Type, Default) ->
    case asn1ct_gen:unify_if_string(Type) of
	restrictedstring ->
	    {exception,need(check_restrictedstring, 2),[Default]};
	_ ->
	    %% Open type. Do our best.
	    {literal,Default}
    end.

do_named_bitstring(NBL, Default0) when is_list(Default0) ->
    Default = lists:sort(Default0),
    Bs = asn1ct_gen:named_bitstring_value(Default, NBL),
    Func = case asn1ct:use_legacy_types() of
	       false -> check_named_bitstring;
	       true -> check_legacy_named_bitstring
	   end,
    {exception,need(Func, 4),[Default,Bs,bit_size(Bs)]};
do_named_bitstring(_, Default) when is_bitstring(Default) ->
    Func = case asn1ct:use_legacy_types() of
	       false -> check_named_bitstring;
	       true -> check_legacy_named_bitstring
	   end,
    {exception,need(Func, 3),[Default,bit_size(Default)]}.

do_seq_set(#gen{pack=record}=Gen, Cs0, Default) ->
    Tag = element(1, Default),
    Cs1 = [T || #'ComponentType'{typespec=T} <- Cs0],
    Cs = components(Gen, Cs1, tl(tuple_to_list(Default))),
    case are_all_literals(Cs) of
	true ->
	    Literal = list_to_tuple([Tag|[L || {literal,L} <- Cs]]),
	    {literal,Literal};
	false ->
	    Key = {Cs,Default},
	    DoGen = fun(Fd, Name) ->
                            S = gen_components(Name, Tag, Cs),
                            ok = file:write(Fd, S)
                    end,
	    Func = asn1ct_func:call_gen("is_default_cs_", Key, DoGen),
	    {exception,atom_to_list(Func),[]}
    end;
do_seq_set(#gen{pack=map}=Gen, Cs0, Default) ->
    Cs1 = [{N,T} || #'ComponentType'{name=N,typespec=T} <- Cs0],
    Cs = map_components(Gen, Cs1, Default),
    AllLiterals = lists:all(fun({_,{literal,_}}) -> true;
                               ({_,_}) -> false
                            end, Cs),
    case AllLiterals of
	true ->
            L = [{Name,Lit} || {Name,{literal,Lit}} <- Cs],
	    {literal,maps:from_list(L)};
	false ->
	    Key = {Cs,Default},
	    DoGen = fun(Fd, Name) ->
                            S = gen_map_components(Name, Cs),
                            ok = file:write(Fd, S)
                    end,
	    Func = asn1ct_func:call_gen("is_default_cs_", Key, DoGen),
	    {exception,atom_to_list(Func),[]}
    end.

do_sof(Gen, Type, Default0) ->
    Default = lists:sort(Default0),
    Cs0 = lists:duplicate(length(Default), Type),
    Cs = components(Gen, Cs0, Default),
    case are_all_literals(Cs) of
	true ->
	    Literal = [Lit || {literal,Lit} <- Cs],
	    {exception,need(check_literal_sof, 2),[Literal]};
	false ->
	    Key = Cs,
	    DoGen = fun(Fd, Name) ->
                            S = gen_sof(Name, Cs),
                            ok = file:write(Fd, S)
		  end,
	    Func = asn1ct_func:call_gen("is_default_sof", Key, DoGen),
	    {exception,atom_to_list(Func),[]}
    end.

are_all_literals([{literal,_}|T]) ->
    are_all_literals(T);
are_all_literals([_|_]) ->
    false;
are_all_literals([]) -> true.

gen_components(Name, Tag, Cs) ->
    [atom_to_list(Name),"(Value) ->\n",
     "case Value of\n",
     "{",term2str(Tag)|gen_cs_1(Cs, 1, [])].

gen_cs_1([{literal,Lit}|T], I, Acc) ->
    [",\n",term2str(Lit)|gen_cs_1(T, I, Acc)];
gen_cs_1([H|T], I, Acc) ->
    Var = "E"++integer_to_list(I),
    [",\n",Var|gen_cs_1(T, I+1, [{Var,H}|Acc])];
gen_cs_1([], _, Acc) ->
    ["} ->\n"|gen_cs_2(Acc, "")].

gen_cs_2([{Var,{exception,Func,Args}}|T], Sep) ->
    [Sep,Func,"(",Var,arg2str(Args),")"|gen_cs_2(T, ",\n")];
gen_cs_2([], _) ->
    [";\n",
     "_ ->\n"
     "throw(false)\n"
     "end.\n"].

gen_map_components(Name, Cs) ->
    [atom_to_list(Name),"(Value) ->\n",
     "case Value of\n",
     "#{"|gen_map_cs_1(Cs, 1, "", [])].

gen_map_cs_1([{Name,{literal,Lit}}|T], I, Sep, Acc) ->
    Var = "E"++integer_to_list(I),
    G = Var ++ " =:= " ++ term2str(Lit),
    [Sep,term2str(Name),":=",Var|
     gen_map_cs_1(T, I+1, ",\n", [{guard,G}|Acc])];
gen_map_cs_1([{Name,Exc}|T], I, Sep, Acc) ->
    Var = "E"++integer_to_list(I),
    [Sep,term2str(Name),":=",Var|
     gen_map_cs_1(T, I+1, ",\n", [{exc,{Var,Exc}}|Acc])];
gen_map_cs_1([], _, _, Acc) ->
    G = lists:join(", ", [S || {guard,S} <- Acc]),
    Exc = [E || {exc,E} <- Acc],
    Body = gen_map_cs_2(Exc, ""),
    case G of
        [] ->
            ["} ->\n"|Body];
        [_|_] ->
            ["} when ",G," ->\n"|Body]
    end.

gen_map_cs_2([{Var,{exception,Func,Args}}|T], Sep) ->
    [Sep,Func,"(",Var,arg2str(Args),")"|gen_map_cs_2(T, ",\n")];
gen_map_cs_2([], _) ->
    [";\n",
     "_ ->\n"
     "throw(false)\n"
     "end.\n"].

gen_sof(Name, Cs) ->
    [atom_to_list(Name),"(Value) ->\n",
     "case length(Value) of\n",
     integer_to_list(length(Cs))," -> ok;\n"
     "_ -> throw(false)\n"
     "end,\n"
     "T0 = lists:sort(Value)"|gen_sof_1(Cs, 1)].

gen_sof_1([{exception,Func,Args}|Cs], I) ->
    NumStr = integer_to_list(I),
    H = "H" ++ NumStr,
    T = "T" ++ NumStr,
    Prev = "T" ++ integer_to_list(I-1),
    [",\n",
     "[",H,case Cs of
	       [] -> [];
	       [_|_] -> ["|",T]
	   end,"] = ",Prev,",\n",
     Func,"(",H,arg2str(Args),")"|gen_sof_1(Cs, I+1)];
gen_sof_1([], _) ->
    ".\n".

components(Gen, [#type{def=Def}|Ts], [V|Vs]) ->
    [do_gen(Gen, Def, V)|components(Gen, Ts, Vs)];
components(_Gen, [], []) -> [].

map_components(Gen, [{Name,#type{def=Def}}|Ts], Value) ->
    case maps:find(Name, Value) of
        {ok,V} ->
            [{Name,do_gen(Gen, Def, V)}|map_components(Gen, Ts, Value)];
        error ->
            map_components(Gen, Ts, Value)
    end;
map_components(_Gen, [], _Value) -> [].

gen_choice(Name, Tag, Func, Args) ->
    NameStr = atom_to_list(Name),
    [NameStr,"({",term2str(Tag),",Value}) ->\n"
     " ",Func,"(Value",arg2str(Args),");\n",
     NameStr,"(_) ->\n"
     " throw(false).\n"].

pre_process_oid(Oid) ->
    Reserved = reserved_oid(),
    pre_process_oid(tuple_to_list(Oid), Reserved, []).

pre_process_oid([H|T]=Tail, Res0, Acc) ->
    case lists:keyfind(H, 2, Res0) of
	false ->
	    {lists:reverse(Acc),Tail};
	{Names0,H,Res} ->
	    Names = case is_list(Names0) of
			false -> [Names0];
			true -> Names0
		    end,
	    Keys = [H|Names],
	    pre_process_oid(T, Res, [Keys|Acc])
    end.

reserved_oid() ->
    [{['itu-t',ccitt],0,
      [{recommendation,0,[]},
       {question,1,[]},
       {administration,2,[]},
       {'network-operator',3,[]},
       {'identified-organization',4,[]}]},
     {iso,1,[{standard,0,[]},
	     {'member-body',2,[]},
	     {'identified-organization',3,[]}]},
     {['joint-iso-itu-t','joint-iso-ccitt'],2,[]}].

arg2str(Args) ->
    [", "++term2str(Arg) || Arg <- Args].

term2str(T) ->
    io_lib:format("~w", [T]).

need(F, A) ->
    asn1ct_func:need({check,F,A}),
    atom_to_list(F).
