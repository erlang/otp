%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2017. All Rights Reserved.
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

%% Rewrite the instruction stream on tagged tuple tests.
%% Tagged tuples means a tuple of any arity with an atom as its
%% first element, such as records and error tuples.
%%
%% From:
%%     ...
%%     {test,is_tuple,Fail,[Src]}.
%%     {test,test_arity,Fail,[Src,Sz]}.
%%     ...
%%     {get_tuple_element,Src,0,Dst}.
%%     ...
%%     {test,is_eq_exact,Fail,[Dst,Atom]}.
%%     ...
%% To:
%%     ...
%%     {test,is_tagged_tuple,Fail,[Src,Sz,Atom]}.
%%     ...
%%

-module(beam_record).
-export([module/2]).

-import(lists, [reverse/1,reverse/2]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
        Is1 = beam_utils:anno_defs(Is0),
        Idx = beam_utils:index_labels(Is1),
        Is = rewrite(reverse(Is1), Idx),
        {function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

rewrite(Is, Idx) ->
    rewrite(Is, Idx, 0, []).

rewrite([{test,test_arity,Fail,[Src,N]}=TA,
         {test,is_tuple,Fail,[Src]}=TT|Is], Idx, Def, Acc0) ->
    case is_tagged_tuple(Acc0, Def, Fail, Src, Idx) of
        no ->
            rewrite(Is, Idx, 0, [TT,TA|Acc0]);
        {yes,Atom,Acc} ->
            I = {test,is_tagged_tuple,Fail,[Src,N,Atom]},
            rewrite(Is, Idx, Def, [I|Acc])
    end;
rewrite([{block,[{'%anno',{def,Def}}|Bl]}|Is], Idx, _Def, Acc) ->
    rewrite(Is, Idx, Def, [{block,Bl}|Acc]);
rewrite([{label,L}=I|Is], Idx0, Def, Acc) ->
    Idx = beam_utils:index_label(L, Acc, Idx0),
    rewrite(Is, Idx, Def, [I|Acc]);
rewrite([I|Is], Idx, Def, Acc) ->
    rewrite(Is, Idx, Def, [I|Acc]);
rewrite([], _, _, Acc) -> Acc.

is_tagged_tuple([{block,Bl},
                 {test,is_eq_exact,Fail,[Dst,{atom,_}=Atom]}|Is],
                Def, Fail, Src, Idx) ->
    case is_tagged_tuple_1(Bl, Is, Fail, Src, Dst, Idx, Def, []) of
        no ->
            no;
        {yes,[]} ->
            {yes,Atom,Is};
        {yes,[_|_]=Block} ->
            {yes,Atom,[{block,Block}|Is]}
    end;
is_tagged_tuple(_, _, _, _, _) ->
    no.

is_tagged_tuple_1([{set,[Dst],[Src],{get_tuple_element,0}}=I|Bl],
                  Is, Fail, Src, Dst, Idx, Def, Acc) ->
    %% Check usage of Dst to find out whether the get_tuple_element
    %% is needed.
    case usage(Dst, Is, Fail, Idx) of
        killed ->
            %% Safe to remove the get_tuple_element instruction.
            {yes,reverse(Acc, Bl)};
        used ->
            %% Actively used. Must keep instruction.
            {yes,reverse(Acc, [I|Bl])};
        not_used ->
            %% Not actually used (but must be initialized).
            case is_defined(Dst, Def) of
                false ->
                    %% Dst must be initialized, but the
                    %% actual value does not matter.
                    Kill = {set,[Dst],[nil],move},
                    {yes,reverse(Acc, [Kill|Bl])};
                true ->
                    %% The register is previously initialized.
                    %% We can remove the instruction.
                    {yes,reverse(Acc, Bl)}
            end
    end;
is_tagged_tuple_1([I|Bl], Is, Fail, Src, Dst, Idx, Def, Acc) ->
    is_tagged_tuple_1(Bl, Is, Fail, Src, Dst, Idx, Def, [I|Acc]);
is_tagged_tuple_1(_, _, _, _, _, _, _, _) ->
    no.

usage(Dst, Is, Fail, Idx) ->
    beam_utils:usage(Dst, [{test,is_number,Fail,[nil]}|Is], Idx).

is_defined({x,X}, Def) ->
    (Def bsr X) band 1 =:= 1.
