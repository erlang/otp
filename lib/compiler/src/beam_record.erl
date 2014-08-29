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
%% File:    beam_record.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-03
%%

-module(beam_record).
-export([module/2]).

%% Rewrite the instruction stream on tagged tuple tests.
%% Tagged tuples means a tuple of any arity with an atom as its first element.
%% Typically records, ok-tuples and error-tuples.
%% 
%% from:
%%     ...
%%     {test,is_tuple,Fail,[Src]}.
%%     {test,test_arity,Fail,[Src,Sz]}.
%%     ...
%%     {get_tuple_element,Src,0,Dst}.
%%     ...
%%     {test,is_eq_exact,Fail,[Dst,Atom]}.
%%     ...
%% to:
%%     ...
%%     {test,is_tagged_tuple,Fail,[Src,Sz,Atom]}.
%%     ...


-import(lists, [reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is}) ->
    try
        Idx = beam_utils:index_labels(Is),
        {function,Name,Arity,CLabel,rewrite(Is,Idx)}
    catch
        Class:Error ->
            Stack = erlang:get_stacktrace(),
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

rewrite(Is,Idx) ->
    rewrite(Is,Idx,[]).

rewrite([{test,is_tuple,Fail,[Src]}=I1,
         {test,test_arity,Fail,[Src,N]}=I2|Is],Idx,Acc) ->
    case is_tagged_tuple(Is,Fail,Src,Idx) of
        no ->
            rewrite(Is,Idx,[I2,I1|Acc]);
        {Atom,[{block,[]}|Is1]} ->
            rewrite(Is1,Idx,[{test,is_tagged_tuple,Fail,[Src,N,Atom]}|Acc]);
        {Atom,Is1} ->
            rewrite(Is1,Idx,[{test,is_tagged_tuple,Fail,[Src,N,Atom]}|Acc])
    end;
rewrite([I|Is],Idx,Acc) ->
    rewrite(Is,Idx,[I|Acc]);
rewrite([],_,Acc) -> reverse(Acc).

is_tagged_tuple([{block,[{set,[Dst],[Src],{get_tuple_element,0}}=B|Bs]},
                 {test,is_eq_exact,Fail,[Dst,{atom,_}=Atom]}|Is],Fail,Src,Idx) ->

    %% if Dst is killed in the instruction stream and at fail label,
    %% we can safely remove get_tuple_element.
    %%
    %% if Dst is not killed in the stream, we cannot remove get_tuple_element
    %% since it is referenced.

    case is_killed(Dst,Is,Fail,Idx) of
        true  -> {Atom,[{block,Bs}|Is]};
        false -> {Atom,[{block,[B|Bs]}|Is]}
    end;
is_tagged_tuple([{block,[{set,_,_,_}=B|Bs]},
                 {test,is_eq_exact,_,_}=I|Is],Fail,Src,Idx) ->
    case is_tagged_tuple([{block,Bs},I|Is],Fail,Src,Idx) of
        {Atom,[{block,Bsr}|Isr]} -> {Atom,[{block,[B|Bsr]}|Isr]};
        no                       -> no
    end;
is_tagged_tuple(_Is,_Fail,_Src,_Idx) ->
    no.

is_killed(Dst,Is,{_,Lbl},Idx) ->
    beam_utils:is_killed(Dst,Is,Idx) andalso
        beam_utils:is_killed_at(Dst,Lbl,Idx).
