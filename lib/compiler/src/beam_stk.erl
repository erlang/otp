%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
%% Purpose: Make sure that kill_stacktrace instructions don't destroy
%%          last call optimization.

-module(beam_stk).
-export([module/2]).
-import(lists, [reverse/1]).

-spec module(beam_asm:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
        Is = backward(reverse(Is0), #{}, []),
	{function,Name,Arity,CLabel,Is}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

backward([{label,L}=I|Is], Map0, [{bif,kill_stacktrace,_,_,_}=KillStk,
                                  {deallocate,_}=D,
                                  return=Ret|_]=Acc) ->
    Seq = [KillStk,D,Ret],
    Map = Map0#{L=>Seq},
    backward(Is, Map, [I|Acc]);
backward([{jump,{f,L}}=Jump|Is], Map, Acc) ->
    case Map of
        #{L:=Seq} ->
            backward(Is, Map, Seq ++ Acc);
        #{} ->
            backward(Is, Map, [Jump|Acc])
    end;
backward([{bif,kill_stacktrace,_,_,_}=I|Is], Map,
         [{bif,kill_stacktrace,_,_,_}|Acc]) ->
    backward(Is, Map, [I|Acc]);
backward([{call,_,_}=Call|Is], Map, Acc0) ->
    Acc = preserve_last_call_opt(Acc0),
    backward(Is, Map, [Call|Acc]);
backward([{call_ext,_,_}=Call|Is], Map, Acc0) ->
    Acc = preserve_last_call_opt(Acc0),
    backward(Is, Map, [Call|Acc]);
backward([I|Is], Map, Acc) ->
    backward(Is, Map, [I|Acc]);
backward([], _, Acc) -> Acc.

preserve_last_call_opt([{bif,kill_stacktrace,_,_,_}|
                        [{deallocate,_}|_]=Acc]) ->
    %% Remove the kill_stacktrace instruction to preserve
    %% the last-call optimization.
    Acc;
preserve_last_call_opt(Acc) -> Acc.
