%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
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
-module(error_info_lib).
-export([test_error_info/2, test_error_info/3]).

test_error_info(Module, List) ->
    test_error_info(Module, List, []).

test_error_info(Module, L0, Options) ->
    L1 = lists:foldl(fun({_,A}, Acc) when is_integer(A) -> Acc;
                        ({F,A}, Acc) -> [{F,A,[]}|Acc];
                        ({F,A,Opts}, Acc) -> [{F,A,Opts}|Acc]
                     end, [], L0),
    Tests = ordsets:from_list([{F,length(A)} || {F,A,_} <- L1] ++
                                  [{F,A} || {F,A} <- L0, is_integer(A)]),
    Bifs0 = get_bifs(Module, Options),
    Bifs = ordsets:from_list(Bifs0),
    NYI = [{F,lists:duplicate(A, '*'),nyi} || {F,A} <- Bifs -- Tests],
    L = lists:sort(NYI ++ L1),
    do_error_info(L, Module, []).

get_bifs(Module, Options) ->
    case lists:member(snifs_only, Options) of
        true ->
            [{F,A} || {M,F,A} <- erlang:system_info(snifs),
                      M =:= Module,
                      A =/= 0];
        false ->
            [{F,A} || {F,A} <- Module:module_info(exports),
                      A =/= 0,
                      F =/= module_info]
    end.

do_error_info([{_,Args,nyi}=H|T], Module, Errors) ->
    case lists:all(fun(A) -> A =:= '*' end, Args) of
        true ->
            do_error_info(T, Module, [{nyi,H}|Errors]);
        false ->
            do_error_info(T, Module, [{bad_nyi,H}|Errors])
    end;
do_error_info([{F,Args,Opts}|T], Module, Errors) ->
    eval_bif_error(F, Args, Opts, T, Module, Errors);
do_error_info([], _Module, Errors0) ->
    case lists:sort(Errors0) of
        [] ->
            ok;
        [_|_]=Errors ->
            io:format("\n~p\n", [Errors]),
            ct:fail({length(Errors),errors})
    end.

eval_bif_error(F, Args, Opts, T, Module, Errors0) ->
    try apply(Module, F, Args) of
        Result ->
            case lists:member(no_fail, Opts) of
                true ->
                    do_error_info(T, Module, Errors0);
                false ->
                    do_error_info(T, Module, [{should_fail,{F,Args},Result}|Errors0])
            end
    catch
        error:Reason:Stk ->
            SF = fun(Mod, _, _) -> Mod =:= test_server end,
            Str = erl_error:format_exception(error, Reason, Stk, #{stack_trim_fun => SF}),
            BinStr = iolist_to_binary(Str),
            ArgStr = lists:join(", ", [io_lib:format("~p", [A]) || A <- Args]),
            io:format("\n~p:~p(~s)\n~ts", [Module,F,ArgStr,BinStr]),

            case Stk of
                [{Module,ActualF,ActualArgs,Info}|_] ->
                    RE = <<"[*][*][*] argument \\d+:">>,
                    Errors1 = case re:run(BinStr, RE, [{capture, none}]) of
                                  match ->
                                      Errors0;
                                  nomatch when Reason =:= system_limit ->
                                      Errors0;
                                  nomatch ->
                                      case lists:member(unexplained, Opts) of
                                          true ->
                                              Errors0;
                                          false ->
                                              [{no_explanation,{F,Args},Info}|Errors0]
                                      end
                              end,

                    Errors = case {ActualF,ActualArgs} of
                                 {F,Args} ->
                                     Errors1;
                                 _ ->
                                     [{renamed,{F,length(Args)},{ActualF,ActualArgs}}|Errors1]
                             end,

                    do_error_info(T, Module, Errors);
                _ ->
                    Errors = [{renamed,{F,length(Args)},hd(Stk)}|Errors0],
                    do_error_info(T, Module, Errors)
            end
    end.
