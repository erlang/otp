%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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

-module(maybe_SUITE).
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([basic/1, nested/1]).

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [basic,nested]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

-record(value, {v}).

basic(_Config) ->
    {ok,42,fish} = basic_1(0, #{0 => {ok,42}, 42 => {ok,fish}}),
    error = basic_1(0, #{0 => {ok,42}, 42 => {error,whatever}}),
    error = basic_1(0, #{0 => {ok,42}, 42 => error}),
    error = basic_1(0, #{0 => error}),
    error = basic_1(0, #{0 => {error,whatever}}),
    some_value = basic_1(0, #{0 => #value{v=some_value}}),
    {'EXIT',{{else_clause,something_wrong},[_|_]}} = catch basic_1(0, #{0 => something_wrong}),

    {ok,life,"universe",everything} = basic_2(0, #{0 => {ok,life},
                                                   life => "universe",
                                                   "universe" => {ok,everything}}),
    error = basic_2(0, #{0 => {ok,life},
                         life => "universe",
                         "universe" => error}),
    {'EXIT',{{badmatch,not_a_list},[_|_]}} = catch basic_2(0, #{0 => {ok,life},
                                                                life => not_a_list}),
    {'EXIT',{{else_clause,not_ok},[_|_]}} = catch basic_2(0, #{0 => {ok,life},
                                                               life => "universe",
                                                               "universe" => not_ok}),
    {'EXIT',{{else_clause,not_ok},[_|_]}} = catch basic_2(0, #{0 => not_ok}),

    {ok,42,fish,dolphins} = basic_3(0, #{0 => {ok,42}, 42 => {ok,fish},
                                         fish => {ok,#value{v=dolphins}}}),
    {error,whatever} = basic_3(0, #{0 => {ok,42}, 42 => {error,whatever}}),
    failed = basic_3(0, #{0 => {ok,42}, 42 => failed}),
    failed_early = basic_3(0, #{0 => failed_early}),

    y = maybe nomatch ?= id(x) else _ -> y end,
    y = maybe nomatch ?= id(x) else _ -> x, y end,

    x = maybe nomatch ?= id(x) else E1 -> E1 end,

    6 = maybe X1 = 2+2, X1+2 end,
    6 = maybe X2 = 2+2, X2+2 else {error, T} -> T end,
    {"llo", "hello", "hello"} = maybe Y1 = "he"++X3=Z1 ?= "hello", {X3,Y1,Z1} end,
    {"llo", "hello", "llo"} = maybe Y2 = "he"++(X4=Z2) ?= "hello", {X4,Y2,Z2} end,

    whatever = maybe
                   AlwaysMatching ?= id(whatever),
                   AlwaysMatching
               else
                   E2 -> E2
               end,

    <<0>> = basic_4(id({<<0>>})),

    ok.

basic_1(V0, M) ->
    Res = basic_1a(V0, M),
    {wrapped,Res} = basic_1b(V0, M),
    {wrapped,Res} = basic_1c(V0, M),
    Res.

basic_1a(V0, M) ->
    maybe
        {ok,V1} ?= do_something(V0, M),
        {ok,V2} ?= do_something(V1, M),
        {ok,V1,V2}
    else
        {error,_} ->
            error;
        error ->
            error;
        #value{v=V} ->
            V
    end.

basic_1b(V0, M) ->
    Result =
        maybe
            {ok,V1} ?= do_something(V0, M),
            {ok,V2} ?= do_something(V1, M),
            {ok,V1,V2}
        else
            {error,_} ->
                error;
            error ->
                error;
            #value{v=V} ->
                V
        end,
    {wrapped,Result}.

basic_1c(V0, M) ->
    OK = id(ok),
    Error = id(error),
    Result =
        maybe
            {OK,V1} ?= do_something(V0, M),
            {OK,V2} ?= do_something(V1, M),
            {OK,V1,V2}
        else
            {Error,_} ->
                Error;
            Error ->
                Error;
            #value{v=V} ->
                V
        end,
    {wrapped,Result}.

basic_2(V0, M) ->
    Res = basic_2a(V0, M),
    {wrapped,Res} = basic_2b(V0, M),
    Res.

basic_2a(V0, M) ->
    maybe
        {ok,V1} ?= do_something(V0, M),
        V2 = [_|_] = do_something(V1, M),
        {ok,V3} ?= do_something(V2, M),
        {ok,V1,V2,V3}
    else
        {error,_} ->
            error;
        error ->
            error;
        #value{v=V} ->
            V
    end.

basic_2b(V0, M) ->
    Result =
        maybe
            {ok,V1} ?= do_something(V0, M),
            V2 = [_|_] = do_something(V1, M),
            {ok,V3} ?= do_something(V2, M),
            {ok,V1,V2,V3}
        else
            {error,_} ->
                error;
            error ->
                error;
            #value{v=V} ->
                V
        end,
    _ = id(0),
    {wrapped,Result}.

basic_3(V0, M) ->
    Res = basic_3a(V0, M),
    {wrapped,Res} = basic_3b(V0, M),
    Res.

basic_3a(V0, M) ->
    maybe
        {ok,V1} ?= do_something(V0, M),
        {ok,V2} ?= do_something(V1, M),
        {ok,#value{v=V3}} ?= do_something(V2, M),
        {ok,V1,V2,V3}
    end.

basic_3b(V0, M) ->
    Result =
        maybe
            {ok,V1} ?= do_something(V0, M),
            {ok,V2} ?= do_something(V1, M),
            {ok,#value{v=V3}} ?= do_something(V2, M),
            {ok,V1,V2,V3}
        end,
    {wrapped,Result}.

basic_4({X}) ->
    maybe 
        <<_:(ok)>> ?= X
    end.

nested(_Config) ->
    {outer_fail,not_ok} = nested_1(0, #{0 => not_ok}),
    {x,{error,inner}} = nested_1(0, #{0 => {ok,x}, x => {error,inner}}),
    {outer_fail,{unexpected,not_error}} = nested_1(0, #{0 => {ok,x}, x => not_error}),
    ok.

nested_1(V0, M) ->
    Res = nested_1a(V0, M),
    {wrapped,Res} = nested_1b(V0, M),
    {wrapped,Res} = nested_1c(V0, M),
    Res.

nested_1a(V0, M) ->
    maybe
        {ok,V1} ?= do_something(V0, M),
        V2 = {error,_} ?=
            maybe
                {error, _} ?= id(do_something(V1, M))
            else
                Unexpected -> {unexpected, Unexpected}
            end,
        {V1,V2}
    else
        Res -> {outer_fail,Res}
    end.

nested_1b(V0, M) ->
    Result =
        maybe
            {ok,V1} ?= do_something(V0, M),
            V2 = {error,_} ?=
                maybe
                    {error, _} ?= id(do_something(V1, M))
                else
                    Unexpected -> {unexpected, Unexpected}
                end,
            {V1,V2}
        else
            Res -> {outer_fail,Res}
        end,
    {wrapped,Result}.

nested_1c(V0, M) ->
    Result =
        maybe
            R ?= maybe
                     {ok,V1} ?= do_something(V0, M),
                     {error,_} = V2 ?=
                         maybe
                             {error, _} ?= id(do_something(V1, M))
                         else
                             Unexpected -> {unexpected, Unexpected}
                         end,
                     {V1,V2}
                 else
                     Res -> {outer_fail,Res}
                 end,
            R
        else
            Var -> Var
        end,
    {wrapped,Result}.

%% Utility functions.

do_something(V, M) ->
    map_get(id(V), M).

id(X) -> X.
