%% ---------------------------------------------------------------------
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012 Richard Carlsson
%% @doc Trivial Lisp interpreter in Erlang.

-module(lisp).

-export([eval/1]).

-export([init/0, equal/2, gt/2, knot/1]).

-record(st, {env}).

-define(INTERPRETED, true).
-include("lisp_test.erl").

eval(P) ->
    {X, _} = eval(P, init()),
    X.

init() ->
    Env = [{print, {builtin, fun do_print/2}}
           ,{list, {builtin, fun do_list/2}}
           ,{apply, {builtin, fun do_apply/2}}
           ,{plus, {builtin, fun do_plus/2}}
           ,{equal, {builtin, fun do_equal/2}}
           ,{gt, {builtin, fun do_gt/2}}
           ,{knot, {builtin, fun do_knot/2}}
           ,{y, y()}
          ],
    #st{env=dict:from_list(Env)}.

eval([lambda, Ps, B], #st{env=E}=St) when is_list(Ps) ->
    case lists:all(fun is_atom/1, Ps) andalso
        (length(Ps) =:= length(lists:usort(Ps))) of
        true -> {{lambda, Ps, B, E}, St};
        false -> throw(bad_lambda)
    end;
eval([lambda | _], _) ->
    throw(bad_lambda);
eval([def, A, V, B], #st{env=E0}=St) when is_atom(A) ->
    {V1, St1} = eval(V, St),
    E1 = bind(A, V1, E0),
    {X, St2} = eval(B, St1#st{env=E1}),
    {X, St2#st{env=E0}};
eval([def | _], _) ->
    throw(bad_def);
eval([quote, A], St) ->
    {A, St};
eval([quote | _], _) ->
    throw(bad_quote);
eval([iff, X, A, B], St) ->
    case eval(X, St) of
        {[], St1} -> eval(B, St1);
        {_, St1} -> eval(A, St1)
    end;
eval([do], _St0) ->
    throw(bad_do);
eval([do | As], St0) ->
    lists:foldl(fun (X, {_,St}) -> eval(X, St) end, {[],St0}, As);
eval([_|_]=L, St) ->
    {[F | As], St1} = lists:mapfoldl(fun eval/2, St, L),
    call(F, As, St1);
eval(A, St) when is_atom(A) ->
    {deref(A, St), St};
eval(C, St) ->
    {C, St}.

%% UTILITY FUNCTIONS

deref(A, #st{env=E}) ->
    case dict:find(A, E) of
        {ok, V} -> V;
        error -> throw({undefined, A})
    end.

bind(A, V, E) ->
    dict:store(A, V, E).

bind_args([P | Ps], [A | As], E) ->
    bind_args(Ps, As, dict:store(P, A, E));
bind_args([], [], E) ->
    E;
bind_args(_, _, _) ->
    throw(bad_arity).

call({lambda, Ps, B, E}, As, #st{env=E0}=St) ->
    {X, St1} = eval(B, St#st{env=bind_args(Ps, As, E)}),
    {X, St1#st{env=E0}};
call({builtin, F}, As, St) ->
    F(As, St);
call(X, _, _) ->
    throw({bad_fun, X}).

bool(true) -> 1;
bool(false) -> [].

%% BUILTINS

y() ->
    {Y, _} = eval([lambda, [f],
                   [[lambda, [x], [f, [lambda, [y], [[x, x], y]]]],
                    [lambda, [x], [f, [lambda, [y], [[x, x], y]]]]]],
                  #st{env=dict:new()}),
    Y.

do_print([S | Xs], St) ->
    io:format(S, Xs),
    {[], St};
do_print(_, _) ->
    throw(bad_print).

do_list(As, St) ->
    {As, St}.

do_apply([F, As], St) ->
    call(F, As, St);
do_apply(_, _) ->
    throw(bad_apply).

do_plus([X, Y], St) when is_number(X), is_number(Y) ->
    {X + Y, St};
do_plus(As, _) ->
    throw({bad_plus, As}).

do_equal([X, Y], St) ->
    {equal(X, Y), St};
do_equal(As, _) ->
    throw({bad_equal, As}).

equal(X, Y) ->
    bool(X =:= Y).

do_gt([X, Y], St) ->
    {gt(X, Y), St};
do_gt(As, _) ->
    throw({bad_gt, As}).

gt(X, Y) ->
    bool(X > Y).

do_knot([X], St) ->
    {knot(X), St};
do_knot(As, _) ->
    throw({bad_gt, As}).

knot([]) ->
    1;
knot(_) ->
    [].
