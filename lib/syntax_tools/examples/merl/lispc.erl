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
%% @doc Lisp compiler in Erlang.

-module(lispc).

-export([eval/1]).

-record(st, {}).

-include("lisp_test.erl").

-include("merl.hrl").

eval(Lisp) ->
    compile(Lisp, tmp),
    tmp:eval().

compile(Lisp, ModName) ->
    {Code, _} = gen(Lisp, #st{}),
    Main = ?Q(["() ->",
               "  __print = fun (S, Xs) -> io:format(S,Xs), [] end,",
               "  __apply = fun erlang:apply/2,",
               "  __plus = fun erlang:'+'/2,",
               "  __equal = fun lisp:equal/2,",
               "  __gt = fun lisp:gt/2,",
               "  __knot = fun lisp:knot/1,",
               "  __y = fun (F) ->",
               "          (fun (X) -> F(fun (Y) -> (X(X))(Y) end) end)",
               "           (fun (X) -> F(fun (Y) -> (X(X))(Y) end) end)",
               "     end,",
               "  _@Code"]),
    Forms = merl_build:module_forms(
              merl_build:add_function(true, eval, [Main],
                                      merl_build:init_module(ModName))),
    %% %% Write source to file for debugging
    %% file:write_file(lists:concat([ModName, "_gen.erl"]),
    %%                 erl_prettypr:format(erl_syntax:form_list(Forms),
    %%                                     [{paper,160},{ribbon,80}])),
    merl:compile_and_load(Forms, [verbose]).

var(Atom) ->
    merl:var(list_to_atom("__" ++ atom_to_list(Atom))).

gen([lambda, Ps, B], St) when is_list(Ps) ->
    case lists:all(fun is_atom/1, Ps) andalso
        (length(Ps) =:= length(lists:usort(Ps))) of
        true ->
            Vars = [var(P) || P <- Ps],
            {Body, St1} = gen(B, St),
            {?Q("fun (_@Vars) -> _@Body end"), St1};
        false ->
            throw(bad_lambda)
    end;
gen([lambda | _], _) ->
    throw(bad_lambda);
gen([def, A, V, B], St) when is_atom(A) ->
    Var = var(A),
    {Val, St1} = gen(V, St),
    {Body, St2} = gen(B, St1),
    {?Q("(fun (_@Var) -> _@Body end)(_@Val)"), St2};
gen([def | _], _) ->
    throw(bad_def);
gen([quote, A], St) ->
    {merl:term(A), St};
gen([quote | _], _) ->
    throw(bad_quote);
gen([iff, X, A, B], St) ->
    {Cond, St1} = gen(X, St),
    {True, St2} = gen(A, St1),
    {False, St3} = gen(B, St2),
    {?Q(["case _@Cond of",
         "  [] -> _@False;",
         "  _ -> _@True",
         "end"]),
     St3};
gen([do], _) ->
    throw(bad_do);
gen([do | As], St0) ->
    {Body, St1} = lists:mapfoldl(fun gen/2, St0, As),
    {?Q("begin _@Body end"), St1};
gen([list | As], St0) ->
    {Elem, St1} = lists:mapfoldl(fun gen/2, St0, As),
    {?Q("[ _@Elem ]"), St1};
gen([_|_]=L, St) ->
    {[F | As], St1} = lists:mapfoldl(fun gen/2, St, L),
    {?Q("((_@F)(_@As))"), St1};
gen(A, St) when is_atom(A) ->
    {var(A), St};
gen(C, St) ->
    {merl:term(C), St}.
