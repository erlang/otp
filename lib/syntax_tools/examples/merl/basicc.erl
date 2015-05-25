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
%% @doc Basic compiler in Erlang.

-module(basicc).

-export([run/2, make_lines/1, bool/1]).

-include_lib("eunit/include/eunit.hrl").

-define(INTERPRETED, true).
-include("basic_test.erl").

-include("merl.hrl").

run(N, Prog) ->
    compile(Prog, tmp),
    tmp:run(N, Prog).

make_lines(Prog) ->
    ets:new(line, [private, named_table, ordered_set]),
    lists:foreach(fun ({L,_}) -> ets:insert(line, {L,label(L)}) end, Prog).

compile(Prog, ModName) ->
    make_lines(Prog),
    Fs0 = lists:map(fun ({L, X}) ->
                            {true, label(L),
                             case stmt(X) of
                                 {Stmt, false} ->
                                     [?Q("() -> _@Stmt")];
                                 {Stmt, true} ->
                                     Next = case ets:next(line, L) of
                                                '$end_of_table' ->
                                                    ?Q("stop(0)");
                                                L1 ->
                                                    Label = label(L1),
                                                    ?Q("_@Label@()")
                                            end,
                                     [?Q("() -> _@Stmt, _@Next")]
                             end}
                    end, Prog),
    ets:delete(line),
    Run = ?Q(["(N, Prog) ->",
              " ets:new(var, [private, named_table]),",
              " basicc:make_lines(Prog),",
              " goto(N)"
             ]),
    Stop = ?Q(["(R) ->",
               " ets:delete(var),",
               " ets:delete(line),",
               " R"
              ]),
    Goto =  ?Q(["(L) ->",
                " case ets:lookup(line, L) of",
                "  [{_, X}] -> apply(tmp, X, []);",
                "  _ ->",
                "   case ets:next(line, L) of",
                "    '$end_of_table' -> stop(0);",
                "    L1 -> goto(L1)",
                "   end",
                " end"]),
    Fs = [{true, run, [Run]},
          {false, stop, [Stop]},
          {true, goto, [Goto]}
          | Fs0],
    Forms = merl_build:module_forms(
              lists:foldl(fun ({X, Name, Cs}, S) ->
                                  merl_build:add_function(X, Name, Cs, S)
                          end,
                          merl_build:init_module(ModName),
                          Fs)),
    %% %% Write source to file for debugging
    %% file:write_file(lists:concat([ModName, "_gen.erl"]),
    %%                 erl_prettypr:format(erl_syntax:form_list(Forms),
    %%                                     [{paper,160},{ribbon,80}])),
    merl:compile_and_load(Forms, [verbose]).

label(L) ->
    list_to_atom("label_" ++ integer_to_list(L)).

stmt({print, S, As}) ->
    Exprs = [expr(A) || A <- As],
    {[?Q(["io:format(_@S@, [_@Exprs])"])], true};
stmt({set, V, X}) ->
    Expr = expr(X),
    {[?Q(["ets:insert(var, {_@V@, _@Expr})"])], true};
stmt({goto, X}) ->
    {[jump(X)], false};
stmt({stop, X}) ->
    Expr = expr(X),
    {[?Q(["stop(_@Expr)"])], false};
stmt({iff, X, A, B}) ->
    Cond = expr(X),
    True = jump(A),
    False = jump(B),
    {?Q(["case _@Cond of",
         "  0 -> _@False;",
         "  _ -> _@True",
         "end"]),
     false}.

jump(X) ->
    case ets:lookup(line, X) of
        [{_, F}] ->
            ?Q(["_@F@()"]);
        true ->
            Expr = expr(X),
            [?Q(["goto(_@Expr)"])]
    end.

expr(X) when is_number(X) ; is_list(X) ->
    ?Q("_@X@");
expr(X) when is_atom(X) ->
    ?Q(["case ets:lookup(var, _@X@) of",
        " [] -> 0;",
        " [{_,V}] -> V",
        "end"]);
expr({plus, X, Y}) ->
    ExprX = expr(X),
    ExprY = expr(Y),
    ?Q("_@ExprX + _@ExprY");
expr({equal, X, Y}) ->
    ExprX = expr(X),
    ExprY = expr(Y),
    ?Q("basicc:bool(_@ExprX == _@ExprY)");
expr({gt, X, Y}) ->
    ExprX = expr(X),
    ExprY = expr(Y),
    ?Q("basicc:bool(_@ExprX > _@ExprY)");
expr({knot, X}) ->
    Expr = expr(X),
    ?Q(["case _@Expr of",
        " 0 -> 1;",
        " _ -> 0",
        "end"]).

bool(true) -> 1;
bool(false) -> 0.
