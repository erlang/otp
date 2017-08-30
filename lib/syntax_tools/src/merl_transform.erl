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
%% @copyright 2012-2015 Richard Carlsson
%% @doc Parse transform for merl. Enables the use of automatic metavariables
%% and using quasi-quotes in matches and case switches. Also optimizes calls
%% to functions in `merl' by partially evaluating them, turning strings to
%% templates, etc., at compile-time.
%%
%% Using `-include_lib("syntax_tools/include/merl.hrl").' enables this
%% transform, unless the macro `MERL_NO_TRANSFORM' is defined first.

-module(merl_transform).

-export([parse_transform/2]).

%% NOTE: We cannot use inline metavariables or any other parse transform
%% features in this module, because it must be possible to compile it with
%% the parse transform disabled!
-include("merl.hrl").

%% TODO: unroll calls to switch? it will probably get messy

%% TODO: use Igor to make resulting code independent of merl at runtime?

parse_transform(Forms, _Options) ->
    erl_syntax:revert_forms(expand(erl_syntax:form_list(Forms))).

expand(Tree0) ->
    Tree = pre(Tree0),
    post(case erl_syntax:subtrees(Tree) of
             [] ->
                 Tree;
             Gs ->
                 erl_syntax:update_tree(Tree,
                                        [[expand(T) || T <- G] || G <- Gs])
         end).

pre(T) ->
    merl:switch(
      T,
      [{?Q("merl:quote(_@line, _@text) = _@expr"),
        fun ([{expr,_}, {line,Line}, {text,Text}]) ->
                erl_syntax:is_literal(Text) andalso erl_syntax:is_literal(Line)
        end,
        fun ([{expr,Expr}, {line,Line}, {text,Text}]) ->
                pre_expand_match(Expr, erl_syntax:concrete(Line),
                                 erl_syntax:concrete(Text))
        end},
       {?Q(["case _@expr of",
            "  merl:quote(_@_, _@text) when _@__@_ -> _@@_; _@_@_ -> 0",
            "end"]),
        fun case_guard/1,
        fun (As) -> case_body(As, T) end},
       fun () -> T end
      ]).

case_guard([{expr,_}, {text,Text}]) ->
    erl_syntax:is_literal(Text).

case_body([{expr,Expr}, {text,_Text}], T) ->
    pre_expand_case(Expr, erl_syntax:case_expr_clauses(T), get_location(T)).

post(T) ->
    merl:switch(
      T,
      [{?Q("merl:_@function(_@@args)"),
        [{fun ([{args, As}, {function, F}]) ->
                  lists:all(fun erl_syntax:is_literal/1, [F|As])
          end,
          fun ([{args, As}, {function, F}]) ->
                  Line = get_location(F),
                  [F1|As1] = lists:map(fun erl_syntax:concrete/1, [F|As]),
                  eval_call(Line, F1, As1, T)
          end},
         fun ([{args, As}, {function, F}]) ->
                 merl:switch(
                   F,
                   [{?Q("qquote"), fun ([]) -> expand_qquote(As, T, 1) end},
                    {?Q("subst"), fun ([]) -> expand_template(F, As, T) end},
                    {?Q("match"), fun ([]) -> expand_template(F, As, T) end},
                    fun () -> T end
                   ])
         end]},
       fun () -> T end]).

expand_qquote([Line, Text, Env], T, _) ->
    case erl_syntax:is_literal(Line) of
        true ->
            expand_qquote([Text, Env], T, erl_syntax:concrete(Line));
        false ->
            T
    end;
expand_qquote([Text, Env], T, Line) ->
    case erl_syntax:is_literal(Text) of
        true ->
            As = [Line, erl_syntax:concrete(Text)],
            %% expand further if possible
            expand(merl:qquote(Line, "merl:subst(_@tree, _@env)",
                               [{tree, eval_call(Line, quote, As, T)},
                                {env, Env}]));
        false ->
            T
    end;
expand_qquote(_As, T, _StartPos) ->
    T.

expand_template(F, [Pattern | Args], T) ->
    case erl_syntax:is_literal(Pattern) of
        true ->
            Line = get_location(Pattern),
            As = [erl_syntax:concrete(Pattern)],
            merl:qquote(Line, "merl:_@function(_@pattern, _@args)",
               [{function, F},
                {pattern, eval_call(Line, template, As, T)},
                {args, Args}]);
        false ->
            T
    end;
expand_template(_F, _As, T) ->
    T.

eval_call(Line, F, As, T) ->
    try apply(merl, F, As) of
        T1 when F =:= quote ->
            %% lift metavariables in a template to Erlang variables
            Template = merl:template(T1),
            Vars = merl:template_vars(Template),
            case lists:any(fun is_inline_metavar/1, Vars) of
                true when is_list(T1) ->
                    merl:qquote(Line, "merl:tree([_@template])",
                                [{template, merl:meta_template(Template)}]);
                true ->
                    merl:qquote(Line, "merl:tree(_@template)",
                                [{template, merl:meta_template(Template)}]);
                false ->
                    merl:term(T1)
            end;
        T1 ->
            merl:term(T1)
    catch
        throw:_Reason -> T
    end.

pre_expand_match(Expr, Line, Text) ->
    {Template, Out, _Vars} = rewrite_pattern(Line, Text),
    merl:qquote(Line, "{ok, _@out} = merl:match(_@template, _@expr)",
                [{expr, Expr},
                 {out, Out},
                 {template, erl_syntax:abstract(Template)}]).

rewrite_pattern(Line, Text) ->
    %% we must rewrite the metavariables in the pattern to use lowercase,
    %% and then use real matching to bind the Erlang-level variables
    T0 = merl:template(merl:quote(Line, Text)),
    Vars = [V || V <- merl:template_vars(T0), is_inline_metavar(V)],
    {merl:alpha(T0, [{V, var_to_tag(V)} || V <- Vars]),
     erl_syntax:list([erl_syntax:tuple([erl_syntax:abstract(var_to_tag(V)),
                                        erl_syntax:variable(var_name(V))])
                      || V <- Vars]),
     Vars}.

var_name(V) when is_integer(V) ->
    V1 = if V > 99, (V rem 100) =:= 99 ->
                 V div 100;
            V > 9, (V rem 10) =:= 9 ->
                 V div 10;
            true -> V
         end,
    list_to_atom("Q" ++ integer_to_list(V1));
var_name(V) -> V.

var_to_tag(V) when is_integer(V) -> V;
var_to_tag(V) ->
    list_to_atom(string:to_lower(atom_to_list(V))).

pre_expand_case(Expr, Clauses, Line) ->
    merl:qquote(Line, "merl:switch(_@expr, _@clauses)",
                [{clauses, erl_syntax:list([pre_expand_case_clause(C)
                                            || C <- Clauses])},
                 {expr, Expr}]).

pre_expand_case_clause(T) ->
    %% note that the only allowed non ``?Q(...) -> ...'' clause is ``_ -> ...''
    merl:switch(
      T,
      [{?Q("(merl:quote(_@line, _@text)) when _@__@guard -> _@@body"),
        fun ([{body,_}, {guard,_}, {line,Line}, {text,Text}]) ->
                erl_syntax:is_literal(Text) andalso erl_syntax:is_literal(Line)
        end,
        fun ([{body,Body}, {guard,Guard}, {line,Line}, {text,Text}]) ->
                pre_expand_case_clause(Body, Guard, erl_syntax:concrete(Line),
                                       erl_syntax:concrete(Text))
        end},
       {?Q("_ -> _@@body"),
        fun (Env) -> merl:qquote("fun () -> _@body end", Env) end}
      ]).

pre_expand_case_clause(Body, Guard, Line, Text) ->
    %% this is similar to a meta-match ``?Q("...") = Term''
    %% (note that the guards may in fact be arbitrary expressions)
    {Template, Out, Vars} = rewrite_pattern(Line, Text),
    GuardExprs = rewrite_guard(Guard),
    Param = [{body, Body},
             {guard,GuardExprs},
             {out, Out},
             {template, erl_syntax:abstract(Template)},
             {unused, dummy_uses(Vars)}],
    case GuardExprs of
        [] ->
            merl:qquote(Line, ["{_@template, ",
                               " fun (_@out) -> _@unused, _@body end}"],
                        Param);
        _ ->
            merl:qquote(Line, ["{_@template, ",
                               " fun (_@out) -> _@unused, _@guard end, ",
                               " fun (_@out) -> _@unused, _@body end}"],
                        Param)
    end.

%% We have to insert dummy variable uses at the beginning of the "guard" and
%% "body" function bodies to avoid warnings for unused variables in the
%% generated code. (Expansions at the Erlang level can't be marked up as
%% compiler generated to allow later compiler stages to ignore them.)
dummy_uses(Vars) ->
    [?Q("_ = _@var", [{var, erl_syntax:variable(var_name(V))}])
     || V <- Vars].

rewrite_guard([]) -> [];
rewrite_guard([D]) -> [make_orelse(erl_syntax:disjunction_body(D))].

make_orelse([]) -> [];
make_orelse([C]) -> make_andalso(erl_syntax:conjunction_body(C));
make_orelse([C | Cs]) ->
    ?Q("_@expr orelse _@rest",
       [{expr, make_andalso(erl_syntax:conjunction_body(C))},
        {rest, make_orelse(Cs)}]).

make_andalso([E]) -> E;
make_andalso([E | Es]) ->
    ?Q("_@expr andalso _@rest", [{expr, E}, {rest, make_andalso(Es)}]).

is_inline_metavar(Var) when is_atom(Var) ->
    is_erlang_var(atom_to_list(Var));
is_inline_metavar(Var) when is_integer(Var) ->
    Var > 9 andalso (Var rem 10) =:= 9;
is_inline_metavar(_) -> false.

is_erlang_var([C|_]) when C >= $A, C =< $Z ; C >= $À, C =< $Þ, C /= $× ->
    true;
is_erlang_var(_) ->
    false.

get_location(T) ->
    Pos = erl_syntax:get_pos(T),
    case erl_anno:is_anno(Pos) of
        true ->
            erl_anno:location(Pos);
        false ->
            Pos
    end.
