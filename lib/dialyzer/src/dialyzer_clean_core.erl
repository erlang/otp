%% -*- erlang-indent-level: 2 -*-
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

-module(dialyzer_clean_core).
-export([clean/1]).

-spec clean(cerl:cerl()) -> cerl:cerl().

clean(Tree) ->
  case cerl:type(Tree) of
    apply ->
      Op = clean(cerl:apply_op(Tree)),
      Args = clean_list(cerl:apply_args(Tree)),
      cerl:update_c_apply(Tree, Op, Args);
    binary ->
      Segments = clean_list(cerl:binary_segments(Tree)),
      cerl:update_c_binary(Tree, Segments);
    bitstr ->
      Val = clean(cerl:bitstr_val(Tree)),
      Size = clean(cerl:bitstr_size(Tree)),
      Unit = cerl:bitstr_unit(Tree),
      Type = cerl:bitstr_type(Tree),
      Flags = cerl:bitstr_flags(Tree),
      cerl:update_c_bitstr(Tree, Val, Size, Unit, Type, Flags);
    'case' ->
      Arg = clean(cerl:case_arg(Tree)),
      Clauses = clean_clauses(cerl:case_clauses(Tree)),
      cerl:update_c_case(Tree, Arg, Clauses);
    call ->
      Args = clean_list(cerl:call_args(Tree)),
      Module = clean(cerl:call_module(Tree)),
      Name = clean(cerl:call_name(Tree)),
      cerl:update_c_call(Tree, Module, Name, Args);
    'catch' ->
      Body  = clean(cerl:catch_body(Tree)),
      cerl:update_c_catch(Tree, Body);
     cons ->
      Hd = clean(cerl:cons_hd(Tree)),
      Tl = clean(cerl:cons_tl(Tree)),
      cerl:update_c_cons_skel(Tree, Hd, Tl);
    'fun' ->
      Body = clean(cerl:fun_body(Tree)),
      Vars = cerl:fun_vars(Tree),
      cerl:update_c_fun(Tree, Vars, Body);
    'let' ->
      Arg = clean(cerl:let_arg(Tree)),
      Body = clean(cerl:let_body(Tree)),
      Vars = cerl:let_vars(Tree),
      cerl:update_c_let(Tree, Vars, Arg, Body);
    letrec ->
      clean_letrec(Tree);
    literal ->
      Tree;
    module ->
      Defs = clean_defs(cerl:module_defs(Tree)),
      Name = cerl:module_name(Tree),
      Exports = cerl:module_exports(Tree),
      Attrs = cerl:module_attrs(Tree),
      cerl:update_c_module(Tree, Name, Exports, Attrs, Defs);
    primop ->
      Args = clean_list(cerl:primop_args(Tree)),
      Name = cerl:primop_name(Tree),
      cerl:update_c_primop(Tree, Name, Args);
    'receive' ->
      Clauses = clean_clauses(cerl:receive_clauses(Tree)),
      Timeout = clean(cerl:receive_timeout(Tree)),
      Action = clean(cerl:receive_action(Tree)),
      cerl:update_c_receive(Tree, Clauses, Timeout, Action);
    seq ->
      Arg = clean(cerl:seq_arg(Tree)),
      Body = clean(cerl:seq_body(Tree)),
      cerl:update_c_seq(Tree, Arg, Body);
    'try' ->
      Arg = clean(cerl:try_arg(Tree)),
      Body = clean(cerl:try_body(Tree)),
      Handler = clean(cerl:try_handler(Tree)),
      Vs = cerl:try_vars(Tree),
      Evs = cerl:try_evars(Tree),
      Try = cerl:update_c_try(Tree, Arg, Vs, Body, Evs, Handler),
      Try;
    tuple ->
      Elements = clean_list(cerl:tuple_es(Tree)),
      cerl:update_c_tuple_skel(Tree, Elements);
    map ->
      Arg = clean(cerl:map_arg(Tree)),
      Entries = clean_map_pairs(cerl:map_es(Tree)),
      cerl:update_c_map(Tree, Arg, Entries);
    values ->
      Elements = clean_list(cerl:values_es(Tree)),
      cerl:update_c_values(Tree, Elements);
    var ->
      Tree
  end.

clean_letrec(Tree) ->
  case lists:member(letrec_goto, cerl:get_ann(Tree)) of
    true ->
      %% This is a restricted form of letrec used to allow rewriting
      %% pattern matching without duplicating code. When a letrec is
      %% used in this way, Dialyzer will not be able to infer much
      %% type information, so we will need to eliminate the letrec.
      [{_Name, Fun}] = cerl:letrec_defs(Tree),
      FunBody = cerl:fun_body(Fun),
      FunBody1 = clean(FunBody),
      Body = clean(cerl:letrec_body(Tree)),
      case dialyzer_ignore(Body) of
        true ->
          %% The body of the letrec directly transfer controls to
          %% defined function in the letrec. We only need to keep
          %% the body of that function. (This is is the code for
          %% a receive construct.)
          FunBody1;
        false ->
          %% The body is non-trivial. Here is an example:
          %%
          %%    letrec 'more_matching'/0 =
          %%                fun () ->
          %%                    case CaseExpr of . . . end
          %%                end
          %%    in case CaseExpr of
          %%          <<..., Tail>> ->
          %%             case Tail of
          %%               <<...>> -> . . .
          %%               _ -> apply 'more_matching'/0()
          %%             end
          %%          _ -> apply 'more_matching'/0()
          %%       end
          %%
          %% The clauses that invoke `apply` are marked with
          %% a `dialyzer_ignore` annotation to indicate that
          %% Dialyzer should ignore them.
          %%
          %% The example is translated like this:
          %%
          %%    case primop:dialyzer_unknown() of
          %%       'a' ->
          %%          case Var of
          %%             <<..., Tail>> ->
          %%                case Tail of
          %%                  <<...>> -> . . .
          %%                end
          %%          end
          %%       'b' ->
          %%          %% Body of more_matching/0.
          %%          case Var of . . . end
          %%    end
          %%
          PrimopUnknown = cerl:c_primop(cerl:abstract(dialyzer_unknown), []),
          Clauses = [cerl:c_clause([cerl:abstract(a)], Body),
                     cerl:c_clause([cerl:abstract(b)], FunBody1)],
          cerl:c_case(PrimopUnknown, Clauses)
      end;
    false ->
      %% This is a plain letrec. (Originating from a list or binary comprehension.)
      Defs = clean_defs(cerl:letrec_defs(Tree)),
      Body = clean(cerl:letrec_body(Tree)),
      cerl:update_c_letrec(Tree, Defs, Body)
  end.

clean_defs(Defs) ->
  [{Name, clean(Fun)} || {Name, Fun} <- Defs].

clean_clauses([Clause|Tail]) ->
  case clean_clause(Clause) of
    ignore ->
      %% The clause is either annotated with `dialyzer_ignore` or its
      %% body is primop that raises an exception.
      clean_clauses(Tail);
    Clause1 ->
      Tail1 = clean_clauses(Tail),
      [Clause1|Tail1]
  end;
clean_clauses([]) ->
  [].

clean_clause(Clause) ->
  Body = cerl:clause_body(Clause),
  case dialyzer_ignore(Clause) orelse is_raising_body(Body) of
    true ->
      ignore;
    false ->
      G = clean(cerl:clause_guard(Clause)),
      Body1 = clean(Body),
      Pats = cerl:clause_pats(Clause),
      cerl:update_c_clause(Clause, Pats, G, Body1)
  end.

is_raising_body(Body) ->
  case cerl:type(Body) of
    primop ->
      case cerl:atom_val(cerl:primop_name(Body)) of
	match_fail -> true;
	raise -> true;
	_ -> false
      end;
    _ ->
      false
  end.

clean_list(Trees) ->
  [clean(Tree) || Tree <- Trees].

clean_map_pairs([Pair|Pairs]) ->
  Key = clean(cerl:map_pair_key(Pair)),
  Val = clean(cerl:map_pair_val(Pair)),
  Pairs1 = clean_map_pairs(Pairs),
  Op = cerl:map_pair_op(Pair),
  Pair1 = cerl:update_c_map_pair(Pair, Op, Key, Val),
  [Pair1|Pairs1];
clean_map_pairs([]) ->
  [].

dialyzer_ignore(Tree) ->
  lists:member(dialyzer_ignore, cerl:get_ann(Tree)).
