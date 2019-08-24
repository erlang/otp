%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------------
-module(cuter_types).

-export([parse_spec/3, retrieve_types/1, retrieve_specs/1, find_spec/2, get_kind/1]).

-export([params_of_t_function_det/1, ret_of_t_function_det/1, atom_of_t_atom_lit/1, integer_of_t_integer_lit/1,
         elements_type_of_t_list/1, elements_type_of_t_nonempty_list/1, elements_types_of_t_tuple/1,
         elements_types_of_t_union/1, bounds_of_t_range/1, segment_size_of_bitstring/1]).

-export_type([erl_type/0, erl_spec_clause/0, erl_spec/0, stored_specs/0, stored_types/0, stored_spec_value/0, t_range_limit/0]).

-include("cuter_macros.hrl").
-include("cuter_types.hrl").


%% Define tags
-define(type_variable, vart).
-define(type_var, tvar).
-define(max_char, 16#10ffff).

%% Pre-processed types.

-type type_name() :: atom().
-type type_arity() :: byte().
-type type_var() :: {?type_var, atom()}.
-type remote_type() :: {module(), type_name(), type_arity()}.
-type record_name() :: atom().
-type record_field_name() :: atom().
-type record_field_type() :: {record_field_name(), raw_type()}.
-type dep() :: remote_type().
-type deps() :: ordsets:ordset(remote_type()).
-record(t, {
  kind,
  rep,
  deps = ordsets:new() :: deps()
}).
-type erl_type() :: t_any()               % any()
                  | t_nil()               % []
                  | t_atom()              % atom()
                  | t_atom_lit()          % Erlang_Atom
                  | t_integer()           % integer(), +infinity, -inifinity
                  | t_integer_lit()       % Erlang_Integer
                  | t_float()             % float()
                  | t_tuple()             % tuple(), {TList}
                  | t_list()              % list(Type)
                  | t_nonempty_list()     % nonempty_list(Type)
                  | t_union()             % Type1 | ... | TypeN
                  | t_range()             % Erlang_Integer..Erlang_Integer
                  | t_bitstring()         % <<_:M>>
                  | t_function()          % function() | Fun | BoundedFun
                  .
-type raw_type() :: erl_type()
                  | t_local()             % Local Type Usage
                  | t_remote()            % Remote Type Usage
                  | t_record()            % Record Usage
                  | t_type_var()          % Type Variable
                  .

-type t_any() :: #t{kind :: ?any_tag}.
-type t_nil() :: #t{kind :: ?nil_tag}.
-type t_atom() :: #t{kind :: ?atom_tag}.
-type t_atom_lit() :: #t{kind :: ?atom_lit_tag, rep :: atom()}.
-type t_integer() :: #t{kind :: ?integer_tag}.
-type t_integer_lit() :: #t{kind :: ?integer_lit_tag, rep :: integer()}.
-type t_float() :: #t{kind :: ?float_tag}.
-type t_tuple() :: #t{kind :: ?tuple_tag, rep :: [raw_type()]}.
-type t_list() :: #t{kind :: ?list_tag, rep :: raw_type()}.
-type t_nonempty_list() :: #t{kind :: ?nonempty_list_tag, rep :: raw_type()}.
-type t_union() :: #t{kind :: ?union_tag, rep :: [raw_type()]}.
-type t_range() :: #t{kind :: ?range_tag, rep :: {t_range_limit(), t_range_limit()}}.
-type t_range_limit() :: t_integer_lit() | t_integer_inf().
-type t_integer_inf() :: t_integer_pos_inf() | t_integer_neg_inf().
-type t_integer_pos_inf() :: #t{kind :: ?pos_inf}.
-type t_integer_neg_inf() :: #t{kind :: ?neg_inf}.
-type t_bitstring() :: #t{kind :: ?bitstring_tag, rep :: 1|8}.
-type t_function() :: #t{kind :: ?function_tag} | t_function_det().
-type t_function_det() :: #t{kind :: ?function_tag, rep :: {[raw_type()], raw_type(), [t_constraint()]}, deps :: deps()}.
-type t_constraint() :: {t_type_var(), raw_type()}.
-type t_local() :: #t{kind :: ?local_tag, rep :: {type_name(), [raw_type()]}}.
-type t_remote() :: #t{kind :: ?remote_tag, rep :: {module(), type_name(), [raw_type()]}}.
-type t_record() :: #t{kind :: ?record_tag, rep :: {record_name(), [record_field_type()]}}.
-type t_type_var() :: #t{kind :: ?type_variable, rep :: type_var()}.

%% How pre-processed types are stored.
-type stored_type_key() :: {record, record_name()} | {type, type_name(), type_arity()}.
-type stored_type_value() :: [record_field_type()] | {any(), [type_var()]}. % raw_type()
-type stored_types() :: dict:dict(stored_type_key(), stored_type_value()).

-type stored_spec_key() :: {type_name(), type_arity()}.
-type stored_spec_value() :: [t_function_det()].
-type stored_specs() :: dict:dict(stored_spec_key(), stored_spec_value()).

-type type_var_env() :: dict:dict(type_var(), raw_type()).
-type erl_spec_clause() :: t_function_det().
-type erl_spec() :: [erl_spec_clause()].

%% Pre-process the type & record declarations of a module.
-spec retrieve_types([cuter_cerl:cerl_attr_type()]) -> stored_types().
retrieve_types(TypeAttrs) ->
  lists:foldl(fun process_type_attr/2, dict:new(), TypeAttrs).

-spec process_type_attr(cuter_cerl:cerl_recdef() | cuter_cerl:cerl_typedef(), stored_types()) -> stored_types().
%% Declaration of a record.
process_type_attr({{record, Name}, Fields, []}, Processed) ->
  Fs = [t_field_from_form(Field) || Field <- Fields],
  Record = t_record(Name, Fs),
  dict:store({record, Name}, Record, Processed);
%% Declaration of a type.
process_type_attr({Name, Repr, Vars}, Processed) ->
  Type = safe_t_from_form(Repr),
  Vs = [{?type_var, Var} || {var, _, Var} <- Vars],
  dict:store({type, Name, length(Vs)}, {Type, Vs}, Processed).

%% The fields of a declared record.
-spec t_field_from_form(cuter_cerl:cerl_record_field()) -> record_field_type().
t_field_from_form({record_field, _, {atom, _, Name}}) ->
  {Name, t_any()};
t_field_from_form({record_field, _, {atom, _, Name}, _Default}) ->
  {Name, t_any()};
t_field_from_form({typed_record_field, {record_field, _, {atom, _, Name}}, Type}) ->
  {Name, safe_t_from_form(Type)};
t_field_from_form({typed_record_field, {record_field, _, {atom, _, Name}, _Default}, Type}) ->
  {Name, safe_t_from_form(Type)}.

%% Provision for unsupported types.
safe_t_from_form(Form) ->
  try t_from_form(Form)
  catch throw:{unsupported, Info} ->
    cuter_pp:form_has_unsupported_type(Info),
    t_any()
  end.

%% Parse a type.

-spec t_from_form(cuter_cerl:cerl_type()) -> raw_type().
%% Erlang_Atom
t_from_form({atom, _, Atom}) ->
  t_atom_lit(Atom);
%% Erlang_Integer
t_from_form({integer, _, Integer}) ->
  t_integer_lit(Integer);
%% integer()
t_from_form({type, _, integer, []}) ->
  t_integer();
%% nil
t_from_form({type, _, nil, []}) ->
  t_nil();
%% any()
t_from_form({type, _, any, []}) ->
  t_any();
%% term()
t_from_form({type, _, term, []}) ->
  t_any();
%% atom()
t_from_form({type, _, atom, []}) ->
  t_atom();
%% module()
t_from_form({type, _, module, []}) ->
  t_module();
%% float()
t_from_form({type, _, float, []}) ->
  t_float();
%% tuple()
t_from_form({type, _, tuple, any}) ->
  t_tuple();
%% {TList}
t_from_form({type, _, tuple, Types}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_tuple(Ts);
%% list()
t_from_form({type, _, list, []}) ->
  t_list();
%% list(Type)
t_from_form({type, _, list, [Type]}) ->
  T = t_from_form(Type),
  t_list(T);
%% Type1 | ... | TypeN
t_from_form({type, _, union, Types}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_union(Ts);
%% boolean()
t_from_form({type, _, boolean, []}) ->
  t_union([t_atom_lit(true), t_atom_lit(false)]);
%% number()
t_from_form({type, _, number, []}) ->
  t_union([t_integer(), t_float()]);
%% Erlang_Integer..Erlang_Integer
t_from_form({type, _, range, [{integer, _, I1}, {integer, _, I2}]}) ->
  t_range(t_integer_lit(I1), t_integer_lit(I2));
%% non_neg_integer()
t_from_form({type, _, non_neg_integer, []}) ->
  t_range(t_integer_lit(0), t_pos_inf());
%% pos_integer()
t_from_form({type, _, pos_integer, []}) ->
  t_range(t_integer_lit(1), t_pos_inf());
%% neg_integer()
t_from_form({type, _, neg_integer, []}) ->
  t_range(t_neg_inf(), t_integer_lit(-1));
%% char()
t_from_form({type, _, char, []}) ->
  t_char();
%% byte()
t_from_form({type, _, byte, []}) ->
  t_byte();
%% mfa()
t_from_form({type, _, mfa, []}) ->
  t_tuple([t_module(), t_atom(), t_byte()]);
%% string()
t_from_form({type, _, string, []}) ->
  t_list(t_char());
%% nonempty_list()
t_from_form({type, _, nonempty_list, []}) ->
  t_nonempty_list();
%% nonempty_list(Type)
t_from_form({type, _, nonempty_list, [Type]}) ->
  T = t_from_form(Type),
  t_nonempty_list(T);
%% binary()
t_from_form({type, _, binary, []}) ->
  t_bitstring(8);
%% bitstring()
t_from_form({type, _, bitstring, []}) ->
  t_bitstring(1);
%% function()
t_from_form({type, _, function, []}) ->
  t_function();
%% fun((TList) -> Type)
t_from_form({type, _, 'fun', [_Product, _RetType]}=Fun) ->
  t_function_from_form(Fun);
%% fun((TList) -> Type) (bounded_fun)
t_from_form({type, _, 'bounded_fun', [_Fun, _Cs]}=BoundedFun) ->
  t_bounded_function_from_form(BoundedFun);
%% ann_type
t_from_form({ann_type, _, [_Var, Type]}) ->
  t_from_form(Type);
%% paren_type
t_from_form({paren_type, _, [Type]}) ->
  t_from_form(Type);
%% remote_type
t_from_form({remote_type, _, [{atom, _, M}, {atom, _, Name}, Types]}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_remote(M, Name, Ts);
%% Record
t_from_form({type, _, record, [{atom, _, Name} | FieldTypes]}) ->
  Fields = [t_bound_field_from_form(F) || F <- FieldTypes],
  t_record(Name, Fields);
%% Map
t_from_form({type, _, map, _}=X) ->
  throw({unsupported, X});
%% local type
t_from_form({type, _, Name, Types}) ->
  Ts = [t_from_form(T) || T <- Types],
  t_local(Name, Ts);
%% Type Variable
t_from_form({var, _, Var}) ->
  t_var(Var);
%% Unsupported forms
t_from_form(Type) ->
  throw({unsupported, Type}).

-spec t_bound_field_from_form(cuter_cerl:cerl_type_record_field()) -> record_field_type().
%% Record Field.
t_bound_field_from_form({type, _, field_type, [{atom, _, Name}, Type]}) ->
  {Name, t_from_form(Type)}.

-spec t_function_from_form(cuter_cerl:cerl_func()) -> t_function_det().
t_function_from_form({type, _, 'fun', [{type, _, 'product', Types}, RetType]}) ->
  Ret = t_from_form(RetType),
  Ts = [t_from_form(T) || T <- Types],
  t_function(Ts, Ret).

-spec t_bounded_function_from_form(cuter_cerl:cerl_bounded_func()) -> t_function_det().
t_bounded_function_from_form({type, _, 'bounded_fun', [Fun, Constraints]}) ->
  {type, _, 'fun', [{type, _, 'product', Types}, RetType]} = Fun,
  Ret = t_from_form(RetType),
  Ts = [t_from_form(T) || T <- Types],
  Cs = [t_constraint_from_form(C) || C <- Constraints],
  t_function(Ts, Ret, Cs).

-spec t_constraint_from_form(cuter_cerl:cerl_constraint()) -> t_constraint().
t_constraint_from_form({type, _, constraint, [{atom, _, is_subtype}, [{var, _, Var}, Type]]}) ->
  {t_var(Var), t_from_form(Type)}.


%% Type constructors.

-spec t_any() -> t_any().
t_any() ->
  #t{kind = ?any_tag}.

-spec t_atom_lit(atom()) -> t_atom_lit().
t_atom_lit(Atom) ->
  #t{kind = ?atom_lit_tag, rep = Atom}.

-spec t_atom() -> t_atom().
t_atom() ->
  #t{kind = ?atom_tag}.

-spec t_module() -> t_atom().
t_module() -> t_atom().

-spec t_integer_lit(integer()) -> t_integer_lit().
t_integer_lit(Integer) ->
  #t{kind = ?integer_lit_tag, rep = Integer}.

-spec t_integer() -> t_integer().
t_integer() ->
  #t{kind = ?integer_tag}.

-spec t_range(t_range_limit(), t_range_limit()) -> t_range().
t_range(Int1, Int2) ->
  #t{kind = ?range_tag, rep = {Int1, Int2}}.

-spec t_pos_inf() -> t_integer_pos_inf().
t_pos_inf() ->
  #t{kind = ?pos_inf}.

-spec t_neg_inf() -> t_integer_neg_inf().
t_neg_inf() ->
  #t{kind = ?neg_inf}.

-spec t_char() -> t_range().
t_char() ->
  t_range(t_integer_lit(0), t_integer_lit(?max_char)).

-spec t_nil() -> t_nil().
t_nil() ->
  #t{kind = ?nil_tag}.

-spec t_float() -> t_float().
t_float() ->
  #t{kind = ?float_tag}.

-spec t_list() -> t_list().
t_list() ->
  #t{kind = ?list_tag, rep = t_any()}.

-spec t_list(raw_type()) -> t_list().
t_list(Type) ->
  #t{kind = ?list_tag, rep = Type, deps = get_deps(Type)}.

-spec t_nonempty_list() -> t_nonempty_list().
t_nonempty_list() ->
  #t{kind = ?nonempty_list_tag, rep = t_any()}.

-spec t_nonempty_list(raw_type()) -> t_nonempty_list().
t_nonempty_list(Type) ->
  #t{kind = ?nonempty_list_tag, rep = Type, deps = get_deps(Type)}.

-spec t_tuple() -> t_tuple().
t_tuple() ->
  #t{kind = ?tuple_tag, rep = []}.

-spec t_tuple([raw_type()]) -> t_tuple().
t_tuple(Types) ->
  #t{kind = ?tuple_tag, rep = Types, deps = unify_deps(Types)}.

-spec t_union([raw_type()]) -> t_union().
t_union(Types) ->
  #t{kind = ?union_tag, rep = Types, deps = unify_deps(Types)}.

-spec t_byte() -> t_range().
t_byte() ->
  t_range(t_integer_lit(0), t_integer_lit(255)).

-spec t_local(type_name(), [raw_type()]) -> t_local().
t_local(Name, Types) ->
  Rep = {Name, Types},
  #t{kind = ?local_tag, rep = Rep, deps = unify_deps(Types)}.

-spec t_remote(module(), type_name(), [raw_type()]) -> t_remote().
t_remote(Mod, Name, Types) ->
  Rep = {Mod, Name, Types},
  Dep = {Mod, Name, length(Types)},
  #t{kind = ?remote_tag, rep = Rep, deps = add_dep(Dep, unify_deps(Types))}.

-spec t_var(atom()) -> t_type_var().
t_var(Var) ->
  #t{kind = ?type_variable, rep = {?type_var, Var}}.

-spec t_record(record_name(), [record_field_type()]) -> t_record().
t_record(Name, Fields) ->
  Rep = {Name, Fields},
  Ts = [T || {_, T} <- Fields],
  #t{kind = ?record_tag, rep = Rep, deps = unify_deps(Ts)}.

-spec fields_of_t_record(t_record()) -> [record_field_type()].
fields_of_t_record(Record) ->
  Rep = Record#t.rep,
  element(2, Rep).

-spec t_bitstring(1 | 8) -> t_bitstring().
t_bitstring(N) ->
  #t{kind = ?bitstring_tag, rep = N}.

-spec t_function() -> t_function().
t_function() ->
  #t{kind = ?function_tag}.

-spec t_function([raw_type()], raw_type()) -> t_function_det().
t_function(Types, Ret) ->
  Rep = {Types, Ret, []},
  #t{kind = ?function_tag, rep = Rep, deps = unify_deps([Ret|Types])}.

-spec t_function([raw_type()], raw_type(), [t_constraint()]) -> t_function_det().
t_function(Types, Ret, Constraints) ->
  Rep = {Types, Ret, Constraints},
  Ts = [T || {_V, T} <- Constraints],
  #t{kind = ?function_tag, rep = Rep, deps = unify_deps([Ret|Types] ++ Ts)}.

%% Accessors of representations.

-spec params_of_t_function_det(t_function_det()) -> [raw_type()].
params_of_t_function_det(#t{kind = ?function_tag, rep = {Params, _Ret, _Constraints}}) ->
  Params.

-spec ret_of_t_function_det(t_function_det()) -> raw_type().
ret_of_t_function_det(#t{kind = ?function_tag, rep = {_Params, Ret, _Constraints}}) ->
  Ret.

-spec atom_of_t_atom_lit(t_atom_lit()) -> atom().
atom_of_t_atom_lit(#t{kind = ?atom_lit_tag, rep = Atom}) ->
  Atom.

-spec integer_of_t_integer_lit(t_integer_lit()) -> integer().
integer_of_t_integer_lit(#t{kind = ?integer_lit_tag, rep = Integer}) ->
  Integer.

-spec elements_type_of_t_list(t_list()) -> raw_type().
elements_type_of_t_list(#t{kind = ?list_tag, rep = Type}) ->
  Type.

-spec elements_type_of_t_nonempty_list(t_nonempty_list()) -> raw_type().
elements_type_of_t_nonempty_list(#t{kind = ?nonempty_list_tag, rep = Type}) ->
  Type.

-spec elements_types_of_t_tuple(t_tuple()) -> [raw_type()].
elements_types_of_t_tuple(#t{kind = ?tuple_tag, rep = Types}) ->
  Types.

-spec elements_types_of_t_union(t_union()) -> [raw_type()].
elements_types_of_t_union(#t{kind = ?union_tag, rep = Types}) ->
  Types.

-spec bounds_of_t_range(t_range()) -> {t_range_limit(), t_range_limit()}.
bounds_of_t_range(#t{kind = ?range_tag, rep = Limits}) ->
  Limits.

-spec segment_size_of_bitstring(t_bitstring()) -> integer().
segment_size_of_bitstring(#t{kind = ?bitstring_tag, rep = Sz}) ->
  Sz.

-spec is_tvar_wild_card(t_type_var()) -> boolean().
is_tvar_wild_card(#t{kind = ?type_variable, rep = {?type_var, Var}}) ->
  Var =:= '_'.

%% Helper functions for kinds.

-spec get_kind(raw_type()) -> atom().
get_kind(Type) ->
  Type#t.kind.

%% Helper functions for dependencies.

-spec get_deps(raw_type()) -> deps().
get_deps(Type) ->
  Type#t.deps.

-spec has_deps(raw_type()) -> boolean().
has_deps(Type) ->
  get_deps(Type) =/= ordsets:new().

-spec add_dep(dep(), deps()) -> deps().
add_dep(Dep, Deps) ->
  ordsets:add_element(Dep, Deps).

-spec unify_deps([raw_type()]) -> deps().
unify_deps(Types) ->
  ordsets:union([T#t.deps || T <- Types]).

%% Deal with specs.

-spec retrieve_specs([cuter_cerl:cerl_attr_spec()]) -> stored_specs().
retrieve_specs(SpecAttrs) ->
  lists:foldl(fun process_spec_attr/2, dict:new(), SpecAttrs).

-spec process_spec_attr(cuter_cerl:cerl_attr_spec(), stored_specs()) -> stored_specs().
process_spec_attr({FA, Specs}, Processed) ->
  Xs = [t_spec_from_form(Spec) || Spec <- Specs],
  dict:store(FA, Xs, Processed).

-spec t_spec_from_form(cuter_cerl:cerl_spec_func()) -> t_function_det().
t_spec_from_form({type, _, 'fun', _}=Fun) ->
  t_function_from_form(Fun);
t_spec_from_form({type, _, 'bounded_fun', _}=Fun) ->
  t_bounded_function_from_form(Fun).

-spec find_spec(stored_spec_key(), stored_specs()) -> {'ok', stored_spec_value()} | 'error'.
find_spec(FA, Specs) ->
  dict:find(FA, Specs).

%% Parse the spec of an MFA.

-type spec_parse_reply() :: {error, has_remote_types | recursive_type}
                          | {error, unsupported_type, type_name()}
                          | {ok, erl_spec()}.

-spec parse_spec(stored_spec_key(), stored_spec_value(), stored_types()) -> spec_parse_reply().
parse_spec(FA, Spec, Types) ->
  try parse_spec_clauses(FA, Spec, Types, []) of
    {error, has_remote_types}=E -> E;
    Parsed -> {ok, Parsed}
  catch
    throw:remote_type -> {error, has_remote_types};
    throw:recursive_type -> {error, recursive_type};
    throw:{unsupported, Name} -> {error, unsupported_type, Name}
  end.


parse_spec_clauses(_FA, [], _Types, Acc) ->
  lists:reverse(Acc);
parse_spec_clauses(FA, [Clause|Clauses], Types, Acc) ->
  case has_deps(Clause) of
    true  -> {error, has_remote_types};
    false ->
      Visited = ordsets:add_element(FA, ordsets:new()),
      Simplified = simplify(Clause, Types, dict:new(), Visited),
      parse_spec_clauses(FA, Clauses, Types, [Simplified|Acc])
  end.

add_constraints_to_env([], Env) ->
  Env;
add_constraints_to_env([{Var, Type}|Cs], Env) ->
  F = fun(StoredTypes, E, Visited) -> simplify(Type, StoredTypes, E, Visited) end,
  Env1 = dict:store(Var#t.rep, F, Env),
  add_constraints_to_env(Cs, Env1).

bind_parameters([], [], Env) ->
  Env;
bind_parameters([P|Ps], [A|As], Env) ->
  F = fun(StoredTypes, E, Visited) -> simplify(A, StoredTypes, E, Visited) end,
  Env1 = dict:store(P, F, Env),
  bind_parameters(Ps, As, Env1).

-spec simplify(raw_type(), stored_types(), type_var_env(), ordsets:ordset(stored_spec_key())) -> raw_type().
%% fun
simplify(#t{kind = ?function_tag, rep = {Params, Ret, Constraints}}=Raw, StoredTypes, Env, Visited) ->
  Env1 = add_constraints_to_env(Constraints, Env),
  ParamsSimplified = [simplify(P, StoredTypes, Env1, Visited) || P <- Params],
  RetSimplified = simplify(Ret, StoredTypes, Env1, Visited),
  Rep = {ParamsSimplified, RetSimplified, []},
  Raw#t{rep = Rep};
%% tuple
simplify(#t{kind = ?tuple_tag, rep = Types}=Raw, StoredTypes, Env, Visited) ->
  Rep = [simplify(T, StoredTypes, Env, Visited) || T <- Types],
  Raw#t{rep = Rep};
%% list / nonempty_list
simplify(#t{kind = Tag, rep = Type}=Raw, StoredTypes, Env, Visited) when Tag =:= ?list_tag; Tag =:= ?nonempty_list_tag ->
  Rep = simplify(Type, StoredTypes, Env, Visited),
  Raw#t{rep = Rep};
%% union
simplify(#t{kind = ?union_tag, rep = Types}=Raw, StoredTypes, Env, Visited) ->
  Rep = [simplify(T, StoredTypes, Env, Visited) || T <- Types],
  Raw#t{rep = Rep};
%% local type
simplify(#t{kind = ?local_tag, rep = {Name, Args}}, StoredTypes, Env, Visited) ->
  Arity = length(Args),
  TA = {Name, Arity},
  case ordsets:is_element(TA, Visited) of
    true -> throw(recursive_type);
    false ->
      case dict:find({type, Name, Arity}, StoredTypes) of
        error -> throw({unsupported, Name});
        {ok, {Type, Params}} ->
          Env1 = bind_parameters(Params, Args, Env),
          simplify(Type, StoredTypes, Env1, [TA|Visited])
      end
  end;
%% type variable
simplify(#t{kind = ?type_variable, rep = TVar}=T, StoredTypes, Env, Visited) ->
  case is_tvar_wild_card(T) of
    true -> t_any();
    false ->
      V = dict:fetch(TVar, Env),
      V(StoredTypes, Env, Visited)
  end;
simplify(#t{kind = ?remote_tag}, _StoredTypes, _Env, _Visited) ->
  throw(remote_type);
%% record
simplify(#t{kind = ?record_tag, rep = {Name, OverridenFields}}, StoredTypes, Env, Visited) ->
  RecordDecl = dict:fetch({record, Name}, StoredTypes),
  Fields = fields_of_t_record(RecordDecl),
  ActualFields = replace_record_fields(Fields, OverridenFields),
  FinalFields = [{N, simplify(T, StoredTypes, Env, Visited)} || {N, T} <- ActualFields],
  Simplified = [T || {_, T} <- FinalFields],
  t_tuple([t_atom_lit(Name)|Simplified]);
%% all others
simplify(Raw, _StoredTypes, _Env, _Visited) ->
  Raw.

-spec replace_record_fields([record_field_type()], [record_field_type()]) -> [record_field_type()].
replace_record_fields(Fields, []) ->
  Fields;
replace_record_fields(Fields, [{Name, Type}|Rest]) ->
  Replaced = lists:keyreplace(Name, 1, Fields, {Name, Type}),
  replace_record_fields(Replaced, Rest).
