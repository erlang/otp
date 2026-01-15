%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2001-2003 Richard Carlsson
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% %CopyrightEnd%
%%
%% @private
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Datatype representation for EDoc.

-module(edoc_types).

-export([is_predefined/2, is_new_predefined/2,
         to_ref/1, to_xml/3, to_label/1, arg_names/1, set_arg_names/2,
         arg_descs/1, range_desc/1]).

%% @headerfile "edoc_types.hrl"

-include("edoc_types.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-type t_spec() :: #t_spec{name :: t_name(),
                          type :: t_fun(),
                          defs :: [t_def()]}.
%% Function specification.

-type type() :: t_atom() | t_binary() | t_float() | t_fun() | t_integer()
	      | t_integer_range() | t_list() | t_nil()| t_nonempty_list()
	      | t_record() | t_tuple() | t_type() | t_union() | t_var()
	      | t_paren().

-type t_typedef() :: #t_typedef{name :: t_name(),
                               args :: [type()],
                               type :: type() | undefined,
                               defs :: [t_def()]}.
%% Type declaration/definition.

-type t_throws() :: #t_throws{type :: type(),
                             defs :: [t_def()]}.
%% Exception declaration.

-type t_def() :: #t_def{name :: t_type() | t_var(),
           type :: type()}.
%% Local definition `name = type'.

-type t_name() :: #t_name{app :: [] | atom(),
			  module :: [] | atom(),
			  name :: [] | atom()}.

-type t_var() :: #t_var{a :: list(),
			name :: [] | atom()}.
%% Type variable.

-type t_type() :: #t_type{a :: list(),
			  name :: t_name(),
			  args :: [type()]}.
%% Abstract type `name(...)'.

-type t_union() :: #t_union{a :: list(),
			    types :: [type()]}.
%% Union type `t1 | ... | tN'.

-type t_fun() :: #t_fun{a :: list(),
			args :: [type()],
			range :: type()}.
%% Function `(t1, ..., tN) -> range'.

-type t_tuple() :: #t_tuple{a :: list(),
			    types :: [type()]}.
%% Tuple type `{t1,...,tN}'.

-type t_list() :: #t_list{a :: list(),
			  type :: type()}.
%% List type `[type]'.

-type t_nil() :: #t_nil{a :: list()}.
%% Empty-list constant `[]'.

-type t_nonempty_list() :: #t_nonempty_list{a :: list(),
					    type :: type()}.
%% List type `[type, ...]'.

-type t_atom() :: #t_atom{a :: list(),
			  val :: atom()}.
%% Atom constant.

-type t_integer() :: #t_integer{a :: list(),
				val :: integer()}.
%% Integer constant.

-type t_integer_range() :: #t_integer_range{a :: list(),
					    from :: integer(),
					    to :: integer()}.

-type t_binary() :: #t_binary{a :: list(),
			      base_size :: integer(),
			      unit_size :: integer()}.

-type t_float() :: #t_float{a :: list(),
			    val :: float()}.
%% Floating-point constant.

-type t_record() :: #t_record{a :: list(),
			      name :: t_atom(),
			      fields :: [t_field()]}.
%% Record "type" `#r{f1, ..., fN}'.

-type t_field() :: #t_field{a :: list(),
			    name :: type(),
			    type :: type()}.
%% Named field `n1 = t1'.

-type t_paren() :: #t_paren{a :: list(), type :: type()}.
%% Parentheses.

is_predefined(cons, 2) -> true;
is_predefined(deep_string, 0) -> true;
is_predefined(F, A) -> erl_internal:is_type(F, A).

is_new_predefined(_, _) -> false.

-spec to_ref(t_typedef() | t_def() | t_type() | t_name()) -> edoc_refs:t().
to_ref(#t_typedef{name = N}) ->
    to_ref(N);
to_ref(#t_def{name = N}) ->
    to_ref(N);
to_ref(#t_type{name = N}) ->
    to_ref(N);
to_ref(#t_name{module = [], name = N}) ->
    edoc_refs:type(N);
to_ref(#t_name{app = [], module = M, name = N}) ->
    edoc_refs:type(M, N);
to_ref(#t_name{app = A, module = M, name = N}) ->
    edoc_refs:type(A, M, N).

to_label(N) ->
    edoc_refs:to_label(to_ref(N)).

get_uri(Name, Env) ->
    NewName = infer_module_app(Name),
    edoc_refs:get_uri(to_ref(NewName), Env).

get_docgen_uri(Name, _Env) ->
    NewName = infer_module_app(Name),
    edoc_refs:get_docgen_link(to_ref(NewName)).

infer_module_app(#t_name{app = [], module = M} = TName) when is_atom(M) ->
    case edoc_lib:infer_module_app(M) of
	no_app ->
	    TName;
	{app, App} when is_atom(App) ->
	    TName#t_name{app = App}
    end;
infer_module_app(Other) ->
    Other.

-spec to_xml(type() | t_spec() | t_typedef() | t_def() | t_throws(),
             term(), Opts :: proplists:proplist()) -> term().
to_xml(#t_var{name = N}, _Env, _Opts) ->
    {typevar, [{name, atom_to_list(N)}], []};
to_xml(#t_name{module = [], name = N}, _Env, _Opts) ->
    {erlangName, [{name, atom_to_list(N)}], []};
to_xml(#t_name{app = [], module = M, name = N}, _Env, _Opts) ->
    {erlangName, [{module, atom_to_list(M)},
		  {name, atom_to_list(N)}], []};
to_xml(#t_name{app = A, module = M, name = N}, _Env, _Opts) ->
    {erlangName, [{app, atom_to_list(A)},
		  {module, atom_to_list(M)},
		  {name, atom_to_list(N)}], []};
to_xml(#t_type{name = N, args = As}, Env, Opts) ->
    Predef = case N of
		 #t_name{module = [], name = T} ->
                     NArgs = length(As),
		     is_predefined(T, NArgs);
		 _ ->
		     false
	     end,
    HRef = case {Predef, proplists:get_value(link_predefined_types, Opts, false)} of
	       {true, false} -> [];
	       {true, true} ->
                   {DocgenRel, DocgenURI} = get_docgen_uri(N#t_name{ module = erlang }, Env),
                   [{'docgen-rel',DocgenRel},
                    {'docgen-href',DocgenURI},
                    {href, get_uri(N#t_name{ module = erlang }, Env)}];
	       {false, _} ->
                   {DocgenRel, DocgenURI} = get_docgen_uri(N, Env),
                   [{'docgen-rel',DocgenRel},
                    {'docgen-href',DocgenURI},
                    {href, get_uri(N, Env)}]
	   end,
    {abstype, HRef, [to_xml(N, Env, Opts) | map(fun wrap_utype/3, As, Env, Opts)]};
to_xml(#t_fun{args = As, range = T}, Env, Opts) ->
    {'fun', [{argtypes, map(fun wrap_utype/3, As, Env, Opts)},
	     wrap_utype(T, Env, Opts)]};
to_xml(#t_map{ types = Ts}, Env, Opts) ->
    {map, map(fun to_xml/3, Ts, Env, Opts)};
to_xml(#t_map_field{assoc_type = AT, k_type=K, v_type=V}, Env, Opts) ->
    {map_field, [{assoc_type, AT}], [wrap_utype(K,Env, Opts), wrap_utype(V, Env, Opts)]};
to_xml(#t_tuple{types = Ts}, Env, Opts) ->
    {tuple, map(fun wrap_utype/3, Ts, Env, Opts)};
to_xml(#t_list{type = T}, Env, Opts) ->
    {list, [wrap_utype(T, Env, Opts)]};
to_xml(#t_nil{}, _Env, _Opts) ->
    nil;
to_xml(#t_paren{type = T}, Env, Opts) ->
    {paren, [wrap_utype(T, Env, Opts)]};
to_xml(#t_nonempty_list{type = T}, Env, Opts) ->
    {nonempty_list, [wrap_utype(T, Env, Opts)]};
to_xml(#t_atom{val = V}, _Env, _Opts) ->
    {atom, [{value, atom_to_list(V)}], []};
to_xml(#t_integer{val = V}, _Env, _Opts) ->
    {integer, [{value, integer_to_list(V)}], []};
to_xml(#t_integer_range{from = From, to = To}, _Env, _Opts) ->
    {range, [{value, integer_to_list(From)++".."++integer_to_list(To)}], []};
to_xml(#t_binary{base_size = 0, unit_size = 0}, _Env, _Opts) ->
    {binary, [{value, "<<>>"}], []};
to_xml(#t_binary{base_size = B, unit_size = 0}, _Env, _Opts) ->
    {binary, [{value, io_lib:fwrite("<<_:~w>>", [B])}], []};
%to_xml(#t_binary{base_size = 0, unit_size = 8}, _Env, _Opts) ->
%    {binary, [{value, "binary()"}], []};
to_xml(#t_binary{base_size = 0, unit_size = U}, _Env, _Opts) ->
    {binary, [{value, io_lib:fwrite("<<_:_*~w>>", [U])}], []};
to_xml(#t_binary{base_size = B, unit_size = U}, _Env, _Opts) ->
    {binary, [{value, io_lib:fwrite("<<_:~w, _:_*~w>>", [B, U])}], []};
to_xml(#t_float{val = V}, _Env, _Opts) ->
    {float, [{value, io_lib:write(V)}], []};
to_xml(#t_union{types = Ts}, Env, Opts) ->
    {union, map(fun wrap_utype/3, Ts, Env, Opts)};
to_xml(#t_record{name = N = #t_atom{}, fields = Fs}, Env, Opts) ->
    {record, [to_xml(N, Env, Opts) | map(fun to_xml/3, Fs, Env, Opts)]};
to_xml(#t_field{name = N = #t_atom{}, type = T}, Env, Opts) ->
    {field, [to_xml(N, Env, Opts), wrap_type(T, Env, Opts)]};
to_xml(#t_def{name = N = #t_var{}, type = T}, Env, Opts) ->
    {localdef, [to_xml(N, Env, Opts), wrap_type(T, Env, Opts)]};
to_xml(#t_def{name = N, type = T}, Env, Opts) ->
    {localdef, [{label, to_label(N)}],
     [to_xml(N, Env, Opts), wrap_type(T, Env, Opts)]};
to_xml(#t_spec{name = N, type = T, defs = Ds}, Env, Opts) ->
    {typespec, [to_xml(N, Env, Opts), wrap_utype(T, Env, Opts)
		| map(fun to_xml/3, Ds, Env, Opts)]};
to_xml(#t_typedef{name = N, args = As, type = undefined, defs = Ds},
	 Env, Opts) ->
    {typedef, [to_xml(N, Env, Opts),
	       {argtypes, map(fun wrap_utype/3, As, Env, Opts)}
	       | map(fun to_xml/3, Ds, Env, Opts)]};
to_xml(#t_typedef{name = N, args = As, type = T, defs = Ds}, Env, Opts) ->
    {typedef, [to_xml(N, Env, Opts),
	       {argtypes, map(fun wrap_utype/3, As, Env, Opts)},
	       wrap_type(T, Env, Opts)
	       | map(fun to_xml/3, Ds, Env, Opts)]};
to_xml(#t_throws{type = T, defs = Ds}, Env, Opts) ->
    {throws, [wrap_type(T, Env, Opts)
	      | map(fun to_xml/3, Ds, Env, Opts)]}.

wrap_type(T, Env, Opts) ->
    {type, [to_xml(T, Env, Opts)]}.

wrap_utype(T, Env, Opts) ->
    E = to_xml(T, Env, Opts),
    case arg_name(T) of
	'_' -> {type, [E]};
	A -> {type, [{name, atom_to_list(A)}], [E]}
    end.

map(F, Xs, Env, Opts) ->
    [F(X, Env, Opts) || X <- Xs].

is_name(A) when is_atom(A) -> true;
is_name(_) -> false.

is_desc(A) when is_list(A) -> true;
is_desc(_) -> false.

arg_name(T) ->
    find(?t_ann(T), fun is_name/1, '_').

arg_names(S) ->
    arg_anns(S, fun is_name/1, '_').

arg_descs(S) ->
    arg_anns(S, fun is_desc/1, "").

range_desc(#t_spec{type = #t_fun{range = T}}) ->
    find(?t_ann(T), fun is_desc/1, "").

arg_anns(#t_spec{type = #t_fun{args = As}}, F, Def) ->
    [find(?t_ann(A), F, Def) || A <- As].

find([A| As], F, Def) ->
    case F(A) of
	true -> A;
	false -> find(As, F, Def)
    end;
find([], _, Def) -> Def.

set_arg_names(S, Ns) ->
    set_arg_anns(S, Ns, fun is_name/1).

%% set_arg_descs(S, Ns) ->
%%    set_arg_anns(S, Ns, fun is_desc/1).

set_arg_anns(#t_spec{type = #t_fun{args = As}=T}=S, Ns, F) ->
    Zip = fun (A, N) ->
		  ?set_t_ann(A, update(?t_ann(A), N, F))
	  end,
    S#t_spec{type = T#t_fun{args = lists:zipwith(Zip, As, Ns)}}.

update([A| As], N, F) ->
    case F(A) of
	true -> [N | As];
	false -> [A| update(As, N, F)]
    end;
update([], N, _) -> [N].
