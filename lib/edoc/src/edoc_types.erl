%% =====================================================================
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
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Datatype representation for EDoc.

-module(edoc_types).

-export([is_predefined/2, is_new_predefined/2,
         to_ref/1, to_xml/2, to_label/1, arg_names/1, set_arg_names/2,
         arg_descs/1, range_desc/1]).

%% @headerfile "edoc_types.hrl"

-include("edoc_types.hrl").
-include_lib("xmerl/include/xmerl.hrl").

is_predefined(cons, 2) -> true;
is_predefined(deep_string, 0) -> true;
is_predefined(F, A) -> erl_internal:is_type(F, A).

is_new_predefined(map, 0) -> true;
is_new_predefined(_, _) -> false.

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
    edoc_refs:get_uri(to_ref(Name), Env).

to_xml(#t_var{name = N}, _Env) ->
    {typevar, [{name, atom_to_list(N)}], []};
to_xml(#t_name{module = [], name = N}, _Env) ->
    {erlangName, [{name, atom_to_list(N)}], []};
to_xml(#t_name{app = [], module = M, name = N}, _Env) ->
    {erlangName, [{module, atom_to_list(M)},
		  {name, atom_to_list(N)}], []};
to_xml(#t_name{app = A, module = M, name = N}, _Env) ->
    {erlangName, [{app, atom_to_list(A)},
		  {module, atom_to_list(M)},
		  {name, atom_to_list(N)}], []};
to_xml(#t_type{name = N, args = As}, Env) ->
    Predef = case N of
		 #t_name{module = [], name = T} ->
                     NArgs = length(As),
		     is_predefined(T, NArgs);
		 _ ->
		     false
	     end,
    HRef = case Predef of
	       true -> [];
	       false -> [{href, get_uri(N, Env)}]
	   end,
    {abstype, HRef, [to_xml(N, Env) | map(fun wrap_utype/2, As, Env)]};
to_xml(#t_fun{args = As, range = T}, Env) ->
    {'fun', [{argtypes, map(fun wrap_utype/2, As, Env)},
	     wrap_utype(T, Env)]};
to_xml(#t_map{ types = Ts}, Env) ->
    {map, map(fun to_xml/2, Ts, Env)};
to_xml(#t_map_field{assoc_type = AT, k_type=K, v_type=V}, Env) ->
    {map_field, [{assoc_type, AT}], [wrap_utype(K,Env), wrap_utype(V, Env)]};
to_xml(#t_tuple{types = Ts}, Env) ->
    {tuple, map(fun wrap_utype/2, Ts, Env)};
to_xml(#t_list{type = T}, Env) ->
    {list, [wrap_utype(T, Env)]};
to_xml(#t_nil{}, _Env) ->
    nil;
to_xml(#t_paren{type = T}, Env) ->
    {paren, [wrap_utype(T, Env)]};
to_xml(#t_nonempty_list{type = T}, Env) ->
    {nonempty_list, [wrap_utype(T, Env)]};
to_xml(#t_atom{val = V}, _Env) ->
    {atom, [{value, atom_to_list(V)}], []};
to_xml(#t_integer{val = V}, _Env) ->
    {integer, [{value, integer_to_list(V)}], []};
to_xml(#t_integer_range{from = From, to = To}, _Env) ->
    {range, [{value, integer_to_list(From)++".."++integer_to_list(To)}], []};
to_xml(#t_binary{base_size = 0, unit_size = 0}, _Ens) ->
    {binary, [{value, "<<>>"}], []};
to_xml(#t_binary{base_size = B, unit_size = 0}, _Ens) ->
    {binary, [{value, io_lib:fwrite("<<_:~w>>", [B])}], []};
%to_xml(#t_binary{base_size = 0, unit_size = 8}, _Ens) ->
%    {binary, [{value, "binary()"}], []};
to_xml(#t_binary{base_size = 0, unit_size = U}, _Ens) ->
    {binary, [{value, io_lib:fwrite("<<_:_*~w>>", [U])}], []};
to_xml(#t_binary{base_size = B, unit_size = U}, _Ens) ->
    {binary, [{value, io_lib:fwrite("<<_:~w, _:_*~w>>", [B, U])}], []};
to_xml(#t_float{val = V}, _Env) ->
    {float, [{value, io_lib:write(V)}], []};
to_xml(#t_union{types = Ts}, Env) ->
    {union, map(fun wrap_utype/2, Ts, Env)};
to_xml(#t_record{name = N = #t_atom{}, fields = Fs}, Env) ->
    {record, [to_xml(N, Env) | map(fun to_xml/2, Fs, Env)]};
to_xml(#t_field{name = N = #t_atom{}, type = T}, Env) ->
    {field, [to_xml(N, Env), wrap_type(T, Env)]};
to_xml(#t_def{name = N = #t_var{}, type = T}, Env) ->
    {localdef, [to_xml(N, Env), wrap_type(T, Env)]};
to_xml(#t_def{name = N, type = T}, Env) ->
    {localdef, [{label, to_label(N)}],
     [to_xml(N, Env), wrap_type(T, Env)]};
to_xml(#t_spec{name = N, type = T, defs = Ds}, Env) ->
    {typespec, [to_xml(N, Env), wrap_utype(T, Env)
		| map(fun to_xml/2, Ds, Env)]};
to_xml(#t_typedef{name = N, args = As, type = undefined, defs = Ds},
	 Env) ->
    {typedef, [to_xml(N, Env),
	       {argtypes, map(fun wrap_utype/2, As, Env)}
	       | map(fun to_xml/2, Ds, Env)]};
to_xml(#t_typedef{name = N, args = As, type = T, defs = Ds}, Env) ->
    {typedef, [to_xml(N, Env),
	       {argtypes, map(fun wrap_utype/2, As, Env)},
	       wrap_type(T, Env)
	       | map(fun to_xml/2, Ds, Env)]};
to_xml(#t_throws{type = T, defs = Ds}, Env) ->
    {throws, [wrap_type(T, Env)
	      | map(fun to_xml/2, Ds, Env)]}.

wrap_type(T, Env) ->
    {type, [to_xml(T, Env)]}.

wrap_utype(T, Env) ->
    E = to_xml(T, Env),
    case arg_name(T) of
	'_' -> {type, [E]};
	A -> {type, [{name, atom_to_list(A)}], [E]}
    end.

map(F, Xs, Env) ->
    [F(X, Env) || X <- Xs].

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
