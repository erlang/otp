%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Datatype representation for EDoc.

-module(edoc_types).

-export([is_predefined/2, is_new_predefined/2, is_predefined_otp_type/2,
         to_ref/1, to_xml/2, to_label/1, arg_names/1, set_arg_names/2,
         arg_descs/1, range_desc/1]).

%% @headerfile "edoc_types.hrl"

-include("edoc_types.hrl").
-include_lib("xmerl/include/xmerl.hrl").


is_predefined(any, 0) -> true;
is_predefined(atom, 0) -> true;
is_predefined(binary, 0) -> true;
is_predefined(bool, 0) -> true;    % kept for backwards compatibility
is_predefined(char, 0) -> true;
is_predefined(cons, 2) -> true;
is_predefined(deep_string, 0) -> true;
is_predefined(float, 0) -> true;
is_predefined(function, 0) -> true;
is_predefined(integer, 0) -> true;
is_predefined(list, 0) -> true;
is_predefined(list, 1) -> true;
is_predefined(nil, 0) -> true;
is_predefined(none, 0) -> true;
is_predefined(no_return, 0) -> true;
is_predefined(number, 0) -> true;
is_predefined(pid, 0) -> true;
is_predefined(port, 0) -> true;
is_predefined(reference, 0) -> true;
is_predefined(string, 0) -> true;
is_predefined(term, 0) -> true;
is_predefined(tuple, 0) -> true;
is_predefined(F, A) -> is_new_predefined(F, A).

%% Should eventually be coalesced with is_predefined/2.
is_new_predefined(arity, 0) -> true;
is_new_predefined(bitstring, 0) -> true;
is_new_predefined(boolean, 0) -> true;
is_new_predefined(byte, 0) -> true;
is_new_predefined(iodata, 0) -> true;
is_new_predefined(iolist, 0) -> true;
is_new_predefined(maybe_improper_list, 0) -> true;
is_new_predefined(maybe_improper_list, 2) -> true;
is_new_predefined(mfa, 0) -> true;
is_new_predefined(module, 0) -> true;
is_new_predefined(neg_integer, 0) -> true;
is_new_predefined(node, 0) -> true;
is_new_predefined(non_neg_integer, 0) -> true;
is_new_predefined(nonempty_improper_list, 2) -> true;
is_new_predefined(nonempty_list, 0) -> true;
is_new_predefined(nonempty_list, 1) -> true;
is_new_predefined(nonempty_maybe_improper_list, 0) -> true;
is_new_predefined(nonempty_maybe_improper_list, 2) -> true;
is_new_predefined(nonempty_string, 0) -> true;
is_new_predefined(pos_integer, 0) -> true;
is_new_predefined(timeout, 0) -> true;
is_new_predefined(_, _) -> false.

%% The following types will be removed later, but they are currently
%% kind of built-in.
is_predefined_otp_type(array, 0) -> true;
is_predefined_otp_type(dict, 0) -> true;
is_predefined_otp_type(digraph, 0) -> true;
is_predefined_otp_type(gb_set, 0) -> true;
is_predefined_otp_type(gb_tree, 0) -> true;
is_predefined_otp_type(queue, 0) -> true;
is_predefined_otp_type(set, 0) -> true;
is_predefined_otp_type(_, _) -> false.

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
		     (is_predefined(T, NArgs)
                      orelse is_predefined_otp_type(T, NArgs));
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
    {atom, [{value, io_lib:write(V)}], []};
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
