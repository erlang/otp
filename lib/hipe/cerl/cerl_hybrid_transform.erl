%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(cerl_hybrid_transform).

%% Use compile option `{core_transform, cerl_hybrid_transform}' to
%% insert this as a compilation pass.

-export([transform/2, core_transform/2]).

-spec core_transform(cerl:cerl(), [term()]) -> cerl:cerl().

core_transform(Code, Opts) ->
    cerl:to_records(transform(cerl:from_records(Code), Opts)).

-spec transform(cerl:cerl(), [term()]) -> cerl:cerl().

transform(Code, _Opts) ->
    Code0 = cerl_trees:map(fun unfold_literal/1, Code),
    {Code1, _} = cerl_trees:label(Code0),
    io:fwrite("Running hybrid heap analysis..."),
    {T1,_} = statistics(runtime),
    {Code2, _, Vars} = cerl_messagean:annotate(Code1),
    {T2,_} = statistics(runtime),
    io:fwrite("(~w ms), transform...", [T2 - T1]),
    Code3 = rewrite(Code2, Vars),
    io:fwrite("done.\n"),
    cerl_trees:map(fun fold_literal/1, Code3).

unfold_literal(T) ->
    cerl:unfold_literal(T).

fold_literal(T) ->
    cerl:fold_literal(T).

%% If escape-annotated:
%% {...} => hybrid:tuple([...])
%% [H | T] => hybrid:cons(H, T)
%%
%% Wrapper for args to hybrid:cons/hybrid:tuple that may need copying:
%% hybrid:copy(A)

rewrite(Node, Vars) ->
    case cerl:type(Node) of
	tuple ->
	    Es = rewrite_list(cerl:tuple_es(Node), Vars),
	    case is_escaping(Node) of
		false ->
		    cerl:update_c_tuple(Node, Es);
		true ->
		    Es1 = wrap(Es, Node, Vars),
		    cerl:update_c_call(Node,
				       cerl:abstract(hybrid),
				       cerl:abstract(tuple),
				       [cerl:make_list(Es1)])
%%% 		    cerl:update_c_call(Node, cerl:abstract(hybrid),
%%% 				       cerl:abstract(tuple), Es1)
	    end;
	cons ->
	    H = rewrite(cerl:cons_hd(Node), Vars),
	    T = rewrite(cerl:cons_tl(Node), Vars),
	    case is_escaping(Node) of
		false ->
		    cerl:update_c_cons(Node, H, T);
		true ->
		    Es = wrap([H, T], Node, Vars),
		    cerl:update_c_call(Node,
				       cerl:abstract(hybrid),
				       cerl:abstract(cons),
				       Es)
	    end;
%%% 	call ->
%%% 	    M = rewrite(cerl:call_module(Node)),
%%% 	    F = rewrite(cerl:call_name(Node)),
%%% 	    As = rewrite_list(cerl:call_args(Node)),
%%% 	    case cerl:is_c_atom(M) andalso cerl:is_c_atom(F) of
%%% 		true ->
%%% 		    case {cerl:atom_val(M), cerl:atom_val(F), length(As)} of
%%% 			{erlang, '!', 2} ->
%%% 			    cerl:update_c_call(Node,
%%% 					       cerl:abstract(hipe_bifs),
%%% 					       cerl:abstract(send),
%%% 					       [cerl:make_list(As)]);
%%% 			_ ->
%%% 			    cerl:update_c_call(Node, M, F, As)
%%% 		    end;
%%% 		false ->
%%% 		    cerl:update_c_call(Node, M, F, As)
%%% 	    end;
	clause ->
	    B = rewrite(cerl:clause_body(Node), Vars),
	    cerl:update_c_clause(Node, cerl:clause_pats(Node),
				 cerl:clause_guard(Node), B);
	primop ->
	    case cerl:atom_val(cerl:primop_name(Node)) of
		match_fail ->
		    Node;
		_ ->
		    As = rewrite_list(cerl:primop_args(Node), Vars),
		    cerl:update_c_primop(Node, cerl:primop_name(Node), As)
	    end;
	_T ->
	    case cerl:subtrees(Node) of
		[] ->
		    Node;
		Gs ->
		    cerl:update_tree(Node, [rewrite_list(Ns, Vars)
					    || Ns <- Gs])
	    end
    end.

rewrite_list([N | Ns], Vars) ->
    [rewrite(N, Vars) | rewrite_list(Ns, Vars)];
rewrite_list([], _) ->
    [].

is_escaping(T) ->
    lists:member(escapes, cerl:get_ann(T)).

wrap(Es, Node, Vars) ->
    L = cerl_trees:get_label(Node),
    Xs = dict:fetch(L, Vars),
    wrap(Es, Xs).

wrap([E | Es], [{S, _} | Xs]) ->
    case ordsets:is_element(unsafe, S) of
%%  case cerl:type(E) =/= literal of
	true ->
	    [cerl:c_call(cerl:abstract(hybrid),
			 cerl:abstract(copy),
			 [E])
	     | wrap(Es, Xs)];
	false ->
	    [E | wrap(Es, Xs)]
    end;
wrap([], _) ->
    [].
