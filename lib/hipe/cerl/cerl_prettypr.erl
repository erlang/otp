%% =====================================================================
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%% %CopyrightEnd%
%%
%% Core Erlang prettyprinter, using the 'prettypr' module.
%%
%% Copyright (C) 1999-2002 Richard Carlsson
%%
%% Author contact: richardc@it.uu.se
%% =====================================================================
%%
%% @doc Core Erlang prettyprinter.
%%
%% <p>This module is a front end to the pretty-printing library module
%% <code>prettypr</code>, for text formatting of Core Erlang abstract
%% syntax trees defined by the module <code>cerl</code>.</p>

%% TODO: add printing of comments for `comment'-annotations?

-module(cerl_prettypr).

-define(NO_UNUSED, true).

-export([format/1, format/2, annotate/3]).
-ifndef(NO_UNUSED).
-export([best/1, best/2, layout/1, layout/2, get_ctxt_paperwidth/1,
 	 set_ctxt_paperwidth/2, get_ctxt_linewidth/1,
 	 set_ctxt_linewidth/2, get_ctxt_hook/1, set_ctxt_hook/2,
 	 get_ctxt_user/1, set_ctxt_user/2]).
-endif.

-import(prettypr, [text/1, nest/2, above/2, beside/2, sep/1, par/1,
		   par/2, follow/3, follow/2, floating/1, empty/0]).

-import(cerl, [abstract/1, alias_pat/1, alias_var/1, apply_args/1,
	       apply_op/1, atom_lit/1, binary_segments/1, bitstr_val/1,
	       bitstr_size/1, bitstr_unit/1, bitstr_type/1,
	       bitstr_flags/1, call_args/1, call_module/1, call_name/1,
	       case_arg/1, case_clauses/1, catch_body/1, c_atom/1,
	       c_binary/1, c_bitstr/5, c_int/1, clause_body/1,
	       clause_guard/1, clause_pats/1, concrete/1, cons_hd/1,
	       cons_tl/1, float_lit/1, fun_body/1, fun_vars/1,
	       get_ann/1, int_lit/1, is_c_cons/1, is_c_let/1,
	       is_c_nil/1, is_c_seq/1, is_print_string/1, let_arg/1,
	       let_body/1, let_vars/1, letrec_body/1, letrec_defs/1,
	       module_attrs/1, module_defs/1, module_exports/1,
	       module_name/1, primop_args/1, primop_name/1,
	       receive_action/1, receive_clauses/1, receive_timeout/1,
	       seq_arg/1, seq_body/1, string_lit/1, try_arg/1,
	       try_body/1, try_vars/1, try_evars/1, try_handler/1,
	       tuple_es/1, type/1, values_es/1, var_name/1,
	       map_arg/1, map_es/1, is_c_map_empty/1,
	       map_pair_key/1, map_pair_val/1, map_pair_op/1
	   ]).

-define(PAPER, 76).
-define(RIBBON, 45).
-define(NOUSER, undefined).
-define(NOHOOK, none).

-type hook() :: 'none' | fun((cerl:cerl(), _, _) -> prettypr:document()).

-record(ctxt, {line = 0         :: integer(),
	       body_indent = 4  :: non_neg_integer(),
	       sub_indent = 2   :: non_neg_integer(),
	       hook = ?NOHOOK   :: hook(),
	       noann = false    :: boolean(),
	       paper = ?PAPER   :: integer(),
	       ribbon = ?RIBBON :: integer(),
	       user = ?NOUSER   :: term()}).
-type context() :: #ctxt{}.

%% =====================================================================
%% The following functions examine and modify contexts:

%% @spec (context()) -> integer()
%% @doc Returns the paper widh field of the prettyprinter context.
%% @see set_ctxt_paperwidth/2

-ifndef(NO_UNUSED).
get_ctxt_paperwidth(Ctxt) ->
    Ctxt#ctxt.paper.
-endif.	% NO_UNUSED
%% @clear

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the paper widh field of the prettyprinter context.
%%
%% <p> Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.</p>
%%
%% @see get_ctxt_paperwidth/1

-ifndef(NO_UNUSED).
set_ctxt_paperwidth(Ctxt, W) ->
    Ctxt#ctxt{paper = W}.
-endif.	% NO_UNUSED
%% @clear

%% @spec (context()) -> integer()
%% @doc Returns the line widh field of the prettyprinter context.
%% @see set_ctxt_linewidth/2

-ifndef(NO_UNUSED).
get_ctxt_linewidth(Ctxt) ->
    Ctxt#ctxt.ribbon.
-endif.	% NO_UNUSED
%% @clear

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the line widh field of the prettyprinter context.
%%
%% <p> Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.</p>
%%
%% @see get_ctxt_linewidth/1

-ifndef(NO_UNUSED).
set_ctxt_linewidth(Ctxt, W) ->
    Ctxt#ctxt{ribbon = W}.
-endif.	% NO_UNUSED
%% @clear

%% @spec (context()) -> hook()
%% @doc Returns the hook function field of the prettyprinter context.
%% @see set_ctxt_hook/2

-ifndef(NO_UNUSED).
get_ctxt_hook(Ctxt) ->
    Ctxt#ctxt.hook.
-endif.	% NO_UNUSED
%% @clear

%% @spec (context(), hook()) -> context()
%% @doc Updates the hook function field of the prettyprinter context.
%% @see get_ctxt_hook/1

-ifndef(NO_UNUSED).
set_ctxt_hook(Ctxt, Hook) ->
    Ctxt#ctxt{hook = Hook}.
-endif.	% NO_UNUSED
%% @clear

%% @spec (context()) -> term()
%% @doc Returns the user data field of the prettyprinter context.
%% @see set_ctxt_user/2

-ifndef(NO_UNUSED).
get_ctxt_user(Ctxt) ->
    Ctxt#ctxt.user.
-endif.	% NO_UNUSED
%% @clear

%% @spec (context(), term()) -> context()
%% @doc Updates the user data field of the prettyprinter context.
%% @see get_ctxt_user/1

-ifndef(NO_UNUSED).
set_ctxt_user(Ctxt, X) ->
    Ctxt#ctxt{user = X}.
-endif.	% NO_UNUSED
%% @clear


%% =====================================================================
%% @spec format(Tree::cerl()) -> string()
%% @equiv format(Tree, [])

-spec format(cerl:cerl()) -> string().

format(Node) ->
    format(Node, []).


%% =====================================================================
%% @spec format(Tree::cerl(), Options::[term()]) -> string()
%%           cerl() = cerl:cerl()
%%
%% @type hook() = (cerl(), context(), Continuation) -> document()
%%	    Continuation = (cerl(), context()) -> document().
%%
%% A call-back function for user-controlled formatting. See <a
%% href="#format-2"><code>format/2</code></a>.
%%
%% @type context(). A representation of the current context of the
%% pretty-printer. Can be accessed in hook functions.
%%
%% @doc Prettyprint-formats a Core Erlang syntax tree as text.
%%
%% <p>Available options:
%% <dl>
%%   <dt>{hook, none | <a href="#type-hook">hook()</a>}</dt>
%%       <dd>Unless the value is <code>none</code>, the given function
%%       is called for every node; see below for details. The default
%%       value is <code>none</code>.</dd>
%%
%%   <dt>{noann, boolean()}</dt>
%%       <dd>If the value is <code>true</code>, annotations on the code
%%       are not printed. The default value is <code>false</code>.</dd>
%%
%%   <dt>{paper, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, including indentation. The default value is 76.</dd>
%%
%%   <dt>{ribbon, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, not counting indentation. The default value is 45.</dd>
%%
%%   <dt>{user, term()}</dt>
%%       <dd>User-specific data for use in hook functions. The default
%%       value is <code>undefined</code>.</dd>
%% </dl></p>
%%
%% <p>A hook function (cf. the <a
%% href="#type-hook"><code>hook()</code></a> type) is passed the current
%% syntax tree node, the context, and a continuation. The context can be
%% examined and manipulated by functions such as
%% <code>get_ctxt_user/1</code> and <code>set_ctxt_user/2</code>. The
%% hook must return a "document" data structure (see
%% <code>layout/2</code> and <code>best/2</code>); this may be
%% constructed in part or in whole by applying the continuation
%% function. For example, the following is a trivial hook:
%% <pre>
%%     fun (Node, Ctxt, Cont) -> Cont(Node, Ctxt) end
%% </pre>
%% which yields the same result as if no hook was given.
%% The following, however:
%% <pre>
%%     fun (Node, Ctxt, Cont) ->
%%         Doc = Cont(Node, Ctxt),
%%         prettypr:beside(prettypr:text("&lt;b>"),
%%                         prettypr:beside(Doc,
%%                                         prettypr:text("&lt;/b>")))
%%     end
%% </pre>
%% will place the text of any annotated node (regardless of the
%% annotation data) between HTML "boldface begin" and "boldface end"
%% tags. The function <code>annotate/3</code> is exported for use in
%% hook functions.</p>
%%
%% @see cerl
%% @see format/1
%% @see layout/2
%% @see best/2
%% @see annotate/3
%% @see get_ctxt_user/1
%% @see set_ctxt_user/2

-spec format(cerl:cerl(), [term()]) -> string().

format(Node, Options) ->
    W = proplists:get_value(paper, Options, ?PAPER),
    L = proplists:get_value(ribbon, Options, ?RIBBON),
    prettypr:format(layout(Node, Options), W, L).


%% =====================================================================
%% @spec best(Tree::cerl()) -> empty | document()
%% @equiv best(Node, [])

-ifndef(NO_UNUSED).
best(Node) ->
    best(Node, []).
-endif.	% NO_UNUSED
%% @clear


%% =====================================================================
%% @spec best(Tree::cerl(), Options::[term()]) ->
%%           empty | document()
%%
%% @doc Creates a fixed "best" abstract layout for a Core Erlang syntax
%% tree. This is similar to the <code>layout/2</code> function, except
%% that here, the final layout has been selected with respect to the
%% given options. The atom <code>empty</code> is returned if no such
%% layout could be produced. For information on the options, see the
%% <code>format/2</code> function.
%%
%% @see best/1
%% @see layout/2
%% @see format/2
%% @see prettypr:best/2

-ifndef(NO_UNUSED).
best(Node, Options) ->
    W = proplists:get_value(paper, Options, ?PAPER),
    L = proplists:get_value(ribbon, Options, ?RIBBON),
    prettypr:best(layout(Node, Options), W, L).
-endif.	% NO_UNUSED
%% @clear


%% =====================================================================
%% @spec layout(Tree::cerl()) -> document()
%% @equiv layout(Tree, [])

-ifndef(NO_UNUSED).
layout(Node) ->
    layout(Node, []).
-endif.	% NO_UNUSED
%% @clear


%% =====================================================================
%% @spec annotate(document(), Terms::[term()], context()) -> document()
%%
%% @doc Adds an annotation containing <code>Terms</code> around the
%% given abstract document. This function is exported mainly for use in
%% hook functions; see <code>format/2</code>.
%%
%% @see format/2

-spec annotate(prettypr:document(), [term()], context()) -> prettypr:document().

annotate(Doc, As0, Ctxt) ->
    case strip_line(As0) of
	[] ->
	    Doc;
	As ->
	    case Ctxt#ctxt.noann of
		false ->
		    Es = seq(As, floating(text(",")), Ctxt,
			     fun lay_concrete/2),
		    follow(beside(floating(text("(")), Doc),
			   beside(text("-| ["),
				  beside(par(Es), floating(text("])")))),
			   Ctxt#ctxt.sub_indent);
		true ->
		    Doc
	    end
    end.


%% =====================================================================
%% @spec layout(Tree::cerl(), Options::[term()]) -> document()
%%	    document() = prettypr:document()
%%
%% @doc Creates an abstract document layout for a syntax tree. The
%% result represents a set of possible layouts (cf. module
%% <code>prettypr</code>). For information on the options, see
%% <code>format/2</code>; note, however, that the <code>paper</code> and
%% <code>ribbon</code> options are ignored by this function.
%%
%% <p>This function provides a low-level interface to the pretty
%% printer, returning a flexible representation of possible layouts,
%% independent of the paper width eventually to be used for formatting.
%% This can be included as part of another document and/or further
%% processed directly by the functions in the <code>prettypr</code>
%% module, or used in a hook function (see <code>format/2</code> for
%% details).</p>
%%
%% @see prettypr
%% @see format/2
%% @see layout/1

-spec layout(cerl:cerl(), [term()]) -> prettypr:document().

layout(Node, Options) ->
    lay(Node,
	#ctxt{hook = proplists:get_value(hook, Options, ?NOHOOK),
	      noann = proplists:get_bool(noann, Options),
	      paper = proplists:get_value(paper, Options, ?PAPER),
	      ribbon = proplists:get_value(ribbon, Options, ?RIBBON),
	      user = proplists:get_value(user, Options)}).

lay(Node, Ctxt) ->
    case get_line(get_ann(Node)) of
	none ->
	    lay_0(Node, Ctxt);
	Line ->
	    if Line > Ctxt#ctxt.line ->
		    Ctxt1 = Ctxt#ctxt{line = Line},
		    Txt = io_lib:format("% Line ~w",[Line]),
%		    beside(lay_0(Node, Ctxt1), floating(text(Txt)));
		    above(floating(text(Txt)), lay_0(Node, Ctxt1));
	       true ->
		    lay_0(Node, Ctxt)
	    end
    end.

lay_0(Node, Ctxt) ->
    case Ctxt#ctxt.hook of
	?NOHOOK ->
	    lay_ann(Node, Ctxt);
	Hook ->
	    %% If there is a hook, we apply it.
	    Hook(Node, Ctxt, fun lay_ann/2)
    end.

%% This adds an annotation list (if nonempty) around a document, unless
%% the `noann' option is enabled.

lay_ann(Node, Ctxt) ->
    Doc = lay_1(Node, Ctxt),
    As = get_ann(Node),
    annotate(Doc, As, Ctxt).

%% This part ignores annotations:

lay_1(Node, Ctxt) ->
    case type(Node) of
	literal ->
	    lay_literal(Node, Ctxt);
	var ->
	    lay_var(Node, Ctxt);
	values ->
	    lay_values(Node, Ctxt);
	cons ->
	    lay_cons(Node, Ctxt);
	tuple ->
	    lay_tuple(Node, Ctxt);
	map ->
	    lay_map(Node, Ctxt);
	map_pair ->
	    lay_map_pair(Node, Ctxt);
	'let' ->
	    lay_let(Node, Ctxt);
	seq ->
	    lay_seq(Node, Ctxt);
	apply ->
	    lay_apply(Node, Ctxt);
	call ->
	    lay_call(Node, Ctxt);
	primop ->
	    lay_primop(Node, Ctxt);
	'case' ->
	    lay_case(Node, Ctxt);
	clause ->
	    lay_clause(Node, Ctxt);
	alias ->
	    lay_alias(Node, Ctxt);
	'fun' ->
	    lay_fun(Node, Ctxt);
	'receive' ->
	    lay_receive(Node, Ctxt);
	'try' ->
	    lay_try(Node, Ctxt);
	'catch' ->
	    lay_catch(Node, Ctxt);
	letrec ->
	    lay_letrec(Node, Ctxt);
	module ->
	    lay_module(Node, Ctxt);
	binary ->
	    lay_binary(Node, Ctxt);
	bitstr ->
	    lay_bitstr(Node, Ctxt)
    end.

lay_literal(Node, Ctxt) ->
    case concrete(Node) of
	V when is_atom(V) ->
	    text(atom_lit(Node));
	V when is_float(V) ->
	    text(tidy_float(float_lit(Node)));
	V when is_integer(V) ->
	    %% Note that we do not even try to recognize values
	    %% that could represent printable characters - we
	    %% always print an integer.
	    text(int_lit(Node));
        V when is_bitstring(V) ->
            Val = fun(I) when is_integer(I) -> I;
                     (B) when is_bitstring(B) ->
                          BZ = bit_size(B), <<BV:BZ>> = B, BV
                  end,
            Sz = fun(I) when is_integer(I) -> 8;
                    (B) when is_bitstring(B) -> bit_size(B)
                 end,
	    lay_binary(c_binary([c_bitstr(abstract(Val(B)),
					  abstract(Sz(B)),
					  abstract(1),
					  abstract(integer),
					  abstract([unsigned, big]))
				 || B <- bitstring_to_list(V)]),
		       Ctxt);
	[] ->
	    text("[]");
	[_ | _] ->
	    %% `lay_cons' will check for strings.
	    lay_cons(Node, Ctxt);
	V when is_tuple(V) ->
	    lay_tuple(Node, Ctxt);
	M when is_map(M) ->
            lay_map(Node, Ctxt)
    end.

lay_var(Node, Ctxt) ->
    %% When formatting variable names, no two names should ever map to
    %% the same string. We assume below that an atom representing a
    %% variable name either has the character sequence of a proper
    %% variable, or otherwise does not need single-quoting.
    case var_name(Node) of
	V when is_atom(V) ->
	    S = atom_to_list(V),
	    case S of
		[C | _] when C >= $A, C =< $Z ->
		    %% Ordinary uppercase-prefixed names are printed
		    %% just as they are.
		    text(S);
		[C | _] when C >= $\300, C =< $\336, C /= $\327 ->
		    %% These are also uppercase (ISO 8859-1).
		    text(S);
		[$_| _] ->
		    %% If the name starts with '_' we keep the name as is.
		    text(S);
		_ ->
		    %% Plain atom names are prefixed with a single "_".
		    %% E.g. 'foo' => "_foo".
		    text([$_ | S])
	    end;
	V when is_integer(V) ->
	    %% Integers are always simply prefixed with "_";
	    %% e.g. 4711 => "_4711".
	    text([$_ | integer_to_list(V)]);
	{N, A} when is_atom(N), is_integer(A) ->
	    %% Function names have no overlap problem.
	    beside(lay_noann(c_atom(atom_to_list(N)), Ctxt),
		   beside(text("/"), lay_noann(c_int(A), Ctxt)))
    end.

lay_values(Node, Ctxt) ->
    lay_value_list(values_es(Node), Ctxt).

lay_cons(Node, Ctxt) ->
    case is_print_string(Node) of
	true ->
	    lay_string(string_lit(Node), Ctxt);
	false ->
	    beside(floating(text("[")),
		   beside(par(lay_list_elements(Node, Ctxt)),
			  floating(text("]"))))
    end.

lay_string(S, Ctxt) ->
    %% S includes leading/trailing double-quote characters. The segment
    %% width is 2/3 of the ribbon width - this seems to work well.
    W = (Ctxt#ctxt.ribbon) * 2 div 3,
    lay_string_1(S, length(S), W).

lay_string_1(S, L, W) when L > W, W > 0 ->
    %% Note that L is the minimum, not the exact, printed length.
    case split_string(S, W - 1, L) of
	{_, ""} ->
	    text(S);
	{S1, S2} ->
	    above(text(S1 ++ "\""),
		  lay_string_1([$" | S2], L - W + 1, W))
    end;
lay_string_1(S, _L, _W) ->
    text(S).

split_string(Xs, N, L) ->
    split_string_1(Xs, N, L, []).

%% We only split strings at whitespace, if possible. We must make sure
%% we do not split an escape sequence.

split_string_1([$\s | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$\s | As]), Xs};
split_string_1([$\t | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$t, $\\ | As]), Xs};
split_string_1([$\n | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$n, $\\ | As]), Xs};
split_string_1([$\\ | Xs], N, L, As) ->
    split_string_2(Xs, N - 1, L - 1, [$\\ | As]);
split_string_1(Xs, N, L, As) when N =< -10, L >= 5 ->
    {lists:reverse(As), Xs};
split_string_1([X | Xs], N, L, As) ->
    split_string_1(Xs, N - 1, L - 1, [X | As]);
split_string_1([], _N, _L, As) ->
    {lists:reverse(As), ""}.

split_string_2([$^, X | Xs], N, L, As) ->
    split_string_1(Xs, N - 2, L - 2, [X, $^ | As]);
split_string_2([X1, X2, X3 | Xs], N, L, As) when
  X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7, X3 >= $0, X3 =< $7 ->
    split_string_1(Xs, N - 3, L - 3, [X3, X2, X1 | As]);
split_string_2([X1, X2 | Xs], N, L, As) when 
  X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7 ->
    split_string_1(Xs, N - 2, L - 2, [X2, X1 | As]);
split_string_2([X | Xs], N, L, As) ->
    split_string_1(Xs, N - 1, L - 1, [X | As]).

lay_tuple(Node, Ctxt) ->
    beside(floating(text("{")),
	   beside(par(seq(tuple_es(Node), floating(text(",")),
			  Ctxt, fun lay/2)),
		  floating(text("}")))).

lay_map(Node, Ctxt) ->
    Arg = map_arg(Node),
    After = case is_c_map_empty(Arg) of
	true -> floating(text("}~"));
	false ->
	    beside(floating(text(" | ")),
		beside(lay(Arg,Ctxt),
		    floating(text("}~"))))
    end,
    beside(floating(text("~{")),
        beside(par(seq(map_es(Node), floating(text(",")), Ctxt, fun lay/2)),
	    After)).

lay_map_pair(Node, Ctxt) ->
    K = map_pair_key(Node),
    V = map_pair_val(Node),
    OpTxt = case concrete(map_pair_op(Node)) of
	assoc -> "=>";
	exact -> ":="
    end,
    beside(lay(K,Ctxt),beside(floating(text(OpTxt)),lay(V,Ctxt))).

lay_let(Node, Ctxt) ->
    V = lay_value_list(let_vars(Node), Ctxt),
    D1 = par([follow(text("let"),
		     beside(V, floating(text(" ="))),
		     Ctxt#ctxt.sub_indent),
	      lay(let_arg(Node), Ctxt)],
	     Ctxt#ctxt.body_indent),
    B = let_body(Node),
    D2 = lay(B, Ctxt),
    case is_c_let(B) of
	true ->
	    sep([beside(D1, floating(text(" in"))), D2]);
	false ->
	    sep([D1, beside(text("in "), D2)])
    end.

lay_seq(Node, Ctxt) ->
    D1 = beside(text("do "), lay(seq_arg(Node), Ctxt)),
    B = seq_body(Node),
    D2 = lay(B, Ctxt),
    case is_c_seq(B) of
	true ->
	    sep([D1, D2]);
	false ->
	    sep([D1, nest(3, D2)])
    end.

lay_apply(Node, Ctxt) ->
    As = seq(apply_args(Node), floating(text(",")), Ctxt,
	     fun lay/2),
    beside(follow(text("apply"), lay(apply_op(Node), Ctxt)),
	   beside(text("("),
		  beside(par(As), floating(text(")"))))).

lay_call(Node, Ctxt) ->
    As = seq(call_args(Node), floating(text(",")), Ctxt,
	     fun lay/2),
    beside(follow(text("call"),
		  beside(beside(lay(call_module(Node), Ctxt),
				floating(text(":"))),
			 lay(call_name(Node), Ctxt)),
		  Ctxt#ctxt.sub_indent),
	   beside(text("("), beside(par(As),
				    floating(text(")"))))).

lay_primop(Node, Ctxt) ->
    As = seq(primop_args(Node), floating(text(",")), Ctxt,
	     fun lay/2),
    beside(follow(text("primop"),
		  lay(primop_name(Node), Ctxt),
		  Ctxt#ctxt.sub_indent),
	   beside(text("("), beside(par(As),
				    floating(text(")"))))).

lay_case(Node, Ctxt) ->
    Cs = seq(case_clauses(Node), none, Ctxt, fun lay/2),
    sep([par([follow(text("case"),
		     lay(case_arg(Node), Ctxt),
		     Ctxt#ctxt.sub_indent),
	      text("of")],
	     Ctxt#ctxt.sub_indent),
	 nest(Ctxt#ctxt.sub_indent,
	      vertical(Cs)),
	 text("end")]).

lay_clause(Node, Ctxt) ->
    P = lay_value_list(clause_pats(Node), Ctxt),
    G = lay(clause_guard(Node), Ctxt),
    H = par([P, follow(follow(text("when"), G,
			      Ctxt#ctxt.sub_indent),
		       floating(text("->")))],
	    Ctxt#ctxt.sub_indent),
    par([H, lay(clause_body(Node), Ctxt)],
	Ctxt#ctxt.body_indent).

lay_alias(Node, Ctxt) ->
    follow(beside(lay(alias_var(Node), Ctxt),
		  text(" =")),
	   lay(alias_pat(Node), Ctxt),
	   Ctxt#ctxt.body_indent).

lay_fun(Node, Ctxt) ->
    Vs = seq(fun_vars(Node), floating(text(",")),
	     Ctxt, fun lay/2),
    par([follow(text("fun"),
		beside(text("("),
		       beside(par(Vs),
			      floating(text(") ->")))),
		Ctxt#ctxt.sub_indent),
	 lay(fun_body(Node), Ctxt)],
	Ctxt#ctxt.body_indent).

lay_receive(Node, Ctxt) ->
    Cs = seq(receive_clauses(Node), none, Ctxt, fun lay/2),
    sep([text("receive"),
	 nest(Ctxt#ctxt.sub_indent, vertical(Cs)),
	 sep([follow(text("after"),
		     beside(lay(receive_timeout(Node), Ctxt),
			    floating(text(" ->"))),
		     Ctxt#ctxt.sub_indent),
	      nest(Ctxt#ctxt.sub_indent,
		   lay(receive_action(Node), Ctxt))])]).

lay_try(Node, Ctxt) ->
    Vs = lay_value_list(try_vars(Node), Ctxt),
    Evs = lay_value_list(try_evars(Node), Ctxt),
    sep([follow(text("try"),
		lay(try_arg(Node), Ctxt),
		Ctxt#ctxt.body_indent),
	 follow(beside(beside(text("of "), Vs),
		       floating(text(" ->"))),
		lay(try_body(Node), Ctxt),
		Ctxt#ctxt.body_indent),
	 follow(beside(beside(text("catch "), Evs),
		       floating(text(" ->"))),
		lay(try_handler(Node), Ctxt),
		Ctxt#ctxt.body_indent)]).

lay_catch(Node, Ctxt) ->
    follow(text("catch"),
	   lay(catch_body(Node), Ctxt),
	   Ctxt#ctxt.sub_indent).

lay_letrec(Node, Ctxt) ->
    Es = seq(letrec_defs(Node), none, Ctxt, fun lay_fdef/2),
    sep([text("letrec"),
	 nest(Ctxt#ctxt.sub_indent, vertical(Es)),
	 beside(text("in "), lay(letrec_body(Node), Ctxt))]).

lay_module(Node, Ctxt) ->
    %% Note that the module name, exports and attributes may not
    %% be annotated in the printed format.
    Xs = seq(module_exports(Node), floating(text(",")), Ctxt,
	     fun lay_noann/2),
    As = seq(module_attrs(Node), floating(text(",")), Ctxt,
	     fun lay_attrdef/2),
    Es = seq(module_defs(Node), none, Ctxt, fun lay_fdef/2),
    sep([follow(text("module"),
		follow(lay_noann(module_name(Node), Ctxt),
		       beside(beside(text("["), par(Xs)),
			      floating(text("]")))),
		Ctxt#ctxt.sub_indent),
	 nest(Ctxt#ctxt.sub_indent,
	      follow(text("attributes"),
		     beside(beside(text("["), par(As)),
			    floating(text("]"))),
		     Ctxt#ctxt.sub_indent)),
	 nest(Ctxt#ctxt.sub_indent, vertical(Es)),
	 text("end")]).

lay_binary(Node, Ctxt) ->
    beside(floating(text("#{")),
	   beside(sep(seq(binary_segments(Node), floating(text(",")),
			  Ctxt, fun lay_bitstr/2)),
		  floating(text("}#")))).

lay_bitstr(Node, Ctxt) ->
    Head = beside(floating(text("#<")),
		  beside(lay(bitstr_val(Node), Ctxt),
			 floating(text(">")))),
    As = [bitstr_size(Node),
	  bitstr_unit(Node),
	  bitstr_type(Node),
	  bitstr_flags(Node)],
    beside(Head, beside(floating(text("(")),
			beside(sep(seq(As, floating(text(",")),
				       Ctxt, fun lay/2)),
			       floating(text(")"))))).

%% In all places where "<...>"-sequences can occur, it is OK to
%% write 1-element sequences without the "<" and ">".

lay_value_list([E], Ctxt) ->
    lay(E, Ctxt);
lay_value_list(Es, Ctxt) ->
    beside(floating(text("<")),
	   beside(par(seq(Es, floating(text(",")),
			  Ctxt, fun lay/2)),
		  floating(text(">")))).

lay_noann(Node, Ctxt) ->
    lay(Node, Ctxt#ctxt{noann = true}).

lay_concrete(T, Ctxt) ->
    lay(abstract(T), Ctxt).

lay_attrdef({K, V}, Ctxt) ->
    follow(beside(lay_noann(K, Ctxt), floating(text(" ="))),
	   lay_noann(V, Ctxt),
	   Ctxt#ctxt.body_indent).

lay_fdef({N, F}, Ctxt) ->
    par([beside(lay(N, Ctxt), floating(text(" ="))),
	 lay(F, Ctxt)],
	Ctxt#ctxt.body_indent).

lay_list_elements(Node, Ctxt) ->
    T = cons_tl(Node),
    A = case Ctxt#ctxt.noann of
	    false ->
		get_ann(T);
	    true ->
		[]
	end,
    H = lay(cons_hd(Node), Ctxt),
    case is_c_cons(T) of
	true when A =:= [] ->
	    [beside(H, floating(text(",")))
	     | lay_list_elements(T, Ctxt)];
	_ ->
	    case is_c_nil(T) of
		true when A =:= [] ->
		    [H];
		_ ->
		    [H, beside(floating(text("| ")),
			       lay(T, Ctxt))]
	    end
    end.

seq([H | T], Separator, Ctxt, Fun) ->
    case T of
	[] ->
	    [Fun(H, Ctxt)];
	_ ->
	    [maybe_append(Separator, Fun(H, Ctxt))
	     | seq(T, Separator, Ctxt, Fun)]
    end;
seq([], _, _, _) ->
    [empty()].

maybe_append(none, D) ->
    D;
maybe_append(Suffix, D) ->
    beside(D, Suffix).

vertical([D]) ->
    D;
vertical([D | Ds]) ->
    above(D, vertical(Ds));
vertical([]) ->
    [].

% horizontal([D]) ->
%     D;
% horizontal([D | Ds]) ->
%     beside(D, horizontal(Ds));
% horizontal([]) ->
%     [].

tidy_float([$., C | Cs]) ->
    [$., C | tidy_float_1(Cs)];  % preserve first decimal digit
tidy_float([$e | _] = Cs) ->
    tidy_float_2(Cs);
tidy_float([C | Cs]) ->
    [C | tidy_float(Cs)];
tidy_float([]) ->
    [].

tidy_float_1([$0, $0, $0 | Cs]) ->
    tidy_float_2(Cs);    % cut mantissa at three consecutive zeros.
tidy_float_1([$e | _] = Cs) ->
    tidy_float_2(Cs);
tidy_float_1([C | Cs]) ->
    [C | tidy_float_1(Cs)];
tidy_float_1([]) ->
    [].

tidy_float_2([$e, $+, $0]) -> [];
tidy_float_2([$e, $+, $0 | Cs]) -> tidy_float_2([$e, $+ | Cs]);
tidy_float_2([$e, $+ | _] = Cs) -> Cs;
tidy_float_2([$e, $-, $0]) -> [];
tidy_float_2([$e, $-, $0 | Cs]) -> tidy_float_2([$e, $- | Cs]);
tidy_float_2([$e, $- | _] = Cs) -> Cs;
tidy_float_2([$e | Cs]) -> tidy_float_2([$e, $+ | Cs]);
tidy_float_2([_ | Cs]) -> tidy_float_2(Cs);
tidy_float_2([]) -> [].

get_line([L | _As]) when is_integer(L) ->
    L;
get_line([_ | As]) ->
    get_line(As);
get_line([]) ->
    none.

strip_line([A | As]) when is_integer(A) ->
    strip_line(As);
strip_line([A | As]) ->
    [A | strip_line(As)];
strip_line([]) ->
    [].

%% =====================================================================
