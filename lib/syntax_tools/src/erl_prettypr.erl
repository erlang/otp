%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 1997-2006 Richard Carlsson
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
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

-module(erl_prettypr).
-moduledoc """
Pretty printing of abstract Erlang syntax trees.

This module is a front end to the pretty-printing library module `prettypr`, for
text formatting of abstract syntax trees defined by the module `erl_syntax`.
""".

-compile(nowarn_deprecated_catch).

-export([format/1, format/2, best/1, best/2, layout/1, layout/2,
	 get_ctxt_precedence/1, set_ctxt_precedence/2,
	 get_ctxt_paperwidth/1, set_ctxt_paperwidth/2,
	 get_ctxt_linewidth/1, set_ctxt_linewidth/2, get_ctxt_hook/1,
	 set_ctxt_hook/2, get_ctxt_user/1, set_ctxt_user/2]).

-import(prettypr, [text/1, nest/2, above/2, beside/2, sep/1, par/1,
		   par/2, floating/3, floating/1, break/1, follow/2,
		   follow/3, empty/0]).

-import(erl_parse, [preop_prec/1, inop_prec/1, func_prec/0,
		    max_prec/0, type_inop_prec/1, type_preop_prec/1]).

-define(PADDING, 2).
-define(PAPER, 80).
-define(RIBBON, 56).
-define(NOUSER, undefined).
-define(NOHOOK, none).

-type hook() :: 'none'
              | fun((syntaxTree(), _, _) -> prettypr:document()).
-type clause_t() :: 'case_expr' | 'fun_expr'
                  | 'if_expr' | 'maybe_expr' | 'receive_expr' | 'try_expr'
                  | {'function', prettypr:document()}
                  | 'spec'.

-record(ctxt, {prec = 0           :: integer(),
	       sub_indent = 2     :: non_neg_integer(),
	       break_indent = 4   :: non_neg_integer(),
	       clause = undefined :: clause_t() | 'undefined',
	       hook = ?NOHOOK     :: hook(),
	       paper = ?PAPER     :: integer(),
	       ribbon = ?RIBBON   :: integer(),
	       user = ?NOUSER     :: term(),
               encoding = epp:default_encoding() :: epp:source_encoding(),
	       empty_lines = sets:new() :: sets:set(integer())}).

-type context() :: #ctxt{}.

-doc """
Returns the operator precedence field of the prettyprinter context.

_See also: _`set_ctxt_precedence/2`.
""".
-spec get_ctxt_precedence(context()) -> integer().

get_ctxt_precedence(Ctxt) ->
    Ctxt#ctxt.prec.

-doc """
Updates the operator precedence field of the prettyprinter context.

See the [`//stdlib/erl_parse`](`m:erl_parse`) module for operator
precedences.

_See also: _[//stdlib/erl_parse](`m:erl_parse`), `get_ctxt_precedence/1`.
""".
-spec set_ctxt_precedence(context(), integer()) -> context().

set_ctxt_precedence(Ctxt, Prec) ->
    set_prec(Ctxt, Prec).

set_prec(Ctxt, Prec) ->
    Ctxt#ctxt{prec = Prec}.    % used internally

reset_prec(Ctxt) ->
    set_prec(Ctxt, 0).    % used internally

-doc """
Returns the paper widh field of the prettyprinter context.

_See also: _`set_ctxt_paperwidth/2`.
""".
-spec get_ctxt_paperwidth(context()) -> integer().

get_ctxt_paperwidth(Ctxt) ->
    Ctxt#ctxt.paper.

-doc """
Updates the paper widh field of the prettyprinter context.

> #### Note {: .info }
>
> Changing this value (and passing the resulting context to a
> continuation function) does not affect the normal formatting, but may
> affect user-defined behaviour in hook functions.

_See also: _`get_ctxt_paperwidth/1`.
""".
-spec set_ctxt_paperwidth(context(), integer()) -> context().

set_ctxt_paperwidth(Ctxt, W) ->
    Ctxt#ctxt{paper = W}.

-doc """
Returns the line widh field of the prettyprinter context.

_See also: _`set_ctxt_linewidth/2`.
""".
-spec get_ctxt_linewidth(context()) -> integer().

get_ctxt_linewidth(Ctxt) ->
    Ctxt#ctxt.ribbon.

-doc """
Updates the line widh field of the prettyprinter context.

> #### Note {: .info }
>
> Changing this value (and passing the resulting context to a
> continuation function) does not affect the normal formatting, but may
> affect user-defined behaviour in hook functions.

_See also: _`get_ctxt_linewidth/1`.
""".
-spec set_ctxt_linewidth(context(), integer()) -> context().

set_ctxt_linewidth(Ctxt, W) ->
    Ctxt#ctxt{ribbon = W}.

-doc """
Returns the hook function field of the prettyprinter context.

_See also: _`set_ctxt_hook/2`.
""".
-spec get_ctxt_hook(context()) -> hook().

get_ctxt_hook(Ctxt) ->
    Ctxt#ctxt.hook.

-doc """
Updates the hook function field of the prettyprinter context.

_See also: _`get_ctxt_hook/1`.
""".
-spec set_ctxt_hook(context(), hook()) -> context().

set_ctxt_hook(Ctxt, Hook) ->
    Ctxt#ctxt{hook = Hook}.

-doc """
Returns the user data field of the prettyprinter context.

_See also: _`set_ctxt_user/2`.
""".
-spec get_ctxt_user(context()) -> term().

get_ctxt_user(Ctxt) ->
    Ctxt#ctxt.user.

-doc """
Updates the user data field of the prettyprinter context.

_See also: _`get_ctxt_user/1`.
""".
-spec set_ctxt_user(context(), term()) -> context().

set_ctxt_user(Ctxt, X) ->
    Ctxt#ctxt{user = X}.


-doc #{equiv => format(Tree, [])}.
-spec format(syntaxTree()) -> string().

format(Node) ->
    format(Node, []).

-doc """
An abstract syntax tree.

See the `m:erl_syntax` module for details.
""".
-type syntaxTree() :: erl_syntax:syntaxTree().


-doc """
Prettyprint-formats an abstract Erlang syntax tree as text.

For example, if you have a `.beam` file that has been compiled with
`debug_info`, the following should print the source code for the
module (as it looks in the debug info representation):

```text
     {ok,{_,[{abstract_code,{_,AC}}]}} =
             beam_lib:chunks("myfile.beam",[abstract_code]),
     io:put_chars(erl_prettypr:format(erl_syntax:form_list(AC)))
```

Available options:

- **`{hook, none | hook()}`** - Unless the value is `none`, the
  given function is called for each node whose list of annotations is
  not empty. The default value is `none`.

- **`{paper, integer()}`** - Specifies the preferred maximum number of
  characters on any line, including indentation. The default value is 80.

- **`{ribbon, integer()}`** - Specifies the preferred maximum number of
  characters on any line, not counting indentation. The default value is 65.

- **`{user, term()}`** - User-specific data for use in hook functions. The
  default value is `undefined`.

- **`{encoding, epp:source_encoding()}`** - Specifies the encoding of the
  generated file.

A hook function (see the [`hook()`](`t:hook/0`) type) is passed the current
syntax tree node, the context, and a continuation. The context can be examined
and manipulated by functions such as [`get_ctxt_user/1`](`get_ctxt_user/1`) and
[`set_ctxt_user/2`](`set_ctxt_user/2`). The hook must return a "document" data
structure (see `layout/2` and `best/2`); this may be constructed in part or in
whole by applying the continuation function. For example, the following is a
trivial hook:

```text
      fun (Node, Ctxt, Cont) -> Cont(Node, Ctxt) end
```

which yields the same result as if no hook was given. The following, however:

```text
      fun (Node, Ctxt, Cont) ->
          Doc = Cont(Node, Ctxt),
          prettypr:beside(prettypr:text("<b>"),
                          prettypr:beside(Doc,
                                          prettypr:text("</b>")))
      end
```

will place the text of any annotated node (regardless of the annotation data)
between HTML "boldface begin" and "boldface end" tags.

_See also: _`m:erl_syntax`, `best/2`, `format/1`, `get_ctxt_user/1`, `layout/2`,
`set_ctxt_user/2`.
""".
-spec format(syntaxTree(), [term()]) -> string().

format(Node, Options) ->
    W = proplists:get_value(paper, Options, ?PAPER),
    L = proplists:get_value(ribbon, Options, ?RIBBON),
    prettypr:format(layout(Node, Options), W, L).


-doc #{equiv => best(Tree, [])}.
-spec best(syntaxTree()) -> 'empty' | prettypr:document().

best(Node) ->
    best(Node, []).


-doc """
Creates a fixed "best" abstract layout for a syntax tree.

This is similar to the [`layout/2`](`layout/2`) function, except that
here, the final layout has been selected with respect to the given
options. The atom `empty` is returned if no such layout could be
produced. For information on the options, see the
[`format/2`](`format/2`) function.

_See also: _`best/1`, `format/2`, `layout/2`, `prettypr:best/3`.
""".
-spec best(syntaxTree(), [term()]) -> 'empty' | prettypr:document().

best(Node, Options) ->
    W = proplists:get_value(paper, Options, ?PAPER),
    L = proplists:get_value(ribbon, Options, ?RIBBON),
    prettypr:best(layout(Node, Options), W, L).


-doc #{equiv => layout(Tree, [])}.
-spec layout(syntaxTree()) -> prettypr:document().

layout(Node) ->
    layout(Node, []).


-doc """
Creates an abstract document layout for a syntax tree.

The result represents a set of possible layouts (see module
`m:prettypr`). For information on the options, see `format/2`;
however, note that the `paper` and `ribbon` options are ignored by
this function.

This function provides a low-level interface to the pretty printer, returning a
flexible representation of possible layouts, independent of the paper width
eventually to be used for formatting. This can be included as part of another
document and/or further processed directly by the functions in the `prettypr`
module, or used in a hook function (see [`format/2`](`format/2`) for details).

_See also: _`m:prettypr`, `format/2`, `layout/1`.
""".
-spec layout(syntaxTree(), [term()]) -> prettypr:document().

layout(Node, Options) ->
    lay(Node,
	#ctxt{hook = proplists:get_value(hook, Options, ?NOHOOK),
	      paper = proplists:get_value(paper, Options, ?PAPER),
	      ribbon = proplists:get_value(ribbon, Options, ?RIBBON),
	      user = proplists:get_value(user, Options),
              encoding = proplists:get_value(encoding, Options,
                                             epp:default_encoding()),
              empty_lines = proplists:get_value(empty_lines, Options, sets:new())}).

lay(Node, Ctxt) ->
    case erl_syntax:get_ann(Node) of
	[] ->
	    %% Hooks are not called if there are no annotations.
	    lay_1(Node, Ctxt);
	_As ->
	    case Ctxt#ctxt.hook of
		?NOHOOK ->
		    lay_1(Node, Ctxt);
		Hook ->
		    Hook(Node, Ctxt, fun lay_1/2)
	    end
    end.

%% This handles attached comments:

lay_1(Node, Ctxt) ->
    case erl_syntax:has_comments(Node) of
	true ->
	    D1 = lay_2(Node, Ctxt),
	    D2 = lay_postcomments(erl_syntax:get_postcomments(Node),
				  D1),
	    lay_precomments(erl_syntax:get_precomments(Node), D2);
	false ->
	    lay_2(Node, Ctxt)
    end.

%% For pre-comments, all padding is ignored.

lay_precomments([], D) ->
    D;
lay_precomments(Cs, D) ->
    above(floating(break(stack_comments(Cs, false)), -1, -1), D).

%% For postcomments, individual padding is added.

lay_postcomments([], D) ->
    D;
lay_postcomments(Cs, D) ->
    beside(D, floating(break(stack_comments(Cs, true)), 1, 0)).

%% Format (including padding, if `Pad' is `true', otherwise not)
%% and stack the listed comments above each other.

stack_comments([C | Cs], Pad) ->
    D = stack_comment_lines(erl_syntax:comment_text(C)),
    D1 = case Pad of
	     true ->
		 P = case erl_syntax:comment_padding(C) of
			 none ->
			     ?PADDING;
			 P1 ->
			     P1
		     end,
		 beside(text(spaces(P)), D);
	     false ->
		 D
	 end,
    case Cs of
	[] ->
	    D1;	   % done
	_ ->
	    above(D1, stack_comments(Cs, Pad))
    end.

%% Stack lines of text above each other and prefix each string in
%% the list with a single `%' character.

stack_comment_lines([S | Ss]) ->
    D = text(add_comment_prefix(S)),
    case Ss of
	[] ->
	    D;
	_ ->
	    above(D, stack_comment_lines(Ss))
    end;
stack_comment_lines([]) ->
    empty().

add_comment_prefix(S) ->
    [$% | S].

%% This part ignores annotations and comments:

lay_2(Node, Ctxt) ->
    case erl_syntax:type(Node) of
	%% We list literals and other common cases first.

	variable ->
	    text(erl_syntax:variable_literal(Node));

	atom ->
	    text(erl_syntax:atom_literal(Node, Ctxt#ctxt.encoding));

	integer ->
	    text(erl_syntax:integer_literal(Node));

	float ->
	    text(tidy_float(erl_syntax:float_literal(Node)));

	char ->
	    text(erl_syntax:char_literal(Node, Ctxt#ctxt.encoding));

	string ->
	    lay_string(erl_syntax:string_literal(Node, Ctxt#ctxt.encoding), Ctxt);

	nil ->
	    text("[]");

	tuple ->
	    Es = seq(erl_syntax:tuple_elements(Node),
		     floating(text(",")), reset_prec(Ctxt),
		     fun lay/2),
	    beside(floating(text("{")),
		   beside(sep(Es),
			  floating(text("}"))));

	list ->
	    Ctxt1 = reset_prec(Ctxt),
	    Node1 = erl_syntax:compact_list(Node),
	    D1 = sep(seq(erl_syntax:list_prefix(Node1),
			 floating(text(",")), Ctxt1,
			 fun lay/2)),
	    D = case erl_syntax:list_suffix(Node1) of
		    none ->
			beside(D1, floating(text("]")));
		    S ->
			follow(D1,
			       beside(
				 floating(text("| ")),
				 beside(lay(S, Ctxt1),
					floating(text("]")))))
		end,
	    beside(floating(text("[")), D);

	operator ->
	    floating(text(erl_syntax:operator_literal(Node)));

	infix_expr ->
	    Operator = erl_syntax:infix_expr_operator(Node),
	    {PrecL, Prec, PrecR} =
		case erl_syntax:type(Operator) of
		    operator ->
			inop_prec(
			  erl_syntax:operator_name(Operator));
		    _ ->
			{0, 0, 0}
		end,
	    D1 = lay(erl_syntax:infix_expr_left(Node),
		     set_prec(Ctxt, PrecL)),
	    D2 = lay(Operator, reset_prec(Ctxt)),
	    D3 = lay(erl_syntax:infix_expr_right(Node),
		     set_prec(Ctxt, PrecR)),
	    D4 = par([D1, D2, D3], Ctxt#ctxt.break_indent),
	    maybe_parentheses(D4, Prec, Ctxt);

	prefix_expr ->
	    Operator = erl_syntax:prefix_expr_operator(Node),
	    {{Prec, PrecR}, Name} =
		case erl_syntax:type(Operator) of
		    operator ->
			N = erl_syntax:operator_name(Operator),
			{preop_prec(N), N};
		    _ ->
			{{0, 0}, any}
		end,
	    D1 = lay(Operator, reset_prec(Ctxt)),
	    D2 = lay(erl_syntax:prefix_expr_argument(Node),
		     set_prec(Ctxt, PrecR)),
	    D3 = case Name of
		     '+' ->
			 beside(D1, D2);
		     '-' ->
			 beside(D1, D2);
		     _ ->
			 par([D1, D2], Ctxt#ctxt.break_indent)
		 end,
	    maybe_parentheses(D3, Prec, Ctxt);

	application ->
	    {PrecL, Prec} = func_prec(),
	    D = lay(erl_syntax:application_operator(Node),
		    set_prec(Ctxt, PrecL)),
	    As = seq(erl_syntax:application_arguments(Node),
		     floating(text(",")), reset_prec(Ctxt),
		     fun lay/2),
	    D1 = beside(D, beside(text("("),
				  beside(sep(As),
					 floating(text(")"))))),
	    maybe_parentheses(D1, Prec, Ctxt);

	match_expr ->
	    {PrecL, Prec, PrecR} = inop_prec('='),
	    D1 = lay(erl_syntax:match_expr_pattern(Node),
		     set_prec(Ctxt, PrecL)),
	    D2 = lay(erl_syntax:match_expr_body(Node),
		     set_prec(Ctxt, PrecR)),
	    D3 = follow(beside(D1, floating(text(" ="))), D2,
			Ctxt#ctxt.break_indent),
	    maybe_parentheses(D3, Prec, Ctxt);

	underscore ->
	    text("_");

	clause ->
	    %% The style used for a clause depends on its context
	    Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
	    D1 = par(seq(erl_syntax:clause_patterns(Node),
			 floating(text(",")), Ctxt1,
			 fun lay/2)),
	    D2 = case erl_syntax:clause_guard(Node) of
		     none ->
			 none;
		     G ->
			 lay(G, Ctxt1)
		 end,
	    D3 = lay_clause_expressions(erl_syntax:clause_body(Node), Ctxt1),
	    case Ctxt#ctxt.clause of
		fun_expr ->
		    make_fun_clause(D1, D2, D3, Ctxt);
		{function, N} ->
		    make_fun_clause(N, D1, D2, D3, Ctxt);
		if_expr ->
		    make_if_clause(D1, D2, D3, Ctxt);
		case_expr ->
		    make_case_clause(D1, D2, D3, Ctxt);
		maybe_expr ->
		    make_case_clause(D1, D2, D3, Ctxt);
		receive_expr ->
		    make_case_clause(D1, D2, D3, Ctxt);
		try_expr ->
		    make_case_clause(D1, D2, D3, Ctxt);
		undefined ->
		    %% If a clause is formatted out of context, we
		    %% use a "fun-expression" clause style.
		    make_fun_clause(D1, D2, D3, Ctxt)
	    end;

	function ->
	    %% Comments on the name itself will be repeated for each
	    %% clause, but that seems to be the best way to handle it.
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:function_name(Node), Ctxt1),
	    D2 = lay_clauses(erl_syntax:function_clauses(Node),
			     {function, D1}, Ctxt1),
	    beside(D2, floating(text(".")));

	case_expr ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:case_expr_argument(Node), Ctxt1),
	    D2 = lay_clauses(erl_syntax:case_expr_clauses(Node),
			     case_expr, Ctxt1),
	    sep([par([follow(text("case"), D1, Ctxt1#ctxt.break_indent),
		      text("of")],
		     Ctxt1#ctxt.break_indent),
		 nest(Ctxt1#ctxt.break_indent, D2),
		 text("end")]);

	if_expr ->
	    Ctxt1 = reset_prec(Ctxt),
	    D = lay_clauses(erl_syntax:if_expr_clauses(Node),
			    if_expr, Ctxt1),
	    sep([follow(text("if"), D, Ctxt1#ctxt.break_indent),
		 text("end")]);

	fun_expr ->
	    Ctxt1 = reset_prec(Ctxt),
	    D = lay_clauses(erl_syntax:fun_expr_clauses(Node),
			    fun_expr, Ctxt1),
	    sep([follow(text("fun"), D, Ctxt1#ctxt.break_indent),
		 text("end")]);

        named_fun_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:named_fun_expr_name(Node), Ctxt1),
            D = lay_clauses(erl_syntax:named_fun_expr_clauses(Node),
                            {function,D1}, Ctxt1),
            sep([follow(text("fun"), D, Ctxt1#ctxt.break_indent),
                 text("end")]);

	module_qualifier ->
	    {PrecL, _Prec, PrecR} = inop_prec(':'),
	    D1 = lay(erl_syntax:module_qualifier_argument(Node),
		     set_prec(Ctxt, PrecL)),
	    D2 = lay(erl_syntax:module_qualifier_body(Node),
		     set_prec(Ctxt, PrecR)),
	    beside(D1, beside(text(":"), D2));

        maybe_expr ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = vertical(seq(erl_syntax:maybe_expr_body(Node),
                              floating(text(",")), Ctxt1, fun lay/2)),
            Es0 = [text("end")],
            Es1 = case erl_syntax:maybe_expr_else(Node) of
                      none -> Es0;
                      ElseNode ->
                          ElseCs = erl_syntax:else_expr_clauses(ElseNode),
                          D3 = lay_clauses(ElseCs, maybe_expr, Ctxt1),
                          [text("else"),
                           nest(Ctxt1#ctxt.break_indent, D3)
                          | Es0]
                  end,
	    sep([par([text("maybe"), nest(Ctxt1#ctxt.break_indent, D1),
		      hd(Es1)]) | tl(Es1)]);

	maybe_match_expr ->
	    {PrecL, Prec, PrecR} = inop_prec('='),
	    D1 = lay(erl_syntax:maybe_match_expr_pattern(Node),
		     set_prec(Ctxt, PrecL)),
	    D2 = lay(erl_syntax:maybe_match_expr_body(Node),
		     set_prec(Ctxt, PrecR)),
	    D3 = follow(beside(D1, floating(text(" ?="))), D2,
			Ctxt#ctxt.break_indent),
	    maybe_parentheses(D3, Prec, Ctxt);


	%%
	%% The rest is in alphabetical order (except map and types)
	%%

	arity_qualifier ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:arity_qualifier_body(Node), Ctxt1),
	    D2 = lay(erl_syntax:arity_qualifier_argument(Node), Ctxt1),
	    beside(D1, beside(text("/"), D2));

	attribute ->
	    %% The attribute name and arguments are formatted similar to
	    %% a function call, but prefixed with a "-" and followed by
	    %% a period. If the arguments is `none', we only output the
	    %% attribute name, without following parentheses.
	    Ctxt1 = reset_prec(Ctxt),
            Args = erl_syntax:attribute_arguments(Node),
            N = case erl_syntax:attribute_name(Node) of
                    {atom, _, 'if'} ->
                        erl_syntax:variable('if');
                    N0 ->
                        N0
                end,
            D = case attribute_type(Node) of
                    spec ->
                        [SpecTuple] = Args,
                        [FuncName, FuncTypes] =
                            erl_syntax:tuple_elements(SpecTuple),
                        Name =
                            case erl_syntax:type(FuncName) of
                                tuple ->
                                    case erl_syntax:tuple_elements(FuncName) of
                                        [F0, _] ->
                                            F0;
                                        [M0, F0, _] ->
                                            erl_syntax:module_qualifier(M0,
                                                                        F0);
                                        _ ->
                                            FuncName
                                    end;
                                _ ->
                                    FuncName
                            end,
                        Types = dodge_macros(FuncTypes),
                        D1 = lay_clauses(erl_syntax:concrete(Types),
                                         spec, Ctxt1),
                        beside(follow(lay(N, Ctxt1),
                                      lay(Name, Ctxt1),
                                      Ctxt1#ctxt.break_indent),
                               D1);
                    type ->
                        [TypeTuple] = Args,
                        [Name, Type0, Elements] =
                            erl_syntax:tuple_elements(TypeTuple),
                        TypeName = dodge_macros(Name),
                        Type = dodge_macros(Type0),
                        As0 = dodge_macros(Elements),
                        As = erl_syntax:concrete(As0),
                        D1 = lay_type_application(TypeName, As, Ctxt1),
                        D2 = lay(erl_syntax:concrete(Type), Ctxt1),
                        beside(follow(lay(N, Ctxt1),
                                      beside(D1, floating(text(" :: "))),
                                      Ctxt1#ctxt.break_indent),
                               D2);
                    Tag when Tag =:= export_type;
                             Tag =:= optional_callbacks ->
                        [FuncNs] = Args,
                        FuncNames = erl_syntax:concrete(dodge_macros(FuncNs)),
                        As = unfold_function_names(FuncNames),
                        beside(lay(N, Ctxt1),
                               beside(text("("),
                                      beside(lay(As, Ctxt1),
                                             floating(text(")")))));
                    _ when Args =:= none ->
			lay(N, Ctxt1);
                    _ ->
                        D1 = sep(seq(Args, text(","), Ctxt1,
                                     fun lay/2)),
			beside(lay(N, Ctxt1),
			       beside(text("("),
				      beside(D1, floating(text(")")))))
                end,
	    beside(floating(text("-")), beside(D, floating(text("."))));

	binary ->
	    Ctxt1 = reset_prec(Ctxt),
	    Es = seq(erl_syntax:binary_fields(Node),
		     floating(text(",")), Ctxt1, fun lay/2),
	    beside(floating(text("<<")),
		   beside(par(Es), floating(text(">>"))));

	binary_field ->
	    Ctxt1 = set_prec(Ctxt, max_prec()),
	    D1 = lay(erl_syntax:binary_field_body(Node), Ctxt1),
	    D2 = case erl_syntax:binary_field_types(Node) of
		     [] ->
			 empty();
		     Ts ->
			 beside(floating(text("/")),
				lay_bit_types(Ts, Ctxt1))
		 end,
	    beside(D1, D2);

	block_expr ->
	    Ctxt1 = reset_prec(Ctxt),
	    Es = seq(erl_syntax:block_expr_body(Node),
		     floating(text(",")), Ctxt1, fun lay/2),
	    sep([text("begin"),
		 nest(Ctxt1#ctxt.break_indent, sep(Es)),
		 text("end")]);

	'catch_expr' ->                         %Quoted to help Emacs.
	    {Prec, PrecR} = preop_prec('catch'),
	    D = lay(erl_syntax:catch_expr_body(Node),
		    set_prec(Ctxt, PrecR)),
	    D1 = follow(text("catch"), D, Ctxt#ctxt.break_indent),
	    maybe_parentheses(D1, Prec, Ctxt);

	class_qualifier ->
	    Ctxt1 = set_prec(Ctxt, max_prec()),
	    D1 = lay(erl_syntax:class_qualifier_argument(Node), Ctxt1),
            D2 = lay(erl_syntax:class_qualifier_body(Node), Ctxt1),
            Stacktrace = erl_syntax:class_qualifier_stacktrace(Node),
            case erl_syntax:variable_name(Stacktrace) of
                '_' ->
                    beside(D1, beside(text(":"), D2));
                _ ->
                    D3 = lay(Stacktrace, Ctxt1),
                    beside(D1, beside(beside(text(":"), D2),
                                      beside(text(":"), D3)))
            end;
	comment ->
	    D = stack_comment_lines(
		  erl_syntax:comment_text(Node)),
	    %% Default padding for standalone comments is empty.
	    case erl_syntax:comment_padding(Node) of
		none ->
		    floating(break(D));
		P ->
		    floating(break(beside(text(spaces(P)), D)))
	    end;

	conjunction ->
	    par(seq(erl_syntax:conjunction_body(Node),
		    floating(text(",")), reset_prec(Ctxt),
		    fun lay/2));

	disjunction ->
	    %% For clarity, we don't paragraph-format
	    %% disjunctions; only conjunctions (see above).
	    sep(seq(erl_syntax:disjunction_body(Node),
		    floating(text(";")), reset_prec(Ctxt),
		    fun lay/2));

	error_marker ->
	    E = erl_syntax:error_marker_info(Node),
	    beside(text("** "),
		   beside(lay_error_info(E, reset_prec(Ctxt)),
			  text(" **")));

	eof_marker ->
	    empty();

	form_list ->
	    Es = seq(erl_syntax:form_list_elements(Node), none,
		     reset_prec(Ctxt), fun lay/2),
	    vertical_sep(text(""), Es);

	generator ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:generator_pattern(Node), Ctxt1),
	    D2 = lay(erl_syntax:generator_body(Node), Ctxt1),
	    par([D1, beside(text("<- "), D2)], Ctxt1#ctxt.break_indent);

	strict_generator ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:strict_generator_pattern(Node), Ctxt1),
	    D2 = lay(erl_syntax:strict_generator_body(Node), Ctxt1),
	    par([D1, beside(text("<:- "), D2)], Ctxt1#ctxt.break_indent);

	binary_generator ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:binary_generator_pattern(Node), Ctxt1),
	    D2 = lay(erl_syntax:binary_generator_body(Node), Ctxt1),
	    par([D1, beside(text("<= "), D2)], Ctxt1#ctxt.break_indent);

	strict_binary_generator ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:strict_binary_generator_pattern(Node), Ctxt1),
	    D2 = lay(erl_syntax:strict_binary_generator_body(Node), Ctxt1),
	    par([D1, beside(text("<:= "), D2)], Ctxt1#ctxt.break_indent);

	map_generator ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:map_generator_pattern(Node), Ctxt1),
	    D2 = lay(erl_syntax:map_generator_body(Node), Ctxt1),
	    par([D1, beside(text("<- "), D2)], Ctxt1#ctxt.break_indent);

	strict_map_generator ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:strict_map_generator_pattern(Node), Ctxt1),
	    D2 = lay(erl_syntax:strict_map_generator_body(Node), Ctxt1),
	    par([D1, beside(text("<:- "), D2)], Ctxt1#ctxt.break_indent);

	zip_generator ->
	    Ctxt1 = reset_prec(Ctxt),
	    par(seq(erl_syntax:zip_generator_body(Node),
			 floating(text("&&")), Ctxt1,
			 fun lay/2));

	implicit_fun ->
	    D = lay(erl_syntax:implicit_fun_name(Node),
		    reset_prec(Ctxt)),
	    beside(floating(text("fun ")), D);

	list_comp ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:list_comp_template(Node), Ctxt1),
	    D2 = par(seq(erl_syntax:list_comp_body(Node),
			 floating(text(",")), Ctxt1,
			 fun lay/2)),
	    beside(floating(text("[")),
		   par([D1, beside(floating(text("|| ")),
				   beside(D2, floating(text("]"))))]));

	binary_comp ->
	    Ctxt1 = set_prec(Ctxt, max_prec()),
	    D1 = lay(erl_syntax:binary_comp_template(Node), Ctxt1),
	    D2 = par(seq(erl_syntax:binary_comp_body(Node),
			 floating(text(",")), Ctxt1,
			 fun lay/2)),
	    beside(floating(text("<< ")),
		   par([D1, beside(floating(text(" || ")),
				   beside(D2, floating(text(" >>"))))]));

	map_comp ->
	    Ctxt1 = set_prec(Ctxt, max_prec()),
	    D1 = lay(erl_syntax:map_comp_template(Node), Ctxt1),
	    D2 = par(seq(erl_syntax:map_comp_body(Node),
			 floating(text(",")), Ctxt1,
			 fun lay/2)),
	    beside(floating(text("#{")),
		   par([D1, beside(floating(text("|| ")),
				   beside(D2, floating(text("}"))))]));
	macro ->
	    %% This is formatted similar to a normal function call, but
	    %% prefixed with a "?".
	    Ctxt1 = reset_prec(Ctxt),
	    N = erl_syntax:macro_name(Node),
	    D = case erl_syntax:macro_arguments(Node) of
		    none->
			lay(N, Ctxt1);
		    Args ->
			As = seq(Args, floating(text(",")),
				 set_prec(Ctxt1, max_prec()), fun lay/2),
			beside(lay(N, Ctxt1),
			       beside(text("("),
				      beside(par(As),
					     floating(text(")")))))
		end,
	    D1 = beside(floating(text("?")), D),
	    maybe_parentheses(D1, 0, Ctxt);    % must be conservative!

	parentheses ->
	    D = lay(erl_syntax:parentheses_body(Node),
		    reset_prec(Ctxt)),
	    lay_parentheses(D, Ctxt);

	receive_expr ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay_clauses(erl_syntax:receive_expr_clauses(Node),
			     receive_expr, Ctxt1),
	    D2 = case erl_syntax:receive_expr_timeout(Node) of
		     none ->
			 D1;
		     T ->
			 D3 = lay(T, Ctxt1),
			 A = erl_syntax:receive_expr_action(Node),
			 D4 = sep(seq(A, floating(text(",")),
				      Ctxt1, fun lay/2)),
			 sep([D1,
			      follow(floating(text("after")),
				     append_clause_body(D4, D3,
							Ctxt1),
				     Ctxt1#ctxt.break_indent)])
		 end,
	    sep([text("receive"),
		 nest(Ctxt1#ctxt.break_indent, D2),
		 text("end")]);

	record_access ->
	    {PrecL, Prec, PrecR} = inop_prec('#'),
	    D1 = lay(erl_syntax:record_access_argument(Node),
		     set_prec(Ctxt, PrecL)),
	    D2 = beside(
		   floating(text(".")),
		   lay(erl_syntax:record_access_field(Node),
		       set_prec(Ctxt, PrecR))),
	    T = erl_syntax:record_access_type(Node),
	    D3 = beside(beside(floating(text("#")),
                               lay(T, reset_prec(Ctxt))),
                        D2),
	    maybe_parentheses(beside(D1, D3), Prec, Ctxt);

	record_expr ->
	    {PrecL, Prec, _} = inop_prec('#'),
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:record_expr_type(Node), Ctxt1),
	    D2 = par(seq(erl_syntax:record_expr_fields(Node),
			 floating(text(",")), Ctxt1,
			 fun lay/2)),
	    D3 = beside(beside(floating(text("#")), D1),
			beside(text("{"),
			       beside(D2, floating(text("}"))))),
	    D4 = case erl_syntax:record_expr_argument(Node) of
		     none ->
			 D3;
		     A ->
			 beside(lay(A, set_prec(Ctxt, PrecL)), D3)
		 end,
	    maybe_parentheses(D4, Prec, Ctxt);

	record_field ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:record_field_name(Node), Ctxt1),
	    case erl_syntax:record_field_value(Node) of
		none ->
		    D1;
		V ->
		    par([D1, floating(text("=")), lay(V, Ctxt1)],
			Ctxt1#ctxt.break_indent)
	    end;

	record_index_expr ->
	    {Prec, PrecR} = preop_prec('#'),
	    D1 = lay(erl_syntax:record_index_expr_type(Node),
		     reset_prec(Ctxt)),
	    D2 = lay(erl_syntax:record_index_expr_field(Node),
		     set_prec(Ctxt, PrecR)),
	    D3 = beside(beside(floating(text("#")), D1),
			beside(floating(text(".")), D2)),
	    maybe_parentheses(D3, Prec, Ctxt);

        map_expr ->
            {PrecL, Prec, _} = inop_prec('#'),
            Ctxt1 = reset_prec(Ctxt),
            D1 = par(seq(erl_syntax:map_expr_fields(Node),
                         floating(text(",")), Ctxt1, fun lay/2)),
            D2 = beside(text("#{"), beside(D1, floating(text("}")))),
            D3 = case erl_syntax:map_expr_argument(Node) of
                         none ->
                             D2;
                         A ->
                             beside(lay(A, set_prec(Ctxt, PrecL)), D2)
                 end,
            maybe_parentheses(D3, Prec, Ctxt);

        map_field_assoc ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:map_field_assoc_name(Node), Ctxt1),
            D2 = lay(erl_syntax:map_field_assoc_value(Node), Ctxt1),
            par([D1, floating(text("=>")), D2], Ctxt1#ctxt.break_indent);

        map_field_exact ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:map_field_exact_name(Node), Ctxt1),
            D2 = lay(erl_syntax:map_field_exact_value(Node), Ctxt1),
            par([D1, floating(text(":=")), D2], Ctxt1#ctxt.break_indent);

	size_qualifier ->
	    Ctxt1 = set_prec(Ctxt, max_prec()),
	    D1 = lay(erl_syntax:size_qualifier_body(Node), Ctxt1),
	    D2 = lay(erl_syntax:size_qualifier_argument(Node), Ctxt1),
	    beside(D1, beside(text(":"), D2));

	text ->
	    text(erl_syntax:text_string(Node));

        typed_record_field ->
            {_, Prec, _} = type_inop_prec('::'),
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:typed_record_field_body(Node), Ctxt1),
            D2 = lay(erl_syntax:typed_record_field_type(Node),
                     set_prec(Ctxt, Prec)),
            D3 = par([D1, floating(text("::")), D2],
                     Ctxt1#ctxt.break_indent),
            maybe_parentheses(D3, Prec, Ctxt);

	try_expr ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = sep(seq(erl_syntax:try_expr_body(Node),
			 floating(text(",")), Ctxt1, fun lay/2)),
	    Es0 = [text("end")],
	    Es1 = case erl_syntax:try_expr_after(Node) of
		      [] -> Es0;
		      As ->
			  D2 = sep(seq(As, floating(text(",")), Ctxt1,
				       fun lay/2)),
			  [text("after"),
			   nest(Ctxt1#ctxt.break_indent, D2)
			   | Es0]
		  end,
	    Es2 = case erl_syntax:try_expr_handlers(Node) of
		      [] -> Es1;
		      Hs ->
			  D3 = lay_clauses(Hs, try_expr, Ctxt1),
			  [text("catch"),
			   nest(Ctxt1#ctxt.break_indent, D3)
			   | Es1]
		  end,
	    Es3 = case erl_syntax:try_expr_clauses(Node) of
		      [] -> Es2;
		      Cs ->
			  D4 = lay_clauses(Cs, try_expr, Ctxt1),
			  [text("of"),
			   nest(Ctxt1#ctxt.break_indent, D4)
			   | Es2]
		  end,
	    sep([par([follow(text("try"), D1, Ctxt1#ctxt.break_indent),
		      hd(Es3)])
		 | tl(Es3)]);

	warning_marker ->
	    E = erl_syntax:warning_marker_info(Node),
	    beside(text("%% WARNING: "),
		   lay_error_info(E, reset_prec(Ctxt)));

        %%
        %% Types
        %%

        annotated_type ->
            {_, Prec, _} = type_inop_prec('::'),
            D1 = lay(erl_syntax:annotated_type_name(Node),
                     reset_prec(Ctxt)),
            D2 = lay(erl_syntax:annotated_type_body(Node),
                     set_prec(Ctxt, Prec)),
            D3 = follow(beside(D1, floating(text(" ::"))), D2,
                               Ctxt#ctxt.break_indent),
            maybe_parentheses(D3, Prec, Ctxt);

        type_application ->
            Name = erl_syntax:type_application_name(Node),
            Arguments = erl_syntax:type_application_arguments(Node),
            %% Prefer shorthand notation.
            case erl_syntax_lib:analyze_type_application(Node) of
                {nil, 0} ->
                    text("[]");
                {list, 1} ->
                    [A] = Arguments,
                    D1 = lay(A, reset_prec(Ctxt)),
                    beside(text("["), beside(D1, text("]")));
                {nonempty_list, 1} ->
                    [A] = Arguments,
                    D1 = lay(A, reset_prec(Ctxt)),
                    beside(text("["), beside(D1, text(", ...]")));
                _ ->
                    lay_type_application(Name, Arguments, Ctxt)
            end;

        bitstring_type ->
                Ctxt1 = set_prec(Ctxt, max_prec()),
            M = erl_syntax:bitstring_type_m(Node),
            N = erl_syntax:bitstring_type_n(Node),
            D1 = [beside(text("_:"), lay(M, Ctxt1)) ||
                     (erl_syntax:type(M) =/= integer orelse
                      erl_syntax:integer_value(M) =/= 0)],
            D2 = [beside(text("_:_*"), lay(N, Ctxt1)) ||
                     (erl_syntax:type(N) =/= integer orelse
                      erl_syntax:integer_value(N) =/= 0)],
            F = fun(D, _) -> D end,
            D = seq(D1 ++ D2, floating(text(",")), Ctxt1, F),
            beside(floating(text("<<")),
                   beside(par(D), floating(text(">>"))));

        fun_type ->
            text("fun()");

        constrained_function_type ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:constrained_function_type_body(Node),
                     Ctxt1),
            Ctxt2 = Ctxt1#ctxt{clause = undefined},
            D2 = lay(erl_syntax:constrained_function_type_argument(Node),
                     Ctxt2),
            beside(D1,
                   beside(floating(text(" when ")), D2));

        function_type ->
            {Before, After} = case Ctxt#ctxt.clause of
                                  spec ->
                                      {"", ""};
                                  _ ->
                                      {"fun(", ")"}
                              end,
            Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
            D1 = case erl_syntax:function_type_arguments(Node) of
                     any_arity ->
                         text("(...)");
                     Arguments ->
                         As = seq(Arguments,
                                  floating(text(",")), Ctxt1,
                                  fun lay/2),
                         beside(text("("),
                                beside(par(As),
                                       floating(text(")"))))
                 end,
            D2 = lay(erl_syntax:function_type_return(Node), Ctxt1),
            beside(floating(text(Before)),
                   beside(D1,
                          beside(floating(text(" -> ")),
                                 beside(D2, floating(text(After))))));

        constraint ->
            Name = erl_syntax:constraint_argument(Node),
            Args = erl_syntax:constraint_body(Node),
            case is_subtype(Name, Args) of
                true ->
                    [Var, Type] = Args,
                    {PrecL, Prec, PrecR} = type_inop_prec('::'),
                    D1 = lay(Var, set_prec(Ctxt, PrecL)),
                    D2 = lay(Type, set_prec(Ctxt, PrecR)),
                    D3 = follow(beside(D1, floating(text(" ::"))), D2,
                                Ctxt#ctxt.break_indent),
                    maybe_parentheses(D3, Prec, Ctxt);
                false ->
                    lay_type_application(Name, Args, Ctxt)
            end;

        map_type ->
            case erl_syntax:map_type_fields(Node) of
                any_size ->
                    text("map()");
                Fs ->
                    Ctxt1 = reset_prec(Ctxt),
                    Es = seq(Fs,
                             floating(text(",")), Ctxt1,
                             fun lay/2),
                    D = beside(floating(text("#{")),
                               beside(par(Es),
                                      floating(text("}")))),
                    {Prec, _PrecR} = type_preop_prec('#'),
                    maybe_parentheses(D, Prec, Ctxt)
            end;

        map_type_assoc ->
            Name = erl_syntax:map_type_assoc_name(Node),
            Value = erl_syntax:map_type_assoc_value(Node),
            lay_type_assoc(Name, Value, Ctxt);

        map_type_exact ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:map_type_exact_name(Node), Ctxt1),
            D2 = lay(erl_syntax:map_type_exact_value(Node), Ctxt1),
            par([D1, floating(text(":=")), D2], Ctxt1#ctxt.break_indent);

        integer_range_type ->
            {PrecL, Prec, PrecR} = type_inop_prec('..'),
            D1 = lay(erl_syntax:integer_range_type_low(Node),
                     set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:integer_range_type_high(Node),
                     set_prec(Ctxt, PrecR)),
            D3 = beside(D1, beside(text(".."), D2)),
            maybe_parentheses(D3, Prec, Ctxt);

        record_type ->
            {Prec, _PrecR} = type_preop_prec('#'),
            D1 = beside(text("#"),
                        lay(erl_syntax:record_type_name(Node),
                            reset_prec(Ctxt))),
            Es = seq(erl_syntax:record_type_fields(Node),
                     floating(text(",")), reset_prec(Ctxt),
                     fun lay/2),
            D2 = beside(D1,
                        beside(text("{"),
                               beside(par(Es),
                                      floating(text("}"))))),
            maybe_parentheses(D2, Prec, Ctxt);

	record_type_field ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:record_type_field_name(Node), Ctxt1),
            D2 = lay(erl_syntax:record_type_field_type(Node), Ctxt1),
            par([D1, floating(text("::")), D2], Ctxt1#ctxt.break_indent);

        tuple_type ->
            case erl_syntax:tuple_type_elements(Node) of
                any_size ->
                    text("tuple()");
                Elements ->
                    Es = seq(Elements,
                             floating(text(",")), reset_prec(Ctxt),
                             fun lay/2),
                    beside(floating(text("{")),
                           beside(sep(Es), floating(text("}"))))
            end;

        type_union ->
            {_, Prec, PrecR} = type_inop_prec('|'),
            Es = sep(seq(erl_syntax:type_union_types(Node),
                         floating(text(" |")), set_prec(Ctxt, PrecR),
                         fun lay/2)),
            maybe_parentheses(Es, Prec, Ctxt);

        user_type_application ->
            lay_type_application(erl_syntax:user_type_application_name(Node),
                                 erl_syntax:user_type_application_arguments(Node),
                                 Ctxt)

    end.

attribute_type(Node) ->
    N = erl_syntax:attribute_name(Node),
    case catch erl_syntax:concrete(N) of
        opaque ->
            type;
        spec ->
            spec;
        callback ->
            spec;
        type ->
            type;
        export_type ->
            export_type;
        optional_callbacks ->
            optional_callbacks;
        _ ->
            N
    end.

is_subtype(Name, [Var, _]) ->
    (erl_syntax:is_atom(Name, is_subtype) andalso
     erl_syntax:type(Var) =:= variable);
is_subtype(_, _) -> false.

unfold_function_names(Ns) ->
    F = fun ({Atom, Arity}) ->
		erl_syntax:arity_qualifier(erl_syntax:atom(Atom),
                                           erl_syntax:integer(Arity))
	end,
    erl_syntax:list([F(N) || N <- Ns]).

%% Macros are not handled well.
dodge_macros(Type) ->
    F = fun (T) ->
                case erl_syntax:type(T) of
                    macro ->
                        Var = erl_syntax:macro_name(T),
                        VarName0 = erl_syntax:variable_name(Var),
                        VarName = list_to_atom("?"++atom_to_list(VarName0)),
                        Atom = erl_syntax:atom(VarName),
                        Atom;
                    _ -> T
                end
        end,
    erl_syntax_lib:map(F, Type).

lay_parentheses(D, _Ctxt) ->
    beside(floating(text("(")), beside(D, floating(text(")")))).

maybe_parentheses(D, Prec, Ctxt) ->
    case Ctxt#ctxt.prec of
	P when P > Prec ->
	    lay_parentheses(D, Ctxt);
	_ ->
	    D
    end.

lay_string(S, Ctxt) ->
    %% S includes leading/trailing double-quote characters. The segment
    %% width is 2/3 of the ribbon width - this seems to work well.
    W = (Ctxt#ctxt.ribbon * 2) div 3,
    lay_string_1(S, length(S), W).

lay_string_1(S, L, W) when L > W, W > 0 ->
    %% Note that L is the minimum, not the exact, printed length.
    case split_string(S, W - 1, L) of
	{_S1, ""} ->
	    text(S);
	{S1, S2} ->
	    above(text(S1 ++ "\""),
		  lay_string_1([$" | S2], L - W + 1, W))  %" stupid emacs
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
split_string_2([$x, ${ | Xs], N, L, As) ->
    split_string_3(Xs, N - 2, L - 2, [${, $x | As]);
split_string_2([X1, X2, X3 | Xs], N, L, As) when
  X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7, X3 >= $0, X3 =< $7 ->
    split_string_1(Xs, N - 3, L - 3, [X3, X2, X1 | As]);
split_string_2([X1, X2 | Xs], N, L, As) when
  X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7 ->
    split_string_1(Xs, N - 2, L - 2, [X2, X1 | As]);
split_string_2([X | Xs], N, L, As) ->
    split_string_1(Xs, N - 1, L - 1, [X | As]).

split_string_3([$} | Xs], N, L, As) ->
    split_string_1(Xs, N - 1, L - 1, [$} | As]);
split_string_3([X | Xs], N, L, As) when
  X >= $0, X =< $9; X >= $a, X =< $z; X >= $A, X =< $Z ->
    split_string_3(Xs, N - 1, L -1, [X | As]);
split_string_3([X | Xs], N, L, As) when
  X >= $0, X =< $9 ->
    split_string_1(Xs, N - 1, L -1, [X | As]).

%% Note that there is nothing in `lay_clauses' that actually requires
%% that the elements have type `clause'; it just sets up the proper
%% context and arranges the elements suitably for clauses.

lay_clauses(Cs, Type, Ctxt) ->
    vertical(seq(Cs, floating(text(";")),
		 Ctxt#ctxt{clause = Type},
		 fun lay/2)).

%% Note that for the clause-making functions, the guard argument
%% can be `none', which has different interpretations in different
%% contexts.

make_fun_clause(P, G, B, Ctxt) ->
    make_fun_clause(none, P, G, B, Ctxt).

make_fun_clause(N, P, G, B, Ctxt) ->
    D = make_fun_clause_head(N, P, Ctxt),
    make_case_clause(D, G, B, Ctxt).

make_fun_clause_head(N, P, Ctxt) ->
    D = lay_parentheses(P, Ctxt),
    if N =:= none ->
	    D;
       true ->
	    beside(N, D)
    end.

make_case_clause(P, G, B, Ctxt) ->
    append_clause_body(B, append_guard(G, P, Ctxt), Ctxt).

make_if_clause(_P, G, B, Ctxt) ->
    %% We ignore the patterns; they should be empty anyway.
    G1 = if G =:= none ->
		 text("true");
	    true ->
		 G
	 end,
    append_clause_body(B, G1, Ctxt).

append_clause_body(B, D, Ctxt) ->
    append_clause_body(B, D, floating(text(" ->")), Ctxt).

append_clause_body(B, D, S, Ctxt) ->
    sep([beside(D, S), nest(Ctxt#ctxt.break_indent, B)]).

append_guard(none, D, _) ->
    D;
append_guard(G, D, Ctxt) ->
    par([D, follow(text("when"), G, Ctxt#ctxt.sub_indent)],
	Ctxt#ctxt.break_indent).

lay_bit_types([T], Ctxt) ->
    lay(T, Ctxt);
lay_bit_types([T | Ts], Ctxt) ->
    beside(lay(T, Ctxt),
	   beside(floating(text("-")),
		  lay_bit_types(Ts, Ctxt))).

lay_error_info({L, M, T}=T0, Ctxt) when is_integer(L), is_atom(M) ->
    case catch M:format_error(T) of
	S when is_list(S) ->
	    if L > 0 ->
		    beside(text(io_lib:format("~w: ",[L])), text(S));
	       true ->
		    text(S)
	    end;
	_ ->
	    lay_concrete(T0, Ctxt)
    end;
lay_error_info(T, Ctxt) ->
    lay_concrete(T, Ctxt).

lay_concrete(T, Ctxt) ->
    lay(erl_syntax:abstract(T), Ctxt).

lay_type_assoc(Name, Value, Ctxt) ->
    Ctxt1 = reset_prec(Ctxt),
    D1 = lay(Name, Ctxt1),
    D2 = lay(Value, Ctxt1),
    par([D1, floating(text("=>")), D2], Ctxt1#ctxt.break_indent).

lay_type_application(Name, Arguments, Ctxt) ->
    {PrecL, Prec} = func_prec(), %
    D1 = lay(Name, set_prec(Ctxt, PrecL)),
    As = seq(Arguments,
             floating(text(",")), reset_prec(Ctxt),
             fun lay/2),
    D = beside(D1, beside(text("("),
                          beside(par(As),
                                 floating(text(")"))))),
    maybe_parentheses(D, Prec, Ctxt).

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

vertical_sep(_Sep, [D]) ->
    D;
vertical_sep(Sep, [D | Ds]) ->
    above(above(D, Sep), vertical_sep(Sep, Ds));
vertical_sep(_Sep, []) ->
    [].

spaces(N) when N > 0 ->
    [$\040 | spaces(N - 1)];
spaces(_) ->
    [].

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
tidy_float_2([_C | Cs]) -> tidy_float_2(Cs);
tidy_float_2([]) -> [].

lay_clause_expressions([H], Ctxt) ->
	lay(H, Ctxt);
lay_clause_expressions([H | T], Ctxt) ->
    Clause = beside(lay(H, Ctxt), floating(text(","))),
    Next = lay_clause_expressions(T, Ctxt),
    case is_last_and_before_empty_line(H, T, Ctxt) of
	true ->
	    above(above(Clause, text("")), Next);
        false ->
            above(Clause, Next)
    end;
lay_clause_expressions([], _) ->
    empty().

is_last_and_before_empty_line(H, [], #ctxt{empty_lines = EmptyLines}) ->
    try sets:is_element(get_line(H) + 1, EmptyLines)
    catch error:badarith -> false
    end;
is_last_and_before_empty_line(H, [H2 | _], #ctxt{empty_lines = EmptyLines}) ->
    try ((get_line(H2) - get_line(H)) >= 2) and sets:is_element(get_line(H) + 1, EmptyLines)
    catch error:badarith -> false
    end.

get_line(Tree) ->
    Anno = erl_syntax:get_pos(Tree),
    erl_anno:line(Anno).

%% =====================================================================
