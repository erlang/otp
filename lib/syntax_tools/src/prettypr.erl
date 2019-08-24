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
%% @copyright 2000-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

%% @doc A generic pretty printer library. This module uses a
%% strict-style context passing implementation of John Hughes algorithm,
%% described in "The design of a Pretty-printing Library". The
%% paragraph-style formatting, empty documents, floating documents, and
%% null strings are my own additions to the algorithm.
%%
%% To get started, you should read about the {@link document()} data
%% type; the main constructor functions: {@link text/1}, {@link
%% above/2}, {@link beside/2}, {@link nest/2}, {@link sep/1}, and {@link
%% par/2}; and the main layout function {@link format/3}.
%%
%% If you simply want to format a paragraph of plain text, you probably
%% want to use the {@link text_par/2} function, as in the following
%% example:
%% ```
%% prettypr:format(prettypr:text_par("Lorem ipsum dolor sit amet"), 20)
%% '''

%% @TODO can floats be moved in/out of sep:s without too much pain?

-module(prettypr).

-export([above/2, beside/2, best/3, break/1, empty/0, floating/1,
	 floating/3, follow/2, follow/3, format/1, format/2, format/3,
	 nest/2, par/1, par/2, sep/1, text/1, null_text/1, text_par/1,
	 text_par/2]).

-export_type([document/0]).

%% ---------------------------------------------------------------------

-type deep_string() :: [char() | deep_string()].

%% Document structures fully implemented and available to the user:
-record(text,   {s  :: deep_string()}).
-record(nest,   {n  :: integer(),  d  :: document()}).
-record(beside, {d1 :: document(), d2 :: document()}).
-record(above,  {d1 :: document(), d2 :: document()}).
-record(sep,    {ds :: [document()], i = 0 :: integer(),
                 p = false :: boolean()}).

%% Document structure which is not clear whether it is fully implemented: 
-record(float,  {d  :: document(), h :: integer(), v :: integer()}).

%% Document structures not available to the user:
-record(union,  {d1 :: document(), d2 :: document()}).
-record(fit,    {d  :: document()}).

%% ---------------------------------------------------------------------
%% A small warning for hackers: it's fairly easy to break this
%% thing (in particular, to muck up the complexity) if you don't
%% understand how it works.
%% ---------------------------------------------------------------------


%% =====================================================================
%% @type document(). An abstract character-based "document" representing
%% a number of possible layouts, which can be processed to produce a
%% single concrete layout. A concrete layout can then be rendered as a
%% sequence of characters containing linebreaks, which can be passed to
%% a printer or terminal that uses a fixed-width font.
%%
%% For example, a document `sep([text("foo"), text("bar")])' 
%% represents the two layouts
%% ```foo bar'''
%% and
%% ```foo
%%    bar'''
%%
%% Which layout is chosen depends on the available horizontal space.
%% When processing a document, the main parameters are the <em>paper
%% width</em> and the <em>line width</em> (also known as the "ribbon
%% width"). In the resulting layout, no text should be printed beyond
%% the paper width (which by default is 80 characters) as long as it can
%% be avoided, and each single line of text (its indentation not
%% counted, hence "ribbon") should preferably be no wider than the
%% specified line width (which by default is 65).
%%
%% Documents can be joined into a single new document using the
%% constructor functions of this module. Note that the new document
%% often represents a larger number of possible layouts than just the
%% sum of the components.

-type document() :: 'null' | #text{} | #nest{} | #beside{}
                  | #above{} | #sep{} | #float{} | #union{} | #fit{}.

%% =====================================================================
%% @spec text(Characters::string()) -> document()
%%
%% @doc Yields a document representing a fixed, unbreakable sequence of
%% characters. The string should contain only <em>printable</em>
%% characters (tabs allowed but not recommended), and <em>not</em>
%% newline, line feed, vertical tab, etc. A tab character (`\t') is
%% interpreted as padding of 1-8 space characters to the next column of
%% 8 characters <em>within the string</em>.
%%
%% @see empty/0
%% @see null_text/1
%% @see text_par/2

-spec text(string()) -> #text{}.

text(S) ->
    mktext(string(S)).	  % convert to internal representation

%% This function is used internally only, and expects a string on
%% the internal representation:

mktext(S) ->
    #text{s = S}.


%% =====================================================================
%% @spec null_text(Characters::string()) -> document()
%%
%% @doc Similar to {@link text/1}, but the result is treated as having
%% zero width. This is regardless of the actual length of the string.
%% Null text is typically used for markup, which is supposed to have no
%% effect on the actual layout.
%%
%% The standard example is when formatting source code as HTML to be
%% placed within `<pre>...</pre>' markup, and using e.g. `<i>' and `<b>'
%% to make parts of the source code stand out. In this case, the markup
%% does not add to the width of the text when viewed in an HTML browser,
%% so the layout engine should simply pretend that the markup has zero
%% width.
%%
%% @see text/1
%% @see empty/0

-spec null_text(string()) -> #text{}.

null_text(S) ->
    mktext(null_string(S)).    % convert to internal representation


%% =====================================================================
%% @spec text_par(Text::string()) -> document()
%% @equiv text_par(Text, 0)

-spec text_par(string()) -> document().

text_par(S) ->
    text_par(S, 0).


%% =====================================================================
%% @spec text_par(Text::string(), Indentation::integer()) -> document()
%%
%% @doc Yields a document representing paragraph-formatted plain text.
%% The optional `Indentation' parameter specifies the extra indentation
%% of the first line of the paragraph. For example, `text_par("Lorem
%% ipsum dolor sit amet", N)' could represent
%% ```Lorem ipsum dolor
%%    sit amet'''
%% if `N' = 0, or
%% ```  Lorem ipsum
%%    dolor sit amet'''
%% if `N' = 2, or
%% ```Lorem ipsum dolor
%%      sit amet'''
%% if `N' = -2.
%% 
%% (The sign of the indentation is thus reversed compared to the {@link
%% par/2} function, and the behaviour varies slightly depending on the
%% sign in order to match the expected layout of a paragraph of text.)
%%
%% Note that this is just a utility function, which does all the work of
%% splitting the given string into words separated by whitespace and
%% setting up a {@link par/2. `par'} with the proper indentation,
%% containing a list of {@link text/1. `text'} elements.
%%
%% @see text_par/1
%% @see text/1
%% @see par/2

-spec text_par(string(), integer()) -> document().

text_par(S, 0) ->
    par(words(S));
text_par(S, N) when N > 0 ->
    nest(N, par(words(S), -N));
text_par(S, N) when N < 0 ->
    par(words(S), -N).

words(S) ->
    words(S, [], []).

words([$\s | Cs], As, Ws) -> words_1(Cs, As, Ws);
words([$\t | Cs], As, Ws) -> words_1(Cs, As, Ws);
words([$\n | Cs], As, Ws) -> words_1(Cs, As, Ws);
words([C | Cs], As, Ws) -> words(Cs, [C | As], Ws);
words([], [], Ws) -> lists:reverse(Ws);
words([], As, Ws) -> words_1([], As, Ws).

words_1(Cs, [], Ws) ->
    words(Cs, [], Ws);
words_1(Cs, As, Ws) ->
    words(Cs, [], [text(lists:reverse(As)) | Ws]).


%% =====================================================================
%% @spec empty() -> document()
%%
%% @doc Yields the empty document, which has neither height nor width.
%% (`empty' is thus different from an empty {@link text/1. `text'}
%% string, which has zero width but height 1.)
%% 
%% Empty documents are occasionally useful; in particular, they have the
%% property that `above(X, empty())' will force a new line after `X'
%% without leaving an empty line below it; since this is a common idiom,
%% the utility function {@link break/1} will place a given document in
%% such a context.
%%
%% @see text/1

-spec empty() -> 'null'.

empty() ->
    null.


%% =====================================================================
%% @spec break(document()) -> document()
%%
%% @doc Forces a line break at the end of the given document. This is a
%% utility function; see {@link empty/0} for details.

-spec break(document()) -> #above{}.

break(D) ->
    above(D, empty()).


%% =====================================================================
%% @spec nest(N::integer(), D::document()) -> document()
%%
%% @doc Indents a document a number of character positions to the right.
%% Note that `N' may be negative, shifting the text to the left, or
%% zero, in which case `D' is returned unchanged.

-spec nest(integer(), document()) -> document().

nest(N, D) ->
    if N =:= 0 ->
	    D;
       true ->
	    #nest{n = N, d = D}
    end.


%% =====================================================================
%% @spec beside(D1::document(), D2::document()) -> document()
%%
%% @doc Concatenates documents horizontally. Returns a document
%% representing the concatenation of the documents `D1' and `D2' such
%% that the last character of `D1' is horizontally adjacent to the first
%% character of `D2', in all possible layouts. (Note: any indentation of
%% `D2' is lost.)
%%
%% Examples:
%% ```ab  cd  =>  abcd
%%
%%    ab  ef      ab
%%    cd  gh  =>  cdef
%%                  gh'''

-spec beside(document(), document()) -> #beside{}.

beside(D1, D2) ->
    #beside{d1 = D1, d2 = D2}.


%% =====================================================================
%% @spec above(D1::document(), D2::document()) -> document()
%%
%% @doc Concatenates documents vertically. Returns a document
%% representing the concatenation of the documents `D1' and `D2' such
%% that the first line of `D2' follows directly below the last line of
%% `D1', and the first character of `D2' is in the same horizontal
%% column as the first character of `D1', in all possible layouts.
%%
%% Examples:
%% ```ab  cd  =>  ab
%%                cd
%%
%%                   abc
%%    abc   fgh  =>   de
%%     de    ij      fgh
%%                    ij'''

-spec above(document(), document()) -> #above{}.

above(D1, D2) ->
    #above{d1 = D1, d2 = D2}.


%% =====================================================================
%% @spec sep(Docs::[document()]) -> document()
%%
%% @doc Arranges documents horizontally or vertically, separated by
%% whitespace. Returns a document representing two alternative layouts
%% of the (nonempty) sequence `Docs' of documents, such that either all
%% elements in `Docs' are concatenated horizontally, and separated by a
%% space character, or all elements are concatenated vertically (without
%% extra separation).
%%
%% Note: If some document in `Docs' contains a line break, the vertical
%% layout will always be selected.
%%
%% Examples:
%% ```                             ab
%%    ab  cd  ef  =>  ab cd ef  |  cd
%%                                 ef
%%
%%    ab           ab
%%    cd  ef  =>   cd
%%                 ef'''
%%
%% @see par/2

-spec sep([document()]) -> #sep{}.

sep(Ds) ->
    #sep{ds = Ds}.


%% =====================================================================
%% @spec par(Docs::[document()]) -> document()
%% @equiv par(Ds, 0)

-spec par([document()]) -> #sep{}.

par(Ds) ->
    par(Ds, 0).


%% =====================================================================
%% @spec par(Docs::[document()], Offset::integer()) -> document()
%%
%% @doc Arranges documents in a paragraph-like layout. Returns a
%% document representing all possible left-aligned paragraph-like
%% layouts of the (nonempty) sequence `Docs' of documents. Elements in
%% `Docs' are separated horizontally by a single space character and
%% vertically with a single line break. All lines following the first
%% (if any) are indented to the same left column, whose indentation is
%% specified by the optional `Offset' parameter relative to the position
%% of the first element in `Docs'. For example, with an offset of -4,
%% the following layout can be produced, for a list of documents
%% representing the numbers 0 to 15:
%%
%% ```    0 1 2 3
%%    4 5 6 7 8 9
%%    10 11 12 13
%%    14 15'''
%% or with an offset of +2:
%% ```0 1 2 3 4 5 6
%%      7 8 9 10 11
%%      12 13 14 15'''
%%
%% The utility function {@link text_par/2} can be used to easily
%% transform a string of text into a `par' representation by splitting
%% it into words.
%%
%% Note that whenever a document in `Docs' contains a line break, it
%% will be placed on a separate line. Thus, neither a layout such as
%% ```ab cd
%%       ef'''
%% nor
%% ```ab
%%    cd ef'''
%% will be generated. However, a useful idiom for making the former
%% variant possible (when wanted) is `beside(par([D1, text("")], N),
%% D2)' for two documents `D1' and `D2'. This will break the line
%% between `D1' and `D2' if `D1' contains a line break (or if otherwise
%% necessary), and optionally further indent `D2' by `N' character
%% positions. The utility function {@link follow/3} creates this context
%% for two documents `D1' and `D2', and an optional integer `N'.
%%
%% @see par/1
%% @see text_par/2

-spec par([document()], integer()) -> #sep{}.

par(Ds, N) ->
    mksep(Ds, N, true).

%% Used internally only:

mksep(Ds, N, P) when is_integer(N) ->
    #sep{ds = Ds, i = N, p = P}.


%% =====================================================================
%% @spec follow(D1::document(), D2::document()) -> document()
%% @equiv follow(D1, D2, 0)

-spec follow(document(), document()) -> #beside{}.

follow(D1, D2) ->
    follow(D1, D2, 0).


%% =====================================================================
%% @spec follow(D1::document(), D2::document(), Offset::integer()) ->
%%           document()
%% 
%% @doc Separates two documents by either a single space, or a line
%% break and intentation. In other words, one of the layouts
%% ```abc def'''
%% or
%% ```abc
%%     def'''
%% will be generated, using the optional offset in the latter case. This
%% is often useful for typesetting programming language constructs.
%%
%% This is a utility function; see {@link par/2} for further details.
%%
%% @see follow/2

-spec follow(document(), document(), integer()) -> #beside{}.

follow(D1, D2, N) when is_integer(N) ->
    beside(par([D1, nil()], N), D2).


%% =====================================================================
%% @spec floating(document()) -> document()
%% @equiv floating(D, 0, 0)

-spec floating(document()) -> #float{}.

floating(D) ->
    floating(D, 0, 0).


%% =====================================================================
%% @spec floating(D::document(), Hp::integer(), Vp::integer()) ->
%%           document()
%%
%% @doc Creates a "floating" document. The result represents the same
%% set of layouts as `D'; however, a floating document may be moved
%% relative to other floating documents immediately beside or above it,
%% according to their relative horizontal and vertical priorities. These
%% priorities are set with the `Hp' and `Vp' parameters; if omitted,
%% both default to zero.
%%
%% Notes: Floating documents appear to work well, but are currently less
%% general than you might wish, losing effect when embedded in certain
%% contexts. It is possible to nest floating-operators (even with
%% different priorities), but the effects may be difficult to predict.
%% In any case, note that the way the algorithm reorders floating
%% documents amounts to a "bubblesort", so don't expect it to be able to
%% sort large sequences of floating documents quickly.

-spec floating(document(), integer(), integer()) -> #float{}.

floating(D, H, V) when is_integer(H), is_integer(V) ->
    #float{d = D, h = H, v = V}.


%% =====================================================================
%% @spec format(D::document()) -> string()
%% @equiv format(D, 80)

-spec format(document()) -> string().

format(D) ->
    format(D, 80).


%% =====================================================================
%% @spec format(D::document(), PaperWidth::integer()) -> string()
%% @equiv format(D, PaperWidth, 65)

-spec format(document(), integer()) -> string().

format(D, W) ->
    format(D, W, 65).


%% =====================================================================
%% @spec format(D:: document(), PaperWidth::integer(),
%%              LineWidth::integer()) -> string()
%% @throws no_layout
%%
%% @doc Computes a layout for a document and returns the corresponding
%% text. See {@link document()} for further information. Throws
%% `no_layout' if no layout could be selected.
%%
%% `PaperWidth' specifies the total width (in character positions) of
%% the field for which the text is to be laid out. `LineWidth' specifies
%% the desired maximum width (in number of characters) of the text
%% printed on any single line, disregarding leading and trailing white
%% space. These parameters need to be properly balanced in order to
%% produce good layouts. By default, `PaperWidth' is 80 and `LineWidth'
%% is 65.
%%
%% @see best/3

-spec format(document(), integer(), integer()) -> string().

format(D, W, R) ->
    case best(D, W, R) of
	empty ->
	    throw(no_layout);
	L -> layout(L)
    end.


%% =====================================================================
%% Representation:
%%
%%	document() = #text{s = string()}
%%		   | #nest{n = integer(), d = document()}
%%		   | #beside{d1 = document(), d2 = document()}
%%		   | #above{d1 = document(), d2 = document()}
%%		   | #sep{ds = [document()], i = integer(), p = boolean()}
%%		   | null
%%
%% A `text' node simply represents a string (which should not contain
%% linefeed or carriage return characters). A `nest' node specifies a
%% relative indentation (in number of character positions) of a
%% document. The indentation could be a negative number. A `beside' node
%% specifies a horizontal composition of two documents, and an `above'
%% node a vertical composition. A `sep' node specifies a list of
%% alternative documents; the `i' field holds the extra indentation of
%% all documents but the first in `ds', and if the `p' field is `true'
%% then the list is typeset in paragraph mode.
%%
%% The function `best/3' yields a representation of a "best layout",
%% suitable for direct conversion to text, having the following
%% restricted form:
%%
%%	layout() = #text{s = string()}
%%		 | #above{d1 = #text{s = string()}, d2 = layout()}
%%		 | #nest{n = integer(), d = layout()}
%%		 | null
%%
%% The function `layout/1' performs the final transformation to a single
%% flat string from the restricted document form.

layout(L) ->
    lists:reverse(layout(0, L, [])).

layout(N, #above{d1 = #text{s = S}, d2 = L}, Cs) ->
    layout(N, L, [$\n | flatrev(string_chars(S), indent(N, Cs))]);
layout(N, #nest{n = N1, d = L}, Cs) ->
    layout(N + N1, L, Cs);
layout(N, #text{s = S}, Cs) ->
    flatrev(string_chars(S), indent(N, Cs));
layout(_N, null, Cs) ->
    Cs.

indent(N, Cs) when N >= 8 ->
    indent(N - 8, [$\t | Cs]);
indent(N, Cs) when N > 0 ->
    indent(N - 1, [$\s | Cs]);
indent(_N, Cs) ->
    Cs.

flatrev(Cs, As) ->
    flatrev(Cs, As, []).

flatrev([C = [_|_] | Cs], As, Ss) ->
    flatrev(C, As, [Cs | Ss]);
flatrev([[] | Cs], As, Ss) ->
    flatrev(Cs, As, Ss);
flatrev([C | Cs], As, Ss) ->
    flatrev(Cs, [C | As], Ss);
flatrev([], As, [S | Ss]) ->
    flatrev(S, As, Ss);
flatrev([], As, []) ->
    As.


%% =====================================================================
%% @spec best(document(), PaperWidth::integer(),
%%            LineWidth::integer()) -> empty | document()
%%
%% @doc Selects a "best" layout for a document, creating a corresponding
%% fixed-layout document. If no layout could be produced, the atom
%% `empty' is returned instead. For details about `PaperWidth' and
%% `LineWidth', see {@link format/3}. The function is idempotent.
%%
%% One possible use of this function is to compute a fixed layout for a
%% document, which can then be included as part of a larger document.
%% For example:
%% ```above(text("Example:"), nest(8, best(D, W - 12, L - 6)))'''
%% will format `D' as a displayed-text example indented by 8, whose
%% right margin is indented by 4 relative to the paper width `W' of the
%% surrounding document, and whose maximum individual line length is
%% shorter by 6 than the line length `L' of the surrounding document.
%%
% This function is used by the {@link format/3} function to prepare a
%% document before being laid out as text.

%% Recall that a document represents a set of possible layouts. `best'
%% selects the "best" layout of a document, returning a simplified
%% representation that can be given directly to `layout', unless the
%% returned value is `empty', signaling that no layout could be
%% produced. In addition, documents on the form `#union{d1 = D1, d2 =
%% D2}' and `#fit{d = D}' are used internally.
%%
%% Note: It is vital for this algorithm to maintain the invariant on
%% unions that the left argument has a longer first line than the right
%% argument!

%% Contexts:
%%
%%	#c_best_nest{w = integer(), r = integer(), i = integer()}
%%	#c_above_nest{d = document(), i = integer(), c = ctxt()}
%%	#c_beside{d = document(), c = ctxt()}
%%	#c_text_beside{s = string(), c = ctxt()}
%%	#c_sep_nest{ds = [document()], i = integer(), p = boolean(),
%%		    c = ctxt()}
%%	#c_best_nest_or{w = integer(), r = integer(), i = integer(),
%%			d = document()}
%%	#c_fit{c = ctxt()}

%% best(w, r, nest(i, *))
-record(c_best_nest, {w :: integer(), r :: integer(), i :: integer()}).

%% above(*, nest(i, d))
-record(c_above_nest, {d :: document(), i = 0 :: integer(), c :: ctxt()}).

-record(c_beside, {d :: document(), c :: ctxt()}).	%% beside(*, d)

-record(c_text_beside, {s :: string(), c :: ctxt()}).	%% beside(text(s), *)

%% p = false	=>	sep([* | map(nest i, ds)])
%% p = true	=>	par([* | map(nest i, ds)])

-record(c_sep_nest, {ds :: [document()], i :: integer(),
		     p  :: boolean(),    c :: ctxt()}).

%% nicest(best(w, r, nest(i, *)), best(w, r, d))
-record(c_best_nest_or, {w :: integer(), r :: integer(),
			 i :: integer(), d :: document()}).

-record(c_fit, {c :: ctxt()}).			%% fit(*)

%% beside(float(d, h, v), *)
-record(c_float_beside, {d :: document(), h :: integer(),
			 v :: integer(),  c :: ctxt()}).
%% above(float(d, h, v), nest(i, *))
-record(c_float_above_nest, {d :: document(), h :: integer(),
			     v :: integer(),  i :: integer(), c :: ctxt()}).

%% Contexts introduced:		In case:
%%
%%	c_best_nest		top-level call
%%	c_above_nest		above (c_best_nest)
%%	c_beside		beside (c_best_nest)
%%	c_text_beside		text (c_beside)
%%	c_sep_nest		sep (c_best_nest)
%%	c_best_nest_or		union (c_best_nest)
%%	c_fit			fit
%%	c_float_beside		float (c_beside)
%%	c_float_above_nest	float (c_above_nest)

-type ctxt() :: #c_best_nest{} | #c_above_nest{}
	      | #c_beside{} | #c_text_beside{}
	      | #c_sep_nest{} | #c_best_nest_or{}
	      | #c_fit{} | #c_float_beside{} | #c_float_above_nest{}.

%% Entry point for the layout algorithm:

-spec best(document(), integer(), integer()) -> 'empty' | document().

best(D, W, R) ->
    rewrite(D, #c_best_nest{w = W, r = R, i = 0}).

rewrite(#text{s = S}, C) ->
    case C of
	#c_best_nest{i = N} ->
	    nest(N, mktext(S));		% finish
	#c_above_nest{d = D1, i = N1, c = C1} ->
	    case C1 of
		#c_best_nest{w = W, r = R, i = N} ->
		    %% Move out completed line.
		    %% (Note new indentation N1.)
		    nest(N,
			 above(mktext(S),
			       rewrite(D1,
				       #c_best_nest{w = W - N,
						    r = R,
						    i = N1})));
		#c_beside{d = D2, c = C2} ->
		    %% Associativity (not symmetric)
		    rewrite(above(mktext(S),
				  nest(N1, beside(D1, D2))), C2);
		#c_text_beside{s = S1, c = C2} ->
		    %% Join segments (note the indentation!)
		    rewrite(above(mktext(concat(S1, S)),
				  nest(N1 + width(S1), D1)),
			    C2);
		#c_sep_nest{ds = Ds, i = N, c = C2} ->
		    case is_empty_string(S) of
			false ->
			    %% Move out the prefix (note the
			    %% indentation!)
			    W = width(S),
			    rewrite(beside(
				      mktext(S),
				      mksep([above(nil(),
						   nest(N1 - W,
							D1))
					     | Ds],
					    N - W,
					    C1#c_sep_nest.p)),
				    C2);
			true ->
			    %% Like when we have just an empty
			    %% string and nothing else, this
			    %% forces us to expand the `sep'. The
			    %% line break will then force a normal
			    %% `sep' to select the vertical
			    %% alternative, but for a `par', we
			    %% need to force a line break before
			    %% the remaining elements are laid
			    %% out. (Note the indentation!)
			    case C1#c_sep_nest.p of
				false ->
				    rewrite(expand_sep(
					      above(nil(),
						    nest(N1, D1)),
					      Ds, N),
					    C2);
				true ->
				    rewrite(expand_par(
					      above(nil(),
						    nest(N1, D1)),
					      Ds, N),
					    C2)
			    end
		    end;
		#c_best_nest_or{w = W, r = R, i = N, d = D} ->
		    L = width(S),
		    case ((L + N) > W) or (L > R) of
			true ->
			    %% The first line of the LHS layout is
			    %% not nice, so select the RHS.
			    rewrite(D, #c_best_nest{w = W, r = R,
						    i = N});
			false ->
			    %% Select the LHS. (Note the
			    %% indentation!)
			    rewrite(above(mktext(S),
					  nest(N1, D1)),
				    #c_best_nest{w = W, r = R,
						 i = N})
		    end;
		#c_float_beside{d = D2, c = C2} ->
		    rewrite(beside(D2, above(mktext(S),
					     nest(N1, D1))),
			    C2);
		#c_float_above_nest{d = D2, i = N2, c = C2} ->
		    rewrite(above(D2,
				  nest(N2, above(mktext(S),
						 nest(N1, D1)))),
			    C2);
		#c_above_nest{} ->
		    exit(badarg);	% this can't happen
		#c_fit{} ->
		    exit(badarg)	% this can't happen
	    end;
	#c_beside{d = D1, c = C1} ->
	    case C1 of
		#c_above_nest{d = D2, i = N, c = C2} ->
		    case is_empty_string(S) of
			false ->
			    %% Move out the prefix (note the
			    %% indentation!)
			    W = width(S),
			    rewrite(beside(mktext(S),
					   above(
					     beside(nil(), D1),
					     nest(N - W, D2))),
				    C2);
			true ->
			    %% Pass on
			    rewrite(D1, #c_text_beside{s = S,
						       c = C1})
		    end;
		#c_text_beside{s = S1, c = C2} ->
		    %% Associativity (we simplify early)
		    rewrite(beside(mktext(concat(S1, S)), D1),
			    C2);
		#c_sep_nest{ds = Ds, i = N, c = C2} ->
		    case is_empty_string(S) of
			false ->
			    %% Move out the prefix (note the
			    %% indentation!)
			    W = width(S),
			    rewrite(beside(mktext(S),
					   mksep(
					     [beside(nil(), D1)
					      | Ds],
					     N - W,
					     C1#c_sep_nest.p)),
				    C2);
			true ->
			    %% Pass on
			    rewrite(D1, #c_text_beside{s = S,
						       c = C1})
		    end;
		#c_best_nest_or{w = W, r = R, i = N, d = D} ->
		    L = width(S),
		    case ((L + N) > W) or (L > R) of
			true ->
			    %% The first line of the LHS layout is
			    %% not nice, so select the RHS.
			    rewrite(D, #c_best_nest{w = W, r = R,
						    i = N});
			false ->
			    %% Pass on
			    rewrite(D1, #c_text_beside{s = S,
						       c = C1})
		    end;
		#c_float_beside{d = D2, c = C2} ->
		    rewrite(beside(D2, beside(mktext(S), D1)),
			    C2);
		#c_float_above_nest{d = D2, i = N, c = C2} ->
		    rewrite(above(D2,
				  nest(N, beside(mktext(S), D1))),
			    C2);
		_ ->
		    %% Pass on
		    rewrite(D1, #c_text_beside{s = S, c = C1})
	    end;
	#c_text_beside{s = S1, c = C1} ->
	    rewrite(mktext(concat(S1, S)), C1);	% join segments
	#c_sep_nest{ds = Ds, i = N, c = C1} ->
	    case is_empty_string(S) of
		false ->
		    %% Move out the prefix (note the indentation!)
		    rewrite(beside(mktext(S),
				   mksep([nil() | Ds],
					 N - width(S),
					 C#c_sep_nest.p)),
			    C1);
		true ->
		    %% This is the only place where we are forced to
		    %% introduce a union. Recall the invariant that the
		    %% left argument must have a longer first line than
		    %% the right argument; also recall that `Ds' is
		    %% always nonempty here. Now, since [D | Ds]
		    %% contains at least two elements, the first line of
		    %% the horizontal layout will always contain at
		    %% least one space character more than the first
		    %% line of the vertical layout.
		    case C#c_sep_nest.p of
			false ->
			    rewrite(expand_sep(nil(), Ds, N), C1);
			true ->
			    rewrite(expand_par(nil(), Ds, N), C1)
		    end
	    end;
	#c_best_nest_or{w = W, r = R, i = N, d = D} ->
	    L = width(S),
	    case ((L + N) > W) or (L > R) of
		true ->
		    %% The first line of the LHS layout is not
		    %% nice, so select the RHS (which contains
		    %% at least two lines).
		    rewrite(D, #c_best_nest{w = W, r = R, i = N});
		false ->
		    nest(N, mktext(S))	  % finish
	    end;
	#c_fit{c = C1} ->
	    %% Identity:
	    rewrite(mktext(S), C1);
	#c_float_beside{d = D1, c = C1} ->
	    rewrite(beside(D1, mktext(S)), C1);
	#c_float_above_nest{d = D1, i = N, c = C1} ->
	    rewrite(above(D1, nest(N, mktext(S))), C1)
    end;
rewrite(#nest{n = N, d = D}, C) ->
    case C of
	#c_best_nest{w = W, r = R, i = N1} ->
	    %% Note that we simplify by not creating an actual `nest'
	    %% node, but instead just modifying the context:
	    %% rewrite(nest(N1, nest(N, D))) = rewrite(nest(N1 + N, D)).
	    rewrite(D, #c_best_nest{w = W, r = R, i = N + N1});
	#c_above_nest{d = D1, i = N1, c = C1} ->
	    %% Distributivity
	    %% (Note the indentation!)
	    rewrite(nest(N, above(D, nest(N1 - N, D1))), C1);
	#c_beside{d = D1, c = C1} ->
	    %% Associativity (not symmetric):
	    rewrite(nest(N, beside(D, D1)), C1);
	#c_text_beside{} ->
	    rewrite(D, C);   % (`beside' kills RHS indentation)
	#c_sep_nest{ds = Ds, i = N1, c = C1} ->
	    %% Distributivity (in the vertical form, the RHS
	    %% indentation is killed)
	    rewrite(nest(N, mksep([D | Ds],
				  N1 - N,
				  C#c_sep_nest.p)),
		    C1);
	#c_fit{c = C1} ->
	    %% Distributivity:
	    rewrite(nest(N, fit(D)), C1);
	#c_float_beside{} ->
	    rewrite(D, C);    % (`beside' kills RHS indentation)
	#c_float_above_nest{d = D1, h = H, v = V, i = N1,
			    c = C1} ->
	    rewrite(D, #c_float_above_nest{d = D1, h = H, v = V,
					   i = N + N1, c = C1});
	#c_best_nest_or{} ->
	    exit(badarg)    % this can't happen
    end;
rewrite(#above{d1 = D1, d2 = D2}, C) ->
    case C of
	#c_above_nest{d = D3, i = N, c = C1} ->
	    %% Associativity:
	    %% (Note the indentation!)
	    rewrite(D1, #c_above_nest{d = above(D2, nest(N, D3)),
				      c = C1});
	#c_beside{d = D3, c = C1} ->
	    %% Associativity (not symmetric):
	    rewrite(above(D1, beside(D2, D3)), C1);
	#c_fit{c = C1} ->
	    rewrite(empty, C1);	% this is the whole point of `fit'
	_ ->
	    rewrite(D1, #c_above_nest{d = D2, c = C})	% pass on
    end;
rewrite(#beside{d1 = D1, d2 = D2}, C) ->
    case C of
	#c_beside{d = D3, c = C1} ->
	    %% Associativity:
	    rewrite(D1, #c_beside{d = beside(D2, D3), c = C1});
	#c_fit{c = C1} ->
	    %% Distributivity:
	    rewrite(beside(fit(D1), fit(D2)), C1);
	_ ->
	    rewrite(D1, #c_beside{d = D2, c = C})	% pass on
    end;
rewrite(#sep{ds = Ds, i = N, p = P}, C) ->
    case C of
	#c_fit{c = C1} ->
	    %% The vertical layout is thus impossible, and the
	    %% extra indentation has no effect.
	    rewrite(fit(horizontal(Ds)), C1);
	#c_float_beside{d = D1, c = C1} ->
	    %% Floats are not moved in or out of sep's
	    rewrite(beside(D1, mksep(Ds, N, P)), C1);
	#c_float_above_nest{d = D1, i = N1, c = C1} ->
	    %% Floats are not moved in or out of sep's
	    rewrite(above(D1, nest(N1, mksep(Ds, N, P))), C1);
	_ ->
	    enter_sep(Ds, N, P, C)		% pass on
    end;
rewrite(#union{d1 = D1, d2 = D2}, C) ->
    %% Introduced by the occurrence of an empty `text' string in a
    %% `sep' context. See the note above about the invariant for
    %% unions!
    case C of
	#c_best_nest{w = W, r = R, i = N} ->
	    %% Pass on
	    rewrite(D1, #c_best_nest_or{w = W, r = R, i = N,
					d = D2});
	#c_above_nest{d = D3, i = N, c = C1} ->
	    %% Distributivity:
	    %% (Note the indentation!)
	    rewrite(union(above(D1, nest(N, D3)),
			  above(D2, nest(N, D3))),
		    C1);
	#c_beside{d = D3, c = C1} ->
	    %% Distributivity:
	    rewrite(union(beside(D1, D3), beside(D2, D3)), C1);
	#c_text_beside{s = S, c = C1} ->
	    %% Distributivity:
	    rewrite(union(beside(mktext(S), D1),
			  beside(mktext(S), D2)),
		    C1);
	#c_sep_nest{ds = Ds, i = N, c = C1} ->
	    %% Distributivity:
	    rewrite(union(mksep([D1 | Ds], N, C#c_sep_nest.p),
			  mksep([D2 | Ds], N, C#c_sep_nest.p)),
		    C1);
	#c_best_nest_or{w = W, r = R, i = N, d = D3} ->
	    %% Associativity:
	    rewrite(D1, #c_best_nest_or{w = W, r = R, i = N,
					d = union(D2, D3)});
	#c_fit{c = C1} ->
	    %% Distributivity:
	    rewrite(union(fit(D1), fit(D2)), C1);
	#c_float_beside{d = D3, h = H, v = V, c = C1} ->
	    %% Distributivity:
	    rewrite(union(beside(floating(D3, H, V), D1),
			  beside(floating(D3, H, V), D2)),
		    C1);
	#c_float_above_nest{d = D3, h = H, v = V, i = N, c = C1} ->
	    %% Distributivity:
	    rewrite(union(above(floating(D3, H, V), nest(N, D1)),
			  above(floating(D3, H, V), nest(N, D2))),
		    C1)
    end;
rewrite(empty, C) ->
    %% Introduced by `sep'.
    case C of
	#c_best_nest{} ->
	    empty;		% preserve `empty'
	#c_above_nest{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_beside{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_text_beside{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_sep_nest{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_best_nest_or{w = W, r = R, i = N, d = D} ->
	    %% Try the other layout
	    rewrite(D, #c_best_nest{w = W, r = R, i = N});
	#c_fit{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_float_beside{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_float_above_nest{c = C1} ->
	    rewrite(empty, C1)	% preserve `empty'
	end;
rewrite(#fit{d = D}, C) ->
    %% Introduced by the occurrence of an empty `text' string in a
    %% `sep' context.
    case C of
	#c_fit{} ->
	    %% Idempotency:
	    rewrite(D, C);
	_ ->
	    rewrite(D, #c_fit{c = C})	% pass on
    end;
rewrite(#float{d = D, h = H, v = V}, C) ->
    case C of
	#c_beside{d = D1, c = C1} ->
	    case C1 of
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when H1 > H ->
		    %% Move left
		    rewrite(beside(floating(D, H, V),
				   beside(floating(D2, H1, V1),
					  D1)),
			    C2);
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when V1 /= V ->
		    %% Align vertically
		    rewrite(above(floating(D2, H1, V1),
				  beside(floating(D, H, V), D1)),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = N1, c = C2}
		when V1 > V ->
		    %% Move up (note the indentation, and note
		    %% that all three become aligned vertically)
		    rewrite(above(nest(N1, floating(D, H, V)),
				  above(floating(D2, H1, V1),
					D1)),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = _N1, c = C2}
		when V1 == V, H1 /= H ->
		    %% Align horizontally
		    rewrite(beside(floating(D2, H1, V1),
				   beside(floating(D, H, V),
					  D1)),
			    C2);
		_ ->
		    rewrite(D1, #c_float_beside{d = D, h = H,
						v = V, c = C1})
	    end;
	#c_above_nest{d = D1, i = N, c = C1} ->
	    case C1 of
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when H1 > H ->
		    %% Move left (indentation is lost; note that
		    %% all three become aligned horizontally)
		    rewrite(beside(floating(D, H, V),
				   beside(floating(D2, H1, V1),
					  D1)),
			    C2);
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when V1 /= V ->
		    %% Align vertically
		    rewrite(above(floating(D2, H1, V1),
				  above(floating(D, H, V),
					nest(N, D1))),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = N1, c = C2}
		when V1 > V ->
		    %% Move up (note the indentation)
		    rewrite(above(nest(N1, floating(D, H, V)),
				  above(floating(D2, H1, V1),
					nest(N + N1, D1))),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = _N1, c = C2}
		when V1 == V, H1 /= H ->
		    %% Align horizontally
		    rewrite(beside(
			      floating(D2, H1, V1),
			      above(floating(D, H, V),
				    nest(N, D1))),
			    C2);
		_ ->
		    rewrite(D1, #c_float_above_nest{d = D, h = H,
						    v = V, i = N,
						    c = C1})
	    end;
	#c_fit{c = C1} ->
	    rewrite(floating(fit(D), H, V), C1);
	#c_float_beside{d = D1, h = H1, v = V1, c = C1} ->
	    if H1 > H ->
		    %% Swap
		    rewrite(beside(floating(D, H, V),
				   floating(D1, H1, V1)),
			    C1);
	       V1 /= V ->
		    %% Align vertically
		    rewrite(above(floating(D, H, V),
				  floating(D1, H1, V1)),
			    C1);
	       true ->
		    %% Drop the 'float' wrapper of the rightmost.
		    rewrite(beside(floating(D1, H1, V1), D), C1)
	    end;
	#c_float_above_nest{d = D1, h = H1, v = V1, i = N,
			    c = C1} ->
	    if V1 > V ->
		    %% Swap (note the indentation)
		    rewrite(above(nest(N, floating(D, H, V)),
				  floating(D1, H1, V1)),
			    C1);
	       V1 == V, H1 /= H ->
		    %% Align horizontally
		    rewrite(beside(floating(D, H, V),
				   floating(D1, H1, V1)),
			    C1);
	       true ->
		    %% Drop the 'float' wrapper of the lower.
		    rewrite(above(floating(D1, H1, V1),
				  nest(N, D)),
			    C1)
	    end;
	_ ->
	    %% All other cases simply drop the `float' wrapper.
	    rewrite(D, C)
    end;
rewrite(null, C) ->
    case C of
	#c_best_nest{} ->
	    null;    % done
	#c_above_nest{d = D, i = N, c = C1} ->
	    rewrite(nest(N, D), C1);
	#c_beside{d = D, c = C1} ->
	    rewrite(D, C1);
	#c_text_beside{s = S, c = C1} ->
	    rewrite(mktext(S), C1);
	#c_sep_nest{} ->
	    %% In a `nest' context, an empty document behaves like
	    %% the empty string.
	    rewrite(nil(), C);
	#c_best_nest_or{w = W, r = R, i = N} ->
	    %% An empty document as "nice" as it can be, so we
	    %% discard the alternative.
	    rewrite(null, #c_best_nest{w = W, r = R, i = N});
	#c_fit{c = C1} ->
	    rewrite(null, C1);    % identity
	#c_float_beside{d = D, h = _H, v = _V, c = C1} ->
	    %% We just remove the float wrapper; cf. below.
	    rewrite(beside(D, null), C1);
	#c_float_above_nest{d = D, h = _H, v = _V, i = N, c = C1} ->
	    %% It is important that this case just removes the
	    %% float wrapper; the empty document must be preserved
	    %% until later, or it will not be useful for forcing
	    %% line breaks.
	    rewrite(above(D, nest(N, null)), C1)
    end.

%% Both `null' and `empty' are already in use, so what do you do?

nil() ->
    text("").

hspace() ->
    text([$\s]).

union(D1, D2) ->
    #union{d1 = D1, d2 = D2}.

fit(D) ->
    #fit{d = D}.

enter_sep(Ds, N, P, C) ->
    case Ds of
	[D] ->
	    rewrite(D, C);    % Handle this case separately
	[D | Ds1] ->
	    %% Note that we never build a `sep'-context with an
	    %% empty "tail" list! `Ds1' is nonempty here!
	    rewrite(D, #c_sep_nest{ds = Ds1, i = N, p = P, c = C})
    end.

%% When we expand a `sep', the extra indentation appears as `nest'
%% operators, but not until then.

expand_sep(D, Ds, N) ->
    union(fit(horizontal([D | Ds])),
	  vertical([D | [nest(N, D1) || D1 <- Ds]])).

expand_par(D, [D1 | Ds] = DL, N) ->
    union(beside(fit(D),
		 beside(hspace(),
			mksep([fit(D1) | Ds], N - 1, true))),
	  above(D, nest(N, par(DL)))).

horizontal(Ds) ->
    foldr1(fun (D1, D2) ->
		   beside(D1, beside(hspace(), D2))
	   end, Ds).

vertical(Ds) ->
    foldr1(fun above/2, Ds).

foldr1(_F, [H]) ->
    H;
foldr1(F, [H | T]) ->
    F(H, foldr1(F, T)).

%% Internal representation of strings; stores the field width and does
%% not perform list concatenation until the text is requested. Strings
%% are thus deep lists whose first element is the length of the string.
%% Null strings are strings whose "official width" is zero, typically
%% used for markup that is not supposed to affect the indentation.

string(S) ->
    [strwidth(S) | S].

null_string(S) ->
    [0 | S].

concat([_ | []], [_ | _] = S) ->
    S;
concat([_ | _] = S, [_ | []]) ->
    S;
concat([L1 | S1], [L2 | S2]) ->
    [L1 + L2 | [S1 | S2]].

string_chars([_ | S]) ->
    S.

width(S) ->
    hd(S).

is_empty_string([_ | []]) ->
    true;
is_empty_string([_ | _]) ->
    false.

%% We need to use `strwidth' instead of list `length', to properly
%% handle Tab characters in the text segments. Note that the width of
%% tabs is hard-coded as 8 character positions, and that strings are
%% individually considered to be aligned at column 0; Tab characters are
%% not really nice to give to a prettyprinter, and this seems to be the
%% best interpretation.

strwidth(S) ->
    strwidth(S, 0).

strwidth([$\t | Cs], N) ->
    strwidth(Cs, N - (N rem 8) + 8);
strwidth([_ | Cs], N) ->
    strwidth(Cs, N + 1);
strwidth([], N) ->
    N.

%% =====================================================================
