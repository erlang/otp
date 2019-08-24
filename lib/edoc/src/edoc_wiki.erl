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

%% @doc EDoc wiki expansion, parsing and postprocessing of XML text.
%% Uses {@link //xmerl. XMerL}.
%% @end

%% Notes:
%%
%% * Whatever happens in this module, it must interact nicely with the
%% actual XML-parsing. It is not acceptable to break any existing and
%% legal XML markup so that it does not parse or is rendered wrong.
%%
%% * The focus should always be on making *documentation* easier to
%% write. No wiki notation should be introduced unless it is clear that
%% it is better than using plain XHTML, making typing less cumbersome
%% and the resulting text easier to read. The wiki notation should be a
%% small bag of easy-to-remember tricks for making XHTML documentation
%% easier to write, not a complete markup language in itself. As a
%% typical example, it is hardly worthwile to introduce a special
%% notation like say, ""..."" for emphasized text, since <em>...</em> is
%% not much harder to write, not any less readable, and no more
%% difficult to remember, especially since emphasis is not very often
%% occurring in normal documentation.
%%
%% * The central reasoning for the code-quoting goes like this: I don't
%% want to have special escape characters within the quotes (like
%% backslash in C), to allow quoting of the quote characters themselves.
%% I also don't want to use the "`" character both for opening and
%% closing quotes. Therefore, you can either use `...' - and then you
%% cannot use the "'" character without ending the quote - or you can
%% use ``...'' - which allows single but not double "'" characters
%% within the quote. Whitespace is automatically removed from the
%% beginning and the end of the quoted strings; this allows you to write
%% things like "`` 'foo@bar' ''". Text that contains "''" has to be
%% written within <code>...</code>.
%%
%% To produce a single "`" character without starting a quote, write
%% "`'" (no space between "`" and "'").
%%
%% For verbatim/preformatted text, the ```...'''-quotes expand to
%% "<pre><![CDATA[...]]></pre>". The indentation at the start of the
%% quoted string is preserved; whitespace is stripped only at the end.
%% Whole leading lines of whitespace are however skipped.

-module(edoc_wiki).

-export([parse_xml/2, expand_text/2]).

-include("edoc.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(BASE_HEADING, 3).


%% Parsing Wiki-XML with pre-and post-expansion.

parse_xml(Data, Line) ->
    par(parse_xml_1(expand_text(Data, Line), Line)).

parse_xml_1(Text, Line) ->
    Text1 = "<doc>" ++ Text ++ "</doc>",
    %% Any coding except "utf-8".
    Opts = [{line, Line}, {encoding, 'iso-8859-1'}],
    case catch {ok, xmerl_scan:string(Text1, Opts)} of
	{ok, {E, _}} ->
	    E#xmlElement.content;
	{'EXIT', {fatal, {Reason, L, _C}}} ->
	    throw_error(L, {"XML parse error: ~p.", [Reason]});
	{'EXIT', Reason} ->
	    throw_error(Line, {"error in XML parser: ~P.", [Reason, 10]});
	Other ->
	    throw_error(Line, {"nocatch in XML parser: ~P.", [Other, 10]})
    end.

%% Expand wiki stuff in arbitrary text.

expand_text(Cs, L) ->
    lists:reverse(expand_new_line(Cs, L, [])).

%% Interestingly, the reverse of "code" is "edoc". :-)

expand_new_line([$\s = C | Cs], L, As) ->
    expand_new_line(Cs, L, [C | As]);
expand_new_line([$\t = C | Cs], L, As) ->
    expand_new_line(Cs, L, [C | As]);
expand_new_line([$\n = C | Cs], L, As) ->
    expand_new_line(Cs, L + 1, [C | As]);
expand_new_line([$=, $=, $=, $= | Cs], L, As) ->
    expand_heading(Cs, 2, L, As);
expand_new_line([$=, $=, $= | Cs], L, As) ->
    expand_heading(Cs, 1, L, As);
expand_new_line([$=, $= | Cs], L, As) ->
    expand_heading(Cs, 0, L, As);
expand_new_line(Cs, L, As) ->
    expand(Cs, L, As).

expand([$`, $' | Cs], L, As) ->
    expand(Cs, L, [$` | As]);    % produce "`" - don't start a new quote
expand([$`, $`, $` | Cs], L, As) ->
    %% If this is the first thing on the line, compensate for the
    %% indentation, unless we had to skip one or more empty lines.
    {Cs1, Skipped} = strip_empty_lines(Cs),    % avoid vertical space
    N = if Skipped > 0 ->
		0;
	   true ->
		{As1, _} = edoc_lib:split_at(As, $\n),
		case edoc_lib:is_space(As1) of
		    true -> 3 + length(As1);
		    false -> 2    % nice default - usually right.
		end
	end,
    Ss = lists:duplicate(N, $\s),
    expand_triple(Cs1, L + Skipped, Ss ++ "[ATADC[!<>erp<" ++ As);
expand([$`, $` | Cs], L, As) ->
    expand_double(edoc_lib:strip_space(Cs), L, ">edoc<" ++ As);
expand([$` | Cs], L, As) ->
    expand_single(edoc_lib:strip_space(Cs), L, ">edoc<" ++ As);
expand([$[ | Cs], L, As) ->
    expand_uri(Cs, L, As);
expand([$\n = C | Cs], L, As) ->
    expand_new_line(Cs, L + 1, [C | As]);
expand([C | Cs], L, As) ->
    expand(Cs, L, [C | As]);
expand([], _, As) ->
    As.

%% == Heading ==
%% === SubHeading ===
%% ==== SubSubHeading ====

expand_heading([$= | _] = Cs, N, L, As) ->
    expand_heading_1(Cs, N, L, As);
expand_heading(Cs, N, L, As) ->
    {Cs1, Cs2} = edoc_lib:split_at(Cs, $\n),
    case edoc_lib:strip_space(lists:reverse(Cs1)) of
	[$=, $= | Cs3] ->
	    {Es, Ts} = lists:splitwith(fun (X) -> X =:= $= end, Cs3),
	    if length(Es) =:= N ->
		    Ts1 = edoc_lib:strip_space(
			    lists:reverse(edoc_lib:strip_space(Ts))),
		    expand_heading_2(Ts1, Cs2, N, L, As);
	       true ->
		    H1 = lists:duplicate(N+2, $=),
		    H2 = "==" ++ Es,
		    throw_error(L, {"heading end marker mismatch: "
				     "~s...~s", [H1, H2]})
	    end;
	_ ->
	    expand_heading_1(Cs, N, L, As)
    end.

expand_heading_1(Cs, N, L, As) ->
    expand(Cs, L, lists:duplicate(N + 2, $=) ++ As).

expand_heading_2(Ts, Cs, N, L, As) ->
    H = ?BASE_HEADING + N,
    Ts1 = io_lib:format("<h~w><a name=\"~ts\">~ts</a></h~w>\n",
			[H, make_label(Ts), Ts, H]),
    expand_new_line(Cs, L + 1, lists:reverse(lists:flatten(Ts1), As)).

make_label([$\s | Cs]) ->
    [$_ | make_label(edoc_lib:strip_space(Cs))];
make_label([$\t | Cs]) ->
    [$_ | make_label(edoc_lib:strip_space(Cs))];
make_label([$\n | Cs]) ->
    [$_ | make_label(edoc_lib:strip_space(Cs))];
make_label([C | Cs]) ->
    [C | make_label(Cs)];
make_label([]) ->
    [].

%% `...'

expand_single(Cs, L, As) ->
    expand_single(Cs, L, As, L).

expand_single([$' | Cs], L, As, _L0) ->
    expand(Cs, L, ">edoc/<" ++ edoc_lib:strip_space(As));
expand_single([$< | Cs], L, As, L0) ->
    expand_single(Cs, L, ";tl&" ++ As, L0);
expand_single([$> | Cs], L, As, L0) ->
    expand_single(Cs, L, ";tg&" ++ As, L0);
expand_single([$& | Cs], L, As, L0) ->
    expand_single(Cs, L, ";pma&" ++ As, L0);
expand_single([$\n = C | Cs], L, As, L0) ->
    expand_single(Cs, L + 1, [C | As], L0);
expand_single([C | Cs], L, As, L0) ->
    expand_single(Cs, L, [C | As], L0);
expand_single([], L, _, L0) ->
    throw_error(L0, {"`-quote ended unexpectedly at line ~w", [L]}).

%% ``...''

expand_double(Cs, L, As) ->
    expand_double(Cs, L, As, L).

expand_double([$', $' | Cs], L, As, _L0) ->
    expand(Cs, L, ">edoc/<" ++ edoc_lib:strip_space(As));
expand_double([$< | Cs], L, As, L0) ->
    expand_double(Cs, L, ";tl&" ++ As, L0);
expand_double([$> | Cs], L, As, L0) ->
    expand_double(Cs, L, ";tg&" ++ As, L0);
expand_double([$& | Cs], L, As, L0) ->
    expand_double(Cs, L, ";pma&" ++ As, L0);
expand_double([$\n = C | Cs], L, As, L0) ->
    expand_double(Cs, L + 1, [C | As], L0);
expand_double([C | Cs], L, As, L0) ->
    expand_double(Cs, L, [C | As], L0);
expand_double([], L, _, L0) ->
    throw_error(L0, {"``-quote ended unexpectedly at line ~w", [L]}).

%% ```...'''

expand_triple(Cs, L, As) ->
    expand_triple(Cs, L, As, L).

expand_triple([$', $', $' | Cs], L, As, _L0) ->      % ' stupid emacs
    expand(Cs, L, ">erp/<>]]" ++ edoc_lib:strip_space(As));
expand_triple([$], $], $> | Cs], L, As, L0) ->
    expand_triple(Cs, L, ";tg&]]" ++ As, L0);
expand_triple([$\n = C | Cs], L, As, L0) ->
    expand_triple(Cs, L + 1, [C | As], L0);
expand_triple([C | Cs], L, As, L0) ->
    expand_triple(Cs, L, [C | As], L0);
expand_triple([], L, _, L0) ->
    throw_error(L0, {"```-quote ended unexpectedly at line ~w", [L]}).

%% e.g. [file:/...] or [http://... LinkText]

expand_uri("http:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:ptth", As);
expand_uri("https:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:sptth", As);
expand_uri("ftp:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:ptf", As);
expand_uri("file:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:elif", As);
expand_uri("mailto:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:otliam", As);
expand_uri("nfs:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:sfn", As);
expand_uri("shttp:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:ptths", As);
expand_uri("xmpp:/" ++ Cs, L, As) ->
    expand_uri(Cs, L, "/:ppmx", As);
expand_uri(Cs, L, As) ->
    expand(Cs, L, [$[ | As]).

expand_uri([$] | Cs], L, Us, As) ->
    expand(Cs, L, push_uri(Us, ">tt/<" ++ Us ++ ">tt<", As));
expand_uri([$\s = C | Cs], L, Us, As) ->
    expand_uri(Cs, 0, L, [C], Us, As);
expand_uri([$\t = C | Cs], L, Us, As) ->
    expand_uri(Cs, 0, L, [C], Us, As);
expand_uri([$\n = C | Cs], L, Us, As) ->
    expand_uri(Cs, 1, L, [C], Us, As);
expand_uri([C | Cs], L, Us, As) ->
    expand_uri(Cs, L, [C | Us], As);
expand_uri([], L, Us, _As) ->
    expand_uri_error(Us, L).

expand_uri([$] | Cs], N, L, Ss, Us, As) ->
    Ss1 = lists:reverse(edoc_lib:strip_space(
			  lists:reverse(edoc_lib:strip_space(Ss)))),
    expand(Cs, L + N, push_uri(Us, Ss1, As));
expand_uri([$\n = C | Cs], N, L, Ss, Us, As) ->
    expand_uri(Cs, N + 1, L, [C | Ss], Us, As);
expand_uri([C | Cs], N, L, Ss, Us, As) ->
    expand_uri(Cs, N, L, [C | Ss], Us, As);
expand_uri([], _, L, _Ss, Us, _As) ->
    expand_uri_error(Us, L).

-spec expand_uri_error(list(), pos_integer()) -> no_return().

expand_uri_error(Us, L) ->
    {Ps, _} = edoc_lib:split_at(lists:reverse(Us), $:),
    throw_error(L, {"reference '[~ts:...' ended unexpectedly", [Ps]}).


push_uri(Us, Ss, As) ->
    ">a/<" ++ Ss ++ ">\"pot_\"=tegrat \"" ++ Us ++ "\"=ferh a<" ++ As.


strip_empty_lines(Cs) ->
    strip_empty_lines(Cs, 0).

strip_empty_lines([], N) ->
    {[], N};					% reached the end of input
strip_empty_lines(Cs, N) ->
    {Cs1, Cs2} = edoc_lib:split_at(Cs, $\n),
    case edoc_lib:is_space(Cs1) of
	true ->
	    strip_empty_lines(Cs2, N + 1);
	false ->
	    {Cs, N}
    end.


%% Scanning element content for paragraph breaks (empty lines).
%% Paragraphs are flushed by block level elements.

par(Es) ->
    par(Es, [], []).

par([E=#xmlText{value = Value} | Es], As, Bs) ->
    par_text(Value, As, Bs, E, Es);
par([E=#xmlElement{name = Name} | Es], As, Bs) ->
    %% (Note that paragraphs may not contain any further block-level
    %% elements, including other paragraphs. Tables get complicated.)
    case Name of
	'p'          -> par_flush(Es, [E | As], Bs);
	'hr'         -> par_flush(Es, [E | As], Bs);
	'h1'         -> par_flush(Es, [E | As], Bs);
	'h2'         -> par_flush(Es, [E | As], Bs);
	'h3'         -> par_flush(Es, [E | As], Bs);
	'h4'         -> par_flush(Es, [E | As], Bs);
	'h5'         -> par_flush(Es, [E | As], Bs);
	'h6'         -> par_flush(Es, [E | As], Bs);
	'pre'        -> par_flush(Es, [E | As], Bs);
	'address'    -> par_flush(Es, [E | As], Bs);
	'div'        -> par_flush(Es, [par_elem(E) | As], Bs);
	'blockquote' -> par_flush(Es, [par_elem(E) | As], Bs);
	'form'       -> par_flush(Es, [par_elem(E) | As], Bs);
	'fieldset'   -> par_flush(Es, [par_elem(E) | As], Bs);
	'noscript'   -> par_flush(Es, [par_elem(E) | As], Bs);
	'ul'         -> par_flush(Es, [par_subelem(E) | As], Bs);
	'ol'         -> par_flush(Es, [par_subelem(E) | As], Bs);
	'dl'         -> par_flush(Es, [par_subelem(E) | As], Bs);
	'table'      -> par_flush(Es, [par_subelem(E) | As], Bs);
	_            -> par(Es, [E | As], Bs)
    end;
par([E | Es], As, Bs) ->
    par(Es, [E | As], Bs);
par([], As, Bs) ->
    lists:reverse(As ++ Bs).

par_text(Cs, As, Bs, E, Es) ->
    case ptxt(Cs) of
	none ->
	    %% no blank lines: keep this element as it is
	    par(Es, [E | As], Bs);
	{Cs1, Ss, Cs2} ->
	    Es1 = case Cs1 of
		      [] -> lists:reverse(As);
		      _ -> lists:reverse(As, [E#xmlText{value = Cs1}])
		  end,
	    Bs0 = case Es1 of
		      [] -> Bs;
		      _ -> [#xmlElement{name = p, content = Es1} | Bs]
		  end,
	    Bs1 = [#xmlText{value = Ss} | Bs0],
	    case Cs2 of
		[] ->
		    par(Es, [], Bs1);
		_ ->       
		    par_text(Cs2, [], Bs1, #xmlText{value = Cs2}, Es)
	    end
    end.

par_flush(Es, As, Bs) ->
    par(Es, [], As ++ Bs).

par_elem(E) ->
    E#xmlElement{content = par(E#xmlElement.content)}.

%% Only process content of subelements; ignore immediate content.
par_subelem(E) ->
    E#xmlElement{content = par_subelem_1(E#xmlElement.content)}.

par_subelem_1([E=#xmlElement{name = Name} | Es]) ->
    E1 = case par_skip(Name) of
	     true ->
		 E;
	     false ->
		 case par_sub(Name) of
		     true ->
			 par_subelem(E);
		     false ->
			 par_elem(E)
		 end
	 end,
    [E1 | par_subelem_1(Es)];
par_subelem_1([E | Es]) ->
    [E | par_subelem_1(Es)];
par_subelem_1([]) ->
    [].

par_skip('caption') -> true;
par_skip('col') -> true;
par_skip('colgroup') -> true;
par_skip(_) -> false.

par_sub(tr) -> true;
par_sub(thead) -> true;
par_sub(tfoot) -> true;
par_sub(tbody) -> true;
par_sub(_) -> false.


%% scanning text content for a blank line

ptxt(Cs) ->
    ptxt(Cs, []).

ptxt([$\n | Cs], As) ->
    ptxt_1(Cs, As, [$\n]);
ptxt([C | Cs], As) ->
    ptxt(Cs, [C | As]);
ptxt([], _As) ->
    none.

%% scanning text following an initial newline
ptxt_1([C=$\s | Cs], As, Ss) ->
    ptxt_1(Cs, As, [C | Ss]);
ptxt_1([C=$\t | Cs], As, Ss) ->
    ptxt_1(Cs, As, [C | Ss]);
ptxt_1([C=$\n | Cs], As, Ss) ->
    %% blank line detected
    ptxt_2(Cs, As, [C | Ss]);
ptxt_1(Cs, As, Ss) ->
    %% not a blank line
    ptxt(Cs, lists:reverse(Ss, As)).

%% collecting whitespace following a blank line
ptxt_2([C=$\s | Cs], As, Ss) ->
    ptxt_2(Cs, As, [C | Ss]);
ptxt_2([C=$\t | Cs], As, Ss) ->
    ptxt_2(Cs, As, [C | Ss]);
ptxt_2([C=$\n | Cs], As, Ss) ->
    ptxt_2(Cs, As, [C | Ss]);
ptxt_2(Cs, As, Ss) ->
    %% ended by non-whitespace or end of element
    case edoc_lib:is_space(As) of
	true ->
	    {[], lists:reverse(Ss ++ As), Cs};
	false ->
	    {lists:reverse(As), lists:reverse(Ss), Cs}
    end.


-spec throw_error(non_neg_integer(), {string(), [_]}) -> no_return().

throw_error(L, D) ->
    throw({error, L, D}).
