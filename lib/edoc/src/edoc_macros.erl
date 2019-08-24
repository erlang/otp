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
%% @copyright 2001-2005 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc EDoc macro expansion

-module(edoc_macros).

-export([expand_tags/3, std_macros/1, check_defs/1]).

-import(edoc_report, [report/2, error/3, warning/4]).

-include("edoc.hrl").
-include("edoc_types.hrl").

-define(DEFAULT_XML_EXPORT, xmerl_html).


std_macros(Env) ->
    (if Env#env.module =:= [] -> [];
	true -> [{module, atom_to_list(Env#env.module)}]
     end
     ++
     [{date, fun date_macro/3},
      {docRoot, Env#env.root},
      {link, fun link_macro/3},
      {section, fun section_macro/3},
      {time, fun time_macro/3},
      {type, fun type_macro/3},
      {version, fun version_macro/3}]).


%% Check well-formedness of user-specified list of macro definitions.

check_defs([{K, D} | Ds]) when is_atom(K), is_list(D) ->
    check_defs(Ds);
check_defs([X | _Ds]) ->
    report("bad macro definition: ~P.", [X, 10]),
    exit(error);
check_defs([]) ->
    ok.

%% Code for special macros should throw {error, Line, Reason} for error
%% reporting, where Reason and Line are passed to edoc_report:error(...)
%% together with the file name etc. The expanded text must be flat!

date_macro(_S, _Line, _Env) ->
    edoc_lib:datestr(date()).    

time_macro(_S, _Line, _Env) ->
    edoc_lib:timestr(time()).

version_macro(S, Line, Env) ->
    date_macro(S, Line, Env)
	++ " " ++ time_macro(S, Line, Env).

link_macro(S, Line, Env) ->
    {S1, S2} = edoc_lib:split_at_stop(S),
    Ref = edoc_parser:parse_ref(S1, Line),
    URI = edoc_refs:get_uri(Ref, Env),
    Txt = if S2 =:= [] -> "<code>" ++ S1 ++ "</code>";
	     true -> S2
	  end,
    Target = case edoc_refs:is_top(Ref, Env) of
		 true -> " target=\"_top\""; % note the initial space
		 false -> ""
	     end,
    lists:flatten(io_lib:fwrite("<a href=\"~ts\"~ts>~ts</a>",
				[URI, Target, Txt])).

section_macro(S, _Line, _Env) ->
    S1 = lists:reverse(edoc_lib:strip_space(
			 lists:reverse(edoc_lib:strip_space(S)))),
    lists:flatten(io_lib:format("<a href=\"#~ts\">~ts</a>",
				[edoc_lib:to_label(S1), S1])).

type_macro(S, Line, Env) ->
    S1 = "t()=" ++ S,
    Def = edoc_parser:parse_typedef(S1, Line),
    {#t_typedef{type = T}, _} = Def,
    Txt = edoc_layout:type(edoc_data:type(T, Env)),
    lists:flatten(io_lib:fwrite("<code>~ts</code>", [Txt])).


%% Expand inline macros in tag content.

expand_tags(Ts, Env, Where) ->
    Defs = dict:from_list(lists:reverse(Env#env.macros)),
    expand_tags(Ts, Defs, Env, Where).

expand_tags([#tag{data = Cs, line = L} = T | Ts], Defs, Env, Where) ->
    [T#tag{data = expand_tag(Cs, L, Defs, Env, Where)}
     | expand_tags(Ts, Defs, Env, Where)];
expand_tags([T | Ts], Defs, Env, Where) ->
    [T | expand_tags(Ts, Defs, Env, Where)];
expand_tags([], _, _, _) ->
    [].

expand_tag(Cs, L, Defs, Env, Where) ->
    case catch {ok, expand_text(Cs, L, Defs, Env, Where)} of
 	{ok, Cs1} ->
	    lists:reverse(Cs1);
 	{'EXIT', R} ->
	    exit(R);
	{error, L1, Error} ->
	    error(L1, Where, Error),
	    exit(error);
	Other ->
	    throw(Other)
    end.

%% Expand macros in arbitrary lines of text.
%% The result is in reverse order.

-record(state, {where, env, seen}).

expand_text(Cs, L, Defs, Env, Where) ->
    St = #state{where = Where,
		env = Env,
		seen = sets:new()},
    expand(Cs, L, Defs, St, []).

%% Inline macro syntax: "{@name content}"
%%   where 'content' is optional, and separated from 'name' by one or
%%   more whitespace characters. The content is bound to the '{@?}'
%%   parameter variable, and the macro definition is expanded and
%%   substituted for the call. Recursion is detected and reported as an
%%   error, since there are (currently) no control flow operators.
%% Escape sequences:
%%   "@{" -> "{"
%%   "@}" -> "}"
%%   "@@" -> "@"

expand([$@, $@ | Cs], L, Defs, St, As) ->
    expand(Cs, L, Defs, St, [$@ | As]);
expand([$@, ${ | Cs], L, Defs, St, As) ->
    expand(Cs, L, Defs, St, [${ | As]);
expand([$@, $} | Cs], L, Defs, St, As) ->
    expand(Cs, L, Defs, St, [$} | As]);
expand([${, $@ | Cs], L, Defs, St, As) ->
    expand_macro(Cs, L, Defs, St, As);
expand([$\n = C | Cs], L, Defs, St, As) ->
    expand(Cs, L + 1, Defs, St, [C | As]);
expand([C | Cs], L, Defs, St, As) ->
    expand(Cs, L, Defs, St, [C | As]);
expand([], _, _, _, As) ->
    As.

expand_macro(Cs, L, Defs, St, As) ->
    {M, Cs1, L1} = macro_name(Cs, L),
    {Arg, Cs2, L2} = macro_content(Cs1, L1),
    As1 = expand_macro_def(M, Arg, L, Defs, St, As),
    expand(Cs2, L2, Defs, St, As1).

%% The macro argument (the "content") is expanded in the environment of
%% the call, and the result is bound to the '{@?}' parameter. The result
%% of the macro expansion is then expanded again. This allows macro
%% definitions to contain calls to other macros, avoids name capture of
%% '{@?}', and makes it easier to write handler functions for special
%% macros such as '{@link ...}', since the argument is already expanded.

expand_macro_def(M, Arg, L, Defs, St, As) ->
    Seen = St#state.seen,
    case sets:is_element(M, Seen) of
	true ->
	    throw_error(L, {"recursive macro expansion of {@~s}.",
			    [M]});
	false ->
	    Arg1 = lists:reverse(expand(Arg, L, Defs, St, [])),
	    Defs1 = dict:store('?', Arg1, Defs),
	    St1 = St#state{seen = sets:add_element(M, Seen)},
	    case dict:find(M, Defs) of
		{ok, Def} ->
		    Txt = if is_function(Def) ->
				  Def(Arg1, L, St1#state.env);
			     is_list(Def) ->
				  Def
			  end,
		    expand(Txt, L, Defs1, St1, As);
		error ->
		    warning(L, St1#state.where,
			    "undefined macro {@~s}.", [M]),
		    "??"
	    end
    end.

%% The macro name ends at the first whitespace or '}' character.  The
%% content, if any, starts at the next non-whitespace character.

%% See edoc_tags:scan_tag/is_name/1 for details on what is a valid
%% name. In macro names we also allow '?' as the initial character.

macro_name(Cs, L) ->
    macro_name(Cs, [], L).

macro_name([C | Cs], As, L) when C >= $a, C =< $z ->
    macro_name_1(Cs, [C | As], L);
macro_name([C | Cs], As, L) when C >= $A, C =< $Z ->
    macro_name_1(Cs, [C | As], L);
macro_name([C | Cs], As, L) when C >= $\300, C =< $\377,
				 C =/= $\327, C =/= $\367 ->
    macro_name_1(Cs, [C | As], L);
macro_name([$_ | Cs], As, L) ->
    macro_name_1(Cs, [$_ | As], L);
macro_name([$? | Cs], As, L) ->
    macro_name_1(Cs, [$? | As], L);
macro_name([$\s | _Cs], _As, L) ->
    throw_error(L, macro_name);
macro_name([$\t | _Cs], _As, L) ->
    throw_error(L, macro_name);
macro_name([$\n | _Cs], _As, L) ->
    throw_error(L, macro_name);
macro_name([C | _Cs], As, L) ->
    throw_error(L, {macro_name, [C | As]});
macro_name([], _As, L) ->
    throw_error(L, macro_name).

macro_name_1([C | Cs], As, L) when C >= $a, C =< $z ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([C | Cs], As, L) when C >= $A, C =< $Z ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([C | Cs], As, L) when C >= $0, C =< $9 ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([C | Cs], As, L) when C >= $\300, C =< $\377,
				   C =/= $\327, C =/= $\367 ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([$_ | Cs], As, L) ->
    macro_name_1(Cs, [$_ | As], L);
macro_name_1([$\s | Cs], As, L) ->
    macro_name_2(Cs, As, L);
macro_name_1([$\t | Cs], As, L) ->
    macro_name_2(Cs, As, L);
macro_name_1([$\n | Cs], As, L) ->
    macro_name_2(Cs, As, L + 1);
macro_name_1([$} | _] = Cs, As, L) ->
    macro_name_3(Cs, As, L);
macro_name_1([C | _Cs], As, L) ->
    throw_error(L, {macro_name, [C | As]});
macro_name_1([], _As, L) ->
    throw_error(L, unterminated_macro).

macro_name_2([$\s | Cs], As, L) ->
    macro_name_2(Cs, As, L);
macro_name_2([$\t | Cs], As, L) ->
    macro_name_2(Cs, As, L);
macro_name_2([$\n | Cs], As, L) ->
    macro_name_2(Cs, As, L + 1);
macro_name_2([_ | _] = Cs, As, L) ->
    macro_name_3(Cs, As, L);
macro_name_2([], _As, L) ->
    throw_error(L, unterminated_macro).

macro_name_3(Cs, As, L) ->
    {list_to_atom(lists:reverse(As)), Cs, L}.


%% The macro content ends at the first non-escaped '}' character that is
%% not balanced by a corresponding non-escaped '{@' sequence.
%% Escape sequences are those defined above.

macro_content(Cs, L) ->
    %% If there is an error, we report the start line, not the end line.
    case catch {ok, macro_content(Cs, [], L, 0)} of
	{ok, X} ->
	    X;
	{'EXIT', R} ->
	    exit(R);
	'end' ->
	    throw_error(L, unterminated_macro);
	Other ->
	    throw(Other)
    end.

%% @throws 'end'

macro_content([$@, $@ | Cs], As, L, N) ->
    macro_content(Cs, [$@, $@ | As], L, N);  % escaped '@'
macro_content([$@, $} | Cs], As, L, N) ->
    macro_content(Cs, [$}, $@ | As], L, N);  % escaped '}'
macro_content([$@, ${ | Cs], As, L, N) ->
    macro_content(Cs, [${, $@ | As], L, N);  % escaped '{'
macro_content([${, $@ | Cs], As, L, N) ->
    macro_content(Cs, [$@, ${ | As], L, N + 1);
macro_content([$} | Cs], As, L, 0) ->
    {lists:reverse(As), Cs, L};
macro_content([$} | Cs], As, L, N) ->
    macro_content(Cs, [$} | As], L, N - 1);
macro_content([$\n = C | Cs], As, L, N) ->
    macro_content(Cs, [C | As], L + 1, N);
macro_content([C | Cs], As, L, N) ->
    macro_content(Cs, [C | As], L, N);
macro_content([], _As, _L, _N) ->
    throw('end').

-type line() :: erl_anno:line().
-type err()  :: 'unterminated_macro'
	      | 'macro_name'
	      | {'macro_name', string()}
	      | {string(), [string()]}.

-spec throw_error(line(), err()) -> no_return().

throw_error(L, unterminated_macro) ->
    throw_error(L, {"unexpected end of macro.", []});
throw_error(L, macro_name) ->
    throw_error(L, {"missing macro name.", []});
throw_error(L, {macro_name, S}) ->
    throw_error(L, {"bad macro name: '@~s...'.", [lists:reverse(S)]});
throw_error(L, D) ->
    throw({error, L, D}).
