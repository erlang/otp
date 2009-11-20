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
%% $Id$
%%
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <richardc@it.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc EDoc tag scanning.

%% TODO: tag/macro for including the code of a function as `<pre>'-text.
%% TODO: consider new tag: @license text

-module(edoc_tags).

-export([tags/0, tags/1, tag_names/0, tag_parsers/0, scan_lines/2,
	 filter_tags/3, check_tags/4, parse_tags/4]).

-import(edoc_report, [report/4, warning/4, error/3]).

-include("edoc.hrl").
-include("edoc_types.hrl").


%% Tags are described by {Name, Parser, Flags}.
%%   Name = atom()
%%   Parser = text | xml | (Text,Line,Where) -> term()
%%   Flags = [Flag]
%%   Flag = module | function | package | overview | single
%%
%% Note that the pseudo-tag '@clear' is not listed here.
%% (Cf. the function 'filter_tags'.)
%%
%% Rejected tag suggestions:
%% - @keywords (never up to date; free text search is better)
%% - @uses [modules] (never up to date; false dependencies)
%% - @maintainer (never up to date; duplicates author info)
%% - @contributor (unnecessary; mention in normal documentation)
%% - @creator (unnecessary; already have copyright/author)
%% - @history (never properly updated; use version control etc.)
%% - @category (useless; superseded by keywords or free text search)

tags() ->
    All = [module,footer,function,package,overview],
    [{author, fun parse_contact/4, [module,package,overview]},
     {copyright, text, [module,package,overview,single]},
     {deprecated, xml, [module,function,package,single]},
     {doc, xml,	[module,function,package,overview,single]},
     {docfile, fun parse_file/4, All},
     {'end', text, All},
     {equiv, fun parse_expr/4, [function,single]},
     {headerfile, fun parse_header/4, All},
     {hidden, text, [module,function,single]},
     {param, fun parse_param/4, [function]},
     {private, text, [module,function,single]},
     {reference, xml, [module,footer,package,overview]},
     {returns, xml, [function,single]},
     {see, fun parse_see/4, [module,function,package,overview]},
     {since, text, [module,function,package,overview,single]},
     {spec, fun parse_spec/4, [function,single]},
     {throws, fun parse_throws/4, [function,single]},
     {title, text, [overview,single]},
     {'TODO', xml, All},
     {todo, xml, All},
     {type, fun parse_typedef/4, [module,footer,function]},
     {version, text, [module,package,overview,single]}].

aliases('TODO') -> todo;
aliases(return) -> returns;
aliases(T) -> T.

%% Selecting tags based on flags.
tags(Flag) ->
    [T || {T,_,Fs} <- tags(), lists:member(Flag, Fs)].

%% The set of known tags.
tag_names() ->
    [T || {T,_,_} <- tags()].

%% The pairs of tags and their parsers.
tag_parsers() ->
    [{T,F} || {T,F,_} <- tags()].


%% Scanning lines of comment text.

scan_lines(Ss, L) ->
    lists:reverse(scan_lines(Ss, L, [])).

scan_lines([S | Ss], L, As) ->
    scan_lines(S, Ss, L, As);
scan_lines([], _L, As) ->
    As.

%% Looking for a leading '@', skipping whitespace.
%% Also accept "TODO:" at start of line as equivalent to "@TODO".

scan_lines([$\s | Cs], Ss, L, As) -> scan_lines(Cs, Ss, L, As);
scan_lines([$\t | Cs], Ss, L, As) -> scan_lines(Cs, Ss, L, As);
scan_lines([$@ | Cs], Ss, L, As) -> scan_tag(Cs, Ss, L, As, []);
scan_lines(("TODO:"++_)=Cs, Ss, L, As) -> scan_tag(Cs, Ss, L, As, []);
scan_lines(_, Ss, L, As) -> scan_lines(Ss, L + 1, As).

%% Scanning chars following '@', accepting only nonempty valid names.
%% See edoc_lib:is_name/1 for details on what is a valid name. In tags
%% we also allow the initial letter to be uppercase or underscore.

scan_tag([C | Cs], Ss, L, As, Ts) when C >= $a, C =< $z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag([C | Cs], Ss, L, As, Ts) when C >= $A, C =< $Z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag([C | Cs], Ss, L, As, Ts) when C >= $\300, C =< $\377,
					 C =/= $\327, C =/= $\367 ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag([$_ | Cs], Ss, L, As, Ts) ->
    scan_tag_1(Cs, Ss, L, As, [$_ | Ts]);
scan_tag(_Cs, Ss, L, As, _Ts) ->
    scan_lines(Ss, L + 1, As).    % not a valid name

scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $a, C =< $z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $A, C =< $Z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $0, C =< $9 ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $\300, C =< $\377,
					 C =/= $\327, C =/= $\367 ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([$_ | Cs], Ss, L, As, Ts) ->
    scan_tag_1(Cs, Ss, L, As, [$_ | Ts]);
scan_tag_1(Cs, Ss, L, As, Ts) ->
    scan_tag_2(Cs, Ss, L, As, {Ts, L}).

%% Check that the tag is followed by whitespace, linebreak, or colon.

scan_tag_2([$\s | Cs], Ss, L, As, T) ->
    scan_tag_lines(Ss, T, [Cs], L + 1, As);
scan_tag_2([$\t | Cs], Ss, L, As, T) ->
    scan_tag_lines(Ss, T, [Cs], L + 1, As);
scan_tag_2([$: | Cs], Ss, L, As, T) ->
    scan_tag_lines(Ss, T, [Cs], L + 1, As);
scan_tag_2([], Ss, L, As, T) ->
    scan_tag_lines(Ss, T, [[]], L + 1, As);
scan_tag_2(_, Ss, L, As, _T) ->
    scan_lines(Ss, L + 1, As).

%% Scanning lines after a tag is found.

scan_tag_lines([S | Ss], T, Ss1, L, As) ->
    scan_tag_lines(S, S, Ss, T, Ss1, L, As);
scan_tag_lines([], {Ts, L1}, Ss1, _L, As) ->
    [make_tag(Ts, L1, Ss1) | As].

%% Collecting tag text lines until end of comment or next tagged line.

scan_tag_lines([$\s | Cs], S, Ss, T, Ss1, L, As) ->
    scan_tag_lines(Cs, S, Ss, T, Ss1, L, As);
scan_tag_lines([$\t | Cs], S, Ss, T, Ss1, L, As) ->
    scan_tag_lines(Cs, S, Ss, T, Ss1, L, As);
scan_tag_lines([$@, C | _Cs], S, Ss, {Ts, L1}, Ss1, L, As)
  when C >= $a, C =< $z ->
    scan_lines(S, Ss, L, [make_tag(Ts, L1, Ss1) | As]);
scan_tag_lines([$@, C | _Cs], S, Ss, {Ts, L1}, Ss1, L, As)
  when C >= $A, C =< $Z ->
    scan_lines(S, Ss, L, [make_tag(Ts, L1, Ss1) | As]);
scan_tag_lines([$@, C | _Cs], S, Ss, {Ts, L1}, Ss1, L, As)
  when C >= $\300, C =< $\377, C =/= $\327, C =/= $\367 ->
    scan_lines(S, Ss, L, [make_tag(Ts, L1, Ss1) | As]);
scan_tag_lines("TODO:"++_, S, Ss, {Ts, L1}, Ss1, L, As) ->
    scan_lines(S, Ss, L, [make_tag(Ts, L1, Ss1) | As]);
scan_tag_lines(_Cs, S, Ss, T, Ss1, L, As) ->
    scan_tag_lines(Ss, T, [S | Ss1], L + 1, As).

make_tag(Cs, L, Ss) ->
    #tag{name = aliases(list_to_atom(lists:reverse(Cs))),
	 line = L,
	 data = append_lines(lists:reverse(Ss))}.

%% Flattening lines of text and inserting line breaks.

append_lines([L]) -> L;
append_lines([L | Ls]) -> L ++ [$\n | append_lines(Ls)];
append_lines([]) -> [].

%% Filtering out unknown tags.

filter_tags(Ts, Tags, Where) ->
    filter_tags(Ts, Tags, Where, []).

filter_tags([#tag{name = clear} | Ts], Tags, Where, _Ts1) ->
    filter_tags(Ts, Tags, Where);
filter_tags([#tag{name = N, line = L} = T | Ts], Tags, Where, Ts1) ->
    case sets:is_element(N, Tags) of
	true ->
	    filter_tags(Ts, Tags, Where, [T | Ts1]);
	false ->
	    warning(L, Where, "tag @~s not recognized.", [N]),
	    filter_tags(Ts, Tags, Where, Ts1)
    end;
filter_tags([], _, _, Ts) ->
    lists:reverse(Ts).

%% Check occurrances of tags.

check_tags(Ts, Allow, Single, Where) ->
    check_tags(Ts, Allow, Single, Where, false, sets:new()).

check_tags([#tag{name = T, line = L} | Ts], Allow, Single, Where, Error, Seen) ->
    case sets:is_element(T, Seen) of
	true ->
	    case sets:is_element(T, Single) of
		false ->
		    check_tags(Ts, Allow, Single, Where, Error, Seen);
		true ->
		    report(L, Where, "multiple @~s tag.", [T]),
		    check_tags(Ts, Allow, Single, Where, true, Seen)
	    end;
	false ->
	    Seen1 = sets:add_element(T, Seen),
	    case sets:is_element(T, Allow) of
		true ->
		    check_tags(Ts, Allow, Single, Where, Error, Seen1);
		false ->
		    report(L, Where, "tag @~s not allowed here.", [T]),
		    check_tags(Ts, Allow, Single, Where, true, Seen1)
	    end
    end;
check_tags([], _, _, _, Error, _) ->
    Error.


%% Parses tag contents for specific tags.

parse_tags(Ts, How, Env, Where) ->
    parse_tags(Ts, How, Env, Where, []).

parse_tags([#tag{name = Name} = T | Ts], How, Env, Where, Ts1) ->
    case dict:fetch(Name, How) of
	text ->
	    parse_tags(Ts, How, Env, Where, [T | Ts1]);
	xml ->
	    [T1] = parse_tag(T, fun parse_xml/4, Env, Where),
	    parse_tags(Ts, How, Env, Where, [T1 | Ts1]);
	F when is_function(F) ->
	    Ts2 = parse_tag(T, F, Env, Where),
	    parse_tags(Ts, How, Env, Where, lists:reverse(Ts2, Ts1))
    end;
parse_tags([], _How, _Env, _Where, Ts) ->
    lists:reverse(Ts).

parse_tag(T, F, Env, Where) ->
    case catch {ok, F(T#tag.data, T#tag.line, Env, Where)} of
	{ok, Data} ->
	    [T#tag{data = Data}];
	{expand, Ts} ->
	    Ts;
	{error, L, Error} ->
	    error(L, Where, Error),
	    exit(error);
	{'EXIT', R} -> exit(R);
	Other -> throw(Other)
    end.

%% parser functions for the built-in content types. They also perform
%% some sanity checks on the results.

parse_xml(Data, Line, _Env, _Where) ->
    edoc_wiki:parse_xml(Data, Line).

parse_see(Data, Line, _Env, _Where) ->
    edoc_parser:parse_see(Data, Line).

parse_expr(Data, Line, _Env, _Where) ->
    edoc_lib:parse_expr(Data, Line).

parse_spec(Data, Line, _Env, {_, {F, A}} = _Where) ->
    Spec = edoc_parser:parse_spec(Data, Line),
    #t_spec{name = N, type = #t_fun{args = As}} = Spec,
    if length(As) /= A ->
	    throw_error(Line, "@spec arity does not match.");
       true ->
	    case N of
		undefined ->
		    Spec#t_spec{name = #t_name{module = [], name = F}};
		#t_name{module = [], name = F} ->
		    Spec;
		_ ->
		    throw_error(Line, "@spec name does not match.")
	    end
    end.

parse_param(Data, Line, _Env, {_, {_F, _A}} = _Where) ->
    edoc_parser:parse_param(Data, Line).

parse_throws(Data, Line, _Env, {_, {_F, _A}} = _Where) ->
    edoc_parser:parse_throws(Data, Line).

parse_contact(Data, Line, _Env, _Where) ->
    case edoc_lib:parse_contact(Data, Line) of
	{"", "", _URI} ->
	    throw_error(Line, "must specify name or e-mail.");
	Info ->
	    Info
    end.

parse_typedef(Data, Line, _Env, _Where) ->
    Def = edoc_parser:parse_typedef(Data, Line),
    {#t_typedef{name = #t_name{name = T}}, _} = Def,
    case edoc_types:is_predefined(T) of
	true ->
	    throw_error(Line, {"redefining built-in type '~w'.", [T]});
	false ->
	    Def
    end.

parse_file(Data, Line, Env, _Where) ->
    case edoc_lib:parse_expr(Data, Line) of
	{string, _, File0} ->
	    File = edoc_lib:strip_space(File0),
	    case edoc_extract:file(File, module, Env, []) of
		{ok, Ts} ->
		    throw({expand, Ts});
		{error, R} ->
		    throw_error(Line, {read_file, File, R})
	    end;
	_ ->
	    throw_error(Line, file_not_string)
    end.

parse_header(Data, Line, Env, {Where, _}) ->
    parse_header(Data, Line, Env, Where);
parse_header(Data, Line, Env, Where) when is_list(Where) ->
    case edoc_lib:parse_expr(Data, Line) of
	{string, _, File} ->
	    Dir = filename:dirname(Where),
	    Path = Env#env.includes ++ [Dir],
	    case edoc_lib:find_file(Path, "", File) of
		"" ->
		    throw_error(Line, {file_not_found, File});
		File1 ->
		    Ts = edoc_extract:header(File1, Env, []),
		    throw({expand, Ts})
	    end;
	_ ->
	    throw_error(Line, file_not_string)
    end.

throw_error(L, {read_file, File, R}) ->
    throw_error(L, {"error reading file '~s': ~w",
		    [edoc_lib:filename(File), R]});
throw_error(L, {file_not_found, F}) ->
    throw_error(L, {"file not found: ~s", [F]});
throw_error(L, file_not_string) ->
    throw_error(L, "expected file name as a string");
throw_error(L, D) ->
    throw({error, L, D}).
