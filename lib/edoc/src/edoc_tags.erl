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

%% @doc EDoc tag scanning.

%% TODO: tag/macro for including the code of a function as `<pre>'-text.
%% TODO: consider new tag: @license text

-module(edoc_tags).

-export([tags/0, tags/1, tag_names/0, tag_parsers/0, scan_lines/2,
	 filter_tags/2, filter_tags/3, check_tags/4, parse_tags/4,
         check_types/3]).

-import(edoc_report, [report/4, warning/4, error/3]).

-include("edoc.hrl").
-include("edoc_types.hrl").


%% Tags are described by {Name, Parser, Flags}.
%%   Name = atom()
%%   Parser = text | xml | (Text,Line,Where) -> term()
%%   Flags = [Flag]
%%   Flag = module | function | overview | single
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
    All = [module,footer,function,overview],
    [{author, fun parse_contact/4, [module,overview]},
     {copyright, text, [module,overview,single]},
     {deprecated, xml, [module,function,single]},
     {doc, xml,	[module,function,overview,single]},
     {docfile, fun parse_file/4, All},
     {'end', text, All},
     {equiv, fun parse_expr/4, [function,single]},
     {headerfile, fun parse_header/4, All},
     {hidden, text, [module,function,single]},
     {param, fun parse_param/4, [function]},
     {private, text, [module,function,single]},
     {reference, xml, [module,footer,overview]},
     {returns, xml, [function,single]},
     {see, fun parse_see/4, [module,function,overview]},
     {since, text, [module,function,overview,single]},
     {spec, fun parse_spec/4, [function,single]},
     {throws, fun parse_throws/4, [function,single]},
     {title, text, [overview,single]},
     {'TODO', xml, All},
     {todo, xml, All},
     {type, fun parse_typedef/4, [module,footer,function]},
     {version, text, [module,overview,single]}].

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

filter_tags(Ts, Tags) ->
    filter_tags(Ts, Tags, no).

filter_tags(Ts, Tags, Where) ->
    filter_tags(Ts, Tags, Where, []).

filter_tags([#tag{name = clear} | Ts], Tags, Where, _Ts1) ->
    filter_tags(Ts, Tags, Where);
filter_tags([#tag{name = N, line = L} = T | Ts], Tags, Where, Ts1) ->
    case sets:is_element(N, Tags) of
	true ->
	    filter_tags(Ts, Tags, Where, [T | Ts1]);
	false ->
	    case Where of
		no -> ok;
		_ -> warning(L, Where, "tag @~s not recognized.", [N])
	    end,
	    filter_tags(Ts, Tags, Where, Ts1)
    end;
filter_tags([], _, _, Ts) ->
    lists:reverse(Ts).

%% Check occurrences of tags.

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

parse_typedef(Data, Line, _Env, Where) ->
    Def = edoc_parser:parse_typedef(Data, Line),
    {#t_typedef{name = #t_name{name = T}, args = As}, _} = Def,
    NAs = length(As),
    case edoc_types:is_predefined(T, NAs) of
	true ->
            case edoc_types:is_new_predefined(T, NAs) of
                false ->
                    throw_error(Line, {"redefining built-in type '~w'.",
                                       [T]});
                true ->
                    warning(Line, Where, "redefining built-in type '~w'.",
                            [T]),
                    Def
            end;
	false ->
	    Def
    end.

-type line() :: erl_anno:line().

-spec parse_file(_, line(), _, _) -> no_return().

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

-spec parse_header(_, line(), _, _) -> no_return().

parse_header(Data, Line, Env, {Where, _}) ->
    parse_header(Data, Line, Env, Where);
parse_header(Data, Line, Env, Where) when is_list(Where) ->
    case edoc_lib:parse_expr(Data, Line) of
	{string, _, File} ->
	    Dir = filename:dirname(Where),
	    Path = Env#env.includes ++ [Dir],
	    case edoc_lib:find_file(Path, File) of
		"" ->
		    throw_error(Line, {file_not_found, File});
		File1 ->
		    Ts = edoc_extract:header(File1, Env, []),
		    throw({expand, Ts})
	    end;
	_ ->
	    throw_error(Line, file_not_string)
    end.

-type err() :: 'file_not_string'
             | {'file_not_found', file:filename()}
             | {'read_file', file:filename(), term()}
             | string().

-spec throw_error(line(), err()) -> no_return().

throw_error(L, {read_file, File, R}) ->
    throw_error(L, {"error reading file '~ts': ~w",
		    [edoc_lib:filename(File), R]});
throw_error(L, {file_not_found, F}) ->
    throw_error(L, {"file not found: ~ts", [F]});
throw_error(L, file_not_string) ->
    throw_error(L, "expected file name as a string");
throw_error(L, D) ->
    throw({error, L, D}).

%% Checks local types.

-record(parms, {tab, warn, file, line}).

check_types(Entries, Opts, File) ->
    Tags = edoc_data:get_all_tags(Entries),
    TypeTags = [Tag || #tag{data = {#t_typedef{},_}}=Tag <- Tags],
    Entries2 = edoc_data:hidden_filter(Entries, Opts),
    Tags2 = edoc_data:get_all_tags(Entries2),
    SpecTags = [Tag || #tag{data = #t_spec{}}=Tag <- Tags2],
    DT = ets:new(types, [bag]),
    _ = [add_type(DT, Name, As, File, Line) ||
            #tag{line = Line,
                 data = {#t_typedef{name = Name, args = As},_}} <- TypeTags],
    Warn = proplists:get_value(report_missing_types, Opts,
                               ?REPORT_MISSING_TYPES) =:= true,
    P = #parms{tab = DT, warn = Warn, file = File, line = 0},
    try check_types3(TypeTags++SpecTags, P, [])
    after true = ets:delete(DT)
    end.

add_type(DT, Name, Args, File, Line) ->
    NArgs = length(Args),
    TypeName = {Name, NArgs},
    case lists:member(TypeName, ets:lookup(DT, Name)) of
        true ->
            #t_name{name = N} = Name,
            type_warning(Line, File, "duplicated type", N, NArgs);
        false ->
            ets:insert(DT, {Name, NArgs})
    end.

check_types3([], _P, _Ls)->
    ok;
check_types3([Tag | Tags], P, Ls) ->
    check_type(Tag, P, Ls, Tags).

check_type(#tag{line = L, data = Data}, P0, Ls, Ts) ->
    P = P0#parms{line = L},
    case Data of
        {#t_typedef{type = Type, defs = Defs},_} ->
            check_type(Type, P, Ls, Defs++Ts);
        #t_spec{type = Type, defs = Defs} ->
            LocalTypes =
                [{N,length(Args)} ||
                    #t_def{name = #t_type{name = N, args = Args}} <- Defs],
            check_type(Type, P, LocalTypes, Defs),
            check_types3(Ts, P, Ls);
        _->
            check_types3(Ts, P0, Ls)
    end;
check_type(#t_def{type = Type}, P, Ls, Ts) ->
    check_type(Type, P, Ls, Ts);
check_type(#t_type{name = Name, args = Args}, P, Ls, Ts) ->
    check_used_type(Name, Args, P, Ls),
    check_types3(Args++Ts, P, Ls);
check_type(#t_var{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_fun{args = Args, range = Range}, P, Ls, Ts) ->
    check_type(Range, P, Ls, Args++Ts);
check_type(#t_map{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_tuple{types = Types}, P, Ls, Ts) ->
    check_types3(Types ++Ts, P, Ls);
check_type(#t_list{type = Type}, P, Ls, Ts) ->
    check_type(Type, P, Ls, Ts);
check_type(#t_nil{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_paren{type = Type}, P, Ls, Ts) ->
    check_type(Type, P, Ls, Ts);
check_type(#t_nonempty_list{type = Type}, P, Ls, Ts) ->
    check_type(Type, P, Ls, Ts);
check_type(#t_atom{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_integer{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_integer_range{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_binary{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_float{}, P, Ls, Ts) ->
    check_types3(Ts, P, Ls);
check_type(#t_union{types = Types}, P, Ls, Ts) ->
    check_types3(Types++Ts, P, Ls);
check_type(#t_record{fields = Fields}, P, Ls, Ts) ->
    check_types3(Fields++Ts, P, Ls);
check_type(#t_field{type = Type}, P, Ls, Ts) ->
    check_type(Type, P, Ls, Ts);
check_type(undefined, P, Ls, Ts) ->
    check_types3(Ts, P, Ls).

check_used_type(#t_name{name = N, module = Mod}=Name, Args, P, LocalTypes) ->
    NArgs = length(Args),
    TypeName = {Name, NArgs},
    DT = P#parms.tab,
    case
        Mod =/= []
        orelse lists:member(TypeName, ets:lookup(DT, Name))
        orelse edoc_types:is_predefined(N, NArgs)
        orelse lists:member(TypeName, LocalTypes)
    of
        true ->
            ok;
        false ->
            #parms{warn = W, line = L, file = File} = P,
            %% true = ets:insert(DT, TypeName),
            _ = [type_warning(L, File, "missing type", N, NArgs) || W],
	    ok
    end.

type_warning(Line, File, S, N, NArgs) ->
    AS = ["/"++integer_to_list(NArgs) || NArgs > 0],
    warning(Line, File, S++" ~w~s", [N, AS]).
