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
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Utility functions for EDoc.

-module(edoc_lib).

-export([count/2, lines/1, split_at/2, split_at_stop/1,
	 split_at_space/1, filename/1, transpose/1, segment/2,
	 get_first_sentence/1, is_space/1, strip_space/1, parse_expr/2,
	 parse_contact/2, escape_uri/1, join_uri/2, is_relative_uri/1,
	 is_name/1, to_label/1, find_doc_dirs/0, find_sources/2,
	 find_file/2, try_subdir/2, unique/1,
	 write_file/3, write_file/4, write_info_file/3,
	 read_info_file/1, get_doc_env/1, get_doc_env/3, copy_file/2,
	 uri_get/1, run_doclet/2, run_layout/2,
	 simplify_path/1, timestr/1, datestr/1, read_encoding/2]).

-import(edoc_report, [report/2, warning/2]).

-include("edoc.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(FILE_BASE, "/").


%% ---------------------------------------------------------------------
%% List and string utilities

%% @private
timestr({H,M,Sec}) ->
    lists:flatten(io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w",[H,M,Sec])).

%% @private
datestr({Y,M,D}) ->
    Ms = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
	  "Oct", "Nov", "Dec"],
    lists:flatten(io_lib:fwrite("~s ~w ~w",[lists:nth(M, Ms),D,Y])).

%% @private
read_encoding(File, Options) ->
    case epp:read_encoding(File, Options) of
        none -> epp:default_encoding();
        Encoding -> Encoding
    end.

%% @private
count(X, Xs) ->
    count(X, Xs, 0).

count(X, [X | Xs], N) ->
    count(X, Xs, N + 1);
count(X, [_ | Xs], N) ->
    count(X, Xs, N);
count(_X, [], N) ->
    N.

%% @private
lines(Cs) ->
    lines(Cs, [], []).

lines([$\n | Cs], As, Ls) ->
    lines(Cs, [], [lists:reverse(As) | Ls]);
lines([C | Cs], As, Ls) ->
    lines(Cs, [C | As], Ls);
lines([], As, Ls) ->
    lists:reverse([lists:reverse(As) | Ls]).

%% @private
split_at(Cs, K) ->
    split_at(Cs, K, []).

split_at([K | Cs], K, As) ->
    {lists:reverse(As), Cs};
split_at([C | Cs], K, As) ->
    split_at(Cs, K, [C | As]);
split_at([], _K, As) ->
    {lists:reverse(As), []}.

%% @private
split_at_stop(Cs) ->
    split_at_stop(Cs, []).

split_at_stop([$., $\s | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_stop([$., $\t | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_stop([$., $\n | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_stop([$.], As) ->
    {lists:reverse(As), []};
split_at_stop([C | Cs], As) ->
    split_at_stop(Cs, [C | As]);
split_at_stop([], As) ->
    {lists:reverse(As), []}.

%% @private
split_at_space(Cs) ->
    split_at_space(Cs, []).

split_at_space([$\s | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_space([$\t | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_space([$\n | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_space([C | Cs], As) ->
    split_at_space(Cs, [C | As]);
split_at_space([], As) ->
    {lists:reverse(As), []}.

%% @private
is_space([$\s | Cs]) -> is_space(Cs);
is_space([$\t | Cs]) -> is_space(Cs);
is_space([$\n | Cs]) -> is_space(Cs);
is_space([_C | _Cs]) -> false;
is_space([]) -> true.

%% @private
strip_space([$\s | Cs]) -> strip_space(Cs);
strip_space([$\t | Cs]) -> strip_space(Cs);
strip_space([$\n | Cs]) -> strip_space(Cs);
strip_space(Cs) -> Cs.

%% @private
segment(Es, N) ->
    segment(Es, [], [], 0, N).

segment([E | Es], As, Cs, N, M) when N < M ->
    segment(Es, [E | As], Cs, N + 1, M);
segment([_ | _] = Es, As, Cs, _N, M) ->
    segment(Es, [], [lists:reverse(As) | Cs], 0, M);
segment([], [], Cs, _N, _M) ->
    lists:reverse(Cs);
segment([], As, Cs, _N, _M) ->
    lists:reverse([lists:reverse(As) | Cs]).

%% @private
transpose([]) -> [];
transpose([[] | Xss]) -> transpose(Xss);
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _T] <- Xss]]
     | transpose([Xs | [T || [_H | T] <- Xss]])].

%% Note that the parser will not produce two adjacent text segments;
%% thus, if a text segment ends with a period character, it marks the
%% end of the summary sentence only if it is also the last segment in
%% the list, or is followed by a 'p' or 'br' ("whitespace") element.

%% @private
get_first_sentence([#xmlElement{name = p, content = Es} | _]) ->
    %% Descend into initial paragraph.
    get_first_sentence_1(Es);
get_first_sentence(Es) ->
    get_first_sentence_1(Es).

get_first_sentence_1([E = #xmlText{value = Txt} | Es]) ->
    Last = case Es of
	       [#xmlElement{name = p} | _] -> true;
	       [#xmlElement{name = br} | _] -> true;
	       [] -> true;
	       _ -> false
	   end,
    case end_of_sentence(Txt, Last) of
	{value, Txt1} ->
	    [E#xmlText{value = Txt1}];
	none ->
	    [E | get_first_sentence_1(Es)]
    end;
get_first_sentence_1([E | Es]) ->
    % Skip non-text segments - don't descend further
    [E | get_first_sentence_1(Es)];
get_first_sentence_1([]) ->
    [].

end_of_sentence(Cs, Last) ->
    end_of_sentence(Cs, Last, []).

%% We detect '.' and '!' as end-of-sentence markers.

end_of_sentence([C=$., $\s | _], _, As) ->
    end_of_sentence_1(C, true, As);
end_of_sentence([C=$., $\t | _], _, As) ->
    end_of_sentence_1(C, true, As);
end_of_sentence([C=$., $\n | _], _, As) ->
    end_of_sentence_1(C, true, As);
end_of_sentence([C=$.], Last, As) ->
    end_of_sentence_1(C, Last, As);
end_of_sentence([C=$!, $\s | _], _, As) ->
    end_of_sentence_1(C, true, As);
end_of_sentence([C=$!, $\t | _], _, As) ->
    end_of_sentence_1(C, true, As);
end_of_sentence([C=$!, $\n | _], _, As) ->
    end_of_sentence_1(C, true, As);
end_of_sentence([C=$!], Last, As) ->
    end_of_sentence_1(C, Last, As);
end_of_sentence([C | Cs], Last, As) ->
    end_of_sentence(Cs, Last, [C | As]);
end_of_sentence([], Last, As) ->
    end_of_sentence_1($., Last, strip_space(As)).  % add a '.'

end_of_sentence_1(C, true, As) ->
    {value, lists:reverse([C | As])};
end_of_sentence_1(_, false, _) ->
    none.

%% For handling ISO 8859-1 (Latin-1) we use the following information:
%%
%% 000 - 037	NUL - US	control
%% 040 - 057	SPC - /		punctuation
%% 060 - 071	0 - 9		digit
%% 072 - 100	: - @		punctuation
%% 101 - 132	A - Z		uppercase
%% 133 - 140	[ - `		punctuation
%% 141 - 172	a - z		lowercase
%% 173 - 176	{ - ~		punctuation
%% 177		DEL		control
%% 200 - 237			control
%% 240 - 277	NBSP - ¿	punctuation
%% 300 - 326	À - Ö		uppercase
%% 327		×		punctuation
%% 330 - 336	Ø - Þ		uppercase
%% 337 - 366	ß - ö		lowercase
%% 367		÷		punctuation
%% 370 - 377	ø - ÿ		lowercase

%% Names must begin with a lowercase letter and contain only
%% alphanumerics and underscores.

%% @private
is_name([C | Cs]) when C >= $a, C =< $z ->
    is_name_1(Cs);
is_name([C | Cs]) when C >= $\337, C =< $\377, C =/= $\367 ->
    is_name_1(Cs);
is_name(_) -> false.

is_name_1([C | Cs]) when C >= $a, C =< $z ->
    is_name_1(Cs);
is_name_1([C | Cs]) when C >= $A, C =< $Z ->
    is_name_1(Cs);
is_name_1([C | Cs]) when C >= $0, C =< $9 ->
    is_name_1(Cs);
is_name_1([C | Cs]) when C >= $\300, C =< $\377, C =/= $\327, C =/= $\367 ->
    is_name_1(Cs);
is_name_1([$_ | Cs]) ->
    is_name_1(Cs);
is_name_1([]) -> true;
is_name_1(_) -> false.

%% @private
unique([X | Xs]) -> [X | unique(Xs, X)];
unique([]) -> [].

unique([X | Xs], X) -> unique(Xs, X);
unique([X | Xs], _) -> [X | unique(Xs, X)];
unique([], _) -> [].


%% ---------------------------------------------------------------------
%% Parsing utilities

%% @doc EDoc Erlang expression parsing. For parsing things like the
%% content of <a href="overview-summary.html#ftag-equiv">`@equiv'</a>
%% tags, and strings denoting file names, e.g. in @headerfile. Also used
%% by {@link edoc_run}.
%% @private

parse_expr(S, L) ->
    case erl_scan:string(S ++ ".", L) of
	{ok, Ts, _} ->
	    case erl_parse:parse_exprs(Ts) of
		{ok, [Expr]} ->
		    Expr;
 		{error, {999999, erl_parse, _}} ->
 		    throw_error(eof, L);
 		{error, E} ->
 		    throw_error(E, L)
	    end;
	{error, E, _} ->
	    throw_error(E, L)
    end.


%% @doc EDoc "contact information" parsing. This is the type of the
%% content in e.g.
%% <a href="overview-summary.html#mtag-author">`@author'</a> tags.
%% @private

%% % @type info() = #info{name  = string(),
%% %                      email = string(),
%% %                      uri   = string()}

-record(info, {name = ""  :: string(),
	       email = "" :: string(),
	       uri = ""   :: string()}).

parse_contact(S, L) ->
    I = scan_name(S, L, #info{}, []),
    {I#info.name, I#info.email, I#info.uri}.

%% The name is taken as the first non-whitespace-only string before,
%% between, or following the e-mail/URI sections. Subsequent text that
%% is not e/mail or URI is ignored.

scan_name([$< | Cs], L, I, As) ->
    case I#info.email of
	"" ->
	    {Cs1, I1} = scan_email(Cs, L, set_name(I, As), []),
	    scan_name(Cs1, L, I1, []);
	_ ->
	    throw_error("multiple '<...>' sections.", L)
    end;
scan_name([$[ | Cs], L, I, As) ->
    case I#info.uri of
	"" ->
	    {Cs1, I1} = scan_uri(Cs, L, set_name(I, As), []),
	    scan_name(Cs1, L, I1, []);
	_ ->
	    throw_error("multiple '[...]' sections.", L)
    end;
scan_name([$\n | Cs], L, I, As) ->
    scan_name(Cs, L + 1, I, [$\n | As]);
scan_name([C | Cs], L, I, As) ->
    scan_name(Cs, L, I, [C | As]);
scan_name([], _L, I, As) ->
    set_name(I, As).

scan_uri([$] | Cs], _L, I, As) ->
    {Cs, I#info{uri = strip_and_reverse(As)}};
scan_uri([$\n | Cs], L, I, As) ->
    scan_uri(Cs, L + 1, I, [$\n | As]);
scan_uri([C | Cs], L, I, As) ->
    scan_uri(Cs, L, I, [C | As]);
scan_uri([], L, _I, _As) ->
    throw_error({missing, $]}, L).

scan_email([$> | Cs], _L, I, As) ->
    {Cs, I#info{email = strip_and_reverse(As)}};
scan_email([$\n | Cs], L, I, As) ->
    scan_email(Cs, L + 1, I, [$\n | As]);
scan_email([C | Cs], L, I, As) ->
    scan_email(Cs, L, I, [C | As]);
scan_email([], L, _I, _As) ->
    throw_error({missing, $>}, L).

set_name(I, As) ->
    case I#info.name of
	"" -> I#info{name = strip_and_reverse(As)};
	_ -> I
    end.

strip_and_reverse(As) ->
    edoc_lib:strip_space(lists:reverse(edoc_lib:strip_space(As))).


%% ---------------------------------------------------------------------
%% URI and Internet

%% This is a conservative URI escaping, which escapes anything that may
%% not appear in an NMTOKEN ([a-zA-Z0-9]|'.'|'-'|'_'), including ':'.
%% Characters are first encoded in UTF-8.
%%
%% Note that this should *not* be applied to complete URI, but only to
%% segments that may need escaping, when forming a complete URI.
%%
%% TODO: general utf-8 encoding for all of Unicode (0-16#10ffff)

%% @private
escape_uri([C | Cs]) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C > 16#7f ->
    %% This assumes that characters are at most 16 bits wide.
    escape_byte(((C band 16#c0) bsr 6) + 16#c0)
	++ escape_byte(C band 16#3f + 16#80)
	++ escape_uri(Cs);
escape_uri([C | Cs]) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri([]) ->
    [].

escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].

hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N > 9, N =< 15 ->
    N + $a - 10.

% utf8([C | Cs]) when C > 16#7f ->
%     [((C band 16#c0) bsr 6) + 16#c0, C band 16#3f ++ 16#80 | utf8(Cs)];
% utf8([C | Cs]) ->
%     [C | utf8(Cs)];
% utf8([]) ->
%     [].

%% Please note that URI are *not* file names. Don't use the stdlib
%% 'filename' module for operations on (any parts of) URI.

%% @private
join_uri(Base, "") ->
    Base;
join_uri("", Path) ->
    Path;
join_uri(Base, Path) ->
    Base ++ "/" ++ Path.

%% Check for relative URI; "network paths" ("//...") not included!

%% @private
is_relative_uri([$: | _]) ->
    false;
is_relative_uri([$/, $/ | _]) ->
    false;
is_relative_uri([$/ | _]) ->
    true;
is_relative_uri([$? | _]) ->
    true;
is_relative_uri([$# | _]) ->
    true;
is_relative_uri([_ | Cs]) ->
    is_relative_uri(Cs);
is_relative_uri([]) ->
    true.

%% @private
uri_get("file:///" ++ Path) ->
    uri_get_file(Path);
uri_get("file://localhost/" ++ Path) ->
    uri_get_file(Path);
uri_get("file://" ++ Path) ->
    Msg = io_lib:format("cannot handle 'file:' scheme with "
			"nonlocal network-path: 'file://~ts'.",
			[Path]),
    {error, Msg};
uri_get("file:/" ++ Path) ->
    uri_get_file(Path);
uri_get("file:" ++ Path) ->
    Msg = io_lib:format("ignoring malformed URI: 'file:~ts'.", [Path]),
    {error, Msg};
uri_get("http:" ++ Path) ->
    uri_get_http("http:" ++ Path);
uri_get("ftp:" ++ Path) ->
    uri_get_ftp("ftp:" ++ Path);
uri_get("//" ++ Path) ->
    Msg = io_lib:format("cannot access network-path: '//~ts'.", [Path]),
    {error, Msg};
uri_get([C, $:, $/ | _]=Path) when C >= $A, C =< $Z; C >= $a, C =< $z ->
    uri_get_file(Path);  % special case for Windows
uri_get([C, $:, $\ | _]=Path) when C >= $A, C =< $Z; C >= $a, C =< $z ->
    uri_get_file(Path);  % special case for Windows
uri_get(URI) ->
    case is_relative_uri(URI) of
	true ->
	    uri_get_file(URI);
	false ->
	    Msg = io_lib:format("cannot handle URI: '~ts'.", [URI]),
	    {error, Msg}
    end.

uri_get_file(File0) ->
    File = filename:join(?FILE_BASE, File0),
    case read_file(File) of
	{ok, Text} ->
	    {ok, Text};
	{error, R} ->
	    {error, file:format_error(R)}
    end.

uri_get_http(URI) ->
    %% Try using option full_result=false
    case catch {ok, httpc:request(get, {URI,[]}, [],
				  [{full_result, false}])} of
	{'EXIT', _} ->
	    uri_get_http_r10(URI);
	Result ->
	    uri_get_http_1(Result, URI)
    end.

uri_get_http_r10(URI) ->
    %% Try most general form of request
    Result = (catch {ok, httpc:request(get, {URI,[]}, [], [])}),
    uri_get_http_1(Result, URI).

uri_get_http_1(Result, URI) ->
    case Result of
	{ok, {ok, {200, Text}}} when is_list(Text) ->
	    %% new short result format
	    {ok, Text};
	{ok, {ok, {Status, Text}}} when is_integer(Status), is_list(Text) ->
	    %% new short result format when status /= 200
	    Phrase = httpd_util:reason_phrase(Status),
	    {error, http_errmsg(Phrase, URI)};
	{ok, {ok, {{_Vsn, 200, _Phrase}, _Hdrs, Text}}} when is_list(Text) ->
	    %% new long result format
	    {ok, Text};
	{ok, {ok, {{_Vsn, _Status, Phrase}, _Hdrs, Text}}} when is_list(Text) ->
	    %% new long result format when status /= 200
	    {error, http_errmsg(Phrase, URI)};
	{ok, {200,_Hdrs,Text}} when is_list(Text) ->
	    %% old result format
	    {ok, Text};
	{ok, {Status,_Hdrs,Text}} when is_list(Text) ->
	    %% old result format when status /= 200
	    Phrase = httpd_util:reason_phrase(Status),
	    {error, http_errmsg(Phrase, URI)};
	{ok, {error, R}} ->
	    Reason = inet:format_error(R),
	    {error, http_errmsg(Reason, URI)};
	{ok, R} ->
	    Reason = io_lib:format("bad return value ~tP", [R, 5]),
	    {error, http_errmsg(Reason, URI)};
	{'EXIT', R} ->
	    Reason = io_lib:format("crashed with reason ~tw", [R]),
	    {error, http_errmsg(Reason, URI)};
	R ->
	    Reason = io_lib:format("uncaught throw: ~tw", [R]),
	    {error, http_errmsg(Reason, URI)}
    end.

http_errmsg(Reason, URI) ->
    io_lib:format("http error: ~ts: '~ts'", [Reason, URI]).

%% TODO: implement ftp access method

uri_get_ftp(URI) ->
    Msg = io_lib:format("cannot access ftp scheme yet: '~ts'.", [URI]),
    {error, Msg}.

%% @private
to_label([$\s | Cs]) ->
    to_label(Cs);
to_label([$\t | Cs]) ->
    to_label(Cs);
to_label([$\n | Cs]) ->
    to_label(Cs);
to_label([]) ->
    [];
to_label(Cs) ->
    to_label_1(Cs).

to_label_1([$\s | Cs]) ->
    to_label_2([$\s | Cs]);
to_label_1([$\t | Cs]) ->
    to_label_2([$\s | Cs]);
to_label_1([$\n | Cs]) ->
    to_label_2([$\s | Cs]);
to_label_1([C | Cs]) ->
    [C | to_label_1(Cs)];
to_label_1([]) ->
    [].

to_label_2(Cs) ->
    case to_label(Cs) of
	[] -> [];
	Cs1 -> [$_ | Cs1]
    end.


%% ---------------------------------------------------------------------
%% Files

%% @private
filename([C | T]) when is_integer(C), C > 0 ->
    [C | filename(T)];
filename([H|T]) ->
    filename(H) ++ filename(T);
filename([]) ->
    [];
filename(N) when is_atom(N) ->
    atom_to_list(N);
filename(N) ->
    report("bad filename: `~tP'.", [N, 25]),
    exit(error).

%% @private
copy_file(From, To) ->
    case file:copy(From, To) of
	{ok, _} -> ok;
	{error, R} ->
	    R1 = file:format_error(R),
	    report("error copying '~ts' to '~ts': ~ts.", [From, To, R1]),
	    exit(error)
    end.

list_dir(Dir, Error) ->
    case file:list_dir(Dir) of
	{ok, Fs} ->
	    Fs;
	{error, R} ->
	    F = case Error of
		    %% true ->
		    %%	fun (S, As) -> report(S, As), exit(error) end;
		    false ->
			fun (S, As) -> warning(S, As), [] end
		end,
	    R1 = file:format_error(R),
	    F("could not read directory '~ts': ~ts.", [filename(Dir), R1])
    end.

%% @private
simplify_path(P) ->
    case filename:basename(P) of
	"." ->
	    simplify_path(filename:dirname(P));
	".." ->
	    simplify_path(filename:dirname(filename:dirname(P)));
	_ ->
	    P
    end.

%% The directories From and To are assumed to exist.

%% copy_dir(From, To) ->
%%     Es = list_dir(From, true),    % error if listing fails
%%     lists:foreach(fun (E) -> copy_dir(From, To, E) end, Es).

%% copy_dir(From, To, Entry) ->
%%     From1 = filename:join(From, Entry),
%%     To1 = filename:join(To, Entry),
%%     case filelib:is_dir(From1) of
%% 	true ->
%% 	    make_dir(To1),
%% 	    copy_dir(From1, To1);
%% 	false ->
%% 	    copy_file(From1, To1)
%%     end.

%% make_dir(Dir) ->
%%     case file:make_dir(Dir) of
%% 	ok -> ok;
%% 	{error, R} ->
%% 	    R1 = file:format_error(R),
%% 	    report("cannot create directory '~ts': ~ts.", [Dir, R1]),
%% 	    exit(error)
%%     end.

%% @private
try_subdir(Dir, Subdir) ->
    D = filename:join(Dir, Subdir),
    case filelib:is_dir(D) of
	true -> D;
	false -> Dir
    end.

%% @spec (Text::deep_string(), Dir::edoc:filename(),
%%        Name::edoc:filename()) -> ok
%%
%% @doc Write the given `Text' to the file named by `Name' in directory
%% `Dir'. If the target directory does not exist, it will be created.
%% @private

write_file(Text, Dir, Name) ->
    write_file(Text, Dir, Name, [{encoding,latin1}]).

write_file(Text, Dir, Name, Options) ->
    File = filename:join([Dir, Name]),
    ok = filelib:ensure_dir(File),
    case file:open(File, [write] ++ Options) of
	{ok, FD} ->
	    io:put_chars(FD, Text),
	    ok = file:close(FD);
	{error, R} ->
	    R1 = file:format_error(R),
	    report("could not write file '~ts': ~ts.", [File, R1]),
	    exit(error)
    end.

%% @private
write_info_file(App, Modules, Dir) ->
    Ts = [{modules, Modules}],
    Ts1 = if App =:= ?NO_APP -> Ts;
	     true -> [{application, App} | Ts]
	  end,
    S0 = [io_lib:fwrite("~p.\n", [T]) || T <- Ts1],
    S = ["%% encoding: UTF-8\n" | S0],
    write_file(S, Dir, ?INFO_FILE, [{encoding,unicode}]).

%% @spec (Name::edoc:filename()) -> {ok, string()} | {error, Reason}
%%
%% @doc Reads text from the file named by `Name'.

read_file(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
            Enc = edoc_lib:read_encoding(File, []),
            case catch unicode:characters_to_list(Bin, Enc) of
                String when is_list(String) ->
                    {ok, String};
                _ ->
                    {error, invalid_unicode}
            end;
	{error, Reason} -> {error, Reason}
    end.


%% ---------------------------------------------------------------------
%% Info files

info_file_data(Ts) ->
    App = proplists:get_value(application, Ts, ?NO_APP),
    Ms = proplists:append_values(modules, Ts),
    {App, Ms}.

%% Local file access - don't complain if file does not exist.

%% @private
read_info_file(Dir) ->
    File = filename:join(Dir, ?INFO_FILE),
    case filelib:is_file(File) of
	true ->
	    case read_file(File) of
		{ok, Text} ->
		    parse_info_file(Text, File);
		{error, R} ->
		    R1 = file:format_error(R),
		    warning("could not read '~ts': ~ts.", [File, R1]),
		    {?NO_APP, []}
	    end;
	false ->
	    {?NO_APP, []}
    end.

%% URI access

uri_get_info_file(Base) ->
    URI = join_uri(Base, ?INFO_FILE),
    case uri_get(URI) of
	{ok, Text} ->
	    parse_info_file(Text, URI);
	{error, Msg} ->
	    warning("could not read '~ts': ~ts.", [URI, Msg]),
	    {?NO_APP, []}
    end.

parse_info_file(Text, Name) ->
    case parse_terms(Text) of
	{ok, Vs} ->
	    info_file_data(Vs);
	{error, eof} ->
	    warning("unexpected end of file in '~ts'.", [Name]),
	    {?NO_APP, []};
	{error, {_Line,Module,R}} ->
	    warning("~ts: ~ts.", [Module:format_error(R), Name]),
	    {?NO_APP, []}
    end.

parse_terms(Text) ->
    case erl_scan:string(Text) of
	{ok, Ts, _Line} ->
	    parse_terms_1(Ts, [], []);
	{error, R, _Line} ->
	    {error, R}
    end.

parse_terms_1([T={dot, _L} | Ts], As, Vs) ->
    case erl_parse:parse_term(lists:reverse([T | As])) of
	{ok, V} ->
	    parse_terms_1(Ts, [], [V | Vs]);
	{error, R} ->
	    {error, R}
    end;
parse_terms_1([T | Ts], As, Vs) ->
    parse_terms_1(Ts, [T | As], Vs);
parse_terms_1([], [], Vs) ->
    {ok, lists:reverse(Vs)};
parse_terms_1([], _As, _Vs) ->
    {error, eof}.


%% ---------------------------------------------------------------------
%% Source files

%% @doc See {@link edoc:run/2} for a description of the options
%% `subpackages', `source_suffix'.
%% @private

%% NEW-OPTIONS: subpackages, source_suffix
%% DEFER-OPTIONS: edoc:run/2

find_sources(Path, Opts) ->
    Rec = proplists:get_bool(subpackages, Opts),
    Ext = proplists:get_value(source_suffix, Opts, ?DEFAULT_SOURCE_SUFFIX),
    find_sources(Path, Rec, Ext, Opts).

find_sources(Path, Rec, Ext, _Opts) ->
    lists:flatten(find_sources_1(Path, Rec, Ext)).

find_sources_1([P | Ps], Rec, Ext) ->
    Dir = P,
    Fs1 = find_sources_1(Ps, Rec, Ext),
    case filelib:is_dir(Dir) of
	true ->
	    [find_sources_2(Dir, Rec, Ext) | Fs1];
	false ->
	    Fs1
    end;
find_sources_1([], _Rec, _Ext) ->
    [].

find_sources_2(Dir, Rec, Ext) ->
	Es = list_dir(Dir, false),    % just warn if listing fails
	Es1 = [{E, Dir} || E <- Es, is_source_file(E, Ext)],
	case Rec of
		true ->
			[find_sources_3(Es, Dir, Rec, Ext) | Es1];
		false ->
			Es1
	end.

find_sources_3(Es, Dir, Rec, Ext) ->
    [find_sources_2(filename:join(Dir, E),
		    Rec, Ext)
     || E <- Es, is_source_dir(E, Dir)].

is_source_file(Name, Ext) ->
    (filename:extension(Name) == Ext)
	andalso is_name(filename:rootname(Name, Ext)).

is_source_dir(Name, Dir) ->
    filelib:is_dir(filename:join(Dir, Name)).

%% @private
find_file([P | Ps], Name) ->
    File = filename:join(P, Name),
    case filelib:is_file(File) of
	true ->
	    File;
	false ->
	    find_file(Ps, Name)
    end;
find_file([], _Name) ->
    "".

%% @private
find_doc_dirs() ->
    find_doc_dirs(code:get_path()).

find_doc_dirs([P0 | Ps]) ->
    P = filename:absname(P0),
    P1 = case filename:basename(P) of
	     ?EBIN_DIR ->
		 filename:dirname(P);
	     _ ->
		 P
	 end,
    Dir = try_subdir(P1, ?EDOC_DIR),
    File = filename:join(Dir, ?INFO_FILE),
    case filelib:is_file(File) of
	true ->
	    [Dir | find_doc_dirs(Ps)];
	false ->
	    find_doc_dirs(Ps)
    end;
find_doc_dirs([]) ->
    [].

%% All names with "internal linkage" are mapped to the empty string, so
%% that relative references will be created. For apps, the empty string
%% implies that we use the default app-path.

%% NEW-OPTIONS: doc_path
%% DEFER-OPTIONS: get_doc_env/3

get_doc_links(App, Modules, Opts) ->
    Path = proplists:append_values(doc_path, Opts) ++ find_doc_dirs(),
    Ds = [{P, uri_get_info_file(P)} || P <- Path],
    Ds1 = [{"", {App, Modules}} | Ds],
    D = dict:new(),
    make_links(Ds1, D, D).

make_links([{Dir, {App, Ms}} | Ds], A, M) ->
    A1 = if App == ?NO_APP -> A;
	    true -> add_new(App, Dir, A)
	 end,
    F = fun (K, D) -> add_new(K, Dir, D) end,
    M1 = lists:foldl(F, M, Ms),
    make_links(Ds, A1, M1);
make_links([], A,  M) ->
    F = fun (D) ->
		fun (K) ->
			case dict:find(K, D) of
			    {ok, V} -> V;
			    error -> ""
			end
		end
	end,
    {F(A), F(M)}.

add_new(K, V, D) ->
    case dict:is_key(K, D) of
	true ->
	    D;
	false ->
	    dict:store(K, V, D)
    end.

%% @spec (Options::proplist()) -> edoc_env()
%% @equiv get_doc_env([], [], Opts)
%% @private

get_doc_env(Opts) ->
    get_doc_env([], [], Opts).

%% @spec (App, Modules, Options::proplist()) -> edoc_env()
%%     App = [] | atom()
%%     Modules = [atom()]
%%     proplist() = [term()]
%%
%% @type proplist() = //stdlib/proplists:property().
%% @type edoc_env(). Environment information needed by EDoc for
%% generating references. The data representation is not documented.
%%
%% @doc Creates an environment data structure used by parts of EDoc for
%% generating references, etc. See {@link edoc:run/2} for a description
%% of the options `file_suffix', `app_default' and `doc_path'.
%%
%% @see edoc_extract:source/4
%% @see edoc:get_doc/3

%% NEW-OPTIONS: file_suffix, app_default
%% INHERIT-OPTIONS: get_doc_links/4
%% DEFER-OPTIONS: edoc:run/2

get_doc_env(App, Modules, Opts) ->
    Suffix = proplists:get_value(file_suffix, Opts,
				 ?DEFAULT_FILE_SUFFIX),
    AppDefault = proplists:get_value(app_default, Opts, ?APP_DEFAULT),
    Includes = proplists:append_values(includes, Opts),

    {A, M} = get_doc_links(App, Modules, Opts),
    #env{file_suffix = Suffix,
	 apps = A,
	 modules = M,
	 app_default = AppDefault,
	 includes = Includes
	}.

%% ---------------------------------------------------------------------
%% Plug-in modules

%% @doc See {@link edoc:run/2} for a description of the `doclet' option.

%% NEW-OPTIONS: doclet
%% DEFER-OPTIONS: edoc:run/2

%% @private
run_doclet(Fun, Opts) ->
    run_plugin(doclet, ?DEFAULT_DOCLET, Fun, Opts).

%% @doc See {@link edoc:layout/2} for a description of the `layout'
%% option.

%% NEW-OPTIONS: layout
%% DEFER-OPTIONS: edoc:layout/2

%% @private
run_layout(Fun, Opts) ->
    run_plugin(layout, ?DEFAULT_LAYOUT, Fun, Opts).

run_plugin(Name, Default, Fun, Opts) ->
    run_plugin(Name, Name, Default, Fun, Opts).

run_plugin(Name, Key, Default, Fun, Opts) when is_atom(Name) ->
    Module = get_plugin(Key, Default, Opts),
    case catch {ok, Fun(Module)} of
	{ok, Value} ->
	    Value;
	R ->
	    report("error in ~ts '~w': ~tP.", [Name, Module, R, 20]),
	    exit(error)
    end.

get_plugin(Key, Default, Opts) ->
    case proplists:get_value(Key, Opts, Default) of
	M when is_atom(M) ->
	    M;
	Other ->
	    report("bad value for option '~w': ~tP.", [Key, Other, 10]),
	    exit(error)
    end.


%% ---------------------------------------------------------------------
%% Error handling

-type line() :: erl_anno:line().
-type err()  :: 'eof'
	      | {'missing', char()}
	      | {line(), atom(), string()}
	      | string().

-spec throw_error(err(), line()) -> no_return().

throw_error({missing, C}, L) ->
    throw_error({"missing '~c'.", [C]}, L);
throw_error(eof, L) ->
    throw({error,L,"unexpected end of expression."});
throw_error({L, M, D}, _L) ->
    throw({error,L,{format_error,M,D}});
throw_error(D, L) ->
    throw({error, L, D}).
