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
%% =====================================================================
%% @copyright 1997-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

%% @doc Functions for reading comment lines from Erlang source code.

-module(erl_comment_scan).

-export([file/1, join_lines/1, scan_lines/1, string/1]).

-export_type([comment/0]).

%% =====================================================================

-type comment()     :: {Line:: integer(),
                        Column:: integer(),
                        Indentation :: integer(),
                        Text :: [string()]}.
-type commentLine() :: {Line :: integer(),
                        Column :: integer(),
                        Indent :: integer(),
                        Text :: string()}.

%% =====================================================================
%% @spec file(FileName::file:filename()) -> [Comment]
%%
%%	    Comment = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%          Column = integer()
%%          Indentation = integer()
%%          Text = [string()]
%%
%% @doc Extracts comments from an Erlang source code file. Returns a
%% list of entries representing <em>multi-line</em> comments, listed in
%% order of increasing line-numbers. For each entry, `Text'
%% is a list of strings representing the consecutive comment lines in
%% top-down order; the strings contain <em>all</em> characters following
%% (but not including) the first comment-introducing `%'
%% character on the line, up to (but not including) the line-terminating
%% newline.
%%
%% Furthermore, `Line' is the line number and
%% `Column' the left column of the comment (i.e., the column
%% of the comment-introducing `%' character).
%% `Indent' is the indentation (or padding), measured in
%% character positions between the last non-whitespace character before
%% the comment (or the left margin), and the left column of the comment.
%% `Line' and `Column' are always positive
%% integers, and `Indentation' is a nonnegative integer.
%%
%% Evaluation exits with reason `{read, Reason}' if a read
%% error occurred, where `Reason' is an atom corresponding to
%% a Posix error code; see the module {@link //kernel/file} for details.

-spec file(file:filename()) -> [comment()].

file(Name) ->
    Name1 = filename(Name),
    case catch {ok, file:read_file(Name1)} of
	{ok, V} ->
	    case V of
		{ok, B} ->
                    Encoding = epp:read_encoding_from_binary(B),
                    Enc = case Encoding of
                              none -> epp:default_encoding();
                              Enc0 -> Enc0
                          end,
                    case catch unicode:characters_to_list(B, Enc) of
                        String when is_list(String) ->
                            string(String);
                        R when Encoding =:= none ->
                            case
                              catch unicode:characters_to_list(B, latin1)
                            of
                                String when is_list(String) ->
                                    string(String);
                                _ ->
                                    error_read_file(Name1),
                                    exit(R)
                            end;
                        R ->
                            error_read_file(Name1),
                            exit(R)
                    end;
		{error, E} ->
		    error_read_file(Name1),
		    exit({read, E})
	    end;
	{'EXIT', E} ->
	    error_read_file(Name1),
	    exit(E);
	R ->
	    error_read_file(Name1),
	    throw(R)
    end.


%% =====================================================================
%% @spec string(string()) -> [Comment]
%%
%%	    Comment = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%          Column = integer()
%%          Indentation = integer()
%%          Text = [string()]
%%
%% @doc Extracts comments from a string containing Erlang source code.
%% Except for reading directly from a string, the behaviour is the same
%% as for {@link file/1}.
%%
%% @see file/1

-spec string(string()) -> [comment()].

string(Text) ->
    lists:reverse(join_lines(scan_lines(Text))).


%% =====================================================================
%% @spec scan_lines(string()) -> [CommentLine]
%%
%%	    CommentLine = {Line, Column, Indent, Text}
%%	    Line = integer()
%%	    Column = integer()
%%	    Indent = integer()
%%	    Text = string()
%%
%% @doc Extracts individual comment lines from a source code string.
%% Returns a list of comment lines found in the text, listed in order of
%% <em>decreasing</em> line-numbers, i.e., the last comment line in the
%% input is first in the resulting list. `Text' is a single
%% string, containing all characters following (but not including) the
%% first comment-introducing `%' character on the line, up
%% to (but not including) the line-terminating newline. For details on
%% `Line', `Column' and `Indent', see {@link file/1}.

-spec scan_lines(string()) -> [commentLine()].

scan_lines(Text) ->
    scan_lines(Text, 1, 0, 0, []).

scan_lines([$\040 | Cs], L, Col, M, Ack) ->
    scan_lines(Cs, L, Col + 1, M, Ack);
scan_lines([$\t | Cs], L, Col, M, Ack) ->
    scan_lines(Cs, L, tab(Col), M, Ack);
scan_lines([$\n | Cs], L, _Col, _M, Ack) ->
    scan_lines(Cs, L + 1, 0, 0, Ack);
scan_lines([$\r, $\n | Cs], L, _Col, _M, Ack) ->
    scan_lines(Cs, L + 1, 0, 0, Ack);
scan_lines([$\r | Cs], L, _Col, _M, Ack) ->
    scan_lines(Cs, L + 1, 0, 0, Ack);
scan_lines([$% | Cs], L, Col, M, Ack) ->
    scan_comment(Cs, "", L, Col, M, Ack);
scan_lines([$$ | Cs], L, Col, _M, Ack) ->
    scan_char(Cs, L, Col + 1, Ack);
scan_lines([$" | Cs], L, Col, _M, Ack) ->
    scan_string(Cs, $", L, Col + 1, Ack);
scan_lines([$' | Cs], L, Col, _M, Ack) ->
    scan_string(Cs, $', L, Col + 1, Ack);
scan_lines([_C | Cs], L, Col, _M, Ack) ->
    N = Col + 1,
    scan_lines(Cs, L, N, N, Ack);
scan_lines([], _L, _Col, _M, Ack) ->
    Ack.

tab(Col) ->
    Col - (Col rem 8) + 8.

scan_comment([$\n | Cs], Cs1, L, Col, M, Ack) ->
    seen_comment(Cs, Cs1, L, Col, M, Ack);
scan_comment([$\r, $\n | Cs], Cs1, L, Col, M, Ack) ->
    seen_comment(Cs, Cs1, L, Col, M, Ack);
scan_comment([$\r | Cs], Cs1, L, Col, M, Ack) ->
    seen_comment(Cs, Cs1, L, Col, M, Ack);
scan_comment([C | Cs], Cs1, L, Col, M, Ack) ->
    scan_comment(Cs, [C | Cs1], L, Col, M, Ack);
scan_comment([], Cs1, L, Col, M, Ack) ->
    seen_comment([], Cs1, L, Col, M, Ack).

%% Add a comment line to the ackumulator and return to normal
%% scanning. Note that we compute column positions starting at 0
%% internally, but the column values in the comment descriptors
%% should start at 1.

seen_comment(Cs, Cs1, L, Col, M, Ack) ->
    %% Compute indentation and strip trailing spaces
    N = Col - M,
    Text = lists:reverse(string:trim(Cs1, leading)),
    Ack1 = [{L, Col + 1, N, Text} | Ack],
    scan_lines(Cs, L + 1, 0, 0, Ack1).

scan_string([Quote | Cs], Quote, L, Col, Ack) ->
    N = Col + 1,
    scan_lines(Cs, L, N, N, Ack);
scan_string([$\t | Cs], Quote, L, Col, Ack) ->
    scan_string(Cs, Quote, L, tab(Col), Ack);
scan_string([$\n | Cs], Quote, L, _Col, Ack) ->
    %% Newlines should really not occur in strings/atoms, but we
    %% want to be well behaved even if the input is not.
    scan_string(Cs, Quote, L + 1, 0, Ack);
scan_string([$\r, $\n | Cs], Quote, L, _Col, Ack) ->
    scan_string(Cs, Quote, L + 1, 0, Ack);
scan_string([$\r | Cs], Quote, L, _Col, Ack) ->
    scan_string(Cs, Quote, L + 1, 0, Ack);
scan_string([$\\, _C | Cs], Quote, L, Col, Ack) ->
    scan_string(Cs, Quote, L, Col + 2, Ack);  % ignore character C
scan_string([_C | Cs], Quote, L, Col, Ack) ->
    scan_string(Cs, Quote, L, Col + 1, Ack);
scan_string([], _Quote, _L, _Col, Ack) ->
    %% Finish quietly.
    Ack.

scan_char([$\t | Cs], L, Col, Ack) ->
    N = tab(Col),
    scan_lines(Cs, L, N, N, Ack);    % this is not just any whitespace
scan_char([$\n | Cs], L, _Col, Ack) ->
    scan_lines(Cs, L + 1, 0, 0, Ack);    % handle this, just in case
scan_char([$\r, $\n | Cs], L, _Col, Ack) ->
    scan_lines(Cs, L + 1, 0, 0, Ack);
scan_char([$\r | Cs], L, _Col, Ack) ->
    scan_lines(Cs, L + 1, 0, 0, Ack);
scan_char([$\\, _C | Cs], L, Col, Ack) ->
    N = Col + 2,    % character C must be ignored
    scan_lines(Cs, L, N, N, Ack);
scan_char([_C | Cs], L, Col, Ack) ->
    N = Col + 1,    % character C must be ignored
    scan_lines(Cs, L, N, N, Ack);
scan_char([], _L, _Col, Ack) ->
    %% Finish quietly.
    Ack.


%% =====================================================================
%% @spec join_lines([CommentLine]) -> [Comment]
%%
%%	    CommentLine = {Line, Column, Indent, string()}
%%	    Line = integer()
%%	    Column = integer()
%%	    Indent = integer()
%%	    Comment = {Line, Column, Indent, Text}
%%	    Text = [string()]
%%
%% @doc Joins individual comment lines into multi-line comments. The
%% input is a list of entries representing individual comment lines,
%% <em>in order of decreasing line-numbers</em>; see
%% {@link scan_lines/1} for details. The result is a list of
%% entries representing <em>multi-line</em> comments, <em>still listed
%% in order of decreasing line-numbers</em>, but where for each entry,
%% `Text' is a list of consecutive comment lines in order of
%% <em>increasing</em> line-numbers (i.e., top-down).
%%
%% @see scan_lines/1

-spec join_lines([commentLine()]) -> [comment()].

join_lines([{L, Col, Ind, Txt} | Lines]) ->
    join_lines(Lines, [Txt], L, Col, Ind);
join_lines([]) ->
    [].

%% In the following, we assume that the current `Txt' is never empty.
%% Recall that the list is in reverse line-number order.

join_lines([{L1, Col1, Ind1, Txt1} | Lines], Txt, L, Col, Ind) ->
    if L1 =:= L - 1, Col1 =:= Col, Ind + 1 =:= Col ->
	    %% The last test above checks that the previous
	    %% comment was alone on its line; otherwise it won't
	    %% be joined with the current; this is not always what
	    %% one wants, but works well in general.
	    join_lines(Lines, [Txt1 | Txt], L1, Col1, Ind1);
       true ->
	    %% Finish the current comment and let the new line
	    %% start the next one.
	    [{L, Col, Ind, Txt}
	     | join_lines(Lines, [Txt1], L1, Col1, Ind1)]
    end;
join_lines([], Txt, L, Col, Ind) ->
    [{L, Col, Ind, Txt}].


%% =====================================================================
%% Utility functions for internal use

filename([C|T]) when is_integer(C), C > 0 ->
    [C | filename(T)];
filename([]) ->
    [];
filename(N) ->
    report_error("bad filename: `~tP'.", [N, 25]),
    exit(error).

error_read_file(Name) ->
    report_error("error reading file `~ts'.", [Name]).

report_error(S, Vs) ->
    error_logger:error_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).

%% =====================================================================
