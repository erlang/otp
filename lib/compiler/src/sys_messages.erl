%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2020-2024 Facebook, Inc. and its affiliates.
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

-module(sys_messages).
-moduledoc false.

-export([format_messages/4, list_errors/3]).

-type loc() :: erl_anno:location().
-type message() :: {Where :: none | {File::string(), loc()}, Text :: iolist()}.

-spec format_messages(File::string(), Prefix::string(), [erl_lint:error_info()],
                      Opts::[term()]) -> [message()].

format_messages(F, P, [{none, Mod, E} | Es], Opts) ->
    M = {none, io_lib:format("~ts: ~s~ts\n", [F, P, Mod:format_error(E)])},
    [M | format_messages(F, P, Es, Opts)];
format_messages(F, P, [{Loc, Mod, E} | Es], Opts) ->
    StartLoc = Loc,
    EndLoc = StartLoc,
    Src = quote_source(F, StartLoc, EndLoc, Opts),
    Msg = io_lib:format("~ts:~ts: ~s~ts\n~ts", [
        F,
        fmt_pos(StartLoc),
        P,
        Mod:format_error(E),
        Src
    ]),
    Pos = StartLoc,
    [{{F, Pos}, Msg} | format_messages(F, P, Es, Opts)];
format_messages(_, _, [], _Opts) ->
    [].

-spec list_errors(File::string(), [erl_lint:error_info()], Opts::[term()]) -> ok.

list_errors(F, [{none, Mod, E} | Es], Opts) ->
    io:fwrite("~ts: ~ts\n", [F, Mod:format_error(E)]),
    list_errors(F, Es, Opts);
list_errors(F, [{Loc, Mod, E} | Es], Opts) ->
    StartLoc = Loc,
    EndLoc = StartLoc,
    Src = quote_source(F, StartLoc, EndLoc, Opts),
    io:fwrite("~ts:~ts: ~ts\n~ts", [F, fmt_pos(StartLoc), Mod:format_error(E), Src]),
    list_errors(F, Es, Opts);
list_errors(_F, [], _Opts) ->
    ok.

fmt_pos({Line, Col}) ->
    io_lib:format("~w:~w", [Line, Col]);
fmt_pos(Line) ->
    io_lib:format("~w", [Line]).

quote_source(File, StartLoc, EndLoc, Opts) ->
    case proplists:get_bool(brief, Opts) of
        true -> "";
        false -> quote_source_1(File, StartLoc, EndLoc)
    end.

quote_source_1(File, Line, Loc2) when is_integer(Line) ->
    quote_source_1(File, {Line, 1}, Loc2);
quote_source_1(File, Loc1, Line) when is_integer(Line) ->
    quote_source_1(File, Loc1, {Line, -1});
quote_source_1(File, {StartLine, StartCol}, {EndLine, EndCol}) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Enc = case epp:read_encoding_from_binary(Bin) of
                      none -> epp:default_encoding();
                      Enc0 -> Enc0
                  end,
            Ctx =
                if
                    StartLine =:= EndLine -> 0;
                    true -> 1
                end,
            case seek_line(Bin, 1, StartLine - Ctx) of
                {ok, Bin1} ->
                    quote_source_2(Bin1, Enc, StartLine, StartCol, EndLine, EndCol, Ctx);
                error ->
                    ""
            end;
        {error, _} ->
            ""
    end.

quote_source_2(Bin, Enc, StartLine, StartCol, EndLine, EndCol, Ctx) ->
    case take_lines(Bin, Enc, StartLine - Ctx, EndLine + Ctx) of
        [] ->
            "";
        Lines ->
            Lines1 =
                case length(Lines) =< (4 + Ctx) of
                    true ->
                        Lines;
                    false ->
                        %% before = context + start line + following line
                        %% after = end line + context
                        %% (total lines: 3 + 1 + context)
                        Before = lists:sublist(Lines, 2 + Ctx),
                        After = lists:reverse(
                            lists:sublist(lists:reverse(Lines), 1 + Ctx)
                        ),
                        Before ++ [{0, "..."}] ++ After
                end,
            Lines2 = decorate(Lines1, StartLine, StartCol, EndLine, EndCol),
            [[fmt_line(L, Text) || {L, Text} <:- Lines2], $\n]
    end.

line_prefix() ->
    "% ".

fmt_line(L, Text) ->
    {LineText, LineTextLength} = line_to_txt(L),
    [line_prefix(),
     io_lib:format("~*.ts| ", [LineTextLength, LineText]),
     Text, "\n"].

line_to_txt(L) ->
    LineText = integer_to_list(abs(L)),
    Length = max(4, length(LineText)),
    if
        L < 0 ->
            {"", Length};
        true ->
            {LineText, Length}
    end.

decorate([{Line, Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) when
  Line =:= StartLine, EndLine =:= StartLine ->
    %% start and end on same line
    S = underline(Text, StartCol, EndCol),
    decorate(S, L, Ls, StartLine, StartCol, EndLine, EndCol);
decorate([{Line, Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) when Line =:= StartLine ->
    %% start with end on separate line
    S = underline(Text, StartCol, string:length(Text) + 1),
    decorate(S, L, Ls, StartLine, StartCol, EndLine, EndCol);
decorate([{_Line, _Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) ->
    [L | decorate(Ls, StartLine, StartCol, EndLine, EndCol)];
decorate([], _StartLine, _StartCol, _EndLine, _EndCol) ->
    [].

%% don't produce empty decoration lines
decorate("", L, Ls, StartLine, StartCol, EndLine, EndCol) ->
    [L | decorate(Ls, StartLine, StartCol, EndLine, EndCol)];
decorate(Text, {Line, _} = L, Ls, StartLine, StartCol, EndLine, EndCol) ->
    [L, {-Line, Text} | decorate(Ls, StartLine, StartCol, EndLine, EndCol)].

%% End typically points to the first position after the actual region.
%% If End = Start, we adjust it to Start+1 to mark at least one character
%% TODO: colorization option
underline(_Text, Start, End) when End < Start ->
    % no underlining at all if end column is unknown
    "";
underline(Text, Start, Start) ->
    underline(Text, Start, Start + 1);
underline(Text, Start, End) ->
    underline(Text, Start, End, 1).

underline([$\t | Text], Start, End, N) when N < Start ->
    [$\t | underline(Text, Start, End, N + 1)];
underline([_ | Text], Start, End, N) when N < Start ->
    [$\s | underline(Text, Start, End, N + 1)];
underline(_Text, _Start, End, N) ->
    underline_1(N, End).

underline_1(N, End) when N < End ->
    [$^ | underline_1(N + 1, End)];
underline_1(_N, _End) ->
    "".

seek_line(Bin, L, L) -> {ok, Bin};
seek_line(<<$\n, Rest/binary>>, N, L) -> seek_line(Rest, N + 1, L);
seek_line(<<$\r, $\n, Rest/binary>>, N, L) -> seek_line(Rest, N + 1, L);
seek_line(<<_, Rest/binary>>, N, L) -> seek_line(Rest, N, L);
seek_line(<<>>, _, _) -> error.

take_lines(<<>>, _Enc, _Here, _To) ->
    [];
take_lines(Bin, Enc, Here, To) when Here =< To ->
    {Text, Rest} = take_line(Bin, <<>>),
    [{Here, text_to_string(Text, Enc)}
     | take_lines(Rest, Enc, Here + 1, To)];
take_lines(_Bin, _Enc, _Here, _To) ->
    [].

text_to_string(Text, Enc) ->
    case unicode:characters_to_list(Text, Enc) of
        String when is_list(String) -> String;
        {error, String, _Rest} -> String;
        {incomplete, String, _Rest} -> String
    end.

take_line(<<$\n, Rest/binary>>, Ack) ->
    {Ack, Rest};
take_line(<<$\r, $\n, Rest/binary>>, Ack) ->
    {Ack, Rest};
take_line(<<B, Rest/binary>>, Ack) ->
    take_line(Rest, <<Ack/binary, B>>);
take_line(<<>>, Ack) ->
    {Ack, <<>>}.
