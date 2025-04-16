%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
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
%%
-module(shell_docs_markdown).

-moduledoc false.
%% this module is expected to be consumed by `shell_docs.erl`.
%% any output from `process_md/1` may not correspond 1-to-1 to
%% a common conversion from markdown to Erlang+HTML.
%% the end goal is that the output can be pretty printed correctly.

-export([parse_md/1]).

%% Valid inline prefixes, e.g., t:my_type()
-define(INLINE_PREFIX(X), (X =:= $t orelse X =:= $m orelse X =:= $e orelse X =:= $c)).

%% Allowed lists and their ways to capture them
-define(IS_BULLET(X), (X =:= $* orelse X =:= $- orelse X =:= $+)).
-define(IS_NUMBERED(X), (is_integer(X) andalso min(0, X) =:= 0)).

%% Parsing format symbols and symbols separators between formats
-define(VALID_BREAK(Symb), (Symb =:= $\s orelse Symb =:= $\n orelse Symb =:= <<>>)).
-define(VALID_SEPARATOR(Symb), (?VALID_BREAK(Symb) orelse ?VALID_PUNCTUATION(Symb))).
-define(VALID_FORMAT(Format), (Format =:= $* orelse Format =:= $_)).
-define(VALID_ESCAPED(Char),
        ((Char) =:= $! orelse (Char) =:= $" orelse (Char) =:= $# orelse (Char) =:= $$
         orelse (Char) =:= $% orelse (Char) =:= $& orelse (Char) =:= $' orelse (Char) =:= $(
         orelse (Char) =:= $\) orelse (Char) =:= $* orelse (Char) =:= $+ orelse (Char) =:= $,
         orelse (Char) =:= $- orelse (Char) =:= $. orelse (Char) =:= $/ orelse (Char) =:= $:
         orelse (Char) =:= $; orelse (Char) =:= $< orelse (Char) =:= $= orelse (Char) =:= $>
         orelse (Char) =:= $? orelse (Char) =:= $@ orelse (Char) =:= $[ orelse (Char) =:= $\\
         orelse (Char) =:= $] orelse (Char) =:= $^ orelse (Char) =:= $_ orelse (Char) =:= $`
         orelse (Char) =:= ${ orelse (Char) =:= $| orelse (Char) =:= $} orelse (Char) =:= $~
       )).
-define(VALID_PUNCTUATION(Symb),
        (Symb =:= $. orelse Symb =:= $, orelse
         Symb =:= $( orelse Symb =:= $) orelse
         Symb =:= $: orelse Symb =:= $;)).

-spec parse_md(Doc0 :: binary()) -> Doc1 :: shell_docs:chunk_elements().
parse_md(Doc) when is_binary(Doc) ->
    %% remove links from [this form][1] to [this form].
    Doc1 = re:replace(Doc, ~b"\\[([^\\]]*?)\\]\\[(.*?)\\]", "\\1", [{return, binary}, global]),

    Lines = binary:split(Doc1, [<<"\r\n">>, <<"\n">>], [global]),
    AST = parse_md(Lines, []),
    format_line(AST).

%% Formats a line
-spec format_line(Input :: shell_docs:chunk_elements()) -> Result :: shell_docs:chunk_elements().
format_line(Ls) ->
    format_line(Ls, sets:new()).

-spec format_line(Input, OmissionSet) -> Result when
      Input :: shell_docs:chunk_elements(),
      Result :: shell_docs:chunk_elements(),
      OmissionSet :: sets:set(atom()).
format_line([], _BlockSet0) ->
    [];
format_line([{Tag, Attrs, List} | Rest], BlockSet0) ->
    case format_line(List, sets:add_element(Tag, BlockSet0)) of
        [] ->
            format_line(Rest, BlockSet0);
        Ls ->
            [{Tag, Attrs, Ls}] ++ format_line(Rest, BlockSet0)
    end;
format_line([Bin | Rest], BlockSet0) when is_binary(Bin) ->
    %% Ignores formatting these elements
    Restriction = sets:from_list([h1, h2, h3, h4, h5, h6, pre]),

    case sets:is_disjoint(Restriction, BlockSet0) of
        true ->
            case format_inline(create_paragraph(Bin)) of
                {p, [], []} ->
                    %% ignore empty paragraphs
                    %% they can happen if the line only contains links
                    %% that are removed, so no content exists
                    format_line(Rest, BlockSet0);
                B ->
                    [B | format_line(Rest, BlockSet0)]
            end;
        false ->
            [Bin | format_line(Rest, BlockSet0)]
    end.


-spec parse_md(Markdown, HtmlErlang) -> HtmlErlang when
      Markdown :: [binary()],
      HtmlErlang :: shell_docs:chunk_elements().
parse_md([], Block) ->
    Block;

parse_md([<<"    ", Line/binary>> | Rest], Block) ->
   Block ++ process_code([<<"    ", Line/binary>> | Rest], []);

%%
%% Lists and paragraphs
%%
parse_md(Rest, Block) when is_list(Rest) ->
    process_rest(Rest, Block).

process_table([<<$|, _Data/binary>>=Header, Delimiter | Rest]) ->
    maybe
        {true, Fields} ?= check_start_closing_table(Header),
        {true, Fields} ?= is_delimiter(Delimiter),
        {DataRows, NotTable} ?= extract_body(Rest, Fields),
        Table = lists:map(fun(Line) -> <<Line/binary, "\n">> end, [Header, Delimiter | DataRows]),
        {[create_table(Table)], NotTable}
    else
        false ->
            error;
        {true, _DiffFieldNumber} ->
            error
    end;
process_table(_) ->
    %% The table either misses a delimiter or data rows,
    %% which makes it an invalid table
    error.


is_delimiter(<<$|, Line/binary>>) ->
    NoSpacesBin = re:replace(Line, ~b"(\s|:)", <<>>, [{return, binary}, global]),
    is_delimiter(NoSpacesBin, 0);
is_delimiter(_A) ->
    false.

is_delimiter(<<>>, Count) ->
    {true, Count};
is_delimiter(<<$|, Line/binary>>, Count) ->
    is_delimiter(Line, Count + 1);
is_delimiter(<<$-, Line/binary>>, Count) ->
    is_delimiter(Line, Count);
is_delimiter(_Bin, _Count) ->
    false.

extract_body([], _Fields) ->
    {[], []};
extract_body([<<>> | Rest], _Fields) ->
    {[], Rest};
extract_body([<<$|, _/binary>>=Line | Rest], Fields) ->
    maybe
        {true, Fields} ?= check_start_closing_table(Line),
        {Row, Rest1} ?= extract_body(Rest, Fields),
        {[Line | Row], Rest1}
    else
        _E ->
            false
    end;
extract_body(Rest, _Fields) when is_list(Rest) ->
    {[], Rest}.


check_start_closing_table(Line) ->
    Line1 = re:replace(Line, ~b"(\s)", <<>>, [{return, binary}, global]),
    FirstBar = binary:first(Line1),
    LastBar = binary:last(Line1),
    case FirstBar =:= $| andalso LastBar =:= $| of
        true ->
            SkipVerbatimSlash = length(binary:matches(Line, ~"\\|")),
            {true, length(binary:matches(Line, ~"|")) - SkipVerbatimSlash - 1};
        false ->
            false
    end.

process_rest([P | Rest], Block) ->
    process_list_or_p(P, Rest, Block).

detect_rest(<<BulletList, $\s, _Line/binary>>) when ?IS_BULLET(BulletList) ->
    ul;
detect_rest(<<NumberedList, $., $\s, _Line/binary>>) when ?IS_NUMBERED(NumberedList) ->
    ol;
detect_rest(_) ->
    p.

process_list_or_p(P, Rest, Block) ->
    {StrippedP, SpaceCount} = strip_spaces(P, 0, infinity),
    case detect_rest(StrippedP) of
        List when List =:= ul;
                  List =:= ol ->
            {Content, Rest1} = process_list(List, P, Rest, SpaceCount, Block),
            Content ++ parse_md(Rest1, []);
        _ ->
            %% Note: It could be that some text has been indented
            %% further than normal. If there is some rendering issue,
            %% maybe here we need to strip P from spaces and re-run parse_md(P).
            %% Not an issue, so far.
            process_kind_block([StrippedP | Rest], Block)
    end.

process_list(Format, LineContent, Rest, SpaceCount, Block) ->
    {Content, Rest1} = create_list(Format, [LineContent | Rest], SpaceCount, Block),
    {Block ++ [create_item_list(Format, lists:reverse(Content))], Rest1}.

create_item_list(ul, Items) when is_list(Items) ->
    ul(Items);
create_item_list(ol, Items) when is_list(Items) ->
    ol(Items).

ul(Items) when is_list(Items) ->
    {ul, [], Items}.

ol(Items) when is_list(Items) ->
    {ol, [], Items}.

li(Items) when is_list(Items)->
    {li, [], Items}.

create_list(Format, [Line | Rest], SpaceCount, Acc) ->
    process_block(Format, [Line | Rest], SpaceCount, Acc).

process_block(_Format, [], _SpaceCount, Acc) ->
    {lists:reverse(Acc), []};
process_block(Format, [Line | Rest], SpaceCount, Acc) ->
    {Content, RemainingRest, Done} = get_next_block(Format, [Line | Rest], SpaceCount, Acc),
    Items = case Content of
                L when L =:= []; L =:= Acc ->
                    [];
                _ ->
                    [li(parse_md(lists:reverse(Content), [])) | Acc]
            end,
    case Done of
        true ->
            {Items, RemainingRest};
        false ->
            {Items2, Rest2} = process_block(Format, RemainingRest, SpaceCount, []),
            {Items2 ++ Items, Rest2}
    end.

get_next_block(_Format, [], _SpaceCount, Acc) ->
    {Acc, [], true};
get_next_block(Format, [Line | Rest], SpaceCount, Acc) ->
    {Stripped, NextCount} = strip_spaces(Line, 0, infinity),
    case detect_next_kind(Format, Stripped, Rest, SpaceCount, NextCount) of
        next ->
            {NewLine, _} = strip_spaces(Line, 0, min(NextCount, 2)),
            %% Try to remove extra space padding from line, so that when
            %% the accumulated lines are processed by process_md,
            %% there is no extra space to take into account.
            %%
            %% Example:
            %%
            %% ~"- ```erlang
            %%     foo() -> ok.
            %%     ```"
            %% is seen by process_md later on as
            %% ~"```erlang
            %%     foo() -> ok.
            %%   ```"
            %% This step tries to to always remove extra space padding.
            %% This is because markdown is not consistent in the spacing rules
            %% and when we mix positioning of list blocks where the space
            %% matters with places where space does not, it becomes ambiguous
            %% to parse correctly.
            %%
            %% Example 2:
            %%
            %% ~" -  First line in list
            %%          second line in list
            %%    and continue here"
            %%
            get_next_block(Format, Rest, SpaceCount, [NewLine | Acc]);
        done ->
            {Acc, [Line | Rest], true};
        list ->
            {Acc, [strip_list_line(Stripped) | Rest], false}
    end.

detect_list_format_change(State, ExpectedFormat, Line, SpaceCount, NextCount) ->
    ListType = detect_rest(Line),
    case ExpectedFormat =:= ListType orelse ListType =:= p of
        false when NextCount =< SpaceCount ->
            done;
        _ ->
            State
    end.

detect_next_kind(Format, Line, Rest, SpaceCount, NextCount) ->
    State = process_next_kind(Line, Rest, SpaceCount, NextCount),
    detect_list_format_change(State, Format, Line, SpaceCount, NextCount).

process_next_kind(<<BulletFormat>>, _, _SpaceCount, _NextCount)
  when ?IS_BULLET(BulletFormat) ->
    %% Important: otherwise it may consider the symbol to mean
    %%            a setext heading underline, when it was a mistake to ignore
    list;
process_next_kind(<<BulletFormat, $\s, _/binary>>, _, SpaceCount, NextCount)
  when ?IS_BULLET(BulletFormat) andalso (NextCount =< SpaceCount) ->
    list;
process_next_kind(<<OrderedFormat, $., $\s, _/binary>>, _, SpaceCount, NextCount)
  when ?IS_NUMBERED(OrderedFormat) andalso  (NextCount =< SpaceCount) ->
    list;
process_next_kind(<<>>, [<<$\s, _/binary>> | _], _SpaceCount, _NextCount) ->
    next;
process_next_kind(<<>>, _, _, _) ->
    done;
process_next_kind(_A, _B, _C, _D) ->
    next.

strip_list_line(Line) ->
    case Line of
        <<BulletFormat>>
          when ?IS_BULLET(BulletFormat) ->
            %% Important: otherwise it may consider the symbol to mean
            %%            a setext heading underline, when it was a mistake to ignore
            <<>>;
        <<BulletFormat, $\s, Continuation/binary>>
          when ?IS_BULLET(BulletFormat) ->
            strip_list_line(Continuation);
        <<OrderedFormat, $., Continuation/binary>>
          when ?IS_NUMBERED(OrderedFormat) ->
            strip_list_line(Continuation);
        <<OrderedFormat, $\s, Continuation/binary>>
          when ?IS_NUMBERED(OrderedFormat) ->
            strip_list_line(Continuation);
        _ ->
            Line
    end.

%%
%% Headings
%%
process_kind_block([<<" ", Line/binary>> | Rest], Block) ->
    process_kind_block([Line | Rest], Block);
process_kind_block([<<"# ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 1,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_kind_block([<<"## ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 2,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_kind_block([<<"### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 3,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_kind_block([<<"#### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 4,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_kind_block([<<"##### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 5,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_kind_block([<<"###### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 6,
    Block ++ process_heading(HeadingLevel, Heading, Rest);

%%
%% Quotes where
%%
process_kind_block([<<">", _/binary>>=Line | Rest], Block) ->
    Block ++ process_quote([Line | Rest], []);

%%
%% process block code
%%
process_kind_block([<<"```", Line/binary>> | Rest], Block) ->
    Block ++ process_fence_code(Rest, [], Line);
%%
%% New line
%%
process_kind_block([<<"">> | Rest], Block) ->
    Block ++ parse_md(Rest, []);
%%
%% Comments
%%
process_kind_block([<<"<!--", Line/binary>> | Rest], Block) ->
    Block ++ parse_md(process_comment([Line | Rest]), []);
%%
%% Tables
%%
process_kind_block([<<$|, _/binary>> | _]=Lines, Block) ->
    maybe
        {Table, Rest1} ?= process_table(Lines),
        Block ++ Table ++ parse_md(Rest1, [])
    else
        error ->
            %% looked like a table but it was not
            %% process as per the remaining option: paragraph
            process_paragraph(Lines, Block)
    end;

process_kind_block([P | Rest], Block) ->
    process_paragraph([P | Rest], Block).

process_paragraph([P | Rest], Block) ->
    case process_p([P | Rest], Block) of
        {[_ | _]=Rest1, Block1} ->
            process_setext_header({Rest1, Block1});
        {Rest2, Block2} ->
            parse_md(Rest2, Block2)
    end.

process_setext_header({[Line | Remaining]=Rest1, [H]=Block1}) ->
    case strip_spaces(Line, 0, infinity) of
        {Stripped, SpaceCount} when SpaceCount =:= 4; Stripped == <<>> ->
            %% new code block, example:
            %%
            %%     foo() -> ok.
            %%
            parse_md(Rest1, Block1);
        {Stripped, _} ->
            HeaderSymbol = binary:first(Stripped),
            maybe
                true ?= repeated_bin_char(Stripped, HeaderSymbol),
                <<$#, _/binary>>=Header ?= header_bin_level(HeaderSymbol),
                HeaderBlock = parse_md([<<Header/binary, H/binary>>], []),
                HeaderBlock ++ parse_md(Remaining, [])
            else
                _ ->
                    parse_md(Rest1, Block1)
            end
    end.

header_bin_level($=) -> <<"# ">>;
header_bin_level($-) -> <<"## ">>;
header_bin_level(_) -> error.

repeated_bin_char(<<>>, _) ->
    true;
repeated_bin_char(<<Char, RestLine/binary>>, Char) ->
    repeated_bin_char(RestLine, Char);
repeated_bin_char(_, _) ->
    false.

process_p([<<>>], Block) when is_list(Block) ->
    {[], Block};
process_p([P | Rest], []) when is_binary(P) ->
    {Rest, [P]};
process_p([<<$\s, P/binary>> | Rest], [Bin | RestBlock]) when is_binary(Bin) ->
    process_p([P | Rest], [Bin | RestBlock]);
process_p([P | Rest], [Bin | RestBlock]) when is_binary(Bin), is_binary(P) ->
    %% merge lines and add space, if required.
    case binary:last(Bin) of
        $\s ->
            {Rest, [<<Bin/binary, P/binary>> | RestBlock]};
        _ ->
            {Rest, [<<Bin/binary, $\s, P/binary>> | RestBlock]}
    end.

strip_spaces(<<" ", Rest/binary>>, Acc, Max) when Max =:= infinity; Acc < Max ->
    strip_spaces(Rest, Acc + 1, Max);
strip_spaces(Rest, Acc, _) ->
    {Rest, Acc}.

-type chunk_element_type() :: a | code | em | strong | i | b|
                              p | 'div' | br | pre | ul |
                              ol | li | dl | dt | dd |
                              h1 | h2 | h3 | h4 | h5 | h6.
-type chunk_element_attrs() :: [].
-type code_element_attrs() :: [{class,unicode:chardata()}].
-type quote() :: {blockquote, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type code() :: {pre, chunk_element_attrs(), [{code, code_element_attrs(), shell_docs:chunk_elements()}]}.
-type p() :: {p, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type i() :: {i, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type em() :: {em, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type code_inline() :: {code, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type header() :: {h1 | h2 | h3 | h4 | h5 | h6, chunk_element_attrs(), shell_docs:chunk_elements() | [binary()]}.

-spec process_heading(Level, Heading, Rest) -> HtmlErlang when
      Level         :: 1..6,
      Heading       :: binary(),
      Rest          :: [binary()],
      HtmlErlang    :: shell_docs:chunk_elements().
process_heading(Level, Text, Rest) ->
    Header = create_header(Level, Text),
    [format_inline(Header) | parse_md(Rest, [])].

-spec create_header(Level, Header) -> header() when
      Level  :: 1..6,
      Header :: binary().
create_header(Level, Heading) ->
    {header_level(Level), [], [Heading]}.

%% this function makes it easier for type checker
%% to understand the meaning.
header_level(1) -> h1;
header_level(2) -> h2;
header_level(3) -> h3;
header_level(4) -> h4;
header_level(5) -> h5;
header_level(6) -> h6.

-spec process_quote(Line, PrevLines) -> HtmlErlang when
      Line       :: [binary()],  %% Represents current parsing line.
      PrevLines  :: [binary()],  %% Represent unprocessed lines.
      HtmlErlang :: shell_docs:chunk_elements().
process_quote([<<">">> | Rest], PrevLines) ->
    process_quote(Rest, PrevLines);
process_quote([<<"> ", Line/binary>> | Rest], PrevLines) ->
    process_quote(Rest, [Line | PrevLines]);
process_quote([<<">> ", Line/binary>> | Rest], PrevLines) ->
    process_quote([<<"> ", Line/binary>> | Rest], PrevLines);
process_quote([<<">", Line/binary>> | Rest], PrevLines) ->
    process_quote([<<"> ", Line/binary>> | Rest], PrevLines);
process_quote(Rest, PrevLines) ->
    [quote(parse_md(lists:reverse(PrevLines), []))] ++ parse_md(Rest, []).

-spec format_inline(Inline) -> Inline :: {chunk_element_type(), [], shell_docs:chunk_elements()} when
      Inline :: {chunk_element_type(), [], [binary()]}.
format_inline({Tag, [], Ls}) when is_list(Ls) ->
    F = fun (Bin, Acc) when is_list(Acc), is_binary(Bin) ->
                L = case format_link(Bin) of
                        <<>> ->
                            %% ignore empty lines if we remove {: .text }\n
                            [];
                        Text ->
                            Format = [], % existing format to match on closing
                            Buffer = [], % tracks the current thing to put within a specific format
                            process_inline(Text, Format, Buffer)
                    end,
                L ++ Acc
        end,
    FormattedLines = lists:foldr(F, [], Ls),
    {Tag, [], lists:reverse(FormattedLines)}.

format_link(Bin) when is_binary(Bin) ->
    Options = [{return, binary}, global],

    %% replace using greedy search, works on
    %% [foo bar](etc)
    %% thanks to Elixir folks:
    %% https://github.com/elixir-lang/elixir/blob/30db5d91fbe54f066776840a9f09116e6f1ea81b/lib/elixir/lib/io/ansi/docs.ex#L623
    R1 = re:replace(Bin, ~b"\\[([^\\]]*?)\\]\\((.*?)\\)", "\\1", Options),

    %% replace using lazy search, useful when there are [] symbols
    %% e.g., [`foo([ok])`](etc)
    R2 = re:replace(R1, ~b"\\[(.*)\\]\\((.*?)\\)", "\\1", Options),

    %% remove anchors
    R3 = re:replace(R2, ~b"{:[^}]*}", <<>>, Options),

    %% remove links of the form: `[foo]: erlang.org`
    re:replace(R3, ~b"\\[([^\\]]*?)\\]: (.*?)", "", [{return, binary}, global]).

-spec process_inline(Line, Format, Buffer) -> Result when
      Line :: binary(),
      Format :: [binary()],
      Buffer :: shell_docs:chunk_elements(),
      Result :: shell_docs:chunk_elements().
process_inline(Bin, Fs, Buffer) ->
    case process_format(Bin, Fs, Buffer) of
        {not_closed, Buffer1} ->
            Buffer1;
        {ok, Buffer1} ->
            Buffer1;
        {Continuation, Buffer1} when is_binary(Continuation)->
            process_inline(Continuation, [], Buffer1)
    end.

-spec process_format(Text, Format, Buffer) -> {Continuation, ResultBuffer} when
      Text :: binary(),
      Format :: [binary()],
      Buffer :: shell_docs:chunk_elements(),
      ResultBuffer :: shell_docs:chunk_elements(),
      Continuation :: not_closed | ok | binary().
%%
%% Handle inline code
%%
process_format(<<$`, Continuation/binary>>, Fs, Buffer) ->
    {Buffer1, Continuation2} = code_inliner(Continuation, []),
    process_format(Continuation2, Fs, merge_buffers(Buffer1, Buffer));


process_format(<<$\\, Char, Rest/binary>>, Fs, Buffer)
  when ?VALID_ESCAPED(Char) ->
    process_format(Rest, Fs, merge_buffers([<<Char>>],  Buffer));

%% This case deals with closing of formatting code on special characters,
%% e.g.,"_Keys (ssh)_," should understand the last underscore as the closing
%% of italics. This is not the general case, see example "ssh_added_here"
%% which must not be considered italics
process_format(<<Char, Format, Format, Char2, Rest/binary>>, [<<Format>>, <<Format>>]=Fs, Buffer)
  when (Char =/= Format andalso not ?VALID_FORMAT(Char) andalso ?VALID_PUNCTUATION(Char)) andalso
       ?VALID_FORMAT(Format) andalso
       (Char2 =/= Format andalso not ?VALID_FORMAT(Char2) andalso ?VALID_SEPARATOR(Char2)) ->
    close_format(<<Char2, Rest/binary>>, Fs, merge_buffers([<<Char>>],  Buffer));
process_format(<<Char, Format, Char2, Rest/binary>>, [<<Format>>], Buffer)
  when (Char =/= Format andalso not ?VALID_FORMAT(Char) andalso ?VALID_PUNCTUATION(Char)) andalso
       ?VALID_FORMAT(Format) andalso
       (Char2 =/= Format andalso not ?VALID_FORMAT(Char2) andalso ?VALID_SEPARATOR(Char2)) ->
    close_format(<<Char2, Rest/binary>>, [<<Format>>], merge_buffers([<<Char>>],  Buffer));

%% This case deals with ssh_added_here to not be consider italics,
%% so no formatting of code should happen
process_format(<<Char, Format, Char2, Rest/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) andalso
       (Char  =/= Format andalso Char  =/= $\s andalso Char  =/= $\n andalso not ?VALID_FORMAT(Char) andalso
        not ?VALID_PUNCTUATION(Char)) andalso
       (Char2 =/= Format andalso Char2 =/= $\s andalso Char2 =/= $\n andalso not ?VALID_FORMAT(Char2) andalso
        not ?VALID_PUNCTUATION(Char2)) ->
    process_format(Rest, Fs, merge_buffers([<<Char, Format, Char2>>],  Buffer));
process_format(<<Char, Format, Format, Char2, Rest/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) andalso
       (Char  =/= Format andalso Char  =/= $\s andalso Char  =/= $\n andalso not ?VALID_FORMAT(Char) andalso
        not ?VALID_PUNCTUATION(Char)) andalso
       (Char2 =/= Format andalso Char2 =/= $\s andalso Char2 =/= $\n andalso not ?VALID_FORMAT(Char2) andalso
        not ?VALID_PUNCTUATION(Char2)) ->
    process_format(Rest, Fs, merge_buffers([<<Char, Format, Char2>>],  Buffer));

%%
%% Handle closing of bold / italics
%%
process_format(<<Format, Format, Symb/binary>>, [<<Format>>, <<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(Symb, [<<Format>>, <<Format>>], Buffer);
process_format(<<Format, Format, Symb, Continuation/binary>>, [<<Format>>, <<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(<<Symb, Continuation/binary>>, [<<Format>>, <<Format>>], Buffer);
process_format(<<Format, Format, $\r, $\n, Continuation/binary>>, [<<Format>>, <<Format>>], Buffer) ->
    close_format(<<$\r, $\n, Continuation/binary>>, [<<Format>>, <<Format>>], Buffer);
process_format(<<Format, Format, Continuation/binary>>, [<<Format>>, <<Format>>], Buffer) ->
    close_format(Continuation, [<<Format>>, <<Format>>], Buffer);


process_format(<<Symb, Format, Format>>, [<<Format>>, <<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(<<>>, [<<Format>>, <<Format>>], merge_buffers([<<Symb>>], Buffer));
process_format(<<Symb, Format, Format, Continuation/binary>>, [<<Format>>, <<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(<<Continuation/binary>>, [<<Format>>, <<Format>>], merge_buffers([<<Symb>>], Buffer));


process_format(<<Format, Symb/binary>>, [<<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(<<>>, [<<Format>>], Buffer);
process_format(<<Format, Symb, Continuation/binary>>, [<<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(<<Symb, Continuation/binary>>, [<<Format>>], Buffer);
process_format(<<Format, $\r, $\n, Continuation/binary>>, [<<Format>>], Buffer) ->
    close_format(<<$\r, $\n, Continuation/binary>>, [<<Format>>], Buffer);
process_format(<<Format, Continuation/binary>>, [<<Format>>], Buffer) ->
    close_format(Continuation, [<<Format>>], Buffer);

process_format(<<Symb, Format>>, [<<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(<<>>, [<<Format>>], merge_buffers([<<Symb>>], Buffer));
process_format(<<Symb, Format, Continuation/binary>>, [<<Format>>], Buffer)
  when ?VALID_SEPARATOR(Symb) ->
    close_format(<<Continuation/binary>>, [<<Format>>], merge_buffers([<<Symb>>], Buffer));

%%
%% Handle opening blocks of bold / italics
%%
process_format(<<Format, Format>>, [<<Format2>>, <<Format2>>]=Fs, Buffer)
  when ?VALID_FORMAT(Format) andalso Format =/= Format2 ->
    %% open a new format that will never be matched because
    %% the Continuation has ended <<>>.
    process_format(<<>>, Fs, merge_buffers([<<Format>>, <<Format>>], Buffer));
process_format(<<Symb, Format, Format, Continuation/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) andalso ?VALID_SEPARATOR(Symb) ->
    open_format(Continuation, [<<Format>>, <<Format>>], Fs, merge_buffers([<<Symb>>], Buffer));
process_format(<<$\r, $\n, Format, Format, Continuation/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) ->
    open_format(Continuation, [<<Format>>, <<Format>>], Fs, merge_buffers([<<$\r, $\n>>], Buffer));
process_format(<<Format, Format, Continuation/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) ->
    open_format(Continuation, [<<Format>>, <<Format>>], Fs, Buffer);


process_format(<<Format>>, [<<Format2>>]=Fs, Buffer)
  when ?VALID_FORMAT(Format) andalso Format =/= Format2 ->
    %% open a new format that will never be matched
    process_format(<<>>, Fs, merge_buffers([<<Format>>], Buffer));
process_format(<<Symb, Format, Continuation/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) andalso ?VALID_SEPARATOR(Symb) ->
    open_format(Continuation, [<<Format>>], Fs, merge_buffers([<<Symb>>], Buffer));
process_format(<< $\r, $\n, Format, Continuation/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) ->
    open_format(Continuation, [<<Format>>], Fs, merge_buffers([<<$\r, $\n>>], Buffer));
process_format(<<Format, Continuation/binary>>, Fs, Buffer)
  when ?VALID_FORMAT(Format) ->
    open_format(Continuation, [<<Format>>], Fs, Buffer);


%%
%% Handle non-formatting characters
%%
process_format(<<Char, Rest/binary>>, Format, Buffer) ->
    process_format(Rest, Format, merge_buffers([<<Char>>],  Buffer));
process_format(<<>>, [], Buffer) ->
    {ok, Buffer};
process_format(<<>>, _Format, Buffer) ->
    {not_closed, Buffer}.

%%
%% Parses text until it finds a closing $`
-spec code_inliner(Line, Buffer) -> {ReturnedBuffer, Rest} when
      Line :: binary(),
      Buffer :: shell_docs:chunk_elements(),
      ReturnedBuffer :: shell_docs:chunk_elements(),
      Rest :: binary().
code_inliner(Line, Buffer0) ->
    F = fun Inliner(<<$\\, $`, Rest/binary>>, Buffer) ->
                %% append chars to buffer
                Inliner(Rest, merge_buffers([<<$`>>], Buffer));
            Inliner(<<$`, Rest/binary>>, Buffer) ->
                %% close inline
                {[code_inline(Buffer)], Rest};
            Inliner(<<Char, Rest/binary>>, Buffer) ->
                %% append chars to buffer
                Inliner(Rest, merge_buffers([<<Char>>], Buffer));
            Inliner(<<>>, Buffer) ->
                %% end without closing inline
                {Buffer, <<>>}
        end,
    case Line of
       <<Prefix, $:, Rest/binary>> when ?INLINE_PREFIX(Prefix) ->
         F(Rest, Buffer0);
      _ ->
         F(Line, Buffer0)
    end.

-spec open_format(Continuation, NewFormat, PrevFormat, Buffer) -> Result when
      Continuation :: binary(),
      NewFormat    :: [binary()], % list of the following binary chars $* | $_ | $`,
      PrevFormat   :: [binary()], % same as above
      Buffer       :: shell_docs:chunk_elements(),
      Result       :: dynamic().
open_format(Continuation, NewFormat, PrevFormat, Buffer) ->
    case process_format(Continuation, NewFormat, []) of
        {not_closed, _LastBufferedResult} ->
            %% the 'NewFormat' could not find a closing match when
            %% processing, thus one must prepend 'NewFormat'
            %% to whatever the buffer at that time had, so that
            %% we add the symbols in 'NewFormat' at their opening place
            %% if they are a binary:
            %%
            %% Example:
            %%
            %% Buffer = [<<"Here">>] and NewFormat = [<<"*">>]
            %% merge_buffers(Buffer, NewFormat) = [<<"*Here">>].
            %%
            %% Buffer = [{em, [], X}] and Buffer2 = [<<"Zzz">>]
            %% merge_buffers(Buffer, Buffer2) = [{em, [], X}, <<"Zzz">>].
            %%
            PrependToBuffer = merge_buffers(NewFormat, Buffer),
            process_format(Continuation, PrevFormat, PrependToBuffer);
        {ok, Buffer1} ->
            %% There is no more continuation, i.e., nothing to process,
            %% so we are finished.
            {ok, merge_buffers(Buffer1, Buffer)};
        {Continuation1, LastBufferedResult} when is_binary(Continuation1) ->
            %% The NewFormat was closed, so we continue processing
            %% the resulting continuation
            process_format(Continuation1, PrevFormat, merge_buffers(LastBufferedResult, Buffer))
    end.

-spec close_format(Continuation, ClosingFormat, Buffer) -> Result when
      Continuation  :: binary(),
      ClosingFormat :: [binary()], % list of the following binary chars $* | $_ | $`,
      Buffer        :: shell_docs:chunk_elements(),
      Result        :: dynamic().
close_format(Continuation, ClosingFormat, Buffer) ->
    {Continuation, format(ClosingFormat, Buffer)}.

%% assume that buffers have their elements in reverse order.
%% i.e., [{p, [], [<<"Here ">>, {em, [], <<"there">>}]}] will appear here as
%% [{p, [], [{em, [], <<"there">>}, <<"Here ">>]}]
%% the reversal operation is taking care of once, in another place.
merge_buffers(EndBuffer, BeginningBuffer) ->
    lists:foldr(fun compact_buffers/2, [], EndBuffer ++ BeginningBuffer).

compact_buffers(X, []) when is_binary(X); is_tuple(X) ->
    [X];
compact_buffers(X, Acc) when is_tuple(X) ->
    [X | Acc];
compact_buffers(X, [LastEntry | Beginning]) when is_tuple(LastEntry) ->
    [X, LastEntry | Beginning];
compact_buffers(<<X/binary>>, [<<LastEntry/binary>> | Beginning]) ->
    [<<LastEntry/binary, X/binary>> | Beginning].

-spec format(Format, Line) -> Result when
      Line :: shell_docs:chunk_elements(),
      Result :: [{chunk_element_type(), chunk_element_attrs(), shell_docs:chunk_elements()}],
      Format :: [binary()].
format(Format, Line0) when is_list(Line0)->
    Line1 = lists:reverse(Line0),
    Formatted = case Format of
                    [<<"*">>, <<"*">>] ->
                        em(Line1);
                    [<<"_">>, <<"_">>] ->
                        em(Line1);
                    [<<"_">>] ->
                        i(Line1);
                    [<<"*">>] ->
                        i(Line1);
                    [<<"`">>] ->
                        code_inline(Line1)
                end,
    [Formatted].

-spec process_code(Line, PrevLines) -> HtmlErlang when
      Line       :: [binary()],  %% Represents current parsing line.
      PrevLines  :: [binary()],  %% Represent unprocessed lines.
      HtmlErlang :: shell_docs:chunk_elements().
process_code([], Block) ->
    [create_code(Block, [])];
process_code([<<"    ", Line/binary>> | Rest], Block) ->
    %% process blank line followed by code
    process_code(Rest, [Line | Block]);
process_code(Rest, Block) ->
    process_code([], Block) ++ parse_md(Rest, []).

process_fence_code([], Block, Leading) ->
    case string:trim(hd(binary:split(Leading, [~"\t", ~" "]))) of
        <<>> -> [create_code(Block, [])];
        Trimmed -> [create_code(Block, [{class, <<"language-", Trimmed/binary>>}])]
    end;
process_fence_code([<<"```">> | Rest], Block, Leading) ->
    %% close block
    process_fence_code([], Block, Leading) ++ parse_md(Rest, []);
process_fence_code([Line | Rest], Block, Leading) ->
    {Stripped, _} = strip_spaces(Line, 0, infinity),
    maybe
        <<"```", RestLine/binary>> ?= Stripped,
        {<<>>, _} ?= strip_spaces(RestLine, 0, infinity),
        process_fence_code([<<"```">> | Rest], Block, Leading)
    else
        _ ->
            process_fence_code(Rest, [Line | Block], Leading)
    end.

-spec process_comment(Line :: [binary()]) -> [binary()].
process_comment([]) ->
    [];
process_comment([Line | Rest]) ->
    case binary:split(Line, <<"-->">>) of
        [_] ->
            % Line is just comment, process next line
            case process_comment(Rest) of
                [] ->
                    %% closing comment not found
                    error(missing_close_comment);
                Result ->
                    Result
            end;
        [_Comment, Text] ->
            % Skip comment, return text plus continuation line
            [Text | Rest]
    end.

-spec create_paragraph(Line) -> P when
      Line :: binary(),
      P :: p().
create_paragraph(<<$\s, Line/binary>>) ->
    create_paragraph(Line);
create_paragraph(Line) when is_binary(Line) ->
    p(Line).

-spec create_code(Lines :: [binary()], code_element_attrs()) -> code().
create_code(CodeBlocks, CodeAttrs) when is_list(CodeBlocks) ->
    %% assumes that the code block is in reverse order
    Bin = trim_and_add_new_line(CodeBlocks),
    {pre, [], [{code, CodeAttrs, [Bin]}]}.

create_table(Table) when is_list(Table) ->
    {pre, [], [{code, [{class, ~"table"}], Table}]}.


-spec quote(Quote :: list()) -> quote().
quote(List) when is_list(List) ->
    {blockquote, [], List}.

-spec p(Line :: binary()) -> p().
p(X) when is_binary(X) ->
    {p, [], [X]}.

-spec code_inline(Text :: shell_docs:chunk_elements()) -> code_inline().
code_inline(X) when is_list(X) ->
    {code, [], X}.

-spec i(Text :: shell_docs:chunk_elements()) -> i().
i(X) when is_list(X) ->
    {i, [], X}.

-spec em(Text :: shell_docs:chunk_elements()) -> em().
em(X) when is_list(X) ->
    {em, [], X}.

%% Assumes that the list is reversed, so Last is the last element.
-spec trim_and_add_new_line([binary()]) -> binary().
trim_and_add_new_line([]) ->
    ~"\n";
trim_and_add_new_line(Lines) when is_list(Lines) ->
    trim_and_add_new_line(Lines, <<>>).

trim_and_add_new_line([], Acc) when is_binary(Acc) ->
    Acc;
trim_and_add_new_line([Line | Rest], Acc) when is_binary(Line), is_binary(Acc) ->
    Line1 = re:replace(Line, ~b"(\n|\r\n)", <<>>, [{return, binary}, global]),
    trim_and_add_new_line(Rest, <<Line1/binary, "\n", Acc/binary>>).
