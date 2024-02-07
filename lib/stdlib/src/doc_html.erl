-module(doc_html).

-export([markdown_to_shelldoc/1, process_md/1]).

-include_lib("kernel/include/eep48.hrl").

-define(DEFAULT_FORMAT, <<"text/markdown">>).

-spec markdown_to_shelldoc(#docs_v1{}) -> #docs_v1{}.
markdown_to_shelldoc(#docs_v1{format = Format}=Docs) ->
    DefaultFormat = binary_to_list(?DEFAULT_FORMAT),
    case Format of
        _ when Format =:= ?DEFAULT_FORMAT orelse Format =:= DefaultFormat ->
            ModuleDoc = Docs#docs_v1.module_doc,
            Doc = Docs#docs_v1.docs,
            Docs#docs_v1{format = ?NATIVE_FORMAT,
                         module_doc = process_moduledoc(ModuleDoc),
                         docs = process_doc_attr(Doc)};
        _  ->
            Docs
    end.

-spec process_moduledoc(Doc :: map() | none | hidden) -> map() | none | hidden.
process_moduledoc(Doc) when Doc =:= none orelse Doc =:= hidden ->
    Doc;
process_moduledoc(Doc) when is_map(Doc) ->
    maps:map(fun (_K, V) -> process_md(V) end, Doc).

-spec process_doc_attr(Doc :: [doc()]) -> [doc()].
process_doc_attr(Doc) ->
    lists:map(fun process_doc/1, Doc).

-type doc() :: { { Attribute :: function | type | callback,
                   FunName :: atom(),
                   Arity :: non_neg_integer()},
                 Anno :: erl_anno:anno(),
                 Signature :: [binary()],
                 Doc :: #{ binary() := binary() | term()} | none | hidden,
                 Metadata :: map()
               }.

-spec process_doc(doc() | [doc()]) -> doc().
process_doc(Docs) when is_list(Docs) ->
    lists:map(fun process_doc/1, Docs);
process_doc({_At, _A, _S, Doc, _M}=Entry) when Doc =:= none orelse Doc =:= hidden ->
    Entry;
process_doc({Attributes, Anno, Signature, Doc, Metadata}) ->
    Docs = maps:map(fun (_K, V) -> process_md(V) end, Doc),
    {Attributes, Anno, Signature, Docs, Metadata}.

-spec process_md(Doc0 :: binary()) -> Doc1 :: shell_docs:chunk_elements().
process_md(Doc) when is_binary(Doc) ->
    Lines = binary:split(Doc, [<<"\r\n">>, <<"\n">>], [global]),
    process_md(Lines, []).

-spec process_md(Markdown, HtmlErlang) -> HtmlErlang when
      Markdown :: [binary()],
      HtmlErlang :: shell_docs:chunk_elements().
process_md([], Block) ->
    Block;
process_md([<<"# ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 1,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_md([<<"## ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 2,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_md([<<"### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 3,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_md([<<"#### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 4,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_md([<<"##### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 5,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_md([<<"###### ", Heading/binary>> | Rest], Block) ->
    HeadingLevel = 6,
    Block ++ process_heading(HeadingLevel, Heading, Rest);
process_md([<<">", Line/binary>> | Rest], Block) ->
    Block ++ process_quote([<<">", Line/binary>> | Rest], []);
process_md([<<"   >", Line/binary>> | Rest], Block) ->
    Block ++ process_quote([<<">", Line/binary>> | Rest], []);
process_md([<<"    ", Line/binary>> | Rest], Block) ->
    Block ++ process_code([<<"    ", Line/binary>> | Rest], []);
process_md([<<"```", _Line/binary>> | Rest], Block) ->
    Block ++ process_fence_code(Rest, []);
process_md([<<"">> | Rest], Block) ->
    Block ++ process_br(Rest);
process_md([<<"<!--", Line/binary>> | Rest], Block) ->
    Block ++ process_md(process_comment([Line | Rest]), []);
process_md(Rest, Block) when is_list(Rest) ->
    process_rest(Rest, Block).

-define(IS_BULLET(X), X =:= $*; X =:= $-; X =:= $+).
-define(IS_NUMBERED(X), is_integer(X), min(0, X) =:= 0).

process_rest([P | Rest]=Doc, Block) ->
    {StrippedP, SpaceCount} = strip_spaces(P, 0, infinity),
    {Content, Rest1, Block2} = case StrippedP of
                                   <<BulletList, $\s, Line/binary>> when ?IS_BULLET(BulletList) ->
                                       process_list(ul, Line, Rest, SpaceCount, Block);
                                   <<NumberedList, $., $\s, Line/binary>> when ?IS_NUMBERED(NumberedList) ->
                                       process_list(ol, Line, Rest, SpaceCount, Block);
                                   _ ->
                                       process_p(Doc, Block)
                               end,
    Content ++ process_md(Rest1, Block2).

-spec process_list(ul | ol , LineContent, Rest, SpaceCount, Block) -> Result when
      LineContent :: binary(),
      Rest        :: [binary()],
      SpaceCount  :: non_neg_integer(),
      Block       :: shell_docs:chunk_elements(),
      Result      :: {shell_docs:chunk_elements(), [binary()], shell_docs:chunk_elements()}.
process_list(Format, LineContent, Rest, SpaceCount, Block) ->
    LineFormatted = li(process_md(LineContent)),
    {Content, Rest1, Done} = process_list_next(Format, Rest, SpaceCount, [LineFormatted]),
    Paragraph = case Done of
                    true ->
                        process_br([]);
                    false ->
                        []
                end,
    Content1 = compact_content(Content),
    {Block ++ [create_item_list(Format, Content1)], Rest1, Paragraph}.

compact_content(Content) ->
    Result = lists:foldr(fun (X, []) ->
                                 [X];
                             ({li, [], _}=Li2, [{li, [], _}=Li1 | Acc]) ->
                                 [Li2, Li1 | Acc];
                             ({_, [], _}=P, [{li, [], Items} | Acc]) when is_list(Items) ->
                                 [{li, [], [P | Items]} | Acc];
                             (X, Acc) when is_list(Acc), is_tuple(X) ->
                                 [X | Acc]
                         end, [], Content),
    reversal_html(Result).

reversal_html(Items) ->
    lists:map(fun ({Tag, [], Ls}) when Tag =/= p -> {Tag, [], reversal_html(Ls)};
                  (X) -> X
              end, lists:reverse(Items)).

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

process_list_next(_, [], _SpaceCount, Acc) ->
    {Acc, [], true};
process_list_next(Format, [Line | Rest], SpaceCount, Acc) ->
    {Stripped, NextSpaceCount} = strip_spaces(Line, 0, infinity),
    case process_list_next_kind(Stripped, Rest, NextSpaceCount, SpaceCount) of
        {new_list_item, StrippedFormattedItem} ->
            %% next item on the list
            process_list_next(Format, Rest, SpaceCount, [li(StrippedFormattedItem) | Acc]);
        {next, StrippedFormattedItem} ->
            %% paragraph within the list
            process_list_next(Format, Rest, SpaceCount, StrippedFormattedItem ++ Acc);
        done ->
            {Acc, [Line | Rest], true};
        list ->
            {Acc, [Line | Rest], false};
        nested_list ->
            {NestedAcc, Remaining, _Done1} = process_list_next(Format, [Line | Rest], NextSpaceCount, []),
            case Acc of
                [{Tag, [], Items} | Acc1]  ->
                    ListItems = [create_item_list(Format, NestedAcc)] ++ Items,
                    process_list_next(Format, Remaining, SpaceCount, [{Tag, [], ListItems}  | Acc1]);
                [] ->
                    process_list_next(Format, Remaining, SpaceCount, NestedAcc)
            end
    end.

process_list_next_kind(<<BulletFormat, $\s, Line/binary>>, _Rest, NextCount, Count)
  when ?IS_BULLET(BulletFormat) ->
    process_list_nestedness(Line, Count, NextCount);
process_list_next_kind(<<OrderedFormat, $., $\s, Line/binary>>, _Rest, NextCount, Count)
  when ?IS_NUMBERED(OrderedFormat) ->
    process_list_nestedness(Line, Count, NextCount);
process_list_next_kind(<<>>, [<<" ">> | [_|_]], _NextCount, _Count) ->
    %% paragraph within the list
    {next, process_md(<<>>)};
process_list_next_kind(<<>>, _, _, _) ->
    %% this list is done
    done;
process_list_next_kind(Item, _, _, _) ->
    %% paragraph within the list
    {next, process_md(Item)}.

process_list_nestedness(Line, Count, NextCount) ->
    case NextCount - Count of
        0 ->
            {new_list_item, process_md(Line)};
        X when X < 0 ->
            %% closing list
            list;
        X when X > 0 ->
            %% nested new list
            nested_list
    end.

-spec process_p(Doc, Block) -> {Line, Rest, ReturnedBlock} when
      Doc :: [binary()],
      Block :: shell_docs:chunk_elements(),
      Line  :: shell_docs:chunk_elements(),
      Rest  :: [binary()],
      ReturnedBlock :: shell_docs:chunk_elements().
process_p([P | Rest], [{p, [], Ps} | RestBlock]) when is_binary(P) ->
    %% merge two paragraphs under a single one
    {p, [], Paragraph} = process_paragraph(<<"\n", P/binary>>),
    Result = [{p, [], Ps ++ Paragraph} | RestBlock],
    {[], Rest, Result};
process_p([P | Rest], Block) when is_binary(P) ->
    Paragraph = [process_paragraph(P)],
    {[], Rest, Block ++ Paragraph}.


strip_spaces(<<" ", Rest/binary>>, Acc, Max) when Max =:= infinity; Acc =< Max ->
    strip_spaces(Rest, Acc + 1, Max);
strip_spaces(Rest, Acc, _) ->
    {Rest, Acc}.

-type chunk_element_type() :: a | code | em | strong | i | b|
                              p | 'div' | br | pre | ul |
                              ol | li | dl | dt | dd |
                              h1 | h2 | h3 | h4 | h5 | h6.
-type chunk_element_attrs() :: [].  %% | [shell_docs:chunk_element_attr()].
-type quote() :: {pre, chunk_element_attrs(), [{code,[], shell_docs:chunk_elements()}]}.
-type code() :: {pre, chunk_element_attrs(), [{code,[], shell_docs:chunk_elements()}]}.
-type p() :: {p, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type i() :: {i, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type em() :: {em, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type code_inline() :: {code, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type br() :: {br, chunk_element_attrs(), shell_docs:chunk_elements()}.
-type header() :: {h1 | h2 | h3 | h4 | h5 | h6, chunk_element_attrs(), shell_docs:chunk_elements() | [binary()]}.

-spec process_heading(Level, Heading, Rest) -> HtmlErlang when
      Level         :: 1..6,
      Heading       :: binary(),
      Rest          :: [binary()],
      HtmlErlang    :: shell_docs:chunk_elements().
process_heading(Level, Text, Rest) ->
    Header = create_header(Level, Text),
    [format_inline(Header) | process_md(Rest, [])].

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
process_quote([], PrevLines) ->
    [create_quote(PrevLines)];
process_quote([<<">>", Line/binary>> | Rest], PrevLines) ->
    Line1 = trim_leading_space(Line),
    [create_quote(PrevLines), create_quote([]) | process_quote(Rest, [Line1])];
process_quote([<<">", Line/binary>> | Rest], PrevLines) ->
    Line1 = trim_leading_space(Line),
    process_quote(Rest, [Line1 | PrevLines]);
process_quote(Rest, PrevLines) ->
    [create_quote(PrevLines) | process_md(Rest, [])].

-spec process_paragraph(P) -> HtmlErlang when
      P            :: binary(),
      HtmlErlang   :: {chunk_element_type(), [], shell_docs:chunk_elements()}.
process_paragraph(P0) ->
    P = create_paragraph(P0),
    format_inline(P).

-spec format_inline(Inline) -> Inline :: {chunk_element_type(), [], shell_docs:chunk_elements()} when
      Inline :: {chunk_element_type(), [], [binary()]}.
format_inline({Tag, [], Ls}) when is_list(Ls) ->
    FormattedLines = lists:foldr(fun (L, Acc) when is_list(Acc) -> process(L) ++ Acc end, [], Ls),
    {Tag, [], lists:reverse(FormattedLines)}.

-spec process(Text :: binary()) -> shell_docs:chunk_elements().
process(Bin) when is_binary(Bin)->
    Format = [], % existing format to match on closing
    Buffer = [], % tracks the current thing to put within a specific format
    Text = format_link(Bin),
    process_inline(Text, Format, Buffer).

format_link(Bin) when is_binary(Bin) ->
    remove_square_brackets(Bin).

remove_square_brackets(Bin) when is_binary(Bin) ->
    %% thanks to Elixir folks:
    %% https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/io/ansi/docs.ex#L626C22-L626C44
    R = re:replace(Bin, "\\\[([^\\\]]*?)\\\]\\\((.*?)\\\)", "\\1 (\\2)"),
    case R of
        Text when is_list(Text) ->
            list_to_binary(Text);
        Text when is_binary(Text) ->
            Text
    end.

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
process_format(<<Format, Format, Continuation/binary>>, [<<Format>>, <<Format>>], Buffer)
  when Format =:= $*; Format =:= $_; Format =:= $` ->
    close_format(Continuation, [<<Format>>, <<Format>>], Buffer);
process_format(<<Format, Continuation/binary>>, [<<Format>>], Buffer)
  when Format =:= $*; Format =:= $_; Format =:= $` ->
    %% close the format
    close_format(Continuation, [<<Format>>], Buffer);
process_format(<<Format, Format>>, Fs, Buffer)
  when Format =:= $*; Format =:= $_; Format =:= $` ->
    %% open a new format that will never be matched because
    %% the Continuation has ended <<>>.
    process_format(<<>>, Fs, merge_buffers([<<Format>>, <<Format>>], Buffer));
process_format(<<Format, Format, Continuation/binary>>, Fs, Buffer)
  when Format =:= $*; Format =:= $_; Format =:= $` ->
    open_format(Continuation, [<<Format>>, <<Format>>], Fs, Buffer);
process_format(<<Format>>, Fs, Buffer)
  when Format =:= $*; Format =:= $_; Format =:= $` ->
    %% open a new format that will never be matched
    process_format(<<>>, Fs, merge_buffers([<<Format>>], Buffer));
process_format(<<Format, Continuation/binary>>, Fs, Buffer)
  when Format =:= $*; Format =:= $_; Format =:= $` ->
    open_format(Continuation, [<<Format>>], Fs, Buffer);
process_format(<<Char, Rest/binary>>, Format, Buffer) ->
    process_format(Rest, Format, merge_buffers([<<Char>>],  Buffer));
process_format(<<>>, [], Buffer) ->
    {ok, Buffer};
process_format(<<>>, _Format, Buffer) ->
    {not_closed, Buffer}.

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
    [create_code(Block)];
process_code([<<"    ", Line/binary>> | Rest], Block) ->
    %% process blank line followed by code
    process_code(Rest, [Line | Block]);
process_code(Rest, Block) ->
    process_code([], Block) ++ process_md(Rest, []).

process_fence_code([], Block) ->
    [create_code(Block)];
process_fence_code([<<"```">> | Rest], Block) ->
    %% close block
    process_fence_code([], Block) ++ process_md(Rest, []);
process_fence_code([Line | Rest], Block) ->
    process_fence_code(Rest, [Line | Block]).


-spec process_br(Text :: [binary()]) -> shell_docs:chunk_elements().
process_br(Rest) ->
    [br() | process_md(Rest, [])].

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

-spec create_quote(Lines) -> Quote when
      Lines :: [binary()],
      Quote :: quote().
create_quote([]) ->
    quote(<<"\n">>);
create_quote([Last | _]=Lines) ->
    ProcessedLines =
        lists:foldl(fun (Line, Acc) when is_binary(Line), is_binary(Acc) ->
                            Line1 = trim_and_add_new_line(Last, Line),
                            <<Line1/binary, Acc/binary>>
                    end, <<>>, Lines),
    quote(ProcessedLines).

-spec create_paragraph(Line) -> P when
      Line :: binary(),
      P :: p().
create_paragraph(Line) ->
    p(Line).

-spec create_code(Lines :: [binary()]) -> code().
create_code(X) ->
    create_quote(X).

-spec quote(Quote :: binary()) -> quote().
quote(X) when is_binary(X) ->
    {pre,[], [{code,[], [X]}]}.

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

-spec br() -> br().
br() ->
    {br, [], []}.

-spec trim_and_add_new_line(binary(), binary()) -> binary().
trim_and_add_new_line(Last, Line) ->
    Line1 = trim(Line),
    case Last =:= Line of
        true -> Line1;
        false -> <<Line1/binary, "\n">>
    end.

-spec trim_leading_space(binary()) -> binary().
trim_leading_space(<<" ", Rest/binary>>) ->
    trim_leading_space(Rest);
trim_leading_space(Rest) when is_binary(Rest) ->
    Rest.

-spec trim(binary()) -> binary().
trim(Line) ->
    %% The split should not produce
    list_to_binary(binary:split(Line, [<<"\r\n">>, <<"\n">>], [trim_all])).
