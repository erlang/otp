%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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
-module(prim_tty).

%% Todo:
%%  * Try to move buffer handling logic to Erlang
%%    * This may not be possible for performance reasons, but should be tried
%%    * It seems like unix decodes and then encodes utf8 when emitting it
%%  * user_drv module should be able to handle both nif and driver without too many changes.
%%
%% Problems to solve:
%%  * Do not use non blocking io
%%  * Reset tty settings at _exit
%%  * Allow remsh in oldshell (can we do this?)
%%  * See if we can run a tty in windows shell
%%  * Allow unicode detection for noshell/noinput
%%  * ?Allow multi-line editing?
%%    * The current implementation only allows the cursor to move and edit on current line
%%
%% Concepts to keep in mind:
%%   Code point: A single unicode "thing", examples: "a", "😀" (unicode smilie)
%%   Grapheme cluster: One or more code points, "
%%   Logical character: Any character that the user typed or printed.
%%            One unicode grapheme cluster is a logical character
%%        Examples: "a", "\t", "😀" (unicode smilie), "\x{1F600}", "\e[0m" (ansi sequences),
%%                  "^C"
%%      When we step or delete we count logical characters even if they are multiple chars.
%%        (I'm unsure how ansi should be handled with regard to delete?)
%%
%%   Actual characters: The actual unicode grapheme clusters printed
%%   Column: The number of rendered columns for a logical character
%%
%%  When navigating using move(left) and move(right) the terminal will move one
%%  actual character, so if we want to move one logical character we may have to
%%  emit more moves. The same is true when overwriting.
%%
%%  When calculating the current column position we have to use the column size
%%  of the characters as otherwise smilies will becomes incorrect.
%%
%%  In the current ttysl_drv and also this implementation there are never any newlines
%%  in the buffer.
%%
%%  Printing of unicode characters:
%%    Read this post: https://jeffquast.com/post/terminal_wcwidth_solution/
%%    Summary: the wcwidth implementation in libc is often lacking. So we should
%%        create our own. We can get the size of all unicode graphemes by rendering
%%        them on a terminal and see how much the cursor moves. We can query where the
%%        cursor is by using "\033[6n"[1]. How many valid grapheme clusters are there?
%%
%%    On consoles that does support fetching the surrent cursor position, we may get
%%    away with only using that to check where we are and where to go. And on consoles
%%    that do not we just have to make a best effort and use libc wcwidth.
%%
%%    We need to know the width of characters when:
%%      * Printing a \t
%%      * Compensating for xn
%% [1]: https://www.linuxquestions.org/questions/programming-9/get-cursor-position-in-c-947833/
%%    Notes:
%%      - [129306,127996] (hand with skintone) seems to move the cursor more than
%%        it should on iTerm...The skintone code point seems to not
%%        work at all on Linux and instead emits hand + brown square which is 4 characters
%%        wide which is what the cursor on iTerm was positioned at. So maybe
%%        there is a mismatch somewhere in iTerm wcwidth handling.
%%      - edlin and user needs to agree on what one "character" is, right now edlin uses
%%        code points and not grapheme clusters.
%%      - We can deal with xn by always emitting it after a put_chars command. We must make
%%        sure not to emit it before we emit any newline in the put_chars though as that
%%        potentially will insert two newlines instead of one.
%%
%%  Windows:
%%    Since 2017:ish we can use Virtual Terminal Sequences[2] to control the terminal.
%%    It seems like these are mostly ANSI escape comparitible, so it should be possible
%%    to just use the same mechanism on windows and unix.
%% [2]: https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
%%    Windows does not seem to have any wcwidth, so maybe we have to generate a similar table
%%    as the PR in [3] and add string:width/1.
%% [3]: https://github.com/microsoft/terminal/pull/5795
%%
%%
%%  Things I've tried and discarded:
%%    * Use get position to figure out xn fix for newlines
%%      * There is too large a latency (about 10ms) to get the position, so things like
%%        `c:i()` becomes a lot slower.
%%    * Use tty insert mode
%%      * This only works when the cursor is on the last line, when it is on a previous
%%        line it only edit that line.
%%    * Use tty delete mode
%%      * Same problem as insert mode, it only deleted current line, and does not move
%%        to previous line automatically.

-export([init/1, reinit/2, isatty/1, handles/1, unicode/1, unicode/2,
         handle_signal/2, window_size/1, handle_request/2, write/2, write/3, npwcwidth/1,
         npwcwidthstring/1]).
-export([reader_stop/1, disable_reader/1, enable_reader/1]).

-nifs([isatty/1, tty_create/0, tty_init/3, tty_set/1, setlocale/1,
       tty_select/3, tty_window_size/1, write_nif/2, read_nif/2, isprint/1,
       wcwidth/1, wcswidth/1,
       sizeof_wchar/0, tgetent_nif/1, tgetnum_nif/1, tgetflag_nif/1, tgetstr_nif/1,
       tgoto_nif/2, tgoto_nif/3, tty_read_signal/2]).

%% Exported in order to remove "unused function" warning
-export([sizeof_wchar/0, wcswidth/1, tgoto/2, tgoto/3]).

%% proc_lib exports
-export([reader/1, writer/1]).

-on_load(on_load/0).

%%-define(debug, true).
-ifdef(debug).
-define(dbg(Term), dbg(Term)).
-else.
-define(dbg(Term), ok).
-endif.
%% Copied from https://github.com/chalk/ansi-regex/blob/main/index.js
-define(ANSI_REGEXP, <<"^[\e",194,155,"][[\\]()#;?]*(?:(?:(?:(?:;[-a-zA-Z\\d\\/#&.:=?%@~_]+)*|[a-zA-Z\\d]+(?:;[-a-zA-Z\\d\\/#&.:=?%@~_]*)*)?",7,")|(?:(?:\\d{1,4}(?:;\\d{0,4})*)?[\\dA-PR-TZcf-nq-uy=><~]))">>).
-record(state, {tty,
                reader,
                writer,
                options,
                unicode,
                buffer_before = [],  %% Current line before cursor in reverse
                buffer_after = [],   %% Current line after  cursor not in reverse
                buffer_expand,       %% Characters in expand buffer
                cols = 80,
                rows = 24,
                xn = false,
                clear = <<"\e[H\e[2J">>,
                up = <<"\e[A">>,
                down = <<"\n">>,
                left = <<"\b">>,
                right = <<"\e[C">>,
                %% Tab to next 8 column windows is "\e[1I", for unix "ta" termcap
                tab = <<"\e[1I">>,
                delete_after_cursor = <<"\e[J">>,
                insert = false,
                delete = false,
                position = <<"\e[6n">>, %% "u7" on my Linux
                position_reply = <<"\e\\[([0-9]+);([0-9]+)R">>,
                ansi_regexp = ?ANSI_REGEXP,
                %% The SGR (Select Graphic Rendition) parameters https://en.wikipedia.org/wiki/ANSI_escape_code#SGR
                ansi_sgr = <<"^[\e",194,155,"]\\[[0-9;:]*m">>
               }).

-type options() :: #{ tty => boolean(),
                      input => boolean(),
                      canon => boolean(),
                      echo => boolean(),
                      sig => boolean()
                    }.
-type request() ::
        {putc, unicode:unicode_binary()} |
        {expand, unicode:unicode_binary()} |
        {insert, unicode:unicode_binary()} |
        {delete, integer()} |
        {move, integer()} |
        clear |
        beep.
-opaque state() :: #state{}.
-export_type([state/0]).

-spec on_load() -> ok.
on_load() ->
    on_load(#{}).

-spec on_load(Extra) -> ok when
      Extra :: map().
on_load(Extra) ->
    case erlang:load_nif(atom_to_list(?MODULE), Extra) of
        ok -> ok;
        {error,{reload,_}} ->
            ok
    end.

-spec window_size(state()) -> {ok, {non_neg_integer(), non_neg_integer()}} | {error, term()}.
window_size(State = #state{ tty = TTY }) ->
    case tty_window_size(TTY) of
        {error, enotsup} when map_get(tty, State#state.options) ->
            %% When the TTY is enabled, we should return a "dummy" row and column
            %% when we cannot find the proper size.
            {ok, {State#state.cols, State#state.rows}};
        WinSz ->
            WinSz
    end.

-spec init(options()) -> state().
init(UserOptions) when is_map(UserOptions) ->

    Options = options(UserOptions),
    {ok, TTY} = tty_create(),

    %% Initialize the locale to see if we support utf-8 or not
    UnicodeMode =
        case setlocale(TTY) of
            primitive ->
                lists:any(
                  fun(Key) ->
                          string:find(os:getenv(Key,""),"UTF-8") =/= nomatch
                  end, ["LC_ALL", "LC_CTYPE", "LANG"]);
            UnicodeLocale when is_boolean(UnicodeLocale) ->
                UnicodeLocale
        end,

    init_term(#state{ tty = TTY, unicode = UnicodeMode, options = Options }).
init_term(State = #state{ tty = TTY, options = Options }) ->
    TTYState =
        case maps:get(tty, Options) of
            true ->
                ok = tty_init(TTY, stdout, Options),
                NewState = init(State, os:type()),
                ok = tty_set(TTY),
                NewState;
            false ->
                State
        end,

    WriterState =
        if TTYState#state.writer =:= undefined ->
                {ok, Writer} = proc_lib:start_link(?MODULE, writer, [State#state.tty]),
                TTYState#state{ writer = Writer };
           true ->
                TTYState
        end,
    ReaderState =
        case {maps:get(input, Options), TTYState#state.reader} of
            {true, undefined} ->
                {ok, Reader} = proc_lib:start_link(?MODULE, reader, [[State#state.tty, self()]]),
                WriterState#state{ reader = Reader };
            {true, _} ->
                WriterState;
            {false, undefined} ->
                WriterState
        end,

    update_geometry(ReaderState).

-spec reinit(state(), options()) -> state().
reinit(State, UserOptions) ->
    init_term(State#state{ options = options(UserOptions) }).

options(UserOptions) ->
    maps:merge(
      #{ input => true,
         tty => true,
         canon => false,
         echo => false }, UserOptions).

init(State, {unix,_}) ->

    case os:getenv("TERM") of
        false ->
            error(enotsup);
        Term ->
            case tgetent(Term) of
                ok -> ok;
                {error,_} -> error(enotsup)
            end
    end,

    %% See https://www.gnu.org/software/termutils/manual/termcap-1.3/html_mono/termcap.html#SEC23
    %% for a list of all possible termcap capabilities
    Clear = case tgetstr("clear") of
             {ok, C} -> C;
             false -> (#state{})#state.clear
         end,
    Cols = case tgetnum("co") of
               {ok, Cs} -> Cs;
               _ -> (#state{})#state.cols
           end,
    Up = case tgetstr("up") of
             {ok, U} -> U;
             false -> error(enotsup)
         end,
    Down = case tgetstr("do") of
               false -> (#state{})#state.down;
               {ok, D} -> D
           end,
    Left = case {tgetflag("bs"),tgetstr("bc")} of
               {true,_} -> (#state{})#state.left;
               {_,false} -> (#state{})#state.left;
               {_,{ok, L}} -> L
           end,

    Right = case tgetstr("nd") of
                {ok, R} -> R;
                false -> error(enotsup)
            end,
    Insert =
        case tgetstr("IC") of
            {ok, IC} -> IC;
            false -> (#state{})#state.insert
        end,

    Tab = case tgetstr("ta") of
              {ok, TA} -> TA;
              false -> (#state{})#state.tab
          end,

    Delete = case tgetstr("DC") of
                 {ok, DC} -> DC;
                 false -> (#state{})#state.delete
             end,

    Position = case tgetstr("u7") of
                   {ok, <<"\e[6n">> = U7} ->
                       %% User 7 should contain the codes for getting
                       %% cursor position.
                       % User 6 should contain how to parse the reply
                       {ok, <<"\e[%i%d;%dR">>} = tgetstr("u6"),
                       <<"\e[6n">> = U7;
                   false -> (#state{})#state.position
               end,

    %% According to the manual this should only be issued when the cursor
    %% is at position 0, but until we encounter such a console we keep things
    %% simple and issue this with the cursor anywhere
    DeleteAfter = case tgetstr("cd") of
                      {ok, DA} ->
                          DA;
                      false ->
                          (#state{})#state.delete_after_cursor
                  end,

    State#state{
      cols = Cols,
      clear = Clear,
      xn = tgetflag("xn"),
      up = Up,
      down = Down,
      left = Left,
      right = Right,
      insert = Insert,
      delete = Delete,
      tab = Tab,
      position = Position,
      delete_after_cursor = DeleteAfter
     };
init(State, {win32, _}) ->
    State#state{
      %% position = false,
      xn = true }.

-spec handles(state()) -> #{ read := undefined | reference(),
                             write := reference() }.
handles(#state{ reader = undefined,
                writer = {_WriterPid, WriterRef}}) ->
    #{ read => undefined, write => WriterRef };
handles(#state{ reader = {_ReaderPid, ReaderRef},
                writer = {_WriterPid, WriterRef}}) ->
    #{ read => ReaderRef, write => WriterRef }.

-spec unicode(state()) -> boolean().
unicode(State) ->
    State#state.unicode.

-spec unicode(state(), boolean()) -> state().
unicode(#state{ reader = Reader } = State, Bool) ->
    case Reader of
        {ReaderPid, _} ->
            call(ReaderPid, {set_unicode_state, Bool});
        undefined ->
            ok
    end,
    State#state{ unicode = Bool }.

-spec reader_stop(state()) -> state().
reader_stop(#state{ reader = {ReaderPid, _} } = State) ->
    {error, _} = call(ReaderPid, stop),
    State#state{ reader = undefined }.

-spec handle_signal(state(), winch | cont) -> state().
handle_signal(State, winch) ->
    update_geometry(State);
handle_signal(State, cont) ->
    tty_set(State#state.tty),
    State.

-spec disable_reader(state()) -> ok.
disable_reader(#state{ reader = {ReaderPid, _} }) ->
    ok = call(ReaderPid, disable).

-spec enable_reader(state()) -> ok.
enable_reader(#state{ reader = {ReaderPid, _} }) ->
    ok = call(ReaderPid, enable).

call(Pid, Msg) ->
    Alias = erlang:monitor(process, Pid, [{alias, reply_demonitor}]),
    Pid ! {Alias, Msg},
    receive
        {Alias, Reply} ->
            Reply;
        {'DOWN',Alias,_,_,Reason}  ->
            {error, Reason}
    end.

reader([TTY, Parent]) ->
    register(user_drv_reader, self()),
    ReaderRef = make_ref(),
    SignalRef = make_ref(),
    ok = tty_select(TTY, SignalRef, ReaderRef),
    proc_lib:init_ack({ok, {self(), ReaderRef}}),
    FromEnc = case os:type() of
                  {unix, _} -> utf8;
                  {win32, _} ->
                      case isatty(stdin) of
                          true ->
                              {utf16, little};
                          _ ->
                              %% When not reading from a console
                              %% the data read is utf8 encoded
                              utf8
                      end
              end,
    reader_loop(TTY, Parent, SignalRef, ReaderRef, FromEnc, <<>>).

reader_loop(TTY, Parent, SignalRef, ReaderRef, FromEnc, Acc) ->
    receive
        {DisableAlias, disable} ->
            DisableAlias ! {DisableAlias, ok},
            receive
                {EnableAlias, enable} ->
                    EnableAlias ! {EnableAlias, ok},
                    reader_loop(TTY, Parent, SignalRef, ReaderRef, FromEnc, Acc)
            end;
        {select, TTY, SignalRef, ready_input} ->
            {ok, Signal} = tty_read_signal(TTY, SignalRef),
            Parent ! {ReaderRef,{signal,Signal}},
            reader_loop(TTY, Parent, SignalRef, ReaderRef, FromEnc, Acc);
        {Alias, {set_unicode_state, _}} when FromEnc =:= {utf16, little} ->
            %% Ignore requests on windows
            Alias ! {Alias, true},
            reader_loop(TTY, Parent, SignalRef, ReaderRef, FromEnc, Acc);
        {Alias, {set_unicode_state, Bool}} ->
            Alias ! {Alias, FromEnc =/= latin1},
            NewFromEnc = if Bool -> utf8; not Bool -> latin1 end,
            reader_loop(TTY, Parent, SignalRef, ReaderRef, NewFromEnc, Acc);
        {_Alias, stop} ->
            ok;
        {select, TTY, ReaderRef, ready_input} ->
            case read_nif(TTY, ReaderRef) of
                {error, closed} ->
                    Parent ! {ReaderRef, eof},
                    ok;
                {ok, <<>>} ->
                    %% EAGAIN or EINTR
                    reader_loop(TTY, Parent, SignalRef, ReaderRef, FromEnc, Acc);
                {ok, UtfXBytes} ->

                    {Bytes, NewAcc, NewFromEnc} =
                        case unicode:characters_to_binary([Acc, UtfXBytes], FromEnc, utf8) of
                            {error, B, Error} ->
                                %% We should only be able to get incorrect encoded data when
                                %% using utf8 (i.e. we are on unix)
                                FromEnc = utf8,
                                Parent ! {self(), set_unicode_state, false},
                                receive
                                    {Alias, {set_unicode_state, false}} ->
                                        Alias ! {Alias, true}
                                end,
                                receive
                                    {Parent, set_unicode_state, true} -> ok
                                end,
                                Latin1Chars = unicode:characters_to_binary(Error, latin1, utf8),
                                {<<B/binary,Latin1Chars/binary>>, <<>>, latin1};
                            {incomplete, B, Inc} ->
                                {B, Inc, FromEnc};
                            B when is_binary(B) ->
                                {B, <<>>, FromEnc}
                        end,
                    Parent ! {ReaderRef, {data, Bytes}},
                    reader_loop(TTY, Parent, SignalRef, ReaderRef, NewFromEnc, NewAcc)
            end
    end.

writer(TTY) ->
    register(user_drv_writer, self()),
    WriterRef = make_ref(),
    proc_lib:init_ack({ok, {self(), WriterRef}}),
    writer_loop(TTY, WriterRef).

-spec write(state(), unicode:chardata()) -> ok.
write(#state{ writer = {WriterPid, _}}, Chars) ->
    WriterPid ! {write, erlang:iolist_to_iovec(Chars)}, ok.
-spec write(state(), unicode:chardata(), From :: pid()) -> {ok, reference()}.
write(#state{ writer = {WriterPid, _WriterRef}}, Chars, From) ->
    Ref = erlang:monitor(process, WriterPid),
    WriterPid ! {write, From, erlang:iolist_to_iovec(Chars)},
    {ok, Ref}.

writer_loop(TTY, WriterRef) ->
    receive
        {write, []} ->
            writer_loop(TTY, WriterRef);
        {write, Chars} ->
            _ = write_nif(TTY, Chars),
            writer_loop(TTY, WriterRef);
        {write, From, []} ->
            From ! {WriterRef, ok},
            writer_loop(TTY, WriterRef);
        {write, From, Chars} ->
            case write_nif(TTY, Chars) of
                ok ->
                    From ! {WriterRef, ok},
                    writer_loop(TTY, WriterRef);
                {error, Reason} ->
                    exit(self(), Reason)
            end
    end.

-spec handle_request(state(), request()) -> {erlang:iovec(), state()}.
handle_request(State = #state{ options = #{ tty := false } }, Request) ->
    case Request of
        {putc, Binary} ->
            {encode(Binary, State#state.unicode), State};
        beep ->
            {<<7>>, State};
        _Ignore ->
            {<<>>, State}
    end;
%% Clear the expand buffer after the cursor when we handle any request.
handle_request(State = #state{ buffer_expand = Expand, unicode = U }, Request)
  when Expand =/= undefined ->
    BBCols = cols(State#state.buffer_before, U),
    BACols = cols(State#state.buffer_after, U),
    ClearExpand = [move_cursor(State, BBCols, BBCols + BACols),
                   State#state.delete_after_cursor,
                   move_cursor(State, BBCols + BACols, BBCols)],
    {Output, NewState} = handle_request(State#state{ buffer_expand = undefined }, Request),
    {[ClearExpand, encode(Output, U)], NewState};
%% Print characters in the expandbuffer after the cursor
handle_request(State = #state{ unicode = U }, {expand, Binary}) ->
    BBCols = cols(State#state.buffer_before, U),
    BACols = cols(State#state.buffer_after, U),
    Expand = iolist_to_binary(["\r\n",string:trim(Binary, both)]),
    MoveToEnd = move_cursor(State, BBCols, BBCols + BACols),
    {ExpandBuffer, NewState} = insert_buf(State#state{ buffer_expand = [] }, Expand),
    BECols = cols(NewState#state.cols, BBCols + BACols, NewState#state.buffer_expand, U),
    MoveToOrig = move_cursor(State, BECols, BBCols),
    {[MoveToEnd, encode(ExpandBuffer, U), MoveToOrig], NewState};
%% putc prints Binary and overwrites any existing characters
handle_request(State = #state{ unicode = U }, {putc, Binary}) ->
    %% Todo should handle invalid unicode?
    {PutBuffer, NewState} = insert_buf(State, Binary),
    if NewState#state.buffer_after =:= [] ->
            {encode(PutBuffer, U), NewState};
       true ->
            %% Delete any overwritten characters after current the cursor
            OldLength = logical(State#state.buffer_before),
            NewLength = logical(NewState#state.buffer_before),
            {_, _, _, NewBA} = split(NewLength - OldLength, NewState#state.buffer_after, U),
            {encode(PutBuffer, U), NewState#state{ buffer_after = NewBA }}
    end;
handle_request(State = #state{ unicode = U }, {delete, N}) when N > 0 ->
    {_DelNum, DelCols, _, NewBA} = split(N, State#state.buffer_after, U),
    BBCols = cols(State#state.buffer_before, U),
    NewBACols = cols(NewBA, U),
    {[encode(NewBA, U),
      lists:duplicate(DelCols, $\s),
      xnfix(State, BBCols + NewBACols + DelCols),
      move_cursor(State,
                  BBCols + NewBACols + DelCols,
                  BBCols)],
      State#state{ buffer_after = NewBA }};
handle_request(State = #state{ unicode = U }, {delete, N}) when N < 0 ->
    {_DelNum, DelCols, _, NewBB} = split(-N, State#state.buffer_before, U),
    NewBBCols = cols(NewBB, U),
    BACols = cols(State#state.buffer_after, U),
    {[move_cursor(State, NewBBCols + DelCols, NewBBCols),
      encode(State#state.buffer_after,U),
      lists:duplicate(DelCols, $\s),
      xnfix(State, NewBBCols + BACols + DelCols),
      move_cursor(State, NewBBCols + BACols + DelCols, NewBBCols)],
     State#state{ buffer_before = NewBB } };
handle_request(State, {delete, 0}) ->
    {"",State};
handle_request(State = #state{ unicode = U }, {move, N}) when N < 0 ->
    {_DelNum, DelCols, NewBA, NewBB} = split(-N, State#state.buffer_before, U),
    NewBBCols = cols(NewBB, U),
    Moves = move_cursor(State, NewBBCols + DelCols, NewBBCols),
    {Moves, State#state{ buffer_before = NewBB,
                         buffer_after = NewBA ++ State#state.buffer_after} };
handle_request(State = #state{ unicode = U }, {move, N}) when N > 0 ->
    {_DelNum, DelCols, NewBB, NewBA} = split(N, State#state.buffer_after, U),
    BBCols = cols(State#state.buffer_before, U),
    {move_cursor(State, BBCols, BBCols + DelCols),
     State#state{ buffer_after = NewBA,
                  buffer_before = NewBB ++ State#state.buffer_before} };
handle_request(State, {move, 0}) ->
    {"",State};
handle_request(State = #state{ xn = OrigXn, unicode = U }, {insert, Chars}) ->
    {InsertBuffer, NewState0} = insert_buf(State#state{ xn = false }, Chars),
    NewState = NewState0#state{ xn = OrigXn },
    BBCols = cols(NewState#state.buffer_before, U),
    BACols = cols(NewState#state.buffer_after, U),
    {[ encode(InsertBuffer, U),
       encode(NewState#state.buffer_after, U),
       xnfix(State, BBCols + BACols),
       move_cursor(State, BBCols + BACols, BBCols) ],
     NewState};
handle_request(State, beep) ->
    {<<7>>, State};
handle_request(State, clear) ->
    {State#state.clear, State};
handle_request(State, Req) ->
    erlang:display({unhandled_request, Req}),
    {"", State}.

%% Split the buffer after N logical characters returning
%% the number of real characters deleted and the column length
%% of those characters
split(N, Buff, Unicode) ->
    ?dbg({?FUNCTION_NAME, N, Buff, Unicode}),
    split(N, Buff, [], 0, 0, Unicode).
split(0, Buff, Acc, Chars, Cols, _Unicode) ->
    ?dbg({?FUNCTION_NAME, {Chars, Cols, Acc, Buff}}),
    {Chars, Cols, Acc, Buff};
split(N, _Buff, _Acc, _Chars, _Cols, _Unicode) when N < 0 ->
    ok = N;
split(_N, [], Acc, Chars, Cols, _Unicode) ->
    {Chars, Cols, Acc, []};
split(N, [Char | T], Acc, Cnt, Cols, Unicode) when is_integer(Char) ->
    split(N - 1, T, [Char | Acc], Cnt + 1, Cols + npwcwidth(Char, Unicode), Unicode);
split(N, [Chars | T], Acc, Cnt, Cols, Unicode) when is_list(Chars) ->
    split(N - length(Chars), T, [Chars | Acc],
          Cnt + length(Chars), Cols + cols(Chars, Unicode), Unicode);
split(N, [SkipChars | T], Acc, Cnt, Cols, Unicode) when is_binary(SkipChars) ->
    split(N, T, [SkipChars | Acc], Cnt, Cols, Unicode).

logical([]) ->
    0;
logical([Char | T]) when is_integer(Char) ->
    1 + logical(T);
logical([Chars | T]) when is_list(Chars) ->
    length(Chars) + logical(T);
logical([SkipChars | T]) when is_binary(SkipChars) ->
    logical(T).

move_cursor(#state{ cols = W } = State, FromCol, ToCol) ->
    ?dbg({?FUNCTION_NAME, FromCol, ToCol}),
    [case (ToCol div W) - (FromCol div W) of
         0 -> "";
         N when N < 0 ->
             ?dbg({move, up, -N}),
             move(up, State, -N);
         N ->
             ?dbg({move, down, N}),
             move(down, State, N)
     end,
     case (ToCol rem W) - (FromCol rem W) of
         0 -> "";
         N when N < 0 ->
             ?dbg({down, left, -N}),
             move(left, State, -N);
         N ->
             ?dbg({down, right, N}),
             move(right, State, N)
     end].

move(up, #state{ up = Up }, N) ->
    lists:duplicate(N, Up);
move(down, #state{ down = Down }, N) ->
    lists:duplicate(N, Down);
move(left, #state{ left = Left }, N) ->
    lists:duplicate(N, Left);
move(right, #state{ right = Right }, N) ->
    lists:duplicate(N, Right).

cols([],_Unicode) ->
    0;
cols([Char | T], Unicode) when is_integer(Char) ->
    npwcwidth(Char, Unicode) + cols(T, Unicode);
cols([Chars | T], Unicode) when is_list(Chars) ->
    cols(Chars, Unicode) + cols(T, Unicode);
cols([SkipSeq | T], Unicode) when is_binary(SkipSeq) ->
    %% Any binary should be an ANSI escape sequence
    %% so we skip that
    cols(T, Unicode).

cols(ColsPerLine, CurrCols, Chars, Unicode) when CurrCols > ColsPerLine ->
    ColsPerLine + cols(ColsPerLine, CurrCols - ColsPerLine, Chars, Unicode);
cols(_ColsPerLine, CurrCols, [], _Unicode) ->
    CurrCols;
cols(ColsPerLine, CurrCols, ["\r\n" | T], Unicode) ->
    CurrCols + (ColsPerLine - CurrCols) + cols(ColsPerLine, 0, T, Unicode);
cols(ColsPerLine, CurrCols, [H | T], Unicode) ->
    cols(ColsPerLine, CurrCols + cols([H], Unicode), T, Unicode).

update_geometry(State) ->
    case tty_window_size(State#state.tty) of
        {ok, {Cols, Rows}} when Cols > 0 ->
            ?dbg({?FUNCTION_NAME, Cols}),
            State#state{ cols = Cols, rows = Rows };
        _Error ->
            ?dbg({?FUNCTION_NAME, _Error}),
            State
    end.

npwcwidthstring(String) when is_list(String) ->
    npwcwidthstring(unicode:characters_to_binary(String));
npwcwidthstring(String) ->
    case string:next_grapheme(String) of
        [] -> 0;
        [$\e | Rest] ->
            case re:run(String, ?ANSI_REGEXP, [unicode]) of
                {match, [{0, N}]} ->
                    <<_Ansi:N/binary, AnsiRest/binary>> = String,
                    npwcwidthstring(AnsiRest);
                _ ->
                    npwcwidth($\e) + npwcwidthstring(Rest)
            end;
        [H|Rest] -> npwcwidth(H) + npwcwidthstring(Rest)
    end.

npwcwidth(Char) ->
    npwcwidth(Char, true).
npwcwidth(Char, true) ->
    case wcwidth(Char) of
        {error, not_printable} -> 0;
        {error, enotsup} ->
            case unicode_util:is_wide(Char) of
                true -> 2;
                false -> 1
            end;
        C -> C
    end;
npwcwidth(Char, false) ->
    byte_size(char_to_latin1(Char)).


%% Return the xn fix for the current cursor position.
%% We use get_position to figure out if we need to calculate the current columns
%%  or not.
%%
%% We need to know the actual column because get_position will return the last
%% column number when the cursor is:
%%   * in the last column
%%   * off screen
%%
%% and it is when the cursor is off screen that we should do the xnfix.
xnfix(#state{ position = _, unicode = U } = State) ->
    xnfix(State, cols(State#state.buffer_before, U)).
%% Return the xn fix for CurrCols location.
xnfix(#state{ xn = true, cols = Cols } = State, CurrCols)
  when CurrCols =/= 0, CurrCols rem Cols == 0 ->
    [<<"\s">>,move(left, State, 1)];
xnfix(_, _CurrCols) ->
    ?dbg({xnfix, _CurrCols}),
    [].

characters_to_output(Chars) ->
    try unicode:characters_to_binary(Chars) of
        Binary ->
            Binary
    catch error:badarg ->
            unicode:characters_to_binary(
              lists:map(
                fun({ansi, Ansi}) ->
                        Ansi;
                   (Char) ->
                        Char
                end, Chars)
              )
    end.
characters_to_buffer(Chars) ->
    lists:flatmap(
      fun({ansi, _Ansi}) ->
              "";
         (Char) ->
              [Char]
      end, Chars).

insert_buf(State, Binary) when is_binary(Binary) ->
    insert_buf(State, Binary, [], []).
insert_buf(State, Bin, LineAcc, Acc) ->
    case string:next_grapheme(Bin) of
        [] when State#state.buffer_expand =/= undefined ->
            Expand = lists:reverse(LineAcc),
            {[Acc, characters_to_output(Expand)],
             State#state{ buffer_expand = characters_to_buffer(Expand) }};
        [] ->
            NewBB = characters_to_buffer(LineAcc) ++ State#state.buffer_before,
            NewState = State#state{ buffer_before = NewBB },
            {[Acc, characters_to_output(lists:reverse(LineAcc)), xnfix(NewState)],
             NewState};
        [$\t | Rest] ->
            insert_buf(State, Rest, [State#state.tab | LineAcc], Acc);
        [$\e | Rest] ->
            case re:run(Bin, State#state.ansi_regexp, [unicode]) of
                {match, [{0, N}]} ->
                    <<Ansi:N/binary, AnsiRest/binary>> = Bin,
                    case re:run(Bin, State#state.ansi_sgr, [unicode]) of
                        {match, [{0, N}]} ->
                            %% We include the graphics ansi sequences in the
                            %% buffer that we step over
                            insert_buf(State, AnsiRest, [Ansi | LineAcc], Acc);
                        _ ->
                            %% Any other ansi sequences are just printed and
                            %% then dropped as they "should" not effect rendering
                            insert_buf(State, AnsiRest, [{ansi, Ansi} | LineAcc], Acc)
                    end;
                _ ->
                    insert_buf(State, Rest, [$\e | LineAcc], Acc)
            end;
        [NLCR | Rest] when NLCR =:= $\n; NLCR =:= $\r ->
            Tail =
                if NLCR =:= $\n ->
                        <<$\r,$\n>>;
                   true ->
                        <<$\r>>
                end,
            if State#state.buffer_expand =:= undefined ->
                    insert_buf(State#state{ buffer_before = [], buffer_after = [] }, Rest, [],
                               [Acc, [characters_to_output(lists:reverse(LineAcc)), Tail]]);
               true ->
                    insert_buf(State, Rest, [binary_to_list(Tail) | LineAcc], Acc)
            end;
        [Cluster | Rest] when is_list(Cluster) ->
            insert_buf(State, Rest, [Cluster | LineAcc], Acc);
        %% We have gotten a code point that may be part of the previous grapheme cluster. 
        [Char | Rest] when Char >= 128, LineAcc =:= [], State#state.buffer_before =/= [],
                           State#state.buffer_expand =:= undefined ->
            [PrevChar | BB] = State#state.buffer_before,
            case string:next_grapheme([PrevChar | Bin]) of
                [PrevChar | _] ->
                    %% It was not part of the previous cluster, so just insert
                    %% it as a normal character
                    insert_buf(State, Rest, [Char | LineAcc], Acc);
                [Cluster | ClusterRest] ->
                    %% It was part of the previous grapheme cluster, so we output
                    %% it and insert it into the before_buffer
                    %% TODO: If an xnfix was done on PrevChar,
                    %%       then we should rewrite the entire grapheme cluster.
                    {_, ToWrite} = lists:split(length(lists:flatten([PrevChar])), Cluster),
                    insert_buf(State#state{ buffer_before = [Cluster | BB] },
                               ClusterRest, LineAcc,
                               [Acc, unicode:characters_to_binary(ToWrite)])
            end;
        [Char | Rest] when Char >= 128 ->
            insert_buf(State, Rest, [Char | LineAcc], Acc);
        [Char | Rest] ->
            case {isprint(Char), Char} of
                {true,_} ->
                    insert_buf(State, Rest, [Char | LineAcc], Acc);
                {false, 8#177} -> %% DEL
                    insert_buf(State, Rest, ["^?" | LineAcc], Acc);
                {false, _} ->
                    insert_buf(State, Rest, ["^" ++ [Char bor 8#40] | LineAcc], Acc)
            end
    end.

-spec to_latin1(erlang:binary()) -> erlang:iovec().
to_latin1(Bin) ->
    case is_usascii(Bin) of
        true -> [Bin];
        false -> lists:flatten([binary_to_latin1(Bin)])
    end.

is_usascii(<<Char/utf8,T/binary>>) when Char < 128 ->
    is_usascii(T);
is_usascii(<<>>) ->
    true;
is_usascii(_) ->
    false.

binary_to_latin1(Buffer) ->
    [char_to_latin1(CP) || CP <- unicode:characters_to_list(Buffer)].
char_to_latin1(UnicodeChar) when UnicodeChar >= 512 ->
    <<"\\x{",(integer_to_binary(UnicodeChar, 16))/binary,"}">>;
char_to_latin1(UnicodeChar) when UnicodeChar >= 128 ->
    <<"\\",(integer_to_binary(UnicodeChar, 8))/binary>>;
char_to_latin1(UnicodeChar) ->
    <<UnicodeChar>>.

encode(UnicodeChars, true) ->
    unicode:characters_to_binary(UnicodeChars);
encode(UnicodeChars, false) ->
    to_latin1(unicode:characters_to_binary(UnicodeChars)).

%% Using get_position adds about 10ms of latency
%% get_position(#state{ position = false }) ->
%%     unknown;
%% get_position(State) ->
%%     [] = write(State, State#state.position),
%%     get_position(State, <<>>).
%% get_position(State, Acc) ->
%%     receive
%%         {select,TTY,Ref,ready_input}
%%             when TTY =:= State#state.tty,
%%                  Ref =:= State#state.read ->
%%             {Bytes, <<>>} = read_input(State#state{ acc = Acc }),
%%             case re:run(Bytes, State#state.position_reply, [unicode]) of
%%                 {match,[{Start,Length},Row,Col]} ->
%%                     <<Before:Start/binary,_:Length/binary,After/binary>> = Bytes,
%%                     %% This should be put in State in order to not screw up the
%%                     %% message order...
%%                     [State#state.parent ! {{self(), State#state.tty}, {data, <<Before/binary, After/binary>>}}
%%                      || Before =/= <<>>, After =/= <<>>],
%%                     {binary_to_integer(binary:part(Bytes,Row)),
%%                      binary_to_integer(binary:part(Bytes,Col))};
%%                 nomatch ->
%%                     get_position(State, Bytes)
%%             end
%%     after 1000 ->
%%             unknown
%%     end.

-ifdef(debug).
dbg(_) ->
    ok.
-endif.

%% Nif functions
-spec isatty(stdin | stdout | stderr) -> boolean() | ebadf.
isatty(_Fd) ->
    erlang:nif_error(undef).
tty_create() ->
    erlang:nif_error(undef).
tty_init(_TTY, _Fd, _Options) ->
    erlang:nif_error(undef).
tty_set(_TTY) ->
    erlang:nif_error(undef).
setlocale(_TTY) ->
    erlang:nif_error(undef).
tty_select(_TTY, _SignalRef, _ReadRef) ->
    erlang:nif_error(undef).
write_nif(_TTY, _IOVec) ->
    erlang:nif_error(undef).
read_nif(_TTY, _Ref) ->
    erlang:nif_error(undef).
tty_window_size(_TTY) ->
    erlang:nif_error(undef).
isprint(_Char) ->
    erlang:nif_error(undef).
wcwidth(_Char) ->
    erlang:nif_error(undef).
sizeof_wchar() ->
    erlang:nif_error(undef).
wcswidth(_Char) ->
    erlang:nif_error(undef).
tgetent(Char) ->
    tgetent_nif([Char,0]).
tgetnum(Char) ->
    tgetnum_nif([Char,0]).
tgetflag(Char) ->
    tgetflag_nif([Char,0]).
tgetstr(Char) ->
    tgetstr_nif([Char,0]).
tgoto(Char, Arg) ->
    tgoto_nif([Char,0], Arg).
tgoto(Char, Arg1, Arg2) ->
    tgoto_nif([Char,0], Arg1, Arg2).
tgetent_nif(_Char) ->
    erlang:nif_error(undef).
tgetnum_nif(_Char) ->
    erlang:nif_error(undef).
tgetflag_nif(_Char) ->
    erlang:nif_error(undef).
tgetstr_nif(_Char) ->
    erlang:nif_error(undef).
tgoto_nif(_Ent, _Arg) ->
    erlang:nif_error(undef).
tgoto_nif(_Ent, _Arg1, _Arg2) ->
    erlang:nif_error(undef).
tty_read_signal(_TTY, _Ref) ->
    erlang:nif_error(undef).

