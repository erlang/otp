<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2024-2025. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Creating a terminal application

This guide will show how to create a very simple tic-tac-toe game in
the shell. We will go through how to read key-strokes and how to update
the screen to show the tic-tac-toe board. The game will be implemented as an
`m:escript`, but it can just as well be implemented in a regular system.

Let us start by drawing the board which will look like this:

```txt
╔═══════╤═══════╤═══════╗
║┌─────┐│       │       ║
║│     ││       │       ║     Place an X by pressing Enter
║└─────┘│       │       ║
╟───────┼───────┼───────╢
║       │       │       ║
║       │       │       ║
║       │       │       ║
╟───────┼───────┼───────╢
║       │       │       ║
║       │       │       ║
║       │       │       ║
╚═══════╧═══════╧═══════╝
```


We will use the alternate screen buffer for our game so first we need to set
that up using `m:io_ansi`:

```
#!/usr/bin/env escript
main(_Args) ->
    
    %% Enable alternate screen buffer and hide cursor
    io_ansi:fwrite([alternate_screen, cursor_hide]),
    draw_board(),
    timer:sleep(5000),
    io_ansi:fwrite([alternate_screen_off, cursor_show]),
    ok.
```

We then use the box drawing parts of Unicode to draw our board:

```
draw_board() ->
    %% Place cursor at row 6 column 0
    io_ansi:fwrite([{cursor, 6, 0}])
    io:put_chars(
      ["     ╔═══════╤═══════╤═══════╗\r\n",
       "     ║       │       │       ║\r\n",
       "     ║       │       │       ║     Place an X by pressing Enter\r\n",
       "     ║       │       │       ║\r\n",
       "     ╟───────┼───────┼───────╢\r\n",
       "     ║       │       │       ║\r\n",
       "     ║       │       │       ║\r\n",
       "     ║       │       │       ║\r\n",
       "     ╟───────┼───────┼───────╢\r\n",
       "     ║       │       │       ║\r\n",
       "     ║       │       │       ║\r\n",
       "     ║       │       │       ║\r\n",
       "     ╚═══════╧═══════╧═══════╝\r\n"]),
    ok.
```

Let us add some interactivity to our game! To do that we need to change the
shell from running in `cooked` to `raw` mode. This is done by calling
[`shell:start_interactive({noshell, raw})`](`shell:start_interactive/1`).
We can then use `io:get_chars/2` to read key strokes from the user. The key
strokes will be returned as [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code), 
so we will use `io_ansi:scan/1` to interpret them.

It could look something like this:

```
main(_Args) ->
    ok = shell:start_interactive({noshell, raw}),
    
    try
        %% Enable alternate screen buffer, hide cursor and enable keypad_transmit_mode
        io_ansi:fwrite([alternate_screen, cursor_hide, keypad_transmit_mode]),
        draw_board(),
        loop(0)
    after ->
        io_ansi:fwrite([alternate_screen_off, cursor_show, keypad_transmit_mode_off]),
    end.

loop(Pos) ->
    io_ansi:fwrite(lists:flatten(draw_state(Pos))),
    case io:get_chars("", 1024) of
        eof -> stop;
        Chars ->
            case handle_input(io_ansi:scan(Chars), Pos) of
                stop -> stop;
                NewPos ->
                    io_ansi:fwrite(clear_selection(Pos)),
                    loop(NewPos)
            end
    end.

handle_input([kcursor_up | Rest], {Pos, Turn, State}) ->
    %% Up key
    handle_input(Rest, max(0, Pos - 3));
handle_input([kcursor_down | Rest], {Pos, Turn, State}) ->
    %% Down key
    handle_input(Rest, min(8, Pos + 3));
handle_input([kcursor_forward | Rest], {Pos, Turn, State}) ->
    %% right key
    handle_input(Rest, min(8, Pos + 1));
handle_input([kcursor_backward | Rest], {Pos, Turn, State}) ->
    %% left key
    handle_input(Rest, max(0, Pos - 1));
handle_input([<<"q",_Rest/binary>> | _T], _State) ->
    stop;
handle_input([_ | T], State) ->
    handle_input(T, State);
handle_input([], State) ->
    State.
```

Note that when using `io:get_chars/2` with the shell set in `{noshell, raw}` mode
it will return as soon as any data is available. The number of characters
is the maximum number that will be returned. We use 1024 here to make sure that
we always get all the data in one read.

We also need to draw the selection marker, we do this using some simple drawing
routines.

```
%% Clear/draw the selection markers, making sure
%% not to overwrite if a X or O exists.
clear_selection(Pos) ->
    [set_position(Pos),
     "       ",{cursor_backward, 7}, cursor_down,
     " ",{cursor_forward,5}," ",
     {cursor_backward, 7}, cursor_down,
     "       "].

draw_selection(Pos) ->
    [set_position(Pos),
     "┌─────┐",
     {cursor_backward, 7}, cursor_down,
     "│",{cursor_forward,5},"│",
     {cursor_backward, 7}, cursor_down,
     "└─────┘"].

%% Set the cursor position to be at the top
%% left of the field of the given position
set_position(Pos) ->
    Row = 6 + (Pos div 3) * 4,
    Col = 7 + (Pos rem 3) * 8,
    {cursor, Row + 1, Col - 1}.
```

Now we have a program where we can move the marker around the board.
To complete the game we need to add some state so that we know which
squares are marked and whos turn it is. You can find the final solution
in [tic-tac-toe.es](assets/tic-tac-toe.es).