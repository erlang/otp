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


We will use the alternate screen buffer for our game so first we need to set that up:

```
#!/usr/bin/env escript
main(_Args) ->
    
    io:put_chars("\e[?1049h"), %% Enable alternate screen buffer
    io:put_chars("\e[?25l"), %% Hide the cursor
    draw_board(),
    timer:sleep(5000),
    io:put_chars("\e[?25h"), %% Show the cursor
    io:put_chars("\e[?1049l"), %% Disable alternate screen buffer
    ok.
```

We then use the box drawing parts of Unicode to draw our board:

```
draw_board() ->
    io:put_chars("\e[5;0H"), %% Move cursor to top left
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
so we will have need to handle the codes for up, down, left, right and enter.

It could look something like this:

```
main(_Args) ->
    ok = shell:start_interactive({noshell, raw}),
    
    io:put_chars("\e[?1049h"), %% Enable alternate screen buffer
    io:put_chars("\e[?25l"), %% Hide the cursor
    draw_board(),
    loop(0),
    io:put_chars("\e[?25h"), %% Show the cursor
    io:put_chars("\e[?1049l"), %% Disable alternate screen buffer
    ok.

loop(Pos) ->
    io:put_chars(draw_selection(Pos)),
    %% Read at most 1024 characters from stdin.
    Chars = io:get_chars("", 1024),
    case handle_input(Chars, Pos) of
        stop -> stop;
        NewPos ->
            io:put_chars(clear_selection(Pos)),
            loop(NewPos)
    end.

handle_input("\e[A" ++ Rest, Pos) ->
    %% Up key
    handle_input(Rest, max(0, Pos - 3));
handle_input("\e[B" ++ Rest, Pos) ->
    %% Down key
    handle_input(Rest, min(8, Pos + 3));
handle_input("\e[C" ++ Rest, Pos) ->
    %% right key
    handle_input(Rest, min(8, Pos + 1));
handle_input("\e[D" ++ Rest, Pos) ->
    %% left key
    handle_input(Rest, max(0, Pos - 1));
handle_input("q" ++ _, _State) ->
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
%%   \b = Move cursor left
%%   \e[C = Move cursor right
%%   \n = Move cursor down
clear_selection(Pos) ->
    [set_position(Pos),
     "       ","\b\b\b\b\b\b\b\n",
     " \e[C\e[C\e[C\e[C\e[C ",
     "\b\b\b\b\b\b\b\n","       "].

draw_selection(Pos) ->
    [set_position(Pos),
     "┌─────┐","\b\b\b\b\b\b\b\n",
     "│\e[C\e[C\e[C\e[C\e[C│",
     "\b\b\b\b\b\b\b\n","└─────┘"].

%% Set the cursor position to be at the top
%% left of the field of the given position
set_position(Pos) ->
    Row = 6 + (Pos div 3) * 4,
    Col = 7 + (Pos rem 3) * 8,
    io_lib:format("\e[~p;~pH",[Row, Col]).
```
{: #monospace-font }

Now we have a program where we can move the marker around the board.
To complete the game we need to add some state so that we know which
squares are marked and whos turn it is. You can find the final solution
in [tic-tac-toe.es](assets/tic-tac-toe.es).