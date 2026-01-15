#!/usr/bin/env escript

%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

main(_Args) ->
    ok = shell:start_interactive({noshell, raw}),
    
    io:put_chars("\e[?1049h"), %% Enable alternate screen buffer
    io:put_chars("\e[?25l"), %% Hide the cursor
    draw_board(),
    loop({0, "X", list_to_tuple(lists:duplicate(9, ""))}),
    io:put_chars("\e[?25h"), %% Show the cursor
    io:put_chars("\e[?1049l"), %% Disable alternate screen buffer
    ok.

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

loop(State) ->
    io:put_chars(draw_state(State)),
    case handle_input(io:get_chars("", 30), State) of
        stop -> stop;
        NewState ->
            io:put_chars(clear_selection(State)),
            loop(NewState)
    end.

%% Clear/draw the selection markers, making sure
%% not to overwrite if a X or O exists.
%%   \b = Move cursor left
%%   \e[C = Move cursor right
%%   \n = Move cursor down
clear_selection({Pos, _, _}) ->
    [set_position(Pos),
     "       ","\b\b\b\b\b\b\b\n",
     " \e[C\e[C\e[C\e[C\e[C ",
     "\b\b\b\b\b\b\b\n","       "].

draw_selection({Pos, _, _}) ->
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

%% Update selection and whos turn it is
draw_state({_, Turn, _} = State) ->
    [draw_selection(State),
     io_lib:format("\e[7;45H~s",[Turn])].

%% Draw X or O
draw_marker(Pos, Turn) ->
    [set_position(Pos), "\e[C\e[C\e[C\n", Turn].

handle_input(eof, _State) ->
    stop;
handle_input("\e[A" ++ Rest, {Pos, Turn, State}) ->
    %% Up key
    handle_input(Rest, {max(0, Pos - 3), Turn, State});
handle_input("\e[B" ++ Rest, {Pos, Turn, State}) ->
    %% Down key
    handle_input(Rest, {min(8, Pos + 3), Turn, State});
handle_input("\e[C" ++ Rest, {Pos, Turn, State}) ->
    %% right key
    handle_input(Rest, {min(8, Pos + 1), Turn, State});
handle_input("\e[D" ++ Rest, {Pos, Turn, State}) ->
    %% left key
    handle_input(Rest, {max(0, Pos - 1), Turn, State});
handle_input("\r" ++ Rest, {Pos, Turn, State} = OldState) ->
    NewState =
        case element(Pos+1, State) of
            "" when Turn =:= "X" ->
                io:put_chars(draw_marker(Pos, Turn)),
                {Pos, "O", setelement(Pos+1, State, Turn)};
            "" when Turn =:= "O" ->
                io:put_chars(draw_marker(Pos, Turn)),
                {Pos, "X", setelement(Pos+1, State, Turn)};
            _ -> io:put_chars("\^G"), OldState
        end,
    handle_input(Rest, NewState);
handle_input("q" ++ _, _State) ->
    stop;
handle_input([_ | T], State) ->
    handle_input(T, State);
handle_input([], State) ->
    State.