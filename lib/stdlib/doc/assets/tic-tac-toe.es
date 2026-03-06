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
    
    try
        %% Enable alternate screen buffer, hide cursor and enable keypad_transmit_mode
        io_ansi:fwrite([alternate_screen, cursor_hide, keypad_transmit_mode]),
        draw_board(),
        loop({0, "X", list_to_tuple(lists:duplicate(9, ""))}),
        timer:sleep(5000)
    after ->
        io_ansi:fwrite([alternate_screen_off, cursor_show, keypad_transmit_mode_off])
    end.

draw_board() ->
    io_ansi:fwrite([{cursor, 6, 0}]),
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
    io_ansi:fwrite(lists:flatten(draw_state(State))),
    case io:get_chars("", 1024) of
        eof -> stop;
        Chars ->
            case handle_input(io_ansi:scan(Chars), State) of
                stop -> stop;
                NewState ->
                    io_ansi:fwrite(clear_selection(State)),
                    loop(NewState)
            end
    end.

%% Clear/draw the selection markers, making sure
%% not to overwrite if a X or O exists.
clear_selection({Pos, _, _}) ->
    [set_position(Pos),
     "       ",{cursor_backward, 7}, cursor_down,
     " ",{cursor_forward,5}," ",
     {cursor_backward, 7}, cursor_down,
     "       "].

draw_selection({Pos, _, _}) ->
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

%% Update selection and whos turn it is
draw_state({_, Turn, _} = State) ->
    [draw_selection(State),
     {cursor, 8, 44}, Turn].

%% Draw X or O
draw_marker(Pos, Turn) ->
    [set_position(Pos), {cursor_forward, 3}, cursor_down, Turn].

handle_input([kcursor_up | Rest], {Pos, Turn, State}) ->
    %% Up key
    handle_input(Rest, {max(0, Pos - 3), Turn, State});
handle_input([kcursor_down | Rest], {Pos, Turn, State}) ->
    %% Down key
    handle_input(Rest, {min(8, Pos + 3), Turn, State});
handle_input([kcursor_forward | Rest], {Pos, Turn, State}) ->
    %% right key
    handle_input(Rest, {min(8, Pos + 1), Turn, State});
handle_input([kcursor_backward | Rest], {Pos, Turn, State}) ->
    %% left key
    handle_input(Rest, {max(0, Pos - 1), Turn, State});
handle_input([<<"\r",Rest/binary>> | T], {Pos, Turn, State} = OldState) ->
    NewState =
        case element(Pos+1, State) of
            "" when Turn =:= "X" ->
                io_ansi:fwrite(draw_marker(Pos, Turn)),
                {Pos, "O", setelement(Pos+1, State, Turn)};
            "" when Turn =:= "O" ->
                io_ansi:fwrite(draw_marker(Pos, Turn)),
                {Pos, "X", setelement(Pos+1, State, Turn)};
            _ -> io_ansi:fwrite("\^G"), OldState
        end,
    handle_input([Rest | T], NewState);
handle_input([<<"q",_Rest/binary>> | _T], _State) ->
    stop;
handle_input([<<>> | T], State) ->
    handle_input(T, State);
handle_input([C | T], State) ->
    io_ansi:fwrite([cursor_save, {cursor, 40, 0}, "Unknown character: ~p", cursor_restore],[C]),
    handle_input(T, State);
handle_input([], State) ->
    State.