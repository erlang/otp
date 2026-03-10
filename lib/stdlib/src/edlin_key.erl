%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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
-module(edlin_key).
-moduledoc false.
-export([get_key_map/0, get_valid_escape_key/2, normalize_kitty_key/1]).
-import(lists, [reverse/1, reverse/2]).
get_key_map() ->
    KeyMap = application:get_env(stdlib, shell_keymap, none),
    case KeyMap of
        none -> key_map();
        _ -> merge(KeyMap)
    end.

%% Incase we are receiving partial input, we need to wait for the rest of the input.
%% We return a mode whenever we are waiting for more input.
get_valid_escape_key([], {csi, [_]=Acc}=_Mode) ->
    %% we allow \e[1, \e[2... to be a valid escape sequences
    %% note that this blocks <Esc>[1;... inputed one character at a time 
    {key, "\e["++Acc, []};
get_valid_escape_key([], Res) ->
    case Res of
        {Atom, Acc, Rest} ->
            case Atom of
                finished -> {key, Acc, Rest};
                invalid -> {invalid, Acc, Rest}
            end;
        {Atom, Acc} ->
            case Atom of
                finished -> {key, Acc, []};
                invalid -> {invalid, Acc, []};
                csi -> {mode, {csi, Acc}}
            end;
        meta -> {escape_prefix, meta};
        meta_o -> {key,"\eO", []};
        meta_meta -> {escape_prefix, meta_meta};
        meta_csi -> {escape_prefix, meta_csi};
        meta_left_sq_bracket -> {escape_prefix, meta_left_sq_bracket}
    end;
get_valid_escape_key([C|Rest], none) ->
    case C of
        $\e -> get_valid_escape_key(Rest,meta);
        C when $\^@ =< C, C =< $\^_; C =:= $\^? -> {key, [C], Rest};
        _ -> {insert, C, Rest}
    end;
get_valid_escape_key([C|Rest], meta) ->
    case C of
        $\e -> get_valid_escape_key(Rest, meta_meta);
        $O -> get_valid_escape_key(Rest, meta_o);
        $[ -> get_valid_escape_key(Rest, meta_left_sq_bracket);
        _ when $! =< C, C =< $~ -> get_valid_escape_key(Rest, {finished, "\e"++[C]});
        _ when $\^@ =< C, C =< $\^_; C=:= $\^? -> get_valid_escape_key(Rest, {finished, "\e"++[C]});
        _ -> get_valid_escape_key(Rest, {invalid, "\e"++[C]})
    end;
get_valid_escape_key([C|Rest], meta_meta) ->
    case C of
        $[ -> get_valid_escape_key(Rest, meta_csi);
        _ when $! =< C, C =< $~ -> get_valid_escape_key(Rest, {finished, "\e\e"++[C]});
        _ -> get_valid_escape_key(Rest, {invalid, "\e\e"++[C]})
    end;
get_valid_escape_key([C|Rest], meta_o) ->
    case C of
        _ when $! =< C, C =< $~ -> get_valid_escape_key(Rest, {finished, "\eO"++[C]});
        _ -> get_valid_escape_key(Rest, {invalid, "\eO"++[C]})
    end;
get_valid_escape_key([C|Rest], meta_csi) ->
    case C of
        _ when $! =< C, C =< $~ -> get_valid_escape_key(Rest, {finished, "\e\e["++[C]});
        _ -> get_valid_escape_key(Rest, {invalid, "\e["++[C]})
    end;
get_valid_escape_key([C|Rest], meta_left_sq_bracket) ->
    case C of
        _ when $0 =< C, C =< $9 -> get_valid_escape_key(Rest, {csi, [C]});
        _ when $a =< C, C =< $z; $A =< C, C =< $Z -> get_valid_escape_key(Rest, {finished, "\e["++[C]});
        _ -> get_valid_escape_key(Rest, {invalid, "\e["++[C]})
    end;
get_valid_escape_key([C|Rest], {csi, [$:|Acc]}) ->
    case C of
        _ when $0 =< C, C =< $9 -> get_valid_escape_key(Rest, {csi, [C,$:|Acc]});
        _ -> get_valid_escape_key(Rest, {invalid, "\e["++reverse([$:|Acc])++[C]})
    end;
get_valid_escape_key([C|Rest], {csi, [$;|Acc]}) ->
    case C of
        _ when $0 =< C, C =< $9 -> get_valid_escape_key(Rest, {csi, [C,$;|Acc]});
        _ -> get_valid_escape_key(Rest, {invalid, "\e["++reverse([$;|Acc])++[C]})
    end;
get_valid_escape_key([C|Rest], {csi, Acc}) ->
    case C of
        $~ -> get_valid_escape_key(Rest, {finished, "\e["++reverse([$~|Acc])});
        $: -> get_valid_escape_key(Rest, {csi, [$:|Acc]});
        $; -> get_valid_escape_key(Rest, {csi, [$;|Acc]});
        _ when $0 =< C, C =< $9 -> get_valid_escape_key(Rest, {csi, [C|Acc]});
        $m -> {invalid, "\e["++reverse([$m|Acc]), [$m|Rest]};
        _ when $! =< C, C =< $~ -> get_valid_escape_key(Rest, {finished, "\e["++reverse([C|Acc])})
    end;
get_valid_escape_key([C|Rest], {finished, Acc}) ->
    case C of
        $~ -> get_valid_escape_key([], {finished, Acc++[C], Rest});
        _ -> get_valid_escape_key([], {finished, Acc, [C|Rest]})
    end;
get_valid_escape_key(Rest, {invalid, Acc}) ->
    {invalid, Acc, Rest};
get_valid_escape_key(Rest, Acc) ->
    {invalid, Acc, Rest}.

normalize_kitty_key("\e[" ++ _ = Key) ->
    case parse_kitty_key(Key) of
        {ok, Codepoint, Modifiers} ->
            kitty_to_legacy_key(Codepoint, Modifiers, Key);
        error ->
            case normalize_kitty_function_key(Key) of
                undefined -> Key;
                Normalized -> Normalized
            end
    end;
normalize_kitty_key(Key) ->
    Key.

parse_kitty_key("\e[" ++ Rest) ->
    case take_decimal(Rest) of
        error ->
            error;
        {Codepoint, Rest1} ->
            parse_kitty_key_tail(Codepoint, Rest1)
    end;
parse_kitty_key(_) ->
    error.

parse_kitty_key_tail(Codepoint, ":" ++ Rest) ->
    parse_kitty_key_tail(Codepoint, skip_decimal(Rest));
parse_kitty_key_tail(Codepoint, "u") ->
    {ok, Codepoint, 1};
parse_kitty_key_tail(Codepoint, ";" ++ Rest) ->
    case take_decimal(Rest) of
        error ->
            error;
        {Modifiers, Rest1} ->
            case skip_kitty_key_suffix(Codepoint, Rest1) of
                {ok, Codepoint, _} ->
                    {ok, Codepoint, Modifiers};
                error ->
                    error
            end
    end;
parse_kitty_key_tail(_, _) ->
    error.

skip_kitty_key_suffix(Codepoint, ":" ++ Rest) ->
    skip_kitty_key_suffix(Codepoint, skip_decimal(Rest));
skip_kitty_key_suffix(Codepoint, ";" ++ Rest) ->
    skip_kitty_key_suffix(Codepoint, skip_decimal(Rest));
skip_kitty_key_suffix(Codepoint, "u") ->
    {ok, Codepoint, 1};
skip_kitty_key_suffix(_, _) ->
    error.

take_decimal([C | Rest]) when $0 =< C, C =< $9 ->
    take_decimal(Rest, C - $0);
take_decimal(_) ->
    error.

take_decimal([C | Rest], Acc) when $0 =< C, C =< $9 ->
    take_decimal(Rest, Acc * 10 + C - $0);
take_decimal(Rest, Acc) ->
    {Acc, Rest}.

skip_decimal([C | Rest]) when $0 =< C, C =< $9 ->
    skip_decimal(Rest);
skip_decimal(Rest) ->
    Rest.

kitty_to_legacy_key(Codepoint, Modifiers, Fallback) ->
    Flags = Modifiers - 1,
    Alt = (Flags band 2) =/= 0,
    Ctrl = (Flags band 4) =/= 0,
    case kitty_base_key(Codepoint) of
        undefined ->
            case kitty_ctrl_key(Codepoint, Ctrl) of
                undefined ->
                    case kitty_alt_key(Codepoint, Alt, Ctrl) of
                        undefined -> Fallback;
                        Key when Alt -> "\e" ++ Key;
                        Key -> Key
                    end;
                Key ->
                    case Alt of
                        true -> "\e" ++ Key;
                        false -> Key
                    end
            end;
        Key when Alt ->
            "\e" ++ Key;
        Key ->
            Key
    end.

kitty_base_key(8) -> "\^H";
kitty_base_key(9) -> "\t";
kitty_base_key(13) -> "\r";
kitty_base_key(27) -> "\e";
kitty_base_key(127) -> "\^?";
kitty_base_key(_) -> undefined.

kitty_ctrl_key(127, true) ->
    "\^?";
kitty_ctrl_key(Codepoint, true) when $a =< Codepoint, Codepoint =< $z ->
    [Codepoint band 31];
kitty_ctrl_key(Codepoint, true) when $A =< Codepoint, Codepoint =< $Z ->
    [Codepoint band 31];
kitty_ctrl_key($@, true) -> [0];
kitty_ctrl_key($[, true) -> [27];
kitty_ctrl_key($\\, true) -> [28];
kitty_ctrl_key($], true) -> [29];
kitty_ctrl_key($^, true) -> [30];
kitty_ctrl_key($_, true) -> [31];
kitty_ctrl_key($?, true) -> [127];
kitty_ctrl_key(_, _) -> undefined.

kitty_alt_key(Codepoint, true, _Ctrl) when 32 =< Codepoint, Codepoint =< 126 ->
    [Codepoint];
kitty_alt_key(_, _, _) ->
    undefined.

normalize_kitty_function_key("\e[" ++ Rest) ->
    case parse_kitty_function_key(Rest) of
        {ok, Sequence} -> "\e[" ++ Sequence;
        error -> undefined
    end;
normalize_kitty_function_key(_) ->
    undefined.

parse_kitty_function_key("1~") ->
    {ok, "H"};
parse_kitty_function_key("4~") ->
    {ok, "F"};
parse_kitty_function_key("7~") ->
    {ok, "H"};
parse_kitty_function_key("8~") ->
    {ok, "F"};
parse_kitty_function_key(Rest) ->
    case take_decimal(Rest) of
        error ->
            error;
        {1, ";" ++ Rest1} ->
            parse_kitty_function_suffix(Rest1);
        {Codepoint, ";" ++ Rest1} when Codepoint =:= 2;
                                        Codepoint =:= 3;
                                        Codepoint =:= 5;
                                        Codepoint =:= 6;
                                        Codepoint =:= 7;
                                        Codepoint =:= 8;
                                        11 =< Codepoint, Codepoint =< 24 ->
            parse_kitty_function_tilde(Codepoint, Rest1);
        _ ->
            error
    end.

parse_kitty_function_suffix(Rest) ->
    case take_decimal(Rest) of
        error ->
            error;
        {Modifiers, [Suffix]} when Suffix =:= $A;
                                   Suffix =:= $B;
                                   Suffix =:= $C;
                                   Suffix =:= $D;
                                   Suffix =:= $F;
                                   Suffix =:= $H;
                                   Suffix =:= $P;
                                   Suffix =:= $Q;
                                   Suffix =:= $S ->
            {ok, "1;" ++ integer_to_list(Modifiers) ++ [Suffix]};
        _ ->
            error
    end.

parse_kitty_function_tilde(Codepoint, Rest) ->
    case take_decimal(Rest) of
        error ->
            error;
        {Modifiers, "~"} ->
            {ok, integer_to_list(Codepoint) ++ ";" ++ integer_to_list(Modifiers) ++ "~"};
        _ ->
            error
    end.

merge(KeyMap) ->
    merge(KeyMap, [normal, search, tab_expand, help], key_map()).
merge(_, [], KeyMap) ->
    KeyMap;
merge(InputKeyMap, [Mode|ShellModes], KeyMap) ->
    InputKeyMapModeValidated = maps:filtermap(
        fun(Key, Value) when is_list(Key), is_atom(Value) ->
            try
                {key, Key, []} = get_valid_escape_key(Key, none),
                case lists:member(Value,valid_functions()) of
                    true -> {true, Value};
                    false -> io:format(standard_error, "Invalid function ~p in entry {~p,~p}~n", [Value, Key, Value]), false
                end
            catch
                _:_ ->
                    io:format(standard_error, "Invalid key ~p in entry {~p,~p}~n", [Key,Key,Value]),
                    false
            end;
            (default, Value) ->
                case lists:member(Value,valid_functions()) of
                    true -> {true, Value};
                    false -> io:format(standard_error, "Invalid function ~p in entry {default,~p}~n", [Value, Value]), false
                end;
            (Key,Value) ->
                io:format(standard_error, "Invalid entry {~p,~p}~n", [Key, Value]), false
        end, maps:get(Mode, InputKeyMap, #{})),
    KeyMap1 = KeyMap#{Mode => maps:merge(maps:get(Mode, KeyMap), InputKeyMapModeValidated)},

    merge(InputKeyMap, ShellModes, KeyMap1).

%% Default Keymap for erl shell.
%% This is a keymap that corresponds to what was previously
%% configured in edlin.erl.
%% They are now in a map of maps that supports multiple shell modes,
%% normal, search, expand.
%%
%% See below for unused keys.
%%
key_map() -> #{
        normal => normal_map(),
        search => #{
            "\^[OA" => move_expand_up,
            "\^[[A" => move_expand_up,
            "\^[OB" => move_expand_down,
            "\^[[B" => move_expand_down,
            "\^[[6~" => scroll_expand_down,
            "\^[[5~" => scroll_expand_up,
            "\^R" => skip_up,
            "\^S" => skip_down,
            "\^[C" => search_cancel,
            "\^[c" => search_cancel,
            "\n" => search_found,
            "\r" => search_found,
            "\^H" => backward_delete_char,
            "\^?" => backward_delete_char,
            default => search_quit
            %% # everything else should exit search mode and edit the search result (search_quit),
        },
        tab_expand => #{
            "\^[OA" => move_expand_up,
            "\^[[A" => move_expand_up,
            "\^[OB" => move_expand_down,
            "\^[[B" => move_expand_down,
            "\^[[6~" => scroll_expand_down,
            "\^[[5~" => scroll_expand_up,
            "\t" => tab_expand_full,
            default => tab_expand_quit %% go to normal mode and evaluate key input again
        },
        help => #{
            "\^[OA" => move_expand_up,
            "\^[[A" => move_expand_up,
            "\^[OB" => move_expand_down,
            "\^[[B" => move_expand_down,
            "\^[[6~" => scroll_expand_down,
            "\^[[5~" => scroll_expand_up,
            "\^[h" => help_full,
            default => tab_expand_quit %% go to normal mode and evaluate key input again
        }
    }.

normal_map() ->
    #{
        %% Enter
        "\n" => new_line_finish,
        "\r" => new_line_finish,
        %%% Alt+Enter or Esc + Enter
        "\^[\n" => new_line,
        "\^[\r" => new_line,
        %% Tab ^I
        "\t" => tab_expand,

        %% Ctrl+alpha_key, can not distinguish case
        "\^A" => beginning_of_line,
        "\^B" => backward_char,
        %%"\^C" => sig_term_menu, currently handled by user_drv.erl
        "\^D" => forward_delete_char,
        "\^E" => end_of_line,
        "\^F" => forward_char,
        %%"\^G" => jcl_menu, currently handled by user_drv.erl
        "\^H" => backward_delete_char,
        %%"\^I" => tab_expand, same as \t
        %%"\^J" => new_line_finish, same as \n
        "\^K" => kill_line,
        "\^L" => clear,
        %%"\^M" => new_line_finish, same as \r
        "\^N" => history_down,
        "\^O" => open_editor,
        "\^P" => history_up,
        "\^R" => search,
        "\^T" => transpose_char,
        "\^U" => backward_kill_line,
        "\^W" => backward_kill_word,
        %%"\^X" => ,
        "\^Y" => yank,
        %%"\^Z" => sig_stop, currently not handled by edlin.erl
        "\^]" => auto_blink, % Ctrl+5 seems to do the same thing,

        %%# Alt+alpha_key or Esc + alpha_key, can distinguish case,
        "\^[B" => backward_word,
        "\^[b" => backward_word,
        "\^[c" => clear_line,
        "\^[D" => kill_word,
        "\^[d" => kill_word,
        "\^[F" => forward_word,
        "\^[f" => forward_word,
        "\^[r" => format_expression,
        "\^[h" => help,
        "\^[L" => redraw_line,
        "\^[l" => redraw_line,
        "\^[o" => open_editor,
        "\^[T" => transpose_word,
        "\^[t" => transpose_word,
        "\^[<" => beginning_of_expression,
        "\^[>" => end_of_expression,

        %% # Deletion keys
        %% ## Backspace
        "\^?" => backward_delete_char,
        %% ## Alt+Backspace
        "\^[\^?" => backward_kill_word,
        %% ## Del
        "\^[[3~" => forward_delete_char,
        "\^[[3;5~" => forward_delete_word,

        %% # Navigation keys
        %% ## Home
        "\^[[H" => beginning_of_line,
        "\^[OH" => beginning_of_line,

        %% ## End
        "\^[[F" => end_of_line,
        "\^[OF" => end_of_line,

        %% # Arrow keys
        %% ## Up
        "\^[OA" => history_up,
        "\^[[A" => history_up,
        "\^[[1;3A" => backward_line,
        "\^[[1;5A" => backward_line,
        "\^[[1;4A" => beginning_of_expression,

        %% ## Down
        "\^[OB" => history_down,
        "\^[[B" => history_down,
        "\^[[1;3B" => forward_line,
        "\^[[1;5B" => forward_line,
        "\^[[1;4B" => end_of_expression,

        %% ## Left
        "\^[OD" => backward_char,
        "\^[[D" => backward_char,
        "\^[[3D" => backward_word,
        "\^[[1;3D" => backward_word,
        "\^[[5D" => backward_word,
        "\^[[1;5D" => backward_word,

        %% ## Right
        "\^[OC" => forward_char,
        "\^[[C" => forward_char,
        "\^[[3C" => forward_word,
        "\^[[1;3C" => forward_word,
        "\^[[5C" => forward_word,
        "\^[[1;5C" => forward_word,
        default => none
    }.
valid_functions() ->
    [auto_blink,           %% Automatically close the closest matching opening parenthesis
     backward_char,        %% Move backward one character
     backward_delete_char, %% Delete the character behind the cursor
     backward_delete_word, %% Delete the word behind the cursor
     backward_kill_line,   %% Delete all characters from the cursor to the beginning of the line and save them in the kill buffer
     backward_kill_word,   %% Delete the word behind the cursor and save it in the kill buffer
     backward_line,        %% Move backward one line
     backward_word,        %% Move backward one word
     beginning_of_expression,%% Move to the beginning of the expression
     beginning_of_line,    %% Move to the beginning of the line
     clear,                %% Clear the screen
     clear_line,           %% Clear the current expression
     end_of_expression,    %% Move to the end of the expression
     end_of_line,          %% Move to the end of the line
     format_expression,    %% Format the current expression
     forward_char,         %% Move forward one character
     forward_delete_char,  %% Delete the character under the cursor
     forward_delete_word,  %% Delete the characters until the closest non-word character
     forward_line,         %% Move forward one line
     forward_word,         %% Move forward one word
     help,                 %% Open up a pager with help for function or module closest to the cursor
     history_down,         %% Move to the next item in the history
     history_up,           %% Move to the previous item in the history
     %%jcl_menu,
     kill_line,            %% Delete all characters from the cursor to the end of the line and save them in the kill buffer
     kill_word,            %% Delete the word behind the cursor and save it in the kill buffer
     move_expand_up,       %% Move up one line in the expand area e.g. help or tab completion pager
     move_expand_down,     %% Move down one line in the expand area e.g. help or tab completion pager
     new_line_finish,      %% Add a newline at the end of the line and try to evaluate the current expression
     new_line,             %% Add a newline at the cursor position
     none,                 %% Do nothing
     open_editor,          %% Open the current line in an editor i.e. EDITOR=code -w
     redraw_line,          %% Redraw the current line
     scroll_expand_up,     %% Scroll up five lines in the expand area e.g. help or tab completion pager
     scroll_expand_down,   %% Scroll down five lines in the expand area e.g. help or tab completion pager
     search_cancel,        %% Cancel the current search
     search_found,         %% Accept the current search result and submit it
     search_quit,          %% Accept the current search result, but edit it before submitting
     search,               %% Enter search mode, search the history
     %%sig_stop,  
     %%sig_term_menu,
     skip_down,            %% Skip to the next line in the history that matches the current search expression
     skip_up,              %% Skip to the previous line in the history that matches the current search expression
     tab_expand_full,      %% Output all possible tab completions
     tab_expand_quit,      %% Go back to normal mode
     tab_expand,           %% Autocomplete the current word, or show 5 lines of possible completions
     transpose_char,       %% Swap the character behind the cursor with the one in front of it
     transpose_word,       %% Swap the word behind the cursor with the one in front of it
     yank].                %% Insert the contents of the kill buffer at the cursor position

%% Unused keys in normal mode:
%% Ctrl+char
%% ^Q
%% ^S
%% ^V
%% ^@, ^\, ^], ^^, ^_  %% not straightforward how to type these
%% 
%% Alt+Shift+char, Alt+char or Esc + Shift+char, Esc + char
%% ^[A, a
%% ^[C
%% ^[E, e
%% ^[G, g
%% ^[H, h
%% ^[I, i
%% ^[J, j
%% ^[K, k
%% ^[M, m
%% ^[N, n
%% ^[O, o
%% ^[P, p
%% ^[Q, q
%% ^[R, r
%% ^[S, s
%% ^[U, u
%% ^[V, v
%% ^[W, w
%% ^[X, x
%% ^[Z, z
%% ^[1, ^[2, ^[3, ^[4, ^[5, ^[6, ^[7, ^[8, ^[9, ^[0
%% Any symbol that is typable while holding alt key, or esc key followed by a symbol:
%% e.g. ^[!, ^[@, ^[#, ^[$, ^[%, ^[^, ^[&, ^[*, ^[(, ^[),
%% (F1-F4)
%% ^[OP, ^[OQ, ^[OR, ^[OS
%% ^[[1;2P, ^[[1;2Q, ^[[1;2R, ^[[1;2S (Shift-F1-F4)
%% ^[[1;5P, ^[[1;5Q, ^[[1;5R, ^[[1;5S (Ctrl-F1-F4)
%% (F5-F12)
%% ^[15~, ^[17~, ^[18~, ^[19~, ^[20~, ^[21~, ^[23~, ^[24~
%% ^[[15;2~, ^[[17;2~, ^[[18;2~, ^[[19;2~, ^[[20;2~, ^[[21;2~, ^[[23;2~, ^[[24;2~ (Shift-F5-F12)
%% ^[[15;5~, ^[[17;5~, ^[[18;5~, ^[[19;5~, ^[[20;5~, ^[[21;5~, ^[[23;5~, ^[[24;5~ (Ctrl-F5-F12)
