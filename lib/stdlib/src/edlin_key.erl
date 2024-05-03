%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2024. All Rights Reserved.
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
-export([get_key_map/0, get_valid_escape_key/2]).
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
get_valid_escape_key([C|Rest], {csi, [$;|Acc]}) ->
    case C of
        _ when $0 =< C, C =< $9 -> get_valid_escape_key(Rest, {csi, [C,$;|Acc]});
        _ -> get_valid_escape_key(Rest, {invalid, "\e["++reverse([$;|Acc])++[C]})
    end;
get_valid_escape_key([C|Rest], {csi, Acc}) ->
    case C of
        $~ -> get_valid_escape_key(Rest, {finished, "\e["++reverse([$~|Acc])});
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
            default => tab_expand_quit %% go to normal mode and evaluate key input again
        }
    }.

normal_map() ->
    #{
        %% Enter
        "\n" => new_line_finish,
        "\r" => new_line_finish,
        %%% Alt-Enter or Esc + Enter
        "\^[\n" => new_line,
        "\^[\r" => new_line,
        %% Tab ^I
        "\t" => tab_expand,

        %% Ctrl-alpha_key, can not distinguish case
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
        "\^]" => auto_blink, % ctrl+5 seems to do the same thing,

        %%# Alt-alpha_key or Esc + alpha_key, can distinguish case,
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
%% Alt-Shift-char, Alt-char or Esc + Shift-char, Esc + char
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
