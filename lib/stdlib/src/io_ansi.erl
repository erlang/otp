%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

%% 
%% https://hexdocs.pm/elixir/IO.ANSI.html
%% https://en.wikipedia.org/wiki/ANSI_escape_code
%% https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
%% https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
%%

-module(io_ansi).
-moduledoc """
Controlling the terminal using virtual terminal sequences (aka [ANSI escape codes]).

This module provides an interface to emit and parse virtual terminal sequences (VTS),
also known as [ANSI escape codes]. VTS can be used to:

- change the style of text or background in the terminal by adding color or emphasis.
- delete printed characters or lines.
- move, hide or show the cursor

and more things. As different terminals are interpret VTSs slightly
differently, `m:io_ansi` uses the local [terminfo] database together with
predefined sequences to emit the correct sequence for the terminal that is
currently used. To fetch values directly from the [terminfo] database you can use
`tput/2`, `tigetnum/1` and `tigetflag/1`.

`m:io_ansi` provides two interfaces to emit sequences. You can either call the
function representing the sequence you want to emit, for example `io_ansi:blue()`
and it will return the sequence representing blue.

```erlang
1> io_ansi:blue().
<<"\e[34m">>
```

This will use the [terminfo] database locally where the call is made, so it may
not be correct if used across nodes.

You can also use the [`io_ansi:format/1,2,3`](`io_ansi:format/3`) functions
which works just as `io_lib:bformat/3`, except that it also accepts atoms and
tuples that represent VTSs. For example:

```erlang
1> io_ansi:format([blue,"~p"], [red]).
<<"\e[34mred\e(B\e[m">>
```

`io_ansi:format/3` will automatically reset the terminal to its original state
and strip any VTSs that are not supported by the terminal. It can also be disabled
through an option. For example:

```erlang
1> io_ansi:format([blue,"~p"], [red], [{enabled, false}]).
<<"red">>
```

Finally there is [`io_ansi:fwrite/1,2,3,4`](`io_ansi:fwrite/4`) which does not
return the string to be printed, but instead sends it to the `t:io:device/0`
that should handle it. `io_ansi:fwrite/4` works across nodes and will use the
[terminfo] database where the data is outputted to decide what to emit.

[terminfo]: https://man7.org/linux/man-pages/man5/terminfo.5.html
[ANSI escape codes]: https://en.wikipedia.org/wiki/ANSI_escape_code
""".


-export([tput/1, tput/2, tigetnum/1, tigetflag/1, tinfo/0]).
-export([format/1, format/2, format/3, fwrite/1, fwrite/2, fwrite/3, fwrite/4,
         enabled/0, enabled/1, scan/1]).

-export([black/0, blue/0, cyan/0, green/0, magenta/0, red/0, white/0, yellow/0,
         color/1, color/3, default_color/0]).
-export([black_background/0, red_background/0, green_background/0, yellow_background/0,
         blue_background/0, magenta_background/0, cyan_background/0, white_background/0,
         background/1, background/3, default_background/0]).
-export([light_black/0, light_red/0, light_green/0, light_yellow/0, light_blue/0,
         light_magenta/0, light_cyan/0, light_white/0]).
-export([light_black_background/0, light_red_background/0, light_green_background/0,
         light_yellow_background/0, light_blue_background/0, light_magenta_background/0,
         light_cyan_background/0, light_white_background/0]).
-export([modify_color/4]).
-export([bold/0, bold_off/0, underline/0, underline_off/0, negative/0, negative_off/0]).
-export([hyperlink_start/1, hyperlink_start/2, hyperlink_reset/0]).
-export([clear/0, erase_display/0, insert_character/1, delete_character/0,
         delete_character/1, erase_character/1, insert_line/0, insert_line/1,
         delete_line/0, delete_line/1, erase_line/0]).
-export([alternate_character_set_mode/0, alternate_character_set_mode_off/0]).
-export([cursor/2, cursor_up/0, cursor_up/1, cursor_down/0, cursor_down/1,
         cursor_forward/0, cursor_forward/1, cursor_backward/0, cursor_backward/1,
         cursor_home/0, reverse_index/0, cursor_save/0, cursor_restore/0, 
         cursor_show/0, cursor_hide/0, 
         cursor_next_line/0, cursor_previous_line/0, cursor_horizontal_absolute/1,
         cursor_vertical_absolute/1, cursor_horizontal_vertical/2, cursor_report_position/0]).
-export([alternate_screen/0, alternate_screen_off/0,
        scroll_forward/0, scroll_forward/1, scroll_backward/0, scroll_backward/1,
        scroll_change_region/2]).
-export([tab/0, tab_backward/0, tab_set/0, tab_clear/0, tab_clear_all/0]).
-export([keypad_transmit_mode/0, keypad_transmit_mode_off/0]).
-export([reset/0, device_report_attributes/0]).

-import(lists, [concat/1]).

-doc "The format string that can be passed to `format/3` and `fwrite/4`".
-type format() :: [string() | vts()].

-doc "Virtual terminal sequences that control the foreground (aka text) color.".
-type foreground_color() :: black | blue | cyan | green | magenta | red | white | yellow |
                            light_black | light_blue | light_cyan | light_green |
                            light_magenta | light_red | light_white | light_yellow |
                            {color, 0..255} | {color, R :: 0..255, G :: 0..255, B :: 0..255} |
                            default_color.
-doc "Virtual terminal sequences that control the background color.".
-type background_color() :: black_background | blue_background | cyan_background |
                            green_background | magenta_background | red_background |
                            white_background | yellow_background | default_background |
                            light_black_background | light_blue_background |
                            light_cyan_background | light_green_background |
                            light_magenta_background | light_red_background |
                            light_white_background | light_yellow_background |
                            {color_background, 0..255} |
                            {color_background, R :: 0..255, G :: 0..255, B :: 0..255}.
-doc "Virtual terminal sequences that control color.".
-type color() :: foreground_color() | background_color() |
                 {modify_color, Index :: 0..255, R :: 0..255, G :: 0..255, B :: 0..255}.

-doc "Virtual terminal sequences that control text style.".
-type style() :: bold | bold_off | underline | underline_off | negative | negative_off.

-type hyperlink_params() :: [{Key :: unicode:chardata(), Value :: unicode:chardata()}].

-doc "Virtual terminal sequences that control whether emitted text shall be a hyper link or not.".
-type hyperlink() :: {hyperlink, URL :: uri_string:uri_string(), Text :: unicode:chardata()} |
                     {hyperlink, URL :: uri_string:uri_string(), hyperlink_params(), Text :: unicode:chardata()} |
                     {hyperlink_start, URL :: uri_string:uri_string()} |
                     {hyperlink_start, URL :: uri_string:uri_string(), hyperlink_params()} |
                     hyperlink_reset.

-doc "Virtual terminal sequences that control text formatting.".
-type text_formatting() :: color() | style() | hyperlink().
-doc "Virtual terminal sequences that can erase or owerwrite text.".
-type text_modification() :: clear | erase_display |
                             insert_character | delete_character | erase_character |
                             insert_line | delete_line | erase_line.
-doc "Virtual terminal sequences that works on text.".
-type text() :: text_formatting() | text_modification() |
                alternate_character_set_mode | alternate_character_set_mode_off.
-doc "Virtual terminal sequences that controls the cursor.".
-type cursor() ::
        {cursor, Line :: non_neg_integer(), Column :: non_neg_integer()} |
        cursor_down | cursor_up | cursor_backward | cursor_forward |
        {cursor_down | cursor_backward | cursor_forward | cursor_up, N :: non_neg_integer()} |
        cursor_home | reverse_index | cursor_save | cursor_restore |
        cursor_show | cursor_hide |
        cursor_next_line | cursor_previous_line | cursor_horizontal_absolute |
        cursor_vertical_absolute | cursor_horizontal_vertical | cursor_report_position.
-doc "Virtual terminal sequences that controls the screen.".
-type window() :: alternate_screen | alternate_screen_off |
                  scroll_forward | scroll_backward | scroll_change_region.
-doc "Virtual terminal sequences that works with tabs.".
-type tab() :: tab | tab_backward | tab_set | tab_clear | tab_clear_all.
-doc "Virtual terminal sequences for cursor input.".
-type input() :: keypad_transmit_mode | keypad_transmit_mode_off |
        kcursor_down | kcursor_up | kcursor_backward | kcursor_forward | 
        kcursor_home | kcursor_end.
-doc "Virtual terminal sequences.".
-type vts() :: text() | cursor() | window() | tab() | input() | reset | device_report_attributes.

-type option() :: {reset, boolean()} | { enabled, boolean()} | io_lib:format_options().
-type options() :: [option()].

-export_type([vts/0]).

-doc #{ equiv => tput(TermInfoCap, []) }.
-doc #{ group => ~"Functions: terminfo" }.
-spec tput(TermInfoCap :: string()) -> unicode:unicode_binary().
tput(TermInfoCap) ->
    tput(TermInfoCap, []).

-doc """
Returns the string representing the action taken by the given terminal capability.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tput/2` will use the terminfo definition
associated with the `TERM` environment variable when the Erlang VM is started.
It is not possible to change after startup.

If the given capability is not defined in the terminfo database an `enotsup`
error is generated, if the given capability is invalid a `badarg` error is 
generated.

This function does not work on Windows and will always generate a `badarg`
exception.

Example:

```erlang
%% Set the foreground color to 3
1> io_ansi:tput("setaf",[3]).
<<"\e[33m">>
%% Move the cursor up 2 spaces
2> io_ansi:tput("cuu",[2]).
<<"\e[2A">>
%% Move the cursor down 1 space
3> io_ansi:tput("cud1").
<<"\n">>
%% unsupported capability
4> io_ansi:tput("slm").
** exception error: {enotsup,"slm"}
     in function  io_ansi:tput/2
%% unknown capability
5> io_ansi:tput("foobar").
** exception error: {einval,"foobar",[]}
     in function  io_ansi:tput/2
```
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tput(TermInfoCapName :: string(), Args :: [integer()]) ->
    unicode:unicode_binary().
tput(TermInfoCap, Args) ->
    try prim_tty:tigetstr(TermInfoCap) of
        {ok, TermInfoStr} ->
            try prim_tty:tputs(TermInfoStr, Args) of
                {ok, S} -> S
            catch error:badarg ->
                erlang:error({badarg, TermInfoCap, Args})
            end;
        false ->
            erlang:error({enotsup, TermInfoCap})
    catch error:badarg ->
        erlang:error({einval, TermInfoCap, Args})
    end.

-doc """
Returns the number representing a terminfo capability.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tigetnum/1` will use the terminfo
definition associated with the `TERM` environment variable when the Erlang VM is
started. It is not possible to change after startup.

Returns `-1` if the capability is not available.

Example:

```erlang
1> io_ansi:tigetnum("co").
80
2> io_ansi:tigetnum("foobar").
-1
```
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tigetnum(TermInfoCapName :: string()) -> -1 | non_neg_integer().
tigetnum(TermInfoCap) ->
    prim_tty:tigetnum(TermInfoCap).

-doc """
Returns the true if the terminfo capability is available, otherwise false.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tigetflag/1` will use the terminfo
definition associated with the `TERM` environment variable when the Erlang VM is
started. It is not possible to change after startup.

Example:

```erlang
1> io_ansi:tigetflag("xn").
true
2> io_ansi:tigetflag("foobar").
false
```
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tigetflag(TermInfoCapName :: string()) -> boolean().
tigetflag(TermInfoCap) ->
    prim_tty:tigetflag(TermInfoCap).

-doc """
Returns information about all available terminfo capabilities. See
the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation for details on each.

`tinfo/0` will use the terminfo definition associated with the `TERM` environment
variable when the Erlang VM is started. It is not possible to change after startup.

When calling `tput/2`, `tigetnum/1` and `tigetflag/1` you should provide the `name`
of the capability you want.

Example:

```erlang
1> io_ansi:tinfo().
#{ bool => [#{code => "xr",name => "OTxr",full_name => "return_does_clr_eol"} | ...],
   str => [#{code => "bx",name => "box1",full_name => "box_chars_1"} | ...],
   num => [#{code => "kn",name => "OTkn", full_name => "number_of_function_keys"} | ...]
 }
```
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tinfo() -> #{ bool := [#{ code := string(), name := string(), full_name := string()}]}.
tinfo() -> 
    prim_tty:tinfo().

-define(FUNCTION(NAME),
        NAME() -> Fun = lookup(NAME, []), unicode:characters_to_binary(Fun())).
-define(FUNCTION(NAME, ARG1),
        NAME(ARG1) -> Fun = lookup(NAME, [ARG1]), unicode:characters_to_binary(Fun(ARG1))).
-define(FUNCTION(NAME, ARG1, ARG2),
        NAME(ARG1, ARG2) -> Fun = lookup(NAME, [ARG1, ARG2]), unicode:characters_to_binary(Fun(ARG1, ARG2))).
-define(FUNCTION(NAME, ARG1, ARG2, ARG3),
        NAME(ARG1, ARG2, ARG3) -> Fun = lookup(NAME, [ARG1, ARG2, ARG3]), unicode:characters_to_binary(Fun(ARG1, ARG2, ARG3))).
-define(FUNCTION(NAME, ARG1, ARG2, ARG3, ARG4),
        NAME(ARG1, ARG2, ARG3, ARG4) -> Fun = lookup(NAME, [ARG1, ARG2, ARG3, ARG4]), unicode:characters_to_binary(Fun(ARG1, ARG2, ARG3, ARG4))).

-define(SPEC(NAME),
        -spec NAME() -> unicode:chardata()).
-define(SPEC(NAME, ARG1),
        -spec NAME(ARG1 :: integer()) -> unicode:chardata()).
-define(SPEC(NAME, ARG1, ARG2),
        -spec NAME(ARG1 :: integer(), ARG2 :: integer()) -> unicode:chardata()).

-doc """
Change foreground (aka text) color to black.

Example:
```erlang
1> io_ansi:black().
<<"\e[30m">>
```
""".
?SPEC(black).
?FUNCTION(black).

-doc """
Change foreground (aka text) color to red.

Example:
```erlang
1> io_ansi:red().
<<"\e[31m">>
```
""".
?SPEC(red).
?FUNCTION(red).

-doc """
Change foreground (aka text) color to green.

Example:
```erlang
1> io_ansi:green().
<<"\e[32m">>
```
""".
?SPEC(green).
?FUNCTION(green).

-doc """
Change foreground (aka text) color to yellow.

Example:
```erlang
1> io_ansi:yellow().
<<"\e[33m">>
```
""".
?SPEC(yellow).
?FUNCTION(yellow).

-doc """
Change foreground (aka text) color to blue.

Example:
```erlang
1> io_ansi:blue().
<<"\e[34m">>
```
""".
?SPEC(blue).
?FUNCTION(blue).

-doc """
Change foreground (aka text) color to magenta.

Example:
```erlang
1> io_ansi:magenta().
<<"\e[35m">>
```
""".
?SPEC(magenta).
?FUNCTION(magenta).

-doc """
Change foreground (aka text) color to cyan.

Example:
```erlang
1> io_ansi:cyan().
<<"\e[36m">>
```
""".
?SPEC(cyan).
?FUNCTION(cyan).

-doc """
Change foreground (aka text) color to white.

Example:
```erlang
1> io_ansi:white().
<<"\e[37m">>
```
""".
?SPEC(white).
?FUNCTION(white).

-doc """
Change foreground (aka text) color to index color. `Index` 0-15 are equivilant to
the named colors in `t:foreground_color/0` in the order that they are listed.

Example:
```erlang
1> io_ansi:color(5).
<<"\e[35m">>
2> io_ansi:color(80).
<<"\e[38;5;80m">>
```
""".
-spec color(Index :: 0..255 | 0..87) -> unicode:chardata().
?FUNCTION(color, Index).

-doc """
Change foreground (aka text) color to RGB color.

Example:
```erlang
1> io_ansi:color(255, 0, 0).
<<"\e[38;2;255;0;0m">>
```
""".
-spec color(0..255, 0..255, 0..255) -> unicode:chardata().
?FUNCTION(color, Red, Green, Blue).

-doc """
Change foreground (aka text) color to the default color.

Example:
```erlang
1> io_ansi:default_color().
<<"\e[39m">>
```
""".
?SPEC(default_color).
?FUNCTION(default_color).

-doc """
Change background color to black.

Example:
```erlang
1> io_ansi:black_background().
<<"\e[40m">>
```
""".
?SPEC(black_background).
?FUNCTION(black_background).

-doc """
Change background color to red.

Example:
```erlang
1> io_ansi:red_background().
<<"\e[41m">>
```
""".
?SPEC(red_background).
?FUNCTION(red_background).

-doc """
Change background color to green.

Example:
```erlang
1> io_ansi:green_background().
<<"\e[42m">>
```
""".
?SPEC(green_background).
?FUNCTION(green_background).

-doc """
Change background color to yellow.

Example:
```erlang
1> io_ansi:yellow_background().
<<"\e[43m">>
```
""".
?SPEC(yellow_background).
?FUNCTION(yellow_background).

-doc """
Change background color to blue.

Example:
```erlang
1> io_ansi:blue_background().
<<"\e[44m">>
```
""".
?SPEC(blue_background).
?FUNCTION(blue_background).

-doc """
Change background color to magenta.

Example:
```erlang
1> io_ansi:magenta_background().
<<"\e[45m">>
```
""".
?SPEC(magenta_background).
?FUNCTION(magenta_background).

-doc """
Change background color to cyan.

Example:
```erlang
1> io_ansi:cyan_background().
<<"\e[46m">>
```
""".
?SPEC(cyan_background).
?FUNCTION(cyan_background).

-doc """
Change background color to white.

Example:
```erlang
1> io_ansi:white_background().
<<"\e[47m">>
```
""".
?SPEC(white_background).
?FUNCTION(white_background).

-doc """
Change background color to index color. `Index` 0-15 are equivilant to
the named colors in `t:background_color/0` in the order that they are listed.

Example:
```erlang
1> io_ansi:background(2).
<<"\e[42m">>
2> io_ansi:background(80).
<<"\e[48;5;80m">>
```
""".
-spec background(Index :: 0..255 | 0..87) -> unicode:chardata().
?FUNCTION(background, Index).

-doc """
Change background color to RGB color.

Example:
```erlang
1> io_ansi:background(255, 255, 0).
<<"\e[48;2;255;255;0m">>
```
""".
-spec background(0..255, 0..255, 0..255) -> unicode:chardata().
?FUNCTION(background, Red, Green, Blue).

-doc """
Change background color to the default color.

Example:
```erlang
1> io_ansi:default_background().
<<"\e[49m">>
```
""".
?SPEC(default_background).
?FUNCTION(default_background).

-doc """
Change foreground (aka text) color to light black.

Example:
```erlang
1> io_ansi:light_black().
<<"\e[90m">>
```
""".
?SPEC(light_black).
?FUNCTION(light_black).
-doc """
Change foreground (aka text) color to light red.

Example:
```erlang
1> io_ansi:light_red().
<<"\e[91m">>
```
""".
?SPEC(light_red).
?FUNCTION(light_red).
-doc """
Change foreground (aka text) color to light green.

Example:
```erlang
1> io_ansi:light_green().
<<"\e[92m">>
```
""".
?SPEC(light_green).
?FUNCTION(light_green).
-doc """
Change foreground (aka text) color to light yellow.

Example:
```erlang
1> io_ansi:light_yellow().
<<"\e[93m">>
```
""".
?SPEC(light_yellow).
?FUNCTION(light_yellow).
-doc """
Change foreground (aka text) color to light magenta.

Example:
```erlang
1> io_ansi:light_magenta().
<<"\e[95m">>
```
""".
?SPEC(light_magenta).
?FUNCTION(light_magenta).
-doc """
Change foreground (aka text) color to light blue.

Example:
```erlang
1> io_ansi:light_blue().
<<"\e[94m">>
```
""".
?SPEC(light_blue).
?FUNCTION(light_blue).
-doc """
Change foreground (aka text) color to light cyan.

Example:
```erlang
1> io_ansi:light_cyan().
<<"\e[96m">>
```
""".
?SPEC(light_cyan).
?FUNCTION(light_cyan).
-doc """
Change foreground (aka text) color to light white.

Example:
```erlang
1> io_ansi:light_white().
<<"\e[97m">>
```
""".
?SPEC(light_white).
?FUNCTION(light_white).

-doc """
Change background color to light black.

Example:
```erlang
1> io_ansi:light_black_background().
<<"\e[100m">>
```
""".
?SPEC(light_black_background).
?FUNCTION(light_black_background).
-doc """
Change background color to light red.

Example:
```erlang
1> io_ansi:light_red_background().
<<"\e[101m">>
```
""".
?SPEC(light_red_background).
?FUNCTION(light_red_background).
-doc """
Change background color to light green.

Example:
```erlang
1> io_ansi:light_green_background().
<<"\e[102m">>
```
""".
?SPEC(light_green_background).
?FUNCTION(light_green_background).
-doc """
Change background color to light yellow.

Example:
```erlang
1> io_ansi:light_yellow_background().
<<"\e[103m">>
```
""".
?SPEC(light_yellow_background).
?FUNCTION(light_yellow_background).
-doc """
Change background color to light magenta.

Example:
```erlang
1> io_ansi:light_magenta_background().
<<"\e[105m">>
```
""".
?SPEC(light_magenta_background).
?FUNCTION(light_magenta_background).
-doc """
Change background color to light blue.

Example:
```erlang
1> io_ansi:light_blue_background().
<<"\e[104m">>
```
""".
?SPEC(light_blue_background).
?FUNCTION(light_blue_background).
-doc """
Change background color to light cyan.

Example:
```erlang
1> io_ansi:light_cyan_background().
<<"\e[106m">>
```
""".
?SPEC(light_cyan_background).
?FUNCTION(light_cyan_background).

-doc """
Change background color to light white.

Example:
```erlang
1> io_ansi:light_white_background().
<<"\e[107m">>
```
""".
?SPEC(light_white_background).
?FUNCTION(light_white_background).

-doc """
Modify the color referenced by `Index` to be RGB.

Calling this function for `Index` 0-15 will change the color of the named colors
in `t:foreground_color/0` and `t:background_color/0`.

Example:
```erlang
1> io_ansi:modify_color(1, 255, 100, 0).
<<"\e]4;1;rgb:41/19/00\e\\">>
```
""".
-spec modify_color(Index :: 0..255, R :: 0..255, G :: 0..255, B :: 0..255) -> unicode:chardata().
?FUNCTION(modify_color, Index, R, G, B).

-doc """
Turn on bold text style.

Example:
```erlang
1> io_ansi:bold().
<<"\e[1m">>
```
""".
?SPEC(bold).
?FUNCTION(bold).

-doc """
Turn off bold text style.

Example:
```erlang
1> io_ansi:bold_off().
<<"\e[22m">>
```
""".
?SPEC(bold_off).
?FUNCTION(bold_off).

-doc """
Turn on underline text style.

Example:
```erlang
1> io_ansi:underline().
<<"\e[4m">>
```
""".
?SPEC(underline).
?FUNCTION(underline).

-doc """
Turn off underline text style.

Example:
```erlang
1> io_ansi:underline_off().
<<"\e[24m">>
```
""".
?SPEC(underline_off).
?FUNCTION(underline_off).

-doc """
Turn on negative text style.

Example:
```erlang
1> io_ansi:negative().
<<"\e[7m">>
```
""".
?SPEC(negative).
?FUNCTION(negative).

-doc """
Turn off negative text style.

Example:
```erlang
1> io_ansi:negative_off().
<<"\e[27m">>
```
""".
?SPEC(negative_off).
?FUNCTION(negative_off).

-doc """
Clear screen and set cursor to home.

Example:
```erlang
1> io_ansi:clear().
<<"\e[H\e[2J">>
```
""".
?SPEC(clear).
?FUNCTION(clear).
-doc """
Clear screen after cursor.

Example:
```erlang
1> io_ansi:erase_display().
<<"\e[J">>
```
""".
?SPEC(erase_display).
?FUNCTION(erase_display).

-doc """
Insert `Chars` at cursor.

Example:
```erlang
1> io_ansi:insert_character(3).
<<"\e[3@">>
```
""".
?SPEC(insert_character, Chars).
?FUNCTION(insert_character, Chars).

-doc """
Delete 1 character at cursor.

Example:
```erlang
1> io_ansi:delete_character().
<<"\e[P">>
```
""".
?SPEC(delete_character).
?FUNCTION(delete_character).

-doc """
Delete `Chars` characters at cursor by shifting the text `Chars` characters to the left.

Example:
```erlang
1> io_ansi:delete_character(2).
<<"\e[2P">>
```
""".
?SPEC(delete_character, Chars).
?FUNCTION(delete_character, Chars).

-doc """
Erase `Chars` characters at cursor by making `Chars` characters before the cursor blank.

Example:
```erlang
1> io_ansi:erase_character(4).
<<"\e[4X">>
```
""".
?SPEC(erase_character, Chars).
?FUNCTION(erase_character, Chars).

-doc """
Insert 1 line at cursor.

Example:
```erlang
1> io_ansi:insert_line().
<<"\e[L">>
```
""".
?SPEC(insert_line).
?FUNCTION(insert_line).

-doc """
Insert `Lines` lines at cursor.

Example:
```erlang
1> io_ansi:insert_line(2).
<<"\e[2L">>
```
""".
?SPEC(insert_line, Lines).
?FUNCTION(insert_line, Lines).

-doc """
Delete 1 line at cursor.

Example:
```erlang
1> io_ansi:delete_line().
<<"\e[M">>
```
""".
?SPEC(delete_line).
?FUNCTION(delete_line).

-doc """
Delete `Lines` lines at cursor.

Example:
```erlang
1> io_ansi:delete_line(3).
<<"\e[3M">>
```
""".
?SPEC(delete_line, Lines).
?FUNCTION(delete_line, Lines).

-doc """
Erase line at cursor.

Example:
```erlang
1> io_ansi:erase_line().
<<"\e[K">>
```
""".
?SPEC(erase_line).
?FUNCTION(erase_line).

-doc """
Enable the alternate characters set mode

Example:
```erlang
1> io_ansi:alternate_character_set_mode().
<<"\e(0">>
2> io_ansi:fwrite(["%%", alternate_character_set_mode, " tqqu\n"]).
%% ├──┤
ok
```
""".
?SPEC(alternate_character_set_mode).
?FUNCTION(alternate_character_set_mode).

-doc """
Disable the alternate characters set mode

Example:
```erlang
1> io_ansi:alternate_character_set_mode_off().
<<"\e(B">>
```
""".
?SPEC(alternate_character_set_mode_off).
?FUNCTION(alternate_character_set_mode_off).

-doc """
Move the cursor to the given position. Position 0,0 is at the top left of the
terminal.

Example:
```erlang
1> io_ansi:cursor(5, 10).
<<"\e[6;11H">>
```
""".
?SPEC(cursor, Line, Column).
?FUNCTION(cursor, Line, Column).

-doc """
Move the cursor up one line.

Example:
```erlang
1> io_ansi:cursor_up().
<<"\e[A">>
```
""".
?SPEC(cursor_up).
?FUNCTION(cursor_up).

-doc """
Move the cursor up `N` lines.

Example:
```erlang
1> io_ansi:cursor_up(42).
<<"\e[42A">>
```
""".
?SPEC(cursor_up, N).
?FUNCTION(cursor_up, N).

-doc """
Move the cursor down one line.

Example:
```erlang
1> io_ansi:cursor_down().
<<"\n">>
```
""".
?SPEC(cursor_down).
?FUNCTION(cursor_down).

-doc """
Move the cursor down `N` lines.

Example:
```erlang
1> io_ansi:cursor_down(42).
<<"\e[42B">>
```
""".
?SPEC(cursor_down, N).
?FUNCTION(cursor_down, N).

-doc """
Move the cursor forward one character.

Example:
```erlang
1> io_ansi:cursor_forward().
<<"\e[C">>
```
""".
?SPEC(cursor_forward).
?FUNCTION(cursor_forward).
-doc """
Move the cursor forward `N` characters.

Example:
```erlang
1> io_ansi:cursor_forward().
<<"\e[C">>
```
""".
?SPEC(cursor_forward, N).
?FUNCTION(cursor_forward, N).

-doc """
Move the cursor backward `N` characters.

Example:
```erlang
1> io_ansi:cursor_backward().
<<"\b">>
```
""".
?SPEC(cursor_backward).
?FUNCTION(cursor_backward).

-doc """
Move the cursor backward `N` characters.

Example:
```erlang
1> io_ansi:cursor_backward(42).
<<"\e[42D">>
```
""".
?SPEC(cursor_backward, N).
?FUNCTION(cursor_backward, N).

-doc """
Move the cursor to the start of the current line.

Example:
```erlang
1> io_ansi:cursor_home().
<<"\e[H">>
```
""".
?SPEC(cursor_home).
?FUNCTION(cursor_home).

-doc """
Move the cursor up one line, but keeps the cursor on the same location on the
screen by scrolling the screen down.

Example:
```erlang
1> io_ansi:reverse_index().
<<"\eM">>
```
""".
?SPEC(reverse_index).
?FUNCTION(reverse_index).

-doc """
Save the current cursor position.

Example:
```erlang
1> io_ansi:cursor_save().
<<"\e7">>
```
""".
?SPEC(cursor_save).
?FUNCTION(cursor_save).

-doc """
Restore a saved cursor position.

Example:
```erlang
1> io_ansi:cursor_restore().
<<"\e8">>
```
""".
?SPEC(cursor_restore).
?FUNCTION(cursor_restore).

-doc """
Show the cursor.

Example:
```erlang
1> io_ansi:cursor_show().
<<"\e[?12;25h">>
```
""".
?SPEC(cursor_show).
?FUNCTION(cursor_show).

-doc """
Hide the cursor.

Example:
```erlang
1> io_ansi:cursor_hide().
<<"\e[?25l">>
```
""".
?SPEC(cursor_hide).
?FUNCTION(cursor_hide).

-doc """
Move the cursor down one line and then returns it to home.

Example:
```erlang
1> io_ansi:cursor_next_line().
<<"\eE">>
```
""".
?SPEC(cursor_next_line).
?FUNCTION(cursor_next_line).

-doc """
Move the cursor up one line and then returns it to home.

Example:
```erlang
1> io_ansi:cursor_previous_line().
<<"\e[F">>
```
""".
?SPEC(cursor_previous_line).
?FUNCTION(cursor_previous_line).

-doc """
Move the cursor to column `X`.

Example:
```erlang
1> io_ansi:cursor_horizontal_absolute(10).
<<"\e[11G">>
```
""".
?SPEC(cursor_horizontal_absolute, X).
?FUNCTION(cursor_horizontal_absolute, X).

-doc """
Move the cursor to line `X`.

Example:
```erlang
1> io_ansi:cursor_vertical_absolute(20).
<<"\e[21d">>
```
""".
?SPEC(cursor_vertical_absolute, X).
?FUNCTION(cursor_vertical_absolute, X).

-doc """
Move the cursor to line `X` and column `Y`.

Example:
```erlang
1> io_ansi:cursor_horizontal_vertical(10, 20).
<<"\e[10;20f">>
```
""".
?SPEC(cursor_horizontal_vertical, X, Y).
?FUNCTION(cursor_horizontal_vertical, X, Y).

-doc """
Instruct the terminal to report the current cursor position.

Examples:

```erlang
1> io_ansi:cursor_report_position().
~"\e[6n"
```

```bash
## Enter noshell-raw mode and request curson location and then print
## the reply to stdout.
$ erl -noshell -eval 'shell:start_interactive({noshell,raw}),
    io_ansi:fwrite([cursor_report_position]),
    io:format("~p",[io:get_chars("",20)])' -s init stop
"\e[58;1R"
```
""".
?SPEC(cursor_report_position).
?FUNCTION(cursor_report_position).

-doc """
Activate the alternate screen.

Example:
```erlang
1> io_ansi:alternate_screen().
<<"\e[?1049h\e[22;0;0t">>
```
""".
?SPEC(alternate_screen).
?FUNCTION(alternate_screen).

-doc """
Deactivate the alternate screen.

Example:
```erlang
1> io_ansi:alternate_screen_off().
<<"\e[?1049l\e[23;0;0t">>
```
""".
?SPEC(alternate_screen_off).
?FUNCTION(alternate_screen_off).

-doc """
Scroll the screen forward 1 step.

Example:
```erlang
1> io_ansi:scroll_forward().
<<"\e[1S">>
```
""".
?SPEC(scroll_forward).
?FUNCTION(scroll_forward).

-doc """
Scroll the screen forward `N` step.

Example:
```erlang
1> io_ansi:scroll_forward(42).
<<"\e[42S">>
```
""".
?SPEC(scroll_forward, N).
?FUNCTION(scroll_forward, N).

-doc """
Scroll the screen backward 1 step.

Example:
```erlang
1> io_ansi:scroll_backward().
<<"\e[1T">>
```
""".
?SPEC(scroll_backward).
?FUNCTION(scroll_backward).

-doc """
Scroll the screen backward `N` step.

Example:
```erlang
1> io_ansi:scroll_backward(42).
<<"\e[42T">>
```
""".
?SPEC(scroll_backward, Steps).
?FUNCTION(scroll_backward, Steps).

-doc """
Change the scolling region to be from `Line1` to `Line2`.

Example:
```erlang
1> io_ansi:scroll_change_region(10, 20).
<<"\e[11;21r">>
```
""".
?SPEC(scroll_change_region, Line1, Line2).
?FUNCTION(scroll_change_region, Line1, Line2).

-doc """
Move cursor one tab forward.

Example:
```erlang
1> io_ansi:tab().
<<"\t">>
```
""".
?SPEC(tab).
?FUNCTION(tab).

-doc """
Move cursor one tab backward.

Example:
```erlang
1> io_ansi:tab_backward().
<<"\e[Z">>
```
""".
?SPEC(tab_backward).
?FUNCTION(tab_backward).

-doc """
Set a new tab location at the current cursor location.

Example:
```erlang
1> io_ansi:tab_set().
<<"\eH">>
```
""".
?SPEC(tab_set).
?FUNCTION(tab_set).

-doc """
Clear any tab location at the current cursor location.

Example:
```erlang
1> io_ansi:tab_clear().
<<"\e[0g">>
```
""".
?SPEC(tab_clear).
?FUNCTION(tab_clear).
-doc """
Clear all tab locations.

Example:
```erlang
1> io_ansi:tab_clear_all().
<<"\e[3g">>
```
""".
?SPEC(tab_clear_all).
?FUNCTION(tab_clear_all).

-doc """
Enable keypad transmit mode.

Example:
```erlang
1> io_ansi:keypad_transmit_mode().
<<"\e[?1h\e=">>
```
""".
?SPEC(keypad_transmit_mode).
?FUNCTION(keypad_transmit_mode).

-doc """
Disable keypad transmit mode.

Example:
```erlang
1> io_ansi:keypad_transmit_mode_off().
<<"\e[?1l\e>">>
```
""".
?SPEC(keypad_transmit_mode_off).
?FUNCTION(keypad_transmit_mode_off).

-doc """
Reset virtual terminal sequences to their original state.

This only resets the things supported by the loaded terminfo database,
which means that OSCs such as `hyperlink_start/2` are not reset but have
to be reset by emitting `hyperlink_reset/0`.

Example:
```erlang
1> io_ansi:reset().
<<"\e(B\e[m">>
```
""".
?SPEC(reset).
?FUNCTION(reset).

-doc """
Tell the terminal emulator to report its device attributes.

Examples:

```erlang
1> io_ansi:device_report_attributes().
<<"\e[0c">>
```

```sh
## Enter noshell-raw mode and request device attributes and then print
## the reply to stdout.
$ erl -noshell -eval 'shell:start_interactive({noshell,raw}),
     io_ansi:fwrite([device_report_attributes]),
     io:format("~p",[io:get_chars("",20)])' -s init stop
"\e[?65;1;9c"
```
""".
?SPEC(device_report_attributes).
?FUNCTION(device_report_attributes).

%% See https://gcc.gnu.org/cgit/gcc/commit/?id=458c8d6459c4005fc9886b6e25d168a6535ac415 for
%% details on how to check whether we can use terminal URLs.
%% The specification for how to ANSI URLs work is here: https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda

-doc #{ equiv => hyperlink_start(URL, []) }.
-spec hyperlink_start(uri_string:uri_string()) -> unicode:chardata().
?FUNCTION(hyperlink_start, URL).

-doc """
Start a hyperlink pointing to the given `URL` using `Params`.

The hyperlink can be any type of URL, but typically it would be a file or http
URL.

Example:
```erlang
1> io_ansi:hyperlink_start("https://erlang.org").
<<"\e]8;https://erlang.org;\e\\">>
2> io_ansi:format([{hyperlink_start, "file://tmp/debug.log"},"debug log",hyperlink_reset]).
~"\e]8;file://tmp/debug.log;\e\\debug log\e]8;;\e\\\e(B\e[m"
```

See [Hyperlinks (a.k.a. HTML-like anchors) in terminal emulators](https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda)
for more details on limitations and usage of terminal hyperlinks.
""".
-spec hyperlink_start(uri_string:uri_string(),
        [{Key :: unicode:chardata(), Value :: unicode:chardata()}]) -> unicode:chardata().
?FUNCTION(hyperlink_start, URL, Params).
-doc """
Stop emitting a hyperlink.

Example:
```erlang
1> io_ansi:hyperlink_reset().
<<"\e]8;;\e\\">>
```
""".
?SPEC(hyperlink_reset).
?FUNCTION(hyperlink_reset).

-doc """
Check if `t:io:user/0` can interpret ANSI escape sequences.

Example:
```erlang
1> io_ansi:enabled().
true
```
""".
-spec enabled() -> boolean().
enabled() ->
    enabled(user).

-doc """
Check if `Device` can interpret ANSI escape sequences.

This is done by checking if `Device` represents a terminal and if the `TERM`
environment variable is set to a terminal type that supports virtual terminal
sequences.

Example:
```erlang
1> io_ansi:enabled(standard_error).
true
2> {ok, File} = file:open("tmp",[write]), io_ansi:enabled(File).
false
```
""".
-spec enabled(io:device()) -> boolean().
enabled(Device) ->
    IsTerminal =
        case whereis(user_drv) =/= self() of
            true ->
                case io:getopts(Device) of
                    {error, _} -> false;
                    Opts ->
                        proplists:get_value(terminal, Opts, false)
                end;
            false ->
                %% if called from within the user_drv process, we only check if
                %% stdin/stdout are TTYs in order to avoid deadlocks
                prim_tty:isatty(stdin) =:= true andalso prim_tty:isatty(stdout) =:= true
        end,
    IsSmartTerminal =
        case os:type() of
            {win32, _} -> true;
            _ ->
                try
                    prim_tty:tigetstr("sgr0") =/= false
                catch error:badarg ->
                    false
                end
        end,
    IsTerminal andalso IsSmartTerminal.

-doc """
Scan the string for virtial terminal sequences.

The recognized VTSs will be converted into the corresponding `t:vts/0`.

If you intend to parse arrow keys it is recommended that you first set the terminal in
application mode by using `keypad_transmit_mode/0`. This will make it easier for
`m:io_ansi` to correctly detect arrow keys.

Any unrecognized [control sequence introducers](https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands),
will be placed in a tuple tagged with `csi`.

Example:

```erlang
1> io_ansi:scan("\eOA").
[kcursor_up]
2> io_ansi:scan("\eOB").
[kcursor_down]
3> io_ansi:scan(io_ansi:format([bold, "text"])).
[bold, ~"text", reset]
4> io_ansi:scan(io_ansi:format([{cursor, 0, 0}])).
[{csi, ~"\e[1;1H"}, reset]
```

""".
-spec scan(unicode:chardata()) -> [unicode:unicode_binary() | vts() | {csi, unicode:unicode_binary()}].
scan(Data) ->
    scan_binary(unicode:characters_to_binary(Data), <<>>, []).

scan_binary(<<CSI/utf8, R/binary>> = Data, Bin, Acc) when CSI =:= $\e; CSI =:= 155 ->
    case lookup_vts(Data) of
        undefined ->
            case re:run(Data, prim_tty:ansi_regexp(), [unicode]) of
                {match, [{0, N}]} ->
                    <<Ansi:N/binary, AnsiRest/binary>> = Data,
                    scan_binary(AnsiRest, <<>>, [{csi, Ansi}, Bin | Acc]);
                nomatch ->
                        scan_binary(R, <<Bin/binary, CSI/utf8>>, Acc)
            end;
        {Code, _, Rest} ->
            scan_binary(Rest, <<>>, [Code, Bin | Acc])
    end;
scan_binary(<<C/utf8, R/binary>>, Bin, Acc) ->
    scan_binary(R, <<Bin/binary, C/utf8>>, Acc);
scan_binary(<<>>, Bin, Acc) ->
    [NonEmpty || NonEmpty <- lists:reverse([Bin | Acc]), NonEmpty =/= <<>>]. 

lookup_vts(Data) ->
    try
        [case Data of
                <<Value:(byte_size(Value))/binary, Rest/binary>> ->
                    throw({Key, Value, Rest});
                _ ->
                    ok
            end || Key := Values <- get_vts_mappings(),
             Value <- Values],
        undefined
    catch throw:KeyValueRest ->
        KeyValueRest
    end.

-doc #{ equiv => format(Format, []) }.
-spec format(format()) -> unicode:unicode_binary().
format(Format) ->
    format(Format, []).

-doc #{ equiv => format(Format, [], []) }.
-spec format(format(), Data :: [term()]) -> unicode:unicode_binary().
format(Format, Data) ->
    format(Format, Data, []).

-doc """
Returns a character list that represents `Data` formatted in accordance with
`Format`.

This function works just as `io_lib:bformat/2`, except that it also allows
atoms and tuples represeting virtual terminal sequences as part of the
`Format` string.

Calling `format/3` will always emit a `reset/0` VTS at the end of the returned
string. To not emit this, set the `reset` option to `false`.

To force enabling or disabling of emitting VTSs set the `enabled` option to
`true` or `false`.

Example:

```erlang
1> io_ansi:format([blue, underline, "Hello world"]).
~"\e[34m\e[4mHello world\e(B\e[m"
2> io_ansi:format([blue, underline, "Hello ~p"],[world]).
~"\e[34m\e[4mHello world\e(B\e[m"
3> io_ansi:format([blue, underline, "Hello ~p"],[world],[{reset,false}]).
~"\e[34m\e[4mHello world"
4> io_ansi:format([blue, underline, "Hello ~p"],[world],[{enabled,false}]).
~"Hello world"
5> io_ansi:format([invalid_code, "Hello world"]).
** exception error: {invalid_code,invalid_code}
     in function  io_ansi:format_internal/3
```

For a detailed description of the available formatting options, see `io:fwrite/3`.
""".
-spec format(format(), Data :: [term()], options()) -> unicode:unicode_binary().
format(Format, Data, Options) ->
    format_internal(Format, Data, Options).

format_internal(Format, Data, Options) ->
    UseAnsi = case proplists:get_value(enabled, Options) of
                    undefined -> enabled();
                    Enabled -> Enabled
              end,
    %% Only to be used by fwrite
    FormatOnly = proplists:get_value(format_only, Options, false),
    AppendReset = [reset || proplists:get_value(reset, Options, true)],
    Mappings = get_mappings(),
    try lists:foldl(
          fun(Ansi, {Acc, Args}) when is_atom(Ansi) orelse is_tuple(Ansi), FormatOnly ->
                  {[Ansi | Acc], Args};
             (Ansi, {Acc, Args}) when is_atom(Ansi), UseAnsi ->
                AnsiFun = lookup(Mappings, Ansi, []),
                  {[AnsiFun() | Acc], Args};
             (Ansi, {Acc, Args}) when is_tuple(Ansi), UseAnsi ->
                [AnsiCode | AnsiArgs] = tuple_to_list(Ansi),
                AnsiFun = lookup(Mappings, AnsiCode, AnsiArgs),
                  {[apply(AnsiFun, AnsiArgs) | Acc], Args};
             (Ansi, {Acc, Args}) when is_atom(Ansi); is_tuple(Ansi) ->
                  {Acc, Args};
             (Fmt, {Acc, Args}) ->
                  {Scanned, Rest} = io_lib_format:scan(Fmt, Args),
                  {[io_lib_format:build_bin(Scanned) | Acc], Rest}
          end, {[], Data}, group([Format,AppendReset])) of
        {Scanned, []} ->
            if FormatOnly ->
                    lists:flatten(lists:reverse(Scanned));
               not FormatOnly ->
                    unicode:characters_to_binary(lists:reverse(Scanned))
            end;
        _ ->
            erlang:error(badarg, [Format, Data, Options])
    catch throw:{invalid_code, Code, []} ->
            erlang:error({invalid_code, Code});
        throw:{invalid_code, Code, Args} ->
            erlang:error({invalid_code, {Code, Args}});
        E:R:ST ->
            erlang:raise(E,R,ST)
            %%            erlang:error(badarg, [Format, Data, Options])
    end.

-doc #{ equiv => fwrite(standard_io, Format, [], []) }.
-spec fwrite(Format :: format()) -> ok.
fwrite(Format) ->
    fwrite(standard_io, Format, [], []).

-doc #{ equiv => fwrite(standard_io, Format, Data, []) }.
-spec fwrite(Format :: format(), [term()]) -> ok.
fwrite(Format, Data) ->
    fwrite(standard_io, Format, Data, []).

-doc #{ equiv => fwrite(standard_io, Format, Data, Options) }.
-spec fwrite(Format :: format(), [term()], options()) -> ok.
fwrite(Format, Data, Options) ->
    fwrite(standard_io, Format, Data, Options).

-doc """
Writes the items in `Data` on the [`IoDevice`](`t:io:device/0`) in accordance with `Format`.

This function works just as `io:fwrite/2`, except that it also allows atoms and
tuples representing virtual terminal sequences (VTS) as part of the `Format` string.

See `format/3` for details on how the different `Options` can be used.

Example:

```erlang
1> io_ansi:fwrite([blue, "%% Hello world\n"]).
%% Hello world
ok
2> io_ansi:fwrite([underline, "%% Hello ~p\n"], [world]).
%% Hello world
ok
3> io_ansi:fwrite([invalid_code, "%% Hello ~p\n"], [world]).
** exception error: {error,{put_ansi,unicode,invalid_code}}
     in function  io_ansi:fwrite/4
        called as io_ansi:fwrite(standard_io,[invalid_code,"%% Hello ~p\n"],[world],[])
```

The decision what each VTS should be converted to is done by the destination I/O
device. This means that if the I/O device is on a remote node, the terminfo
database loaded into that remote node will be used.

All VTSs are stripped if the target I/O device does not support handling VTSs,
either because it is not implemented by the device (for example if the device
is a `t:file:io_server/0`) or if the device does not support a certain VTS.
If you want to force usage of VTSs you can pass `{enabled, true}` and that will
use the local defintions to translate.
""".
-spec fwrite(IODevice :: io:device(), Format :: format(), [term()], options()) -> ok.
fwrite(Device, Format, Data, Options) ->
    Ref = make_ref(),
    try
        lists:foldl(
          fun F(Chars, ok) when is_binary(Chars) ->
                  io:request(Device, {put_chars, unicode, Chars});
              F(Ansi, ok) when is_atom(Ansi); is_tuple(Ansi) ->
                  case io:request(Device,{put_ansi, Options, Ansi}) of
                      {error, request} ->
                          %% The IO server did not support printing ansi.
                          case proplists:get_value(enabled, Options, Ref) of
                              true ->
                                  %% If ansi is forced by the ansi option,
                                  %% we format using the local ansi definition
                                  %% and send as characters
                                  F(format([Ansi], [], Options), ok);
                              Ref ->
                                  %% We drop the ansi codes
                                  ok
                          end;
                      Else -> Else
                  end;
              F(_Data, Error) ->
                  throw({Ref, Error})
          end, ok, format_internal(Format, Data, [{format_only, true} | Options]))
    catch {Ref, Error} ->
            erlang:error(Error, [Device, Format, Data, Options])
    end.

%% This function takes a t:format and returns a t:format where all characters
%% are flattened and grouped. That is: group(["aa","bb",red,["cc"], "dd"]) -> group(["aabb",red,"ccdd"])
group(Fmt) ->
    group(lists:flatten(Fmt), []).
group([Ansi | T], []) when is_atom(Ansi); is_tuple(Ansi) ->
    [Ansi | group(T, [])];
group([Ansi | T], Acc) when is_atom(Ansi); is_tuple(Ansi) ->
    [lists:reverse(Acc), Ansi | group(T, [])];
group([C | T], Acc) ->
    group(T, [C | Acc]);
group([], []) ->
    [];
group([], Acc) ->
    [lists:reverse(Acc)].

sgr(Args) ->
    ["\e[",lists:join($;,Args),"m"].

%% Lookup a fun for a vts
lookup(Key, Args) ->
    lookup(get_mappings(), Key, Args).
lookup(Mappings, Key, Args) ->
    case maps:get(length(Args), maps:get(Key, Mappings, #{}), undefined) of
        #{ terminfo := TermInfoFun } -> TermInfoFun;
        #{ ansi := AnsiFun } -> AnsiFun;
        undefined -> throw({invalid_code, Key, Args})
    end.

get_mappings() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            persistent_term:put(?MODULE, init_mappings()),
            get_mappings();
        Value -> Value
    end.

%% Take the description in default_mappings and munge it so that
%% we can look them up quickly.
-spec init_mappings() -> #{ vts() => #{ arity() => function() } }.
init_mappings() ->
    maps:map(
      fun
          Map(Key, Mapping) when not is_list(Mapping) ->
                    Map(Key, [Mapping]);
          Map(_Key, Mappings) when is_list(Mappings) ->
                    maps:from_list(
                      lists:map(
                        fun
                            Fun({TermInfoCap, AnsiString}) when is_list(AnsiString) ->
                                Fun({TermInfoCap, unicode:characters_to_binary(AnsiString)});
                            Fun({undefined, AnsiFun}) when is_function(AnsiFun) ->
                                       {arity, Arity} = erlang:fun_info(AnsiFun, arity),
                                       {Arity, #{ ansi => AnsiFun }};
                            Fun({undefined, AnsiString}) when not is_function(AnsiString) ->
                                       Fun({undefined, fun() -> AnsiString end});
                            Fun({TermInfoCap, AnsiFun}) when is_function(AnsiFun) ->
                                       try prim_tty:tigetstr(TermInfoCap) of
                                           {ok, TermInfoStr} ->
                                               {arity, Arity} = erlang:fun_info(AnsiFun, arity),
                                               TermInfoFun =
                                                   case Arity of
                                                       0 -> fun() -> {ok, S} = prim_tty:tputs(TermInfoStr, []), S end;
                                                       1 -> fun(A) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A]), S end;
                                                       2 -> fun(A, B) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B]), S end;
                                                       3 -> fun(A, B, C) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B, C]), S end;
                                                       4 -> fun(A, B, C, D) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B, C, D]), S end;
                                                       5 -> fun(A, B, C, D, E) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B , C, D, E]), S end;
                                                       6 -> fun(A, B, C, D, E, F) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F]), S end;
                                                       7 -> fun(A, B, C, D, E, F, G) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F, G]), S end;
                                                       8 -> fun(A, B, C, D, E, F, G, H) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F, G, H]), S end;
                                                       9 -> fun(A, B, C, D, E, F, G, H, I) -> {ok, S} = prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F, G, H, I]), S end
                                                   end,
                                               { Arity, #{ terminfo => TermInfoFun, ansi => AnsiFun }};
                                           false -> Fun({undefined, AnsiFun})
                                       catch error:badarg ->
                                               Fun({undefined, AnsiFun})
                                       end;
                            Fun({{TermInfoCap, Args}, AnsiString}) when not is_function(AnsiString) ->
                                       try prim_tty:tigetstr(TermInfoCap) of
                                           {ok, S} ->
                                               {ok, TermInfoStr} = prim_tty:tputs(S, Args),
                                               {0, #{ terminfo => fun() -> TermInfoStr end,
                                                      ansi => fun() -> AnsiString end } };
                                           false ->
                                               Fun({undefined, AnsiString})
                                       catch error:badarg ->
                                               Fun({undefined, AnsiString})
                                       end;
                            Fun({TermInfoCap, AnsiString})  when not is_function(AnsiString) ->
                                       Fun({{TermInfoCap, []}, AnsiString})
                               end, Mappings))
            end, default_mappings()).

get_vts_mappings() ->
    case persistent_term:get(io_ansi_vts_mappings, undefined) of
        undefined ->
            persistent_term:put(io_ansi_vts_mappings, init_vts_mappings()),
            get_vts_mappings();
        Value -> Value
    end.

%% Take the description in default_mappings and so that we can do a reverse lookup quickly
-spec init_vts_mappings() -> #{ vts() => [unicode:unicode_binary()] }.
init_vts_mappings() ->
    maps:map(fun Map(Key, Value) when not is_list(Value) ->
                     Map(Key, [Value]);
                 Map(_Key, Values) when is_list(Values) ->
                     lists:flatmap(fun F({_TermInfoCap, Fun}) when is_function(Fun) ->
                                           [];
                                       F({undefined, Ansi}) ->
                                           [unicode:characters_to_binary(Ansi)];
                                       F({{TermInfoCap, Args}, Ansi}) ->
                                           try prim_tty:tigetstr(TermInfoCap) of
                                               {ok, S} ->
                                                   {ok, TermInfoStr} = prim_tty:tputs(S, Args),
                                                   [unicode:characters_to_binary(TermInfoStr),
                                                    unicode:characters_to_binary(Ansi)];
                                               undefined ->
                                                   F({undefined, Ansi})
                                           catch error:badarg ->
                                                   F({undefined, Ansi})
                                           end;
                                       F({TermInfoCap, Ansi}) ->
                                           F({{TermInfoCap, []}, Ansi})
                                   end, Values)
             end, default_mappings()).

default_mappings() ->
    #{ cursor =>
           [{"cup", fun(Line, Column) -> concat(["\e[",Line+1,";",Column+1,"H"]) end}],
       cursor_home => {"home", "\eH"},
       cursor_up =>
           [{"cuu1", "\e[A"},
            {"cuu", fun(Steps) -> concat(["\e[", Steps ,"A"]) end}],

       cursor_down =>
           [{"cud1", "\n"},
            {"cud", fun(Steps) -> concat(["\e[", Steps ,"B"]) end}],

       cursor_backward =>
           [{"cub1", "\b"},
            {"cub", fun(Steps) -> concat(["\e[", Steps ,"D"]) end}],

       cursor_forward =>
           [{"cuf1", "\e[C"},
            {"cuf", fun(Steps) -> concat(["\e[", Steps ,"C"]) end}],

       reverse_index => { "ri", "\eM" },
       cursor_save => { "sc", "\e7" },
       cursor_restore => { "rc", "\e8" },
       cursor_show => { "cvvis", "\e[?25h"},
       cursor_hide => { "civis", "\e[?25l"},

       cursor_next_line => { "nel", "\e[E" },
       cursor_previous_line => { undefined, "\e[F" },
       cursor_horizontal_absolute => { "hpa", fun(X) -> concat(["\e[", X, "G"]) end},
       cursor_vertical_absolute => { "vpa", fun(Y) -> ["\e[", Y, "d"] end},
       cursor_horizontal_vertical => { undefined, fun(X, Y) -> concat(["\e[", X, ";", Y, "f"]) end},

       alternate_screen => { "smcup", "\e[?1049h" },
       alternate_screen_off => { "rmcup", "\e[?1049l" },

       scroll_forward => [{{"indn", [1]}, "\eS"},
                          { "indn", fun(Steps) -> concat(["\e", Steps, "S"]) end}],
       scroll_backward => [{{"rin", [1]}, "\eT"},
                           { "rin", fun(Steps) -> concat(["\e", Steps, "T"]) end}],
       scroll_change_region => {"csr", fun(Line1, Line2) -> concat(["\e[",Line1,";",Line2,"r"]) end},

       insert_character => { "ich", fun(Chars) -> concat(["\e[", Chars, "@"]) end},
       delete_character => [{"dch1", "\e[P"},
                            { "dch", fun(Chars) -> concat(["\e[", Chars, "P"]) end}],
       erase_character => { "ech", fun(Chars) -> concat(["\e[", Chars, "X"]) end},
       insert_line => [{"il1", "\e[L"},
                       { "il", fun(Chars) -> concat(["\e[", Chars, "L"]) end}],
       delete_line => [{"dl1", "\e[M"},
                       { "dl", fun(Chars) -> concat(["\e[", Chars, "M"]) end}],

       erase_display => { "ed", "\e[J"},
       erase_line => {"el", "\e[K"},
       clear => { "clear", "\e[H\e[2J"},

       modify_color => { "initc", fun(Index, R, G, B) ->
                                          io_lib:format("\e]4;~.16b;rgb:~.16b/~.16b/~.16b\e\\",[Index, R, G, B])
                                  end},

       keypad_transmit_mode => { "smkx", "\e=" },
       keypad_transmit_mode_off => { "rmkx", "\e>" },

       kcursor_home => {"khome", "\eH"},
       kcursor_end => {"kend", "\eF"},
       kcursor_up => {"kcuu1", "\e[A"},
       kcursor_down => {"kcud1", "\e[B"},
       kcursor_backward => {"kcub1", "\e[D"},
       kcursor_forward => {"kcuf1", "\e[C"},

       cursor_report_position => { "u7", "\e[6n" },
       device_report_attributes => { undefined, "\e[0c"},

       tab_set => { "hts", "\eH" },
       tab => { "ht", "\e[I" },
       tab_backward => { "cbt", "\eI" },
       tab_clear => { undefined, "\e[0g" },
       tab_clear_all => { "tbc", "\e[3g" },

       reset => { "sgr0", sgr(["0"])},

       black => { {"setaf", [0]}, sgr(["30"]) },
       red => { {"setaf", [1]}, sgr(["31"]) },
       green => { {"setaf", [2]}, sgr(["32"]) },
       yellow => { {"setaf", [3]}, sgr(["33"]) },
       blue => { {"setaf", [4]}, sgr(["34"]) },
       magenta => { {"setaf", [5]}, sgr(["35"]) },
       cyan => { {"setaf", [6]}, sgr(["36"]) },
       white => { {"setaf", [7]}, sgr(["37"]) },
       default_color => { undefined, sgr(["39"]) },
       color => [{"setaf", fun(Index) -> sgr(["38","5",s(Index)]) end},
                 {undefined, fun(R, G, B) -> sgr(["38","2",s(R),s(G),s(B)]) end}],

       black_background => { {"setab", [0]}, sgr(["40"]) },
       red_background => { {"setab", [1]}, sgr(["41"]) },
       green_background => { {"setab", [2]}, sgr(["42"]) },
       yellow_background => { {"setab", [3]}, sgr(["43"]) },
       blue_background => { {"setab", [4]}, sgr(["44"]) },
       magenta_background => { {"setab", [5]}, sgr(["45"]) },
       cyan_background => { {"setab", [6]}, sgr(["46"]) },
       white_background => { {"setab", [7]}, sgr(["47"]) },
       default_background => { undefined, sgr(["49"]) },
       background => [{"setab", fun(Index) -> sgr(["48","5",s(Index)]) end},
                      {undefined, fun(R, G, B) -> sgr(["48","2",s(R),s(G),s(B)]) end}],

       light_black => { {"setaf", [8]}, sgr(["90"]) },
       light_red => { {"setaf", [9]}, sgr(["91"]) },
       light_green => { {"setaf", [10]}, sgr(["92"]) },
       light_yellow => { {"setaf", [11]}, sgr(["93"]) },
       light_blue => { {"setaf", [12]}, sgr(["94"]) },
       light_magenta => { {"setaf", [13]}, sgr(["95"]) },
       light_cyan => { {"setaf", [14]}, sgr(["96"]) },
       light_white => { {"setaf", [15]}, sgr(["97"]) },

       light_black_background => { {"setab", [8]}, sgr(["100"]) },
       light_red_background => { {"setab", [9]}, sgr(["101"]) },
       light_green_background => { {"setab", [10]}, sgr(["102"]) },
       light_yellow_background => { {"setab", [11]}, sgr(["103"]) },
       light_blue_background => { {"setab", [12]}, sgr(["104"]) },
       light_magenta_background => { {"setab", [13]}, sgr(["105"]) },
       light_cyan_background => { {"setab", [14]}, sgr(["106"]) },
       light_white_background => { {"setab", [15]}, sgr(["107"]) },

       bold => { "bold", "\e[1m" },
       bold_off => { undefined, "\e[22m" },
       underline => { "smul", "\e[4m" },
       underline_off => { "rmul", "\e[24m" },
       negative => { "smso", "\e[7m"},
       negative_off => { "rmso", "\e[27m"},

       alternate_character_set_mode => {"smacs", "\e(0" },
       alternate_character_set_mode_off => {"rmacs", "\e(B" },

       hyperlink_start =>
            [{undefined, fun(Url) -> hyperlink(Url, []) end},
             {undefined, fun(Url, Params) -> hyperlink(Url, Params) end}],

       hyperlink_reset =>
           [{undefined, fun() -> hyperlink("", []) end}]
     }.

hyperlink(URL, Params) ->
    StringParams = lists:join($:, [[K, "=", V] || {K, V} <- Params]),
    io_lib:format("\e]8;~s;~s\e\\",[URL, StringParams]).

s(Int) when is_integer(Int) ->
    integer_to_list(Int).