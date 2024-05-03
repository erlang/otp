<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

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
# werl

The Erlang Emulator on Windows

## Description

> #### Note {: .info }
>
> Since Erlang/OTP 26 the werl.exe command is now only a link to
> [erl.exe](erl_cmd.md) as the normal Erlang shell has been updated to work on
> both Unix and Windows in the same way.
>
> The text below is kept for systems that have yet to upgrade to Erlang/OTP 26

On Windows, the preferred way to start the Erlang system for interactive use is
as follows:

`werl <arguments>`

This starts Erlang in its own window, with fully functioning command-line
editing and scrollbars. All flags except `-oldshell` work as they do for
[`erl(1)`](erl_cmd.md).

- To copy text to the clipboard, use `Ctrl-C`.
- To paste text, use `Ctrl-V`.
- To interrupt the runtime system or the shell process (depending on what has
  been specified with system flag `+B`), use `Ctrl-Break`.

In cases where you want to redirect standard input and/or standard output or use
Erlang in a pipeline, `werl` is not suitable, and the `erl` program is to be
used instead.

The `werl` window is in many ways modeled after the `xterm` window present on
other platforms, as the `xterm` model fits well with line-oriented command-based
interaction. This means that selecting text is line-oriented rather than
rectangle-oriented.

- To select text in the `werl` window, press and hold the left mouse button and
  drag the mouse over the text you want to select. If the selection crosses line
  boundaries, the selected text consists of complete lines where applicable
  (just like in a word processor).
- To select more text than fits in the window, start by selecting a small part
  in the beginning of the text you want, then use the scrollbar to view the end
  of the desired selection, point to it, and press the _right_ mouse button. The
  whole area between your first selection and the point where you right-clicked
  is included in the selection.
- To copy the selected text to the clipboard, either use `Ctrl-C`, use the menu,
  or press the copy button in the toolbar.

Pasted text is inserted at the current prompt position and is interpreted by
Erlang as usual keyboard input.

- To retrieve previous command lines, press the `Up arrow` or use `Ctrl-P`.

A drop-down box in the toolbar contains the command history. Selecting a command
in the drop-down box inserts the command at the prompt, as if you used the
keyboard to retrieve the command.

- To stop the Erlang emulator, close the `werl` window.
