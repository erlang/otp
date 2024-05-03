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
# tty - A Command-Line Interface

`tty` is a simple command-line interface program where keystrokes are collected
and interpreted. Completed lines are sent to the shell for interpretation. A
simple history mechanism saves previous lines, which can be edited before
sending them to the shell. `tty` is started when Erlang is started with the
following command:

```text
erl
```

`tty` operates in several different modes:

- Normal mode, in which text lines can be edited and sent to the shell.
- Search mode, in which the user can search for previous commands in the history
  buffer.
- Shell break mode, which allows the user to kill the current shell, start
  multiple shells, and so on.

## Normal Mode

In normal mode keystrokes from the user are collected and interpreted by `tty`.
Most of the _Emacs_ line-editing commands are supported. The following is a
complete list of the supported line-editing commands.

Typographic conventions:

- `C-a` means pressing the _Ctrl_ key and the letter `a` simultaneously.
- `C-S-a` means pressing the _Ctrl_ key, the _Shift_ key, and the letter `a`
  simultaneously.
- `M-f` means pressing the _Esc_ key and the letter `f` in sequence or pressing
  the _Alt_ key and the letter `f` simultaneously.
- `Home` and `End` represent the keys with the same name on the keyboard.
- `Left`, `Right`, `Up` and `Down` represent the corresponding arrow keys.
- When a function has multiple possible key sequences they are listed on
  individual lines in the `Key Sequence` column.

| _Key Sequence_       | _Function_                                                      |
| -------------------- | ----------------------------------------------------------------|
| `Home`               | Beginning of line                                               |
| `C-a`                | Beginning of line                                               |
| `C-b`                | Backward character                                              |
| `C-Left` or `M-Left` | Backward word                                                   |
| `M-b`                | Backward word                                                   |
| `C-d`                | Delete character                                                |
| `M-d`                | Delete word                                                     |
| `End`                | End of line                                                     |
| `C-e`                | End of line                                                     |
| `C-f`                | Forward character                                               |
| `C-Right` or `M-Right` | Forward word                                                  |
| `M-f`                | Forward word                                                    |
| `C-g`                | Enter shell break mode                                          |
| `C-k`                | Kill line                                                       |
| `C-u`                | Backward kill line                                              |
| `C-l`                | Clears the screen                                               |
| `M-c`                | Clears the current expression                                   |
| `M-l`                | Redraw line                                                     |
| `C-n`                | Fetch next line from the history buffer                         |
| `C-o` or `M-o`       | Edit the current line using the editor specified in the environment variable `VISUAL` or `EDITOR`. The environment variables can contain arguments to the editor if needed, for example `VISUAL="emacs -nw"`. On Windows the editor cannot be a console based editor. |
| `C-p`                | Fetch previous line from the history buffer                     |
| `M-r`                | Format current expression using `shell:format_shell_func/1`     |
| `C-r`                | Enter search mode and then search backward in the shell history |
| `C-s`                | In search mode, search forward in the shell history             |
| `C-t`                | Transpose characters                                            |
| `C-w`                | Backward kill word                                              |
| `C-y`                | Insert previously killed text                                   |
| `C-]`                | Insert matching closing bracket                                 |
| `C-Up` or `M-Up`     | Navigate one row up when editing multiple lines                 |
| `C-Down` or `M-Down` | Navigate one row down when editing multiple lines               |
| `M-Enter`            | Insert a new line at cursor                                     |
| `M-<` or `M-S-Up`    | Navigate to the start of the current expression                 |
| `M->` or `M-S-Down`  | Navigate to the end of the current expression                   |
| `Tab` or `C-i`       | Autocomplete current expression, or show completion suggestions |
| `M-c`                | Clear current expression                                        |
| `M-h`                | Display help for the module or function closest on the left of the cursor.|
| `PageUp`             | Scroll the expand, search or help buffer 5 lines upwards.       |
| `PageDown`           | Scroll the expand, search or help buffer 5 lines downwards.     |

_Table: tty Text Editing_

## Shell Break Mode

In this mode the following can be done:

- Kill or suspend the current shell
- Connect to a suspended shell
- Start a new shell
