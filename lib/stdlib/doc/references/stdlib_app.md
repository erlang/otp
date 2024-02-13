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
# STDLIB

The STDLIB application.

## Description

The STDLIB application is mandatory in the sense that the minimal system based
on Erlang/OTP consists of Kernel and STDLIB. The STDLIB application contains no
services.

## Configuration

The following configuration parameters are defined for the STDLIB application.
For more information about configuration parameters, see the
[`app(4)`](`e:kernel:app.md`) module in Kernel.

- **`shell_esc = icl | abort`{: #shell_esc }** - Can be used to change the
  behavior of the Erlang shell when _^G_ is pressed.

- **`restricted_shell = module()`{: #restricted_shell }** - Can be used to run
  the Erlang shell in restricted mode.

- **`shell_catch_exception = boolean()`{: #shell_catch_exception }** - Can be
  used to set the exception handling of the evaluator process of Erlang shell.

- **`shell_expand_location = above | below`{: #shell_expand_location }** - Sets
  where the tab expansion text should appear in the shell. The default is
  `below`. This will open a pager below the cursor that is scrollable one line
  at a time with `Up/Down` arrow keys or 5 lines at a time with `PgUp/PgDn`.

- **`shell_history_length = integer() >= 0`{: #shell_history_length }** - Can be
  used to determine how many commands are saved by the Erlang shell. See
  `m:edlin` for more.

- **`shell_keymap = #{}`{: #shell_keymap }** - Can be used to override the
  default keymap configuration for the shell.

- **`format_shell_func = {Mod, Func} | string() | default`{: #format_shell_func
  }** - Can be used to set the formatting of the Erlang shell output. This has
  an effect on commands that have been submitted and how it is saved in history
  or if the formatting hotkey is pressed while editing an expression (Alt-f by
  default). You can specify a Mod:Func/1 that expects the whole expression as a
  string and returns a formatted expressions as a string. See
  `shell:format_shell_func/1` for how to set it from inside the shell.

  If instead a string is provided, it will be used as a shell command. Your
  command must include `${file}` somewhere in the string, for the shell to know
  where the file goes in the command.

  ```text
  -stdlib format_shell_func "\"emacs -batch \${file} -l ~/erlang-format/emacs-format-file -f emacs-format-function\""
  ```

  ```text
  -stdlib format_shell_func "{shell, erl_pp_format_func}"
  ```

- **`shell_prompt_func = {Mod, Func} | default`{: #shell_prompt_func }** - where

  - `Mod = atom()`
  - `Func = atom()`

  Can be used to set a customized Erlang shell prompt function.

- **`shell_multiline_prompt = {Mod, Func} | string() | default`{:
  #shell_multiline_prompt }** - where

  - `Mod = atom()`
  - `Func = atom()`

  Can be used to set a customized multiline shell prompt function. The multiline
  prompt function takes the main prompt as its only parameter.

- **`shell_saved_results = integer() >= 0`{: #shell_saved_results }** - Can be
  used to determine how many results are saved by the Erlang shell.

- **`shell_session_slogan = string() | fun() -> string())`{:
  #shell_session_slogan }** - The slogan printed when starting an Erlang shell.
  Example:

  ```erlang
  $ erl -stdlib shell_session_slogan '"Test slogan"'
  Erlang/OTP 26 [DEVELOPMENT] [erts-13.0.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

  Test slogan
  1>
  ```

- **`shell_slogan = string() | fun(() -> string())`{: #shell_slogan }** - The
  slogan printed when starting the Erlang shell subsystem. Example:

  ```erlang
  $ erl -stdlib shell_slogan '"Test slogan"'
  Test slogan
  Eshell V13.0.2  (abort with ^G)
  1>
  ```

  The default is the return value of
  [`erlang:system_info(system_version)`](`m:erlang#system_info_system_version`).

- **`shell_strings = boolean()`{: #shell_strings }** - Can be used to determine
  how the Erlang shell outputs lists of integers.

## See Also

[`app(4)`](`e:kernel:app.md`), `m:application`, `m:shell`
