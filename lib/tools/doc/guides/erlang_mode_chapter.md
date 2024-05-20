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
# The Erlang mode for Emacs

## Purpose

The purpose of this user guide is to introduce you to the Erlang mode
for Emacs and gives some relevant background information of the
functions and features.  See also [Erlang mode reference
manual](../references/erlang.el.md) The purpose of the Erlang mode
itself is to facilitate the developing process for the Erlang
programmer.

## Pre-requisites

Basic knowledge of Emacs and Erlang/OTP.

## Elisp

Two Elisp modules are included in this tool package for
Emacs. `erlang.el` defines the actual Erlang mode and
`erlang-start.el` makes some nice initializations.

## Setup on UNIX

To set up the Erlang Emacs mode on a UNIX systems, edit or create the file `.emacs`
in the your home directory.

Below is a complete example of what should be added to a user's `.emacs`
provided that OTP is installed in the directory `/usr/local/otp `:

```text
(setq load-path (cons  "/usr/local/otp/lib/tools-<ToolsVer>/emacs"
load-path))
(setq erlang-root-dir "/usr/local/otp")
(setq exec-path (cons "/usr/local/otp/bin" exec-path))
(require 'erlang-start)
```

## Setup on Windows

To set up the Erlang Emacs mode on a Windows systems, edit/create the file
`.emacs`, the location of the file depends on the configuration of the system.
If the _HOME_ environment variable is set, Emacs will look for the `.emacs` file
in the directory indicated by the `HOME` variable. If `HOME` is not set, Emacs
will look for the `.emacs` file in `C:\ `.

Below is a complete example of what should be added to a user's `.emacs`
provided that OTP is installed in the directory `C:\Program Files\Erlang OTP`:

```lisp
(setq load-path (cons  "C:/Program Files/Erlang OTP/lib/tools-<ToolsVer>/emacs"
load-path))
(setq erlang-root-dir "C:/Program Files/Erlang OTP")
(setq exec-path (cons "C:/Program Files/Erlang OTP/bin" exec-path))
(require 'erlang-start)
```

> #### Note {: .info }
>
> In `.emacs`, the slash character (`/`) can be used as path separator. But if you
> decide to use the backslash character (`\`), note that backslashes have to be
> doubled, since they are treated as escape characters by Emacs.

## Indentation

The "Oxford Advanced Learners Dictionary of Current English" says the following
about the word "indent":

> "start (a line of print or writing) farther from the margin than the others".

The Erlang mode does, of course, provide this feature. The layout used is based
on the common use of the language.

It is strongly recommended to use this feature and avoid to indent lines in a
nonstandard way. Some motivations are:

- Code using the same layout is easy to read and maintain.
- Since several features of Erlang mode is based on the standard layout they
  might not work correctly if a nonstandard layout is used.

The indentation features can be used to reindent large sections of a file. If
some lines use nonstandard indentation they will be reindented.

## Editing

- _`M-x erlang-mode RET`_ \- This command activates the Erlang major mode for
  the current buffer. When this mode is active the mode line contain the word
  "Erlang".

When the Erlang mode is correctly installed, it is automatically activated when
a file ending in `.erl` or `.hrl` is opened in Emacs.

When a file is saved the name in the `-module().` line is checked against the
file name. Should they mismatch Emacs can change the module specifier so that it
matches the file name. By default, the user is asked before the change is
performed.

An "electric" command is a character that in addition to just inserting the
character performs some type of action. For example the `;` character is typed
in a situation where is ends a function clause a new function header is
generated. The electric commands are as follows:

- `erlang-electric-comma` \- Insert a comma character and possibly a new
  indented line.
- `erlang-electric-semicolon` \- Insert a semicolon character and possibly a
  prototype for the next line.
- `erlang-electric-gt` \- Insert a `>` character and possible a new indented line.

To disable all electric commands set the variable `erlang-electric-commands` to
the empty list. In short, place the following line in your `.emacs`\-file:

```text
(setq erlang-electric-commands '())
```

## Syntax highlighting

It is possible for Emacs to use colors when displaying a buffer. By "syntax
highlighting", we mean that syntactic components, for example keywords and
function names, will be colored.

The basic idea of syntax highlighting is to make the structure of a program
clearer. For example, the highlighting will make it easier to spot simple bugs.
Have not you ever written a variable in lower-case only? With syntax
highlighting a variable will colored while atoms will be shown with the normal
text color.

## Tags

Tags is a standard Emacs package used to record information about source files
in large development projects. In addition to listing the files of a project, a
tags file normally contains information about all functions and variables that
are defined. By far, the most useful command of the tags system is its ability
to find the definition of functions in any file in the project. But the Tags
system is not limited to this feature, for example, it is possible to do a text
search in all files in a project, or to perform a project-wide search and
replace.

In order to use the Tags system a file named `TAGS` must be created. The file
can be seen as a database over all functions, records, and macros in all files
in the project. The `TAGS` file can be created using two different methods for
Erlang. The first is the standard Emacs utility "etags", the second is by using
the Erlang module `tags`.

## Etags

`etags` is a program that is part of the Emacs distribution. It is normally
executed from a command line, like a Unix shell or a DOS box.

The `etags` program of fairly modern versions of Emacs and XEmacs has native
support for Erlang. To check if your version does include this support, issue
the command `etags --help` at a the command line prompt. At the end of the help
text there is a list of supported languages. Unless Erlang is a member of this
list I suggest that you should upgrade to a newer version of Emacs.

As seen in the help text — unless you have not upgraded your Emacs yet — `etags`
associate the file extensions `.erl` and `.hrl` with Erlang.

Basically, the `etags` utility is run using the following form:

```bash
etags file1.erl file2.erl
```

This will create a file named `TAGS` in the current directory.

The `etags` utility can also read a list of files from its standard input by
supplying a single dash in place of the file names. This feature is useful when
a project consists of a large number of files. The standard UNIX command `find`
can be used to generate the list of files, for example:

```bash
find . -name "*.[he]rl" -print | etags -
```

The above line will create a `TAGS` file covering all the Erlang source files in
the current directory, and in the subdirectories below.

See the GNU Emacs Manual and the etags man page for more info.

## Shell

The look and feel on an Erlang shell inside Emacs should be the same as in a
normal Erlang shell. There is just one major difference, the cursor keys will
actually move the cursor around just like in any normal Emacs buffer. The
command line history can be accessed by the following commands:

- *`C-up `*or _`M-p `_(`comint-previous-input`) - Move to the previous line in
  the input history.
- *`C-down `*or _`M-n `_(`comint-next-input`) - Move to the next line in the
  input history.

If the Erlang shell buffer would be killed the command line history is saved to
a file. The command line history is automatically retrieved when a new Erlang
shell is started.

## Compilation

The classic edit-compile-bugfix cycle for Erlang is to edit the source file in
an editor, save it to a file and switch to an Erlang shell. In the shell the
compilation command is given. Should the compilation fail you have to bring out
the editor and locate the correct line.

With the Erlang editing mode the entire edit-compile-bugfix cycle can be
performed without leaving Emacs. Emacs can order Erlang to compile a file and it
can parse the error messages to automatically place the point on the erroneous
lines.
