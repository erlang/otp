# Erlang mode for Emacs

Possibly the most important feature of an editor designed for programmers is the
ability to indent a line of code in accordance with the structure of the
programming language. The Erlang mode does, of course, provide this feature. The
layout used is based on the common use of the language. The mode also provides
things as syntax highlighting, electric commands, module name verification,
comment support including paragraph filling, skeletons, tags support etc.

In the following descriptions the use of the word _Point_ means: "Point can be
seen as the position of the cursor. More precisely, the point is the position
between two characters while the cursor is drawn over the character following
the point".

## Indent

The following command are directly available for indentation.

-   _`TAB`_ (`erlang-indent-command`) - Indents the current line of code.
-   _`M-C-\`_ (`indent-region`) - Indents all lines in the region.
-   _`M-l`_ (`indent-for-comment`) - Insert a comment character to the right of
    the code on the line (if any).

Lines containing comment are indented differently depending on the number of
%-characters used:

-   Lines with one %-character is indented to the right of the code. The column
    is specified by the variable `comment-column`, by default column 48 is used.
-   Lines with two %-characters will be indented to the same depth as code would
    have been in the same situation.
-   Lines with three of more %-characters are indented to the left margin.
-   _`C-c C-q`_ (`erlang-indent-function`) - Indents the current Erlang
    function.
-   _`M-x erlang-indent-clause RET`_ - Indent the current Erlang clause.
-   _`M-x erlang-indent-current-buffer RET`_ - Indent the entire buffer.

## Edit - Fill Comment

When editing normal text in text mode you can let Emacs reformat the text by the
`fill-paragraph` command. This command will not work for comments since it will
treat the comment characters as words. The Erlang editing mode provides a
command that knows about the Erlang comment structure and can be used to fill
text paragraphs in comments. Ex:

```erlang
%% This is   just a very simple test to show
%% how the Erlang fill
%% paragraph   command works.
```

Clearly, the text is badly formatted. Instead of formatting this paragraph line
by line, let's try `erlang-fill-paragraph` by pressing _`M-q`_. The result is:

```erlang
%% This is just a very simple test to show how the Erlang fill
%% paragraph command works.
```

## Edit - Comment/Uncomment Region

_`C-c C-c`_ will put comment characters at the beginning of all lines in a
marked region. If you want to have two comment characters instead of one you can
do _`C-u 2 C-c C-c`_

_`C-c C-u`_ will undo a comment-region command.

## Edit - Moving the point

-   _`M-C-a`_ (`erlang-beginning-of-function`) - Move the point to the beginning
    of the current or preceding Erlang function. With an numeric argument (ex
    _`C-u 2 M-C-a`_) the function skips backwards over this many Erlang
    functions. Should the argument be negative the point is moved to the
    beginning of a function below the current function.
-   _`C-c M-a`_ (`erlang-beginning-of-clause`) - As above but move point to the
    beginning of the current or preceding Erlang clause.
-   _`M-C-e`_ (`erlang-end-of-function`) - Move to the end of the current or
    following Erlang function. With an numeric argument (ex _`C-u 2 M-C-e`_) the
    function skips backwards over this many Erlang functions. Should the
    argument be negative the point is moved to the end of a function below the
    current function.
-   _`C-c M-e`_ (`erlang-end-of-clause`) - As above but move point to the end of
    the current or following Erlang clause.

## Edit - Marking

-   _`M-C-h`_ (`erlang-mark-function`) - Put the region around the current
    Erlang function. The point is placed in the beginning and the mark at the
    end of the function.
-   _`C-c M-h`_ (`erlang-mark-clause`) Put the region around the current Erlang
    clause. The point is placed in the beginning and the mark at the end of the
    function.

## Edit - Function Header Commands

-   _`C-c C-j`_ (`erlang-generate-new-clause`) - Create a new clause in the
    current Erlang function. The point is placed between the parentheses of the
    argument list.
-   _`C-c C-y`_ (`erlang-clone-arguments`) - Copy the function arguments of the
    preceding Erlang clause. This command is useful when defining a new clause
    with almost the same argument as the preceding.

## Edit - Alignment

-   _`C-c C-a`_ (`align-current`) - aligns comments, arrows, assignments,
    and type annotations around the cursor.

```erlang
Example:

sum(L) -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H);  % recurse
sum([], Sum) -> Sum.   % base case

-record { two :: int(), % hello
          three = hello :: string(),    % there
          four = 42 :: int() }.

becomes:

sum(L) -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H); % recurse
sum([], Sum)    -> Sum.             % base case

-record { two           :: int(),    % hello
          three = hello :: string(), % there
          four  = 42    :: int() }.
```

## Syntax highlighting

The syntax highlighting can be activated from the Erlang menu. There are four
different alternatives:

-   Off: Normal black and white display.
-   Level 1: Function headers, reserved words, comments, strings, quoted atoms,
    and character constants will be colored.
-   Level 2: The above, attributes, Erlang bif:s, guards, and words in comments
    enclosed in single quotes will be colored.
-   Level 3: The above, variables, records, and macros will be colored. (This
    level is also known as the Christmas tree level.)

## Tags

For the tag commands to work it requires that you have generated a tag file. See
[Erlang mode users guide](erlang_mode_chapter.md#tags)

-   _`M-.`_ (`find-tag`) - Find a function definition. The default value is the
    function name under the point.
-   Find Tag (`erlang-find-tag`) - Like the Elisp-function
    `find-tag'. Capable of retrieving Erlang modules. Tags can be given on the forms `tag',
    `module:', `module:tag'.
-   _`M-+`_ (`erlang-find-next-tag`) - Find the next occurrence of tag.
-   _`M-TAB`_ (`erlang-complete-tag`) - Perform completion on the tag entered in
    a tag search. Completes to the set of names listed in the current tags
    table.
-   Tags aprops (`tags-apropos`) - Display list of all tags in tags table REGEXP
    matches.
-   _`C-x t s`_ (`tags-search`) - Search through all files listed in tags table
    for match for REGEXP. Stops when a match is found.

## Skeletons

A skeleton is a piece of pre-written code that can be inserted into the buffer.
Erlang mode comes with a set of predefined skeletons. The skeletons can be
accessed either from the Erlang menu of from commands named
`tempo-template-erlang-*`, as the skeletons is defined using the standard Emacs
package "tempo". Here follows a brief description of the available skeletons:

-   Simple skeletons: If, Case, Receive, Receive After, Receive Loop - Basic
    code constructs.
-   Header elements: Module, Author - These commands insert lines on the form
    `-module('xxx').` and `-author('my@home').`. They can be used directly, but
    are also used as part of the full headers described below.
-   Full Headers: Small (minimum requirement), Medium (with fields for basic
    information about the module), and Large Header (medium header with some
    extra layout structure).
-   Small Server - skeleton for a simple server not using OTP.
-   Application - skeletons for the OTP application behavior
-   Supervisor - skeleton for the OTP supervisor behavior
-   Supervisor Bridge - skeleton for the OTP supervisor bridge behavior
-   gen_server - skeleton for the OTP gen_server behavior
-   gen_event - skeleton for the OTP gen_event behavior
-   gen_fsm - skeleton for the OTP gen_fsm behavior
-   gen_statem (StateName/3) - skeleton for the OTP gen_statem behavior using
    state name functions
-   gen_statem (handle_event/4) - skeleton for the OTP gen_statem behavior using
    one state function
-   Library module - skeleton for a module that does not implement a process.
-   Corba callback - skeleton for a Corba callback module.
-   Erlang test suite - skeleton for a callback module for the erlang test
    server.

## Shell

-   New shell (`erlang-shell`) - Starts a new Erlang shell.
-   _`C-c C-z,`_ (`erlang-shell-display `) - Displays an Erlang shell, or starts
    a new one if there is no shell started.

## Compile

-   _`C-c C-k,`_ (`erlang-compile`) - Compiles the Erlang module in the current
    buffer. You can also use _`C-u C-c C-k`_ to debug compile the module with
    the debug options `debug_info` and `export_all`.
-   _`C-c C-l,`_ (`erlang-compile-display`) - Display compilation output.
-   _``C-u C-x` ``_ Start parsing the compiler output from the beginning. This
    command will place the point on the line where the first error was found.
-   _`` C-x` ``_ (`erlang-next-error`) - Move the point on to the next error.
    The buffer displaying the compilation errors will be updated so that the
    current error will be visible.

## Man

On unix you can view the manual pages in emacs. In order to find the manual
pages, the variable `erlang-root-dir` should be bound to the name of the
directory containing the Erlang installation. The name should not include the
final slash. Practically, you should add a line on the following form to your
~/.emacs,

```text
(setq erlang-root-dir "/the/erlang/root/dir/goes/here")
```

## Starting IMenu

-   _`M-x imenu-add-to-menubar RET`_ - This command will create the IMenu menu
    containing all the functions in the current buffer.The command will ask you
    for a suitable name for the menu. Not supported by Xemacs.

## Version

-   _`M-x erlang-version RET`_ - This command displays the version number of the
    Erlang editing mode. Remember to always supply the version number when
    asking questions about the Erlang mode.
