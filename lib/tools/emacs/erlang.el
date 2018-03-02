;;; erlang.el --- Major modes for editing and running Erlang -*- lexical-binding: t; -*-

;; Copyright (C) 2004  Free Software Foundation, Inc.
;; Author:   Anders Lindgren
;; Keywords: erlang, languages, processes
;; Date:     2011-12-11
;; Version:  2.8.1
;; Package-Requires: ((emacs "24.1"))

;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 1996-2017. All Rights Reserved.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; %CopyrightEnd%
;;

;; Lars Thorsén's modifications of 2000-06-07 included.
;; The original version of this package was written by Robert Virding.
;;
;;; Commentary:

;; Introduction:
;; ------------
;;
;; This package provides support for the programming language Erlang.
;; The package provides an editing mode with lots of bells and
;; whistles, compilation support, and it makes it possible for the
;; user to start Erlang shells that run inside Emacs.
;;
;; See the Erlang distribution for full documentation of this package.

;; Installation:
;; ------------
;;
;; Place this file in Emacs load path, byte-compile it, and add the
;; following line to the appropriate init file:
;;
;;    (require 'erlang-start)
;;
;; The full documentation contains much more extensive description of
;; the installation procedure.

;; Reporting Bugs:
;; --------------
;;
;; Please send bug reports to the following email address:
;;      erlang-bugs@erlang.org
;; or if you have a patch suggestion to:
;;      erlang-patches@erlang.org
;; Please state as exactly as possible:
;;    - Version number of Erlang Mode (see the menu), Emacs, Erlang,
;;      and of any other relevant software.
;;    - What the expected result was.
;;    - What you did, preferably in a repeatable step-by-step form.
;;    - A description of the unexpected result.
;;    - Relevant pieces of Erlang code causing the problem.
;;    - Personal Emacs customisations, if any.
;;
;; Should the Emacs generate an error, please set the Emacs variable
;; `debug-on-error' to `t'.  Repeat the error and enclose the debug
;; information in your bug-report.
;;
;; To toggle the variable you can use the following command:
;;     M-x toggle-debug-on-error RET
;;; Code:

(eval-when-compile (require 'cl))

;; Variables:

(defgroup erlang nil
  "The Erlang programming language."
  :group 'languages)

(defconst erlang-version "2.8.1"
  "The version number of Erlang mode.")

(defcustom erlang-root-dir nil
  "The directory where the Erlang system is installed.
The name should not contain the trailing slash.

Should this variable be nil, no manual pages will show up in the
Erlang mode menu."
  :group 'erlang
  :type '(restricted-sexp :match-alternatives (stringp 'nil))
  :safe (lambda (val) (or (eq nil val) (stringp val))))

(defvar erlang-menu-items '(erlang-menu-base-items
                            erlang-menu-skel-items
                            erlang-menu-shell-items
                            erlang-menu-compile-items
                            erlang-menu-man-items
                            erlang-menu-personal-items
                            erlang-menu-version-items)
  "List of menu item list to combine to create Erlang mode menu.

External programs which temporarily add menu items to the Erlang mode
menu may use this variable.  Please use the function `add-hook' to add
items.

Please call the function `erlang-menu-init' after every change to this
variable.")

(defvar erlang-menu-base-items
  '(("Indent"
     (("Indent Line" erlang-indent-command)
      ("Indent Region " erlang-indent-region mark-active)
      ("Indent Clause" erlang-indent-clause)
      ("Indent Function" erlang-indent-function)
      ("Indent Buffer" erlang-indent-current-buffer)))
    ("Edit"
     (("Fill Comment" erlang-fill-paragraph)
      ("Comment Region" comment-region mark-active)
      ("Uncomment Region" uncomment-region mark-active)
      nil
      ("Beginning of Function" erlang-beginning-of-function)
      ("End of Function" erlang-end-of-function)
      ("Mark Function" erlang-mark-function)
      nil
      ("Beginning of Clause" erlang-beginning-of-clause)
      ("End of Clause" erlang-end-of-clause)
      ("Mark Clause" erlang-mark-clause)
      nil
      ("New Clause" erlang-generate-new-clause)
      ("Clone Arguments" erlang-clone-arguments)
      nil
      ("Align Arrows" erlang-align-arrows)))
    ("Syntax Highlighting"
     (("Level 4" erlang-font-lock-level-4)
      ("Level 3" erlang-font-lock-level-3)
      ("Level 2" erlang-font-lock-level-2)
      ("Level 1" erlang-font-lock-level-1)
      ("Off" erlang-font-lock-level-0)))
    ("TAGS"
     (("Find Tag" find-tag)
      ("Find Next Tag" erlang-find-next-tag)
                                        ;("Find Regexp" find-tag-regexp)
      ("Complete Word" erlang-complete-tag)
      ("Tags Apropos" tags-apropos)
      ("Search Files" tags-search))))
  "Description of menu used in Erlang mode.

This variable must be a list.  The elements are either nil representing
a horizontal line or a list with two or three elements.  The first is
the name of the menu item, the second is the function to call, or a
submenu, on the same same form as ITEMS.  The third optional argument
is an expression which is evaluated every time the menu is displayed.
Should the expression evaluate to nil the menu item is ghosted.

Example:
    '((\"Func1\" function-one)
      (\"SubItem\"
       ((\"Yellow\" function-yellow)
        (\"Blue\" function-blue)))
      nil
      (\"Region Function\" spook-function midnight-variable))

Call the function `erlang-menu-init' after modifying this variable.")

(defvar erlang-menu-shell-items
  '(nil
    ("Shell"
     (("Start New Shell" erlang-shell)
      ("Display Shell"   erlang-shell-display))))
  "Description of the Shell menu used by Erlang mode.

Please see the documentation of `erlang-menu-base-items'.")

(defvar erlang-menu-compile-items
  '(("Compile"
     (("Compile Buffer" erlang-compile)
      ("Display Result" erlang-compile-display)
      ("Next Error"     erlang-next-error))))
  "Description of the Compile menu used by Erlang mode.

Please see the documentation of `erlang-menu-base-items'.")

(defvar erlang-menu-version-items
  '(nil
    ("Version" erlang-version))
  "Description of the version menu used in Erlang mode.")

(defvar erlang-menu-personal-items nil
  "Description of personal menu items used in Erlang mode.

Please see the variable `erlang-menu-base-items' for a description
of the format.")

(defvar erlang-menu-man-items nil
  "The menu containing man pages.

The format of the menu should be compatible with `erlang-menu-base-items'.
This variable is added to the list of Erlang menus stored in
`erlang-menu-items'.")

(defvar erlang-menu-skel-items '()
  "Description of the menu containing the skeleton entries.
The menu is in the form described by the variable `erlang-menu-base-items'.")

(defvar erlang-mode-hook nil
  "Functions to run when Erlang mode is activated.

This hook is used to change the behaviour of Erlang mode.  It is
normally used by the user to personalise the programming environment.
When used in a site init file, it could be used to customise Erlang
mode for all users on the system.

The functions added to this hook are run every time Erlang mode is
started.  See also `erlang-load-hook', a hook which is run once,
when Erlang mode is loaded into Emacs, and `erlang-shell-mode-hook'
which is run every time a new inferior Erlang shell is started.

To use a hook, create an Emacs lisp function to perform your actions
and add the function to the hook by calling `add-hook'.

The following example binds the key sequence C-c C-c to the command
`erlang-compile' (normally bound to C-c C-k).  The example also
activates Font Lock mode to fontify the buffer and adds a menu
containing all functions defined in the current buffer.

To use the example, copy the following lines to your `~/.emacs' file:

    (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

    (defun my-erlang-mode-hook ()
      (local-set-key \"\\C-c\\C-c\" 'erlang-compile)
      (if window-system
          (progn
            (setq font-lock-maximum-decoration t)
            (font-lock-mode 1)))
      (if (and window-system (fboundp 'imenu-add-to-menubar))
          (imenu-add-to-menubar \"Imenu\")))")

(defvar erlang-load-hook nil
  "Functions to run when Erlang mode is loaded.

This hook is used to change the behaviour of Erlang mode.  It is
normally used by the user to personalise the programming environment.
When used in a site init file, it could be used to customize Erlang
mode for all users on the system.

The difference between this hook and `erlang-mode-hook' and
`erlang-shell-mode-hook' is that the functions in this hook
is only called once, when the Erlang mode is loaded into Emacs
the first time.

Natural actions for the functions added to this hook are actions which
only should be performed once, and actions which should be performed
before starting Erlang mode.  For example, a number of variables are
used by Erlang mode before `erlang-mode-hook' is run.

The following example sets the variable `erlang-root-dir' so that the
manual pages can be retrieved (note that you must set the value of
`erlang-root-dir' to match the location of Erlang on your system):

    (add-hook 'erlang-load-hook 'my-erlang-load-hook)

    (defun my-erlang-load-hook ()
       (setq erlang-root-dir \"/usr/local/erlang\"))")

(defvar erlang-new-file-hook nil
  "Functions to run when a new Erlang source file is being edited.

A useful function is `tempo-template-erlang-normal-header'.
\(This function only exists when the `tempo' package is available.)")

(defcustom erlang-check-module-name 'ask
  "Non-nil means check that module name and file name agrees when saving.

If the value of this variable is the symbol `ask', the user is
prompted.  If the value is t the source is silently changed."
  :group 'erlang
  :type '(choice (const :tag "Check on save" 'ask)
                 (const :tag "Don't check on save" t)))

(defvar erlang-electric-commands
  '(erlang-electric-comma
    erlang-electric-semicolon
    erlang-electric-gt)
  "List of activated electric commands.

The list should contain the electric commands which should be active.
Currently, the available electric commands are:
    `erlang-electric-comma'
    `erlang-electric-semicolon'
    `erlang-electric-gt'
    `erlang-electric-newline'

Should the variable be bound to t, all electric commands
are activated.

To deactivate all electric commands, set this variable to nil.")

(defcustom erlang-electric-newline-inhibit t
  "Set to non-nil to inhibit newline after electric command.

This is useful since a lot of people press return after executing an
electric command.

In order to work, the command must also be in the
list `erlang-electric-newline-inhibit-list'.

Note that commands in this list are required to set the variable
`erlang-electric-newline-inhibit' to nil when the newline shouldn't be
inhibited."
  :group 'erlang
  :type 'boolean
  :safe 'booleanp)

(defvar erlang-electric-newline-inhibit-list
  '(erlang-electric-semicolon
    erlang-electric-comma
    erlang-electric-gt)
  "Commands which can inhibit the next newline.")

(defcustom erlang-electric-semicolon-insert-blank-lines nil
  "Number of blank lines inserted before header, or nil.

This variable controls the behaviour of `erlang-electric-semicolon'
when a new function header is generated.  When nil, no blank line is
inserted between the current line and the new header.  When bound to a
number it represents the number of blank lines which should be
inserted."
  :group 'erlang)

(defvar erlang-electric-semicolon-criteria
  '(erlang-next-lines-empty-p
    erlang-at-keyword-end-p
    erlang-at-end-of-function-p)
  "List of functions controlling `erlang-electric-semicolon'.
The functions in this list are called, in order, whenever a semicolon
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert prototype, and stop checking

If every function in the list is called with no determination made,
then no prototype is inserted.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-electric-comma-criteria
  '(erlang-stop-when-inside-argument-list
    erlang-stop-when-at-guard
    erlang-next-lines-empty-p
    erlang-at-keyword-end-p
    erlang-at-end-of-clause-p
    erlang-at-end-of-function-p)
  "List of functions controlling `erlang-electric-comma'.
The functions in this list are called, in order, whenever a comma
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert prototype, and stop checking

If every function in the list is called with no determination made,
then no prototype is inserted.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-electric-arrow-criteria
  '(erlang-stop-when-in-type-spec
    erlang-next-lines-empty-p
    erlang-at-end-of-function-p)
  "List of functions controlling the arrow aspect of `erlang-electric-gt'.
The functions in this list are called, in order, whenever a `>'
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert prototype, and stop checking

If every function in the list is called with no determination made,
then no prototype is inserted.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-electric-newline-criteria
  '(t)
  "List of functions controlling `erlang-electric-newline'.

The electric newline commands indents the next line.  Should the
current line begin with a comment the comment start is copied to
the newly created line.

The functions in this list are called, in order, whenever a comma
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- trigger the electric command.

If every function in the list is called with no determination made,
then no prototype is inserted.  Should the atom t be a member of the
list, it is treated as a function triggering the electric command.

The test is performed by the function `erlang-test-criteria-list'.")

(defcustom erlang-next-lines-empty-threshold 2
  "Number of blank lines required to activate an electric command.

Actually, this value controls the behaviour of the function
`erlang-next-lines-empty-p' which normally is a member of the
criteria lists controlling the electric commands.  (Please see
the variables `erlang-electric-semicolon-criteria' and
`erlang-electric-comma-criteria'.)

The variable is bound to a threshold value, a number, representing the
number of lines which must be empty.

Setting this variable to zero, electric commands will always be
triggered by `erlang-next-lines-empty-p', unless inhibited by other
rules.

Should this variable be nil, `erlang-next-lines-empty-p' will never
trigger an electric command.  The same effect would be reached if the
function `erlang-next-lines-empty-p' would be removed from the criteria
lists.

Note that even if `erlang-next-lines-empty-p' should not trigger an
electric command, other functions in the criteria list could."
  :group 'erlang
  :type '(restricted-sexp :match-alternatives (integerp 'nil))
  :safe (lambda (val) (or (eq val nil) (integerp val))))

(defcustom erlang-new-clause-with-arguments nil
  "Non-nil means that the arguments are cloned when a clause is generated.

A new function header can be generated by calls to the function
`erlang-generate-new-clause' and by use of the electric semicolon."
  :group 'erlang
  :type 'boolean
  :safe 'booleanp)

(defcustom erlang-compile-use-outdir t
  "When nil, go to the directory containing source file when compiling.

This is a workaround for a bug in the `outdir' option of compile.  If the
outdir is not in the current load path, Erlang doesn't load the object
module after it has been compiled.

To activate the workaround, place the following in your `~/.emacs' file:
    (setq erlang-compile-use-outdir nil)"
  :group 'erlang
  :type 'boolean
  :safe 'booleanp)

(defcustom erlang-indent-level 4
  "Indentation of Erlang calls/clauses within blocks."
  :group 'erlang
  :type 'integer
  :safe 'integerp)

(defcustom erlang-icr-indent nil
  "Indentation of Erlang if/case/receive patterns.
nil means keeping default behavior.  When non-nil, indent to the column of
if/case/receive."
  :group 'erlang
  :type 'boolean
  :safe 'booleanp)

(defcustom erlang-indent-guard 2
  "Indentation of Erlang guards."
  :group 'erlang
  :type 'integer
  :safe 'integerp)

(defcustom erlang-argument-indent 2
  "Indentation of the first argument in a function call.
When nil, indent to the column after the `(' of the
function."
  :group 'erlang
  :type '(restricted-sexp :match-alternatives (integerp 'nil))
  :safe (lambda (val) (or (eq val nil) (integerp val))))

(defcustom erlang-tab-always-indent t
  "Non-nil means TAB in Erlang mode should always re-indent the current line,
regardless of where in the line point is when the TAB command is used."
  :group 'erlang
  :type 'boolean
  :safe 'booleanp)

(defvar erlang-man-inhibit (eq system-type 'windows-nt)
  "Inhibit the creation of the Erlang Manual Pages menu.

The Windows distribution of Erlang does not include man pages, hence
there is no attempt to create the menu.")

(defvar erlang-man-dirs
  '(("Man - Commands" "/man/man1" t)
    ("Man - Modules" "/man/man3" t)
    ("Man - Files" "/man/man4" t)
    ("Man - Applications" "/man/man6" t))
  "The man directories displayed in the Erlang menu.

Each item in the list should be a list with three elements, the first
the name of the menu, the second the directory, and the last a flag.
Should the flag the nil, the directory is absolute, should it be non-nil
the directory is relative to the variable `erlang-root-dir'.")

(defvar erlang-man-max-menu-size 35
  "The maximum number of menu items in one menu allowed.")

(defvar erlang-man-display-function 'erlang-man-display
  "Function used to display man page.

The function is called with one argument, the name of the file
containing the man page.  Use this variable when the default
function, `erlang-man-display', does not work on your system.")

(defvar erlang-compile-extra-opts '()
  "Additional options to the compilation command.
This is an elisp list of options. Each option can be either:
- an atom
- a dotted pair
- a string
Example: '(bin_opt_info (i . \"/path1/include\") (i . \"/path2/include\"))")

(defvar erlang-compile-command-function-alist
  '((".erl\\'" . inferior-erlang-compute-erl-compile-command)
    (".xrl\\'" . inferior-erlang-compute-leex-compile-command)
    (".yrl\\'" . inferior-erlang-compute-yecc-compile-command)
    ("." . inferior-erlang-compute-erl-compile-command))
  "Alist of filename patterns vs corresponding compilation functions.
Each element looks like (REGEXP . FUNCTION). Compiling a file whose name
matches REGEXP specifies FUNCTION to use to compute the compilation
command. The FUNCTION will be called with two arguments: module name and
default compilation options, like output directory. The FUNCTION
is expected to return a string.")

(defvar erlang-leex-compile-opts '()
  "Options to pass to leex when compiling xrl files.
This is an elisp list of options. Each option can be either:
- an atom
- a dotted pair
- a string")

(defvar erlang-yecc-compile-opts '()
  "Options to pass to yecc when compiling yrl files.
This is an elisp list of options. Each option can be either:
- an atom
- a dotted pair
- a string")

(eval-and-compile
  (defvar erlang-regexp-modern-p t
    "Non-nil when this version of Emacs uses a modern version of regexp.
Supporting \_< and \_> This is determined by checking the version of Emacs used."))

(eval-and-compile
  (defconst erlang-atom-quoted-regexp
    "'\\(?:[^\\']\\|\\(?:\\\\.\\)\\)*'"
    "Regexp describing a single-quoted atom"))

(eval-and-compile
  (defconst erlang-atom-regular-regexp
    (if erlang-regexp-modern-p
        "\\_<[[:lower:]]\\(?:\\sw\\|\\s_\\)*\\_>"
      "\\<[[:lower:]]\\(?:\\sw\\|\\s_\\)*\\>")
    "Regexp describing a regular (non-quoted) atom"))

(eval-and-compile
  (defconst erlang-atom-regexp
    (concat "\\(" erlang-atom-quoted-regexp "\\|"
            erlang-atom-regular-regexp "\\)")
    "Regexp describing an Erlang atom."))

(eval-and-compile
  (defconst erlang-atom-regexp-matches 1
    "Number of regexp parenthesis pairs in `erlang-atom-regexp'.

This is used to determine parenthesis matches in complex regexps which
contains `erlang-atom-regexp'."))


(eval-and-compile
  (defconst erlang-variable-regexp
    (if erlang-regexp-modern-p
        "\\_<\\([[:upper:]_]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
      "\\<\\([[:upper:]_]\\(?:\\sw\\|\\s_\\)*\\)\\>")
    "Regexp which should match an Erlang variable.

The regexp must be surrounded with a pair of regexp parentheses."))

(eval-and-compile
  (defconst erlang-variable-regexp-matches 1
    "Number of regexp parenthesis pairs in `erlang-variable-regexp'.

This is used to determine matches in complex regexps which contains
`erlang-variable-regexp'."))

(defconst erlang-module-function-regexp
  (eval-when-compile
    (concat erlang-atom-regexp ":" erlang-atom-regexp))
  "Regexp matching an erlang module:function.")

(defconst erlang-name-regexp
    (concat "\\("
            "\\(?:\\sw\\|\\s_\\)+"
            "\\|"
            erlang-atom-quoted-regexp
            "\\)")
    "Matches a name of a function, macro or record")

(defconst erlang-id-regexp
  (concat "\\(?:\\(qualified-function\\|record\\|macro\\|module\\) \\)?"
          "\\(?:" erlang-atom-regexp ":\\)?"
          erlang-name-regexp "?"
          "\\(?:/\\([0-9]+\\)\\)?"))

(eval-and-compile
  (defun erlang-regexp-opt (strings &optional paren)
    "Like `regexp-opt', except if PAREN is `symbols', then the
resulting regexp is surrounded by \\_< and \\_>."
    (if (eq paren 'symbols)
        (if erlang-regexp-modern-p
            (concat "\\_<" (regexp-opt strings t) "\\_>")
          (concat "\\<" (regexp-opt strings t) "\\>"))
      (regexp-opt strings paren))))


(eval-and-compile
  (defvar erlang-keywords
    '("after"
      "begin"
      "catch"
      "case"
      "cond"
      "end"
      "fun"
      "if"
      "let"
      "of"
      "receive"
      "try"
      "when")
    "Erlang reserved keywords"))

(eval-and-compile
  (defconst erlang-keywords-regexp (erlang-regexp-opt erlang-keywords 'symbols)))

(eval-and-compile
  (defvar erlang-operators
    '("and"
      "andalso"
      "band"
      "bnot"
      "bor"
      "bsl"
      "bsr"
      "bxor"
      "div"
      "not"
      "or"
      "orelse"
      "rem"
      "xor")
    "Erlang operators"))
;; What about these?
;; '+' '-' '*' '/' '>', '>=', '<', '=<', '=:=', '==', '=/=', '/='

(eval-and-compile
  (defconst erlang-operators-regexp (erlang-regexp-opt erlang-operators 'symbols)))


(eval-and-compile
  (defvar erlang-guards
    '("is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_map"
      "is_number"
      "is_pid"
      "is_port"
      "is_record"
      "is_reference"
      "is_tuple"
      "atom"
      "binary"
      "bitstring"
      "boolean"
      ;;"float" ; Not included to avoid clashes with the bif float/1
      "function"
      "integer"
      "list"
      "number"
      "pid"
      "port"
      "record"
      "reference"
      "tuple")
    "Erlang guards"))

(eval-and-compile
  (defconst erlang-guards-regexp (erlang-regexp-opt erlang-guards 'symbols)))

(eval-and-compile
  (defvar erlang-predefined-types
    '("any"
      "arity"
      "boolean"
      "byte"
      "char"
      "cons"
      "deep_string"
      "iodata"
      "iolist"
      "maybe_improper_list"
      "module"
      "mfa"
      "nil"
      "neg_integer"
      "none"
      "non_neg_integer"
      "nonempty_list"
      "nonempty_improper_list"
      "nonempty_maybe_improper_list"
      "nonempty_string"
      "no_return"
      "pos_integer"
      "string"
      "term"
      "timeout"
      "map")
    "Erlang type specs types"))

(eval-and-compile
  (defconst erlang-predefined-types-regexp
    (erlang-regexp-opt erlang-predefined-types 'symbols)))


(eval-and-compile
  (defvar erlang-int-bifs
    '("abs"
      "apply"
      "atom_to_binary"
      "atom_to_list"
      "binary_to_atom"
      "binary_to_existing_atom"
      "binary_to_float"
      "binary_to_integer"
      "binary_to_list"
      "binary_to_term"
      "binary_part"
      "bit_size"
      "bitsize"
      "bitstring_to_list"
      "byte_size"
      "ceil"
      "check_old_code"
      "check_process_code"
      "date"
      "delete_module"
      "demonitor"
      "disconnect_node"
      "element"
      "erase"
      "error"
      "exit"
      "floor"
      "float"
      "float_to_binary"
      "float_to_list"
      "garbage_collect"
      "get"
      "get_keys"
      "group_leader"
      "halt"
      "hd"
      "integer_to_list"
      "integer_to_binary"
      "iolist_size"
      "iolist_to_binary"
      "is_alive"
      "is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_map"
      "is_number"
      "is_pid"
      "is_port"
      "is_process_alive"
      "is_record"
      "is_reference"
      "is_tuple"
      "length"
      "link"
      "list_to_atom"
      "list_to_binary"
      "list_to_bitstring"
      "list_to_existing_atom"
      "list_to_float"
      "list_to_integer"
      "list_to_pid"
      "list_to_port"
      "list_to_ref"
      "list_to_tuple"
      "load_module"
      "make_ref"
      "map_size"
      "max"
      "min"
      "module_loaded"
      "monitor"
      "monitor_node"
      "node"
      "nodes"
      "now"
      "open_port"
      "pid_to_list"
      "port_close"
      "port_command"
      "port_connect"
      "port_control"
      "port_to_list"
      "pre_loaded"
      "process_flag"
      "process_info"
      "processes"
      "purge_module"
      "put"
      "ref_to_list"
      "register"
      "registered"
      "round"
      "self"
      "setelement"
      "size"
      "spawn"
      "spawn_link"
      "spawn_monitor"
      "spawn_opt"
      "split_binary"
      "statistics"
      "term_to_binary"
      "time"
      "throw"
      "tl"
      "trunc"
      "tuple_size"
      "tuple_to_list"
      "unlink"
      "unregister"
      "whereis")
    "Erlang built-in functions (BIFs)"))

(eval-and-compile
  (defconst erlang-int-bif-regexp (erlang-regexp-opt erlang-int-bifs 'symbols)))


(eval-and-compile
  (defvar erlang-ext-bifs
    '("adler32"
      "adler32_combine"
      "alloc_info"
      "alloc_sizes"
      "append"
      "append_element"
      "await_proc_exit"
      "bump_reductions"
      "call_on_load_function"
      "cancel_timer"
      "crasher"
      "crc32"
      "crc32_combine"
      "decode_packet"
      "delay_trap"
      "delete_element"
      "display"
      "display_nl"
      "display_string"
      "dist_get_stat"
      "dist_ctrl_get_data"
      "dist_ctrl_get_data_notification"
      "dist_ctrl_input_handler"
      "dist_ctrl_put_data"
      "dmonitor_node"
      "dmonitor_p"
      "dt_append_vm_tag_data"
      "dt_get_tag"
      "dt_get_tag_data"
      "dt_prepend_vm_tag_data"
      "dt_put_tag"
      "dt_restore_tag"
      "dt_spread_tag"
      "convert_time_unit"
      "external_size"
      "finish_after_on_load"
      "finish_loading"
      "format_cpu_topology"
      "fun_info"
      "fun_info_mfa"
      "fun_to_list"
      "function_exported"
      "garbage_collect_message_area"
      "gather_gc_info_result"
      "get_cookie"
      "get_module_info"
      "get_stacktrace"
      "has_prepared_code_on_load"
      "hibernate"
      "insert_element"
      "iolist_to_iovec"
      "is_builtin"
      "load_nif"
      "loaded"
      "localtime"
      "localtime_to_universaltime"
      "make_fun"
      "make_tuple"
      "match_spec_test"
      "md5"
      "md5_final"
      "md5_init"
      "md5_update"
      "memory"
      "module_info"
      "monitor_node"
      "monotonic_time"
      "nif_error"
      "phash"
      "phash2"
      "port_call"
      "port_get_data"
      "port_info"
      "port_set_data"
      "ports"
      "posixtime_to_universaltime"
      "prepare_loading"
      "process_display"
      "raise"
      "read_timer"
      "resume_process"
      "send"
      "send_after"
      "send_nosuspend"
      "seq_trace"
      "seq_trace_info"
      "seq_trace_print"
      "set_cookie"
      "set_cpu_topology"
      "setnode"
      "spawn_opt"
      "start_timer"
      "subtract"
      "suspend_process"
      "system_flag"
      "system_info"
      "system_monitor"
      "system_profile"
      "system_time"
      "trace"
      "trace_delivered"
      "trace_info"
      "trace_pattern"
      "time_offset"
      "timestamp"
      "universaltime"
      "universaltime_to_localtime"
      "universaltime_to_posixtime"
      "unique_integer"
      "yield")
    "Erlang built-in functions (BIFs) that needs erlang: prefix"))

(eval-and-compile
  (defconst erlang-ext-bif-regexp
    (erlang-regexp-opt (append erlang-int-bifs erlang-ext-bifs) 'symbols)))


(defvar erlang-defun-prompt-regexp (concat "^" erlang-atom-regexp "\\s *(")
  "Regexp which should match beginning of a clause.")

(defvar erlang-file-name-extension-regexp "\\.erl$"
  "Regexp which should match an Erlang file name.

This regexp is used when an Erlang module name is extracted from the
name of an Erlang source file.

The regexp should only match the section of the file name which should
be excluded from the module name.

To match all files set this variable to \"\\\\(\\\\..*\\\\|\\\\)$\".
The matches all except the extension.  This is useful if the Erlang
tags system should interpret tags on the form `module:tag' for
files written in other languages than Erlang.")

(defvar erlang-inferior-shell-split-window t
  "If non-nil, when starting an inferior shell, split windows.
If nil, the inferior shell replaces the window. This is the traditional
behaviour.")

(defvar erlang-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";"       'erlang-electric-semicolon)
    (define-key map ","       'erlang-electric-comma)
    (define-key map "<"         'erlang-electric-lt)
    (define-key map ">"         'erlang-electric-gt)
    (define-key map "\C-m"      'erlang-electric-newline)
    (define-key map [(backspace)] 'backward-delete-char-untabify)
    (define-key map "\M-q"      'erlang-fill-paragraph)
    (define-key map "\M-\t"     'erlang-complete-tag)
    (define-key map "\C-c\M-\t" 'tempo-complete-tag)
    (define-key map "\M-+"      'erlang-find-next-tag)
    (define-key map "\C-c\M-a"  'erlang-beginning-of-clause)
    (define-key map "\C-c\M-b"  'tempo-backward-mark)
    (define-key map "\C-c\M-e"  'erlang-end-of-clause)
    (define-key map "\C-c\M-f"  'tempo-forward-mark)
    (define-key map "\C-c\M-h"  'erlang-mark-clause)
    (define-key map "\C-c\C-c"  'comment-region)
    (define-key map "\C-c\C-j"  'erlang-generate-new-clause)
    (define-key map "\C-c\C-k"  'erlang-compile)
    (define-key map "\C-c\C-l"  'erlang-compile-display)
    (define-key map "\C-c\C-s"  'erlang-show-syntactic-information)
    (define-key map "\C-c\C-q"  'erlang-indent-function)
    (define-key map "\C-c\C-u"  'uncomment-region)
    (define-key map "\C-c\C-y"  'erlang-clone-arguments)
    (define-key map "\C-c\C-a"  'erlang-align-arrows)
    (define-key map "\C-c\C-z"  'erlang-shell-display)
    map)
  "Keymap used in Erlang mode.")
(defvar erlang-mode-abbrev-table nil
  "Abbrev table in use in Erlang-mode buffers.")
(defvar erlang-mode-syntax-table nil
  "Syntax table in use in Erlang-mode buffers.")



(defvar erlang-skel-file "erlang-skels"
  "The type of erlang-skeletons that should be used, default
   uses edoc type, for the old type, standard comments,
   set \"erlang-skels-old\" in your .emacs and restart.

   Or define your own and set the variable to that file.")

;; Tempo skeleton templates:
(load erlang-skel-file)

;; Font-lock variables

;; The next few variables define different Erlang font-lock patterns.
;; They could be appended to form a custom font-lock appearance.
;;
;; The function `erlang-font-lock-set-face' could be used to change
;; the face of a pattern.
;;
;; Note that Erlang strings and atoms are highlighted with using
;; syntactic analysis.

(defvar erlang-font-lock-keywords-function-header
  (list
   (list (concat "^" erlang-atom-regexp "\\s-*(")
         1 'font-lock-function-name-face t))
  "Font lock keyword highlighting a function header.")

(defface erlang-font-lock-exported-function-name-face
  '((default (:inherit font-lock-function-name-face)))
  "Face used for highlighting exported functions."
  :group 'erlang)

(defvar erlang-font-lock-exported-function-name-face
  'erlang-font-lock-exported-function-name-face)

(defvar erlang-inhibit-exported-function-name-face nil
  "Inhibit separate face for exported functions")

(defvar erlang-font-lock-keywords-exported-function-header
  (list
   (list #'erlang-match-next-exported-function
         1 'erlang-font-lock-exported-function-name-face t))
  "Font lock keyword highlighting an exported function header.")

(defvar erlang-font-lock-keywords-int-bifs
  (list
   (list (concat erlang-int-bif-regexp "\\s-*(")
         1 'font-lock-builtin-face))
  "Font lock keyword highlighting built in functions.")

(defvar erlang-font-lock-keywords-ext-bifs
  (list
   (list (concat "\\<\\(erlang\\)\\s-*:\\s-*" erlang-ext-bif-regexp "\\s-*(")
         '(1 'font-lock-builtin-face)
         '(2 'font-lock-builtin-face)))
  "Font lock keyword highlighting built in functions.")

(defvar erlang-font-lock-keywords-int-function-calls
  (list
   (list (concat erlang-atom-regexp "\\s-*(")
         1 'font-lock-type-face))
  "Font lock keyword highlighting an internal function call.")

(defvar erlang-font-lock-keywords-ext-function-calls
  (list
   (list (concat erlang-atom-regexp "\\s-*:\\s-*"
                 erlang-atom-regexp "\\s-*(")
         '(1 'font-lock-type-face)
         '(2 'font-lock-type-face)))
  "Font lock keyword highlighting an external function call.")

(defvar erlang-font-lock-keywords-fun-n
  (list
   (list (concat "\\(" erlang-atom-regexp "/[0-9]+\\)")
         1 'font-lock-type-face))
  "Font lock keyword highlighting a fun descriptor in F/N format.")

(defvar erlang-font-lock-keywords-operators
  (list
   (list erlang-operators-regexp
         1 'font-lock-builtin-face))
  "Font lock keyword highlighting Erlang operators.")

(defvar erlang-font-lock-keywords-dollar
  (list
   (list "\\(\\$\\([^\\]\\|\\\\\\([^0-7^\n]\\|[0-7]+\\|\\^[a-zA-Z]\\)\\)\\)"
         1 'font-lock-constant-face))
  "Font lock keyword highlighting numbers in ASCII form (e.g. $A).")

(defvar erlang-font-lock-keywords-arrow
  (list
   (list "->\\(\\s \\|$\\)" 1 'font-lock-function-name-face))
  "Font lock keyword highlighting clause arrow.")

(defvar erlang-font-lock-keywords-lc
  (list
   (list "\\(<-\\|<=\\|||\\)\\(\\s \\|$\\)" 1 'font-lock-keyword-face))
  "Font lock keyword highlighting list comprehension operators.")

(defvar erlang-font-lock-keywords-keywords
  (list
   (list erlang-keywords-regexp 1 'font-lock-keyword-face))
  "Font lock keyword highlighting Erlang keywords.")

(defvar erlang-font-lock-keywords-attr
  (list
   (list (concat "^\\(-" erlang-atom-regexp "\\)\\(\\s-\\|\\.\\|(\\)")
         1 (if (boundp 'font-lock-preprocessor-face)
               'font-lock-preprocessor-face
             'font-lock-constant-face)))
  "Font lock keyword highlighting attributes.")

(defvar erlang-font-lock-keywords-quotes
  (list
   (list "`\\([-+a-zA-Z0-9_:*][-+a-zA-Z0-9_:*]+\\)'"
         1
         'font-lock-keyword-face
         t))
  "Font lock keyword highlighting words in single quotes in comments.

This is not the highlighting of Erlang strings and atoms, which
are highlighted by syntactic analysis.")

(defvar erlang-font-lock-keywords-guards
  (list
   (list (concat "[^:]" erlang-guards-regexp "\\s-*(")
         1 'font-lock-builtin-face))
  "Font lock keyword highlighting guards.")

(defvar erlang-font-lock-keywords-predefined-types
  (list
   (list (concat "[^:]" erlang-predefined-types-regexp "\\s-*(")
         1 'font-lock-builtin-face))
  "Font lock keyword highlighting predefined types.")


(defvar erlang-font-lock-keywords-macros
  (list
   (list (concat "?\\s-*\\(" erlang-atom-regexp
                 "\\|" erlang-variable-regexp "\\)")
         1 'font-lock-constant-face)
   (list (concat "^\\(-\\(?:define\\|ifn?def\\)\\)\\s-*(\\s-*\\(" erlang-atom-regexp
                 "\\|" erlang-variable-regexp "\\)")
         (if (boundp 'font-lock-preprocessor-face)
             (list 1 'font-lock-preprocessor-face t)
           (list 1 'font-lock-constant-face t))
         (list 3 'font-lock-type-face t t))
   (list "^-e\\(lse\\|ndif\\)\\>" 0 'font-lock-preprocessor-face t))
  "Font lock keyword highlighting macros.
This must be placed in front of `erlang-font-lock-keywords-vars'.")

(defvar erlang-font-lock-keywords-records
  (list
   (list (concat "#\\s *" erlang-atom-regexp)
         1 'font-lock-type-face)
   ;; Don't highlight numerical constants.
   (list (if erlang-regexp-modern-p
             "\\_<[0-9]+#\\([0-9a-zA-Z]+\\)"
           "\\<[0-9]+#\\([0-9a-zA-Z]+\\)")
         1 nil t)
   (list (concat "^-record\\s-*(\\s-*" erlang-atom-regexp)
         1 'font-lock-type-face))
  "Font lock keyword highlighting Erlang records.
This must be placed in front of `erlang-font-lock-keywords-vars'.")

(defvar erlang-font-lock-keywords-vars
  (list
   (list (concat "[^#]" erlang-variable-regexp) ; no numerical constants
         1 'font-lock-variable-name-face))
  "Font lock keyword highlighting Erlang variables.
Must be preceded by `erlang-font-lock-keywords-macros' to work properly.")

(defvar erlang-font-lock-descr-string
  "Font-lock keywords used by Erlang Mode.

There exists three levels of Font Lock keywords for Erlang:
  `erlang-font-lock-keywords-1' - Function headers and reserved keywords.
  `erlang-font-lock-keywords-2' - Bifs, guards and `single quotes'.
  `erlang-font-lock-keywords-3' - Variables, macros and records.
  `erlang-font-lock-keywords-4' - Exported functions, Function names,
                                  Funs, LCs (not Atoms).

To use a specific level, please set the variable
`font-lock-maximum-decoration' to the appropriate level.  Note that the
variable must be set before Erlang mode is activated.

Example:
    (setq font-lock-maximum-decoration 2)")

(defvar erlang-font-lock-keywords-1
  (append erlang-font-lock-keywords-function-header
          erlang-font-lock-keywords-dollar
          erlang-font-lock-keywords-arrow
          erlang-font-lock-keywords-keywords
          )
  ;; DocStringOrig: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords-2
  (append erlang-font-lock-keywords-1
          erlang-font-lock-keywords-int-bifs
          erlang-font-lock-keywords-ext-bifs
          erlang-font-lock-keywords-attr
          erlang-font-lock-keywords-quotes
          erlang-font-lock-keywords-guards
          )
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords-3
  (append erlang-font-lock-keywords-2
          erlang-font-lock-keywords-operators
          erlang-font-lock-keywords-macros
          erlang-font-lock-keywords-records
          erlang-font-lock-keywords-vars
          erlang-font-lock-keywords-predefined-types
          )
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords-4
  (append erlang-font-lock-keywords-3
          erlang-font-lock-keywords-exported-function-header
          erlang-font-lock-keywords-int-function-calls
          erlang-font-lock-keywords-ext-function-calls
          erlang-font-lock-keywords-fun-n
          erlang-font-lock-keywords-lc
          )
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords erlang-font-lock-keywords-4
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-syntax-table nil
  "Syntax table used by Font Lock mode.

The difference between this and the standard Erlang Mode
syntax table is that `_' is treated as part of words by
this syntax table.")

(defvar erlang-replace-etags-tags-completion-table nil
  "Internal flag used by advice `erlang-replace-tags-table'.
This is non-nil when `etags-tags-completion-table' should be
replaced by `erlang-etags-tags-completion-table'.")


;;; Avoid errors while compiling this file.

;; defvar some obsolete variables, which we still support for
;; backwards compatibility reasons.
(eval-when-compile
  (defvar comment-indent-hook)
  (defvar dabbrev-case-fold-search)
  (defvar tempo-match-finder)
  (defvar compilation-menu-map)
  (defvar next-error-last-buffer))

(eval-when-compile
  (require 'comint)
  (require 'tempo)
  (require 'compile))


(defun erlang-version ()
  "Return the current version of Erlang mode."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "Erlang mode version %s, written by Anders Lindgren"
               erlang-version))
  erlang-version)

;;;###autoload
(define-derived-mode erlang-mode prog-mode "Erlang"
  "Major mode for editing Erlang source files in Emacs.
It knows about syntax and comment, it can indent code, it is capable
of fontifying the source file, the TAGS commands are aware of Erlang
modules, and the Erlang man pages can be accessed.

Should this module, \"erlang.el\", be installed properly, Erlang mode
is activated whenever an Erlang source or header file is loaded into
Emacs.  To indicate this, the mode line should contain the word
\"Erlang\".

The main feature of Erlang mode is indentation, press TAB and the
current line will be indented correctly.

Comments starting with only one `%' are indented to the column stored
in the variable `comment-column'.  Comments starting with two `%':s
are indented with the same indentation as code.  Comments starting
with at least three `%':s are indented to the first column.

However, Erlang mode contains much more, this is a list of the most
useful commands:
     TAB     - Indent the line.
     C-c C-q - Indent current function.
     M-;     - Create a comment at the end of the line.
     M-q     - Fill a comment, i.e. wrap lines so that they (hopefully)
                 will look better.
     M-a     - Goto the beginning of an Erlang clause.
     M-C-a   - Ditto for function.
     M-e     - Goto the end of an Erlang clause.
     M-C-e   - Ditto for function.
     M-h     - Mark current Erlang clause.
     M-C-h   - Ditto for function.
     C-c C-z - Start, or switch to, an inferior Erlang shell.
     C-c C-k - Compile current file.
     C-x `   - Next error.
     ,       - Electric comma.
     ;       - Electric semicolon.

Erlang mode check the name of the file against the module name when
saving, whenever a mismatch occurs Erlang mode offers to modify the
source.

The variable `erlang-electric-commands' controls the electric
commands.  To deactivate all of them, set it to nil.

There exists a large number of commands and variables in the Erlang
module.  Please press `M-x apropos RET erlang RET' to see a complete
list.  Press `C-h f name-of-function RET' and `C-h v name-of-variable
RET'to see the full description of functions and variables,
respectively.

On entry to this mode the contents of the hook `erlang-mode-hook' is
executed.

Please see the beginning of the file `erlang.el' for more information
and examples of hooks.

Other commands:
\\{erlang-mode-map}"
  ;; Use our own syntax table function
  :syntax-table nil
  (erlang-syntax-table-init)
  (erlang-electric-init)
  (erlang-menu-init)
  (erlang-mode-variables)
  (erlang-check-module-name-init)
  (erlang-man-init)
  (erlang-tags-init)
  (erlang-font-lock-init)
  (erlang-skel-init)
  (tempo-use-tag-list 'erlang-tempo-tags)
  (when (and (fboundp 'add-function) (fboundp 'erldoc-eldoc-function))
    (or eldoc-documentation-function
        (setq-local eldoc-documentation-function #'ignore))
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'erldoc-eldoc-function))
  (run-hooks 'erlang-mode-hook)
  (if (zerop (buffer-size))
      (run-hooks 'erlang-new-file-hook)))

;;;###autoload
(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript"
             "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app"))
  (add-to-list 'auto-mode-alist (cons r 'erlang-mode)))

(defun erlang-syntax-table-init ()
  (erlang-ensure-syntax-table-is-initialized)
  (set-syntax-table erlang-mode-syntax-table))

(defun erlang-ensure-syntax-table-is-initialized ()
  (unless erlang-mode-syntax-table
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?\n ">" table)
      (modify-syntax-entry ?\" "\"" table)
      (modify-syntax-entry ?# "." table)
      ;; (modify-syntax-entry ?$ "\\" table)   ;; Creates problems with indention afterwards
      ;; (modify-syntax-entry ?$ "'" table)    ;; Creates syntax highlighting and indention problems
      (modify-syntax-entry ?$ "/" table)    ;; Misses the corner case "string that ends with $"
      ;; we have to live with that for now..it is the best alternative
      ;; that can be worked around with "string that ends with \$"
      (modify-syntax-entry ?% "<" table)
      (modify-syntax-entry ?& "." table)
      (modify-syntax-entry ?\' "\"" table)
      (modify-syntax-entry ?* "." table)
      (modify-syntax-entry ?+ "." table)
      (modify-syntax-entry ?- "." table)
      (modify-syntax-entry ?/ "." table)
      (modify-syntax-entry ?: "." table)
      (modify-syntax-entry ?< "." table)
      (modify-syntax-entry ?= "." table)
      (modify-syntax-entry ?> "." table)
      (modify-syntax-entry ?\\ "\\" table)
      (modify-syntax-entry ?_ "_" table)
      (modify-syntax-entry ?| "." table)
      (modify-syntax-entry ?^ "'" table)

      ;; Pseudo bit-syntax: Latin1 double angle quotes as parens.
      ;;(modify-syntax-entry ?\253 "(?\273" table)
      ;;(modify-syntax-entry ?\273 ")?\253" table)

      (setq erlang-mode-syntax-table table))))



(defun erlang-electric-init ()
  ;; Set up electric character functions to work with
  ;; delsel/pending-del mode. Also, set up text properties for bit
  ;; syntax handling.
  (mapc #'(lambda (cmd)
            (put cmd 'delete-selection t)) ;for delsel (Emacs)
        '(erlang-electric-semicolon
          erlang-electric-comma
          erlang-electric-gt))

  (put 'bitsyntax-open-outer 'syntax-table '(4 . ?>))
  (put 'bitsyntax-open-outer 'rear-nonsticky '(category))
  (put 'bitsyntax-open-inner 'rear-nonsticky '(category))
  (put 'bitsyntax-close-inner 'rear-nonsticky '(category))
  (put 'bitsyntax-close-outer 'syntax-table '(5 . ?<))
  (put 'bitsyntax-close-outer 'rear-nonsticky '(category))
  (make-local-variable 'parse-sexp-lookup-properties)
  (setq parse-sexp-lookup-properties 't))


(defun erlang-mode-variables ()
  (or erlang-mode-abbrev-table
      (define-abbrev-table 'erlang-mode-abbrev-table ()))
  (setq local-abbrev-table erlang-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'defun-prompt-regexp)
  (setq defun-prompt-regexp erlang-defun-prompt-regexp)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+\\s *")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'erlang-indent-command)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'erlang-indent-region)
  (set (make-local-variable 'comment-indent-function) 'erlang-comment-indent)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'imenu-prev-index-position-function)
       'erlang-beginning-of-function)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'erlang-get-function-name-and-arity)
  (set (make-local-variable 'tempo-match-finder)
       "[^-a-zA-Z0-9_]\\([-a-zA-Z0-9_]*\\)\\=")
  (set (make-local-variable 'beginning-of-defun-function)
       'erlang-beginning-of-function)
  (set (make-local-variable 'end-of-defun-function) 'erlang-end-of-function)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'fill-paragraph-function) 'erlang-fill-paragraph)
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'outline-regexp) "[[:lower:]0-9_]+ *(.*) *-> *$")
  (set (make-local-variable 'outline-level) (lambda () 1))
  (set (make-local-variable 'add-log-current-defun-function)
       'erlang-current-defun))

(defun erlang-font-lock-init ()
  "Initialize Font Lock for Erlang mode."
  (or erlang-font-lock-syntax-table
      (setq erlang-font-lock-syntax-table
            (let ((table (copy-syntax-table erlang-mode-syntax-table)))
              (modify-syntax-entry ?_ "w" table)
              table)))
  (set (make-local-variable 'font-lock-syntax-table)
       erlang-font-lock-syntax-table)
  (set (make-local-variable (if (boundp 'syntax-begin-function)
                                'syntax-begin-function
                              'font-lock-beginning-of-syntax-function))
       'erlang-beginning-of-clause)
  (make-local-variable 'font-lock-keywords)
  (let ((level (cond ((boundp 'font-lock-maximum-decoration)
                      (symbol-value 'font-lock-maximum-decoration))
                     ((boundp 'font-lock-use-maximal-decoration)
                      (symbol-value 'font-lock-use-maximal-decoration))
                     (t nil))))
    (if (consp level)
        (setq level (cdr-safe (or (assq 'erlang-mode level)
                                  (assq t level)))))
    ;; `level' can here be:
    ;;      A number - The fontification level
    ;;      nil      - Use the default
    ;;      t        - Use maximum
    (cond ((eq level nil)
           (set 'font-lock-keywords erlang-font-lock-keywords))
          ((eq level 1)
           (set 'font-lock-keywords erlang-font-lock-keywords-1))
          ((eq level 2)
           (set 'font-lock-keywords erlang-font-lock-keywords-2))
          ((eq level 3)
           (set 'font-lock-keywords erlang-font-lock-keywords-3))
          (t
           (set 'font-lock-keywords erlang-font-lock-keywords-4))))

  ;; Modern font-locks can handle the above much more elegantly:
  (set (make-local-variable 'font-lock-defaults)
       '((erlang-font-lock-keywords erlang-font-lock-keywords-1
                                    erlang-font-lock-keywords-2
                                    erlang-font-lock-keywords-3
                                    erlang-font-lock-keywords-4)
         nil nil ((?_ . "w")) erlang-beginning-of-clause
         (font-lock-mark-block-function . erlang-mark-clause)
         (font-lock-syntactic-keywords
          ;; A dollar sign right before the double quote that ends a
          ;; string is not a character escape.
          ;;
          ;; And a "string" consists of a double quote not escaped by a
          ;; dollar sign, any number of non-backslash non-newline
          ;; characters or escaped backslashes, a dollar sign
          ;; (otherwise we wouldn't care) and a double quote.  This
          ;; doesn't match multi-line strings, but this is probably
          ;; the best we can get, since while font-locking we don't
          ;; know whether matching started inside a string: limiting
          ;; search to a single line keeps things sane.
          . (("\\(?:^\\|[^$]\\)\"\\(?:[^\"\n]\\|\\\\\"\\)*\\(\\$\\)\"" 1 "w")
             ;; Likewise for atoms
             ("\\(?:^\\|[^$]\\)'\\(?:[^'\n]\\|\\\\'\\)*\\(\\$\\)'" 1 "w")
             ;; And the dollar sign in $\" or $\' escapes two
             ;; characters, not just one.
             ("\\(\\$\\)\\\\[\"']" 1 "'"))))))



;; Useful when defining your own keywords.
(defun erlang-font-lock-set-face (ks &rest faces)
  "Replace the face components in a list of keywords.

The first argument, KS, is a list of keywords.  The rest of the
arguments are expressions to replace the face information with.  The
first expression replaces the face of the first keyword, the second
expression the second keyword etc.

Should an expression be nil, the face of the corresponding keyword is
not changed.

Should fewer expressions than keywords be given, the last expression
is used for all remaining keywords.

Normally, the expressions are just atoms representing the new face.
They could however be more complex, returning different faces in
different situations.

This function only handles keywords with elements on the forms:
  (REGEXP NUMBER FACE)
  (REGEXP NUMBER FACE OVERWRITE)

This could be used when defining your own special font-lock setup, e.g:

\(setq my-font-lock-keywords
      (append erlang-font-lock-keywords-function-header
              erlang-font-lock-keywords-dollar
              (erlang-font-lock-set-face
               erlang-font-lock-keywords-macros 'my-neon-green-face)
              (erlang-font-lock-set-face
               erlang-font-lock-keywords-lc 'my-deep-red 'my-light-red)
              erlang-font-lock-keywords-attr))

For a more elaborate example, please see the beginning of the file
`erlang.el'."
  (let ((res '()))
    (while ks
      (let* ((regexp (car (car ks)))
             (number (car (cdr (car ks))))
             (new-face (if (and faces (car faces))
                           (car faces)
                         (car (cdr (cdr (car ks))))))
             (overwrite (car (cdr (cdr (cdr (car ks))))))
             (new-keyword (list regexp number new-face)))
        (if overwrite (nconc new-keyword (list overwrite)))
        (setq res (cons new-keyword res))
        (setq ks (cdr ks))
        (if (and faces (cdr faces))
            (setq faces (cdr faces)))))
    (nreverse res)))


(defun erlang-font-lock-level-0 ()
  ;; DocStringOrig: font-cmd
  "Unfontify current buffer."
  (interactive)
  (font-lock-mode 0))


(defun erlang-font-lock-level-1 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 1.
This highlights function headers, reserved keywords, strings and comments."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-1)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))


(defun erlang-font-lock-level-2 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 2.
This highlights level 1 features (see `erlang-font-lock-level-1')
plus bifs, guards and `single quotes'."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-2)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))


(defun erlang-font-lock-level-3 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 3.
This highlights level 2 features (see `erlang-font-lock-level-2')
plus variables, macros and records."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-3)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))

(defun erlang-font-lock-level-4 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 4.
This highlights level 3 features (see `erlang-font-lock-level-2')
plus variables, macros and records."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-4)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))


(defun erlang-menu-init ()
  "Init menus for Erlang mode.

The variable `erlang-menu-items' contain a description of the Erlang
mode menu.  Normally, the list contains atoms, representing variables
bound to pieces of the menu.

Personal extensions could be added to `erlang-menu-personal-items'.

This function should be called if any variable describing the
menu configuration is changed."
  (erlang-menu-install "Erlang" erlang-menu-items erlang-mode-map t))


(defun erlang-menu-install (name items keymap &optional popup)
  "Install a menu in Emacs based on an abstract description.

NAME is the name of the menu.

ITEMS is a list.  The elements are either nil representing a horizontal
line or a list with two or three elements.  The first is the name of
the menu item, the second the function to call, or a submenu, on the
same same form as ITEMS.  The third optional element is an expression
which is evaluated every time the menu is displayed.  Should the
expression evaluate to nil the menu item is ghosted.

KEYMAP is the keymap to add to menu to.

Please see the variable `erlang-menu-base-items'."
  (define-key keymap (vector 'menu-bar (intern name))
    (erlang-menu-make-keymap name items)))


(defun erlang-menu-make-keymap (name items)
  "Build a menu."
  (let ((menumap (funcall (symbol-function 'make-sparse-keymap)
                          name))
        (count 0)
        id def first second third)
    (setq items (reverse items))
    (while items
      ;; Replace any occurrence of atoms by their value.
      (while (and items (atom (car items)) (not (null (car items))))
        (if (and (boundp (car items))
                 (listp (symbol-value (car items))))
            (setq items (append (reverse (symbol-value (car items)))
                                (cdr items)))
          (setq items (cdr items))))
      (setq first (car-safe (car items)))
      (setq second (car-safe (cdr-safe (car items))))
      (setq third (car-safe (cdr-safe (cdr-safe (car items)))))
      (cond ((null first)
             (setq count (+ count 1))
             (setq id (intern (format "separator-%d" count)))
             (setq def '("--" . nil)))
            ((and (consp second) (eq (car second) 'lambda))
             (setq count (+ count 1))
             (setq id (intern (format "lambda-%d" count)))
             (setq def (cons first second)))
            ((symbolp second)
             (setq id second)
             (setq def (cons first second)))
            (t
             (setq count (+ count 1))
             (setq id (intern (format "submenu-%d" count)))
             (setq def (erlang-menu-make-keymap first second))))
      (define-key menumap (vector id) def)
      (if third
          (put id 'menu-enable third))
      (setq items (cdr items)))
    (cons name menumap)))

(defun erlang-menu-substitute (items alist)
  "Substitute functions in menu described by ITEMS.

The menu ITEMS is updated destructively.

ALIST is list of pairs where the car is the old function and cdr the new."
  (let (first second pair)
    (while items
      (setq first (car-safe (car items)))
      (setq second (car-safe (cdr-safe (car items))))
      (cond ((null first))
            ((symbolp second)
             (setq pair (and second (assq second alist)))
             (if pair
                 (setcar (cdr (car items)) (cdr pair))))
            ((and (consp second) (eq (car second) 'lambda)))
            (t
             (erlang-menu-substitute second alist)))
      (setq items (cdr items)))))


(defun erlang-menu-add-above (entry above items)
  "Add menu ENTRY above menu entry ABOVE in menu ITEMS.
Do nothing if the items already should be in the menu.
Should ABOVE not be in the list, the entry is added at
the bottom of the menu.

The new menu is returned.  No guarantee is given that the original
menu is left unchanged.

The equality test is performed by `eq'.

Example:  (erlang-menu-add-above 'my-erlang-menu-items
                                 'erlang-menu-man-items)"
  (erlang-menu-add-below entry above items t))


(defun erlang-menu-add-below (entry below items &optional above-p)
  "Add menu ENTRY below menu items BELOW in the Erlang menu.
Do nothing if the items already should be in the menu.
Should BELOW not be in the list, items is added at the bottom
of the menu.

The new menu is returned.  No guarantee is given that the original
menu is left unchanged.

The equality test is performed by `eq'.

Example:

\(setq erlang-menu-items
      (erlang-menu-add-below 'my-erlang-menu-items
                             'erlang-menu-base-items
                             erlang-menu-items))"
  (if (memq entry items)
      items                             ; Return the original menu.
    (let ((head '())
          (done nil)
          res)
      (while (not done)
        (cond ((null items)
               (setq res (append head (list entry)))
               (setq done t))
              ((eq below (car items))
               (setq res
                     (if above-p
                         (append head (cons entry items))
                       (append head (cons (car items)
                                          (cons entry (cdr items))))))
               (setq done t))
              (t
               (setq head (append head (list (car items))))
               (setq items (cdr items)))))
      res)))

(defun erlang-menu-delete (entry items)
  "Delete ENTRY from menu ITEMS.

The new menu is returned.  No guarantee is given that the original
menu is left unchanged."
  (delq entry items))

;; Man code:

(defun erlang-man-init ()
  "Add menus containing the manual pages of the Erlang.

The variable `erlang-man-dirs' contains entries describing
the location of the manual pages."
  (interactive)
  (if (or erlang-man-inhibit
          (and (boundp 'menu-bar-mode)
               (not menu-bar-mode)))
      ()
    (setq erlang-menu-man-items
          '(nil
            ("Man - Function" erlang-man-function)))
    (if erlang-man-dirs
        (setq erlang-menu-man-items
              (append erlang-menu-man-items
                      (erlang-man-make-top-menu erlang-man-dirs))))
    (setq erlang-menu-items
          (erlang-menu-add-above 'erlang-menu-man-items
                                 'erlang-menu-version-items
                                 erlang-menu-items))
    (erlang-menu-init)))


(defun erlang-man-uninstall ()
  "Remove the man pages from the Erlang mode."
  (interactive)
  (setq erlang-menu-items
        (erlang-menu-delete 'erlang-menu-man-items erlang-menu-items))
  (erlang-menu-init))


;; The man menu is a hierarchal structure, with the manual sections
;; at the top, described by `erlang-man-dirs'.  The next level could
;; either be the manual pages if not to many, otherwise it is an index
;; menu whose submenus will contain up to `erlang-man-max-menu-size'
;; manual pages.

(defun erlang-man-make-top-menu (dir-list)
  "Create one menu entry per element of DIR-LIST.
The format is described in the documentation of `erlang-man-dirs'."
  (let ((menu '())
        dir)
    (while dir-list
      (setq dir (cond ((nth 2 (car dir-list))
                       ;; Relative to `erlang-root-dir'.
                       (and (stringp erlang-root-dir)
                            (erlang-man-dir (nth 1 (car dir-list)))))
                      (t
                       ;; Absolute
                       (nth 1 (car dir-list)))))
      (if (and dir
               (file-readable-p dir))
          (setq menu (cons (list (car (car dir-list))
                                 (erlang-man-make-middle-menu
                                  (erlang-man-get-files dir)))
                           menu)))
      (setq dir-list (cdr dir-list)))
    ;; Should no menus be found, generate a menu item which
    ;; will display a help text, when selected.
    (if menu
        (nreverse menu)
      '(("Man Pages"
         (("Error! Why?" erlang-man-describe-error)))))))

(defun erlang-man-dir (subdir)
  (concat erlang-root-dir "/lib/erlang/" subdir))

;; Should the menu be to long, let's split it into a number of
;; smaller menus.  Warning, this code contains beautiful
;; destructive operations!
(defun erlang-man-make-middle-menu (filelist)
  "Create the second level menu from FILELIST.

Should the list be longer than `erlang-man-max-menu-size', a tree of
menus is created."
  (if (<= (length filelist) erlang-man-max-menu-size)
      (erlang-man-make-menu filelist)
    (let ((menu '())
          (filelist (copy-sequence filelist))
          segment submenu pair)
      (while filelist
        (setq pair (nthcdr (- erlang-man-max-menu-size 1) filelist))
        (setq segment filelist)
        (if (null pair)
            (setq filelist nil)
          (setq filelist (cdr pair))
          (setcdr pair nil))
        (setq submenu (erlang-man-make-menu segment))
        (setq menu (cons (list (concat (car (car submenu))
                                       " -- "
                                       (car (car (reverse submenu))))
                               submenu)
                         menu)))
      (nreverse menu))))


(defun erlang-man-make-menu (filelist)
  "Make a leaf menu based on FILELIST."
  (let ((menu '())
        item)
    (while filelist
      (setq item (erlang-man-make-menu-item (car filelist)))
      (if item
          (setq menu (cons item menu)))
      (setq filelist (cdr filelist)))
    (nreverse menu)))


(defun erlang-man-make-menu-item (file)
  "Create a menu item containing the name of the man page."
  (and (string-match ".+/\\([^/]+\\)\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?$" file)
       (let ((page (substring file (match-beginning 1) (match-end 1))))
         (list (capitalize page)
               (list 'lambda '()
                     '(interactive)
                     (list 'funcall 'erlang-man-display-function
                           file))))))


(defun erlang-man-get-files (dir)
  "Return files in directory DIR."
  (directory-files dir t ".+\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?\\'"))


(defun erlang-man-module (&optional module)
  "Find manual page for MODULE, defaults to module of function under point.
This function is aware of imported functions."
  (interactive
   (list (let* ((mod (erlang-default-module))
                (input (read-string
                        (format "Manual entry for module%s: "
                                (if (or (null mod) (string= mod ""))
                                    ""
                                  (format " (default %s)" mod))))))
           (if (string= input "")
               mod
             input))))
  (setq module (or module
                   (erlang-default-module)))
  (when (or (null module) (string= module ""))
    (error "No Erlang module name given"))
  (let ((dir-list erlang-man-dirs)
        (pat (concat "/" (regexp-quote module)
                     "\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?$"))
        (file nil)
        file-list)
    (while (and dir-list (null file))
      (let ((dir (if (nth 2 (car dir-list))
                     (erlang-man-dir (nth 1 (car dir-list)))
                   (nth 1 (car dir-list)))))
        (when (file-directory-p dir)
          (setq file-list (erlang-man-get-files dir))
          (while (and file-list (null file))
            (if (string-match pat (car file-list))
                (setq file (car file-list)))
            (setq file-list (cdr file-list))))
        (setq dir-list (cdr dir-list))))
    (if file
        (funcall erlang-man-display-function file)
      ;; Did not found the manual file.  Fallback to manual-entry.
      (manual-entry module))))

(defun erlang-default-module ()
  (let ((id (erlang-get-identifier-at-point)))
    (if (eq (erlang-id-kind id) 'qualified-function)
        (erlang-id-module id)
      (erlang-id-name id))))


;; Warning, the function `erlang-man-function' is a hack!
;; It links itself into the man code in a non-clean way.  I have
;; chosen to keep it since it provides a very useful functionality
;; which is not possible to achieve using a clean approach.
;;   / AndersL

(defvar erlang-man-function-name nil
  "Name of function for last `erlang-man-function' call.
Used for communication between `erlang-man-function' and the
patch to `Man-notify-when-ready'.")

(defun erlang-man-function (&optional name)
  "Find manual page for NAME, where NAME is module:function.
The entry for `function' is displayed.

This function is aware of imported functions."
  (interactive
   (list (let* ((default (erlang-default-function-or-module))
                (input (read-string
                        (format
                         "Manual entry for `module:func' or `module'%s: "
                         (if default
                             (format " (default %s)" default)
                           "")))))
           (if (string= input "")
               default
             input))))
  (require 'man)
  (setq name (or name
                 (erlang-default-function-or-module)))
  (let ((modname nil)
        (funcname nil))
    (cond ((string-match ":" name)
           (setq modname (substring name 0 (match-beginning 0)))
           (setq funcname (substring name (match-end 0) nil)))
          ((stringp name)
           (setq modname name)))
    (when (or (null modname) (string= modname ""))
      (error "No Erlang module name given"))
    (cond ((fboundp 'Man-notify-when-ready)
           ;; Emacs 19:  The man command could possibly start an
           ;; asynchronous process, i.e. we must hook ourselves into
           ;; the system to be activated when the man-process
           ;; terminates.
           (if (null funcname)
               ()
             (erlang-man-patch-notify)
             (setq erlang-man-function-name funcname))
           (condition-case err
               (erlang-man-module modname)
             (error (setq erlang-man-function-name nil)
                    (signal (car err) (cdr err)))))
          (t
           (erlang-man-module modname)
           (when funcname
             (erlang-man-find-function (current-buffer) funcname))))))


;; Should the defadvice be at the top level, the package `advice' would
;; be required.  Now it is only required when this functionality
;; is used.  (Emacs 19 specific.)
(defun erlang-man-patch-notify ()
  "Patch the function `Man-notify-when-ready' to search for function.
The variable `erlang-man-function-name' is assumed to be bound to
the function name, or to nil.

The reason for patching a function is that under Emacs 19, the man
command is executed asynchronously."
  (condition-case nil
      (require 'advice)
    ;; This should never happened since this is only called when
    ;; running under Emacs 19.
    (error (error (concat "This command needs the package `advice', "
                          "please upgrade your Emacs."))))
  (require 'man)
  (defadvice Man-notify-when-ready
      (after erlang-Man-notify-when-ready activate)
    "Set point at the documentation of the function name in
`erlang-man-function-name' when the man page is displayed."
    (if erlang-man-function-name
        (erlang-man-find-function (ad-get-arg 0) erlang-man-function-name))
    (setq erlang-man-function-name nil)))


(defun erlang-man-find-function (buf func)
  "Find manual page for function in `erlang-man-function-name' in buffer BUF."
  (if func
      (let ((win (get-buffer-window buf)))
        (if win
            (progn
              (set-buffer buf)
              (goto-char (point-min))
              (if (re-search-forward
                   (concat "^[ \t]+" func " ?(")
                   (point-max) t)
                  (progn
                    (forward-word -1)
                    (set-window-point win (point)))
                (message "Could not find function `%s'" func)))))))

(defvar erlang-man-file-regexp
  "\\(.*\\)/man[^/]*/\\([^.]+\\)\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?$")

(defun erlang-man-display (file)
  "Display FILE as a `man' file.
This is the default manual page display function.
The variables `erlang-man-display-function' contains the function
to be used."
  (require 'man)
  (if file
      (let ((process-environment (copy-sequence process-environment)))
        (if (string-match erlang-man-file-regexp file)
            (let ((dir (substring file (match-beginning 1) (match-end 1)))
                  (page (substring file (match-beginning 2) (match-end 2))))
              (setenv "MANPATH" dir)
              (manual-entry page))
          (error "Can't find man page for %s\n" file)))))


(defun erlang-man-describe-error ()
  "Describe why the manual pages weren't found."
  (interactive)
  (with-output-to-temp-buffer "*Erlang Man Error*"
    (princ "Normally, this menu should contain Erlang manual pages.

In order to find the manual pages, the variable `erlang-root-dir'
should be bound to the name of the directory containing the Erlang
installation.  The name should not include the final slash.

Practically, you should add a line on the following form to
your ~/.emacs, or ask your system administrator to add it to
the site init file:
    (setq erlang-root-dir \"/the/erlang/root/dir/goes/here\")

For example:
    (setq erlang-root-dir \"/usr/local/erlang\")

After installing the line, kill and restart Emacs, or restart Erlang
mode with the command `M-x erlang-mode RET'.")))

;; Skeleton code:

;; This code is based on the package `tempo' which is part of modern
;; Emacsen.

(defvar erlang-skel)
(defun erlang-skel-init ()
  "Generate the skeleton functions and menu items.
The variable `erlang-skel' contains the name and descriptions of
all skeletons.

The skeleton routines are based on the `tempo' package.  Should this
package not be present, this function does nothing."
  (interactive)
  (require 'tempo)
  (let ((skel erlang-skel)
        (menu '()))
    (while skel
      (cond ((null (car skel))
             (setq menu (cons nil menu)))
            (t
             (funcall (symbol-function 'tempo-define-template)
                      (concat "erlang-" (nth 1 (car skel)))
                      ;; The tempo template used contains an `include'
                      ;; function call only, hence changes to the
                      ;; variables describing the templates take effect
                      ;; immediately.
                      (list (list 'erlang-skel-include (nth 2 (car skel))))
                      (nth 1 (car skel))
                      (car (car skel))
                      'erlang-tempo-tags)
             (setq menu (cons (erlang-skel-make-menu-item
                               (car skel)) menu))))
      (setq skel (cdr skel)))
    (setq erlang-menu-skel-items
          (list nil (list "Skeletons" (nreverse menu))))
    (setq erlang-menu-items
          (erlang-menu-add-above 'erlang-menu-skel-items
                                 'erlang-menu-version-items
                                 erlang-menu-items))
    (erlang-menu-init)))

(defun erlang-skel-make-menu-item (skel)
  (let ((func (intern (concat "tempo-template-erlang-" (nth 1 skel)))))
    (cond ((null (nth 3 skel))
           (list (car skel) func))
          (t
           (list (car skel)
                 (list 'lambda '()
                       '(interactive)
                       (list 'funcall
                             (list 'quote (nth 3 skel))
                             (list 'quote func))))))))

;; Functions designed to be added to the skeleton menu.
;; (Not normally used)
(defun erlang-skel-insert (func)
  "Insert skeleton generated by FUNC and goto first tempo mark."
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))

(defun erlang-skel-header (func)
  "Insert the header generated by FUNC at the beginning of the buffer."
  (goto-char (point-min))
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))


;; Functions used inside the skeleton descriptions.
(defun erlang-skel-skip-blank ()
  (skip-chars-backward " \t")
  nil)

(defun erlang-skel-include (&rest args)
  "Include a template inside another template.

Example of use, assuming that `erlang-skel-func' is defined:

 (defvar foo-skeleton '(\"%%% New function:\"
                        (erlang-skel-include erlang-skel-func)))

Technically, this function returns the `tempo' attribute`(l ...)' which
can contain other `tempo' attributes.  Please see the function
`tempo-define-template' for a description of the `(l ...)' attribute."
  (let ((res '())
        entry)
    (while args
      (setq entry (car args))
      (while entry
        (setq res (cons (car entry) res))
        (setq entry (cdr entry)))
      (setq args (cdr args)))
    (cons 'l (nreverse res))))

(defvar erlang-skel-separator-length 70)

(defun erlang-skel-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%)
            (make-string (- erlang-skel-separator-length percent) ?-)
            "\n")))

(defun erlang-skel-double-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%)
            (make-string (- erlang-skel-separator-length percent) ?=)
            "\n")))

(defun erlang-skel-dd-mmm-yyyy ()
  "Return the current date as a string in \"DD Mon YYYY\" form.
The first character of DD is space if the value is less than 10."
  (let ((date (current-time-string)))
    (format "%2d %s %s"
            (string-to-number (substring date 8 10))
            (substring date 4 7)
            (substring date -4))))

;; Indentation code:

(defun erlang-indent-command (&optional whole-exp)
  "Indent current line as Erlang code.
With argument, indent any additional lines of the same clause
rigidly along with this one."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as Erlang
      ;; and shift remaining lines of clause the same amount.
      (let ((shift-amt (erlang-indent-line))
            beg end)
        (save-excursion
          (if erlang-tab-always-indent
              (beginning-of-line))
          (setq beg (point))
          (erlang-end-of-clause 1)
          (setq end (point))
          (goto-char beg)
          (forward-line 1)
          (setq beg (point)))
        (if (> end beg)
            (indent-code-rigidly beg end shift-amt "\n")))
    (if (and (not erlang-tab-always-indent)
             (save-excursion
               (skip-chars-backward " \t")
               (not (bolp))))
        (insert-tab)
      (erlang-indent-line))))


(defun erlang-indent-line ()
  "Indent current line as Erlang code.
Return the amount the indentation changed by."
  (let ((pos (- (point-max) (point)))
        indent beg
        shift-amt)
    (beginning-of-line 1)
    (setq beg (point))
    (skip-chars-forward " \t")
    (cond ((looking-at "%")
           (setq indent (funcall comment-indent-function))
           (setq shift-amt (- indent (current-column))))
          (t
           (setq indent (erlang-calculate-indent))
           (cond ((null indent)
                  (setq indent (current-indentation)))
                 ((eq indent t)
                  ;; This should never occur here.
                  (error "Erlang mode error"))
                 ;;((= (char-syntax (following-char)) ?\))
                 ;; (setq indent (1- indent)))
                 )
           (setq shift-amt (- indent (current-column)))))
    (if (zerop shift-amt)
        nil
      (delete-region beg (point))
      (indent-to indent))
    ;; If initial point was within line's indentation, position
    ;; after the indentation. Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))
    (run-hooks 'erlang-indent-line-hook)
    shift-amt))


(defun erlang-indent-region (beg end)
  "Indent region of Erlang code.

This is automagically called by the user level function `indent-region'."
  (interactive "r")
  (save-excursion
    (let ((case-fold-search nil)
          (continue t)
          (from-end (- (point-max) end))
          indent-point;; The beginning of the current line
          indent;; The indent amount
          state)
      (goto-char beg)
      (beginning-of-line)
      (setq indent-point (point))
      (erlang-beginning-of-clause)
      ;; Parse the Erlang code from the beginning of the clause to
      ;; the beginning of the region.
      (while (< (point) indent-point)
        (let ((pt (point)))
          (setq state (erlang-partial-parse pt indent-point state))
          (if (= pt (point))
              (error "Illegal syntax"))))
      ;; Indent every line in the region
      (while continue
        (goto-char indent-point)
        (skip-chars-forward " \t")
        (cond ((looking-at "%")
               ;; Do not use our stack to help the user to customize
               ;; comment indentation.
               (setq indent (funcall comment-indent-function)))
              ((looking-at "$")
               ;; Don't indent empty lines.
               (setq indent 0))
              (t
               (setq indent
                     (save-excursion
                       (erlang-calculate-stack-indent (point) state)))
               (cond ((null indent)
                      (setq indent (current-indentation)))
                     ((eq indent t)
                      ;; This should never occur here.
                      (error "Erlang mode error"))
                     ;;((= (char-syntax (following-char)) ?\))
                     ;; (setq indent (1- indent)))
                     )))
        (if (zerop (- indent (current-column)))
            nil
          (delete-region indent-point (point))
          (indent-to indent))
        ;; Find the next line in the region
        (goto-char indent-point)
        (save-excursion
          (forward-line 1)
          (setq indent-point (point)))
        (if (>= from-end (- (point-max) indent-point))
            (setq continue nil)
          (while (< (point) indent-point)
            (let ((pt (point)))
              (setq state (erlang-partial-parse
                           pt indent-point state))
              (if (= pt (point))
                  (error "Illegal syntax")))))))))


(defun erlang-indent-current-buffer ()
  "Indent current buffer as Erlang code."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (erlang-indent-region (point-min) (point-max)))))


(defun erlang-indent-function ()
  "Indent current Erlang function."
  (interactive)
  (save-excursion
    (let ((end (progn (erlang-end-of-function 1) (point)))
          (beg (progn (erlang-beginning-of-function 1) (point))))
      (erlang-indent-region beg end))))


(defun erlang-indent-clause ()
  "Indent current Erlang clause."
  (interactive)
  (save-excursion
    (let ((end (progn (erlang-end-of-clause 1) (point)))
          (beg (progn (erlang-beginning-of-clause 1) (point))))
      (erlang-indent-region beg end))))


(defmacro erlang-push (x stack) (list 'setq stack (list 'cons x stack)))
(defmacro erlang-pop (stack) (list 'setq stack (list 'cdr stack)))
;; Would much prefer to make caddr a macro but this clashes.
(defun erlang-caddr (x) (car (cdr (cdr x))))


(defun erlang-calculate-indent (&optional parse-start)
  "Compute appropriate indentation for current line as Erlang code.
Return nil if line starts inside string, t if in a comment."
  (save-excursion
    (let ((indent-point (point))
          (case-fold-search nil)
          (state nil))
      (if parse-start
          (goto-char parse-start)
        (erlang-beginning-of-clause))
      (while (< (point) indent-point)
        (let ((pt (point)))
          (setq state (erlang-partial-parse pt indent-point state))
          (if (= pt (point))
              (error "Illegal syntax"))))
      (erlang-calculate-stack-indent indent-point state))))

(defun erlang-show-syntactic-information ()
  "Show syntactic information for current line."

  (interactive)

  (save-excursion
    (let ((starting-point (point))
          (case-fold-search nil)
          (state nil))
      (erlang-beginning-of-clause)
      (while (< (point) starting-point)
        (setq state (erlang-partial-parse (point) starting-point state)))
      (message "%S" state))))


(defun erlang-partial-parse (from to &optional state)
  "Parse Erlang syntax starting at FROM until TO, with an optional STATE.
Value is list (stack token-start token-type in-what)."
  (goto-char from)                      ; Start at the beginning
  (erlang-skip-blank to)
  (let ((cs (char-syntax (following-char)))
        (stack (car state))
        (token (point))
        in-what)
    (cond

     ;; Done: Return previous state.
     ((>= token to)
      (setq token (nth 1 state))
      (setq cs (nth 2 state))
      (setq in-what (nth 3 state)))

     ;; Word constituent: check and handle keywords.
     ((= cs ?w)
      (cond ((looking-at "\\(end\\|after\\)[^_a-zA-Z0-9]")
             ;; Must pop top icr layer, `after' will push a new
             ;; layer next.
             (progn
               (while (and stack (eq (car (car stack)) '->))
                 (erlang-pop stack))
               (if (and stack (memq (car (car stack)) '(icr begin fun try)))
                   (erlang-pop stack))))
            ((looking-at "catch\\b.*of")
             t)
            ((looking-at "catch\\b\\s *\\($\\|%\\|.*->\\)")
             ;; Must pop top icr layer, `catch' in try/catch
             ;;will push a new layer next.
             (progn
               (while (and stack (eq (car (car stack)) '->))
                 (erlang-pop stack))
               (if (and stack (memq (car (car stack)) '(icr begin try)))
                   (erlang-pop stack))))
            )
      (cond ((looking-at "\\(if\\|case\\|receive\\)[^_a-zA-Z0-9]")
             ;; Must push a new icr (if/case/receive) layer.
             (erlang-push (list 'icr token (current-column)) stack))
            ((looking-at "\\(try\\|after\\)[^_a-zA-Z0-9]")
             ;; Must handle separately, try catch or try X of -> catch
             ;; same for `after', it could be
             ;; receive after Time -> X end, or
             ;; try after X end
             (erlang-push (list 'try token (current-column)) stack))
            ((looking-at "\\(of\\)[^_a-zA-Z0-9]")
             ;; Must handle separately, try X of -> catch
             (if (and stack (eq (car (car stack)) 'try))
                 (let ((try-column (nth 2 (car stack)))
                       (try-pos (nth 1 (car stack))))
                   (erlang-pop stack)
                   (erlang-push (list 'icr try-pos try-column) stack))))

            ((looking-at "\\(fun\\)[^_a-zA-Z0-9]")
             ;; Push a new layer if we are defining a `fun'
             ;; expression, not when we are refering an existing
             ;; function.  'fun's defines are only indented one level now.
             (if (save-excursion
                   (goto-char (match-end 1))
                   (erlang-skip-blank to)
                   ;; Use erlang-variable-regexp here to look for an
                   ;; optional variable name to match EEP37 named funs.
                   (if (looking-at erlang-variable-regexp)
                       (progn
                         (goto-char (match-end 0))
                         (erlang-skip-blank to)))
                   (eq (following-char) ?\())
                 (erlang-push (list 'fun token (current-column)) stack)))
            ((looking-at "\\(begin\\)[^_a-zA-Z0-9]")
             (erlang-push (list 'begin token (current-column)) stack))
            ;; Normal when case
            ;;((looking-at "when\\s ")
            ;;((looking-at "when\\s *\\($\\|%\\)")
            ((looking-at "when[^_a-zA-Z0-9]")
             (erlang-push (list 'when token (current-column)) stack))
            ((looking-at "catch\\b.*of")
             t)
            ((looking-at "catch\\b\\s *\\($\\|%\\|.*->\\)")
             (erlang-push (list 'icr token (current-column)) stack))
            ;;(erlang-push (list '-> token (current-column)) stack))
            ;;((looking-at "^of$")
            ;; (erlang-push (list 'icr token (current-column)) stack)
            ;;(erlang-push (list '-> token (current-column)) stack))
            )
      (forward-sexp 1))
     ;; String: Try to skip over it. (Catch error if not complete.)
     ((= cs ?\")
      (condition-case nil
          (progn
            (forward-sexp 1)
            (if (> (point) to)
                (progn
                  (setq in-what 'string)
                  (goto-char to))))
        (error
         (setq in-what 'string)
         (goto-char to))))

     ;; Expression prefix e.i. $ or ^ (Note ^ can be in the character
     ;; literal $^ or part of string and $ outside of a string denotes
     ;; a character literal)
     ((= cs ?')
      (cond
       ((= (following-char) ?\") ;; $ or ^ was the last char in a string
        (forward-char 1))
       (t
        ;; Maybe a character literal, quote the next char to avoid
        ;; situations as $" being seen as the begining of a string.
        ;; Note the quoting something in the middle of a string is harmless.
        (quote (following-char))
        (forward-char 1))))

     ;; Symbol constituent or punctuation

     ((memq cs '(?. ?_))
      (cond

       ;; Clause end
       ((= (following-char) ?\;)
        (if (eq (car (car (last stack))) 'spec)
            (while (memq (car (car stack)) '(when ::))
              (erlang-pop stack)))
        (if (and stack (eq (car (car stack)) '->))
            (erlang-pop stack))
        (forward-char 1))

       ;; Parameter separator
       ((looking-at ",")
        (forward-char 1)
        (if (and stack (eq (car (car stack)) '::))
            ;; Type or spec
            (erlang-pop stack)))

       ;; Function end
       ((looking-at "\\.\\(\\s \\|\n\\|\\s<\\)")
        (setq stack nil)
        (forward-char 1))

       ;; Function head
       ((looking-at "->")
        (if (and stack (eq (car (car stack)) 'when))
            (erlang-pop stack))
        (erlang-push (list '-> token (current-column)) stack)
        (forward-char 2))

       ;; List-comprehension divider
       ((looking-at "||")
        (erlang-push (list '|| token (current-column)) stack)
        (forward-char 2))

       ;; Bit-syntax open. Note that map syntax allows "<<" to follow ":="
       ;; or "=>" without intervening whitespace, so handle that case here
       ((looking-at "\\(:=\\|=>\\)?<<")
        (erlang-push (list '<< token (current-column)) stack)
        (forward-char (- (match-end 0) (match-beginning 0))))

       ;; Bit-syntax close
       ((looking-at ">>")
        (while (memq (car (car stack)) '(|| ->))
          (erlang-pop stack))
        (cond ((eq (car (car stack)) '<<)
               (erlang-pop stack))
              ((memq (car (car stack)) '(icr begin fun))
               (error "Missing `end'"))
              (t
               (error "Unbalanced parentheses")))
        (forward-char 2))

       ;; Macro
       ((= (following-char) ??)
        ;; Skip over the ?
        (forward-char 1)
        )

       ;; Type spec's
       ((looking-at "-type\\s \\|-opaque\\s ")
        (if stack
            (forward-char 1)
          (erlang-push (list 'icr token (current-column)) stack)
          (forward-char 6)))
       ((looking-at "-spec\\s ")
        (if stack
            (forward-char 1)
          (forward-char 6)
          (skip-chars-forward "^(\n")
          (erlang-push (list 'spec (point) (current-column)) stack)
          ))

       ;; Type spec delimiter
       ((looking-at "::")
        (erlang-push (list ':: token (current-column)) stack)
        (forward-char 2))

       ;; Don't follow through in the clause below
       ;; '|' don't need spaces around it
       ((looking-at "|")
        (forward-char 1))

       ;; Other punctuation: Skip over it and any following punctuation
       ((= cs ?.)
        ;; Skip over all characters in the operand.
        (skip-syntax-forward "."))

       ;; Other char: Skip over it.
       (t
        (forward-char 1))))

     ;; Open parenthesis
     ((= cs ?\()
      (erlang-push (list '\( token (current-column)) stack)
      (forward-char 1))

     ;; Close parenthesis
     ((= cs ?\))
      (while (memq (car (car stack)) '(|| -> :: when))
        (erlang-pop stack))
      (cond ((eq (car (car stack)) '\()
             (erlang-pop stack)
             (if (and (eq (car (car stack)) 'fun)
                      (or (eq (car (car (last stack))) 'spec)
                          (eq (car (car (cdr stack))) '::))) ;; -type()
                 ;; Inside fun type def ') closes fun definition
                 (erlang-pop stack)))
            ((eq (car (car stack)) 'icr)
             (erlang-pop stack)
             ;; Normal catch not try-catch might have caused icr
             ;; and then incr should be removed and is not an error.
             (if (eq (car (car stack)) '\()
                 (erlang-pop stack)
               (error "Missing `end'")
               ))
            ((eq (car (car stack)) 'begin)
             (error "Missing `end'"))
            (t
             (error "Unbalanced parenthesis"))
            )
      (forward-char 1))

     ;; Character quote: Skip it and the quoted char.
     ((= cs ?/)
      (forward-char 2))

     ;; Character escape: Skip it and the escape sequence.
     ((= cs ?\\)
      (forward-char 1)
      (skip-syntax-forward "w"))

     ;; Everything else
     (t
      (forward-char 1)))
    (list stack token cs in-what)))

(defun erlang-calculate-stack-indent (indent-point state)
  "From the given last position and state (stack) calculate indentation.
Return nil if inside string, t if in a comment."
  (let* ((stack (and state (car state)))
         (token (nth 1 state))
         (stack-top (and stack (car stack))))
    (cond ((null state)                 ;No state
           0)
          ((nth 3 state)
           ;; Return nil or t.
           (eq (nth 3 state) 'comment))
          ((null stack)
           (if (looking-at "when[^_a-zA-Z0-9]")
               erlang-indent-guard
             0))
          ((eq (car stack-top) '\()
           ;; Element of list, tuple or part of an expression,
           (cond ((null erlang-argument-indent)
                  ;; indent to next column.
                  (1+ (nth 2 stack-top)))
                 ((= (char-syntax (following-char)) ?\))
                  (goto-char (nth 1 stack-top))
                  (cond ((erlang-record-or-function-args-p)
                         ;; Line ends with parenthesis.
                         (let ((previous (erlang-indent-find-preceding-expr))
                               (stack-pos (nth 2 stack-top)))
                           (if (>= previous stack-pos) stack-pos
                             (- (+ previous erlang-argument-indent) 1))))
                        (t
                         (nth 2 stack-top))))
                 ((= (following-char) ?,)
                  ;; a comma at the start of the line: line up with opening parenthesis.
                  (min (nth 2 stack-top)
                       (erlang-indent-element stack-top indent-point token)))
                 (t
                  (erlang-indent-element stack-top indent-point token))))
          ;;
          ((eq (car stack-top) '<<)
           ;; Element of binary (possible comprehension) expression,
           (cond ((null erlang-argument-indent)
                  ;; indent to next column.
                  (+ 2 (nth 2 stack-top)))
                 ((looking-at "\\(>>\\)[^_a-zA-Z0-9]")
                  (nth 2 stack-top))
                 ((= (following-char) ?,)
                  (min (+ (nth 2 stack-top) 1)
                       (- (erlang-indent-to-first-element stack-top 2) 1)))
                 (t
                  (erlang-indent-to-first-element stack-top 2))))

          ((memq (car stack-top) '(icr fun spec))
           ;; The default indentation is the column of the option
           ;; directly following the keyword. (This does not apply to
           ;; `case'.)  Should no option be on the same line, the
           ;; indentation is the indentation of the keyword +
           ;; `erlang-indent-level'.
           ;;
           ;; `after' should be indented to the same level as the
           ;; corresponding receive.
           (cond ((looking-at "\\(after\\|of\\)\\($\\|[^_a-zA-Z0-9]\\)")
                  (nth 2 stack-top))
                 ((looking-at "when[^_a-zA-Z0-9]")
                  ;; Handling one when part
                  (+ (nth 2 stack-top) erlang-indent-level erlang-indent-guard))
                 (t
                  (save-excursion
                    (goto-char (nth 1 stack-top))
                    (if (and erlang-icr-indent
                             (looking-at "\\(if\\|case\\|receive\\)[^_a-zA-Z0-9]"))
                        (+ (nth 2 stack-top) erlang-icr-indent)
                      (if (looking-at "\\(case\\|receive\\)[^_a-zA-Z0-9]")
                          (+ (nth 2 stack-top) erlang-indent-level)
                        (skip-chars-forward "a-z")
                        (skip-chars-forward " \t")
                        (if (memq (following-char) '(?% ?\n))
                            (+ (nth 2 stack-top) erlang-indent-level)
                          (current-column))))))))
          ((and (eq (car stack-top) '||) (looking-at "\\(]\\|>>\\)[^_a-zA-Z0-9]"))
           (nth 2 (car (cdr stack))))
          ;; Real indentation, where operators create extra indentation etc.
          ((memq (car stack-top) '(-> || try begin))
           (if (looking-at "\\(of\\)[^_a-zA-Z0-9]")
               (nth 2 stack-top)
             (goto-char (nth 1 stack-top))
             ;; Check if there is more code after the '->' on the
             ;; same line. If so use this indentation as base, else
             ;; use parent indentation + 2 * level as base.
             (let ((off erlang-indent-level)
                   (skip 2))
               (cond ((null (cdr stack))) ; Top level in function.
                     ((eq (car stack-top) 'begin)
                      (setq skip 5))
                     ((eq (car stack-top) 'try)
                      (setq skip 5))
                     ((eq (car stack-top) '->)
                      ;; If in fun definition use standard indent level not double
                      ;;(if (not (eq (car (car (cdr stack))) 'fun))
                      ;; Removed it made multi clause fun's look too bad
                      (setq off (+ erlang-indent-level (if (not erlang-icr-indent)
                                                           erlang-indent-level
                                                         erlang-icr-indent)))))
               (let ((base (erlang-indent-find-base stack indent-point off skip)))
                 ;; Special cases
                 (goto-char indent-point)
                 (cond ((looking-at "\\(;\\|end\\|after\\)\\($\\|[^_a-zA-Z0-9]\\)")
                        (if (eq (car stack-top) '->)
                            (erlang-pop stack))
                        (cond ((and stack (looking-at ";"))
                               (+ (erlang-caddr (car stack)) (- erlang-indent-level 2)))
                              (stack (erlang-caddr (car stack)))
                              (t off)))
                       ((looking-at "catch\\b\\($\\|[^_a-zA-Z0-9]\\)")
                        ;; Are we in a try
                        (let ((start (if (eq (car stack-top) '->)
                                         (car (cdr stack))
                                       stack-top)))
                          (if (null start) nil
                            (goto-char (nth 1 start)))
                          (cond ((looking-at "try\\($\\|[^_a-zA-Z0-9]\\)")
                                 (progn
                                   (if (eq (car stack-top) '->)
                                       (erlang-pop stack))
                                   (if stack
                                       (erlang-caddr (car stack))
                                     0)))
                                (t (erlang-indent-standard indent-point token base 'nil))))) ;; old catch
                       (t
                        (erlang-indent-standard indent-point token base 'nil)
                        ))))
             ))
          ((eq (car stack-top) 'when)
           (goto-char (nth 1 stack-top))
           (if (looking-at "when\\s *\\($\\|%\\)")
               (progn
                 (erlang-pop stack)
                 (if (and stack (memq (nth 0 (car stack)) '(icr fun)))
                     (progn
                       (goto-char (nth 1 (car stack)))
                       (+ (nth 2 (car stack)) erlang-indent-guard
                          ;; receive XYZ    or    receive
                          ;;                          XYZ
                          ;; This if thing does not seem to be needed
                          ;;(if (looking-at "[a-z]+\\s *\\($\\|%\\)")
                          ;;    erlang-indent-level
                          ;;  (* 2 erlang-indent-level))))
                          (* 2 erlang-indent-level)))
                   ;;erlang-indent-level))
                   (+ erlang-indent-level erlang-indent-guard)))
             ;; "when" is followed by code, let's indent to the same
             ;; column.
             (forward-char 4)           ; Skip "when"
             (skip-chars-forward " \t")
             (current-column)))
          ;; Type and Spec indentation
          ((eq (car stack-top) '::)
           (if (looking-at "[},)]")
               ;; Closing function spec, record definition with types,
               ;; or a comma at the start of the line
               ;; pop stack and recurse
               (erlang-calculate-stack-indent indent-point
                                              (cons (erlang-pop stack) (cdr state)))
             (cond ((null erlang-argument-indent)
                    ;; indent to next column.
                    (+ 2 (nth 2 stack-top)))
                   ((looking-at "::[^_a-zA-Z0-9]")
                    (nth 2 stack-top))
                   (t
                    (let ((start-alternativ (if (looking-at "|") 2 0)))
                      (goto-char (nth 1 stack-top))
                      (- (cond ((looking-at "::\\s *\\($\\|%\\)")
                                ;; Line ends with ::
                                (if (eq (car (car (last stack))) 'spec)
                                    (+ (erlang-indent-find-preceding-expr 1)
                                       erlang-argument-indent)
                                  (+ (erlang-indent-find-preceding-expr 2)
                                     erlang-argument-indent)))
                               (t
                                ;; Indent to the same column as the first
                                ;; argument.
                                (goto-char (+ 2 (nth 1 stack-top)))
                                (skip-chars-forward " \t")
                                (current-column))) start-alternativ))))))
          )))

(defun erlang-indent-to-first-element (stack-top extra)
  ;; Indent to the same column as the first
  ;; argument.  extra should be 1 for lists tuples or 2 for binaries
  (goto-char (+ (nth 1 stack-top) extra))
  (skip-chars-forward " \t")
  (current-column))

(defun erlang-indent-element (stack-top indent-point token)
  (goto-char (nth 1 stack-top))
  (let ((base (cond ((erlang-record-or-function-args-p)
                     ;; Line ends with parenthesis.
                     (erlang-indent-parenthesis (nth 2 stack-top)))
                    (t
                     (erlang-indent-to-first-element stack-top 1)))))
    (erlang-indent-standard indent-point token base 't)))

(defun erlang-indent-standard (indent-point token base inside-parenthesis)
  "Standard indent when in blocks or tuple or arguments.
   Look at last thing to see in what state we are, move relative to the base."
  (goto-char token)
  (cond ((looking-at "||\\|,\\|->\\||")
         base)
        ((erlang-at-keyword)
         (+ (current-column) erlang-indent-level))
        ((or (= (char-syntax (following-char)) ?.)
             (erlang-at-operator))
         (+ base erlang-indent-level))
        (t
         (goto-char indent-point)
         (cond ((memq (following-char) '(?\( ))
                ;; Function application.
                (+ (erlang-indent-find-preceding-expr)
                   erlang-argument-indent))
               ;; Empty line, or end; treat it as the end of
               ;; the block.  (Here we have a choice: should
               ;; the user be forced to reindent continued
               ;; lines, or should the "end" be reindented?)

               ;; Avoid treating comments a continued line.
               ((= (following-char) ?%)
                base)
               ((and (= (following-char) ?,) inside-parenthesis)
                ;; a comma at the start of the line line up with parenthesis
                (- base 1))
               ;; Continued line (e.g. line beginning
               ;; with an operator.)
               (t
                (if (or (erlang-at-operator) (not inside-parenthesis))
                    (+ base erlang-indent-level)
                  base))))))

(defun erlang-indent-find-base (stack indent-point &optional offset skip)
  "Find the base column for current stack."
  (or skip (setq skip 2))
  (or offset (setq offset erlang-indent-level))
  (save-excursion
    (let* ((stack-top (car stack)))
      (goto-char (nth 1 stack-top))
      (if (< skip (- (point-max) (point)))
          (progn
            (forward-char skip)
            (if (looking-at "\\s *\\($\\|%\\)")
                (progn
                  (if (memq (car stack-top) '(-> ||))
                      (erlang-pop stack))
                  ;; Take parent identation + offset,
                  ;; else just erlang-indent-level if no parent
                  (if stack
                      (+ (erlang-caddr (car stack))
                         offset)
                    erlang-indent-level))
              (erlang-skip-blank indent-point)
              (current-column)))
        (+ (current-column) skip)))))


;; Does not handle `begin' .. `end'.
(defun erlang-indent-find-preceding-expr (&optional arg)
  "Return the first column of the preceding expression.
This assumes that the preceding expression is either simple
\(i.e. an atom) or parenthesized."
  (save-excursion
    (or arg (setq arg 1))
    (ignore-errors (forward-sexp (- arg)))
    (let ((col (current-column)))
      (skip-chars-backward " \t")
      ;; Special hack to handle: (note line break)
      ;; [#myrecord{
      ;;  foo = foo}]
      ;; where the call (forward-sexp -1) will fail when point is at the `#'.
      (or
       (ignore-errors
         ;; Needed to match the colon in "'foo':'bar'".
         (cond ((eq (preceding-char) ?:)
                (backward-char 1)
                (forward-sexp -1)
                (current-column))
               ((eq  (preceding-char) ?#)
                ;; We may now be at:
                ;; - either a construction of a new record
                ;; - or update of a record, in which case we want
                ;;   the column of the expression to be updated.
                ;;
                ;; To see which of the two cases we are at, we first
                ;; move an expression backwards, check for keywords,
                ;; then immediately an expression forwards.  Moving
                ;; backwards skips past tokens like `,' or `->', but
                ;; when moving forwards again, we won't skip past such
                ;; tokens.  We use this: if, after having moved
                ;; forwards, we're back where we started, then it was
                ;; a record update.
                ;; The check for keywords is to detect cases like:
                ;;   case Something of #record_construction{...}
                (backward-char 1)
                (let ((record-start (point))
                      (record-start-col (current-column)))
                  (forward-sexp -1)
                  (let ((preceding-expr-col (current-column))
                        ;; white space definition according to erl_scan
                        (white-space "\000-\040\200-\240"))
                    (if (erlang-at-keyword)
                        ;; The (forward-sexp -1) call moved past a keyword
                        (1+ record-start-col)
                      (forward-sexp 1)
                      (skip-chars-forward white-space record-start)
                      ;; Are we back where we started?  If so, it was an update.
                      (if (= (point) record-start)
                          preceding-expr-col
                        (goto-char record-start)
                        (1+ (current-column)))))))
               (t col)))
       col))))

(defun erlang-record-or-function-args-p ()
  (and (looking-at "[({]\\s *\\($\\|%\\)")
       (or (eq (following-char) ?\( )
           (save-excursion
             (ignore-errors (forward-sexp (- 1)))
             (eq (preceding-char) ?#)))))

(defun erlang-indent-parenthesis (stack-position)
  (let ((previous (erlang-indent-find-preceding-expr)))
    (cond ((eq previous stack-position) ;; tuple or map not a record
           (1+ stack-position))
          ((> previous stack-position)
           (+ stack-position erlang-argument-indent))
          (t
           (+ previous erlang-argument-indent)))))

(defun erlang-skip-blank (&optional lim)
  "Skip over whitespace and comments until limit reached."
  (or lim (setq lim (point-max)))
  (let (stop)
    (while (and (not stop) (< (point) lim))
      (cond ((= (following-char) ?%)
             (skip-chars-forward "^\n" lim))
            ((= (following-char) ?\n)
             (skip-chars-forward "\n" lim))
            ((looking-at "\\s ")
             (if (re-search-forward "\\S " lim 'move)
                 (forward-char -1)))
            (t
             (setq stop t))))
    stop))

(defun erlang-at-keyword ()
  "Are we looking at an Erlang keyword which will increase indentation?"
  (looking-at (concat "\\(when\\|if\\|fun\\|case\\|begin\\|"
                      "of\\|receive\\|after\\|catch\\|try\\)\\b")))

(defun erlang-at-operator ()
  "Are we looking at an Erlang operator?"
  (looking-at
   "\\(bnot\\|div\\|mod\\|band\\|bor\\|bxor\\|bsl\\|bsr\\)\\b"))

(defun erlang-comment-indent ()
  "Compute Erlang comment indentation.

Used both by `indent-for-comment' and the Erlang specific indentation
commands."
  (cond ((looking-at "%%%") 0)
        ((looking-at "%%")
         (or (erlang-calculate-indent)
             (current-indentation)))
        (t
         (save-excursion
           (skip-chars-backward " \t")
           (max (if (bolp) 0 (1+ (current-column)))
                comment-column)))))

;;; Erlang movement commands

;; All commands below work as movement commands.  I.e. if the point is
;; at the end of the clause, and the command `erlang-end-of-clause' is
;; executed, the point is moved to the end of the NEXT clause.  (This
;; mimics the behaviour of `end-of-defun'.)
;;
;; Personally I would like to rewrite them to be "pure", and add a set
;; of movement functions, like `erlang-next-clause',
;; `erlang-previous-clause', and the same for functions.
;;
;; The current implementation makes it hopeless to use the functions as
;; subroutines in more complex commands.   /andersl

(defun erlang-beginning-of-clause (&optional arg)
  "Move backward to previous start of clause.
With argument, do this that many times.
Return t unless search stops due to end of buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      ;; Step back to the end of the previous line, unless we are at
      ;; the beginning of the buffer.  The reason for this move is
      ;; that the regexp below includes the last character of the
      ;; previous line.
      (if (bobp)
          (or (looking-at "\n")
              (forward-char 1))
        (forward-char -1)
        (if (looking-at "\\`\n")
            (forward-char 1))))
  ;; The regexp matches a function header that isn't
  ;; included in a string.
  (and (re-search-forward "\\(\\`\\|\\`\n\\|[^\\]\n\\)\\(-?[a-z]\\|'\\|-\\)"
                          nil 'move (- arg))
       (let ((beg (match-beginning 2)))
         (and beg (goto-char beg))
         t)))

(defun erlang-end-of-clause (&optional arg)
  "Move to the end of the current clause.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (and (looking-at "[ \t]*[%\n]")
              (zerop (forward-line 1))))
  ;; Move to the next clause.
  (erlang-beginning-of-clause (- arg))
  (beginning-of-line);; Just to be sure...
  (let ((continue t))
    (while (and (not (bobp)) continue)
      (forward-line -1)
      (unless (looking-at "[ \t]*[%\n]")
        (end-of-line)
        (setq continue nil)))))

(defun erlang-mark-clause ()
  "Put mark at end of clause, point at beginning."
  (interactive)
  (push-mark (point))
  (erlang-end-of-clause 1)
  ;; Sets the region.
  (push-mark (point) nil t)
  (erlang-beginning-of-clause 1)
  ;; The above function deactivates the mark.
  (if (boundp 'deactivate-mark)
      (funcall (symbol-function 'set) 'deactivate-mark nil)))

(defun erlang-beginning-of-function (&optional arg)
  "Move backward to previous start of function.
With positive argument, do this that many times.
With negative argument, search forward.

Return t unless search stops due to end of buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (cond
   ;; Search backward
   ((> arg 0)
    (while (and (> arg 0)
                (and (erlang-beginning-of-clause 1)
                     (let ((start (point))
                           (name (erlang-name-of-function))
                           (arity (erlang-get-function-arity)))
                       ;; Note: "arity" is nil for e.g. "-import", hence
                       ;; two "-import" clauses are not considered to
                       ;; be part of the same function.
                       (while (and (erlang-beginning-of-clause 1)
                                   (string-equal name
                                                 (erlang-name-of-function))
                                   arity
                                   (equal arity
                                          (erlang-get-function-arity)))
                         (setq start (point)))
                       (goto-char start)
                       t)))
      (setq arg (1- arg))))
   ;; Search forward
   ((< arg 0)
    (end-of-line)
    (erlang-beginning-of-clause 1)
    ;; Step -arg functions forward.
    (while (and (< arg 0)
                ;; Step one function forward, or stop if the end of
                ;; the buffer was reached.  Return t if we found the
                ;; function.
                (let ((name (erlang-name-of-function))
                      (arity (erlang-get-function-arity))
                      (found (erlang-beginning-of-clause -1)))
                  (while (and found
                              (string-equal name (erlang-name-of-function))
                              arity
                              (equal arity
                                     (erlang-get-function-arity)))
                    (setq found (erlang-beginning-of-clause -1)))
                  found))
      (setq arg (1+ arg)))))
  (zerop arg))


(defun erlang-end-of-function (&optional arg)
  "Move forward to next end of function.

With argument, do this that many times.
With negative argument go towards the beginning of the buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((first t))
    ;; Forward
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)))
        (while (progn
                 (if (and first
                          (progn
                            (forward-char 1)
                            (erlang-beginning-of-clause 1)))
                     nil
                   (or (bobp) (forward-char -1))
                   (erlang-beginning-of-clause -1))
                 (setq first nil)
                 (erlang-pass-over-function)
                 (skip-chars-forward " \t")
                 (if (looking-at "[%\n]")
                     (forward-line 1))
                 (<= (point) pos))))
      (setq arg (1- arg)))
    ;; Backward
    (while (< arg 0)
      (let ((pos (point)))
        (erlang-beginning-of-clause 1)
        (erlang-pass-over-function)
        (forward-line 1)
        (if (>= (point) pos)
            (if (erlang-beginning-of-function 2)
                (progn
                  (erlang-pass-over-function)
                  (skip-chars-forward " \t")
                  (if (looking-at "[%\n]")
                      (forward-line 1)))
              (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(eval-and-compile
  (if (default-boundp 'beginning-of-defun-function)
      (defalias 'erlang-mark-function 'mark-defun)
    (defun erlang-mark-function ()
      "Put mark at end of function, point at beginning."
      (interactive)
      (push-mark (point))
      (erlang-end-of-function 1)
      ;; Sets the region.
      (push-mark (point) nil t)
      (erlang-beginning-of-function 1)
      ;; The above function deactivates the mark.
      (if (boundp 'deactivate-mark)
          (funcall (symbol-function 'set) 'deactivate-mark nil)))))

(defun erlang-pass-over-function ()
  (while (progn
           (erlang-skip-blank)
           (and (not (looking-at "\\.\\(\\s \\|\n\\|\\s<\\)"))
                (not (eobp))))
    (forward-sexp 1))
  (if (not (eobp))
      (forward-char 1)))

(defun erlang-name-of-function ()
  (save-excursion
    ;; Skip over attribute leader.
    (if (looking-at "-[ \t]*")
        (re-search-forward "-[ \t]*" nil 'move))
    (let ((start (point)))
      (forward-sexp 1)
      (buffer-substring start (point)))))


;;; Miscellaneous

(defun erlang-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Erlang comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `%':s."
  (interactive "P")
  (let ((has-comment nil)
        ;; If has-comment, the appropriate fill-prefix for the comment.
        comment-fill-prefix)
    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; Find the command prefix.
       ((looking-at (concat "\\s *" comment-start-skip))
        (setq has-comment t)
        (setq comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))
       ;; A line with some code, followed by a comment?  Remember that the
       ;; % which starts the comment shouldn't be part of a string or
       ;; character.
       ((progn
          (while (not (looking-at "%\\|$"))
            (skip-chars-forward "^%\n\"\\\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((eq (char-after (point)) ?\") (forward-sexp 1))))
          (looking-at comment-start-skip))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))
    (if (not has-comment)
        (fill-paragraph justify)
      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
        (narrow-to-region
         ;; Find the first line we should include in the region to fill.
         (save-excursion
           (while (and (zerop (forward-line -1))
                       (looking-at "^\\s *%")))
           ;; We may have gone to far.  Go forward again.
           (or (looking-at "^\\s *%")
               (forward-line 1))
           (point))
         ;; Find the beginning of the first line past the region to fill.
         (save-excursion
           (while (progn (forward-line 1)
                         (looking-at "^\\s *%")))
           (point)))
        ;; Lines with only % on them can be paragraph boundaries.
        (let ((paragraph-start (concat paragraph-start "\\|^[ \t%]*$"))
              (paragraph-separate (concat paragraph-start "\\|^[ \t%]*$"))
              (fill-prefix comment-fill-prefix))
          (fill-paragraph justify))))))

(defun erlang-generate-new-clause ()
  "Create additional Erlang clause header.

Parses the source file for the name of the current Erlang function.
Create the header containing the name, A pair of parentheses,
and an arrow. The space between the function name and the
first parenthesis is preserved.  The point is placed between
the parentheses."
  (interactive)
  (let ((name (save-excursion
                (and (erlang-beginning-of-clause)
                     (erlang-get-function-name t))))
        (arrow (save-excursion
                 (and (erlang-beginning-of-clause)
                      (erlang-get-function-arrow)))))
    (if (or (null arrow) (null name))
        (error "Can't find name of current Erlang function"))
    (if (and (bolp) (eolp))
        nil
      (end-of-line)
      (newline))
    (insert name)
    (save-excursion
      (insert ") " arrow))
    (if erlang-new-clause-with-arguments
        (erlang-clone-arguments))))


(defun erlang-clone-arguments ()
  "Insert, at the point, the argument list of the previous clause.

The mark is set at the beginning of the inserted text, the point
at the end."
  (interactive)
  (let ((args (save-excursion
                (beginning-of-line)
                (and (erlang-beginning-of-clause)
                     (erlang-get-function-arguments))))
        (p (point)))
    (if (null args)
        (error "Can't clone argument list"))
    (insert args)
    (set-mark p)))

;;; Information retrieval functions.

(defun erlang-get-module ()
  "Return the name of the module as specified by `-module'.

Return nil if file contains no `-module' attribute."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((md (match-data)))
        (unwind-protect
            (if (re-search-forward
                 (eval-when-compile
                   (concat "^-module\\s *(\\s *\\(\\("
                           erlang-atom-regexp
                           "\\)?\\)\\s *)\\s *\\."))
                 (point-max) t)
                (erlang-remove-quotes
                 (buffer-substring-no-properties (match-beginning 1)
                                          (match-end 1)))
              nil)
          (store-match-data md))))))


(defun erlang-get-module-from-file-name (&optional file)
  "Extract the module name from a file name.

First, the directory part is removed.  Second, the part of the file name
matching `erlang-file-name-extension-regexp' is removed.

Should the match fail, nil is returned.

By modifying `erlang-file-name-extension-regexp' to match files other
than Erlang source files, Erlang specific functions could be applied on
non-Erlang files.  Most notably; the support for Erlang modules in the
tags system could be used by files written in other languages."
  (or file (setq file buffer-file-name))
  (if (null file)
      nil
    (setq file (file-name-nondirectory file))
    (if (string-match erlang-file-name-extension-regexp file)
        (substring file 0 (match-beginning 0))
      nil)))


;; Used by `erlang-get-export' and `erlang-get-import'.

(defun erlang-get-function-arity-list ()
  "Parse list of `function/arity' as used by `-import' and `-export'.

Point must be before the opening bracket.  When the
function returns the point will be placed after the closing bracket.

The function does not return an error if the list is incorrectly
formatted.

Return list of (function . arity).  The order of the returned list
corresponds to the order of the parsed Erlang list."
  (let ((res '()))
    (erlang-skip-blank)
    (forward-char 1)
    (if (not (eq (preceding-char) ?\[))
        '()                             ; Not looking at an Erlang list.
      (while                            ; Note: `while' has no body.
          (progn
            (erlang-skip-blank)
            (and (looking-at (eval-when-compile
                               (concat erlang-atom-regexp "/\\([0-9]+\\)\\>")))
                 (progn
                   (setq res (cons
                              (cons
                               (erlang-remove-quotes
                                (buffer-substring-no-properties
                                 (match-beginning 1) (match-end 1)))
                               (string-to-number
                                (buffer-substring-no-properties
                                 (match-beginning
                                  (+ 1 erlang-atom-regexp-matches))
                                 (match-end
                                  (+ 1 erlang-atom-regexp-matches)))))
                              res))
                   (goto-char (match-end 0))
                   (erlang-skip-blank)
                   (forward-char 1)
                   ;; Test if there are more exported functions.
                   (eq (preceding-char) ?,))))))
    (nreverse res)))


;;;  Note that `-export' and the open parenthesis must be written on
;;;  the same line.

(defun erlang-get-export ()
  "Return a list of `(function . arity)' as specified by `-export'."
  (save-excursion
    (goto-char (point-min))
    (let ((md (match-data))
          (res '()))
      (unwind-protect
          (progn
            (while (re-search-forward "^-export\\s *(" (point-max) t)
              (erlang-skip-blank)
              (setq res (nconc res (erlang-get-function-arity-list))))
            res)
        (store-match-data md)))))


(defun erlang-get-import ()
  "Parse an Erlang source file for imported functions.

Return an alist with module name as car part and list of conses containing
function and arity as cdr part."
  (save-excursion
    (goto-char (point-min))
    (let ((md (match-data))
          (res '()))
      (unwind-protect
          (progn
            (while (re-search-forward "^-import\\s *(" (point-max) t)
              (erlang-skip-blank)
              (if (looking-at erlang-atom-regexp)
                  (let ((module (erlang-remove-quotes
                                 (buffer-substring-no-properties
                                  (match-beginning 0)
                                  (match-end 0)))))
                    (goto-char (match-end 0))
                    (erlang-skip-blank)
                    (if (eq (following-char) ?,)
                        (progn
                          (forward-char 1)
                          (erlang-skip-blank)
                          (let ((funcs (erlang-get-function-arity-list))
                                (pair (assoc module res)))
                            (if pair
                                (setcdr pair (nconc (cdr pair) funcs))
                              (setq res (cons (cons module funcs)
                                              res)))))))))
            (nreverse res))
        (store-match-data md)))))


(defun erlang-get-function-name (&optional arg)
  "Return name of current function, or nil.

If optional argument is non-nil, everything up to and including
the first `(' is returned.

Normally used in conjunction with `erlang-beginning-of-clause', e.g.:
              (save-excursion
                (if (not (eobp)) (forward-char 1))
                (and (erlang-beginning-of-clause)
                     (erlang-get-function-name t)))"
  (let ((n (if arg 0 1)))
    (and (looking-at (eval-when-compile
                       (concat "^" erlang-atom-regexp "\\s *(")))
         (buffer-substring-no-properties (match-beginning n) (match-end n)))))


(defun erlang-get-function-arrow ()
  "Return arrow of current function, could be \"->\" or nil.

Normally used in conjunction with `erlang-beginning-of-clause', e.g.:
              (save-excursion
                (if (not (eobp)) (forward-char 1))
                (and (erlang-beginning-of-clause)
                     (erlang-get-function-arrow)))"
  (and
   (save-excursion
     (re-search-forward "->" (point-max) t)
     (buffer-substring-no-properties (- (point) 2) (+ (point) 1)))))

(defun erlang-get-function-arity ()
  "Return the number of arguments of function at point, or nil."
  (erlang-get-arity-after-regexp (concat "^" erlang-atom-regexp "\\s *(")))

(defun erlang-get-argument-list-arity ()
  "Return the number of arguments in argument list at point, or nil.
The point should be before the opening parenthesis of the
argument list before calling this function."
  (erlang-get-arity-after-regexp "\\s *("))

(defun erlang-get-arity-after-regexp (regexp)
  "Return the number of arguments in argument list after REGEXP, or nil."
  (when (looking-at regexp)
    (save-excursion
      (goto-char (match-end 0))
      (erlang-get-arity))))

(defun erlang-get-arity ()
  "Return the number of arguments in argument list at point, or nil.
The point should be after the opening parenthesis of the argument
list before calling this function."
  (condition-case nil
      (let ((res 0)
            (cont t))
        (while cont
          (cond ((eobp)
                 (setq res nil)
                 (setq cont nil))
                ((looking-at "\\s *)")
                 (setq cont nil))
                ((looking-at "\\s *\\($\\|%\\)")
                 (forward-line 1))
                ((looking-at "\\s *<<[^>]*?>>")
                 (when (zerop res)
                   (setq res (+ 1 res)))
                 (goto-char (match-end 0)))
                ((looking-at "\\s *,")
                 (setq res (+ 1 res))
                 (goto-char (match-end 0)))
                (t
                 (when (zerop res)
                   (setq res (+ 1 res)))
                 (forward-sexp 1))))
        res)
    (error nil)))


(defun erlang-get-function-name-and-arity ()
  "Return the name and arity of the function at point, or nil.
The return value is a string of the form \"foo/1\"."
  (let ((name (erlang-get-function-name))
        (arity (erlang-get-function-arity)))
    (and name arity (format "%s/%d" name arity))))

(defun erlang-get-function-arguments ()
  "Return arguments of current function, or nil."
  (if (not (looking-at (eval-when-compile
                         (concat "^" erlang-atom-regexp "\\s *("))))
      nil
    (save-excursion
      (condition-case nil
          (let ((start (match-end 0)))
            (goto-char (- start 1))
            (forward-sexp)
            (buffer-substring-no-properties start (- (point) 1)))
        (error nil)))))


;; erlang-get-function-under-point is replaced by
;; erlang-get-identifier-at-point as far as internal erlang.el usage
;; is concerned.  But it is kept for backward compatibility.  It is
;; used by erldoc.el and maybe other code out there.
(defun erlang-get-function-under-point ()
  "Return the module and function under the point, or nil.

Should no explicit module name be present at the point, the
list of imported functions is searched.

The following could be returned:
   (\"module\"  \"function\")    -- Both module and function name found.
   (nil       \"function\")    -- No module name was found.
   nil                       -- No function name found.

See also `erlang-get-identifier-at-point'."
  (let* ((id (erlang-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (module (erlang-id-module id))
         (name (erlang-id-name id)))
    (cond ((eq kind 'qualified-function)
           (list module name))
          (name
           (list nil name)))))

(defun erlang-get-identifier-at-point ()
  "Return the erlang identifier at point, or nil.

Should no explicit module name be present at the point, the
list of imported functions is searched.

When an identifier is found return a list with 4 elements:

1. Kind - One of the symbols qualified-function, record, macro,
module or nil.

2. Module - Module name string or nil.  In case of a
qualified-function a search fails if no entries with correct
module are found.  For other kinds the module is just a
preference.  If no matching entries are found the search will be
retried without regard to module.

3. Name - String name of function, module, record or macro.

4. Arity - Integer in case of functions and macros if the number
of arguments could be found, otherwise nil."
  (save-excursion
    (save-match-data
      (if (eq (char-syntax (following-char)) ? )
          (skip-chars-backward " \t"))
      (skip-chars-backward "[:word:]_:'")
      (cond ((looking-at erlang-module-function-regexp)
             (erlang-get-qualified-function-id-at-point))
            ((looking-at (concat erlang-atom-regexp ":"))
             (erlang-get-module-id-at-point))
            ((looking-at erlang-name-regexp)
             (erlang-get-some-other-id-at-point))))))

(defun erlang-get-qualified-function-id-at-point ()
  (let ((kind 'qualified-function)
        (module (erlang-remove-quotes
                 (buffer-substring-no-properties
                  (match-beginning 1) (match-end 1))))
        (name (erlang-remove-quotes
               (buffer-substring-no-properties
                (match-beginning (1+ erlang-atom-regexp-matches))
                (match-end (1+ erlang-atom-regexp-matches)))))
        (arity (progn
                 (goto-char (match-end 0))
                 (erlang-get-argument-list-arity))))
    (list kind module name arity)))

(defun erlang-get-module-id-at-point ()
  (let ((kind 'module)
        (module nil)
        (name (erlang-remove-quotes
               (buffer-substring-no-properties (match-beginning 1)
                                        (match-end 1))))
        (arity nil))
    (list kind module name arity)))

(defun erlang-get-some-other-id-at-point ()
  (let ((name (erlang-remove-quotes
               (buffer-substring-no-properties
                (match-beginning 0) (match-end 0))))
        (imports (erlang-get-import))
        kind module arity)
    (while (and imports (null module))
      (if (assoc name (cdr (car imports)))
          (setq module (car (car imports)))
        (setq imports (cdr imports))))
    (cond ((eq (preceding-char) ?#)
           (setq kind 'record))
          ((eq (preceding-char) ??)
           (setq kind 'macro))
          ((and (null module) (not (member name erlang-int-bifs)))
           (setq module (erlang-get-module))))
    (setq arity (progn
                  (goto-char (match-end 0))
                  (erlang-get-argument-list-arity)))
    (list kind module name arity)))

(defmacro erlang-with-id (slots id-string &rest body)
  (declare (indent 2))
  (let ((id-var (make-symbol "id")))
    `(let* ((,id-var (erlang-id-to-list ,id-string))
            ,@(mapcar (lambda (slot)
                        (list slot
                              (list (intern (format "erlang-id-%s" slot))
                                    id-var)))
                      slots))
       ,@body)))

(defun erlang-id-to-string (id)
  (when id
    (erlang-with-id (kind module name arity) id
      (format "%s%s%s%s"
              (if kind (format "%s " kind) "")
              (if module (format "%s:" module) "")
              name
              (if arity (format "/%s" arity) "")))))

(defun erlang-id-to-list (id)
  (if (listp id)
      id
    (save-match-data
      (erlang-ensure-syntax-table-is-initialized)
      (with-syntax-table erlang-mode-syntax-table
        (let (case-fold-search)
          (when (string-match erlang-id-regexp id)
            (list (when (match-string 1 id)
                    (intern (match-string 1 id)))
                  (match-string 2 id)
                  (match-string 3 id)
                  (when (match-string 4 id)
                    (string-to-number (match-string 4 id))))))))))

(defun erlang-id-kind (id)
  (car (erlang-id-to-list id)))

(defun erlang-id-module (id)
  (nth 1 (erlang-id-to-list id)))

(defun erlang-id-name (id)
  (nth 2 (erlang-id-to-list id)))

(defun erlang-id-arity (id)
  (nth 3 (erlang-id-to-list id)))


(defun erlang-default-function-or-module ()
  (erlang-with-id (kind module name) (erlang-get-identifier-at-point)
    (let ((x (cond ((eq kind 'module)
                    (format "%s:" name))
                   ((eq kind 'record)
                    (format "-record(%s" name))
                   ((eq kind 'macro)
                    (format "-define(%s" name))
                   (t
                    name))))
      (if module
          (format "%s:%s" module x)
        x))))


;; TODO: Escape single quotes inside the string without
;; replace-regexp-in-string.
(defun erlang-add-quotes-if-needed (str)
  "Return STR, possibly with quotes."
  (let ((case-fold-search nil)) ; force string matching to be case sensitive
    (if (and (stringp str)
             (not (string-match (eval-when-compile
                                  (concat "\\`" erlang-atom-regexp "\\'")) str)))
        (progn
          (setq str (replace-regexp-in-string "'" "\\'" str t t ))
          (concat "'" str "'"))
      str)))


(defun erlang-remove-quotes (str)
  "Return STR without quotes, if present."
  (let ((md (match-data)))
    (prog1
        (if (string-match "\\`'\\(.*\\)'\\'" str)
            (substring str 1 -1)
          str)
      (store-match-data md))))

(defun erlang-match-next-exported-function (max)
  "Returns non-nil if there is an exported function in the current
buffer between point and MAX."
  (block nil
         (while (and (not erlang-inhibit-exported-function-name-face)
                     (erlang-match-next-function max))
           (when (erlang-last-match-exported-p)
             (return (match-data))))))

(defun erlang-match-next-function (max)
  "Searches forward in current buffer for the next erlang function,
bounded by position MAX."
  (re-search-forward erlang-defun-prompt-regexp max 'move-point))

(defun erlang-last-match-exported-p ()
  "Returns non-nil if match-data describes the name and arity of an
exported function."
  (save-excursion
    (save-match-data
      (goto-char (match-beginning 1))
      (erlang-function-exported-p
       (erlang-remove-quotes (erlang-get-function-name))
       (erlang-get-function-arity)))))

(defun erlang-function-exported-p (name arity)
  "Returns non-nil if function of name and arity is exported in current buffer."
  (save-excursion
    (let* ((old-match-data (match-data))
           (exports        (erlang-get-export)))
      (store-match-data old-match-data)
      (member (cons name arity) exports))))


;;; Check module name

;; The function `write-file', bound to C-x C-w, calls
;; `set-visited-file-name' which clears the hook.  :-(
;; To make sure that the hook always is present, we advise
;; `set-visited-file-name'.
(defun erlang-check-module-name-init ()
  "Initialize the functionality to compare file and module names.

Unless we have `before-save-hook', we advice the function
`set-visited-file-name' since it clears the variable
`local-write-file-hooks'."
  (if (boundp 'before-save-hook)
      (add-hook 'before-save-hook 'erlang-check-module-name nil t)
    (require 'advice)
    (when (fboundp 'ad-advised-definition-p)
      (unless (ad-advised-definition-p 'set-visited-file-name)
        (defadvice set-visited-file-name (after erlang-set-visited-file-name
                                                activate)
          (if (eq major-mode 'erlang-mode)
              (add-hook 'local-write-file-hooks 'erlang-check-module-name))))
      (add-hook 'local-write-file-hooks 'erlang-check-module-name))))


(defun erlang-check-module-name ()
  "If the module name doesn't match file name, ask for permission to change.

The variable `erlang-check-module-name' controls the behaviour of this
function.  It it is nil, this function does nothing.  If it is t, the
source is silently changed.  If it is set to the atom `ask', the user
is prompted.

This function is normally placed in the hook `local-write-file-hooks'."
  (if erlang-check-module-name
      (let ((mn (erlang-add-quotes-if-needed
                 (erlang-get-module)))
            (fn (erlang-add-quotes-if-needed
                 (erlang-get-module-from-file-name (buffer-file-name)))))
        (if (and (stringp mn) (stringp fn))
            (or (string-equal mn fn)
                (if (or (eq erlang-check-module-name t)
                        (y-or-n-p
                         "Module does not match file name. Modify source? "))
                    (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (if (re-search-forward
                             (eval-when-compile
                               (concat "^-module\\s *(\\s *\\(\\("
                                       erlang-atom-regexp
                                       "\\)?\\)\\s *)\\s *\\."))
                             (point-max) t)
                            (progn
                              (goto-char (match-beginning 1))
                              (delete-region (match-beginning 1)
                                             (match-end 1))
                              (insert fn))))))))))
  ;; Must return nil since it is added to `local-write-file-hook'.
  nil)


;;; Electric functions.

(defun erlang-electric-semicolon (&optional arg)
  "Insert a semicolon character and possibly a prototype for the next line.

The variable `erlang-electric-semicolon-criteria' states a criterion,
when fulfilled a newline is inserted, the next line is indented and a
prototype for the next line is inserted.  Normally the prototype
consists of \" ->\".  Should the semicolon end the clause a new clause
header is generated.

The variable `erlang-electric-semicolon-insert-blank-lines' controls
the number of blank lines inserted between the current line and new
function header.

Behaves just like the normal semicolon when supplied with a
numerical arg, point is inside string or comment, or when there are
non-whitespace characters following the point on the current line."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (or arg
          (and (listp erlang-electric-commands)
               (not (memq 'erlang-electric-semicolon
                          erlang-electric-commands)))
          (erlang-in-literal)
          (not (looking-at "\\s *\\(%.*\\)?$"))
          (null (erlang-test-criteria-list
                 erlang-electric-semicolon-criteria)))
      (setq erlang-electric-newline-inhibit nil)
    (setq erlang-electric-newline-inhibit t)
    (undo-boundary)
    (erlang-indent-line)
    (end-of-line)
    (newline)
    (if (condition-case nil
            (progn (erlang-indent-line) t)
          (error (if (bolp) (delete-char -1))))
        (if (not (bolp))
            (save-excursion
              (insert " ->"))
          (condition-case nil
              (progn
                (erlang-generate-new-clause)
                (if erlang-electric-semicolon-insert-blank-lines
                    (save-excursion
                      (beginning-of-line)
                      (newline
                       erlang-electric-semicolon-insert-blank-lines))))
            (error (if (bolp) (delete-char -1))))))))


(defun erlang-electric-comma (&optional arg)
  "Insert a comma character and possibly a new indented line.
The variable `erlang-electric-comma-criteria' states a criterion,
when fulfilled a newline is inserted and the next line is indented.

Behaves just like the normal comma when supplied with a
numerical arg, point is inside string or comment, or when there are
non-whitespace characters following the point on the current line."
  (interactive "P")

  (self-insert-command (prefix-numeric-value arg))

  (if (or arg
          (and (listp erlang-electric-commands)
               (not (memq 'erlang-electric-comma erlang-electric-commands)))
          (erlang-in-literal)
          (not (looking-at "\\s *\\(%.*\\)?$"))
          (null (erlang-test-criteria-list
                 erlang-electric-comma-criteria)))
      (setq erlang-electric-newline-inhibit nil)
    (setq erlang-electric-newline-inhibit t)
    (undo-boundary)
    (erlang-indent-line)
    (end-of-line)
    (newline)
    (condition-case nil
        (erlang-indent-line)
      (error (if (bolp) (delete-char -1))))))

(defun erlang-electric-lt (&optional arg)
  "Insert a less-than sign, and optionally mark it as an open paren."

  (interactive "p")

  (self-insert-command arg)

  ;; Was this the second char in bit-syntax open (`<<')?
  (unless (<= (point) 2)
    (save-excursion
      (backward-char 2)
      (when (and (eq (char-after (point)) ?<)
                 (not (eq (get-text-property (point) 'category)
                          'bitsyntax-open-inner)))
        ;; Then mark the two chars...
        (put-text-property (point) (1+ (point))
                           'category 'bitsyntax-open-outer)
        (forward-char 1)
        (put-text-property (point) (1+ (point))
                           'category 'bitsyntax-open-inner)
        ;;...and unmark any subsequent less-than chars.
        (forward-char 1)
        (while (eq (char-after (point)) ?<)
          (remove-text-properties (point) (1+ (point))
                                  '(category nil))
          (forward-char 1))))))

(defun erlang-after-bitsyntax-close ()
  "Return t if point is immediately after a bit-syntax close parenthesis (`>>')."
  (and (>= (point) 3)
       (save-excursion
         (backward-char 2)
         (and (eq (char-after (point)) ?>)
              (not (eq (get-text-property (point) 'category)
                       'bitsyntax-close-outer))))))

(defun erlang-after-arrow ()
  "Return true if point is immediately after a function arrow (`->')."
  (and (>= (point) 2)
       (and
        (save-excursion
          (backward-char)
          (eq (char-before (point)) ?-))
        (or (not (listp erlang-electric-commands))
            (memq 'erlang-electric-gt
                  erlang-electric-commands))
        (not (erlang-in-literal))
        (looking-at "\\s *\\(%.*\\)?$")
        (erlang-test-criteria-list erlang-electric-arrow-criteria))))


(defun erlang-electric-gt (&optional arg)
  "Insert a greater-than sign, and optionally mark it as a close paren."

  (interactive "p")

  (self-insert-command arg)

  (cond
   ;; Did we just write a bit-syntax close (`>>')?
   ((erlang-after-bitsyntax-close)
    (save-excursion
      ;; Then mark the two chars...
      (backward-char 2)
      (put-text-property (point) (1+ (point))
                         'category 'bitsyntax-close-inner)
      (forward-char)
      (put-text-property (point) (1+ (point))
                         'category 'bitsyntax-close-outer)
      ;;...and unmark any subsequent greater-than chars.
      (forward-char)
      (while (eq (char-after (point)) ?>)
        (remove-text-properties (point) (1+ (point))
                                '(category nil))
        (forward-char))))

   ;; Did we just write a function arrow (`->')?
   ((erlang-after-arrow)
    (let ((erlang-electric-newline-inhibit t))
      (undo-boundary)
      (end-of-line)
      (newline)
      (condition-case nil
          (erlang-indent-line)
        (error (if (bolp) (delete-char -1))))))

   ;; Then it's just a plain greater-than.
   (t
    nil)))


(defun erlang-electric-arrow (&optional arg)
  "Insert a '>'-sign and possibly a new indented line.

This command is only `electric' when the `>' is part of an `->' arrow.
The variable `erlang-electric-arrow-criteria' states a sequence of
criteria, which decides when a newline should be inserted and the next
line indented.

It behaves just like the normal greater than sign when supplied with a
numerical arg, point is inside string or comment, or when there are
non-whitespace characters following the point on the current line.

After being split/merged into `erlang-after-arrow' and
`erlang-electric-gt', it is now unused and disabled."
  (interactive "P")
  (let ((prec (preceding-char)))
    (self-insert-command (prefix-numeric-value arg))
    (if (or arg
            (and (listp erlang-electric-commands)
                 (not (memq 'erlang-electric-arrow
                            erlang-electric-commands)))
            (not (eq prec ?-))
            (erlang-in-literal)
            (not (looking-at "\\s *\\(%.*\\)?$"))
            (null (erlang-test-criteria-list
                   erlang-electric-arrow-criteria)))
        (setq erlang-electric-newline-inhibit nil)
      (setq erlang-electric-newline-inhibit t)
      (undo-boundary)
      (end-of-line)
      (newline)
      (condition-case nil
          (erlang-indent-line)
        (error (if (bolp) (delete-char -1)))))))


(defun erlang-electric-newline (&optional arg)
  "Break line at point and indent, continuing comment if within one.
The variable `erlang-electric-newline-criteria' states a criterion,
when fulfilled a newline is inserted and the next line is indented.

Should the current line begin with a comment, and the variable
`comment-multi-line' be non-nil, a new comment start is inserted.

Should the previous command be another electric command we assume that
the user pressed newline out of old habit, hence we will do nothing."
  (interactive "P")
  (cond ((and (not arg)
              erlang-electric-newline-inhibit
              (memq last-command erlang-electric-newline-inhibit-list))
         ())                            ; Do nothing!
        ((or arg
             (and (listp erlang-electric-commands)
                  (not (memq 'erlang-electric-newline
                             erlang-electric-commands)))
             (null (erlang-test-criteria-list
                    erlang-electric-newline-criteria)))
         (newline (prefix-numeric-value arg)))
        (t
         (if (and comment-multi-line
                  (save-excursion
                    (beginning-of-line)
                    (looking-at (concat "\\s *" comment-start-skip))))
             (let ((str (buffer-substring
                         (or (match-end 1) (match-beginning 0))
                         (min (match-end 0) (point)))))
               (newline)
               (undo-boundary)
               (insert str))
           (newline)
           (undo-boundary)
           (indent-according-to-mode)))))


(defun erlang-test-criteria-list (criteria)
  "Given a list of criterion functions, test if criteria are fulfilled.

Each element in the criteria list can a function returning nil, t or
the atom `stop'.  t means that the criterion is fulfilled, `stop' means
that it isn't fulfilled and that the search should stop,
and nil means continue searching.

Should the list contain the atom t the criterion is assumed to be
fulfilled, unless preceded by a function returning `stop', of course.

Should the argument be the atom t instead of a list, the criterion is
assumed to be trivially true.

Should all functions return nil, the criteria are assumed not to be
fulfilled.

Return t if criteria fulfilled, nil otherwise."
  (if (eq criteria t)
      t
    (save-excursion
      (let ((answer nil))
        (while (and criteria (null answer))
          (if (eq (car criteria) t)
              (setq answer t)
            (setq answer (funcall (car criteria))))
          (setq criteria (cdr criteria)))
        (if (and answer (not (eq answer 'stop)))
            t
          nil)))))


(defun erlang-in-literal (&optional lim)
  "Test if point is in string, quoted atom or comment.

Return one of the three atoms `atom', `string', and `comment'.
Should the point be inside none of the above mentioned types of
context, nil is returned."
  (save-excursion
    (let* ((lim (or lim (save-excursion
                          (erlang-beginning-of-clause)
                          (point))))
           (state (funcall (symbol-function 'syntax-ppss))))
      (cond
       ((eq (nth 3 state) ?') 'atom)
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))


(defun erlang-at-end-of-function-p ()
  "Test if point is at end of an Erlang function.

This function is designed to be a member of a criteria list."
  (eq (save-excursion (erlang-skip-blank) (point))
      (save-excursion
        (erlang-beginning-of-function -1) (point))))


(defun erlang-at-end-of-clause-p ()
  "Test if point is at end of an Erlang clause.

This function is designed to be a member of a criteria list."
  (eq (save-excursion (erlang-skip-blank) (point))
      (save-excursion
        (erlang-beginning-of-clause -1) (point))))


(defun erlang-stop-when-inside-argument-list ()
  "Return `stop' if inside parenthesis list, nil otherwise.

Knows about the list comprehension syntax.  When the point is
after `||', `stop' is not returned.

This function is designed to be a member of a criteria list."
  (save-excursion
    (condition-case nil
        (let ((orig-point (point))
              (state nil))
          (up-list -1)
          (if (not (eq (following-char) ?\[))
              'stop
            ;; Do not return `stop' when inside a list comprehension
            ;; construction.  (The point must be after `||').
            (while (< (point) orig-point)
              (let ((pt (point)))
                (setq state (erlang-partial-parse pt orig-point state))
                (if (= pt (point))
                    (error "Illegal syntax"))))
            (if (and (car state) (eq (car (car (car state))) '||))
                nil
              'stop)))
      (error
       nil))))


(defun erlang-stop-when-at-guard ()
  "Return `stop' when at function guards.

This function is designed to be a member of a criteria list."
  (save-excursion
    (beginning-of-line)
    (if (and (looking-at (eval-when-compile
                           (concat "^" erlang-atom-regexp "\\s *(")))
             (not (looking-at
                   (eval-when-compile
                     (concat "^" erlang-atom-regexp ".*->")))))
        'stop
      nil)))


(defun erlang-stop-when-in-type-spec ()
  "Return `stop' when in a type spec line.

This function is designed to be a member of a criteria list."
  (save-excursion
    (beginning-of-line)
    (when (save-match-data (looking-at "-\\(spec\\|type\\|callback\\)"))
      'stop)))


(defun erlang-next-lines-empty-p ()
  "Return non-nil if next lines are empty.

The variable `erlang-next-lines-empty-threshold' contains the number
of lines required to be empty.

A line containing only spaces and tabs is considered empty.

This function is designed to be a member of a criteria list."
  (and erlang-next-lines-empty-threshold
       (save-excursion
         (let ((left erlang-next-lines-empty-threshold)
               (cont t))
           (while (and cont (> left 0))
             (forward-line 1)
             (setq cont (looking-at "\\s *$"))
             (setq left (- left 1)))
           cont))))


(defun erlang-at-keyword-end-p ()
  "Test if next readable token is the keyword end.

This function is designed to be a member of a criteria list."
  (save-excursion
    (erlang-skip-blank)
    (looking-at "end[^_a-zA-Z0-9]")))


;;; Erlang tags support which is aware of erlang modules.

(eval-when-compile
  (require 'etags))


;; Variables:

(defvar erlang-tags-function-alist
  '((find-tag                 . erlang-find-tag)
    (find-tag-other-window    . erlang-find-tag-other-window)
    (find-tag-regexp          . erlang-find-tag-regexp)
    (find-tag-other-frame     . erlang-find-tag-other-frame))
  "Alist of old tags commands and the replacement functions.")

(defvar erlang-tags-installed nil
  "Non-nil when the Erlang tags system is installed.")
(defvar erlang-tags-file-list '()
  "List of files in tag list. Used when finding tag on form `module:'.")
(defvar erlang-tags-completion-table nil
  "Like `tags-completion-table', this table contains `tag' and `module:tag'.")
(defvar erlang-tags-buffer-installed-p nil
  "Non-nil when Erlang module recognising functions installed.")
(defvar erlang-tags-buffer-list '()
  "Temporary list of buffers.")
(defvar erlang-tags-orig-completion-table nil
  "Temporary storage for `tags-completion-table'.")
(defvar erlang-tags-orig-tag-order nil
  "Temporary storage for `find-tag-tag-order'.")
(defvar erlang-tags-orig-regexp-tag-order nil
  "Temporary storage for `find-tag-regexp-tag-order'.")
(defvar erlang-tags-orig-search-function nil
  "Temporary storage for `find-tag-search-function'.")
(defvar erlang-tags-orig-regexp-search-function nil
  "Temporary storage for `find-tag-regexp-search-function'.")
(defvar erlang-tags-orig-format-hooks nil
  "Temporary storage for `tags-table-format-hooks'.") ;v19
(defvar erlang-tags-orig-format-functions nil
  "Temporary storage for `tags-table-format-functions'.") ;v > 19

(defun erlang-tags-init ()
  "Install an alternate version of tags, aware of Erlang modules.

After calling this function, the tags functions are aware of
Erlang modules.  Tags can be entered on the for `module:tag' as well
as on the old form `tag'.

In the completion list, `module:tag' and `module:' shows up."
  (interactive)
  (require 'etags)
  (set (make-local-variable 'find-tag-default-function)
       'erlang-find-tag-for-completion)
  (if (>= emacs-major-version 25)
      (add-hook 'xref-backend-functions
                #'erlang-etags--xref-backend nil t)
    (erlang-tags-define-keys (current-local-map))
    (setq erlang-tags-installed t)))



;; Set all keys bound to `find-tag' et.al. in the global map and the
;; menu to `erlang-find-tag' et.al. in `map'.
;;
;; The function `substitute-key-definition' does not work properly
;; in all version of Emacs.

(defun erlang-tags-define-keys (map)
  "Bind tags commands to keymap MAP aware of Erlang modules."
  (let ((alist erlang-tags-function-alist))
    (while alist
      (let* ((old (car (car alist)))
             (new (cdr (car alist)))
             (keys (append (where-is-internal old global-map))))
        (while keys
          (define-key map (car keys) new)
          (setq keys (cdr keys))))
      (setq alist (cdr alist))))
  ;; Update the menu.
  (erlang-menu-substitute erlang-menu-base-items erlang-tags-function-alist)
  (erlang-menu-init))

;; Return `t' since it is used inside `tags-loop-form'.
;;;###autoload
(defun erlang-find-tag (modtagname &optional next-p regexp-p)
  "Like `find-tag'.  Capable of retrieving Erlang modules.

Tags can be given on the forms `tag', `module:', `module:tag'."
  (interactive (erlang-tag-interactive "Find `module:tag' or `tag': "))
  (switch-to-buffer (erlang-find-tag-noselect modtagname next-p regexp-p))
  t)


;; Code mainly from `find-tag-other-window' in `etags.el'.
;;;###autoload
(defun erlang-find-tag-other-window (tagname &optional next-p regexp-p)
  "Like `find-tag-other-window' but aware of Erlang modules."
  (interactive (erlang-tag-interactive
                "Find `module:tag' or `tag' other window: "))

  ;; This is to deal with the case where the tag is found in the
  ;; selected window's buffer; without this, point is moved in both
  ;; windows.  To prevent this, we save the selected window's point
  ;; before doing find-tag-noselect, and restore it afterwards.
  (let* ((window-point (window-point (selected-window)))
         (tagbuf (erlang-find-tag-noselect tagname next-p regexp-p))
         (tagpoint (progn (set-buffer tagbuf) (point))))
    (set-window-point (prog1
                          (selected-window)
                        (switch-to-buffer-other-window tagbuf)
                        ;; We have to set this new window's point; it
                        ;; might already have been displaying a
                        ;; different portion of tagbuf, in which case
                        ;; switch-to-buffer-other-window doesn't set
                        ;; the window's point from the buffer.
                        (set-window-point (selected-window) tagpoint))
                      window-point)))


(defun erlang-find-tag-other-frame (tagname &optional next-p)
  "Like `find-tag-other-frame' but aware of Erlang modules."
  (interactive (erlang-tag-interactive
                "Find `module:tag' or `tag' other frame: "))
  (let ((pop-up-frames t))
    (erlang-find-tag-other-window tagname next-p)))


(defun erlang-find-tag-regexp (regexp &optional next-p other-window)
  "Like `find-tag-regexp' but aware of Erlang modules."
  (interactive (if (fboundp 'find-tag-regexp)
                   (erlang-tag-interactive
                    "Find `module:regexp' or `regexp': ")
                 (error "This version of Emacs can't find tags by regexps")))
  (funcall (if other-window
               'erlang-find-tag-other-window
             'erlang-find-tag)
           regexp next-p t))


;; Just like C-u M-.  This could be added to the menu.
(defun erlang-find-next-tag ()
  "Find next tag, like \\[find-tag] with prefix arg."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (if erlang-tags-installed
        (call-interactively 'erlang-find-tag)
      (call-interactively 'find-tag))))


;; Mimics `find-tag-noselect' found in `etags.el', but uses `find-tag' to
;; be compatible with `tags.el'.
;;
;; Handles three cases:
;; * `module:'  Loop over all possible file names.  Stop if a file-name
;;              without extension and directory matches the module.
;;
;; * `module:tag'
;;              Emacs 19: Replace test functions with functions aware of
;;              Erlang modules.  Tricky because the etags system wasn't
;;              built for these kind of operations...
;;
;;              Emacs 18: We loop over `find-tag' until we find a file
;;              whose module matches the requested module.  The
;;              drawback is that a lot of files could be loaded into
;;              Emacs.
;;
;; * `tag'      Just give it to `find-tag'.

(defun erlang-find-tag-noselect (modtagname &optional next-p regexp-p)
  "Like `find-tag-noselect' but aware of Erlang modules."
  (interactive (erlang-tag-interactive "Find `module:tag' or `tag': "))
  (or modtagname
      (setq modtagname (symbol-value 'last-tag)))
  (funcall (symbol-function 'set) 'last-tag modtagname)
  ;; `tags.el' uses this variable to record how M-, would
  ;; know where to restart a tags command.
  (if (boundp 'tags-loop-form)
      (funcall (symbol-function 'set)
               'tags-loop-form '(erlang-find-tag nil t)))
  (save-window-excursion
    (cond
     ((string-match ":$" modtagname)
      ;; Only the module name was given.  Read all files whose file name
      ;; match.
      (let ((modname (substring modtagname 0 (match-beginning 0)))
            (file nil))
        (if (not next-p)
            (save-excursion
              (visit-tags-table-buffer)
              (setq erlang-tags-file-list
                    (funcall (symbol-function 'tags-table-files)))))
        (while (null file)
          (or erlang-tags-file-list
              (save-excursion
                (if (and (funcall
                          (symbol-function 'visit-tags-table-buffer) 'same)
                         (funcall
                          (symbol-function 'visit-tags-table-buffer) t))
                    (setq erlang-tags-file-list
                          (funcall (symbol-function 'tags-table-files)))
                  (error "No %stags containing %s" (if next-p "more " "")
                         modtagname))))
          (if erlang-tags-file-list
              (let ((this-module (erlang-get-module-from-file-name
                                  (car erlang-tags-file-list))))
                (if (and (stringp this-module)
                         (string= modname this-module))
                    (setq file (car erlang-tags-file-list)))
                (setq erlang-tags-file-list (cdr erlang-tags-file-list)))))
        (set-buffer (or (get-file-buffer file)
                        (find-file-noselect file)))))

     ((string-match ":" modtagname)
      (progn
        (erlang-tags-install-module-check)
        (unwind-protect
            (funcall (symbol-function 'find-tag)
                     modtagname next-p regexp-p)
          (erlang-tags-remove-module-check))))
     (t
      (funcall (symbol-function 'find-tag) modtagname next-p regexp-p)))
    (current-buffer)))                  ; Return the new buffer.







;; Process interactive arguments for erlang-find-tag-*.
;;
;; Negative arguments work only for `etags', not `tags'.  This is not
;; a problem since negative arguments means step back into the
;; history list, a feature not implemented in `tags'.

(defun erlang-tag-interactive (prompt)
  (condition-case nil
      (require 'etags)
    (error
     (require 'tags)))
  (if current-prefix-arg
      (list nil (if (< (prefix-numeric-value current-prefix-arg) 0)
                    '-
                  t))
    (let* ((default (erlang-default-function-or-module))
           (prompt (if default
                       (format "%s(default %s) " prompt default)
                     prompt))
           (spec (completing-read prompt 'erlang-tags-complete-tag)))
      (list (if (equal spec "")
                (or default (error "There is no default tag"))
              spec)))))


;; Search tag functions which are aware of Erlang modules.  The tactic
;; is to store new search functions into the local variables of the
;; TAGS buffers.  The variables are restored directly after the
;; search.  The situation is complicated by the fact that new TAGS
;; files can be loaded during the search.
;;

(defun erlang-tags-install-module-check ()
  "Install our own tag search functions."
  ;; Make sure our functions are installed in TAGS files loaded
  ;; into Emacs while searching.
  (setq erlang-tags-orig-format-functions
        (symbol-value 'tags-table-format-functions))
  (funcall (symbol-function 'set) 'tags-table-format-functions
           (cons 'erlang-tags-recognize-tags-table
                 erlang-tags-orig-format-functions))
  (setq erlang-tags-buffer-list '())

  ;; Install our functions in the TAGS files already resident.
  (save-excursion
    (let ((files (symbol-value 'tags-table-computed-list)))
      (while files
        (if (stringp (car files))
            (if (get-file-buffer (car files))
                (progn
                  (set-buffer (get-file-buffer (car files)))
                  (erlang-tags-install-local))))
        (setq files (cdr files))))))


(defun erlang-tags-install-local ()
  "Install our tag search functions in current buffer."
  (if erlang-tags-buffer-installed-p
      ()
    ;; Mark this buffer as "installed" and record.
    (set (make-local-variable 'erlang-tags-buffer-installed-p) t)
    (setq erlang-tags-buffer-list
          (cons (current-buffer) erlang-tags-buffer-list))

    ;; Save the original values.
    (set (make-local-variable 'erlang-tags-orig-tag-order)
         (symbol-value 'find-tag-tag-order))
    (set (make-local-variable 'erlang-tags-orig-regexp-tag-order)
         (symbol-value 'find-tag-regexp-tag-order))
    (set (make-local-variable 'erlang-tags-orig-search-function)
         (symbol-value 'find-tag-search-function))
    (set (make-local-variable 'erlang-tags-orig-regexp-search-function)
         (symbol-value 'find-tag-regexp-search-function))

    ;; Install our own functions.
    (set (make-local-variable 'find-tag-search-function)
         'erlang-tags-search-forward)
    (set (make-local-variable 'find-tag-regexp-search-function)
         'erlang-tags-regexp-search-forward)
    (set (make-local-variable 'find-tag-tag-order)
         (mapcar #'erlang-make-order-function-aware-of-modules
                 erlang-tags-orig-tag-order))
    (set (make-local-variable 'find-tag-regexp-tag-order)
         (mapcar #'erlang-make-order-function-aware-of-modules
                 erlang-tags-orig-regexp-tag-order))))

(defun erlang-make-order-function-aware-of-modules (f)
  `(lambda (tag)
     (let (mod)
       (when (string-match ":" tag)
         (setq mod (substring tag 0 (match-beginning 0)))
         (setq tag (substring tag (match-end 0) nil)))
       (and (funcall ',f tag)
            (or (null mod)
                (erlang-tag-at-point-match-module-p mod))))))

(defun erlang-tag-at-point-match-module-p (mod)
  (string-equal mod (erlang-get-module-from-file-name
                     (funcall (symbol-function 'file-of-tag)))))


(defun erlang-tags-remove-module-check ()
  "Remove our own tags search functions."
  (funcall (symbol-function 'set)
           'tags-table-format-functions
           erlang-tags-orig-format-functions)

  ;; Remove our functions from the TAGS files.  (Note that
  ;; `tags-table-computed-list' need not be the same list as when
  ;; the search was started.)
  (save-excursion
    (let ((buffers erlang-tags-buffer-list))
      (while buffers
        (if (buffer-name (car buffers))
            (progn
              (set-buffer (car buffers))
              (erlang-tags-remove-local)))
        (setq buffers (cdr buffers))))))


(defun erlang-tags-remove-local ()
  "Remove our tag search functions from current buffer."
  (if (null erlang-tags-buffer-installed-p)
      ()
    (funcall (symbol-function 'set) 'erlang-tags-buffer-installed-p nil)
    (funcall (symbol-function 'set)
             'find-tag-tag-order erlang-tags-orig-tag-order)
    (funcall (symbol-function 'set)
             'find-tag-regexp-tag-order erlang-tags-orig-regexp-tag-order)
    (funcall (symbol-function 'set)
             'find-tag-search-function erlang-tags-orig-search-function)
    (funcall (symbol-function 'set)
             'find-tag-regexp-search-function
             erlang-tags-orig-regexp-search-function)))


(defun erlang-tags-recognize-tags-table ()
  "Install our functions in all loaded TAGS files.

This function is added to `tags-table-format-hooks/functions' when searching
for a tag on the form `module:tag'."
  (if (null (funcall (symbol-function 'etags-recognize-tags-table)))
      nil
    (erlang-tags-install-local)
    t))


(defun erlang-tags-search-forward (tag &optional bound noerror count)
  "Forward search function, aware of Erlang module prefix."
  (if (string-match ":" tag)
      (setq tag (substring tag (match-end 0) nil)))
  ;; Avoid unintended recursion.
  (if (eq erlang-tags-orig-search-function 'erlang-tags-search-forward)
      (search-forward tag bound noerror count)
    (funcall erlang-tags-orig-search-function tag bound noerror count)))


(defun erlang-tags-regexp-search-forward (tag &optional bound noerror count)
  "Forward regexp search function, aware of Erlang module prefix."
  (if (string-match ":" tag)
      (setq tag (substring tag (match-end 0) nil)))
  (if (eq erlang-tags-orig-regexp-search-function
          'erlang-tags-regexp-search-forward)
      (re-search-forward tag bound noerror count)
    (funcall erlang-tags-orig-regexp-search-function
             tag bound noerror count)))

;;; Tags completion, Emacs 19 `etags' specific.
;;;
;;; The basic idea is to create a second completion table `erlang-tags-
;;; completion-table' containing all normal tags plus tags on the form
;;; `module:tag' and `module:'.

(if (fboundp 'advice-add)
    ;; Emacs 24.4+
    (progn
      (require 'etags)
      (advice-add 'etags-tags-completion-table :around
                  #'erlang-etags-tags-completion-table-advice))
  ;; Emacs 23.1-24.3
  (defadvice etags-tags-completion-table (around
                                          erlang-replace-tags-table
                                          activate)
    (if erlang-replace-etags-tags-completion-table
        (setq ad-return-value (erlang-etags-tags-completion-table))
      ad-do-it)))

(defun erlang-etags-tags-completion-table-advice (oldfun)
  (if erlang-replace-etags-tags-completion-table
      (erlang-etags-tags-completion-table)
    (funcall oldfun)))

(defun erlang-complete-tag ()
  "Perform tags completion on the text around point.
Completes to the set of names listed in the current tags table.

Should the Erlang tags system be installed this command knows
about Erlang modules."
  (interactive)
  (require 'etags)
  (let ((erlang-replace-etags-tags-completion-table t))
    (complete-tag)))


(defun erlang-find-tag-for-completion ()
  (let ((start (save-excursion
                 (skip-chars-backward "[:word:][:digit:]_:'")
                 (point))))
    (unless (eq start (point))
      (buffer-substring-no-properties start (point)))))


;; Based on `tags-complete-tag', but this one uses
;; `erlang-tags-completion-table' instead of `tags-completion-table'.
;;
;; This is the entry-point called by system function `completing-read'.
;;
;; Used for minibuffer completion in Emacs 19-24 and completion in
;; erlang buffers in Emacs 19-22.
(defun erlang-tags-complete-tag (string predicate what)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (save-excursion
      ;; If we need to ask for the tag table, allow that.
      (let ((enable-recursive-minibuffers t))
        (visit-tags-table-buffer))
      (if (eq what t)
          (all-completions string (erlang-tags-completion-table) predicate)
        (try-completion string (erlang-tags-completion-table) predicate)))))


;; `tags-completion-table' calls itself recursively, make it
;; call our own wedge instead.  Note that the recursive call
;; is very rare;  it only occurs when a tags-file contains
;; `include'-statements.
(defun erlang-tags-completion-table ()
  "Build completion table.  Tags on the form `tag' or `module:tag'."
  (setq erlang-tags-orig-completion-table
        (symbol-function 'tags-completion-table))
  (fset 'tags-completion-table
        (symbol-function 'erlang-tags-completion-table-1))
  (unwind-protect
      (erlang-tags-completion-table-1)
    (fset 'tags-completion-table
          erlang-tags-orig-completion-table)))

(defun erlang-tags-completion-table-1 ()
  (make-local-variable 'erlang-tags-completion-table)
  (or erlang-tags-completion-table
      (let ((tags-completion-table nil)
            (tags-completion-table-function
             'erlang-etags-tags-completion-table))
        (funcall erlang-tags-orig-completion-table)
        (setq erlang-tags-completion-table tags-completion-table))))



;; Emacs 25 expects this function to return a list (and it is ok for
;; it to include duplicates).  Older emacsen expects an obarray.
(defun erlang-etags-tags-completion-table ()
  (if (>= emacs-major-version 25)
      (erlang-etags-tags-completion-table-list)
    (let ((obarray (make-vector 511 0)))
      (dolist (tag (erlang-etags-tags-completion-table-list))
        (intern tag obarray))
      obarray)))

;; Based on `etags-tags-completion-table'.  The difference is that we
;; add three strings to the list, the tag, module: and module:tag.
;; The module is extracted from the file name of a tag.  (This one
;; only works if we are looking at an `etags' file. However, this is
;; the only format supported by Emacs, so far.)
(defun erlang-etags-tags-completion-table-list ()
  (let ((progress-reporter
         (make-progress-reporter
          (format "Making tags completion table for %s..." buffer-file-name)
          (point-min) (point-max)))
        table module)
    (save-excursion
      (goto-char (point-min))
      (while (progn
               (while (and (eq (following-char) ?\f)
                           (looking-at "\f\n\\([^,\n]*\\),.*\n"))
                 (let ((file (buffer-substring (match-beginning 1)
                                               (match-end 1))))
                   (setq module (erlang-get-module-from-file-name file))
                   (when module
                     (push (concat module ":") table)
                     (push (concat module ":module_info") table))
                   (forward-line 2)))
               ;; This regexp matches an explicit tag name or the
               ;; place where it would start.
               (re-search-forward
                "[\f\t\n\r()=,; ]?\177\\\(?:\\([^\n\001]+\\)\001\\)?"
                nil t))
        (let ((tag (if (match-beginning 1)
                       ;; There is an explicit tag name.
                       (buffer-substring (match-beginning 1) (match-end 1))
                     ;; No explicit tag name.  Backtrack a little,
                     ;; and look for the implicit one.
                     (goto-char (match-beginning 0))
                     (skip-chars-backward "^\f\t\n\r()=,; ")
                     (buffer-substring (point) (match-beginning 0)))))
          (forward-line 1)
          (push tag table)
          (when (stringp module)
            (push (concat module ":" tag) table))
          (progress-reporter-update progress-reporter (point)))))
    table))




;;; Xref backend erlang-etags

;; In GNU Emacs 25 xref was introduced.  It is a framework for cross
;; referencing commands, in particular commands for finding
;; definitions.  It does not replace etags.  It rather resides on top
;; of it and provides user-friendly commands.  The idea is that the
;; user commands should be the same regardless of what backend does
;; the actual finding of definitions.

;; The backend below is a wrapper around the built-in etags backend.
;; It adds awareness of the module:tag syntax in a similar way that is
;; done above for the old etags commands.

(defvar erlang-current-arity nil
  "The arity of the function currently being searched.

There is no information about arity in the TAGS file.
Consecutive functions with same name but different arity will
only get one entry in the TAGS file.  Matching TAGS entries are
therefore selected without regarding arity.  The arity is
considered first when it is time to jump to the definition.")

(defun erlang-etags--xref-backend () 'erlang-etags)

(defun erlang-soft-require (feature)
  (when (locate-library (symbol-name feature))
    (require feature)))

(and (erlang-soft-require 'xref)
     (erlang-soft-require 'cl-generic)
     (erlang-soft-require 'eieio)
     (erlang-soft-require 'etags)
     ;; The purpose of using eval here is to avoid compilation
     ;; warnings in emacsen without cl-defmethod etc.
     (eval
      '(progn
         (cl-defmethod xref-backend-identifier-at-point
             ((_backend (eql erlang-etags)))
           (if (eq this-command 'xref-find-references)
               (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                 (thing-at-point 'symbol))
             (erlang-id-to-string (erlang-get-identifier-at-point))))

         (cl-defmethod xref-backend-definitions
             ((_backend (eql erlang-etags)) identifier)
           (erlang-xref-find-definitions identifier))

         (cl-defmethod xref-backend-apropos
             ((_backend (eql erlang-etags)) identifier)
           (erlang-xref-find-definitions identifier t))

         (cl-defmethod xref-backend-identifier-completion-table
             ((_backend (eql erlang-etags)))
           (let ((erlang-replace-etags-tags-completion-table t))
             (tags-completion-table)))

         (defclass erlang-xref-location (xref-etags-location) ())

         (defun erlang-convert-xrefs (xrefs)
           (mapcar (lambda (xref)
                     (oset xref location (erlang-make-location
                                          (oref xref location)))
                     xref)
                   xrefs))

         (defun erlang-make-location (etags-location)
           (with-slots (tag-info file) etags-location
             (make-instance 'erlang-xref-location :tag-info tag-info
                            :file file)))

         (cl-defmethod xref-location-marker ((locus erlang-xref-location))
           (with-slots (tag-info file) locus
             (with-current-buffer (find-file-noselect file)
               (save-excursion
                 (or (erlang-goto-tag-location-by-arity tag-info)
                     (etags-goto-tag-location tag-info))
                 ;; Reset erlang-current-arity.  We want to jump to
                 ;; correct arity in the first attempt.  That is now
                 ;; done.  Possible remaining jumps will be from
                 ;; entries in the *xref* buffer and then we want to
                 ;; ignore the arity.  (Alternatively we could remove
                 ;; all but one xref entry per file when we know the
                 ;; arity).
                 (setq erlang-current-arity nil)
                 (point-marker)))))

         (defun erlang-xref-context (xref)
           (with-slots (tag-info) (xref-item-location xref)
             (car tag-info))))))


(defun erlang-goto-tag-location-by-arity (tag-info)
  (when erlang-current-arity
    (let* ((tag-text (car tag-info))
           (tag-pos (cdr (cdr tag-info)))
           (tag-line (car (cdr tag-info)))
           (regexp (erlang-tag-info-regexp tag-text))
           (startpos (or tag-pos
                         (when tag-line
                           (goto-char (point-min))
                           (forward-line (1- tag-line))
                           (point))
                         (point-min))))
      (setq startpos (max (- startpos 2000)
                          (point-min)))
      (goto-char startpos)
      (let ((pos (or (erlang-search-by-arity regexp)
                     (unless (eq startpos (point-min))
                       (goto-char (point-min))
                       (erlang-search-by-arity regexp)))))
        (when pos
          (goto-char pos)
          t)))))

(defun erlang-tag-info-regexp (tag-text)
  (concat "^"
          (regexp-quote tag-text)
          ;; Erlang function entries in TAGS includes the opening
          ;; parenthesis for the argument list.  Erlang macro entries
          ;; do not.  Add it here in order to end up in correct
          ;; position for erlang-get-arity.
          (if (string-prefix-p "-define" tag-text)
              "\\s-*("
            "")))

(defun erlang-search-by-arity (regexp)
  (let (pos)
    (while (and (null pos)
                (re-search-forward regexp nil t))
      (when (eq erlang-current-arity (save-excursion (erlang-get-arity)))
        (setq pos (point-at-bol))))
    pos))


(defun erlang-xref-find-definitions (identifier &optional is-regexp)
  (erlang-with-id (kind module name arity) identifier
    (setq erlang-current-arity arity)
    (cond ((eq kind 'module)
           (erlang-xref-find-definitions-module name))
          (module
           (erlang-xref-find-definitions-module-tag module
                                                    name
                                                    (eq kind
                                                        'qualified-function)
                                                    is-regexp))
          (t
           (erlang-xref-find-definitions-tag kind name is-regexp)))))

(defun erlang-xref-find-definitions-module (module)
  (and (fboundp 'xref-make)
       (fboundp 'xref-make-file-location)
       (let* ((first-time t)
              (cbuf (current-buffer))
              xrefs matching-files)
         (save-excursion
           (while (erlang-visit-tags-table-buffer (not first-time) cbuf)
             (setq first-time nil)
             (let ((files (tags-table-files)))
               (while files
                 (let* ((file (car files))
                        (m (erlang-get-module-from-file-name file)))
                   (when (and m (string-equal m module))
                     (unless (member file matching-files)
                       (push file
                             matching-files)
                       (push (xref-make file
                                        (xref-make-file-location file 1 0))
                             xrefs))))
                 (setq files (cdr files))))))
         (nreverse xrefs))))

(defun erlang-visit-tags-table-buffer (cont cbuf)
  (if (< emacs-major-version 26)
      (visit-tags-table-buffer cont)
    (visit-tags-table-buffer cont cbuf)))

(defun erlang-xref-find-definitions-module-tag (module
                                                tag
                                                is-qualified
                                                is-regexp)
  "Find definitions of TAG and filter away definitions outside of
MODULE.  If IS-QUALIFIED is nil and no definitions was found inside
the MODULE then return any definitions found outside.  If
IS-REGEXP is non-nil then TAG is a regexp."
  (and (fboundp 'etags--xref-find-definitions)
       (fboundp 'erlang-convert-xrefs)
       (let ((xrefs (erlang-convert-xrefs
                     (etags--xref-find-definitions tag is-regexp)))
             xrefs-in-module)
         (dolist (xref xrefs)
           (when (string-equal module (erlang-xref-module xref))
             (push xref xrefs-in-module)))
         (cond (is-qualified xrefs-in-module)
               (xrefs-in-module xrefs-in-module)
               (t xrefs)))))

(defun erlang-xref-find-definitions-tag (kind tag is-regexp)
  "Find all definitions of TAG and reorder them so that
definitions in the currently visited file comes first."
  (and (fboundp 'etags--xref-find-definitions)
       (fboundp 'erlang-convert-xrefs)
       (let* ((current-file (and (buffer-file-name)
                                 (file-truename (buffer-file-name))))
              (regexp (erlang-etags-regexp kind tag is-regexp))
              (xrefs (erlang-convert-xrefs
                      (etags--xref-find-definitions regexp t)))
              local-xrefs non-local-xrefs)
         (while xrefs
           (let ((xref (car xrefs)))
             (if (string-equal (erlang-xref-truename-file xref)
                               current-file)
                 (push xref local-xrefs)
               (push xref non-local-xrefs))
             (setq xrefs (cdr xrefs))))
         (append (reverse local-xrefs)
                 (reverse non-local-xrefs)))))

(defun erlang-etags-regexp (kind tag is-regexp)
  (let ((tag-regexp (if is-regexp
                        tag
                      (regexp-quote tag))))
    (cond ((eq kind 'record)
           (concat "-record\\s-*(\\s-*" tag-regexp))
          ((eq kind 'macro)
           (concat "-define\\s-*(\\s-*" tag-regexp))
          (t tag-regexp))))


(defun erlang-xref-module (xref)
  (erlang-get-module-from-file-name (erlang-xref-file xref)))

(defun erlang-xref-truename-file (xref)
  (let ((file (erlang-xref-file xref)))
    (and file
         (file-truename file))))

(defun erlang-xref-file (xref)
  (and (fboundp 'xref-location-group)
       (fboundp 'xref-item-location)
       (xref-location-group (xref-item-location xref))))



;;;
;;; Prepare for other methods to run an Erlang slave process.
;;;

(defvar erlang-shell-function 'inferior-erlang
  "Command to execute start a new Erlang shell.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-shell-display-function 'inferior-erlang-run-or-select
  "Command to execute to display Erlang shell.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-compile-function 'inferior-erlang-compile
  "Command to execute to compile current buffer.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-compile-erlang-function "c"
  "Erlang function to call to compile an erlang file.")

(defvar erlang-compile-display-function 'inferior-erlang-run-or-select
  "Command to execute to view last compilation.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-next-error-function 'inferior-erlang-next-error
  "Command to execute to go to the next error.

Change this variable to use your favorite Erlang compilation
package.")


;;;###autoload
(defun erlang-shell ()
  "Start a new Erlang shell.

The variable `erlang-shell-function' decides which method to use,
default is to start a new Erlang host.  It is possible that, in the
future, a new shell on an already running host will be started."
  (interactive)
  (call-interactively erlang-shell-function))


;;;###autoload (autoload 'run-erlang "erlang" "Start a new Erlang shell." t)

;; It is customary for Emacs packages to supply a function on this
;; form, even though it violates the `erlang-*' name convention.
(defalias 'run-erlang 'erlang-shell)


(defun erlang-shell-display ()
  "Display an Erlang shell, or start a new."
  (interactive)
  (call-interactively erlang-shell-display-function))


;;;###autoload
(defun erlang-compile ()
  "Compile Erlang module in current buffer."
  (interactive)
  (call-interactively erlang-compile-function))


(defun erlang-compile-display ()
  "Display compilation output."
  (interactive)
  (call-interactively erlang-compile-display-function))


(defun erlang-next-error ()
  "Display next error message from the latest compilation."
  (interactive)
  (call-interactively erlang-next-error-function))



;;;
;;; Erlang Shell Mode -- Major mode used for Erlang shells.
;;;

;; This mode is designed to be implementation independent,
;; e.g. it does not assume that we are running an inferior
;; Erlang, there exists a lot of other possibilities.

(defvar erlang-shell-buffer-name "*erlang*"
  "The name of the Erlang link shell buffer.")

(defcustom erlang-shell-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :package-version '(erlang . "2.8.0"))

(defvar erlang-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\t"    'erlang-complete-tag)

    ;; Normally the other way around.
    (define-key map "\C-a"     'comint-bol)
    (define-key map "\C-c\C-a" 'beginning-of-line)

    (define-key map "\C-d"     nil)     ; Was `comint-delchar-or-maybe-eof'
    (define-key map "\M-\C-m"  'compile-goto-error)
    map)
  "Keymap used by Erlang shells.")

(defvar erlang-input-ring-file-name "~/.erlang_history"
  "When non-nil, file name used to store Erlang shell history information.")

(define-derived-mode erlang-shell-mode comint-mode "Erlang Shell"
  "Major mode for interacting with an Erlang shell.

The following special commands are available:
\\{erlang-shell-mode-map}"
  (erlang-mode-variables)
  ;; Needed when compiling directly from the Erlang shell.
  (setq compilation-last-buffer (current-buffer))
  (setq comint-prompt-regexp "^[^>=]*> *")
  (make-local-variable 'comint-prompt-read-only)
  (setq comint-prompt-read-only erlang-shell-prompt-read-only)
  (setq comint-eol-on-send t)
  (setq comint-input-ignoredups t)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-scroll-to-bottom-on-output t)
  (add-hook 'comint-output-filter-functions
            'inferior-erlang-strip-delete nil t)
  (add-hook 'comint-output-filter-functions
            'inferior-erlang-strip-ctrl-m nil t)
  (setq comint-input-ring-file-name erlang-input-ring-file-name)
  (comint-read-input-ring t)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring)
  (compilation-minor-mode 1)
  (set (make-local-variable 'minor-mode-overriding-map-alist)
       `((compilation-minor-mode
          . ,(let ((map (make-sparse-keymap)))
               ;; It would be useful to put keymap properties on the
               ;; error lines so that we could use RET and mouse-2
               ;; on them directly.
               (when (boundp 'compilation-skip-threshold) ; new compile.el
                 (define-key map [mouse-2] #'erlang-mouse-2-command)
                 (define-key map "\C-m" #'erlang-RET-command))
               (if (boundp 'compilation-menu-map)
                   (define-key map [menu-bar compilation]
                     (cons "Errors" compilation-menu-map)))
               map))))
  (erlang-tags-init))


(defun erlang-mouse-2-command (event)
  "Command bound to `mouse-2' in inferior Erlang buffer.
Selects Comint or Compilation mode command as appropriate."
  (interactive "e")
  (if (save-window-excursion
        (save-excursion
          (mouse-set-point event)
          (consp (get-text-property (line-beginning-position) 'message))))
      (call-interactively (lookup-key compilation-mode-map [mouse-2]))
    (call-interactively (lookup-key comint-mode-map [mouse-2]))))

(defun erlang-RET-command ()
  "Command bound to `RET' in inferior Erlang buffer.
Selects Comint or Compilation mode command as appropriate."
  (interactive)
  (if (consp (get-text-property (line-beginning-position) 'message))
      (call-interactively (lookup-key compilation-mode-map "\C-m"))
    (call-interactively (lookup-key comint-mode-map "\C-m"))))

;;;
;;; Inferior Erlang -- Run an Erlang shell as a subprocess.
;;;

(defvar inferior-erlang-display-buffer-any-frame nil
  "When nil, `inferior-erlang-display-buffer' use only selected frame.
When t, all frames are searched.  When 'raise, the frame is raised.")

(defvar inferior-erlang-shell-type 'newshell
  "The type of Erlang shell to use.

When this variable is set to the atom `oldshell', the old shell is used.
When set to `newshell' the new shell is used.  Should the variable be
nil, the default shell is used.

This variable influence the setting of other variables.")

(defvar inferior-erlang-machine "erl"
  "The name of the Erlang shell.")

(defvar inferior-erlang-machine-options '()
  "The options used when activating the Erlang shell.

This must be a list of strings.")

(defvar inferior-erlang-process-name "inferior-erlang"
  "The name of the inferior Erlang process.")

(defvar inferior-erlang-buffer-name erlang-shell-buffer-name
  "The name of the inferior Erlang buffer.")

(defvar inferior-erlang-prompt-timeout 60
  "Number of seconds before `inferior-erlang-wait-prompt' timeouts.

The time specified is waited after every output made by the inferior
Erlang shell.  When this variable is t, we assume that we always have
a prompt.  When nil, we will wait forever, or until \\[keyboard-quit].")

(defvar inferior-erlang-process nil
  "Process of last invoked inferior Erlang, or nil.")

(defvar inferior-erlang-buffer nil
  "Buffer of last invoked inferior Erlang, or nil.")

;; Enable uniquifying Erlang shell buffers based on directory name.
(eval-after-load "uniquify"
  '(add-to-list 'uniquify-list-buffers-directory-modes 'erlang-shell-mode))

;;;###autoload
(defun inferior-erlang (&optional command)
  "Run an inferior Erlang.
With prefix command, prompt for command to start Erlang with.

This is just like running Erlang in a normal shell, except that
an Emacs buffer is used for input and output.
\\<comint-mode-map>
The command line history can be accessed with  \\[comint-previous-input]  and  \\[comint-next-input].
The history is saved between sessions.

Entry to this mode calls the functions in the variables
`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{erlang-shell-mode-map}"
  (interactive
   (when current-prefix-arg
     (list (read-shell-command "Erlang command: "))))
  (require 'comint)
  (let (cmd opts)
    (if command
        (setq cmd "sh"
              opts (list "-c" command))
      (setq cmd inferior-erlang-machine
            opts inferior-erlang-machine-options)
      (cond ((eq inferior-erlang-shell-type 'oldshell)
             (setq opts (cons "-oldshell" opts)))
            ((eq inferior-erlang-shell-type 'newshell)
             (setq opts (append '("-newshell" "-env" "TERM" "vt100") opts)))))

    ;; Using create-file-buffer and list-buffers-directory in this way
    ;; makes uniquify give each buffer a unique name based on the
    ;; directory.
    (let ((fake-file-name (expand-file-name inferior-erlang-buffer-name default-directory)))
      (setq inferior-erlang-buffer (create-file-buffer fake-file-name))
      (apply 'make-comint-in-buffer
             inferior-erlang-process-name
             inferior-erlang-buffer
             cmd
             nil opts)
      (with-current-buffer inferior-erlang-buffer
        (setq list-buffers-directory fake-file-name))))

  (setq inferior-erlang-process
        (get-buffer-process inferior-erlang-buffer))
  (funcall (symbol-function 'set-process-query-on-exit-flag)
           inferior-erlang-process nil)
  (if erlang-inferior-shell-split-window
      (switch-to-buffer-other-window inferior-erlang-buffer)
    (switch-to-buffer inferior-erlang-buffer))
  (if (and (not (eq system-type 'windows-nt))
           (eq inferior-erlang-shell-type 'newshell))
      (setq comint-process-echoes t))
  (erlang-shell-mode))


(defun inferior-erlang-run-or-select ()
  "Switch to an inferior Erlang buffer, possibly starting new process."
  (interactive)
  (if (null (inferior-erlang-running-p))
      (inferior-erlang)
    (inferior-erlang-display-buffer t)))


(defun inferior-erlang-display-buffer (&optional select)
  "Make the inferior Erlang process visible.
The window is returned.

Should `inferior-erlang-display-buffer-any-frame' be nil the buffer is
displayed in the current frame.  Should it be non-nil, and the buffer
already is visible in any other frame, no new window will be created.
Should it be the atom 'raise, the frame containing the window will
be raised.

Should the optional argument SELECT be non-nil, the window is
selected.  Should the window be in another frame, that frame is raised.

Note, should the mouse pointer be places outside the raised frame, that
frame will become deselected before the next command."
  (interactive)
  (or (inferior-erlang-running-p)
      (error "No inferior Erlang process is running"))
  (let ((win (inferior-erlang-window
              inferior-erlang-display-buffer-any-frame))
        (frames-p (fboundp 'selected-frame)))
    (if (null win)
        (let ((old-win (selected-window)))
          (save-excursion
            (switch-to-buffer-other-window inferior-erlang-buffer)
            (setq win (selected-window)))
          (select-window old-win))
      (if (and window-system
               frames-p
               (or select
                   (eq inferior-erlang-display-buffer-any-frame 'raise))
               (not (eq (selected-frame) (window-frame win))))
          (raise-frame (window-frame win))))
    (if select
        (select-window win))
    (sit-for 0)
    win))


(defun inferior-erlang-running-p ()
  "Non-nil when an inferior Erlang is running."
  (and inferior-erlang-process
       (memq (process-status inferior-erlang-process) '(run open))
       inferior-erlang-buffer
       (buffer-name inferior-erlang-buffer)))


(defun inferior-erlang-window (&optional all-frames)
  "Return the window containing the inferior Erlang, or nil."
  (and (inferior-erlang-running-p)
       (if all-frames
           (get-buffer-window inferior-erlang-buffer t)
         (get-buffer-window inferior-erlang-buffer))))


(defun inferior-erlang-wait-prompt ()
  "Wait until the inferior Erlang shell prompt appears."
  (if (eq inferior-erlang-prompt-timeout t)
      ()
    (or (inferior-erlang-running-p)
        (error "No inferior Erlang shell is running"))
    (with-current-buffer inferior-erlang-buffer
      (let ((msg nil))
        (while (save-excursion
                 (goto-char (process-mark inferior-erlang-process))
                 (forward-line 0)
                 (not (looking-at comint-prompt-regexp)))
          (if msg
              ()
            (setq msg t)
            (message "Waiting for Erlang shell prompt (press C-g to abort)."))
          (or (accept-process-output inferior-erlang-process
                                     inferior-erlang-prompt-timeout)
              (error "No Erlang shell prompt before timeout")))
        (if msg (message ""))))))

(defun inferior-erlang-send-empty-cmd-unless-already-at-prompt ()
  "If not already at a prompt, try to send an empty cmd to get a prompt.
The empty command resembles hitting RET. This is useful in some
situations, for instance if a crash or error report from sasl
has been printed after the last prompt."
  (with-current-buffer inferior-erlang-buffer
    (if (> (point-max) 1)
        ;; make sure we get a prompt if buffer contains data
        (if (save-excursion
              (goto-char (process-mark inferior-erlang-process))
              (forward-line 0)
              (not (looking-at comint-prompt-regexp)))
            (inferior-erlang-send-command "")))))

(autoload 'comint-send-input "comint")

(defun inferior-erlang-send-command (cmd &optional hist)
  "Send command CMD to the inferior Erlang.

The contents of the current command line (if any) will
be placed at the next prompt.

If optional second argument is non-nil the command is inserted into
the history list.

Return the position after the newly inserted command."
  (or (inferior-erlang-running-p)
      (error "No inferior Erlang process is running"))
  (let ((old-buffer (current-buffer))
        (insert-point (marker-position (process-mark inferior-erlang-process)))
        (insert-length (if comint-process-echoes
                           0
                         (1+ (length cmd)))))
    (set-buffer inferior-erlang-buffer)
    (goto-char insert-point)
    (insert cmd)
    ;; Strange things happened if `comint-eol-on-send' is declared
    ;; in the `let' expression above, but setq:d here. The
    ;; `set-buffer' statement obviously makes the buffer local
    ;; instance of `comint-eol-on-send' shadow this one.
    ;; I'm considering this a bug in Elisp.
    ;;
    ;; This was previously cautioned against in the Lisp manual.  It
    ;; has been sorted out in Emacs 21.  -- fx
    (let ((comint-eol-on-send nil)
          (comint-input-filter (if hist comint-input-filter 'ignore)))
      (comint-send-input nil t))
    ;; Adjust all windows whose points are incorrect.
    (if (null comint-process-echoes)
        (walk-windows
         (function
          (lambda (window)
            (if (and (eq (window-buffer window) inferior-erlang-buffer)
                     (= (window-point window) insert-point))
                (set-window-point window
                                  (+ insert-point insert-length)))))
         nil t))
    (set-buffer old-buffer)
    (+ insert-point insert-length)))


(defun inferior-erlang-strip-delete (&optional s)
  "Remove `^H' (delete) and the characters it was supposed to remove."
  (interactive)
  (if (and (boundp 'comint-last-input-end)
           (boundp 'comint-last-output-start))
      (save-excursion
        (goto-char
         (if (called-interactively-p 'interactive)
             (symbol-value 'comint-last-input-end)
           (symbol-value 'comint-last-output-start)))
        (while (progn (skip-chars-forward "^\C-h")
                      (not (eq (point) (point-max))))
          (delete-char 1)
          (or (bolp)
              (backward-delete-char 1))))))


;; Basically `comint-strip-ctrl-m', with a few extra checks.
(defun inferior-erlang-strip-ctrl-m (&optional string)
  "Strip trailing `^M' characters from the current output group."
  (interactive)
  (if (and (boundp 'comint-last-input-end)
           (boundp 'comint-last-output-start))
      (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
        (save-excursion
          (goto-char
           (if (called-interactively-p 'interactive)
               (symbol-value 'comint-last-input-end)
             (symbol-value 'comint-last-output-start)))
          (while (re-search-forward "\r+$" pmark t)
            (replace-match "" t t))))))


(defun inferior-erlang-compile (arg)
  "Compile the file in the current buffer.

With prefix arg, compiles for debug.

Should Erlang return `{error, nofile}' it could not load the object
module after completing the compilation.  This is due to a bug in the
compile command `c' when using the option `outdir'.

There exists two workarounds for this bug:

  1) Place the directory in the Erlang load path.

  2) Set the Emacs variable `erlang-compile-use-outdir' to nil.
     To do so, place the following line in your `~/.emacs'-file:
        (setq erlang-compile-use-outdir nil)"
  (interactive "P")
  (save-some-buffers)
  (inferior-erlang-prepare-for-input)
  (let* ((dir (inferior-erlang-compile-outdir))
         (noext (substring (erlang-local-buffer-file-name) 0 -4))
         (opts (append (list (cons 'outdir dir))
                       (if current-prefix-arg
                           (list 'debug_info 'export_all))
                       erlang-compile-extra-opts))
         end)
    (with-current-buffer inferior-erlang-buffer
      (when (fboundp 'compilation-forget-errors)
        (compilation-forget-errors)))
    (setq end (inferior-erlang-send-command
               (inferior-erlang-compute-compile-command noext opts)
               nil))
    (sit-for 0)
    (inferior-erlang-wait-prompt)
    (with-current-buffer inferior-erlang-buffer
      (setq compilation-error-list nil)
      (set-marker compilation-parsing-end end))
    (setq compilation-last-buffer inferior-erlang-buffer)))

(defun inferior-erlang-prepare-for-input (&optional no-display)
  "Create an inferior erlang buffer if needed and ready it for input.
The buffer is displayed, according to `inferior-erlang-display-buffer'
unless the optional NO-DISPLAY is non-nil."
  (or (inferior-erlang-running-p)
      (save-excursion
        (inferior-erlang)))
  (or (inferior-erlang-running-p)
      (error "Error starting inferior Erlang shell"))
  (if (not no-display)
      (inferior-erlang-display-buffer))
  (inferior-erlang-send-empty-cmd-unless-already-at-prompt)
  (sit-for 0)
  (inferior-erlang-wait-prompt))

(defun inferior-erlang-compile-outdir ()
  "Return the directory to compile the current buffer into."
  (let* ((buffer-dir (directory-file-name
                      (file-name-directory (erlang-local-buffer-file-name))))
         (parent-dir (directory-file-name
                      (file-name-directory buffer-dir)))
         (ebin-dir (concat (file-name-as-directory parent-dir) "ebin"))
         (buffer-dir-base-name (file-name-nondirectory
                                (expand-file-name
                                 (concat (file-name-as-directory buffer-dir)
                                         ".")))))
    (if (and (string= buffer-dir-base-name "src")
             (file-directory-p ebin-dir))
        (file-name-as-directory ebin-dir)
      (file-name-as-directory buffer-dir))))

(defun inferior-erlang-compute-compile-command (module-name opts)
  (let ((ccfn erlang-compile-command-function-alist)
        (res (inferior-erlang-compute-erl-compile-command module-name opts))
        ccfn-entry
        done
        result)
    (if (not (null (erlang-local-buffer-file-name)))
        (while (and (not done) (not (null ccfn)))
          (setq ccfn-entry (car ccfn))
          (setq ccfn (cdr ccfn))
          (if (string-match (car ccfn-entry) (erlang-local-buffer-file-name))
              (let ((c-fn (cdr ccfn-entry)))
                (setq done t)
                (if (not (null c-fn))
                    (setq result (funcall c-fn module-name opts)))))))
    result))

(defun inferior-erlang-compute-erl-compile-command (module-name opts)
  (let* ((out-dir-opt (assoc 'outdir opts))
         (out-dir     (cdr out-dir-opt)))
    (if erlang-compile-use-outdir
        (format "%s(\"%s\"%s)."
                erlang-compile-erlang-function
                module-name
                (inferior-erlang-format-comma-opts opts))
      (let (;; Hopefully, noone else will ever use these...
            (tmpvar "Tmp7236")
            (tmpvar2 "Tmp8742"))
        (format
         (concat
          "f(%s), {ok, %s} = file:get_cwd(), "
          "file:set_cwd(\"%s\"), "
          "%s = %s(\"%s\"%s), file:set_cwd(%s), f(%s), %s.")
         tmpvar2 tmpvar
         out-dir
         tmpvar2
         erlang-compile-erlang-function
         module-name (inferior-erlang-format-comma-opts
                      (remq out-dir-opt opts))
         tmpvar tmpvar tmpvar2)))))

(defun inferior-erlang-compute-leex-compile-command (module-name opts)
  (let ((file-name        (erlang-local-buffer-file-name))
        (erl-compile-expr (inferior-erlang-remove-any-trailing-dot
                           (inferior-erlang-compute-erl-compile-command
                            module-name opts))))
    (format (concat "f(LErr1__), f(LErr2__), "
                    "case case leex:file(\"%s\", [%s]) of"
                    " ok -> ok;"
                    " {ok,_} -> ok;"
                    " {ok,_,_} -> ok;"
                    " LErr1__ -> LErr1__ "
                    "end of"
                    " ok -> %s;"
                    " LErr2__ -> LErr2__ "
                    "end.")
            file-name
            (inferior-erlang-format-comma-opts erlang-leex-compile-opts)
            erl-compile-expr)))

(defun inferior-erlang-compute-yecc-compile-command (module-name opts)
  (let ((file-name        (erlang-local-buffer-file-name))
        (erl-compile-expr (inferior-erlang-remove-any-trailing-dot
                           (inferior-erlang-compute-erl-compile-command
                            module-name opts))))
    (format (concat "f(YErr1__), f(YErr2__), "
                    "case case yecc:file(\"%s\", [%s]) of"
                    " {ok,_} -> ok;"
                    " {ok,_,_} -> ok;"
                    " YErr1__ -> YErr1__ "
                    "end of"
                    " ok -> %s;"
                    " YErr2__ -> YErr2__ "
                    "end.")
            file-name
            (inferior-erlang-format-comma-opts erlang-yecc-compile-opts)
            erl-compile-expr)))

(defun inferior-erlang-remove-any-trailing-dot (str)
  (if (string= (substring str -1) ".")
      (substring str 0 (1- (length str)))
    str))

(defun inferior-erlang-format-comma-opts (opts)
  (if (null opts)
      ""
    (concat ", " (inferior-erlang-format-opt opts))))

(defun inferior-erlang-format-opt (opt)
  (cond ((stringp opt) (concat "\"" opt "\""))
        ((vectorp opt) (inferior-erlang-tuple (append opt nil)))
        ((atom opt)    (format "%s" opt))
        ((consp opt)   (if (listp (cdr opt))
                           (inferior-erlang-list opt)
                         (inferior-erlang-tuple (list (car opt) (cdr opt)))))
        (t (error "Unexpected erlang compile option %s" opt))))

(defun inferior-erlang-tuple (opts)
  (concat "{" (mapconcat 'inferior-erlang-format-opt
                         opts
                         ", ")
          "}"))

(defun inferior-erlang-list (opts)
  (concat "[" (mapconcat 'inferior-erlang-format-opt
                         opts
                         ", ")
          "]"))


(defun erlang-local-buffer-file-name ()
  ;; When editing a file remotely via tramp,
  ;; the buffer's file name may be for example
  ;; "/ssh:host.example.com:/some/path/x.erl"
  ;;
  ;; If I try to compile such a file using C-c C-k, an
  ;; erlang shell on the remote host is automatically
  ;; started if needed, but for it to successfully compile
  ;; the file, the c(...)  command that is sent must contain
  ;; the file name "/some/path/x.erl" without the
  ;; tramp-prefix "/ssh:host.example.com:".
  (cond ((null (buffer-file-name))
         nil)
        ((erlang-tramp-remote-file-p)
         (erlang-tramp-get-localname))
        (t
         (buffer-file-name))))

(defun erlang-tramp-remote-file-p ()
  (and (fboundp 'tramp-tramp-file-p)
       (tramp-tramp-file-p (buffer-file-name))))

(defun erlang-tramp-get-localname ()
  (when (fboundp 'tramp-dissect-file-name)
    (let ((tramp-info (tramp-dissect-file-name (buffer-file-name))))
      (if (fboundp 'tramp-file-name-localname)
          (tramp-file-name-localname tramp-info)
        ;; In old versions of tramp, it was `tramp-file-name-path'
        ;; instead of the newer `tramp-file-name-localname'
        (when (fboundp 'tramp-file-name-path)
          (tramp-file-name-path tramp-info))))))

;; `next-error' only accepts buffers with major mode `compilation-mode'
;; or with the minor mode `compilation-minor-mode' activated.
;; (To activate the minor mode is out of the question, since it will
;; ruin the inferior Erlang keymap.)
;; This is done differently in Emacs 21.
(defun inferior-erlang-next-error (&optional argp)
  "Just like `next-error'.
Capable of finding error messages in an inferior Erlang buffer."
  (interactive "P")
  (let ((done nil)
        (buf (or (and (boundp 'next-error-last-buffer)
                      next-error-last-buffer)
                 (and (boundp 'compilation-last-buffer)
                      compilation-last-buffer))))
    (if (and (bufferp buf)
             (with-current-buffer buf
               (and (eq major-mode 'erlang-shell-mode)
                    (setq major-mode 'compilation-mode))))
        (unwind-protect
            (progn
              (setq done t)
              (next-error argp))
          (with-current-buffer buf
            (setq major-mode 'erlang-shell-mode))))
    (or done
        (next-error argp))))


(defun inferior-erlang-change-directory (&optional dir)
  "Make the inferior Erlang change directory.
The default is to go to the directory of the current buffer."
  (interactive)
  (or dir (setq dir (file-name-directory (erlang-local-buffer-file-name))))
  (or (inferior-erlang-running-p)
      (error "No inferior Erlang is running"))
  (inferior-erlang-display-buffer)
  (inferior-erlang-send-empty-cmd-unless-already-at-prompt)
  (inferior-erlang-wait-prompt)
  (inferior-erlang-send-command (format "cd('%s')." dir) nil))

(defun erlang-align-arrows (start end)
  "Align arrows (\"->\") in function clauses from START to END.
When called interactively, aligns arrows after function clauses inside
the region.

With a prefix argument, aligns all arrows, not just those in function
clauses.

Example:

sum(L) -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum) -> Sum.

becomes:

sum(L)          -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum."
  (interactive "r")
  (save-excursion
    (let (;; regexp for matching arrows. without a prefix argument,
          ;; the regexp matches function heads. With a prefix, it
          ;; matches any arrow.
          (re (if current-prefix-arg
                  "^.*\\(\\)->"
                (eval-when-compile
                  (concat "^" erlang-atom-regexp ".*\\(\\)->"))))
          ;; part of regexp matching directly before the arrow
          (arrow-match-pos (if current-prefix-arg
                               1
                             (1+ erlang-atom-regexp-matches)))
          ;; accumulator for positions where arrows are found, ordered
          ;; by buffer position (from greatest to smallest)
          (arrow-positions '())
          ;; accumulator for longest distance from start of line to arrow
          (most-indent 0)
          ;; marker to track the end of the region we're aligning
          (end-marker (progn (goto-char end)
                             (point-marker))))
      ;; Pass 1: Find the arrow positions, adjust the whitespace
      ;; before each arrow to one space, and find the greatest
      ;; indentation level.
      (goto-char start)
      (while (re-search-forward re end-marker t)
        (goto-char (match-beginning arrow-match-pos))
        (just-one-space)                ; adjust whitespace
        (setq arrow-positions (cons (point) arrow-positions))
        (setq most-indent (max most-indent (erlang-column-number))))
      (set-marker end-marker nil)       ; free the marker
      ;; Pass 2: Insert extra padding so that all arrow indentation is
      ;; equal. This is done last-to-first by buffer position, so that
      ;; inserting spaces before one arrow doesn't change the
      ;; positions of the next ones.
      (mapc (lambda (arrow-pos)
              (goto-char arrow-pos)
              (let* ((pad (- most-indent (erlang-column-number))))
                (when (> pad 0)
                  (insert-char ?\  pad))))
            arrow-positions))))

(defun erlang-column-number ()
  "Return the column number of the current position in the buffer.
Tab characters are counted by their visual width."
  (string-width (buffer-substring (line-beginning-position) (point))))

(defun erlang-current-defun ()
  "`add-log-current-defun-function' for Erlang."
  (save-excursion
    (erlang-beginning-of-function)
    (if (looking-at "[a-z0-9_]+")
        (match-string 0))))

(defconst erlang-unload-hook
  (list (lambda ()
          (ad-unadvise 'Man-notify-when-ready)
          (ad-unadvise 'set-visited-file-name))))

;; The end...

(provide 'erlang)

(run-hooks 'erlang-load-hook)

;; Local variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; erlang.el ends here
