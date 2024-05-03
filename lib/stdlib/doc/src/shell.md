The Erlang shell.

The shell is a user interface program for entering expression sequences. The
expressions are evaluated and a value is returned. The shell provides an Emacs
like set of shortcuts for editing the text of the current line. See
[tty - A Command-Line Interface](`e:erts:tty.md`) in the ERTS User's Guide for a
list of all available shortcuts. You may also change the shortcuts to suit your
preferences more, see [edlin - line editor in the shell](`m:edlin`).

A history mechanism saves previous commands and their values, which can then be
incorporated in later commands. How many commands and results to save can be
determined by the user, either interactively, by calling `history/1` and
`results/1`, or by setting the application configuration parameters
[`shell_history_length`](stdlib_app.md#shell_history_length) and
[`shell_saved_results`](stdlib_app.md#shell_saved_results) for the STDLIB
application. The shell history can be saved to disk by setting the application
configuration parameter
[`shell_history`](`e:kernel:kernel_app.md#shell_history`) for the Kernel
application.

The shell uses a helper process for evaluating commands to protect the history
mechanism from exceptions. By default the evaluator process is killed when an
exception occurs, but by calling `catch_exception/1` or by setting the
application configuration parameter `shell_catch_exception` for the STDLIB
application this behavior can be changed. See also the example below.

Variable bindings, and local process dictionary changes that are generated in
user expressions are preserved, and the variables can be used in later commands
to access their values. The bindings can also be forgotten so the variables can
be reused.

The special shell commands all have the syntax of (local) function calls. They
are evaluated as normal function calls and many commands can be used in one
expression sequence.

If a command (local function call) is not recognized by the shell, an attempt is
first made to find the function in module `user_default`, where customized local
commands can be placed. If found, the function is evaluated, otherwise an
attempt is made to evaluate the function in module `shell_default`. Module
`user_default` must be explicitly loaded.

The shell also permits the user to start multiple concurrent jobs. A job can be
regarded as a set of processes that can communicate with the shell.

There is some support for reading and printing records in the shell. During
compilation record expressions are translated to tuple expressions. In runtime
it is not known whether a tuple represents a record, and the record definitions
used by the compiler are unavailable at runtime. So, to read the record syntax
and print tuples as records when possible, record definitions must be maintained
by the shell itself.

The shell commands for reading, defining, forgetting, listing, and printing
records are described below. Notice that each job has its own set of record
definitions. To facilitate matters, record definitions in modules
`shell_default` and `user_default` (if loaded) are read each time a new job is
started. For example, adding the following line to `user_default` makes the
definition of `file_info` readily available in the shell:

```erlang
-include_lib("kernel/include/file.hrl").
```

The shell runs in two modes:

- `Normal (possibly restricted)` mode, in which commands can be edited and
  expressions evaluated
- Job Control Mode, `JCL`, in which jobs can be started, killed, detached, and
  connected

Only the currently connected job can 'talk' to the shell.

## Shell Commands

The commands below are the built-in shell commands that are always available. In
most system the commands listed in the `m:c` module are also available in the
shell.

- **`b()`** - Prints the current variable bindings.

- **`f()`** - Removes all variable bindings.

- **`f(X)`** - Removes the binding of variable `X`.

  > #### Note {: .info }
  >
  > If a huge value is stored in a variable binding, you have to both call
  > `f(X)` and call [`history(0)`](`history/1`) or [`results(0)`](`results/1`)
  > to free up that memory.

- **`h()`** - Prints the history list.

- **[`history(N)`](`history/1`)** - Sets the number of previous commands to keep
  in the history list to `N`. The previous number is returned. Defaults to 20.

- **[`results(N)`](`results/1`)** - Sets the number of results from previous
  commands to keep in the history list to `N`. The previous number is returned.
  Defaults to 20.

- **`e(N)`** - Repeats command `N`, if `N` is positive. If it is negative, the
  `N`th previous command is repeated (that is, `e(-1)` repeats the previous
  command).

- **`v(N)`** - Uses the return value of command `N` in the current command, if
  `N` is positive. If it is negative, the return value of the `N`th previous
  command is used (that is, `v(-1)` uses the value of the previous command).

- **`help()`** - Evaluates `shell_default:help()`.

- **`h(Module, Function)`** - Print the documentation for `Module:Function` in
  the shell if available.

- **`ht(Module, Type)`** - Print the documentation for `Module:Type` in the
  shell if available.

- **`hcb(Module, Callback)`** - Print the documentation for `Module:Callback` in
  the shell if available.

- **`c(Mod)`** - Evaluates `shell_default:c(Mod)`. This compiles and loads the
  module `Mod` and purges old versions of the code, if necessary. `Mod` can be
  either a module name or a a source file path, with or without `.erl`
  extension.

- **[`catch_exception(Bool)`](`catch_exception/1`)** - Sets the exception
  handling of the evaluator process. The previous exception handling is
  returned. The default (`false`) is to kill the evaluator process when an
  exception occurs, which causes the shell to create a new evaluator process.
  When the exception handling is set to `true`, the evaluator process lives on.
  This means, for example, that ports and ETS tables as well as processes linked
  to the evaluator process survive the exception.

- **`rd(RecordName, RecordDefinition)`** - Defines a record in the shell.
  `RecordName` is an atom and `RecordDefinition` lists the field names and the
  default values. Usually record definitions are made known to the shell by use
  of the `rr/1,2,3` commands described below, but sometimes it is handy to
  define records on the fly.

- **`rf()`** - Removes all record definitions, then reads record definitions
  from the modules `shell_default` and `user_default` (if loaded). Returns the
  names of the records defined.

- **`rf(RecordNames)`** - Removes selected record definitions. `RecordNames` is
  a record name or a list of record names. To remove all record definitions, use
  `'_'`.

- **`rl()`** - Prints all record definitions.

- **`rl(RecordNames)`** - Prints selected record definitions. `RecordNames` is a
  record name or a list of record names.

- **`rp(Term)`** - Prints a term using the record definitions known to the
  shell. All of `Term` is printed; the depth is not limited as is the case when
  a return value is printed.

- **`rr(Module)`** - Reads record definitions from a module's BEAM file. If
  there are no record definitions in the BEAM file, the source file is located
  and read instead. Returns the names of the record definitions read. `Module`
  is an atom.

- **`rr(Wildcard)`** - Reads record definitions from files. Existing definitions
  of any of the record names read are replaced. `Wildcard` is a wildcard string
  as defined in `m:filelib`, but not an atom.

- **`rr(WildcardOrModule, RecordNames)`** - Reads record definitions from files
  but discards record names not mentioned in `RecordNames` (a record name or a
  list of record names).

- **`rr(WildcardOrModule, RecordNames, Options)`** - Reads record definitions
  from files. The compiler options `{i, Dir}`, `{d, Macro}`, and
  `{d, Macro, Value}` are recognized and used for setting up the include path
  and macro definitions. To read all record definitions, use `'_'` as value of
  `RecordNames`.

- **`lf()`** - Outputs locally defined function with function specs if they
  exist.

- **`lt()`** - Outputs locally defined types.

- **`lr()`** - Outputs locally defined records.

- **`ff()`** - Forget locally defined functions (including function specs if
  they exist).

- **`ff({FunName,Arity})`** - Forget a locally defined function (including
  function spec if it exist). Where `FunName` is the name of the function as an
  atom and `Arity` is an integer.

- **`tf()`** - Forget locally defined types.

- **`tf(Type)`** - Forget locally defined type where `Type` is the name of the
  type represented as an atom.

- **`fl()`** - Forget locally defined functions, types and records.

- **`save_module(FilePath)`** - Saves all locally defined functions, types and
  records to a module file, where `FilePath` should include both the path to the
  file and the name of the module with `.erl` suffix.

  Example: `src/my_module.erl`

## Example

The following example is a long dialog with the shell. Commands starting with
`>` are inputs to the shell. All other lines are output from the shell.

```erlang
strider 1> erl
Erlang (BEAM) emulator version 5.3 [hipe] [threads:0]

Eshell V5.3  (abort with ^G)
1> Str = "abcd".
"abcd"
```

Command 1 sets variable `Str` to string `"abcd"`.

```erlang
2> L = length(Str).
4
```

Command 2 sets `L` to the length of string `Str`.

```erlang
3> Descriptor = {L, list_to_atom(Str)}.
{4,abcd}
```

Command 3 builds the tuple `Descriptor`, evaluating the BIF
[`list_to_atom/1` ](`erlang:list_to_atom/1`).

```erlang
4> L.
4
```

Command 4 prints the value of variable `L`.

```erlang
5> b().
Descriptor = {4,abcd}
L = 4
Str = "abcd"
ok
```

Command 5 evaluates the internal shell command `b()`, which is an abbreviation
of "bindings". This prints the current shell variables and their bindings. `ok`
at the end is the return value of function `b()`.

```erlang
6> f(L).
ok
```

Command 6 evaluates the internal shell command `f(L)` (abbreviation of
"forget"). The value of variable `L` is removed.

```erlang
7> b().
Descriptor = {4,abcd}
Str = "abcd"
ok
```

Command 7 prints the new bindings.

```erlang
8> f(L).
ok
```

Command 8 has no effect, as `L` has no value.

```erlang
9> {L, _} = Descriptor.
{4,abcd}
```

Command 9 performs a pattern matching operation on `Descriptor`, binding a new
value to `L`.

```erlang
10> L.
4
```

Command 10 prints the current value of `L`.

```erlang
11> {P, Q, R} = Descriptor.
** exception error: no match of right hand side value {4,abcd}
```

Command 11 tries to match `{P, Q, R}` against `Descriptor`, which is `{4, abc}`.
The match fails and none of the new variables become bound. The printout
starting with "`** exception error:`" is not the value of the expression (the
expression had no value because its evaluation failed), but a warning printed by
the system to inform the user that an error has occurred. The values of the
other variables (`L`, `Str`, and so on) are unchanged.

```erlang
12> P.
* 1:1: variable 'P' is unbound
13> Descriptor.
{4,abcd}
```

Commands 12 and 13 show that `P` is unbound because the previous command failed,
and that `Descriptor` has not changed.

```erlang
14>{P, Q} = Descriptor.
{4,abcd}
15> P.
4
```

Commands 14 and 15 show a correct match where `P` and `Q` are bound.

```erlang
16> f().
ok
```

Command 16 clears all bindings.

The next few commands assume that `test1:demo(X)` is defined as follows:

```erlang
demo(X) ->
    put(aa, worked),
    X = 1,
    X + 10.
```

```erlang
17> put(aa, hello).
undefined
18> get(aa).
hello
```

Commands 17 and 18 set and inspect the value of item `aa` in the process
dictionary.

```erlang
19> Y = test1:demo(1).
11
```

Command 19 evaluates `test1:demo(1)`. The evaluation succeeds and the changes
made in the process dictionary become visible to the shell. The new value of
dictionary item `aa` can be seen in command 20.

```erlang
20> get().
[{aa,worked}]
21> put(aa, hello).
worked
22> Z = test1:demo(2).
** exception error: no match of right hand side value 1
     in function  test1:demo/1
```

Commands 21 and 22 change the value of dictionary item `aa` to `hello` and call
`test1:demo(2)`. Evaluation fails and the changes made to the dictionary in
`test1:demo(2)`, before the error occurred, are discarded.

```erlang
23> Z.
* 1:1: variable 'Z' is unbound
24> get(aa).
hello
```

Commands 23 and 24 show that `Z` was not bound and that dictionary item `aa` has
retained its original value.

```erlang
25> erase(), put(aa, hello).
undefined
26> spawn(test1, demo, [1]).
<0.57.0>
27> get(aa).
hello
```

Commands 25, 26, and 27 show the effect of evaluating `test1:demo(1)` in the
background. In this case, the expression is evaluated in a newly spawned
process. Any changes made in the process dictionary are local to the newly
spawned process and therefore not visible to the shell.

```erlang
28> io:format("hello hello\n").
hello hello
ok
29> e(28).
hello hello
ok
30> v(28).
ok
```

Commands 28, 29 and 30 use the history facilities of the shell. Command 29
re-evaluates command 28. Command 30 uses the value (result) of command 28. In
the cases of a pure function (a function with no side effects), the result is
the same. For a function with side effects, the result can be different.

The next few commands show some record manipulation. It is assumed that `ex.erl`
defines a record as follows:

`-record(rec, {a, b = val()}).`

`val() ->`  
    `3.`

```erlang
31> c(ex).
{ok,ex}
32> rr(ex).
[rec]
```

Commands 31 and 32 compile file `ex.erl` and read the record definitions in
`ex.beam`. If the compiler did not output any record definitions on the BEAM
file, `rr(ex)` tries to read record definitions from the source file instead.

```erlang
33> rl(rec).
-record(rec,{a,b = val()}).
ok
```

Command 33 prints the definition of the record named `rec`.

```erlang
34> #rec{}.
** exception error: undefined shell command val/0
```

Command 34 tries to create a `rec` record, but fails as function `val/0` is
undefined.

```erlang
35> #rec{b = 3}.
#rec{a = undefined,b = 3}
```

Command 35 shows the workaround: explicitly assign values to record fields that
cannot otherwise be initialized.

```erlang
36> rp(v(-1)).
#rec{a = undefined,b = 3}
ok
```

Command 36 prints the newly created record using record definitions maintained
by the shell.

```erlang
37> rd(rec, {f = orddict:new()}).
rec
```

Command 37 defines a record directly in the shell. The definition replaces the
one read from file `ex.beam`.

```erlang
38> #rec{}.
#rec{f = []}
ok
```

Command 38 creates a record using the new definition, and prints the result.

```erlang
39> rd(rec, {c}), A.
* 1:15: variable 'A' is unbound
40> #rec{}.
#rec{c = undefined}
ok
```

Command 39 and 40 show that record definitions are updated as side effects. The
evaluation of the command fails, but the definition of `rec` has been carried
out.

For the next command, it is assumed that `test1:loop(N)` is defined as follows:

`loop(N) ->`  
    `io:format("Hello Number: ~w~n", [N]),`  
    `loop(N+1).`

```text
41> test1:loop(0).
Hello Number: 0
Hello Number: 1
Hello Number: 2
Hello Number: 3

User switch command
 --> i
 --> c
.
.
.
Hello Number: 3374
Hello Number: 3375
Hello Number: 3376
Hello Number: 3377
Hello Number: 3378
** exception exit: killed
```

Command 41 evaluates `test1:loop(0)`, which puts the system into an infinite
loop. At this point the user types `^G` (Control G), which suspends output from
the current process, which is stuck in a loop, and activates `JCL` mode. In
`JCL` mode the user can start and stop jobs.

In this particular case, command `i` ("interrupt") terminates the looping
program, and command `c` connects to the shell again. As the process was running
in the background before we killed it, more printouts occur before message
"`** exception exit: killed`" is shown.

```erlang
42> E = ets:new(t, []).
#Ref<0.1662103692.2407923716.214192>
```

Command 42 creates an ETS table.

```erlang
43> ets:insert({d,1,2}).
** exception error: undefined function ets:insert/1
```

Command 43 tries to insert a tuple into the ETS table, but the first argument
(the table) is missing. The exception kills the evaluator process.

```erlang
44> ets:insert(E, {d,1,2}).
** exception error: argument is of wrong type
     in function  ets:insert/2
        called as ets:insert(16,{d,1,2})
```

Command 44 corrects the mistake, but the ETS table has been destroyed as it was
owned by the killed evaluator process.

```erlang
45> f(E).
ok
46> catch_exception(true).
false
```

Command 46 sets the exception handling of the evaluator process to `true`. The
exception handling can also be set when starting Erlang by
`erl -stdlib shell_catch_exception true`.

```erlang
47> E = ets:new(t, []).
#Ref<0.1662103692.2407923716.214197>
48> ets:insert({d,1,2}).
* exception error: undefined function ets:insert/1
```

Command 48 makes the same mistake as in command 43, but this time the evaluator
process lives on. The single star at the beginning of the printout signals that
the exception has been caught.

```erlang
49> ets:insert(E, {d,1,2}).
true
```

Command 49 successfully inserts the tuple into the ETS table.

```erlang
50> ets:insert(#Ref<0.1662103692.2407923716.214197>, {e,3,4}).
true
```

Command 50 inserts another tuple into the ETS table. This time the first
argument is the table identifier itself. The shell can parse commands with pids
(`<0.60.0>`), ports (`#Port<0.536>`), references
(`#Ref<0.1662103692.2407792644.214210>`), and external functions
(`#Fun<a.b.1>`), but the command fails unless the corresponding pid, port,
reference, or function can be created in the running system.

```erlang
51> halt().
strider 2>
```

Command 51 exits the Erlang runtime system.

## JCL Mode

When the shell starts, it starts a single evaluator process. This process,
together with any local processes that it spawns, is referred to as a `job`.
Only the current job, which is said to be `connected`, can perform operations
with standard I/O. All other jobs, which are said to be `detached`, are
`blocked` if they attempt to use standard I/O.

All jobs that do not use standard I/O run in the normal way.

The shell escape key `^G` (Control G) detaches the current job and activates
`JCL` mode. The `JCL` mode prompt is `"-->"`. If `"?"` is entered at the prompt,
the following help message is displayed:

```text
--> ?
c [nn]            - connect to job
i [nn]            - interrupt job
k [nn]            - kill job
j                 - list all jobs
s [shell]         - start local shell
r [node [shell]]  - start remote shell
q                 - quit erlang
? | h             - this message
```

The `JCL` commands have the following meaning:

- **`c [nn]`** - Connects to job number `<nn>` or the current job. The standard
  shell is resumed. Operations that use standard I/O by the current job are
  interleaved with user inputs to the shell.

- **`i [nn]`** - Stops the current evaluator process for job number `nn` or the
  current job, but does not kill the shell process. So, any variable bindings
  and the process dictionary are preserved and the job can be connected again.
  This command can be used to interrupt an endless loop.

- **`k [nn]`** - Kills job number `nn` or the current job. All spawned processes
  in the job are killed, provided they have not evaluated the `group_leader/1`
  BIF and are located on the local machine. Processes spawned on remote nodes
  are not killed.

- **`j`** - Lists all jobs. A list of all known jobs is printed. The current job
  name is prefixed with '\*'.

- **`s`** - Starts a new job. This is assigned the new index `[nn]`, which can
  be used in references.

- **`s [shell]`** - Starts a new job. This is assigned the new index `[nn]`,
  which can be used in references. If optional argument `shell` is specified, it
  is assumed to be a module that implements an alternative shell.

- **`r [node]`** - Starts a remote job on `node`. This is used in distributed
  Erlang to allow a shell running on one node to control a number of
  applications running on a network of nodes. If optional argument `shell` is
  specified, it is assumed to be a module that implements an alternative shell.

- **`q`** - Quits Erlang. Notice that this option is disabled if Erlang is
  started with the ignore break, `+Bi`, system flag (which can be useful, for
  example when running a restricted shell, see the next section).

- **`?`** - Displays the help message above.

The behavior of shell escape can be changed by the STDLIB application variable
`shell_esc`. The value of the variable can be either `jcl`
(`erl -stdlib shell_esc jcl`) or `abort` (`erl -stdlib shell_esc abort`). The
first option sets `^G` to activate `JCL` mode (which is also default behavior).
The latter sets `^G` to terminate the current shell and start a new one. `JCL`
mode cannot be invoked when `shell_esc` is set to `abort`.

If you want an Erlang node to have a remote job active from the start (rather
than the default local job), start Erlang with flag
[`-remsh`](`e:erts:erl_cmd.md#remsh`), for example,
`erl -remsh other_node@other_host`

## Restricted Shell

The shell can be started in a restricted mode. In this mode, the shell evaluates
a function call only if allowed. This feature makes it possible to, for example,
prevent a user from accidentally calling a function from the prompt that could
harm a running system (useful in combination with system flag `+Bi`).

When the restricted shell evaluates an expression and encounters a function call
or an operator application, it calls a callback function (with information about
the function call in question). This callback function returns `true` to let the
shell go ahead with the evaluation, or `false` to abort it. There are two
possible callback functions for the user to implement:

- `local_allowed(Func, ArgList, State) -> {boolean(),NewState}`

  This is used to determine if the call to the local function `Func` with
  arguments `ArgList` is to be allowed.

- `non_local_allowed(FuncSpec, ArgList, State) -> {boolean(),NewState} | {{redirect,NewFuncSpec,NewArgList},NewState}`

  This is used to determine if the call to non-local function `FuncSpec`
  (`{Module,Func}` or a fun) with arguments `ArgList` is to be allowed. The
  return value `{redirect,NewFuncSpec,NewArgList}` can be used to let the shell
  evaluate some other function than the one specified by `FuncSpec` and
  `ArgList`.

These callback functions are called from local and non-local evaluation function
handlers, described in the `m:erl_eval` manual page. (Arguments in `ArgList` are
evaluated before the callback functions are called.)

From OTP 25.0, if there are errors evaluating Erlang constructs, such as
`badmatch` during pattern matching or `bad_generator` in a comprehension, the
evaluator will dispatch to `erlang:raise(error, Reason, Stacktrace)`. This call
will be checked against the `non_local_allowed/3` callback function. You can
either forbid it, allow it, or redirect to another call of your choice.

Argument `State` is a tuple `{ShellState,ExprState}`. The return value
`NewState` has the same form. This can be used to carry a state between calls to
the callback functions. Data saved in `ShellState` lives through an entire shell
session. Data saved in `ExprState` lives only through the evaluation of the
current expression.

There are two ways to start a restricted shell session:

- Use STDLIB application variable `restricted_shell` and specify, as its value,
  the name of the callback module. Example (with callback functions implemented
  in `callback_mod.erl`): `$ erl -stdlib restricted_shell callback_mod`.
- From a normal shell session, call function `start_restricted/1`. This exits
  the current evaluator and starts a new one in restricted mode.

_Notes:_

- When restricted shell mode is activated or deactivated, new jobs started on
  the node run in restricted or normal mode, respectively.
- If restricted mode has been enabled on a particular node, remote shells
  connecting to this node also run in restricted mode.
- The callback functions cannot be used to allow or disallow execution of
  functions called from compiled code (only functions called from expressions
  entered at the shell prompt).

Errors when loading the callback module is handled in different ways depending
on how the restricted shell is activated:

- If the restricted shell is activated by setting the STDLIB variable during
  emulator startup, and the callback module cannot be loaded, a default
  restricted shell allowing only the commands `q()` and `init:stop()` is used as
  fallback.
- If the restricted shell is activated using `start_restricted/1` and the
  callback module cannot be loaded, an error report is sent to the error logger
  and the call returns `{error,Reason}`.

## Prompting

The default shell prompt function displays the name of the node (if the node can
be part of a distributed system) and the current command number. The user can
customize the prompt function by calling `prompt_func/1` or by setting
application configuration parameter `shell_prompt_func` for the STDLIB
application. Similarly the multiline prompt can be configured as well, by
calling `multiline_prompt_func/1` or by setting the application parameter
`shell_multiline_prompt` for the STDLIB application.

A customized prompt function is stated as a tuple `{Mod, Func}`. The function is
called as `Mod:Func(L)`, where `L` is a list of key-value pairs created by the
shell. Currently there is only one pair: `{history, N}`, where `N` is the
current command number. The function is to return a list of characters or an
atom. This constraint is because of the Erlang I/O protocol. Unicode characters
beyond code point 255 are allowed in the list and the atom. Notice that in
restricted mode the call `Mod:Func(L)` must be allowed or the default shell
prompt function is called.
