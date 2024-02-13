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
# Debugger

## Getting Started

To use Debugger, the basic steps are as follows:

_Step 1._ Start Debugger by calling `debugger:start()`.

The [Monitor window](debugger_chapter.md#monitor) is displayed with information
about all debugged processes, interpreted modules, and selected options.
Initially there are normally no debugged processes. First, it must be specified
which modules that are to be _debugged_ (also called _interpreted_). Proceed as
follows:

_Step 2._ Select _Module > Interpret..._ in the Monitor window.

The [Interpret Modules window](debugger_chapter.md#interpret) is displayed.

_Step 3._ Select the appropriate modules from the Interpret Dialog window.

> #### Note {: .info }
>
> Only modules compiled with option `debug_info` set can be interpreted.
> Non-interpretable modules are displayed within parenthesis in the Interpret
> Modules window.

_Step 4._ In the Monitor window, select _Module > the module to be interpreted >
View_.

The contents of the source file is displayed in the
[View Module window](debugger_chapter.md#view).

_Step 5._ Set the [breakpoints](debugger_chapter.md#breakpoints), if any.

_Step 6._ Start the program to be debugged. This is done the normal way from the
Erlang shell.

All processes executing code in interpreted modules are displayed in the Monitor
window.

_Step 7._ To _attach_ to one of these processes, double-click it, or select the
process and then choose _Process > Attach_. Attaching to a process opens an
[Attach Process window](debugger_chapter.md#attach) for this process.

_Step 8._ From the Attach Process window, you can control the process execution,
inspect variable values, set breakpoints, and so on.

[](){: #breakpoints }

## Breakpoints and Break Dialog Windows

Once the appropriate modules are interpreted, breakpoints can be set at relevant
locations in the source code. Breakpoints are specified on a line basis. When a
process reaches a breakpoint, it stops and waits for commands (_Step_, _Skip_,
_Continue_ ...) from the user.

> #### Note {: .info }
>
> When a process reaches a breakpoint, only that process is stopped. Other
> processes are not affected.

Breakpoints are created and deleted using the _Break_ menu of either the Monitor
window, View Module window, or Attach Process window.

### Executable Lines

To have an effect, a breakpoint must be set at an _executable line_, which is a
line of code containing an executable expression such as a matching or a
function call. A blank line or a line containing a comment, function head, or
pattern in a `case` statement or `receive` statement is not executable.

In the following example, lines 2, 4, 6, 8, and 11 are executable lines:

```erlang
1: is_loaded(Module,Compiled) ->
2:   case get_file(Module,Compiled) of
3:     {ok,File} ->
4:       case code:which(Module) of
5:         ?TAG ->
6:           {loaded,File};
7:         _ ->
8:           unloaded
9:       end;
10:    false ->
11:      false
12:  end.
```

### Status and Trigger Action

A breakpoint can be either _active_ or _inactive_. Inactive breakpoints are
ignored.

Each breakpoint has a _trigger action_ that specifies what is to happen when a
process has reached it (and stopped):

- _Enable_ \- Breakpoint is to remain active (default).
- _Disable_ \- Breakpoint is to be made inactive.
- _Delete_ \- Breakpoint is to be deleted.

### Line Breakpoints

A line breakpoint is created at a certain line in a module.

![Line Break Dialog Window](assets/line_break_dialog.jpg "Line Break Dialog Window")

Right-click the _Module_ entry to open a popup menu from which the appropriate
module can be selected.

A line breakpoint can also be created (and deleted) by double-clicking the line
when the module is displayed in the View Module window or Attach Process window.

### Conditional Breakpoints

A conditional breakpoint is created at a certain line in the module, but a
process reaching the breakpoint stops only if a specified condition is true.

The condition is specified by the user as a module name `CModule` and a function
name `CFunction`. When a process reaches the breakpoint,
`CModule:CFunction(Bindings)` is evaluated. If and only if this function call
returns `true`, the process stops. If the function call returns `false`, the
breakpoint is silently ignored.

`Bindings` is a list of variable bindings. To retrieve the value of `Variable`
(given as an atom), use function
[`int:get_binding(Variable,Bindings)`](`int:get_binding/2`). The function
returns `unbound` or `{value,Value}`.

![Conditional Break Dialog Window](assets/cond_break_dialog.jpg "Conditional Break Dialog Window")

Right-click the _Module_ entry to open a popup menu from which the appropriate
module can be selected.

_Example:_

A conditional breakpoint calling `c_test:c_break/1` is added at line 6 in module
`fact`. Each time the breakpoint is reached, the function is called. When `N` is
equal to 3, the function returns `true` and the process stops.

Extract from `fact.erl`:

```erlang
5. fac(0) -> 1;
6. fac(N) when N > 0, is_integer(N) -> N * fac(N-1).
```

Definition of `c_test:c_break/1`:

```erlang
-module(c_test).
-export([c_break/1]).

c_break(Bindings) ->
    case int:get_binding('N', Bindings) of
        {value, 3} ->
            true;
        _ ->
            false
    end.
```

### Function Breakpoints

A function breakpoint is a set of line breakpoints, one at the first line of
each clause in the specified function.

![Function Break Dialog Window](assets/function_break_dialog.jpg "Function Break Dialog Window")

To open a popup menu from which the appropriate module can be selected,
right-click the _Module_ entry.

To bring up all functions of the module in the listbox, click the _OK_ button
(or press the _Return_ or _Tab_ key) when a module name has been specified,.

[](){: #stack_trace }

## Stack Trace

The Erlang emulator keeps track of a _stack trace_, information about recent
function calls. This information is used if an error occurs, for example:

```erlang
1> catch a+1.
{'EXIT',{badarith,[{erlang,'+',[a,1],[]},
                   {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,573}]},
                   {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,357}]},
                   {shell,exprs,7,[{file,"shell.erl"},{line,674}]},
                   {shell,eval_exprs,7,[{file,"shell.erl"},{line,629}]},
                   {shell,eval_loop,3,[{file,"shell.erl"},{line,614}]}]}}
```

For details about the stack trace, see section
[Errors and Error Handling](`e:system:errors.md`) in the Erlang Reference
Manual.

Debugger emulates the stack trace by keeping track of recently called
interpreted functions. (The real stack trace cannot be used, as it shows which
functions of Debugger have been called, rather than which interpreted
functions.)

This information can be used to traverse the chain of function calls, using the
_Up_ and _Down_ buttons in the
[Attach Process window](debugger_chapter.md#attach).

By default, Debugger only saves information about recursive function calls, that
is, function calls that have not yet returned a value (option _Stack On, No
Tail_).

Sometimes, however, it can be useful to save all calls, even tail-recursive
calls. This is done with option _Stack On, Tail_. Notice that this option
consumes more memory and slows down execution of interpreted functions when
there are many tail-recursive calls.

To turn off the Debugger stack trace facility, select option _Stack Off_.

> #### Note {: .info }
>
> If an error occurs, the stack trace becomes empty in this case.

For information about how to change the stack trace option, see section
[Monitor Window](debugger_chapter.md#monitor).

[](){: #monitor }

## Monitor Window

The Monitor window is the main window of Debugger and displays the following:

- A listbox containing the names of all interpreted modules

  Double-clicking a module brings up the View Module window.

- Which options are selected
- Information about all debugged processes, that is, all processes that have
  been or are executing code in interpreted modules

![Monitor Window](assets/monitor.jpg "Monitor Window")

The _Auto Attach_ boxes, _Stack Trace_ label, _Back Trace Size_ label, and
_Strings_ box display some options set. For details about these options, see
section [Options Menu](debugger_chapter.md#options).

### Process Grid

- **_Pid_** - The process identifier.

- **_Initial Call_** - The first call to an interpreted function by this
  process. (`Module:Function/Arity`)

- **_Name_** - The registered name, if any. If a registered name is not
  displayed, it can be that Debugger received information about the process
  before the name was registered. Try selecting _Edit > Refresh_.

- **_Status_** - The current status, one of the following:

  - **_idle_** - The interpreted function call has returned a value, and the
    process is no longer executing interpreted code.

  - **_running_** - The process is running.

  - **_waiting_** - The process is waiting in a `receive` statement.

  - **_break_** - The process is stopped at a breakpoint.

  - **_exit_** - The process has terminated.

  - **_no_conn_** - There is no connection to the node where the process is
    located.

- **_Information_** - More information, if any. If the process is stopped at a
  breakpoint, the field contains information about the location `{Module,Line}`.
  If the process has terminated, the field contains the exit reason.

### File Menu

- **_Load Settings..._** - Tries to load and restore Debugger settings from a
  file previously saved using _Save Settings..._ (see below). Any errors are
  silently ignored.

  Notice that settings saved by Erlang/OTP R16B01 or later cannot be read by
  Erlang/OTP R16B or earlier.

- **_Save Settings..._** - Saves Debugger settings to a file. The settings
  include the set of interpreted files, breakpoints, and the selected options.
  The settings can be restored in a later Debugger session using _Load
  Settings..._ (see above). Any errors are silently ignored.

- **_Exit_** - Stops Debugger.

### Edit Menu

- **_Refresh_** - Updates information about debugged processes. Information
  about all terminated processes are removed from the window. All Attach Process
  windows for terminated processes are closed.

- **_Kill All_** - Terminates all processes listed in the window using
  [`exit(Pid,kill)`](`exit/2`).

### Module Menu

- **_Interpret..._** - Opens the
  [Interpret Modules window](debugger_chapter.md#interpret), where new modules
  to be interpreted can be specified.

- **_Delete All_** - Stops interpreting all modules. Processes executing in
  interpreted modules terminate.

For each interpreted module, a corresponding entry is added to the _Module_
menu, with the following submenu:

- **_Delete_** - Stops interpreting the selected module. Processes executing in
  this module terminate.

- **_View_** - Opens a [View Module window](debugger_chapter.md#view),
  displaying the contents of the selected module.

### Process Menu

The following menu items apply to the currently selected process, provided it is
stopped at a breakpoint (for details, see section
[Attach Process window](debugger_chapter.md#attach)):

- **_Step_**

- **_Next_**

- **_Continue_**

- **_Finish_**

The following menu items apply to the currently selected process:

- **_Attach_** - Attaches to the process and open an
  [Attach Process window](debugger_chapter.md#attach).

- **_Kill_** - Terminates the process using [`exit(Pid,kill)`](`exit/2`).

### Break Menu

The items in this menu are used to create and delete breakpoints. For details,
see section [Breakpoints](debugger_chapter.md#breakpoints).

- **_Line Break..._** - Sets a line breakpoint.

- **_Conditional Break..._** - Sets a conditional breakpoint.

- **_Function Break..._** - Sets a function breakpoint.

- **_Enable All_** - Enables all breakpoints.

- **_Disable All_** - Disables all breakpoints.

- **_Delete All_** - Removes all breakpoints.

For each breakpoint, a corresponding entry is added to the _Break_ menu, from
which it is possible to disable, enable, or delete the breakpoint, and to change
its trigger action.

[](){: #options }

### Options Menu

- **_Trace Window_** - Sets the areas to be visible in an
  [Attach Process window](debugger_chapter.md#attach). Does not affect existing
  Attach Process windows.

- **_Auto Attach_** - Sets the events a debugged process is to be attached to
  automatically. Affects existing debugged processes.

  - _First Call_ \- The first time a process calls a function in an interpreted
    module.
  - _On Exit_ \- At process termination.
  - _On Break_ \- When a process reaches a breakpoint.

- **_Stack Trace_** - Sets the stack trace option, see section
  [Stack Trace](debugger_chapter.md#stack_trace). Does not affect existing
  debugged processes.

  - _Stack On, Tail_ \- Saves information about all current calls.
  - _Stack On, No Tail_ \- Saves information about current calls, discarding
    previous information when a tail recursive call is made.
  - _Stack Off_ \- Does not save any information about current calls.

- **_Strings_** - Sets the integer lists to be printed as strings. Does not
  affect existing debugged processes.

  - _Use range of +pc flag_ \- Uses the printable character range set by the
    [`erl(1)`](`e:erts:erl_cmd.md`) flag
    [`+pc`](`e:erts:erl_cmd.md#printable_character_range`).

- **_Back Trace Size..._** - Sets how many call frames to be fetched when
  inspecting the call stack from the Attach Process window. Does not affect
  existing Attach Process windows.

### Windows Menu

Contains a menu item for each open Debugger window. Selecting one of the items
raises the corresponding window.

### Help Menu

- **_Help_** - Shows the Debugger documentation. This function requires a web
  browser.

[](){: #interpret }

## Interpret Modules Window

The Interpret Modules window is used for selecting which modules to interpret.
Initially, the window displays the modules (`erl` files) and subdirectories of
the current working directory.

Interpretable modules are modules for which a `.beam` file, compiled with option
`debug_info` set, is located in the same directory as the source code, or in an
`ebin` directory next to it.

Modules for which these requirements are not fulfilled are not interpretable and
are therefore displayed within parentheses.

Option `debug_info` causes _debug information_ or _abstract code_ to be added to
the `.beam` file. This increases the file size and makes it possible to
reconstruct the source code. It is therefore recommended not to include debug
information in code aimed for target systems.

An example of how to compile code with debug information using `erlc`:

```erlang
% erlc +debug_info module.erl
```

An example of how to compile code with debug information from the Erlang shell:

```text
4> c(module, debug_info).
```

![Interpret Modules Window](assets/interpret.jpg "Interpret Modules Window")

To browse the file hierarchy and interpret the appropriate modules, either
select a module name and click _Choose_ (or press carriage return), or
double-click the module name. Interpreted modules have the type `erl src`.

To interpret all displayed modules in the chosen directory, click _All_.

To close the window, click _Done_.

> #### Note {: .info }
>
> When Debugger is started in global mode (which is the default, see
> `debugger:start/0`), modules added (or deleted) for interpretation are added
> (or deleted) on all known Erlang nodes.

[](){: #attach }

## Attach Process Window

From an Attach Process window, you can interact with a debugged process. One
window is opened for each process that has been attached to. Notice that when
attaching to a process, its execution is automatically stopped.

![Attach Process Window](assets/attach.jpg "Attach Process Window")

The window is divided into the following five parts:

- The Code area, displaying the code being executed. The code is indented and
  each line is prefixed with its line number. If the process execution is
  stopped, the current line is marked with `-->`. An existing break point at a
  line is marked with a stop symbol. In the example shown in the illustration,
  the execution stopped at line 6, before the execution of `fac/1`.

  Active breakpoints are displayed in red and inactive breakpoints in blue.

- The Button area, with buttons for quick access to frequently used functions in
  the _Process_ menu.
- The Evaluator area, where you can evaluate functions within the context of the
  debugged process, if that process execution is stopped.
- The Bindings area, displaying all variables bindings. If you click a variable
  name, the value is displayed in the Evaluator area. Double-click a variable
  name to open a window where the variable value can be edited. Notice however
  that pid, port, reference, or fun values cannot be edited unless they can be
  represented in the running system.
- The Trace area, which displays a trace output for the process.

  - **`++ (N) <L>`** - Function call, where `N` is the call level and `L` the
    line number.

  - **`-- (N)`** - Function return value

    .

  - **`==> Pid : Msg`** - The message `Msg` is sent to process `Pid`.

  - **`<== Msg`** - The message `Msg` is received.

  - **`++ (N) receive`** - Waiting in a `receive`.

  - **`++ (N) receive with timeout`** - Waiting in a `receive...after`.

  The Trace area also displays Back Trace, a summary of the current function
  calls on the stack.

Using the _Options_ menu, you can set which areas to be displayed. By default,
all areas except the Trace area are displayed.

### File Menu

- **_Close_** - Closes this window and detach from the process.

### Edit Menu

- **_Go to line..._** - Goes to a specified line number.

- **_Search..._** - Searches for a specified string.

### Process Menu

- **_Step_** - Executes the current code line, stepping into any (interpreted)
  function calls.

- **_Next_** - Executes the current code line and stop at the next line.

- **_Continue_** - Continues the execution.

- **_Finish_** - Continues the execution until the current function returns.

- **_Skip_** - Skips the current code line and stop at the next line. If used on
  the last line in a function body, the function returns `skipped`.

- **_Time Out_** - Simulates a time-out when executing a `receive...after`
  statement.

- **_Stop_** - Stops the execution of a running process, that is, make the
  process stop at a breakpoint. The command takes effect (visibly) the next time
  the process receives a message.

- **_Where_** - Verifies that the current location of the execution is visible
  in the code area.

- **_Kill_** - Terminates the process using [`exit(Pid,kill)`](`exit/2`).

- **_Messages_** - Inspects the message queue of the process. The queue is
  displayed in the Evaluator area.

- **_Back Trace_** - Displays the back trace of the process, a summary of the
  current function calls on the stack, in the Trace area. Requires that the
  Trace area is visible and that the Stack Trace option is _Stack On, Tail_ or
  _Stack On, No Tail_.

- **_Up_** - Inspects the previous function call on the stack, showing the
  location and variable bindings.

- **_Down_** - Inspects the next function call on the stack, showing the
  location and variable bindings.

### Options Menu

- **_Trace Window_** - Sets which areas are to be visible. Does not affect other
  Attach Process windows.

- **_Stack Trace_** - Same as in the
  [Monitor window](debugger_chapter.md#monitor), but only affects the debugged
  process the window is attached to.

- **_Strings_** - Same as in the [Monitor window](debugger_chapter.md#monitor),
  but only affects the debugged process the window is attached to.

- **_Back Trace Size..._** - Sets how many call frames are to be fetched when
  inspecting the call stack. Does not affect other Attach Process windows.

### Break, Windows, and Help Menus

The _Break_, _Windows_, and _Help_ menus are the same as in the
[Monitor Window](debugger_chapter.md#monitor), except that the _Breaks_ menu
applies only to local breakpoints.

[](){: #view }

## View Module Window

The View Module window displays the contents of an interpreted module and makes
it possible to set breakpoints.

![View Module Window](assets/view.jpg "View Module Window")

The source code is indented and each line is prefixed with its line number.

Clicking a line highlights it and selects it to be the target of the breakpoint
functions available from the _Break_ menu. To set a line breakpoint on a line,
double-click it. To remove the breakpoint, double-click the line with an
existing breakpoint.

Breakpoints are marked with a stop symbol.

### File and Edit Menus

The _File_ and _Edit_ menus are the same as in the
[Attach Process Window](debugger_chapter.md#attach).

### Break, Windows, and Help Menus

The _Break_, _Windows_, and _Help_ menus are the same as in the
[Monitor Window](debugger_chapter.md#monitor), except that the _Break_ menu
applies only to local breakpoints.

## Performance

Execution of interpreted code is naturally slower than for regularly compiled
modules. Using Debugger also increases the number of processes in the system, as
for each debugged process another process (the meta process) is created.

It is also worth to keep in mind that programs with timers can behave
differently when debugged. This is especially true when stopping the execution
of a process (for example, at a breakpoint). Time-outs can then occur in other
processes that continue execution as normal.

## Code Loading Mechanism

Code loading works almost as usual, except that interpreted modules are also
stored in a database and debugged processes use only this stored code.
Reinterpreting an interpreted module results in the new version being stored as
well, but does not affect existing processes executing an older version of the
code. This means that the code replacement mechanism of Erlang does not work for
debugged processes.

## Debugging Remote Nodes

By using `debugger:start/1`, you can specify if Debugger is to be started in
local or global mode:

```text
debugger:start(local | global)
```

If no argument is provided, Debugger starts in global mode.

In local mode, code is interpreted only at the current node. In global mode,
code is interpreted at all known nodes. Processes at other nodes executing
interpreted code are automatically displayed in the Monitor window and can be
attached to like any other debugged process.

It is possible, but definitely not recommended, to start Debugger in global mode
on more than one node in a network, as the nodes interfere with each other,
leading to inconsistent behavior.
