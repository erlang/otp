%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
%% Purpose : User interface to the Erlang debugger/interpreter.

-module(i).
-moduledoc """
Debugger/Interpreter Interface.

The `i` module provides short forms for some of the functions used by the
graphical Debugger and some of the functions in module `m:int`, the Erlang
interpreter.

This module also provides facilities for displaying status information about
interpreted processes and break points.

It is possible to attach to interpreted processes by only giving the corresponding
process identity. By default, an attachment window is displayed. Processes
at other Erlang nodes can be attached manually or automatically.

The functions in this module are defined in the Erlang shell. That is,
they can be called without the `i:` prefix. For example:

```erlang
1> ii(t).
{module,t}
2> iaa([init]).
true
```
""".

-export([help/0,ia/1,ia/2,ia/3,ia/4,iaa/1,iaa/2,
	 ib/2,ib/3,ib/4,ibd/2,ibe/2,iba/3,ibc/3,ic/0,ii/1,ii/2,
	 il/0,im/0,ini/1,ini/2,inq/1,ip/0,ipb/0,ipb/1,iq/1,
	 ir/0,ir/1,ir/2,ir/3,iv/0,ist/1]).

-import(io, [format/1,format/2]).
-import(lists, [sort/1,foreach/2]).

-doc """
Returns the current version of the interpreter (Debugger).
""".
-spec iv() -> atom().
iv() ->
    Vsn = string:slice(filename:basename(code:lib_dir(debugger)), 9),
    list_to_atom(Vsn).

-doc """
Starts a new graphical Monitor window.

This is the Monitor window, the main window of Debugger. All the
Debugger and interpreter functionality is accessed from the Monitor
window. This window displays the status of all processes that have
been or are executing interpreted modules.
""".
-spec im() -> pid().
im() ->
    case debugger:start() of
	{ok, Pid} ->
	    Pid;
	{error, {already_started, Pid}} ->
	    Pid
    end.

-doc """
Interprets the specified module(s) on the local node.

- If `AbsModule :: Module | File`, then `Result :: {module, Module} | error`.
- If `AbsModules :: [AbsModule]`, then `Result :: ok`.

See `int:i/1` for more information.
""".
-spec ii(AbsModules | AbsModule) -> Result when
          AbsModules :: [AbsModule,...],
          AbsModule :: Module | File,
          Module :: module(),
          File :: file:name_all(),
          Result :: AbsModuleResult | AbsModulesResult,
          AbsModuleResult :: {module, Module} | error,
          AbsModulesResult :: ok.
ii(Module) ->
    int:i(Module).

-doc false.
ii(Module,_Options) ->
    int:i(Module).

-doc """
Stops interpreting the specified module on the local node.
""".
-spec iq(AbsModule) -> ok when
      AbsModule :: Module | File,
      Module :: module(),
      File :: file:name_all().
iq(Module) ->
    int:n(Module).

-doc """
Interprets the specified module(s) on all known nodes.

- If `AbsModule :: Module | File`, then `Result :: {module, Module} | error`.
- If `AbsModules :: [AbsModule]`, then `Result :: ok`.

See `int:ni/1` for more information.
""".
-spec ini(AbsModules | AbsModule) -> Result when
          AbsModules :: [AbsModule],
          AbsModule :: Module | File,
          Module :: module(),
          File :: file:name_all(),
          Result :: AbsModuleResult | AbsModulesResult,
          AbsModuleResult :: {module, Module} | error,
          AbsModulesResult :: ok.
ini(Module) ->
    int:ni(Module).

-doc false.
ini(Module,_Options) ->
    int:ni(Module).

-doc """
Stops interpreting the specified module on all known nodes.
""".
-spec inq(AbsModule) -> ok when AbsModule :: Module | File,
    Module :: module(),
    File :: file:name_all().
inq(Module) ->
    int:nn(Module).

-doc """
Creates a breakpoint at `Line` in `Module`.
""".
-spec ib(Module, Line) -> ok | {error, break_exists}
            when Module :: module(), Line :: integer().
ib(Module,Line) ->
    int:break(Module,Line).

-doc """
Creates breakpoints at the first line of every clause of function
`Module:Name/Arity`.
""".
-spec ib(Module, Name, Arity) -> ok | {error, function_not_found}
            when Module :: module(), Name :: atom(), Arity :: integer().
ib(Module,Function,Arity) ->
    int:break_in(Module,Function,Arity).

%% -------------------------------------------
%% Break at entrance of specified function.
%% Breaks is set at the first expression for
%% all function clauses.
%% Associate the condition to the break.
%% -------------------------------------------

-doc false.
ib(Module,Function,Arity,Cond) ->
    Breaks1 = int:all_breaks(Module),
    ok = int:break_in(Module,Function,Arity),
    Breaks2 = int:all_breaks(Module),
    lists:foreach(fun({Mod,Line}) -> int:test_at_break(Mod,Line,Cond) end,
		  Breaks2--Breaks1).

-doc """
Makes the breakpoint at `Line` in `Module` inactive.
""".
-spec ibd(Module, Line) -> ok when Module :: module(), Line :: integer().
ibd(Mod,Line) ->
    int:disable_break(Mod,Line).

-doc """
Makes the breakpoint at `Line` in `Module` active.
""".
-spec ibe(Module, Line) -> ok when Module :: module(), Line :: integer().
ibe(Mod,Line) ->
    int:enable_break(Mod,Line).

-doc """
Sets the trigger action of the breakpoint at `Line` in `Module` to `Action`.
""".
-spec iba(Module, Line, Action) -> ok
             when
                 Module :: module(),
                 Line :: integer(),
                 Action :: enable | disable | delete.
iba(Mod,Line,Action) ->
    int:action_at_break(Mod,Line,Action).

-doc """
Sets the conditional test of the breakpoint at `Line` in `Module` to `Function`.

The conditional test is performed by calling `Module:Name(Bindings)`, where
`Bindings` is the current variable bindings. The function must return `true`
(break) or `false` (do not break). To retrieve the value of a variable `Var`
use [int:get_binding(Var, Bindings)](`int:get_binding/2`).
""".
-spec ibc(Module, Line, Function) -> ok when
      Module :: module(),
      Line :: integer(),
      Function :: {Module, Name},
      Name :: atom().
ibc(Mod,Line,Fnk) ->
    int:test_at_break(Mod,Line,Fnk).

-doc """
Deletes the breakpoint at `Line` in `Module`.
""".
-spec ir(Module, Line) -> ok when Module :: module(), Line :: integer().
ir(Module,Line) ->
    int:delete_break(Module,Line).

-doc """
Deletes the breakpoints at the first line of every clause of function
`Module:Name/Arity`.
""".
-spec ir(Module, Name, Arity) -> ok | {error, function_not_found}
            when Module :: module(), Name :: atom(), Arity :: integer().
ir(Module,Function,Arity) ->
    int:del_break_in(Module,Function,Arity).

-doc """
Deletes all breakpoints in `Module`.
""".
-spec ir(Module) -> ok when Module :: module().
ir(Module) ->
    int:no_break(Module).

-doc """
Deletes all breakpoints in all interpreted modules.
""".
-spec ir() -> ok.
ir() ->
    int:no_break().

-doc """
Makes a printout of all interpreted modules.

Modules are printed together with the full path name of the
corresponding source code file.
""".
-spec il() -> ok.
il() ->
    Mods = sort(int:interpreted()),
    ilformat("Module","File"),
    foreach(fun(Mod) -> ilformat(atom_to_list(Mod), get_file(Mod)) end, Mods).

get_file(Mod) ->
    case int:file(Mod) of
	{error,not_loaded} -> % Marked interpreted but not loaded
	    "not loaded";
	File ->
	    File
    end.

ilformat(A1, A2) ->
    format("~-20s     ~ts\n", [A1,A2]).

-doc """
Prints all existing breakpoints.
""".
-spec ipb() -> ok.
ipb() ->
    Bps = lists:keysort(1,int:all_breaks()),
    bhformat("Module","Line","Status","Action","Condition"),
    pb_print(Bps).

-doc """
Prints all existing breakpoints in `Module`.
""".
-spec ipb(Module) -> ok when Module :: module().
ipb(Module) when is_atom(Module) ->
    ipb1(Module);
ipb(Module) when is_list(Module) ->
    ipb1(list_to_atom(Module)).

ipb1(Module) ->
    Bps = lists:keysort(1,int:all_breaks(Module)),
    bhformat("Module","Line","Status","Action","Condition"),
    pb_print(Bps).

pb_print([{{Mod,Line},[Status,Action,_,null|_]}|Bps]) ->
    bformat(Mod,Line,Status,Action,""),
    pb_print(Bps);
pb_print([{{Mod,Line},[Status,Action,_,Cond|_]}|Bps]) ->
    bformat(Mod,Line,Status,Action,
	    io_lib:format("~w",[Cond])),
    pb_print(Bps);
pb_print(_) ->
    ok.

bhformat(A1, A2, A3, A4, A5) ->
    format("~-15s ~-9s ~-12s ~-12s ~-21s~n", [A1,A2,A3,A4,A5]).

bformat(A1, A2, A3, A4, A5) ->
    format("~-15w ~-9w ~-12w ~-12w ~-21s~n", [A1,A2,A3,A4,A5]).

-doc """
Sets how to save call frames in the stack.

See [int:stack_trace/1](`int:stack_trace/0`) for more information.
""".
-spec ist(Flag) -> true when Flag :: all | no_tail | false.
ist(Flag) ->
    int:stack_trace(Flag),
    true.

-doc """
Sets when to attach to a debugged process automatically.

Debugger supplies a function that opens "Attach Process" window for
the process.

See [int:auto_attach/2](`int:auto_attach/2`) for more information.
""".
-spec iaa(Flags) -> true when Flags :: [init | break | exit].
iaa(Flags) ->
    iaa(Flags, {dbg_wx_trace,start,[]}).

-doc """
Sets when and how to attach to a debugged process automatically.

See [int:auto_attach/2](`int:auto_attach/2`) for more information.
""".
-spec iaa(Flags, Function) -> true when
      Flags :: [init | break | exit],
      Function :: {Module,Name,Args},
      Module :: module(),
      Name :: atom(),
      Args :: [term()].
iaa(Flags, Function) ->
    int:auto_attach(Flags, Function),
    true.

-doc """
Attaches to the debugged process `Pid`.

An "Attach Process" window is opened for the process.
""".
-spec ia(Pid) -> ok | no_proc when Pid :: pid().
ia(Pid) ->
    ia(Pid, {dbg_wx_trace,start}).

-doc """
Equivalent to [`ia(Pid)`](`ia/1`), where `Pid` is the result of calling the shell
function `pid(X, Y, Z)`.
""".
-spec ia(X, Y, Z) -> ok | no_proc
            when X :: integer(), Y :: integer(), Z :: integer().
ia(X, Y, Z) ->
    ia(c:pid(X, Y, Z)).

-doc """
Attaches to the debugged process `Pid`.

The interpreter calls [`spawn(Module, Name, [Pid])`](`spawn/3`) (and
ignores the result).
""".
-spec ia(Pid, Function) -> ok | no_proc when
      Pid :: pid(),
      Function :: {Module,Name},
      Module :: module(),
      Name :: atom().
ia(Pid, Function) ->
    case lists:keymember(Pid, 1, int:snapshot()) of
	false -> no_proc;
	true  -> int:attach(Pid,Function)
    end.

-doc """
Equivalent to [`ia(Pid, Function)`](`ia/2`), where `Pid` is the result of calling the
shell function `pid(X, Y, Z)`.

An attached process is expected to call the unofficial function
`int:attached(Pid)` and to be able to handle messages from the
interpreter. For an example, see `dbg_wx_trace.erl`.

""".
-spec ia(X,Y,Z, Function) -> ok | no_proc when
      X :: integer(),
      Y :: integer(),
      Z :: integer(),
      Function :: {Module,Name},
      Module :: module(),
      Name :: atom().
ia(X, Y, Z, Function) ->
    ia(c:pid(X, Y, Z), Function).

-doc """
Prints the current status of all interpreted processes.
""".
-spec ip() -> ok.
ip() ->
    Stats = int:snapshot(),
    hformat("Pid","Initial Call","Status","Info"),
    ip(Stats).

ip([{Pid,{M,F,A},Status,{}}|Stats]) ->
    hformat(io_lib:format("~w",[Pid]),
	    io_lib:format("~w:~tw/~w",[M,F,length(A)]),
	    io_lib:format("~w",[Status]),
	    ""),
    ip(Stats);
ip([{Pid,{M,F,A},Status,Info}|Stats]) ->
    hformat(io_lib:format("~w",[Pid]),
	    io_lib:format("~w:~tw/~w",[M,F,length(A)]),
	    io_lib:format("~w",[Status]),
	    io_lib:format("~w",[Info])),
    ip(Stats);
ip([]) ->
    ok.

hformat(A1, A2, A3, A4) ->
    format("~-12s ~-21ts ~-9s ~-21s~n", [A1,A2,A3,A4]).


-doc """
Clears information about processes executing interpreted code by removing all
information about terminated processes.
""".
-spec ic() -> ok.
ic() ->
    int:clear().

-doc """
Prints help for using the functions in this module.
""".
-spec help() -> ok.
help() ->
    S = ~"""
        iv()              -- print the current version of the interpreter
        im()              -- pop up a monitor window
        ii(Mod)           -- interpret Mod(s) (or AbsMod(s))
        ii(Mod, Opt)      -- interpret Mod(s) (or AbsMod(s))
                             use Opt as options (same as for compile)
        iq(Mod)           -- do not interpret Mod(s)
        ini(Mod)          -- ii/1 on all Erlang nodes
        ini(Mod, Op)      -- ii/2 on all Erlang nodes
        inq(Mod)          -- iq on all Erlang nodes
        ib(Mod, Line)     -- set a break point at Line in Mod
        ib(M, F, Arity)   -- set a break point in M:F/Arity
        ibd(Mod, Line)    -- disable the break point at Line in Mod
        ibe(Mod, Line)    -- enable the break point at Line in Mod
        iba(M, L, Action) -- set a new action at break
        ibc(M, L, Action) -- set a new condition for break
        ir(Mod, Line)     -- remove the break point at Line in Mod
        ir(M, F, Arity)   -- remove the break point in M:F/Arity
        ir(Mod)           -- remove all break points in Mod
        ir()              -- remove all existing break points
        il()              -- list all interpreted modules
        ip()              -- print status of all interpreted processes
        ic()              -- remove all terminated interpreted processes
        ipb()             -- list all break points
        ipb(Mod)          -- list all break points in Mod
        ia(Pid)           -- attach to Pid
        ia(X, Y, Z)       -- attach to pid(X, Y, Z)
        ia(Pid, Fun)      -- use own Fun = {M,F} as attach application
        ia(X, Y, Z, Fun)  -- use own Fun = {M,F} as attach application
        iaa([Flag])       -- set automatic attach to process
                             Flag is init, break, or exit
        iaa([Fl], Fun)    -- use own Fun = {M,F} as attach application
        ist(Flag)         -- set stack trace flag
                             Flag is all (true), no_tail, or false
        """,
    io:put_chars(S),
    io:nl().
