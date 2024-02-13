%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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

It is possible to attach to interpreted processes by giving the corresponding
process identity only. By default, an attachment window is displayed. Processes
at other Erlang nodes can be attached manually or automatically.

By preference, these functions can be included in module `m:shell_default`. By
default, they are included in that module.

## See Also

`m:int`
""".

-export([help/0,ia/1,ia/2,ia/3,ia/4,iaa/1,iaa/2,
	 ib/2,ib/3,ib/4,ibd/2,ibe/2,iba/3,ibc/3,ic/0,ii/1,ii/2,
	 il/0,im/0,ini/1,ini/2,inq/1,ip/0,ipb/0,ipb/1,iq/1,
	 ir/0,ir/1,ir/2,ir/3,iv/0,ist/1]).

-import(io, [format/1,format/2]).
-import(lists, [sort/1,foreach/2]).

-doc """
iv() -> atom()

Returns the current version number of the interpreter. Same as the version
number of the Debugger application.
""".
-spec iv() -> atom().
iv() ->
    Vsn = string:slice(filename:basename(code:lib_dir(debugger)), 9),
    list_to_atom(Vsn).

%% -------------------------------------------
%% Start a new graphical monitor.
%% A monitor displays status for all processes
%% running interpreted modules.
%% -------------------------------------------

-doc """
im() -> pid()

Starts a new graphical monitor. This is the Monitor window, the main window of
Debugger. All the Debugger and interpreter functionality is accessed from the
Monitor window. This window displays the status of all processes that have been
or are executing interpreted modules.
""".
-spec im() -> pid().
im() ->
    case debugger:start() of
	{ok, Pid} ->
	    Pid;
	{error, {already_started, Pid}} ->
	    Pid
    end.

%% -------------------------------------------
%% Add Module(s) as being interpreted.
%% The actual paths will be searched for the
%% corresponding source file(s) (Module.erl).
%% Module(s) can be given with absolute path.
%% -------------------------------------------

-doc(#{equiv => ini/1}).
-spec ii(AbsModule) -> {module, Module} | error when
      AbsModule :: Module | File,
      Module :: module(),
      File :: file:name_all();
        (AbsModules) -> ok when
      AbsModules :: [AbsModule],
      AbsModule :: Module | File,
      Module :: module(),
      File :: file:name_all().
ii(Module) ->
    int:i(Module).

-doc false.
ii(Module,_Options) ->
    int:i(Module).

%% -------------------------------------------
%% Don't interpret module(s). The module will be
%% removed from the set of modules interpreted.
%% -------------------------------------------

-doc """
iq(AbsModule) -> ok

Stops interpreting the specified module. [`iq/1`](`iq/1`) stops interpreting the
module only at the current node. [`inq/1`](`inq/1`) stops interpreting the
module at all known nodes.
""".
-spec iq(AbsModule) -> ok when
      AbsModule :: Module | File,
      Module :: module(),
      File :: file:name_all().
iq(Module) ->
    int:n(Module).

%% -------------------------------------------
%% The corresponding functions for distributed
%% erlang. The loading ... will be performed
%% at all nodes using the broadcast facility.
%% -------------------------------------------

-doc """
ini(AbsModule) -> {module, Module} | errorini(AbsModules) -> ok

Interprets the specified module(s). [`ii/1`](`ii/1`) interprets the module(s)
only at the current node, see `int:i/1`. [`ini/1`](`ini/1`) interprets the
module(s) at all known nodes, see `int:ni/1`.
""".
-spec ini(AbsModules) -> ok when
      AbsModules :: [AbsModule],
      AbsModule :: Module | File,
      Module :: module(),
      File :: file:name_all();
         (AbsModule) -> {module, Module} | error when
      AbsModule :: Module | File,
      Module :: module(),
      File :: file:name_all().
ini(Module) ->
    int:ni(Module).

-doc false.
ini(Module,_Options) ->
    int:ni(Module).

-doc(#{equiv => iq/1}).
-spec inq(AbsModule) -> ok when AbsModule :: Module | File,
    Module :: module(),
    File :: file:name_all().
inq(Module) ->
    int:nn(Module).

%% -------------------------------------------
%% Add a new break point at Line in Module.
%% -------------------------------------------

-doc """
ib(Module, Line) -> ok | {error, break_exists}

Creates a breakpoint at `Line` in `Module`.
""".
-spec ib(Module, Line) -> ok | {error, break_exists}
            when Module :: module(), Line :: integer().
ib(Module,Line) ->
    int:break(Module,Line).

%% -------------------------------------------
%% Break at entrance of specified function.
%% Breaks is set at the first expression for 
%% all function clauses.
%% -------------------------------------------

-doc """
ib(Module, Name, Arity) -> ok | {error, function_not_found}

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

%% -------------------------------------------
%% Make an existing break point inactive.
%% -------------------------------------------

-doc """
ibd(Module, Line) -> ok

Makes the breakpoint at `Line` in `Module` inactive.
""".
-spec ibd(Module, Line) -> ok when Module :: module(), Line :: integer().
ibd(Mod,Line) ->
    int:disable_break(Mod,Line).
    
%% -------------------------------------------
%% Make an existing break point active.
%% -------------------------------------------

-doc """
ibe(Module, Line) -> ok

Makes the breakpoint at `Line` in `Module` active.
""".
-spec ibe(Module, Line) -> ok when Module :: module(), Line :: integer().
ibe(Mod,Line) ->
    int:enable_break(Mod,Line).

%% -------------------------------------------
%% Set which status a break point shall have
%% after it has been triggered the next time.
%% Action is: enable, disable or delete.
%% -------------------------------------------

-doc """
iba(Module, Line, Action) -> ok

Sets the trigger action of the breakpoint at `Line` in `Module` to `Action`.
""".
-spec iba(Module, Line, Action) -> ok
             when
                 Module :: module(),
                 Line :: integer(),
                 Action :: enable | disable | delete.
iba(Mod,Line,Action) ->
    int:action_at_break(Mod,Line,Action).

%% -------------------------------------------
%% Add a conditional function to a break point.
%% The given function shall have arity 1 and
%% return either true or false.
%% The argument of the given function is the
%% current variable bindings of the process at
%% the place of the break point, the bindings
%% can be inspected using int:get_binding/2.

%% Fnk == {Module,Function}
%% Fnk == {Module,Function,ExtraArgs}
%% -------------------------------------------

-doc """
ibc(Module, Line, Function) -> ok

Sets the conditional test of the breakpoint at `Line` in `Module` to `Function`.

The conditional test is performed by calling `Module:Name(Bindings)`, where
`Bindings` is the current variable bindings. The function must return `true`
(break) or `false` (do not break). To retrieve the value of a variable `Var`,
use [int:get_binding(Var, Bindings)](`int:get_binding/2`).
""".
-spec ibc(Module, Line, Function) -> ok when
      Module :: module(),
      Line :: integer(),
      Function :: {Module, Name},
      Name :: atom().
ibc(Mod,Line,Fnk) ->
    int:test_at_break(Mod,Line,Fnk).

%% -------------------------------------------
%% Delete break point.
%% -------------------------------------------

-doc """
ir(Module, Line) -> ok

Deletes the breakpoint at `Line` in `Module`.
""".
-spec ir(Module, Line) -> ok when Module :: module(), Line :: integer().
ir(Module,Line) ->
    int:delete_break(Module,Line).

%% -------------------------------------------
%% Delete break at entrance of specified function.
%% -------------------------------------------

-doc """
ir(Module, Name, Arity) -> ok | {error, function_not_found}

Deletes the breakpoints at the first line of every clause of function
`Module:Name/Arity`.
""".
-spec ir(Module, Name, Arity) -> ok | {error, function_not_found}
            when Module :: module(), Name :: atom(), Arity :: integer().
ir(Module,Function,Arity) ->
    int:del_break_in(Module,Function,Arity).

%% -------------------------------------------
%% Delete all break points in module.
%% -------------------------------------------

-doc """
ir(Module) -> ok

Deletes all breakpoints in `Module`.
""".
-spec ir(Module) -> ok when Module :: module().
ir(Module) ->
    int:no_break(Module).

%% -------------------------------------------
%% Delete all break points (for all modules).
%% -------------------------------------------

-doc """
ir() -> ok

Deletes all breakpoints.
""".
-spec ir() -> ok.
ir() ->
    int:no_break().

%% -------------------------------------------
%% Print all interpreted modules.
%% -------------------------------------------

-doc """
il() -> ok

Makes a printout of all interpreted modules. Modules are printed together with
the full path name of the corresponding source code file.
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

%% -------------------------------------------
%% Print all break points in modules.
%% -------------------------------------------

-doc """
ipb() -> ok

Prints all existing breakpoints.
""".
-spec ipb() -> ok.
ipb() ->
    Bps = lists:keysort(1,int:all_breaks()),
    bhformat("Module","Line","Status","Action","Condition"),
    pb_print(Bps).

-doc """
ipb(Module) -> ok

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

%% -------------------------------------------
%% Set the stack trace flag.
%% Flag can be all (true), no_tail or false.
%% -------------------------------------------

-doc """
ist(Flag) -> true

Sets how to save call frames in the stack, see
[int:stack_trace/1](`int:stack_trace/0`).
""".
-spec ist(Flag) -> true when Flag :: all | no_tail | false.
ist(Flag) ->
    int:stack_trace(Flag),
    true.

%% -------------------------------------------
%% Set the automatic attachment flag.
%% Flags can be init, break and exit.
%% iaa(Flag) or ia([Flag,Flag,...])
%% -------------------------------------------

-doc(#{equiv => iaa/2}).
-spec iaa(Flags) -> true when Flags :: [init | break | exit].
iaa(Flag) ->
    iaa(Flag,{dbg_wx_trace,start,[]}).

%% -------------------------------------------
%% Set the automatic attachment flag.
%% Flags can be init, break and exit.
%% Use given function to start up an attachment
%% window.
%% ia(Flag,Fnk) or ia([Flag,Flag,...],Fnk)
%%   where Fnk == {M,F}
%% The given Fnk must have arity 3 or 4.
%% -------------------------------------------

-doc """
iaa(Flags, Function) -> true

Sets when and how to attach to a debugged process automatically, see
[int:auto_attach/2](`int:auto_attach/0`). `Function` defaults to the standard
function used by Debugger.
""".
-spec iaa(Flags, Function) -> true when
      Flags :: [init | break | exit],
      Function :: {Module,Name,Args},
      Module :: module(),
      Name :: atom(),
      Args :: [term()].
iaa(Flag,Fnk) ->
    int:auto_attach(Flag,Fnk),
    true.

%% -------------------------------------------
%% Attach to process.
%% -------------------------------------------

-doc """
ia(Pid) -> ok | no_proc

Attaches to the debugged process `Pid`. An Attach Process window is opened for
the process.
""".
-spec ia(Pid) -> ok | no_proc when Pid :: pid().
ia(Pid) ->
    ia(Pid,{dbg_wx_trace,start}).

%% -------------------------------------------
%% Attach to process.
%% X,Y,Z is combined to a process identity.
%% -------------------------------------------

-doc """
ia(X,Y,Z) -> ok | no_proc

Same as [`ia(Pid)`](`ia/1`), where `Pid` is the result of calling the shell
function `pid(X,Y,Z)`.
""".
-spec ia(X, Y, Z) -> ok | no_proc
            when X :: integer(), Y :: integer(), Z :: integer().
ia(X,Y,Z) ->
    ia(c:pid(X,Y,Z)).

%% -------------------------------------------
%% Attach to process.
%% Use Fnk == {M,F} as the attaching interface.
%% -------------------------------------------

-doc """
ia(Pid, Function) -> ok | no_proc

Attaches to the debugged process `Pid`. The interpreter calls
[`spawn(Module, Name, [Pid])`](`spawn/3`) (and ignores the result).
""".
-spec ia(Pid, Function) -> ok | no_proc when
      Pid :: pid(),
      Function :: {Module,Name},
      Module :: module(),
      Name :: atom().
ia(Pid,Fnk) ->
    case lists:keymember(Pid, 1, int:snapshot()) of
	false -> no_proc;
	true  -> int:attach(Pid,Fnk)
    end.

-doc """
ia(X,Y,Z, Function) -> ok | no_proc

Same as [`ia(Pid, Function)`](`ia/2`), where `Pid` is the result of calling the
shell function `pid(X,Y,Z)`. An attached process is expected to call the
unofficial function `int:attached(Pid)` and to be able to handle messages from
the interpreter. For an example, see `dbg_wx_trace.erl`.
""".
-spec ia(X,Y,Z, Function) -> ok | no_proc when
      X :: integer(),
      Y :: integer(),
      Z :: integer(),
      Function :: {Module,Name},
      Module :: module(),
      Name :: atom().
ia(X,Y,Z,Fnk) ->
    ia(c:pid(X,Y,Z),Fnk).

%% -------------------------------------------
%% Print status for all interpreted processes.
%% -------------------------------------------

-doc """
ip() -> ok

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


%% -------------------------------------------
%% Delete all terminated processes from the 
%% interpreter.
%% -------------------------------------------

-doc """
ic() -> ok

Clears information about processes executing interpreted code by removing all
information about terminated processes.
""".
-spec ic() -> ok.
ic() ->
    int:clear().

%% -------------------------------------------
%% Help printout
%% -------------------------------------------

-doc """
help() -> ok

Prints help text.
""".
-spec help() -> ok.
help() ->
    format("iv()         -- print the current version of the interpreter~n"),
    format("im()         -- pop up a monitor window~n"),
    format("ii(Mod)      -- interpret Mod(s) (or AbsMod(s))~n"),
    format("ii(Mod,Op)   -- interpret Mod(s) (or AbsMod(s))~n"),
    format("                use Op as options (same as for compile)~n"),
    format("iq(Mod)      -- do not interpret Mod(s)~n"),
    format("ini(Mod)     -- ii/1 at all Erlang nodes~n"),
    format("ini(Mod,Op)  -- ii/2 at all Erlang nodes~n"),
    format("inq(Mod)     -- iq at all Erlang nodes~n"),
    format("ib(Mod,Line) -- set a break point at Line in Mod~n"),
    format("ib(M,F,Arity)-- set a break point in M:F/Arity~n"),
    format("ibd(Mod,Line)-- disable the break point at Line in Mod~n"),
    format("ibe(Mod,Line)-- enable the break point at Line in Mod~n"),
    format("iba(M,L,Action)-- set a new action at break~n"),
    format("ibc(M,L,Action)-- set a new condition for break~n"),
    format("ir(Mod,Line) -- remove the break point at Line in Mod~n"),
    format("ir(M,F,Arity)-- remove the break point in M:F/Arity~n"),
    format("ir(Mod)      -- remove all break points in Mod~n"),
    format("ir()         -- remove all existing break points~n"),
    format("il()         -- list all interpreted modules~n"),
    format("ip()         -- print status of all interpreted processes~n"),
    format("ic()         -- remove all terminated interpreted processes~n"),
    format("ipb()        -- list all break points~n"),
    format("ipb(Mod)     -- list all break points in Mod~n"),
    format("ia(Pid)      -- attach to Pid~n"),
    format("ia(X,Y,Z)    -- attach to pid(X,Y,Z)~n"),
    format("ia(Pid,Fun)  -- use own Fun = {M,F} as attach application~n"),
    format("ia(X,Y,Z,Fun)-- use own Fun = {M,F} as attach application~n"),
    format("iaa([Flag])  -- set automatic attach to process~n"),
    format("                Flag is init,break and exit~n"),
    format("iaa([Fl],Fun)-- use own Fun = {M,F} as attach application~n"),
    format("ist(Flag)    -- set stack trace flag~n"),
    format("                Flag is all (true),no_tail or false~n"),
    ok.



