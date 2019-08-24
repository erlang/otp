%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2017. All Rights Reserved.
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

-export([help/0,ia/1,ia/2,ia/3,ia/4,iaa/1,iaa/2,
	 ib/2,ib/3,ib/4,ibd/2,ibe/2,iba/3,ibc/3,ic/0,ii/1,ii/2,
	 il/0,im/0,ini/1,ini/2,inq/1,ip/0,ipb/0,ipb/1,iq/1,
	 ir/0,ir/1,ir/2,ir/3,iv/0,ist/1]).

-import(io, [format/1,format/2]).
-import(lists, [sort/1,foreach/2]).

iv() ->
    Vsn = string:slice(filename:basename(code:lib_dir(debugger)), 9),
    list_to_atom(Vsn).

%% -------------------------------------------
%% Start a new graphical monitor.
%% A monitor displays status for all processes
%% running interpreted modules.
%% -------------------------------------------

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

ii(Module) ->
    int:i(Module).

ii(Module,_Options) ->
    int:i(Module).

%% -------------------------------------------
%% Don't interpret module(s). The module will be
%% removed from the set of modules interpreted.
%% -------------------------------------------

iq(Module) ->
    int:n(Module).

%% -------------------------------------------
%% The corresponding functions for distributed
%% erlang. The loading ... will be performed
%% at all nodes using the broadcast facility.
%% -------------------------------------------

ini(Module) ->
    int:ni(Module).

ini(Module,_Options) ->
    int:ni(Module).

inq(Module) ->
    int:nn(Module).

%% -------------------------------------------
%% Add a new break point at Line in Module.
%% -------------------------------------------

ib(Module,Line) ->
    int:break(Module,Line).

%% -------------------------------------------
%% Break at entrance of specified function.
%% Breaks is set at the first expression for 
%% all function clauses.
%% -------------------------------------------

ib(Module,Function,Arity) ->
    int:break_in(Module,Function,Arity).

%% -------------------------------------------
%% Break at entrance of specified function.
%% Breaks is set at the first expression for 
%% all function clauses.
%% Associate the condition to the break.
%% -------------------------------------------

ib(Module,Function,Arity,Cond) ->
    Breaks1 = int:all_breaks(Module),
    ok = int:break_in(Module,Function,Arity),
    Breaks2 = int:all_breaks(Module),
    lists:foreach(fun({Mod,Line}) -> int:test_at_break(Mod,Line,Cond) end,
		  Breaks2--Breaks1).

%% -------------------------------------------
%% Make an existing break point inactive.
%% -------------------------------------------

ibd(Mod,Line) ->
    int:disable_break(Mod,Line).
    
%% -------------------------------------------
%% Make an existing break point active.
%% -------------------------------------------

ibe(Mod,Line) ->
    int:enable_break(Mod,Line).

%% -------------------------------------------
%% Set which status a break point shall have
%% after it has been triggered the next time.
%% Action is: enable, disable or delete.
%% -------------------------------------------

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

ibc(Mod,Line,Fnk) ->
    int:test_at_break(Mod,Line,Fnk).

%% -------------------------------------------
%% Delete break point.
%% -------------------------------------------

ir(Module,Line) ->
    int:delete_break(Module,Line).

%% -------------------------------------------
%% Delete break at entrance of specified function.
%% -------------------------------------------

ir(Module,Function,Arity) ->
    int:del_break_in(Module,Function,Arity).

%% -------------------------------------------
%% Delete all break points in module.
%% -------------------------------------------

ir(Module) ->
    int:no_break(Module).

%% -------------------------------------------
%% Delete all break points (for all modules).
%% -------------------------------------------

ir() ->
    int:no_break().

%% -------------------------------------------
%% Print all interpreted modules.
%% -------------------------------------------

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

ipb() ->
    Bps = lists:keysort(1,int:all_breaks()),
    bhformat("Module","Line","Status","Action","Condition"),
    pb_print(Bps).

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

ist(Flag) ->
    int:stack_trace(Flag),
    true.

%% -------------------------------------------
%% Set the automatic attachment flag.
%% Flags can be init, break and exit.
%% iaa(Flag) or ia([Flag,Flag,...])
%% -------------------------------------------

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

iaa(Flag,Fnk) ->
    int:auto_attach(Flag,Fnk),
    true.

%% -------------------------------------------
%% Attach to process.
%% -------------------------------------------

ia(Pid) ->
    ia(Pid,{dbg_wx_trace,start}).

%% -------------------------------------------
%% Attach to process.
%% X,Y,Z is combind to a process identity.
%% -------------------------------------------

ia(X,Y,Z) ->
    ia(c:pid(X,Y,Z)).

%% -------------------------------------------
%% Attach to process.
%% Use Fnk == {M,F} as the attaching interface.
%% -------------------------------------------

ia(Pid,Fnk) ->
    case lists:keymember(Pid, 1, int:snapshot()) of
	false -> no_proc;
	true  -> int:attach(Pid,Fnk)
    end.

ia(X,Y,Z,Fnk) ->
    ia(c:pid(X,Y,Z),Fnk).

%% -------------------------------------------
%% Print status for all interpreted processes.
%% -------------------------------------------

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

ic() ->
    int:clear().

%% -------------------------------------------
%% Help printout
%% -------------------------------------------

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


