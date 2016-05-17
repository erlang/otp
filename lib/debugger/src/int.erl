%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(int).

%% External exports
-export([i/1, i/2, ni/1, ni/2, n/1, nn/1, interpreted/0, file/1,
	 interpretable/1]).
-export([auto_attach/0, auto_attach/1, auto_attach/2,
	 stack_trace/0, stack_trace/1]).
-export([break/2, delete_break/2, break_in/3, del_break_in/3,
	 no_break/0, no_break/1,
	 disable_break/2, enable_break/2,
	 action_at_break/3, test_at_break/3, get_binding/2,
	 all_breaks/0, all_breaks/1]).
-export([snapshot/0, clear/0]).
-export([continue/1, continue/3]).

%% External exports only to be used by Debugger
-export([start/0, stop/0, subscribe/0]).
-export([attach/2, step/1, next/1, finish/1]).

%% External exports only to be used by an attached process
-export([attached/1, meta/2, meta/3, contents/2, functions/1]).

%% External export only to be used by error_handler
-export([eval/3]).

-include_lib("kernel/include/file.hrl").

%%==Erlang Interpreter================================================
%%
%% int
%% ---
%% Interface module.
%%
%% i
%% -
%% Interface module to int, retained for backwards compatibility only.
%%
%% dbg_debugged
%% ------------
%% Contains the message loops for a debugged process and is the main
%% entry point from the breakpoint handler in the error_handler module
%% (via the int module).
%%
%% When a process is debugged, most code is executed in another
%% process, called the meta process. When the meta process is
%% interpreting code, the process being debugged just waits in a
%% receive loop in dbg_debugged. However the debugged process itself
%% calls any BIFs that must execute in the correct process (such as
%% link/1 and spawn_link/1), and external code which is not
%% interpreted.
%%
%% dbg_icmd, dbg_ieval
%% -------------------
%% Code for the meta process.
%%
%% dbg_iserver
%% -----------
%% Interpreter main process, keeping and distributing information
%% about interpreted modules and debugged processes.
%%
%% dbg_idb
%% -------
%% ETS wrapper, allowing transparent access to tables at a remote node.
%%
%% dbg_iload
%% ---------
%% Code for interpreting a module.
%%====================================================================
 
%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% i(AbsMods) -> {module,Mod} | error | ok
%% ni(AbsMods) -> {module,Mod} | error | ok
%%   AbsMods = AbsMod | [AbsMod]
%%     AbsMod = atom() | string()
%%     Mod = atom()
%%     Options = term() ignored
%%--------------------------------------------------------------------
i(AbsMods) -> i2(AbsMods, local, ok).
i(AbsMods, _Options) -> i2(AbsMods, local, ok).
ni(AbsMods) -> i2(AbsMods, distributed, ok).
ni(AbsMods, _Options) -> i2(AbsMods, distributed, ok).
    
i2([AbsMod|AbsMods], Dist, Acc)
  when is_atom(AbsMod); is_list(AbsMod); is_tuple(AbsMod) -> 
    Res = int_mod(AbsMod, Dist),
    case Acc of
	error ->
	    i2(AbsMods, Dist, Acc);
	_ ->
	    i2(AbsMods, Dist, Res)
    end;
i2([], _Dist, Acc) ->
    Acc;
i2(AbsMod, Dist, _Acc) when is_atom(AbsMod); is_list(AbsMod); is_tuple(AbsMod) ->
    int_mod(AbsMod, Dist).

%%--------------------------------------------------------------------
%% n(AbsMods) -> ok
%% nn(AbsMods) -> ok
%%--------------------------------------------------------------------
n(AbsMods) -> n2(AbsMods, local).
nn(AbsMods) -> n2(AbsMods, distributed).

n2([AbsMod|AbsMods], Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    del_mod(AbsMod, Dist),
    n2(AbsMods, Dist);
n2([AbsMod], Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    del_mod(AbsMod, Dist);
n2([], _Dist) ->
    ok;
n2(AbsMod, Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    del_mod(AbsMod, Dist).

%%--------------------------------------------------------------------
%% interpreted() -> [Mod]
%%--------------------------------------------------------------------
interpreted() ->
    dbg_iserver:safe_call(all_interpreted).

%%--------------------------------------------------------------------
%% file(Mod) -> File | {error, not_loaded}
%%   Mod = atom()
%%   File = string()
%%--------------------------------------------------------------------
file(Mod) when is_atom(Mod) ->
    dbg_iserver:safe_call({file, Mod}).

%%--------------------------------------------------------------------
%% interpretable(AbsMod) -> true | {error, Reason}
%%   AbsMod = Mod | File
%%   Reason = no_src | no_beam | no_debug_info | badarg | {app, App}
%%--------------------------------------------------------------------
interpretable(AbsMod) ->
    case check(AbsMod) of
	{ok, _Res} -> true;
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% auto_attach() -> false | {Flags, Function}
%% auto_attach(false)
%% auto_attach(false|Flags, Function)
%%   Flags = Flag | [Flag]
%%     Flag = init | break | exit
%%   Function = {Mod, Func} | {Mod, Func, Args}
%% Will result in calling:
%%  spawn(Mod, Func, [Dist, Pid, Meta | Args]) (living process) or
%%  spawn(Mod, Func, [Dist, Pid, Reason, Info | Args]) (dead process)
%%--------------------------------------------------------------------
auto_attach() ->
    dbg_iserver:safe_call(get_auto_attach).

auto_attach(false) ->
    dbg_iserver:safe_cast({set_auto_attach, false}).

auto_attach([], _Function) ->
    auto_attach(false);
auto_attach(Flags, {Mod, Func}) ->
    auto_attach(Flags, {Mod, Func, []});
auto_attach(Flags, {Mod, Func, Args}) when is_atom(Mod),is_atom(Func),is_list(Args) ->
    check_flags(Flags),
    dbg_iserver:safe_cast({set_auto_attach, Flags, {Mod, Func, Args}}).

check_flags([init|Flags]) -> check_flags(Flags);
check_flags([break|Flags]) -> check_flags(Flags);
check_flags([exit|Flags]) -> check_flags(Flags);
check_flags([]) -> true.

%%--------------------------------------------------------------------
%% stack_trace() -> Flag
%% stack_trace(Flag)
%%   Flag = all | true | no_tail | false
%%--------------------------------------------------------------------
stack_trace() ->
    dbg_iserver:safe_call(get_stack_trace).

stack_trace(true) ->
    stack_trace(all);
stack_trace(Flag) ->
    check_flag(Flag),
    dbg_iserver:safe_cast({set_stack_trace, Flag}).

check_flag(all) -> true;
check_flag(no_tail) -> true;
check_flag(false) -> true.

%%--------------------------------------------------------------------
%% break(Mod, Line) -> ok | {error, break_exists}
%% delete_break(Mod, Line) -> ok
%% break_in(Mod, Func, Arity) -> ok | {error, function_not_found}
%% del_break_in(Mod, Function, Arity) -> ok | {error, function_not_found}
%% no_break()
%% no_break(Mod)
%% disable_break(Mod, Line) -> ok
%% enable_break(Mod, Line) -> ok
%% action_at_break(Mod, Line, Action) -> ok
%% test_at_break(Mod, Line, Function) -> ok
%% get_binding(Var, Bindings) -> {value, Value} | unbound
%% all_breaks() -> [Break]
%% all_breaks(Mod) -> [Break]
%%   Mod = atom()
%%   Line = integer()
%%   Func = atom() function name
%%   Arity = integer()
%%   Action = enable | disable | delete
%%   Function = {Mod, Func} must have arity 1 (Bindings)
%%   Var = atom()
%%   Bindings = Value = term()
%%   Break = {Point, Options}
%%     Point = {Mod, Line}
%%     Options = [Status, Action, null, Cond]
%%       Status = active | inactive
%%       Cond = null | Function
%%--------------------------------------------------------------------
break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    dbg_iserver:safe_call({new_break, {Mod, Line},
			   [active, enable, null, null]}).

delete_break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    dbg_iserver:safe_cast({delete_break, {Mod, Line}}).

break_in(Mod, Func, Arity) when is_atom(Mod), is_atom(Func), is_integer(Arity) ->
    case dbg_iserver:safe_call({is_interpreted, Mod, Func, Arity}) of
	{true, Clauses} ->
	    Lines = first_lines(Clauses),
	    lists:foreach(fun(Line) -> break(Mod, Line) end, Lines);
	false ->
	    {error, function_not_found}
    end.

del_break_in(Mod, Func, Arity) when is_atom(Mod), is_atom(Func), is_integer(Arity) ->
    case dbg_iserver:safe_call({is_interpreted, Mod, Func, Arity}) of
	{true, Clauses} ->
	    Lines = first_lines(Clauses),
	    lists:foreach(fun(Line) -> delete_break(Mod, Line) end,
			  Lines);
	false ->
	    {error, function_not_found}
    end.

first_lines(Clauses) ->
    [first_line(Clause) || Clause <- Clauses].

first_line({clause,_L,_Vars,_,Exprs}) ->
    first_line(Exprs);
first_line([Expr|_Exprs]) -> % Expr = {Op, Line, ..varying no of args..}
    element(2, Expr).

no_break() ->
    dbg_iserver:safe_cast(no_break).

no_break(Mod) when is_atom(Mod) ->
    dbg_iserver:safe_cast({no_break, Mod}).

disable_break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    dbg_iserver:safe_cast({break_option, {Mod, Line}, status, inactive}).
    
enable_break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    dbg_iserver:safe_cast({break_option, {Mod, Line}, status, active}).

action_at_break(Mod, Line, Action) when is_atom(Mod), is_integer(Line) ->
    check_action(Action),
    dbg_iserver:safe_cast({break_option, {Mod, Line}, action, Action}).

check_action(enable) -> true;
check_action(disable) -> true;
check_action(delete) -> true.

test_at_break(Mod, Line, Function) when is_atom(Mod), is_integer(Line) ->
    check_function(Function),
    dbg_iserver:safe_cast({break_option, {Mod, Line}, condition, Function}).

check_function({Mod, Func}) when is_atom(Mod), is_atom(Func) -> true.

get_binding(Var, Bs) ->
    dbg_icmd:get_binding(Var, Bs).

all_breaks() ->
    dbg_iserver:safe_call(all_breaks).
all_breaks(Mod) when is_atom(Mod) ->
    dbg_iserver:safe_call({all_breaks, Mod}).

%%--------------------------------------------------------------------
%% snapshot() -> [{Pid, Init, Status, Info}]
%%   Pid = pid()
%%   Init = atom()  First interpreted function
%%   Status = idle | running | waiting | break | exit
%%   Info = {} | {Mod, Line} | ExitReason
%%     Mod = atom()
%%     Line = integer()
%%     ExitReason = term()
%%--------------------------------------------------------------------
snapshot() ->
    dbg_iserver:safe_call(snapshot).

%%--------------------------------------------------------------------
%% clear()
%%--------------------------------------------------------------------
clear() ->
    dbg_iserver:safe_cast(clear).
    
%%--------------------------------------------------------------------
%% continue(Pid) -> ok | {error, not_interpreted}
%% continue(X, Y, Z) -> ok | {error, not_interpreted}
%%--------------------------------------------------------------------
continue(Pid) when is_pid(Pid) ->
    case dbg_iserver:safe_call({get_meta, Pid}) of
	{ok, Meta} when is_pid(Meta) ->
	    dbg_icmd:continue(Meta),
	    ok;
	Error ->
	    Error
    end.
    
continue(X, Y, Z) when is_integer(X), is_integer(Y), is_integer(Z) ->
    continue(c:pid(X, Y, Z)).


%%====================================================================
%% External exports only to be used by Debugger
%%====================================================================

%%--------------------------------------------------------------------
%% start()
%% stop()
%% Functions for starting and stopping dbg_iserver explicitly.
%%--------------------------------------------------------------------
start() -> dbg_iserver:start().
stop() ->
    lists:foreach(
      fun(Mod) ->
	      _ = everywhere(distributed,
			     fun() ->
				 erts_debug:breakpoint({Mod,'_','_'}, false)
			     end)
      end,
      interpreted()),
    dbg_iserver:stop().

%%--------------------------------------------------------------------
%% subscribe()
%% Subscribe to information from dbg_iserver. The process calling this
%% function will receive the following messages:
%%   {int, {interpret, Mod}}
%%   {int, {no_interpret, Mod}}
%%   {int, {new_process, {Pid, Function, Status, Info}}}
%%   {int, {new_status, Pid, Status, Info}}
%%   {int, {new_break, {Point, Options}}}
%%   {int, {delete_break, Point}}
%%   {int, {break_options, {Point, Options}}}
%%   {int, no_break}
%%   {int, {no_break, Mod}}
%%   {int, {auto_attach, false|{Flags, Function}}}
%%   {int, {stack_trace, Flag}}
%%--------------------------------------------------------------------
subscribe() -> dbg_iserver:cast({subscribe, self()}).

%%--------------------------------------------------------------------
%% attach(Pid, Function)
%%   Pid = pid()
%%   Function = {Mod, Func} | {Mod, Func, Args} (see auto_attach/2)
%% Tell dbg_iserver to attach to Pid using Function. Will result in:
%%   spawn(Mod, Func, [Pid, Status | Args])
%%--------------------------------------------------------------------
attach(Pid, {Mod, Func}) ->
    attach(Pid, {Mod, Func, []});
attach(Pid, Function) ->
    dbg_iserver:cast({attach, Pid, Function}).

%%--------------------------------------------------------------------
%% step(Pid)
%% next(Pid)
%% (continue(Pid))
%% finish(Pid)
%%--------------------------------------------------------------------
step(Pid) ->
    {ok, Meta} = dbg_iserver:call({get_meta, Pid}),
    dbg_icmd:step(Meta).
next(Pid) ->
    {ok, Meta} = dbg_iserver:call({get_meta, Pid}),
    dbg_icmd:next(Meta).
finish(Pid) ->
    {ok, Meta} = dbg_iserver:call({get_meta, Pid}),
    dbg_icmd:finish(Meta).


%%====================================================================
%% External exports only to be used by an attached process
%%====================================================================

%%--------------------------------------------------------------------
%% attached(Pid) -> {ok, Meta} | error
%%   Pid = Meta = pid()
%% Tell dbg_iserver that I have attached to Pid. dbg_iserver informs
%% the meta process and returns its pid. dbg_iserver may also refuse,
%% if there already is a process attached to Pid.
%%--------------------------------------------------------------------
attached(Pid) ->
    dbg_iserver:call({attached, self(), Pid}).

%%--------------------------------------------------------------------
%% meta(Meta, Cmd)
%%   Meta = pid()
%%   Cmd = step | next | continue | finish | skip | timeout | stop
%%   Cmd = messages => [Message]
%% meta(Meta, Cmd, Arg)
%%   Cmd = trace,       Arg = bool()
%%   Cmd = stack_trace  Arg = all | notail | false
%%   Cmd = stack_frame  Arg = {up|down, Sp}
%%       => {Sp, Mod, Line} | top | bottom
%%   Cmd = backtrace    Arg = integer()
%%       => {Sp, Mod, {Func, Arity}, Line}
%%   Cmd = eval        Arg = {Cm, Cmd} | {Cm, Cmd, Sp}
%%--------------------------------------------------------------------
meta(Meta, step) -> dbg_icmd:step(Meta);
meta(Meta, next) -> dbg_icmd:next(Meta);
meta(Meta, continue) -> dbg_icmd:continue(Meta);
meta(Meta, finish) -> dbg_icmd:finish(Meta);
meta(Meta, skip) -> dbg_icmd:skip(Meta);
meta(Meta, timeout) -> dbg_icmd:timeout(Meta);
meta(Meta, stop) -> dbg_icmd:stop(Meta);
meta(Meta, messages) -> dbg_icmd:get(Meta, messages, null).

meta(Meta, trace, Trace) -> dbg_icmd:set(Meta, trace, Trace);
meta(Meta, stack_trace, Flag) -> dbg_icmd:set(Meta, stack_trace, Flag);
meta(Meta, bindings, Stack) -> dbg_icmd:get(Meta, bindings, Stack);
meta(Meta, stack_frame, Arg) -> dbg_icmd:get(Meta, stack_frame, Arg);
meta(Meta, backtrace, N) -> dbg_icmd:get(Meta, backtrace, N);
meta(Meta, eval, Arg) -> dbg_icmd:eval(Meta, Arg).

%%--------------------------------------------------------------------
%% contents(Mod, Pid) -> string()
%%   Mod = atom()
%%   Pid = pid() | any
%% Return the contents of an interpreted module.
%%--------------------------------------------------------------------
contents(Mod, Pid) ->
    {ok, Bin} = dbg_iserver:call({contents, Mod, Pid}),
    binary_to_list(Bin).

%%--------------------------------------------------------------------
%% functions(Mod) -> [[Name, Arity]]
%%   Mod = Name = atom()
%%   Arity = integer()
%%--------------------------------------------------------------------
functions(Mod) ->
    [F || F <- dbg_iserver:call({functions, Mod}), functions_1(F)].

functions_1([module_info, _Arity]) -> false;
functions_1(_Func) -> true.


%%====================================================================
%% External exports only to be used by error_handler
%%====================================================================

eval(Mod, Func, Args) ->
    dbg_debugged:eval(Mod, Func, Args).


%%====================================================================
%% Internal functions
%%====================================================================

%%--Interpreting modules----------------------------------------------

int_mod({Mod, Src, Beam, BeamBin}, Dist)
  when is_atom(Mod), is_list(Src), is_list(Beam), is_binary(BeamBin) ->
    try
	case is_file(Src) of
	    true ->
		check_application(Src),
		case check_beam(BeamBin) of
		    {ok, Exp, Abst, _BeamBin} ->
			load({Mod, Src, Beam, BeamBin, Exp, Abst}, Dist);
		    error -> 
			error
		end;
	    false ->
		error
	end
    catch
	throw:Reason ->
	    Reason
    end;
int_mod(AbsMod, Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    case check(AbsMod) of
	{ok, Res} -> 
	    load(Res, Dist);
	{error, {app, App}} ->
	    io:format("** Cannot interpret ~p module: ~p~n",
		      [App, AbsMod]),
	    error;
	_Error ->
	    io:format("** Invalid beam file or no abstract code: ~tp\n",
		      [AbsMod]),
	    error
    end.

check(Mod) when is_atom(Mod) -> catch check_module(Mod);
check(File) when is_list(File) -> catch check_file(File).

load({Mod, Src, Beam, BeamBin, Exp, Abst}, Dist) ->
    _ = everywhere(Dist,
		   fun() ->
		       code:purge(Mod),
		       erts_debug:breakpoint({Mod,'_','_'}, false),
		       {module,Mod} = code:load_binary(Mod, Beam, BeamBin)
		   end),
    case erl_prim_loader:get_file(filename:absname(Src)) of
	{ok, SrcBin, _} ->
	    MD5 = code:module_md5(BeamBin),
	    Bin = term_to_binary({interpreter_module,Exp,Abst,SrcBin,MD5}),
	    {module, Mod} = dbg_iserver:safe_call({load, Mod, Src, Bin}),
	    _ = everywhere(Dist,
			   fun() ->
			       true = erts_debug:breakpoint({Mod,'_','_'}, true) > 0
			   end),
	    {module, Mod};
	error ->
	    error
    end.

check_module(Mod) ->
    case code:which(Mod) of
	Beam when is_list(Beam) ->
	    case find_src(Mod, Beam) of
		Src when is_list(Src) ->
		    check_application(Src),
		    case check_beam(Beam) of
			{ok, Exp, Abst, BeamBin} ->
			    {ok, {Mod, Src, Beam, BeamBin, Exp, Abst}};
			error -> 
			    {error, no_debug_info}
		    end;
		error ->
		    {error, no_src}
	    end;
	_ -> 
	    {error, badarg}
    end.

check_file(Name0) ->
    Src =
	case is_file(Name0) of
	    true -> 
		Name0;
	    false ->
		Name = Name0 ++ ".erl",
		case is_file(Name) of
		    true -> Name;
		    false -> error
		end
	end,
    if
	is_list(Src) ->
	    check_application(Src),
	    Mod = scan_module_name(Src),
	    case find_beam(Mod, Src) of
		Beam when is_list(Beam) ->
		    case check_beam(Beam) of
			{ok, Exp, Abst, BeamBin} ->
			    {ok, {Mod, Src, Beam, BeamBin, Exp, Abst}};
			error ->
			    {error, no_debug_info}
		    end;
		error ->
		    {error, no_beam}
	    end;
	true ->
	    {error, badarg}
    end.

%% Try to avoid interpreting a kernel, stdlib, gs or debugger module.
check_application(Src) ->
    case lists:reverse(filename:split(filename:absname(Src))) of
	[_Mod,"src",AppS|_] ->
	    check_application2(AppS);
	_ -> ok
    end.
check_application2("kernel-"++_) -> throw({error,{app,kernel}});
check_application2("stdlib-"++_) -> throw({error,{app,stdlib}});
check_application2("erts-"++_) -> throw({error,{app,erts}});
check_application2("gs-"++_) -> throw({error,{app,gs}});
check_application2("debugger-"++_) -> throw({error,{app,debugger}});
check_application2(_) -> ok.

find_src(Mod, Beam) ->
    Src0 = filename:rootname(Beam) ++ ".erl",
    case is_file(Src0) of
	true -> Src0;
	false ->
	    EbinDir = filename:dirname(Beam),
	    Src = filename:join([filename:dirname(EbinDir), "src",
				 filename:basename(Src0)]),
	    case is_file(Src) of
		true -> Src;
		false -> find_src_from_module(Mod)
	    end
    end.

find_src_from_module(Mod) ->
    Compile = Mod:module_info(compile),
    case lists:keyfind(source, 1, Compile) of
	{source, Src} ->
	    case is_file(Src) of
		true -> Src;
		false -> error
	    end;
	false ->
	    error
    end.

find_beam(Mod, Src) ->
    SrcDir = filename:dirname(Src),
    BeamFile = atom_to_list(Mod) ++ code:objfile_extension(),
    File = filename:join(SrcDir, BeamFile),
    case is_file(File) of
	true -> File;
	false -> find_beam_1(BeamFile, SrcDir)
    end.

find_beam_1(BeamFile, SrcDir) ->
    RootDir = filename:dirname(SrcDir),
    EbinDir = filename:join(RootDir, "ebin"),
    CodePath = [EbinDir | code:get_path()],
    lists:foldl(fun(_, Beam) when is_list(Beam) -> Beam;
		   (Dir, error) ->
			File = filename:join(Dir, BeamFile),
			case is_file(File) of
			    true -> File;
			    false -> error
			end
		end,
		error,
		CodePath).

check_beam(BeamBin) when is_binary(BeamBin) ->
    case beam_lib:chunks(BeamBin, [abstract_code,exports]) of
	{ok,{_Mod,[{abstract_code,no_abstract_code}|_]}} ->
	    error;
	{ok,{_Mod,[{abstract_code,Abst},{exports,Exp}]}} ->
	    {ok,Exp,Abst, BeamBin};
	_ -> 
	    error
    end;
check_beam(Beam) when is_list(Beam) ->
    {ok, Bin, _FullPath} = erl_prim_loader:get_file(filename:absname(Beam)),
    check_beam(Bin).

is_file(Name) ->
    filelib:is_regular(filename:absname(Name), erl_prim_loader).

everywhere(distributed, Fun) ->
    case is_alive() of
	true -> rpc:multicall(erlang, apply, [Fun,[]]);
	false -> Fun()
    end;
everywhere(local, Fun) ->
    Fun().

scan_module_name(File) ->
    try
        {ok, Bin, _FullPath} =
            erl_prim_loader:get_file(filename:absname(File)),
        scan_module_name_1([], <<>>, Bin, enc(Bin))
    catch
        _:_ ->
            throw({error, no_beam})
    end.

scan_module_name_1(Cont0, B0, Bin0, Enc) ->
    N = min(100, byte_size(Bin0)),
    {Bin1, Bin} = erlang:split_binary(Bin0, N),
    {Chars, B1} =
        case unicode:characters_to_list(list_to_binary([B0, Bin1]), Enc) of
            {incomplete, List, Binary} ->
                {List, Binary};
            List when is_list(List), List =/= [] ->
                {List, <<>>}
        end,
    scan_module_name_2(Cont0, Chars, B1, Bin, Enc).

scan_module_name_2(Cont0, Chars, B1, Bin, Enc) ->
    case erl_scan:tokens(Cont0, Chars, _AnyLine = 1) of
        {done, {ok, Ts, _}, Rest} ->
            scan_module_name_3(Ts, Rest, B1, Bin, Enc);
        {more, Cont} ->
            scan_module_name_1(Cont, B1, Bin, Enc)
    end.

scan_module_name_3([{'-',_},{atom,_,module},{'(',_} | _]=Ts,
                   _Chars, _B1, _Bin, _Enc) ->
    scan_module_name_4(Ts);
scan_module_name_3([{'-',_},{atom,_,_} | _], Chars, B1, Bin, Enc) ->
    scan_module_name_2("", Chars, B1, Bin, Enc).

scan_module_name_4(Ts) ->
    {ok, {attribute,_,module,M}} = erl_parse:parse_form(Ts),
    true = is_atom(M),
    M.

enc(Bin) ->
    case epp:read_encoding_from_binary(Bin) of
        none -> epp:default_encoding();
        Encoding -> Encoding
    end.

%%--Stop interpreting modules-----------------------------------------

del_mod(AbsMod, Dist) ->
    Mod = if
	      is_atom(AbsMod) -> AbsMod;
	      is_list(AbsMod) ->
		  list_to_atom(filename:basename(AbsMod,".erl"))
	  end,
    dbg_iserver:safe_cast({delete, Mod}),
    _ = everywhere(Dist,
		   fun() ->
		       erts_debug:breakpoint({Mod,'_','_'}, false),
		       erlang:yield()
		   end),
    ok.
