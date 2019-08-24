%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
-module(dbg_ieval).

-export([eval/3,exit_info/5]).
-export([eval_expr/3]).
-export([check_exit_msg/3,exception/4]).

-include("dbg_ieval.hrl").

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% eval(Mod, Func, Args) -> Meta
%%   Mod = Func = atom()
%%   Args = [term()]
%%   MFA = {Mod,Func,Args} | {Mod,Func,Arity} | {Fun,Args}
%%   Arity = integer()
%%   Meta = pid()
%% Entry point from debugged process (dbg_debugged).
%% Immediately returns the pid for the meta process.
%% The evaluated value will later be sent as a message to
%% the calling process.
%%--------------------------------------------------------------------
eval(Mod, Func, Args) ->
    Debugged = self(),
    Int = dbg_iserver:find(),
    case dbg_iserver:call(Int, {get_meta,Debugged}) of
	{ok,Meta} ->
	    Meta ! {re_entry, Debugged, {eval,{Mod,Func,Args}}},
	    Meta;
	{error,not_interpreted} ->
	    spawn(fun() ->
			  meta(Int, Debugged, Mod, Func, Args)
		  end)
    end.

%%--------------------------------------------------------------------
%% exit_info(Int, AttPid, OrigPid, Reason, ExitInfo)
%%  Int = AttPid = OrigPid = pid()
%%  Reason = term()
%%  ExitInfo = {{Mod,Line}, Bs, Stack} | {}
%% Meta process started when attaching to a terminated process.
%% Spawned (by dbg_iserver) in response to user request.
%%--------------------------------------------------------------------
exit_info(Int, AttPid, OrigPid, Reason, ExitInfo) ->
    put(int, Int),
    put(attached, AttPid),
    put(breakpoints, dbg_iserver:call(Int, all_breaks)),
    put(self, OrigPid),
    put(exit_info, ExitInfo),
    
    case ExitInfo of
	{{Mod,Line},Bs,S} ->
	    dbg_istk:from_external(S),
	    Le = dbg_istk:stack_level(),
	    dbg_icmd:tell_attached({exit_at, {Mod, Line}, Reason, Le}),
	    exit_loop(OrigPid, Reason, Bs,#ieval{module=Mod,line=Line});
	{} ->
	    dbg_istk:init(),
	    dbg_icmd:tell_attached({exit_at, null, Reason, 1}),
	    exit_loop(OrigPid, Reason, erl_eval:new_bindings(),#ieval{})
    end.

%%--------------------------------------------------------------------
%% eval_expr(Expr, Bs, Ieval) -> {value, Value, Bs}
%%
%% Evalute a shell expression in the real process.
%% Called (dbg_icmd) in response to a user request.
%%--------------------------------------------------------------------
eval_expr(Expr, Bs, Ieval) ->

    %% Save current exit info
    ExitInfo = get(exit_info),
    Stacktrace = get(stacktrace),

    %% Emulate a surrounding catch
    try debugged_cmd({eval,Expr,Bs}, Bs, Ieval)
    catch
	Class:Reason ->
	    Result = case Class of
			 throw -> Reason;
			 _ -> {'EXIT', Reason}
		     end,

	    %% Reset exit info
	    put(exit_info, ExitInfo),
	    put(stacktrace, Stacktrace),

	    {value, Result, Bs}
    end.

%%--------------------------------------------------------------------
%% check_exit_msg(Msg, Bs, Ieval)
%%   Msg = term()
%% Check if Msg is an 'EXIT' msg from the iserver or a 'DOWN' msg
%% from the debugged process. If so exit with correct reason.
%%--------------------------------------------------------------------
check_exit_msg({'EXIT', Int, Reason}, _Bs, #ieval{level=Le}) ->
    %% This *must* be interpreter which has terminated,
    %% we are not linked to anyone else
    if
	Le =:= 1 ->
	    exit(Reason);
	Le > 1 ->
	    exit({Int, Reason})
    end;
check_exit_msg({'DOWN',_,_,_,Reason}, Bs,
	       #ieval{level=Le, module=Mod, line=Li}) ->
    %% This *must* be Debugged which has terminated,
    %% we are not monitoring anyone else

    %% Inform Int about current position, bindings and stack
    ExitInfo =
	case get(exit_info) of

	    %% Debugged has been terminated by someone
	    %% - really the position, bindings and stack are of no
	    %%   importance in this case
	    %% If we don't save them, however, post-mortem analysis
	    %% of the process isn't possible
	    undefined when Le =:= 1 -> % died outside interpreted code
		{};
	    undefined when Le > 1 ->
		StackExternal = (dbg_istk:delayed_to_external())(),
		{{Mod, Li}, Bs, StackExternal};

	    %% Debugged has terminated due to an exception
	    ExitInfo0 when is_function(ExitInfo0, 0) ->
		ExitInfo0()
	end,
    dbg_iserver:cast(get(int), {set_exit_info,self(),ExitInfo}),

   if
	Le =:= 1 ->
	    exit(Reason);
	Le > 1 ->
	    exit({get(self), Reason})
    end;
check_exit_msg(_Msg, _Bs, _Ieval) ->
    ignore.

%%--------------------------------------------------------------------
%% exception(Class, Reason, Bs, Ieval)
%%   Class = error | exit | throw
%%   Reason = term()
%%   Bs = bindings()
%%   Ieval = #ieval{}
%% Store information about where in the code the error is located
%% and then raise the exception.
%%--------------------------------------------------------------------
exception(Class, Reason, Bs, Ieval) ->
    exception(Class, Reason, Bs, Ieval, false).

exception(Class, Reason, Bs, Ieval, false) ->
    do_exception(Class, Reason,
		 dbg_istk:delayed_stacktrace(no_args, Ieval),
		 Bs, Ieval);
exception(Class, Reason, Bs, Ieval, true) ->
    do_exception(Class, Reason,
		 dbg_istk:delayed_stacktrace(include_args, Ieval),
		 Bs, Ieval).

do_exception(Class, Reason, Stacktrace, Bs, #ieval{module=M, line=Line}) ->
    StackFun = dbg_istk:delayed_to_external(),
    ExitInfo = fun() ->
		       {{M,Line},Bs,StackFun()}
	       end,
    put(exit_info, ExitInfo),
    put(stacktrace, Stacktrace),
    erlang:Class(Reason).

%%====================================================================
%% Internal functions
%%====================================================================

%%--Loops-------------------------------------------------------------

%% Entry point for first-time initialization of meta process
meta(Int, Debugged, M, F, As) ->
    process_flag(trap_exit, true),
    erlang:monitor(process, Debugged),

    %% Inform dbg_iserver, get the initial status in return
    Pargs = case {M, F} of
		%% If it's a fun we're evaluating, show a text
		%% representation of the fun and its arguments,
		%% not dbg_ieval:eval_fun(...)
		{dbg_ieval, EvalFun} when EvalFun =:= eval_fun;
					  EvalFun =:= eval_named_fun ->
		    {Mx, Fx} = lists:last(As),
		    {Mx, Fx, lists:nth(2, As)};
		_ ->
		    {M, F, As}
	    end,
    Status = dbg_iserver:call(Int, {new_process,Debugged,self(),Pargs}),
    
    %% Initiate process dictionary
    put(int, Int),           % pid() dbg_iserver
    put(attached, undefined),% pid() attached process
    put(breakpoints, dbg_iserver:call(Int, all_breaks)),
    put(cache, []),
    put(next_break, Status), % break | running (other values later)
    put(self, Debugged),     % pid() interpreted process
    dbg_istk:init(),
    put(stacktrace, []),
    put(trace_stack, dbg_iserver:call(Int, get_stack_trace)),
    put(trace, false),       % bool() Trace on/off
    put(user_eval, []),


    %% Send the result of the meta process
    Ieval = #ieval{},
    Debugged ! {sys, self(), eval_mfa(Debugged,M,F,As,Ieval)},

    dbg_iserver:cast(Int, {set_status, self(), idle, {}}),
    dbg_icmd:tell_attached(idle),

    meta_loop(Debugged, erl_eval:new_bindings(), Ieval).

debugged_cmd(Cmd, Bs, Ieval) ->
    Debugged = get(self),
    Debugged ! {sys, self(), {command,Cmd}},
    meta_loop(Debugged, Bs, Ieval).

meta_loop(Debugged, Bs, #ieval{level=Le} = Ieval) ->
    receive

	%% The following messages can only be received when Meta is
	%% waiting for Debugged to evaluate non-interpreted code
	%% or a Bif. Le>1
	{sys, Debugged, {value,Val}} ->
	    {value, Val, Bs};
	{sys, Debugged, {value,Val,Bs2}} ->
	    {value, Val, merge_bindings(Bs2, Bs, Ieval)};
	{sys, Debugged, {exception,{Class,Reason,Stk}}} ->
	    case get(exit_info) of

		%% Error occurred outside of interpreted code.
		undefined ->
		    MakeStk0 = dbg_istk:delayed_stacktrace(),
		    MakeStk = fun(Depth0) ->
				      Depth = max(0, Depth0 - length(Stk)),
				      Stk ++ MakeStk0(Depth)
			      end,
		    do_exception(Class, Reason, MakeStk, Bs, Ieval);

		%% Error must have occured within a re-entry to
		%% interpreted code, simply raise the exception
		_ ->
		    erlang:Class(Reason)
	    end;

	%% Re-entry to Meta from non-interpreted code
	{re_entry, Debugged, {eval,{M,F,As}}} when Le =:= 1 ->
	    %% Reset process dictionary
	    %% This is really only necessary if the process left
	    %% interpreted code at a call level > 1
	    dbg_istk:init(),
	    put(stacktrace, []),
	    put(exit_info, undefined),
	    
	    dbg_iserver:cast(get(int), {set_status,self(),running,{}}),
	    dbg_icmd:tell_attached(running),

	    %% Tell attached process(es) to update source code.
	    dbg_icmd:tell_attached({re_entry,M,F}),

	    %% Send the result of the meta process
	    Debugged ! {sys,self(),eval_mfa(Debugged,M,F,As,Ieval)},

	    dbg_iserver:cast(get(int), {set_status,self(),idle,{}}),
	    dbg_icmd:tell_attached(idle),
	    meta_loop(Debugged, Bs, Ieval);

	%% Evaluation in Debugged results in call to interpreted
	%% function (probably? a fun)
	{re_entry, Debugged, {eval,{M,F,As}}} when Le>1 ->
	    Ieval2 = Ieval#ieval{module=undefined, line=-1},
	    Debugged ! {sys,self(),eval_mfa(Debugged,M,F,As,Ieval2)},
	    meta_loop(Debugged, Bs, Ieval);

	Msg ->
	    check_exit_msg(Msg, Bs, Ieval),
	    dbg_icmd:handle_msg(Msg, idle, Bs, Ieval),
	    meta_loop(Debugged, Bs, Ieval)
    end.

exit_loop(OrigPid, Reason, Bs, Ieval) ->
    receive
	Msg ->
	    check_exit_msg(Msg, Bs, Ieval),
	    dbg_icmd:handle_msg(Msg, exit_at, Bs, Ieval),
	    exit_loop(OrigPid, Reason, Bs, Ieval)
    end.

%%--Trace function----------------------------------------------------

%%--------------------------------------------------------------------
%% trace(What, Args)
%%   What = send | receivex | received | call | return | bif
%%   Args depends on What, see the code.
%%--------------------------------------------------------------------
trace(What, Args) ->
    trace(What, Args, get(trace)).

trace(return, {_Le,{dbg_apply,_,_,_}}, _Bool) ->
    ignore;
trace(What, Args, true) ->
    Fun = fun(P) -> format_trace(What, Args, P) end,
    dbg_icmd:tell_attached({trace_output, Fun});
trace(_What, _Args, false) ->
    ignore.

format_trace(What, Args, P) ->
    case What of
        send ->
            {To,Msg} = Args,
            io_lib:format("==> ~w : "++P++"~n", [To, Msg]);
        receivex ->
            {Le, TimeoutP} = Args,
            Tail = case TimeoutP of
                       true -> "with timeout~n";
                       false -> "~n"
                   end,
            io_lib:format("   (~w) receive " ++ Tail, [Le]);

        received when Args =:= null ->
            io_lib:format("~n", []);
        received -> % Args=Msg
            io_lib:format("~n<== "++P++"~n", [Args]);

        call ->
            {Called, {Le,Li,M,F,As}} = Args,
            case Called of
                extern ->	
                    io_lib:format("++ (~w) <~w> ~w:~tw~ts~n",
                                  [Le,Li,M,F,format_args(As, P)]);
                local ->
                    io_lib:format("++ (~w) <~w> ~tw~ts~n",
                                  [Le,Li,F,format_args(As, P)])
            end;
        call_fun ->
            {Le,Li,F,As} = Args,
            io_lib:format("++ (~w) <~w> ~tw~ts~n",
                          [Le, Li, F, format_args(As, P)]);
        return ->
            {Le,Val} = Args,
            io_lib:format("-- (~w) "++P++"~n", [Le, Val]);


        bif ->
            {Le,Li,M,F,As} = Args,
            io_lib:format("++ (~w) <~w> ~w:~tw~ts~n",
                          [Le, Li, M, F, format_args(As, P)])
    end.

format_args(As, P) when is_list(As) ->
    [$(,format_args1(As, P),$)];
format_args(A, P) ->
    [$/,io_lib:format(P, [A])].

format_args1([A], P) ->
    [io_lib:format(P, [A])];
format_args1([A|As], P) ->
    [io_lib:format(P, [A]),$,|format_args1(As, P)];
format_args1([], _) ->
    [].

%%--Other useful functions--------------------------------------------

%% Mimic catch behaviour
catch_value(error, Reason) ->
    {'EXIT',{Reason,get_stacktrace()}};
catch_value(exit, Reason) ->
    {'EXIT',Reason};
catch_value(throw, Reason) ->
    Reason.

%%--Code interpretation-----------------------------------------------

%%--------------------------------------------------------------------
%% Top level function of meta evaluator. 
%% Return message to be replied to the target process.
%%--------------------------------------------------------------------
eval_mfa(Debugged, M, F, As, #ieval{level=Le}=Ieval0) ->
    Int = get(int),
    Bs = erl_eval:new_bindings(),
    Ieval = Ieval0#ieval{level=Le+1,top=true},
    try do_eval_function(M, F, As, Bs, extern, Ieval) of
	{value, Val, _Bs} ->
	    trace(return, {Le,Val}),
	    {ready, Val}
    catch
	exit:{Debugged, Reason} ->
	    exit(Reason);
	exit:{Int, Reason} ->
	    exit(Reason);
	Class:Reason ->
	    {exception, {Class, Reason, get_stacktrace()}}
    end.

eval_function(Mod, Name, As, Bs, Called, Ieval0, Lc) ->
    Tail = Lc andalso get(trace_stack) =:= no_tail,
    case Tail of
	false ->
	    Ieval = dbg_istk:push(Bs, Ieval0, Lc),
	    {value,Val,_} = do_eval_function(Mod, Name, As, Bs, Called, Ieval),
	    dbg_istk:pop(),
	    trace(return, {Ieval#ieval.level,Val}),
	    {value,Val,Bs};
	true ->
	    do_eval_function(Mod, Name, As, Bs, Called, Ieval0)
    end.

do_eval_function(Mod, Fun, As0, Bs0, _, Ieval0) when is_function(Fun);
						    Mod =:= ?MODULE,
						    Fun =:= eval_fun orelse
						    Fun =:= eval_named_fun ->
    #ieval{level=Le,line=Li,top=Top} = Ieval0,
    case lambda(Fun, As0) of
	{[{clause,Fc,_,_,_}|_]=Cs,Module,Name,As,Bs} ->
	    Ieval = Ieval0#ieval{module=Module,function=Name,
				 arguments=As0,line=Fc},
	    trace(call_fun, {Le,Li,Name,As}),
	    fnk_clauses(Cs, As, Bs, Ieval);

	not_interpreted when Top -> % We are leaving interpreted code
	    trace(call_fun, {Le,Li,Fun,As0}),
	    {value, {dbg_apply,erlang,apply,[Fun,As0]}, Bs0};
	not_interpreted ->
	    trace(call_fun, {Le,Li,Fun,As0}),
	    debugged_cmd({apply,erlang,apply,[Fun,As0]}, Bs0, Ieval0);

	{error,Reason} ->
	    %% It's ok not to push anything in this case, the error
	    %% reason contains information about the culprit
	    %% ({badarity,{{Mod,Name},As}})
	    exception(error, Reason, Bs0, Ieval0)
    end;

do_eval_function(Mod, Name, As0, Bs0, Called, Ieval0) ->
    #ieval{level=Le,line=Li,top=Top} = Ieval0,
    trace(call, {Called, {Le,Li,Mod,Name,As0}}),
    Ieval = Ieval0#ieval{module=Mod,function=Name,arguments=As0},
    case get_function(Mod, Name, As0, Called) of
	[{clause,FcLine,_,_,_}|_]=Cs ->
	    fnk_clauses(Cs, As0, erl_eval:new_bindings(),
			Ieval#ieval{line=FcLine});

	not_interpreted when Top -> % We are leaving interpreted code
	    {value, {dbg_apply,Mod,Name,As0}, Bs0};
	not_interpreted ->
	    debugged_cmd({apply,Mod,Name,As0}, Bs0, Ieval);

	undef ->
	    exception(error, undef, Bs0, Ieval, true)
    end.

lambda(eval_fun, [Cs,As,Bs,{Mod,Name}=F]) ->
    %% Fun defined in interpreted code, called from outside
    if 
	length(element(3,hd(Cs))) =:= length(As) ->
	    db_ref(Mod),  %% Adds ref between module and process
	    {Cs,Mod,Name,As,Bs};
	true -> 
	    {error,{badarity,{F,As}}}
    end;
lambda(eval_named_fun, [Cs,As,Bs0,FName,RF,{Mod,Name}=F]) ->
    %% Fun defined in interpreted code, called from outside
    if
	length(element(3,hd(Cs))) =:= length(As) ->
	    db_ref(Mod),  %% Adds ref between module and process
	    Bs1 = add_binding(FName, RF, Bs0),
	    {Cs,Mod,Name,As,Bs1};
	true ->
	    {error,{badarity,{F,As}}}
    end;
lambda(Fun, As) when is_function(Fun) ->
    %% Fun called from within interpreted code...
    case erlang:fun_info(Fun, module) of

	%% ... and the fun was defined in interpreted code
	{module, ?MODULE} ->
	    {Mod,Name,Bs, Cs} =
		case erlang:fun_info(Fun, env) of
		    {env,[{{M,F},Bs0,Cs0}]} ->
		        {M,F,Bs0, Cs0};
		    {env,[{{M,F},Bs0,Cs0,FName}]} ->
		        {M,F,add_binding(FName, Fun, Bs0), Cs0}
		end,
	    {arity, Arity} = erlang:fun_info(Fun, arity),
	    if 
		length(As) =:= Arity ->
		    db_ref(Mod), %% Adds ref between module and process
		    {Cs,Mod,Name,As,Bs};
		true ->
		    {error,{badarity,{Fun,As}}}
	    end;

	%% ... and the fun was defined outside interpreted code
	_ ->
	    not_interpreted
    end.

get_function(Mod, Name, Args, local) ->
    Arity = length(Args),
    Key = {Mod,Name,Arity},
    case cached(Key) of
	false ->
	    DbRef = db_ref(Mod),
	    case dbg_idb:match_object(DbRef, {{Mod,Name,Arity,'_'},'_'}) of
		[{{Mod,Name,Arity,Exp},Clauses}] ->
		    cache(Key, {Exp,Clauses}),
		    Clauses;
		_ -> undef
	    end;
	{_Exp,Cs} -> Cs
    end;
get_function(Mod, Name, Args, extern) ->
    Arity = length(Args),
    Key = {Mod,Name,Arity},
    case cached(Key) of
	false ->
	    case db_ref(Mod) of
		not_found -> not_interpreted;
		DbRef ->
		    case dbg_idb:lookup(DbRef, {Mod,Name,Arity,true}) of
			{ok,Data} ->
			    cache(Key, {true,Data}),
			    Data;
			not_found ->
			    case dbg_idb:lookup(DbRef, module) of
				{ok,_} -> undef;
				not_found -> not_interpreted
			    end
		    end
	    end;
	{true,Cs} -> Cs;
	{false,_} -> undef
    end.

db_ref(Mod) ->
    case get(?DB_REF_KEY(Mod)) of
	undefined ->
	    case dbg_iserver:call(get(int),
				  {get_module_db, Mod, get(self)}) of
		not_found ->
		    not_found;
		ModDb ->
		    Node = node(get(int)),
		    DbRef = if
				Node =/= node() -> {Node,ModDb};
				true -> ModDb
			    end,
		    put(?DB_REF_KEY(Mod), DbRef),
		    DbRef
	    end;
	DbRef ->
	    DbRef
    end.

cache(Key, Data) ->
    put(cache, lists:sublist([{Key,Data}|get(cache)], 5)).
	    
cached(Key) ->
    case lists:keyfind(Key, 1, get(cache))  of
	{Key,Data} -> Data;
	false -> false
    end.

%% Try to find a matching function clause
%% #ieval.level is set, the other fields must be set in this function
fnk_clauses([{clause,Line,Pars,Gs,Body}|Cs], As, Bs0, Ieval) ->
    case head_match(Pars, As, [], Bs0) of
	{match,Bs1} ->
	    Bs = add_bindings(Bs1, Bs0),
	    case guard(Gs, Bs) of
		true ->
		    seq(Body, Bs, Ieval#ieval{line=Line});
		false ->
		    fnk_clauses(Cs, As, Bs0, Ieval)
	    end;
	nomatch ->
	    fnk_clauses(Cs, As, Bs0, Ieval)
    end;
fnk_clauses([], _As, Bs, Ieval) ->
    exception(error, function_clause, Bs, Ieval, true).

seq([E], Bs0, Ieval) ->
    case dbg_icmd:cmd(E, Bs0, Ieval) of
	{skip,Bs} ->
	    {value,skipped,Bs};
	Bs ->
	    expr(E, Bs, Ieval)
    end;
seq([E|Es], Bs0, Ieval) ->
    case dbg_icmd:cmd(E, Bs0, Ieval) of
	{skip,Bs} ->
	    seq(Es, Bs, Ieval);
	Bs1 ->
	    {value,_,Bs} = expr(E, Bs1, Ieval#ieval{top=false}),
	    seq(Es, Bs, Ieval)
    end;
seq([], Bs, _) ->
    {value,true,Bs}.

%% Variable
expr({var,Line,V}, Bs, Ieval) ->
    case binding(V, Bs) of
	{value,Val} ->
	    {value,Val,Bs};
	unbound ->
	    exception(error, {unbound,V}, Bs, Ieval#ieval{line=Line})
    end;

expr({value,_,Val}, Bs, _Ieval) ->
    {value,Val,Bs};
expr({value,Val}, Bs, _Ieval) -> % Special case straight values
    {value,Val,Bs};

%% List
expr({cons,Line,H0,T0}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line,top=false},
    {value,H,Bs1} = expr(H0, Bs0, Ieval),
    {value,T,Bs2} = expr(T0, Bs0, Ieval),
    {value,[H|T],merge_bindings(Bs2, Bs1, Ieval)};

%% Tuple
expr({tuple,Line,Es0}, Bs0, Ieval) ->
    {Vs,Bs} = eval_list(Es0, Bs0, Ieval#ieval{line=Line}),
    {value,list_to_tuple(Vs),Bs};

%% Map
expr({map,Line,Fs}, Bs0, Ieval) ->
    {Map,Bs} = eval_new_map_fields(Fs, Bs0, Ieval#ieval{line=Line,top=false},
				   fun expr/3),
    {value,Map,Bs};
expr({map,Line,E0,Fs0}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line,top=false},
    {value,E,Bs1} = expr(E0, Bs0, Ieval),
    {Fs,Bs2} = eval_map_fields(Fs0, Bs0, Ieval),
    _ = maps:put(k, v, E),			%Validate map.
    Value = lists:foldl(fun ({map_assoc,K,V}, Mi) -> maps:put(K,V,Mi);
			    ({map_exact,K,V}, Mi) -> maps:update(K,V,Mi)
			end, E, Fs),
    {value,Value,merge_bindings(Bs2, Bs1, Ieval)};
%% A block of statements
expr({block,Line,Es},Bs,Ieval) ->
    seq(Es, Bs, Ieval#ieval{line=Line});

%% Catch statement
expr({'catch',Line,Expr}, Bs0, Ieval) ->
    try expr(Expr, Bs0, Ieval#ieval{line=Line, top=false})
    catch
	Class:Reason ->
	    %% Exception caught, reset exit info
	    put(exit_info, undefined),
	    dbg_istk:pop(Ieval#ieval.level),
	    Value = catch_value(Class, Reason),
	    trace(return, {Ieval#ieval.level,Value}),
	    {value, Value, Bs0}
    end;

%% Try-catch statement
expr({'try',Line,Es,CaseCs,CatchCs,[]}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    try seq(Es, Bs0, Ieval#ieval{top=false}) of
	{value,Val,Bs} = Value ->
	    case CaseCs of
		[] -> Value;
		_ ->
		    case_clauses(Val, CaseCs, Bs, try_clause, Ieval)
	    end
    catch
	Class:Reason when CatchCs =/= [] ->
	    catch_clauses({Class,Reason,[]}, CatchCs, Bs0, Ieval)
    end;
expr({'try',Line,Es,CaseCs,CatchCs,As}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    try seq(Es, Bs0, Ieval#ieval{top=false}) of
	{value,Val,Bs} = Value ->
	    case CaseCs of
		[] -> Value;
		_ ->
		    case_clauses(Val, CaseCs, Bs, try_clause, Ieval)
	    end
    catch
	Class:Reason when CatchCs =/= [] ->
	    catch_clauses({Class,Reason,[]}, CatchCs, Bs0, Ieval)
    after
            seq(As, Bs0, Ieval#ieval{top=false})
    end;

%% Case statement
expr({'case',Line,E,Cs}, Bs0, Ieval) ->
    {value,Val,Bs} =
	expr(E, Bs0, Ieval#ieval{line=Line, top=false}),
    case_clauses(Val, Cs, Bs, case_clause, Ieval#ieval{line=Line});

%% If statement
expr({'if',Line,Cs}, Bs, Ieval) ->
    if_clauses(Cs, Bs, Ieval#ieval{line=Line});

%% Andalso/orelse
expr({'andalso',Line,E1,E2}, Bs0, Ieval) ->
    case expr(E1, Bs0, Ieval#ieval{line=Line, top=false}) of
        {value,false,_}=Res ->
           Res;
        {value,true,Bs} ->
            {value,Val,_} = expr(E2, Bs, Ieval#ieval{line=Line, top=false}),
            {value,Val,Bs};
        {value,Val,Bs} ->
            exception(error, {badarg,Val}, Bs, Ieval)
    end;
expr({'orelse',Line,E1,E2}, Bs0, Ieval) ->
    case expr(E1, Bs0, Ieval#ieval{line=Line, top=false}) of
        {value,true,_}=Res ->
           Res;
        {value,false,Bs} ->
            {value,Val,_} = expr(E2, Bs, Ieval#ieval{line=Line, top=false}),
            {value,Val,Bs};
        {value,Val,Bs} ->
            exception(error, {badarg,Val}, Bs, Ieval)
    end;

%% Matching expression
expr({match,Line,Lhs,Rhs0}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,Rhs,Bs1} = expr(Rhs0, Bs0, Ieval#ieval{top=false}),
    case match(Lhs, Rhs, Bs1) of
	{match,Bs} ->
	    {value,Rhs,Bs};
	nomatch ->
	    exception(error, {badmatch,Rhs}, Bs1, Ieval)
    end;

%% Construct a fun
expr({make_fun,Line,Name,Cs}, Bs, #ieval{module=Module}=Ieval) ->
    Arity = length(element(3,hd(Cs))),
    Info = {{Module,Name},Bs,Cs},
    Fun = 
	case Arity of
	    0 -> fun() -> eval_fun([], Info) end;
	    1 -> fun(A) -> eval_fun([A], Info) end;
	    2 -> fun(A,B) -> eval_fun([A,B], Info) end;
	    3 -> fun(A,B,C) -> eval_fun([A,B,C], Info) end;
	    4 -> fun(A,B,C,D) -> eval_fun([A,B,C,D], Info) end;
	    5 -> fun(A,B,C,D,E) -> eval_fun([A,B,C,D,E], Info) end;
	    6 -> fun(A,B,C,D,E,F) -> eval_fun([A,B,C,D,E,F], Info) end;
	    7 -> fun(A,B,C,D,E,F,G) -> 
			 eval_fun([A,B,C,D,E,F,G], Info) end;
	    8 -> fun(A,B,C,D,E,F,G,H) -> 
			 eval_fun([A,B,C,D,E,F,G,H], Info) end;
	    9 -> fun(A,B,C,D,E,F,G,H,I) -> 
			 eval_fun([A,B,C,D,E,F,G,H,I], Info) end;
	    10 -> fun(A,B,C,D,E,F,G,H,I,J) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J], Info) end;
	    11 -> fun(A,B,C,D,E,F,G,H,I,J,K) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K], Info) end;
	    12 -> fun(A,B,C,D,E,F,G,H,I,J,K,L) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L], Info) end;
	    13 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M], Info) end;
	    14 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N], Info) end;
	    15 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], Info) end;
	    16 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Info) end;
	    17 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], Info) end;
	    18 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], Info) end;
	    19 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],Info) end;
	    20 -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) -> 
			  eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],Info) end;
	    _Other ->
		exception(error, {'argument_limit',{'fun',Cs}}, Bs,
			  Ieval#ieval{line=Line})
	end,
    {value,Fun,Bs};

%% Construct a fun
expr({make_named_fun,Line,Name,FName,Cs}, Bs, #ieval{module=Module}=Ieval) ->
    Arity = length(element(3,hd(Cs))),
    Info = {{Module,Name},Bs,Cs,FName},
    Fun =
	case Arity of
	    0 -> fun RF() -> eval_named_fun([], RF, Info) end;
	    1 -> fun RF(A) -> eval_named_fun([A], RF, Info) end;
	    2 -> fun RF(A,B) ->
			 eval_named_fun([A,B], RF, Info) end;
	    3 -> fun RF(A,B,C) ->
			 eval_named_fun([A,B,C], RF, Info) end;
	    4 -> fun RF(A,B,C,D) ->
			 eval_named_fun([A,B,C,D], RF, Info) end;
	    5 -> fun RF(A,B,C,D,E) ->
			 eval_named_fun([A,B,C,D,E],
					RF, Info) end;
	    6 -> fun RF(A,B,C,D,E,F) ->
			 eval_named_fun([A,B,C,D,E,F],
					RF, Info) end;
	    7 -> fun RF(A,B,C,D,E,F,G) ->
			 eval_named_fun([A,B,C,D,E,F,G],
					RF, Info) end;
	    8 -> fun RF(A,B,C,D,E,F,G,H) ->
			 eval_named_fun([A,B,C,D,E,F,G,H],
					RF, Info) end;
	    9 -> fun RF(A,B,C,D,E,F,G,H,I) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I],
					RF, Info) end;
	    10 -> fun RF(A,B,C,D,E,F,G,H,I,J) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J],
					RF, Info) end;
	    11 -> fun RF(A,B,C,D,E,F,G,H,I,J,K) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K],
					RF, Info) end;
	    12 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L],
					RF, Info) end;
	    13 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M],
					RF, Info) end;
	    14 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N],
					RF, Info) end;
	    15 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],
					RF, Info) end;
	    16 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
					RF, Info) end;
	    17 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q],
					RF, Info) end;
	    18 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,
					     R],
					RF, Info) end;
	    19 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,
					     R,S],
					RF, Info) end;
	    20 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ->
			 eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,
					     R,S,T],
					RF, Info) end;
	    _Other ->
		exception(error, {'argument_limit',{named_fun,FName,Cs}}, Bs,
			  Ieval#ieval{line=Line})
	end,
    {value,Fun,Bs};

%% Construct an external fun.
expr({make_ext_fun,Line,MFA0}, Bs0, Ieval0) ->
    {[M,F,A],Bs} = eval_list(MFA0, Bs0, Ieval0),
    try erlang:make_fun(M, F, A) of
	Value ->
	    {value,Value,Bs}
    catch
	error:badarg ->
	    Ieval1 = Ieval0#ieval{line=Line},
	    Ieval2 = dbg_istk:push(Bs0, Ieval1, false),
	    Ieval = Ieval2#ieval{module=erlang,function=make_fun,
				 arguments=[M,F,A],line=-1},
	    exception(error, badarg, Bs, Ieval, true)
    end;

%% Local function call
expr({local_call,Line,F,As0,Lc}, Bs0, #ieval{module=M} = Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {As,Bs} = eval_list(As0, Bs0, Ieval),
    eval_function(M, F, As, Bs, local, Ieval, Lc);

%% Remote function call
expr({call_remote,Line,M,F,As0,Lc}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {As,Bs} = eval_list(As0, Bs0, Ieval),
    eval_function(M, F, As, Bs, extern, Ieval, Lc);

%% Emulated semantics of some BIFs
expr({dbg,Line,self,[]}, Bs, #ieval{level=Le}) ->
    trace(bif, {Le,Line,erlang,self,[]}),
    Self = get(self),
    trace(return, {Le,Self}),
    {value,Self,Bs};
expr({dbg,Line,get_stacktrace,[]}, Bs, #ieval{level=Le}) ->
    trace(bif, {Le,Line,erlang,get_stacktrace,[]}),
    Stacktrace = get_stacktrace(),
    trace(return, {Le,Stacktrace}),
    {value,Stacktrace,Bs};
expr({dbg,Line,raise,As0}, Bs0, #ieval{level=Le}=Ieval0) ->
    %% Since erlang:get_stacktrace/0 is emulated, we will
    %% need to emulate erlang:raise/3 too so that we can
    %% capture the stacktrace.
    Ieval = Ieval0#ieval{line=Line},
    {[Class,Reason,Stk0]=As,Bs} = eval_list(As0, Bs0, Ieval),
    trace(bif, {Le,Line,erlang,raise,As}),
    try
	%% Evaluate raise/3 for error checking and
	%% truncating of the stacktrace to the correct depth.
	Error = erlang:raise(Class, Reason, Stk0),
	trace(return, {Le,Error}),
	{value,Error,Bs}
    catch
	_:_:Stk ->                              %Possibly truncated.
	    StkFun = fun(_) -> Stk end,
	    do_exception(Class, Reason, StkFun, Bs, Ieval)
    end;
expr({dbg,Line,throw,As0}, Bs0, #ieval{level=Le}=Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {[Term],Bs} = eval_list(As0, Bs0, Ieval),
    trace(bif, {Le,Line,erlang,throw,[Term]}),
    exception(throw, Term, Bs, Ieval);
expr({dbg,Line,error,As0}, Bs0, #ieval{level=Le}=Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {[Term],Bs} = eval_list(As0, Bs0, Ieval),
    trace(bif, {Le,Line,erlang,error,[Term]}),
    exception(error, Term, Bs, Ieval);
expr({dbg,Line,exit,As0}, Bs0, #ieval{level=Le}=Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {[Term],Bs} = eval_list(As0, Bs0, Ieval),
    trace(bif, {Le,Line,erlang,exit,[Term]}),
    exception(exit, Term, Bs, Ieval);

%% Call to "safe" BIF, ie a BIF that can be executed in Meta process
expr({safe_bif,Line,M,F,As0}, Bs0, #ieval{level=Le}=Ieval0) ->
    Ieval1 = Ieval0#ieval{line=Line},
    {As,Bs} = eval_list(As0, Bs0, Ieval1),
    trace(bif, {Le,Line,M,F,As}),
    Ieval2 = dbg_istk:push(Bs0, Ieval1, false),
    Ieval = Ieval2#ieval{module=M,function=F,arguments=As,line=-1},
    {_,Value,_} = Res = safe_bif(M, F, As, Bs, Ieval),
    trace(return, {Le,Value}),
    dbg_istk:pop(),
    Res;

%% Call to a BIF that must be evaluated in the correct process
expr({bif,Line,M,F,As0}, Bs0, #ieval{level=Le}=Ieval0) ->
    Ieval1 = Ieval0#ieval{line=Line},
    {As,Bs} = eval_list(As0, Bs0, Ieval1),
    trace(bif, {Le,Line,M,F,As}),
    Ieval2 = dbg_istk:push(Bs0, Ieval1, false),
    Ieval = Ieval2#ieval{module=M,function=F,arguments=As,line=-1},
    {_,Value,_} = Res = debugged_cmd({apply,M,F,As}, Bs, Ieval),
    trace(return, {Le,Value}),
    dbg_istk:pop(),
    Res;

%% Call to an operation
expr({op,Line,Op,As0}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {As,Bs} = eval_list(As0, Bs0, Ieval),
    try apply(erlang,Op,As) of
	Value ->
	    {value,Value,Bs}
    catch
	Class:Reason ->
	    exception(Class, Reason, Bs, Ieval)
    end;

%% apply/2 (fun)
expr({apply_fun,Line,Fun0,As0,Lc}, Bs0, #ieval{level=Le}=Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    FunValue = case expr(Fun0, Bs0, Ieval) of
		   {value,{dbg_apply,Mx,Fx,Asx},Bsx} ->
		       debugged_cmd({apply,Mx,Fx,Asx},
				    Bsx, Ieval#ieval{level=Le+1});
		   OtherFunValue ->
		       OtherFunValue
	       end,
    case FunValue of
	{value,Fun,Bs1} when is_function(Fun) ->
	    {As,Bs} = eval_list(As0, Bs1, Ieval),
	    eval_function(undefined, Fun, As, Bs, extern, Ieval, Lc);
	{value,{M,F},Bs1} when is_atom(M), is_atom(F) ->
	    {As,Bs} = eval_list(As0, Bs1, Ieval),
	    eval_function(M, F, As, Bs, extern, Ieval, Lc);
	{value,BadFun,Bs1} ->
	    exception(error, {badfun,BadFun}, Bs1, Ieval)
    end;

%% apply/3
expr({apply,Line,As0,Lc}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {[M,F,As],Bs} = eval_list(As0, Bs0, Ieval),
    eval_function(M, F, As, Bs, extern, Ieval, Lc);
    
%% Receive statement
expr({'receive',Line,Cs}, Bs0, #ieval{level=Le}=Ieval) ->
    trace(receivex, {Le,false}),
    eval_receive(get(self), Cs, Bs0, Ieval#ieval{line=Line});

%% Receive..after statement
expr({'receive',Line,Cs,To,ToExprs}, Bs0, #ieval{level=Le}=Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,ToVal,ToBs} = expr(To, Bs0, Ieval#ieval{top=false}),
    trace(receivex, {Le,true}),
    check_timeoutvalue(ToVal, ToBs, To, Ieval),
    {Stamp,_} = statistics(wall_clock),
    eval_receive(get(self), Cs, ToVal, ToExprs, ToBs, Bs0,
		 0, Stamp, Ieval);

%% Send (!)
expr({send,Line,To0,Msg0}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    Ieval1 = Ieval#ieval{top=false},
    {value,To,Bs1} = expr(To0, Bs0, Ieval1),
    {value,Msg,Bs2} = expr(Msg0, Bs0, Ieval1),
    Bs = merge_bindings(Bs2, Bs1, Ieval),
    eval_send(To, Msg, Bs, Ieval);

%% Binary
expr({bin,Line,Fs}, Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line,top=false},
    try
	eval_bits:expr_grp(Fs, Bs0,
			   fun (E, B) -> expr(E, B, Ieval) end,
			   [],
			   false)
    catch
	Class:Reason ->
	    exception(Class, Reason, Bs0, Ieval)
    end;

%% List comprehension
expr({lc,_Line,E,Qs}, Bs, Ieval) ->
    eval_lc(E, Qs, Bs, Ieval);
expr({bc,_Line,E,Qs}, Bs, Ieval) ->
    eval_bc(E, Qs, Bs, Ieval);

%% Brutal exit on unknown expressions/clauses/values/etc.
expr(E, _Bs, _Ieval) ->
    erlang:error({'NYI',E}).

%% Interpreted fun() called from uninterpreted module, recurse
eval_fun(As, {Info,Bs,Cs}) ->
    dbg_debugged:eval(?MODULE, eval_fun, [Cs,As,Bs,Info]).

%% Interpreted named fun() called from uninterpreted module, recurse
eval_named_fun(As, RF, {Info,Bs,Cs,FName}) ->
    dbg_debugged:eval(?MODULE, eval_named_fun, [Cs,As,Bs,FName,RF,Info]).

%% eval_lc(Expr,[Qualifier],Bindings,IevalState) ->
%%	{value,Value,Bindings}.
%% This is evaluating list comprehensions "straight out of the book".
%% Copied from rv's implementation in erl_eval.
eval_lc(E, Qs, Bs, Ieval) ->
    {value,eval_lc1(E, Qs, Bs, Ieval),Bs}.

eval_lc1(E, [{generate,Line,P,L0}|Qs], Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,L1,Bs1} = expr(L0, Bs0, Ieval#ieval{top=false}),
    CompFun = fun(NewBs) -> eval_lc1(E, Qs, NewBs, Ieval) end,
    eval_generate(L1, P, Bs1, CompFun, Ieval);
eval_lc1(E, [{b_generate,Line,P,L0}|Qs], Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,Bin,_} = expr(L0, Bs0, Ieval#ieval{top=false}),
    CompFun = fun(NewBs) -> eval_lc1(E, Qs, NewBs, Ieval) end,
    eval_b_generate(Bin, P, Bs0, CompFun, Ieval);
eval_lc1(E, [{guard,Q}|Qs], Bs0, Ieval) ->
    case guard(Q, Bs0) of
	true -> eval_lc1(E, Qs, Bs0, Ieval);
	false -> []
    end;
eval_lc1(E, [Q|Qs], Bs0, Ieval) ->
    case expr(Q, Bs0, Ieval#ieval{top=false}) of
	{value,true,Bs} -> eval_lc1(E, Qs, Bs, Ieval);
	{value,false,_Bs} -> [];
	{value,V,Bs} -> exception(error, {bad_filter,V}, Bs, Ieval)
    end;
eval_lc1(E, [], Bs, Ieval) ->
    {value,V,_} = expr(E, Bs, Ieval#ieval{top=false}),
    [V].

%% eval_bc(Expr,[Qualifier],Bindings,IevalState) ->
%%	{value,Value,Bindings}.
%% This is evaluating list comprehensions "straight out of the book".
%% Copied from rv's implementation in erl_eval.
eval_bc(E, Qs, Bs, Ieval) ->
    Val = erlang:list_to_bitstring(eval_bc1(E, Qs, Bs, Ieval)),
    {value,Val,Bs}.

eval_bc1(E, [{generate,Line,P,L0}|Qs], Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,L1,Bs1} = expr(L0, Bs0, Ieval#ieval{top=false}),
    CompFun = fun(NewBs) -> eval_bc1(E, Qs, NewBs, Ieval) end,
    eval_generate(L1, P, Bs1, CompFun, Ieval);
eval_bc1(E, [{b_generate,Line,P,L0}|Qs], Bs0, Ieval0) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,Bin,_} = expr(L0, Bs0, Ieval#ieval{top=false}),
    CompFun = fun(NewBs) -> eval_bc1(E, Qs, NewBs, Ieval) end,
    eval_b_generate(Bin, P, Bs0, CompFun, Ieval);
eval_bc1(E, [{guard,Q}|Qs], Bs0, Ieval) ->
    case guard(Q, Bs0) of
	true -> eval_bc1(E, Qs, Bs0, Ieval);
	false -> []
    end;
eval_bc1(E, [Q|Qs], Bs0, Ieval) ->
    case expr(Q, Bs0, Ieval#ieval{top=false}) of
	{value,true,Bs} -> eval_bc1(E, Qs, Bs, Ieval);
	{value,false,_Bs} -> [];
	{value,V,Bs} -> exception(error, {bad_filter,V}, Bs, Ieval)
    end;
eval_bc1(E, [], Bs, Ieval) ->
    {value,V,_} = expr(E, Bs, Ieval#ieval{top=false}),
    [V].

eval_generate([V|Rest], P, Bs0, CompFun, Ieval) ->
    case catch match1(P, V, erl_eval:new_bindings(), Bs0) of
	{match,Bsn} ->
	    Bs2 = add_bindings(Bsn, Bs0),
            CompFun(Bs2) ++ eval_generate(Rest, P, Bs0, CompFun, Ieval);
	nomatch -> 
	    eval_generate(Rest, P, Bs0, CompFun, Ieval)
	end;
eval_generate([], _P, _Bs0, _CompFun, _Ieval) ->
    [];
eval_generate(Term, _P, Bs, _CompFun, Ieval) ->
    exception(error, {bad_generator,Term}, Bs, Ieval).

eval_b_generate(<<_/bitstring>>=Bin, P, Bs0, CompFun, Ieval) ->
    Mfun = match_fun(Bs0),
    Efun = fun(Exp, Bs) -> expr(Exp, Bs, #ieval{}) end,
    case eval_bits:bin_gen(P, Bin, erl_eval:new_bindings(), Bs0, Mfun, Efun) of
	{match,Rest,Bs1} ->
	    Bs2 = add_bindings(Bs1, Bs0),
	    CompFun(Bs2) ++ eval_b_generate(Rest, P, Bs0, CompFun, Ieval);
	{nomatch,Rest} ->
	    eval_b_generate(Rest, P, Bs0, CompFun, Ieval);
	done ->
	    []
    end;
eval_b_generate(Term, _P, Bs, _CompFun, Ieval) ->
    exception(error, {bad_generator,Term}, Bs, Ieval).

safe_bif(M, F, As, Bs, Ieval) ->
    try apply(M, F, As) of
       	Value ->
	    {value,Value,Bs}
    catch
	Class:Reason ->
	    exception(Class, Reason, Bs, Ieval, true)
    end.

eval_send(To, Msg, Bs, Ieval) ->
    try To ! Msg of
	Msg -> 
	    trace(send, {To,Msg}),
	    {value,Msg,Bs}
    catch
	Class:Reason ->
	    exception(Class, Reason, Bs, Ieval)
    end.

%% Start tracing of messages before fetching current messages in
%% the queue to make sure that no messages are lost. 
eval_receive(Debugged, Cs, Bs0,
	     #ieval{module=M,line=Line,level=Le}=Ieval) ->
    %% To avoid private message passing protocol between META
    %% and interpreted process.
    erlang:trace(Debugged,true,['receive']),
    {_,Msgs} = erlang:process_info(Debugged,messages),
    case receive_clauses(Cs, Bs0, Msgs) of
	nomatch ->
	    dbg_iserver:cast(get(int), {set_status, self(),waiting,{}}),
	    dbg_icmd:tell_attached({wait_at,M,Line,Le}),
	    eval_receive1(Debugged, Cs, Bs0, Ieval);
	{eval,B,Bs,Msg} ->
	    rec_mess(Debugged, Msg, Bs, Ieval),
	    seq(B, Bs, Ieval)
    end.

eval_receive1(Debugged, Cs, Bs0, Ieval) ->
    Msgs = do_receive(Debugged, Bs0, Ieval),
    case receive_clauses(Cs, Bs0, Msgs) of
	nomatch ->
	    eval_receive1(Debugged, Cs, Bs0, Ieval);
	{eval,B,Bs,Msg} ->
	    rec_mess(Debugged, Msg, Bs0, Ieval),
	    dbg_iserver:cast(get(int), {set_status, self(),running,{}}),
	    dbg_icmd:tell_attached(running),
	    seq(B, Bs, Ieval)
    end.

check_timeoutvalue(ToVal,_,_,_) when is_integer(ToVal), ToVal>=0 -> true;
check_timeoutvalue(infinity,_,_,_) -> true;
check_timeoutvalue(_ToVal, ToBs, To, Ieval) ->
    Line = element(2, To),
    exception(error, timeout_value, ToBs, Ieval#ieval{line=Line}).

eval_receive(Debugged, Cs, 0, ToExprs, ToBs, Bs0, 0, _Stamp, Ieval) ->
    {_,Msgs} = erlang:process_info(Debugged,messages),
    case receive_clauses(Cs, Bs0, Msgs) of
	nomatch ->
	    trace(received,null),
	    seq(ToExprs, ToBs, Ieval);
	{eval,B,Bs,Msg} ->
	    rec_mess_no_trace(Debugged, Msg, Bs0, Ieval),
	    seq(B, Bs, Ieval)
    end;
eval_receive(Debugged, Cs, ToVal, ToExprs, ToBs, Bs0,
	     0, Stamp, #ieval{module=M,line=Line,level=Le}=Ieval)->
    erlang:trace(Debugged,true,['receive']),
    {_,Msgs} = erlang:process_info(Debugged,messages),
    case receive_clauses(Cs, Bs0, Msgs) of
	nomatch ->
	    {Stamp1,Time1} = newtime(Stamp,ToVal),
	    dbg_iserver:cast(get(int), {set_status, self(),waiting,{}}),
	    dbg_icmd:tell_attached({wait_after_at,M,Line,Le}),
	    eval_receive(Debugged, Cs, Time1, ToExprs, ToBs, Bs0,
			 infinity,Stamp1, Ieval);
	{eval,B,Bs,Msg} ->
	    rec_mess(Debugged, Msg, Bs0, Ieval),
	    seq(B, Bs, Ieval)
    end;
eval_receive(Debugged, Cs, ToVal, ToExprs, ToBs, Bs0,
	     _, Stamp, Ieval) ->
    case do_receive(Debugged, ToVal, Stamp, Bs0, Ieval) of
	timeout ->
	    trace(received,null),
	    rec_mess(Debugged),
	    dbg_iserver:cast(get(int), {set_status, self(),running,{}}),
	    dbg_icmd:tell_attached(running),
	    seq(ToExprs, ToBs, Ieval);
	Msgs ->
	    case receive_clauses(Cs, Bs0, Msgs) of
		nomatch ->
		    {Stamp1,Time1} = newtime(Stamp,ToVal),
		    eval_receive(Debugged, Cs, Time1, ToExprs, ToBs,
				 Bs0, infinity,Stamp1, Ieval);
		{eval,B,Bs,Msg} ->
		    rec_mess(Debugged, Msg, Bs0, Ieval),
		    dbg_iserver:cast(get(int),
				     {set_status, self(), running, {}}),
		    dbg_icmd:tell_attached(running),
		    seq(B, Bs, Ieval)
	    end
    end.

do_receive(Debugged, Bs, Ieval) ->
    receive
	{trace,Debugged,'receive',Msg} ->
	    [Msg];
	Msg ->
	    check_exit_msg(Msg, Bs, Ieval),
	    dbg_icmd:handle_msg(Msg, wait_at, Bs, Ieval),
	    do_receive(Debugged, Bs, Ieval)
    end.

do_receive(Debugged, Time, Stamp, Bs, Ieval) ->
    receive
	{trace,Debugged,'receive',Msg} ->
	    [Msg];
	{user, timeout} ->
	    timeout;
	Msg ->
	    check_exit_msg(Msg, Bs, Ieval),
	    dbg_icmd:handle_msg(Msg, wait_after_at, Bs, Ieval),
	    {Stamp1,Time1} = newtime(Stamp,Time),
	    do_receive(Debugged, Time1, Stamp1, Bs, Ieval)
    after Time ->
	    timeout
    end.

newtime(Stamp,infinity) ->
    {Stamp,infinity};
newtime(Stamp,Time) ->
    {Stamp1,_} = statistics(wall_clock),
    case Time - (Stamp1 - Stamp) of
	NewTime when NewTime > 0 ->
	    {Stamp1,NewTime};
	_ ->
	    {Stamp1,0}
    end.

rec_mess(Debugged, Msg, Bs, Ieval) ->
    erlang:trace(Debugged, false, ['receive']),
    flush_traces(Debugged),
    Debugged ! {sys,self(),{'receive',Msg}},
    rec_ack(Debugged, Bs, Ieval).

rec_mess(Debugged) ->
    erlang:trace(Debugged, false, ['receive']),
    flush_traces(Debugged).

rec_mess_no_trace(Debugged, Msg, Bs, Ieval) ->
    Debugged ! {sys,self(),{'receive',Msg}},
    rec_ack(Debugged, Bs, Ieval).

rec_ack(Debugged, Bs, Ieval) ->
    receive
	{Debugged,rec_acked} ->
	    true;
	Msg ->
	    check_exit_msg(Msg, Bs, Ieval),
	    io:format("***WARNING*** Unexp msg ~p, ieval ~p~n",
		      [Msg, Ieval])
    end.

flush_traces(Debugged) ->
    receive
	{trace,Debugged,'receive',_} ->
	    flush_traces(Debugged)
    after 0 ->
	    true
    end.

%% eval_list(ExpressionList, Bindings, Ieval)
%%  Evaluate a list of expressions "in parallel" at the same level.
eval_list(Es, Bs, Ieval) ->
    eval_list_1(Es, [], Bs, Bs, Ieval#ieval{top=false}).

eval_list_1([E|Es], Vs, BsOrig, Bs0, Ieval) ->
    {value,V,Bs1} = expr(E, BsOrig, Ieval),
    eval_list_1(Es, [V|Vs], BsOrig, merge_bindings(Bs1, Bs0, Ieval), Ieval);
eval_list_1([], Vs, _, Bs, _Ieval) ->
    {lists:reverse(Vs,[]),Bs}.

%% if_clauses(Clauses, Bindings, Ieval)
if_clauses([{clause,_,[],G,B}|Cs], Bs, Ieval) ->
    case guard(G, Bs) of
	true ->
	    seq(B, Bs, Ieval);
	false ->
	    if_clauses(Cs, Bs, Ieval)
    end;
if_clauses([], Bs, Ieval) ->
    exception(error, if_clause, Bs, Ieval).

%% case_clauses(Value, Clauses, Bindings, Error, Ieval)
%%   Error = try_clause | case_clause
case_clauses(Val, [{clause,_,[P],G,B}|Cs], Bs0, Error, Ieval) ->
    case match(P, Val, Bs0) of
	{match,Bs} ->
	    case guard(G, Bs) of
		true ->
		    seq(B, Bs, Ieval);
		false ->
		    case_clauses(Val, Cs, Bs0, Error, Ieval)
	    end;
	nomatch ->
	    case_clauses(Val, Cs, Bs0, Error, Ieval)
    end;
case_clauses(Val,[], Bs, Error, Ieval) ->
    exception(error, {Error,Val}, Bs, Ieval).

%% catch_clauses(Exception, Clauses, Bindings, Ieval)
%%   Exception = {Class,Reason,[]}
catch_clauses(Exception, [{clause,_,[P],G,B}|CatchCs], Bs0, Ieval) ->
    case match(P, Exception, Bs0) of
	{match,Bs} ->
	    case guard(G, Bs) of
		true ->
		    %% Exception caught, reset exit info
		    put(exit_info, undefined),
		    dbg_istk:pop(Ieval#ieval.level),
		    seq(B, Bs, Ieval);
		false ->
		    catch_clauses(Exception, CatchCs, Bs0, Ieval)
	    end;
	nomatch ->
	    catch_clauses(Exception, CatchCs, Bs0, Ieval)
    end;
catch_clauses({Class,Reason,[]}, [], _Bs, _Ieval) ->
    erlang:Class(Reason).

receive_clauses(Cs, Bs0, [Msg|Msgs]) ->
    case rec_clauses(Cs, Bs0, Msg) of
	nomatch ->
	    receive_clauses(Cs, Bs0, Msgs);
	{eval,B,Bs} ->
	    {eval,B,Bs,Msg}
    end;
receive_clauses(_, _, []) ->
    nomatch.

rec_clauses([{clause,_,Pars,G,B}|Cs], Bs0, Msg) ->
    case rec_match(Pars, Msg, Bs0) of
	{match,Bs} ->
	    case guard(G, Bs) of
		true ->
		    trace(received, Msg),
		    {eval,B,Bs};
		false ->
		    rec_clauses(Cs, Bs0, Msg)
	    end;
	nomatch ->
	    rec_clauses(Cs, Bs0, Msg)
    end;
rec_clauses([], _, _) ->
    nomatch.

%% guard(GuardTests,Bindings)
%%  Evaluate a list of guards.
guard([], _) -> true;
guard(Gs, Bs) -> or_guard(Gs, Bs).
    
or_guard([G|Gs], Bs) ->
    %% Short-circuit OR.
    and_guard(G, Bs) orelse or_guard(Gs, Bs);
or_guard([], _) -> false.

and_guard([G|Gs], Bs) ->
    %% Short-circuit AND.
    case catch guard_expr(G, Bs) of
	{value,true} -> and_guard(Gs, Bs);
	_ -> false
    end;
and_guard([],_) -> true.

guard_exprs([A0|As0], Bs) ->
    {value,A} = guard_expr(A0, Bs),
    {values,As} = guard_exprs(As0, Bs),
    {values,[A|As]};
guard_exprs([], _) ->
    {values,[]}.

guard_expr({'andalso',_,E1,E2}, Bs) ->
    case guard_expr(E1, Bs) of
	{value,false}=Res -> Res;
	{value,true} ->
	    case guard_expr(E2, Bs) of
		{value,_Val}=Res -> Res
	    end
    end;
guard_expr({'orelse',_,E1,E2}, Bs) ->
    case guard_expr(E1, Bs) of
	{value,true}=Res -> Res;
	{value,false} ->
	    case guard_expr(E2, Bs) of
		{value,_Val}=Res -> Res
	    end
    end;
guard_expr({dbg,_,self,[]}, _) ->
    {value,get(self)};
guard_expr({safe_bif,_,erlang,'not',As0}, Bs) ->
    {values,As} = guard_exprs(As0, Bs),
    {value,apply(erlang, 'not', As)};
guard_expr({safe_bif,_,Mod,Func,As0}, Bs) ->
    {values,As} = guard_exprs(As0, Bs),
    {value,apply(Mod, Func, As)};
guard_expr({var,_,V}, Bs) ->
    {value,_} = binding(V, Bs);
guard_expr({value,_,Val}, _Bs) ->
    {value,Val};
guard_expr({cons,_,H0,T0}, Bs) ->
    {value,H} = guard_expr(H0, Bs),
    {value,T} = guard_expr(T0, Bs),
    {value,[H|T]};
guard_expr({tuple,_,Es0}, Bs) ->
    {values,Es} = guard_exprs(Es0, Bs),
    {value,list_to_tuple(Es)};
guard_expr({map,_,Fs}, Bs0) ->
    F = fun (G0, B0, _) ->
		{value,G} = guard_expr(G0, B0),
		{value,G,B0}
	end,
    {Map,_} = eval_new_map_fields(Fs, Bs0, #ieval{top=false}, F),
    {value,Map};
guard_expr({map,_,E0,Fs0}, Bs) ->
    {value,E} = guard_expr(E0, Bs),
    Fs = eval_map_fields_guard(Fs0, Bs),
    Value = lists:foldl(fun ({map_assoc,K,V}, Mi) -> maps:put(K,V,Mi);
                            ({map_exact,K,V}, Mi) -> maps:update(K,V,Mi) end,
                        E, Fs),
    {value,Value};
guard_expr({bin,_,Flds}, Bs) ->
    {value,V,_Bs} = 
	eval_bits:expr_grp(Flds, Bs,
			   fun(E,B) ->
				   {value,V} = guard_expr(E,B),
				   {value,V,B}
			   end, [], false),
    {value,V}.


%% eval_map_fields([Field], Bindings, IEvalState) ->
%%  {[{map_assoc | map_exact,Key,Value}],Bindings}

eval_map_fields(Fs, Bs, Ieval) ->
    eval_map_fields(Fs, Bs, Ieval, fun expr/3).

eval_map_fields_guard(Fs0, Bs) ->
    {Fs,_} = eval_map_fields(Fs0, Bs, #ieval{},
                             fun (G0, Bs0, _) ->
                            {value,G} = guard_expr(G0, Bs0),
                            {value,G,Bs0}
                    end),
    Fs.

eval_map_fields(Fs, Bs, Ieval, F) ->
    eval_map_fields(Fs, Bs, Ieval, F, []).

eval_map_fields([{map_field_assoc,Line,K0,V0}|Fs], Bs0, Ieval0, F, Acc) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,K,Bs1} = F(K0, Bs0, Ieval),
    {value,V,Bs2} = F(V0, Bs1, Ieval),
    eval_map_fields(Fs, Bs2, Ieval0, F, [{map_assoc,K,V}|Acc]);
eval_map_fields([{map_field_exact,Line,K0,V0}|Fs], Bs0, Ieval0, F, Acc) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,K,Bs1} = F(K0, Bs0, Ieval),
    {value,V,Bs2} = F(V0, Bs1, Ieval),
    eval_map_fields(Fs, Bs2, Ieval0, F, [{map_exact,K,V}|Acc]);
eval_map_fields([], Bs, _Ieval, _F, Acc) ->
    {lists:reverse(Acc),Bs}.

eval_new_map_fields(Fs, Bs0, Ieval, F) ->
    eval_new_map_fields(Fs, Bs0, Ieval, F, []).

eval_new_map_fields([{Line,K0,V0}|Fs], Bs0, Ieval0, F, Acc) ->
    Ieval = Ieval0#ieval{line=Line},
    {value,K,Bs1} = F(K0, Bs0, Ieval),
    {value,V,Bs2} = F(V0, Bs1, Ieval),
    eval_new_map_fields(Fs, Bs2, Ieval0, F, [{K,V}|Acc]);
eval_new_map_fields([], Bs, _, _, Acc) ->
    {maps:from_list(lists:reverse(Acc)),Bs}.

%% match(Pattern,Term,Bs) -> {match,Bs} | nomatch
match(Pat, Term, Bs) ->
    try match1(Pat, Term, Bs, Bs)
    catch
	Result -> Result
    end.

match1({value,_,V}, V, Bs,_BBs) ->
    {match,Bs};
match1({var,_,'_'}, Term, Bs,_BBs) -> % Anonymous variable matches
    {match,add_anon(Term, Bs)};   % everything,save it anyway
match1({var,_,Name}, Term, Bs, _BBs) ->
    case binding(Name, Bs) of
	{value,Term} ->
	    {match,Bs};
	{value,_} ->
	    throw(nomatch);
	unbound ->
	    {match,[{Name,Term}|Bs]} % Add the new binding
    end;
match1({match,_,Pat1,Pat2}, Term, Bs0, BBs) ->
    {match,Bs1} = match1(Pat1, Term, Bs0, BBs),
    match1(Pat2, Term, Bs1, BBs);
match1({cons,_,H,T}, [H1|T1], Bs0, BBs) ->
    {match,Bs} = match1(H, H1, Bs0, BBs),
    match1(T, T1, Bs, BBs);
match1({tuple,_,Elts}, Tuple, Bs, BBs) 
  when length(Elts) =:= tuple_size(Tuple) ->
    match_tuple(Elts, Tuple, 1, Bs, BBs);
match1({map,_,Fields}, Map, Bs, BBs) when is_map(Map) ->
    match_map(Fields, Map, Bs, BBs);
match1({bin,_,Fs}, B, Bs0, BBs) when is_bitstring(B) ->
    try eval_bits:match_bits(Fs, B, Bs0, BBs,
			     match_fun(BBs),
			     fun(E, Bs) -> expr(E, Bs, #ieval{}) end,
			     false)
    catch
	_:_ -> throw(nomatch)
    end;
match1(_,_,_,_) ->
    throw(nomatch).

match_fun(BBs) ->
    fun(match, {L,R,Bs}) -> match1(L, R, Bs, BBs);
       (binding, {Name,Bs}) -> binding(Name, Bs);
       (add_binding, {Name,Val,Bs}) -> add_binding(Name, Val, Bs)
    end.

match_tuple([E|Es], Tuple, I, Bs0, BBs) ->
    {match,Bs} = match1(E, element(I, Tuple), Bs0, BBs),
    match_tuple(Es, Tuple, I+1, Bs, BBs);
match_tuple([], _, _, Bs, _BBs) ->
    {match,Bs}.

match_map([{map_field_exact,_,K0,Pat}|Fs], Map, Bs0, BBs) ->
    {value,K,BBs} = expr(K0, BBs, #ieval{}),
    case maps:find(K, Map) of
        {ok,Value} ->
            {match,Bs} = match1(Pat, Value, Bs0, BBs),
            match_map(Fs, Map, Bs, BBs);
        error -> throw(nomatch)
    end;
match_map([], _, Bs, _BBs) ->
    {match,Bs}.

head_match([Par|Pars], [Arg|Args], Bs0, BBs) ->
    try match1(Par, Arg, Bs0, BBs) of
	{match,Bs} -> head_match(Pars, Args, Bs, BBs)
    catch 
	Result -> Result
    end;
head_match([],[],Bs,_) -> {match,Bs}.

rec_match([Par],Msg,Bs0) ->
    match(Par,Msg,Bs0).

binding(Name,[{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_,_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_|Bs]) ->
    binding(Name,Bs);
binding(_,[]) ->
    unbound.

add_anon(Val,[{'_',_}|Bs]) ->
    [{'_',Val}|Bs];
add_anon(Val,[B1,{'_',_}|Bs]) ->
    [B1,{'_',Val}|Bs];
add_anon(Val,[B1,B2,{'_',_}|Bs]) ->
    [B1,B2,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,{'_',_}|Bs]) ->
    [B1,B2,B3,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,B4,{'_',_}|Bs]) ->
    [B1,B2,B3,B4,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,B4,B5,{'_',_}|Bs]) ->
    [B1,B2,B3,B4,B5,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,B4,B5,B6|Bs]) ->
    [B1,B2,B3,B4,B5,B6|add_anon(Val,Bs)];
add_anon(Val,[B1,B2,B3,B4,B5|Bs]) ->
    [B1,B2,B3,B4,B5|add_anon(Val,Bs)];
add_anon(Val,[B1,B2,B3,B4|Bs]) ->
    [B1,B2,B3,B4|add_anon(Val,Bs)];
add_anon(Val,[B1,B2,B3|Bs]) ->
    [B1,B2,B3|add_anon(Val,Bs)];
add_anon(Val,[B1,B2|Bs]) ->
    [B1,B2|add_anon(Val,Bs)];
add_anon(Val,[B1|Bs]) ->
    [B1|add_anon(Val,Bs)];
add_anon(Val,[]) ->
    [{'_',Val}].

%% merge_bindings(Bindings1, Bindings2, Ieval)
%% Merge bindings detecting bad matches. 
%% Special case '_',save the new one !!!
%% Bindings1 is the newest bindings.
merge_bindings(Bs, Bs, _Ieval) ->
    Bs; % Identical bindings
merge_bindings([{Name,V}|B1s], B2s, Ieval) ->
    case binding(Name, B2s) of
	{value,V} -> % Already there, and the same
	    merge_bindings(B1s, B2s, Ieval);
	{value,_} when Name =:= '_' -> % Already there, but anonymous
	    B2s1 = lists:keydelete('_', 1, B2s),
	    [{Name,V}|merge_bindings(B1s, B2s1, Ieval)];
	{value,_} -> % Already there, but different => badmatch
	    exception(error, {badmatch,V}, B2s, Ieval);
	unbound -> % Not there,add it
	    [{Name,V}|merge_bindings(B1s, B2s, Ieval)]
    end;
merge_bindings([], B2s, _Ieval) ->
    B2s.

%% add_bindings(Bindings1,Bindings2)
%% Add Bindings1 to Bindings2. Bindings in
%% Bindings1 hides bindings in Bindings2.
%% Used in list comprehensions (and funs).
add_bindings(Bs1,[]) ->
    Bs1;
add_bindings([{Name,V}|Bs],ToBs0) ->
    ToBs = add_binding(Name,V,ToBs0),
    add_bindings(Bs,ToBs);
add_bindings([],ToBs) ->
    ToBs.

add_binding(N,Val,[{N,_}|Bs]) ->
    [{N,Val}|Bs];
add_binding(N,Val,[B1,{N,_}|Bs]) ->
    [B1,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,{N,_}|Bs]) ->
    [B1,B2,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,{N,_}|Bs]) ->
    [B1,B2,B3,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,B4,{N,_}|Bs]) ->
    [B1,B2,B3,B4,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,B4,B5,{N,_}|Bs]) ->
    [B1,B2,B3,B4,B5,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,B4,B5,B6|Bs]) ->
    [B1,B2,B3,B4,B5,B6|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2,B3,B4,B5|Bs]) ->
    [B1,B2,B3,B4,B5|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2,B3,B4|Bs]) ->
    [B1,B2,B3,B4|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2,B3|Bs]) ->
    [B1,B2,B3|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2|Bs]) ->
    [B1,B2|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1|Bs]) ->
    [B1|add_binding(N,Val,Bs)];
add_binding(N,Val,[]) ->
    [{N,Val}].

%% get_stacktrace() -> Stacktrace
%%  Return the latest stacktrace for the process.
get_stacktrace() ->
    case get(stacktrace) of
	MakeStk when is_function(MakeStk, 1) ->
	    %% The stacktrace has not been constructed before.
	    %% Construct it and remember the result.
	    Depth = erlang:system_flag(backtrace_depth, 8),
	    erlang:system_flag(backtrace_depth, Depth),
	    Stk = MakeStk(Depth),
	    put(stacktrace, Stk),
	    Stk;
	Stk when is_list(Stk) ->
	    Stk
    end.
