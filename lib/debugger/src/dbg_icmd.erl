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
-module(dbg_icmd).

%% Internal command receiver/handler
-export([cmd/3]).

%% User control of process execution and settings
-export([step/1, next/1, continue/1, finish/1, skip/1, timeout/1,
	 stop/1]).
-export([eval/2]).
-export([set/3, get/3]).
-export([handle_msg/4]).

%% Library functions for attached process handling
-export([tell_attached/1]).

%% get_binding/2
-export([get_binding/2]).

-include("dbg_ieval.hrl").

%%====================================================================
%% Internal command receiver/handler
%%====================================================================

%%--------------------------------------------------------------------
%% cmd(Expr, Bs, Ieval) -> {skip, Bs} | Bs
%% This function is called from dbg_ieval before evaluating any
%% expression to give the user the chance to inspect variables etc.
%% get(next_break) => break | running
%%                  | Le
%% specifies if the process should break.
%%--------------------------------------------------------------------

cmd(Expr, Bs, Ieval) ->
    cmd(Expr, Bs, get(next_break), Ieval).

%% Evaluation should break
cmd(Expr, Bs, break, Ieval) ->
    break(Expr, Bs, Ieval);
%% Evaluation should continue, unless there is a breakpoint at
%% the current line
cmd(Expr, Bs, running, #ieval{level=Le,module=M}=Ieval) ->
    Line = element(2, Expr),
    case break_p(M, Line, Le, Bs) of
	true ->
	    put(next_break, break),
	    break(Expr, Bs, Ieval);
	false ->
	    handle_cmd(Bs, running, Ieval)
    end;
%% Evaluation should continue for now (until we've returned to
%% call level Next)
cmd(Expr, Bs, Next, #ieval{level=Le}=Ieval) when is_integer(Next),
						 Next<Le ->
    Line = element(2, Expr),
    handle_cmd(Bs, Next, Ieval#ieval{line=Line});
%% Evaluation has returned to call level Next, break
cmd(Expr, Bs, Next, #ieval{level=Le}=Ieval) when is_integer(Next),
                                                 Next>=Le ->
    put(next_break, break),
    break(Expr, Bs, Ieval).

%% break_p(Mod, Line, Le, Bs) -> true | false
%% Checks if there is a breakpoint at Line in Mod.
%% As a side effect, disables or deletes breakpoint as specified
break_p(Mod, Line, Le, Bs) ->
    case lists:keysearch({Mod, Line}, 1, get(breakpoints)) of
	{value, {_Point, [active, Action, _, Cond]}} ->
	    case get(user_eval) of
		[{Line, Le}|_] -> false;
		_ ->
		    Bool = case Cond of
			       null -> true;
			       {CM, CN} ->
				   try CM:CN(Bs) of
				       true -> true;
				       false -> false;
				       _Term -> false
				   catch
				       _C:_R -> false
				   end
			   end,
		    if
			Bool ->
			    case Action of
				enable -> ignore;
				disable ->
				    dbg_iserver:cast(get(int),
						     {break_option,
						      {Mod, Line},
						      status,
						      inactive});
				delete ->
				    dbg_iserver:cast(get(int),
						     {delete_break,
						      {Mod, Line}})
			    end;
			true -> ignore
		    end,
		    Bool
	    end;
	_Other -> % {value, {_Point, [inactive|_]}} | false
	    false
    end.

%% Called whenever evaluation enters break mode, informs attached
%% process and dbg_iserver
break(Expr, Bs, #ieval{level=Le,module=M}=Ieval) ->
    Line = element(2, Expr),
    dbg_iserver:cast(get(int), {set_status,self(),break,{M,Line}}),
    tell_attached({break_at,M,Line,Le}),
    handle_cmd(Bs, break, Ieval#ieval{line=Line}).

%%--------------------------------------------------------------------
%% handle_cmd(Bs0, Status, Ieval) -> Bs1 | {skip, Bs1}
%%   Status = break | running | Le
%% In break mode, loop waiting for user commands (and handle other
%% messages meanwhile).
%% In other modes, handle other messages, if any.
%%--------------------------------------------------------------------
handle_cmd(Bs, break, #ieval{level=Le}=Ieval) ->
    receive
	{user, {cmd, Cmd}} ->
	    dbg_iserver:cast(get(int), {set_status,self(),running,{}}),
	    tell_attached(running),
	    case Cmd of
		step -> Bs;
		next -> put(next_break, Le), Bs;
		continue -> put(next_break, running), Bs;
		finish -> put(next_break, Le-1), Bs;
		skip -> {skip, Bs}
	    end;
	{user, {eval, Cmd}} ->
	    Bs1 = eval_nonrestricted(Cmd, Bs, Ieval),
	    handle_cmd(Bs1, break, Ieval);
	Msg ->
	    dbg_ieval:check_exit_msg(Msg, Bs, Ieval),
	    handle_msg(Msg, break, Bs, Ieval),
	    handle_cmd(Bs, break, Ieval)
    end;
handle_cmd(Bs, Status, Ieval) ->
    receive
	Msg ->
	    dbg_ieval:check_exit_msg(Msg, Bs, Ieval),
	    handle_msg(Msg, Status, Bs, Ieval),
	    handle_cmd(Bs, Status, Ieval)
    after 0 -> 
	    Bs
    end.

%%====================================================================
%% User control of process execution and settings
%%====================================================================

step(Meta) ->     Meta ! {user, {cmd, step}}, ok.
next(Meta) ->     Meta ! {user, {cmd, next}}, ok.
continue(Meta) -> Meta ! {user, {cmd, continue}}, ok.
finish(Meta) ->   Meta ! {user, {cmd, finish}}, ok.
skip(Meta) ->     Meta ! {user, {cmd, skip}}.

timeout(Meta) ->  Meta ! {user, timeout}.

stop(Meta) ->     Meta ! {user, {cmd, stop}}.

eval(Meta, {Mod, Cmd}) ->
    eval(Meta, {Mod, Cmd, nostack});
eval(Meta, {Mod, Cmd, SP}) ->
    Cmd2 = case lists:reverse(Cmd) of    % Commands must end with ".\n"
	       [10,$.|_] -> Cmd;
	       [10|T] -> lists:reverse([10,$.|T]);
	       [$.|T] -> lists:reverse([10,$.|T]);
	       T -> lists:reverse([10,$.|T])
	   end,
    Meta ! {user, {eval, {self(), Mod, Cmd2, SP}}}.

%% Tag           Args
%% ---           ----
%% trace         Bool
%% stack_trace   Flag
set(Meta, Tag, Args) ->
    Meta ! {user, {set, Tag, Args}}.

%% Tag          Args           Reply
%% ---          ----           -----
%% bindings     nostack | SP   [{Var,Val}]  (Var=atom(), Val=term())
%% stack_frame  {up|down, SP}  [{Le,Where,Bs}] | top | bottom
%%                                          (Where = {Mod,Li}
%% messages     null           [Msg]        (Msg=term())
%% backtrace    all | N        [{Le,MFA}]   (MFA={M,F,Args}|{Fun,Args})
get(Meta, Tag, Args) ->
    Meta ! {user, {get, Tag, self(), Args}},
    receive
	{Meta, Tag, Reply} -> Reply
    end.

%%--------------------------------------------------------------------
%% handle_msg({int, Msg} | {user, Msg}, Status, Bs, Ieval)
%%   Status = idle | exit_at | wait_at | wait_after_at
%%         | break | running | Le | {Le,MFA}
%%--------------------------------------------------------------------
handle_msg({int, Msg}, Status, Bs, Ieval) ->
    handle_int_msg(Msg, Status, Bs, Ieval);
handle_msg({user, Msg}, Status, Bs, Ieval) ->
    handle_user_msg(Msg, Status, Bs, Ieval);
handle_msg(Msg, Status, Bs, Ieval) ->
    io:format("***WARNING*** Unexp msg ~p, info ~p~n", 
	      [Msg,{Status,Bs,Ieval}]).

%% handle_int_msg(Msg, Status, Bs, Ieval)
%%   Msg = {attached, AttPid} | {detached, AttPid}
%%       | {old_code, Mod}
%%       | {new_break, Break} | {delete_break, Break}
%%       | {break_options, {Break, Options}}
%%       | no_break | {no_break, Mod}
%%       | stop (only when Status==exit_at, means AttPid has terminated)
%% Interpreter internal messages (from dbg_iserver)
handle_int_msg({attached, AttPid}, Status, _Bs, 
	       #ieval{level=Le,module=M,line=Line}) ->

    %% Update process dictionary
    put(attached, AttPid),
    put(next_break, break),

    %% Tell attached process in which module evalution is located
    if
	Le =:= 1 ->
	    tell_attached({attached, undefined, -1, get(trace)});
	true ->
	    tell_attached({attached, M, Line, get(trace)}),

	    %% Give info about status and call level as well
	    %% In this case, Status cannot be exit_at
	    Msg = case Status of
		      idle -> {func_at,M,Line,Le};
		      break -> {break_at,M,Line,Le};
		      wait_at -> {wait_at,M,Line,Le};
		      wait_after_at -> {wait_after_at,M,Line,Le};
		      _ -> running % running | Le | {Le,MFA}
		  end,
	    tell_attached(Msg)
    end;
handle_int_msg(detached, _Status, _Bs, _Ieval) ->
    %% Update process dictionary
    put(attached, undefined),
    put(next_break, running),
    put(trace, false); % no need for tracing if there is no AttPid
handle_int_msg({old_code,Mod}, Status, Bs,
	       #ieval{level=Le,module=M}=Ieval) ->
    if
	Status =:= idle, Le =:= 1 ->
	    erase(?DB_REF_KEY(Mod)),
	    put(cache, []);
	true ->
	    case dbg_istk:in_use_p(Mod, M) of
		true ->
		    %% A call to Mod is on the stack (or might be),
		    %% so we must terminate.
		    exit(get(self), kill),
		    dbg_ieval:exception(exit, old_code, Bs, Ieval);
		false ->
		    erase(?DB_REF_KEY(Mod)),
		    put(cache, [])
	    end
    end;
handle_int_msg({new_break, Break}, _Status, _Bs, _Ieval) ->
    put(breakpoints, [Break | get(breakpoints)]);
handle_int_msg({delete_break, Point}, _Status, _Bs, _Ieval) ->
    put(breakpoints, lists:keydelete(Point, 1, get(breakpoints)));
handle_int_msg({break_options, Break}, _Status, _Bs, _Ieval) ->
    {Point, _Options} = Break,
    put(breakpoints, lists:keyreplace(Point,1,get(breakpoints), Break));
handle_int_msg(no_break, _Status, _Bs, _Ieval) ->
    put(breakpoints, []);
handle_int_msg({no_break,M}, _Status, _Bs, _Ieval) ->
    put(breakpoints, [ML || {Mod,_L}=ML <- get(breakpoints), Mod=/=M]);
handle_int_msg(stop, exit_at, _Bs, _Ieval) ->
    erlang:exit(normal).

%% handle_user_msg(Msg, Status, Bs, Ieval)
%%   Msg = {cmd, Cmd}, Cmd = step | next | continue | finish| skip| stop
%%       | timeout
%%       | {eval, {Pid, Mod, Str, SP}}
%%       | {set, Tag, Args} | {get, Tag, Pid, Args}
%% Messages from the attached process
%% Msg = {cmd, Cmd}, Cmd /= stop, can only be received in break mode,
%% handled in handle_cmd/3
%% Msg = timeout is handled when needed (when evaluating receive..after)
%% in dbg_ieval:do_receive/5 when Status==wait_after_at
%% For all other Status, it should be ignored
handle_user_msg({cmd, stop}, Status, _Bs, _Ieval) ->
    case lists:member(Status, [running, wait_at, wait_after_at]) of
	true ->
	    put(next_break, break);
	false when is_integer(Status); is_tuple(Status) ->
	    put(next_break, break);
	false -> % idle | exit_at (| break)
	    ignore
    end;
handle_user_msg({cmd, continue}, Status, _Bs, _Ieval) ->
    %% Allow leaving break mode when waiting in a receive
    case lists:member(Status, [wait_at, wait_after_at]) of
	true ->
	    put(next_break, running);
	false ->
	    ignore
    end;
handle_user_msg({cmd, _Cmd}, _Status, _Bs, _Ieval) ->
    ignore;
handle_user_msg(timeout, _Status, _Bs, _Ieval) ->
    ignore;
handle_user_msg({eval,Cmd}, wait_at, Bs, _Ieval) ->
    eval_restricted(Cmd, Bs);
handle_user_msg({eval,Cmd}, wait_after_at, Bs, _Ieval) ->
    eval_restricted(Cmd, Bs);
handle_user_msg({set,trace,Bool}, _Status, _Bs, _Ieval) ->
    put(trace, Bool),
    tell_attached({trace, Bool});
handle_user_msg({set,stack_trace,Flag}, _Status, _Bs, _Ieval) ->
    set_stack_trace(Flag);
handle_user_msg({get,bindings,From,SP}, _Status, Bs, _Ieval) ->
    reply(From, bindings, bindings(Bs, SP));
handle_user_msg({get,stack_frame,From,{Dir,SP}}, _Status, _Bs,_Ieval) ->
    reply(From, stack_frame, dbg_istk:stack_frame(Dir, SP));
handle_user_msg({get,messages,From,_}, _Status, _Bs, _Ieval) ->
    reply(From, messages, messages());
handle_user_msg({get,backtrace,From,N}, _Status, _Bs, Ieval) ->
    reply(From, backtrace, dbg_istk:backtrace(N, Ieval)).

set_stack_trace(true) ->
    set_stack_trace(all);
set_stack_trace(Flag) ->    
    if
	Flag =:= false ->
	    put(stack, []);
	Flag =:= no_tail; Flag =:= all ->
	    ignore
    end,
    put(trace_stack, Flag),
    tell_attached({stack_trace, Flag}).

reply(From, Tag, Reply) ->
    From ! {self(), Tag, Reply}.

bindings(Bs, nostack) ->
    Bs;
bindings(Bs, SP) ->
    case dbg_istk:stack_level() of
	Le when SP > Le ->
	    Bs;
	_ ->
	    dbg_istk:bindings(SP)
    end.

messages() ->
    {messages, Msgs} = erlang:process_info(get(self), messages),
    Msgs.

%%====================================================================
%% Evaluating expressions within process context
%%====================================================================

eval_restricted({From,_Mod,Cmd,SP}, Bs) ->
    case catch parse_cmd(Cmd, 1) of
	{'EXIT', _Reason} ->
	    From ! {self(), {eval_rsp, 'Parse error'}};
	{[{var,_,Var}], XBs} ->
	    Bs2 = bindings(Bs, SP),
	    Res = case get_binding(Var, Bs2) of
		      {value, Value} -> Value;
		      unbound ->
                          case get_binding(Var, XBs) of
                              {value, _} ->
                                  'Only possible to inspect variables';
                              unbound -> unbound
                          end
		  end,
	    From ! {self(), {eval_rsp, Res}};
	{_Forms, _XBs} ->
	    Rsp = 'Only possible to inspect variables',
	    From ! {self(), {eval_rsp, Rsp}}
    end.

eval_nonrestricted({From,Mod,Cmd,SP}, Bs, #ieval{level=Le}) when SP < Le->
    %% Evaluate in stack
    _ = eval_restricted({From, Mod, Cmd, SP}, Bs),
    Bs;
eval_nonrestricted({From, _Mod, Cmd, _SP}, Bs, 
		   #ieval{level=Le,module=M,line=Line}=Ieval) ->
    case catch parse_cmd(Cmd, Line) of
	{'EXIT', _Reason} ->
	    From ! {self(), {eval_rsp, 'Parse error'}},
	    Bs;
	{Forms, XBs} ->
	    mark_running(Line, Le),
            Bs1 = merge_bindings(Bs, XBs),
	    {Res, Bs2} =
		lists:foldl(fun(Expr, {_Res, Bs0}) ->
				    eval_nonrestricted_1(Expr,Bs0,Ieval)
			    end,
			    {null, Bs1},
			    Forms),
	    mark_break(M, Line, Le),
	    From ! {self(), {eval_rsp, Res}},
	    remove_binding_structs(Bs2, XBs)
    end.

eval_nonrestricted_1({match,_,{var,_,Var},Expr}, Bs, Ieval) ->
    {Res,Bs2} = eval_expr(Expr, Bs, Ieval),
    Bs3 = case lists:keyfind(Var, 1, Bs) of
	      {Var,_Value} ->
		  lists:keyreplace(Var, 1, Bs2, {Var,Res});
	      false -> [{Var,Res} | Bs2]
	  end,
    {Res,Bs3};
eval_nonrestricted_1({var,_,Var}, Bs, _Ieval) ->
    Res = case lists:keyfind(Var, 1, Bs) of
	      {Var, Value} -> Value;
	      false -> unbound
	  end,
    {Res,Bs};
eval_nonrestricted_1(Expr, Bs, Ieval) ->
    eval_expr(Expr, Bs, Ieval).

eval_expr(Expr, Bs, Ieval) ->
    {value,Res,Bs2} =
        dbg_ieval:eval_expr(Expr, Bs, Ieval#ieval{top=false}),
    {Res,Bs2}.

%% XBs have unique keys.
merge_bindings(Bs1, XBs) ->
    Bs1 ++ erl_eval:bindings(XBs).

remove_binding_structs(Bs1, XBs) ->
    lists:foldl(fun({N, _V}, Bs) -> lists:keydelete(N, 1, Bs)
                end, Bs1, erl_eval:bindings(XBs)).

mark_running(LineNo, Le) ->
    put(next_break, running),
    put(user_eval, [{LineNo, Le} | get(user_eval)]),
    dbg_iserver:cast(get(int), {set_status, self(), running, {}}),
    tell_attached(running).

mark_break(Cm, LineNo, Le) ->
    put(next_break, break),
    put(user_eval, tl(get(user_eval))),
    tell_attached({break_at, Cm, LineNo, Le}),
    dbg_iserver:cast(get(int), {set_status,self(),break,{Cm,LineNo}}).

parse_cmd(Cmd, LineNo) ->
    {ok,Tokens,_} = erl_scan:string(Cmd, LineNo, [text]),
    {ok,Forms,Bs} = erl_eval:extended_parse_exprs(Tokens),
    {Forms, Bs}.

%%====================================================================
%% Library functions for attached process handling
%%====================================================================

tell_attached(Msg) ->
    case get(attached) of
	undefined -> ignore;
	AttPid ->
	    AttPid ! {self(), Msg},
            ignore
    end.

%%====================================================================
%% get_binding/2
%%====================================================================

get_binding(Var, Bs) ->
    case lists:keyfind(Var, 1, Bs) of
	{Var, Value} -> {value, Value};
	false -> unbound
    end.
