%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% @doc Generic connection owner process.
%%%
%%% @type handle() = pid(). A handle for using a connection implemented
%%% with ct_gen_conn.erl.

-module(ct_gen_conn).

-compile(export_all).

-export([start/4, stop/1]).
-export([call/2, do_within_time/2]).

-ifdef(debug).
-define(dbg,true).
-else.
-define(dbg,false).
-endif.

-record(gen_opts,{callback,
		  name,
		  address,
		  init_data,
		  conn_pid,
		  cb_state,
		  ct_util_server}).

%%%-----------------------------------------------------------------
%%% @spec start(Name,Address,InitData,CallbackMod) -> 
%%%                                     {ok,Handle} | {error,Reason}
%%%      Name = term()
%%%      CallbackMod = atom()
%%%      InitData = term()
%%%      Address = term()
%%%
%%% @doc Open a connection and start the generic connection owner process.
%%%
%%% <p>The <code>CallbackMod</code> is a specific callback module for
%%% each type of connection (e.g. telnet, ftp,...). It must export the
%%% function <code>init/3</code> which takes the arguments
%%% <code>Name</code>, <code>Addresse</code>) and
%%% <code>InitData</code> and returna
%%% <code>{ok,ConnectionPid,State}</code> or
%%% <code>{error,Reason}</code>.</p>
start(Name,Address,InitData,CallbackMod) ->
    case ct_util:does_connection_exist(Name,Address,CallbackMod) of
	{ok,Pid} ->
	    log("ct_gen_conn:start","Using existing connection!\n",[]),
	    {ok,Pid};
	false ->
	    Self = self(),
	    Pid = spawn(fun() -> 
				init_gen(Self, #gen_opts{callback=CallbackMod,
							 name=Name,
							 address=Address,
							 init_data=InitData})
			end),
	    MRef = erlang:monitor(process,Pid),
	    receive 
		{connected,Pid} -> 
		    erlang:demonitor(MRef, [flush]),
		    ct_util:register_connection(Name,Address,CallbackMod,Pid),
		    {ok,Pid};
		{Error,Pid} ->
		    receive {'DOWN',MRef,process,_,_} -> ok end,
		    Error;
		{'DOWN',MRef,process,_,Reason} ->
		    log("ct_gen_conn:start",
			"Connection process died: ~p\n",
			[Reason]),
		    {error,{connection_process_died,Reason}}
	    end
    end.


%%%-----------------------------------------------------------------
%%% @spec stop(Handle) -> ok
%%%      Handle = handle()
%%%
%%% @doc Close the telnet connection and stop the process managing it.
stop(Pid) ->
    call(Pid,stop).

%%%-----------------------------------------------------------------
%%% @spec log(Heading,Format,Args) -> ok
%%%
%%% @doc Log activities on the current connection (tool-internal use only).
%%% @see ct_logs:log/3 
log(Heading,Format,Args) ->
    log(log,[Heading,Format,Args]).

%%%-----------------------------------------------------------------
%%% @spec start_log(Heading) -> ok
%%%
%%% @doc Log activities on the current connection (tool-internal use only).
%%% @see ct_logs:start_log/1 
start_log(Heading) ->
    log(start_log,[Heading]).

%%%-----------------------------------------------------------------
%%% @spec cont_log(Format,Args) -> ok
%%%
%%% @doc Log activities on the current connection (tool-internal use only).
%%% @see ct_logs:cont_log/2 
cont_log(Format,Args) ->
    log(cont_log,[Format,Args]).

%%%-----------------------------------------------------------------
%%% @spec end_log() -> ok
%%%
%%% @doc Log activities on the current connection (tool-internal use only).
%%% @see ct_logs:end_log/0 
end_log() ->
    log(end_log,[]).

%%%-----------------------------------------------------------------
%%% @spec do_within_time(Fun,Timeout) -> FunResult | {error,Reason}
%%%      Fun = function()
%%%      Timeout = integer()
%%%
%%% @doc Execute a function within a limited time (tool-internal use only).
%%%
%%% <p>Execute the given <code>Fun</code>, but interrupt if it takes
%%% more than <code>Timeout</code> milliseconds.</p>
%%%
%%% <p>The execution is also interrupted if the connection is
%%% closed.</p>
do_within_time(Fun,Timeout) ->
    Self = self(),
    Silent = get(silent),
    TmpPid = spawn_link(fun() -> put(silent,Silent),
				 R = Fun(),
				 Self ! {self(),R} 
			end),
    ConnPid = get(conn_pid),
    receive 
	{TmpPid,Result} ->
	    Result;
	{'EXIT',ConnPid,_Reason}=M ->
	    unlink(TmpPid),
	    exit(TmpPid,kill),
	    self() ! M,
	    {error,connection_closed}
    after 
	Timeout ->
	    exit(TmpPid,kill),
	    receive
		{TmpPid,Result} ->
		    %% TmpPid just managed to send the result at the same time
		    %% as the timeout expired.
		    receive {'EXIT',TmpPid,_reason} -> ok end,
		    Result;
		{'EXIT',TmpPid,killed} ->
		    %% TmpPid did not send the result before the timeout expired.
		    {error,timeout}
	    end
    end.

%%%=================================================================
%%% Internal functions
call(Pid,Msg) ->
    MRef = erlang:monitor(process,Pid),
    Ref = make_ref(),
    Pid ! {Msg,{self(),Ref}},
    receive
	{Ref, Result} -> 
	    erlang:demonitor(MRef, [flush]),
	    case Result of
		{retry,_Data} ->
		    call(Pid,Result);
		Other ->
		    Other
	    end;
	{'DOWN',MRef,process,_,Reason}  -> 
	    {error,{process_down,Pid,Reason}}
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result}.

init_gen(Parent,Opts) ->
    process_flag(trap_exit,true),
    CtUtilServer = whereis(ct_util_server),
    link(CtUtilServer),
    put(silent,false),
    case catch (Opts#gen_opts.callback):init(Opts#gen_opts.name,
					     Opts#gen_opts.address,
					     Opts#gen_opts.init_data) of
	{ok,ConnPid,State} when is_pid(ConnPid) ->
	    link(ConnPid),
	    put(conn_pid,ConnPid),
	    Parent ! {connected,self()},
	    loop(Opts#gen_opts{conn_pid=ConnPid,
			       cb_state=State,
			       ct_util_server=CtUtilServer});
	{error,Reason} ->
	    Parent ! {{error,Reason},self()}
    end.

loop(Opts) ->
    receive
	{'EXIT',Pid,Reason} when Pid==Opts#gen_opts.conn_pid ->
	    log("Connection down!\nOpening new!","Reason: ~p\nAddress: ~p\n",
		[Reason,Opts#gen_opts.address]),
	    case reconnect(Opts) of
		{ok, NewPid, NewState} ->
		    link(NewPid),
		    put(conn_pid,NewPid),
		    loop(Opts#gen_opts{conn_pid=NewPid,cb_state=NewState});
		Error ->
		    ct_util:unregister_connection(self()),
		    log("Reconnect failed. Giving up!","Reason: ~p\n",[Error])
	    end;
	{'EXIT',Pid,Reason} ->
	    case Opts#gen_opts.ct_util_server of
		Pid ->
		    exit(Reason);
		_ ->
		    loop(Opts)
	    end;
	{stop, From} ->
	    ct_util:unregister_connection(self()),
	    (Opts#gen_opts.callback):terminate(Opts#gen_opts.conn_pid,
					       Opts#gen_opts.cb_state),
	    return(From,ok),
	    ok;
	{{retry,{Error,_Name,CPid,_Msg}}, From} when CPid == Opts#gen_opts.conn_pid ->
	    %% only retry if failure is because of a reconnection
	    Return = case Error of
			 {error,_} -> Error;
			 Reason -> {error,Reason}
		     end,
	    return(From, Return),
	    loop(Opts);
	{{retry,{_Error,_Name,_CPid,Msg}}, From} ->
	    log("Rerunning command","Connection reestablished. Rerunning command...",[]),
	    {Return,NewState} = 
		(Opts#gen_opts.callback):handle_msg(Msg,Opts#gen_opts.cb_state),
	    return(From, Return),
	    loop(Opts#gen_opts{cb_state=NewState});	    
	{Msg,From={Pid,_Ref}} when is_pid(Pid) ->
	    {Return,NewState} = 
		(Opts#gen_opts.callback):handle_msg(Msg,Opts#gen_opts.cb_state),
	    return(From, Return),
	    loop(Opts#gen_opts{cb_state=NewState})
    end.

nozero({ok,S}) when is_list(S) ->
    {ok,[C || C <- S,
	      C=/=0,
	      C=/=13]};
nozero(M) ->
    M.

reconnect(Opts) ->
    (Opts#gen_opts.callback):reconnect(Opts#gen_opts.address,
				       Opts#gen_opts.cb_state).


log(Func,Args) ->
    case get(silent) of
	true when not ?dbg-> 
	    ok;
	_ ->
	    apply(ct_logs,Func,Args)
    end.


