%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%%% @doc Generic connection owner process.
%%%
%%% @type handle() = pid(). A handle for using a connection implemented
%%% with ct_gen_conn.erl.

-module(ct_gen_conn).

-export([start/4, stop/1, get_conn_pid/1, check_opts/1]).
-export([call/2, call/3, return/2, do_within_time/2]).
-export([log/3, start_log/1, cont_log/2, cont_log_no_timestamp/2, end_log/0]).

%%----------------------------------------------------------------------
%% Exported types
%%----------------------------------------------------------------------
-export_type([server_id/0,
	      target_name/0,
	      key_or_name/0]).

-ifdef(debug).
-define(dbg,true).
-else.
-define(dbg,false).
-endif.

-record(gen_opts,{callback,
		  name,
		  address,
		  init_data,
		  reconnect    = true,
		  forward      = false,
		  use_existing = true,
		  old          = false,
		  conn_pid,
		  cb_state,
		  ct_util_server}).

%%------------------------------------------------------------------
%% Type declarations
%%------------------------------------------------------------------
-type server_id() :: atom().
%% A `ServerId' which exists in a configuration file.
-type target_name() :: atom().
%% A name which is associated to a `server_id()' via a
%% `require' statement or a call to {@link ct:require/2} in the
%% test suite.
-type key_or_name() :: server_id() | target_name().


%%%-----------------------------------------------------------------
%%% @spec start(Address,InitData,CallbackMod,Opts) ->
%%%                                     {ok,Handle} | {error,Reason}
%%%      Name = term()
%%%      CallbackMod = atom()
%%%      InitData = term()
%%%      Address = term()
%%%      Opts = [Opt]
%%%      Opt = {name,Name} | {use_existing_connection,boolean()} |
%%%            {reconnect,boolean()} | {forward_messages,boolean()}
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
%%%
%%% If no name is given, the <code>Name</code> argument in init/3 will
%%% have the value <code>undefined</code>.
%%%
%%% The callback modules must also export
%%% ```
%%% handle_msg(Msg,From,State) -> {reply,Reply,State} |
%%%                               {noreply,State} |
%%%                               {stop,Reply,State}
%%% terminate(ConnectionPid,State) -> term()
%%% close(Handle) -> term()
%%% '''
%%%
%%% The <code>close/1</code> callback function is actually a callback
%%% for ct_util, for closing registered connections when
%%% ct_util_server is terminated. <code>Handle</code> is the Pid of
%%% the ct_gen_conn process.
%%%
%%% If option <code>reconnect</code> is <code>true</code>, then the
%%% callback must also export
%%% ```
%%% reconnect(Address,State) -> {ok,ConnectionPid,State}
%%% '''
%%%
%%% If option <code>forward_messages</code> is <ocde>true</code>, then
%%% the callback must also export
%%% ```
%%% handle_msg(Msg,State) -> {noreply,State} | {stop,State}
%%% '''
%%%
%%% An old interface still exists. This is used by ct_telnet, ct_ftp
%%% and ct_ssh. The start function then has an explicit
%%% <code>Name</code> argument, and no <code>Opts</code> argument. The
%%% callback must export:
%%%
%%% ```
%%% init(Name,Address,InitData) -> {ok,ConnectionPid,State}
%%% handle_msg(Msg,State) -> {Reply,State}
%%% reconnect(Address,State) -> {ok,ConnectionPid,State}
%%% terminate(ConnectionPid,State) -> term()
%%% close(Handle) -> term()
%%% '''
%%%
start(Address,InitData,CallbackMod,Opts) when is_list(Opts) ->
    do_start(Address,InitData,CallbackMod,Opts);
start(Name,Address,InitData,CallbackMod) ->
    do_start(Address,InitData,CallbackMod,[{name,Name},{old,true}]).

%%%-----------------------------------------------------------------
%%% @spec stop(Handle) -> ok
%%%      Handle = handle()
%%%
%%% @doc Close the connection and stop the process managing it.
stop(Handle) ->
    call(Handle,stop,5000).

%%%-----------------------------------------------------------------
%%% @spec get_conn_pid(Handle) -> ok
%%%      Handle = handle()
%%%
%%% @doc Return the connection pid associated with Handle
get_conn_pid(Handle) ->
    call(Handle,get_conn_pid).

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
%%% @spec cont_log_no_timestamp(Format,Args) -> ok
%%%
%%% @doc Log activities on the current connection (tool-internal use only).
%%% @see ct_logs:cont_log/2
cont_log_no_timestamp(Format,Args) ->
    log(cont_log_no_timestamp,[Format,Args]).

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
do_start(Address,InitData,CallbackMod,Opts0) ->
    Opts = check_opts(Opts0,#gen_opts{callback=CallbackMod,
				      address=Address,
				      init_data=InitData}),
    case ct_util:does_connection_exist(Opts#gen_opts.name,
				       Address,CallbackMod) of
	{ok,Pid} when Opts#gen_opts.use_existing ->
	    log("ct_gen_conn:start","Using existing connection!\n",[]),
	    {ok,Pid};
	{ok,Pid} when not Opts#gen_opts.use_existing ->
	    {error,{connection_exists,Pid}};
	false ->
	    do_start(Opts)
    end.

do_start(Opts) ->
    Self = self(),
    Pid = spawn(fun() -> init_gen(Self, Opts) end),
    MRef = erlang:monitor(process,Pid),
    receive
	{connected,Pid} ->
	    erlang:demonitor(MRef, [flush]),
	    ct_util:register_connection(Opts#gen_opts.name,
					Opts#gen_opts.address,
					Opts#gen_opts.callback, Pid),
	    {ok,Pid};
	{Error,Pid} ->
	    receive {'DOWN',MRef,process,_,_} -> ok end,
	    Error;
	{'DOWN',MRef,process,_,Reason} ->
	    log("ct_gen_conn:start",
		"Connection process died: ~p\n",
		[Reason]),
	    {error,{connection_process_died,Reason}}
    end.

check_opts(Opts0) ->
    check_opts(Opts0,#gen_opts{}).

check_opts([{name,Name}|T],Opts) ->
    check_opts(T,Opts#gen_opts{name=Name});
check_opts([{reconnect,Bool}|T],Opts) ->
    check_opts(T,Opts#gen_opts{reconnect=Bool});
check_opts([{forward_messages,Bool}|T],Opts) ->
    check_opts(T,Opts#gen_opts{forward=Bool});
check_opts([{use_existing_connection,Bool}|T],Opts) ->
    check_opts(T,Opts#gen_opts{use_existing=Bool});
check_opts([{old,Bool}|T],Opts) ->
    check_opts(T,Opts#gen_opts{old=Bool});
check_opts([],Opts) ->
    Opts.

call(Pid, Msg) ->
    call(Pid, Msg, infinity).

call(Pid, Msg, Timeout) ->
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
    after Timeout ->
	    erlang:demonitor(MRef, [flush]),
	    log("ct_gen_conn",
		"Connection process ~w not responding. Killing now!",
		[Pid]),
	    exit(Pid, kill),
	    {error,{process_down,Pid,forced_termination}}
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result},
    ok.

init_gen(Parent,Opts) ->
    process_flag(trap_exit,true),
    put(silent,false),
    try (Opts#gen_opts.callback):init(Opts#gen_opts.name,
				      Opts#gen_opts.address,
				      Opts#gen_opts.init_data) of
	{ok,ConnPid,State} when is_pid(ConnPid) ->
	    link(ConnPid),
	    put(conn_pid,ConnPid),
	    CtUtilServer = whereis(ct_util_server),
	    link(CtUtilServer),
	    Parent ! {connected,self()},
	    loop(Opts#gen_opts{conn_pid=ConnPid,
			       cb_state=State,
			       ct_util_server=CtUtilServer});
	{error,Reason} ->
	    Parent ! {{error,Reason},self()}
    catch
	throw:{error,Reason} ->
	    Parent ! {{error,Reason},self()}
    end.

loop(Opts) ->
    receive
	{'EXIT',Pid,Reason} when Pid==Opts#gen_opts.conn_pid ->
	    case Opts#gen_opts.reconnect of
		true ->
		    log("Connection down!\nOpening new!",
			"Reason: ~p\nAddress: ~p\n",
			[Reason,Opts#gen_opts.address]),
		    case reconnect(Opts) of
			{ok, NewPid, NewState} ->
			    link(NewPid),
			    put(conn_pid,NewPid),
			    loop(Opts#gen_opts{conn_pid=NewPid,
					       cb_state=NewState});			
			Error ->
			    ct_util:unregister_connection(self()),
			    log("Reconnect failed. Giving up!",
				"Reason: ~p\n",
				[Error])
		    end;
		false ->
		    ct_util:unregister_connection(self()),
		    log("Connection closed!","Reason: ~p\n",[Reason])
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
	{{retry,{Error,_Name,CPid,_Msg}}, From} when 
	      CPid == Opts#gen_opts.conn_pid ->
	    %% only retry if failure is because of a reconnection
	    Return = case Error of
			 {error,_} -> Error;
			 Reason -> {error,Reason}
		     end,
	    return(From, Return),
	    loop(Opts);
	{{retry,{_Error,_Name,_CPid,Msg}}, From} ->
	    log("Rerunning command","Connection reestablished. "
		"Rerunning command...",[]),
	    {Return,NewState} =
		(Opts#gen_opts.callback):handle_msg(Msg,Opts#gen_opts.cb_state),
	    return(From, Return),
	    loop(Opts#gen_opts{cb_state=NewState});
	{get_conn_pid, From} ->
	    return(From, Opts#gen_opts.conn_pid),
	    loop(Opts);
	{Msg, From={Pid,_Ref}} when is_pid(Pid), Opts#gen_opts.old==true ->
	    {Return,NewState} =
		(Opts#gen_opts.callback):handle_msg(Msg,Opts#gen_opts.cb_state),
	    return(From, Return),
	    loop(Opts#gen_opts{cb_state=NewState});
	{Msg,From={Pid,_Ref}} when is_pid(Pid) ->
	    case (Opts#gen_opts.callback):handle_msg(Msg,From,
						     Opts#gen_opts.cb_state) of
		{reply,Reply,NewState} ->
		    return(From,Reply),
		    loop(Opts#gen_opts{cb_state=NewState});
		{noreply,NewState} ->
		    loop(Opts#gen_opts{cb_state=NewState});
		{stop,Reply,NewState} ->
		    ct_util:unregister_connection(self()),
		    (Opts#gen_opts.callback):terminate(Opts#gen_opts.conn_pid,
						       NewState),
		    return(From,Reply)
	    end;
	Msg when Opts#gen_opts.forward==true ->
	    case (Opts#gen_opts.callback):handle_msg(Msg,
						     Opts#gen_opts.cb_state) of
		{noreply,NewState} ->
		    loop(Opts#gen_opts{cb_state=NewState});
		{stop,NewState} ->
		    ct_util:unregister_connection(self()),
		    (Opts#gen_opts.callback):terminate(Opts#gen_opts.conn_pid,
						       NewState)
	    end
    end.

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
