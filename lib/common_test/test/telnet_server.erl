-module(telnet_server).
-compile(export_all).

%% telnet control characters
-define(SE,	240).
-define(NOP,	241).
-define(DM,	242).
-define(BRK,	243).
-define(IP,	244).
-define(AO,	245).
-define(AYT,	246).
-define(EC,	247).
-define(EL,	248).
-define(GA,	249).
-define(SB,	250).
-define(WILL,	251).
-define(WONT,	252).
-define(DO,	253).
-define(DONT,	254).
-define(IAC,	255).

%% telnet options
-define(BINARY,            0).
-define(ECHO,	           1).
-define(SUPPRESS_GO_AHEAD, 3).
-define(TERMINAL_TYPE,     24).  

-record(state,
	{listen,
	 client,
	 users,
	 authorized=false,
	 suppress_go_ahead=false,
	 buffer=[],
	 break=false}).

-type options() :: [{port,pos_integer()} | {users,users()}].
-type users() :: [{user(),password()}].
-type user() :: string().
-type password() :: string(). 

-spec start(Opts::options()) -> Pid::pid().
start(Opts) when is_list(Opts) ->
    spawn_link(fun() -> init(Opts) end).

-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
    Ref = erlang:monitor(process,Pid),
    Pid ! stop,
    receive {'DOWN',Ref,_,_,_} -> ok end.

init(Opts) ->
    Port = proplists:get_value(port,Opts),
    Users = proplists:get_value(users,Opts,[]),
    {ok, LSock} = listen(5, Port, [list, {packet, 0}, 
				   {active, true},
				   {reuseaddr,true}]),
    State = #state{listen=LSock,users=Users},
    accept(State),
    ok = gen_tcp:close(LSock),
    dbg("telnet_server closed the listen socket ~p\n", [LSock]),
    timer:sleep(1000),
    ok.

listen(0, _Port, _Opts) ->
    {error,eaddrinuse};
listen(Retries, Port, Opts) ->
    case gen_tcp:listen(Port, Opts) of
	{error,eaddrinuse} ->
	    dbg("Listen port not released, trying again..."),
	    timer:sleep(5000),
	    listen(Retries-1, Port, Opts);
	Ok = {ok,_LSock} ->
	    Ok;
	Error ->
	    exit(Error)
    end.

accept(#state{listen=LSock}=State) ->
    Server = self(),
    Acceptor = spawn_link(fun() -> do_accept(LSock,Server) end),
    receive 
	{Acceptor,Sock} when is_port(Sock) ->
	    dbg("Connected to client on socket ~p\n", [Sock]),
	    case init_client(State#state{client=Sock}) of
		stopped ->
		    dbg("telnet_server stopped\n"),
		    ok;
		R ->
		    dbg("Connection to client " 
			"closed with reason ~p~n",[R]),
		    accept(State)
	    end;
	{Acceptor,closed} ->
	    dbg("Listen socket closed unexpectedly, "
		"terminating telnet_server\n"),
	    ok;
	stop  ->
	    dbg("telnet_server stopped\n"),
	    ok
    end.

do_accept(LSock,Server) ->
    case gen_tcp:accept(LSock) of
	{ok, Sock} ->
	    ok = gen_tcp:controlling_process(Sock,Server),
	    Server ! {self(),Sock};
	{error,closed} ->
	    %% This will happen when stop/1 is called, since listen
	    %% socket is closed. Then the server is probably already
	    %% dead by now, but to be 100% sure not to hang, we send a
	    %% message to say what happened.
	    Server ! {self(),closed}
    end.

init_client(#state{client=Sock}=State) ->
    dbg("Server sending: ~p~n",["login: "]),
    R = case gen_tcp:send(Sock,"login: ") of
	    ok ->
		loop(State);
	    Error ->
		Error
	end,
    _ = gen_tcp:close(Sock),
    R.

loop(State=#state{client=Sock}) ->
    receive
	{tcp,Sock,Data} ->
	    try handle_data(Data,State) of
		{ok,State1} ->
		    loop(State1);
		closed ->
		    _ = flush(State),
		    closed
	    catch 
		throw:Error ->
		    _ = flush(State),
		    Error
	    end;
        {tcp_closed,Sock} ->
            closed;
	{tcp_error,Sock,Error} ->
	    {error,tcp,Error};
	disconnect ->
	    dbg("Server closing connection on socket ~p~n", [Sock]),
	    timer:sleep(1000),
	    ok = gen_tcp:close(Sock),
	    _ = flush(State);
	stop ->
	    _ = flush(State),
	    stopped
    end.

flush(State=#state{client=Sock}) ->
    receive
	{tcp,Sock,Data} = M->
	    dbg("Message flushed after close or error: ~p~n", [M]),
	    try handle_data(Data,State) of
		{ok,State1} ->
		    flush(State1);
		closed ->
		    flush(State)
	    catch
		throw:Error ->
		    Error
	    end;
	{tcp_closed,Sock} = M ->
	    dbg("Message flushed after close or error: ~p~n", [M]),
	    ok;
	{tcp_error,Sock,Error} = M ->
	    dbg("Message flushed after close or error: ~p~n", [M]),
	    {error,tcp,Error}
    after 100 ->
	    ok
    end.

handle_data(Cmd,#state{break=true}=State) ->
    dbg("Server got data when in break mode: ~p~n",[Cmd]),
    handle_break_cmd(Cmd,State);
handle_data([?IAC|Cmd],State) ->
    dbg("Server got cmd: ~p~n",[Cmd]),
    handle_cmd(Cmd,State);
handle_data(Data,State) ->
    dbg("Server got data: ~p~n",[Data]),
    case get_line(Data,[]) of
	{Line,Rest} ->
	    WholeLine = lists:flatten(lists:reverse(State#state.buffer,Line)),
	    case do_handle_data(WholeLine,State) of
		{ok,State1} ->
		    case Rest of
			[] -> {ok,State1};
			_ -> handle_data(Rest,State1)
		    end;
		{close,State1} ->
		    dbg("Server closing connection~n",[]),
		    gen_tcp:close(State1#state.client),
		    closed
	    end;
	false ->
	    {ok,State#state{buffer=[Data|State#state.buffer]}}
    end.

%% Add function clause below to handle new telnet commands sent with
%% ?IAC from client. This can be done from ct_telnet:send or
%% ct_telnet:cmd if using the option {newline,false}. Also, ct_telnet
%% sends DONT SUPPRESS_GO_AHEAD.
handle_cmd([?DO,?SUPPRESS_GO_AHEAD|T],State) ->
    send([?IAC,?WILL,?SUPPRESS_GO_AHEAD],State),
    handle_data(T,State#state{suppress_go_ahead=true});
handle_cmd([?DONT,?SUPPRESS_GO_AHEAD|T],State) ->
    send([?IAC,?WONT,?SUPPRESS_GO_AHEAD],State),
    handle_data(T,State#state{suppress_go_ahead=false});
handle_cmd([?BRK|T],State) ->
    %% Used when testing 'newline' option in ct_telnet:send and ct_telnet:cmd.
    send("# ",State),
    handle_data(T,State#state{break=true});
handle_cmd([?AYT|T],State) ->
    %% Used when testing 'newline' option in ct_telnet:send and ct_telnet:cmd.
    send("yes\r\n> ",State),
    handle_data(T,State);
handle_cmd([?NOP|T],State) ->
    %% Used for 'keep alive'
    handle_data(T,State);
handle_cmd([_H|T],State) ->
    %% Not responding to this command
    handle_cmd(T,State);
handle_cmd([],State) ->
    {ok,State}.

handle_break_cmd([$q|T],State) ->
    %% Dummy cmd allowed in break mode - quit break mode
    send("\r\n> ",State),
    handle_data(T,State#state{break=false});
handle_break_cmd([_H|T],State) ->
    %% Unknown command i break mode - ignore
    handle_break_cmd(T,State);
handle_break_cmd([],State) ->
    {ok,State}.


%% Add function clause below to handle new text command (text entered
%% from the telnet prompt)
do_handle_data(Data,#state{authorized=false}=State) ->
    check_user(Data,State);
do_handle_data(Data,#state{authorized={user,_}}=State) ->
    check_pwd(Data,State);
do_handle_data("echo " ++ Data,State) ->
    send(Data++"\r\n> ",State),
    {ok,State};
do_handle_data("echo_sep " ++ Data,State) ->
    Msgs = string:tokens(Data," "),
    lists:foreach(fun(Msg) ->
			  send(Msg,State),
			  timer:sleep(10)
		  end, Msgs),
    send("\r\n> ",State),
    {ok,State};
do_handle_data("echo_no_prompt " ++ Data,State) ->
    send(Data,State),
    {ok,State};
do_handle_data("echo_ml " ++ Data,State) ->
    Lines = string:tokens(Data," "),
    ReturnData = string:join(Lines,"\n"),
    send(ReturnData++"\r\n> ",State),
    {ok,State};
do_handle_data("echo_ml_no_prompt " ++ Data,State) ->
    Lines = string:tokens(Data," "),
    ReturnData = string:join(Lines,"\n"),
    send(ReturnData,State),
    {ok,State};
do_handle_data("echo_loop " ++ Data,State) ->
    [TStr|Lines] = string:tokens(Data," "),
    ReturnData = string:join(Lines,"\n"),
    send_loop(list_to_integer(TStr),ReturnData,State),
    {ok,State};
do_handle_data("echo_delayed_prompt "++Data,State) ->
    [MsStr|EchoData] = string:tokens(Data, " "),
    send(string:join(EchoData,"\n"),State),
    timer:sleep(list_to_integer(MsStr)),
    send("\r\n> ",State),
    {ok,State};
do_handle_data("disconnect_after " ++WaitStr,State) ->
    Wait = list_to_integer(string:strip(WaitStr,right,$\n)),
    dbg("Server will close connection in ~w ms...", [Wait]),
    erlang:send_after(Wait,self(),disconnect),
    {ok,State};
do_handle_data("disconnect" ++_,State) ->
    {close,State};
do_handle_data([],State) ->
    send("> ",State),
    {ok,State};
do_handle_data(_Data,State) ->
    send("error: unknown command\r\n> ",State),
    {ok,State}.

check_user(User,State) ->
    case lists:keyfind(User,1,State#state.users) of
	{User,Pwd} ->
	    dbg("user ok\n"),
	    send("Password: ",State),
	    {ok,State#state{authorized={user,Pwd}}};
	false ->
	    throw({error,unknown_user})
    end.

check_pwd(Pwd,#state{authorized={user,Pwd}}=State) ->
    dbg("password ok\n"),
    send("Welcome to the ultimate telnet server!\r\n> ",State),
    {ok,State#state{authorized=true}};
check_pwd(_,_State) ->
    throw({error,authentication}).

send(Data,State) ->
    dbg("Server sending: ~p~n",[Data]),
    case gen_tcp:send(State#state.client,Data) of
	ok ->
	    ok;
	{error,Error} ->
	    throw({error,send,Error})
    end.
    
send_loop(T,Data,State) ->
    dbg("Server sending ~p in loop for ~w ms...~n",[Data,T]),
    send_loop(os:timestamp(),T,Data,State).

send_loop(T0,T,Data,State) ->
    ElapsedMS = trunc(timer:now_diff(os:timestamp(),T0)/1000),
    if ElapsedMS >= T ->
	    ok;
       true ->
	    send(Data,State),
	    timer:sleep(500),
	    send_loop(T0,T,Data,State)
    end.

get_line([$\r,$\n|Rest],Acc) ->
    {lists:reverse(Acc),Rest};
get_line([$\r,0|Rest],Acc) ->
    {lists:reverse(Acc),Rest};
get_line([$\n|Rest],Acc) ->
    {lists:reverse(Acc),Rest};
get_line([H|T],Acc) ->
    get_line(T,[H|Acc]);
get_line([],_) ->
    false.

dbg(_F) ->
    dbg(_F,[]).
dbg(_F,_A) ->
    TS = timestamp(),
    io:format("[telnet_server, ~s]\n" ++ _F,[TS|_A]).

timestamp() ->
    {MS,S,US} = os:timestamp(),
    {{Year,Month,Day}, {Hour,Min,Sec}} =
        calendar:now_to_local_time({MS,S,US}),
    MilliSec = trunc(US/1000),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
                                "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
                                [Year,Month,Day,Hour,Min,Sec,MilliSec])).
