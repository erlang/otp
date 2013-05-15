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
	 buffer=[]}).

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
    {ok, LSock} = gen_tcp:listen(Port, [list, {packet, 0}, 
                                        {active, true}]),
    State = #state{listen=LSock,users=Users},
    accept(State),
    ok = gen_tcp:close(LSock).

accept(#state{listen=LSock}=State) ->
    Server = self(),
    Acceptor = spawn_link(fun() -> do_accept(LSock,Server) end),
    receive 
	{Acceptor,Sock} when is_port(Sock) ->
	    case init_client(State#state{client=Sock}) of
		stopped ->
		    io:format("telnet_server stopped\n"),
		    ok;
		R ->
		    io:format("connection to client closed with reason ~p~n",[R]),
		    accept(State)
	    end;
	{Acceptor,closed} ->
	    io:format("listen socket closed unexpectedly, "
		      "terminating telnet_server\n"),
	    ok;
	stop  ->
	    io:format("telnet_server stopped\n"),
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

loop(State) ->
    receive
	{tcp,_,Data} ->
	    try handle_data(Data,State) of
		{ok,State1} ->
		    loop(State1)
	    catch 
		throw:Error ->
		    Error
	    end;
        {tcp_closed, _} ->
            closed;
	{tcp_error,_,Error} ->
	    {error,tcp,Error};
	stop ->
	    stopped
    end.

handle_data([?IAC|Cmd],State) ->
    dbg("Server got cmd: ~p~n",[Cmd]),
    handle_cmd(Cmd,State);
handle_data(Data,State) ->
    dbg("Server got data: ~p~n",[Data]),
    case get_line(Data,[]) of
	{Line,Rest} ->
	    WholeLine = lists:flatten(lists:reverse(State#state.buffer,Line)),
	    {ok,State1} = do_handle_data(WholeLine,State),
	    case Rest of
		[] -> {ok,State1};
		_ -> handle_data(Rest,State1)
	    end;
	false ->
	    {ok,State#state{buffer=[Data|State#state.buffer]}}
    end.

%% Add function clause below to handle new telnet commands (sent with
%% ?IAC from client - this is not possible to do from ct_telnet API,
%% but ct_telnet sends DONT SUPPRESS_GO_AHEAD)
handle_cmd([?DO,?SUPPRESS_GO_AHEAD|T],State) ->
    send([?IAC,?WILL,?SUPPRESS_GO_AHEAD],State),
    handle_cmd(T,State#state{suppress_go_ahead=true});
handle_cmd([?DONT,?SUPPRESS_GO_AHEAD|T],State) ->
    send([?IAC,?WONT,?SUPPRESS_GO_AHEAD],State),
    handle_cmd(T,State#state{suppress_go_ahead=false});
handle_cmd([?IAC|T],State) ->
    %% Multiple commands in one packet
    handle_cmd(T,State);
handle_cmd([_H|T],State) ->
    %% Not responding to this command
    handle_cmd(T,State);
handle_cmd([],State) ->
    {ok,State}.

%% Add function clause below to handle new text command (text entered
%% from the telnet prompt)
do_handle_data(Data,#state{authorized=false}=State) ->
    check_user(Data,State);
do_handle_data(Data,#state{authorized={user,_}}=State) ->
    check_pwd(Data,State);
do_handle_data("echo "++ Data,State) ->
    send(Data++"\r\n> ",State),
    {ok,State};
do_handle_data("echo_no_prompt "++ Data,State) ->
    send(Data,State),
    {ok,State};
do_handle_data("echo_ml "++ Data,State) ->
    Lines = string:tokens(Data," "),
    ReturnData = string:join(Lines,"\n"),
    send(ReturnData++"\r\n> ",State),
    {ok,State};
do_handle_data("echo_ml_no_prompt "++ Data,State) ->
    Lines = string:tokens(Data," "),
    ReturnData = string:join(Lines,"\n"),
    send(ReturnData,State),
    {ok,State};
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
    io:format(_F).
dbg(_F,_A) ->
    io:format(_F,_A).
