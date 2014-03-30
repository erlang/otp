%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2013. All Rights Reserved.
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

%%---------------------------------------------------------------
%% Basic negotiated options with Telnet (RFC 854)
%%
%% Side A request: I WILL set option Opt.
%% Side B answer:  DO go ahead, or no, DON'T set it.
%% 
%% Side A request: Please DO set this option. 
%% Side B answer:  Ok I WILL, or no, I WON'T set it.
%%
%% "Enable option" requests may be rejected. 
%% "Disable option" requests must not.
%%---------------------------------------------------------------

-module(ct_telnet_client).

%% -define(debug, true).

-export([open/2, open/3, open/4, open/5, close/1]).
-export([send_data/2, get_data/1]).

-define(TELNET_PORT, 23).
-define(OPEN_TIMEOUT,10000).
-define(IDLE_TIMEOUT,10000).

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
-define(WINDOW_SIZE,       31).

-record(state,{conn_name, get_data, keep_alive=true, log_pos=1}).

open(Server, ConnName) ->
    open(Server, ?TELNET_PORT, ?OPEN_TIMEOUT, true, ConnName).

open(Server, Port, ConnName) ->
    open(Server, Port, ?OPEN_TIMEOUT, true, ConnName).

open(Server, Port, Timeout, ConnName) ->
    open(Server, Port, Timeout, true, ConnName).

open(Server, Port, Timeout, KeepAlive, ConnName) ->
    Self = self(),
    Pid = spawn(fun() ->
			init(Self, Server, Port, Timeout,
			     KeepAlive, ConnName)
		end),
    receive 
	{open,Pid} ->
	    {ok,Pid};
	{Error,Pid} ->
	    Error
    end.

close(Pid) ->
    Pid ! {close,self()},
    receive closed -> ok 
    after 5000 -> ok
    end.	    

send_data(Pid, Data) ->
    Pid ! {send_data, Data++"\n"},
    ok.

get_data(Pid) ->
    Pid ! {get_data,self()},
    receive 
	{data,Data} ->
	    {ok,Data}
    end.


%%%-----------------------------------------------------------------
%%% Internal functions
init(Parent, Server, Port, Timeout, KeepAlive, ConnName) ->
    case gen_tcp:connect(Server, Port, [list,{packet,0}], Timeout) of
	{ok,Sock} ->
	    dbg("~p connected to: ~p (port: ~w, keep_alive: ~w)\n",
		[ConnName,Server,Port,KeepAlive]),
	    send([?IAC,?DO,?SUPPRESS_GO_AHEAD], Sock, ConnName),	      
	    Parent ! {open,self()},
	    loop(#state{conn_name=ConnName, get_data=10, keep_alive=KeepAlive},
		 Sock, []),
	    gen_tcp:close(Sock);
        Error ->
	    Parent ! {Error,self()}
    end.

loop(State, Sock, Acc) ->
    receive
	{tcp_closed,_} ->
	    dbg("Connection closed\n", []),
	    Data = lists:reverse(lists:append(Acc)),
	    dbg("Printing queued messages: ~tp",[Data]),
	    ct_telnet:log(State#state.conn_name,
			  general_io, "~ts",
			  [lists:sublist(Data,
					 State#state.log_pos,
					 length(Data))]),
	    receive
		{get_data,Pid} ->
		    Pid ! closed
	    after 100 ->
		    ok
	    end;
	{tcp,_,Msg0} ->
	    dbg("tcp msg: ~tp~n",[Msg0]),
	    Msg = check_msg(Sock,Msg0,[]),
	    loop(State, Sock, [Msg | Acc]);
	{send_data,Data} ->
	    send(Data, Sock, State#state.conn_name),
	    loop(State, Sock, Acc);
	{get_data,Pid} ->
	    NewState = 
		case Acc of
		    [] ->
			dbg("get_data nodata\n",[]),
			erlang:send_after(100,self(),{get_data_delayed,Pid}),
			if State#state.keep_alive == true ->
				State#state{get_data=State#state.get_data - 1};
			   State#state.keep_alive == false ->
				State
			end;
		    _ ->
			Data = lists:reverse(lists:append(Acc)),
			Len = length(Data),
			dbg("get_data ~tp\n",[Data]),
			ct_telnet:log(State#state.conn_name,
				      general_io, "~ts",
				      [lists:sublist(Data,
						     State#state.log_pos,
						     Len)]),
			Pid ! {data,Data},
			State#state{log_pos = 1}
		end,
	    loop(NewState, Sock, []);
	{get_data_delayed,Pid} ->
	    NewState =
		case State of
		    #state{keep_alive = true, get_data = 0} ->
			if Acc == [] -> send([?IAC,?NOP], Sock, 
					     State#state.conn_name);
			   true -> ok
			end,
			State#state{get_data=10};
		    _ ->
			State
		end,
	    {NewAcc,Pos} = 
		case erlang:is_process_alive(Pid) of
		    true when Acc /= [] ->
			Data = lists:reverse(lists:append(Acc)),
			Len = length(Data),
			dbg("get_data_delayed ~tp\n",[Data]),
			ct_telnet:log(State#state.conn_name,
				      general_io, "~ts",
				      [lists:sublist(Data,
						     State#state.log_pos,
						     Len)]),
			Pid ! {data,Data},
			{[],1};
		    true when Acc == [] ->
			dbg("get_data_delayed nodata\n",[]),
			Pid ! {data,[]},
			{[],1};
		    false ->
			{Acc,NewState#state.log_pos}
		end,
	    loop(NewState#state{log_pos=Pos}, Sock, NewAcc);			       
	{close,Pid} ->
	    dbg("Closing connection\n", []),
	    if Acc == [] ->
		    ok;
	       true ->
		    Data = lists:reverse(lists:append(Acc)),
		    dbg("Printing queued messages: ~tp",[Data]),
		    ct_telnet:log(State#state.conn_name,
				  general_io, "~ts",
				  [lists:sublist(Data,
						 State#state.log_pos,
						 length(Data))])
	    end,
	    gen_tcp:close(Sock),
	    Pid ! closed
    after wait(State#state.keep_alive,?IDLE_TIMEOUT) ->
	    Data = lists:reverse(lists:append(Acc)),
	    case Data of
		[] ->
		    send([?IAC,?NOP], Sock, State#state.conn_name),
		    loop(State, Sock, Acc);
		_ when State#state.log_pos == length(Data)+1 ->
		    loop(State, Sock, Acc);
		_ ->
		    dbg("Idle timeout, printing ~tp\n",[Data]),
		    Len = length(Data),
		    ct_telnet:log(State#state.conn_name,
				  general_io, "~ts",
				  [lists:sublist(Data,
						 State#state.log_pos,
						 Len)]),
		    loop(State#state{log_pos = Len+1}, Sock, Acc)
	    end
    end.

wait(true, Time) -> Time;
wait(false, _) -> infinity.   

send(Data, Sock, ConnName) ->
    case Data of
	[?IAC|_] = Cmd ->
	    cmd_dbg(Cmd);
	_ ->
	    dbg("Sending: ~tp\n", [Data]),
	    try io_lib:format("[~w] ~ts", [?MODULE,Data]) of
		Str ->
		    ct_telnet:log(ConnName, general_io, Str, [])
	    catch
		_:_ -> ok
	    end
    end,
    gen_tcp:send(Sock, Data),
    ok.

%% [IAC,IAC] = buffer data value 255
check_msg(Sock, [?IAC,?IAC | T], Acc) ->
    check_msg(Sock, T, [?IAC|Acc]);

%% respond to a command
check_msg(Sock, [?IAC | Cs], Acc) ->
    case get_cmd(Cs) of
	{Cmd,Cs1} ->
	    dbg("Got ", []), 
	    cmd_dbg(Cmd),
	    respond_cmd(Cmd, Sock),
	    check_msg(Sock, Cs1, Acc); 
	error ->
	    Acc
    end;

%% buffer a data value
check_msg(Sock, [H|T], Acc) ->
    check_msg(Sock, T, [H|Acc]);

check_msg(_Sock, [], Acc) ->
    Acc.


%% Positive responses (WILL and DO).

respond_cmd([?WILL,?ECHO], Sock) ->
    R = [?IAC,?DO,?ECHO],
    cmd_dbg(R),
    gen_tcp:send(Sock, R);

respond_cmd([?DO,?ECHO], Sock) ->
    R = [?IAC,?WILL,?ECHO],
    cmd_dbg(R),
    gen_tcp:send(Sock, R);

%% Answers from server

respond_cmd([?WILL,?SUPPRESS_GO_AHEAD], _Sock) ->
    dbg("Server will suppress-go-ahead\n", []);

respond_cmd([?WONT,?SUPPRESS_GO_AHEAD], _Sock) ->
    dbg("Warning! Server won't suppress-go-ahead\n", []);

respond_cmd([?DONT | _Opt], _Sock) ->		% server ack?
    ok;						
respond_cmd([?WONT | _Opt], _Sock) ->		% server ack?
    ok;						

%% Negative responses (WON'T and DON'T). These are default!

respond_cmd([?WILL,Opt], Sock) ->
    R = [?IAC,?DONT,Opt],
    cmd_dbg(R),
    gen_tcp:send(Sock, R);

respond_cmd([?DO | Opt], Sock) ->
    R = [?IAC,?WONT | Opt],
    cmd_dbg(R),
    gen_tcp:send(Sock, R);

%% Commands without options (which we ignore)

respond_cmd(?NOP, _Sock) ->
    ok;

%% Unexpected messages.

respond_cmd([Cmd | Opt], _Sock) when Cmd >= 240, Cmd =< 255 ->
    dbg("Received cmd: ~w. Ignored!\n", [[Cmd | Opt]]);

respond_cmd([Cmd | Opt], _Sock)  ->
    dbg("WARNING: Received unknown cmd: ~w. Ignored!\n", [[Cmd | Opt]]).


get_cmd([Cmd | Rest]) when Cmd == ?SB ->
    get_subcmd(Rest, []);

get_cmd([Cmd | Rest]) when Cmd >= 240, Cmd =< 249 ->
    {?NOP, Rest};

get_cmd([Cmd,Opt | Rest]) when Cmd >= 251, Cmd =< 254 ->
    {[Cmd,Opt], Rest};

get_cmd(_Other) ->
    error.

get_subcmd([?SE | Rest], Acc) ->
    {[?SE | lists:reverse(Acc)], Rest};

get_subcmd([Opt | Rest], Acc) ->
    get_subcmd(Rest, [Opt | Acc]).

-ifdef(debug).
dbg(_Str,_Args) ->
    io:format(_Str,_Args).

cmd_dbg(_Cmd) ->
    case _Cmd of
	[?IAC|Cmd1] ->
	    cmd_dbg(Cmd1);
	[Ctrl|Opts] ->
	    CtrlStr =
		case Ctrl of
		    ?DO ->   "DO";
		    ?DONT -> "DONT";
		    ?WILL -> "WILL";
		    ?WONT -> "WONT";
		    ?NOP ->  "NOP";
		    _ ->     "CMD"
		end,
	    Opts1 =
		case Opts of
		    [Opt] -> Opt;
		    _ -> Opts
		end,
	    io:format("~ts(~w): ~w\n", [CtrlStr,Ctrl,Opts1]);
	Any  ->
	    io:format("Unexpected in cmd_dbg:~n~w~n",[Any])
    end.

-else.
dbg(_Str,_Args) ->
    ok.

cmd_dbg(_Cmd) ->
    ok.
-endif.
