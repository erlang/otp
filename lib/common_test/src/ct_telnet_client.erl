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

-export([open/1, open/2, open/3, open/4, close/1]).
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

-record(state,{get_data, keep_alive=true}).

open(Server) ->
    open(Server, ?TELNET_PORT, ?OPEN_TIMEOUT, true).

open(Server, Port) ->
    open(Server, Port, ?OPEN_TIMEOUT, true).

open(Server, Port, Timeout) ->
    open(Server, Port, Timeout, true).

open(Server, Port, Timeout, KeepAlive) ->
    Self = self(),
    Pid = spawn(fun() -> init(Self, Server, Port, Timeout, KeepAlive) end),
    receive 
	{open,Pid} ->
	    {ok,Pid};
	{Error,Pid} ->
	    Error
    end.

close(Pid) ->
    Pid ! close.

send_data(Pid, Data) ->
    Pid ! {send_data, Data++"\n"},
    ok.

get_data(Pid) ->
    Pid ! {get_data, self()},
    receive 
	{data,Data} ->
	    {ok, Data}
    end.


%%%-----------------------------------------------------------------
%%% Internal functions
init(Parent, Server, Port, Timeout, KeepAlive) ->
    case gen_tcp:connect(Server, Port, [list,{packet,0}], Timeout) of
	{ok,Sock} ->
	    dbg("Connected to: ~p (port: ~w, keep_alive: ~w)\n", [Server,Port,KeepAlive]),
	    send([?IAC,?DO,?SUPPRESS_GO_AHEAD], Sock),	      
	    Parent ! {open,self()},
	    loop(#state{get_data=10, keep_alive=KeepAlive}, Sock, []),
	    gen_tcp:close(Sock);
        Error ->
	    Parent ! {Error,self()}
    end.

loop(State, Sock, Acc) ->
    receive
	{tcp_closed,_} ->
	    dbg("Connection closed\n", []),
	    receive
		{get_data,Pid} ->
		    Pid ! closed
	    after 100 ->
		    ok
	    end;
	{tcp,_,Msg0} ->
	    dbg("tcp msg: ~p~n",[Msg0]),
	    Msg = check_msg(Sock,Msg0,[]),
	    loop(State, Sock, [Msg | Acc]);
	{send_data,Data} ->
	    send(Data, Sock),
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
			Pid ! {data,lists:reverse(lists:append(Acc))},
			State
		end,
	    loop(NewState, Sock, []);
	{get_data_delayed,Pid} ->
	    NewState =
		case State of
		    #state{keep_alive = true, get_data = 0} ->
			if Acc == [] -> send([?IAC,?NOP], Sock);
			   true -> ok
			end,
			State#state{get_data=10};
		    _ ->
			State
		end,
	    NewAcc = 
		case erlang:is_process_alive(Pid) of
		    true ->
			Pid ! {data,lists:reverse(lists:append(Acc))},
			[];
		    false ->
			Acc
		end,
	    loop(NewState, Sock, NewAcc);			       
	close ->
	    dbg("Closing connection\n", []),
	    gen_tcp:close(Sock),
	    ok
    after wait(State#state.keep_alive,?IDLE_TIMEOUT) ->
	    if 
		Acc == [] -> send([?IAC,?NOP], Sock);
		true -> ok
	    end,
	    loop(State, Sock, Acc)
    end.

wait(true, Time) -> Time;
wait(false, _) -> infinity.   

send(Data, Sock) ->
    case Data of
	[?IAC|_] = Cmd ->
	    cmd_dbg(Cmd);
	_ ->
	    dbg("Sending: ~p\n", [Data])
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
	    io:format("~s(~w): ~w\n", [CtrlStr,Ctrl,Opts1]);
	Any  ->
	    io:format("Unexpected in cmd_dbg:~n~w~n",[Any])
    end.

-else.
dbg(_Str,_Args) ->
    ok.

cmd_dbg(_Cmd) ->
    ok.
-endif.