%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
-module(vxworks_client).

-export([open/1, close/1, send_data/2, send_data/3, send_data_wait_for_close/2, reboot/1]).
-export([init/2]).    

-include("ts.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This is a client talking to a test server daemon on a VxWorks card.
%%% 
%%% User interface:
%%%
%%% open/1
%%% Start a client and establish the connection with the test server daemon
%%% 
%%% send_data/2
%%% Send data/command to the test server daemon, don't wait for any return
%%% 
%%% send_data/3
%%% Send data/command to the test server daemon and wait for the given
%%% return value.
%%%
%%% send_data_wait_for_close/2
%%% Send data/command to the test server daemon and wait for the daemon to
%%% close the connection.
%%% 
%%% close/1
%%% Close the client.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% User interface
%%

reboot(Target) ->
    {ok, {_,_,_,_,_,[Addr|_]}} = inet:gethostbyname(Target),
    Fun = fun({ok, Socket}) ->
		  gen_tcp:send(Socket, "q\n"),
		  receive
		      {tcp_closed, Socket} ->
			  gen_tcp:close(Socket),
			  {ok, socket_closed}
		  after 5000 ->
			  exit({timeout, tryagain})
		  end
	  end,
    io:format("Stopping (rebooting) ~p ",[Target]),
    case fun_target(Addr, Fun) of
	{ok, socket_closed} ->
	    ok;
	_Else ->
	    io:format("No contact with ts daemon - exiting ...~n"),
	    exit({stop, no_ts_daemon_contact})
    end.
			    

%% open(Target) -> {ok,Client} | {error, Reason}
open(Target) ->
    {ok, {_,_,_,_,_,[Addr|_]}} = inet:gethostbyname(Target),
    Fun = fun({ok, Socket}) ->
		  P = spawn(?MODULE,init,[Target,Socket]),
		  inet_tcp:controlling_process(Socket,P),
		  {ok,P}
	  end,
    case fun_target(Addr,Fun) of
	{ok, Pid} ->
	    {ok, Pid};
	{error,Reason} ->
	    {error, Reason}
    end.

%% send_data(Client,Data) -> ok
send_data(Pid,Data) ->
    Pid ! {send_data,Data++"\n"},
    ok.

%% send_data(Client,Data,ExpectedReturn) -> {ok,ExpectedReturn} | {error,Reason}
send_data(Pid,Data,Return) ->
    Pid ! {send_data,Data++"\n",Return,self()},
    receive {Pid,Result} -> Result end.

%% send_data_wait_for_close(Client,Data) -> ok | {error,Reason}
send_data_wait_for_close(Pid,Data) ->
    send_data(Pid,Data,tcp_closed).

%% close(Client) -> ok
close(Pid) ->
    Pid ! close,
    ok.


%%
%% Internal
%%

init(Target,Socket) ->
    process_flag(trap_exit,true),
    loop(Target,Socket).

loop(Target,Socket) ->
    receive
	{send_data,Data} ->
	    %% io:format("vx client sending: ~p~n", [Data]),
	    gen_tcp:send(Socket, Data),
	    loop(Socket,Target);
	{send_data,Data,tcp_closed,From} ->
	    %% io:format("vx client sending: ~p~n", [Data]),
	    gen_tcp:send(Socket, Data),
	    receive
		{tcp_closed, Socket} ->
		    From ! {self(),ok}
	    after 5000 ->
		    From ! {self(),{error,timeout}}
	    end,
	    closed(Socket,normal);
	{send_data,Data,Return,From} ->
	    %% io:format("vx client sending: ~p~n", [Data]),
	    gen_tcp:send(Socket, Data),
	    case receive_line(Socket,[],Return,200) of
		{tcp_closed, Socket} ->
		    From ! {self(),{error,{socket_closed,Target}}},
		    closed(Socket,{socket_closed,Target});
		{tcp,Socket,_Rest} ->
		    From ! {self(),{ok,Data}},
		    got_data(Target,Socket,Data);
		error ->
		    From ! {self(),{error,{catatonic,Target}}}
	    end;
	close ->
	    closed(Socket,normal);
	{tcp_closed, Socket} ->
	    closed(Socket,{socket_closed,Target});
	{tcp,Socket,Data} ->
	    got_data(Target,Socket,Data)
    end.
	    


closed(Socket,Reason) ->
    gen_tcp:close(Socket),
    exit(Reason).

got_data(Target,Socket,Data) ->
    if is_atom(Target) ->
	    io:format("~w: ~s",[Target,uncr(Data)]);
       true ->
	     io:format("~s: ~s",[Target,uncr(Data)])
    end,
    loop(Target,Socket).
	
uncr([]) ->
    [];
uncr([$\r | T]) ->
    uncr(T);
uncr([H | T]) ->
    [H | uncr(T)].

strip_line(Line) ->
    RPos = string:rchr(Line, $\n),
    string:substr(Line,RPos+1).

maybe_done_receive(Socket,Ack,Match,C) ->
    case string:str(Ack,Match) of
	0 ->
	    receive_line(Socket,strip_line(Ack),Match,C);
	_ ->
	    {tcp,Socket,strip_line(Ack)}
    end.
    

receive_line(_Socket,_Ack,_Match,0) ->
    error;
receive_line(Socket,Ack,Match,Counter) ->
    receive
	{tcp_closed, Socket} ->
	    {tcp_closed, Socket};
	{tcp,Socket,Data} ->
	    NewAck = Ack ++ Data,
	    case {string:str(NewAck,"\r") > 0,
		  string:str(NewAck,"\n") > 0} of
		{true,_} ->
		    maybe_done_receive(Socket,NewAck,Match,Counter-1);
		{_,true} ->
		    maybe_done_receive(Socket,NewAck,Match,Counter-1);
		_ ->
		    receive_line(Socket,NewAck,Match,Counter)
	    end
    after 20000 ->
	    error
    end.
   

%% Misc functions
fun_target(Addr, Fun) ->
    io:format("["),
    fun_target(Addr, Fun, 60).		%Vx-cards need plenty of time.

fun_target(_Addr, _Fun, 0) ->
    io:format(" no contact with ts daemon]~n"),
    {error,failed_to_connect};
fun_target(Addr, Fun, Tries_left) ->
    receive after 1 -> ok end,
    case do_connect(Addr, Fun) of
	{ok, Value} ->
	    io:format(" ok]~n"),
	    {ok, Value};
	_Error -> % typical {error, econnrefused}
	    io:format("."),
	    receive after 10000 -> ok end,
	    fun_target(Addr, Fun, Tries_left-1)
    end.
	    
do_connect(Addr, Fun) ->
    case gen_tcp:connect(Addr, ?TS_PORT, [{reuseaddr, true}], 60000) of
	{ok, Socket} ->
	    Fun({ok, Socket});
	Error ->
	    Error
    end.


    
