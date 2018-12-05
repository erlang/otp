%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(socket_test_ttest_tcp_socket).

-export([
	 accept/1, accept/2,
	 active/2,
	 close/1,
	 connect/2,
	 controlling_process/2,
	 listen/0, listen/1,
	 port/1,
	 peername/1,
	 recv/2, recv/3,
	 send/2,
	 shutdown/2,
	 sockname/1
	]).


-define(READER_RECV_TIMEOUT, 1000).


%% ==========================================================================

accept(#{sock := LSock}) ->
    case socket:accept(LSock) of
	{ok, Sock} ->
	    Self = self(),
	    Reader = spawn(fun() -> reader_init(Self, Sock, false) end),
	    {ok, #{sock => Sock, reader => Reader}};
	{error, _} = ERROR ->
	    ERROR
    end.

accept(#{sock := LSock}, Timeout) ->
    case socket:accept(LSock, Timeout) of
	{ok, Sock} ->
	    Self = self(),
	    Reader = spawn(fun() -> reader_init(Self, Sock, false) end),
	    {ok, #{sock => Sock, reader => Reader}};
	{error, _} = ERROR ->
	    ERROR
    end.


active(#{reader := Pid}, NewActive) 
  when (is_boolean(NewActive) orelse (NewActive =:= once)) ->
    Pid ! {?MODULE, active, NewActive},
    ok.


close(#{sock := Sock, reader := Pid}) ->
    Pid ! {?MODULE, stop},
    socket:close(Sock).

%% Create a socket and connect it to a peer
connect(Addr, Port) ->
    try
	begin
	    Sock =
		case socket:open(inet, stream, tcp) of
		    {ok, S} ->
			S;
		    {error, OReason} ->
			throw({error, {open, OReason}})
		end,
	    case socket:bind(Sock, any) of
		{ok, _} ->
		    ok;
		{error, BReason} ->
		    (catch socket:close(Sock)),
		    throw({error, {bind, BReason}})
	    end,
	    SA = #{family => inet,
		   addr   => Addr,
		   port   => Port},
	    case socket:connect(Sock, SA) of
		ok ->
		    ok;
		{error, CReason} ->
		    (catch socket:close(Sock)),
		    throw({error, {connect, CReason}})
	    end,
	    Self = self(),
	    Reader = spawn(fun() -> reader_init(Self, Sock, false) end),
	    {ok, #{sock => Sock, reader => Reader}}
	end
    catch
	throw:ERROR:_ ->
	    ERROR
    end.


controlling_process(#{sock := Sock, reader := Pid}, NewPid) ->
    case socket:setopt(Sock, otp, controlling_process, NewPid) of
	ok ->
	    Pid ! {?MODULE, self(), controlling_process, NewPid},
	    receive
		{?MODULE, Pid, controlling_process} ->
		    ok
	    end;
	{error, _} = ERROR ->
	    ERROR
    end.


%% Create a listen socket
listen() ->
    listen(0).

listen(Port) when is_integer(Port) andalso (Port >= 0) ->
    try
	begin
	    Sock = case socket:open(inet, stream, tcp) of
		       {ok, S} ->
			   S;
		       {error, OReason} ->
			   throw({error, {open, OReason}})
		   end,
	    SA = #{family => inet,
		   port   => Port},
	    case socket:bind(Sock, SA) of
		{ok, _} ->
		    ok;
		{error, BReason} ->
		    (catch socket:close(Sock)),
		    throw({error, {bind, BReason}})
	    end,
	    case socket:listen(Sock) of
		ok ->
                        ok;
                    {error, LReason} ->
		    (catch socket:close(Sock)),
                        throw({error, {listen, LReason}})
                end,
	    {ok, #{sock => Sock}}
	end
    catch
	throw:{error, Reason}:_ ->
	    {error, Reason}
    end.


port(#{sock := Sock}) ->
    case socket:sockname(Sock) of
	{ok, #{port := Port}} ->
	    {ok, Port};
	{error, _} = ERROR ->
	    ERROR
    end.


peername(#{sock := Sock}) ->
    case socket:peername(Sock) of
	{ok, #{addr := Addr, port := Port}} ->
	    {ok, {Addr, Port}};
	{error, _} = ERROR ->
	    ERROR
    end.


recv(#{sock := Sock}, Length) ->
    socket:recv(Sock, Length).
recv(#{sock := Sock}, Length, Timeout) ->
    socket:recv(Sock, Length, Timeout).


send(#{sock := Sock}, Length) ->
    socket:send(Sock, Length).


shutdown(#{sock := Sock}, How) ->
    socket:shutdown(Sock, How).


sockname(#{sock := Sock}) ->
    case socket:sockname(Sock) of
	{ok, #{addr := Addr, port := Port}} ->
	    {ok, {Addr, Port}};
	{error, _} = ERROR ->
	    ERROR
    end.


%% ==========================================================================

reader_init(ControllingProcess, Sock, Active) 
  when is_pid(ControllingProcess) andalso
       (is_boolean(Active) orelse (Active =:= once)) ->
    MRef = erlang:monitor(process, ControllingProcess),
    reader_loop(#{ctrl_proc      => ControllingProcess,
		  ctrl_proc_mref => MRef,
		  active         => Active,
		  sock           => Sock}).


%% Never read
reader_loop(#{active    := false,
	      ctrl_proc := Pid} = State) ->
    receive
	{?MODULE, stop} ->
	    exit(normal);

	{?MODULE, Pid, controlling_process, NewPid} ->
	    MRef = maps:get(ctrl_proc_mref, State),
	    erlang:demonitor(MRef, [flush]),
	    NewMRef = erlang:monitor(process, NewPid),
	    Pid ! {?MODULE, self(), controlling_process},
	    reader_loop(State#{ctrl_proc      => NewPid,
			       ctrl_proc_mref => NewMRef});

	{?MODULE, active, NewActive} ->
	    reader_loop(State#{active => NewActive});

	{'DOWN', MRef, process, Pid, Reason} ->
	    case maps:get(ctrl_proc_mref, State) of
		MRef when (Reason =:= normal) ->
		    exit(normal);
		MRef ->
		    exit({controlling_process, Reason});
		_ ->
		    reader_loop(State)
	    end
    end;

%% Read *once* and then change to false
reader_loop(#{active    := once,
	      sock      := Sock,
	      ctrl_proc := Pid} = State) ->
    case socket:recv(Sock, 0, ?READER_RECV_TIMEOUT) of
	{ok, Data} ->
	    Pid ! {socket, #{sock => Sock, reader => self()}, Data},
	    reader_loop(State#{active => false});
	{error, timeout} ->
	    receive
		{?MODULE, stop} ->
		    exit(normal);

		{?MODULE, Pid, controlling_process, NewPid} ->
		    MRef = maps:get(ctrl_proc_mref, State),
		    erlang:demonitor(MRef, [flush]),
		    MRef = erlang:monitor(process, NewPid),
		    Pid ! {?MODULE, self(), controlling_process},
		    reader_loop(State#{ctrl_proc      => NewPid,
				       ctrl_proc_mref => MRef});

		{?MODULE, active, NewActive} ->
		    reader_loop(State#{active => NewActive});

		{'DOWN', MRef, process, Pid, Reason} ->
		    case maps:get(ctrl_proc_mref, State) of
			MRef when (Reason =:= normal) ->
			    exit(normal);
			MRef ->
			    exit({controlling_process, Reason});
			_ ->
			    reader_loop(State)
		    end
	    after 0 ->
		    reader_loop(State)
	    end;

	{error, closed} ->
	    Pid ! {socket_closed, #{sock => Sock, reader => self()}},
	    exit(normal);

	{error, Reason} ->
	    Pid ! {socket_error, #{sock => Sock, reader => self()}, Reason},
	    exit(Reason)
    end;

%% Read and forward data continuously
reader_loop(#{active    := true,
	      sock      := Sock,
	      ctrl_proc := Pid} = State) ->
    case socket:recv(Sock, 0, ?READER_RECV_TIMEOUT) of
	{ok, Data} ->
	    Pid ! {socket, #{sock => Sock, reader => self()}, Data},
	    reader_loop(State);
	{error, timeout} ->
	    receive
		{?MODULE, stop} ->
		    exit(normal);

		{?MODULE, Pid, controlling_process, NewPid} ->
		    MRef = maps:get(ctrl_proc_mref, State),
		    erlang:demonitor(MRef, [flush]),
		    MRef = erlang:monitor(process, NewPid),
		    Pid ! {?MODULE, self(), controlling_process},
		    reader_loop(State#{ctrl_proc      => NewPid,
				       ctrl_proc_mref => MRef});

		{?MODULE, active, NewActive} ->
		    reader_loop(State#{active => NewActive});

		{'DOWN', MRef, process, Pid, Reason} ->
		    case maps:get(ctrl_proc_mref, State) of
			MRef when (Reason =:= normal) ->
			    exit(normal);
			MRef ->
			    exit({controlling_process, Reason});
			_ ->
			    reader_loop(State)
		    end
	    after 0 ->
		    reader_loop(State)
	    end;

	{error, closed} ->
	    Pid ! {socket_closed, #{sock => Sock, reader => self()}},
	    exit(normal);

	{error, Reason} ->
	    Pid ! {socket_error, #{sock => Sock, reader => self()}, Reason},
	    exit(Reason)
    end.


	    
		    
		  

%% ==========================================================================


			   
