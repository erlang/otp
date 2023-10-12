%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2021. All Rights Reserved.
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

%% =======================================================================
%% This is just a simple utility that sets op several socket connections
%% and monitors. Data is exchanged.
%% This is done to make it possible to see that the socket sections of
%% observer works.
%% =======================================================================

-module(observer_socket_test).

-export([start/0]).

-define(NUM_ACCEPTORS, 5).

-define(MTAG,    42).
-define(PRINT,   01).
-define(REQUEST, 02).
-define(REPLY,   03).

-define(SOCKET_MONITOR, whereis(socket_monitor)).

-define(MSG_DATA1, <<"This is test data 0123456789 0123456789 0123456789">>).
-define(MSG_DATA2, <<"This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789">>).
-define(MSG_DATA3, <<"This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789">>).
-define(DATA, ?MSG_DATA1).


start() ->
    put(sname, "starter"),
    %% put(debug, true),
    i("try start socket-monitor"),
    SockMon = start_socket_monitor(),
    i("try start server"),
    Domain = inet,
    Type   = stream,
    Proto  = tcp,
    {Server, SockAddr}  = start_server(Domain, Type, Proto),
    i("try start client"),
    Client = start_client(Domain, Type, Proto, SockAddr),
    i("done"),
    {SockMon, Server, Client}.



%% =======================================================================
%% Socket Monitor
%% =======================================================================

start_socket_monitor() ->
    Self = self(),
    {SockMon, MRef} = spawn_monitor(fun() -> socket_monitor_start(Self) end),
    receive
	{'DOWN', MRef, process, SockMon, Reason} ->
	    e("received unexpected down message from socket-monitor:"
	      "~n   ~p", [Reason]),
	    exit({socket_monitor, Reason});
	{SockMon, ready} ->
	    SockMon
    end.

socket_monitor_start(Parent) ->
    put(sname, "socket-monitor"),
    put(debug, true),
    erlang:register(socket_monitor, self()),
    Parent ! {self(), ready},
    %% i("started"),
    socket_monitor_loop(#{}).


socket_monitor_loop(State) ->
    receive
	{'DOWN', MRef, Kind, Sock, Info} when (Kind =:= socket) orelse
					      (Kind =:= port) ->
	    {{Sock, SockStr, _Pid}, State2} = maps:take(MRef, State),
	    i("received (socket) DOWN message: ~s"
	      "~n   ~p", [SockStr, Info]),
	    socket_monitor_loop(State2);

	{monitor, Sock, SockStr, Fun, Pid} ->
	    i("request to monitor socket: ~s", [SockStr]),
	    MRef = Fun(Sock),
	    socket_monitor_loop(State#{MRef => {Sock, SockStr, Pid}})
    end.



%% =======================================================================
%% Server
%% The server(s) processes are implemented on plain socket.
%% =======================================================================

start_server(Domain, Type, Proto) ->
    Self = self(),
    {Listener, MRef} =
	spawn_monitor(fun() ->
			      listener_start(Self, Domain, Type, Proto)
		      end),
    receive
	{'DOWN', MRef, process, Listener, Reason} ->
	    e("received unexpected down message from listener:"
	      "~n   ~p", [Reason]),
	    exit({listener, Reason});
	{Listener, ready, SockAddr} ->
	    {Listener, SockAddr}
    end.


listener_start(Parent, Domain, Type, Proto)
  when (Type =:= stream) andalso (Proto =:= tcp) ->
    put(sname, "listener"),
    %% put(debug, true),
    i("starting"),
    LSock     = listener_create_lsock(Domain, Type, Proto),
    LSM       = socket:monitor(LSock),
    ?SOCKET_MONITOR !
	{monitor, LSock, string_of(socket, LSock),
	 sockmon_fun(socket), self()},
    Acceptors = listener_create_acceptors(LSock),
    {ok, SockAddr} = socket:sockname(LSock),
    Parent ! {self(), ready, SockAddr},
    i("started"),
    listener_loop(#{lsock => LSock, lmon => LSM, accs => Acceptors}).

listener_loop(#{lsock := Sock} = State) ->
    receive
	{'DOWN', _MRef, socket, Sock, Reason} ->
	    e("unexpected (socket) down received: "
	      "~n   ~p", [Reason]),
	    listener_exit(socket_down, Reason);

	{'DOWN', _MRef, process, Pid, Reason} ->
	    i("unexpected (process ~p) down received: "
	      "~n   ~p", [Pid, Reason]),
	    listener_loop(listener_handle_down(State, Pid, Reason))
    end.

listener_handle_down(#{lsock := LSock, accs := Acceptors0} = State,
		     Pid, Reason) ->
    case maps:remove(Pid, Acceptors0) of
	Acceptors0 ->
	    i("unexpected down from unknown process ~p received: "
	      "~n   ~p", [Pid, Reason]),
	    State;
	_Acceptors1 ->
	    %% We could create a new acceptor here, but we need to make
	    %% sure we do not end up in a create-die loop, easier to just
	    %% assume *they should never die*...
	    e("unexpected down from acceptor process ~p received: "
	      "~n   ~p", [Pid, Reason]),
	    (catch socket:close(LSock)),
	    listener_exit(acceptor_down, Reason)
    end.

listener_create_lsock(Domain, Type, Proto) ->
    i("try extract local address"),
    Addr = case which_local_addr(Domain) of
	       {ok, A} ->
		   A;
	       {error, Reason} ->
		   listener_exit(failed_local_addr, Reason)
	   end,
    i("try create socket"),
    LSock = case socket:open(Domain, Type, Proto) of
		{ok, S} ->
		    S;
		{error, Reason1} ->
		    listener_exit(failed_socket_open, Reason1)
	    end,
    i("try bind socket"),
    case socket:bind(LSock, #{family => Domain,
			      addr   => Addr,
			      port   => 0}) of
	ok ->
	    ok;
	{error, Reason2} ->
	    listener_exit(failed_socket_bind, Reason2)
    end,
    i("try make listen socket"),
    case socket:listen(LSock, 10) of
	ok ->
	    ok;
	{error, Reason3} ->
	    listener_exit(failed_socket_listen, Reason3)
    end,
    i("listen socket created"),
    LSock.


listener_create_acceptors(LSock) ->
    listener_create_acceptors(LSock, 1, []).

listener_create_acceptors(_LSock, ID, Acc) when (ID > ?NUM_ACCEPTORS) ->
    maps:from_list(Acc);
listener_create_acceptors(LSock, ID, Acc) ->
    i("try create acceptor ~w", [ID ]),
    Acceptor = listener_create_acceptor(LSock, ID),
    listener_create_acceptors(LSock, ID+1, [Acceptor|Acc]).

listener_create_acceptor(LSock, ID) ->
    Self     = self(),
    Acceptor = {Pid, MRef} =
	spawn_monitor(fun() -> acceptor_start(Self, LSock, ID) end),
    receive
	{'$socket', LSock, abort, Info} ->
	    e("received unexpected select abort: "
	      "~n   ~p", [Info]),
	    listener_exit(abort, Info);

	{'DOWN', MRef, process, Pid, Reason} ->
	    e("received unexpected acceptor ~w down: "
	      "~n   ~p", [ID, Reason]),
	    listener_exit(acceptor_start, Reason);

	{Pid, ready} ->
	    i("received expected acceptor ~w ready", [ID]),
	    Acceptor
    end.

listener_exit(Tag, Reason) ->
    exit({listener, Tag, Reason}).


%% ---

acceptor_start(Listener, LSock, ID) ->
    put(sname, f("acceptor[~w]", [ID])),
    %% put(debug, true),
    MRef = erlang:monitor(process, Listener),
    Listener ! {self(), ready},
    i("started"),
    acceptor_loop(#{id       => ID,
		    hid      => 1,
		    listener => {Listener, MRef},
		    lsock    => LSock,
		    select   => undefined,
		    handlers => []}).

acceptor_loop(#{select := undefined} = State) ->
    acceptor_loop(acceptor_try_accept(State));

acceptor_loop(State) ->
    acceptor_loop(acceptor_try_select(State)).

acceptor_try_accept(#{lsock := LSock} = State) ->
    i("try accept (nowait)"),
    case socket:accept(LSock, nowait) of
	{ok, ASock} ->
	    %% i("accepted - spawn handler"),	
	    ?SOCKET_MONITOR !
		{monitor, ASock, string_of(socket, ASock),
		 sockmon_fun(socket), self()},
	    acceptor_create_handler(State, ASock);
	{select, SelectInfo} ->
	    %% i("selected:"
	    %%   "~n   ~p", [SelectInfo]),
	    State#{select => SelectInfo};
	{error, Reason} ->
	    e("accept failed: "
	      "~n   ~p", [Reason]),
	    acceptor_exit(State, accept, Reason)
    end.


acceptor_try_select(
  #{lsock    := LSock,
    listener := {Listener, MRef},
    select   := {select_info, _, Info} = SelectInfo} = State) ->
    i("await select message"),
    receive
	{'$socket', LSock, abort, Info} ->
	    e("received unexpected select abort: "
	      "~n   ~p", [Info]),
	    acceptor_exit(State, abort, Info);

	{'$socket', LSock, select, Info} ->
	    i("received select message: "
	      "~n   ~p", [Info]),
	    case socket:accept(LSock) of
		{ok, ASock} ->
		    %% i("accepted - spawn handler"),
		    ?SOCKET_MONITOR !
			{monitor, ASock, string_of(socket, ASock),
			 sockmon_fun(socket), self()},
		    acceptor_create_handler(State#{select => undefined},
					    ASock);
		{error, Reason} ->
		    e("accept failed: "
		      "~n   ~p", [Reason]),
		    %% This is a bit overkill, but just to be on the safe side
		    (catch socket:cancel(LSock, SelectInfo)),
		    acceptor_exit(State, post_select_accept, Reason)
	    end;

	{'DOWN', MRef, process, Listener, Reason} ->
	    e("listener down received: "
	      "~n   ~p", [Reason]),
	    (catch socket:cancel(LSock, SelectInfo)),
	    acceptor_exit(State, listener, Reason);

	{'DOWN', _MRef, process, _Pid, {{handler,_,_HID},recv,normal}} ->
	    %% i("received normal exit from handler ~w (~p)", [_HID, _Pid]),
	    acceptor_try_select(State);

	Any ->
	    i("received unexpected message: "
	      "~n   ~p"
	      "~nwhen"
	      "~n   LSock: ~p"
	      "~n   Info:  ~p", [Any, LSock, Info]),
	    acceptor_try_select(State)

    end.


acceptor_create_handler(#{listener := {Listener, MRef},
			  hid      := HID} = State, ASock) ->
    Self    = self(),
    _Handler = {HPid, HMRef} =
	spawn_monitor(fun() -> handler_start(Self, HID) end),
    receive
	{'$socket', ASock, abort, Info} ->
	    e("received unexpected select abort: "
	      "~n   ~p", [Info]),
	    acceptor_exit(State, abort, Info);

	{'DOWN', MRef, process, Listener, Reason} ->
	    e("listener down received: "
	      "~n   ~p", [Reason]),
	    exit(HPid, kill),
	    acceptor_exit(State, listener, Reason);

	{'DOWN', HMRef, process, HPid, Reason} ->
	    e("new handler (~p) down received: "
	      "~n   ~p", [HPid, Reason]),
	    acceptor_exit(State, handler, Reason);

	{HPid, ready} ->
	    case socket:setopt(ASock, otp, controlling_process, HPid) of
		ok ->
		    HPid ! {continue, ASock},
		    State#{hid => HID+1};
		{error, Reason} ->
		    e("failed changing controlling process: "
		      "~n   ~p", [Reason]),
		    (catch socket:close(ASock)),
		    exit(HPid, kill),
		    acceptor_exit(State, failed_changing_ctrl_proc, Reason)
	    end
    end.
    

acceptor_exit(#{id := ID}, Tag, Reason) ->
    exit({{acceptor, ID}, Tag, Reason}).


%% ---

handler_start(Parent, ID) ->
    put(sname, f("handler[~w]", [ID])),
    %% put(debug, true),
    i("starting"),
    MRef = erlang:monitor(process, Parent),
    Parent ! {self(), ready},
    receive
	{'DOWN', MRef, process, Parent, Reason} ->
	    e("parent (~p) down received: "
	      "~n   ~p", [Parent, Reason]),
	    handler_exit(#{id => undefined, parent => {Parent, undefined}},
			 parent, Reason);

	{continue, Sock} ->
	    i("started"),
	    handler_loop(#{id     => ID,
			   parent => {Parent, MRef},
			   sock   => Sock,
			   select => undefined,
			   buf    => <<>>})
    end.


handler_loop(#{select := undefined} = State) ->
    handler_loop(handler_try_read(State));
handler_loop(State) ->
    handler_loop(handler_try_select(State)).


handler_try_read(#{sock := Sock} = State) ->
    case socket:recv(Sock) of
	{ok, Data} ->
	    handler_process_data(State, Data);
	{select, SelectInfo} -> %% Will never happen with this, recv/1, call...
	    State#{select => SelectInfo};
	{error, closed} ->
	    i("recv got closed => client done"),
	    handler_exit(State, recv, normal);
	{error, Reason} ->
	    e("recv failed: "
	      "~n   ~p", [Reason]),
	    (catch socket:close(Sock)),
	    handler_exit(State, recv, Reason)
    end.

handler_try_select(
  #{sock   := Sock,
    parent := {Parent, MRef},
    select := {select_info, _, Info} = SelectInfo} = State) ->
    receive
	{'$socket', Sock, abort, Info} ->
	    e("received unexpected select abort: "
	      "~n   ~p", [Info]),
	    handler_exit(State, abort, Info);

	{'$socket', Sock, select, Info} ->
	    case socket:recv(Sock) of
		{ok, Data} ->
		    handler_process_data(State#{select => undefined}, Data);
		{error, Reason} ->
		    i("recv failed: "
		      "~n   ~p", [Reason]),
		    (catch socket:close(Sock)),
		    handler_exit(State, recv, Reason)
	    end;
	{'DOWN', MRef, process, Parent, Reason} ->
	    e("parent down received: "
	      "~n   ~p", [Reason]),
	    (catch socket:cancel(Sock, SelectInfo)),
	    (catch socket:close(Sock)),
	    handler_exit(State, parent, Reason)
    end.

handler_process_data(#{buf := <<>>} = State, NewData) ->
    handler_process_data(State#{buf => NewData});
handler_process_data(State, <<>>) ->
    handler_process_data(State);
handler_process_data(#{buf := Buf} = State, NewData) ->
    handler_process_data(State#{buf => <<Buf/binary, NewData/binary>>}).

handler_process_data(#{buf := <<?MTAG:32,
			       ID:32,
			       TYPE:32,
			       SZ:32,
			       Data:SZ/binary,
			       Rest/binary>>} = State) ->
    handler_process_data(State, ID, TYPE, Data),
    handler_process_data(State#{buf => Rest});
handler_process_data(State) ->
    State.


handler_process_data(_State, ID, ?PRINT, Data) ->
    i("~w: print"
      "~n   ~p", [ID, erlang:binary_to_list(Data)]);
handler_process_data(#{sock := Sock} = State, ID, ?REQUEST, Data) ->
    SZ = size(Data),
    Reply = <<?MTAG:32, ID:32, ?REPLY:32, SZ:32, Data/binary>>,
    case socket:send(Sock, Reply) of
	ok ->
	    ok;
	{error, Reason} ->
	    e("failed sending reply for request ~w: "
	      "~n   ~p", [ID, Reason]),
	    (catch socket:close(Sock)),
	    handler_exit(State, recv, Reason)
    end.

handler_exit(#{id := ID, parent := {Pid, _}}, Tag, Reason) ->
    exit({{handler, Pid, ID}, Tag, Reason}).


%% =======================================================================
%% Client
%% Of the client(s), one is implemented on gen_tcp with
%% inet_backend = socket and the other with inet_backend = inet.
%% The clients run for a period of 30 sec - 2 min, and then die.
%% Then a new is created!
%% Each client sends simple requests (with some dummy date), and
%% also once every 15 seconds an 'print' message (which the server
%% is to simply print the data (which is supposed to be a string).
%% =======================================================================

start_client(Domain, Type, Proto, ServerSockAddr) ->
    Self = self(),
    Client = {Pid, MRef} =
	spawn_monitor(fun() ->
			      client_ctrl_start(Self,
						Domain, Type, Proto,
						ServerSockAddr)
		      end),
    receive
	{'DOWN', MRef, process, Pid, Reason} ->
	    e("received unexpected down message from client (~p):"
	      "~n   ~p", [Pid, Reason]),
	    exit({client, Reason});

	{Pid, ready} ->
	    i("received expected ready from client"),
	    Client
    end.


client_ctrl_start(Parent,
		  Domain, Type, Proto,
		  ServerSockAddr) ->
    put(sname, "client-ctrl"),
    %% put(debug, true),
    State0 = #{parent   => Parent,
	       domain   => Domain,
	       type     => Type,
	       protocol => Proto,
	       server   => ServerSockAddr,
	       cid      => 1,
	       clients  => #{}},
    State1 = start_gen_client(State0, socket),
    State2 = start_gen_client(State1, inet),
    State3 = start_esock_client(State2),
    State4 = start_esock_client(State3),
    Parent ! {self(), ready},
    i("started"),
    client_ctrl_loop(State4).


client_ctrl_loop(State) ->
    receive
	{'DOWN', MRef, process, Pid, Reason} ->
	    client_ctrl_loop(client_ctrl_handle_down(State, MRef, Pid, Reason))
    end.

client_ctrl_handle_down(#{clients := Clients0} = State,
			MRef, Pid, Reason) ->
    case maps:take(Pid, Clients0) of
	{{ID, MRef, Backend}, Clients1} ->
	    i("received down from (gen) ~w-client ~w (~p): "
	      "~n   ~p", [Backend, ID, Pid, Reason]),
	    start_gen_client(State#{clients => Clients1}, Backend);
	{{ID, MRef}, Clients1} ->
	    i("received down from (esock) ~w-client ~w (~p): "
	      "~n   ~p", [ID, Pid, Reason]),
	    start_esock_client(State#{clients => Clients1});
	error ->
	    i("received down from unknown process ~p: "
	      "~n   ~p", [Pid, Reason]),
	    State
    end.
    
client_ctrl_exit(Tag, Reason) ->
    exit({'client-ctrl', Tag, Reason}).


start_gen_client(#{domain   := Domain,
		   type     := Type,
		   protocol := Proto,
		   server   := #{addr := Addr, port := Port},
		   cid      := ID,
		   clients  := Clients0} = State, Backend) ->
    Self = self(),
    i("try start (gen) client ~w", [ID]),
    LifeTime = rand:uniform(timer:minutes(3)) + timer:minutes(2),
    {Pid, MRef} =
	spawn_monitor(fun() ->
			      gen_client_start(Self,
					       Backend, ID,
					       LifeTime,
					       Domain, Type, Proto,
					       Addr, Port)
		      end),
    receive
	{'DOWN', MRef, process, Pid, Reason} ->
	    e("received unexpected down message from client ~w (~p):"
	      "~n   ~p", [ID, Pid, Reason]),
	    client_ctrl_exit({client, ID}, Reason);

	{Pid, ready} ->
	    i("received expected ready from client ~w (~p)", [ID, Pid]),
	    Clients1 = Clients0#{Pid => {ID, MRef, Backend}},
	    State#{cid => ID + 1, clients => Clients1}
    end.


start_esock_client(#{domain   := Domain,
		     type     := Type,
		     protocol := Proto,
		     server   := #{addr := Addr, port := Port},
		     cid      := ID,
		     clients  := Clients0} = State) ->
    Self = self(),
    i("try start (esock) client ~w", [ID]),
    LifeTime = rand:uniform(timer:minutes(3)) + timer:minutes(2),
    {Pid, MRef} =
	spawn_monitor(fun() ->
			      esock_client_start(Self,
						 ID,
						 LifeTime,
						 Domain, Type, Proto,
						 Addr, Port)
		      end),
    receive
	{'DOWN', MRef, process, Pid, Reason} ->
	    e("received unexpected down message from client ~w (~p):"
	      "~n   ~p", [ID, Pid, Reason]),
	    client_ctrl_exit({client, ID}, Reason);

	{Pid, ready} ->
	    i("received expected ready from client ~w (~p)", [ID, Pid]),
	    Clients1 = Clients0#{Pid => {ID, MRef}},
	    State#{cid => ID + 1, clients => Clients1}
    end.


gen_client_start(Parent, Backend, ID,
		 LifeTime,
		 Domain, Type, Proto,
		 ServerAddr, ServerPort)
  when (Type =:= stream) andalso (Proto =:= tcp) ->
    put(sname, f("gen-client[~w,~w]", [Backend, ID])),
    %% put(debug, true),
    i("starting"),
    State = gen_client_connect(#{id          => ID,
				 backend     => Backend,
				 domain      => Domain,
				 type        => Type,
				 protocol    => Proto,
				 server_addr => ServerAddr,
				 server_port => ServerPort}),
    erlang:send_after(LifeTime, self(), terminate),
    MRef = erlang:monitor(process, Parent),
    Parent ! {self(), ready},
    i("started"),
    gen_client_loop(State#{condition => send_request,
			   mid       => 1,
			   parent    => Parent,
			   mref      => MRef,
			   buf       => <<>>}).


gen_client_connect(#{backend     := Backend,
		     type        := Type,
		     protocol    := Proto,
		     server_addr := ServerAddr,
		     server_port := ServerPort} = State)
  when (Type =:= stream) andalso (Proto =:= tcp) ->
    COpts = [{inet_backend, Backend}, {active, true}, binary],
    i("try connect to ~s:~w",
      [inet_parse:ntoa(ServerAddr), ServerPort]),
    case gen_tcp:connect(ServerAddr, ServerPort, COpts) of
	{ok, Sock} ->
	    %% i("connected"),
	    ?SOCKET_MONITOR !
		{monitor, Sock,
		 string_of(inet, Sock),
		 sockmon_fun(inet), self()},
	    State#{sock => Sock};
	{error, Reason} ->
	    e("failed connecting: "
	      "~n   ~p", [Reason]),
	    client_exit(State, connect, Reason)
    end.


esock_client_start(Parent, ID,
		   LifeTime,
		   Domain, Type, Proto,
		   ServerAddr, ServerPort)
  when (Type =:= stream) andalso (Proto =:= tcp) ->
    put(sname, f("esock-client[~w]", [ID])),
    %% put(debug, true),
    i("starting"),
    State = esock_client_connect(#{id          => ID,
				   domain      => Domain,
				   type        => Type,
				   protocol    => Proto,
				   server_addr => ServerAddr,
				   server_port => ServerPort}),
    erlang:send_after(LifeTime, self(), terminate),
    MRef = erlang:monitor(process, Parent),
    Parent ! {self(), ready},
    i("started"),
    esock_client_loop(State#{condition => send_request,
			     select    => undefined,
			     mid       => 1,
			     parent    => Parent,
			     mref      => MRef,
			     buf       => <<>>}).


esock_client_connect(#{domain      := Domain,
		       type        := Type,
		       protocol    := Proto,
		       server_addr := ServerAddr,
		       server_port := ServerPort} = State)
  when (Type =:= stream) andalso (Proto =:= tcp) ->
    i("try open socket"),
    Sock =
	case socket:open(Domain, Type, Proto) of
	    {ok, S} ->
		i("opened"),
		S;
	    {error, Reason1} ->
		e("failed open: "
		  "~n   ~p", [Reason1]),
		client_exit(State, open, Reason1)
	end,
    %% We are on the same machine (as the server), so just reuse that address
    i("try bind to ~s", [inet_parse:ntoa(ServerAddr)]),
    case socket:bind(Sock, #{family => Domain, addr => ServerAddr}) of
	ok ->
	    i("bound"),
	    ok;
	{error, Reason2} ->
	    e("failed bind: "
	      "~n   ~p", [Reason2]),
	    (catch socket:close(Sock)),
	    client_exit(State, bind, Reason2)
    end,
    i("try connect to ~s:~w", [inet_parse:ntoa(ServerAddr), ServerPort]),
    case socket:connect(Sock, #{family => Domain,
				addr   => ServerAddr,
				port   => ServerPort}) of
	ok ->
	    i("connected"),
	    ?SOCKET_MONITOR !
		{monitor, Sock,
		 string_of(socket, Sock),
		 sockmon_fun(socket), self()},
	    State#{sock => Sock};
	{error, Reason3} ->
	    e("failed connecting: "
	      "~n   ~p", [Reason3]),
	    (catch socket:close(Sock)),
	    client_exit(State, connect, Reason3)
    end.


gen_client_loop(#{condition := terminate,
		  sock      := Sock}) ->
    (catch gen_tcp:close(Sock)),
    exit(normal);
gen_client_loop(#{condition := {await_reply, _MID},
		  parent    := Parent,
		  sock      := Sock} = State) ->
    receive
	{'DOWN', _MRef, process, Parent, Reason} ->
	    e("unexpected down from parent received: "
	      "~n   ~p", [Reason]),
	    client_exit(State, parent_down, Reason);

	{tcp, Sock, Data} ->
	    %% i("received (~w bytes of) data", [size(Data)]),
	    gen_client_loop(client_process_data(State, Data));

	terminate ->
	    gen_client_loop(State#{condition => terminate})

    end;
gen_client_loop(#{condition := send_request,
		  sock      := Sock,
		  mid       := MID} = State) ->
    %% i("try send request ~w", [MID]),
    Data = ?DATA,
    SZ   = size(Data),
    Req  = <<?MTAG:32, MID:32, ?REQUEST:32, SZ:32, Data/binary>>,
    case gen_tcp:send(Sock, Req) of
	ok ->
	    gen_client_loop(State#{condition => {await_reply, MID},
				   mid => MID + 1});
	{error, Reason} ->
	    e("failed sending request ~w: "
	      "~n   ~p", [MID, Reason]),
	    client_exit(State, send, Reason)
    end.



esock_client_loop(#{condition := terminate,
		    sock      := Sock}) ->
    (catch socket:close(Sock)),
    exit(normal);
esock_client_loop(#{condition := {await_reply, _MID},
		    select    := undefined,
		    sock      := Sock} = State) ->
    %% i("try (nowait) recv"),
    case socket:recv(Sock, 0, nowait) of
	{ok, Data} when is_binary(Data) ->
	    %% i("received (~w bytes of) data", [size(Data)]),
	    esock_client_loop(client_process_data(State, Data));
	%% This is the "old" style
	{ok, {Data, SelectInfo}} when is_binary(Data) ->
	    %% i("partial recv - select"),
	    Buf0 = maps:take(buf, State),
	    Buf2 = <<Buf0/binary, Data/binary>>,
	    esock_client_loop(State#{buf    => Buf2,
				     select => SelectInfo});
	%% This is the "new" style
	{select, {Data, SelectInfo}} when is_binary(Data) ->
	    %% i("partial recv - select"),
	    Buf0 = maps:take(buf, State),
	    Buf2 = <<Buf0/binary, Data/binary>>,
	    esock_client_loop(State#{buf    => Buf2,
				     select => SelectInfo});
	{select, SelectInfo} ->
	    %% i("select"),
	    esock_client_loop(State#{select => SelectInfo});
	{error, Reason} ->
	    e("recv failed: "
	      "~n   ~p", [Reason]),
	    (catch socket:close(Sock)),
	    client_exit(State, recv, Reason)
    end;
esock_client_loop(#{condition := {await_reply, _MID},
		    select    := {select_info, _, Info} = SelectInfo,
		    parent    := Parent,
		    sock      := Sock} = State) ->
    receive
	{'DOWN', _MRef, process, Parent, Reason} ->
	    e("unexpected down from parent received: "
	      "~n   ~p", [Reason]),
	    (catch socket:cancel(Sock, SelectInfo)),
	    (catch socket:close(Sock)),
	    client_exit(State, parent_down, Reason);

	{'$socket', Sock, abort, Info} ->
	    e("received unexpected select abort: "
	      "~n   ~p", [Info]),
	    (catch socket:close(Sock)),
	    client_exit(State, abort, Info);

	{'$socket', Sock, select, Info} ->
	    %% i("select message received - try recv"),
	    case socket:recv(Sock) of
		{ok, Data} ->
		    %% i("recv succeed (~w bytes of data received)", [size(Data)]),
		    esock_client_loop(
		      client_process_data(State#{select => undefined}, Data));
		{error, Reason} ->
		    e("recv failed: "
		      "~n   ~p", [Reason]),
		    (catch socket:close(Sock)),
		    client_exit(State, recv, Reason)
	    end;

	terminate ->
	    esock_client_loop(State#{condition => terminate})

    end;
esock_client_loop(#{condition := send_request,
		    sock      := Sock,
		    mid       := MID} = State) ->
    %% i("try send request ~w", [MID]),
    Data = ?DATA,
    SZ   = size(Data),
    Req  = <<?MTAG:32, MID:32, ?REQUEST:32, SZ:32, Data/binary>>,
    case socket:send(Sock, Req) of
	ok ->
	    esock_client_loop(State#{condition => {await_reply, MID},
				     mid => MID + 1});
	{error, Reason} ->
	    e("failed sending request ~w: "
	      "~n   ~p", [MID, Reason]),
	    client_exit(State, send, Reason)
    end.



client_process_data(#{condition := {await_reply, MID},
		      buf       := Buf} = State, NewData) ->
    case <<Buf/binary, NewData/binary>> of
	<<?MTAG:32, MID:32, ?REPLY:32, SZ:32, _Data:SZ/binary, Rest/binary>> ->
	    State#{condition => send_request, buf => Rest};
	<<?MTAG:32, MID2:32, ?REPLY:32, SZ:32, _Data:SZ/binary, _Rest/binary>> ->
	    client_exit(State, unexpected_msg, MID2);
	NewBuf ->
	    State#{buf => NewBuf}
    end.

client_exit(#{id := ID}, Tag, Reason) ->
    exit({{client, ID}, Tag, Reason}).


%% =======================================================================
%% Utility
%% =======================================================================

%% This gets the local address (not {127, _} or {0, ...} or {16#fe80, ...})
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_addr(Domain) ->
    case which_local_host_info(Domain) of
        {ok, #{addr := Addr}} ->
            {ok, Addr};
        {error, _Reason} = ERROR ->
            ERROR
    end.
    

%% Returns the interface (name), flags and address (not 127...)
%% of the local host.
which_local_host_info(Domain) ->
    case inet:getifaddrs() of
        {ok, IFL} ->
            which_local_host_info(Domain, IFL);
        {error, _} = ERROR ->
            ERROR
    end.

which_local_host_info(_Domain, []) ->
    {error, no_address};
which_local_host_info(Domain, [{"docker" ++ _, _}|IFL]) ->
    which_local_host_info(Domain, IFL);
which_local_host_info(Domain, [{"br-" ++ _, _}|IFL]) ->
    which_local_host_info(Domain, IFL);
which_local_host_info(Domain, [{Name, IFO}|IFL]) ->
    case if_is_running_and_not_loopback(IFO) of
        true ->
            try which_local_host_info2(Domain, IFO) of
                Info ->
                    {ok, Info#{name => Name}}
            catch
                throw:_:_ ->
                    which_local_host_info(Domain, IFL)
            end;
        false ->
            which_local_host_info(Domain, IFL)
    end;
which_local_host_info(Domain, [_|IFL]) ->
    which_local_host_info(Domain, IFL).

if_is_running_and_not_loopback(If) ->
    lists:keymember(flags, 1, If) andalso
        begin
            {value, {flags, Flags}} = lists:keysearch(flags, 1, If),
            (not lists:member(loopback, Flags)) andalso
                lists:member(running, Flags)
        end.


which_local_host_info2(inet = _Domain, IFO) ->
    Addr      = which_local_host_info3(addr,  IFO,
                                       fun({A, _, _, _}) when (A =/= 127) -> true;
                                          (_) -> false
                                       end),
    NetMask   = which_local_host_info3(netmask,  IFO,
                                       fun({_, _, _, _}) -> true;
                                          (_) -> false
                                       end),
    BroadAddr = which_local_host_info3(broadaddr,  IFO,
                                       fun({_, _, _, _}) -> true;
                                          (_) -> false
                                       end),
    Flags     = which_local_host_info3(flags, IFO, fun(_) -> true end),
    #{flags     => Flags,
      addr      => Addr,
      broadaddr => BroadAddr,
      netmask   => NetMask};
which_local_host_info2(inet6 = _Domain, IFO) ->
    Addr    = which_local_host_info3(addr,  IFO,
                                     fun({A, _, _, _, _, _, _, _}) 
                                           when (A =/= 0) andalso 
                                                (A =/= 16#fe80) -> true;
                                        (_) -> false
                                     end),
    NetMask = which_local_host_info3(netmask,  IFO,
                                       fun({_, _, _, _, _, _, _, _}) -> true;
                                          (_) -> false
                                       end),
    Flags   = which_local_host_info3(flags, IFO, fun(_) -> true end),
    #{flags   => Flags,
      addr    => Addr,
      netmask => NetMask}.

which_local_host_info3(_Key, [], _) ->
    throw({error, no_address});
which_local_host_info3(Key, [{Key, Val}|IFO], Check) ->
    case Check(Val) of
        true ->
            Val;
        false ->
            which_local_host_info3(Key, IFO, Check)
    end;
which_local_host_info3(Key, [_|IFO], Check) ->
    which_local_host_info3(Key, IFO, Check).


%% ---

string_of(socket = _Module, Socket) ->
    socket:to_list(Socket);
string_of(inet = _Module, Socket) ->
    inet:socket_to_list(Socket).


sockmon_fun(Module) ->
    fun(Sock) -> Module:monitor(Sock) end.
	    

%% ---



%% ---

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = TS) ->
    {_Date, Time}  = calendar:now_to_local_time(TS),
    {Hour,Min,Sec} = Time,
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.4~w",
                             [Hour, Min, Sec, round(N3/1000)]),  
    lists:flatten(FormatTS).


e(F, A) ->
    p(true, "<ERROR> ", F, A).

i(F) ->
    i(F, []).

i(F, A) ->
    p(get(debug), "", F, A).

p(true, PRE, F, A) ->
    io:format("[ ~s, ~s ] ~s" ++ F ++ "~n",
	      [formated_timestamp(), get(sname), PRE | A]);
p(_, _, _, _) ->
    ok.

