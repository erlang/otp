%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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
	 connect/1, connect/2, connect/3,
	 controlling_process/2,
	 listen/0, listen/1, listen/2,
	 port/1,
	 peername/1,
	 recv/2, recv/3,
	 send/2,
	 shutdown/2,
	 sockname/1
	]).


-define(LIB, socket_test_lib).

-define(READER_RECV_TIMEOUT, 1000).

-define(DATA_MSG(Sock, Method, Data),
        {socket,
         #{sock => Sock, reader => self(), method => Method},
         Data}).

-define(CLOSED_MSG(Sock, Method),
        {socket_closed,
         #{sock => Sock, reader => self(), method => Method}}).

-define(ERROR_MSG(Sock, Method, Reason),
        {socket_error,
         #{sock => Sock, reader => self(), method => Method},
         Reason}).


%% ==========================================================================

%% This does not really work. Its just a placeholder for the time being...

%% getopt(Sock, Opt) when is_atom(Opt) ->
%%     socket:getopt(Sock, socket, Opt).

%% setopt(Sock, Opt, Value) when is_atom(Opt) ->
%%     socket:setopts(Sock, socket, Opt, Value).


%% ==========================================================================

%% The way we use server async its no point in doing a async accept call
%% (we do never actually run the test with more than one client).
accept(#{sock := LSock, opts := #{async  := Async,
                                  method := Method} = Opts}) ->
    case socket:accept(LSock) of
        {ok, Sock} ->
	    Self = self(),
	    Reader = spawn(fun() ->
                                   reader_init(Self, Sock, Async, false, Method)
                           end),
            maybe_start_stats_timer(Opts, Reader),
	    {ok, #{sock => Sock, reader => Reader, method => Method}};
	{error, _} = ERROR ->
	    ERROR
    end.

%% If a timeout has been explictly specified, then we do not use
%% async here. We will pass it on to the reader process.
accept(#{sock := LSock, opts := #{async  := Async,
                                  method := Method} = Opts}, Timeout) ->
    case socket:accept(LSock, Timeout) of
	{ok, Sock} ->
	    Self = self(),
	    Reader = spawn(fun() ->
                                   reader_init(Self, Sock, Async, false, Method)
                           end),
            maybe_start_stats_timer(Opts, Reader),
	    {ok, #{sock => Sock, reader => Reader, method => Method}};
	{error, _} = ERROR ->
	    ERROR
    end.


active(#{reader := Pid}, NewActive) 
  when (is_boolean(NewActive) orelse (NewActive =:= once)) ->
    Pid ! {?MODULE, active, NewActive},
    ok.


close(#{sock := Sock, reader := Pid}) ->
    Pid ! {?MODULE, stop},
    Unlink = case socket:sockname(Sock) of
                 {ok, #{family := local, path := Path}} ->
                     fun() -> os:cmd("unlink " ++ Path), ok end;
                 _ ->
                     fun() -> ok end
             end,
    Res = socket:close(Sock),
    Unlink(),
    Res.

%% Create a socket and connect it to a peer
connect(ServerPath) when is_list(ServerPath) ->
    Domain     = local,
    ClientPath = mk_unique_path(),
    LocalSA    = #{family => Domain,
                   path   => ClientPath},
    ServerSA   = #{family => Domain, path => ServerPath},
    Opts       = #{domain => Domain,
                   proto  => default,
                   method => plain},
    Cleanup = fun() -> os:cmd("unlink " ++ ClientPath), ok end,
    do_connect(LocalSA, ServerSA, Cleanup, Opts).

connect(Addr, Port) when is_tuple(Addr) andalso is_integer(Port) ->
    Domain   = inet,
    LocalSA  = any,
    ServerSA = #{family => Domain,
                 addr   => Addr,
                 port   => Port},
    Opts     = #{domain => Domain,
                 proto  => tcp,
                 method => plain},
    Cleanup  = fun() -> ok end,
    do_connect(LocalSA, ServerSA, Cleanup, Opts);
connect(ServerPath,
        #{domain := local = Domain} = Opts)
  when is_list(ServerPath) ->
    ClientPath = mk_unique_path(),
    LocalSA    = #{family => Domain,
                   path   => ClientPath},
    ServerSA   = #{family => Domain,
                   path   => ServerPath},
    Cleanup    = fun() -> os:cmd("unlink " ++ ClientPath), ok end,
    do_connect(LocalSA, ServerSA, Cleanup, Opts#{proto => default}).

connect(Addr, Port, #{domain := Domain} = Opts) ->
    LocalSA  = any,
    ServerSA = #{family => Domain,
                 addr   => Addr,
                 port   => Port},
    Cleanup  = fun() -> ok end,
    do_connect(LocalSA, ServerSA, Cleanup, Opts#{proto => tcp}).

do_connect(LocalSA, ServerSA, Cleanup, #{domain := Domain,
                                         proto  := Proto,
                                         async  := Async,
                                         method := Method} = Opts) ->
    try
	begin
	    Sock =
		case socket:open(Domain, stream, Proto) of
		    {ok, S} ->
			S;
		    {error, OReason} ->
			throw({error, {open, OReason}})
		end,
	    case socket:bind(Sock, LocalSA) of
		{ok, _} ->
		    ok;
		{error, BReason} ->
		    (catch socket:close(Sock)),
                    Cleanup(),
		    throw({error, {bind, BReason}})
	    end,
	    case socket:connect(Sock, ServerSA) of
		ok ->
		    ok;
		{error, CReason} ->
		    (catch socket:close(Sock)),
                    Cleanup(),
		    throw({error, {connect, CReason}})
	    end,
	    Self   = self(),
	    Reader = spawn(fun() ->
                                   reader_init(Self, Sock, Async, false, Method)
                           end),
            maybe_start_stats_timer(Opts, Reader),
	    {ok, #{sock => Sock, reader => Reader, method => Method}}
	end
    catch
	throw:ERROR:_ ->
	    ERROR
    end.

mk_unique_path() ->
    [NodeName | _] = string:tokens(atom_to_list(node()), [$@]),
    ?LIB:f("/tmp/esock_~s_~w", [NodeName, erlang:system_time(nanosecond)]).

maybe_start_stats_timer(#{stats_to       := Pid,
                          stats_interval := T},
                        Reader) when is_pid(Pid) ->
    erlang:start_timer(T, Pid, {stats, T, "reader", Reader});
maybe_start_stats_timer(_O, _) ->
    ok.

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

listen(Port) when is_integer(Port) ->
    listen(Port, #{domain => inet, async => false, method => plain});
listen(Path) when is_list(Path) ->
    listen(Path, #{domain => local, async => false, method => plain}).

listen(0, #{domain := local} = Opts) ->
    listen(mk_unique_path(), Opts);
listen(Path, #{domain := local = Domain} = Opts)
  when is_list(Path) andalso (Path =/= []) ->
    SA = #{family => Domain,
           path   => Path},
    Cleanup = fun() -> os:cmd("unlink " ++ Path), ok end,
    do_listen(SA, Cleanup, Opts#{proto => default});
listen(Port, #{domain := Domain} = Opts)
  when is_integer(Port) andalso (Port >= 0) ->
    %% Bind fills in the rest
    case ?LIB:which_local_host_info(Domain) of
	{ok, #{addr := Addr}} ->
	    SA = #{family => Domain,
		   addr   => Addr,
		   port   => Port},
	    Cleanup = fun() -> ok end,
	    do_listen(SA, Cleanup, Opts#{proto => tcp});
	{error, _} = ERROR ->
	    ERROR
    end.

do_listen(SA,
          Cleanup,
          #{domain := Domain, proto  := Proto, 
            async  := Async,  method := Method} = Opts)
  when (Method =:= plain) orelse (Method =:= msg) andalso
       is_boolean(Async) ->
    try
	begin
	    Sock = case socket:open(Domain, stream, Proto) of
		       {ok, S} ->
			   S;
		       {error, OReason} ->
			   throw({error, {open, OReason}})
		   end,
	    case socket:bind(Sock, SA) of
		{ok, _} ->
		    ok;
		{error, BReason} ->
		    (catch socket:close(Sock)),
                    Cleanup(),
		    throw({error, {bind, BReason}})
	    end,
	    case socket:listen(Sock) of
		ok ->
                    ok;
                {error, LReason} ->
		    (catch socket:close(Sock)),
                    Cleanup(),
                    throw({error, {listen, LReason}})
            end,
	    {ok, #{sock => Sock, opts => Opts}}
	end
    catch
	throw:{error, Reason}:_ ->
	    {error, Reason}
    end.


port(#{sock := Sock}) ->
    case socket:sockname(Sock) of
	{ok, #{family := local, path := Path}} ->
	    {ok, Path};
	{ok, #{port := Port}} ->
	    {ok, Port};
	{error, _} = ERROR ->
	    ERROR
    end.


peername(#{sock := Sock}) ->
    case socket:peername(Sock) of
	{ok, #{family := local, path := Path}} ->
	    {ok, Path};
	{ok, #{addr := Addr, port := Port}} ->
	    {ok, {Addr, Port}};
	{error, _} = ERROR ->
	    ERROR
    end.


recv(#{sock := Sock, method := plain}, Length) ->
    socket:recv(Sock, Length);
recv(#{sock := Sock, method := msg}, Length) ->
    case socket:recvmsg(Sock, Length, 0, [], infinity) of
        {ok, #{iov := [Bin]}} ->
            {ok, Bin};
        {error, _} = ERROR ->
            ERROR
    end.

recv(#{sock := Sock, method := plain}, Length, Timeout) ->
    socket:recv(Sock, Length, Timeout);
recv(#{sock := Sock, method := msg}, Length, Timeout) ->
    case socket:recvmsg(Sock, Length, 0, [], Timeout) of
        {ok, #{iov := [Bin]}} ->
            {ok, Bin};
        {error, _} = ERROR ->
            ERROR
    end.


send(#{sock := Sock, method := plain}, Bin) ->
    socket:send(Sock, Bin);
send(#{sock := Sock, method := msg}, Bin) ->
    socket:sendmsg(Sock, #{iov => [Bin]}).


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

reader_init(ControllingProcess, Sock, Async, Active, Method) 
  when is_pid(ControllingProcess) andalso
       is_boolean(Async) andalso
       (is_boolean(Active) orelse (Active =:= once)) andalso 
       ((Method =:= plain) orelse (Method =:= msg)) ->
    put(verbose, false),
    MRef = erlang:monitor(process, ControllingProcess),
    reader_loop(#{ctrl_proc      => ControllingProcess,
		  ctrl_proc_mref => MRef,
                  async          => Async,
                  select_info    => undefined,
                  select_num     => 0, % Count the number of select messages
		  active         => Active,
		  sock           => Sock,
                  method         => Method}).


%% Never read
reader_loop(#{active    := false,
	      ctrl_proc := Pid} = State) ->
    receive
	{?MODULE, stop} ->
            reader_exit(State, stop);

	{?MODULE, Pid, controlling_process, NewPid} ->
	    OldMRef = maps:get(ctrl_proc_mref, State),
	    erlang:demonitor(OldMRef, [flush]),
	    NewMRef = erlang:monitor(process, NewPid),
	    Pid ! {?MODULE, self(), controlling_process},
	    reader_loop(State#{ctrl_proc      => NewPid,
			       ctrl_proc_mref => NewMRef});

	{?MODULE, active, NewActive} ->
	    reader_loop(State#{active => NewActive});

	{'DOWN', MRef, process, Pid, Reason} ->
	    case maps:get(ctrl_proc_mref, State) of
		MRef ->
                    reader_exit(State, {ctrl_exit, Reason});
		_ ->
		    reader_loop(State)
	    end
    end;

%% Read *once* and then change to false
reader_loop(#{active    := once,
	      async     := false,
              sock      := Sock,
              method    := Method,
	      ctrl_proc := Pid} = State) ->
    case do_recv(Method, Sock) of
	{ok, Data} ->
	    Pid ! ?DATA_MSG(Sock, Method, Data),
	    reader_loop(State#{active => false});
	{error, timeout} ->
	    receive
		{?MODULE, stop} ->
		    reader_exit(State, stop);

		{?MODULE, Pid, controlling_process, NewPid} ->
		    OldMRef = maps:get(ctrl_proc_mref, State),
		    erlang:demonitor(OldMRef, [flush]),
		    NewMRef = erlang:monitor(process, NewPid),
		    Pid ! {?MODULE, self(), controlling_process},
		    reader_loop(State#{ctrl_proc      => NewPid,
				       ctrl_proc_mref => NewMRef});

		{?MODULE, active, NewActive} ->
		    reader_loop(State#{active => NewActive});

		{'DOWN', MRef, process, Pid, Reason} ->
		    case maps:get(ctrl_proc_mref, State) of
                        MRef ->
                            reader_exit(State, {ctrl_exit, Reason});
			_ ->
			    reader_loop(State)
		    end
	    after 0 ->
		    reader_loop(State)
	    end;

	{error, closed} = E1 ->
            Pid ! ?CLOSED_MSG(Sock, Method),
            reader_exit(State, E1);

	{error, Reason} = E2 ->
	    Pid ! ?ERROR_MSG(Sock, Method, Reason),
            reader_exit(State, E2)
    end;
reader_loop(#{active      := once,
	      async       := true,
              select_info := undefined,
              sock        := Sock,
              method      := Method,
	      ctrl_proc   := Pid} = State) ->
    case do_recv(Method, Sock, nowait) of
        {select, SelectInfo} ->
            reader_loop(State#{select_info => SelectInfo});
	{ok, Data} ->
	    Pid ! ?DATA_MSG(Sock, Method, Data),
	    reader_loop(State#{active => false});

	{error, closed} = E1 ->
	    Pid ! ?CLOSED_MSG(Sock, Method),
            reader_exit(State, E1);

	{error, Reason} = E2 ->
	    Pid ! ?ERROR_MSG(Sock, Method, Reason),
            reader_exit(State, E2)
    end;
reader_loop(#{active      := once,
	      async       := true,
              select_info := {select_info, _, Ref},
              select_num  := N,
              sock        := Sock,
              method      := Method,
	      ctrl_proc   := Pid} = State) ->
    receive
        {?MODULE, stop} ->
            reader_exit(State, stop);
        
        {?MODULE, Pid, controlling_process, NewPid} ->
            OldMRef = maps:get(ctrl_proc_mref, State),
            erlang:demonitor(OldMRef, [flush]),
            NewMRef = erlang:monitor(process, NewPid),
            Pid ! {?MODULE, self(), controlling_process},
            reader_loop(State#{ctrl_proc      => NewPid,
                               ctrl_proc_mref => NewMRef});
        
        {?MODULE, active, NewActive} ->
            reader_loop(State#{active => NewActive});
        
        {'DOWN', MRef, process, Pid, Reason} ->
            case maps:get(ctrl_proc_mref, State) of
                MRef ->
                    reader_exit(State, {ctrl_exit, Reason});
                _ ->
                    reader_loop(State)
            end;

        {'$socket', Sock, select, Ref} ->
            case do_recv(Method, Sock, nowait) of
                {ok, Data} when is_binary(Data) ->
                    Pid ! ?DATA_MSG(Sock, Method, Data),
                    reader_loop(State#{active      => false,
                                       select_info => undefined,
                                       select_num  => N+1});
                
                {error, closed} = E1 ->
                    Pid ! ?CLOSED_MSG(Sock, Method),
                    reader_exit(State, E1);
                
                {error, Reason} = E2 ->
                    Pid ! ?ERROR_MSG(Sock, Method, Reason),
                    reader_exit(State, E2)
            end
    end;

%% Read and forward data continuously
reader_loop(#{active    := true,
	      async     := false,
	      sock      := Sock,
              method    := Method,
	      ctrl_proc := Pid} = State) ->
    case do_recv(Method, Sock) of
	{ok, Data} ->
	    Pid ! ?DATA_MSG(Sock, Method, Data),
	    reader_loop(State);
	{error, timeout} ->
	    receive
		{?MODULE, stop} ->
                    reader_exit(State, stop);

		{?MODULE, Pid, controlling_process, NewPid} ->
		    OldMRef = maps:get(ctrl_proc_mref, State),
		    erlang:demonitor(OldMRef, [flush]),
		    NewMRef = erlang:monitor(process, NewPid),
		    Pid ! {?MODULE, self(), controlling_process},
		    reader_loop(State#{ctrl_proc      => NewPid,
				       ctrl_proc_mref => NewMRef});

		{?MODULE, active, NewActive} ->
		    reader_loop(State#{active => NewActive});

		{'DOWN', MRef, process, Pid, Reason} ->
		    case maps:get(ctrl_proc_mref, State) of
			MRef ->
                            reader_exit(State, {ctrl_exit, Reason});
			_ ->
			    reader_loop(State)
		    end
	    after 0 ->
		    reader_loop(State)
	    end;

	{error, closed} = E1 ->
	    Pid ! ?CLOSED_MSG(Sock, Method),
            reader_exit(State, E1);

	{error, Reason} = E2 ->
	    Pid ! ?ERROR_MSG(Sock, Method, Reason),
            reader_exit(State, E2)
    end;
reader_loop(#{active      := true,
	      async       := true,
              select_info := undefined,
	      sock        := Sock,
              method      := Method,
	      ctrl_proc   := Pid} = State) ->
    case do_recv(Method, Sock) of
        {select, SelectInfo} ->
            reader_loop(State#{select_info => SelectInfo});
	{ok, Data} ->
	    Pid ! ?DATA_MSG(Sock, Method, Data),
	    reader_loop(State);

	{error, closed} = E1 ->
	    Pid ! ?CLOSED_MSG(Sock, Method),
            reader_exit(State, E1);

	{error, Reason} = E2 ->
	    Pid ! ?ERROR_MSG(Sock, Method, Reason),
            reader_exit(State, E2)
    end;
reader_loop(#{active      := true,
	      async       := true,
              select_info := {select_info, _, Ref},
              select_num  := N,
	      sock        := Sock,
              method      := Method,
	      ctrl_proc   := Pid} = State) ->
    receive
        {?MODULE, stop} ->
            reader_exit(State, stop);
        
        {?MODULE, Pid, controlling_process, NewPid} ->
            OldMRef = maps:get(ctrl_proc_mref, State),
            erlang:demonitor(OldMRef, [flush]),
            NewMRef = erlang:monitor(process, NewPid),
            Pid ! {?MODULE, self(), controlling_process},
            reader_loop(State#{ctrl_proc      => NewPid,
                               ctrl_proc_mref => NewMRef});
        
        {?MODULE, active, NewActive} ->
            reader_loop(State#{active => NewActive});
        
        {'DOWN', MRef, process, Pid, Reason} ->
            case maps:get(ctrl_proc_mref, State) of
                MRef ->
                    reader_exit(State, {ctrl_exit, Reason});
                _ ->
                    reader_loop(State)
            end;

        {'$socket', Sock, select, Ref} ->
            case do_recv(Method, Sock, nowait) of
                {ok, Data} when is_binary(Data) ->
                    Pid ! ?DATA_MSG(Sock, Method, Data),
                    reader_loop(State#{select_info => undefined,
                                       select_num  => N+1});
                
                {error, closed} = E1 ->
                    Pid ! ?CLOSED_MSG(Sock, Method),
                    reader_exit(State, E1);
                
                {error, Reason} = E2 ->
                    Pid ! ?ERROR_MSG(Sock, Method, Reason),
                    reader_exit(State, E2)
            end
    end.


do_recv(Method, Sock) ->
    do_recv(Method, Sock, ?READER_RECV_TIMEOUT).

do_recv(plain, Sock, Timeout) ->
    socket:recv(Sock, 0, Timeout);
do_recv(msg, Sock, Timeout) ->
    case socket:recvmsg(Sock, 0, 0, [], Timeout) of
        {ok, #{iov := [Bin]}} ->
            {ok, Bin};
        {select, _} = SELECT ->
            SELECT;
        {error, _} = ERROR ->
            ERROR
    end.


reader_exit(#{async := false, active := Active}, stop) ->
    vp("reader stopped when active: ~w", [Active]),
    exit(normal);
reader_exit(#{async       := true, 
              active      := Active,
              select_info := SelectInfo,
              select_num  := N}, stop) ->
    vp("reader stopped when active: ~w"
       "~n   Current select info:       ~p"
       "~n   Number of select messages: ~p", [Active, SelectInfo, N]),
    exit(normal);
reader_exit(#{async := false, active := Active}, {ctrl_exit, normal}) ->
    vp("reader ctrl exit when active: ~w", [Active]),
    exit(normal);
reader_exit(#{async       := true, 
              active      := Active,
              select_info := SelectInfo,
              select_num  := N}, {ctrl_exit, normal}) ->
    vp("reader ctrl exit when active: ~w"
       "~n   Current select info:       ~p"
       "~n   Number of select messages: ~p", [Active, SelectInfo, N]),
    exit(normal);
reader_exit(#{async := false, active := Active}, {ctrl_exit, Reason}) ->
    vp("reader exit when ctrl crash when active: ~w", [Active]),
    exit({controlling_process, Reason});
reader_exit(#{async       := true, 
              active      := Active,
              select_info := SelectInfo,
              select_num  := N}, {ctrl_exit, Reason}) ->
    vp("reader exit when ctrl crash when active: ~w"
       "~n   Current select info:       ~p"
       "~n   Number of select messages: ~p", [Active, SelectInfo, N]),
    exit({controlling_process, Reason});
reader_exit(#{async := false, active := Active}, {error, closed}) ->
    vp("reader exit when socket closed when active: ~w", [Active]),
    exit(normal);
reader_exit(#{async       := true, 
              active      := Active,
              select_info := SelectInfo,
              select_num  := N}, {error, closed}) ->
    vp("reader exit when socket closed when active: ~w "
       "~n   Current select info:       ~p"
       "~n   Number of select messages: ~p", [Active, SelectInfo, N]),
    exit(normal);
reader_exit(#{async := false, active := Active}, {error, Reason}) ->
    vp("reader exit when socket error when active: ~w", [Active]),
    exit(Reason);
reader_exit(#{async       := true, 
              active      := Active,
              select_info := SelectInfo,
              select_num  := N}, {error, Reason}) ->
    vp("reader exit when socket error when active: ~w: "
       "~n   Current select info:       ~p"
       "~n   Number of select messages: ~p", [Active, SelectInfo, N]),
    exit(Reason).






%% ==========================================================================

vp(F, A) ->
    vp(get(verbose), F, A).

vp(true, F, A) ->
    p(F, A);
vp(_, _, _) ->
    ok.

p(F, A) ->
    io:format(F ++ "~n", A).

