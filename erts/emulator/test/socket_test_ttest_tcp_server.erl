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

%% ==========================================================================
%%
%% This is the "simple" server using gen_tcp. The server is supposed to be
%% as simple as possible in order to incur as little overhead as possible.
%%
%% There are three ways to run the server: active, passive or active-once.
%%
%% The server does only two things; accept connnections and then reply
%% to requests (actually the handler(s) does that). No timing or counting.
%% That is all done by the clients.
%%
%% ==========================================================================

-module(socket_test_ttest_tcp_server).

-export([
         %% This are for the test suite
         start_monitor/3,

         %% This are for starting in a shell when run "manually"
         start/2,

         stop/1
        ]).

%% Internal exports
-export([
         do_start/3
        ]).

-include_lib("kernel/include/inet.hrl").
-include("socket_test_ttest.hrl").

-define(ACC_TIMEOUT,  5000).
-define(RECV_TIMEOUT, 5000).

-define(LIB,            socket_test_ttest_lib).
-define(I(F),           ?LIB:info(F)).
-define(I(F,A),         ?LIB:info(F, A)).
-define(E(F,A),         ?LIB:error(F, A)).
-define(F(F,A),         ?LIB:format(F, A)).
-define(FORMAT_TIME(T), ?LIB:format_time(T)).
-define(T(),            ?LIB:t()).
-define(TDIFF(T1,T2),   ?LIB:tdiff(T1, T2)).


%% ==========================================================================

start_monitor(Node, Transport, Active) when (Node =/= node()) ->
    case rpc:call(Node, ?MODULE, do_start, [self(), Transport, Active]) of
        {badrpc, _} = Reason ->
            {error, Reason};
        {ok, {Pid, AddrPort}} ->
            MRef = erlang:monitor(process, Pid),
            {ok, {{Pid, MRef}, AddrPort}};
        {error, _} = ERROR ->
            ERROR
    end;
start_monitor(_, Transport, Active) ->
    case do_start(self(), Transport, Active) of
        {ok, {Pid, AddrPort}} ->
            MRef = erlang:monitor(process, Pid),
            {ok, {{Pid, MRef}, AddrPort}};
        {error, _} = ERROR ->
            ERROR
    end.
            


start(Transport, Active) ->
    do_start(self(), Transport, Active).

%% Note that the Async option is actually only "used" for the
%% socket transport module (it details how to implement the
%% active feature).
do_start(Parent, Transport, Active)
  when is_pid(Parent) andalso
       (is_atom(Transport) orelse is_tuple(Transport)) andalso
       (is_boolean(Active) orelse (Active =:= once)) ->
    Starter    = self(),
    ServerInit = fun() -> 
                         put(sname, "server"),
                         server_init(Starter, Parent, Transport, Active)
                 end,
    {Pid, MRef} = spawn_monitor(ServerInit),
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, Reason};
        {?MODULE, Pid, {ok, AddrPort}} ->
            erlang:demonitor(MRef),
            {ok, {Pid, AddrPort}};
        {?MODULE, Pid, {error, _} = ERROR} ->
            erlang:demonitor(MRef, [flush]),
            ERROR
    end.
    

stop(Pid) when is_pid(Pid) ->
    req(Pid, stop).


%% ==========================================================================

server_init(Starter, Parent, Transport, Active) ->
    ?I("init with"
       "~n   Transport: ~p"
       "~n   Active:    ~p", [Transport, Active]),
    {Mod, Listen, StatsInterval} = process_transport(Transport, Active),
    case Listen(0) of
        {ok, LSock} ->
            case Mod:port(LSock) of
                {ok, PortOrPath} ->
                    Result =
                        if
                            is_integer(PortOrPath) ->
                                %% This is just for convenience
                                case Mod:sockname(LSock) of
				    {ok, {Addr, _}} ->
					?I("listening on:"
					   "~n   Addr: ~p (~s)"
					   "~n   Port: ~w"
					   "~n", [Addr,
						  inet:ntoa(Addr),
						  PortOrPath]),
					{Addr, PortOrPath};
				    {error, SNReason} ->
					exit({sockname, SNReason})
				end;
                            is_list(PortOrPath) ->
                                ?I("listening on:"
                                   "~n   Path: ~s"
                                   "~n", [PortOrPath]),
                                PortOrPath
                        end,
                    Starter ! {?MODULE, self(), {ok, Result}},
                    server_loop(#{parent         => Parent,
				  mod            => Mod,
                                  active         => Active,
                                  lsock          => LSock,
                                  port_or_path   => PortOrPath,
                                  handlers       => [],
                                  stats_interval => StatsInterval,
				  %% Accumulation
				  runtime  => 0,
				  mcnt     => 0,
				  bcnt     => 0,
				  hcnt     => 0
				 });
                {error, PReason} ->
                    (catch Mod:close(LSock)),
                    exit({port, PReason})
            end;
        {error, LReason} ->
            exit({listen, LReason})
    end.

process_transport(Mod, _) when is_atom(Mod) ->
    {Mod, fun(Port) -> Mod:listen(Port) end, infinity};
process_transport({Mod, #{stats_interval := T} = Opts}, Active)
  when (Active =/= false) ->
    {Mod, fun(Port) -> Mod:listen(Port, Opts#{stats_to => self()}) end, T};
process_transport({Mod, #{stats_interval := T} = Opts}, _Active) ->
    {Mod, fun(Port) -> Mod:listen(Port, Opts) end, T};
process_transport({Mod, Opts}, _Active) ->
    {Mod, fun(Port) -> Mod:listen(Port, Opts) end, infinity}.



server_loop(State) ->
    server_loop( server_handle_message( server_accept(State, ?ACC_TIMEOUT), 0) ).

server_accept(#{mod := Mod, lsock := LSock} = State, Timeout) ->
    case Mod:accept(LSock, Timeout) of
        {ok, Sock} ->
            server_handle_accepted(State, Sock);
        {error, timeout} when (Timeout =/= nowait) ->
            State;
        {error, AReason} ->
	    (catch Mod:close(LSock)),
            exit({accept, AReason})
    end.

%% server_accept(#{mod   := Mod,
%%                 lsock := LSock} = State) ->
%%     case Mod:accept(LSock, ?ACC_TIMEOUT) of
%%         {ok, Sock} ->
%%             server_handle_accepted(State, Sock);
%%         {error, timeout} ->
%%             State;
%%         {error, AReason} ->
%% 	    (catch Mod:close(LSock)),
%%             exit({accept, AReason})
%%     end.

server_handle_accepted(#{mod      := Mod,
                         lsock    := LSock,
                         active   := Active,
                         handlers := Handlers} = State,
                      Sock) ->
    ?I("accepted connection from ~s", 
       [case Mod:peername(Sock) of
            {ok, Peer} ->
                format_peername(Peer);
            {error, _} ->
                "-"
        end]),
    {Pid, _} = handler_start(),
    ?I("handler ~p started -> try transfer socket control", [Pid]),
    case Mod:controlling_process(Sock, Pid) of
        ok ->
            maybe_start_stats_timer(State, Pid),
            ?I("server-accept: handler ~p started", [Pid]),
            handler_continue(Pid, Mod, Sock, Active),
            Handlers2 = [Pid | Handlers],
            State#{handlers => Handlers2};
        {error, CPReason} ->
            (catch Mod:close(Sock)),
            (catch Mod:close(LSock)),
            exit({controlling_process, CPReason})
    end.
    

format_peername({Addr, Port}) ->
    case inet:gethostbyaddr(Addr) of
        {ok, #hostent{h_name = N}} ->
            ?F("~s (~s:~w)", [N, inet:ntoa(Addr), Port]);
        {error, _} ->
            ?F("~p, ~p", [Addr, Port])
    end;
format_peername(Path) when is_list(Path) ->
    Path.

maybe_start_stats_timer(#{active := Active, stats_interval := Time}, Handler)
  when (Active =/= false) andalso (is_integer(Time) andalso (Time > 0)) ->
    start_stats_timer(Time, "handler", Handler);
maybe_start_stats_timer(_, _) ->
    ok.

start_stats_timer(Time, ProcStr, Pid) ->
    erlang:start_timer(Time, self(), {stats, Time, ProcStr, Pid}).

server_handle_message(#{mod      := Mod,
                        lsock    := LSock,
                        parent   := Parent,
                        handlers := H} = State, Timeout) ->
    receive
        {timeout, _TRef, {stats, Interval, ProcStr, Pid}} ->
            case server_handle_stats(ProcStr, Pid) of
                ok ->
                    start_stats_timer(Interval, ProcStr, Pid);
                skip ->
                    ok
            end,
            State;

        {?MODULE, Ref, Parent, stop} ->
            reply(Parent, Ref, ok),
            lists:foreach(fun(P) -> handler_stop(P) end, H),
            (catch Mod:close(LSock)),
            exit(normal);

        {'DOWN', _MRef, process, Pid, Reason} -> 
	    server_handle_down(Pid, Reason, State)
            
    after Timeout ->
            State
    end.

server_handle_stats(ProcStr, Pid) ->
    case ?LIB:formated_process_stats(Pid) of
        "" ->
            skip;
        FormatedStats ->
            ?I("Statistics for ~s ~p:~s", [ProcStr, Pid, FormatedStats]),
            ok
    end.


server_handle_down(Pid, Reason, #{handlers := Handlers} = State) ->
    case lists:delete(Pid, Handlers) of
        Handlers ->
            ?I("unknown process ~p died", [Pid]),                    
            State;
        Handlers2 ->
            server_handle_handler_down(Pid, Reason, State#{handlers => Handlers2})
    end.


server_handle_handler_down(Pid,
			   {done, RunTime, MCnt, BCnt},
			   #{runtime := AccRunTime,
			     mcnt    := AccMCnt,
			     bcnt    := AccBCnt,
			     hcnt    := AccHCnt} = State) ->
    AccRunTime2 = AccRunTime + RunTime,
    AccMCnt2    = AccMCnt + MCnt,
    AccBCnt2    = AccBCnt + BCnt,
    AccHCnt2    = AccHCnt + 1,
    MsgCount2Str =
        fun(RT, ART, MC, AMC) when (RT > 0) ->
                ?F("~w => ~w (~w) msgs / ms", [MC, MC div RT, AMC div ART]);
           (_, _, MC, AMC) ->
                ?F("~w (~w)", [MC, AMC])
        end,
    ByteCount2Str =
        fun(RT, ART, BC, ABC) when (RT > 0) ->
                ?F("~w => ~w (~w) bytes / ms", [BC, BC div RT, ABC div ART]);
           (_, _, BC, ABC) ->
                ?F("~w", [BC, ABC])
        end,
    ?I("handler ~p (~w) done: "
       "~n   Run Time:      ~s"
       "~n   Message Count: ~s"
       "~n   Byte Count:    ~s",
       [Pid, AccHCnt2,
        ?FORMAT_TIME(RunTime),
        MsgCount2Str(RunTime, AccRunTime2, MCnt, AccMCnt2),
        ByteCount2Str(RunTime, AccRunTime2, BCnt, AccBCnt2)]),
    State#{runtime => AccRunTime2,
	   mcnt    => AccMCnt2,
	   bcnt    => AccBCnt2,
	   hcnt    => AccHCnt2};
server_handle_handler_down(Pid, Reason, State) ->
    ?I("handler ~p terminated: "
       "~n   ~p", [Pid, Reason]),
    State.



%% ==========================================================================

handler_start() ->
    Self         = self(),
    HandlerInit = fun() -> put(sname, "handler"), handler_init(Self) end,
    spawn_monitor(HandlerInit).

handler_continue(Pid, Mod, Sock, Active) ->
    req(Pid, {continue, Mod, Sock, Active}).

handler_stop(Pid) ->
    req(Pid, stop).

handler_init(Parent) ->
    ?I("starting"),
    receive
	{?MODULE, Ref, Parent, {continue, Mod, Sock, Active}} ->
	    ?I("received continue"),
	    reply(Parent, Ref, ok),
	    handler_initial_activation(Mod, Sock, Active),
	    handler_loop(#{parent      => Parent,
			   mod         => Mod,
			   sock        => Sock,
			   active      => Active,
			   start       => ?T(),
			   mcnt        => 0,
			   bcnt        => 0,
			   last_reply  => none,
			   acc         => <<>>})

    after 5000 ->
	    ?I("timeout when message queue: "
               "~n   ~p"
               "~nwhen"
               "~n   Parent: ~p", [process_info(self(), messages), Parent]),
	    handler_init(Parent)
    end.

handler_loop(State) ->
    handler_loop( handler_handle_message( handler_recv_message(State) ) ).

%% When passive, we read *one* request and then attempt to reply to it.
handler_recv_message(#{mod        := Mod,
		       sock       := Sock,
                       active     := false,
                       mcnt       := MCnt,
                       bcnt       := BCnt,
                       last_reply := LID} = State) ->
    case handler_recv_message2(Mod, Sock) of
        {ok, {MsgSz, ID, Body}} ->
            handler_send_reply(Mod, Sock, ID, Body),
            State#{mcnt       => MCnt + 1,
                   bcnt       => BCnt + MsgSz,
		   last_reply => ID};
        {error, closed} ->
            handler_done(State);
        {error, timeout} ->
	    ?I("timeout when: "
               "~n   MCnt: ~p"
               "~n   BCnt: ~p"
               "~n   LID:  ~p", [MCnt, BCnt, LID]),
            State
    end;


%% When "active" (once or true), we receive one data "message", which may 
%% contain any number of requests or only part of a request. Then we 
%% process this data together with whatever we had "accumulated" from 
%% prevous messages. Each request will be extracted and replied to. If
%% there is some data left, not enough for a complete request, we store
%% this in 'acc' (accumulate it).
handler_recv_message(#{mod        := Mod,
		       sock       := Sock,
                       active     := Active,
                       mcnt       := MCnt,
                       bcnt       := BCnt,
                       last_reply := LID,
                       acc        := Acc} = State) ->
    case handler_recv_message3(Mod, Sock, Acc, LID) of
	{ok, {MCnt2, BCnt2, LID2}, NewAcc} ->
	    handler_maybe_activate(Mod, Sock, Active),
	    State#{mcnt       => MCnt + MCnt2,
		   bcnt       => BCnt + BCnt2,
		   last_reply => LID2,
		   acc        => NewAcc};

        {error, closed} ->
            if
                (size(Acc) =:= 0) ->
                    handler_done(State);
                true ->
                    ?E("client done with partial message: "
                       "~n   Last Reply Sent: ~w"
                       "~n   Message Count:   ~w"
                       "~n   Byte    Count:   ~w"
                       "~n   Partial Message: ~w bytes",
                       [LID, MCnt, BCnt, size(Acc)]),
                    exit({closed_with_partial_message, LID})
            end;

        {error, timeout} ->
	    ?I("timeout when: "
               "~n   MCnt:      ~p"
               "~n   BCnt:      ~p"
               "~n   LID:       ~p"
               "~n   size(Acc): ~p", [MCnt, BCnt, LID, size(Acc)]),
            State
    end.

handler_process_data(Acc, Mod, Sock, LID) ->
    handler_process_data(Acc, Mod, Sock, 0, 0, LID).

%% Extract each complete request, one at a time.
handler_process_data(<<?TTEST_TAG:32,
		       ?TTEST_TYPE_REQUEST:32,
 		       ID:32,
 		       SZ:32,
 		       Rest/binary>>,
 		     Mod, Sock,
 		     MCnt, BCnt, _LID) when (size(Rest) >= SZ)  ->
    <<Body:SZ/binary, Rest2/binary>> = Rest,
    case handler_send_reply(Mod, Sock, ID, Body) of
 	ok ->
 	    handler_process_data(Rest2, Mod, Sock, MCnt+1, BCnt+16+SZ, ID);
 	{error, _} = ERROR ->
 	    ERROR
    end;
handler_process_data(Data, _Mod, _Sock, MCnt, BCnt, LID) ->
    {ok, {MCnt, BCnt, LID}, Data}.
	    
	    
handler_recv_message2(Mod, Sock) ->
    case Mod:recv(Sock, 4*4, ?RECV_TIMEOUT) of
        {ok, <<?TTEST_TAG:32,
               ?TTEST_TYPE_REQUEST:32,
               ID:32,
               SZ:32>> = Hdr} ->
            case Mod:recv(Sock, SZ, ?RECV_TIMEOUT) of
                {ok, Body} when (SZ =:= size(Body)) ->
                    {ok, {size(Hdr) + size(Body), ID, Body}};
                {error, BReason} ->
                    ?E("failed reading body (~w) of message ~w:"
                       "~n   ~p", [SZ, ID, BReason]),
                    exit({recv, body, ID, SZ, BReason})
            end;
        {error, timeout} = ERROR ->
            ERROR;
        {error, closed} = ERROR ->
            ERROR;
        {error, HReason} ->
            ?E("Failed reading header of message:"
               "~n   ~p", [HReason]),
            exit({recv, header, HReason})
    end.


handler_recv_message3(Mod, Sock, Acc, LID) ->
    receive
        {TagClosed, Sock} when (TagClosed =:= tcp_closed) orelse
			       (TagClosed =:= socket_closed) ->
            {error, closed};

        {TagErr, Sock, Reason} when (TagErr =:= tcp_error) orelse
				    (TagErr =:= socket_error) ->
            {error, Reason};

        {Tag, Sock, Msg} when (Tag =:= tcp) orelse
			      (Tag =:= socket) ->
            handler_process_data(<<Acc/binary, Msg/binary>>, Mod, Sock, LID)
        
    after ?RECV_TIMEOUT ->
            {error, timeout}
    end.



handler_send_reply(Mod, Sock, ID, Data) ->
    SZ = size(Data),
    Msg = <<?TTEST_TAG:32,
	    ?TTEST_TYPE_REPLY:32,
	    ID:32,
	    SZ:32,
	    Data/binary>>,
    case Mod:send(Sock, Msg) of
        ok ->
            ok;
        {error, Reason} ->
            (catch Mod:close(Sock)),
            exit({send, Reason})
    end.


handler_done(State) ->
    handler_done(State, ?T()).

handler_done(#{start := Start,
	       mod   := Mod,
               sock  := Sock,
               mcnt  := MCnt,
               bcnt  := BCnt}, Stop) ->
    (catch Mod:close(Sock)),
    exit({done, ?TDIFF(Start, Stop), MCnt, BCnt}).


handler_handle_message(#{parent := Parent} = State) ->
    receive
        {'EXIT', Parent, Reason} ->
            exit({parent_exit, Reason})
    after 0 ->
            State
    end.


handler_initial_activation(_Mod, _Sock, false = _Active) ->
    ok;
handler_initial_activation(Mod, Sock, Active) ->
    Mod:active(Sock, Active).


handler_maybe_activate(Mod, Sock, once = Active) ->
    Mod:active(Sock, Active);
handler_maybe_activate(_, _, _) ->
    ok.



%% ==========================================================================

%% which_addr() ->
%%     case inet:getifaddrs() of
%%         {ok, IfAddrs} ->
%%             which_addrs(inet, IfAddrs);
%%         {error, Reason} ->
%%             exit({getifaddrs, Reason})
%%     end.

%% which_addrs(_Family, []) ->
%%     exit({getifaddrs, not_found});
%% which_addrs(Family, [{"lo", _} | IfAddrs]) ->
%%     %% Skip
%%     which_addrs(Family, IfAddrs);
%% which_addrs(Family, [{"docker" ++ _, _} | IfAddrs]) ->
%%     %% Skip docker
%%     which_addrs(Family, IfAddrs);
%% which_addrs(Family, [{"br-" ++ _, _} | IfAddrs]) ->
%%     %% Skip docker
%%     which_addrs(Family, IfAddrs);
%% which_addrs(Family, [{"en" ++ _, IfOpts} | IfAddrs]) ->
%%     %% Maybe take this one
%%     case which_addr(Family, IfOpts) of
%%         {ok, Addr} ->
%%             Addr;
%%         error ->
%%             which_addrs(Family, IfAddrs)
%%     end;
%% which_addrs(Family, [{_IfName, IfOpts} | IfAddrs]) ->
%%     case which_addr(Family, IfOpts) of
%%         {ok, Addr} ->
%%             Addr;
%%         error ->
%%             which_addrs(Family, IfAddrs)
%%     end.

%% which_addr(_, []) ->
%%     error;
%% which_addr(inet, [{addr, Addr}|_])
%%   when is_tuple(Addr) andalso (size(Addr) =:= 4) ->
%%     {ok, Addr};
%% which_addr(inet6, [{addr, Addr}|_])
%%   when is_tuple(Addr) andalso (size(Addr) =:= 8) ->
%%     {ok, Addr};
%% which_addr(Family, [_|IfOpts]) ->
%%     which_addr(Family, IfOpts).


%% ==========================================================================

req(Pid, Req) ->
    Ref = make_ref(),
    Pid ! {?MODULE, Ref, self(), Req},
    receive
        {'EXIT', Pid, Reason} ->
            {error, {exit, Reason}};
        {?MODULE, Ref, Reply} ->
            Reply
    end.

reply(Pid, Ref, Reply) ->
    Pid ! {?MODULE, Ref, Reply}.


%% ==========================================================================

%% t() ->
%%     os:timestamp().

%% tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
%%     T1 = A1*1000000000+B1*1000+(C1 div 1000), 
%%     T2 = A2*1000000000+B2*1000+(C2 div 1000), 
%%     T2 - T1.

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp({_N1, _N2, N3} = TS) ->
%%     {_Date, Time}  = calendar:now_to_local_time(TS),
%%     {Hour,Min,Sec} = Time,
%%     FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.4~w",
%%                              [Hour, Min, Sec, round(N3/1000)]),  
%%     lists:flatten(FormatTS).

%% %% Time is always in number os ms (milli seconds)
%% format_time(T) ->
%%     f("~p", [T]).


%% ==========================================================================

%% f(F, A) ->
%%     lists:flatten(io_lib:format(F, A)).

%% e(F, A) ->
%%     p(get(sname), "<ERROR> " ++ F, A).

%% i(F) ->
%%     i(F, []).

%% i(F, A) ->
%%     p(get(sname), "<INFO> " ++ F, A).

%% p(undefined, F, A) ->
%%     p("- ", F, A);
%% p(Prefix, F, A) ->
%%     io:format("[~s, ~s] " ++ F ++ "~n", [formated_timestamp(), Prefix |A]).

