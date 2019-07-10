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
%% This is the "simple" client using gen_tcp. The client is supposed to be
%% as simple as possible in order to incur as little overhead as possible.
%%
%% There are three ways to run the client: active, passive or active-once.
%%
%% The client is the entity that controls the test, timing and counting.
%%
%% ==========================================================================
%%
%% Before the actual test starts, the client performs a "warmup".
%% The warmup has two functions. First, to ensure that everything is "loaded"
%% and, second, to calculate an approximate roundtrip time, in order to
%% "know" how many iterations we should make (to run for the expected time).
%% This is not intended to be exact, but just to ensure that all tests take
%% approx the same time to run.
%%
%% ==========================================================================

-module(socket_test_ttest_tcp_client).

-export([
         %% These are for the test suite
         start_monitor/5, start_monitor/6, start_monitor/8,

         %% These are for starting in a shell when run "manually"
         start/3, start/4, start/6, start/7,
         stop/1
        ]).

%% Internal exports
-export([
         do_start/9
        ]).

-include_lib("kernel/include/inet.hrl").
-include("socket_test_ttest.hrl").
-include("socket_test_ttest_client.hrl").

-define(RECV_TIMEOUT,              10000).
-define(MAX_OUTSTANDING_DEFAULT_1, 100).
-define(MAX_OUTSTANDING_DEFAULT_2, 10).
-define(MAX_OUTSTANDING_DEFAULT_3, 3).

-define(LIB,            socket_test_ttest_lib).
-define(I(F),           ?LIB:info(F)).
-define(I(F,A),         ?LIB:info(F, A)).
-define(E(F,A),         ?LIB:error(F, A)).
-define(F(F,A),         ?LIB:format(F, A)).
-define(FORMAT_TIME(T), ?LIB:format_time(T)).
-define(T(),            ?LIB:t()).
-define(TDIFF(T1,T2),   ?LIB:tdiff(T1, T2)).

-type active()          :: once | boolean().
-type msg_id()          :: 1..3.
-type max_outstanding() :: pos_integer().
-type runtime()         :: pos_integer().


%% ==========================================================================

start_monitor(Node, Notify, Transport, ServerInfo, Active) ->
    start_monitor(Node, Notify, Transport, ServerInfo, Active, ?MSG_ID_DEFAULT).

start_monitor(Node, Notify, Transport, ServerInfo, Active, 1 = MsgID) ->
    start_monitor(Node, Notify, Transport, ServerInfo, Active, MsgID,
                  ?MAX_OUTSTANDING_DEFAULT_1, ?RUNTIME_DEFAULT);
start_monitor(Node, Notify, Transport, ServerInfo, Active, 2 = MsgID) ->
    start_monitor(Node, Notify, Transport, ServerInfo, Active, MsgID,
                  ?MAX_OUTSTANDING_DEFAULT_2, ?RUNTIME_DEFAULT);
start_monitor(Node, Notify, Transport, ServerInfo, Active, 3 = MsgID) ->
    start_monitor(Node, Notify, Transport, ServerInfo, Active, MsgID,
                  ?MAX_OUTSTANDING_DEFAULT_3, ?RUNTIME_DEFAULT).

start_monitor(Node, Notify, Transport, ServerInfo, Active,
              MsgID, MaxOutstanding, RunTime)
  when (Node =/= node()) ->
    Args = [false,
	    self(), Notify,
            Transport, ServerInfo, Active, MsgID, MaxOutstanding, RunTime],
    case rpc:call(Node, ?MODULE, do_start, Args) of
        {badrpc, _} = Reason ->
            {error, Reason};
        {ok, Pid} when is_pid(Pid) ->
            MRef = erlang:monitor(process, Pid),
            {ok, {Pid, MRef}};
        {error, _} = ERROR ->
            ERROR
    end;
start_monitor(_, Notify, Transport, ServerInfo, Active,
              MsgID, MaxOutstanding, RunTime) ->
    case do_start(false,
		  self(), Notify,
                  Transport, Active, ServerInfo,
                  MsgID, MaxOutstanding, RunTime) of
        {ok, Pid} ->
            MRef = erlang:monitor(process, Pid),
            {ok, {Pid, MRef}};
        {error, _} = ERROR ->
            ERROR
    end.


start(Transport, ServerInfo, Active) ->
    start(Transport, ServerInfo, Active, ?MSG_ID_DEFAULT).

start(Transport, ServerInfo, Active, 1 = MsgID) ->
    start(false,
	  Transport, ServerInfo, Active, MsgID,
          ?MAX_OUTSTANDING_DEFAULT_1, ?RUNTIME_DEFAULT);
start(Transport, ServerInfo, Active, 2 = MsgID) ->
    start(false,
	  Transport, ServerInfo, Active, MsgID,
          ?MAX_OUTSTANDING_DEFAULT_2, ?RUNTIME_DEFAULT);
start(Transport, ServerInfo, Active, 3 = MsgID) ->
    start(false,
	  Transport, ServerInfo, Active, MsgID,
          ?MAX_OUTSTANDING_DEFAULT_3, ?RUNTIME_DEFAULT).

start(Transport, ServerInfo, Active, MsgID, MaxOutstanding, RunTime) ->
    start(false,
	  Transport, ServerInfo, Active, MsgID, MaxOutstanding, RunTime).

start(Quiet, Transport, ServerInfo, Active, MsgID, MaxOutstanding, RunTime) ->
    Notify = fun(R) -> present_results(R) end,
    do_start(Quiet,
	     self(), Notify,
             Transport, ServerInfo, Active, MsgID, MaxOutstanding, RunTime).
                      

-spec do_start(Quiet,
	       Parent,
               Notify,
               Transport,
               ServerInfo,
               Active,
               MsgID,
               MaxOutstanding,
               RunTime) -> {ok, Pid} | {error, Reason} when
      Quiet          :: boolean(),
      Parent         :: pid(),
      Notify         :: function(),
      Transport      :: atom() | tuple(),
      ServerInfo     :: {inet:ip_address(), inet:port_number()} | string(),
      Active         :: active(),
      MsgID          :: msg_id(),
      MaxOutstanding :: max_outstanding(),
      RunTime        :: runtime(),
      Pid            :: pid(),
      Reason         :: term().

do_start(Quiet,
	 Parent, Notify,
         Transport, ServerInfo, Active, MsgID, MaxOutstanding, RunTime)
  when is_boolean(Quiet) andalso
       is_pid(Parent) andalso
       is_function(Notify) andalso
       (is_atom(Transport) orelse is_tuple(Transport)) andalso
       (is_boolean(Active) orelse (Active =:= once)) andalso
       (is_tuple(ServerInfo) orelse is_list(ServerInfo)) andalso 
       (is_integer(MsgID) andalso (MsgID >= 1) andalso (MsgID =< 3)) andalso
       (is_integer(MaxOutstanding) andalso (MaxOutstanding > 0)) andalso
       (is_integer(RunTime) andalso (RunTime > 0)) ->
    Starter = self(),
    Init = fun() -> put(sname, "client"),
                    init(Quiet,
			 Starter, 
                         Parent,
                         Notify,
                         Transport, Active, ServerInfo,
                         MsgID, MaxOutstanding, RunTime)
           end,
    {Pid, MRef} = spawn_monitor(Init),
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, Reason};
        {?MODULE, Pid, ok} ->
            erlang:demonitor(MRef),
            {ok, Pid};
        {?MODULE, Pid, {error, _} = ERROR} ->
            erlang:demonitor(MRef, [flush]),
            ERROR
    end.


%% We should not normally stop this (it terminates when its done).
stop(Pid) when is_pid(Pid) ->
    req(Pid, stop).


%% ==========================================================================

init(Quiet,
     Starter,
     Parent, Notify,
     Transport, Active, ServerInfo,
     MsgID, MaxOutstanding, RunTime) ->
    if
	not Quiet ->
	    ?I("init with"
	       "~n   Transport:            ~p"
	       "~n   Active:               ~p"
	       "~n   ServerInfo:           ~s"
	       "~n   Msg ID:               ~p (=> 16 + ~w bytes)"
	       "~n   Max Outstanding:      ~p"
	       "~n   (Suggested) Run Time: ~p ms",
	       [Transport, Active,
                case ServerInfo of
                    {Addr, Port} ->
                        ?F("Addr: ~s, Port: ~w", [inet:ntoa(Addr), Port]);
                    Path ->
                        Path
                end,
		MsgID, size(which_msg_data(MsgID)), MaxOutstanding, RunTime]);
	true ->
	    ok
    end,
    {Mod, Connect} = process_transport(Transport),
    case Connect(ServerInfo) of
        {ok, Sock} ->
            if not Quiet -> ?I("connected");
	       true      -> ok
	    end,
            Starter ! {?MODULE, self(), ok},
	    initial_activation(Mod, Sock, Active),
            Results = loop(#{quiet           => Quiet,
			     slogan          => run,
			     runtime         => RunTime,
			     start           => ?T(),
			     parent          => Parent,
			     mod             => Mod,
			     sock            => Sock,
			     active          => Active,
			     msg_data        => which_msg_data(MsgID),
			     outstanding     => 0,
			     max_outstanding => MaxOutstanding,
			     sid             => 1,
			     rid             => 1,
			     scnt            => 0,
			     rcnt            => 0,
			     bcnt            => 0,
			     num             => undefined,
			     acc             => <<>>}),
            Notify(Results),
            (catch Mod:close(Sock)),
            exit(normal);
        {error, Reason} ->
            ?E("connect failed: ~p"
	       "~n   ~p", [Reason, ServerInfo]),
            exit({connect, Reason, ServerInfo})
    end.

process_transport(Mod) when is_atom(Mod) ->
    %% In this case we assume it to be a plain tcp socket
    {Mod, fun({A, P}) -> Mod:connect(A, P) end};
process_transport({Mod, #{domain := Domain} = Opts}) ->
    Connect =
        case Domain of
            local -> fun(Path)   -> Mod:connect(Path, Opts) end;
            _     -> fun({A, P}) -> Mod:connect(A, P, Opts) end
        end,
    {Mod, Connect}.

            
which_msg_data(1) -> ?MSG_DATA1;
which_msg_data(2) -> ?MSG_DATA2;
which_msg_data(3) -> ?MSG_DATA3.


present_results(#{status  := ok,
		  runtime := RunTime,
                  bcnt    := ByteCnt,
                  cnt     := NumIterations}) ->
    ?I("Results: "
       "~n   Run Time:      ~s"
       "~n   ByteCnt:       ~s"
       "~n   NumIterations: ~s", 
       [?FORMAT_TIME(RunTime), 
        if ((ByteCnt =:= 0) orelse (RunTime =:= 0)) ->
                ?F("~w, ~w", [ByteCnt, RunTime]);
          true ->
                ?F("~p => ~p byte / ms", [ByteCnt, ByteCnt div RunTime])
       end,
       if (RunTime =:= 0) ->
               "-";
          true ->
               ?F("~p => ~p iterations / ms",
                  [NumIterations, NumIterations div RunTime])
       end]),
    ok;
present_results(#{status  := Failure,
		  runtime := RunTime,
		  sid     := SID,
		  rid     := RID,
                  scnt    := SCnt,
                  rcnt    := RCnt,
                  bcnt    := BCnt,
		  num     := Num}) ->
    ?I("Time Test failed: "
       "~n   ~p"
       "~n"
       "~nwhen"
       "~n"
       "~n   Run Time:       ~s"
       "~n   Send ID:        ~p"
       "~n   Recv ID:        ~p"
       "~n   Send Count:     ~p"
       "~n   Recv Count:     ~p"
       "~n   Byte Count:     ~p"
       "~n   Num Iterations: ~p",
       [Failure,
        ?FORMAT_TIME(RunTime),
        SID, RID, SCnt, RCnt, BCnt, Num]).



loop(#{runtime := RunTime} = State) ->
    erlang:start_timer(RunTime, self(), stop),
    try do_loop(State)
    catch
        throw:Results ->
            Results
    end.

do_loop(State) ->
    do_loop( handle_message( msg_exchange(State) ) ).

msg_exchange(#{rcnt := Num, num := Num} = State) ->
    finish(ok, State);
msg_exchange(#{scnt := Num, num := Num} = State) ->
    %% We are done sending more requests - now we will just await 
    %% the replies for the (still) outstanding replies.
    msg_exchange( recv_reply(State) );
msg_exchange(#{outstanding     := Outstanding,
               max_outstanding := MaxOutstanding} = State)
  when (Outstanding < MaxOutstanding) ->
    msg_exchange( send_request(State) );
msg_exchange(State) ->
    send_request( recv_reply(State) ).


finish(ok,
       #{start := Start, bcnt  := BCnt, num := Num}) ->
    Stop = ?T(),
    throw(#{status  => ok,
	    runtime => ?TDIFF(Start, Stop),
            bcnt    => BCnt,
            cnt     => Num});
finish(Reason,
       #{start := Start,
	 sid  := SID, rid := RID,
	 scnt := SCnt, rcnt := RCnt, bcnt := BCnt,
	 num  := Num}) ->
    Stop = ?T(),
    throw(#{status  => Reason,
	    runtime => ?TDIFF(Start, Stop),
	    sid     => SID,
	    rid     => RID,
            scnt    => SCnt,
            rcnt    => RCnt,
            bcnt    => BCnt,
            num     => Num}).
    
send_request(#{mod             := Mod,
	       sock            := Sock, 
	       sid             := ID,
	       scnt            := Cnt,
	       outstanding     := Outstanding,
	       max_outstanding := MaxOutstanding,
	       msg_data        := Data} = State)
  when (MaxOutstanding > Outstanding) ->
    SZ   = size(Data),
    Req  = <<?TTEST_TAG:32,
	     ?TTEST_TYPE_REQUEST:32,
	     ID:32,
	     SZ:32,
	     Data/binary>>,
    case Mod:send(Sock, Req) of
        ok ->
            State#{sid         => next_id(ID),
		   scnt        => Cnt + 1,
		   outstanding => Outstanding + 1};
        {error, Reason} ->
            ?E("Failed sending request: ~p", [Reason]),
            exit({send, Reason})
    end;
send_request(State) ->
    State.



recv_reply(#{mod         := Mod,
	     sock        := Sock, 
             rid         := ID,
             active      := false,
             bcnt        := BCnt,
             rcnt        := Cnt,
	     outstanding := Outstanding} = State) ->
    case recv_reply_message1(Mod, Sock, ID) of
	{ok, MsgSz} ->
	    State#{rid         => next_id(ID),
		   bcnt        => BCnt + MsgSz,
		   rcnt        => Cnt + 1,
		   outstanding => Outstanding - 1};

	{error, timeout} ->
	    ?I("receive timeout"),
	    State;

	{error, Reason} ->
	    finish(Reason, State)
    end;
recv_reply(#{mod         := Mod,
	     sock        := Sock,
             rid         := ID,
             active      := Active,
             bcnt        := BCnt,
             scnt        := SCnt,
             rcnt        := RCnt,
	     outstanding := Outstanding,
	     acc         := Acc} = State) ->
    case recv_reply_message2(Mod, Sock, ID, Acc) of
	{ok, {MsgSz, NewAcc}} when is_integer(MsgSz) andalso is_binary(NewAcc) ->
	    maybe_activate(Mod, Sock, Active),
	    State#{rid         => next_id(ID),
		   bcnt        => BCnt + MsgSz,
		   rcnt        => RCnt + 1,
		   outstanding => Outstanding - 1,
		   acc         => NewAcc};

	ok ->
	    State;

	{error, stop} ->
	    ?I("receive [~w] -> stop", [Active]),
	    %% This will have the effect that no more requests are sent...
	    State#{num => SCnt, stop_started => ?T()};

	{error, timeout} ->
	    ?I("receive[~w] -> timeout", [Active]),
	    State;

	{error, Reason} ->
	    finish(Reason, State)
    end.


%% This function reads exactly one (reply) message. No more no less.	
recv_reply_message1(Mod, Sock, ID) ->
    case Mod:recv(Sock, 4*4, ?RECV_TIMEOUT) of
        {ok, <<?TTEST_TAG:32,
               ?TTEST_TYPE_REPLY:32,
               ID:32,
               SZ:32>> = Hdr} ->
            %% Receive the ping-pong reply boby
            case Mod:recv(Sock, SZ, ?RECV_TIMEOUT) of
                {ok, Data} when (size(Data) =:= SZ) ->
		    {ok, size(Hdr) + size(Data)};
                {error, Reason2} ->
		    ?E("Failed reading body: "
                       "~n   ~p: ~p", [Reason2]),
                    {error, {recv_body, Reason2}}
            end;

        {ok, <<BadTag:32,
               BadType:32,
               BadID:32,
               BadSZ:32>>} ->
            {error, {invalid_hdr, 
		     {?TTEST_TAG,        BadTag},
		     {?TTEST_TYPE_REPLY, BadType},
		     {ID,                BadID},
		     BadSZ}};
        {ok, _InvHdr} ->
            {error, invalid_hdr};

        {error, Reason1} ->
	    ?E("Feiled reading header: "
	      "~n   ~p", [Reason1]),
            {error, {recv_hdr, Reason1}}
    end.


%% This function first attempts to process the data we have already
%% accumulated. If that is not enough for a (complete) reply, it
%% will attempt to receive more.
recv_reply_message2(Mod, Sock, ID, Acc) ->
    case process_acc_data(ID, Acc) of
	ok ->
	    %% No or insufficient data, so get more
	    recv_reply_message3(Mod, Sock, ID, Acc);

	{ok, _} = OK -> % We already had a reply accumulated - no need to read more
	    OK;

	{error, _} = ERROR ->
	    ERROR
    end.

%% This function receives a "chunk" of data, then it tries to extract
%% one (reply) message from the accumulated and new data (combined).
recv_reply_message3(_Mod, Sock, ID, Acc) ->
    receive
	{timeout, _TRef, stop} ->
	    {error, stop};

        {TagClosed, Sock} when (TagClosed =:= tcp_closed) orelse
			       (TagClosed =:= socket_closed) ->
            {error, closed};

        {TagErr, Sock, Reason} when (TagErr =:= tcp_error) orelse
				    (TagErr =:= socket_error) ->
            {error, Reason};

        {Tag, Sock, Msg} when (Tag =:= tcp) orelse
			      (Tag =:= socket) ->
            process_acc_data(ID, <<Acc/binary, Msg/binary>>)
        
    after ?RECV_TIMEOUT ->
            ?I("timeout when"
               "~n   ID:        ~p"
               "~n   size(Acc): ~p",
               [ID, size(Acc)]),
            %% {error, timeout}
            recv_reply_message3(_Mod, Sock, ID, Acc)
    end.


process_acc_data(ID, <<?TTEST_TAG:32,
		       ?TTEST_TYPE_REPLY:32,
		       ID:32,
		       SZ:32,
		       Data/binary>>) when (SZ =< size(Data)) ->
    <<_Body:SZ/binary, Rest/binary>> = Data,
    {ok, {4*4+SZ, Rest}};
process_acc_data(ID, <<BadTag:32,
		       BadType:32,
		       BadID:32,
		       BadSZ:32,
		       _Data/binary>>) 
  when ((BadTag  =/= ?TTEST_TAG) orelse
	(BadType =/= ?TTEST_TYPE_REPLY) orelse
	(BadID   =/= ID)) ->
    {error, {invalid_hdr, 
	     {?TTEST_TAG,        BadTag},
	     {?TTEST_TYPE_REPLY, BadType},
	     {ID,                BadID},
	     BadSZ}};
%% Not enough for an entire (reply) message
process_acc_data(_ID, _Data) ->
    ok.

    
handle_message(#{quiet  := Quiet,
		 parent := Parent, sock := Sock, scnt := SCnt} = State) ->
    receive
	{timeout, _TRef, stop} ->
	    if not Quiet -> ?I("STOP");
	       true      -> ok
	    end,
	    %% This will have the effect that no more requests are sent...
	    State#{num => SCnt, stop_started => ?T()};

        {?MODULE, Ref, Parent, stop} ->
            %% This *aborts* the test
            reply(Parent, Ref, ok),
            exit(normal);
	
	%% Only when active
        {TagClosed, Sock, Reason} when (TagClosed =:= tcp_closed) orelse
				       (TagClosed =:= socket_closed) ->
            %% We should never get this (unless the server crashed)
            exit({closed, Reason});

	%% Only when active
        {TagErr, Sock, Reason} when (TagErr =:= tcp_error) orelse
				    (TagErr =:= socket_error) ->
            exit({error, Reason})
            
    after 0 ->
            State
    end.
        

initial_activation(_Mod, _Sock, false = _Active) ->
    ok;
initial_activation(Mod, Sock, Active) ->
    Mod:active(Sock, Active).


maybe_activate(Mod, Sock, once = Active) ->
    Mod:active(Sock, Active);
maybe_activate(_, _, _) ->
    ok.
                  

%% ==========================================================================

req(Pid, Req) ->
    Ref = make_ref(),
    Pid ! {?MODULE, Ref, Pid, Req},
    receive
        {'EXIT', Pid, Reason} ->
            {error, {exit, Reason}};
        {?MODULE, Ref, Reply} ->
            Reply
    end.

reply(Pid, Ref, Reply) ->
    Pid ! {?MODULE, Ref, Reply}.


%% ==========================================================================

next_id(ID) when (ID < ?MAX_ID) ->
    ID + 1;
next_id(_) ->
    1.


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

%% %% e(F) ->
%% %%     i("<ERROR> " ++ F).

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
