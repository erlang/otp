%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Transaction sender process
%%----------------------------------------------------------------------

-module(megaco_trans_sender).

-export([start_link/5, 
	 stop/1,
	 upgrade/2,
	 send_req/3,
	 send_reqs/3,
	 send_ack/2,
	 send_ack_now/2,
	 send_pending/2,
	 send_reply/2,
	 timeout/2,
	 ack_maxcount/2,
	 req_maxcount/2,
	 req_maxsize/2]).
-export([system_continue/3, system_terminate/4, system_code_change/4]).
-export([init/6]).


-include_lib("megaco/include/megaco.hrl").
-include("megaco_message_internal.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").


-record(state, 
	{
	  parent,
	  conn_handle,
	  timeout,
	  req_sz = 0,
	  req_maxsize,  %% Max total size of all accumulated reqs
	  req_maxcount,
	  ack_maxcount,
	  reqs = [],
	  acks = []
	 }).


%%%-----------------------------------------------------------------
%%% Public API
%%%-----------------------------------------------------------------
start_link(CH, To, MaxSzReqs, MaxNoReqs, MaxNoAcks) ->
    ?d("start_link -> entry with"
	"~n   CH:        ~p"
	"~n   To:        ~p"
	"~n   MaxSzReqs: ~p"
	"~n   MaxNoReqs: ~p"
	"~n   MaxNoAcks: ~p", [CH, To, MaxSzReqs, MaxNoReqs, MaxNoAcks]),
    Args = [self(), CH, To, MaxSzReqs, MaxNoReqs, MaxNoAcks],
    proc_lib:start_link(?MODULE, init, Args).

stop(Pid) when is_pid(Pid) ->
    Pid ! stop,
    ok.

upgrade(Pid, CH) when is_pid(Pid) ->
    Pid ! {upgrade, CH},
    ok.

send_req(Pid, Tid, Req) when is_pid(Pid) andalso is_binary(Req) ->
    Pid ! {send_req, Tid, Req},
    ok.

send_reqs(Pid, Tids, Reqs) 
  when is_pid(Pid) andalso 
       is_list(Tids) andalso 
       is_list(Reqs) andalso 
       (length(Tids) =:= length(Reqs)) ->
    Pid ! {send_reqs, Tids, Reqs},
    ok.

send_ack(Pid, Serial) when is_pid(Pid) andalso is_integer(Serial) ->
    Pid ! {send_ack, Serial},
    ok.

send_ack_now(Pid, Serial) when is_pid(Pid) andalso is_integer(Serial) ->
    Pid ! {send_ack_now, Serial},
    ok.

send_pending(Pid, Serial) when is_pid(Pid) andalso is_integer(Serial) ->
    Pid ! {send_pending, Serial},
    ok.

send_reply(Pid, Reply) when is_pid(Pid) andalso is_binary(Reply) ->
    Pid ! {send_reply, Reply}.

ack_maxcount(Pid, Max) when is_pid(Pid) andalso is_integer(Max) ->
    Pid ! {ack_maxcount, Max},
    ok.

req_maxcount(Pid, Max) when is_pid(Pid) andalso is_integer(Max) ->
    Pid ! {req_maxcount, Max},
    ok.

req_maxsize(Pid, Max) when is_pid(Pid) andalso is_integer(Max) ->
    Pid ! {req_maxsize, Max},
    ok.

timeout(Pid, Timeout) when is_pid(Pid) ->
    Pid ! {timeout, Timeout},
    ok.



%%%-----------------------------------------------------------------
%%% Internal exports
%%%-----------------------------------------------------------------

init(Parent, CH, To, MaxSzReqs, MaxNoReqs, MaxNoAcks) ->
    ?d("init -> entry with"
	"~n   Parent:    ~p"
	"~n   CH:        ~p"
	"~n   To:        ~p"
	"~n   MaxSzReqs: ~p"
	"~n   MaxNoReqs: ~p"
	"~n   MaxNoAcks: ~p", [Parent, CH, To, MaxSzReqs, MaxNoReqs, MaxNoAcks]),
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    S = #state{parent       = Parent, 
	       conn_handle  = CH, 
	       timeout      = To, 
	       req_maxsize  = MaxSzReqs, 
	       req_maxcount = MaxNoReqs, 
	       ack_maxcount = MaxNoAcks},
    loop(S, To).


%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
%% idle (= empty)
loop(#state{reqs = [], acks = [], timeout = Timeout} = S, _) ->
    receive 
	{send_ack, Serial} ->
	    ?d("loop(empty) -> received send_ack [~w] request", [Serial]),
	    loop(S#state{acks = [Serial]}, Timeout);

	{send_ack_now, Serial} ->
	    ?d("loop(empty) -> received send_ack_now [~w] request", [Serial]),
	    send_msg(S#state.conn_handle, [], [Serial]),
	    loop(S, Timeout);

	{send_req, Tid, Req} when size(Req) >= S#state.req_maxsize ->
	    ?d("loop(empty) -> received (big) send_req request ~w", [Tid]),
	    send_msg(S#state.conn_handle, [{Tid, Req}], []),
	    loop(S, Timeout);

	{send_req, Tid, Req} ->
	    ?d("loop(empty) -> received send_req request ~w", [Tid]),
	    loop(S#state{req_sz = size(Req), reqs = [{Tid,Req}]}, Timeout);

	{send_reqs, Tids, Reqs} ->
	    ?d("loop(empty) -> received send_reqs request: ~w", [Tids]),
	    {NewS, _} = handle_send_reqs(Tids, Reqs, S),
	    loop(NewS, Timeout);

	{send_pending, Serial} ->
	    ?d("loop(empty) -> received send_pending [~w] request", [Serial]),
	    handle_send_result( 
	      send_pending(S#state.conn_handle, Serial, [], []) 
	     ),
	    loop(S, Timeout);

	{send_reply, Reply} ->
	    ?d("loop(empty) -> received send_reply request", []),
	    #state{conn_handle = CH, req_maxsize = MaxSz} = S,
	    handle_send_result( send_reply(CH, Reply, MaxSz, 0, [], []) ),
	    loop(S, Timeout);

	{upgrade, CH} ->
	    ?d("loop(empty) -> received upgrade request:"
		"~n   CH: ~p", [CH]),
	    loop(S#state{conn_handle = CH}, Timeout);

	{ack_maxcount, NewMax} ->
	    ?d("loop(empty) -> received ack_maxcount request", []),
	    loop(S#state{ack_maxcount = NewMax}, Timeout);

	{req_maxcount, NewMax} ->
	    ?d("loop(empty) -> received req_maxcount request", []),
	    loop(S#state{req_maxcount = NewMax}, Timeout);

	{req_maxsize, NewMax} ->
	    ?d("loop(empty) -> received req_maxsize request", []),
	    loop(S#state{req_maxsize = NewMax}, Timeout);

	{timeout, NewTimeout} ->
	    ?d("loop(empty) -> received timeout request", []),
	    loop(S#state{timeout = NewTimeout}, NewTimeout);

	stop ->
	    ?d("loop(empty) -> received stop request", []),
	    exit(normal);

	{system, From, Msg} ->
	    ?d("loop(empty) -> received system message:"
		"~n   From: ~p"
		"~n   Msg:  ~p", [From, Msg]),
	    Parent = S#state.parent, 
	    sys:handle_system_msg(Msg, From, Parent, 
				  ?MODULE, [], {S, Timeout});
	
	{'EXIT', Parent, Reason} when S#state.parent == Parent ->
	    ?d("loop(empty) -> received upgrade request", []),
	    exit(Reason);

	M ->
	    warning_msg("received unexpected message (ignoring): "
			"~n~p", [M]),
	    loop(S, Timeout)

    end;

%% active (= some acks or reqs waiting to to be sent)
loop(#state{reqs = Reqs, acks = Acks, ack_maxcount = MaxAcks, 
	    timeout = Timeout} = S, To) 
  when To >= 0 ->
    Start = t(),
    receive
	{send_ack, Serial} when length(Acks) + 1 >= MaxAcks ->
	    ?d("loop(active,~w) -> "
		"received [~w] send_ack [~w] request", 
	    [To, length(Acks), Serial]),
	    handle_send_result( 
	      send_msg(S#state.conn_handle, Reqs, [Serial|Acks])
	     ),
	    loop(S#state{req_sz = 0, reqs = [], acks = []}, Timeout);

	{send_ack, Serial} ->
	    ?d("loop(active,~w) -> received send_ack [~w] request", 
		[To, Serial]),
	    loop(S#state{acks = [Serial|Acks]}, to(To, Start));

	{send_ack_now, Serial} ->
	    ?d("loop(active,~w) -> [~w,~w] "
		"received send_ack_now [~w] request", 
		[To, length(Reqs), length(Acks), Serial]),
	    handle_send_result( 
	      send_msg(S#state.conn_handle, Reqs, [Serial|Acks])
	     ),
	    loop(S#state{req_sz = 0, reqs = [], acks = []}, Timeout);

	%% We need to check that this is not a resend!!
	%% In that case, send whatever we have in store
	{send_req, Tid, Req} ->
	    ?d("loop(active,~w) -> received send_req request ~w", [To,Tid]),
	    {NewS, NewT} = 
		case handle_send_req(Tid, Req, S) of
		    {S1, true} ->
			{S1, Timeout};
		    {S1, false} ->
			{S1, to(To, Start)}
		end,
	    loop(NewS, NewT);

	{send_reqs, Tids, NewReqs} ->
	    ?d("loop(active,~w) -> received send_reqs request ~w", [To,Tids]),
	    {NewS, NewT} = 
		case handle_send_reqs(Tids, NewReqs, S) of
		    {S1, true} ->
			{S1, Timeout};
		    {S1, false} ->
			{S1, to(To, Start)}
		end,
	    loop(NewS, NewT);

	{send_pending, Serial} ->
	    ?d("loop(active,~w) -> received send_pending [~w] request", 
		[To, Serial]),
	    handle_send_result( 
	      send_pending(S#state.conn_handle, Serial, Reqs, Acks)
	     ),
	    loop(S#state{req_sz = 0, reqs = [], acks = []}, Timeout);

	{send_reply, Reply} ->
	    ?d("loop(active,~w) -> received send_reply request", [To]),
	    #state{conn_handle = CH, req_maxsize = MaxSz, req_sz = ReqSz} = S,
	    handle_send_result( 
	      send_reply(CH, Reply, MaxSz, ReqSz, Reqs, Acks)
	     ),
	    loop(S#state{req_sz = 0, reqs = [], acks = []}, Timeout);

	{upgrade, CH} ->
	    ?d("loop(active,~w) -> received upgrade request", [To]),
	    loop(S#state{conn_handle = CH}, to(To, Start));

	{req_maxsize, NewMax} ->
	    ?d("loop(active,~w) -> received req_maxsize request", [To]),
	    loop(S#state{req_maxsize = NewMax}, to(To, Start));

	{req_maxcount, NewMax} ->
	    ?d("loop(active,~w) -> received req_maxcount request", [To]),
	    loop(S#state{req_maxcount = NewMax}, to(To, Start));

	{ack_maxcount, NewMax} ->
	    ?d("loop(active,~w) -> received ack_maxcount request", [To]),
	    loop(S#state{ack_maxcount = NewMax}, to(To, Start));

	{timeout, NewTimeout} when NewTimeout > Timeout ->
	    ?d("loop(active,~w) -> received timeout request: ~w", 
		[To, NewTimeout]),
	    %% We need to recalculate To
	    NewTo = NewTimeout - (Timeout - to(To, Start)),
	    loop(S#state{timeout = NewTimeout}, NewTo);

	{timeout, NewTimeout} ->
	    ?d("loop(active,~w) -> received timeout request: ~w", 
		[To, NewTimeout]),
	    %% We need to recalculate To
	    NewTo = to(To, Start) - (Timeout - NewTimeout),
	    loop(S#state{timeout = NewTimeout}, NewTo);

	stop ->
	    ?d("loop(active,~w) -> received stop request", [To]),
	    handle_send_result( send_msg(S#state.conn_handle, Reqs, Acks) ),
	    exit(normal);

	{system, From, Msg} ->
	    ?d("loop(active,~w) -> received system message:"
		"~n   From: ~p"
		"~n   Msg:  ~p", [To, From, Msg]),
	    Parent = S#state.parent, 
	    sys:handle_system_msg(Msg, From, Parent, 
				  ?MODULE, [], {S, to(To, Start)});

	{'EXIT', Parent, Reason} when S#state.parent == Parent ->
	    ?d("loop(active,~w) -> received exit request", [To]),
	    exit(Reason);

	M ->
	    warning_msg("received unexpected message (ignoring): "
			"~n~p", [M]),
	    loop(S, to(To, Start))

    after To ->
	    ?d("loop(active,~w) -> timeout - time to send", [To]),
	    handle_send_result( send_msg(S#state.conn_handle, Reqs, Acks) ),
	    loop(S#state{req_sz = 0, reqs = [], acks = []}, Timeout)
    end;

loop(#state{reqs = Reqs, acks = Acks, timeout = Timeout} = S, _To) ->
    ?d("loop(active) -> timeout [~w, ~w]", [length(Reqs),length(Acks)]),
    handle_send_result( send_msg(S#state.conn_handle, Reqs, Acks) ),
    loop(S#state{req_sz = 0, reqs = [], acks = []}, Timeout).


%%%-----------------------------------------------------------------

%% The request is itself larger then the max size, so first send 
%% everything we have stored in one message, and then the new request
%% in another. 
%% Note that it does not matter if we with this request 
%% passed the maxcount limit.
%% Note that this message cannot be a re-sent, since 
%% such a request would have been stored, but sent immediatly.
handle_send_req(Tid, Req, 
		#state{conn_handle = CH, 
		       req_maxsize = MaxSz, reqs = Reqs, acks = Acks} = S) 
  when size(Req) >= MaxSz ->
    ?d("handle_send_req -> request bigger then maxsize ~w", [MaxSz]),
    handle_send_result( send_msg(CH, Reqs, Acks) ),
    handle_send_result( send_msg(CH, [{Tid, Req}], []) ),
    {S#state{req_sz = 0, reqs = [], acks = []}, true};

%% And handle all the other cases
handle_send_req(Tid, Req, 
		#state{conn_handle  = CH, req_sz = ReqSz, 
		       req_maxcount = MaxReqs, req_maxsize = MaxSz, 
		       reqs = Reqs, acks = Acks} = S) ->
    case lists:keymember(Tid, 1, Reqs) of
	true ->
	    %% A re-send, time to send whatever we have in the store
	    ?d("handle_send_req -> was a re-send, so flush",[]),
	    handle_send_result( send_msg(CH, Reqs, Acks) ),
	    {S#state{req_sz = 0, reqs = [], acks = []}, true};

	false when length(Reqs) + 1 >= MaxReqs ->
	    %% We finally passed the req-maxcount limit
	    ?d("handle_send_req -> maxcount ~w passed", [MaxReqs]),
	    handle_send_result( 
	      send_msg(S#state.conn_handle, [{Tid, Req}|Reqs], Acks)
	     ),
	    {S#state{req_sz = 0, reqs = [], acks = []}, true};

	false when size(Req) + ReqSz >= MaxSz ->
	    %% We finally passed the req-maxsize limit
	    ?d("handle_send_req -> maxsize ~w passed", [MaxSz]),
	    handle_send_result( 
	      send_msg(S#state.conn_handle, [{Tid, Req}|Reqs], Acks)
	     ),
	    {S#state{req_sz = 0, reqs = [], acks = []}, true};

	false ->
	    %% Still not time to send
	    ?d("handle_send_req -> nothing to be sent",[]),
	    {S#state{req_sz = ReqSz + size(Req), reqs = [{Tid, Req}|Reqs]}, 
	     false}
    end.
	    

%% We passed the req-maxcount limit: Time to send, atleast some of 
%% the stuff...
handle_send_reqs(Tids, Reqs0, 
		 #state{conn_handle = CH,
			req_maxsize = MaxSz, req_sz = ReqSz, 
			req_maxcount = MaxReqs, reqs = Reqs, acks = Acks} = S) 
  when length(Reqs0) + length(Reqs) >= MaxReqs ->
    ?d("handle_send_reqs -> maxcount ~w: ~w, ~w", 
	[MaxSz,length(Reqs0),length(Reqs)]),
    Reqs1 = merge_tids_and_reqs(Tids, Reqs0, []),
    {NewReqs, NewReqSz} = send_reqs(CH, Reqs1, Acks, Reqs, ReqSz, MaxSz),
    ?d("handle_send_reqs -> sent:"
	"~n   NewReqSz:        ~w"
	"~n   length(NewReqs): ~w", [NewReqSz, length(NewReqs)]),
    {S#state{req_sz = NewReqSz, reqs = NewReqs, acks = []}, true};

%% We did not pass the req-maxcount limit, but we could have passed the 
%% req-maxsize limit, so maybe send...
handle_send_reqs(Tids, Reqs0, #state{conn_handle = CH,
				     req_maxsize = MaxSz, req_sz = ReqSz, 
				     reqs = Reqs, acks = Acks} = S) ->
    ?d("handle_send_reqs -> not maxcount - maybe maxsize (~w)", [MaxSz]),
    Reqs1 = merge_tids_and_reqs(Tids, Reqs0, []),
    
    case maybe_send_reqs(CH, Reqs1, Acks, Reqs, ReqSz, MaxSz, false) of
	{NewReqs, NewReqSz, true} ->
	    ?d("handle_send_reqs -> sent:"
		"~n   NewReqSz:        ~w"
		"~n   length(NewReqs): ~w", [NewReqSz, length(NewReqs)]),
	    {S#state{req_sz = NewReqSz, reqs = NewReqs, acks = []}, true};
	{NewReqs, NewReqSz, false} ->
	    ?d("handle_send_reqs -> not sent:"
		"~n   NewReqSz:        ~w"
		"~n   length(NewReqs): ~w", [NewReqSz, length(NewReqs)]),
	    {S#state{req_sz = NewReqSz, reqs = NewReqs}, false}
    end.
    
merge_tids_and_reqs([], [], Reqs) ->
    Reqs;
merge_tids_and_reqs([Tid|Tids], [Req|Reqs], Acc) ->
    merge_tids_and_reqs(Tids, Reqs, [{Tid,Req}|Acc]).

%% We know that we shall send, so if maybe_send_reqs does not,
%% we send it our self...
send_reqs(CH, Reqs, Acks, Acc, AccSz, MaxSz) ->
    ?d("send_reqs -> entry when"
	"~n   length(Reqs): ~w"
	"~n   Acks:         ~w"
	"~n   length(Acc):  ~w"
	"~n   AccSz:        ~w", [length(Reqs), Acks, length(Acc), AccSz]),
    case maybe_send_reqs(CH, Reqs, Acks, Acc, AccSz, MaxSz, false) of
	{NewReqs, _NewReqSz, false} ->
	    ?d("send_reqs -> nothing sent yet"
		"~n   length(NewReqs): ~w", [length(NewReqs)]),
	    handle_send_result( send_msg(CH, NewReqs, Acks) ),
	    {[], 0};
	{NewReqs, NewReqSz, true} ->
	    ?d("send_reqs -> something sent"
		"~n   length(NewReqs): ~w"
		"~n   NewReqSz:        ~w", [length(NewReqs), NewReqSz]),
	    {NewReqs, NewReqSz}
    end.


maybe_send_reqs(_CH, [], _Acks, Acc, AccSz, _MaxSz, Sent) ->
    ?d("maybe_send_reqs -> done when"
	"~n   Sent:        ~w"
	"~n   AccSz:       ~w"
	"~n   length(Acc): ~w", [Sent, AccSz, length(Acc)]),
    {Acc, AccSz, Sent};
maybe_send_reqs(CH, [{Tid, Req}|Reqs], Acks, Acc, _AccSz, MaxSz, _Sent) 
  when size(Req) >= MaxSz ->
    %% The request was above the maxsize limit, so first send 
    %% what's in store and the the big request.
    ?d("maybe_send_reqs -> entry when request [~w] size (~w) > max size"
	"~n   Acks:        ~w"
	"~n   length(Acc): ~w", [Tid, size(Req), Acks, length(Acc)]),
    handle_send_result( send_msg(CH, Acc, Acks) ),
    handle_send_result( send_msg(CH, [{Tid, Req}], []) ),
    maybe_send_reqs(CH, Reqs, [], [], 0, MaxSz, true);
maybe_send_reqs(CH, [{Tid, Req}|Reqs], Acks, Acc, AccSz, MaxSz, _Sent) 
  when AccSz + size(Req) >= MaxSz ->
    %% We _did_ pass the maxsize limit with this request, so send 
    ?d("maybe_send_reqs -> entry when sum of requests (~w) > max size"
	"~n   Tid:         ~w"
	"~n   Acks:        ~w"
	"~n   length(Acc): ~w", [Tid, size(Req) + AccSz, Acks, length(Acc)]),
    handle_send_result( send_msg(CH, [{Tid, Req}|Acc], Acks) ),
    maybe_send_reqs(CH, Reqs, [], [], 0, MaxSz, true);
maybe_send_reqs(CH, [{Tid, Req}|Reqs], Acks, Acc, AccSz, MaxSz, Sent) ->
    ?d("maybe_send_reqs -> entry when"
	"~n   Tid:         ~w"
	"~n   size(Req):   ~w"
	"~n   Acks:        ~w"
	"~n   length(Acc): ~w"
	"~n   AccSz:       ~w", [Tid, size(Req), Acks, length(Acc), AccSz]),
    NewAcc   = [{Tid,Req}|Acc], 
    NewAccSz = AccSz + size(Req), 
    maybe_send_reqs(CH, Reqs, Acks, NewAcc, NewAccSz, MaxSz, Sent).

    
%%%-----------------------------------------------------------------
    
send_pending(CH, Serial, Reqs, Acks) ->
    ?d("send_pending -> entry with"
	"~n   Serial:       ~w"
	"~n   length(Reqs): ~w"
	"~n   length(Acks): ~w", [Serial, length(Reqs), length(Acks)]),
    case megaco_config:lookup_local_conn(CH) of
	[CD] ->
	    TP = #'TransactionPending'{transactionId = Serial},
	    Pend = {transactionPending, TP},
	    do_send_msg(CD, Pend, lists:reverse(Reqs), Acks);
	[] ->
	    ok
    end.


%% We need to check the size of the reply. If the reply itself is 
%% larger then the max limit, then it is sent in a separate message.
send_reply(CH, Reply, MaxSz, _ReqSz, Reqs, Acks) ->
    ?d("send_reply -> entry with"
	"~n   length(Reqs): ~w"
	"~n   length(Acks): ~w", [length(Reqs), length(Acks)]),
    case megaco_config:lookup_local_conn(CH) of
	[CD] when size(Reply) > MaxSz ->
	    handle_send_result( send_msg(CD, lists:reverse(Reqs), Acks) ),
	    Rep = {transactionReply, Reply},
	    do_send_msg(CD, Rep, [], []);
	[CD] ->
	    Rep = {transactionReply, Reply},
	    do_send_msg(CD, Rep, lists:reverse(Reqs), Acks);
	[] ->
	    ok
    end.

do_send_msg(CD, Trans, [], []) ->
    Body   = {transactions, [Trans]},
    Slogan = "send trans reply/pending", 
    ?d("do_send_msg -> ~s", [Slogan]),
    megaco_messenger_misc:send_body(CD, Slogan, Body);
do_send_msg(CD, Trans, Reqs0, []) ->
    Reqs   = [{transactionRequest, Req} || {_, Req} <- Reqs0],
    Body   = {transactions, [Trans|Reqs]},
    Slogan = "send trans reply/pending and reqs", 
    ?d("do_send_msg -> ~s", [Slogan]),
    megaco_messenger_misc:send_body(CD, Slogan, Body);
do_send_msg(CD, Trans, [], SerialRanges) ->
    Acks   = make_acks(ranges(SerialRanges), []),
    Body   = {transactions, [Trans, {transactionResponseAck, Acks}]},
    Slogan = "send trans reply/pending and acks", 
    ?d("do_send_msg -> ~s", [Slogan]),
    megaco_messenger_misc:send_body(CD, Slogan, Body);
do_send_msg(CD, Trans, Reqs0, SerialRanges) ->
    Acks   = make_acks(ranges(SerialRanges), []),
    Reqs   = [{transactionRequest, Req} || {_, Req} <- Reqs0],
    Body   = {transactions, [Trans, {transactionResponseAck, Acks}|Reqs]},
    Slogan = "send trans reply/pending, reqs and acks", 
    ?d("do_send_msg -> ~s", [Slogan]),
    megaco_messenger_misc:send_body(CD, Slogan, Body).



send_msg(_, [], []) ->
    ok;
send_msg(CH, Reqs, Serials) ->
    case megaco_config:lookup_local_conn(CH) of
	[ConnData] ->
	    do_send_msg(ConnData, lists:reverse(Reqs), Serials);
	[] ->
	    ok
    end.


do_send_msg(CD, Reqs0, []) ->
    ?d("do_send_msg -> entry with"
	"~n   length(Reqs0): ~p", [length(Reqs0)]),    
    Reqs = [{transactionRequest, Req} || {_, Req} <- Reqs0],
    %% ?d("do_send_msg -> Reqs: ~n~p", [Reqs]),
    Body = {transactions, Reqs},
    megaco_messenger_misc:send_body(CD, "send trans reqs", Body);
do_send_msg(CD, [], SerialRanges) ->
    ?d("do_send_msg -> entry with"
 	"~n   SerialRanges: ~p", [SerialRanges]),    
    Acks = make_acks(ranges(SerialRanges), []),
    %% ?d("do_send_msg -> Reqs: ~n~p", [Reqs]),
    Body = {transactions, [{transactionResponseAck, Acks}]},
    megaco_messenger_misc:send_body(CD, "send trans acks", Body);
do_send_msg(CD, Reqs0, SerialRanges) ->
    ?d("do_send_msg -> entry with"
	"~n   length(Reqs0): ~p"
 	"~n   SerialRanges:  ~p", [length(Reqs0), SerialRanges]),    
    Acks = make_acks(ranges(SerialRanges), []),
    Reqs = [{transactionRequest, Req} || {_, Req} <- Reqs0],
    %% ?d("do_send_msg -> Reqs: ~n~p", [Reqs]),
    Body = {transactions, [{transactionResponseAck, Acks}|Reqs]},
    megaco_messenger_misc:send_body(CD, "send trans reqs and acks", Body).


handle_send_result(ok) ->
    ok;
handle_send_result({ok, _}) ->
    ok;
handle_send_result({error, {send_message_cancelled, _Reason}}) ->
    ok;
handle_send_result({error, {send_message_failed, Reason}}) ->
    error_msg("Failed sending message: ~n   ~p", [Reason]),
    error;
handle_send_result(Error) ->
    error_msg("Failed sending message: ~n   ~p", [Error]),
    error.


ranges(L) ->
    lists:reverse(ranges(lists:sort(L), [], [])).

ranges([], Range, Ranges) ->
    ranges2(Range, Ranges);
ranges([S1|Sn], [S2|_] = Range, Ranges) when S1 == (S2+1) ->
    ranges(Sn, [S1|Range], Ranges);
ranges([S|Sn], Range, Ranges) ->
    ranges(Sn, [S], ranges2(Range, Ranges)).

ranges2([], Ranges) -> 
    Ranges;
ranges2([S], Ranges) ->
    [{S,S}|Ranges];
ranges2(Range0, Ranges) ->
    Range = lists:reverse(Range0),
    [{hd(Range),lists:last(Range)}|Ranges].


make_acks([], Acks) ->
    lists:reverse(Acks);
make_acks([{S,S}|SerialRanges], Acks) ->
    TRA = #'TransactionAck'{firstAck = S},
    make_acks(SerialRanges, [TRA|Acks]);
make_acks([{F,L}|SerialRanges], Acks) ->
    TRA = #'TransactionAck'{firstAck = F, lastAck = L},
    make_acks(SerialRanges, [TRA|Acks]).



%%%-----------------------------------------------------------------

to(To, Start) ->
    To - (t() - Start).

%% Time in milli seconds
t() ->
    erlang:monotonic_time(milli_seconds).

warning_msg(F, A) ->
    ?megaco_warning("Transaction sender: " ++ F, A).
 
error_msg(F, A) ->
    ?megaco_error("Transaction sender: " ++ F, A).
 

%%%-----------------------------------------------------------------
%%% System messages handled here
%%%-----------------------------------------------------------------

system_continue(_Parent, _Dbg, {S,To}) ->
    loop(S, To).

system_terminate(Reason, _Parent, _Dbg, {S, _}) ->
    #state{conn_handle = CH, reqs = Reqs, acks = Acks} = S,
    send_msg(CH, Reqs, Acks),
    exit(Reason).

system_code_change(S, _Module, _OLdVsn, _Extra) ->
    ?d("system_code_change -> entry", []),
    {ok, S}.
