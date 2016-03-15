%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% Purpose: Handle configuration of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_config).

-behaviour(gen_server).

%% Application internal exports
-export([
         start_link/0,
         stop/0,

         start_user/2,
         stop_user/1,

         user_info/2,
         update_user_info/3,
         conn_info/2,
         update_conn_info/3,
         system_info/1,

         %% incr_counter/2,
         incr_trans_id_counter/1,
         incr_trans_id_counter/2,

	 %% Verification functions
         verify_val/2,
%% 	 verify_strict_uint/1,
%% 	 verify_strict_int/1, verify_strict_int/2, 
%% 	 verify_uint/1,
%% 	 verify_int/1, verify_int/2,
         

	 %% Reply limit counter
	 cre_reply_counter/2,
	 get_reply_counter/2,
	 incr_reply_counter/2,
	 del_reply_counter/2,

	 %% Pending limit counter
	 cre_pending_counter/3,
	 get_pending_counter/2,
	 incr_pending_counter/2,
	 del_pending_counter/2,
	 %% Backward compatibillity functions (to be removed in later versions)
	 cre_pending_counter/1,  
	 get_pending_counter/1,  
	 incr_pending_counter/1, 
	 del_pending_counter/1,  

         lookup_local_conn/1,
         connect/4, finish_connect/4,
         autoconnect/4,
         disconnect/1,
	 connect_remote/3,
	 disconnect_remote/2,
	 init_conn_data/4, 

	 trans_sender_exit/2

        ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {parent_pid}).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").


-ifdef(MEGACO_TEST_CODE).
-define(megaco_test_init(),
	(catch ets:new(megaco_test_data, [set, public, named_table]))).
-else.
-define(megaco_test_init(),
	ok).
-endif.

-define(TID_CNT(LMID), {LMID, trans_id_counter}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->
    ?d("start_link -> entry", []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

stop() ->
    ?d("stop -> entry", []),
    call({stop, self()}).

start_user(UserMid, Config) ->
    call({start_user, UserMid, Config}).

stop_user(UserMid) ->
    call({stop_user, UserMid}).

user_info(UserMid, all) ->
    All0 = ets:match_object(megaco_config, {{UserMid, '_'}, '_'}),
    All1 = [{Item, Val} || {{_, Item}, Val} <- All0, Item /= trans_sender],
    case lists:keysearch(trans_id_counter, 1, All1) of
	{value, {_, Val}} ->
	    lists:keyreplace(trans_id_counter, 1, All1, {trans_id, Val});
	false when UserMid /= default ->
	    [{trans_id, undefined_serial}|All1];
	false ->
	    All1
    end;
user_info(UserMid, receive_handle) ->
    case call({receive_handle, UserMid}) of
	{ok, RH} ->
	    RH;
	{error, Reason} ->
	    exit(Reason)
    end;
user_info(UserMid, conn_data) ->
    HandlePat = #megaco_conn_handle{local_mid = UserMid, remote_mid = '_'},
    Pat = #conn_data{conn_handle      	  = HandlePat,
                     serial           	  = '_',
                     max_serial       	  = '_',
                     request_timer    	  = '_',
                     long_request_timer   = '_',

                     auto_ack         	  = '_',

                     trans_ack   	  = '_',
                     trans_ack_maxcount	  = '_',

                     trans_req   	  = '_',
                     trans_req_maxcount	  = '_',
                     trans_req_maxsize	  = '_',

                     trans_timer   	  = '_',
                     trans_sender         = '_',

                     pending_timer     	  = '_',
                     sent_pending_limit   = '_',
                     recv_pending_limit   = '_',
                     reply_timer      	  = '_',
                     control_pid      	  = '_',
                     monitor_ref      	  = '_',
                     send_mod         	  = '_',
                     send_handle      	  = '_',
                     encoding_mod     	  = '_',
                     encoding_config  	  = '_',
                     protocol_version  	  = '_',
                     auth_data         	  = '_',
                     user_mod         	  = '_',
                     user_args         	  = '_',
                     reply_action     	  = '_',
                     reply_data       	  = '_',
		     threaded       	  = '_',
		     strict_version   	  = '_',
		     long_request_resend  = '_',
		     call_proxy_gc_timeout = '_',
		     cancel               = '_',
		     resend_indication    = '_',
		     segment_reply_ind 	  = '_',
		     segment_recv_acc 	  = '_',
		     segment_recv_timer	  = '_',
		     segment_send   	  = '_',
		     segment_send_timer	  = '_',
		     max_pdu_size   	  = '_',
		     request_keep_alive_timeout = '_'
		    },
    %% ok = io:format("PATTERN: ~p~n", [Pat]),
    ets:match_object(megaco_local_conn, Pat);
user_info(UserMid, connections) ->
    [C#conn_data.conn_handle || C <- user_info(UserMid, conn_data)];
user_info(UserMid, mid) ->
    ets:lookup_element(megaco_config, {UserMid, mid}, 2);
user_info(UserMid, orig_pending_limit) ->
    user_info(UserMid, sent_pending_limit);
user_info(UserMid, trans_id) ->
    case (catch user_info(UserMid, trans_id_counter)) of
	{'EXIT', _} ->
	    %% There is only two cases where this can occure:
	    %% 1) The user does not exist
	    %% 2) Called before the first message is sent, use 
	    %%    undefined_serial, since there is no 
	    %%    "current transaction id"
	    case (catch user_info(UserMid, mid)) of
		{'EXIT', _} ->
		    %% case 1:
		    exit({no_such_user, UserMid});
		_ ->
		    undefined_serial
	    end;
	Else ->
	    Else
    end;
user_info(UserMid, Item) ->
    ets:lookup_element(megaco_config, {UserMid, Item}, 2).

update_user_info(UserMid, orig_pending_limit, Val) ->
    update_user_info(UserMid, sent_pending_limit, Val);
update_user_info(UserMid, Item, Val) ->
    call({update_user_info, UserMid, Item, Val}).


conn_info(Data, Item) ->
    %% The purpose of this is a compiler optimization...
    %% Args are processed from left to right.
    do_conn_info(Item, Data).

do_conn_info(mid = _Item, #megaco_conn_handle{local_mid = Mid}) ->
    Mid;
do_conn_info(local_mid = _Item, #megaco_conn_handle{local_mid = LMid}) ->
    LMid;
do_conn_info(remote_mid = _Item, #megaco_conn_handle{remote_mid = RMid}) ->
    RMid;
do_conn_info(conn_handle = _Item, CH) when is_record(CH, megaco_conn_handle) ->
    CH;
do_conn_info(conn_data = _Item, CH) when is_record(CH, megaco_conn_handle) ->
    case lookup_local_conn(CH) of
	[] ->
	    exit({no_such_connection, CH});
	[ConnData] ->
	    ConnData
    end;
do_conn_info(Item, CH) when is_record(CH, megaco_conn_handle) ->
    case lookup_local_conn(CH) of
	[] ->
	    exit({no_such_connection, CH});
	[ConnData] ->
	    do_conn_info(Item, ConnData)
    end;

do_conn_info(cancel = _Item, #conn_data{conn_handle = CH}) ->
    %% To minimise raise-condition propabillity,
    %% we always look in the table instead of
    %% in the record for this one
    ets:lookup_element(megaco_local_conn, CH, #conn_data.cancel);
do_conn_info(cancel = _Item, CH) when is_record(CH, megaco_conn_handle) ->
    %% To minimise raise-condition propabillity,
    %% we always look in the table instead of
    %% in the record for this one
    ets:lookup_element(megaco_local_conn, CH, #conn_data.cancel);

do_conn_info(all = _Item, 
	     #conn_data{conn_handle                = CH,
			serial                     = TransId,
			max_serial                 = MaxTransId,
			request_timer              = ReqTmr,
			long_request_timer         = LongReqTmr,
			auto_ack                   = AutoAck,
			trans_ack                  = TransAck,
			trans_ack_maxcount         = TransAckMaxCount,
			trans_req                  = TransReq, 
			trans_req_maxcount         = TransReqMaxCount, 
			trans_req_maxsize          = TransReqMaxSz, 
			trans_timer                = TransTmr, 
			%% trans_sender,   
			pending_timer              = PendingTmr,
			sent_pending_limit         = SentPendingLimit, 
			recv_pending_limit         = RecvPendingLimit, 
			reply_timer                = ReplyTmr,
			control_pid                = CtrlPid,
			monitor_ref                = MonRef,
			send_mod                   = SendMod,
			send_handle                = SendHandle,
			encoding_mod               = EncodingMod,
			encoding_config            = EncodingConf,
			protocol_version           = ProtoVersion,
			auth_data                  = AuthData,
			user_mod                   = UserMod,
			user_args                  = UserArgs,
			reply_action               = ReplyAction, 
			reply_data                 = ReplyData, 
			threaded                   = Threaded, 
			strict_version             = StrictVersion, 
			long_request_resend        = LongReqResend, 
			call_proxy_gc_timeout      = CallProxyGCTimeout, 
			%% cancel, 
			resend_indication          = ResendInd, 
			segment_reply_ind          = SegReplyInd, 
			segment_recv_acc           = SegRecvAcc, 
			segment_recv_timer         = SegRecvTmr, 
			segment_send               = SegSend, 
			segment_send_timer         = SegSendTmr, 
			max_pdu_size               = MaxPduSz, 
			request_keep_alive_timeout = RequestKeepAliveTmr}) ->
    [{conn_handle,                CH}, 
     {trans_id,                   TransId}, 
     {max_trans_id,               MaxTransId},
     {request_timer,              ReqTmr},
     {long_request_timer,         LongReqTmr},
     {mid,                        CH#megaco_conn_handle.local_mid},
     {local_mid,                  CH#megaco_conn_handle.local_mid},
     {remote_mid,                 CH#megaco_conn_handle.remote_mid},
     {auto_ack,                   AutoAck},
     {trans_ack,                  TransAck},
     {trans_ack_maxcount,         TransAckMaxCount},
     {trans_req,                  TransReq}, 
     {trans_req_maxcount,         TransReqMaxCount}, 
     {trans_req_maxsize,          TransReqMaxSz}, 
     {trans_timer,                TransTmr}, 
     {pending_timer,              PendingTmr},
     {sent_pending_limit,         SentPendingLimit}, 
     {recv_pending_limit,         RecvPendingLimit}, 
     {reply_timer,                ReplyTmr},
     {control_pid,                CtrlPid},
     {monitor_ref,                MonRef},
     {send_mod,                   SendMod},
     {send_handle,                SendHandle},
     {encoding_mod,               EncodingMod},
     {encoding_config,            EncodingConf},
     {protocol_version,           ProtoVersion},
     {auth_data,                  AuthData},
     {user_mod,                   UserMod},
     {user_args,                  UserArgs},
     {reply_action,               ReplyAction}, 
     {reply_data,                 ReplyData}, 
     {threaded,                   Threaded}, 
     {strict_version,             StrictVersion}, 
     {long_request_resend,        LongReqResend}, 
     {call_proxy_gc_timeout,      CallProxyGCTimeout}, 
     {resend_indication,          ResendInd}, 
     {segment_reply_ind,          SegReplyInd}, 
     {segment_recv_acc,           SegRecvAcc}, 
     {segment_recv_timer,         SegRecvTmr}, 
     {segment_send,               SegSend}, 
     {segment_send_timer,         SegSendTmr}, 
     {max_pdu_size,               MaxPduSz}, 
     {request_keep_alive_timeout, RequestKeepAliveTmr}];

do_conn_info(conn_data = _Item, CD) ->
    CD;
do_conn_info(conn_handle = _Item, #conn_data{conn_handle = Val}) ->
    Val;
do_conn_info(mid = _Item, 
	     #conn_data{conn_handle = #megaco_conn_handle{local_mid = Val}}) ->
    Val;
do_conn_info(local_mid = _Item, 
	     #conn_data{conn_handle = #megaco_conn_handle{local_mid = Val}}) ->
    Val;
do_conn_info(remote_mid = _Item, 
	     #conn_data{conn_handle = #megaco_conn_handle{remote_mid = Val}}) ->
    Val;
do_conn_info(trans_id = _Item, 
	     #conn_data{conn_handle = #megaco_conn_handle{local_mid = LMid},
			max_serial  = Max}) ->
    Item2 = {LMid, trans_id_counter},
    case (catch ets:lookup(megaco_config, Item2)) of
	{'EXIT', _} ->
	    undefined_serial;
	[] ->
	    user_info(LMid, min_trans_id);
	[{_, Serial}] ->
	    if
		((Max =:= infinity) andalso 
		 is_integer(Serial) andalso 
		 (Serial < 4294967295)) ->
		    Serial + 1;
		((Max =:= infinity) andalso  
		 is_integer(Serial) andalso 
		 (Serial =:= 4294967295)) ->
		    user_info(LMid, min_trans_id);
		Serial < Max ->
		    Serial  + 1;
		Serial =:= Max ->
		    user_info(LMid, min_trans_id);
		Serial =:= 4294967295 ->
		    user_info(LMid, min_trans_id);
		true ->
		    undefined_serial
	    end
    end;
do_conn_info(max_trans_id = _Item, #conn_data{max_serial = Val}) ->
    Val;
do_conn_info(request_timer = _Item, #conn_data{request_timer = Val}) ->
    Val;
do_conn_info(long_request_timer = _Item, #conn_data{long_request_timer = Val}) ->
    Val;
do_conn_info(auto_ack = _Item, #conn_data{auto_ack = Val}) ->
    Val;
do_conn_info(trans_ack = _Item, #conn_data{trans_ack = Val}) ->
    Val;
do_conn_info(trans_ack_maxcount = _Item, #conn_data{trans_ack_maxcount = Val}) ->
    Val;
do_conn_info(trans_req = _Item, #conn_data{trans_req = Val}) ->
    Val;
do_conn_info(trans_req_maxcount = _Item, #conn_data{trans_req_maxcount = Val}) ->
    Val;
do_conn_info(trans_req_maxsize = _Item, #conn_data{trans_req_maxsize = Val}) ->
    Val;
do_conn_info(trans_timer = _Item, #conn_data{trans_timer = Val}) ->
    Val;
do_conn_info(pending_timer = _Item, #conn_data{pending_timer = Val}) ->
    Val;
do_conn_info(orig_pending_limit = _Item, #conn_data{sent_pending_limit = Val}) ->
    Val;
do_conn_info(sent_pending_limit = _Item, #conn_data{sent_pending_limit = Val}) ->
    Val;
do_conn_info(recv_pending_limit = _Item, #conn_data{recv_pending_limit = Val}) ->
    Val;
do_conn_info(reply_timer = _Item, #conn_data{reply_timer = Val}) ->
    Val;
do_conn_info(control_pid = _Item, #conn_data{control_pid = Val}) ->
    Val;
do_conn_info(send_mod = _Item, #conn_data{send_mod = Val}) ->
    Val;
do_conn_info(send_handle = _Item, #conn_data{send_handle = Val}) ->
    Val;
do_conn_info(encoding_mod = _Item, #conn_data{encoding_mod = Val}) ->
    Val;
do_conn_info(encoding_config = _Item, #conn_data{encoding_config = Val}) ->
    Val;
do_conn_info(protocol_version = _Item, #conn_data{protocol_version = Val}) ->
    Val;
do_conn_info(auth_data = _Item, #conn_data{auth_data = Val}) ->
    Val;
do_conn_info(user_mod = _Item, #conn_data{user_mod = Val}) ->
    Val;
do_conn_info(user_args = _Item, #conn_data{user_args = Val}) ->
    Val;
do_conn_info(reply_action = _Item, #conn_data{reply_action = Val}) ->
    Val;
do_conn_info(reply_data = _Item, #conn_data{reply_data = Val}) ->
    Val;
do_conn_info(threaded = _Item, #conn_data{threaded = Val}) ->
    Val;
do_conn_info(strict_version = _Item, #conn_data{strict_version = Val}) ->
    Val;
do_conn_info(long_request_resend = _Item, 
	     #conn_data{long_request_resend = Val}) ->
    Val;
do_conn_info(call_proxy_gc_timeout = _Item, 
	     #conn_data{call_proxy_gc_timeout = Val}) ->
    Val;
do_conn_info(resend_indication = _Item, #conn_data{resend_indication = Val}) ->
    Val;
do_conn_info(segment_reply_ind = _Item, #conn_data{segment_reply_ind = Val}) ->
    Val;
do_conn_info(segment_recv_acc = _Item, #conn_data{segment_recv_acc = Val}) ->
    Val;
do_conn_info(segment_recv_timer = _Item, 
	     #conn_data{segment_recv_timer = Val}) ->
    Val;
do_conn_info(segment_send = _Item, #conn_data{segment_send = Val}) ->
    Val;
do_conn_info(segment_send_timer = _Item, 
	     #conn_data{segment_send_timer = Val}) ->
    Val;
do_conn_info(max_pdu_size = _Item, #conn_data{max_pdu_size = Val}) ->
    Val;
do_conn_info(request_keep_alive_timeout = _Item, 
	     #conn_data{request_keep_alive_timeout = Val}) ->
    Val;
do_conn_info(receive_handle = _Item, 
	     #conn_data{conn_handle = #megaco_conn_handle{local_mid = LMid},
			encoding_mod    = EM,
			encoding_config = EC,
			send_mod        = SM}) ->
    #megaco_receive_handle{local_mid       = LMid,
			   encoding_mod    = EM,
			   encoding_config = EC,
			   send_mod        = SM};
do_conn_info(Item, Data) 
  when is_record(Data, conn_data) orelse is_record(Data, megaco_conn_handle) ->
    exit({no_such_item, Item});
do_conn_info(_Item, BadData) ->
    {error, {no_such_connection, BadData}}.


%% replace(_, _, []) ->
%%     [];
%% replace(Item, WithItem, [Item|List]) ->
%%     [WithItem|List];
%% replace(Item, WithItem, [OtherItem|List]) ->
%%     [OtherItem | replace(Item, WithItem, List)].


update_conn_info(#conn_data{conn_handle = CH}, Item, Val) ->
    do_update_conn_info(CH, Item, Val);
update_conn_info(CH, Item, Val) 
  when is_record(CH, megaco_conn_handle) andalso (Item /= cancel) ->
    do_update_conn_info(CH, Item, Val);
update_conn_info(BadHandle, _Item, _Val) ->
    {error, {no_such_connection, BadHandle}}.

do_update_conn_info(CH, orig_pending_limit, Val) ->
    do_update_conn_info(CH, sent_pending_limit, Val);
do_update_conn_info(CH, Item, Val) ->
    call({update_conn_data, CH, Item, Val}).


system_info(all) ->
    AllItems = [n_active_requests,
		n_active_replies,
		n_active_connections,
		users,
		connections,
		text_config,
		reply_counters,
		pending_counters], 
    [{Item, system_info(Item)} || Item <- AllItems];
system_info(Item) ->
    case Item of
        n_active_requests ->
            ets:info(megaco_requests, size);
        n_active_replies  ->
            ets:info(megaco_replies, size);
        n_active_connections  ->
            ets:info(megaco_local_conn, size);
        users ->
            Pat = {{'_', mid}, '_'},
            [Mid || {_, Mid} <- ets:match_object(megaco_config, Pat)];
        connections ->
            [C#conn_data.conn_handle || C <- ets:tab2list(megaco_local_conn)];
	text_config ->
	    case ets:lookup(megaco_config, text_config) of
		[] ->
		    [];
		[{text_config, Conf}] ->
		    [Conf]
	    end;
	
	reply_counters ->
	    reply_counters();

	pending_counters ->
	    pending_counters();

	recv_pending_counters ->
	    pending_counters(recv);

	sent_pending_counters ->
	    pending_counters(sent);

	BadItem ->
	    exit({no_such_item, BadItem})

    end.


get_env(Env, Default) ->
    case application:get_env(megaco, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

lookup_local_conn(Handle) ->
    ets:lookup(megaco_local_conn, Handle).


autoconnect(RH, RemoteMid, SendHandle, ControlPid) ->
    ?d("autoconnect -> entry with "
	"~n   RH:         ~p"
	"~n   RemoteMid:  ~p"
	"~n   SendHandle: ~p"
	"~n   ControlPid: ~p", [RH, RemoteMid, SendHandle, ControlPid]),
    case RemoteMid of
	{MidType, _MidValue} when is_atom(MidType) ->
	    call({connect, RH, RemoteMid, SendHandle, ControlPid, auto});
	preliminary_mid ->
	    call({connect, RH, RemoteMid, SendHandle, ControlPid, auto});
	BadMid ->
	    {error, {bad_remote_mid, BadMid}}
    end.

connect(RH, RemoteMid, SendHandle, ControlPid) ->
    ?d("connect -> entry with "
	"~n   RH:         ~p"
	"~n   RemoteMid:  ~p"
	"~n   SendHandle: ~p"
	"~n   ControlPid: ~p", [RH, RemoteMid, SendHandle, ControlPid]),
    case RemoteMid of
	{MidType, _MidValue} when is_atom(MidType) ->
	    call({connect, RH, RemoteMid, SendHandle, ControlPid, 
		  {plain, self()}});
	preliminary_mid ->
	    call({connect, RH, RemoteMid, SendHandle, ControlPid, 
		  {plain, self()}});
	BadMid ->
	    {error, {bad_remote_mid, BadMid}}
    end.

finish_connect(ConnHandle, SendHandle, ControlPid, MFA) ->
    ?d("finish_connect -> entry with "
	"~n   ConnHandle: ~p"
	"~n   SendHandle: ~p"
	"~n   ControlPid: ~p"
	"~n   MFA:        ~p", [ConnHandle, SendHandle, ControlPid, MFA]),
    call({finish_connect, ConnHandle, SendHandle, ControlPid, MFA}).

connect_remote(ConnHandle, UserNode, Ref) ->
    call({connect_remote, ConnHandle, UserNode, Ref}).

disconnect(ConnHandle) ->
    call({disconnect, ConnHandle}).

disconnect_remote(ConnHandle, UserNode) ->
    call({disconnect_remote, ConnHandle, UserNode}).


incr_counter(Item, Incr) ->
    try
	begin
	    ets:update_counter(megaco_config, Item, Incr)
	end
    catch
	error:_ ->
	    %% Counter does not exist, so try creat it
	    try
		begin
		    cre_counter(Item, Incr)
		end
	    catch
		exit:_ ->
		    %% This is a raise condition. 
		    %% When we tried to update the counter above, it
		    %% did not exist, but now it does...
		    ets:update_counter(megaco_config, Item, Incr)
	    end
    end.

cre_counter(Item, Initial) ->
    case whereis(?SERVER) =:= self() of
	false ->
	    case call({cre_counter, Item, Initial}) of
		{ok, Value} ->
		    Value;
		{error, Reason} ->
		    exit({failed_creating_counter, Item, Initial, Reason})
	    end;
	true ->
	    %% Check that the counter does not already exists
	    %% so we don't overwrite an already existing counter
	    case ets:lookup(megaco_config, Item) of
		[] ->
		    ets:insert(megaco_config, {Item, Initial}),
		    {ok, Initial};
		[_] ->
		    %% Possibly a raise condition
		    {error, already_exists}
		
		end
    end.
    

cre_reply_counter(ConnHandle, TransId) ->
    Counter = {reply_counter, ConnHandle, TransId},
    Initial = 1, 
    cre_counter(Counter, Initial).

incr_reply_counter(ConnHandle, TransId) ->
    Counter = {reply_counter, ConnHandle, TransId},
    incr_counter(Counter, 1).

get_reply_counter(ConnHandle, TransId) ->
    Counter = {reply_counter, ConnHandle, TransId},
    [{Counter, Val}] = ets:lookup(megaco_config, Counter),
    Val.

del_reply_counter(ConnHandle, TransId) ->
    Counter = {reply_counter, ConnHandle, TransId},
    ets:delete(megaco_config, Counter).

reply_counters() ->
    Pattern   = {{reply_counter, '_', '_'}, '_'}, 
    Counters1 = ets:match_object(megaco_config, Pattern),
    [{ConnHandle, TransId, CounterVal} || 
	{{reply_counter, ConnHandle, TransId}, CounterVal} <- Counters1].


cre_pending_counter(TransId) ->
    cre_pending_counter(sent, TransId, 0).

cre_pending_counter(Direction, TransId, Initial) ->
    Counter = {pending_counter, Direction, TransId},
    cre_counter(Counter, Initial).

incr_pending_counter(TransId) ->
    incr_pending_counter(sent, TransId).

incr_pending_counter(Direction, TransId) ->
    Counter = {pending_counter, Direction, TransId},
    incr_counter(Counter, 1).

get_pending_counter(TransId) ->
    get_pending_counter(sent, TransId).

get_pending_counter(Direction, TransId) ->
    Counter = {pending_counter, Direction, TransId},
    [{Counter, Val}] = ets:lookup(megaco_config, Counter),
    Val.

del_pending_counter(TransId) ->
    del_pending_counter(sent, TransId).

del_pending_counter(Direction, TransId) ->
    Counter = {pending_counter, Direction, TransId},
    ets:delete(megaco_config, Counter).


pending_counters() ->
    Pattern   = {{pending_counter, '_', '_'}, '_'}, 
    Counters1 = ets:match_object(megaco_config, Pattern),
    Counters2 = [{Direction, TransId, CounterVal} || 
		    {{pending_counter, Direction, TransId}, CounterVal} <- 
			Counters1],
    RecvCounters = [{TransId, CounterVal} || 
		       {recv, TransId, CounterVal} <- Counters2],
    SentCounters = [{TransId, CounterVal} || 
		       {sent, TransId, CounterVal} <- Counters2],
    [{recv, RecvCounters}, {sent, SentCounters}].
    

pending_counters(Direction) 
  when ((Direction =:= sent) orelse (Direction =:= recv)) ->
    Pattern  = {{pending_counter, Direction, '_'}, '_'}, 
    Counters = ets:match_object(megaco_config, Pattern),
    [{TransId, CounterVal} || 
	{{pending_counter, D, TransId}, CounterVal} <- 
	    Counters, (Direction == D)].

%% A wrapping transaction id counter
incr_trans_id_counter(ConnHandle) ->
    incr_trans_id_counter(ConnHandle, 1).
incr_trans_id_counter(ConnHandle, Incr) 
  when is_integer(Incr) andalso (Incr > 0) ->
    case megaco_config:lookup_local_conn(ConnHandle) of
        [] ->
            {error, {no_such_connection, ConnHandle}};
        [ConnData] ->
            LocalMid = ConnHandle#megaco_conn_handle.local_mid,
	    Min      = user_info(LocalMid, min_trans_id), 
	    Max      = 
		case ConnData#conn_data.max_serial of
		    infinity ->
			4294967295;
		    MS ->
			MS
		end,
	    Item     = ?TID_CNT(LocalMid),
	    do_incr_trans_id_counter(ConnData, Item, Min, Max, Incr, -1)
    end.

do_incr_trans_id_counter(ConnData, _Item, _Min, _Max, 0, Serial) ->
    ConnData2 = ConnData#conn_data{serial = Serial},
    {ok, ConnData2};
do_incr_trans_id_counter(ConnData, Item, Min, Max, N, _) ->
    case (catch ets:update_counter(megaco_config, Item, {2, 1, Max, Min})) of
	{'EXIT', _} ->
	    %% This can only happen for the first ever increment,
	    %% in which case N is equal to (the initial) Incr
	    ConnHandle = ConnData#conn_data.conn_handle, 
	    init_trans_id_counter(ConnHandle, Item, N);
	Serial ->
	    do_incr_trans_id_counter(ConnData, Item, Min, Max, N-1, Serial)
    end.

init_trans_id_counter(ConnHandle, Item, Incr) ->
    case whereis(?SERVER) == self() of
        false ->
	    call({init_trans_id_counter, ConnHandle, Item, Incr});
	true ->
	    do_init_trans_id_counter(ConnHandle, Item, Incr)
    end.

do_init_trans_id_counter(ConnHandle, Item, Incr) ->
    case megaco_config:lookup_local_conn(ConnHandle) of
	[] ->
	    {error, {no_such_connection, ConnHandle}};
	[ConnData] ->
	    %% Make sure that the counter still does not exist
	    LocalMid = ConnHandle#megaco_conn_handle.local_mid,
	    Min      = user_info(LocalMid, min_trans_id), 
	    Max      = 
		case ConnData#conn_data.max_serial of
		    infinity ->
			4294967295;
		    MS ->
			MS
		end,
	    Item     = ?TID_CNT(LocalMid),
	    Incr2    = {2, Incr, Max, Min}, 
	    case (catch ets:update_counter(megaco_config, Item, Incr2)) of
		{'EXIT', _} ->
		    %% Yep, we are the first one here 
		    Serial1 = Min + (Incr-1),
		    ets:insert(megaco_config, {Item, Serial1}),
		    ConnData2 = ConnData#conn_data{serial = Serial1},
		    {ok, ConnData2};
		Serial2 ->
		    %% No, someone got there before we did
		    ConnData2 = ConnData#conn_data{serial = Serial2},
		    {ok, ConnData2}
	    end
    end.

%% For backward compatibillity (during code upgrade)
reset_trans_id_counter(ConnHandle, Item, Serial) ->
    LocalMid = ConnHandle#megaco_conn_handle.local_mid,
    case megaco_config:lookup_local_conn(ConnHandle) of
        [] ->
            {error, {no_such_connection, ConnHandle}};
        [ConnData] ->
	    do_reset_trans_id_counter(ConnData, LocalMid, 
				      Item, Serial)
    end.

do_reset_trans_id_counter(ConnData, LocalMid, Item, Serial) 
  when is_integer(Serial) ->
    Max = ConnData#conn_data.max_serial,
    Overflow = 
	if 
	    (Max == infinity) ->
		Serial - 4294967295;

	    is_integer(Max) ->
		Serial - Max
	end,
    NewSerial = user_info(LocalMid, min_trans_id) + (Overflow-1),
    ConnData2 = ConnData#conn_data{serial = NewSerial},
    ets:insert(megaco_config, {Item, NewSerial}),
    {ok, ConnData2}.


trans_sender_exit(Reason, CH) ->
    ?d("trans_sender_exit -> entry with"
	"~n   Reason: ~p"
	"~n   CH: ~p", [Reason, CH]),
    cast({trans_sender_exit, Reason, CH}).


call(Request) ->
    case (catch gen_server:call(?SERVER, Request, infinity)) of
	{'EXIT', _} ->
	    {error, megaco_not_started};
	Res ->
	    Res
    end.


cast(Msg) ->
    case (catch gen_server:cast(?SERVER, Msg)) of
	{'EXIT', _} ->
	    {error, megaco_not_started};
	Res ->
	    Res
    end.


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([Parent]) ->
    ?d("init -> entry with "
	"~n   Parent: ~p", [Parent]),
    process_flag(trap_exit, true),
    case (catch do_init()) of
	ok ->
	    ?d("init -> init ok", []),
	    {ok, #state{parent_pid = Parent}};
	Else ->
	    ?d("init -> init error: "
	       "~n   ~p", [Else]),
	    {stop, Else}
    end.

do_init() ->
    ?megaco_test_init(),
    ets:new(megaco_config,      [public, named_table, {keypos, 1}]),
    ets:new(megaco_local_conn,  [public, named_table, {keypos, 2}]),
    ets:new(megaco_remote_conn, [public, named_table, {keypos, 2}, bag]),
    megaco_stats:init(megaco_stats, global_snmp_counters()),
    init_scanner(),
    init_user_defaults(),
    init_users().
    


init_scanner() ->
    case get_env(scanner, undefined) of
	undefined ->
	    Key  = text_config,
	    Data = [],
	    ets:insert(megaco_config, {Key, Data});
	flex ->
	    start_scanner(megaco_flex_scanner_handler, 
			  start_link, [], [gen_server]);
	{flex, Opts} when is_list(Opts) -> % For future use
	    start_scanner(megaco_flex_scanner_handler, 
			  start_link, [Opts], [gen_server]);
	{M, F, A, Mods} when is_atom(M) andalso 
			     is_atom(F) andalso 
			     is_list(A) andalso 
			     is_list(Mods) ->
	    start_scanner(M, F, A, Mods)
    end.

start_scanner(M, F, A, Mods) ->
    case megaco_misc_sup:start_permanent_worker(M, F, A, Mods) of
	{ok, Pid, Conf} when is_pid(Pid) ->
	    Key  = text_config,
	    Data = [Conf],
	    ets:insert(megaco_config, {Key, Data});
	Else ->
	    throw({scanner_start_failed, Else})
    end.

init_user_defaults() ->
    init_user_default(min_trans_id,         1),
    init_user_default(max_trans_id,         infinity), 
    init_user_default(request_timer,        #megaco_incr_timer{}),
    init_user_default(long_request_timer,   timer:seconds(60)),

    init_user_default(auto_ack,             false),

    init_user_default(trans_ack,            false),
    init_user_default(trans_ack_maxcount,   10),

    init_user_default(trans_req,            false),
    init_user_default(trans_req_maxcount,   10),
    init_user_default(trans_req_maxsize,    2048),

    init_user_default(trans_timer,          0),
    init_user_default(trans_sender,         undefined),

    init_user_default(pending_timer,        timer:seconds(30)),
    init_user_default(sent_pending_limit,   infinity),
    init_user_default(recv_pending_limit,   infinity),
    init_user_default(reply_timer,          timer:seconds(30)),
    init_user_default(send_mod,             megaco_tcp),
    init_user_default(encoding_mod,         megaco_pretty_text_encoder),
    init_user_default(protocol_version,     1),
    init_user_default(auth_data,            asn1_NOVALUE),
    init_user_default(encoding_config,      []),
    init_user_default(user_mod,             megaco_user_default),
    init_user_default(user_args,            []),
    init_user_default(reply_data,           undefined),
    init_user_default(threaded,             false),
    init_user_default(strict_version,       true),
    init_user_default(long_request_resend,  false),
    init_user_default(call_proxy_gc_timeout, timer:seconds(5)),
    init_user_default(cancel,               false),
    init_user_default(resend_indication,    false),
    init_user_default(segment_reply_ind,    false),
    init_user_default(segment_recv_acc,     false),
    init_user_default(segment_recv_timer,   timer:seconds(10)),
    init_user_default(segment_send,         none),
    init_user_default(segment_send_timer,   timer:seconds(5)),
    init_user_default(max_pdu_size,         infinity),
    init_user_default(request_keep_alive_timeout, plain).

init_user_default(Item, Default) when Item /= mid ->
    Val = get_env(Item, Default),
    case do_update_user(default, Item, Val) of
	ok ->
	    ok;
	{error, Reason} ->
	    throw(Reason)
    end.

init_users() ->
    Users = get_env(users, []),
    init_users(Users).

init_users([]) ->
    ok;
init_users([{UserMid, Config} | Rest]) ->
    case handle_start_user(UserMid, Config) of
        ok ->
            init_users(Rest);
        Else ->
            throw({bad_user, UserMid, Else})
    end;
init_users(BadConfig) ->
    throw({bad_config, users, BadConfig}).

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({cre_counter, Item, Incr}, _From, S) ->
    Reply = cre_counter(Item, Incr),
    {reply, Reply, S};

handle_call({del_counter, Item, Incr}, _From, S) ->
    Reply = cre_counter(Item, Incr),
    {reply, Reply, S};

%% For backward compatibillity (code upgrade)
handle_call({incr_trans_id_counter, ConnHandle}, _From, S) ->
    Reply = incr_trans_id_counter(ConnHandle),
    {reply, Reply, S};

handle_call({init_trans_id_counter, ConnHandle, Item, Incr}, _From, S) ->
    Reply = do_init_trans_id_counter(ConnHandle, Item, Incr),
    {reply, Reply, S};

%% For backward compatibillity (code upgrade)
handle_call({reset_trans_id_counter, ConnHandle, Item, Serial}, _From, S) ->
    Reply = reset_trans_id_counter(ConnHandle, Item, Serial),
    {reply, Reply, S};

handle_call({receive_handle, UserMid}, _From, S) ->
    case catch make_receive_handle(UserMid) of
	{'EXIT', _} ->
	    {reply, {error, {no_receive_handle, UserMid}}, S};
	RH ->
	    {reply, {ok, RH}, S}
    end;
handle_call({connect, RH, RemoteMid, SendHandle, ControlPid}, _From, S) ->
    Reply = handle_connect(RH, RemoteMid, SendHandle, ControlPid, auto),
    {reply, Reply, S};
handle_call({connect, RH, RemoteMid, SendHandle, ControlPid, Auto}, _From, S) ->
    Reply = handle_connect(RH, RemoteMid, SendHandle, ControlPid, Auto),
    {reply, Reply, S};

handle_call({finish_connect, ConnHandle, SendHandle, ControlPid, MFA}, 
	    _From, S) ->
    Reply = handle_finish_connect(ConnHandle, SendHandle, ControlPid, MFA),
    {reply, Reply, S};

handle_call({connect_remote, CH, UserNode, Ref}, _From, S) ->
    Reply = handle_connect_remote(CH, UserNode, Ref),
    {reply, Reply, S};

handle_call({disconnect, ConnHandle}, _From, S) ->
    Reply = handle_disconnect(ConnHandle),
    {reply, Reply, S};
handle_call({disconnect_remote, CH, UserNode}, _From, S) ->
    Reply = handle_disconnect_remote(CH, UserNode),
    {reply, Reply, S};

handle_call({start_user, UserMid, Config}, _From, S) ->
    Reply = handle_start_user(UserMid, Config),
    {reply, Reply, S};
handle_call({stop_user, UserMid}, _From, S) ->
    Reply = handle_stop_user(UserMid),
    {reply, Reply, S};
handle_call({update_conn_data, CH, Item, Val}, _From, S) ->
    case lookup_local_conn(CH) of
        [] ->
            {reply, {error, {no_such_connection, CH}}, S};
        [CD] ->
            Reply = handle_update_conn_data(CD, Item, Val),
            {reply, Reply, S}
    end;
handle_call({update_user_info, UserMid, Item, Val}, _From, S) ->
    case catch user_info(UserMid, mid) of
        {'EXIT', _} ->
            {reply, {error, {no_such_user, UserMid}}, S};
        _ ->
            Reply = do_update_user(UserMid, Item, Val),
            {reply, Reply, S}
    end;

handle_call({stop, ParentPid}, _From, #state{parent_pid = ParentPid} = S) ->
    Reason = normal, 
    Reply  = ok,
    {stop, Reason, Reply, S};

handle_call(Req, From, S) ->
    warning_msg("received unexpected request from ~p: "
		"~n~w", [From, Req]),
    {reply, {error, {bad_request, Req}}, S}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast({trans_sender_exit, Reason, CH}, S) ->
    warning_msg("transaction sender [~p] restarting: "
		"~n~p", [CH, Reason]),
    case lookup_local_conn(CH) of
	[] ->
	    error_msg("connection data not found for ~p~n"
		      "when restarting transaction sender", [CH]);
	[CD] ->
	    CD2 = trans_sender_start(CD#conn_data{trans_sender = undefined}),
	    ets:insert(megaco_local_conn, CD2)
    end,
    {noreply, S};

handle_cast(Msg, S) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply, S}.



%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({'EXIT', Pid, Reason}, S) when Pid =:= S#state.parent_pid ->
    {stop, Reason, S};

handle_info(Info, S) ->
    warning_msg("received unknown info: "
		"~n~w", [Info]),
    {noreply, S}.



%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(_Vsn, S, upgrade_from_pre_3_12) ->
    upgrade_user_info_from_pre_3_12(),
    upgrade_conn_data_from_pre_3_12(),
    {ok, S};

code_change(_Vsn, S, downgrade_to_pre_3_12) ->
    downgrade_user_info_to_pre_3_12(),
    downgrade_conn_data_to_pre_3_12(),
    {ok, S};

code_change(_Vsn, S, _Extra) ->
    {ok, S}.


%% -- Upgrade user info --

upgrade_user_info_from_pre_3_12() ->
    NewValues = [{request_keep_alive_timeout, plain}],
    upgrade_user_info(NewValues).

%% upgrade_user_info_from_pre_3_7() ->
%%     NewValues = [{segment_reply_ind,  false},
%% 		 {segment_recv_acc,   false},
%% 		 {segment_recv_timer, #megaco_incr_timer{}},
%% 		 {segment_send,       none}, 
%% 		 {segment_send_timer, infinity}, 
%% 		 {max_pdu_size,       infinity}],
%%     upgrade_user_info(NewValues).

upgrade_user_info(NewValues) ->
    Users = [default|system_info(users)],
    F = fun({Item, Val}) ->
		upgrade_user_info(Users, Item, Val)
	end,
    lists:foreach(F, NewValues),
    ok.

upgrade_user_info(Users, Item, Val) ->
    F = fun(User) -> do_update_user(User, Item, Val) end,
    lists:foreach(F, Users),
    ok.


%% %% -- Downgrade user info --

downgrade_user_info_to_pre_3_12() ->
    NewItems = [
		request_keep_alive_timeout
	       ],
    downgrade_user_info(NewItems).

%% downgrade_user_info_to_pre_3_7() ->
%%     NewItems = [
%% 		segment_reply_ind, 
%% 		segment_recv_acc, 
%% 		segment_recv_timer, 
%% 		segment_send,     
%% 		segment_send_timer, 
%% 		max_pdu_size
%% 	       ],
%%     downgrade_user_info(NewItems).

downgrade_user_info(NewItems) ->
    Users = [default|system_info(users)],
    F = fun(Item) ->
		downgrade_user_info(Users, Item)
	end,
    lists:foreach(F, NewItems),
    ok.
    
downgrade_user_info(Users, Item) ->
    F = fun(User) -> do_downgrade_user_info(User, Item) end,
    lists:foreach(F, Users),
    ok.

do_downgrade_user_info(User, Item) ->
    ets:delete(megaco_config, {User, Item}).
		

%% %% -- Upgrade conn data --

upgrade_conn_data_from_pre_3_12() ->
    Conns    = system_info(connections),
    Defaults = [{request_keep_alive_timeout, plain}], 
    upgrade_conn_data(Conns, Defaults).

%% upgrade_conn_data_from_pre_3_7() ->
%%     Conns    = system_info(connections),
%%     Defaults = [{segment_reply_ind,  false}, 
%% 		{segment_recv_acc,   false}, 
%% 		{segment_recv_timer, #megaco_incr_timer{}}, 
%% 		{segment_send,       false}, 
%% 		{segment_send_timer, #megaco_incr_timer{}}, 
%% 		{max_pdu_size,       infinity}], 
%%     upgrade_conn_data(Conns, Defaults).

upgrade_conn_data(Conns, Defaults) ->
    F = fun(CH) ->
		case lookup_local_conn(CH) of
		    [] ->
			ok;
		    [CD] ->
			do_upgrade_conn_data(CD, Defaults)
		end
	end,
    lists:foreach(F, Conns),
    ok.

do_upgrade_conn_data(OldStyleCD, Defaults) ->
    NewStyleCD = new_conn_data(OldStyleCD, Defaults),
    ets:insert(megaco_local_conn, NewStyleCD).
    
%% Pre 3.12
new_conn_data({conn_data, CH, Serial, MaxSerial, ReqTmr, LongReqTmr, 
	       AutoAck, 
	       TransAck, TransAckMaxCnt, 
	       TransReq, TransReqMaxCnt, TransReqMaxSz, 
	       TransTmr, TransSndr, 
	       
	       PendingTmr, 
	       SentPendingLimit, 
	       RecvPendingLimit,
	       ReplyTmr, CtrPid, MonRef, 
	       Sendmod, SendHandle, 
	       EncodeMod, EncodeConf, 
	       ProtV, AuthData, 
	       UserMod, UserArgs, ReplyAction, ReplyData,
	       Threaded,
	       StrictVersion,
	       LongReqResend,
	       Cancel,
	       ResendIndication, 
	       SegmentReplyInd, 
	       SegmentRecvAcc, 
	       SegmentRecvTimer, 
	       SegmentSend, 
	       SegmentSendTimer, 
	       MaxPDUSize 
	       %% RequestKeepAliveTimerDefault - New values
	      }, 
	      Defaults) ->
    #conn_data{conn_handle          = CH, 
	       serial               = Serial,
	       max_serial           = MaxSerial,
	       request_timer        = ReqTmr,
	       long_request_timer   = LongReqTmr,
	       
	       auto_ack             = AutoAck,
	       
	       trans_ack            = TransAck,
	       trans_ack_maxcount   = TransAckMaxCnt,
	       
	       trans_req            = TransReq,
	       trans_req_maxcount   = TransReqMaxCnt,
	       trans_req_maxsize    = TransReqMaxSz,
	       
	       trans_timer          = TransTmr,
	       trans_sender         = TransSndr,
	       
	       pending_timer        = PendingTmr,
	       sent_pending_limit   = SentPendingLimit,
	       recv_pending_limit   = RecvPendingLimit, 

	       reply_timer          = ReplyTmr,
	       control_pid          = CtrPid,
	       monitor_ref          = MonRef,
	       send_mod             = Sendmod,
	       send_handle          = SendHandle,
	       encoding_mod         = EncodeMod,
	       encoding_config      = EncodeConf,
	       protocol_version     = ProtV,
	       auth_data            = AuthData,
	       user_mod             = UserMod,
	       user_args            = UserArgs,
	       reply_action         = ReplyAction,
	       reply_data           = ReplyData,
	       threaded             = Threaded,
	       strict_version       = StrictVersion,
	       long_request_resend  = LongReqResend, 
	       cancel               = Cancel,
	       resend_indication    = ResendIndication,
	       segment_reply_ind    = SegmentReplyInd,
	       segment_recv_acc     = SegmentRecvAcc,
	       segment_recv_timer   = SegmentRecvTimer,
	       segment_send         = SegmentSend,
	       segment_send_timer   = SegmentSendTimer,
	       max_pdu_size         = MaxPDUSize,
	       request_keep_alive_timeout = get_default(request_keep_alive_timeout, Defaults)
	      }.


get_default(Key, Defaults) ->
    {value, {Key, Default}} = lists:keysearch(Key, 1, Defaults),
    Default.


%% %% -- Downgrade conn data --

downgrade_conn_data_to_pre_3_12() ->
    Conns = system_info(connections),
    Downgrade = fun(NewCD) -> old_conn_data_to_pre_3_12(NewCD) end,
    downgrade_conn_data(Downgrade, Conns).

downgrade_conn_data(Downgrade, Conns) ->
    F = fun(CH) ->
		case lookup_local_conn(CH) of
		    [] ->
			ok;
		    [CD] ->
			do_downgrade_conn_data(Downgrade, CD)
		end
	end,
    lists:foreach(F, Conns).

do_downgrade_conn_data(Downgrade, NewStyleCD) ->
    OldStyleCD = Downgrade(NewStyleCD),
    ets:insert(megaco_local_conn, OldStyleCD).

old_conn_data_to_pre_3_12(
  #conn_data{conn_handle          = CH, 
	     serial               = Serial,
	     max_serial           = MaxSerial,
	     request_timer        = ReqTmr,
	     long_request_timer   = LongReqTmr,
	     
	     auto_ack             = AutoAck,
	     
	     trans_ack            = TransAck,
	     trans_ack_maxcount   = TransAckMaxCnt,
	     
	     trans_req            = TransReq,
	     trans_req_maxcount   = TransReqMaxCnt,
	     trans_req_maxsize    = TransReqMaxSz,
	     
	     trans_timer          = TransTmr,
	     trans_sender         = TransSndr,
	     
	     pending_timer        = PendingTmr,
	     sent_pending_limit   = SentPendingLimit,
	     recv_pending_limit   = RecvPendingLimit, 
	     
	     reply_timer          = ReplyTmr,
	     control_pid          = CtrPid,
	     monitor_ref          = MonRef,
	     send_mod             = Sendmod,
	     send_handle          = SendHandle,
	     encoding_mod         = EncodeMod,
	     encoding_config      = EncodeConf,
	     protocol_version     = ProtV,
	     auth_data            = AuthData,
	     user_mod             = UserMod,
	     user_args            = UserArgs,
	     reply_action         = ReplyAction,
	     reply_data           = ReplyData,
	     threaded             = Threaded,
	     strict_version       = StrictVersion,
	     long_request_resend  = LongReqResend,
	     cancel               = Cancel,
	     resend_indication    = ResendIndication, 
	     segment_reply_ind    = SegmentRecvAcc,
	     segment_recv_acc     = SegmentRecvAcc,
	     segment_recv_timer   = SegmentRecvTimer,
	     segment_send         = SegmentSend,
	     segment_send_timer   = SegmentSendTimer,
	     max_pdu_size         = MaxPDUSize
	     %% request_keep_alive_timeout = RequestKeepAliveTimeout
	    }) ->
    {conn_data, CH, Serial, MaxSerial, ReqTmr, LongReqTmr, 
     AutoAck, 
     TransAck, TransAckMaxCnt, 
     TransReq, TransReqMaxCnt, TransReqMaxSz, 
     TransTmr, TransSndr, 
     PendingTmr, 
     SentPendingLimit, 
     RecvPendingLimit, 
     ReplyTmr, CtrPid, MonRef, 
     Sendmod, SendHandle, 
     EncodeMod, EncodeConf, 
     ProtV, AuthData, 
     UserMod, UserArgs, ReplyAction, ReplyData,
     Threaded,
     StrictVersion,
     LongReqResend,
     Cancel,
     ResendIndication,
     SegmentRecvAcc,
     SegmentRecvAcc,
     SegmentRecvTimer,
     SegmentSend,
     SegmentSendTimer,
     MaxPDUSize}.


		
%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

handle_start_user(default, _Config) ->
    {error, bad_user_mid};
handle_start_user(Mid, Config) ->
    case catch user_info(Mid, mid) of
        {'EXIT', _} ->
	    DefaultConfig = user_info(default, all),
            do_handle_start_user(Mid, DefaultConfig),
	    do_handle_start_user(Mid, Config);
        _LocalMid ->
            {error, {user_already_exists, Mid}}
    end.

do_handle_start_user(UserMid, [{Item, Val} | Rest]) ->
    case do_update_user(UserMid, Item, Val) of
        ok ->
            do_handle_start_user(UserMid, Rest);
        {error, Reason} ->
            ets:match_delete(megaco_config, {{UserMid, '_'}, '_'}),
            {error, Reason}
    end;
do_handle_start_user(UserMid, []) ->
    do_update_user(UserMid, mid, UserMid),
    ok;
do_handle_start_user(UserMid, BadConfig) ->
    ets:match_delete(megaco_config, {{UserMid, '_'}, '_'}),
    {error, {bad_user_config, UserMid, BadConfig}}.

do_update_user(UserMid, Item, Val) ->
    case verify_val(Item, Val) of
        true  ->
            ets:insert(megaco_config, {{UserMid, Item}, Val}),
            ok;
        false ->
            {error, {bad_user_val, UserMid, Item, Val}}
    end.

verify_val(Item, Val) ->
    case Item of
        mid                    -> true;
        local_mid              -> true;
        remote_mid             -> true;
        min_trans_id           -> 
	    megaco_config_misc:verify_strict_uint(Val, 4294967295); % uint32
        max_trans_id           -> 
	    megaco_config_misc:verify_uint(Val, 4294967295);        % uint32
        request_timer          -> verify_timer(Val);
        long_request_timer     -> verify_timer(Val);

        auto_ack               -> 
	    megaco_config_misc:verify_bool(Val);

	trans_ack              -> 
	    megaco_config_misc:verify_bool(Val);
        trans_ack_maxcount     -> 
	    megaco_config_misc:verify_uint(Val);

	trans_req              -> 
	    megaco_config_misc:verify_bool(Val);
        trans_req_maxcount     -> 
	    megaco_config_misc:verify_uint(Val);
        trans_req_maxsize      -> 
	    megaco_config_misc:verify_uint(Val);

        trans_timer            -> 
	    verify_timer(Val) and (Val >= 0);
	trans_sender when Val =:= undefined -> true;

        pending_timer                      -> verify_timer(Val);
        sent_pending_limit                 -> 
	    megaco_config_misc:verify_uint(Val) andalso (Val > 0);
        recv_pending_limit                 -> 
	    megaco_config_misc:verify_uint(Val) andalso (Val > 0);
        reply_timer                        -> verify_timer(Val);
        control_pid      when is_pid(Val)  -> true;
        monitor_ref                        -> true; % Internal usage only
        send_mod         when is_atom(Val) -> true;
        send_handle                        -> true;
        encoding_mod     when is_atom(Val) -> true;
        encoding_config  when is_list(Val) -> true;
        protocol_version                   -> 
	    megaco_config_misc:verify_strict_uint(Val);
        auth_data                          -> true;
        user_mod         when is_atom(Val) -> true;
        user_args        when is_list(Val) -> true;
        reply_data                         -> true;
        threaded                           -> 
	    megaco_config_misc:verify_bool(Val);
        strict_version                     -> 
	    megaco_config_misc:verify_bool(Val);
	long_request_resend                -> 
	    megaco_config_misc:verify_bool(Val);
	call_proxy_gc_timeout              -> 
	    megaco_config_misc:verify_strict_uint(Val);
	cancel                             -> 
	    megaco_config_misc:verify_bool(Val);
	resend_indication                  -> verify_resend_indication(Val);

	segment_reply_ind               -> 
	    megaco_config_misc:verify_bool(Val);
	segment_recv_acc                -> 
	    megaco_config_misc:verify_bool(Val);
	segment_recv_timer              -> verify_timer(Val);
	segment_send                    -> verify_segmentation_window(Val);
	segment_send_timer              -> verify_timer(Val);
	max_pdu_size                    -> 
	    megaco_config_misc:verify_int(Val) andalso (Val > 0);
	request_keep_alive_timeout      -> 
	    (megaco_config_misc:verify_uint(Val) orelse (Val =:= plain));

        _                               -> false
    end.



verify_resend_indication(flag) -> true;
verify_resend_indication(Val)  -> megaco_config_misc:verify_bool(Val).

verify_timer(Timer) ->
    megaco_timer:verify(Timer).

verify_segmentation_window(none) ->
    true;
verify_segmentation_window(K) ->
    megaco_config_misc:verify_int(K, 1, infinity).

handle_stop_user(UserMid) ->
    case catch user_info(UserMid, mid) of
        {'EXIT', _} ->
	    {error, {no_such_user, UserMid}};
	_ ->
	    case catch user_info(UserMid, connections) of
		[] ->
		    ets:match_delete(megaco_config, {{UserMid, '_'}, '_'}),
		    ok;
		{'EXIT', _} ->
		    {error, {no_such_user, UserMid}};
		_Else ->
		    {error, {active_connections, UserMid}}
	    end
    end.

handle_update_conn_data(CD, Item = receive_handle, RH) ->
    UserMid = (CD#conn_data.conn_handle)#megaco_conn_handle.local_mid,
    if
        is_record(RH, megaco_receive_handle) andalso 
        is_atom(RH#megaco_receive_handle.encoding_mod) andalso
        is_list(RH#megaco_receive_handle.encoding_config) andalso
        is_atom(RH#megaco_receive_handle.send_mod) andalso
        (RH#megaco_receive_handle.local_mid /= UserMid) ->
            CD2 = CD#conn_data{
		    encoding_mod    = RH#megaco_receive_handle.encoding_mod,
		    encoding_config = RH#megaco_receive_handle.encoding_config,
		    send_mod        = RH#megaco_receive_handle.send_mod},
            ets:insert(megaco_local_conn, CD2),
            ok;
        true ->
            {error, {bad_user_val, UserMid, Item, RH}}
    end;
handle_update_conn_data(CD, Item, Val) ->
    case verify_val(Item, Val) of
        true ->
            CD2 = replace_conn_data(CD, Item, Val),
            ets:insert(megaco_local_conn, CD2),
            ok;
        false ->
            UserMid = (CD#conn_data.conn_handle)#megaco_conn_handle.local_mid,
            {error, {bad_user_val, UserMid, Item, Val}}
    end.

replace_conn_data(CD, Item, Val) ->
    case Item of
        trans_id             -> CD#conn_data{serial             = Val};
        max_trans_id         -> CD#conn_data{max_serial         = Val};
        request_timer        -> CD#conn_data{request_timer      = Val};
        long_request_timer   -> CD#conn_data{long_request_timer = Val};

	auto_ack             -> update_auto_ack(CD, Val);

	%% Accumulate trans ack before sending
	trans_ack            -> update_trans_ack(CD, Val); 
	trans_ack_maxcount   -> update_trans_ack_maxcount(CD, Val);

	%% Accumulate trans req before sending
	trans_req            -> update_trans_req(CD, Val); 
	trans_req_maxcount   -> update_trans_req_maxcount(CD, Val);
	trans_req_maxsize    -> update_trans_req_maxsize(CD, Val);

	trans_timer          -> update_trans_timer(CD, Val); 
	%% trans_sender      - Automagically updated by 
	%%                     update_auto_ack & update_trans_timer & 
	%%                     update_trans_ack & update_trans_req

        pending_timer        -> CD#conn_data{pending_timer        = Val};
        sent_pending_limit   -> CD#conn_data{sent_pending_limit   = Val};
        recv_pending_limit   -> CD#conn_data{recv_pending_limit   = Val};
        reply_timer          -> CD#conn_data{reply_timer          = Val};
        control_pid          -> CD#conn_data{control_pid          = Val};
        monitor_ref          -> CD#conn_data{monitor_ref          = Val};
        send_mod             -> CD#conn_data{send_mod             = Val};
        send_handle          -> CD#conn_data{send_handle          = Val};
        encoding_mod         -> CD#conn_data{encoding_mod         = Val};
        encoding_config      -> CD#conn_data{encoding_config      = Val};
        protocol_version     -> CD#conn_data{protocol_version     = Val};
        auth_data            -> CD#conn_data{auth_data            = Val};
        user_mod             -> CD#conn_data{user_mod             = Val};
        user_args            -> CD#conn_data{user_args            = Val};
        reply_action         -> CD#conn_data{reply_action         = Val};
        reply_data           -> CD#conn_data{reply_data           = Val};
        threaded             -> CD#conn_data{threaded             = Val};
        strict_version       -> CD#conn_data{strict_version       = Val};
	long_request_resend  -> CD#conn_data{long_request_resend  = Val};
	call_proxy_gc_timeout -> CD#conn_data{call_proxy_gc_timeout = Val};
	cancel               -> CD#conn_data{cancel               = Val};
	resend_indication    -> CD#conn_data{resend_indication    = Val};
        segment_reply_ind    -> CD#conn_data{segment_reply_ind    = Val};
        segment_recv_acc     -> CD#conn_data{segment_recv_acc     = Val};
        segment_recv_timer   -> CD#conn_data{segment_recv_timer   = Val};
        segment_send         -> CD#conn_data{segment_send         = Val};
        segment_send_timer   -> CD#conn_data{segment_send_timer   = Val};
        max_pdu_size         -> CD#conn_data{max_pdu_size         = Val};
        request_keep_alive_timeout -> CD#conn_data{request_keep_alive_timeout = Val}
    end.

%% update auto_ack
update_auto_ack(#conn_data{trans_sender = Pid,
			   trans_req    = false} = CD, 
		false) when is_pid(Pid) ->
    megaco_trans_sender:stop(Pid),
    CD#conn_data{auto_ack = false, trans_sender = undefined};

update_auto_ack(#conn_data{trans_timer  = To, 
			   trans_ack    = true,
			   trans_sender = undefined} = CD, 
		true) when To > 0 ->
    #conn_data{conn_handle        = CH, 
	       trans_ack_maxcount = AcksMax, 
	       trans_req_maxcount = ReqsMax, 
	       trans_req_maxsize  = ReqsMaxSz} = CD,
    {ok, Pid} = megaco_trans_sup:start_trans_sender(CH, To, ReqsMaxSz, 
						    ReqsMax, AcksMax),

    %% Make sure we are notified when/if the transaction 
    %% sender goes down. 
    %% Do we need to store the ref? Will we ever need to 
    %% cancel this (apply_at_exit)?
    megaco_monitor:apply_at_exit(?MODULE, trans_sender_exit, [CH], Pid),

    CD#conn_data{auto_ack = true, trans_sender = Pid};

update_auto_ack(CD, Val) ->
    ?d("update_auto_ack -> entry with ~p", [Val]),
    CD#conn_data{auto_ack = Val}.

%% update trans_ack
update_trans_ack(#conn_data{auto_ack     = true,
			    trans_req    = false,
			    trans_sender = Pid} = CD, 
		      false) when is_pid(Pid) ->
    megaco_trans_sender:stop(Pid),
    CD#conn_data{trans_ack = false, trans_sender = undefined};

update_trans_ack(#conn_data{trans_timer  = To,
			    auto_ack     = true, 
			    trans_sender = undefined} = CD, 
		      true) when To > 0 ->
    #conn_data{conn_handle        = CH, 
	       trans_ack_maxcount = AcksMax, 
	       trans_req_maxcount = ReqsMax, 
	       trans_req_maxsize  = ReqsMaxSz} = CD,
    {ok, Pid} = megaco_trans_sup:start_trans_sender(CH, To, ReqsMaxSz, 
						    ReqsMax, AcksMax),

    %% Make sure we are notified when/if the transaction 
    %% sender goes down. 
    %% Do we need to store the ref? Will we ever need to 
    %% cancel this (apply_at_exit)?
    megaco_monitor:apply_at_exit(?MODULE, trans_sender_exit, [CH], Pid),

    CD#conn_data{trans_ack = true, trans_sender = Pid};

update_trans_ack(CD, Val) ->
    ?d("update_trans_ack -> entry with ~p", [Val]),
    CD#conn_data{trans_ack = Val}.

%% update trans_req
update_trans_req(#conn_data{trans_ack    = false,
			    trans_sender = Pid} = CD, 
		      false) when is_pid(Pid) ->
    megaco_trans_sender:stop(Pid),
    CD#conn_data{trans_req = false, trans_sender = undefined};

update_trans_req(#conn_data{trans_timer  = To, 
			    trans_sender = undefined} = CD, 
		      true) when To > 0 ->
    #conn_data{conn_handle        = CH, 
	       trans_ack_maxcount = AcksMax, 
	       trans_req_maxcount = ReqsMax, 
	       trans_req_maxsize  = ReqsMaxSz} = CD,
    {ok, Pid} = megaco_trans_sup:start_trans_sender(CH, To, ReqsMaxSz, 
						    ReqsMax, AcksMax),

    %% Make sure we are notified when/if the transaction 
    %% sender goes down. 
    %% Do we need to store the ref? Will we ever need to 
    %% cancel this (apply_at_exit)?
    megaco_monitor:apply_at_exit(?MODULE, trans_sender_exit, [CH], Pid),

    CD#conn_data{trans_req = true, trans_sender = Pid};

update_trans_req(CD, Val) ->
    ?d("update_trans_req -> entry with ~p", [Val]),
    CD#conn_data{trans_req = Val}.

%% update trans_timer
update_trans_timer(#conn_data{auto_ack     = true, 
			      trans_ack    = true,
			      trans_sender = undefined} = CD, 
		   To) when To > 0 ->
    #conn_data{conn_handle        = CH, 
	       trans_ack_maxcount = AcksMax, 
	       trans_req_maxcount = ReqsMax, 
	       trans_req_maxsize  = ReqsMaxSz} = CD,
    {ok, Pid} = megaco_trans_sup:start_trans_sender(CH, To, ReqsMaxSz, 
						    ReqsMax, AcksMax),

    %% Make sure we are notified when/if the transaction 
    %% sender goes down. 
    %% Do we need to store the ref? Will we ever need to 
    %% cancel this (apply_at_exit)?
    megaco_monitor:apply_at_exit(?MODULE, trans_sender_exit, [CH], Pid),

    CD#conn_data{trans_timer = To, trans_sender = Pid};

update_trans_timer(#conn_data{trans_req    = true, 
			      trans_sender = undefined} = CD, 
		   To) when To > 0 ->
    #conn_data{conn_handle        = CH, 
	       trans_ack_maxcount = AcksMax, 
	       trans_req_maxcount = ReqsMax, 
	       trans_req_maxsize  = ReqsMaxSz} = CD,
    {ok, Pid} = megaco_trans_sup:start_trans_sender(CH, To, ReqsMaxSz, 
						    ReqsMax, AcksMax),

    %% Make sure we are notified when/if the transaction 
    %% sender goes down. 
    %% Do we need to store the ref? Will we ever need to 
    %% cancel this (apply_at_exit)?
    megaco_monitor:apply_at_exit(?MODULE, trans_sender_exit, [CH], Pid),

    CD#conn_data{trans_timer = To, trans_sender = Pid};

update_trans_timer(#conn_data{trans_sender = Pid} = CD, 0) when is_pid(Pid) ->
    megaco_trans_sender:stop(Pid),
    CD#conn_data{trans_timer = 0, trans_sender = undefined};

update_trans_timer(#conn_data{trans_sender = Pid} = CD, To) 
  when is_pid(Pid) and (To > 0) ->
    megaco_trans_sender:timeout(Pid, To),
    CD#conn_data{trans_timer = To};

update_trans_timer(CD, To) when To > 0 ->
    CD#conn_data{trans_timer = To}.

%% update trans_ack_maxcount
update_trans_ack_maxcount(#conn_data{trans_sender = Pid} = CD, Max) 
  when is_pid(Pid) and (Max > 0) ->
    megaco_trans_sender:ack_maxcount(Pid, Max),
    CD#conn_data{trans_ack_maxcount = Max};

update_trans_ack_maxcount(CD, Max) 
  when Max > 0 ->
    ?d("update_trans_ack_maxcount -> entry with ~p", [Max]),
    CD#conn_data{trans_ack_maxcount = Max}.

%% update trans_req_maxcount
update_trans_req_maxcount(#conn_data{trans_sender = Pid} = CD, Max) 
  when is_pid(Pid) and (Max > 0) ->
    megaco_trans_sender:req_maxcount(Pid, Max),
    CD#conn_data{trans_req_maxcount = Max};

update_trans_req_maxcount(CD, Max) 
  when Max > 0 ->
    ?d("update_trans_req_maxcount -> entry with ~p", [Max]),
    CD#conn_data{trans_req_maxcount = Max}.

%% update trans_req_maxsize
update_trans_req_maxsize(#conn_data{trans_sender = Pid} = CD, Max) 
  when is_pid(Pid) and (Max > 0) ->
    megaco_trans_sender:req_maxsize(Pid, Max),
    CD#conn_data{trans_req_maxsize = Max};

update_trans_req_maxsize(CD, Max) 
  when Max > 0 ->
    ?d("update_trans_req_maxsize -> entry with ~p", [Max]),
    CD#conn_data{trans_req_maxsize = Max}.

    

handle_connect(RH, RemoteMid, SendHandle, ControlPid, Auto) ->
    LocalMid   = RH#megaco_receive_handle.local_mid,
    ConnHandle = #megaco_conn_handle{local_mid  = LocalMid,
				     remote_mid = RemoteMid},
    ?d("handle_connect -> entry with"
	"~n   ConnHandle: ~p", [ConnHandle]),
    case ets:lookup(megaco_local_conn, ConnHandle) of
        [] ->
	    PrelMid = preliminary_mid,
	    PrelHandle = ConnHandle#megaco_conn_handle{remote_mid = PrelMid},
	    case ets:lookup(megaco_local_conn, PrelHandle) of
		[] ->
		    case (catch init_conn_data(RH, 
					       RemoteMid, SendHandle, 
					       ControlPid, Auto)) of
			{'EXIT', _Reason} ->
			    ?d("handle_connect -> init conn data failed: "
				"~n   ~p",[_Reason]),
			    {error, {no_such_user, LocalMid}};
			ConnData ->
			    ?d("handle_connect -> new connection"
				"~n   ConnData: ~p", [ConnData]),
			    %% Brand new connection, use 
			    %% When is the preliminary_mid used?
			    create_snmp_counters(ConnHandle),
			    %% Maybe start transaction sender
			    ConnData2 = trans_sender_start(ConnData),
			    ets:insert(megaco_local_conn, ConnData2),
			    {ok, ConnData2}
		    end;
		[PrelData] ->
		    ?d("handle_connect -> connection upgrade"
			"~n   PrelData: ~p", [PrelData]),
		    %% OK, we need to fix the snmp counters. Used 
		    %% with the temporary (preliminary_mid) conn_handle.
		    create_snmp_counters(ConnHandle),
		    ConnData = PrelData#conn_data{conn_handle = ConnHandle},
		    trans_sender_upgrade(ConnData),
		    ets:insert(megaco_local_conn, ConnData),
		    ets:delete(megaco_local_conn, PrelHandle),
		    update_snmp_counters(ConnHandle, PrelHandle),
		    TH = ConnHandle#megaco_conn_handle{local_mid  = PrelMid,
						       remote_mid = RemoteMid},
		    TD = ConnData#conn_data{conn_handle = TH},
 		    ?report_debug(TD, 
				  "Upgrade preliminary_mid to "
				  "actual remote_mid",
				  [{preliminary_mid, preliminary_mid},
				   {local_mid,       LocalMid},
				   {remote_mid,      RemoteMid}]),
		    {ok, ConnData}
	    end;
        [_ConnData] ->
            {error, {already_connected, ConnHandle}}
    end.

handle_finish_connect(ConnHandle, SendHandle, ControlPid, MFA) ->
    case (catch ets:lookup(megaco_local_conn, ConnHandle)) of
	[#conn_data{monitor_ref = connected} = CD] ->
	    {M, F, A} = MFA,
	    Ref = megaco_monitor:apply_at_exit(M, F, A, ControlPid),
	    ConnData2 = CD#conn_data{monitor_ref = Ref,
				     control_pid = ControlPid,
				     send_handle = SendHandle},
	    ets:insert(megaco_local_conn, ConnData2),
	    {ok, Ref};
	[#conn_data{monitor_ref = Ref}] ->
	    {ok, Ref};
	[] ->
	    {error, {no_such_connection, ConnHandle}}
    end.
    

%% also trans_req == true
trans_sender_start(#conn_data{conn_handle        = CH,
			      auto_ack           = true, 
			      trans_ack          = true, 
			      trans_ack_maxcount = AcksMax, 
			      trans_req_maxcount = ReqsMax, 
			      trans_req_maxsize  = ReqsMaxSz,
			      trans_timer        = To,
			      trans_sender       = undefined} = CD)
  when To > 0 ->

    ?d("trans_sender_start(ack) -> entry when"
	"~n   CH:        ~p"
	"~n   To:        ~p"
	"~n   AcksMax:   ~p"
	"~n   ReqsMax:   ~p"
	"~n   ReqsMaxSz: ~p", [CH, To, ReqsMaxSz, ReqsMax, AcksMax]),

    {ok, Pid} = megaco_trans_sup:start_trans_sender(CH, To, ReqsMaxSz, 
						    ReqsMax, AcksMax),

    ?d("trans_sender_start(ack) -> Pid: ~p", [Pid]),

    %% Make sure we are notified when/if the transaction 
    %% sender goes down. 
    %% Do we need to store the ref? Will we ever need to 
    %% cancel this (apply_at_exit)?
    megaco_monitor:apply_at_exit(?MODULE, trans_sender_exit, [CH], Pid),

    CD#conn_data{trans_sender = Pid};

trans_sender_start(#conn_data{conn_handle        = CH,
			      trans_req          = true, 
			      trans_ack_maxcount = AcksMax, 
			      trans_req_maxcount = ReqsMax, 
			      trans_req_maxsize  = ReqsMaxSz,
			      trans_timer        = To,
			      trans_sender       = undefined} = CD)
  when To > 0 ->

    ?d("trans_sender_start(req) -> entry when"
	"~n   CH:        ~p"
	"~n   To:        ~p"
	"~n   AcksMax:   ~p"
	"~n   ReqsMax:   ~p"
	"~n   ReqsMaxSz: ~p", [CH, To, ReqsMaxSz, ReqsMax, AcksMax]),

    {ok, Pid} = megaco_trans_sup:start_trans_sender(CH, To, ReqsMaxSz, 
						    ReqsMax, AcksMax),

    ?d("trans_sender_start(req) -> Pid: ~p", [Pid]),

    %% Make sure we are notified when/if the transaction 
    %% sender goes down. 
    %% Do we need to store the ref? Will we ever need to 
    %% cancel this (apply_at_exit)?
    megaco_monitor:apply_at_exit(?MODULE, trans_sender_exit, [CH], Pid),

    CD#conn_data{trans_sender = Pid};

trans_sender_start(CD) ->
    ?d("trans_sender_start -> undefined", []),
    CD#conn_data{trans_sender = undefined}.

trans_sender_upgrade(#conn_data{conn_handle  = CH,
				trans_sender = Pid})
  when is_pid(Pid) ->
    ?d("trans_sende_upgrade -> entry when"
	"~n   CH:  ~p"
	"~n   Pid: ~p", [CH, Pid]),
    megaco_trans_sender:upgrade(Pid, CH);
trans_sender_upgrade(_CD) ->
    ok.


handle_connect_remote(ConnHandle, UserNode, Ref) ->
    Pat = #remote_conn_data{conn_handle = ConnHandle,
			    user_node   = UserNode,
			    monitor_ref = '_'},
    case ets:match_object(megaco_remote_conn, Pat) of
        [] ->
	    RCD = #remote_conn_data{conn_handle = ConnHandle,
				    user_node   = UserNode,
				    monitor_ref = Ref},
            ets:insert(megaco_remote_conn, RCD),
            ok;
        _ ->
            {error, {already_connected, ConnHandle, UserNode}}
    end.

init_conn_data(RH, RemoteMid, SendHandle, ControlPid) ->
    init_conn_data(RH, RemoteMid, SendHandle, ControlPid, auto).
init_conn_data(RH, RemoteMid, SendHandle, ControlPid, Auto) ->
    Mid            = RH#megaco_receive_handle.local_mid,
    ConnHandle     = #megaco_conn_handle{local_mid  = Mid,
					 remote_mid = RemoteMid},
    EncodingMod    = RH#megaco_receive_handle.encoding_mod,
    EncodingConfig = RH#megaco_receive_handle.encoding_config,
    SendMod        = RH#megaco_receive_handle.send_mod,
    MonitorRef     = 
	case Auto of
	    auto ->
		undefined_auto_monitor_ref;
	    {plain, ConnectorPid} ->
		{connecting, ConnectorPid}
	end,
    #conn_data{conn_handle          = ConnHandle,
               serial               = undefined_serial,
               max_serial           = user_info(Mid, max_trans_id),
               request_timer        = user_info(Mid, request_timer),
               long_request_timer   = user_info(Mid, long_request_timer),

               auto_ack             = user_info(Mid, auto_ack),
               trans_ack            = user_info(Mid, trans_req),
               trans_req            = user_info(Mid, trans_req),

	       trans_timer          = user_info(Mid, trans_timer),
	       trans_req_maxsize    = user_info(Mid, trans_req_maxsize),
	       trans_req_maxcount   = user_info(Mid, trans_req_maxcount),
	       trans_ack_maxcount   = user_info(Mid, trans_ack_maxcount),

               pending_timer        = user_info(Mid, pending_timer),
               sent_pending_limit   = user_info(Mid, sent_pending_limit),
               recv_pending_limit   = user_info(Mid, recv_pending_limit),
               reply_timer          = user_info(Mid, reply_timer),
               control_pid          = ControlPid,
               monitor_ref          = MonitorRef,
               send_mod             = SendMod,
               send_handle          = SendHandle,
               encoding_mod         = EncodingMod,
               encoding_config      = EncodingConfig,
               protocol_version     = user_info(Mid, protocol_version),
               auth_data            = user_info(Mid, auth_data),
               user_mod             = user_info(Mid, user_mod),
               user_args            = user_info(Mid, user_args),
               reply_action         = undefined,
               reply_data           = user_info(Mid, reply_data),
	       threaded             = user_info(Mid, threaded),
	       strict_version       = user_info(Mid, strict_version),
	       long_request_resend  = user_info(Mid, long_request_resend),
	       call_proxy_gc_timeout = user_info(Mid, call_proxy_gc_timeout),
	       cancel               = false,
	       resend_indication    = user_info(Mid, resend_indication),
	       segment_reply_ind    = user_info(Mid, segment_reply_ind),
	       segment_recv_acc     = user_info(Mid, segment_recv_acc),
	       segment_recv_timer   = user_info(Mid, segment_recv_timer),
	       segment_send         = user_info(Mid, segment_send),
	       segment_send_timer   = user_info(Mid, segment_send_timer),
	       max_pdu_size         = user_info(Mid, max_pdu_size),
	       request_keep_alive_timeout = user_info(Mid, request_keep_alive_timeout)
	      }.

handle_disconnect(ConnHandle) when is_record(ConnHandle, megaco_conn_handle) ->
    case ets:lookup(megaco_local_conn, ConnHandle) of
        [ConnData] ->
	    ets:delete(megaco_local_conn, ConnHandle),
	    RemoteConnData = handle_disconnect_remote(ConnHandle, '_'),
            {ok, ConnData, RemoteConnData};
        [] ->
            {error, {already_disconnected, ConnHandle}}
    end.

handle_disconnect_remote(ConnHandle, UserNode) ->
    Pat = #remote_conn_data{conn_handle = ConnHandle,
			    user_node   = UserNode,
			    monitor_ref = '_'},
    RemoteConnData = ets:match_object(megaco_remote_conn, Pat),
    ets:match_delete(megaco_remote_conn, Pat),
    RemoteConnData.

make_receive_handle(UserMid) ->
    #megaco_receive_handle{local_mid       = UserMid,
			   encoding_mod    = user_info(UserMid, encoding_mod),
			   encoding_config = user_info(UserMid, encoding_config),
			   send_mod        = user_info(UserMid, send_mod)}.


%%-----------------------------------------------------------------
%% Func: create_snmp_counters/1, update_snmp_counters/2
%% Description: create/update all the SNMP statistic counters
%%-----------------------------------------------------------------

create_snmp_counters(CH) ->
    create_snmp_counters(CH, snmp_counters()).

% create_snmp_counters(CH, []) ->
%     ok;
% create_snmp_counters(CH, [Counter|Counters]) ->
%     Key = {CH, Counter},
%     ets:insert(megaco_stats, {Key, 0}),
%     create_snmp_counters(CH, Counters).

create_snmp_counters(CH, Counters) ->
    F = fun(Counter) -> 
		Key = {CH, Counter},
		ets:insert(megaco_stats, {Key, 0}) 
	end,
    lists:foreach(F, Counters).


update_snmp_counters(CH, PrelCH) ->
    update_snmp_counters(CH, PrelCH, snmp_counters()).

update_snmp_counters(_CH, _PrelCH, []) ->
    ok;
update_snmp_counters(CH, PrelCH, [Counter|Counters]) ->
    PrelKey = {PrelCH, Counter},
    Key     = {CH, Counter},
    [{PrelKey,PrelVal}] = ets:lookup(megaco_stats, PrelKey),
    ets:update_counter(megaco_stats, Key, PrelVal),
    ets:delete(megaco_stats, PrelKey),
    update_snmp_counters(CH, PrelCH, Counters).


global_snmp_counters() ->
    [medGwyGatewayNumErrors].

snmp_counters() ->
    [medGwyGatewayNumTimerRecovery,
     medGwyGatewayNumErrors].



%%-----------------------------------------------------------------

%% Time in milli seconds
%% t() ->
%%     {A,B,C} = erlang:now(),
%%     A*1000000000+B*1000+(C div 1000).


%%-----------------------------------------------------------------

warning_msg(F, A) ->
    ?megaco_warning("Config server: " ++ F, A).

error_msg(F, A) ->
    ?megaco_error("Config server: " ++ F, A).

