%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
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
%% Purpose : Main API for Megaco/H.248 protocol stack
%%----------------------------------------------------------------------

-module(megaco).

%%-----------------------------------------------------------------
%% Public interface
%%-----------------------------------------------------------------

-export([
         start/0,
         stop/0,
        
         start_user/2,
         stop_user/1,

	 info/0, 
         user_info/1, user_info/2,
         update_user_info/3,
         conn_info/1, conn_info/2,
         update_conn_info/3,
         system_info/0, system_info/1,

         connect/4, connect/5, 
         disconnect/2,

         call/3,
         cast/3,
         cancel/2,
         process_received_message/4, process_received_message/5,
         receive_message/4, receive_message/5,

	 encode_actions/3,

	 token_tag2string/1, token_tag2string/2, token_tag2string/3, 

	 parse_digit_map/1,
	 eval_digit_map/1,
	 eval_digit_map/2,
	 report_digit_event/2,
	 test_digit_event/2,

	 encode_binary_term_id/2,
	 decode_binary_term_id/2,

	 encode_sdp/1,
	 decode_sdp/1,
         get_sdp_record_from_PropertyGroup/2,

	 versions1/0, versions2/0, 
	 print_version_info/0, print_version_info/1, 
	 ms/0, nc/0, nc/1, ni/0, ni/1,

	 enable_trace/2, disable_trace/0, set_trace/1,

	 report_event/4, report_event/5,

	 test_request/5,
	 test_reply/5
        ]).

-export([
	 get_stats/0, get_stats/1, get_stats/2,
	 reset_stats/0, reset_stats/1
	]).

%% Internal
-export([
         %% These are used both for debugging (verbosity printouts)
         %% and other such "utility" operations (and testing).
         format_timestamp/1, format_timestamp/2, 
         format_short_timestamp/1, format_short_timestamp/2, 
         format_long_timestamp/1, format_long_timestamp/2,
         formated_timestamp/0, 
         formated_short_timestamp/0, 
         formated_long_timestamp/0
        ]).

-export_type([
              void/0,

              mid/0,
              receive_handle/0,
              conn_handle/0,
              action_request/0,
              action_reply/0,
              error_desc/0,
              transaction_reply/0,
              protocol_version/0,

              digit_map_value/0,
              digit_map_kind/0,
              digit_map_event/0,
              digit_map_letter/0,

              property_parm/0,
              property_group/0,
              property_groups/0,
              sdp/0,

              trace_level/0
             ]).

-type void() :: term().

-type mid()               :: megaco_encoder:ip4Address() |
                             megaco_encoder:ip6Address() |
                             megaco_encoder:domainName() |
                             megaco_encoder:deviceName() |
                             megaco_encoder:mtpAddress().
-type megaco_message()    :: megaco_encoder:megaco_message().
-type action_request()    :: megaco_encoder:action_request().
-type action_reply()      :: megaco_encoder:action_reply().
-type error_desc()        :: megaco_encoder:error_desc().
-type transaction_reply() :: megaco_encoder:transaction_reply().
-type protocol_version()  :: megaco_encoder:protocol_version().
-type segment_no()        :: megaco_encoder:segment_no().
-type receive_handle()    :: megaco_user:receive_handle().
-type conn_handle()       :: megaco_user:conn_handle().
-type megaco_timer()      :: megaco_user:megaco_timer().
-type transaction_id()    :: pos_integer().

-type action_reqs()    :: binary() | [action_request()].
-type action_reps()    :: [action_reply()].
-type conn_info_item() :: control_pid |
                          send_handle |
                          local_mid   |
                          remote_mid  |
                          receive_handle |
                          trans_id       |
                          max_trans_id   |
                          request_timer  |
                          long_request_timer         |
                          request_keep_alive_timeout |
                          long_request_resend        |
                          reply_timer           |
                          call_proxy_gc_timeout |
                          auto_ack  |
                          trans_ack |
                          trans_ack_maxcount |
                          trans_req          |
                          trans_req_maxcount |
                          trans_req_maxsize  |
                          trans_timer        |
                          pending_timer      |
                          sent_pending_limit |
                          recv_pending_limit |
                          send_mod           |
                          encoding_mod       |
                          encoding_config    |
                          protocol_version   |
                          strict_version     |
                          reply_data         |
                          threaded           |
                          resend_indication  |
                          segment_reply_ind  |
                          segment_recv_timer |
                          segment_send       |
                          max_pdu_size.
-type user_info_item() :: connections    |
                          receive_handle |
                          trans_id       |
                          min_trans_id   |
                          max_trans_id   |
                          request_timer  |
                          long_request_timer  |
                          long_request_resend |
                          reply_timer    |
                          request_keep_alive_timeout |
                          call_proxy_gc_timeout      |
                          auto_ack           |
                          trans_ack          |
                          trans_ack_maxcount |
                          trans_req          |
                          trans_req_maxcount |
                          trans_req_maxsize  |
                          trans_timer        |
                          pending_timer      |
                          sent_pending_limit |
                          recv_pending_limit |
                          send_mod           |
                          encoding_mod       |
                          encoding_config    |
                          protocol_version   |
                          strict_version     |
                          reply_data         |
                          user_mod           |
                          user_args          |
                          threaded           |
                          resend_indication  |
                          segment_reply_ind  |
                          segment_recv_timer | 
                          segment_send       |
                          max_pdu_size.

-type send_handle()          :: term().

-type digit_map_value()      :: megaco_digit_map:value().
-type digit_map_kind()       :: megaco_digit_map:kind().
-type digit_map_event()      :: megaco_digit_map:event().
-type digit_map_letter()     :: megaco_digit_map:letter().


-type property_parm()       :: megaco_sdp:property_parm().
-type property_group()      :: megaco_sdp:property_group().
-type property_groups()     :: megaco_sdp:property_groups().
-type sdp()                 :: megaco_sdp:sdp().
-type sdp_property_parm()   :: megaco_sdp:sdp_property_parm().
-type sdp_property_group()  :: megaco_sdp:sdp_property_group().
-type sdp_property_groups() :: megaco_sdp:sdp_property_groups().

-type trace_level()       :: min | max | 0..100.
-type trace_event()       :: term().
-type trace_data()        :: term().
-type trace_handler()     :: fun((trace_event(), trace_data()) -> trace_data()).

-type global_counter() :: medGwyGatewayNumErrors.
-type counter()        :: medGwyGatewayNumTimerRecovery |
                          medGwyGatewayNumErrors.
-type counter_value()  :: non_neg_integer().

-type system_info_item() :: text_config          |
                            connections          |
                            users                |
                            n_active_requests    |
                            n_active_replies     |
                            n_active_connections |
                            reply_counters       |
                            pending_counters.


-include("megaco_internal.hrl").


%%-----------------------------------------------------------------
%% Starts the Megaco application
%%-----------------------------------------------------------------

-spec start() -> ok | {error, Reason} when
      Reason :: term().

start() ->
    application:start(?APPLICATION).


%%-----------------------------------------------------------------
%% Stops the Megaco application
%%-----------------------------------------------------------------

-spec stop() -> ok | {error, Reason} when
      Reason :: term().

stop() ->
    application:stop(?APPLICATION).


%%-----------------------------------------------------------------
%% Initial configuration of a user
%%-----------------------------------------------------------------

-spec start_user(UserMid, Config) -> ok | {error, Reason} when
      UserMid :: mid(),
      Config  :: [{Item, Value}],
      Item    :: user_info_item(),
      Value   :: term(),
      Reason  :: term().

start_user(UserMid, Config) ->
    megaco_config:start_user(UserMid, Config).


%%-----------------------------------------------------------------
%% Delete the configuration of a user
%%-----------------------------------------------------------------

-spec stop_user(UserMid) -> ok | {error, Reason} when
      UserMid :: mid(),
      Reason  :: term().

stop_user(UserMid) ->
    megaco_config:stop_user(UserMid).


%%-----------------------------------------------------------------
%% Lookup user information
%%-----------------------------------------------------------------

-spec user_info(UserMid) -> [{Item, Value}] when
      UserMid :: mid(),
      Item    :: requests | replies | user_info_item(),
      Value   :: term().

user_info(UserMid) ->
    [{requests, user_info(UserMid, requests)},
     {replies,  user_info(UserMid, replies)} | user_info(UserMid, all)].

-spec user_info(UserMid, requests) -> [{Conn, [TransId]}] when
      UserMid :: mid(),
      Conn    :: conn_handle(),
      TransId :: transaction_id();

               (UserMid, replies) ->
          [{Conn, [{TransId, ReplyState, Handler}]}] when
      UserMid    :: mid(),
      Conn       :: conn_handle(),
      TransId    :: transaction_id(),
      ReplyState :: prepare | eval_request | waiting_for_ack | aborted,
      Handler    :: undefined | pid();

               (UserMid, Item) -> Value when
      UserMid :: mid(),
      Item    :: user_info_item(),
      Value   :: term().

user_info(UserMid, requests) ->
    megaco_messenger:which_requests(UserMid);
user_info(UserMid, replies) ->
    megaco_messenger:which_replies(UserMid);
user_info(UserMid, Item) ->
    megaco_config:user_info(UserMid, Item).


%%-----------------------------------------------------------------
%% Update information about a user
%%-----------------------------------------------------------------

-spec update_user_info(UserMid, Item, Value) -> ok | {error, Reason} when
      UserMid :: mid(),
      Item    :: user_info_item(),
      Value   :: term(),
      Reason  :: term().

update_user_info(UserMid, Item, Value) ->
    megaco_config:update_user_info(UserMid, Item, Value).


%%-----------------------------------------------------------------
%% Lookup information about an active connection
%%-----------------------------------------------------------------

-spec conn_info(ConnHandle) -> [{Item, Value}] when
      ConnHandle :: conn_handle(),
      Item       :: requests | replies | conn_info_item(),
      Value      :: term().
      
conn_info(ConnHandle) ->
    [{requests, conn_info(ConnHandle, requests)},
     {replies,  conn_info(ConnHandle, replies)} | conn_info(ConnHandle, all)].


-spec conn_info(ConnHandle, all) -> [{Item, Value}] when
      ConnHandle :: conn_handle(),
      Item       :: conn_info_item(),
      Value      :: term();

               (ConnHandle, requests) -> [TransId] when
      ConnHandle :: conn_handle(),
      TransId    :: transaction_id();

               (ConnHandle, replies) ->
          [{TransId, ReplyState, Handler}] when
      ConnHandle :: conn_handle(),
      TransId    :: transaction_id(),
      ReplyState :: prepare | eval_request | waiting_for_ack | aborted,
      Handler    :: undefined | pid();

               (ConnHandle, Item) -> Value when
      ConnHandle :: conn_handle(),
      Item       :: conn_info_item(),
      Value      :: term().

conn_info(ConnHandle, all = Item) ->
    megaco_config:conn_info(ConnHandle, Item);

conn_info(ConnHandle, requests = _Item) ->
    megaco_messenger:which_requests(ConnHandle);

conn_info(ConnHandle, replies) ->
    megaco_messenger:which_replies(ConnHandle);

conn_info(ConnHandle, Item) ->
    megaco_config:conn_info(ConnHandle, Item).


%%-----------------------------------------------------------------
%% Update information about an active connection
%%-----------------------------------------------------------------

-spec update_conn_info(ConnHandle, Item, Value) -> ok | {error, Reason} when
      ConnHandle :: conn_handle(),
      Item       :: conn_info_item(),
      Value      :: term(),
      Reason     :: term().

update_conn_info(ConnHandle, Item, Value) ->
    megaco_config:update_conn_info(ConnHandle, Item, Value).


%%-----------------------------------------------------------------
%% All information for the application
%%-----------------------------------------------------------------

-spec info() -> Info when
      Info  :: [{Key, Value}],
      Key   :: atom(),
      Value :: term().

info() ->
    Stats = 
	case get_stats() of
	    {ok, Statistics} ->
		Statistics;
	    _ ->
		[]
	end,
    SysInfo = system_info(),
    [{statistics, Stats} | info(SysInfo)].

info(SysInfo) ->
    info(SysInfo, []).

info([], Acc) ->
    lists:reverse(Acc);
info([{connections, Conns} | SysInfo], Acc) ->
    Conns2 = extend_conns_info(Conns),
    info(SysInfo, [{connections, Conns2} | Acc]);
info([{users, Users} | SysInfo], Acc) ->
    Users2 = extend_users_info(Users),
    info(SysInfo, [{users, Users2} | Acc]);
info([Info | SysInfo], Acc) ->
    info(SysInfo, [Info | Acc]).

extend_conns_info(Conns) ->
    extend_conns_info(Conns, []).

extend_conns_info([], Acc) ->
    lists:reverse(Acc);
extend_conns_info([Conn | Conns], Acc) ->
    ConnInfo = conn_info(Conn),
    extend_conns_info(Conns, [{Conn, ConnInfo} | Acc]).

extend_users_info(Users) ->
    extend_users_info(Users, []).

extend_users_info([], Acc) ->
    lists:reverse(Acc);
extend_users_info([User | Users], Acc) ->
    UserInfo = user_info(User),
    extend_users_info(Users, [{User, UserInfo} | Acc]).


%%-----------------------------------------------------------------
%% Lookup system information
%%-----------------------------------------------------------------

-spec system_info_items() -> [Item] when
      Item :: system_info_item().

system_info_items() ->
    [
     text_config, 
     connections, 
     users, 
     n_active_requests, 
     n_active_replies, 
     n_active_connections,
     reply_counters,
     pending_counters
    ].

-spec system_info() -> [{Item, Value}] when
      Item  :: system_info_item(),
      Value :: term().
      
system_info() ->
    [{Item, system_info(Item)} || Item <- system_info_items()].

-spec system_info(Item) -> Value when
      Item  :: system_info_item(),
      Value :: term().
      
system_info(Item) ->
    megaco_config:system_info(Item).


%%-----------------------------------------------------------------
%% Establish a "virtual" connection
%%-----------------------------------------------------------------

-spec connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid) ->
          {ok, ConnHandle} | {error, Reason} when
      ReceiveHandle       :: receive_handle(),
      RemoteMid           :: preliminary_mid | mid(),
      SendHandle          :: send_handle(),
      ControlPid          :: pid(),
      ConnHandle          :: conn_handle(),
      Reason              :: ConnectReason | HandleConnectReason | term(),
      ConnectReason       :: {no_such_user,      LocalMid} |
                             {already_connected, ConnHandle} | term(),
      LocalMid            :: mid(),
      HandleConnectReason :: {connection_refused, ConnData, ErrorInfo} | term(),
      ConnData            :: term(),
      ErrorInfo           :: term().      

connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid) ->
    megaco_messenger:connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid).

-spec connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid, Extra) ->
          {ok, ConnHandle} | {error, Reason} when
      ReceiveHandle       :: receive_handle(),
      RemoteMid           :: preliminary_mid | mid(),
      SendHandle          :: send_handle(),
      ControlPid          :: pid(),
      Extra               :: term(),
      ConnHandle          :: conn_handle(),
      Reason              :: ConnectReason | HandleConnectReason | term(),
      ConnectReason       :: {no_such_user,      LocalMid} |
                             {already_connected, ConnHandle} | term(),
      LocalMid            :: mid(),
      HandleConnectReason :: {connection_refused, ConnData, ErrorInfo} | term(),
      ConnData            :: term(),
      ErrorInfo           :: term().      

connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid, Extra) 
  when (Extra =/= ?default_user_callback_extra) ->
    megaco_messenger:connect(ReceiveHandle, RemoteMid, SendHandle, 
			     ControlPid, Extra).


%%-----------------------------------------------------------------
%% Tear down a "virtual" connection
%%-----------------------------------------------------------------

-spec disconnect(ConnHandle, DiscoReason) -> ok | {error, ErrReason} when
      ConnHandle  :: conn_handle(),
      DiscoReason :: term(),
      ErrReason   :: term().

disconnect(ConnHandle, Reason) ->
    megaco_messenger:disconnect(ConnHandle, {user_disconnect, Reason}).


%%-----------------------------------------------------------------
%% Sends a transaction request and waits for a reply
%%-----------------------------------------------------------------

-spec call(ConnHandle, ActionRequests, SendOptions) ->
          {ProtocolVersion, UserReply | [UserReply]} when
      ConnHandle          :: conn_handle(),
      ActionRequests      :: action_reqs() | [action_reqs()],
      SendOptions         :: [SendOption],
      SendOption          :: {request_timer,         megaco_timer()} |
                             {long_request_timer,    megaco_timer()} |
                             {send_handle,           send_handle()} |
                             {protocol_version,      protocol_version()} |
                             {call_proxy_gc_timeout, non_neg_integer()},
      ProtocolVersion     :: protocol_version(),
      UserReply           :: Success | Failure,
      Success             :: {ok, Result} | {ok, Result, SuccessExtra},
      Result              :: MessageResult | SegmentResult,
      MessageResult       :: action_reps(),
      SegmentResult       :: SegmentsOk,
      SegmentsOk          :: [{segment_no(), action_reps()}],
      Failure             :: {error, Reason} | {error, Reason, ErrorExtra},
      Reason              :: MessageReason | SegmentReason |
                             UserCancelReason | SendReason | OtherReason,
      MessageReason       :: error_desc(),
      SegmentReason       :: {segment, SegmentsOk, SegmentsErr} |
                             {segment_timeout,
                              MissingSegments,
                              SegmentsOk,
                              SegmentsErr},
      SegmentsErr         :: {segment_no(), error_desc()},
      MissingSegments     :: [segment_no()],
      UserCancelReason    :: {user_cancel, ReasonForUserCancel},
      ReasonForUserCancel :: term(),
      SendReason          :: SendCancelledReason | SendFailedReason,
      SendCancelledReason :: {send_message_cancelled, term()},
      SendFailedReason    :: {send_message_failed,    term()},
      OtherReason         :: {wrong_mid,
                              WrongMid :: mid(),
                              RightMid :: mid(),
                              transaction_reply()} | term(),
      SuccessExtra        :: term(),
      ErrorExtra          :: term().

call(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:call(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Sends a transaction request but does NOT wait for a reply
%%-----------------------------------------------------------------

-spec cast(ConnHandle, ActionRequests, SendOptions) ->
          ok | {error, Reason} when
      ConnHandle      :: conn_handle(),
      ActionRequests  :: action_reqs() | [action_reqs()],
      SendOptions     :: [SendOption],
      SendOption      :: {request_keep_alive_timeout, RequestKeepAliveTimer} |
                         {request_timer,              megaco_timer()} |
                         {long_request_timer,         megaco_timer()} |
                         {send_handle,                send_handle()} |
                         {reply_data,                 ReplyData} |
                         {protocol_version,           ProtocolVersion},
      RequestKeepAliveTimer :: plain | non_neg_integer(),
      ReplyData       :: term(),
      ProtocolVersion :: protocol_version(),
      Reason          :: term().

cast(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:cast(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Test the validity of the actions
%%-----------------------------------------------------------------
    
-spec test_request(ConnHandle, Version,
                   EncodingMod, EncodingConfig,
                   ActionRequests) -> {MegaMsg, EncodeRes} when
      ConnHandle     :: conn_handle(),
      Version        :: protocol_version(),
      EncodingMod    :: module(),
      EncodingConfig :: list(),
      ActionRequests :: action_reqs() | [action_reqs()],
      MegaMsg        :: megaco_message(),
      EncodeRes      :: {ok, Bin} | {error, Reason},
      Bin            :: binary(),
      Reason         :: term().

test_request(ConnHandle, Version, EncodingMod, EncodingConfig, 
	     ActionRequests) ->
    megaco_messenger:test_request(ConnHandle, ActionRequests, 
				  Version, EncodingMod, EncodingConfig).


%% This tests the actual_reply() type of return from the 
%% handle_trans_request function.
%%

-spec test_reply(ConnHandle, Version,
                 EncodingMod, EncodingConfig,
                 Reply) -> {MegaMsg, EncodeRes} when
      ConnHandle     :: conn_handle(),
      Version        :: protocol_version(),
      EncodingMod    :: module(),
      EncodingConfig :: list(),
      Reply          :: error_desc() | [action_reply()],
      MegaMsg        :: megaco_message(),
      EncodeRes      :: {ok, Bin} | {error, Reason},
      Bin            :: binary(),
      Reason         :: term().

test_reply(ConnHandle, Version, EncodingMod, EncodingConfig, 
	   Reply) ->
    megaco_messenger:test_reply(ConnHandle, Version, 
				EncodingMod, EncodingConfig, Reply).


%%-----------------------------------------------------------------
%% Func: get_stats/0, get_stats/1, get_stats/2
%% Description: Retreive statistics (counters) for TCP
%%-----------------------------------------------------------------

-spec get_stats() -> {ok, [TotalStats]} | {error, Reason} when
      TotalStats :: {conn_handle(),    [Stats]} |
                    {global_counter(), counter_value()},
      Stats      :: {counter(),        counter_value()},
      Reason     :: term().
      
get_stats() ->
    megaco_messenger:get_stats().

-spec get_stats(GCounter) -> {ok, Value} | {error, Reason} when
      GCounter :: global_counter(),
      Value    :: counter_value(),
      Reason   :: term();
               (ConnHandle) -> {ok, [Stats]} | {error, Reason} when
      ConnHandle :: conn_handle(),
      Stats      :: {counter(), counter_value()},
      Reason     :: term().

get_stats(ConnHandleOrGCounter) ->
    megaco_messenger:get_stats(ConnHandleOrGCounter).

-spec get_stats(ConnHandle, Counter) -> {ok, Value} | {error, Reason} when
      ConnHandle :: conn_handle(),
      Counter    :: counter(),
      Value      :: counter_value(),
      Reason     :: term().

get_stats(ConnHandle, Counter) ->
    megaco_messenger:get_stats(ConnHandle, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters) for TCP
%%-----------------------------------------------------------------

-spec reset_stats() -> void().

reset_stats() ->
    megaco_messenger:reset_stats().

-spec reset_stats(GCounter) -> void() when
      GCounter :: global_counter();
               (ConnHandle) -> void() when
      ConnHandle :: conn_handle().

reset_stats(ConnHandleOrGCounter) ->
    megaco_messenger:reset_stats(ConnHandleOrGCounter).


%%-----------------------------------------------------------------
%% Cancel all outstanding messages for this connection
%%-----------------------------------------------------------------

-spec cancel(ConnHandle, CancelReason) -> ok | {error, Reason} when
      ConnHandle   :: conn_handle(),
      CancelReason :: term(),
      Reason       :: term().

cancel(ConnHandle, Reason) ->
    megaco_messenger:cancel(ConnHandle, {user_cancel, Reason}).


%%-----------------------------------------------------------------
%% Process a received message
%%-----------------------------------------------------------------

-spec process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary().

process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:process_received_message(ReceiveHandle, ControlPid, 
					      SendHandle, BinMsg).

-spec process_received_message(ReceiveHandle,
                               ControlPid, SendHandle, BinMsg, Extra) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary(),
      Extra         :: term().

process_received_message(ReceiveHandle,
                         ControlPid, SendHandle, BinMsg, Extra) ->
    megaco_messenger:process_received_message(ReceiveHandle, ControlPid, 
					      SendHandle, BinMsg, 
					      Extra).


-spec receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary().

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:receive_message(ReceiveHandle, ControlPid, 
				     SendHandle, BinMsg).

-spec receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary(),
      Extra         :: term().

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
    megaco_messenger:receive_message(ReceiveHandle, ControlPid, 
				     SendHandle, BinMsg,
				     Extra).


%%-----------------------------------------------------------------
%% Encode the actions list for one or more transactions.
%%-----------------------------------------------------------------

-spec encode_actions(ConnHandle, ActionRequests, Options) ->
          {ok, Result} | {error, Reason} when
      ConnHandle     :: conn_handle(),
      ActionRequests :: action_reqs() | [action_reqs()],
      Options        :: [Option],
      Option         :: {request_timer,      megaco_timer()} |
                        {long_request_timer, megaco_timer()} |
                        {send_handle,        send_handle()} |
                        {protocol_version,   protocol_version()},
      Result         :: binary() | [binary()],
      Reason         :: term().

encode_actions(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:encode_actions(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Convert the (token) tags found in a decoded message into a 
%% printable string.
%%-----------------------------------------------------------------

-spec token_tag2string(Tag) -> Result when
      Tag    :: atom(),
      Result :: string() | {error, Reason},
      Reason :: term().

token_tag2string(Tag) ->
    token_tag2string(Tag, pretty).

-spec token_tag2string(Tag, EncodingMod) -> Result when
      Tag         :: atom(),
      EncodingMod :: pretty | compact | module(),
      Result      :: string() | {error, Reason},
      Reason      :: term().

token_tag2string(Tag, pretty) ->
    token_tag2string(Tag, megaco_pretty_text_encoder);
token_tag2string(Tag, compact) ->
    token_tag2string(Tag, megaco_compact_text_encoder);
token_tag2string(Tag, Mod) when is_atom(Tag) and is_atom(Mod) ->
    Mod:token_tag2string(Tag).

-spec token_tag2string(Tag, EncodingMod, Version) -> Result when
      Tag         :: atom(),
      EncodingMod :: pretty | compact | module(),
      Version     :: protocol_version() | v1 | v2 | v3,
      Result      :: string() | {error, Reason},
      Reason      :: term().

token_tag2string(Tag, pretty, Version) ->
    token_tag2string(Tag, megaco_pretty_text_encoder, Version);
token_tag2string(Tag, compact, Version) ->
    token_tag2string(Tag, megaco_compact_text_encoder, Version);
token_tag2string(Tag, Mod, Version) when is_atom(Tag) andalso is_atom(Mod) ->
    Mod:token_tag2string(Tag, Version).


%%-----------------------------------------------------------------
%% Parses a digit map body
%%-----------------------------------------------------------------

-spec parse_digit_map(DigitMapBody) ->
          {ok, ParsedDigitMap} | {error, Reason} when
      DigitMapBody   :: string(),
      ParsedDigitMap :: term(),
      Reason         :: term().

parse_digit_map(DigitMapBody) ->
    megaco_digit_map:parse(DigitMapBody).


%%-----------------------------------------------------------------
%% Collect digit map letters according to the digit map
%%-----------------------------------------------------------------

-spec eval_digit_map(DigitMap) -> {ok, MatchResult} | {error, Reason} when
      DigitMap       :: digit_map_value() | ParsedDigitMap,
      ParsedDigitMap :: term(),
      MatchResult    :: {Kind, Letters} | {Kind, Letters, Extra},
      Kind           :: digit_map_kind(),
      Letters        :: [digit_map_letter()],
      Extra          :: digit_map_letter(),
      Reason         :: term().

eval_digit_map(DigitMap) ->
    megaco_digit_map:eval(DigitMap).

-spec eval_digit_map(DigitMap, Timers) ->
          {ok, MatchResult} | {error, Reason} when
      DigitMap       :: digit_map_value() | ParsedDigitMap,
      ParsedDigitMap :: term(),
      Timers         :: Ignore | Reject,
      Ignore         :: ignore |
                        {ignore, digit_map_value()},
      Reject         :: reject |
                        {reject, digit_map_value()} |
                        digit_map_value(),
      MatchResult    :: {Kind, Letters} | {Kind, Letters, Extra},
      Kind           :: digit_map_kind(),
      Letters        :: [digit_map_letter()],
      Extra          :: digit_map_letter(),
      Reason         :: term().

eval_digit_map(DigitMap, Timers) ->
    megaco_digit_map:eval(DigitMap, Timers).


%%-----------------------------------------------------------------
%% Send one or more events to event collector process
%%-----------------------------------------------------------------

-spec report_digit_event(DigitMapEvalPid, Events) -> ok | {error, Reason} when
      DigitMapEvalPid :: pid(),
      Events          :: digit_map_event() | [digit_map_event()],
      Reason          :: term().

report_digit_event(DigitMapEvalPid, Event) ->
    megaco_digit_map:report(DigitMapEvalPid, Event).


%%-----------------------------------------------------------------
%% Feed digit map collector with events and return the result
%%-----------------------------------------------------------------

-spec test_digit_event(DigitMap, Events) ->
          {ok,  Kind,  Letters}  |  {error, Reason} when
      DigitMap       :: digit_map_value() | ParsedDigitMap,
      ParsedDigitMap :: term(),
      Events         :: digit_map_event() | [digit_map_event()],
      Kind           :: digit_map_kind(),
      Letters        :: [digit_map_letter()],
      Reason         :: term().

test_digit_event(DigitMap, Events) ->
    megaco_digit_map:test(DigitMap, Events).


%%-----------------------------------------------------------------
%% encode_binary_term_id(Config, MegacoTermId) ->
%% 
%%   {ok, TerminationId} | {error, Reason}
%%
%% Encode the Megaco internal form of a termination id (a
%% megaco_term_id record) into ASN.1'1 internal form of a termination
%% id (a 'TerminationId' record).
%% %%-----------------------------------------------------------------

encode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:encode(Config, TermId).


%%-----------------------------------------------------------------
%% decode_binary_term_id(Config, TerminationId) ->
%% 
%%   {ok, MegacoTermId} | {error, Reason}
%%
%% Decode ASN.1's internal form of a termination id (a 'TerminationId'
%% record) into the Megaco internal form of a termination id (a
%% megaco_term_id record).
%%-----------------------------------------------------------------

decode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:decode(Config, TermId).


%%-----------------------------------------------------------------
%% encode_sdp(SDP) ->
%% 
%%   {ok, PP} | {error, Reason}
%%
%% Encode a SDP construct into a property parm construct
%%-----------------------------------------------------------------

-spec encode_sdp(SDP) -> {ok, PP} | {error, Reason} when
      SDP   :: sdp_property_parm() |
               sdp_property_group() |
               sdp_property_groups() |
               asn1_NOVALUE,
      PP    :: property_parm() |
               property_group() |
               property_groups() |
               asn1_NOVALUE,
      Reason :: term().

encode_sdp(SDP) ->
    megaco_sdp:encode(SDP).


%%-----------------------------------------------------------------
%% decode_sdp(PP) ->
%% 
%%   {ok, SDP} | {error, Reason}
%%
%% Decode a property parm construct into a SDP construct
%%-----------------------------------------------------------------

-spec decode_sdp(PP) -> {ok, SDP} | {error, Reason} when
      PP                      :: property_parm() |
                                 property_group() |
                                 property_groups() |
                                 asn1_NOVALUE,
      SDP                     :: sdp() |
                                 DecodeSdpPropertyGroup |
                                 DecodeSdpPropertyGroups |
                                 asn1_NOVALUE,
      DecodeSdpPropertyGroup  :: [DecodeSDP],
      DecodeSdpPropertyGroups :: [DecodeSdpPropertyGroup],
      DecodeSDP               :: sdp() | {property_parm(), DecodeError},
      DecodeError             :: term(),
      Reason                  :: term().

decode_sdp(PP) ->
    megaco_sdp:decode(PP).


%%-----------------------------------------------------------------
%% dget_sdp_record_from_PropertyGroup(Type, PG) ->
%% 
%%   [sdp()]}
%%
%% Get all sdp records of a certain type from a property group
%%-----------------------------------------------------------------

get_sdp_record_from_PropertyGroup(Type, PG) ->
    megaco_sdp:get_sdp_record_from_PropertyGroup(Type, PG).


%%-----------------------------------------------------------------

-spec print_version_info() -> void().

print_version_info() ->
    {ok, Versions} = megaco:versions1(),
    print_version_info(Versions).

-spec print_version_info(Versions) -> void() when
      Versions    :: [VersionInfo],
      VersionInfo :: term().

print_version_info(Versions) when is_list(Versions) ->
    print_sys_info(Versions),
    print_os_info(Versions),
    print_mods_info(Versions);
print_version_info(BadVersions) ->
    {error, {bad_versions, BadVersions}}.

print_sys_info(Versions) ->
    case key1search(sys_info, Versions) of
	{value, SysInfo} when is_list(SysInfo) ->
	    {value, Arch} = key1search(arch, SysInfo, "Not found"),
	    {value, Ver}  = key1search(ver, SysInfo, "Not found"),
	    io:format("System info: "
		      "~n   Arch: ~s"
		      "~n   Ver:  ~s"
		      "~n", [Arch, Ver]),
	    ok;
	_ ->
	    io:format("System info: Not found~n", []),
	    not_found
    end.
	    
print_os_info(Versions) ->
    case key1search(os_info, Versions) of
	{value, OsInfo} when is_list(OsInfo) ->
	    Fam = 
		case key1search(fam, OsInfo, "Not found") of
		    {value, F} when is_atom(F) ->
			atom_to_list(F);
		    {value, LF} when is_list(LF) ->
			LF;
		    {value, XF} ->
			lists:flatten(io_lib:format("~p", [XF]))
		end,
	    Name = 
		case key1search(name, OsInfo) of
		    {value, N} when is_atom(N) ->
			"[" ++ atom_to_list(N) ++ "]";
		    {value, LN} when is_list(LN) ->
			"[" ++ LN ++ "]";
		    not_found -> 
			""
		end,
	    Ver = 
		case key1search(ver, OsInfo, "Not found") of
		    {value, T} when is_tuple(T) ->
			tversion(T);
		    {value, LV} when is_list(LV) ->
			LV;
		    {value, XV} ->
			lists:flatten(io_lib:format("~p", [XV]))
		end,
	    io:format("OS info: "
		      "~n   Family: ~s ~s"
		      "~n   Ver:    ~s"
		      "~n", [Fam, Name, Ver]),
	    ok;
	_ ->
	    io:format("OS info:     Not found~n", []),
	    not_found
    end.

%% tversion({A, B, C}) ->
%%     lists:flatten(io_lib:format("~w.~w.~w", [A, B, C]));
tversion(T) ->
    L = tuple_to_list(T),
    lversion(L).

lversion([]) ->
    "";
lversion([A]) ->
    integer_to_list(A);
lversion([A|R]) ->
    integer_to_list(A) ++ "." ++ lversion(R).

print_mods_info(Versions) ->
    case key1search(mod_info, Versions) of
	{value, ModsInfo} when is_list(ModsInfo) ->
	    io:format("Module info: ~n", []),
	    lists:foreach(fun print_mod_info/1, ModsInfo);
	_ ->
	    io:format("Module info: Not found~n", []),
	    not_found
    end.

print_mod_info({Module, Info}) ->
    % Maybe a asn1 generated module
    Asn1Vsn = 
	case (catch Module:info()) of
	    AI when is_list(AI) ->
		case (catch key1search(vsn, AI)) of
		    {value, V} when is_atom(V) ->
			atom_to_list(V);
		    _ ->
			"-"
		end;
	    _ ->
		"-"
	end,
    Vsn = 
	case key1search(vsn, Info) of
	    {value, I} when is_integer(I) ->
		integer_to_list(I);
	    _ ->
		"Not found"
	end,
    AppVsn = 
	case key1search(app_vsn, Info) of
	    {value, S1} when is_list(S1) ->
		S1;
	    _ ->
		"Not found"
	end,
    CompVer = 
	case key1search(compiler_version, Info) of
	    {value, S2} when is_list(S2) ->
		S2;
	    _ ->
		"Not found"
	end,
    io:format("   ~w:~n"
	      "      Vsn:          ~s~n"
	      "      App vsn:      ~s~n"
	      "      ASN.1 vsn:    ~s~n"
	      "      Compiler ver: ~s~n",
	      [Module, Vsn, AppVsn, Asn1Vsn, CompVer]),
    ok.



key1search(Key, Vals) ->
    case lists:keysearch(Key, 1, Vals) of
        {value, {Key, Val}} ->
            {value, Val};
        false ->
            not_found
    end.

key1search(Key, Vals, Def) ->
    case key1search(Key, Vals) of
	not_found ->
	    {value, Def};
	Value ->
	    Value
    end.


%%-----------------------------------------------------------------

-spec versions1() -> {ok, VersionInfo} | {error, Reason} when
      VersionInfo :: list(),
      Reason      :: term().

versions1() ->
    case ms1() of
	{ok, Mods} ->
	    {ok, version_info(Mods)};
	Error ->
	    Error
    end.


-spec versions2() -> {ok, VersionInfo} | {error, Reason} when
      VersionInfo :: list(),
      Reason      :: term().

versions2() ->
    case ms2() of
	{ok, Mods} ->
	    {ok, version_info(Mods)};
	Error ->
	    Error
    end.

version_info(Mods) ->
    SysInfo = sys_info(),
    OsInfo  = os_info(),
    ModInfo = [mod_version_info(Mod) || Mod <- Mods],
    [{sys_info, SysInfo}, {os_info, OsInfo}, {mod_info, ModInfo}].
    
mod_version_info(Mod) ->
    Info = Mod:module_info(),
    {value, {attributes, Attr}}   = lists:keysearch(attributes, 1, Info),
    {value, {vsn,        [Vsn]}}  = lists:keysearch(vsn,        1, Attr),
    {value, {app_vsn,    AppVsn}} = lists:keysearch(app_vsn,    1, Attr),
    {value, {compile,    Comp}}   = lists:keysearch(compile,    1, Info),
    {value, {version,    Ver}}    = lists:keysearch(version,    1, Comp),
    {Mod, [{vsn,              Vsn}, 
	   {app_vsn,          AppVsn}, 
	   {compiler_version, Ver}]}.

sys_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    [{arch, SysArch}, {ver, SysVer}].

os_info() ->
    {OsFam, OsName} = os:type(),
    [{fam, OsFam}, {name, OsName}, {ver, os:version()}].
    
ms() ->    
    ms1().

ms1() ->
    App    = ?APPLICATION,
    LibDir = code:lib_dir(App),
    File   = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
        {ok, [{application, App, AppFile}]} ->
	    case lists:keysearch(modules, 1, AppFile) of
		{value, {modules, Mods}} ->
		    {ok, Mods};
		_ ->
		    {error, {invalid_format, modules}}
	    end;
        Error ->
            {error, {invalid_format, Error}}
    end.

ms2() ->
    application:get_key(?APPLICATION, modules).

nc() ->
    {ok, Mods} = ms(),
    nc(Mods).

nc(all) ->
    _ = application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    _ = application:unload(?APPLICATION),
	    nc(Mods);
	_ ->
	    {error, not_found}
    end;
nc(Mods) when is_list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, compile)].

ni() -> 
    case ms() of
	{ok, Mods} ->
	    ni(Mods);
	Error ->
	    Error
    end.

ni(all) -> 
    _ = application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    _ = application:unload(?APPLICATION),
	    ni(Mods);
	_ ->
	    {error, not_found}
    end;
ni(Mods) when is_list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, interpret)].

load(Mod, How) when is_atom(Mod) ->
    case try_load(Mod, How) of
	ok ->
	    ok;
	_ ->
	    io:format( "~n RETRY ~p FROM: ", [Mod]),
	    ModString = atom_to_list(Mod) ++ ".erl",
	    LibDir = code:lib_dir(?APPLICATION),
	    case find_file([LibDir], ModString) of
		{ok, Abs} ->
		    load(Abs, How);
		{error, Reason} ->
		    io:format( " *** ERROR *** ~p~n", [Reason]),
		    {error, Reason}
	    end
    end;
load(Abs, How) ->
    case try_load(Abs, How) of
	ok ->
	    ok;
	{error, Reason} ->
	    io:format( " *** ERROR *** ~p~n", [Reason]),
	    {error, Reason}
    end.

try_load(Mod, How) ->
    io:format( " ~p ", [Mod]),
    Flags = [{d, debug}],
    case How of
	compile ->
	    case catch c:nc(Mod, Flags) of
		{ok, _} -> ok;
		Other   -> {error, Other}
	    end;
	interpret ->
	    case catch int:ni(Mod, Flags) of
		{module, _} -> ok;
		Other       -> {error, Other}
	    end
    end.

find_file([Dir | Dirs], File) ->
    case file:list_dir(Dir) of
	{ok, List} ->
	    case lists:member(File, List) of
		true ->
		    {ok, filename:join([Dir, File])};
		false ->
		    SubDirs = [filename:join([Dir, Sub]) || Sub <- List],
		    case find_file(SubDirs, File) of
			{ok, Abs} ->
			    {ok, Abs};
			{error, _Reason} ->
			    find_file(Dirs, File)
		    end
	    end;
	{error, _Reason} ->
	    find_file(Dirs, File)
    end;
find_file([], File) ->
    {error, {no_such_file, File}}.


%%-----------------------------------------------------------------

%% -----------------------------
%% These functions can be used instead of the et tool for
%% managing trace of the megaco application.

%%-----------------------------------------------------------------
%% enable_trace(Level, Destination) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%% Destination -> File | Port | io | {io, Verbosity} | HandlerSpec
%% File -> string()
%% Port -> integer()
%% Verbosity -> true | false
%% HandlerSpec = {function(), Data}
%% Data = term()
%%
%% Description:
%% This function is used to start tracing at level Level and send
%% the result either to the file File or the port Port. Note that
%% it starts a tracer server.
%% When Destination is the atom io (or the tuple {io, Verbosity}), 
%% all (printable) megaco trace events (trace_ts events which has 
%% Severity withing Limit) will be written to stdout using io:format. 
%% 
%%-----------------------------------------------------------------

-spec enable_trace(Level, Destination) -> void() when
      Level       :: trace_level(),
      Destination :: File | Port | HandlerSpec | io,
      File        :: string(),
      Port        :: integer(),
      HandlerSpec :: {HandlerFun, InitialData},
      HandlerFun  :: trace_handler(),
      InitialData :: trace_data().

enable_trace(Level, File) when is_list(File) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    HandleSpec = {fun handle_trace/2, Fd},
	    dbg:tracer(process, HandleSpec),
	    set_trace(Level);
	Err ->
	    Err
    end;
enable_trace(Level, Port) when is_integer(Port) ->
    dbg:tracer(port, dbg:trace_port(ip, Port)),
    set_trace(Level);
enable_trace(Level, io) ->
    HandleSpec = {fun handle_trace/2, standard_io},
    dbg:tracer(process, HandleSpec),
    set_trace(Level);
enable_trace(Level, {Fun, _Data} = HandleSpec) when is_function(Fun) ->
    dbg:tracer(process, HandleSpec),
    set_trace(Level).


%%-----------------------------------------------------------------
%% disable_trace() -> void()
%% 
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------

-spec disable_trace() -> void().

disable_trace() ->
    %% This is to make handle_trace/2 close the output file (if the
    %% event gets there before dbg closes)
    report_event(stop_trace, stop_trace, stop_trace, stop_trace, stop_trace),
    dbg:stop().


%%-----------------------------------------------------------------
%% set_trace(Level) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%%
%% Description:
%% This function is used to change the trace level when tracing has
%% already been started. 
%%-----------------------------------------------------------------

-spec set_trace(Level) -> void() when
      Level :: trace_level().

set_trace(Level) ->
    Pat = et_selector:make_pattern({?MODULE, Level}),
    et_selector:change_pattern(Pat).



%%-----------------------------------------------------------------
%% report_event(DetailLevel, FromTo, Label, Contents) -> void()
%% report_event(DetailLevel, From, To, Label, Contents) -> void()
%% 
%% Description:
%% This is the function tracing is done for.
%% Trace macros used by the megaco app all call this function.
%%-----------------------------------------------------------------


report_event(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

report_event(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.
    

%% ----------------------------------------------------------------------
%% handle_trace(Event, Verbosity) -> Verbosity
%% 
%% Parameters:
%% Event -> The trace event (only megaco 'trace_ts' events are printed)
%% Verbosity -> max | min | integer() (see Level above)
%%
%% Description:
%% This function is "receive" and print the trace events. 
%% Events are printed if:
%%   - Verbosity is max
%%   - Severity is =< Verbosity (e.g. Severity = 30, and Verbosity = 40)
%% Events are not printed if:
%%   - Verbosity is min
%%   - Severity is > Verbosity
%%-----------------------------------------------------------------

handle_trace(_, closed_file = Fd) ->
    Fd;
handle_trace({trace_ts, _Who, call, 
	      {?MODULE, report_event, 
	       [stop_trace, stop_trace, stop_trace, stop_trace, stop_trace]}, 
	      _Timestamp}, 
	     standard_io = Fd) ->
    Fd;
handle_trace({trace_ts, _Who, call, 
	      {?MODULE, report_event, 
	       [stop_trace, stop_trace, stop_trace, stop_trace, stop_trace]}, 
	      Timestamp}, 
	     Fd) ->
    (catch io:format(Fd, "stop trace at ~s~n", [format_timestamp(Timestamp)])),
    (catch file:close(Fd)),
    closed_file;
handle_trace({trace_ts, Who, call, 
	      {?MODULE, report_event, 
	       [Sev, From, To, Label, Content]}, Timestamp}, 
	     Fd) ->
    (catch print_megaco_trace(Fd, Sev, Who, Timestamp, Label, From, To, Content)),
    Fd;
handle_trace(Event, Fd) ->
    (catch print_trace(Fd, Event)),
    Fd.


print_megaco_trace(Fd, Sev, Who, Timestamp, Label, From, To, Content) ->
    Ts = format_timestamp(Timestamp),
    io:format(Fd, "[megaco trace ~w ~w ~s] ~s "
	      "~n   From:     ~p"
	      "~n   To:       ~p"
	      "~n   Content:  ~p"
	      "~n", 
	      [Sev, Who, Ts, Label, From, To, Content]).
    
print_trace(Fd, {trace, Who, What, Where}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Who, What, Where]);

print_trace(Fd, {trace, Who, What, Where, Extra}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Who, What, Where, Extra]);

print_trace(Fd, {trace_ts, Who, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Ts, Who, What, Where]);

print_trace(Fd, {trace_ts, Who, What, Where, Extra, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Ts, Who, What, Where, Extra]);

print_trace(Fd, {seq_trace, What, Where}) ->
    io:format(Fd, "[seq trace]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [What, Where]);

print_trace(Fd, {seq_trace, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[seq trace ~s]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [Ts, What, Where]);

print_trace(Fd, {drop, Num}) ->
    io:format(Fd, "[drop trace] ~p~n", [Num]);

print_trace(Fd, Trace) ->
    io:format(Fd, "[trace] "
              "~n   ~p"
              "~n", [Trace]).


%% ---------------------------------------------------------------------------
%% # formated_timstamp/0,     formated_timstamp/1
%% # format_short_timstamp/0, format_short_timstamp/1
%% # format_long_timstamp/0,  format_long_timstamp/1
%% 
%% Create a formatted timestamp. Short means that it will not include 
%% the date in the formatted timestamp. Also it will only include millis.
%% ---------------------------------------------------------------------------

formated_timestamp() ->
    formated_long_timestamp().

formated_short_timestamp() ->
    format_short_timestamp(os:timestamp()).

formated_long_timestamp() ->
    format_long_timestamp(os:timestamp()).


%% ---------------------------------------------------------------------------
%% # format_timstamp/1, format_timstamp/2
%% # format_short_timstamp/1, format_short_timstamp/2
%% # format_long_timstamp/1, format_long_timstamp/2
%% 
%% Formats the provided timestamp. Short means that it will not include 
%% the date in the formatted timestamp.
%% ---------------------------------------------------------------------------

-spec format_timestamp(Now :: erlang:timestamp()) ->
    string().

format_timestamp(Now) ->
    format_long_timestamp(Now).

-spec format_short_timestamp(Now :: erlang:timestamp()) ->
    string().

format_short_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(short, Now, N2T).

-spec format_long_timestamp(Now :: erlang:timestamp()) ->
    string().

format_long_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(long, Now, N2T).

-spec format_timestamp(Now :: erlang:timestamp(), 
                       N2T :: function()) ->
    string().

format_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_long_timestamp(Now, N2T).

-spec format_short_timestamp(Now :: erlang:timestamp(), 
                             N2T :: function()) ->
    string().

format_short_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_timestamp(short, Now, N2T).

-spec format_long_timestamp(Now :: erlang:timestamp(), 
                            N2T :: function()) ->
    string().

format_long_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_timestamp(long, Now, N2T).

format_timestamp(Format, {_N1, _N2, N3} = Now, N2T) ->
    {Date, Time} = N2T(Now),
    do_format_timestamp(Format, Date, Time, N3).

do_format_timestamp(short, _Date, Time, N3) ->
    do_format_short_timestamp(Time, N3);
do_format_timestamp(long, Date, Time, N3) ->
    do_format_long_timestamp(Date, Time, N3).
    
do_format_long_timestamp(Date, Time, N3) ->
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                      [YYYY, MM, DD, Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatDate).

do_format_short_timestamp(Time, N3) ->
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w", 
                      [Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatDate).

