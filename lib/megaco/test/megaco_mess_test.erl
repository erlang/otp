%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2019. All Rights Reserved.
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
%% Purpose: Verify the implementation of the ITU-T protocol H.248
%%----------------------------------------------------------------------
%% Run the entire test suite with:
%% 
%%    megaco_test_lib:t(megaco_test).
%%    megaco_test_lib:t({megaco_test, all}).
%%    
%% Or parts of it:
%% 
%%    megaco_test_lib:t({megaco_test, accept}).
%%----------------------------------------------------------------------
-module(megaco_mess_test).

%% -compile(export_all).
-export([
	 all/0, groups/0, 
	 init_per_suite/1,    end_per_suite/1, 
	 init_per_group/2,    end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,

	 connect/1,
	
	 request_and_reply_plain/1,
	 request_and_no_reply/1,
	 request_and_reply_pending_ack_no_pending/1,
	 request_and_reply_pending_ack_one_pending/1,
	 single_trans_req_and_reply/1,
	 single_trans_req_and_reply_sendopts/1,
	 request_and_reply_and_ack/1,
	 request_and_reply_and_no_ack/1,
	 request_and_reply_and_late_ack/1,
	 trans_req_and_reply_and_req/1, 
	 
	
	 pending_ack_plain/1,
	 request_and_pending_and_late_reply/1, 
	 
	 dist/1,
	 
	
	 otp_4359/1,
	 otp_4836/1,
	 otp_5805/1,
	 otp_5881/1,
	 otp_5887/1,
	 otp_6253/1,
	 otp_6275/1,
	 otp_6276/1,
	
	 otp_6442_resend_request1/1,
	 otp_6442_resend_request2/1,
	 otp_6442_resend_reply1/1,
	 otp_6442_resend_reply2/1,
	 
	 otp_6865_request_and_reply_plain_extra1/1,
	 otp_6865_request_and_reply_plain_extra2/1, 
	 otp_7189/1, 
	 otp_7259/1, 
	 otp_7713/1,
	 
	 otp_8183_request1/1, 
	 otp_8212/1
	]).

%% -behaviour(megaco_user).
-export([
	 handle_connect/3,
	 handle_disconnect/4,
	 handle_syntax_error/4,
	 handle_message_error/4,
	 handle_trans_request/4,
	 handle_trans_long_request/4,
	 handle_trans_reply/5,
	 handle_trans_ack/5,
	 handle_unexpected_trans/4,
	 handle_trans_request_abort/5
	]).

%% -behaviour(megaco_transport).
-export([
	 send_message/2,
	 unblock/1
	]).

-ifdef(megaco_hipe_special).
-export([
	 %% Case: request_and_reply_pending_ack_no_pending
	 rarpanp_mgc_verify_handle_connect/1,
	 rarpanp_mgc_verify_service_change_req/2, 
	 rarpanp_mgc_verify_notify_request/1,
	 rarpanp_mgc_verify_handle_disconnect/1,
	 rarpanp_mg_verify_service_change_rep_msg/1,
	 rarpanp_mg_verify_notify_rep_msg/3,
	 
	 %% Case: request_and_reply_pending_ack_one_pending
	 rarpaop_mgc_verify_handle_connect/1,
	 rarpaop_mgc_verify_service_change_req/2,
	 rarpaop_mgc_verify_notify_request/1,
	 rarpaop_mgc_verify_reply_ack/1,
	 rarpaop_mgc_verify_handle_disconnect/1,
	 rarpaop_mg_verify_service_change_rep_msg/1,
	 rarpaop_mg_verify_pending_msg/2,
	 rarpaop_mg_verify_notify_rep_msg/3,

	 %% Case: single_trans_req_and_reply
	 strar_mgc_verify_handle_connect/1,
	 strar_mgc_verify_service_change_req/2,
	 strar_mgc_verify_notify_request/1,
	 strar_mgc_verify_handle_disconnect/1,
	 strar_mg_verify_handle_connect/1,
	 strar_mg_verify_service_change_reply/1,
	 strar_mg_verify_notify_reply/1,

	 %% Case: single_trans_req_and_reply_sendopts
	 straro_mgc_verify_handle_connect/1,
	 straro_mgc_verify_service_change_req/2,
	 straro_mgc_verify_notify_request/1,
	 straro_mgc_verify_handle_trans_ack/1, 
	 straro_mg_verify_handle_connect/1,
	 straro_mg_verify_service_change_reply/1,
	 straro_mg_verify_handle_disconnect/1,

	 %% Case: request_and_reply_and_ack
	 raraa_mgc_verify_handle_connect/1,
	 raraa_mgc_verify_service_change_req/2, 
	 raraa_mgc_verify_notify_req/1,
	 raraa_mgc_verify_handle_trans_ack/1,
	 raraa_mgc_verify_handle_disconnect/1,
	 raraa_mg_verify_service_change_rep_msg/1,
	 raraa_mg_verify_notify_rep_msg/5,

	 %% Case: request_and_reply_and_no_ack
	 rarana_mgc_verify_handle_connect/1,
	 rarana_mgc_verify_service_change_req/2, 
	 rarana_mgc_verify_notify_req/1,
	 rarana_mgc_verify_handle_trans_ack/1,
	 rarana_mgc_verify_handle_disconnect/1,
	 rarana_mg_verify_service_change_rep_msg/1,
	 rarana_mg_verify_notify_rep_msg/5,

	 %% Case: request_and_reply_and_late_ack
	 rarala_mgc_verify_handle_connect/1,
	 rarala_mgc_verify_service_change_req/2, 
	 rarala_mgc_verify_notify_req/1, 
	 rarala_mgc_verify_handle_trans_ack/1,
	 rarala_mgc_verify_handle_disconnect/1,
	 rarala_mg_verify_service_change_rep_msg/1,
	 rarala_mg_verify_notify_rep_msg/5,

	 %% Case: trans_req_and_reply_and_req
	 trarar_mgc_verify_handle_connect/1,
	 trarar_mgc_verify_service_change_req/2,
	 trarar_mgc_verify_notify_req/2,
	 trarar_mgc_verify_handle_disconnect/1,
	 trarar_mg_verify_service_change_rep_msg/1,
	 trarar_mg_verify_notify_rep_msg/5,

	 %% Case: pending_ack_plain
	 pap_mgc_verify_handle_connect/1,
	 pap_mgc_verify_service_change_req/2,
	 pap_mgc_verify_notify_req/1,
	 pap_mgc_verify_notify_req_long/1,
	 pap_mgc_verify_handle_trans_ack/1,
	 pap_mgc_verify_handle_disconnect/1,
	 pap_mg_verify_service_change_rep_msg/1,
	 pap_mg_verify_pending_msg/2,
	 pap_mg_verify_notify_rep_msg/5,

	 %% Case: request_and_pending_and_late_reply
	 rapalr_mgc_verify_service_change_req_msg/1,
	 rapalr_mgc_verify_notify_req_msg/5,
	 rapalr_mgc_verify_trans_ack_msg/2,
	 rapalr_mg_verify_handle_connect/1,
	 rapalr_mg_verify_service_change_rep/1,
	 rapalr_mg_verify_notify_rep/1,
	 
	 %% Case: otp_4836
	 otp_4836_mgc_verify_service_change_req_msg/1,
	 otp_4836_mgc_verify_notify_req_msg/1, 

	 %% Case: otp_5805
	 otp_5805_mgc_verify_handle_connect/1,
	 otp_5805_mgc_verify_service_change_req/2,
	 otp_5805_mgc_verify_handle_syntax_error/1,
	 otp_5805_mgc_verify_handle_disconnect/1, 
	 otp_5805_mg_verify_service_change_rep_msg/1,
	 otp_5805_mg_verify_error_descriptor_msg/1,

	 %% Case: otp_5881
	 otp_5881_mgc_verify_service_change_req_msg/1,
	 otp_5881_mgc_verify_notify_req_msg/1,

	 %% Case: otp_5887
	 otp_5887_mgc_verify_service_change_req_msg/1,
	 otp_5887_mgc_verify_notify_req_msg/1,

	 %% Case: otp_6275
	 otp_6275_mgc_verify_service_change_req_msg/1,
	 otp_6275_mgc_verify_notify_rep_msg/1,
	 otp_6275_mg_verify_handle_connect/1,
	 otp_6275_mg_verify_notify_req/1,
	 otp_6275_mg_verify_handle_trans_rep/1,

	 %% Case: otp_6442_resend_request1
	 otp_6442_resend_request1_mg_verify_handle_connect/1, 
	 otp_6442_resend_request1_mg_verify_service_change_rep/1, 
	 otp_6442_resend_request1_mg_verify_notify_rep/1, 
	 
	 %% Case: otp_6442_resend_request2
	 otp_6442_resend_request2_mg_verify_handle_connect/1, 
	 otp_6442_resend_request2_mg_verify_service_change_rep/1, 
	 otp_6442_resend_request2_mg_verify_notify_rep/1, 
	 
	 %% Case: otp_6442_resend_reply1
	 otp_6442_resend_reply1_mg_verify_handle_connect/1,
	 otp_6442_resend_reply1_mg_verify_service_change_rep/1,
	 otp_6442_resend_reply1_mg_verify_notify_req/2,	 
	 otp_6442_resend_reply1_mg_verify_ack/1,

	 %% Case: otp_6442_resend_reply2
	 otp_6442_resend_reply2_mg_verify_handle_connect/1,
	 otp_6442_resend_reply2_mg_verify_service_change_rep/1,
	 otp_6442_resend_reply2_mg_verify_notify_req/2,	 
	 otp_6442_resend_reply2_mg_verify_ack/1,

	 %% Case: otp_6865_request_and_reply_plain_extra2
	 otp6865e2_mgc_verify_handle_connect/1,
	 otp6865e2_mgc_verify_service_change_req/3,
	 otp6865e2_mgc_verify_notify_req/4,
	 otp6865e2_mgc_verify_reply_ack/2,
	 otp6865e2_mgc_verify_notify_reply/2,
	 otp6865e2_mgc_verify_handle_disconnect/1,
	 otp6865e2_mg_verify_service_change_rep_msg/1,
	 otp6865e2_mg_verify_notify_rep_msg/6,
	 otp6865e2_mg_verify_notify_req_msg/1,

	 %% Case: otp_7189
	 otp_7189_mgc_verify_handle_connect/1,
	 otp_7189_mgc_verify_service_change_req/2,
	 otp_7189_mgc_verify_handle_trans_reply_req/1,
	 otp_7189_mgc_verify_handle_disconnect/1, 
	 otp_7189_mg_verify_service_change_rep_msg/1,
	 otp_7189_mg_verify_notify_req_msg/1,

	 %% Case: otp_6442_resend_request1
	 otp_8183_request1_mg_verify_handle_connect/1, 
	 otp_8183_request1_mg_verify_service_change_rep/1, 
	 otp_8183_request1_mg_verify_notify_rep/1, 
	 
	 %% Utility
	 encode_msg/3,
	 decode_msg/3
	]).
-endif.

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

-define(VERSION, 1).

-define(USER_MOD, megaco_mess_user_test).

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(MGC_START(Pid, Mid, ET, Conf, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Conf, Verb)).
-define(MGC_STOP(Pid),        megaco_test_mgc:stop(Pid)).
-define(MGC_REQ_PEND(Pid,To), megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HP(Pid,To),   megaco_test_mgc:request_handle_pending(Pid,To)).
-define(MGC_ACK_INFO(Pid),    megaco_test_mgc:ack_info(Pid,self())).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
	megaco_test_mg:start(Pid, Mid, Enc, Transp, Conf, Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_REQ(Pid), megaco_test_mg:notify_request(Pid)).
-define(MG_AWAIT_NOTIF_REP(Pid), megaco_test_mg:await_notify_reply(Pid)).
-define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).
-define(MG_NOTIF_RAR(Pid), megaco_test_mg:notify_request_and_reply(Pid)).

-define(SEND(Expr), 
	?VERIFY(ok, ?USER_MOD:apply_proxy(fun() -> Expr end))).

-define(USER(Expected, Reply),
	?USER_MOD:reply(?MODULE,
                        ?LINE,
                        fun(Actual) ->
                                case ?VERIFY(Expected, Actual) of
                                    Expected   -> {ok, Reply};
                                    UnExpected -> {error, {reply_verify,
                                                           ?MODULE,
                                                           ?LINE,
                                                           UnExpected}}
                                end
                        end)).

%% Some generator (utility) macros
-define(GM_START(),              megaco_start).
-define(GM_STOP(),               megaco_stop).
-define(GM_START_USER(M, RI, C), {megaco_start_user, M, RI, C}).
-define(GM_START_USER(M, RI),    ?GM_START_USER(M, RI, [])).
-define(GM_STOP_USER(),          megaco_stop_user).
-define(GMSI(I),                 {megaco_system_info, I}).
-define(GMSI_USERS(),            ?GMSI(users)).
-define(GMSI_CONNS(),            ?GMSI(connections)).
-define(GMCAST(Reqs, Opts),      {megaco_cast, Reqs, Opts}).
-define(GMCAST(Reqs),            ?GMCAST(Reqs, [])).
-define(GMCB(CB, VF),            {megaco_callback, CB, VF}).
-define(GMCB_CONNECT(VF),        ?GMCB(handle_connect, VF)).
-define(GMCB_TRANS_REP(VF),      ?GMCB(handle_trans_reply, VF)).
-define(GMT(T),                  {megaco_trace, T}).
-define(GMT_ENABLE(),            ?GMT(enable)).
-define(GMT_DISABLE(),           ?GMT(disable)).
-define(GD(D),                   {debug, D}).
-define(GD_ENABLE(),             ?GD(true)).
-define(GD_DISABLE(),            ?GD(false)).
-define(GS(T),                   {sleep, T}).

-define(GSND(T, D),              {send, T, D}).
-define(GERCV(T, VF, TO),        {expect_receive, T, {VF, TO}}).


min(M) -> ?MINS(M).

%% Test server callbacks
init_per_testcase(otp_7189 = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout, min(2)} |C]);
init_per_testcase(request_and_no_reply = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout, min(2)} |C]);
init_per_testcase(Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout, min(1)} |C]).

end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [
     connect, 
     {group, request_and_reply},
     {group, pending_ack}, 
     dist, 
     {group, tickets}
    ].

groups() -> 
    [
     {request_and_reply, [],
      [request_and_reply_plain, 
       request_and_no_reply,
       request_and_reply_pending_ack_no_pending,
       request_and_reply_pending_ack_one_pending,
       single_trans_req_and_reply,
       single_trans_req_and_reply_sendopts,
       request_and_reply_and_ack, 
       request_and_reply_and_no_ack,
       request_and_reply_and_late_ack,
       trans_req_and_reply_and_req]},
     {pending_ack, [],
      [pending_ack_plain,
       request_and_pending_and_late_reply]},
     {tickets, [],
      [otp_4359, 
       otp_4836, 
       otp_5805, 
       otp_5881, 
       otp_5887,
       otp_6253, 
       otp_6275, 
       otp_6276, 
       {group, otp_6442},
       {group, otp_6865}, 
       otp_7189, 
       otp_7259, 
       otp_7713,
       {group, otp_8183}, 
       otp_8212]},
     {otp_6442, [],
      [otp_6442_resend_request1, 
       otp_6442_resend_request2,
       otp_6442_resend_reply1, 
       otp_6442_resend_reply2]},
     {otp_6865, [],
      [otp_6865_request_and_reply_plain_extra1,
       otp_6865_request_and_reply_plain_extra2]},
     {otp_8183, [], [otp_8183_request1]}
    ].


init_per_suite(Config) ->
    io:format("~w:init_per_suite -> entry with"
	      "~n   Config:     ~p"
              "~n   OS Type:    ~p"
              "~n   OS Version: ~s"
	      "~n", 
              [?MODULE, 
               Config, 
               os:type(), 
               case os:version() of
                   {Major, Minor, Release} ->
                       ?F("~w.~w.~w", [Major, Minor, Release]);
                   Str when is_list(Str) ->
                       Str
               end]),
    Config.

end_per_suite(_Config) ->
    io:format("~w:end_per_suite -> entry with"
	      "~n   _Config: ~p"
	      "~n", [?MODULE, _Config]),
    ok.


init_per_group(_GroupName, Config) ->
    io:format("~w:init_per_group -> entry with"
	      "~n   _GroupName: ~p"
	      "~n   Config: ~p"
	      "~n", [?MODULE, _GroupName, Config]),
    Config.

end_per_group(_GroupName, Config) ->
    io:format("~w:end_per_group -> entry with"
	      "~n   _GroupName: ~p"
	      "~n   Config: ~p"
	      "~n", [?MODULE, _GroupName, Config]),
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(suite) ->
    [];
connect(doc) ->
    [];
connect(Config) when is_list(Config) ->
    %% ?SKIP("Needs a re-write..."),
    ?ACQUIRE_NODES(1, Config),
    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),

    d("connect -> start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    d("connect -> start (MG) user ~p",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, [{send_mod, bad_send_mod},
	                                  {request_timer, infinity},
	                                  {reply_timer, infinity}])),

    d("connect -> get receive info for ~p",[MgMid]),
    MgRH = user_info(MgMid, receive_handle),
    d("connect -> (MG) try connect to MGC",[]),
    {ok, PrelCH} = ?VERIFY({ok, _}, megaco:connect(MgRH, PrelMid, sh, self())),

    connections([PrelCH]),
    ?VERIFY([PrelCH], megaco:user_info(MgMid, connections)),
    
    ?VERIFY(bad_send_mod, megaco:user_info(MgMid, send_mod)),
    ?VERIFY(bad_send_mod, megaco:conn_info(PrelCH, send_mod)),
    SC = service_change_request(),
    case megaco:call(PrelCH, [SC], []) of
	{_Version, 
	 {error, 
	  {send_message_failed, 
	   {'EXIT', {undef, [{bad_send_mod, send_message, [sh, _]} | _]}}}}
	} ->
	    %% R14B and previous
	    ?LOG("expected send failure (1)", []),
	    ok;
	
	%% As of R15, we also get some extra info (e.g. line numbers)
	{_Version, 
	 {error, 
	  {send_message_failed, 
	   {'EXIT', {undef, [{bad_send_mod, send_message, [sh, _], _} | _]}}}}
	} ->
	    %% R15B and later
	    ?LOG("expected send failure (2)", []),
	    ok;

	Unexpected ->
	    ?ERROR(Unexpected)
    end,

    ?VERIFY(ok, megaco:disconnect(PrelCH, shutdown)),

    ?VERIFY(ok,	megaco:stop_user(MgMid)),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_plain(suite) ->
    [];
request_and_reply_plain(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    d("request_and_reply_plain -> start proxy",[]),
    ?USER_MOD:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = ?USER_MOD,
    d("request_and_reply_plain -> start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],
    d("request_and_reply_plain -> start (MG) user ~p",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),

    d("request_and_reply_plain -> start (MGC) user ~p",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    d("request_and_reply_plain -> get receive info for ~p",[MgMid]),
    MgRH = user_info(MgMid, receive_handle),
    d("request_and_reply_plain -> get receive info for ~p",[MgcMid]),
    MgcRH = user_info(MgcMid, receive_handle), 
    d("request_and_reply_plain -> start transport",[]),
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},
    d("request_and_reply_plain -> (MG) try connect to MGC",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    d("request_and_reply_plain -> (MGC) await connect from MG",[]),
    ?USER({connect, PrelMgCH, _V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("request_and_reply_plain -> (MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("request_and_reply_plain -> (MGC) send service change reply",[]),
    ?USER({connect, MgcCH, _V, []}, ok), % Mgc auto
    Rep = service_change_reply(MgcMid),
    ?USER({request, MgcCH, _V, [[Req]]}, {discard_ack, [Rep]}),
    ?USER({connect, MgCH, _V, []}, ok), % Mg confirm
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    d("request_and_reply_plain -> get (system info) connections",[]),
    connections([MgCH, MgcCH]),
    d("request_and_reply_plain -> get (~p) connections",[MgMid]),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    d("request_and_reply_plain -> get (~p) connections",[MgcMid]),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    Reason = shutdown,
    d("request_and_reply_plain -> (MG) disconnect",[]),
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    d("request_and_reply_plain -> (MGC) disconnect",[]),
    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    d("request_and_reply_plain -> stop megaco app",[]),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    d("request_and_reply_plain -> done",[]),
    ok.


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% OTP-4760
request_and_no_reply(suite) ->
    [];
request_and_no_reply(doc) ->
    [];
request_and_no_reply(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        request_and_no_reply),
    i("starting"),

    MgcNode = make_node_name(mgc),
    Mg1Node = make_node_name(mg1),
    Mg2Node = make_node_name(mg2),
    Mg3Node = make_node_name(mg3),
    Mg4Node = make_node_name(mg4),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   Mg1Node: ~p"
      "~n   Mg2Node: ~p"
      "~n   Mg3Node: ~p"
      "~n   Mg4Node: ~p", 
      [MgcNode, Mg1Node, Mg2Node, Mg3Node, Mg4Node]),
    Nodes = [MgcNode, Mg1Node, Mg2Node, Mg3Node, Mg4Node], 
    ok = megaco_test_lib:start_nodes(Nodes, ?FILE, ?LINE),

    %% Start the MGC
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, [], ?MGC_VERBOSITY),
    ?SLEEP(?SECONDS(1)),

    i("[MG] start"),    
    Mg1Mid = {deviceName, "mg1"},
    Mg2Mid = {deviceName, "mg2"},
    Mg3Mid = {deviceName, "mg3"},
    Mg4Mid = {deviceName, "mg4"},
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
				factor      = 1, 
				incr        = 0,
				max_retries = 2
			       },
    LongReqTmr = #megaco_incr_timer{wait_for    = 10000,
				    factor      = 1, 
				    incr        = 0,
				    max_retries = 3
				   },
    %% Start the MGs
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
		{long_request_timer, LongReqTmr}, 
		{pending_timer,      PendingTmr},
		{reply_timer,        ReplyTmr}],
    {ok, Mg1} = ?MG_START(Mg1Node, Mg1Mid, text, tcp, MgConfig, ?MG_VERBOSITY),
    ?SLEEP(?SECONDS(1)),
    {ok, Mg2} = ?MG_START(Mg2Node, Mg2Mid, text, udp, MgConfig, ?MG_VERBOSITY),
    ?SLEEP(?SECONDS(1)),
    {ok, Mg3} = ?MG_START(Mg3Node, Mg3Mid, binary, tcp, MgConfig, ?MG_VERBOSITY),
    ?SLEEP(?SECONDS(1)),
    {ok, Mg4} = ?MG_START(Mg4Node, Mg4Mid, binary, udp, MgConfig, ?MG_VERBOSITY),
    ?SLEEP(?SECONDS(1)),

    d("MG1 user info: ~p", [?MG_USER_INFO(Mg1, all)]),
    d("MG1 conn info: ~p", [?MG_CONN_INFO(Mg1, all)]),
    d("MG2 user info: ~p", [?MG_USER_INFO(Mg2, all)]),
    d("MG2 conn info: ~p", [?MG_CONN_INFO(Mg2, all)]),
    d("MG3 user info: ~p", [?MG_USER_INFO(Mg3, all)]),
    d("MG3 conn info: ~p", [?MG_CONN_INFO(Mg3, all)]),
    d("MG4 user info: ~p", [?MG_USER_INFO(Mg4, all)]),
    d("MG4 conn info: ~p", [?MG_CONN_INFO(Mg4, all)]),

    i("[MG1] connect to the MGC (service change)"),    
    ServChRes1 = ?MG_SERV_CHANGE(Mg1),
    d("service change result: ~p", [ServChRes1]),
    d("MG1 user info: ~p", [?MG_USER_INFO(Mg1, all)]),
    d("MG1 conn info: ~p", [?MG_CONN_INFO(Mg1, all)]),
    ?SLEEP(?SECONDS(1)),

    i("[MG2] connect to the MGC (service change)"),    
    ServChRes2 = ?MG_SERV_CHANGE(Mg2),
    d("service change result: ~p", [ServChRes2]),
    d("MG2 user info: ~p", [?MG_USER_INFO(Mg2, all)]),
    d("MG2 conn info: ~p", [?MG_CONN_INFO(Mg2, all)]),
    ?SLEEP(?SECONDS(1)),

    i("[MG3] connect to the MGC (service change)"),    
    ServChRes3 = ?MG_SERV_CHANGE(Mg3),
    d("service change result: ~p", [ServChRes3]),
    d("MG3 user info: ~p", [?MG_USER_INFO(Mg3, all)]),
    d("MG3 conn info: ~p", [?MG_CONN_INFO(Mg3, all)]),
    ?SLEEP(?SECONDS(1)),

    i("[MG4] connect to the MGC (service change)"),    
    ServChRes4 = ?MG_SERV_CHANGE(Mg4),
    d("service change result: ~p", [ServChRes4]),
    d("MG4 user info: ~p", [?MG_USER_INFO(Mg4, all)]),
    d("MG4 conn info: ~p", [?MG_CONN_INFO(Mg4, all)]),
    ?SLEEP(?SECONDS(1)),

    d("tell the MGC to ignore requests"),
    ?MGC_REQ_PEND(Mgc, infinity),
    ?SLEEP(?SECONDS(1)),

    d("[MG1] send the notify"),
    ?MG_NOTIF_REQ(Mg1),
    ?SLEEP(?SECONDS(1)),

    d("[MG2] send the notify"),
    ?MG_NOTIF_REQ(Mg2),
    ?SLEEP(?SECONDS(1)),

    d("[MG3] send the notify"),
    ?MG_NOTIF_REQ(Mg3),
    ?SLEEP(?SECONDS(1)),

    d("[MG4] send the notify"),
    ?MG_NOTIF_REQ(Mg4),
    ?SLEEP(?SECONDS(1)),

    d("[MG1] await notify reply"),
    {ok, {_Vsn1, {error, timeout}}} = ?MG_AWAIT_NOTIF_REP(Mg1),
    d("[MG1] received expected reply"),
    ?SLEEP(?SECONDS(1)),

    d("[MG2] await notify reply"),
    {ok, {_Vsn2, {error, timeout}}} = ?MG_AWAIT_NOTIF_REP(Mg2),
    d("[MG2] received expected reply"),
    ?SLEEP(?SECONDS(1)),

    d("[MG3] await notify reply"),
    {ok, {_Vsn3, {error, timeout}}} = ?MG_AWAIT_NOTIF_REP(Mg3),
    d("[MG3] received expected reply"),
    ?SLEEP(?SECONDS(1)),

    d("[MG4] await notify reply"),
    {ok, {_Vsn4, {error, timeout}}} = ?MG_AWAIT_NOTIF_REP(Mg4),
    d("[MG4] received expected reply"),
    ?SLEEP(?SECONDS(1)),

    d("MG1 user info: ~p", [?MG_USER_INFO(Mg1, all)]),
    d("MG1 conn info: ~p", [?MG_CONN_INFO(Mg1, all)]),
    d("MG2 user info: ~p", [?MG_USER_INFO(Mg2, all)]),
    d("MG2 conn info: ~p", [?MG_CONN_INFO(Mg2, all)]),
    d("MG3 user info: ~p", [?MG_USER_INFO(Mg3, all)]),
    d("MG3 conn info: ~p", [?MG_CONN_INFO(Mg3, all)]),
    d("MG4 user info: ~p", [?MG_USER_INFO(Mg4, all)]),
    d("MG4 conn info: ~p", [?MG_CONN_INFO(Mg4, all)]),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_pending_ack_no_pending(suite) ->
    [];
request_and_reply_pending_ack_no_pending(doc) ->
    ["This test case tests that megaco correctly handles the return "
     "value handle_pending_ack from handle_trans_request when NO "
     "pending message has been sent"];
request_and_reply_pending_ack_no_pending(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rar_panp),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    Nodes = [MgcNode, MgNode], 
    ok = megaco_test_lib:start_nodes(Nodes, ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rarpanp_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rarpanp_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%
-ifdef(megaco_hipe_special).
-define(rarpanp_mgc_verify_handle_connect_fun(), 
        {?MODULE, rarpanp_mgc_verify_handle_connect, []}).
-define(rarpanp_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, rarpanp_mgc_verify_service_change_req, [Mid]}).
-define(rarpanp_mgc_verify_notify_req_fun(),
        {?MODULE, rarpanp_mgc_verify_notify_request, []}).
-define(rarpanp_mgc_verify_handle_disconnect_fun(),
        {?MODULE, rarpanp_mgc_verify_handle_disconnect, []}).
-else.
-define(rarpanp_mgc_verify_handle_connect_fun(), 
        fun rarpanp_mgc_verify_handle_connect/1).
-define(rarpanp_mgc_verify_service_change_req_fun(Mid),
        rarpanp_mgc_verify_service_change_req_fun(Mid)).
-define(rarpanp_mgc_verify_notify_req_fun(),
	rarpanp_mgc_verify_notify_request_fun()).
-define(rarpanp_mgc_verify_handle_disconnect_fun(),
	fun rarpanp_mgc_verify_handle_disconnect/1).
-endif.

rarpanp_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ?rarpanp_mgc_verify_handle_connect_fun(), 
    ScrVerify     = ?rarpanp_mgc_verify_service_change_req_fun(Mid),
    NrVerify      = ?rarpanp_mgc_verify_notify_req_fun(),
    DiscoVerify   = ?rarpanp_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect,       ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request, ScrVerify},
	     {megaco_callback, handle_trans_request, NrVerify},
             {megaco_callback, nocall, 10000},
             {megaco_callback, handle_disconnect,    DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarpanp_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarpanp_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
-ifndef(megaco_hipe_special).
rarpanp_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    rarpanp_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

rarpanp_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("rarpanp_mgc_verify_service_change_req -> entry with"
	      "~n   AR:  ~p"
	      "~n   Mid: ~p"
	      "~n", [AR, Mid]),
    (catch rarpanp_mgc_do_verify_service_change_req(AR, Mid));
rarpanp_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpanp_mgc_do_verify_service_change_req(AR, Mid) ->
    io:format("rarpanp_mgc_do_verify_service_change_req -> entry with"
	      "~n   AR:  ~p"
	      "~n   Mid: ~p"
	      "~n", [AR, Mid]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarpanp_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
-ifndef(megaco_hipe_special).
rarpanp_mgc_verify_notify_request_fun() ->
    fun(Req) -> 
	    rarpanp_mgc_verify_notify_request(Req) 
    end.
-endif.

rarpanp_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarpanp_mgc_do_verify_notify_request(AR));
rarpanp_mgc_verify_notify_request(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpanp_mgc_do_verify_notify_request(AR) ->
    io:format("rarpanp_mgc_do_verify_notify_request -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = notify_request_verified, 
	    Replies = [rarpanp_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_pending_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


%% Disconnect verification
rarpanp_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarpanp_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarpanp_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarpanp_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(rarpanp_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(rarpanp_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(rarpanp_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, rarpanp_mg_verify_service_change_rep_msg, []}).
-define(rarpanp_mg_verify_notify_rep_msg_fun(TransId, TermId),
	{?MODULE, rarpanp_mg_verify_notify_rep_msg, [TransId, TermId]}).
-else.
-define(rarpanp_mg_decode_msg_fun(Mod, Conf),
	rarpanp_mg_decode_msg_fun(Mod, Conf)).
-define(rarpanp_mg_encode_msg_fun(Mod, Conf),
	rarpanp_mg_encode_msg_fun(Mod, Conf)).
-define(rarpanp_mg_verify_service_change_rep_msg_fun(),
	rarpanp_mg_verify_service_change_rep_msg_fun()).
-define(rarpanp_mg_verify_notify_rep_msg_fun(TransId, TermId),
	rarpanp_mg_verify_notify_rep_msg_fun(TransId, TermId)).
-endif.

rarpanp_mg_event_sequence(text, tcp) ->
    DecodeFun = ?rarpanp_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?rarpanp_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = rarpanp_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = ?rarpanp_mg_verify_service_change_rep_msg_fun(),
    TransId = 2, 
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = rarpanp_mg_notify_request_msg(Mid, TransId, 1, TermId, 1),
    NrVerifyFun = ?rarpanp_mg_verify_notify_rep_msg_fun(TransId, TermId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 10000}},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
rarpanp_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarpanp_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarpanp_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch rarpanp_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

rarpanp_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("rarpanp_mg_verify_service_change_rep_msg -> entry with"
	      "~n   Mess:  ~p"
	      "~n", [Mess]),
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarpanp_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
rarpanp_mg_verify_notify_rep_msg_fun(TransId, TermId) ->
    fun(Msg) -> 
	    (catch rarpanp_mg_verify_notify_rep_msg(Msg, TransId, TermId)) 
    end.
-endif.

rarpanp_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
			     TransId, TermId) ->
    io:format("rarpanp_mg_verify_notify_rep_msg -> entry with"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Mess:    ~p"
	      "~n", [TransId, TermId, Mess]),
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = asn1_NOVALUE, % No ack
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyRep} ->
		NotifyRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,

    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    io:format("rarpanp_mg_verify_notify_rep_msg -> done when verifyed"
		      "~n", []),
            {ok, M};
	#'NotifyReply'{terminationID   = A,
		       errorDescriptor = B} ->
	    throw({error, {invalid_notifyReply, 
			   {A, TermId}, 
			   {B, asn1_NOVALUE}}});
	_ ->
	    throw({error, {invalid_notifyReply, NR}})
    end;
rarpanp_mg_verify_notify_rep_msg(_TransId, _TermId, Crap) ->
    {error, {invalid_message, Crap}}.

rarpanp_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpanp_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarpanp_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarpanp_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpanp_mg_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = rarpanp_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_pending_ack_one_pending(suite) ->
    [];
request_and_reply_pending_ack_one_pending(doc) ->
    ["This test case tests that megaco correctly handles the return "
     "value handle_pending_ack from handle_trans_request when ONE "
     "pending message has been sent"];
request_and_reply_pending_ack_one_pending(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rar_paop),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator"),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    %% MgcEvSeq = rarpaop_mgc_event_sequence(text, tcp),
    MgcEvSeq = rarpaop_mgc_event_sequence(binary, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    %% MgEvSeq = rarpaop_mg_event_sequence(text, tcp),
    MgEvSeq = rarpaop_mg_event_sequence(binary, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%
-ifdef(megaco_hipe_special).
-define(rarpaop_mgc_verify_handle_connect_fun(), 
        {?MODULE, rarpaop_mgc_verify_handle_connect, []}).
-define(rarpaop_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, rarpaop_mgc_verify_service_change_req, [Mid]}).
-define(rarpaop_mgc_verify_notify_req_fun(),
        {?MODULE, rarpaop_mgc_verify_notify_request, []}).
-define(rarpaop_mgc_verify_reply_ack_fun(),
	{?MODULE, rarpaop_mgc_verify_reply_ack, []}).
-define(rarpaop_mgc_verify_handle_disconnect_fun(),
        {?MODULE, rarpaop_mgc_verify_handle_disconnect, []}).
-else.
-define(rarpaop_mgc_verify_handle_connect_fun(), 
        fun rarpaop_mgc_verify_handle_connect/1).
-define(rarpaop_mgc_verify_service_change_req_fun(Mid),
        rarpaop_mgc_verify_service_change_req_fun(Mid)).
-define(rarpaop_mgc_verify_notify_req_fun(),
	rarpaop_mgc_verify_notify_request_fun()).
-define(rarpaop_mgc_verify_reply_ack_fun(),
	rarpaop_mgc_verify_reply_ack_fun()).
-define(rarpaop_mgc_verify_handle_disconnect_fun(),
	fun rarpaop_mgc_verify_handle_disconnect/1).
-endif.

rarpaop_mgc_event_sequence(text, tcp) ->
    Port      = 2944, 
    TranspMod = megaco_tcp, 
    EncMod    = megaco_pretty_text_encoder,
    EncConf   = [], 
    rarpaop_mgc_event_sequence(Port, TranspMod, EncMod, EncConf);
rarpaop_mgc_event_sequence(binary, tcp) ->
    Port      = 2945, 
    TranspMod = megaco_tcp, 
    EncMod    = megaco_ber_encoder,
    EncConf   = [], 
    rarpaop_mgc_event_sequence(Port, TranspMod, EncMod, EncConf).

rarpaop_mgc_event_sequence(Port, TranspMod, EncMod, EncConf) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             Port},
          {transport_module, TranspMod},
          {encoding_module,  EncMod},
          {encoding_config,  EncConf}
         ],
    ConnectVerify = ?rarpaop_mgc_verify_handle_connect_fun(), 
    ScrVerify     = ?rarpaop_mgc_verify_service_change_req_fun(Mid),
    NrVerify      = ?rarpaop_mgc_verify_notify_req_fun(),
    AckVerify     = ?rarpaop_mgc_verify_reply_ack_fun(),
    DiscoVerify   = ?rarpaop_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect,       ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request, ScrVerify},
	     {megaco_callback, handle_trans_request, NrVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
             {megaco_callback, nocall, 10000},
             {megaco_callback, handle_disconnect,    DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarpaop_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarpaop_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
-ifndef(megaco_hipe_special).
rarpaop_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    rarpaop_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

rarpaop_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch rarpaop_do_verify_service_change_req(AR, Mid));
rarpaop_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpaop_do_verify_service_change_req(AR, Mid) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarpaop_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
-ifndef(megaco_hipe_special).
rarpaop_mgc_verify_notify_request_fun() ->
    fun(Req) -> 
	    rarpaop_mgc_verify_notify_request(Req) 
    end.
-endif.

rarpaop_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarpaop_mgc_do_verify_notify_request(AR));
rarpaop_mgc_verify_notify_request(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpaop_mgc_do_verify_notify_request(AR) ->
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = notify_request_verified, 
	    Replies = [rarpaop_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_pending_ack, AckData}, Replies},
	    {ok, 5000, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


%% Ack verification
-ifndef(megaco_hipe_special).
rarpaop_mgc_verify_reply_ack_fun() ->
    fun(M) -> 
	    rarpaop_mgc_verify_reply_ack(M) 
    end.
-endif.

rarpaop_mgc_verify_reply_ack({handle_trans_ack, _, ?VERSION, ok, _}) ->
    io:format("rarpaop_mgc_verify_reply_ack -> ok~n", []),
    {ok, ok, ok};
rarpaop_mgc_verify_reply_ack({handle_trans_ack, _, ?VERSION, AS, AD} = Crap) ->
    io:format("rarpaop_mgc_verify_reply_ack -> incorrect ack-status:"
	      "~n   AS: ~p"
	      "~n   AD: ~p"
	      "~n", [AS, AD]),
    ED       = cre_ErrDesc({invalid_ack_status, {AS, AD}}),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply};
rarpaop_mgc_verify_reply_ack(Crap) ->
    io:format("rarpaop_mgc_verify_reply_ack -> invalid ack:"
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.


%% Disconnect verification
rarpaop_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarpaop_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarpaop_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarpaop_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(rarpaop_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(rarpaop_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(rarpaop_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, rarpaop_mg_verify_service_change_rep_msg, []}).
-define(rarpaop_mg_verify_pending_msg_fun(TransId),
	{?MODULE, rarpaop_mg_verify_pending_msg, [TransId]}).
-define(rarpaop_mg_verify_notify_rep_msg_fun(TransId, TermId),
	{?MODULE, rarpaop_mg_verify_notify_rep_msg, [TransId, TermId]}).
-else.
-define(rarpaop_mg_decode_msg_fun(Mod, Conf),
	rarpaop_mg_decode_msg_fun(Mod, Conf)).
-define(rarpaop_mg_encode_msg_fun(Mod, Conf),
	rarpaop_mg_encode_msg_fun(Mod, Conf)).
-define(rarpaop_mg_verify_service_change_rep_msg_fun(),
	rarpaop_mg_verify_service_change_rep_msg_fun()).
-define(rarpaop_mg_verify_pending_msg_fun(TransId),
	rarpaop_mg_verify_pending_msg_fun(TransId)).
-define(rarpaop_mg_verify_notify_rep_msg_fun(TransId, TermId),
	rarpaop_mg_verify_notify_rep_msg_fun(TransId, TermId)).
-endif.

rarpaop_mg_event_sequence(text, tcp) ->
    Port      = 2944, 
    EncMod    = megaco_pretty_text_encoder,
    EncConf   = [], 
    rarpaop_mg_event_sequence(Port, EncMod, EncConf);
rarpaop_mg_event_sequence(binary, tcp) ->
    Port      = 2945, 
    EncMod    = megaco_ber_encoder,
    EncConf   = [], 
    rarpaop_mg_event_sequence(Port, EncMod, EncConf).

rarpaop_mg_event_sequence(Port, EncMod, EncConf) ->
    DecodeFun = ?rarpaop_mg_decode_msg_fun(EncMod, EncConf),
    EncodeFun = ?rarpaop_mg_encode_msg_fun(EncMod, EncConf),
    Mid       = {deviceName, "mg"},
    TransId = 2, 
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    ServiceChangeReq = rarpaop_mg_service_change_request_msg(Mid, 1, 0),
    NotifyReq = rarpaop_mg_notify_request_msg(Mid, TransId, 1, TermId, 1),
    Ack = rarpaop_mg_ack_msg(Mid, TransId),
    ScrVerifyFun  = ?rarpaop_mg_verify_service_change_rep_msg_fun(),
    PendVerifyFun = ?rarpaop_mg_verify_pending_msg_fun(TransId),
    NrVerifyFun   = ?rarpaop_mg_verify_notify_rep_msg_fun(TransId, TermId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, Port},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {sleep, 2000},
             {send, "notify request", NotifyReq},
             {expect_receive, "pending", {PendVerifyFun, 5000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 5000}},
             {send, "reply ack", Ack},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
rarpaop_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarpaop_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarpaop_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch rarpaop_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

rarpaop_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarpaop_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
rarpaop_mg_verify_pending_msg_fun(TransId) ->
    fun(Msg) -> 
	    (catch rarpaop_mg_verify_pending_msg(Msg, TransId)) 
    end.
-endif.

rarpaop_mg_verify_pending_msg(#'MegacoMessage'{mess = Mess} = M, TransId) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TP = 
	case Trans of
            {transactionPending, TransPending} ->
		TransPending;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TP of
	#'TransactionPending'{transactionId = TransId} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_transactionPending, TP}})
    end;
rarpaop_mg_verify_pending_msg(Crap, _TransId) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
rarpaop_mg_verify_notify_rep_msg_fun(TransId, TermId) ->
    fun(Msg) -> 
	    (catch rarpaop_mg_verify_notify_rep_msg(Msg, TransId, TermId)) 
    end.
-endif.

rarpaop_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
				 TransId, TermId) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL', % Ack
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyRep} ->
		NotifyRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,

    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
            {ok, M};
	#'NotifyReply'{terminationID   = A,
		       errorDescriptor = B} ->
	    throw({error, {invalid_notifyReply, 
			   {A, TermId}, 
			   {B, asn1_NOVALUE}}});
	_ ->
	    throw({error, {invalid_notifyReply, NR}})
    end;
rarpaop_mg_verify_notify_rep_msg(Crap, _TransId, _TermId) ->
    {error, {invalid_message, Crap}}.

rarpaop_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpaop_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarpaop_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarpaop_mg_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarpaop_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpaop_mg_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = rarpaop_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_trans_req_and_reply(suite) ->
    [];
single_trans_req_and_reply(doc) ->
    ["Receive a (single) transaction request and then send a "
     "reply (discard ack). "
     "The MGC is a megaco instance (megaco event sequence) and the "
     "MG is emulated (tcp event sequence)"];
single_trans_req_and_reply(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        strar),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),


    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = strar_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = strar_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId], 30000),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(strar_mgc_verify_handle_connect_fun(), 
        {?MODULE, strar_mgc_verify_handle_connect, []}).
-define(strar_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, strar_mgc_verify_service_change_req, [Mid]}).
-define(strar_mgc_verify_notify_req_fun(),
        {?MODULE, strar_mgc_verify_notify_request, []}).
-define(strar_mgc_verify_handle_disconnect_fun(),
        {?MODULE, strar_mgc_verify_handle_disconnect, []}).
-else.
-define(strar_mgc_verify_handle_connect_fun(), 
        fun strar_mgc_verify_handle_connect/1).
-define(strar_mgc_verify_service_change_req_fun(Mid),
        strar_mgc_verify_service_change_req_fun(Mid)).
-define(strar_mgc_verify_notify_req_fun(),
	strar_mgc_verify_notify_request_fun()).
-define(strar_mgc_verify_handle_disconnect_fun(),
	fun strar_mgc_verify_handle_disconnect/1).
-endif.

strar_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?strar_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?strar_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?strar_mgc_verify_notify_req_fun(),
    DiscoVerify            = ?strar_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


strar_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("strar_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
strar_mgc_verify_handle_connect(Else) ->
    io:format("strar_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
strar_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    strar_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

strar_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch strar_mgc_do_verify_service_change_req(AR, Mid));
strar_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

strar_mgc_do_verify_service_change_req(AR, Mid) ->
    io:format("strar_mgc_verify_service_change_req -> entry with"
	      "~n   AR:  ~p"
	      "~n   Mid: ~p"
	      "~n", [AR, Mid]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
	end,
    Cmd =
        case CR of
            #'CommandRequest'{command = Command} ->
                Command;
            _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    {Tid, Parms} =
        case Cmd of
            {serviceChangeReq,
             #'ServiceChangeRequest'{terminationID = [TermID],
                                     serviceChangeParms = ServChParms}} ->
                {TermID, ServChParms};
            _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    case Tid of
        #megaco_term_id{contains_wildcards = false, id = ["root"]} ->
            ok;
        _ ->
            Err4      = {invalid_termination_id, Tid},
            ED4       = cre_ErrDesc(Tid),
            ErrReply4 = {discard_ack, ED4},
            throw({error, Err4, ErrReply4})
    end,
    case Parms of
        #'ServiceChangeParm'{serviceChangeMethod = restart,
                             serviceChangeReason = [[$9,$0,$1|_]]} ->
            AckData = [strar_mgc_service_change_reply_ar(Mid, 1)],
            Reply   = {discard_ack, AckData},
            {ok, AR, Reply};
        _ ->
            Err5      = {invalid_SCP, Parms},
            ED5       = cre_ErrDesc(Parms),
            ErrReply5 = {discard_ack, ED5},
            {error, Err5, ErrReply5}
    end.

-ifndef(megaco_hipe_special).
strar_mgc_verify_notify_request_fun() ->
    fun(Req) -> 
	    strar_mgc_verify_notify_request(Req) 
    end.
-endif.

strar_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch strar_mgc_do_verify_notify_request(AR));
strar_mgc_verify_notify_request(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.
    
strar_mgc_do_verify_notify_request(AR) ->
    io:format("strar_mgc_do_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    {Cid, CR} =
	case AR of
	    #'ActionRequest'{contextId       = CtxID, 
			     commandRequests = [CmdReq]} when (CtxID == 1) or
							      (CtxID == 2) ->
		{CtxID, CmdReq};
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
        end,
    Cmd =
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    NR =
        case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    {Tid, OED} =
        case NR of
            #'NotifyRequest'{terminationID            = [TermID],
                             observedEventsDescriptor = ObsEvsDesc,
                             errorDescriptor          = asn1_NOVALUE} ->
                {TermID, ObsEvsDesc};
            _ ->
                Err4      = {invalid_NR, NR},
                ED4       = cre_ErrDesc(NR),
                ErrReply4 = {discard_ack, ED4},
                throw({error, Err4, ErrReply4})
        end,
    OE =
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
            _ ->
                Err5      = {invalid_OED, OED},
                ED5       = cre_ErrDesc(NR),
                ErrReply5 = {discard_ack, ED5},
                throw({error, Err5, ErrReply5})
        end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
            Replies = [strar_mgc_notify_reply_ar(Cid, Tid)],
            Reply   = {discard_ack, Replies},
            {ok, AR, Reply};
        _ ->
            Err6      = {invalid_OE, OE},
            ED6       = cre_ErrDesc(OE),
            ErrReply6 = {discard_ack, ED6},
            {error, Err6, ErrReply6}
    end.

strar_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("strar_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
strar_mgc_verify_handle_disconnect(Else) ->
    io:format("strar_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


strar_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

strar_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

%% strar_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
%%     AR    = strar_mgc_notify_reply_ar(Cid, TermId),
%%     TRes  = cre_transResult([AR]),
%%     TR    = cre_transReply(TransId, TRes),
%%     Trans = cre_transaction(TR),
%%     Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
%%     cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(strar_mg_verify_handle_connect_fun(),
	{?MODULE, strar_mg_verify_handle_connect, []}).
-define(strar_mg_verify_service_change_reply_fun(),
	{?MODULE, strar_mg_verify_service_change_reply, []}).
-define(strar_mg_verify_notify_reply_fun(),
	{?MODULE, strar_mg_verify_notify_reply, []}).
-else.
-define(strar_mg_verify_handle_connect_fun(),
	strar_mg_verify_handle_connect_fun()).
-define(strar_mg_verify_service_change_reply_fun(),
	strar_mg_verify_service_change_reply_fun()).
-define(strar_mg_verify_notify_reply_fun(),
	strar_mg_verify_notify_reply_fun()).
-endif.

strar_mg_event_sequence(text, tcp) ->
    Mid = {deviceName, "mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [strar_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [strar_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?strar_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?strar_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?strar_mg_verify_notify_reply_fun(), 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     {megaco_trace, disable},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     connect,
	     {megaco_callback, handle_connect, ConnectVerify},
	     megaco_connect,
	     {megaco_cast, ServiceChangeReq, []},
	     {megaco_callback, handle_connect, ConnectVerify}, 
	     {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {sleep, 1000},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     {sleep, 1000},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

-ifndef(megaco_hipe_special).
strar_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    strar_mg_verify_handle_connect(Ev) 
    end.
-endif.

strar_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("strar_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
strar_mg_verify_handle_connect(Else) ->
    io:format("strar_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
strar_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> 
	    strar_mg_verify_service_change_reply(Rep) 
    end.
-endif.

strar_mg_verify_service_change_reply(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch strar_mg_do_verify_service_change_reply(AR));
strar_mg_verify_service_change_reply(Crap) ->
    {error, Crap, ok}.

strar_mg_do_verify_service_change_reply(AR) ->
    io:format("strar_mg_verify_service_change_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
strar_mg_verify_notify_reply_fun() ->
    fun(Rep) -> 
	    strar_mg_verify_notify_reply(Rep) 
    end.
-endif.
	     
strar_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			      {ok, [AR]}, _}) ->
    io:format("strar_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
strar_mg_verify_notify_reply(Else) ->
    io:format("strar_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

strar_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

strar_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_trans_req_and_reply_sendopts(suite) ->
    [];
single_trans_req_and_reply_sendopts(doc) ->
    ["Receive a (single) transaction request and then send a "
     "reply with handle_ack and a reply_timer in sendoptions. "
     "The MGC is a megaco instance (megaco event sequence) and the "
     "MG is emulated (tcp event sequence)"];
single_trans_req_and_reply_sendopts(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin, linux]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        straro),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),


    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = straro_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = straro_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId], 30000),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(straro_mgc_verify_handle_connect_fun(), 
        {?MODULE, straro_mgc_verify_handle_connect, []}).
-define(straro_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, straro_mgc_verify_service_change_req, [Mid]}).
-define(straro_mgc_verify_notify_req_fun(),
        {?MODULE, straro_mgc_verify_notify_request, []}).
-define(straro_mgc_verify_handle_trans_ack_fun(),
        {?MODULE, straro_mgc_verify_handle_trans_ack, []}).
-else.
-define(straro_mgc_verify_handle_connect_fun(), 
        fun straro_mgc_verify_handle_connect/1).
-define(straro_mgc_verify_service_change_req_fun(Mid),
        straro_mgc_verify_service_change_req_fun(Mid)).
-define(straro_mgc_verify_notify_req_fun(),
	straro_mgc_verify_notify_request_fun()).
-define(straro_mgc_verify_handle_trans_ack_fun(),
	straro_mgc_verify_handle_trans_ack_fun()).
-endif.

straro_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?straro_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?straro_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?straro_mgc_verify_notify_req_fun(),
    TransAckVerify         = ?straro_mgc_verify_handle_trans_ack_fun(), 
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     TransAckVerify},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


straro_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("straro_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
straro_mgc_verify_handle_connect(Else) ->
    io:format("straro_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
straro_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    straro_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

straro_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch straro_mgc_do_verify_service_change_req(AR, Mid));
straro_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

straro_mgc_do_verify_service_change_req(AR, Mid) ->
    io:format("straro_mgc_do_verify_service_change_req -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
	end,
    Cmd =
        case CR of
            #'CommandRequest'{command = Command} ->
                Command;
            _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    {Tid, Parms} =
        case Cmd of
            {serviceChangeReq,
             #'ServiceChangeRequest'{terminationID = [TermID],
                                     serviceChangeParms = ServChParms}} ->
                {TermID, ServChParms};
            _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    case Tid of
        #megaco_term_id{contains_wildcards = false, id = ["root"]} ->
            ok;
        _ ->
            Err4      = {invalid_termination_id, Tid},
            ED4       = cre_ErrDesc(Tid),
            ErrReply4 = {discard_ack, ED4},
            throw({error, Err4, ErrReply4})
    end,
    case Parms of
        #'ServiceChangeParm'{serviceChangeMethod = restart,
                             serviceChangeReason = [[$9,$0,$1|_]]} ->
            AckData = [straro_mgc_service_change_reply_ar(Mid, 1)],
            Reply   = {discard_ack, AckData},
            {ok, AR, Reply};
        _ ->
            Err5      = {invalid_SCP, Parms},
            ED5       = cre_ErrDesc(Parms),
            ErrReply5 = {discard_ack, ED5},
            {error, Err5, ErrReply5}
    end.

-ifndef(megaco_hipe_special).
straro_mgc_verify_notify_request_fun() ->
    fun(Req) -> 
	    straro_mgc_verify_notify_request(Req) 
    end.
-endif.

straro_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch straro_mgc_do_verify_notify_request(AR));
straro_mgc_verify_notify_request(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.
    
straro_mgc_do_verify_notify_request(AR) ->
    io:format("straro_mgc_do_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    {Cid, CR} =
	case AR of
	    #'ActionRequest'{contextId       = CtxID, 
			     commandRequests = [CmdReq]} when (CtxID == 1) or
							      (CtxID == 2) ->
		{CtxID, CmdReq};
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
        end,
    Cmd =
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    NR =
        case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    {Tid, OED} =
        case NR of
            #'NotifyRequest'{terminationID            = [TermID],
                             observedEventsDescriptor = ObsEvsDesc,
                             errorDescriptor          = asn1_NOVALUE} ->
                {TermID, ObsEvsDesc};
            _ ->
                Err4      = {invalid_NR, NR},
                ED4       = cre_ErrDesc(NR),
                ErrReply4 = {discard_ack, ED4},
                throw({error, Err4, ErrReply4})
        end,
    OE =
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
            _ ->
                Err5      = {invalid_OED, OED},
                ED5       = cre_ErrDesc(NR),
                ErrReply5 = {discard_ack, ED5},
                throw({error, Err5, ErrReply5})
        end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
            Replies = [straro_mgc_notify_reply_ar(Cid, Tid)],
	    SendOpts = [{protocol_version, 99}],
            Reply   = {{handle_ack, get(tc)}, Replies, SendOpts},
            {ok, AR, Reply};
        _ ->
            Err6      = {invalid_OE, OE},
            ED6       = cre_ErrDesc(OE),
            ErrReply6 = {discard_ack, ED6},
            {error, Err6, ErrReply6}
    end.

-ifndef(megaco_hipe_special).
straro_mgc_verify_handle_trans_ack_fun() ->
    fun(Ack) -> 
	    straro_mgc_verify_handle_trans_ack(Ack) 
    end.
-endif.

straro_mgc_verify_handle_trans_ack(
  {handle_trans_ack, _CH, ?VERSION, AS, _AD}) -> 
    (catch straro_mgc_do_verify_handle_trans_ack(AS));
straro_mgc_verify_handle_trans_ack(Crap) ->
    io:format("straro_mgc_verify_handle_trans_ack -> entry with"
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

straro_mgc_do_verify_handle_trans_ack({error, {EM, EF, [EC, Version, Msg], Reason}}) ->
    io:format("straro_mgc_do_handle_verify_handle_trans_ack -> entry with"
	      "~n   EM:      ~p"
	      "~n   EF:      ~p"
	      "~n   EC:      ~p"
	      "~n   Version: ~p"
	      "~n   Msg:     ~p"
	      "~n   Reason:  ~p"
	      "~n", [EM, EF, EC, Version, Msg, Reason]),
    case Reason of
	{bad_version, 99} ->
	    {ok, Reason, ok};
	_ ->
	    {error, {unexpected_reason, Reason}, ok}
    end;
straro_mgc_do_verify_handle_trans_ack(Else) ->
    io:format("straro_mgc_verify_handle_trans_ack -> unknown"
	      "~n   Else: ~p"
	      "~n", [Else]),
    {error, Else, ok}.

%% straro_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
%%     io:format("straro_mgc_verify_handle_disconnect -> ok"
%% 	      "~n   CH: ~p"
%% 	      "~n   R:  ~p"
%% 	      "~n", [CH, R]),
%%     {ok, CH, ok};
%% straro_mgc_verify_handle_disconnect(Else) ->
%%     io:format("straro_mgc_verify_handle_disconnect -> unknown"
%% 	      "~n   Else: ~p~n", [Else]),
%%     {error, Else, ok}.


straro_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

straro_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

%% straro_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
%%     AR    = straro_mgc_notify_reply_ar(Cid, TermId),
%%     TRes  = cre_transResult([AR]),
%%     TR    = cre_transReply(TransId, TRes),
%%     Trans = cre_transaction(TR),
%%     Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
%%     cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(straro_mg_verify_handle_connect_fun(),
	{?MODULE, straro_mg_verify_handle_connect, []}).
-define(straro_mg_verify_service_change_reply_fun(),
	{?MODULE, straro_mg_verify_service_change_reply, []}).
-define(straro_mg_verify_handle_disconnect_fun(),
	{?MODULE, straro_mg_verify_handle_disconnect, []}).
-else.
-define(straro_mg_verify_handle_connect_fun(),
	straro_mg_verify_handle_connect_fun()).
-define(straro_mg_verify_service_change_reply_fun(),
	straro_mg_verify_service_change_reply_fun()).
-define(straro_mg_verify_handle_disconnect_fun(),
	fun straro_mg_verify_handle_disconnect/1).
-endif.

straro_mg_event_sequence(text, tcp) ->
    Mid = {deviceName, "mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [straro_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [straro_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?straro_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?straro_mg_verify_service_change_reply_fun(), 
    DiscoVerify              = ?straro_mg_verify_handle_disconnect_fun(), 
%%     ConnectVerify            = straro_mg_verify_handle_connect_fun(), 
%%     DiscoVerify              = fun straro_mg_verify_handle_disconnect/1,
%%     ServiceChangeReplyVerify = straro_mg_verify_service_change_reply_fun(), 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     {megaco_trace, disable},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     connect,
	     {megaco_callback, handle_connect, ConnectVerify},
	     megaco_connect,
	     {megaco_cast, ServiceChangeReq, []},
	     {megaco_callback, handle_connect, ConnectVerify}, 
	     {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {sleep, 1000},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     {sleep, 1000},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_callback, handle_disconnect, DiscoVerify}, 
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

-ifndef(megaco_hipe_special).
straro_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    straro_mg_verify_handle_connect(Ev) 
    end.
-endif.

straro_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("straro_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
straro_mg_verify_handle_connect(Else) ->
    io:format("straro_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straro_mg_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("straro_mg_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
straro_mg_verify_handle_disconnect(Else) ->
    io:format("straro_mg_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
straro_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> 
	    straro_mg_verify_service_change_reply(Rep) 
    end.
-endif.

straro_mg_verify_service_change_reply(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch straro_mg_do_verify_service_change_reply(AR));
straro_mg_verify_service_change_reply(Crap) ->
    {error, Crap, ok}.

straro_mg_do_verify_service_change_reply(AR) ->
    io:format("straro_mg_verify_service_change_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

%% -ifndef(megaco_hipe_special).
%% straro_mg_verify_notify_reply_fun() ->
%%     fun(Rep) -> 
%% 	    straro_mg_verify_notify_reply(Rep) 
%%     end.
%% -endif.
	     
%% straro_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
%% 			      {ok, [AR]}, _}) ->
%%     io:format("straro_mg_verify_notify_reply -> ok"
%% 	      "~n   AR: ~p~n", [AR]),
%%     {ok, AR, ok};
%% straro_mg_verify_notify_reply(Else) ->
%%     io:format("straro_mg_verify_notify_reply -> unknown"
%% 	      "~n   Else: ~p~n", [Else]),
%%     {error, Else, ok}.

straro_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

straro_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_and_ack(suite) ->
    [];
request_and_reply_and_ack(doc) ->
    ["This test case tests that megaco correctly handles three-way-handshake"];
request_and_reply_and_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        raraa),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = raraa_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = raraa_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

-ifdef(megaco_hipe_special).
-define(raraa_mgc_verify_handle_connect_fun(), 
        {?MODULE, raraa_mgc_verify_handle_connect, []}).
-define(raraa_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, raraa_mgc_verify_service_change_req, [Mid]}).
-define(raraa_mgc_verify_notify_req_fun(),
        {?MODULE, raraa_mgc_verify_notify_req, []}).
-define(raraa_mgc_verify_handle_trans_ack_fun(),
        {?MODULE, raraa_mgc_verify_handle_trans_ack, []}).
-define(raraa_mgc_verify_handle_disconnect_fun(),
        {?MODULE, raraa_mgc_verify_handle_disconnect, []}).
-else.
-define(raraa_mgc_verify_handle_connect_fun(), 
        fun raraa_mgc_verify_handle_connect/1).
-define(raraa_mgc_verify_service_change_req_fun(Mid),
        raraa_mgc_verify_service_change_req_fun(Mid)).
-define(raraa_mgc_verify_notify_req_fun(),
	raraa_mgc_verify_notify_req_fun()).
-define(raraa_mgc_verify_handle_trans_ack_fun(),
        raraa_mgc_verify_handle_trans_ack_fun()).
-define(raraa_mgc_verify_handle_disconnect_fun(),
	fun raraa_mgc_verify_handle_disconnect/1).
-endif.

raraa_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ?raraa_mgc_verify_handle_connect_fun(), 
    ScrVerify     = ?raraa_mgc_verify_service_change_req_fun(Mid),
    NrVerify      = ?raraa_mgc_verify_notify_req_fun(),
    AckVerify     = ?raraa_mgc_verify_handle_trans_ack_fun(),
    DiscoVerify   = ?raraa_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect,           ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,     ScrVerify},
	     {megaco_callback, handle_trans_request,     NrVerify},
	     {megaco_callback, handle_trans_ack,         AckVerify},
             {megaco_callback, handle_disconnect,        DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
raraa_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
raraa_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
-ifndef(megaco_hipe_special).
raraa_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    raraa_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

raraa_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch raraa_do_verify_service_change_req(AR, Mid));
raraa_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

raraa_do_verify_service_change_req(AR, Mid) ->
    io:format("raraa_mgc_verify_service_change_req -> entry with"
	      "~n   AR: ~p"
	      "~n   Mid: ~p"
	      "~n", [AR, Mid]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [raraa_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
-ifndef(megaco_hipe_special).
raraa_mgc_verify_notify_req_fun() ->
    fun(Req) -> 
	    raraa_mgc_verify_notify_req(Req) 
    end.
-endif.

raraa_mgc_verify_notify_req({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch raraa_mgc_do_verify_notify_req(AR));
raraa_mgc_verify_notify_req(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

raraa_mgc_do_verify_notify_req(AR) ->
    io:format("raraa_mgc_verify_notify_req -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = raraa, 
	    Replies = [raraa_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


-ifndef(megaco_hipe_special).
raraa_mgc_verify_handle_trans_ack_fun() ->
    fun(Ack) -> 
	    raraa_mgc_verify_handle_trans_ack(Ack) 
    end.
-endif.

raraa_mgc_verify_handle_trans_ack(
  {handle_trans_ack, CH, ?VERSION, ok, raraa}) ->
    io:format("raraa_mgc_verify_handle_trans_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
raraa_mgc_verify_handle_trans_ack(Crap) ->
    io:format("raraa_mgc_verify_handle_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
raraa_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
raraa_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

raraa_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

raraa_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(raraa_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(raraa_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(raraa_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, raraa_mg_verify_service_change_rep_msg, []}).
-define(raraa_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	{?MODULE, raraa_mg_verify_notify_rep_msg, [TermId, TransId, ReqId, CtxId]}).
-else.
-define(raraa_mg_decode_msg_fun(Mod, Conf),
	raraa_mg_decode_msg_fun(Mod, Conf)).
-define(raraa_mg_encode_msg_fun(Mod, Conf),
	raraa_mg_encode_msg_fun(Mod, Conf)).
-define(raraa_mg_verify_service_change_rep_msg_fun(),
	raraa_mg_verify_service_change_rep_msg_fun()).
-define(raraa_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	raraa_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId)).
-endif.

raraa_mg_event_sequence(text, tcp) ->
    DecodeFun = ?raraa_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?raraa_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = raraa_mg_service_change_request_msg(Mid, 1, 0),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = raraa_mg_notify_request_msg(Mid, TermId, 
					    TransId, ReqId, CtxId),
    TransAck = raraa_mg_trans_ack_msg(Mid, TransId),
    ScrVerifyFun = ?raraa_mg_verify_service_change_rep_msg_fun(),
    NrVerifyFun  = ?raraa_mg_verify_notify_rep_msg_fun(TermId, 
						       TransId, ReqId, CtxId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 10000}},
             {send, "transaction-ack", TransAck},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
raraa_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
raraa_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
raraa_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch raraa_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

raraa_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
raraa_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
raraa_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch raraa_mg_verify_notify_rep_msg(Msg, 
						  TermId, TransId, Rid, Cid)) 
    end.
-endif.

raraa_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
			   TermId, TransId, Rid, Cid) ->
    io:format("raraa_mg_verify_notify_rep_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
raraa_mg_verify_notify_rep_msg(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

raraa_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

raraa_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = raraa_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

raraa_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

raraa_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = raraa_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

raraa_mg_trans_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_and_no_ack(suite) ->
    [];
request_and_reply_and_no_ack(doc) ->
    ["This test case tests that megaco handles a failed three-way-handshake,"
     " i.e. when the ack never arrives"];
request_and_reply_and_no_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rarana),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rarana_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rarana_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]), 

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

-ifdef(megaco_hipe_special).
-define(rarana_mgc_verify_handle_connect_fun(), 
        {?MODULE, rarana_mgc_verify_handle_connect, []}).
-define(rarana_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, rarana_mgc_verify_service_change_req, [Mid]}).
-define(rarana_mgc_verify_notify_req_fun(),
        {?MODULE, rarana_mgc_verify_notify_req, []}).
-define(rarana_mgc_verify_handle_trans_ack_fun(),
	{?MODULE, rarana_mgc_verify_handle_trans_ack, []}).
-define(rarana_mgc_verify_handle_disconnect_fun(),
        {?MODULE, rarana_mgc_verify_handle_disconnect, []}).
-else.
-define(rarana_mgc_verify_handle_connect_fun(), 
        fun rarana_mgc_verify_handle_connect/1).
-define(rarana_mgc_verify_service_change_req_fun(Mid),
        rarana_mgc_verify_service_change_req_fun(Mid)).
-define(rarana_mgc_verify_notify_req_fun(),
	rarana_mgc_verify_notify_req_fun()).
-define(rarana_mgc_verify_handle_trans_ack_fun(),
	rarana_mgc_verify_handle_trans_ack_fun()).
-define(rarana_mgc_verify_handle_disconnect_fun(),
	fun rarana_mgc_verify_handle_disconnect/1).
-endif.

rarana_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ?rarana_mgc_verify_handle_connect_fun(), 
    ScrVerify     = ?rarana_mgc_verify_service_change_req_fun(Mid),
    NrVerify      = ?rarana_mgc_verify_notify_req_fun(),
    AckVerify     = ?rarana_mgc_verify_handle_trans_ack_fun(),
    DiscoVerify   = ?rarana_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             {megaco_update_user_info, reply_timer,        9000},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect,           ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,     ScrVerify},
	     {megaco_callback, handle_trans_request,     NrVerify},
	     {megaco_callback, handle_trans_ack,         AckVerify},
	     %% {megaco_callback, nocall,                   8000},
             {megaco_callback, handle_disconnect,        DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarana_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarana_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
-ifndef(megaco_hipe_special).
rarana_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    rarana_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

rarana_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch rarana_do_verify_service_change_req(AR, Mid));
rarana_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarana_do_verify_service_change_req(AR, Mid) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarana_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
-ifndef(megaco_hipe_special).
rarana_mgc_verify_notify_req_fun() ->
    fun(Req) -> 
	    rarana_mgc_verify_notify_req(Req) 
    end.
-endif.

rarana_mgc_verify_notify_req({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarana_mgc_do_verify_notify_req(AR));
rarana_mgc_verify_notify_req(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarana_mgc_do_verify_notify_req(AR) ->
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = rarana, 
	    Replies = [rarana_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


-ifndef(megaco_hipe_special).
rarana_mgc_verify_handle_trans_ack_fun() ->
    fun(Ack) -> 
	    rarana_mgc_verify_handle_trans_ack(Ack) 
    end.
-endif.

rarana_mgc_verify_handle_trans_ack({handle_trans_ack, CH, ?VERSION, 
			     {error, timeout}, rarana}) ->
    io:format("rarana_mgc_verify_handle_trans_ack -> expected error: ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
rarana_mgc_verify_handle_trans_ack(Crap) ->
    io:format("rarana_mgc_verify_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
rarana_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarana_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarana_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarana_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(rarana_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(rarana_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(rarana_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, rarana_mg_verify_service_change_rep_msg, []}).
-define(rarana_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	{?MODULE, rarana_mg_verify_notify_rep_msg, [TermId, TransId, ReqId, CtxId]}).
-else.
-define(rarana_mg_decode_msg_fun(Mod, Conf),
	rarana_mg_decode_msg_fun(Mod, Conf)).
-define(rarana_mg_encode_msg_fun(Mod, Conf),
	rarana_mg_encode_msg_fun(Mod, Conf)).
-define(rarana_mg_verify_service_change_rep_msg_fun(),
	rarana_mg_verify_service_change_rep_msg_fun()).
-define(rarana_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	rarana_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId)).
-endif.

rarana_mg_event_sequence(text, tcp) ->
    DecodeFun = ?rarana_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?rarana_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = rarana_mg_service_change_request_msg(Mid, 1, 0),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = rarana_mg_notify_request_msg(Mid, TermId, 
					    TransId, ReqId, CtxId),
    ScrVerifyFun = ?rarana_mg_verify_service_change_rep_msg_fun(),
    NrVerifyFun  = ?rarana_mg_verify_notify_rep_msg_fun(TermId, 
							TransId, ReqId, CtxId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 10000}},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
rarana_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarana_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarana_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch rarana_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

rarana_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarana_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
rarana_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rarana_mg_verify_notify_rep_msg(Msg, 
						   TermId, TransId, Rid, Cid)) 
    end.
-endif.

rarana_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
				TermId, TransId, Rid, Cid) ->
    io:format("rarana_mg_verify_notify_rep_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
rarana_mg_verify_notify_rep_msg(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

rarana_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarana_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarana_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarana_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarana_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = rarana_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_and_late_ack(suite) ->
    [];
request_and_reply_and_late_ack(doc) ->
    ["This test case tests that megaco handles three-way-handshake "
     "when the ack is late (and requeire a retransmission)"];
request_and_reply_and_late_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rarala),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rarala_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rarala_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]), 

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

-ifdef(megaco_hipe_special).
-define(rarala_mgc_verify_handle_connect_fun(), 
        {?MODULE, rarala_mgc_verify_handle_connect, []}).
-define(rarala_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, rarala_mgc_verify_service_change_req, [Mid]}).
-define(rarala_mgc_verify_notify_req_fun(),
        {?MODULE, rarala_mgc_verify_notify_req, []}).
-define(rarala_mgc_verify_handle_trans_ack_fun(),
	{?MODULE, rarala_mgc_verify_handle_trans_ack, []}).
-define(rarala_mgc_verify_handle_disconnect_fun(),
        {?MODULE, rarala_mgc_verify_handle_disconnect, []}).
-else.
-define(rarala_mgc_verify_handle_connect_fun(), 
        fun rarala_mgc_verify_handle_connect/1).
-define(rarala_mgc_verify_service_change_req_fun(Mid),
        rarala_mgc_verify_service_change_req_fun(Mid)).
-define(rarala_mgc_verify_notify_req_fun(),
	rarala_mgc_verify_notify_req_fun()).
-define(rarala_mgc_verify_handle_trans_ack_fun(),
	rarala_mgc_verify_handle_trans_ack_fun()).
-define(rarala_mgc_verify_handle_disconnect_fun(),
	fun rarala_mgc_verify_handle_disconnect/1).
-endif.

rarala_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    RepTmr = #megaco_incr_timer{wait_for    = 3000,
				factor      = 1, 
				incr        = 0,
				max_retries = 2
			       },    
    ConnectVerify = ?rarala_mgc_verify_handle_connect_fun(), 
    ScrVerify     = ?rarala_mgc_verify_service_change_req_fun(Mid),
    NrVerify      = ?rarala_mgc_verify_notify_req_fun(),
    AckVerify     = ?rarala_mgc_verify_handle_trans_ack_fun(),
    DiscoVerify   = ?rarala_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             {megaco_update_user_info, reply_timer,        RepTmr},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect,           ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,     ScrVerify},
	     {megaco_callback, handle_trans_request,     NrVerify},
	     {megaco_callback, handle_trans_ack,         AckVerify},
             {megaco_callback, handle_disconnect,        DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarala_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarala_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
-ifndef(megaco_hipe_special).
rarala_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    rarala_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

rarala_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch rarala_do_verify_service_change_req(AR, Mid));
rarala_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarala_do_verify_service_change_req(AR, Mid) ->
    io:format("rarala_mgc_do_verify_service_change_req -> entry with"
	      "~n   AR:  ~p"
	      "~n   Mid: ~p"
	      "~n", [AR, Mid]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarala_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
-ifndef(megaco_hipe_special).
rarala_mgc_verify_notify_req_fun() ->
    fun(Req) -> 
	    rarala_mgc_verify_notify_req(Req) 
    end.
-endif.

rarala_mgc_verify_notify_req({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarala_mgc_do_verify_notify_req(AR));
rarala_mgc_verify_notify_req(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarala_mgc_do_verify_notify_req(AR) ->
    io:format("rarala_mgc_do_verify_notify_req -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = rarala, 
	    Replies = [rarala_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


-ifndef(megaco_hipe_special).
rarala_mgc_verify_handle_trans_ack_fun() ->
    fun(Ack) -> 
	    rarala_mgc_verify_handle_trans_ack(Ack) 
    end.
-endif.

rarala_mgc_verify_handle_trans_ack(
  {handle_trans_ack, CH, ?VERSION, ok, rarala}) ->
    io:format("rarala_mgc_verify_handle_trans_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
rarala_mgc_verify_handle_trans_ack(Crap) ->
    io:format("rarala_mgc_verify_handle_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
rarala_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarala_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarala_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarala_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(rarala_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(rarala_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(rarala_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, rarala_mg_verify_service_change_rep_msg, []}).
-define(rarala_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	{?MODULE, rarala_mg_verify_notify_rep_msg, [TermId, TransId, ReqId, CtxId]}).
-else.
-define(rarala_mg_decode_msg_fun(Mod, Conf),
	rarala_mg_decode_msg_fun(Mod, Conf)).
-define(rarala_mg_encode_msg_fun(Mod, Conf),
	rarala_mg_encode_msg_fun(Mod, Conf)).
-define(rarala_mg_verify_service_change_rep_msg_fun(),
	rarala_mg_verify_service_change_rep_msg_fun()).
-define(rarala_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	rarala_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId)).
-endif.

rarala_mg_event_sequence(text, tcp) ->
    DecodeFun = ?rarala_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?rarala_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = rarala_mg_service_change_request_msg(Mid, 1, 0),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = rarala_mg_notify_request_msg(Mid, TermId, 
					    TransId, ReqId, CtxId),
    TransAck = rarala_mg_trans_ack_msg(Mid, TransId),
    ScrVerifyFun = ?rarala_mg_verify_service_change_rep_msg_fun(),
    NrVerifyFun  = ?rarala_mg_verify_notify_rep_msg_fun(TermId, 
							TransId, ReqId, CtxId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {send, "transaction-ack", TransAck},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
rarala_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarala_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rarala_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch rarala_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

rarala_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarala_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
rarala_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rarala_mg_verify_notify_rep_msg(Msg, 
						   TermId, TransId, Rid, Cid)) 
    end.
-endif.

rarala_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
				TermId, TransId, Rid, Cid) ->
    io:format("rarala_mg_verify_notify_rep_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
rarala_mg_verify_notify_rep_msg(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

rarala_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarala_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarala_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarala_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarala_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = rarala_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarala_mg_trans_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans_req_and_reply_and_req(suite) ->
    [];
trans_req_and_reply_and_req(doc) ->
    ["Receive a transaction request, send a reply (discard ack)"
     "then receive the same reply again after the timeout. "
     "The MGC is a megaco instance (megaco event sequence) and the "
     "MG is emulated (tcp event sequence)"];
trans_req_and_reply_and_req(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin, linux]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        trarar),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),


    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = trarar_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = trarar_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId], 60000),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(trarar_mgc_verify_handle_connect_fun(), 
        {?MODULE, trarar_mgc_verify_handle_connect, []}).
-define(trarar_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, trarar_mgc_verify_service_change_req, [Mid]}).
-define(trarar_mgc_verify_notify_req_fun(Cid),
        {?MODULE, trarar_mgc_verify_notify_req, [Cid]}).
-define(trarar_mgc_verify_handle_disconnect_fun(),
        {?MODULE, trarar_mgc_verify_handle_disconnect, []}).
-else.
-define(trarar_mgc_verify_handle_connect_fun(), 
        fun trarar_mgc_verify_handle_connect/1).
-define(trarar_mgc_verify_service_change_req_fun(Mid),
        trarar_mgc_verify_service_change_req_fun(Mid)).
-define(trarar_mgc_verify_notify_req_fun(Cid),
	trarar_mgc_verify_notify_req_fun(Cid)).
-define(trarar_mgc_verify_handle_disconnect_fun(),
	fun trarar_mgc_verify_handle_disconnect/1).
-endif.

trarar_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?trarar_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?trarar_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify1       = ?trarar_mgc_verify_notify_req_fun(1),
    NotifyReqVerify2       = ?trarar_mgc_verify_notify_req_fun(2),
    NotifyReqVerify3       = ?trarar_mgc_verify_notify_req_fun(3),
    NotifyReqVerify4       = ?trarar_mgc_verify_notify_req_fun(4),
    DiscoVerify            = ?trarar_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, reply_timer, 2000},
	     start_transport,
	     listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify1},
	     {megaco_callback, handle_trans_request, NotifyReqVerify2},
             {megaco_update_conn_info, reply_timer,  4000},
	     {megaco_callback, handle_trans_request, NotifyReqVerify3},
	     {megaco_callback, handle_trans_request, NotifyReqVerify4},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


trarar_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("trarar_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
trarar_mgc_verify_handle_connect(Else) ->
    io:format("trarar_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
trarar_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    trarar_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

trarar_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch trarar_mgc_do_verify_service_change_req(AR, Mid));
trarar_mgc_verify_service_change_req(Crap, _Mid) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

trarar_mgc_do_verify_service_change_req(AR, Mid) ->
    io:format("trarar_mgc_verify_service_change_req -> ok"
	      "~n   AR:  ~p"
	      "~n   Mid: ~p"
	      "~n", [AR, Mid]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
	end,
    Cmd =
        case CR of
            #'CommandRequest'{command = Command} ->
                Command;
            _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    {Tid, Parms} =
        case Cmd of
            {serviceChangeReq,
             #'ServiceChangeRequest'{terminationID = [TermID],
                                     serviceChangeParms = ServChParms}} ->
                {TermID, ServChParms};
            _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    case Tid of
        #megaco_term_id{contains_wildcards = false, id = ["root"]} ->
            ok;
        _ ->
            Err4      = {invalid_termination_id, Tid},
            ED4       = cre_ErrDesc(Tid),
            ErrReply4 = {discard_ack, ED4},
            throw({error, Err4, ErrReply4})
    end,
    case Parms of
        #'ServiceChangeParm'{serviceChangeMethod = restart,
                             serviceChangeReason = [[$9,$0,$1|_]]} ->
            AckData = [trarar_mgc_service_change_reply_ar(Mid, 1)],
            Reply   = {discard_ack, AckData},
            {ok, AR, Reply};
        _ ->
            Err5      = {invalid_SCP, Parms},
            ED5       = cre_ErrDesc(Parms),
            ErrReply5 = {discard_ack, ED5},
            {error, Err5, ErrReply5}
    end.

-ifndef(megaco_hipe_special).
trarar_mgc_verify_notify_req_fun(Cid) ->
    fun(Req) -> 
	    trarar_mgc_verify_notify_req(Req, Cid) 
    end.
-endif.

trarar_mgc_verify_notify_req({handle_trans_request, _, ?VERSION, [AR]}, Cid) ->
    (catch trarar_mgc_do_verify_notify_req(AR, Cid));
trarar_mgc_verify_notify_req(Crap, _Cid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.
    
trarar_mgc_do_verify_notify_req(AR, Cid) ->
    io:format("trarar_mgc_do_verify_notify_req -> entry with"
	      "~n   AR:  ~p"
	      "~n   Cid: ~p"
	      "~n", [AR, Cid]),
    {ContextID, CR} =
	case AR of
	    #'ActionRequest'{contextId       = CtxID, 
			     commandRequests = [CmdReq]} when (CtxID == Cid) ->
		{CtxID, CmdReq};
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
        end,
    Cmd =
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    NR =
        case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    {Tid, OED} =
        case NR of
            #'NotifyRequest'{terminationID            = [TermID],
                             observedEventsDescriptor = ObsEvsDesc,
                             errorDescriptor          = asn1_NOVALUE} ->
                {TermID, ObsEvsDesc};
            _ ->
                Err4      = {invalid_NR, NR},
                ED4       = cre_ErrDesc(NR),
                ErrReply4 = {discard_ack, ED4},
                throw({error, Err4, ErrReply4})
        end,
    OE =
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
            _ ->
                Err5      = {invalid_OED, OED},
                ED5       = cre_ErrDesc(NR),
                ErrReply5 = {discard_ack, ED5},
                throw({error, Err5, ErrReply5})
        end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
            Replies = [trarar_mgc_notify_reply_ar(ContextID, Tid)],
            Reply   = {discard_ack, Replies},
            {ok, AR, Reply};
        _ ->
            Err6      = {invalid_OE, OE},
            ED6       = cre_ErrDesc(OE),
            ErrReply6 = {discard_ack, ED6},
            {error, Err6, ErrReply6}
    end.

trarar_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("trarar_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
trarar_mgc_verify_handle_disconnect(Else) ->
    io:format("trarar_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


trarar_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

trarar_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

%% trarar_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
%%     AR    = trarar_mgc_notify_reply_ar(Cid, TermId),
%%     TRes  = cre_transResult([AR]),
%%     TR    = cre_transReply(TransId, TRes),
%%     Trans = cre_transaction(TR),
%%     Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
%%     cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(trarar_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(trarar_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(trarar_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, trarar_mg_verify_service_change_rep_msg, []}).
-define(trarar_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	{?MODULE, trarar_mg_verify_notify_rep_msg, [TermId, TransId, ReqId, CtxId]}).
-else.
-define(trarar_mg_decode_msg_fun(Mod, Conf),
	trarar_mg_decode_msg_fun(Mod, Conf)).
-define(trarar_mg_encode_msg_fun(Mod, Conf),
	trarar_mg_encode_msg_fun(Mod, Conf)).
-define(trarar_mg_verify_service_change_rep_msg_fun(),
	trarar_mg_verify_service_change_rep_msg_fun()).
-define(trarar_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	trarar_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId)).
-endif.

trarar_mg_event_sequence(text, tcp) ->
    DecodeFun = ?trarar_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?trarar_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = trarar_mg_service_change_request_msg(Mid, 1, 0),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq1 = 
	trarar_mg_notify_request_msg(Mid, TermId, 2, 1, 1),
    NotifyReq2 = 
	trarar_mg_notify_request_msg(Mid, TermId, 2, 2, 2),
    NotifyReq3 = 
	trarar_mg_notify_request_msg(Mid, TermId, 2, 3, 3),
    NotifyReq4 = 
	trarar_mg_notify_request_msg(Mid, TermId, 2, 4, 4),
    ScrVerifyFun = ?trarar_mg_verify_service_change_rep_msg_fun(),
    NrVerifyFun1 = 
	?trarar_mg_verify_notify_rep_msg_fun(TermId, 2, 1, 1),
    NrVerifyFun2 = 
	?trarar_mg_verify_notify_rep_msg_fun(TermId, 2, 2, 2),
    NrVerifyFun3 = 
	?trarar_mg_verify_notify_rep_msg_fun(TermId, 2, 3, 3),
    NrVerifyFun4 = 
	?trarar_mg_verify_notify_rep_msg_fun(TermId, 2, 4, 4),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
	     
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},

	     %% the original setting for reply timer is 2000
             {send, "notify request 1", NotifyReq1},
             {expect_receive, "notify-reply 1", {NrVerifyFun1, 2500}},
	     {sleep, 1000}, 
             {send, "notify request 2", NotifyReq2},
             {expect_receive, "notify-reply 2 (resend of 1)", {NrVerifyFun1, 2500}},
	     {sleep, 3000}, % reply timer is set to 2000
             {send, "notify request 3 (same as 2)", NotifyReq2},
             {expect_receive, "notify-reply 3", {NrVerifyFun2, 2500}},

	     %% reply timer is now set to 4000 but previous was 2000
	     %% so, 3000 is enough to let the timer running with the
	     %% previous settings (2000) to time out
	     {sleep, 3000}, 
             {send, "notify request 4", NotifyReq3},
             {expect_receive, "notify-reply 4", {NrVerifyFun3, 4500}},
	     {sleep, 5000}, 
             {send, "notify request 5", NotifyReq4},
             {expect_receive, "notify-reply 5", {NrVerifyFun4, 4500}},

             {expect_nothing, 5000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
trarar_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
trarar_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
trarar_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch trarar_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

trarar_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
trarar_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
trarar_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch trarar_mg_verify_notify_rep_msg(Msg, 
						   TermId, TransId, Rid, Cid)) 
    end.
-endif.

trarar_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
				TermId, TransId, Rid, Cid) ->
    io:format("trarar_mg_verify_notify_rep_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    io:format("trarar_mg_verify_notify_rep_msg -> "
	      "~n   Body: ~p"
	      "~n", [Body]),
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    io:format("trarar_mg_verify_notify_rep_msg -> "
	      "~n   Trans: ~p"
	      "~n", [Trans]),
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    io:format("trarar_mg_verify_notify_rep_msg -> "
	      "~n   TR: ~p"
	      "~n", [TR]),
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    io:format("trarar_mg_verify_notify_rep_msg -> "
	      "~n   TRes: ~p"
	      "~n", [TRes]),
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    io:format("trarar_mg_verify_notify_rep_msg -> "
	      "~n   AR: ~p"
	      "~n", [AR]),
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
trarar_mg_verify_notify_rep_msg(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

trarar_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

trarar_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = trarar_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

trarar_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

trarar_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = trarar_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

%% trarar_mg_trans_ack_msg(Mid, TransId) ->
%%     TR    = cre_transRespAck(cre_transAck(TransId)),
%%     Trans = cre_transaction(TR),
%%     Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
%%     cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pending_ack_plain(suite) ->
    [];
pending_ack_plain(doc) ->
    ["Receive a request and handle it as a long request, "
     "i.e. return with {pending, _} and expect a call to the "
     "long request function"];
pending_ack_plain(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        pap),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = pap_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = pap_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]), 

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

-ifdef(megaco_hipe_special).
-define(pap_mgc_verify_handle_connect_fun(), 
        {?MODULE, pap_mgc_verify_handle_connect, []}).
-define(pap_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, pap_mgc_verify_service_change_req, [Mid]}).
-define(pap_mgc_verify_notify_req_fun(),
        {?MODULE, pap_mgc_verify_notify_req, []}).
-define(pap_mgc_verify_notify_req_long_fun(),
        {?MODULE, pap_mgc_verify_notify_req_long, []}).
-define(pap_mgc_verify_handle_trans_ack_fun(),
	{?MODULE, pap_mgc_verify_handle_trans_ack, []}).
-define(pap_mgc_verify_handle_disconnect_fun(),
        {?MODULE, pap_mgc_verify_handle_disconnect, []}).
-else.
-define(pap_mgc_verify_handle_connect_fun(), 
        fun pap_mgc_verify_handle_connect/1).
-define(pap_mgc_verify_service_change_req_fun(Mid),
        pap_mgc_verify_service_change_req_fun(Mid)).
-define(pap_mgc_verify_notify_req_fun(),
	pap_mgc_verify_notify_req_fun()).
-define(pap_mgc_verify_notify_req_long_fun(),
	pap_mgc_verify_notify_req_long_fun()).
-define(pap_mgc_verify_handle_trans_ack_fun(),
	pap_mgc_verify_handle_trans_ack_fun()).
-define(pap_mgc_verify_handle_disconnect_fun(),
	fun pap_mgc_verify_handle_disconnect/1).
-endif.

pap_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ?pap_mgc_verify_handle_connect_fun(), 
    ScrVerify     = ?pap_mgc_verify_service_change_req_fun(Mid),
    NrVerify1     = ?pap_mgc_verify_notify_req_fun(),
    NrVerify2     = ?pap_mgc_verify_notify_req_long_fun(),
    AckVerify     = ?pap_mgc_verify_handle_trans_ack_fun(),
    DiscoVerify   = ?pap_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect,            ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,      ScrVerify},
	     {megaco_callback, handle_trans_request,      NrVerify1},
	     {megaco_callback, handle_trans_long_request, NrVerify2},
	     {megaco_callback, handle_trans_ack,          AckVerify},
             {megaco_callback, handle_disconnect,         DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
pap_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
pap_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
-ifndef(megaco_hipe_special).
pap_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> 
	    pap_mgc_verify_service_change_req(Req, Mid) 
    end.
-endif.

pap_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    (catch pap_do_verify_service_change_req(AR, Mid));
pap_mgc_verify_service_change_req(Crap, _Mid) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

pap_do_verify_service_change_req(AR, Mid) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [pap_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
-ifndef(megaco_hipe_special).
pap_mgc_verify_notify_req_fun() ->
    fun(Req) -> 
	    pap_mgc_verify_notify_req(Req) 
    end.
-endif.

pap_mgc_verify_notify_req({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("pap_mgc_verify_notify_req -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    Reply = {pending, AR}, 
    {ok, AR, Reply};
pap_mgc_verify_notify_req(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

%% Notify Request verification
-ifndef(megaco_hipe_special).
pap_mgc_verify_notify_req_long_fun() ->
    fun(Req) -> 
	    pap_mgc_verify_notify_req_long(Req) 
    end.
-endif.

pap_mgc_verify_notify_req_long(
  {handle_trans_long_request, _, ?VERSION, AR}) ->
    (catch pap_mgc_do_verify_notify_req_long(AR));
pap_mgc_verify_notify_req_long(Crap) ->
    ED       = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

pap_mgc_do_verify_notify_req_long(AR) ->
    io:format("pap_mgc_do_verify_notify_req_long -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = pap, 
	    Replies = [pap_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


-ifndef(megaco_hipe_special).
pap_mgc_verify_handle_trans_ack_fun() ->
    fun(Ack) -> 
	    pap_mgc_verify_handle_trans_ack(Ack) 
    end.
-endif.

pap_mgc_verify_handle_trans_ack({handle_trans_ack, CH, ?VERSION, ok, pap}) ->
    io:format("pap_mgc_verify_handle_trans_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
pap_mgc_verify_handle_trans_ack(Crap) ->
    io:format("pap_mgc_verify_handle_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
pap_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
pap_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

pap_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

pap_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(pap_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(pap_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(pap_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, pap_mg_verify_service_change_rep_msg, []}).
-define(pap_mg_verify_pending_msg_fun(TransId),
	{?MODULE, pap_mg_verify_pending_msg, [TransId]}).
-define(pap_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	{?MODULE, pap_mg_verify_notify_rep_msg, [TermId, TransId, ReqId, CtxId]}).
-else.
-define(pap_mg_decode_msg_fun(Mod, Conf),
	pap_mg_decode_msg_fun(Mod, Conf)).
-define(pap_mg_encode_msg_fun(Mod, Conf),
	pap_mg_encode_msg_fun(Mod, Conf)).
-define(pap_mg_verify_service_change_rep_msg_fun(),
	pap_mg_verify_service_change_rep_msg_fun()).
-define(pap_mg_verify_pending_msg_fun(TransId),
	pap_mg_verify_pending_msg_fun(TransId)).
-define(pap_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
	pap_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId)).
-endif.

pap_mg_event_sequence(text, tcp) ->
    DecodeFun = ?pap_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?pap_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = pap_mg_service_change_request_msg(Mid, 1, 0),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = 
	pap_mg_notify_request_msg(Mid, TermId, TransId, ReqId, CtxId),
    TransAck = pap_mg_trans_ack_msg(Mid, TransId),
    ScrVerifyFun = ?pap_mg_verify_service_change_rep_msg_fun(),
    PendingVerifyFun = 
	?pap_mg_verify_pending_msg_fun(TransId),
    NrVerifyFun = 
	?pap_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "pending",      {PendingVerifyFun, 4000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {send, "transaction-ack", TransAck},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
pap_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
pap_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
pap_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch pap_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

pap_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
pap_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
pap_mg_verify_pending_msg_fun(TransId) ->
    fun(Msg) -> 
	    (catch pap_mg_verify_pending_msg(Msg, TransId)) 
    end.
-endif.

pap_mg_verify_pending_msg(#'MegacoMessage'{mess = Mess} = M, TransId) ->
    io:format("pap_mg_verify_pending_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TransId: ~p"
	      "~n", [M, TransId]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TP = 
	case Trans of
            {transactionPending, TransPending} ->
		TransPending;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TP of
	#'TransactionPending'{transactionId = TransId} ->
	    {ok, M};
	_ ->
	    {error, {invalid_transactionPending, TP}}
    end;
pap_mg_verify_pending_msg(Crap, _TransId) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
pap_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch pap_mg_verify_notify_rep_msg(Msg, 
						TermId, TransId, Rid, Cid)) 
    end.
-endif.

pap_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
			   TermId, TransId, Rid, Cid) ->
    io:format("pap_mg_verify_notify_rep_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
pap_mg_verify_notify_rep_msg(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

pap_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

pap_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = pap_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

pap_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

pap_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = pap_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

pap_mg_trans_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_pending_and_late_reply(suite) ->
    [];
request_and_pending_and_late_reply(doc) ->
    ["Receive a request and handle it as a long request, "
     "i.e. return with {pending, _}. Then, expect the sender "
     "to keep re-sending the request until the reply is sent."];
request_and_pending_and_late_reply(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rapalr),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rapalr_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rapalr_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]), 

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

-ifdef(megaco_hipe_special).
-define(rapalr_mgc_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(rapalr_mgc_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(rapalr_mgc_verify_service_change_req_msg_fun(),
	{?MODULE, rapalr_mgc_verify_service_change_req_msg, []}).
-define(rapalr_mgc_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId),
	{?MODULE, rapalr_mgc_verify_notify_req_msg, [TermId, TransId, ReqId, CtxId]}).
-define(rapalr_mgc_verify_trans_ack_msg_fun(TransId),
	{?MODULE, rapalr_mgc_verify_trans_ack_msg, [TransId]}).
-else.
-define(rapalr_mgc_decode_msg_fun(Mod, Conf),
	rapalr_mgc_decode_msg_fun(Mod, Conf)).
-define(rapalr_mgc_encode_msg_fun(Mod, Conf),
	rapalr_mgc_encode_msg_fun(Mod, Conf)).
-define(rapalr_mgc_verify_service_change_req_msg_fun(),
	rapalr_mgc_verify_service_change_req_msg_fun()).
-define(rapalr_mgc_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId),
	rapalr_mgc_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId)).
-define(rapalr_mgc_verify_trans_ack_msg_fun(TransId),
	rapalr_mgc_verify_trans_ack_msg_fun(TransId)).
-endif.

rapalr_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    DecodeFun = ?rapalr_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?rapalr_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ServiceChangeRep = rapalr_mgc_service_change_reply_msg(Mid, 1),
    TermId           = 
	#megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId   = 2,
    ReqId     = 1,
    CtxId     = 1, 
    Pending      = rapalr_mgc_trans_pending_msg(Mid, TransId),
    NotifyRep    = rapalr_mgc_notify_reply_msg(Mid, TransId, 
					       CtxId, TermId),
    ScrVerifyFun = ?rapalr_mgc_verify_service_change_req_msg_fun(),
    NrVerifyFun  = 
	?rapalr_mgc_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId),
    AckVerifyFun = ?rapalr_mgc_verify_trans_ack_msg_fun(TransId),
    EvSeq = [{debug,  false},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},

             %% ANNOUNCE READY
             {trigger, "announce ready", fun() -> CTRL ! announce_mgc end}, 

	     {expect_accept, any},
             {expect_receive, "service-change-request", {ScrVerifyFun, 5000}},
             {send, "service-change-reply", ServiceChangeRep},
             {expect_receive, "notify-request(1)", {NrVerifyFun, 4000}},
             {send, "pending", Pending},
             {expect_receive, "notify-request(2)", {NrVerifyFun, 4000}},
             {expect_receive, "notify-request(3)", {NrVerifyFun, 4000}},
             {send, "notify reply", NotifyRep},
             {expect_receive, "ack", {AckVerifyFun, 4000}},
             {sleep, 1000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
rapalr_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rapalr_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
rapalr_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch rapalr_mgc_verify_service_change_req_msg(Msg)) 
    end.
-endif.

rapalr_mgc_verify_service_change_req_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

-ifndef(megaco_hipe_special).
rapalr_mgc_verify_notify_req_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rapalr_mgc_verify_notify_req_msg(Msg, 
						    TermId, 
						    TransId, Rid, Cid)) 
    end.
-endif.

rapalr_mgc_verify_notify_req_msg(#'MegacoMessage'{mess = Mess} = M,
			     TermId, TransId, Rid, Cid) ->
    io:format("rapalr_mgc_verify_notify_req_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = TransId,
				  actions       = [ActReq]} ->
		ActReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = Cid,
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_actions, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		throw({error, {invalid_commandRequests, CR}})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    OED = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		ObsEvsDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		throw({error, {invalid_observedEventsDescriptor, OED}})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_observedEventLst, OE}})
    end;
rapalr_mgc_verify_notify_req_msg(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.

-ifndef(megaco_hipe_special).
rapalr_mgc_verify_trans_ack_msg_fun(TransId) ->
    fun(Msg) -> 
	    (catch rapalr_mgc_verify_trans_ack_msg(Msg, TransId)) 
    end.
-endif.

rapalr_mgc_verify_trans_ack_msg(#'MegacoMessage'{mess = Mess} = M, 
				TransId) ->
    io:format("rapalr_mgc_verify_trans_ack_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TransId: ~p"
	      "~n", [M, TransId]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TA = 
	case Trans of
            {transactionResponseAck, [TransAck]} ->
		TransAck;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TA of
            #'TransactionAck'{firstAck = TransId,
			      lastAck  = asn1_NOVALUE} ->
		{ok, M};
	    _ ->
		throw({error, {invalid_transactionResponseAck, TA}})
    end;
rapalr_mgc_verify_trans_ack_msg(Crap, _TransId) ->
    {error, {invalid_MegacoMessage, Crap}}.

rapalr_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(1, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rapalr_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

rapalr_mgc_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    AR    = rapalr_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, 'NULL', TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rapalr_mgc_trans_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.



%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(rapalr_mg_verify_handle_connect_fun(),
	{?MODULE, rapalr_mg_verify_handle_connect, []}).
-define(rapalr_mg_verify_service_change_rep_fun(),
	{?MODULE, rapalr_mg_verify_service_change_rep, []}).
-define(rapalr_mg_verify_notify_rep_fun(),
	{?MODULE, rapalr_mg_verify_notify_rep, []}).
-else.
-define(rapalr_mg_verify_handle_connect_fun(),
	rapalr_mg_verify_handle_connect_fun()).
-define(rapalr_mg_verify_service_change_rep_fun(),
	rapalr_mg_verify_service_change_rep_fun()).
-define(rapalr_mg_verify_notify_rep_fun(),
	rapalr_mg_verify_notify_rep_fun()).
-endif.

rapalr_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    LReqTmr = #megaco_incr_timer{wait_for    = 3000,
				 factor      = 1, 
				 incr        = 0,
				 max_retries = 2
				},    
    ServiceChangeReq = rapalr_mg_service_change_request_ar(Mid, 1),
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = rapalr_mg_notify_request_ar(1, Tid, 1),
    ConnectVerify            = ?rapalr_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = ?rapalr_mg_verify_service_change_rep_fun(),
    NotifyReplyVerify        = ?rapalr_mg_verify_notify_rep_fun(),
%%     ConnectVerify            = rapalr_mg_verify_handle_connect_fun(),
%%     ServiceChangeReplyVerify = rapalr_mg_verify_service_change_reply_fun(),
%%     NotifyReplyVerify        = rapalr_mg_verify_notify_reply_fun(),
    EvSeq = [
             {debug, false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             {megaco_update_user_info, long_request_resend, true},
	     {megaco_update_user_info, long_request_timer,  LReqTmr}, 
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
rapalr_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    rapalr_mg_verify_handle_connect(Ev) 
    end.
-endif.

rapalr_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("rapalr_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
rapalr_mg_verify_handle_connect(Else) ->
    io:format("rapalr_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
rapalr_mg_verify_service_change_rep_fun() ->
    fun(Rep) -> 
	    rapalr_mg_verify_service_change_rep(Rep) 
    end.
-endif.

rapalr_mg_verify_service_change_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch rapalr_mg_do_verify_service_change_rep(AR));
rapalr_mg_verify_service_change_rep(Crap) ->
    {error, Crap, ok}.

rapalr_mg_do_verify_service_change_rep(AR) ->
    io:format("rapalr_mg_verify_service_change_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
rapalr_mg_verify_notify_rep_fun() ->
    fun(Rep) -> 
	    rapalr_mg_verify_notify_rep(Rep) 
    end.
-endif.
	     
rapalr_mg_verify_notify_rep({handle_trans_reply, _CH, ?VERSION, 
			      {ok, [AR]}, _}) ->
    io:format("rapalr_mg_verify_notify_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
rapalr_mg_verify_notify_rep(Else) ->
    io:format("rapalr_mg_verify_notify_rep -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


rapalr_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rapalr_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dist(suite) ->
    [];
dist(Config) when is_list(Config) ->
    ?SKIP("Needs a re-write..."),
    [_Local, Dist] = ?ACQUIRE_NODES(2, Config),
    d("dist -> start proxy",[]),
    ?USER_MOD:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = ?USER_MOD,
    d("dist -> start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],

    d("dist -> start megaco user MG (~p)",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),

    d("dist -> start megaco user MGC (~p)",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    d("dist -> retrieve (user info) receive_handle for MG",[]),
    MgRH = user_info(MgMid, receive_handle),

    d("dist -> retrieve (user info) receive_handle for MGC",[]),
    MgcRH = user_info(MgcMid, receive_handle), 

    d("dist -> start transport",[]),
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},

    d("dist -> (MG) connect",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel

    d("dist -> (MG) await connect",[]),
    ?USER({connect, PrelMgCH, _V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("dist -> (MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("dist -> (MGC) await auto-connect",[]),
    ?USER({connect, MgcCH, _V, []}, ok), % Mgc auto


    Rep = service_change_reply(MgcMid),
    d("dist -> (MGC) "
      "await service change request and send reply when received",[]),
    ?USER({request, MgcCH, _V, [[Req]]}, {discard_ack, [Rep]}),

    d("dist -> (MG) await connect",[]),
    ?USER({connect, MgCH, _V, []}, ok), % Mg confirm

    d("dist -> (MG) await service change reply",[]),
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    %% Dist
    d("dist -> start megaco on ~p", [Dist]),
    ?VERIFY(ok,	rpc:call(Dist, megaco, start, [])),

    d("dist -> start megaco user on ~p", [Dist]),
    ?VERIFY(ok,	rpc:call(Dist, megaco, start_user, [MgcMid, UserConfig])),

    d("dist -> (MG) connect to MGC", []),
    MgcPid = self(),
    MgcSH  = {element(2, MgSH), element(1, MgSH)},
    ?SEND(rpc:call(Dist, megaco, connect, [MgcRH, MgMid, MgcSH, MgcPid])), % Mgc dist

    d("dist -> (MGC) await auto-connect (from MG on ~p)", [Dist]),
    ?USER({connect, MgcCH, _V, []}, ok), % Mgc dist auto
    ?RECEIVE([{res, _, {ok, MgcCH}}]),

    d("dist -> (~p:MG) send service change request",[Dist]),
    ?SEND(rpc:call(Dist, megaco, call, [MgcCH, [Req], []])),

    d("dist -> (MG????????) "
      "await service change request and send reply when received",[]),
    ?USER({request, MgCH, _V, [[Req]]}, {discard_ack, [Rep]}),
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    d("dist -> retreive some info",[]),
    connections([MgCH, MgcCH]),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    ?VERIFY([MgcCH], rpc:call(Dist, megaco, system_info, [connections])),
    ?VERIFY([], rpc:call(Dist, megaco, user_info, [MgMid, connections])),
    ?VERIFY([MgcCH], rpc:call(Dist, megaco, user_info, [MgcMid, connections])),

    %% Shutdown

    d("dist -> close down the shop...",[]),
    Reason = shutdown,
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),

    d("dist -> stop proxy",[]),
    ?USER_MOD:stop_proxy(),

    d("dist -> done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_4359(suite) ->
    [];
otp_4359(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Mid = {deviceName, "dummy_mid"},

    io:format("otp_4359 -> start megaco application~n", []),
    ?VERIFY(ok, application:start(megaco)),

    %% megaco:enable_trace(max, io),
    io:format("otp_4359 -> start and configure megaco user~n", []),
    ?VERIFY(ok,	megaco:start_user(Mid, [{send_mod,      ?MODULE},
					{request_timer, infinity},
					{reply_timer,   infinity}])),

    io:format("otp_4359 -> update user info: user_mod -> ~p~n", [?MODULE]),
    ?VERIFY(ok, megaco:update_user_info(Mid, user_mod,  ?MODULE)),
    io:format("otp_4359 -> update user info: user_args -> ~p~n", [self()]),
    ?VERIFY(ok, megaco:update_user_info(Mid, user_args, [self()])),
    io:format("otp_4359 -> retreive receive_handle~n", []),
    RH0 = user_info(Mid, receive_handle),
    io:format("otp_4359 -> RH0: ~p~n", [RH0]),
    RH1 = RH0#megaco_receive_handle{send_mod        = ?MODULE,
				    encoding_mod    = megaco_compact_text_encoder,
				    encoding_config = []},

    %% First an erroneous transaction (missing the transaction id number)
    %% then an valid transaction.
    M = "!/1 ml2 "
	"T={C=${A=${M{O {MO=SR,RG=OFF,RV=OFF}}}}}"
	"T=1{C=${A=${M{O {MO=SR,RG=OFF,RV=OFF}}}}}",

    %% Simulate incomming message
    %% Will result in an (auto) connect first
    io:format("otp_4359 -> simulate receive message~n", []),
    megaco:receive_message(RH1, self(), self(), list_to_binary(M)),
    io:format("otp_4359 -> await actions~n", []),
    Actions = otp_4359_await_actions([{handle_connect, ignore}, 
				      {send_message, ?megaco_bad_request}, 
				      {handle_trans_request, ignore}, 
				      {send_message, ?megaco_not_implemented}]), 
    io:format("otp_4359 -> analyze actions~n", []),
    otp_4359_analyze_result(RH1, Actions),

    Conns = megaco:system_info(connections),
    io:format("otp_4359 -> connections~n~p~n", [Conns]),
    OKs   = lists:duplicate(length(Conns),ok),
    io:format("otp_4359 -> verify (all) connection disconnect~n", []),
    ?VERIFY(OKs, [megaco:disconnect(CH, test_complete) || CH <- Conns]),
    io:format("otp_4359 -> stop user (~p)~n", [Mid]),
    stop_user(Mid),
    io:format("otp_4359 -> stop megaco application~n", []),
    ?VERIFY(ok, application:stop(megaco)),
    io:format("otp_4359 -> make sure we have nothing in the message queue~n", []),
    ?RECEIVE([]),
    io:format("otp_4359 -> done~n", []),
    ok.


otp_4359_await_actions(Exp) ->
    otp_4359_await_actions(Exp, []).

otp_4359_await_actions([], Rep) ->
    lists:reverse(Rep);
otp_4359_await_actions([{M,I}|R] = _All, Rep) ->
    receive
	{M, Info} ->
	    io:format("otp_4359 -> received expected event [~w]~n", [M]),
	    otp_4359_await_actions(R, [{M, I, Info}|Rep])
%% 	Else ->
%% 	    exit({received_unexpected_message, M, Else})
%% 	    %% io:format("received unexpected: ~p~n", [Else]),
%% 	    %% otp_4359_await_actions(All, Rep)
    after 10000 ->
	    exit({timeout,megaco_test_lib:flush()} )
    end.

otp_4359_analyze_result(_RH, []) ->
    ok;
otp_4359_analyze_result(RH, 
			[{send_message, ExpErrorCode, EncodedMessage}|L]) ->
    io:format("otp_4359_analyze_result -> send_message: ", []),
    otp_4359_analyze_encoded_message(RH, ExpErrorCode, EncodedMessage),
    otp_4359_analyze_result(RH,L);
otp_4359_analyze_result(RH, [{M,ignore,_}|T]) ->
    io:format("otp_4359_analyze_result -> ignoring ~p~n", [M]),
    otp_4359_analyze_result(RH,T).

otp_4359_analyze_encoded_message(RH, ExpErrorCode, M) 
  when is_record(RH, megaco_receive_handle) andalso is_binary(M) ->
    #megaco_receive_handle{encoding_mod    = Mod,
			   encoding_config = Conf} = RH,
    case (catch Mod:decode_message(Conf, M)) of
	{ok, #'MegacoMessage'{mess = #'Message'{messageBody = Body}}} ->
	    case Body of
		{transactions, [{transactionReply,Reply}]} ->
		    case Reply of
			#'TransactionReply'{transactionResult = Result} ->
			    case Result of
				{transactionError,ED} when is_record(ED, 'ErrorDescriptor') ->
				    case ED#'ErrorDescriptor'.errorCode of
					ExpErrorCode ->
					    io:format("error code ~p ok~n", [ExpErrorCode]),
					    ok;
					Code ->
					    io:format("error code ~p erroneous~n", [Code]),
					    exit({unexpected_error_code, ExpErrorCode, Code})
				    end;
				_ ->
				    io:format("unexpected trans result~n", []),
				    exit({unexpected_trans_result, Result})
			    end;
			_ ->
			    io:format("unexpected trans reply~n", []),
			    exit({unexpected_trans_reply, Reply})
		    end;
		_ ->
		    io:format("unexpected body~n", []),
		    exit({unexpected_body, Body})
	    end;

	Else ->
	    io:format("unexpected decode result~n", []),
	    exit({unexpected_decode_result, Else})
    end.
	    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_4836(suite) ->
    [];
otp_4836(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_4836),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("start the MGC simulator (generator)"),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("create the MGC event sequence"),
    MgcEvSeq = otp_4836_mgc_event_sequence(text, tcp),

    d("start the MGC simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    i("[MG] start"),    
    MgMid = {deviceName, "mg"},
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                factor      = 1, 
                                incr        = 0,
                                max_retries = 3},
    %% 5000,  %% Does not matter since we will not use this anyway...
    LongReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                    factor      = 1, 
                                    incr        = 0,
                                    max_retries = 3},
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
                {long_request_timer, LongReqTmr}, 
                {pending_timer,      PendingTmr},
                {reply_timer,        ReplyTmr}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("[MG] send the notify"),
    {ok, Reply} = (catch ?MG_NOTIF_RAR(Mg)),
    {1, {ok, [AR]}} = Reply,
    d("[MG] ActionReply: ~p", [AR]),

    d("await the generator reply"),
    await_completion([MgcId]),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    i("done", []),
    ok.


-ifdef(megaco_hipe_special).
-define(otp_4836_mgc_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(otp_4836_mgc_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(otp_4836_mgc_verify_service_change_req_msg_fun(),
	{?MODULE, otp_4836_mgc_verify_service_change_req_msg, []}).
-define(otp_4836_mgc_verify_notify_req_msg_fun(),
	{?MODULE, otp_4836_mgc_verify_notify_req_msg, []}).
-else.
-define(otp_4836_mgc_decode_msg_fun(Mod, Conf),
	otp_4836_mgc_decode_msg_fun(Mod, Conf)).
-define(otp_4836_mgc_encode_msg_fun(Mod, Conf),
	otp_4836_mgc_encode_msg_fun(Mod, Conf)).
-define(otp_4836_mgc_verify_service_change_req_msg_fun(),
	otp_4836_mgc_verify_service_change_req_msg_fun()).
-define(otp_4836_mgc_verify_notify_req_msg_fun(),
	otp_4836_mgc_verify_notify_req_msg_fun()).
-endif.

otp_4836_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    DecodeFun = ?otp_4836_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?otp_4836_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"ctrl"},
    ServiceChangeReply = otp_4836_service_change_reply_msg(Mid, 1, 0),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReply = otp_4836_notify_reply_msg(Mid, 2, 0, TermId),
    Pending = otp_4836_pending_msg(Mid,2),
    ServiceChangeVerifyFun = ?otp_4836_mgc_verify_service_change_req_msg_fun(),
    NotifyReqVerifyFun     = ?otp_4836_mgc_verify_notify_req_msg_fun(),
    MgcEvSeq = [{debug,  true},
		{decode, DecodeFun},
		{encode, EncodeFun},
		{listen, 2944},

                %% ANNOUNCE READY
                {trigger, "announce ready", fun() -> CTRL ! announce_mgc end}, 

		{expect_accept, any},
		{expect_receive, "service-change-request", {ServiceChangeVerifyFun, 10000}}, 
		{send, "service-change-reply", ServiceChangeReply}, 
		{expect_receive, "notify-request", {NotifyReqVerifyFun, 10000}},
		{send, "pending 1", Pending}, 
		{sleep, 100},
		{send, "pending 2", Pending}, 
		{sleep, 500},
  		{send, "notify-reply", NotifyReply}, 
		{sleep, 2000}
	       ],
    MgcEvSeq.

otp_4836_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms,SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
				  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_4836_msg(Mid, TransId, CR, Cid).

otp_4836_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_4836_msg(Mid, TransId, CR, Cid).

otp_4836_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
			 commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
			       transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_4836_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


-ifndef(megaco_hipe_special).
otp_4836_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    encode_msg(M, Mod, Conf)
    end.
-endif.
%% otp_4836_mgc_encode_msg_fun(Mod, Conf, Ver) ->
%%     fun(M) -> 
%% 	    encode_msg(M, Mod, Conf, Ver)
%%     end.

-ifndef(megaco_hipe_special).
otp_4836_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    decode_msg(M, Mod, Conf)
    end.
-endif.
%% otp_4836_mgc_decode_msg_fun(Mod, Conf, Ver) ->
%%     fun(M) -> 
%% 	    decode_msg(M, Mod, Conf, Ver)
%%     end.

%% otp_4836_verify_msg_fun() ->
%%     fun(M) -> 
%% 	    {ok, M} 
%%     end.

-ifndef(megaco_hipe_special).
otp_4836_mgc_verify_service_change_req_msg_fun() ->
    fun(M) ->
	    otp_4836_mgc_verify_service_change_req_msg(M)
    end.
-endif.

otp_4836_mgc_verify_service_change_req_msg(
  #'MegacoMessage'{mess = Mess} = M) -> 
    #'Message'{version     = _V,
	       mId         = _Mid,
	       messageBody = Body} = Mess,
    {transactions, [Trans]} = Body,
    {transactionRequest, TR} = Trans,
    #'TransactionRequest'{transactionId = _Tid,
			  actions = [AR]} = TR,
    #'ActionRequest'{contextId = _Cid,
		     contextRequest = _CtxReq,
		     contextAttrAuditReq = _CtxAar,
		     commandRequests = [CR]} = AR,
    #'CommandRequest'{command = Cmd,
		      optional = _Opt,
		      wildcardReturn = _WR} = CR,
    {serviceChangeReq, SCR} = Cmd,
	    #'ServiceChangeRequest'{terminationID = _TermID,
				    serviceChangeParms = SCP} = SCR,
    #'ServiceChangeParm'{serviceChangeMethod = restart,
			 serviceChangeReason = [[$9,$0,$1|_]]} = SCP,
    {ok, M};
otp_4836_mgc_verify_service_change_req_msg(M) ->
    {error, {invalid_message, M}}.

-ifndef(megaco_hipe_special).
otp_4836_mgc_verify_notify_req_msg_fun() ->
    fun(M) ->
	    otp_4836_mgc_verify_notify_req_msg(M)
    end.
-endif.

otp_4836_mgc_verify_notify_req_msg(#'MegacoMessage'{mess = Mess} = M) -> 
    #'Message'{messageBody = Body} = Mess,
    {transactions, [Trans]} = Body,
    {transactionRequest, TR} = Trans,
    #'TransactionRequest'{actions = [AR]} = TR,
    #'ActionRequest'{commandRequests = [CR1,CR2]} = AR,
    #'CommandRequest'{command = Cmd1} = CR1,
    {notifyReq, NR1} = Cmd1,
    #'NotifyRequest'{observedEventsDescriptor = OED1} = NR1,
    #'ObservedEventsDescriptor'{observedEventLst = [OE1]} = OED1,
    #'ObservedEvent'{eventName = "al/of"} = OE1,
    #'CommandRequest'{command = Cmd2} = CR2,
    {notifyReq, NR2} = Cmd2,
    #'NotifyRequest'{observedEventsDescriptor = OED2} = NR2,
    #'ObservedEventsDescriptor'{observedEventLst = [OE2]} = OED2,
    #'ObservedEvent'{eventName = "al/of"} = OE2,
    {ok, M};
otp_4836_mgc_verify_notify_req_msg(M) ->
    {error, {invalid_message, M}}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5805(suite) ->
    [];
otp_5805(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_5805),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
	 "~n   MgcNode: ~p"
	 "~n   MgNode:  ~p", 
	 [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp_5805_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = 
	megaco_test_megaco_generator:exec(Mgc, MgcEvSeq, timer:minutes(1)),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("start the MG simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("create the MG event sequence"),
    MgEvSeq = otp_5805_mg_event_sequence(text, tcp),

    d("start the MG simulation"),
    {ok, MgId} = 
	megaco_test_tcp_generator:exec(Mg, MgEvSeq, timer:minutes(1)),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]), 

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("done", []),
    ok.


-ifdef(megaco_hipe_special).
-define(otp_5805_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(otp_5805_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(otp_5805_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, otp_5805_mg_verify_service_change_rep_msg, []}).
-define(otp_5805_mg_verify_error_descriptor_msg_fun(),
	{?MODULE, otp_5805_mg_verify_error_descriptor_msg, []}).
-else.
-define(otp_5805_mg_decode_msg_fun(Mod, Conf),
	otp_5805_mg_decode_msg_fun(Mod, Conf)).
-define(otp_5805_mg_encode_msg_fun(Mod, Conf),
	otp_5805_mg_encode_msg_fun(Mod, Conf)).
-define(otp_5805_mg_verify_service_change_rep_msg_fun(),
	otp_5805_mg_verify_service_change_rep_msg_fun()).
-define(otp_5805_mg_verify_error_descriptor_msg_fun(),
	otp_5805_mg_verify_error_descriptor_msg_fun()).
-endif.

otp_5805_mg_event_sequence(text, tcp) ->
    DecodeFun = ?otp_5805_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?otp_5805_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"mg"},
    ServiceChangeReq = otp_5805_mg_service_change_request_msg(Mid, 1, 0),
    NotifyReqNNV = otp_5805_mg_notify_request_msg("1"),
    NotifyReqUV = otp_5805_mg_notify_request_msg("4"),
    ServiceChangeReplyVerifyFun = 
	?otp_5805_mg_verify_service_change_rep_msg_fun(),
    EDVerify = 
	?otp_5805_mg_verify_error_descriptor_msg_fun(),
    MgEvSeq = [{debug,  true},
	       {decode, DecodeFun},
	       {encode, EncodeFun},
	       {connect, 2944},

	       {send, "service-change-request", ServiceChangeReq}, 
	       {expect_receive, "service-change-reply", {ServiceChangeReplyVerifyFun, 5000}}, 
	       {sleep, 1000},
	       {send, "notify request (not negotiated version)", NotifyReqNNV}, 
	       {expect_receive, "error-descriptor", EDVerify}, 
	       {sleep, 1000},
 	       {send, "notify request (unsupported version)", NotifyReqUV}, 
	       {expect_receive, "error-descriptor", EDVerify}, 

	       {expect_nothing, 4000},
	       disconnect
	      ],
    MgEvSeq.

-ifndef(megaco_hipe_special).
otp_5805_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
otp_5805_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
otp_5805_mg_verify_service_change_rep_msg_fun() ->
    fun(M) ->
	    otp_5805_mg_verify_service_change_rep_msg(M)
    end.
-endif.

otp_5805_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) -> 
    case Mess of
	#'Message'{version     = 1,
		   mId         = _MgMid,
		   messageBody = Body} ->
	    case Body of
		{transactions, [Trans]} ->
		    case Trans of
			{transactionReply, TR} ->
			    case TR of
				#'TransactionReply'{transactionId = _Tid,
						    immAckRequired = asn1_NOVALUE,
						    transactionResult = Res} ->
				    case Res of
					{actionReplies, [AR]} ->
					    case AR of
						#'ActionReply'{contextId = _Cid,
							       errorDescriptor = asn1_NOVALUE,
							       contextReply    = _CtxReq,
							       commandReply    = [CR]} ->
						    case CR of
							{serviceChangeReply, SCR} ->
							    case SCR of
								#'ServiceChangeReply'{
								       terminationID       = _TermID,
								       serviceChangeResult = SCRes} ->
								    case SCRes of
									{serviceChangeResParms, SCRP} ->
									    case SCRP of
										#'ServiceChangeResParm'{
											serviceChangeMgcId   = _MgcMid,
											serviceChangeVersion = 2} ->
										    {ok, M};
										_ ->
										    {error, {invalid_scrp, SCRP}}
									    end;
									_ ->
									    {error, {invalid_scres, SCRes}}
								    end;
								_ ->
								    {error, {invalid_scr, SCR}}
							    end;
							_ ->
							    {error, {invalid_cr, CR}}
						    end;
						_ ->
						    {error, {invalid_ar, AR}}
					    end;
					_ ->
					    {error, {invalid_tres, Res}}
				    end;
				_ ->
				    {error, {invalid_tr, TR}}
			    end;
			_ ->
			    {error, {invalid_trans, Trans}}
		    end;
		_ ->
		    {error, {invalid_body, Body}}
	    end;
	_ ->
	    {error, {invalid_mess, Mess}}
    end;
otp_5805_mg_verify_service_change_rep_msg(M) ->
    {error, {invalid_message, M}}.


-ifndef(megaco_hipe_special).
otp_5805_mg_verify_error_descriptor_msg_fun() ->
    fun(M) ->
	    otp_5805_mg_verify_error_descriptor_msg(M)
    end.
-endif.

otp_5805_mg_verify_error_descriptor_msg(#'MegacoMessage'{mess = Mess} = M) -> 
    case Mess of
	#'Message'{version     = 2,
		   mId         = _MgMid,
		   messageBody = Body} ->
	    io:format("otp_5805_mg_verify_error_descriptor_msg_fun -> ok"
		      "~n   Body:  ~p"
		      "~n", [Body]),
	    case Body of
		{messageError, ED} ->
		    case ED of
			#'ErrorDescriptor'{
			      errorCode = ?megaco_version_not_supported} ->
			    {ok, M};
			_ ->
			    {error, {invalid_ed, ED}}
		    end;
		_ ->
		    {error, {invalid_body, Body}}
	    end;
	_ ->
	    {error, {invalid_mess, Mess}}
    end;
otp_5805_mg_verify_error_descriptor_msg(M) ->
    {error, {invalid_message, M}}.

otp_5805_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, 2, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_5805_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp_5805_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

%% otp_5805_mg_notify_request_ar(Rid, Tid, Cid) ->
%%     TT      = cre_timeNotation("19990729", "22000000"),
%%     Ev      = cre_obsEvent("al/of", TT),
%%     EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
%%     NR      = cre_notifyReq([Tid], EvsDesc),
%%     CMD     = cre_command(NR),
%%     CR      = cre_cmdReq(CMD),
%%     cre_actionReq(Cid, [CR]).

otp_5805_mg_notify_request_msg(V) ->
    M = 
"MEGACO/" ++ V ++ " mg
Transaction = 2 {
	Context = 1 {
		Notify = 00000000/00000000/01101101 {
			ObservedEvents = 1 {
				19990729T22000000:al/of
			}
		}
	}
}",
    list_to_binary(M).


%%
%% MGC generator stuff
%% 

-ifdef(megaco_hipe_special).
-define(otp_5805_mgc_verify_handle_connect_fun(), 
        {?MODULE, otp_5805_mgc_verify_handle_connect, []}).
-define(otp_5805_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, otp_5805_mgc_verify_service_change_req, [Mid]}).
-define(otp_5805_mgc_verify_handle_syntax_error_fun(),
        {?MODULE, otp_5805_mgc_verify_handle_syntax_error, []}).
-define(otp_5805_mgc_verify_handle_disconnect_fun(),
        {?MODULE, otp_5805_mgc_verify_handle_disconnect, []}).
-else.
-define(otp_5805_mgc_verify_handle_connect_fun(), 
        fun otp_5805_mgc_verify_handle_connect/1).
-define(otp_5805_mgc_verify_service_change_req_fun(Mid),
        otp_5805_mgc_verify_service_change_req_fun(Mid)).
-define(otp_5805_mgc_verify_handle_syntax_error_fun(),
	fun otp_5805_mgc_verify_handle_syntax_error/1).
-define(otp_5805_mgc_verify_handle_disconnect_fun(),
	fun otp_5805_mgc_verify_handle_disconnect/1).
-endif.

otp_5805_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify          = ?otp_5805_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?otp_5805_mgc_verify_service_change_req_fun(Mid),
    SyntaxErrorVerify1     = ?otp_5805_mgc_verify_handle_syntax_error_fun(), 
    SyntaxErrorVerify2     = ?otp_5805_mgc_verify_handle_syntax_error_fun(),
    DiscoVerify            = ?otp_5805_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
	     {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect, ConnectVerify},
	     {megaco_conn_info, all},
             {megaco_callback, handle_trans_request_sc, ServiceChangeReqVerify},
	     {megaco_update_conn_info, protocol_version, 2}, 
             {megaco_callback, handle_syntax_error, SyntaxErrorVerify1},
             {megaco_callback, handle_syntax_error, SyntaxErrorVerify2},
             {megaco_callback, handle_disconnect, DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

otp_5805_mgc_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("otp_5805_mgc_verify_handle_connect -> ok"
	      "~n   CH:  ~p"
	      "~n", [CH]),
    {ok, CH, ok};
otp_5805_mgc_verify_handle_connect({handle_connect, CH, V}) -> 
    io:format("otp_5805_mgc_verify_handle_connect -> unexpected version"
	      "~n   CH:  ~p"
	      "~n   V:   ~p"
	      "~n", [CH, V]),
    {error, {unexpected_version, V}, ok};
otp_5805_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
otp_5805_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    otp_5805_mgc_verify_service_change_req(Ev, Mid)
    end.
-endif.

otp_5805_mgc_verify_service_change_req({handle_trans_request, _, V, [AR]}, 
				       Mid) ->
    io:format("otp_5805_mgc_verify_service_change_req -> ok so far"
	      "~n   V:   ~p"
	      "~n", [V]),
    case AR of
	#'ActionRequest'{commandRequests = [CR]} ->
	    case CR of
		#'CommandRequest'{command = Cmd} ->
		    case Cmd of
			{serviceChangeReq, 
			 #'ServiceChangeRequest'{terminationID = [Tid],
						 serviceChangeParms = Parms}} ->
			    case Tid of
				#megaco_term_id{contains_wildcards = false, 
						id = ["root"]} ->
				    case Parms of
					#'ServiceChangeParm'{
						 serviceChangeMethod = restart,
						 serviceChangeVersion = 2, 
						 serviceChangeReason = [[$9,$0,$1|_]]} ->
					    Reply = 
						{discard_ack, 
						 [otp_5805_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = otp_5805_err_desc(Parms),
					    ErrReply = 
						{discard_ack, ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = otp_5805_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = otp_5805_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = otp_5805_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = otp_5805_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
otp_5805_mgc_verify_service_change_req(Else, _Mid) ->
    ED       = otp_5805_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp_5805_mgc_verify_handle_syntax_error({handle_syntax_error, CH, _, ED}) 
  when is_record(ED, 'ErrorDescriptor') -> 
    io:format("otp_5805_mgc_verify_handle_syntax_error -> ok so far"
	      "~n   CH: ~p"
	      "~n   ED:  ~p"
	      "~n", [CH, ED]),
    case ED of
	#'ErrorDescriptor'{errorCode = ?megaco_version_not_supported} ->
	    {ok, CH, reply};
	#'ErrorDescriptor'{errorCode = Code} ->
	    {error, {invalid_errorCode, Code}, ok}
    end;
otp_5805_mgc_verify_handle_syntax_error(Else) ->
    io:format("otp_5805_mgc_verify_handle_syntax_error -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp_5805_mgc_verify_handle_disconnect({handle_disconnect, CH, V, _R}) -> 
    io:format("otp_5805_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   V:  ~p"
	      "~n   _R:  ~p"
	      "~n", [CH, V, _R]),
    {ok, CH, ok};
otp_5805_mgc_verify_handle_disconnect(Else) ->
    io:format("otp_5805_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp_5805_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid, 2),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

%% otp_5805_mgc_notify_reply_ar(Cid, TermId) ->
%%     NR    = cre_notifyReply([TermId]),
%%     CR    = cre_cmdReply(NR),
%%     cre_actionReply(Cid, [CR]).


otp_5805_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5881(suite) ->
    [];
otp_5881(Config) when is_list(Config) ->
    ?SKIP("deprecated by OTP-5887"),
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_5881),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("start the MGC simulator (generator)"),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("create the MGC event sequence"),
    MgcEvSeq = otp_5881_mgc_event_sequence(text, tcp),

    d("start the MGC simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    i("[MG] start"),    
    MgMid = {deviceName, "mg"},
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                factor      = 1, 
                                incr        = 0,
                                max_retries = 3},
    %% 5000,  %% Does not matter since we will not use this anyway...
    LongReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                    factor      = 1, 
                                    incr        = 0,
                                    max_retries = 3},
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
                {long_request_timer, LongReqTmr}, 
                {pending_timer,      PendingTmr},
                {reply_timer,        ReplyTmr}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    i("[MG] verify transaction-id: undefined_serial"),    
    otp_5881_verify_trans_id(Mg, undefined_serial),
    
    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("[MG] verify transaction-id: 1"),    
    otp_5881_verify_trans_id(Mg, 1),
    
    d("[MG] send the notify"),
    {ok, Reply} = (catch ?MG_NOTIF_RAR(Mg)),
    {1, {ok, [AR]}} = Reply,
    d("[MG] ActionReply: ~p", [AR]),

    i("[MG] verify transaction-id: 2"),    
    otp_5881_verify_trans_id(Mg, 2),
    
    d("await the generator reply"),
    await_completion([MgcId]),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    i("done", []),
    ok.

otp_5881_verify_trans_id(Mg, Expected) ->
    case ?MG_CONN_INFO(Mg, trans_id) of
	Expected -> 
	    ok;
	ErroneousValue ->
	    throw({unexpected_transaction_id_value, ErroneousValue, Expected})
    end.
    

-ifdef(megaco_hipe_special).
-define(otp_5881_mgc_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(otp_5881_mgc_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(otp_5881_mgc_verify_service_change_req_msg_fun(),
	{?MODULE, otp_5881_mgc_verify_service_change_req_msg, []}).
-define(otp_5881_mgc_verify_notify_req_msg_fun(),
	{?MODULE, otp_5881_mgc_verify_notify_req_msg, []}).
-else.
-define(otp_5881_mgc_decode_msg_fun(Mod, Conf),
	otp_5881_mgc_decode_msg_fun(Mod, Conf)).
-define(otp_5881_mgc_encode_msg_fun(Mod, Conf),
	otp_5881_mgc_encode_msg_fun(Mod, Conf)).
-define(otp_5881_mgc_verify_service_change_req_msg_fun(),
	otp_5881_mgc_verify_service_change_req_msg_fun()).
-define(otp_5881_mgc_verify_notify_req_msg_fun(),
	otp_5881_mgc_verify_notify_req_msg_fun()).
-endif.

otp_5881_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    DecodeFun = ?otp_5881_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?otp_5881_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"ctrl"},
    ServiceChangeReply = otp_5881_service_change_reply_msg(Mid, 1, 0),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReply = otp_5881_notify_reply_msg(Mid, 2, 0, TermId),
    %% Pending = otp_5881_pending_msg(Mid,2),
    ServiceChangeVerifyFun = ?otp_5881_mgc_verify_service_change_req_msg_fun(),
    NotifyReqVerifyFun     = ?otp_5881_mgc_verify_notify_req_msg_fun(),
    MgcEvSeq = [{debug,  true},
		{decode, DecodeFun},
		{encode, EncodeFun},
		{listen, 2944},

                %% ANNOUNCE READY
                {trigger, "announce ready", fun() -> CTRL ! announce_mgc end}, 

		{expect_accept, any},
		{expect_receive, "service-change-request", {ServiceChangeVerifyFun, 10000}}, 
		{send, "service-change-reply", ServiceChangeReply}, 
		{expect_receive, "notify-request", {NotifyReqVerifyFun, 10000}},
  		{send, "notify-reply", NotifyReply},
		{sleep, 2000}
	       ],
    MgcEvSeq.

-ifndef(megaco_hipe_special).
otp_5881_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
otp_5881_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    decode_msg(M, Mod, Conf)
    end.
-endif.

otp_5881_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms,SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
				  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_5881_msg(Mid, TransId, CR, Cid).

otp_5881_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_5881_msg(Mid, TransId, CR, Cid).

otp_5881_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
			 commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
			       transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


%% otp_5881_pending_msg(Mid, TransId) ->
%%     TP   = #'TransactionPending'{transactionId = TransId},
%%     Body = {transactions, [{transactionPending, TP}]},
%%     Mess = #'Message'{version     = 1,
%% 		      mId         = Mid,
%% 		      messageBody = Body},
%%     #'MegacoMessage'{mess = Mess}.


-ifndef(megaco_hipe_special).
otp_5881_mgc_verify_service_change_req_msg_fun() ->
    fun(M) ->
	    otp_5881_mgc_verify_service_change_req_msg(M)
    end.
-endif.

otp_5881_mgc_verify_service_change_req_msg(
  #'MegacoMessage'{mess = Mess} = M) -> 
    #'Message'{version     = _V,
	       mId         = _Mid,
	       messageBody = Body} = Mess,
    {transactions, [Trans]} = Body,
    {transactionRequest, TR} = Trans,
    #'TransactionRequest'{transactionId = _Tid,
			  actions = [AR]} = TR,
    #'ActionRequest'{contextId = _Cid,
		     contextRequest = _CtxReq,
		     contextAttrAuditReq = _CtxAar,
		     commandRequests = [CR]} = AR,
    #'CommandRequest'{command = Cmd,
		      optional = _Opt,
		      wildcardReturn = _WR} = CR,
    {serviceChangeReq, SCR} = Cmd,
    #'ServiceChangeRequest'{terminationID = _TermID,
			    serviceChangeParms = SCP} = SCR,
    #'ServiceChangeParm'{serviceChangeMethod = restart,
			 serviceChangeReason = [[$9,$0,$1|_]]} = SCP,
    {ok, M};
otp_5881_mgc_verify_service_change_req_msg(M) ->
    {error, {invalid_message, M}}.

-ifndef(megaco_hipe_special).
otp_5881_mgc_verify_notify_req_msg_fun() ->
    fun(M) ->
	    otp_5881_mgc_verify_notify_req_msg(M)
    end.
-endif.

otp_5881_mgc_verify_notify_req_msg(#'MegacoMessage'{mess = Mess} = M) -> 
    #'Message'{messageBody = Body} = Mess,
    {transactions, [Trans]} = Body,
    {transactionRequest, TR} = Trans,
    #'TransactionRequest'{actions = [AR]} = TR,
    #'ActionRequest'{commandRequests = [CR1,CR2]} = AR,
    #'CommandRequest'{command = Cmd1} = CR1,
    {notifyReq, NR1} = Cmd1,
    #'NotifyRequest'{observedEventsDescriptor = OED1} = NR1,
    #'ObservedEventsDescriptor'{observedEventLst = [OE1]} = OED1,
    #'ObservedEvent'{eventName = "al/of"} = OE1,
    #'CommandRequest'{command = Cmd2} = CR2,
    {notifyReq, NR2} = Cmd2,
    #'NotifyRequest'{observedEventsDescriptor = OED2} = NR2,
    #'ObservedEventsDescriptor'{observedEventLst = [OE2]} = OED2,
    #'ObservedEvent'{eventName = "al/of"} = OE2,
    {ok, M};
otp_5881_mgc_verify_notify_req_msg(M) ->
    {error, {invalid_message, M}}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5887(suite) ->
    [];
otp_5887(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_5887),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("start the MGC simulator (generator)"),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("create the MGC event sequence"),
    MgcEvSeq = otp_5887_mgc_event_sequence(text, tcp),

    d("start the MGC simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    i("[MG] start"),    
    MgMid = {deviceName, "mg"},
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                factor      = 1, 
                                incr        = 0,
                                max_retries = 3},
    %% 5000,  %% Does not matter since we will not use this anyway...
    LongReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                    factor      = 1, 
                                    incr        = 0,
                                    max_retries = 3},
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
                {long_request_timer, LongReqTmr}, 
                {pending_timer,      PendingTmr},
                {reply_timer,        ReplyTmr}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    i("[MG] conn info: ~n~p", [?MG_CONN_INFO(Mg, all)]), 

    i("[MG] verify conn transaction-id: 1"),    
    otp_5887_verify_conn_trans_id(Mg, 1),

    i("[MG] user info: ~n~p", [?MG_USER_INFO(Mg, all)]), 

    i("[MG] verify user transaction-id: undefined_serial"),    
    otp_5887_verify_user_trans_id(Mg, undefined_serial),
    
    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("[MG] conn info: ~n~p", [?MG_CONN_INFO(Mg, all)]), 

    i("[MG] verify conn transaction-id: 2"),    
    otp_5887_verify_conn_trans_id(Mg, 2),

    i("[MG] user info: ~n~p", [?MG_USER_INFO(Mg, all)]), 

    i("[MG] verify user transaction-id: 1"),    
    otp_5887_verify_user_trans_id(Mg, 1),
    
    d("[MG] send the notify"),
    {ok, Reply} = (catch ?MG_NOTIF_RAR(Mg)),
    {1, {ok, [AR]}} = Reply,
    d("[MG] ActionReply: ~p", [AR]),

    i("[MG] conn info: ~n~p", [?MG_CONN_INFO(Mg, all)]), 

    i("[MG] verify conn transaction-id: 3"),    
    otp_5887_verify_conn_trans_id(Mg, 3),

    i("[MG] user info: ~n~p", [?MG_USER_INFO(Mg, all)]), 

    i("[MG] verify user transaction-id: 2"),    
    otp_5887_verify_user_trans_id(Mg, 2),
    
    d("await the generator reply"),
    await_completion([MgcId]),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    i("done", []),
    ok.


otp_5887_verify_conn_trans_id(Mg, Expected) ->
    F = fun() -> (catch ?MG_CONN_INFO(Mg, trans_id)) end,
    otp_5887_verify_trans_id(F, Expected).

otp_5887_verify_user_trans_id(Mg, Expected) ->
    F = fun() -> (catch ?MG_USER_INFO(Mg, trans_id)) end,
    otp_5887_verify_trans_id(F, Expected).

otp_5887_verify_trans_id(F, Expected) ->
    case F() of
	Expected -> 
	    ok;
	ErroneousValue ->
	    throw({unexpected_transaction_id_value, ErroneousValue, Expected})
    end.
    

-ifdef(megaco_hipe_special).
-define(otp_5887_mgc_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(otp_5887_mgc_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(otp_5887_mgc_verify_service_change_req_msg_fun(),
	{?MODULE, otp_5887_mgc_verify_service_change_req_msg, []}).
-define(otp_5887_mgc_verify_notify_req_msg_fun(),
	{?MODULE, otp_5887_mgc_verify_notify_req_msg, []}).
-else.
-define(otp_5887_mgc_decode_msg_fun(Mod, Conf),
	otp_5887_mgc_decode_msg_fun(Mod, Conf)).
-define(otp_5887_mgc_encode_msg_fun(Mod, Conf),
	otp_5887_mgc_encode_msg_fun(Mod, Conf)).
-define(otp_5887_mgc_verify_service_change_req_msg_fun(),
	otp_5887_mgc_verify_service_change_req_msg_fun()).
-define(otp_5887_mgc_verify_notify_req_msg_fun(),
	otp_5887_mgc_verify_notify_req_msg_fun()).
-endif.

otp_5887_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    DecodeFun = ?otp_5887_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?otp_5887_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"ctrl"},
    ServiceChangeReply = otp_5887_service_change_reply_msg(Mid, 1, 0),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReply = otp_5887_notify_reply_msg(Mid, 2, 0, TermId),
    ServiceChangeVerifyFun = ?otp_5887_mgc_verify_service_change_req_msg_fun(),
    NotifyReqVerifyFun     = ?otp_5887_mgc_verify_notify_req_msg_fun(),
    MgcEvSeq = [{debug,  true},
		{decode, DecodeFun},
		{encode, EncodeFun},
		{listen, 2944},

                %% ANNOUNCE READY
                {trigger, "announce ready", fun() -> CTRL ! announce_mgc end}, 

		{expect_accept, any},
		{expect_receive, "service-change-request", {ServiceChangeVerifyFun, 10000}}, 
		{send, "service-change-reply", ServiceChangeReply}, 
		{expect_receive, "notify-request", {NotifyReqVerifyFun, 10000}},
  		{send, "notify-reply", NotifyReply},
		{sleep, 2000}
	       ],
    MgcEvSeq.

otp_5887_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms,SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
				  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_5887_msg(Mid, TransId, CR, Cid).

otp_5887_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_5887_msg(Mid, TransId, CR, Cid).

otp_5887_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
			 commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
			       transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


%% otp_5887_pending_msg(Mid, TransId) ->
%%     TP   = #'TransactionPending'{transactionId = TransId},
%%     Body = {transactions, [{transactionPending, TP}]},
%%     Mess = #'Message'{version     = 1,
%% 		      mId         = Mid,
%% 		      messageBody = Body},
%%     #'MegacoMessage'{mess = Mess}.


-ifndef(megaco_hipe_special).
otp_5887_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    encode_msg(M, Mod, Conf)
    end.
-endif.
%% otp_5887_mgc_encode_msg_fun(Mod, Conf, Ver) ->
%%     fun(M) -> 
%% 	    encode_msg(M, Mod, Conf, Ver)
%%     end.

-ifndef(megaco_hipe_special).
otp_5887_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    decode_msg(M, Mod, Conf)
    end.
-endif.
%% otp_5887_mgc_decode_msg_fun(Mod, Conf, Ver) ->
%%     fun(M) -> 
%% 	    decode_msg(M, Mod, Conf, Ver)
%%     end.

-ifndef(megaco_hipe_special).
otp_5887_mgc_verify_service_change_req_msg_fun() ->
    fun(M) ->
	    otp_5887_mgc_verify_service_change_req_msg(M)
    end.
-endif.

otp_5887_mgc_verify_service_change_req_msg(
  #'MegacoMessage'{mess = Mess} = M) -> 
    #'Message'{version     = _V,
	       mId         = _Mid,
	       messageBody = Body} = Mess,
    {transactions, [Trans]} = Body,
    {transactionRequest, TR} = Trans,
    #'TransactionRequest'{transactionId = _Tid,
			  actions = [AR]} = TR,
    #'ActionRequest'{contextId = _Cid,
		     contextRequest = _CtxReq,
		     contextAttrAuditReq = _CtxAar,
		     commandRequests = [CR]} = AR,
    #'CommandRequest'{command = Cmd,
		      optional = _Opt,
		      wildcardReturn = _WR} = CR,
    {serviceChangeReq, SCR} = Cmd,
    #'ServiceChangeRequest'{terminationID = _TermID,
			    serviceChangeParms = SCP} = SCR,
    #'ServiceChangeParm'{serviceChangeMethod = restart,
			 serviceChangeReason = [[$9,$0,$1|_]]} = SCP,
    {ok, M};
otp_5887_mgc_verify_service_change_req_msg(M) ->
    {error, {invalid_message, M}}.


-ifndef(megaco_hipe_special).
otp_5887_mgc_verify_notify_req_msg_fun() ->
    fun(M) ->
	    otp_5887_mgc_verify_notify_req_msg(M)
    end.
-endif.
		   
otp_5887_mgc_verify_notify_req_msg(#'MegacoMessage'{mess = Mess} = M) -> 
    #'Message'{messageBody = Body} = Mess,
    {transactions, [Trans]} = Body,
    {transactionRequest, TR} = Trans,
    #'TransactionRequest'{actions = [AR]} = TR,
    #'ActionRequest'{commandRequests = [CR1,CR2]} = AR,
    #'CommandRequest'{command = Cmd1} = CR1,
    {notifyReq, NR1} = Cmd1,
    #'NotifyRequest'{observedEventsDescriptor = OED1} = NR1,
    #'ObservedEventsDescriptor'{observedEventLst = [OE1]} = OED1,
    #'ObservedEvent'{eventName = "al/of"} = OE1,
    #'CommandRequest'{command = Cmd2} = CR2,
    {notifyReq, NR2} = Cmd2,
    #'NotifyRequest'{observedEventsDescriptor = OED2} = NR2,
    #'ObservedEventsDescriptor'{observedEventLst = [OE2]} = OED2,
    #'ObservedEvent'{eventName = "al/of"} = OE2,
    {ok, M};
otp_5887_mgc_verify_notify_req_msg(M) ->
    {error, {invalid_message, M}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6253(suite) ->
    [];
otp_6253(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),

    put(verbosity, debug),
    put(tc, otp_6253),

    d("otp_6253 -> start test case controller",[]),
    ok = megaco_tc_controller:start_link(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),

    ?VERIFY(ok, application:start(megaco)),
    ?VERIFY(ok,	megaco:start_user(MgMid, [{send_mod, ?USER_MOD},
	                                  {request_timer, infinity},
	                                  {reply_timer, infinity}])),

    MgRH = user_info(MgMid, receive_handle),
    {ok, PrelCH} = ?VERIFY({ok, _}, megaco:connect(MgRH, PrelMid, sh, self())),

    connections([PrelCH]),
    ?VERIFY([PrelCH], megaco:user_info(MgMid, connections)),

    SC = service_change_request(),

    %% Instruct the transport module to fail all send_message
    d("otp_6253 -> instruct transport module to fail message send",[]),
    ok = megaco_tc_controller:insert(allow_send_message, {fail, otp_6253}),

    ?VERIFY({1, {error, {send_message_failed, otp_6253}}},
	    megaco:call(PrelCH, [SC], [])),

    sleep(1000),

    %% Instruct the transport module to cancel all send_message
    d("otp_6253 -> instruct transport module to cancel message send",[]),
    ok = megaco_tc_controller:insert(allow_send_message, {cancel, otp_6253}),

    ?VERIFY({1, {error, {send_message_cancelled, otp_6253}}},
	    megaco:call(PrelCH, [SC], [])),

    ?VERIFY(ok, megaco:disconnect(PrelCH, shutdown)),

    ?VERIFY(ok,	megaco:stop_user(MgMid)),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6275(suite) ->
    [];
otp_6275(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_6275),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("start the MGC simulator (generator)"),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("create the MGC event sequence"),
    MgcEvSeq = otp_6275_mgc_event_sequence(text, tcp),

    d("start the MGC simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = otp_6275_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("[MGC] await the generator reply"),
    await_completion([MgcId, MgId]), 

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp_6275_mg_verify_handle_connect_fun(), 
        {?MODULE, otp_6275_mg_verify_handle_connect, []}).
-define(otp_6275_mg_verify_notify_req_fun(),
        {?MODULE, otp_6275_mg_verify_notify_req, []}).
-define(otp_6275_mg_verify_handle_trans_rep_fun(),
        {?MODULE, otp_6275_mg_verify_handle_trans_rep, []}).
-else.
-define(otp_6275_mg_verify_handle_connect_fun(), 
        otp_6275_mg_verify_handle_connect_fun()).
-define(otp_6275_mg_verify_notify_req_fun(),
	otp_6275_mg_verify_notify_req_fun()).
-define(otp_6275_mg_verify_handle_trans_rep_fun(),
	otp_6275_mg_verify_handle_trans_rep_fun()).
-endif.

otp_6275_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    otp_6275_mg_event_sequence2(Mid, RI).

otp_6275_mg_event_sequence2(Mid, RI) ->
    ServiceChangeReq = [otp_6275_mg_service_change_request_ar(Mid, 1)],
    _Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    ConnectVerify    = ?otp_6275_mg_verify_handle_connect_fun(),
    NotifyReqVerify  = ?otp_6275_mg_verify_notify_req_fun(),
    TransReplyVerify = ?otp_6275_mg_verify_handle_trans_rep_fun(), 
    EvSeq = [
             {debug, true},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     %% {megaco_update_user_info, recv_pending_limit, 4},
	     {megaco_update_user_info, request_timer, 3000},
             start_transport,
             {megaco_trace, disable}, %%100},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast, ServiceChangeReq, []},
             {megaco_callback, handle_connect, ConnectVerify},
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_reply, TransReplyVerify},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_6275_mg_verify_handle_connect_fun() ->
    fun(Event) ->
	    (catch otp_6275_mg_verify_handle_connect(Event))
    end.
-endif.

otp_6275_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp_6275_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_6275_mg_verify_handle_connect(Else) ->
    io:format("otp_6275_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
otp_6275_mg_verify_notify_req_fun() ->
    fun(Event) ->
	    (catch otp_6275_mg_verify_notify_req(Event))
    end.
-endif.

otp_6275_mg_verify_notify_req(
  {handle_trans_request, _CH, ?VERSION, [AR]}) ->
    io:format("otp_6275_mg_verify_notify_req -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		ET1 = lists:flatten(
		       io_lib:format("Invalid action request: ~w", [AR])), 
		EC1 = ?megaco_internal_gateway_error, 
		ED1 = #'ErrorDescriptor'{errorCode = EC1,
					 errorText = ET1},
		throw({error, {invalid_ActionRequest, AR}, {discard_ack, ED1}})
	end,
    
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		ET2 = lists:flatten(
			io_lib:format("Invalid command request: ~w", [CR])), 
		EC2 = ?megaco_internal_gateway_error, 
		ED2 = #'ErrorDescriptor'{errorCode = EC2,
					 errorText = ET2},
		throw({error, {invalid_CommandRequest, CR}, {discard_ack, ED2}})
	end,

    case Cmd of
	{notifyReq, NotifyReq} ->
	    ET3 = "Unexpected request",
	    EC3 = ?megaco_transaction_req_received_before_servicechange_reply, 
	    ED3 = #'ErrorDescriptor'{errorCode = EC3,
				     errorText = ET3},
	    throw({ok, {ok, NotifyReq}, {discard_ack, ED3}});
	_ ->
	    ET4 = lists:flatten(
		    io_lib:format("Invalid command: ~w", [Cmd])), 
	    EC4 = ?megaco_internal_gateway_error, 
	    ED4 = #'ErrorDescriptor'{errorCode = EC4,
				     errorText = ET4},
	    throw({error, {invalid_command, Cmd}, {discard_ack, ED4}})
    end;
otp_6275_mg_verify_notify_req({Tag, CH, Version, ARs}) ->
    io:format("otp_6275_mg_verify_notify_req -> ok"
	      "~n   Tag:     ~p"
	      "~n   CH:      ~p"
	      "~n   Version: ~p"
	      "~n   ARs:     ~p"
	      "~n", [Tag, CH, Version, ARs]),
    {error, {invalid_event, {Tag, CH, Version, ARs}}, ok};
otp_6275_mg_verify_notify_req(Else) ->
    io:format("otp_6275_mg_verify_notify_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
otp_6275_mg_verify_handle_trans_rep_fun() ->
    fun(Event) ->
	    (catch otp_6275_mg_verify_handle_trans_rep(Event))
    end.
-endif.

otp_6275_mg_verify_handle_trans_rep(
  {handle_trans_reply, CH, ?VERSION, {error, timeout} = Error, _}) ->
    io:format("otp_6275_mg_verify_trans_rep -> expected error"
	      "~n", []),
    case CH of
	#megaco_conn_handle{remote_mid = preliminary_mid} ->
	    {ok, Error, error};
	_ ->
	    {error, {unexpected_connection, CH}, error}
    end;
otp_6275_mg_verify_handle_trans_rep(
  {handle_trans_reply, _CH, ?VERSION, Error, _}) ->
    io:format("otp_6275_mg_verify_handle_trans_rep -> unexpected error"
	      "~n   Error: ~p"
	      "~n", [Error]),
    {error, Error, error};
otp_6275_mg_verify_handle_trans_rep(Else) ->
    io:format("otp_6275_mg_verify_handle_trans_rep -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, error}.

otp_6275_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%
%% MGC generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp_6275_mgc_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(otp_6275_mgc_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(otp_6275_mgc_verify_service_change_req_msg_fun(),
	{?MODULE, otp_6275_mgc_verify_service_change_req_msg, []}).
-define(otp_6275_mgc_verify_notify_rep_msg_fun(),
	{?MODULE, otp_6275_mgc_verify_notify_rep_msg, []}).
-else.
-define(otp_6275_mgc_decode_msg_fun(Mod, Conf),
	otp_6275_mgc_decode_msg_fun(Mod, Conf)).
-define(otp_6275_mgc_encode_msg_fun(Mod, Conf),
	otp_6275_mgc_encode_msg_fun(Mod, Conf)).
-define(otp_6275_mgc_verify_service_change_req_msg_fun(),
	otp_6275_mgc_verify_service_change_req_msg_fun()).
-define(otp_6275_mgc_verify_notify_rep_msg_fun(),
	otp_6275_mgc_verify_notify_rep_msg_fun()).
-endif.

otp_6275_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    DecodeFun = ?otp_6275_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?otp_6275_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"ctrl"},
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = otp_6275_mgc_notify_request_msg(Mid, 2, 1, TermId, 1),
    SCRVerifyFun         = ?otp_6275_mgc_verify_service_change_req_msg_fun(),
    NotifyReplyVerifyFun = ?otp_6275_mgc_verify_notify_rep_msg_fun(),
    MgcEvSeq = 
	[{debug,  true},
	 {decode, DecodeFun},
	 {encode, EncodeFun},
	 {listen, 2944},
	 
         %% ANNOUNCE READY
         {trigger, "announce ready", fun() -> CTRL ! announce_mgc end}, 

	 {expect_accept, any},
	 {expect_receive, "service-change-request", {SCRVerifyFun, 5000}}, 
	 {sleep, 1000}, %% Do _not_ send SC reply
	 {send, "notify-request", NotifyReq},
	 {expect_receive, "request before sc reply", {NotifyReplyVerifyFun, 5000}},
	 {sleep, 2000}
	],
    MgcEvSeq.

-ifndef(megaco_hipe_special).
otp_6275_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    encode_msg(M, Mod, Conf)
    end.
-endif.

%% otp_6275_mgc_encode_msg_fun(Mod, Conf, Ver) ->
%%     fun(M) -> 
%% 	    encode_msg(M, Mod, Conf, Ver)
%%     end.

-ifndef(megaco_hipe_special).
otp_6275_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    decode_msg(M, Mod, Conf)
    end.
-endif.

%% otp_6275_mgc_decode_msg_fun(Mod, Conf, Ver) ->
%%     fun(M) -> 
%% 	    decode_msg(M, Mod, Conf, Ver)
%%     end.

-ifndef(megaco_hipe_special).
otp_6275_mgc_verify_service_change_req_msg_fun() ->
    fun(M) ->
	    (catch otp_6275_mgc_verify_service_change_req_msg(M))
    end.
-endif.

otp_6275_mgc_verify_service_change_req_msg(
  #'MegacoMessage'{mess = Mess} = M) ->
    io:format("otp_6275_mgc_verify_service_change_req_msg -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of
	    #'Message'{messageBody = MB} ->
		MB;
	    _ ->
		throw({error, {invalid_mess, Mess}})
	end,

    Trans = 
	case Body of
	    {transactions, [Transaction]} ->
		Transaction;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,

    TR = 
	case Trans of
	    {transactionRequest, TransReq} ->
		TransReq;
	    _ ->
		throw({error, {invalid_transaction, Trans}})
	end,

    AR = 
	case TR of
	    #'TransactionRequest'{actions = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,

    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_TransactionRequest, AR}})
	end,

    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		throw({error, {invalid_ActionRequest, CR}})
	end,

    SCR = 
	case Cmd of
	    {serviceChangeReq, ServiceChangeReq} ->
		ServiceChangeReq;
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,

    SCP = 
	case SCR of
	    #'ServiceChangeRequest'{serviceChangeParms = ServiceChangeParms} ->
		ServiceChangeParms;
	    _ ->
		throw({error, {invalid_serviceChangeReq, SCR}})
	end,

    case SCP of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    {ok, M};
       _ ->
	    {error, {invalid_serviceChangeParms, SCP}}
    end;
otp_6275_mgc_verify_service_change_req_msg(Crap) ->
    {error, {invalid_MegacoMessage, Crap}}.

-ifndef(megaco_hipe_special).
otp_6275_mgc_verify_notify_rep_msg_fun() ->
    fun(M) ->
	    (catch otp_6275_mgc_verify_notify_rep_msg(M))
    end.
-endif.

otp_6275_mgc_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("otp_6275_mgc_verify_notify_rep_msg -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of
	    #'Message'{messageBody = MB} ->
		MB;
	    _ ->
		throw({error, {invalid_mess, Mess}})
	end,

    Trans = 
	case Body of
	    {transactions, [Transaction]} ->
		Transaction;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,

    TR = 
	case Trans of
	    {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transaction, Trans}})
	end,

    Res = 
	case TR of
	    #'TransactionReply'{transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,

    ED = 
	case Res of
	    {transactionError, ErrorDesc} ->
		ErrorDesc;
	    _ ->
		throw({error, {invalid_TransactionReply, Res}})
	end,

    case ED of
	#'ErrorDescriptor'{errorCode = ?megaco_transaction_req_received_before_servicechange_reply} ->
	    ok;
	_ ->
	    throw({error, {invalid_transactionError, ED}})
    end,
    {ok, M};
otp_6275_mgc_verify_notify_rep_msg(Crap) ->
    {error, {invalid_MegacoMessage, Crap}}.


otp_6275_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_6275_mgc_notify_request_msg(Mid, TransId, Cid, TermId, Rid) ->
    AR = otp_6275_mgc_notify_request_ar(Rid, TermId, Cid),
    otp_6275_msg(Mid, TransId, AR).


%% --

otp_6275_msg(Mid, TransId, AR) ->
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case can only be run with the stack compiled with
%% the MEGACO_TEST_CODE flag. Therefor there is no point in
%% including this test case in the usual test suite
-ifdef(MEGACO_TEST_CODE).
otp_6276(suite) ->
    [];
otp_6276(doc) ->
    "OTP-6276: Cancel when receiving reply raise condition";
otp_6276(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_6276),
    i("starting"),

    d("create sequence controller"),
    CtrlPid = otp_6276_sequence_controller_start(),
    
    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp_6276_mgc_event_sequence(text, tcp, CtrlPid),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the tcp-simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("send start order to sequence controller"),
    CtrlPid ! {start, self(), Mgc, Mg},

    d("[MG] create the event sequence"),
    MgEvSeq = otp_6276_mg_event_sequence(text, tcp, CtrlPid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the megaco-simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


otp_6276_sequence_controller_start() ->
    Self = self(),
    erlang:spawn(fun() -> otp_6276_sequence_controller(Self) end).
			  
otp_6276_sequence_controller(Parent) ->
    io:format("otp_6276_sequence_controller -> entry with"
	      "~n   Parent: ~p"
	      "~n   self(): ~p"
	      "~n", [Parent, self()]),

    d("start tc controller"),
    put(dbg,true),
    ok = megaco_tc_controller:start_link(),

    
    %% Await MGC announcement
    Mgc = 
	receive
	    {announce_mgc, MgcPid} ->
		MgcPid
	end,
    io:format("otp_6276_sequence_controller -> MGC announced: "
	      "~n   Mgc: ~p"
	      "~n", [Mgc]),

    %% Await MG announcement
    Mg = 
	receive
	    {announce_mg, MgPid} ->
		MgPid
	end,
    io:format("otp_6276_sequence_controller -> MG announced: "
	      "~n   Mg: ~p"
	      "~n", [Mg]),

    %% Await request_received notification
    receive
	{notify_request_received, Mgc} ->
	    io:format("otp_6276_sequence_controller -> "
		      "request received from MGC (~p)"
		      "~n", [Mgc]),
	    ok
    end,

    %% Make sure the cancel operation gets blocked midway:
    megaco_tc_controller:insert(block_on_cancel, {cancel_block, self()}),

    %% Send start cancel order
    io:format("otp_6276_sequence_controller -> "
	      "send cancel start order to MG (~p)"
	      "~n", [Mg]),
    Mg ! {start_cancel, self()},
    receive
	{started_cancel, Mg} ->
	    ok
    end,
    io:format("otp_6276_sequence_controller -> "
	      "cancel started - now await blocked"
	      "~n", []),

    receive
	{cancel_block, Mg} ->
	    ok
    end,
    io:format("otp_6276_sequence_controller -> "
	      "cancel blocked - now instruct MGC to send notify reply"
	      "~n", []),

    %% Make sure the cancel operation gets blocked midway:
    megaco_tc_controller:insert(block_on_reply, {reply_block, self()}),

    %% Send NR-send order
    Mgc ! {notify_reply_send, self()},
    io:format("otp_6276_sequence_controller -> "
	      "NR-send order sent - now await notify reply blocked received"
	      "~n", []),

    ReplyPid = 
	receive
	    {reply_block, Pid, true} ->
		io:format("otp_6276_sequence_controller -> "
			  "notify reply blocked received from ~p (true) - "
			  "now unblock cancel"
			  "~n", [Pid]),
		%% Pid ! {reply_block, self()},
		Pid;
	    {reply_block, Pid, Info} ->
		io:format("otp_6276_sequence_controller -> "
			  "notify reply blocked received from ~p: ~p"
			  "~n", [Pid, Info]),
		Pid ! {reply_block, self()},
		exit({unexpected_reply_block_info, Info})
	end,

    %% Send cancel continue (unblock) order
    Mg ! {cancel_block, self()},
    io:format("otp_6276_sequence_controller -> "
	      "cancel unblocked - now await notify-request cancelled"
	      "~n", []),

    %% Await request cancelled
    receive
	{notify_request_cancelled, Mg} ->
	    ok;
	{notify_request_cancelled, Pid} ->
	    io:format("otp_6276_sequence_controller -> "
		      "notify-request cancelled - from ~p"
		      "~n", [Pid]),
	    ok
    end,
    io:format("otp_6276_sequence_controller -> "
	      "notify-request cancelled - now unblock notify-reply"
	      "~n", []),
    
    %% await notify reply result
    ReplyPid ! {reply_block, self()},
    io:format("otp_6276_sequence_controller -> "
	      "notify-reply unblocked - now await unexpected trans"
	      "~n", []),
    
    %% Await unexpected trans
    receive
	{unexpected_trans, Mg, _Trans} ->
	    ok;
	{unexpected_trans, Pid, _Trans} ->
	    io:format("otp_6276_sequence_controller -> "
		      "unexpected_trans - from ~p"
		      "~n", [Pid]),
	    ok
    end,
    io:format("otp_6276_sequence_controller -> "
	      "unexpected transaction received"
	      "~n", []),

    %% Await unexpected trans
    Mgc ! {done, self()},
    receive
	{done, Mgc} ->
	    ok
    end,
    io:format("otp_6276_sequence_controller -> MGC instructed we are done"
	      "~n", []),

    d("stop tc controller"),
    megaco_tc_controller:stop(),

    io:format("otp_6276_sequence_controller -> done~n", []),

    exit(normal).
			     
	 
%%
%% MGC generator stuff
%% 
otp_6276_mgc_event_sequence(text, tcp, Ctrl) ->
    Mid = {deviceName,"ctrl"},
    EM  = megaco_pretty_text_encoder,
    EC  = [],
    otp_6276_mgc_event_sequence2(Mid, EM, EC, Ctrl).

otp_6276_mgc_event_sequence2(Mid, EM, EC, Ctrl) ->
    CTRL = self(),
    DecodeFun = otp_6276_mgc_decode_msg_fun(EM, EC),
    EncodeFun = otp_6276_mgc_encode_msg_fun(EM, EC),
    AnnounceMe = otp_6276_mgc_announce_fun(Ctrl),
    ServiceChangeReqVerify = 
	otp_6276_mgc_verify_service_change_req_fun(Mid),
    ServiceChangeReply = 
	otp_6276_mgc_service_change_reply_msg(Mid, 1, 0),
    NotifyReqVerify = otp_6276_mgc_verify_notify_request_fun(Ctrl),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    AwaitNrSendOrder = otp_6276_mgc_await_notify_reply_send_order_fun(Ctrl),
    NotifyReply = otp_6276_mgc_notify_reply_msg(Mid, 2, 0, TermId),
    AwaitDoneOrder = otp_6276_mgc_await_done_order_fun(Ctrl),
    EvSeq = 
	[
	 {trigger, AnnounceMe}, 
	 {debug,  true},
	 {decode, DecodeFun},
	 {encode, EncodeFun},
	 {listen, 2944},

         %% ANNOUNCE READY
         {trigger, fun() -> CTRL ! announce_mgc end}, 

	 {expect_accept, any},
	 {expect_receive, "service-change-req", 
	  {ServiceChangeReqVerify, 10000}}, 
	 {send, "service-change-reply", ServiceChangeReply}, 
	 {expect_receive, "notify-request", {NotifyReqVerify, 5000}},
	 
	 {trigger, AwaitNrSendOrder}, 

	 {send, "notify-reply", NotifyReply}, 

	 {trigger, AwaitDoneOrder}, 

	 disconnect
	 ],
    EvSeq.

otp_6276_mgc_announce_fun(Pid) ->
    fun() ->
	    Pid ! {announce_mgc, self()}
    end.
    
otp_6276_mgc_await_notify_reply_send_order_fun(Pid) ->
    fun() ->
	    receive
		{notify_reply_send, Pid} ->
		    Pid ! {notify_reply_send, self()}
	    end
    end.

otp_6276_mgc_await_done_order_fun(Pid) ->
    fun() ->
	    receive
		{done, Pid} ->
		    Pid ! {done, self()}
	    end
    end.

otp_6276_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

otp_6276_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

otp_6276_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms,SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
                                  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_6276_mgc_msg(Mid, TransId, CR, Cid).
     
otp_6276_mgc_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_6276_mgc_msg(Mid, TransId, CR, Cid).

otp_6276_mgc_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
                         commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
                               transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
                      mId         = Mid,
                      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.
     
otp_6276_mgc_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
                      mId         = Mid,
                      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.

otp_6276_mgc_verify_service_change_req_fun(_) ->
    fun(#'MegacoMessage'{mess = Mess} = M) ->
            #'Message'{version     = _V,
                       mId         = _Mid,
                       messageBody = Body} = Mess,
            {transactions, [Trans]} = Body,
            {transactionRequest, TR} = Trans,
            #'TransactionRequest'{transactionId = _Tid,
                                  actions = [AR]} = TR,
            #'ActionRequest'{contextId = _Cid,
                             contextRequest = _CtxReq,
                             contextAttrAuditReq = _CtxAar,
                             commandRequests = [CR]} = AR,
            #'CommandRequest'{command = Cmd,
                              optional = _Opt,
                              wildcardReturn = _WR} = CR,
            {serviceChangeReq, SCR} = Cmd,
            #'ServiceChangeRequest'{terminationID = _TermID,
                                    serviceChangeParms = SCP} = SCR,
            #'ServiceChangeParm'{serviceChangeMethod = restart,
                                 serviceChangeReason = [[$9,$0,$1|_]]} = SCP,
            {ok, M};
       (M) ->
            {error, {invalid_message, M}}
    end.

otp_6276_mgc_verify_notify_request_fun(Pid) ->
    fun(M) ->
	    (catch otp_6276_mgc_verify_notify_request(M, Pid))
    end.

otp_6276_mgc_verify_notify_request(#'MegacoMessage'{mess = Mess} = M, 
				   Pid) ->
    io:format("otp_6276_mgc_verify_notify_request -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of
	    #'Message'{messageBody = MessageBody} ->
		MessageBody;
	    _ ->
		throw({error, {invalid_mess, Mess}})
	end,

    Trans = 
	case Body of
            {transactions, [Transaction]} ->
		Transaction;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,

    TR = 
	case Trans of
            {transactionRequest, TransReq} ->
		TransReq;
	    _ ->
		throw({error, {invalid_transaction, Trans}})
	end,

    AR = 
	case TR of
            #'TransactionRequest'{actions = [Action]} ->
		Action;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,

    io:format("otp_6276_mgc_verify_notify_request -> "
	      "~n   AR: ~p"
	      "~n", [AR]),

    CR = 
	case AR of
            #'ActionRequest'{commandRequests = [CommandReq]} ->
		CommandReq;
	    _ ->
		throw({error, {invalid_TransactionRequest, AR}})
	end,

    Cmd = 
	case CR of
            #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		throw({error, {invalid_ActionRequest, CR}})
	end,

    NR = 
	case Cmd of
            {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_CommandRequest, Cmd}})
	end,

    OED = 
	case NR of
            #'NotifyRequest'{observedEventsDescriptor = ObsEvDesc} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,

    OE = 
	case OED of
            #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    _ ->
		throw({error, {invalid_NotifyRequest, OED}})
	end,

    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
            ok;
	_ ->
	    throw({error, {invalid_ObservedEventsDescriptor, OE}})
    end,
    io:format("otp_6276_mgc_verify_notify_request -> "
	      "send notify_request received to "
	      "~n   Pid:    ~p"
	      "~n   self(): ~p"
	      "~n", [Pid, self()]),
    Pid ! {notify_request_received, self()},
    {ok, M};
otp_6276_mgc_verify_notify_request(M, _Pid) ->
    {error, {invalid_message, M}}.

    
%%
%% MG generator stuff
%% 
otp_6276_mg_event_sequence(text, tcp, Ctrl) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    otp_6276_mg_event_sequence2(Mid, RI, Ctrl).

otp_6276_mg_event_sequence2(Mid, RI, Ctrl) ->
    AnnounceMe = otp_6276_mg_announce_fun(Ctrl),
    ServiceChangeReq = [otp_6276_mg_service_change_request_ar(Mid, 1)],
    ConnectVerify = fun otp_6276_mg_verify_handle_connect/1,
    ServiceChangeReplyVerify = 
	fun otp_6276_mg_verify_service_change_reply/1,
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = [otp_6276_mg_notify_request_ar(1, Tid, 1)],
    AwaitCancelOrder = otp_6276_mg_await_cancel_order_fun(Ctrl),
    TransReplyVerify = otp_6276_mg_verify_trans_reply_fun(Ctrl),
    UnexpTransVerify = otp_6276_mg_verify_unexpected_trans_fun(Ctrl),
    EvSeq = [
	     {trigger, AnnounceMe}, 
             {debug, true},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, recv_pending_limit, 4},
             start_transport,
             {megaco_trace, disable}, %%100},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast, ServiceChangeReq, []},
             {megaco_callback, handle_connect, ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},
             {megaco_cast, NotifyReq, []},

	     {trigger, AwaitCancelOrder},

	     {megaco_cancel, otp_6276},

             {megaco_callback, handle_trans_reply, TransReplyVerify},
             {megaco_callback, handle_unexpected_trans,  UnexpTransVerify},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.

otp_6276_mg_announce_fun(Pid) ->
    fun() ->
	    Pid ! {announce_mg, self()}
    end.
    
otp_6276_mg_await_cancel_order_fun(Pid) ->
    fun() ->
	    io:format("otp_6276_mg_await_cancel_order_fun -> entry with"
		      "~n   Pid:    ~p"
		      "~n   self(): ~p"
		      "~n", [Pid, self()]),
	    receive
		{start_cancel, Pid} ->
		    io:format("otp_6276_mg_await_cancel_order_fun -> "
			      "received cancel start order"
			      "~n   Pid:    ~p"
			      "~n   self(): ~p"
			      "~n", [Pid, self()]),
		    Pid ! {started_cancel, self()}
	    end
    end.

otp_6276_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp_6276_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_6276_mg_verify_handle_connect(Else) ->
    io:format("otp_6276_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp_6276_mg_verify_service_change_reply({handle_trans_reply, _CH, 
					       ?VERSION, 
					       {ok, [AR]}, _}) ->
    io:format("otp_6276_mg_verify_service_change_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionReply'{commandReply = [SCR]} ->
	    case SCR of
		{serviceChangeReply,
		 #'ServiceChangeReply'{terminationID = [Tid],
				       serviceChangeResult = Res}} ->
		    case Tid of
			#megaco_term_id{contains_wildcards = false, 
					id = ["root"]} ->
			    case Res of
				{serviceChangeResParms,
				 #'ServiceChangeResParm'{
				   serviceChangeMgcId = _RemoteMid}} ->
				    {ok, AR, ok};
				{Tag, Val} ->
				    Err = {invalid_service_change_result, 
					   Tag, Val},
				    {error, Err, ok}
			    end;
			_ ->
			    Err = {invalid_termination_id, Tid},
			    {error, Err, ok}
		    end;
		{Tag, Val} ->
		    Err = {invalid_command_reply, Tag, Val},
		    {error, Err, ok}
	    end;
	_ ->
	    Err = {invalid_action_reply, AR},
	    {error, Err, ok}
    end;
otp_6276_mg_verify_service_change_reply(Else) ->
    io:format("otp_6276_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp_6276_mg_verify_trans_reply_fun(Pid) ->
    fun(Event) ->
	    otp_6276_mg_verify_trans_reply(Pid, Event)
    end.

otp_6276_mg_verify_trans_reply(Pid, 
  {handle_trans_reply, _CH, ?VERSION, 
   {error, {user_cancel, otp_6276}} = E, _}) ->
    io:format("otp_6276_mg_verify_trans_reply -> expected error"
	      "~n", []),
    Pid ! {notify_request_cancelled, self()},
    {ok, E, error};
otp_6276_mg_verify_trans_reply(_Pid, 
  {handle_trans_reply, _CH, ?VERSION, 
   {error, Reason} = E, _}) ->
    io:format("otp_6276_mg_verify_trans_reply -> unexpected error"
	      "~n   Reason: ~p"
	      "~n", [Reason]),
    {error, E, error};
otp_6276_mg_verify_trans_reply(_Pid, 
  {handle_trans_reply, _CH, ?VERSION, Result, _}) ->
    io:format("otp_6276_mg_verify_trans_reply -> unexpected result"
	      "~n   Result: ~p"
	      "~n", [Result]),
    {error, Result, error};
otp_6276_mg_verify_trans_reply(_Pid, Else) ->
    io:format("otp_6276_mg_verify_trans_reply -> unknown event"
	      "~n   Else: ~p"
	      "~n", [Else]),
    {error, Else, error}.

otp_6276_mg_verify_unexpected_trans_fun(Pid) ->
    fun(Event) ->
	    otp_6276_mg_verify_unexpected_trans(Pid, Event)
    end.

otp_6276_mg_verify_unexpected_trans(Pid, 
  {handle_unexpected_trans, RH, ?VERSION, Trans}) ->
    io:format("otp_6276_mg_verify_unexpected_trans -> expected event"
	      "~n   RH:    ~p"
	      "~n   Trans: ~p"
	      "~n", [RH, Trans]),
    Pid ! {unexpected_trans, self(), Trans},
    {ok, Trans, error};
otp_6276_mg_verify_unexpected_trans(_Pid, Else) ->
    io:format("otp_6276_mg_verify_unexpected_trans -> unknown event"
	      "~n   Else: ~p"
	      "~n", [Else]),
    {error, Else, error}.

otp_6276_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_6276_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


-else.  % -ifdef(MEGACO_TEST_CODE).

otp_6276(suite) ->
    [];
otp_6276(doc) ->
    "OTP-6276";
otp_6276(Config) when is_list(Config) ->

    ?SKIP("included only if compiled with USE_MEGACO_TEST_CODE=true").

-endif. % -ifdef(MEGACO_TEST_CODE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


otp_6442_resend_request1(suite) ->
    [];
otp_6442_resend_request1(Config) when is_list(Config) ->
    put(verbosity, debug),
    put(sname,     "TEST"),
    put(tc,        otp6442rreq1),
    i("starting"),

    MgNode = make_node_name(mg),
    d("start (MG) node: ~p", [MgNode]),
    ok = megaco_test_lib:start_nodes([MgNode], ?FILE, ?LINE),

    d("[MG] start the simulator "),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgMid = {deviceName,"mg"},
    MgEvSeq = otp_6442_resend_request1_mg_event_sequence(MgMid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    i("await the transport module service change send_message event"),
    Pid = otp_6442_expect(fun otp_6442_rsrq1_verify_scr_msg/1, 5000),

    i("wait some before issuing the service change reply"),
    sleep(500),

    i("send the service change reply"),
    MgcMid = {deviceName,"mgc"},
    ServiceChangeReply = otp_6442_mgc_service_change_reply_msg(MgcMid, 1, 1),
    megaco_test_generic_transport:incomming_message(Pid, ServiceChangeReply),

    i("await the transport module "
      "notify-request send_message event from MG: "
      "ignore"),
    ok = otp_6442_expect(fun otp_6442_rsrq1_verify_first_nr_msg/1, 5000),

    i("await the transport module "
      "notify-request resend_message event from MG: "
      "reply"),
    {TransId2, Cid2, TermId2} = 
	otp_6442_expect(fun otp_6442_rsrq1_verify_second_nr_msg/1, 10000),

    i("wait some before issuing the notify reply"),
    sleep(500),

    i("send the notify reply"),
    NotifyReply = 
	otp_6442_mgc_notify_reply_msg(MgcMid, TransId2, Cid2, TermId2),
    megaco_test_generic_transport:incomming_message(Pid, NotifyReply),

    d("await the generator reply"),
    await_completion([MgId]), 

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


otp_6442_expect(Verify, Timeout) when (Timeout > 0) ->
    T = mtime(),
    receive
	Msg ->
	    case (catch Verify(Msg)) of
		{ok, Result} ->
		    d("verified after ~p msec", [mtime() - T]),
		    Result;
		skip ->
		    otp_6442_expect(Verify, to(Timeout, T));
		{error, Reason} ->
		    exit({verification_failed, Reason})
	    end
    after Timeout ->
	    exit(timeout)
    end;
otp_6442_expect(_, _Timeout) ->
    exit(timeout).

otp_6442_rsrq1_verify_scr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected service change request message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, Pid};
otp_6442_rsrq1_verify_scr_msg(
  {transport_event, {send_message, _SH, BadMsg}, _Pid}) ->
    io:format("otp_6442_rsrq1_verify_scr_msg -> error: "
	      "~n   BadMsg: ~p"
	      "~n", [BadMsg]),
    {error, {invalid_message, BadMsg}};
otp_6442_rsrq1_verify_scr_msg({transport_event, BadEvent, _Pid}) ->
    io:format("otp_6442_rsrq1_verify_scr_msg -> error: "
	      "~n   BadEvent: ~p"
	      "~n", [BadEvent]),
    {error, {invalid_message, BadEvent}};
otp_6442_rsrq1_verify_scr_msg(Msg) ->
    io:format("otp_6442_rsrq1_verify_scr_msg -> error: "
	      "~n   Msg: ~p"
	      "~n", [Msg]),
    {error, {invalid_message, Msg}}.

otp_6442_rsrq1_verify_first_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected first notify request send message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, ok};
otp_6442_rsrq1_verify_first_nr_msg(Msg) ->
    io:format("otp_6442_rsrq1_verify_nr_msg -> error: "
	      "~n   Msg: ~p"
	      "~n", [Msg]),
    {error, {invalid_message, Msg}}.

otp_6442_rsrq1_verify_second_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    io:format("otp_6442_rsrq1_verify_second_nr_msg -> "
	      "entry when received expected message with"
	      "~n   Msg: ~p"
	      "~n", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    #'MegacoMessage'{mess = Mess} = Msg,
    #'Message'{mId         = _Mid,
	       messageBody = Body} = Mess, 
    {transactions, Transactions} = Body,
    [Transaction] = Transactions,
    {transactionRequest, TransReq} = Transaction,
    #'TransactionRequest'{transactionId = TransId,
			  actions       = Actions} = TransReq,
    [Action] = Actions,
    #'ActionRequest'{contextId       = Cid,
		     commandRequests = CmdReqs} = Action,
    [CmdReq] = CmdReqs,
    #'CommandRequest'{command = Cmd} = CmdReq,
    {notifyReq, NR} = Cmd,
    #'NotifyRequest'{terminationID = [TermId]} = NR,
    {ok, {TransId, Cid, TermId}};
otp_6442_rsrq1_verify_second_nr_msg(Msg) ->
    io:format("otp_6442_rsrq1_verify_second_nr_msg -> entry when error with"
	      "~n   Msg: ~p"
	      "~n", [Msg]),
    {error, {invalid_message, Msg}}.


otp_6442_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms, SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
                                  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_6442_mgc_reply_msg(Mid, TransId, CR, Cid).

otp_6442_mgc_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_6442_mgc_reply_msg(Mid, TransId, CR, Cid).

otp_6442_mgc_reply_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
                         commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
                               transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
                      mId         = Mid,
                      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(otp_6442_resend_request1_mg_verify_handle_connect_fun(),
	{?MODULE, otp_6442_resend_request1_mg_verify_handle_connect, []}).
-define(otp_6442_resend_request1_mg_verify_service_change_rep_fun(),
	{?MODULE, otp_6442_resend_request1_mg_verify_service_change_rep, []}).
-define(otp_6442_resend_request1_mg_verify_notify_rep_fun(),
	{?MODULE, otp_6442_resend_request1_mg_verify_notify_rep, []}).
-else.
-define(otp_6442_resend_request1_mg_verify_handle_connect_fun(),
	otp_6442_resend_request1_mg_verify_handle_connect_fun()).
-define(otp_6442_resend_request1_mg_verify_service_change_rep_fun(),
	otp_6442_resend_request1_mg_verify_service_change_rep_fun()).
-define(otp_6442_resend_request1_mg_verify_notify_rep_fun(),
	otp_6442_resend_request1_mg_verify_notify_rep_fun()).
-endif.

otp_6442_resend_request1_mg_event_sequence(Mid) ->
    RI = [
          {port,             self()}, % This is just a trick to get my pid to the transport module
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_test_generic_transport}
         ],
    ServiceChangeReq = 
	otp_6442_resend_request1_mg_service_change_request_ar(Mid, 1),
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = otp_6442_resend_request1_mg_notify_request_ar(1, Tid, 1),
    ConnectVerify = 
	?otp_6442_resend_request1_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = 
	?otp_6442_resend_request1_mg_verify_service_change_rep_fun(),
    NotifyReplyVerify = 
	?otp_6442_resend_request1_mg_verify_notify_rep_fun(),
    %%     ConnectVerify = 
    %% 	otp_6442_resend_request1_mg_verify_handle_connect_fun(),
    %%     ServiceChangeReplyVerify = 
    %% 	otp_6442_resend_request1_mg_verify_service_change_reply_fun(),
    %%     NotifyReplyVerify = otp_6442_resend_request1_mg_verify_notify_reply_fun(),
    EvSeq = [
             {debug, false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, resend_indication, false},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_6442_resend_request1_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    otp_6442_resend_request1_mg_verify_handle_connect(Ev) 
    end.
-endif.

otp_6442_resend_request1_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp_6442_resend_request1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_6442_resend_request1_mg_verify_handle_connect(Else) ->
    io:format("otp_6442_resend_request1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
otp_6442_resend_request1_mg_verify_service_change_rep_fun() ->
    fun(Rep) -> 
	    otp_6442_resend_request1_mg_verify_service_change_rep(Rep) 
    end.
-endif.

otp_6442_resend_request1_mg_verify_service_change_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch otp_6442_resend_request1_mg_do_verify_service_change_rep(AR));
otp_6442_resend_request1_mg_verify_service_change_rep(Crap) ->
    {error, Crap, ok}.

otp_6442_resend_request1_mg_do_verify_service_change_rep(AR) ->
    io:format("otp_6442_resend_request1_mg_verify_service_change_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
otp_6442_resend_request1_mg_verify_notify_rep_fun() ->
    fun(Rep) -> 
	    otp_6442_resend_request1_mg_verify_notify_rep(Rep) 
    end.
-endif.

otp_6442_resend_request1_mg_verify_notify_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    io:format("otp_6442_resend_request1_mg_verify_notify_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp_6442_resend_request1_mg_verify_notify_rep(Else) ->
    io:format("otp_6442_resend_request1_mg_verify_notify_rep -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp_6442_resend_request1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_6442_resend_request1_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6442_resend_request2(suite) ->
    [];
otp_6442_resend_request2(Config) when is_list(Config) ->
    put(verbosity, debug),
    put(sname,     "TEST"),
    put(tc,        otp6442rreq2),
    i("starting"),

    MgNode = make_node_name(mg),
    d("start (MG) node: ~p", [MgNode]),
    ok = megaco_test_lib:start_nodes([MgNode], ?FILE, ?LINE),

    d("[MG] start the simulator "),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    Mid = {deviceName,"mg"},
    MgcMid = {deviceName,"mgc"},
    MgEvSeq = otp_6442_resend_request2_mg_event_sequence(Mid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    i("await the transport module service change send_message event"),
    Pid = otp_6442_expect(fun otp_6442_rsrq2_verify_scr_msg/1, 5000),

    i("wait some before issuing the service change reply"),
    sleep(500),

    i("send the service change reply"),
    ServiceChangeReply = otp_6442_mgc_service_change_reply_msg(MgcMid, 1, 1),
    megaco_test_generic_transport:incomming_message(Pid, ServiceChangeReply),

    i("await the transport module notify-request send_message event from MG: ignore"),
    ok = otp_6442_expect(fun otp_6442_rsrq2_verify_first_nr_msg/1, 5000),

    i("await the transport module notify-request resend_message event from MG: reply"),
    {TransId2, Cid2, TermId2} = 
	otp_6442_expect(fun otp_6442_rsrq2_verify_second_nr_msg/1, 10000),

    i("wait some before issuing the notify reply"),
    sleep(500),

    i("send the notify reply"),
    NotifyReply = otp_6442_mgc_notify_reply_msg(MgcMid, TransId2, Cid2, TermId2),
    megaco_test_generic_transport:incomming_message(Pid, NotifyReply),

    d("await the generator reply"),
    await_completion([MgId]), 

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


otp_6442_rsrq2_verify_scr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected service change request message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, Pid};
otp_6442_rsrq2_verify_scr_msg(Msg) ->
    io:format("otp_6442_rsrq2_verify_nr_msg -> error: "
	      "~n   Msg: ~p"
	      "~n", [Msg]),
    {error, {invalid_message, Msg}}.

otp_6442_rsrq2_verify_first_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected first notify request message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, ok};
otp_6442_rsrq2_verify_first_nr_msg(Msg) ->
    {error, {invalid_message, Msg}}.

otp_6442_rsrq2_verify_second_nr_msg(
  {transport_event, {resend_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected second notify request message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    #'MegacoMessage'{mess = Mess} = Msg,
    #'Message'{mId         = _Mid,
	       messageBody = Body} = Mess, 
    {transactions, Transactions} = Body,
    [Transaction] = Transactions,
    {transactionRequest, TransReq} = Transaction,
    #'TransactionRequest'{transactionId = TransId,
			  actions       = Actions} = TransReq,
    [Action] = Actions,
    #'ActionRequest'{contextId       = Cid,
		     commandRequests = CmdReqs} = Action,
    [CmdReq] = CmdReqs,
    #'CommandRequest'{command = Cmd} = CmdReq,
    {notifyReq, NR} = Cmd,
    #'NotifyRequest'{terminationID = [TermId]} = NR,
    {ok, {TransId, Cid, TermId}};
otp_6442_rsrq2_verify_second_nr_msg(Msg) ->
    {error, {invalid_message, Msg}}.


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(otp_6442_resend_request2_mg_verify_handle_connect_fun(),
	{?MODULE, otp_6442_resend_request2_mg_verify_handle_connect, []}).
-define(otp_6442_resend_request2_mg_verify_service_change_rep_fun(),
	{?MODULE, otp_6442_resend_request2_mg_verify_service_change_rep, []}).
-define(otp_6442_resend_request2_mg_verify_notify_rep_fun(),
	{?MODULE, otp_6442_resend_request2_mg_verify_notify_rep, []}).
-else.
-define(otp_6442_resend_request2_mg_verify_handle_connect_fun(),
	otp_6442_resend_request2_mg_verify_handle_connect_fun()).
-define(otp_6442_resend_request2_mg_verify_service_change_rep_fun(),
	otp_6442_resend_request2_mg_verify_service_change_rep_fun()).
-define(otp_6442_resend_request2_mg_verify_notify_rep_fun(),
	otp_6442_resend_request2_mg_verify_notify_rep_fun()).
-endif.

otp_6442_resend_request2_mg_event_sequence(Mid) ->
    RI = [
          {port,             self()}, % This is just a trick to get my pid to the transport module
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_test_generic_transport}
         ],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = otp_6442_resend_request2_mg_notify_request_ar(1, Tid, 1),
    ServiceChangeReq = 
	otp_6442_resend_request2_mg_service_change_request_ar(Mid, 1),
    ConnectVerify = 
	?otp_6442_resend_request2_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = 
	?otp_6442_resend_request2_mg_verify_service_change_rep_fun(),
    NotifyReplyVerify = 
	?otp_6442_resend_request2_mg_verify_notify_rep_fun(),
%%     ConnectVerify = 
%% 	otp_6442_resend_request2_mg_verify_handle_connect_fun(),
%%     ServiceChangeReplyVerify = 
%% 	otp_6442_resend_request2_mg_verify_service_change_reply_fun(),
%%     NotifyReplyVerify = otp_6442_resend_request2_mg_verify_notify_reply_fun(),
    EvSeq = [
             {debug, false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, resend_indication, true},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_6442_resend_request2_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    otp_6442_resend_request2_mg_verify_handle_connect(Ev) 
    end.
-endif.

otp_6442_resend_request2_mg_verify_handle_connect(
  {handle_connect, CH, ?VERSION}) -> 
    io:format("otp_6442_resend_request2_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_6442_resend_request2_mg_verify_handle_connect(Else) ->
    io:format("otp_6442_resend_request2_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
otp_6442_resend_request2_mg_verify_service_change_rep_fun() ->
    fun(Rep) -> 
	    otp_6442_resend_request2_mg_verify_service_change_rep(Rep) 
    end.
-endif.

otp_6442_resend_request2_mg_verify_service_change_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch otp_6442_resend_request2_mg_do_verify_service_change_rep(AR));
otp_6442_resend_request2_mg_verify_service_change_rep(Crap) ->
    {error, Crap, ok}.

otp_6442_resend_request2_mg_do_verify_service_change_rep(AR) ->
    io:format("otp_6442_resend_request2_mg_verify_service_change_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
otp_6442_resend_request2_mg_verify_notify_rep_fun() ->
    fun(Rep) -> 
	    otp_6442_resend_request2_mg_verify_notify_rep(Rep) 
    end.
-endif.

otp_6442_resend_request2_mg_verify_notify_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    io:format("otp_6442_resend_request2_mg_verify_notify_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp_6442_resend_request2_mg_verify_notify_rep(Else) ->
    io:format("otp_6442_resend_request2_mg_verify_notify_rep -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp_6442_resend_request2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_6442_resend_request2_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6442_resend_reply1(suite) ->
    [];
otp_6442_resend_reply1(Config) when is_list(Config) ->
    put(sname,     "TEST"),
    put(verbosity, debug),
    put(tc,        otp6442rrep1),
    i("starting"),

    MgNode = make_node_name(mg),
    d("start (MG) node: ~p", [MgNode]),
    ok = megaco_test_lib:start_nodes([MgNode], ?FILE, ?LINE),

    d("[MG] start the simulator "),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    Mid     = {deviceName,"mg"},
    MgcMid  = {deviceName,"mgc"},
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    MgEvSeq = otp_6442_resend_reply1_mg_event_sequence(Mid, TermId),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    i("await the transport module service change send_message event"),
    Pid = otp_6442_expect(fun otp_6442_rsrp1_verify_scr_msg/1, 5000),

    i("wait some before issuing the service change reply"),
    sleep(500),

    i("simulate MGC sending the service change reply"),
    ServiceChangeReply = otp_6442_mgc_service_change_reply_msg(MgcMid, 1, 1),
    megaco_test_generic_transport:incomming_message(Pid, ServiceChangeReply),


    i("wait some before issuing the notify request"),
    sleep(500),

    i("simulate MGC sending the notify request"),
    NotifyRequest = otp_6442_mgc_notify_request_msg(MgcMid, TermId, 2, 1),
    megaco_test_generic_transport:incomming_message(Pid, NotifyRequest),

    i("await the transport module first notify-reply send_message event from MG: "
      "ignore"),
    otp_6442_expect(fun otp_6442_rsrp1_verify_first_nr_msg/1, 5000),

    i("await the transport module second notify-reply send_message event from MG: "
      "ack"),
    {TransId, _, _} = otp_6442_expect(fun otp_6442_rsrp1_verify_second_nr_msg/1, 10000),

    i("wait some before issuing the ack"),
    sleep(500),

    i("simulate MGC sending the ack"),
    Ack = otp_6442_mgc_ack_msg(MgcMid, TransId),
    megaco_test_generic_transport:incomming_message(Pid, Ack),


    d("await the generator reply"),
    await_completion([MgId]),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


otp_6442_rsrp1_verify_scr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected service change request message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, Pid};
otp_6442_rsrp1_verify_scr_msg(Msg) ->
    {error, {invalid_message, Msg}}.

otp_6442_rsrp1_verify_first_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected first notify reply message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, ok};
otp_6442_rsrp1_verify_first_nr_msg(Msg) ->
    {error, {invalid_message, Msg}}.

otp_6442_rsrp1_verify_second_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected second notify reply message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    #'MegacoMessage'{mess = Mess} = Msg,
    #'Message'{mId         = _Mid,
	       messageBody = Body} = Mess, 
    {transactions, Transactions} = Body,
    [Transaction] = Transactions,
    {transactionReply, TransRep} = Transaction,
    #'TransactionReply'{transactionId     = TransId,
			immAckRequired    = 'NULL', 
			transactionResult = TransRes} = TransRep,
    {actionReplies, ActReps} = TransRes,
    [ActRep] = ActReps,
    #'ActionReply'{contextId       = Cid,
		   errorDescriptor = asn1_NOVALUE, 
		   contextReply    = asn1_NOVALUE, 
		   commandReply    = CmdReps} = ActRep,
    [CmdRep] = CmdReps,
    {notifyReply, NR} = CmdRep, 
    #'NotifyReply'{terminationID = TermId} = NR,
    {ok, {TransId, Cid, TermId}};
otp_6442_rsrp1_verify_second_nr_msg(Msg) ->
    {error, {invalid_message, Msg}}.


otp_6442_mgc_notify_request_msg(Mid, TermId, TransId, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(1, [Ev]),
    NR      = cre_notifyReq([TermId], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    AR      = cre_actionReq(Cid, [CR]),
    ARs     = [AR],
    TR      = cre_transReq(TransId, ARs),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


otp_6442_mgc_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).



%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(otp_6442_resend_reply1_mg_verify_handle_connect_fun(),
	{?MODULE, otp_6442_resend_reply1_mg_verify_handle_connect, []}).
-define(otp_6442_resend_reply1_mg_verify_service_change_rep_fun(),
	{?MODULE, otp_6442_resend_reply1_mg_verify_service_change_rep, []}).
-define(otp_6442_resend_reply1_mg_verify_notify_req_fun(TermId),
	{?MODULE, otp_6442_resend_reply1_mg_verify_notify_req, [TermId]}).
-define(otp_6442_resend_reply1_mg_verify_ack_fun(),
	{?MODULE, otp_6442_resend_reply1_mg_verify_ack, []}).
-else.
-define(otp_6442_resend_reply1_mg_verify_handle_connect_fun(),
	otp_6442_resend_reply1_mg_verify_handle_connect_fun()).
-define(otp_6442_resend_reply1_mg_verify_service_change_rep_fun(),
	otp_6442_resend_reply1_mg_verify_service_change_rep_fun()).
-define(otp_6442_resend_reply1_mg_verify_notify_req_fun(TermId),
	otp_6442_resend_reply1_mg_verify_notify_req_fun(TermId)).
-define(otp_6442_resend_reply1_mg_verify_ack_fun(),
	otp_6442_resend_reply1_mg_verify_ack_fun()).
-endif.

otp_6442_resend_reply1_mg_event_sequence(Mid, TermId) ->
    RI = [
          {port,             self()}, % This is just a trick to get my pid to the transport module
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_test_generic_transport}
         ],
    ServiceChangeReq = 
	otp_6442_resend_reply1_mg_service_change_request_ar(Mid, 1),
    RepTmr = #megaco_incr_timer{wait_for    = 2000,
                                factor      = 1,
                                max_retries = 1},
    ConnectVerify = 
	?otp_6442_resend_reply1_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = 
	?otp_6442_resend_reply1_mg_verify_service_change_rep_fun(),
    NotifyReqVerify = 
	?otp_6442_resend_reply1_mg_verify_notify_req_fun(TermId),
    AckVerify = 
	?otp_6442_resend_reply1_mg_verify_ack_fun(), 
%%     ConnectVerify = 
%% 	otp_6442_resend_reply1_mg_verify_handle_connect_fun(),
%%     ServiceChangeReplyVerify = 
%% 	otp_6442_resend_reply1_mg_verify_service_change_reply_fun(),
%%     NotifyReqVerify = 
%% 	otp_6442_resend_reply1_mg_verify_notify_request_fun(TermId),
%%     AckVerify = 
%% 	otp_6442_resend_reply1_mg_verify_ack_fun(), 
    EvSeq = [
             {debug, false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, resend_indication, false},
	     {megaco_update_user_info, reply_timer,       RepTmr},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},

             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify},

             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_6442_resend_reply1_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    otp_6442_resend_reply1_mg_verify_handle_connect(Ev) 
    end.
-endif.

otp_6442_resend_reply1_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp_6442_resend_reply1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_6442_resend_reply1_mg_verify_handle_connect(Else) ->
    io:format("otp_6442_resend_reply1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
otp_6442_resend_reply1_mg_verify_service_change_rep_fun() ->
    fun(Rep) -> 
	    otp_6442_resend_reply1_mg_verify_service_change_rep(Rep) 
    end.
-endif.

otp_6442_resend_reply1_mg_verify_service_change_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch otp_6442_resend_reply1_mg_do_verify_service_change_rep(AR));
otp_6442_resend_reply1_mg_verify_service_change_rep(Crap) ->
    {error, Crap, ok}.

otp_6442_resend_reply1_mg_do_verify_service_change_rep(AR) ->
    io:format("otp_6442_resend_reply1_mg_verify_service_change_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
otp_6442_resend_reply1_mg_verify_notify_req_fun(TermId) ->
    fun(Req) ->
	    otp_6442_resend_reply1_mg_verify_notify_req(Req, TermId)
    end.
-endif.

otp_6442_resend_reply1_mg_verify_notify_req(
  {handle_trans_request, _, ?VERSION, [AR]}, TermId) ->
    io:format("otp_6442_resend_reply1_mg_verify_notify_req -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid,
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = OED,
			     errorDescriptor          = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_ack, otp_6442_resend_reply1},
	    Reply = {HandleAck,
		     [otp_6442_resend_reply1_mg_notify_reply_ar(Cid, TermId)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp_6442_resend_reply1_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp_6442_resend_reply1_mg_verify_notify_req(Else, TermId) ->
    io:format("otp_6442_resend_reply1_mg_verify_notify_request -> unknown"
	      "~n   Else:   ~p"
	      "~n   TermId: ~p"
	      "~n", [Else, TermId]),
    ED       = otp_6442_resend_reply1_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp_6442_resend_reply1_mg_notify_reply_ar(Cid, TermId) ->
    NR = cre_notifyReply([TermId]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

-ifndef(megaco_hipe_special).
otp_6442_resend_reply1_mg_verify_ack_fun() ->
    fun(Ack) ->
	    otp_6442_resend_reply1_mg_verify_ack(Ack)
    end.
-endif.

otp_6442_resend_reply1_mg_verify_ack(
  {handle_trans_ack, CH, ?VERSION, ok, otp_6442_resend_reply1}) ->
    io:format("otp_6442_resend_reply1_verify_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
otp_6442_resend_reply1_mg_verify_ack(Else) ->
    io:format("otp_6442_resend_reply1_verify_ack -> unknown"
              "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp_6442_resend_reply1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_6442_resend_reply1_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6442_resend_reply2(suite) ->
    [];
otp_6442_resend_reply2(Config) when is_list(Config) ->
    put(sname,     "TEST"),
    put(verbosity, debug),
    put(tc,        otp6442rrep2),
    i("starting"),

    MgNode = make_node_name(mg),
    d("start (MG) node: ~p", [MgNode]),
    ok = megaco_test_lib:start_nodes([MgNode], ?FILE, ?LINE),

    d("[MG] start the simulator "),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    Mid     = {deviceName,"mg"},
    MgcMid  = {deviceName,"mgc"},
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    MgEvSeq = otp_6442_resend_reply2_mg_event_sequence(Mid, TermId),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    i("await the transport module service change send_message event"),
    Pid = otp_6442_expect(fun otp_6442_rsrp2_verify_scr_msg/1, 5000),

    i("wait some before issuing the service change reply"),
    sleep(500),

    i("simulate MGC sending the service change reply"),
    ServiceChangeReply = otp_6442_mgc_service_change_reply_msg(MgcMid, 1, 1),
    megaco_test_generic_transport:incomming_message(Pid, ServiceChangeReply),


    i("wait some before issuing the notify request"),
    sleep(500),

    i("simulate MGC sending the notify request"),
    NotifyRequest = otp_6442_mgc_notify_request_msg(MgcMid, TermId, 2, 1),
    megaco_test_generic_transport:incomming_message(Pid, NotifyRequest),

    i("await the transport module notify-reply send_message event from MG: ignore"),
    otp_6442_expect(otp_6442_rsrp2_verify_first_nr_msg_fun(), 5000),

    i("await the transport module notify-reply resend_message event from MG: ack"),
    {TransId, _, _} = 
	otp_6442_expect(otp_6442_rsrp2_verify_second_nr_msg_fun(), 10000),

    i("wait some before issuing the ack"),
    sleep(500),

    i("simulate MGC sending the ack"),
    Ack = otp_6442_mgc_ack_msg(MgcMid, TransId),
    megaco_test_generic_transport:incomming_message(Pid, Ack),


    d("await the generator reply"),
    await_completion([MgId]),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


otp_6442_rsrp2_verify_scr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected service change request message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, Pid};
otp_6442_rsrp2_verify_scr_msg(Msg) ->
    {error, {invalid_message, Msg}}.

otp_6442_rsrp2_verify_first_nr_msg_fun() ->
    fun(E) ->
	    otp_6442_rsrp2_verify_first_nr_msg(E)
    end.

otp_6442_rsrp2_verify_first_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected first notify reply message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, ok};
otp_6442_rsrp2_verify_first_nr_msg(Msg) ->
    {error, {invalid_message, Msg}}.

otp_6442_rsrp2_verify_second_nr_msg_fun() ->
    fun(E) ->
	    otp_6442_rsrp2_verify_second_nr_msg(E)
    end.

otp_6442_rsrp2_verify_second_nr_msg(
  {transport_event, {resend_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected second notify reply message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    #'MegacoMessage'{mess = Mess} = Msg,
    #'Message'{mId         = _Mid,
	       messageBody = Body} = Mess, 
    {transactions, Transactions} = Body,
    [Transaction] = Transactions,
    {transactionReply, TransRep} = Transaction,
    #'TransactionReply'{transactionId     = TransId,
			immAckRequired    = 'NULL', 
			transactionResult = TransRes} = TransRep,
    {actionReplies, ActReps} = TransRes,
    [ActRep] = ActReps,
    #'ActionReply'{contextId       = Cid,
		   errorDescriptor = asn1_NOVALUE, 
		   contextReply    = asn1_NOVALUE, 
		   commandReply    = CmdReps} = ActRep,
    [CmdRep] = CmdReps,
    {notifyReply, NR} = CmdRep, 
    #'NotifyReply'{terminationID = TermId} = NR,
    {ok, {TransId, Cid, TermId}};
otp_6442_rsrp2_verify_second_nr_msg(Msg) ->
    d("received expected bad second notify reply message: "
      "~n   Msg: ~p", [Msg]),
    {error, {invalid_message, Msg}}.



%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(otp_6442_resend_reply2_mg_verify_handle_connect_fun(),
	{?MODULE, otp_6442_resend_reply2_mg_verify_handle_connect, []}).
-define(otp_6442_resend_reply2_mg_verify_service_change_rep_fun(),
	{?MODULE, otp_6442_resend_reply2_mg_verify_service_change_rep, []}).
-define(otp_6442_resend_reply2_mg_verify_notify_req_fun(TermId),
	{?MODULE, otp_6442_resend_reply2_mg_verify_notify_req, [TermId]}).
-define(otp_6442_resend_reply2_mg_verify_ack_fun(),
	{?MODULE, otp_6442_resend_reply2_mg_verify_ack, []}).
-else.
-define(otp_6442_resend_reply2_mg_verify_handle_connect_fun(),
	otp_6442_resend_reply2_mg_verify_handle_connect_fun()).
-define(otp_6442_resend_reply2_mg_verify_service_change_rep_fun(),
	otp_6442_resend_reply2_mg_verify_service_change_rep_fun()).
-define(otp_6442_resend_reply2_mg_verify_notify_req_fun(TermId),
	otp_6442_resend_reply2_mg_verify_notify_req_fun(TermId)).
-define(otp_6442_resend_reply2_mg_verify_ack_fun(),
	otp_6442_resend_reply2_mg_verify_ack_fun()).
-endif.

otp_6442_resend_reply2_mg_event_sequence(Mid, TermId) ->
    RI = [
          {port,             self()}, % This is just a trick to get my pid to the transport module
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_test_generic_transport}
         ],
    ServiceChangeReq = 
	otp_6442_resend_reply2_mg_service_change_request_ar(Mid, 1),
    RepTmr = #megaco_incr_timer{wait_for    = 2000,
                                factor      = 1,
                                max_retries = 1},
    ConnectVerify = 
	?otp_6442_resend_reply2_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = 
	?otp_6442_resend_reply2_mg_verify_service_change_rep_fun(),
    NotifyReqVerify = 
	?otp_6442_resend_reply2_mg_verify_notify_req_fun(TermId),
    AckVerify = 
	?otp_6442_resend_reply2_mg_verify_ack_fun(), 
%%     ConnectVerify = 
%% 	otp_6442_resend_reply2_mg_verify_handle_connect_fun(),
%%     ServiceChangeReplyVerify = 
%% 	otp_6442_resend_reply2_mg_verify_service_change_reply_fun(),
%%     NotifyReqVerify = 
%% 	otp_6442_resend_reply2_mg_verify_notify_request_fun(TermId),
%%     AckVerify = 
%% 	otp_6442_resend_reply2_mg_verify_ack_fun(), 
    EvSeq = [
             {debug, false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, resend_indication, true},
	     {megaco_update_user_info, reply_timer,       RepTmr},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},

             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify},

             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_6442_resend_reply2_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    otp_6442_resend_reply2_mg_verify_handle_connect(Ev) 
    end.
-endif.

otp_6442_resend_reply2_mg_verify_handle_connect(
  {handle_connect, CH, ?VERSION}) -> 
    io:format("otp_6442_resend_reply2_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_6442_resend_reply2_mg_verify_handle_connect(Else) ->
    io:format("otp_6442_resend_reply2_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


-ifndef(megaco_hipe_special).
otp_6442_resend_reply2_mg_verify_service_change_rep_fun() ->
    fun(Rep) -> 
	    otp_6442_resend_reply2_mg_verify_service_change_rep(Rep) 
    end.
-endif.

otp_6442_resend_reply2_mg_verify_service_change_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch otp_6442_resend_reply2_mg_do_verify_service_change_rep(AR));
otp_6442_resend_reply2_mg_verify_service_change_rep(Crap) ->
    {error, Crap, ok}.

otp_6442_resend_reply2_mg_do_verify_service_change_rep(AR) ->
    io:format("otp_6442_resend_reply2_mg_verify_service_change_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
otp_6442_resend_reply2_mg_verify_notify_req_fun(TermId) ->
    fun(Req) ->
	    otp_6442_resend_reply2_mg_verify_notify_req(Req, TermId)
    end.
-endif.

otp_6442_resend_reply2_mg_verify_notify_req(
  {handle_trans_request, _, ?VERSION, [AR]}, TermId) ->
    io:format("otp_6442_resend_reply2_mg_verify_notify_req -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid,
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = OED,
			     errorDescriptor          = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_ack, otp_6442_resend_reply2},
	    Reply = {HandleAck,
		     [otp_6442_resend_reply2_mg_notify_reply_ar(Cid, TermId)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp_6442_resend_reply2_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp_6442_resend_reply2_mg_verify_notify_req(Else, TermId) ->
    io:format("otp_6442_resend_reply2_mg_verify_notify_req -> unknown"
	      "~n   Else:   ~p"
	      "~n   TermId: ~p"
	      "~n", [Else, TermId]),
    ED       = otp_6442_resend_reply2_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp_6442_resend_reply2_mg_notify_reply_ar(Cid, TermId) ->
    NR = cre_notifyReply([TermId]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


-ifndef(megaco_hipe_special).
otp_6442_resend_reply2_mg_verify_ack_fun() ->
    fun(Ack) ->
	    otp_6442_resend_reply2_mg_verify_ack(Ack)
    end.
-endif.

otp_6442_resend_reply2_mg_verify_ack(
  {handle_trans_ack, CH, ?VERSION, ok, otp_6442_resend_reply2}) ->
    io:format("otp_6442_resend_reply2_verify_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
otp_6442_resend_reply2_mg_verify_ack(Else) ->
    io:format("otp_6442_resend_reply2_verify_ack -> unknown"
              "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp_6442_resend_reply2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


otp_6442_resend_reply2_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6865_request_and_reply_plain_extra1(suite) ->
    [];
otp_6865_request_and_reply_plain_extra1(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),

    put(sname,     "TEST"),
    put(verbosity, debug),
    put(tc,        otp6865e1),
    i("starting"),

    d("start test case controller",[]),
    ok = megaco_tc_controller:start_link(),

    %% Instruct the transport module to fail all send_message
    d("instruct transport module to provide extra info: ",[]),
    ExtraInfo = otp_6865_extra_info, 
    ok = megaco_tc_controller:insert(extra_transport_info, ExtraInfo),

    d("start proxy",[]),
    ?USER_MOD:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = ?USER_MOD,
    d("start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],
    d("start (MG) user ~p",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),

    d("start (MGC) user ~p",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    d("get receive info for ~p",[MgMid]),
    MgRH = user_info(MgMid, receive_handle),
    d("get receive info for ~p",[MgcMid]),
    MgcRH = user_info(MgcMid, receive_handle), 
    d("start transport",[]),
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},
    d("(MG) try connect to MGC",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    d("await connect from MG",[]),
    ?USER({connect, PrelMgCH, _V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("(MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("(MGC) send service change reply",[]),
    ?USER({connect, MgcCH, _V, [ExtraInfo]}, ok), % Mgc auto
    Rep = service_change_reply(MgcMid),
    ?USER({request, MgcCH, _V, [[Req], ExtraInfo]}, {discard_ack, [Rep]}),
    ?USER({connect, MgCH, _V, [ExtraInfo]}, ok), % Mg confirm
    ?RECEIVE([{res, _, {1, {ok, [Rep], ExtraInfo}}}]),

    d("get (system info) connections",[]),
    connections([MgCH, MgcCH]),
    d("get (~p) connections",[MgMid]),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    d("get (~p) connections",[MgcMid]),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    Reason = shutdown,
    d("(MG) disconnect",[]),
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    d("(MGC) disconnect",[]),
    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    d("stop megaco app",[]),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),

    d("stop test case controller",[]),
    ok = megaco_tc_controller:stop(),

    d("done",[]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6865_request_and_reply_plain_extra2(suite) ->
    [];
otp_6865_request_and_reply_plain_extra2(doc) ->
    [];
otp_6865_request_and_reply_plain_extra2(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp6865e2),
    i("starting"),

    d("start tc controller"),
    ok = megaco_tc_controller:start_link(),

    %% Instruct the transport module to fail all send_message
    d("instruct transport module to provide extra info: ", []),
    ExtraInfo = otp6865e2_extra_info, 
    ok = megaco_tc_controller:insert(extra_transport_info, ExtraInfo),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),


    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp6865e2_mgc_event_sequence(ExtraInfo, text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = otp6865e2_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId], 60000),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("stop tc controller"),
    ok = megaco_tc_controller:stop(),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp6865e2_mgc_verify_handle_connect_fun(ExtraInfo), 
        {?MODULE, otp6865e2_mgc_verify_handle_connect, [ExtraInfo]}).
-define(otp6865e2_mgc_verify_service_change_req_fun(Mid, ExtraInfo),
        {?MODULE, otp6865e2_mgc_verify_service_change_req, [Mid, ExtraInfo]}).
-define(otp6865e2_mgc_verify_notify_req_fun(Cid, ExtraInfo, RequireAck),
        {?MODULE, otp6865e2_mgc_verify_notify_req, [Cid, ExtraInfo, RequireAck]}).
-define(otp6865e2_mgc_verify_reply_ack_fun(ExtraInfo),
	{?MODULE, otp6865e2_mgc_verify_reply_ack, [ExtraInfo]}).
-define(otp6865e2_mgc_verify_notify_reply_fun(ExtraInfo),
	{?MODULE, otp6865e2_mgc_verify_notify_reply, [ExtraInfo]}).
-define(otp6865e2_mgc_verify_handle_disconnect_fun(),
        {?MODULE, otp6865e2_mgc_verify_handle_disconnect, []}).
-else.
-define(otp6865e2_mgc_verify_handle_connect_fun(ExtraInfo), 
        otp6865e2_mgc_verify_handle_connect(ExtraInfo)).
-define(otp6865e2_mgc_verify_service_change_req_fun(Mid, ExtraInfo),
        otp6865e2_mgc_verify_service_change_req_fun(Mid, ExtraInfo)).
-define(otp6865e2_mgc_verify_notify_req_fun(Cid, ExtraInfo, RequireAck),
	otp6865e2_mgc_verify_notify_req_fun(Cid, ExtraInfo, RequireAck)).
-define(otp6865e2_mgc_verify_reply_ack_fun(ExtraInfo),
	otp6865e2_mgc_verify_reply_ack_fun(ExtraInfo)).
-define(otp6865e2_mgc_verify_notify_reply_fun(ExtraInfo),
	otp6865e2_mgc_verify_notify_reply_fun(ExtraInfo)).
-define(otp6865e2_mgc_verify_handle_disconnect_fun(),
	fun otp6865e2_mgc_verify_handle_disconnect/1).
-endif.

otp6865e2_mgc_event_sequence(ExtraInfo, text, tcp) ->
    Mid  = {deviceName, "ctrl"},
    CTRL = self(),
    RI   = [
            {port,             2944},
            {encoding_module,  megaco_pretty_text_encoder},
            {encoding_config,  []},
            {transport_module, megaco_tcp}
           ],
    ConnectVerify          = 
	?otp6865e2_mgc_verify_handle_connect_fun(ExtraInfo), 
    ServiceChangeReqVerify = 
	?otp6865e2_mgc_verify_service_change_req_fun(Mid, ExtraInfo),
    NotifyReqVerify1       = 
	?otp6865e2_mgc_verify_notify_req_fun(1, ExtraInfo, false),
    NotifyReqVerify2       = 
	?otp6865e2_mgc_verify_notify_req_fun(2, ExtraInfo, true),
    AckVerify              = ?otp6865e2_mgc_verify_reply_ack_fun(ExtraInfo),
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = [otp6865e2_mgc_notify_request_ar(1, Tid, 1)],
    NotifyReplyVerify = ?otp6865e2_mgc_verify_notify_reply_fun(ExtraInfo), 
    DiscoVerify            = 
	?otp6865e2_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify1},
	     {megaco_callback, handle_trans_request, NotifyReqVerify2},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_cast,     NotifyReq, []},
	     {megaco_callback, handle_trans_reply,   NotifyReplyVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp6865e2_mgc_verify_handle_connect(ExtraInfo) ->
    fun(Req) ->
	    otp6865e2_mgc_verify_handle_connect(Req, ExtraInfo)
    end.
-endif.

otp6865e2_mgc_verify_handle_connect({handle_connect, CH, ?VERSION, ExtraInfo},
				    ExtraInfo) -> 
    io:format("otp6865e2_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp6865e2_mgc_verify_handle_connect(Else, ExtraInfo) ->
    io:format("otp6865e2_mgc_verify_handle_connect -> unknown"
	      "~n   Else:      ~p"
	      "~n   ExtraInfo: ~p"
	      "~n", [Else, ExtraInfo]),
    {error, {Else, ExtraInfo}, ok}.

-ifndef(megaco_hipe_special).
otp6865e2_mgc_verify_service_change_req_fun(Mid, ExtraInfo) ->
    fun(Req) -> 
	    otp6865e2_mgc_verify_service_change_req(Req, Mid, ExtraInfo) 
    end.
-endif.

otp6865e2_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR], ExtraInfo}, Mid, ExtraInfo) ->
    (catch otp6865e2_mgc_do_verify_service_change_req(AR, Mid));
otp6865e2_mgc_verify_service_change_req(Crap, _Mid, ExtraInfo) ->
    ED = cre_ErrDesc({Crap, ExtraInfo}),
    ErrReply = {discard_ack, ED},
    {error, {Crap, ExtraInfo}, ErrReply}.

otp6865e2_mgc_do_verify_service_change_req(AR, Mid) ->
    io:format("otp6865e2_mgc_verify_service_change_req -> ok"
	      "~n   AR:  ~p"
	      "~n   Mid: ~p"
	      "~n", [AR, Mid]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
	end,
    Cmd =
        case CR of
            #'CommandRequest'{command = Command} ->
                Command;
            _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    {Tid, Parms} =
        case Cmd of
            {serviceChangeReq,
             #'ServiceChangeRequest'{terminationID = [TermID],
                                     serviceChangeParms = ServChParms}} ->
                {TermID, ServChParms};
            _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    case Tid of
        #megaco_term_id{contains_wildcards = false, id = ["root"]} ->
            ok;
        _ ->
            Err4      = {invalid_termination_id, Tid},
            ED4       = cre_ErrDesc(Tid),
            ErrReply4 = {discard_ack, ED4},
            throw({error, Err4, ErrReply4})
    end,
    case Parms of
        #'ServiceChangeParm'{serviceChangeMethod = restart,
                             serviceChangeReason = [[$9,$0,$1|_]]} ->
            AckData = [otp6865e2_mgc_service_change_reply_ar(Mid, 1)],
            Reply   = {discard_ack, AckData},
            {ok, AR, Reply};
        _ ->
            Err5      = {invalid_SCP, Parms},
            ED5       = cre_ErrDesc(Parms),
            ErrReply5 = {discard_ack, ED5},
            {error, Err5, ErrReply5}
    end.

-ifndef(megaco_hipe_special).
otp6865e2_mgc_verify_notify_req_fun(Cid, ExtraInfo, RequireAck) ->
    fun(Req) -> 
	    otp6865e2_mgc_verify_notify_req(Req, Cid, ExtraInfo, RequireAck) 
    end.
-endif.

otp6865e2_mgc_verify_notify_req(
  {handle_trans_request, _, ?VERSION, [AR], ExtraInfo}, 
  Cid, ExtraInfo, RequireAck) ->
    (catch otp6865e2_mgc_do_verify_notify_req(AR, Cid, RequireAck));
otp6865e2_mgc_verify_notify_req(Crap, _Cid, ExtraInfo, _RequireAck) ->
    ED       = cre_ErrDesc({Crap, ExtraInfo}),
    ErrReply = {discard_ack, ED},
    {error, {Crap, ExtraInfo}, ErrReply}.
    
otp6865e2_mgc_do_verify_notify_req(AR, Cid, RequireAck) ->
    io:format("otp6865e2_mgc_do_verify_notify_req -> entry with"
	      "~n   AR:         ~p"
	      "~n   Cid:        ~p"
	      "~n   RequireAck: ~p"
	      "~n", [AR, Cid, RequireAck]),
    {ContextID, CR} =
	case AR of
	    #'ActionRequest'{contextId       = CtxID, 
			     commandRequests = [CmdReq]} when (CtxID == Cid) ->
		{CtxID, CmdReq};
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
        end,
    Cmd =
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    NR =
        case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    {Tid, OED} =
        case NR of
            #'NotifyRequest'{terminationID            = [TermID],
                             observedEventsDescriptor = ObsEvsDesc,
                             errorDescriptor          = asn1_NOVALUE} ->
                {TermID, ObsEvsDesc};
            _ ->
                Err4      = {invalid_NR, NR},
                ED4       = cre_ErrDesc(NR),
                ErrReply4 = {discard_ack, ED4},
                throw({error, Err4, ErrReply4})
        end,
    OE =
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
            _ ->
                Err5      = {invalid_OED, OED},
                ED5       = cre_ErrDesc(NR),
                ErrReply5 = {discard_ack, ED5},
                throw({error, Err5, ErrReply5})
        end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
            Replies = [otp6865e2_mgc_notify_reply_ar(ContextID, Tid)],
            Reply   = 
		case RequireAck of
		    true ->
			{{handle_ack, otp6865e2}, Replies};
		    false ->
			{discard_ack, Replies}
		end,
            {ok, AR, Reply};
        _ ->
            Err6      = {invalid_OE, OE},
            ED6       = cre_ErrDesc(OE),
            ErrReply6 = {discard_ack, ED6},
            {error, Err6, ErrReply6}
    end.

%% Ack verification
-ifndef(megaco_hipe_special).
otp6865e2_mgc_verify_reply_ack_fun(ExtraInfo) ->
    fun(M) -> 
	    otp6865e2_mgc_verify_reply_ack(M, ExtraInfo) 
    end.
-endif.

otp6865e2_mgc_verify_reply_ack(
  {handle_trans_ack, _, ?VERSION, ok, otp6865e2, ExtraInfo}, ExtraInfo) ->
    io:format("otp6865e2_mgc_verify_reply_ack -> ok~n", []),
    {ok, ok, ok};
otp6865e2_mgc_verify_reply_ack(
  {handle_trans_ack, _, ?VERSION, AS, AD, ExtraInfo1} = Crap, ExtraInfo2) ->
    io:format("otp6865e2_mgc_verify_reply_ack -> incorrect ack-status:"
	      "~n   AS:         ~p"
	      "~n   AD:         ~p"
	      "~n   ExtraInfo1: ~p"
	      "~n   ExtraInfo2: ~p"
	      "~n", [AS, AD, ExtraInfo1, ExtraInfo2]),
    ED       = cre_ErrDesc({invalid_ack_status, 
			    {AS, AD, ExtraInfo1, ExtraInfo2}}),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply};
otp6865e2_mgc_verify_reply_ack(Crap, ExtraInfo) ->
    io:format("otp6865e2_mgc_verify_reply_ack -> invalid ack:"
	      "~n   Crap:      ~p"
	      "~n   ExtraInfo: ~p"
	      "~n", [Crap, ExtraInfo]),
    ED       = cre_ErrDesc({Crap, ExtraInfo}),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.


%% Notify reply verification
-ifndef(megaco_hipe_special).
otp6865e2_mgc_verify_notify_reply_fun(ExtraInfo) ->
    fun(Rep) -> 
	    otp6865e2_mgc_verify_notify_reply(Rep, ExtraInfo) 
    end.
-endif.
	     
otp6865e2_mgc_verify_notify_reply(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _, ExtraInfo}, ExtraInfo) ->
    io:format("otp6865e2_mgc_verify_notify_reply -> ok"
	      "~n   AR:        ~p"
	      "~n   ExtraInfo: ~p"
	      "~n", [AR, ExtraInfo]),
    {ok, AR, ok};
otp6865e2_mgc_verify_notify_reply(Else, ExtraInfo) ->
    io:format("otp6865e2_mgc_verify_notify_reply -> received unknown event"
	      "~n   Else:      ~p"
	      "~n   ExtraInfo: ~p"
	      "~n", [Else, ExtraInfo]),
    {error, {Else, ExtraInfo}, ok}.


%% Disconnect verification
otp6865e2_mgc_verify_handle_disconnect(
  {handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("otp6865e2_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
otp6865e2_mgc_verify_handle_disconnect(Else) ->
    io:format("otp6865e2_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp6865e2_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

otp6865e2_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp6865e2_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(otp6865e2_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(otp6865e2_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(otp6865e2_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, otp6865e2_mg_verify_service_change_rep_msg, []}).
-define(otp6865e2_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId, AckRequired),
	{?MODULE, otp6865e2_mg_verify_notify_rep_msg, [TermId, TransId, ReqId, CtxId, AckRequired]}).
-define(otp6865e2_mg_verify_notify_req_msg_fun(),
	{?MODULE, otp6865e2_mg_verify_notify_req_msg, []}).
-else.
-define(otp6865e2_mg_decode_msg_fun(Mod, Conf),
	otp6865e2_mg_decode_msg_fun(Mod, Conf)).
-define(otp6865e2_mg_encode_msg_fun(Mod, Conf),
	otp6865e2_mg_encode_msg_fun(Mod, Conf)).
-define(otp6865e2_mg_verify_service_change_rep_msg_fun(),
	otp6865e2_mg_verify_service_change_rep_msg_fun()).
-define(otp6865e2_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId, AckRequired),
	otp6865e2_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId, AckRequired)).
-define(otp6865e2_mg_verify_notify_req_msg_fun(),
	otp6865e2_mg_verify_notify_req_msg_fun()).
-endif.

otp6865e2_mg_event_sequence(text, tcp) ->
    DecodeFun = ?otp6865e2_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?otp6865e2_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = otp6865e2_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = ?otp6865e2_mg_verify_service_change_rep_msg_fun(),
    TermId1 = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TermId2 = #megaco_term_id{id = ["00000000","00000000","10010010"]},
    NotifyReq1 = 
	otp6865e2_mg_notify_request_msg(Mid, TermId1, 2, 1, 1),
    NrVerifyFun1 = 
	?otp6865e2_mg_verify_notify_rep_msg_fun(TermId1, 2, 1, 1, false),
    NotifyReq2 = 
	otp6865e2_mg_notify_request_msg(Mid, TermId2, 3, 2, 2),
    NrVerifyFun2 = 
	?otp6865e2_mg_verify_notify_rep_msg_fun(TermId2, 3, 2, 2, true),
    TransAck = otp6865e2_mg_trans_ack_msg(Mid, 3),
    NotifyReqVerifyFun  = ?otp6865e2_mg_verify_notify_req_msg_fun(),
    NotifyReply = otp6865e2_mg_notify_reply_msg(Mid, 1, 0, TermId1),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
	     
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},

	     %% the original setting for reply timer is 2000
             {send, "notify request 1", NotifyReq1},
             {expect_receive, "notify-reply 1", {NrVerifyFun1, 2500}},
	     {sleep, 1000}, 
             {send, "notify request 2", NotifyReq2},
             {expect_receive, "notify-reply 2", {NrVerifyFun2, 2500}},
	     {sleep, 100}, 
             {send, "transacktion-ack", TransAck},
             {expect_receive, "notify-request", {NotifyReqVerifyFun, 2500}},
	     {sleep, 100}, 
             {send, "notify-reply", NotifyReply},

             {expect_nothing, 5000},
             disconnect
            ],
    EvSeq.

-ifndef(megaco_hipe_special).
otp6865e2_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            encode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
otp6865e2_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            decode_msg(M, Mod, Conf)
    end.
-endif.

-ifndef(megaco_hipe_special).
otp6865e2_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> 
	    (catch otp6865e2_mg_verify_service_change_rep_msg(Msg)) 
    end.
-endif.

otp6865e2_mg_verify_service_change_rep_msg(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
otp6865e2_mg_verify_service_change_rep_msg(Crap) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
otp6865e2_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid, 
				       AckRequired) ->
    fun(Msg) -> 
	    (catch otp6865e2_mg_verify_notify_rep_msg(Msg, 
						      TermId, TransId, 
						      Rid, Cid,
						      AckRequired)) 
    end.
-endif.

otp6865e2_mg_verify_notify_rep_msg(#'MegacoMessage'{mess = Mess} = M,
				   TermId, TransId, Rid, Cid, AckRequired) ->
    io:format("otp6865e2_mg_verify_notify_rep_msg -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    io:format("otp6865e2_mg_verify_notify_rep_msg -> "
	      "~n   Body: ~p"
	      "~n", [Body]),
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    io:format("otp6865e2_mg_verify_notify_rep_msg -> "
	      "~n   Trans: ~p"
	      "~n", [Trans]),
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    io:format("otp6865e2_mg_verify_notify_rep_msg -> "
	      "~n   TR: ~p"
	      "~n", [TR]),
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = asn1_NOVALUE,
                                transactionResult = TransRes} when (AckRequired == false) ->
		TransRes;
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} when (AckRequired == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    io:format("otp6865e2_mg_verify_notify_rep_msg -> "
	      "~n   TRes: ~p"
	      "~n", [TRes]),
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    io:format("otp6865e2_mg_verify_notify_rep_msg -> "
	      "~n   AR: ~p"
	      "~n", [AR]),
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
otp6865e2_mg_verify_notify_rep_msg(Crap, _TermId, _TransId, _Rid, _Cid, _AckRequired) ->
    {error, {invalid_message, Crap}}.

-ifndef(megaco_hipe_special).
otp6865e2_mg_verify_notify_req_msg_fun() ->
    fun(M) ->
	    otp6865e2_mg_verify_notify_req_msg(M)
    end.
-endif.

otp6865e2_mg_verify_notify_req_msg(#'MegacoMessage'{mess = Mess} = M) -> 
    io:format("otp6865e2_mg_verify_notify_req_msg -> entry with"
	      "~n   M:       ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   Body: ~p"
	      "~n", [Body]),
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   Trans: ~p"
	      "~n", [Trans]),
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   TR: ~p"
	      "~n", [TR]),
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActReq]} ->
		ActReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   AR: ~p"
	      "~n", [AR]),
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid,
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_actions, AR}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   CR: ~p"
	      "~n", [CR]),
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		throw({error, {invalid_commandRequests, CR}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   Cmd: ~p"
	      "~n", [Cmd]),
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   NR: ~p"
	      "~n", [NR]),
    OED = 
	case NR of
	    #'NotifyRequest'{terminationID            = [_TermId],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		ObsEvsDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   OED: ~p"
	      "~n", [OED]),
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		throw({error, {invalid_observedEventsDescriptor, OED}})
	end,
    io:format("otp6865e2_mg_verify_notify_req_msg -> "
	      "~n   OE: ~p"
	      "~n", [OE]),
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    io:format("otp6865e2_mg_verify_notify_req_msg -> verifyed"
		      "~n", []),
	    {ok, M};
	_ ->
	    throw({error, {invalid_observedEventLst, OE}})
    end;
otp6865e2_mg_verify_notify_req_msg(M) ->
    {error, {invalid_message, M}}.

otp6865e2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp6865e2_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp6865e2_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp6865e2_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp6865e2_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = otp6865e2_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp6865e2_mg_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR), 
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = {actionReplies, [AR]},
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp6865e2_mg_trans_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7189(suite) ->
    [];
otp_7189(doc) ->
    "...";
otp_7189(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_7189),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp_7189_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    %% i("wait some time before starting the MG simulator"),
    %% sleep(1000),

    i("await MGC ready announcement"),
    receive
        announce_mgc ->
            i("received MGC ready announcement"),
            ok
    end,

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_tcp_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = otp_7189_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_tcp_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId], 60000),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_megaco_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_tcp_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp_7189_mgc_verify_handle_connect_fun(), 
        {?MODULE, otp_7189_mgc_verify_handle_connect, []}).
-define(otp_7189_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, otp_7189_mgc_verify_service_change_req, [Mid]}).
-define(otp_7189_mgc_verify_handle_trans_rep_fun(),
	{?MODULE, otp_7189_mgc_verify_handle_trans_rep, []}).
-define(otp_7189_mgc_verify_handle_disconnect_fun(),
        {?MODULE, otp_7189_mgc_verify_handle_disconnect, []}).
-else.
-define(otp_7189_mgc_verify_handle_connect_fun(), 
        otp_7189_mgc_verify_handle_connect_fun()).
-define(otp_7189_mgc_verify_service_change_req_fun(Mid),
        otp_7189_mgc_verify_service_change_req_fun(Mid)).
-define(otp_7189_mgc_verify_handle_trans_rep_fun(),
	otp_7189_mgc_verify_handle_trans_rep_fun()).
-define(otp_7189_mgc_verify_handle_disconnect_fun(),
	fun otp_7189_mgc_verify_handle_disconnect/1).
-endif.

otp_7189_mgc_event_sequence(text, tcp) ->
    CTRL = self(),
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq     = [otp_7189_mgc_notify_req_ar(1, Tid, 1)],
    ConnectVerify = ?otp_7189_mgc_verify_handle_connect_fun(),
    ScrVerify     = ?otp_7189_mgc_verify_service_change_req_fun(Mid),
    TransReplyVerify = ?otp_7189_mgc_verify_handle_trans_rep_fun(),
    PendingCountersVerify1 = 
	fun([{Counter, 1}]) ->
		   io:format("received expected recv pending counter:"
			     "~n   Counter: ~p"
			     "~n", [Counter]),
		ok;
	   (BadCounters) ->
		io:format("ERROR: "
			  "received unexpected number of "
			  "recv pending counters "
			  "(expected one with counter value 1):"
			  "~n   BadCounters: ~p"
			  "~n", [BadCounters]),
		{error, {invalid_pending_counters, BadCounters}}
	end,
    PendingCountersVerify2 = 
	fun([]) ->
		io:format("received expected number of recv pending counters (none)"
			  "~n", []),
		ok;
	   (BadCounters) ->
		io:format("ERROR: "
			  "received unexpected number of "
			  "recv pending counters "
			  "(expected none):"
			  "~n   BadCounters: ~p"
			  "~n", [BadCounters]),
		{error, {invalid_pending_counters, BadCounters}}
	end,
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
	     {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, recv_pending_limit, 10},

	     {megaco_update_user_info, long_request_timer, timer:seconds(10)},
	     {megaco_user_info, all},
             start_transport,
             listen,

             %% ANNOUNCE READY
             {trigger, fun() -> CTRL ! announce_mgc end}, 

             {megaco_callback, handle_connect, ConnectVerify},
	     {megaco_conn_info, all},
             {megaco_callback, handle_trans_request, ScrVerify},
	     {sleep, 500}, 
             {megaco_cast, NotifyReq, []},

	     %% Wait for 5 seconds to make sure we are on track
             {megaco_callback, nocall, timer:seconds(5)},
	     {megaco_system_info, recv_pending_counters, PendingCountersVerify1}, 
	     
	     %% Now wait for the timeout to hit
	     {megaco_callback, handle_trans_reply, TransReplyVerify}, 
	     {megaco_system_info, recv_pending_counters, PendingCountersVerify2}, 

             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_7189_mgc_verify_handle_connect_fun() ->
    fun(M) ->
	    otp_7189_mgc_verify_handle_connect(M)
    end.
-endif.

otp_7189_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    {ok, CH, ok};
otp_7189_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
otp_7189_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) ->
	    otp_7189_mgc_verify_service_change_req(Req, Mid)
    end.
-endif.

otp_7189_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("otp_7189_mgc_verify_service_change_req -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{commandRequests = [CR]} ->
	    case CR of
		#'CommandRequest'{command = Cmd} ->
		    case Cmd of
			{serviceChangeReq, 
			 #'ServiceChangeRequest'{terminationID = [Tid],
						 serviceChangeParms = Parms}} ->
			    case Tid of
				#megaco_term_id{contains_wildcards = false, 
						id = ["root"]} ->
				    case Parms of
					#'ServiceChangeParm'{
						 serviceChangeMethod = restart,
						 serviceChangeReason = [[$9,$0,$1|_]]} ->
					    Reply = 
						{discard_ack, 
						 [otp_7189_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = otp_7189_err_desc(Parms),
					    ErrReply = {discard_ack, ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = otp_7189_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = otp_7189_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = otp_7189_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = otp_7189_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
otp_7189_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("otp_7189_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED       = otp_7189_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp_7189_mgc_notify_req_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_7189_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

-ifndef(megaco_hipe_special).
otp_7189_mgc_verify_handle_trans_rep_fun() ->
    fun(Event) ->
            (catch otp_7189_mgc_verify_handle_trans_rep(Event))
    end.
-endif.

otp_7189_mgc_verify_handle_trans_rep(
  {handle_trans_reply, CH, ?VERSION, {error, timeout} = Error, _}) ->
    io:format("otp_6275_mgc_verify_trans_rep -> expected error"
	      "~n   CH: ~p"
              "~n", [CH]),
    {ok, Error, error};
otp_7189_mgc_verify_handle_trans_rep(
  {handle_trans_reply, _CH, ?VERSION, Error, _}) ->
    io:format("otp_6275_mgc_verify_handle_trans_rep -> unexpected error"
              "~n   Error: ~p"
              "~n", [Error]),
    {error, Error, error};
otp_7189_mgc_verify_handle_trans_rep(Else) ->
    io:format("otp_6275_mg_verify_handle_trans_rep -> unknown"
              "~n   Else: ~p~n", [Else]),
    {error, Else, error}.



%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp_7189_mg_decode_msg_fun(Mod, Conf),
	{?MODULE, decode_msg, [Mod, Conf]}).
-define(otp_7189_mg_encode_msg_fun(Mod, Conf),
	{?MODULE, encode_msg, [Mod, Conf]}).
-define(otp_7189_mg_verify_service_change_rep_msg_fun(),
	{?MODULE, otp_7189_mg_verify_service_change_rep_msg, []}).
-define(otp_7189_mg_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId),
	{?MODULE, otp_7189_mg_verify_notify_req_msg, [TermId, TransId, ReqId, CtxId]}).
-else.
-define(otp_7189_mg_decode_msg_fun(Mod, Conf),
	otp_7189_mg_decode_msg_fun(Mod, Conf)).
-define(otp_7189_mg_encode_msg_fun(Mod, Conf),
	otp_7189_mg_encode_msg_fun(Mod, Conf)).
-define(otp_7189_mg_verify_service_change_rep_msg_fun(),
	otp_7189_mg_verify_service_change_rep_msg_fun()).
-define(otp_7189_mg_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId),
	otp_7189_mg_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId)).
-endif.

otp_7189_mg_event_sequence(text, tcp) ->
    DecodeFun = ?otp_7189_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ?otp_7189_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"mg"},
    ServiceChangeReq = otp_7189_mg_service_change_request_msg(Mid, 1, 0),
    TermId    =
        #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId   = 1,
    ReqId     = 1,
    CtxId     = 1,
    Pending   = otp_7189_mg_trans_pending_msg(Mid, TransId),
    ServiceChangeReplyVerifyFun = 
	?otp_7189_mg_verify_service_change_rep_msg_fun(),
    NotifyReqVerify = ?otp_7189_mg_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId),
    EvSeq = [
	     ?GD_ENABLE(),
	     {decode, DecodeFun},
	     {encode, EncodeFun},
	     {connect, 2944},
	     ?GSND("service-change-request", ServiceChangeReq), 
	     ?GERCV("service-change-reply", ServiceChangeReplyVerifyFun, ?SECS(5)), 
	     ?GERCV("notify request",       NotifyReqVerify,             ?SECS(5)),
	     ?GS(100),
	     ?GSND("pending", Pending), 
	     {expect_closed, timer:seconds(120)},
	     disconnect
	    ],
    EvSeq.

otp_7189_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            encode_msg(M, Mod, Conf)
    end.

otp_7189_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            decode_msg(M, Mod, Conf)
    end.

otp_7189_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_7189_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp_7189_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

-ifndef(megaco_hipe_special).
otp_7189_mg_verify_service_change_rep_msg_fun() ->
    fun(M) ->
	    otp_7189_mg_verify_service_change_rep_msg(M)
    end.
-endif.

otp_7189_mg_verify_service_change_rep_msg(
  #'MegacoMessage'{mess = Mess} = M) -> 
    io:format("otp_7189_mg_verify_service_change_rep_msg -> "
	      "ok so far~n",[]),
    #'Message'{version     = _V,
	       mId         = _MgMid,
	       messageBody = Body} = Mess,
    {transactions, [Trans]} = Body,
    {transactionReply, TR} = Trans,
    #'TransactionReply'{transactionId = _Tid,
			immAckRequired = asn1_NOVALUE,
			transactionResult = Res} = TR,
    {actionReplies, [AR]} = Res,
    #'ActionReply'{contextId = _Cid,
		   errorDescriptor = asn1_NOVALUE,
		   contextReply = _CtxReq,
		   commandReply = [CR]} = AR,
    {serviceChangeReply, SCR} = CR,
    #'ServiceChangeReply'{terminationID = _TermID,
			  serviceChangeResult = SCRes} = SCR,
    {serviceChangeResParms, SCRP} = SCRes,
    #'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} = SCRP,
    {ok, M};
otp_7189_mg_verify_service_change_rep_msg(M) ->
    {error, {invalid_message, M}}.

-ifndef(megaco_hipe_special).
otp_7189_mg_verify_notify_req_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) ->
            (catch otp_7189_mg_verify_notify_req_msg(Msg,
						     TermId,
						     TransId, Rid, Cid))
    end.
-endif.

otp_7189_mg_verify_notify_req_msg(#'MegacoMessage'{mess = Mess} = M,
				  TermId, TransId, Rid, Cid) ->
    io:format("otp_7189_mgc_verify_notify_req_msg -> entry with"
              "~n   M:       ~p"
              "~n   TermId:  ~p"
              "~n   TransId: ~p"
              "~n   Rid:     ~p"
              "~n   Cid:     ~p"
              "~n", [M, TermId, TransId, Rid, Cid]),
    Body =
        case Mess of
            #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
                MsgBody;
            _ ->
                throw({error, {invalid_Message, Mess}})
        end,
    Trans =
        case Body of
            {transactions, [Transactions]} ->
                Transactions;
            _ ->
                throw({error, {invalid_messageBody, Body}})
        end,
    TR =
        case Trans of
            {transactionRequest, TransRequest} ->
                TransRequest;
            _ ->
                throw({error, {invalid_transactions, Trans}})
        end,
    AR =
        case TR of
            #'TransactionRequest'{transactionId = TransId,
                                  actions       = [ActReq]} ->
                ActReq;
            _ ->
                throw({error, {invalid_transactionRequest, TR, TransId}})
        end,
    CR =
        case AR of
            #'ActionRequest'{contextId       = Cid,
                             commandRequests = [CmdReq]} ->
                CmdReq;
            _ ->
                throw({error, {invalid_actions, AR}})
        end,
    Cmd =
        case CR of
            #'CommandRequest'{command = Command} ->
                Command;
            _ ->
                throw({error, {invalid_commandRequests, CR}})
        end,
    NR =
        case Cmd of
            {notifyReq, NotifReq} ->
                NotifReq;
            _ ->
                throw({error, {invalid_command, Cmd}})
        end,
    OED =
        case NR of
            #'NotifyRequest'{terminationID            = [TermId],
                             observedEventsDescriptor = ObsEvsDesc,
                             errorDescriptor          = asn1_NOVALUE} ->
                ObsEvsDesc;
            _ ->
                throw({error, {invalid_notifyReq, NR}})
        end,
    OE =
        case OED of
            #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
                ObsEvLst;
            _ ->
                throw({error, {invalid_observedEventsDescriptor, OED}})
        end,
    case OE of
        #'ObservedEvent'{eventName = "al/of"} ->
            {ok, M};
        _ ->
            throw({error, {invalid_observedEventLst, OE}})
    end;
otp_7189_mg_verify_notify_req_msg(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


otp_7189_mg_trans_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
                      mId         = Mid,
                      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_7189_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


otp_7259(suite) ->
    [];
otp_7259(doc) ->
    ["This is a variant of ticket OTP-6442"];
otp_7259(Config) when is_list(Config) ->
    put(verbosity, debug),
    put(sname,     "TEST"),
    put(tc,        otp7259rr),
    i("starting"),

    MgNode = make_node_name(mg),
    d("start (MG) node: ~p", [MgNode]),
    ok = megaco_test_lib:start_nodes([MgNode], ?FILE, ?LINE),

    d("[MG] start the simulator "),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgMid = {deviceName,"mg"},
    MgEvSeq = otp_7259_mg_event_sequence(MgMid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    i("await the transport module service change send_message event"),
    Pid = otp_7259_expect(fun otp_7259_verify_scr_msg/1, 5000),

    i("wait some before issuing the service change reply"),
    sleep(500),

    i("send the service change reply"),
    MgcMid = {deviceName,"mgc"},
    ServiceChangeReply = otp_7259_mgc_service_change_reply_msg(MgcMid, 1, 1),
    megaco_test_generic_transport:incomming_message(Pid, ServiceChangeReply),

    i("await the transport module "
      "notify-request send_message event from MG: "
      "ignore"),
    ok = otp_7259_expect(fun otp_7259_verify_first_nr_msg/1, 5000),

    i("await the transport module "
      "notify-request resend_message event from MG: "
      "reply"),
    {TransId2, Cid2, TermId2} = 
	otp_7259_expect(fun otp_7259_verify_second_nr_msg/1, 10000),

    i("wait some before issuing the notify reply"),
    sleep(500),

    i("send the notify reply"),
    NotifyReply = 
	otp_7259_mgc_notify_reply_msg(MgcMid, TransId2, Cid2, TermId2),
    megaco_test_generic_transport:incomming_message(Pid, NotifyReply),

    d("[MG] await the generator reply"),
    await_completion([MgId], 7000),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


otp_7259_expect(Verify, Timeout) when (Timeout > 0) ->
    T = mtime(),
    receive
	Msg ->
	    case (catch Verify(Msg)) of
		{ok, Result} ->
		    d("verified after ~p msec", [mtime() - T]),
		    Result;
		skip ->
		    otp_7259_expect(Verify, to(Timeout, T));
		{error, Reason} ->
		    exit({verification_failed, Reason})
	    end
    after Timeout ->
	    exit(timeout)
    end;
otp_7259_expect(_, _Timeout) ->
    exit(timeout).

otp_7259_verify_scr_msg(
  {transport_event, {send_message, _SH, {message, Msg, Resend}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') andalso ((Resend =:= true) orelse (Resend =:= false)) ->
    d("received expected service change request message: "
      "~n   Msg:    ~p"
      "~n   Resend: ~p", [Msg, Resend]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, Pid};
otp_7259_verify_scr_msg(Msg) ->
    {error, {invalid_message, Msg}}.

otp_7259_verify_first_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg, Resend}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') andalso ((Resend =:= true) orelse (Resend =:= false)) ->
    d("received expected first notify request send message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, ok};
otp_7259_verify_first_nr_msg(Msg) ->
    {error, {invalid_message, Msg}}.

otp_7259_verify_second_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg, Resend}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') andalso ((Resend =:= true) orelse (Resend =:= false)) ->
    d("received expected second notify request send message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    #'MegacoMessage'{mess = Mess} = Msg,
    #'Message'{mId         = _Mid,
	       messageBody = Body} = Mess, 
    {transactions, Transactions} = Body,
    [Transaction] = Transactions,
    {transactionRequest, TransReq} = Transaction,
    #'TransactionRequest'{transactionId = TransId,
			  actions       = Actions} = TransReq,
    [Action] = Actions,
    #'ActionRequest'{contextId       = Cid,
		     commandRequests = CmdReqs} = Action,
    [CmdReq] = CmdReqs,
    #'CommandRequest'{command = Cmd} = CmdReq,
    {notifyReq, NR} = Cmd,
    #'NotifyRequest'{terminationID = [TermId]} = NR,
    {ok, {TransId, Cid, TermId}};
otp_7259_verify_second_nr_msg(Msg) ->
    {error, {invalid_message, Msg}}.


otp_7259_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms, SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
                                  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_7259_mgc_reply_msg(Mid, TransId, CR, Cid).

otp_7259_mgc_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_7259_mgc_reply_msg(Mid, TransId, CR, Cid).

otp_7259_mgc_reply_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
                         commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
                               transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
                      mId         = Mid,
                      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(otp_7259_mg_verify_handle_connect_fun(),
	{?MODULE, otp_7259_mg_verify_handle_connect, []}).
-define(otp_7259_mg_verify_service_change_rep_fun(),
	{?MODULE, otp_7259_mg_verify_service_change_rep, []}).
-define(otp_7259_mg_verify_notify_rep_fun(),
	{?MODULE, otp_7259_mg_verify_notify_rep, []}).
-else.
-define(otp_7259_mg_verify_handle_connect_fun(),
	otp_7259_mg_verify_handle_connect_fun()).
-define(otp_7259_mg_verify_service_change_rep_fun(),
	otp_7259_mg_verify_service_change_rep_fun()).
-define(otp_7259_mg_verify_notify_rep_fun(),
	otp_7259_mg_verify_notify_rep_fun()).
-endif.

otp_7259_mg_event_sequence(Mid) ->
    RI = [
          {port,             self()}, % This is just a trick to get my pid to the transport module
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_test_generic_transport}
         ],
    ServiceChangeReq = 
	otp_7259_mg_service_change_request_ar(Mid, 1),
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = otp_7259_mg_notify_request_ar(1, Tid, 1),
    ConnectVerify = 
	?otp_7259_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = 
	?otp_7259_mg_verify_service_change_rep_fun(),
    NotifyReplyVerify = 
	?otp_7259_mg_verify_notify_rep_fun(),
    EvSeq = [
             {debug, false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, resend_indication, flag},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_7259_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    otp_7259_mg_verify_handle_connect(Ev) 
    end.
-endif.

otp_7259_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp_7259_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_7259_mg_verify_handle_connect(Else) ->
    io:format("otp_7259_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
otp_7259_mg_verify_service_change_rep_fun() ->
    fun(Rep) -> 
	    otp_7259_mg_verify_service_change_rep(Rep) 
    end.
-endif.

otp_7259_mg_verify_service_change_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch otp_7259_mg_do_verify_service_change_rep(AR));
otp_7259_mg_verify_service_change_rep(Crap) ->
    {error, Crap, ok}.

otp_7259_mg_do_verify_service_change_rep(AR) ->
    io:format("otp_7259_mg_verify_service_change_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
otp_7259_mg_verify_notify_rep_fun() ->
    fun(Rep) -> 
	    otp_7259_mg_verify_notify_rep(Rep) 
    end.
-endif.

otp_7259_mg_verify_notify_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    io:format("otp_7259_mg_verify_notify_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp_7259_mg_verify_notify_rep(Else) ->
    io:format("otp_7259_mg_verify_notify_rep -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp_7259_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_7259_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7713(suite) ->
    [];
otp_7713(doc) ->
    [];
otp_7713(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),

    put(verbosity, debug),
    put(sname,     "TEST"),
    put(tc,        otp7713),
    i("starting"),

    d("start proxy",[]),
    ?USER_MOD:start_proxy(),

    Extra = otp7713_extra, 
    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = ?USER_MOD,
    d("start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],
    d("start (MG) user ~p",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),

    d("start (MGC) user ~p",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    d("get receive info for ~p",[MgMid]),
    MgRH = user_info(MgMid, receive_handle),
    d("get receive info for ~p",[MgcMid]),
    MgcRH = user_info(MgcMid, receive_handle), 
    d("start transport",[]),
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},
    d("(MG) try connect to MGC",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid, Extra)), % Mg prel
    d("await connect from MG", []),
    ?USER({connect, PrelMgCH, _V, [Extra]}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("(MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("(MGC) send service change reply",[]),
    ?USER({connect, MgcCH, _V, []}, ok), % Mgc auto
    Rep = service_change_reply(MgcMid),
    ?USER({request, MgcCH, _V, [[Req]]}, {discard_ack, [Rep]}),
    ?USER({connect, MgCH, _V, []}, ok), % Mg confirm
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    d("get (system info) connections",[]),
    connections([MgCH, MgcCH]),
    d("get (~p) connections",[MgMid]),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    d("get (~p) connections",[MgcMid]),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    Reason = shutdown,
    d("(MG) disconnect",[]),
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    d("(MGC) disconnect",[]),
    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    d("stop megaco app",[]),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),

    d("done",[]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_8183_request1(suite) ->
    [];
otp_8183_request1(Config) when is_list(Config) ->
    put(verbosity, debug),
    put(sname,     "TEST"),
    put(tc,        otp8183r1),
    i("starting"),

    MgNode = make_node_name(mg),
    d("start (MG) node: ~p", [MgNode]),
    ok = megaco_test_lib:start_nodes([MgNode], ?FILE, ?LINE),

    d("[MG] start the simulator "),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgMid = {deviceName,"mg"},
    MgEvSeq = otp_8183_r1_mg_event_sequence(MgMid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    i("await the transport module service change send_message event"),
    Pid = otp_8183_expect(fun(Ev) -> otp_8183_r1_verify_scr_msg(Ev) end, 5000),

    i("wait some before issuing the service change reply"),
    sleep(500),

    i("send the service change reply"),
    MgcMid = {deviceName,"mgc"},
    ServiceChangeReply = otp_8183_r1_mgc_service_change_reply_msg(MgcMid, 1, 1),
    megaco_test_generic_transport:incomming_message(Pid, ServiceChangeReply),

    i("await the transport module "
      "notify-request send_message event from MG: "
      "ignore"),
    {TransId2, Cid2, TermId2} =
	otp_8183_expect(fun(Ev) -> otp_8183_r1_verify_nr_msg(Ev) end, 5000),

    i("wait some before issuing the notify reply (twice)"),
    sleep(500),

    i("send the notify reply - twice"),
    NotifyReply = 
	otp_8183_r1_mgc_notify_reply_msg(MgcMid, TransId2, Cid2, TermId2),
    megaco_test_generic_transport:incomming_message(Pid, NotifyReply),
    sleep(100), %% This is to "make sure" the events come in the "right" order
    megaco_test_generic_transport:incomming_message(Pid, NotifyReply),

    d("await the generator reply"),
    await_completion([MgId]), 

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


otp_8183_expect(Verify, Timeout) when (Timeout > 0) ->
    T = mtime(),
    receive
	Msg ->
	    case (catch Verify(Msg)) of
		{ok, Result} ->
		    d("verified after ~p msec", [mtime() - T]),
		    Result;
		skip ->
		    otp_8183_expect(Verify, to(Timeout, T));
		{error, Reason} ->
		    exit({verification_failed, Reason})
	    end
    after Timeout ->
	    exit(timeout)
    end;
otp_8183_expect(_, _Timeout) ->
    exit(timeout).

otp_8183_r1_verify_scr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid}) 
  when is_record(Msg, 'MegacoMessage') ->
    d("received expected service change request message: "
      "~n   Msg: ~p", [Msg]),
    Reply = ok, 
    Pid ! {transport_reply, Reply, self()},
    {ok, Pid};
otp_8183_r1_verify_scr_msg(
  {transport_event, {send_message, _SH, BadMsg}, _Pid}) ->
    io:format("otp_8183_r1_verify_scr_msg -> error: "
	      "~n   BadMsg: ~p"
	      "~n", [BadMsg]),
    {error, {invalid_message, BadMsg}};
otp_8183_r1_verify_scr_msg({transport_event, BadEvent, _Pid}) ->
    io:format("otp_8183_r1_verify_scr_msg -> error: "
	      "~n   BadEvent: ~p"
	      "~n", [BadEvent]),
    {error, {invalid_message, BadEvent}};
otp_8183_r1_verify_scr_msg(Msg) ->
    io:format("otp_8183_r1_verify_scr_msg -> error: "
	      "~n   Msg: ~p"
	      "~n", [Msg]),
    {error, {invalid_message, Msg}}.

otp_8183_r1_verify_nr_msg(
  {transport_event, {send_message, _SH, {message, Msg}}, Pid})
  when is_record(Msg, 'MegacoMessage') ->
    io:format("otp_8183_r1_verify_nr_msg -> "
              "entry when received expected message with"
              "~n   Msg: ~p"
              "~n", [Msg]),
    Reply = ok,
    Pid ! {transport_reply, Reply, self()},
    #'MegacoMessage'{mess = Mess} = Msg,
    #'Message'{mId         = _Mid,
               messageBody = Body} = Mess,
    {transactions, Transactions} = Body,
    [Transaction] = Transactions,
    {transactionRequest, TransReq} = Transaction,
    #'TransactionRequest'{transactionId = TransId,
                          actions       = Actions} = TransReq,
    [Action] = Actions,
    #'ActionRequest'{contextId       = Cid,
                     commandRequests = CmdReqs} = Action,
    [CmdReq] = CmdReqs,
    #'CommandRequest'{command = Cmd} = CmdReq,
    {notifyReq, NR} = Cmd,
    #'NotifyRequest'{terminationID = [TermId]} = NR,
    {ok, {TransId, Cid, TermId}};
otp_8183_r1_verify_nr_msg(Msg) ->
    io:format("otp_8183_r1_verify_nr_msg -> entry when error with"
              "~n   Msg: ~p"
              "~n", [Msg]),
    {error, {invalid_message, Msg}}.

otp_8183_r1_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms, SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
                                  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_8183_r1_mgc_reply_msg(Mid, TransId, CR, Cid).

otp_8183_r1_mgc_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_8183_r1_mgc_reply_msg(Mid, TransId, CR, Cid).

otp_8183_r1_mgc_reply_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
                         commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
                               transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
                      mId         = Mid,
                      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


%%
%% MG generator stuff
%%
-ifdef(megaco_hipe_special).
-define(otp_8183_r1_mg_verify_handle_connect_fun(),
	{?MODULE, otp_8183_r1_mg_verify_handle_connect, []}).
-define(otp_8183_r1_mg_verify_service_change_rep_fun(),
	{?MODULE, otp_8183_r1_mg_verify_service_change_rep, []}).
-define(otp_8183_r1_mg_verify_notify_rep_fun(Nr
	{?MODULE, otp_8183_r1_mg_verify_notify_rep, [Nr).
-else.
-define(otp_8183_r1_mg_verify_handle_connect_fun(),
	otp_8183_r1_mg_verify_handle_connect_fun()).
-define(otp_8183_r1_mg_verify_service_change_rep_fun(),
	otp_8183_r1_mg_verify_service_change_rep_fun()).
-define(otp_8183_r1_mg_verify_notify_rep_fun(Nr),
	otp_8183_r1_mg_verify_notify_rep_fun(Nr)).
-endif.

otp_8183_r1_mg_event_sequence(Mid) ->
    RI = [
          {port,             self()}, % This is just a trick to get my pid to the transport module
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_test_generic_transport}
         ],
    ServiceChangeReq = 
	otp_8183_r1_mg_service_change_request_ar(Mid, 1),
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = otp_8183_r1_mg_notify_request_ar(1, Tid, 1),
    ConnectVerify = 
	?otp_8183_r1_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = 
	?otp_8183_r1_mg_verify_service_change_rep_fun(),
    NotifyReplyVerify = 
	fun(Nr) -> 
		?otp_8183_r1_mg_verify_notify_rep_fun(Nr) 
	end, 
    EvSeq = [
             {debug, true}, % false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},

             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        [{request_keep_alive_timeout, 5000}]},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify(1)},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify(2)},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


-ifndef(megaco_hipe_special).
otp_8183_r1_mg_verify_handle_connect_fun() ->
    fun(Ev) -> 
	    otp_8183_r1_mg_verify_handle_connect(Ev) 
    end.
-endif.

otp_8183_r1_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp_8183_r1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp_8183_r1_mg_verify_handle_connect(Else) ->
    io:format("otp_8183_r1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

-ifndef(megaco_hipe_special).
otp_8183_r1_mg_verify_service_change_rep_fun() ->
    fun(Rep) -> 
	    otp_8183_r1_mg_verify_service_change_rep(Rep) 
    end.
-endif.

otp_8183_r1_mg_verify_service_change_rep(
  {handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch otp_8183_r1_mg_do_verify_service_change_rep(AR));
otp_8183_r1_mg_verify_service_change_rep(Crap) ->
    io:format("otp_8183_r1_mg_verify_service_change_rep -> crap"
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

otp_8183_r1_mg_do_verify_service_change_rep(AR) ->
    io:format("otp_8183_r1_mg_do_verify_service_change_rep -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

-ifndef(megaco_hipe_special).
otp_8183_r1_mg_verify_notify_rep_fun(Nr) ->
    fun(Rep) -> 
	    otp_8183_r1_mg_verify_notify_rep(Nr, Rep) 
    end.
-endif.

otp_8183_r1_mg_verify_notify_rep(
  Nr, 
  {handle_trans_reply, _CH, ?VERSION, {ok, Nr, [AR]}, _}) ->
    io:format("otp_8183_r1_mg_verify_notify_rep -> ok"
	      "~n   Nr: ~p"
	      "~n   AR: ~p"
	      "~n", [Nr, AR]),
    {ok, AR, ok};
otp_8183_r1_mg_verify_notify_rep(
  ExpNr, 
  {handle_trans_reply, _CH, ?VERSION, {ok, ActNr, [AR]}, _}) ->
    io:format("otp_8183_r1_mg_verify_notify_rep -> error"
	      "~n   Expected Nr: ~p"
	      "~n   Actual Nr:   ~p"
	      "~n   AR:          ~p"
	      "~n", [ExpNr, ActNr, AR]),
    Error = {unexpected_nr, ExpNr, ActNr},
    {error, Error, ok};
otp_8183_r1_mg_verify_notify_rep(
  Nr, 
  {handle_trans_reply, _CH, ?VERSION, Res, _}) ->
    io:format("otp_8183_r1_mg_verify_notify_rep -> error"
	      "~n   Nr:  ~p"
	      "~n   Res: ~p"
	      "~n", [Nr, Res]),
    Error = {unexpected_result, Nr, Res},
    {error, Error, ok};
otp_8183_r1_mg_verify_notify_rep(Nr, Else) ->
    io:format("otp_8183_r1_mg_verify_notify_rep -> unknown"
	      "~n   Nr:   ~p"
	      "~n   Else: ~p"
	      "~n", [Nr, Else]),
    {error, Else, ok}.


otp_8183_r1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_8183_r1_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp8212_scr(MidStr) ->
    Msg = "!/1 " ++ MidStr ++ " T=19731{C=-{SC=ROOT{SV{MT=RS,RE=\"901\"}}}}",
    list_to_binary(Msg).

otp_8212(suite) ->
    [];
otp_8212(doc) ->
    [];
otp_8212(Config) when is_list(Config) ->
    %% ?ACQUIRE_NODES(1, Config),

    put(verbosity, debug),
    put(sname,     "TEST"),
    put(tc,        otp8212),
    i("starting"),

    Extra         = otp8212_extra, 
    NoMid         = preliminary_mid,
    LocalMid      = {deviceName, "MGC"}, 
    RemoteMidStr1 = "bgf1", 
    RemoteMid1    = {deviceName, RemoteMidStr1}, 
    RemoteMidStr2 = "bgf2", 
    RemoteMid2    = {deviceName, RemoteMidStr2}, 
    UserMod       = megaco_mess_otp8212_test,

    d("set megaco trace level to max",[]),
    megaco:enable_trace(max, io),

    d("start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),

    d("start local user (MGC) ~p", [LocalMid]),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod}],
    ?VERIFY(ok,	megaco:start_user(LocalMid, UserConfig)),

    d("get (MGC) receive info for ~p", [LocalMid]),
    RH0 = user_info(LocalMid, receive_handle),
    RH  = RH0#megaco_receive_handle{encoding_mod = megaco_mess_otp8212_test,
				    encoding_config = []},
    
    d("do a pre-connect for ~p", [LocalMid]),
    ControlPid = self(), 
    SendHandle = {RH, ControlPid, RemoteMidStr1, RemoteMidStr2}, 
    ?VERIFY({ok, _}, megaco:connect(RH, NoMid, SendHandle, ControlPid)),
    
    d("simulate incomming service change message from ~p", 
      [RemoteMidStr1]),
    ?VERIFY(ok, 
	    megaco:process_received_message(RH, ControlPid, 
					    otp8212_scr, 
					    otp8212_scr(RemoteMidStr1))),

    d("get the updated connection handle", []),
    [CH] = megaco:user_info(LocalMid, connections),
    
    d("verify connection with ~p", [RemoteMidStr1]),
    ?VERIFY(RemoteMid1, megaco:conn_info(CH, remote_mid)),
    
    d("send a request to ~p but receive no reply but an unexpected call", 
      [RemoteMidStr1]),
    Res = megaco:call(CH, ["action request"], [{request_timer, 2000}]), 
    d("request result: ~p", [Res]),
    ?VERIFY({1, [{error, {wrong_mid, RemoteMid2, RemoteMid1, _}, {Extra, _}}]}, Res),

    Conns = disconnect_all(LocalMid), 
    await_disconnected(Conns),

    d("stop megaco user ~p",[LocalMid]),
    ok = await_stopped_user(LocalMid), 

    d("stop megaco app",[]),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),

    d("done",[]),
    ok.

disconnect_all(LocalMid) ->
    Conns = megaco:user_info(LocalMid, connections), 
    d("[~p] disconnect from all connections: ~n~p", [LocalMid, Conns]),
    lists:foreach(
      fun(Conn) ->
	      d("[~p] disconnect from connection ~p", [LocalMid, Conn]),
	      DiscoRes = megaco:disconnect(Conn, {otp8212_done, self()}),
	      d("[~p] disconnect result: ~p", [LocalMid, DiscoRes])
      end,
      Conns),
    Conns.
    
await_disconnected([]) ->
    ok;
await_disconnected(Conns) ->
    receive
	{disconnected, Conn} ->
	    d("disconnected: ~p", [Conn]),
	    Conns2 = lists:delete(Conn, Conns),
	    await_disconnected(Conns2)
    end.


await_stopped_user(LocalMid) ->
    await_stopped_user(LocalMid, 10).

await_stopped_user(LocalMid, N) when N =< 0 ->
    ?ERROR({failed_stopping_user, LocalMid});
await_stopped_user(LocalMid, N) ->
    case megaco:stop_user(LocalMid) of
	ok ->
	    d("user stopped(~w)", [N]),
	    ok;
	{error, {active_connections, _}} ->
	    d("still active connections when N = ~w", [N]),
	    Conns = disconnect_all(LocalMid), 
	    await_disconnected(Conns), 
	    ?SLEEP(500),
	    await_stopped_user(LocalMid, N-1)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cre_ErrDesc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.
	    

cre_serviceChangeParm(M,R,P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M,
                         serviceChangeReason  = R,
                         serviceChangeProfile = P}.

cre_serviceChangeParm(M, V, R, P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M, 
			 serviceChangeVersion = V,
                         serviceChangeReason  = R, 
                         serviceChangeProfile = P}.

cre_serviceChangeReq(Tid, Parms) ->
    #'ServiceChangeRequest'{terminationID      = Tid, 
                            serviceChangeParms = Parms}.

cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

cre_obsEvent(Name, Not) ->
    #'ObservedEvent'{eventName    = Name, 
                     timeNotation = Not}.

cre_obsEvsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId        = Id, 
                                observedEventLst = EvList}.

cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID            = Tid, 
                     observedEventsDescriptor = EvsDesc}.

cre_command(R) when is_record(R, 'NotifyRequest') ->
    {notifyReq, R};
cre_command(R) when is_record(R, 'ServiceChangeRequest') ->
    {serviceChangeReq, R}.

cre_cmdReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_actionReq(CtxId, CmdReqs) when is_list(CmdReqs) ->
    #'ActionRequest'{contextId       = CtxId,
                     commandRequests = CmdReqs}.

cre_transReq(TransId, ARs) when is_list(ARs) ->
    #'TransactionRequest'{transactionId = TransId,
			  actions       = ARs}.

cre_transResult(ED) when is_record(ED, 'ErrorDescriptor') ->
    {transactionError, ED};
cre_transResult([AR|_] = ARs) when is_record(AR, 'ActionReply') ->
    {actionReplies, ARs}.

cre_transReply(TransId, Res) ->
    #'TransactionReply'{transactionId     = TransId,
			transactionResult = Res}.

cre_transReply(TransId, IAR, Res) ->
    #'TransactionReply'{transactionId     = TransId,
			immAckRequired    = IAR, 
			transactionResult = Res}.


%% --

cre_serviceChangeResParm(Mid) ->
    cre_serviceChangeResParm(Mid, ?VERSION).

cre_serviceChangeResParm(Mid, V) ->
    #'ServiceChangeResParm'{serviceChangeMgcId   = Mid, 
			    serviceChangeVersion = V}.

cre_serviceChangeResult(SCRP) when is_record(SCRP, 'ServiceChangeResParm') ->
    {serviceChangeResParms, SCRP};
cre_serviceChangeResult(ED) when is_record(ED, 'ErrorDescriptor') ->
    {errorDescriptor, ED}.

cre_serviceChangeReply(Tid, Res) ->
    #'ServiceChangeReply'{terminationID       = Tid, 
                          serviceChangeResult = Res}.

cre_cmdReply(R) when is_record(R, 'NotifyReply') ->
    {notifyReply, R};
cre_cmdReply(R) when is_record(R, 'ServiceChangeReply') ->
    {serviceChangeReply, R}.

cre_transRespAck(TransAck) when is_record(TransAck, 'TransactionAck') ->
    [TransAck];
cre_transRespAck(TRA) when is_list(TRA) ->
    TRA.

cre_transAck(TransId) ->
    #'TransactionAck'{firstAck = TransId}.

cre_notifyReply(Tid) ->
    #'NotifyReply'{terminationID = Tid}.

cre_actionReply(CtxId, CmdRep) ->
    #'ActionReply'{contextId    = CtxId,
                   commandReply = CmdRep}.

cre_serviceChangeProf(Name, Ver) when is_list(Name) andalso is_integer(Ver) ->
    #'ServiceChangeProfile'{profileName = Name, 
                            version     = Ver}.

cre_transaction(Trans) when is_record(Trans, 'TransactionRequest') ->
    {transactionRequest, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionPending') ->
    {transactionPending, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionReply') ->
    {transactionReply, Trans};
cre_transaction(Trans) when is_list(Trans) ->
    {transactionResponseAck, Trans}.

cre_transactions(Trans) when is_list(Trans) ->
    {transactions, Trans}.

cre_message(Version, Mid, Body) ->
    #'Message'{version     = Version,
               mId         = Mid,
               messageBody = Body}.

cre_megacoMessage(Mess) ->
    #'MegacoMessage'{mess = Mess}.

service_change_request() ->
    Parm = #'ServiceChangeParm'{serviceChangeMethod = restart,
				serviceChangeReason = [?megaco_cold_boot]},
    SCR = #'ServiceChangeRequest'{terminationID = [?megaco_root_termination_id],
				  serviceChangeParms = Parm},
    CR = #'CommandRequest'{command = {serviceChangeReq, SCR}},
    #'ActionRequest'{contextId = ?megaco_null_context_id,
		     commandRequests = [CR]}.

service_change_reply(MgcMid) ->
    Res = {serviceChangeResParms, #'ServiceChangeResParm'{serviceChangeMgcId = MgcMid}},
    SCR = #'ServiceChangeReply'{terminationID = [?megaco_root_termination_id],
				serviceChangeResult = Res},
    #'ActionReply'{contextId = ?megaco_null_context_id,
		   commandReply = [{serviceChangeReply, SCR}]}.

local_ip_address() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4}} = inet:getaddr(Hostname, inet),
    {A1, A2, A3, A4}.

ipv4_mid() ->
    ipv4_mid(asn1_NOVALUE).

ipv4_mid(Port) ->
    IpAddr = local_ip_address(),
    Ip = tuple_to_list(IpAddr),
    {ip4Address, #'IP4Address'{address = Ip, portNumber = Port}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% -------------------------------------------------------------------------------
%%% Megaco user callback interface
%%% -------------------------------------------------------------------------------

handle_connect(ConnHandle, ProtocolVersion, Pid) ->
    Pid ! {handle_connect, {ConnHandle, ProtocolVersion}},
    ok.

handle_disconnect(_, _, {user_disconnect, test_complete}, _) ->
    ok;
handle_disconnect(ConnHandle, ProtocolVersion, Reason, Pid) ->
    Pid ! {handle_disconnect, {ConnHandle, ProtocolVersion, Reason}},
    ok.

handle_syntax_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
    Pid ! {handle_syntax_error,{ConnHandle, ProtocolVersion, ErrorDescriptor}},
    reply.
         
handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
    Pid ! {handle_message_error,{ConnHandle, ProtocolVersion, ErrorDescriptor}},
    reply.

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests, Pid) ->
    Pid ! {handle_trans_request,{ConnHandle, ProtocolVersion, ActionRequests}},
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
			    errorText = "not implemented yet"},
    {discard_ack, ED}.

handle_trans_long_request(ConnHandle, ProtocolVersion, Data, Pid) ->
    Pid ! {handle_trans_long_request,{ConnHandle, ProtocolVersion, Data}},
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
			    errorText = "not implemented yet"},
    {discard_ack, ED}.

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, Data, Pid) ->
    Pid ! {handle_trans_reply,{ConnHandle, ProtocolVersion, ActualReply, Data}},
    ok.

handle_trans_ack(ConnHandle, ProtocolVersion, Status, Data, Pid) ->
    Pid ! {handle_trans_ack,{ConnHandle, ProtocolVersion, Status, Data}},
    ok.

handle_unexpected_trans(ReceiveHandle, ProtocolVersion, Trans, Pid) ->
    Pid ! {handle_unexpected_trans, 
	   {ReceiveHandle, ProtocolVersion, Trans, Pid}},
    ok.

handle_trans_request_abort(ReceiveHandle, ProtocolVersion, TransNo, HandlerPid, Pid) ->
    Pid ! {handle_trans_request_abort, 
	   {ReceiveHandle, ProtocolVersion, TransNo, HandlerPid}},
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_msg(M, Mod, Conf) ->
    Mod:encode_message(Conf, M).

%% encode_msg(M, Mod, Conf, Ver) ->
%%     Mod:encode_message(Conf, Ver, M).

decode_msg(M, Mod, Conf) ->
    Mod:decode_message(Conf, M).

%% decode_msg(M, Mod, Conf, Ver) ->
%%     Mod:decode_message(Conf, Ver, M).



%%% -------------------------------------------------------------------------------
%%% Megaco transport module interface
%%% -------------------------------------------------------------------------------

send_message(Pid, Data) ->
    Pid ! {send_message, Data},
    ok.

% block(Pid) ->
%     Pid ! {block, dummy},
%     ok.

unblock(Pid) ->
    Pid ! {unblock, dummy},
    ok.

% close(Pid) ->
%     Pid ! {close, dummy},
%     ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_info(Mid, Key) ->
    case (catch megaco:user_info(Mid, Key)) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Val ->
	    ?LOG("user_info -> ok: "
		 "~n   ~p"
		 "~n", [Val]),
	    Val
    end.


stop_user(Mid) ->
    case (catch megaco:stop_user(Mid)) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Val ->
	    ?LOG("stop_user -> ok:"
		 "~n   ~p"
		 "~n", [Val]),
	    Val
    end.

connections() ->
    system_info(connections).

connections(Conns0) ->
    Conns1 = lists:sort(Conns0),
    case lists:sort(connections()) of
	Conns1 ->
	    ?LOG("connections -> ok:"
		 "~n   ~p"
		 "~n", [Conns1]),
	    Conns1;
	Conns2 ->
	    ?ERROR({Conns1, Conns2}),
	    Conns2
    end.

system_info(Key) ->
    megaco:system_info(Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

await_completion(Ids) ->
    case megaco_test_generator_lib:await_completion(Ids) of
	{ok, Reply} ->
	    d("OK => Reply: ~n~p", [Reply]),
	    ok;
	{error, Reply} ->
	    d("ERROR => Reply: ~n~p", [Reply]),
	    ?ERROR({failed, Reply})
    end.

await_completion(Ids, Timeout) ->
    case megaco_test_generator_lib:await_completion(Ids, Timeout) of
	{ok, Reply} ->
	    d("OK => Reply: ~n~p", [Reply]),
	    ok;
	{error, Reply} ->
	    d("ERROR => Reply: ~n~p", [Reply]),
	    ?ERROR({failed, Reply})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) -> receive after X -> ok end.

% error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, "INF", F, A).


d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, "DBG", F, A).


print(Severity, PRE, FMT, ARGS) ->
    print(Severity, get(verbosity), erlang:timestamp(), get(tc), PRE, FMT, ARGS).

print(Severity, Verbosity, Ts, Tc, P, F, A) ->
    print(printable(Severity,Verbosity), Ts, Tc, P, F, A).

print(true, TS, TC, P, F, A) ->
    S = ?F("*** [~s] ~s ~p ~s:~w ***"
           "~n   " ++ F ++ "~n", 
           [megaco:format_timestamp(TS), P, self(), get(sname), TC | A]),
    io:format("~s", [S]),
    io:format(user, "~s", [S]);
print(_, _, _, _, _, _) ->
    ok.


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to(To, Start) ->
    To - (mtime() - Start).

%% Time in milli seconds
mtime() ->
    {A,B,C} = erlang:timestamp(),
    A*1000000000+B*1000+(C div 1000).

