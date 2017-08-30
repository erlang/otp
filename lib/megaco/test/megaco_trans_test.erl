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
%% Purpose: Verify that the transaction sender works with acks.
%% 
%% Test:    ts:run(megaco, megaco_trans_test, [batch]).
%% 
%%----------------------------------------------------------------------
-module(megaco_trans_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(VERSION, 1).
-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(LOAD_COUNTER_START, 10).
-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).

-define(MGC_START(Pid, Mid, ET, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_GET_STATS(Pid, No), megaco_test_mgc:get_stats(Pid, No)).
-define(MGC_RESET_STATS(Pid), megaco_test_mgc:reset_stats(Pid)).
-define(MGC_REQ_DISC(Pid,To), megaco_test_mgc:request_discard(Pid,To)).
-define(MGC_REQ_PEND(Pid,To), megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HAND(Pid),    megaco_test_mgc:request_handle(Pid)).
-define(MGC_REQ_HANDS(Pid),   megaco_test_mgc:request_handle_sloppy(Pid)).
-define(MGC_UPDATE_UI(Pid,Tag,Val), 
	megaco_test_mgc:update_user_info(Pid,Tag,Val)).
-define(MGC_UPDATE_CI(Pid,Tag,Val), 
	megaco_test_mgc:update_conn_info(Pid,Tag,Val)).
-define(MGC_USER_INFO(Pid,Tag), megaco_test_mgc:user_info(Pid,Tag)).
-define(MGC_CONN_INFO(Pid,Tag), megaco_test_mgc:conn_info(Pid,Tag)).
-define(MGC_ACK_INFO(Pid,To),   megaco_test_mgc:ack_info(Pid,To)).
-define(MGC_REQ_INFO(Pid,To),   megaco_test_mgc:req_info(Pid,To)).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
	megaco_test_mg:start(Pid, Mid, Enc, Transp, Conf, Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_GET_STATS(Pid),   megaco_test_mg:get_stats(Pid)).
-define(MG_RESET_STATS(Pid), megaco_test_mg:reset_stats(Pid)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_RAR(Pid), megaco_test_mg:notify_request_and_reply(Pid)).
-define(MG_NOTIF_REQ(Pid), megaco_test_mg:notify_request(Pid)).
-define(MG_NOTIF_AR(Pid),  megaco_test_mg:await_notify_reply(Pid)).
-define(MG_CANCEL(Pid,R),  megaco_test_mg:cancel_request(Pid,R)).
-define(MG_APPLY_LOAD(Pid,CntStart), megaco_test_mg:apply_load(Pid,CntStart)).
-define(MG_UPDATE_UI(Pid,Tag,Val), 
	megaco_test_mg:update_user_info(Pid,Tag,Val)).
-define(MG_UPDATE_CI(Pid,Tag,Val), 
	megaco_test_mg:update_conn_info(Pid,Tag,Val)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).
-define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_GRP_REQ(Pid,N),     megaco_test_mg:group_requests(Pid,N)).
-define(MG_ACK_INFO(Pid,To),   megaco_test_mg:ack_info(Pid,To)).
-define(MG_REP_INFO(Pid,To),   megaco_test_mg:rep_info(Pid,To)).

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(multi_ack_maxcount = Case, Config) ->
    process_flag(trap_exit, true),
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout,timer:minutes(10)}|C]);
init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [{group, ack}, {group, trans_req},
     {group, trans_req_and_ack}, {group, pending},
     {group, reply}, {group, tickets}].

groups() -> 
    [{ack, [],
      [single_ack, multi_ack_timeout, multi_ack_maxcount]},
     {trans_req, [],
      [single_trans_req, multi_trans_req_timeout,
       multi_trans_req_maxcount1, multi_trans_req_maxcount2,
       multi_trans_req_maxsize1, multi_trans_req_maxsize2]},
     {trans_req_and_ack, [],
      [single_trans_req_and_ack,
       multi_trans_req_and_ack_timeout,
       multi_trans_req_and_ack_ackmaxcount,
       multi_trans_req_and_ack_reqmaxcount,
       multi_trans_req_and_ack_maxsize1,
       multi_trans_req_and_ack_maxsize2]},
     {pending, [],
      [single_trans_req_and_pending,
       multi_trans_req_and_pending,
       multi_trans_req_and_ack_and_pending,
       multi_ack_and_pending]},
     {reply, [],
      [multi_trans_req_and_reply,
       multi_trans_req_and_ack_and_reply,
       multi_ack_and_reply]},
     {tickets, [], [{group, otp_7192}]},
     {otp_7192, [], [otp_7192_1, otp_7192_2, otp_7192_3]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_ack(suite) ->
    [];
single_ack(doc) ->
    [];
single_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        single_ack),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("[MG] start"),    
    %% MgConf0 = [{MgNode, "mg", text, tcp, ?MG_VERBOSITY}],
    MgMid = {deviceName, "mg"},
    MgConfig = [{auto_ack, true}, {trans_timer, 5000}, {trans_ack, true}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("ensure the megaco stack calls the handle_trans_ack callback"),
    ?MGC_REQ_HANDS(Mgc),

    d("tell the MGC to send the ack's to us"),
    ?MGC_ACK_INFO(Mgc, self()),

    d("send the notify"),
    ?MG_GRP_REQ(Mg, 1),

    d("send the notify"),
    ?MG_NOTIF_REQ(Mg),

    d("await the ack"),
    await_ack(Mgc, 1, infinity, ok),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_ack_timeout(suite) ->
    [];
multi_ack_timeout(doc) ->
    [];
multi_ack_timeout(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [win32, {unix, [darwin, linux]}], 
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_ack_timeout),
    i("starting"),


    MaxCount = 20,
    MgcNode  = make_node_name(mgc),
    MgNode   = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("[MG] start"),    
    %% MgConf0 = [{MgNode, "mg", text, tcp, ?MG_VERBOSITY}],
    MgMid = {deviceName, "mg"},
    MgConfig = [{auto_ack,           true}, 
		{trans_ack,          true},
		{trans_timer,        10000}, 
		{trans_ack_maxcount, MaxCount + 10}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("wait some time"),
    sleep(1000),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    {ok, _OldAction} = ?MGC_REQ_HANDS(Mgc),

    d("tell the MGC to send the ack's to us"),
    ?MGC_ACK_INFO(Mgc, self()),

    d("set group size to ~p", [MaxCount]),
    ?MG_GRP_REQ(Mg, MaxCount),

    d("[MG] send a group of requests (and await the replies)"),
    ?MG_NOTIF_RAR(Mg),

    d("await the ack(s)"),
    await_ack(Mgc, MaxCount, 60000, ok),

    i("wait some time before closing down"),
    sleep(5000),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_ack_maxcount(suite) ->
    [];
multi_ack_maxcount(doc) ->
    [];
multi_ack_maxcount(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_ack_maxcount),
    i("starting"),

    MaxCount = 10,
    MgcNode  = make_node_name(mgc),
    MgNode   = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("[MG] start"),    
    %% MgConf0 = [{MgNode, "mg", text, tcp, ?MG_VERBOSITY}],
    MgMid = {deviceName, "mg"},
    MgConfig = [%% {auto_ack,          true}, 
		%% {trans_timer,    120000}, 
		%% {trans_ack_maxcount, MaxCount}
	       ],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("wait some time"),
    sleep(1000),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    ?MG_UPDATE_CI(Mg,auto_ack,true), 
    ?MG_UPDATE_CI(Mg,trans_timer,120000), 
    ?MG_UPDATE_CI(Mg,trans_ack_maxcount,MaxCount), 
    ?MG_UPDATE_CI(Mg,trans_ack,true), 

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    {ok, _OldAction} = ?MGC_REQ_HANDS(Mgc),

    d("tell the MGC to send the ack's to us"),
    ?MGC_ACK_INFO(Mgc, self()),

    d("set group size to ~p", [MaxCount]),
    ?MG_GRP_REQ(Mg, MaxCount),

    d("[MG] send a group of requests (and await the replies)"),
    ?MG_NOTIF_RAR(Mg),

    d("await the ack"),
    await_ack(Mgc, MaxCount, 60000, ok),

    i("wait some time before closing down"),
    sleep(5000),


    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_trans_req(suite) ->
    [];
single_trans_req(doc) ->
    [];
single_trans_req(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        single_trans_req),
    process_flag(trap_exit, true),    
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
    MgcEvSeq = str_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = str_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(str_mgc_connect_verify_fun(), 
        {?MODULE, str_mgc_verify_handle_connect, []}).
-define(str_mgc_service_change_req_verify_fun(Mid),
        {?MODULE, str_mgc_verify_service_change_req, [Mid]}).
-define(str_mgc_notify_req_verify_fun(),
        {?MODULE, str_mgc_verify_notify_request, []}).
-define(str_mgc_disco_verify_fun(),
        {?MODULE, str_mgc_verify_handle_disconnect, []}).
-else.
-define(str_mgc_connect_verify_fun(), 
        fun str_mgc_verify_handle_connect/1).
-define(str_mgc_service_change_req_verify_fun(Mid),
        str_mgc_verify_service_change_req_fun(Mid)).
-define(str_mgc_notify_req_verify_fun(),
	str_mgc_verify_notify_request_fun()).
-define(str_mgc_disco_verify_fun(),
	fun str_mgc_verify_handle_disconnect/1).
-endif.

str_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?str_mgc_connect_verify_fun(),
    ServiceChangeReqVerify = ?str_mgc_service_change_req_verify_fun(Mid),
    NotifyReqVerify        = ?str_mgc_notify_req_verify_fun(),
    DiscoVerify            = ?str_mgc_disco_verify_fun(),
%%     ConnectVerify = fun str_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = str_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify = str_mgc_verify_notify_request_fun(),
%%     DiscoVerify = fun str_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, false},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect, ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect, DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


str_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("str_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
str_mgc_verify_handle_connect(Else) ->
    io:format("str_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


str_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    str_mgc_verify_service_change_req(Ev, Mid)
    end.

str_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("str_mgc_verify_service_change_req -> ok"
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
						 [str_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = str_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = str_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = str_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = str_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = str_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
str_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("str_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = str_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.


str_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    str_mgc_verify_notify_request(Ev)
    end.

str_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("str_mgc_verify_notify_request:fun -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [str_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = str_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
str_mgc_verify_notify_request(Else) ->
    io:format("str_mgc_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = str_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.


str_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("str_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
str_mgc_verify_handle_disconnect(Else) ->
    io:format("str_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


str_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

str_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = str_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

str_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

str_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = str_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(str_mg_connect_verify_fun(), 
        {?MODULE, str_mg_verify_handle_connect, []}).
-define(str_mg_service_change_reply_verify_fun(),
        {?MODULE, str_mg_verify_service_change_reply, []}).
-define(str_mg_notify_reply_verify_fun(),
        {?MODULE, str_mg_verify_notify_reply, []}).
-else.
-define(str_mg_connect_verify_fun(), 
        fun str_mg_verify_handle_connect/1).
-define(str_mg_service_change_reply_verify_fun(),
        fun str_mg_verify_service_change_reply/1).
-define(str_mg_notify_reply_verify_fun(),
	fun str_mg_verify_notify_reply/1).
-endif.

str_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [str_mg_service_change_request_ar(Mid, 1)],
    Tid            = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq        = [str_mg_notify_request_ar(1, Tid, 1)],
    ConnectVerify            = ?str_mg_connect_verify_fun(), 
    ServiceChangeReplyVerify = ?str_mg_service_change_reply_verify_fun(), 
    NotifyReplyVerify        = ?str_mg_notify_reply_verify_fun(), 
    %% ConnectVerify = fun str_mg_verify_handle_connect/1,
    %% ServiceChangeReplyVerify = fun str_mg_verify_service_change_reply/1,
    %% NotifyReplyVerify = fun str_mg_verify_notify_reply/1, 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     {megaco_trace, max}, 
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
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

str_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("str_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
str_mg_verify_handle_connect(Else) ->
    io:format("str_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

str_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
				    {ok, [AR]}, _}) ->
    io:format("str_mg_verify_service_change_reply -> ok"
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
str_mg_verify_service_change_reply(Else) ->
    io:format("str_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

str_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			    {ok, [AR]}, _}) ->
    io:format("str_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
str_mg_verify_notify_reply(Else) ->
    io:format("str_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

str_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

str_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = str_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

str_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

str_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = str_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the single_trans_req test case
%%

str_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_timeout(suite) ->
    [];
multi_trans_req_timeout(doc) ->
    [];
multi_trans_req_timeout(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_timeout),
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
    MgcEvSeq = mtrt_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrt_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrt_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrt_mgc_verify_handle_connect, []}).
-define(mtrt_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrt_mgc_verify_service_change_req, [Mid]}).
-define(mtrt_mgc_verify_notify_req_fun(),
        {?MODULE, mtrt_mgc_verify_notify_request, []}).
-define(mtrt_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrt_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrt_mgc_verify_handle_connect_fun(), 
        fun mtrt_mgc_verify_handle_connect/1).
-define(mtrt_mgc_verify_service_change_req_fun(Mid),
        mtrt_mgc_verify_service_change_req_fun(Mid)).
-define(mtrt_mgc_verify_notify_req_fun(),
	mtrt_mgc_verify_notify_request_fun()).
-define(mtrt_mgc_verify_handle_disconnect_fun(),
	fun mtrt_mgc_verify_handle_disconnect/1).
-endif.

mtrt_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrt_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrt_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrt_mgc_verify_notify_req_fun(),
    DiscoVerify            = ?mtrt_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrt_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrt_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrt_mgc_verify_notify_request_fun(),
%%     DiscoVerify            = fun mtrt_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect, ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect, DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrt_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrt_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrt_mgc_verify_handle_connect(Else) ->
    io:format("mtrt_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrt_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrt_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrt_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrt_mgc_verify_service_change_req -> ok"
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
						 [mtrt_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrt_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrt_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrt_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrt_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrt_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrt_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrt_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrt_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrt_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrt_mgc_verify_notify_request(Ev)
    end.

mtrt_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrt_mgc_verify_notify_request:fun -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtrt_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrt_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrt_mgc_verify_notify_request(Else) ->
    io:format("mtrt_mgc_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrt_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrt_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrt_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrt_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrt_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrt_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrt_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrt_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrt_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrt_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrt_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrt_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrt_mg_verify_handle_connect, []}).
-define(mtrt_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrt_mg_verify_service_change_reply, []}).
-define(mtrt_mg_verify_notify_reply_fun(),
        {?MODULE, mtrt_mg_verify_notify_reply, []}).
-else.
-define(mtrt_mg_verify_handle_connect_fun(), 
        fun mtrt_mg_verify_handle_connect/1).
-define(mtrt_mg_verify_service_change_reply_fun(),
        fun mtrt_mg_verify_service_change_reply/1).
-define(mtrt_mg_verify_notify_reply_fun(),
	fun mtrt_mg_verify_notify_reply/1).
-endif.

mtrt_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrt_mg_service_change_request_ar(Mid, 1)],
    Tid            = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq        = [mtrt_mg_notify_request_ar(1, Tid, 1)],
    ConnectVerify            = ?mtrt_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrt_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrt_mg_verify_notify_reply_fun(), 
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
	     {megaco_update_conn_info, trans_timer, 5000},
	     {megaco_update_conn_info, trans_req,   true},
	     {megaco_conn_info, all},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrt_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrt_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrt_mg_verify_handle_connect(Else) ->
    io:format("mtrt_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrt_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
				     {ok, [AR]}, _}) ->
    io:format("mtrt_mg_verify_service_change_reply -> ok"
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
mtrt_mg_verify_service_change_reply(Else) ->
    io:format("mtrt_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrt_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			     {ok, [AR]}, _}) ->
    io:format("mtrt_mg_verify_service_change_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtrt_mg_verify_notify_reply(Else) ->
    io:format("mtrt_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrt_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrt_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrt_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrt_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrt_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrt_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrt_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_maxcount1(suite) ->
    [];
multi_trans_req_maxcount1(doc) ->
    "Test that a message is sent when req_maxcount is reached";
multi_trans_req_maxcount1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_maxcount1),
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
    MgcEvSeq = mtrmc1_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrmc1_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrmc1_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrmc1_mgc_verify_handle_connect, []}).
-define(mtrmc1_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrmc1_mgc_verify_service_change_req, [Mid]}).
-define(mtrmc1_mgc_verify_notify_req_fun(),
        {?MODULE, mtrmc1_mgc_verify_notify_request, []}).
-define(mtrmc1_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrmc1_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrmc1_mgc_verify_handle_connect_fun(), 
        fun mtrmc1_mgc_verify_handle_connect/1).
-define(mtrmc1_mgc_verify_service_change_req_fun(Mid),
        mtrmc1_mgc_verify_service_change_req_fun(Mid)).
-define(mtrmc1_mgc_verify_notify_req_fun(),
	mtrmc1_mgc_verify_notify_request_fun()).
-define(mtrmc1_mgc_verify_handle_disconnect_fun(),
	fun mtrmc1_mgc_verify_handle_disconnect/1).
-endif.

mtrmc1_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrmc1_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrmc1_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrmc1_mgc_verify_notify_req_fun(),
    DiscoVerify            = ?mtrmc1_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify = fun mtrmc1_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrmc1_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify = mtrmc1_mgc_verify_notify_request_fun(),
%%     DiscoVerify = fun mtrmc1_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect, ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect, DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrmc1_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrmc1_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrmc1_mgc_verify_handle_connect(Else) ->
    io:format("mtrmc1_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc1_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrmc1_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrmc1_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrmc1_mgc_verify_service_change_req -> ok"
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
						 [mtrmc1_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrmc1_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrmc1_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrmc1_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrmc1_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrmc1_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrmc1_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrmc1_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrmc1_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrmc1_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrmc1_mgc_verify_notify_request(Ev)
    end.

mtrmc1_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrmc1_mgc_verify_notify_request:fun -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtrmc1_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrmc1_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrmc1_mgc_verify_notify_request(Else) ->
    io:format("mtrmc1_mgc_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrmc1_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrmc1_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrmc1_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrmc1_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrmc1_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrmc1_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrmc1_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrmc1_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrmc1_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrmc1_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrmc1_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrmc1_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrmc1_mg_verify_handle_connect, []}).
-define(mtrmc1_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrmc1_mg_verify_service_change_reply, []}).
-define(mtrmc1_mg_verify_notify_reply_fun(),
        {?MODULE, mtrmc1_mg_verify_notify_reply, []}).
-else.
-define(mtrmc1_mg_verify_handle_connect_fun(), 
        fun mtrmc1_mg_verify_handle_connect/1).
-define(mtrmc1_mg_verify_service_change_reply_fun(),
        fun mtrmc1_mg_verify_service_change_reply/1).
-define(mtrmc1_mg_verify_notify_reply_fun(),
	fun mtrmc1_mg_verify_notify_reply/1).
-endif.

mtrmc1_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrmc1_mg_service_change_request_ar(Mid, 1)],
    Tid            = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq        = [mtrmc1_mg_notify_request_ar(1, Tid, 1)],
    ConnectVerify            = ?mtrmc1_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrmc1_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrmc1_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrmc1_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrmc1_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrmc1_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, trans_req_maxcount, 5},
	     {megaco_update_conn_info, trans_req_maxsize,  4096},
	     {megaco_update_conn_info, trans_timer, 120000},
	     {megaco_update_conn_info, trans_req,   true},
	     {megaco_conn_info, all},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_cast, NotifyReq, []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrmc1_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrmc1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrmc1_mg_verify_handle_connect(Else) ->
    io:format("mtrmc1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc1_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
				      {ok, [AR]}, _}) ->
    io:format("mtrmc1_mg_verify_service_change_reply -> ok"
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
mtrmc1_mg_verify_service_change_reply(Else) ->
    io:format("mtrmc1_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc1_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			      {ok, [AR]}, _}) ->
    io:format("mtrmc1_mg_verify_service_change_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtrmc1_mg_verify_notify_reply(Else) ->
    io:format("mtrmc1_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrmc1_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrmc1_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrmc1_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrmc1_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrmc1_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrmc1_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_maxcount2(suite) ->
    [];
multi_trans_req_maxcount2(doc) ->
    "Test that the message is sent when req_maxcount is reached "
	"with a request bigger then maxsize limit";
multi_trans_req_maxcount2(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_maxcount2),
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
    MgcEvSeq = mtrmc2_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
   {ok, MgcId} =  megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrmc2_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrmc2_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrmc2_mgc_verify_handle_connect, []}).
-define(mtrmc2_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrmc2_mgc_verify_service_change_req, [Mid]}).
-define(mtrmc2_mgc_verify_notify_req_fun(),
        {?MODULE, mtrmc2_mgc_verify_notify_request, []}).
-define(mtrmc2_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrmc2_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrmc2_mgc_verify_handle_connect_fun(), 
        fun mtrmc2_mgc_verify_handle_connect/1).
-define(mtrmc2_mgc_verify_service_change_req_fun(Mid),
        mtrmc2_mgc_verify_service_change_req_fun(Mid)).
-define(mtrmc2_mgc_verify_notify_req_fun(),
	mtrmc2_mgc_verify_notify_request_fun()).
-define(mtrmc2_mgc_verify_handle_disconnect_fun(),
	fun mtrmc2_mgc_verify_handle_disconnect/1).
-endif.

mtrmc2_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrmc2_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrmc2_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrmc2_mgc_verify_notify_req_fun(),
    DiscoVerify            = ?mtrmc2_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrmc2_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrmc2_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrmc2_mgc_verify_notify_request_fun(),
%%     DiscoVerify            = fun mtrmc2_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrmc2_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrmc2_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrmc2_mgc_verify_handle_connect(Else) ->
    io:format("mtrmc2_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc2_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrmc2_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrmc2_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrmc2_mgc_verify_service_change_req -> ok"
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
						 [mtrmc2_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrmc2_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrmc2_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrmc2_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrmc2_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrmc2_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrmc2_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrmc2_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrmc2_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrmc2_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrmc2_mgc_verify_notify_request(Ev)
    end.

mtrmc2_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrmc2_mgc_verify_notify_request:fun -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = [CR]} ->
	    io:format("mtrmc2_mgc_verify_notify_request:fun -> "
		      "single command",[]),
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, 
		     [mtrmc2_mgc_notify_reply_ar1(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = CRs} ->
	    io:format("mtrmc2_mgc_verify_notify_request:fun -> "
		      "multi command (~w)",[length(CRs)]),
	    Tids = [Tid || 
		       #'CommandRequest'{command = 
					 {notifyReq, 
					  #'NotifyRequest'{
					    terminationID = [Tid]}}} 
			   <- CRs],
	    Reply = 
		{discard_ack, 
		 [mtrmc2_mgc_notify_reply_ar2(Cid, Tids)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrmc2_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrmc2_mgc_verify_notify_request(Else) ->
    io:format("mtrmc2_mgc_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrmc2_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrmc2_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrmc2_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrmc2_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrmc2_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrmc2_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrmc2_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrmc2_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrmc2_mgc_notify_reply_ar1(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrmc2_mgc_notify_reply_ar2(Cid, Tids) ->
    CRs = [cre_cmdReply(cre_notifyReply([Tid])) || Tid <- Tids],
    cre_actionReply(Cid, CRs).

mtrmc2_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrmc2_mgc_notify_reply_ar1(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrmc2_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrmc2_mg_verify_handle_connect, []}).
-define(mtrmc2_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrmc2_mg_verify_service_change_reply, []}).
-define(mtrmc2_mg_verify_notify_reply_fun(),
        {?MODULE, mtrmc2_mg_verify_notify_reply, []}).
-else.
-define(mtrmc2_mg_verify_handle_connect_fun(), 
        fun mtrmc2_mg_verify_handle_connect/1).
-define(mtrmc2_mg_verify_service_change_reply_fun(),
        fun mtrmc2_mg_verify_service_change_reply/1).
-define(mtrmc2_mg_verify_notify_reply_fun(),
	fun mtrmc2_mg_verify_notify_reply/1).
-endif.

mtrmc2_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrmc2_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR1 = fun(N) ->
		  [mtrmc2_mg_notify_request_ar1(N, Tid, N)]
	  end,
    NR2 = fun(N) ->
		  [mtrmc2_mg_notify_request_ar2(N, Tid, N)]
	  end,
    ConnectVerify            = ?mtrmc2_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrmc2_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrmc2_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrmc2_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrmc2_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrmc2_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, trans_req_maxcount, 5},
	     {megaco_update_conn_info, trans_req_maxsize,  1024},
	     {megaco_update_conn_info, trans_timer, 120000},
	     {megaco_update_conn_info, trans_req,   true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR1(1), []},
	     {megaco_cast, [NR1(2), NR1(3)], []},
	     {megaco_cast, NR1(4), []},
	     {megaco_cast, NR2(5), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrmc2_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrmc2_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrmc2_mg_verify_handle_connect(Else) ->
    io:format("mtrmc2_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc2_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
				       {ok, [AR]}, _}) ->
    io:format("mtrmc2_mg_verify_service_change_reply -> ok"
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
mtrmc2_mg_verify_service_change_reply(Else) ->
    io:format("mtrmc2_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc2_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("mtrmc2_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionReply'{commandReply = [{notifyReply, _NR}]} ->
	    io:format("mtrmc2_mg_verify_notify_reply -> "
		      "single notify reply", []),
	    {ok, AR, ok};
	#'ActionReply'{commandReply = [{notifyReply, _NR}|_] = CR} ->
	    io:format("mtrmc2_mg_verify_notify_reply -> "
		      "multi notify reply: (~w)", [length(CR)]),
	    {ok, AR, ok};
	_ ->
	    {error, {invalid_action_reply, AR}, ok}
    end;
mtrmc2_mg_verify_notify_reply(Else) ->
    io:format("mtrmc2_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrmc2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrmc2_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrmc2_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrmc2_mg_notify_request_ar1(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrmc2_mg_notify_request_ar2(Rid, Tid, Cid) ->
    F = fun(N) ->
		T       = 22000000 + N,
		TS      = integer_to_list(T),
		TT      = cre_timeNotation("19990729", TS),
		Ev      = cre_obsEvent("al/of", TT),
		EvsDesc = cre_obsEvsDesc(Rid+N, [Ev]),
		NR      = cre_notifyReq([Tid], EvsDesc),
		CMD     = cre_command(NR),
		cre_cmdReq(CMD)
	end,
    Ns = [0,1,2,3,4,5,6,7,8,9],
    CRs = [F(N) || N <- Ns],
    cre_actionReq(Cid, CRs).

mtrmc2_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrmc2_mg_notify_request_ar1(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrmc2_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_maxsize1(suite) ->
    [];
multi_trans_req_maxsize1(doc) ->
    "Test that the message is sent when req_maxsize is reached";
multi_trans_req_maxsize1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_maxsize1),
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
    MgcEvSeq = mtrms1_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrms1_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrms1_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrms1_mgc_verify_handle_connect, []}).
-define(mtrms1_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrms1_mgc_verify_service_change_req, [Mid]}).
-define(mtrms1_mgc_verify_notify_req_fun(),
        {?MODULE, mtrms1_mgc_verify_notify_request, []}).
-define(mtrms1_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrms1_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrms1_mgc_verify_handle_connect_fun(), 
        fun mtrms1_mgc_verify_handle_connect/1).
-define(mtrms1_mgc_verify_service_change_req_fun(Mid),
        mtrms1_mgc_verify_service_change_req_fun(Mid)).
-define(mtrms1_mgc_verify_notify_req_fun(),
	mtrms1_mgc_verify_notify_request_fun()).
-define(mtrms1_mgc_verify_handle_disconnect_fun(),
	fun mtrms1_mgc_verify_handle_disconnect/1).
-endif.

mtrms1_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrms1_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrms1_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrms1_mgc_verify_notify_req_fun(),
    DiscoVerify            = ?mtrms1_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrms1_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrms1_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify1       = mtrms1_mgc_verify_notify_request_fun1(),
%%     DiscoVerify            = fun mtrms1_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrms1_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrms1_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrms1_mgc_verify_handle_connect(Else) ->
    io:format("mtrms1_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms1_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrms1_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrms1_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrms1_mgc_verify_service_change_req -> ok"
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
						 [mtrms1_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrms1_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrms1_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrms1_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrms1_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrms1_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrms1_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrms1_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrms1_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrms1_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrms1_mgc_verify_notify_request(Ev)
    end.

mtrms1_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrms1_mgc_verify_notify_request:fun1 -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, 
		     [mtrms1_mgc_notify_reply_ar1(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrms1_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrms1_mgc_verify_notify_request(Else) ->
    io:format("mtrms1_mgc_verify_notify_request:fun1 -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrms1_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrms1_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrms1_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrms1_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrms1_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrms1_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrms1_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrms1_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrms1_mgc_notify_reply_ar1(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrms1_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrms1_mgc_notify_reply_ar1(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrms1_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrms1_mg_verify_handle_connect, []}).
-define(mtrms1_mg_verify_service_change_reply_fun(Mid),
        {?MODULE, mtrms1_mg_verify_service_change_reply, [Mid]}).
-define(mtrms1_mg_verify_notify_reply_fun(),
        {?MODULE, mtrms1_mg_verify_notify_reply, []}).
-else.
-define(mtrms1_mg_verify_handle_connect_fun(), 
        fun mtrms1_mg_verify_handle_connect/1).
-define(mtrms1_mg_verify_service_change_reply_fun(),
        fun mtrms1_mg_verify_service_change_reply/1).
-define(mtrms1_mg_verify_notify_reply_fun(),
	fun mtrms1_mg_verify_notify_reply/1).
-endif.

mtrms1_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrms1_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(N) ->
		 [mtrms1_mg_notify_request_ar1(N, Tid, N)]
	 end,
    ConnectVerify            = ?mtrms1_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrms1_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrms1_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrms1_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrms1_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrms1_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxsize,  650},
	     {megaco_update_conn_info, trans_timer, 120000},
	     {megaco_update_conn_info, trans_req,   true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1), []},
	     {megaco_cast, [NR(2), NR(3)], []},
	     {megaco_cast, NR(4), []},
	     {megaco_cast, NR(5), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrms1_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrms1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrms1_mg_verify_handle_connect(Else) ->
    io:format("mtrms1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms1_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
				       {ok, [AR]}, _}) ->
    io:format("mtrms1_mg_verify_service_change_reply -> ok"
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
mtrms1_mg_verify_service_change_reply(Else) ->
    io:format("mtrms1_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms1_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("mtrms1_mg_verify_service_change_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtrms1_mg_verify_notify_reply(Else) ->
    io:format("mtrms1_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrms1_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrms1_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrms1_mg_notify_request_ar1(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrms1_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrms1_mg_notify_request_ar1(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


mtrms1_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_maxsize2(suite) ->
    [];
multi_trans_req_maxsize2(doc) ->
    "Test that the message is sent when req_maxsize is reached, "
	"when the 'last' message is bigger then req_maxsize itself";
multi_trans_req_maxsize2(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_maxsize2),
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
    MgcEvSeq = mtrms2_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrms2_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrms2_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrms2_mgc_verify_handle_connect, []}).
-define(mtrms2_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrms2_mgc_verify_service_change_req, [Mid]}).
-define(mtrms2_mgc_verify_notify_req_fun(),
        {?MODULE, mtrms2_mgc_verify_notify_request, []}).
-define(mtrms2_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrms2_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrms2_mgc_verify_handle_connect_fun(), 
        fun mtrms2_mgc_verify_handle_connect/1).
-define(mtrms2_mgc_verify_service_change_req_fun(Mid),
        mtrms2_mgc_verify_service_change_req_fun(Mid)).
-define(mtrms2_mgc_verify_notify_req_fun(),
	mtrms2_mgc_verify_notify_request_fun()).
-define(mtrms2_mgc_verify_handle_disconnect_fun(),
	fun mtrms2_mgc_verify_handle_disconnect/1).
-endif.

mtrms2_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrms2_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrms2_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrms2_mgc_verify_notify_req_fun(),
    DiscoVerify            = ?mtrms2_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrms2_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrms2_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrms2_mgc_verify_notify_request_fun(),
%%     DiscoVerify            = fun mtrms2_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrms2_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrms2_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrms2_mgc_verify_handle_connect(Else) ->
    io:format("mtrms2_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms2_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrms2_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrms2_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrms2_mgc_verify_service_change_req -> ok"
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
						 [mtrms2_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrms2_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrms2_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrms2_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrms2_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrms2_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrms2_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrms2_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrms2_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrms2_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrms2_mgc_verify_notify_request(Ev)
    end.

mtrms2_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrms2_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = [CR]} ->
	    io:format("mtrms2_mgc_verify_notify_request:fun -> "
		      "single command", []),
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, 
		     [mtrms2_mgc_notify_reply_ar1(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = Cid, 
			 commandRequests = CRs} ->
	    io:format("mtrms2_mgc_verify_notify_request:fun -> "
		      "multi command (~w)", [length(CRs)]),
	    Tids = [Tid || 
		       #'CommandRequest'{command = 
					 {notifyReq, 
					  #'NotifyRequest'{
					    terminationID = [Tid]}}} 
			   <- CRs],
	    Reply = 
		{discard_ack, 
		 [mtrms2_mgc_notify_reply_ar2(Cid, Tids)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrms2_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrms2_mgc_verify_notify_request(Else) ->
    io:format("mtrms2_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrms2_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrms2_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrms2_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrms2_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrms2_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrms2_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrms2_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrms2_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrms2_mgc_notify_reply_ar1(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrms2_mgc_notify_reply_ar2(Cid, Tids) ->
    CRs = [cre_cmdReply(cre_notifyReply([Tid])) || Tid <- Tids],
    cre_actionReply(Cid, CRs).

mtrms2_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrms2_mgc_notify_reply_ar1(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrms2_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrms2_mg_verify_handle_connect, []}).
-define(mtrms2_mg_verify_service_change_reply_fun(Mid),
        {?MODULE, mtrms2_mg_verify_service_change_reply, [Mid]}).
-define(mtrms2_mg_verify_notify_reply_fun(),
        {?MODULE, mtrms2_mg_verify_notify_reply, []}).
-else.
-define(mtrms2_mg_verify_handle_connect_fun(), 
        fun mtrms2_mg_verify_handle_connect/1).
-define(mtrms2_mg_verify_service_change_reply_fun(),
        fun mtrms2_mg_verify_service_change_reply/1).
-define(mtrms2_mg_verify_notify_reply_fun(),
	fun mtrms2_mg_verify_notify_reply/1).
-endif.

mtrms2_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrms2_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq1 = [mtrms2_mg_notify_request_ar1(1, Tid, 1)],
    NotifyReq2 = [mtrms2_mg_notify_request_ar2(2, Tid, 2)],
    ConnectVerify            = ?mtrms2_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrms2_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrms2_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrms2_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrms2_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrms2_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxsize,  1024},
	     {megaco_update_conn_info, trans_timer, 120000},
	     {megaco_update_conn_info, trans_req,   true},
	     {megaco_conn_info, all},
	     {megaco_cast, NotifyReq1, []},
	     {megaco_cast, [NotifyReq1, NotifyReq1], []},
	     {megaco_cast, NotifyReq1, []},
	     {megaco_cast, NotifyReq2, []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrms2_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrms2_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrms2_mg_verify_handle_connect(Else) ->
    io:format("mtrms2_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms2_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
				       {ok, [AR]}, _}) ->
    io:format("mtrms2_mg_verify_service_change_reply -> ok"
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
mtrms2_mg_verify_service_change_reply(Else) ->
    io:format("mtrms2_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms2_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("mtrms2_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionReply'{commandReply = [{notifyReply, _NR}]} ->
	    io:format("mtrms2_mg_verify_notify_reply -> "
		      "single notify reply", []),
	    {ok, AR, ok};
	#'ActionReply'{commandReply = [{notifyReply, _NR}|_] = CR} ->
	    io:format("mtrms2_mg_verify_notify_reply -> "
		      "multi notify reply: (~w)", [length(CR)]),
	    {ok, AR, ok};
	_ ->
	    {error, {invalid_action_reply, AR}, ok}
    end;
mtrms2_mg_verify_notify_reply(Else) ->
    io:format("mtrms2_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrms2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrms2_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrms2_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrms2_mg_notify_request_ar1(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrms2_mg_notify_request_ar2(Rid, Tid, Cid) ->
    F = fun(N) ->
		T       = 22000000 + N,
		TS      = integer_to_list(T),
		TT      = cre_timeNotation("19990729", TS),
		Ev      = cre_obsEvent("al/of", TT),
		EvsDesc = cre_obsEvsDesc(Rid+N, [Ev]),
		NR      = cre_notifyReq([Tid], EvsDesc),
		CMD     = cre_command(NR),
		cre_cmdReq(CMD)
	end,
    Ns = [0,1,2,3,4,5,6,7,8,9],
    CRs = [F(N) || N <- Ns],
    cre_actionReq(Cid, CRs).

mtrms2_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrms2_mg_notify_request_ar1(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


mtrms2_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_trans_req_and_ack(suite) ->
    [];
single_trans_req_and_ack(doc) ->
    [];
single_trans_req_and_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        single_trans_req_and_ack),
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
    MgcEvSeq = straa_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = straa_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(straa_mgc_verify_handle_connect_fun(), 
        {?MODULE, straa_mgc_verify_handle_connect, []}).
-define(straa_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, straa_mgc_verify_service_change_req, [Mid]}).
-define(straa_mgc_verify_notify_req_fun(),
        {?MODULE, straa_mgc_verify_notify_request, []}).
-define(straa_mgc_verify_ack_fun(),
        {?MODULE, straa_mgc_verify_ack, []}).
-define(straa_mgc_verify_handle_disconnect_fun(),
        {?MODULE, straa_mgc_verify_handle_disconnect, []}).
-else.
-define(straa_mgc_verify_handle_connect_fun(), 
        fun straa_mgc_verify_handle_connect/1).
-define(straa_mgc_verify_service_change_req_fun(Mid),
        straa_mgc_verify_service_change_req_fun(Mid)).
-define(straa_mgc_verify_notify_req_fun(),
	straa_mgc_verify_notify_request_fun()).
-define(straa_mgc_verify_ack_fun(),
	fun straa_mgc_verify_ack/1).
-define(straa_mgc_verify_handle_disconnect_fun(),
	fun straa_mgc_verify_handle_disconnect/1).
-endif.

straa_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?straa_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?straa_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?straa_mgc_verify_notify_req_fun(),
    AckVerify              = ?straa_mgc_verify_ack_fun(), 
    DiscoVerify            = ?straa_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun straa_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = straa_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = straa_mgc_verify_notify_request_fun(),
%%     AckVerify              = fun straa_mgc_verify_ack/1, 
%%     DiscoVerify            = fun straa_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


straa_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("straa_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
straa_mgc_verify_handle_connect(Else) ->
    io:format("straa_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straa_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    straa_mgc_verify_service_change_req(Ev, Mid)
    end.

straa_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("straa_mgc_verify_service_change_req -> ok"
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
						 [straa_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = straa_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = straa_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = straa_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = straa_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = straa_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
straa_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("straa_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = straa_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

straa_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    straa_mgc_verify_notify_request(Ev)
    end.

straa_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("straa_mgc_verify_notify_request:fun -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, kalle},
	    Reply = {HandleAck, 
		     [straa_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, 
		     [straa_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = straa_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
straa_mgc_verify_notify_request(Else) ->
    io:format("straa_mgc_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = straa_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

straa_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, kalle}) -> 
    io:format("straa_mgc_verify_ack -> ok"
	      "~n   CH: ~p"
	      "~n", [CH]),
    {ok, CH, ok};
straa_mgc_verify_ack(Else) ->
    io:format("straa_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straa_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("straa_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
straa_mgc_verify_handle_disconnect(Else) ->
    io:format("straa_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


straa_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

straa_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = straa_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

straa_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

straa_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = straa_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(straa_mg_verify_handle_connect_fun(), 
        {?MODULE, straa_mg_verify_handle_connect, []}).
-define(straa_mg_verify_service_change_reply_fun(),
        {?MODULE, straa_mg_verify_service_change_reply, []}).
-define(straa_mg_verify_notify_reply_fun(),
        {?MODULE, straa_mg_verify_notify_reply, []}).
-else.
-define(straa_mg_verify_handle_connect_fun(), 
        fun straa_mg_verify_handle_connect/1).
-define(straa_mg_verify_service_change_reply_fun(),
        fun straa_mg_verify_service_change_reply/1).
-define(straa_mg_verify_notify_reply_fun(),
	fun straa_mg_verify_notify_reply/1).
-endif.

straa_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [straa_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(N) ->
		 [straa_mg_notify_request_ar(N, Tid, N)]
	 end,
    ConnectVerify            = ?straa_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?straa_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?straa_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun straa_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun straa_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun straa_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        1000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {sleep, 1000},
	     {megaco_cast, NR(1), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_cast, NR(2), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

straa_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("straa_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
straa_mg_verify_handle_connect(Else) ->
    io:format("straa_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straa_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
				    {ok, [AR]}, _}) ->
    io:format("straa_mg_verify_service_change_reply -> ok"
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
straa_mg_verify_service_change_reply(Else) ->
    io:format("straa_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straa_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			      {ok, [AR]}, _}) ->
    io:format("straa_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
straa_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			      {error, Err}, _}) ->
    io:format("straa_mg_verify_notify_reply -> error"
	      "~n   Err: ~p~n", [Err]),
    {error, Err, ok};
straa_mg_verify_notify_reply(Else) ->
    io:format("straa_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straa_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

straa_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = straa_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

straa_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

straa_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = straa_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the single_trans_req test case
%%

straa_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_ack_timeout(suite) ->
    [];
multi_trans_req_and_ack_timeout(doc) ->
    [];
multi_trans_req_and_ack_timeout(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_and_ack_timeout),
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
    MgcEvSeq = mtrtaat_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrtaat_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrtaat_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrtaat_mgc_verify_handle_connect, []}).
-define(mtrtaat_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrtaat_mgc_verify_service_change_req, [Mid]}).
-define(mtrtaat_mgc_verify_notify_req_fun(),
        {?MODULE, mtrtaat_mgc_verify_notify_request, []}).
-define(mtrtaat_mgc_verify_ack_fun(),
        {?MODULE, mtrtaat_mgc_verify_ack, []}).
-define(mtrtaat_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrtaat_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrtaat_mgc_verify_handle_connect_fun(), 
        fun mtrtaat_mgc_verify_handle_connect/1).
-define(mtrtaat_mgc_verify_service_change_req_fun(Mid),
        mtrtaat_mgc_verify_service_change_req_fun(Mid)).
-define(mtrtaat_mgc_verify_notify_req_fun(),
	mtrtaat_mgc_verify_notify_request_fun()).
-define(mtrtaat_mgc_verify_ack_fun(),
	fun mtrtaat_mgc_verify_ack/1).
-define(mtrtaat_mgc_verify_handle_disconnect_fun(),
	fun mtrtaat_mgc_verify_handle_disconnect/1).
-endif.

mtrtaat_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrtaat_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrtaat_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrtaat_mgc_verify_notify_req_fun(),
    AckVerify              = ?mtrtaat_mgc_verify_ack_fun(), 
    DiscoVerify            = ?mtrtaat_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrtaat_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrtaat_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrtaat_mgc_verify_notify_request_fun(),
%%     AckVerify              = fun mtrtaat_mgc_verify_ack/1, 
%%     DiscoVerify            = fun mtrtaat_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrtaat_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaat_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaat_mgc_verify_handle_connect(Else) ->
    io:format("mtrtaat_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaat_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrtaat_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrtaat_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrtaat_mgc_verify_service_change_req -> ok"
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
						 [mtrtaat_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrtaat_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrtaat_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrtaat_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrtaat_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrtaat_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrtaat_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrtaat_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaat_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaat_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrtaat_mgc_verify_notify_request(Ev)
    end.

mtrtaat_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrtaat_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, kalle},
	    Reply = {HandleAck, 
		     [mtrtaat_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtrtaat_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrtaat_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrtaat_mgc_verify_notify_request(Else) ->
    io:format("mtrtaat_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaat_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.


mtrtaat_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, kalle}) -> 
    io:format("mtrtaat_mgc_verify_ack -> ok"
	      "~n   CH: ~p"
	      "~n", [CH]),
    {ok, CH, ok};
mtrtaat_mgc_verify_ack(Else) ->
    io:format("mtrtaat_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaat_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrtaat_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrtaat_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrtaat_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrtaat_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrtaat_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrtaat_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaat_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrtaat_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrtaat_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrtaat_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrtaat_mg_verify_handle_connect, []}).
-define(mtrtaat_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrtaat_mg_verify_service_change_reply, []}).
-define(mtrtaat_mg_verify_notify_reply_fun(),
        {?MODULE, mtrtaat_mg_verify_notify_reply, []}).
-else.
-define(mtrtaat_mg_verify_handle_connect_fun(), 
        fun mtrtaat_mg_verify_handle_connect/1).
-define(mtrtaat_mg_verify_service_change_reply_fun(),
        fun mtrtaat_mg_verify_service_change_reply/1).
-define(mtrtaat_mg_verify_notify_reply_fun(),
	fun mtrtaat_mg_verify_notify_reply/1).
-endif.

mtrtaat_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrtaat_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtrtaat_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?mtrtaat_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrtaat_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrtaat_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrtaat_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrtaat_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrtaat_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        1000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrtaat_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaat_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaat_mg_verify_handle_connect(Else) ->
    io:format("mtrtaat_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaat_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					{ok, [AR]}, _}) ->
    io:format("mtrtaat_mg_verify_service_change_reply -> ok"
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
mtrtaat_mg_verify_service_change_reply(Else) ->
    io:format("mtrtaat_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaat_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("mtrtaat_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtrtaat_mg_verify_notify_reply(Else) ->
    io:format("mtrtaat_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaat_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaat_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrtaat_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaat_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaat_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrtaat_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrtaat_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_ack_ackmaxcount(suite) ->
    [];
multi_trans_req_and_ack_ackmaxcount(doc) ->
    [];
multi_trans_req_and_ack_ackmaxcount(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_and_ack_ackmaxcount),
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
    MgcEvSeq = mtrtaaamc_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrtaaamc_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrtaaamc_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrtaaamc_mgc_verify_handle_connect, []}).
-define(mtrtaaamc_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrtaaamc_mgc_verify_service_change_req, [Mid]}).
-define(mtrtaaamc_mgc_verify_notify_req_fun(),
        {?MODULE, mtrtaaamc_mgc_verify_notify_request, []}).
-define(mtrtaaamc_mgc_verify_ack_fun(),
        {?MODULE, mtrtaaamc_mgc_verify_ack, []}).
-define(mtrtaaamc_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrtaaamc_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrtaaamc_mgc_verify_handle_connect_fun(), 
        fun mtrtaaamc_mgc_verify_handle_connect/1).
-define(mtrtaaamc_mgc_verify_service_change_req_fun(Mid),
        mtrtaaamc_mgc_verify_service_change_req_fun(Mid)).
-define(mtrtaaamc_mgc_verify_notify_req_fun(),
	mtrtaaamc_mgc_verify_notify_request_fun()).
-define(mtrtaaamc_mgc_verify_ack_fun(),
	fun mtrtaaamc_mgc_verify_ack/1).
-define(mtrtaaamc_mgc_verify_handle_disconnect_fun(),
	fun mtrtaaamc_mgc_verify_handle_disconnect/1).
-endif.

mtrtaaamc_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrtaaamc_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrtaaamc_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrtaaamc_mgc_verify_notify_req_fun(),
    AckVerify              = ?mtrtaaamc_mgc_verify_ack_fun(), 
    DiscoVerify            = ?mtrtaaamc_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrtaaamc_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrtaaamc_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrtaaamc_mgc_verify_notify_request_fun(),
%%     AckVerify              = fun mtrtaaamc_mgc_verify_ack/1, 
%%     DiscoVerify            = fun mtrtaaamc_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrtaaamc_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaaamc_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaaamc_mgc_verify_handle_connect(Else) ->
    io:format("mtrtaaamc_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaaamc_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrtaaamc_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrtaaamc_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrtaaamc_mgc_verify_service_change_req -> ok"
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
						 [mtrtaaamc_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrtaaamc_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrtaaamc_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrtaaamc_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrtaaamc_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrtaaamc_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrtaaamc_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrtaaamc_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaaamc_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaaamc_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrtaaamc_mgc_verify_notify_request(Ev)
    end.

mtrtaaamc_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrtaaamc_mgc_verify_notify_request:fun -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{requestId = Rid, 
					observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, {kalle, Rid}},
	    Reply = {HandleAck, 
		     [mtrtaaamc_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtrtaaamc_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrtaaamc_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrtaaamc_mgc_verify_notify_request(Else) ->
    io:format("mtrtaaamc_mgc_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaaamc_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaaamc_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, 
			  {kalle, Rid}}) -> 
    io:format("mtrtaaamc_mgc_verify_ack -> ok"
	      "~n   CH:  ~p"
	      "~n   Rid: ~p"
	      "~n", [CH, Rid]),
    {ok, CH, ok};
mtrtaaamc_mgc_verify_ack(Else) ->
    io:format("mtrtaaamc_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaaamc_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrtaaamc_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrtaaamc_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrtaaamc_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrtaaamc_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrtaaamc_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrtaaamc_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaaamc_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrtaaamc_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrtaaamc_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrtaaamc_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrtaaamc_mg_verify_handle_connect, []}).
-define(mtrtaaamc_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrtaaamc_mg_verify_service_change_reply, []}).
-define(mtrtaaamc_mg_verify_notify_reply_fun(),
        {?MODULE, mtrtaaamc_mg_verify_notify_reply, []}).
-else.
-define(mtrtaaamc_mg_verify_handle_connect_fun(), 
        fun mtrtaaamc_mg_verify_handle_connect/1).
-define(mtrtaaamc_mg_verify_service_change_reply_fun(),
        fun mtrtaaamc_mg_verify_service_change_reply/1).
-define(mtrtaaamc_mg_verify_notify_reply_fun(),
	fun mtrtaaamc_mg_verify_notify_reply/1).
-endif.

mtrtaaamc_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrtaaamc_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtrtaaamc_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?mtrtaaamc_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrtaaamc_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrtaaamc_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrtaaamc_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrtaaamc_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrtaaamc_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 4},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        5000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_cast, NR(1,4), [{trans_req,false}]},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrtaaamc_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaaamc_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaaamc_mg_verify_handle_connect(Else) ->
    io:format("mtrtaaamc_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaaamc_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					  {ok, [AR]}, _}) ->
    io:format("mtrtaaamc_mg_verify_service_change_reply -> ok"
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
mtrtaaamc_mg_verify_service_change_reply(Else) ->
    io:format("mtrtaaamc_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaaamc_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				  {ok, [AR]}, _}) ->
    io:format("mtrtaaamc_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtrtaaamc_mg_verify_notify_reply(Else) ->
    io:format("mtrtaaamc_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaaamc_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaaamc_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrtaaamc_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaaamc_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaaamc_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrtaaamc_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrtaaamc_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_ack_reqmaxcount(suite) ->
    [];
multi_trans_req_and_ack_reqmaxcount(doc) ->
    [];
multi_trans_req_and_ack_reqmaxcount(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_and_ack_reqmaxcount),
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
    MgcEvSeq = mtrtaarac_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
   {ok, MgcId} =  megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrtaarac_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrtaarac_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrtaarac_mgc_verify_handle_connect, []}).
-define(mtrtaarac_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrtaarac_mgc_verify_service_change_req, [Mid]}).
-define(mtrtaarac_mgc_verify_notify_req_fun(),
        {?MODULE, mtrtaarac_mgc_verify_notify_request, []}).
-define(mtrtaarac_mgc_verify_ack_fun(),
        {?MODULE, mtrtaarac_mgc_verify_ack, []}).
-define(mtrtaarac_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrtaarac_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrtaarac_mgc_verify_handle_connect_fun(), 
        fun mtrtaarac_mgc_verify_handle_connect/1).
-define(mtrtaarac_mgc_verify_service_change_req_fun(Mid),
        mtrtaarac_mgc_verify_service_change_req_fun(Mid)).
-define(mtrtaarac_mgc_verify_notify_req_fun(),
	mtrtaarac_mgc_verify_notify_request_fun()).
-define(mtrtaarac_mgc_verify_ack_fun(),
	fun mtrtaarac_mgc_verify_ack/1).
-define(mtrtaarac_mgc_verify_handle_disconnect_fun(),
	fun mtrtaarac_mgc_verify_handle_disconnect/1).
-endif.

mtrtaarac_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrtaarac_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrtaarac_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrtaarac_mgc_verify_notify_req_fun(),
    AckVerify              = ?mtrtaarac_mgc_verify_ack_fun(), 
    DiscoVerify            = ?mtrtaarac_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrtaarac_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrtaarac_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrtaarac_mgc_verify_notify_request_fun(),
%%     AckVerify              = fun mtrtaarac_mgc_verify_ack/1, 
%%     DiscoVerify            = fun mtrtaarac_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrtaarac_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaarac_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaarac_mgc_verify_handle_connect(Else) ->
    io:format("mtrtaarac_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaarac_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrtaarac_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrtaarac_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrtaarac_mgc_verify_service_change_req -> ok"
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
						 [mtrtaarac_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrtaarac_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrtaarac_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrtaarac_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrtaarac_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrtaarac_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrtaarac_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrtaarac_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaarac_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaarac_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrtaarac_mgc_verify_notify_request(Ev)
    end.

mtrtaarac_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrtaarac_mgc_verify_notify_request:fun -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{requestId = Rid, 
					observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, {kalle, Rid}},
	    Reply = {HandleAck, 
		     [mtrtaarac_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtrtaarac_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrtaarac_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrtaarac_mgc_verify_notify_request(Else) ->
    io:format("mtrtaarac_mgc_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaarac_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaarac_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, 
			  {kalle, Rid}}) -> 
    io:format("mtrtaarac_mgc_verify_ack -> ok"
	      "~n   CH:  ~p"
	      "~n   Rid: ~p"
	      "~n", [CH, Rid]),
    {ok, CH, ok};
mtrtaarac_mgc_verify_ack(Else) ->
    io:format("mtrtaarac_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaarac_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrtaarac_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrtaarac_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrtaarac_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrtaarac_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrtaarac_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrtaarac_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaarac_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrtaarac_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrtaarac_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrtaarac_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrtaarac_mg_verify_handle_connect, []}).
-define(mtrtaarac_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrtaarac_mg_verify_service_change_reply, []}).
-define(mtrtaarac_mg_verify_notify_reply_fun(),
        {?MODULE, mtrtaarac_mg_verify_notify_reply, []}).
-else.
-define(mtrtaarac_mg_verify_handle_connect_fun(), 
        fun mtrtaarac_mg_verify_handle_connect/1).
-define(mtrtaarac_mg_verify_service_change_reply_fun(),
        fun mtrtaarac_mg_verify_service_change_reply/1).
-define(mtrtaarac_mg_verify_notify_reply_fun(),
	fun mtrtaarac_mg_verify_notify_reply/1).
-endif.

mtrtaarac_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrtaarac_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtrtaarac_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?mtrtaarac_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrtaarac_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrtaarac_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrtaarac_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrtaarac_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrtaarac_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 4},
	     {megaco_update_conn_info, trans_timer,        5000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_cast, NR(2,4), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrtaarac_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaarac_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaarac_mg_verify_handle_connect(Else) ->
    io:format("mtrtaarac_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaarac_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					  {ok, [AR]}, _}) ->
    io:format("mtrtaarac_mg_verify_service_change_reply -> ok"
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
mtrtaarac_mg_verify_service_change_reply(Else) ->
    io:format("mtrtaarac_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaarac_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				  {ok, [AR]}, _}) ->
    io:format("mtrtaarac_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtrtaarac_mg_verify_notify_reply(Else) ->
    io:format("mtrtaarac_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaarac_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaarac_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrtaarac_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaarac_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaarac_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrtaarac_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrtaarac_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_ack_maxsize1(suite) ->
    [];
multi_trans_req_and_ack_maxsize1(doc) ->
    [];
multi_trans_req_and_ack_maxsize1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_and_ack_maxsize1),
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
    MgcEvSeq = mtrtaams1_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrtaams1_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrtaams1_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrtaams1_mgc_verify_handle_connect, []}).
-define(mtrtaams1_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrtaams1_mgc_verify_service_change_req, [Mid]}).
-define(mtrtaams1_mgc_verify_notify_req_fun(),
        {?MODULE, mtrtaams1_mgc_verify_notify_request, []}).
-define(mtrtaams1_mgc_verify_ack_fun(),
        {?MODULE, mtrtaams1_mgc_verify_ack, []}).
-define(mtrtaams1_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrtaams1_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrtaams1_mgc_verify_handle_connect_fun(), 
        fun mtrtaams1_mgc_verify_handle_connect/1).
-define(mtrtaams1_mgc_verify_service_change_req_fun(Mid),
        mtrtaams1_mgc_verify_service_change_req_fun(Mid)).
-define(mtrtaams1_mgc_verify_notify_req_fun(),
	mtrtaams1_mgc_verify_notify_request_fun()).
-define(mtrtaams1_mgc_verify_ack_fun(),
	fun mtrtaams1_mgc_verify_ack/1).
-define(mtrtaams1_mgc_verify_handle_disconnect_fun(),
	fun mtrtaams1_mgc_verify_handle_disconnect/1).
-endif.

mtrtaams1_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrtaams1_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrtaams1_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrtaams1_mgc_verify_notify_req_fun(),
    AckVerify              = ?mtrtaams1_mgc_verify_ack_fun(), 
    DiscoVerify            = ?mtrtaams1_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrtaams1_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrtaams1_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrtaams1_mgc_verify_notify_request_fun(),
%%     AckVerify              = fun mtrtaams1_mgc_verify_ack/1, 
%%     DiscoVerify            = fun mtrtaams1_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrtaams1_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaams1_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaams1_mgc_verify_handle_connect(Else) ->
    io:format("mtrtaams1_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaams1_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrtaams1_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrtaams1_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtrtaams1_mgc_verify_service_change_req -> ok"
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
						 [mtrtaams1_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrtaams1_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrtaams1_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrtaams1_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrtaams1_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrtaams1_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrtaams1_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtrtaams1_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaams1_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaams1_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrtaams1_mgc_verify_notify_request(Ev)
    end.

mtrtaams1_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtrtaams1_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{requestId = Rid, 
					observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, {kalle, Rid}},
	    Reply = {HandleAck, 
		     [mtrtaams1_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtrtaams1_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrtaams1_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrtaams1_mgc_verify_notify_request(Else) ->
    io:format("mtrtaams1_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtrtaams1_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaams1_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, 
			  {kalle, Rid}}) -> 
    io:format("mtrtaams1_mgc_verify_ack -> ok"
	      "~n   CH:  ~p"
	      "~n   Rid: ~p"
	      "~n", [CH, Rid]),
    {ok, CH, ok};
mtrtaams1_mgc_verify_ack(Else) ->
    io:format("mtrtaams1_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaams1_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtrtaams1_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtrtaams1_mgc_verify_handle_disconnect(Else) ->
    io:format("mtrtaams1_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtrtaams1_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrtaams1_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrtaams1_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaams1_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrtaams1_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrtaams1_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrtaams1_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrtaams1_mg_verify_handle_connect, []}).
-define(mtrtaams1_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrtaams1_mg_verify_service_change_reply, []}).
-define(mtrtaams1_mg_verify_notify_reply_fun(),
        {?MODULE, mtrtaams1_mg_verify_notify_reply, []}).
-else.
-define(mtrtaams1_mg_verify_handle_connect_fun(), 
        fun mtrtaams1_mg_verify_handle_connect/1).
-define(mtrtaams1_mg_verify_service_change_reply_fun(),
        fun mtrtaams1_mg_verify_service_change_reply/1).
-define(mtrtaams1_mg_verify_notify_reply_fun(),
	fun mtrtaams1_mg_verify_notify_reply/1).
-endif.

mtrtaams1_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrtaams1_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtrtaams1_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?mtrtaams1_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrtaams1_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrtaams1_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrtaams1_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrtaams1_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrtaams1_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxsize,  650},
	     {megaco_update_conn_info, trans_timer,        5000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, [NR(1,2), NR(1,3), NR(1,4)], []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_cast, NR(2,4), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrtaams1_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtrtaams1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtrtaams1_mg_verify_handle_connect(Else) ->
    io:format("mtrtaams1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaams1_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					  {ok, [AR]}, _}) ->
    io:format("mtrtaams1_mg_verify_service_change_reply -> ok"
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
mtrtaams1_mg_verify_service_change_reply(Else) ->
    io:format("mtrtaams1_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaams1_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				  {ok, [AR]}, _}) ->
    io:format("mtrtaams1_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtrtaams1_mg_verify_notify_reply(Else) ->
    io:format("mtrtaams1_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtrtaams1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaams1_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrtaams1_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaams1_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaams1_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrtaams1_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrtaams1_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_ack_maxsize2(suite) ->
    [];
multi_trans_req_and_ack_maxsize2(doc) ->
    [];
multi_trans_req_and_ack_maxsize2(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_and_ack_maxsize2),
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
    MgcEvSeq = mtrtaams2_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtrtaams2_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtrtaams2_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtrtaams2_mgc_verify_handle_connect, []}).
-define(mtrtaams2_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtrtaams2_mgc_verify_service_change_req, [Mid]}).
-define(mtrtaams2_mgc_verify_notify_req_fun(),
        {?MODULE, mtrtaams2_mgc_verify_notify_request, []}).
-define(mtrtaams2_mgc_verify_ack_fun(),
        {?MODULE, mtrtaams2_mgc_verify_ack, []}).
-define(mtrtaams2_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtrtaams2_mgc_verify_handle_disconnect, []}).
-else.
-define(mtrtaams2_mgc_verify_handle_connect_fun(), 
        fun mtrtaams2_mgc_verify_handle_connect/1).
-define(mtrtaams2_mgc_verify_service_change_req_fun(Mid),
        mtrtaams2_mgc_verify_service_change_req_fun(Mid)).
-define(mtrtaams2_mgc_verify_notify_req_fun(),
	mtrtaams2_mgc_verify_notify_request_fun()).
-define(mtrtaams2_mgc_verify_ack_fun(),
	fun mtrtaams2_mgc_verify_ack/1).
-define(mtrtaams2_mgc_verify_handle_disconnect_fun(),
	fun mtrtaams2_mgc_verify_handle_disconnect/1).
-endif.

mtrtaams2_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify          = ?mtrtaams2_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtrtaams2_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtrtaams2_mgc_verify_notify_req_fun(),
    AckVerify              = ?mtrtaams2_mgc_verify_ack_fun(), 
    DiscoVerify            = ?mtrtaams2_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtrtaams2_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtrtaams2_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtrtaams2_mgc_verify_notify_request_fun(),
%%     AckVerify              = fun mtrtaams2_mgc_verify_ack/1, 
%%     DiscoVerify            = fun mtrtaams2_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     {listen, [{serialize, true}]},
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtrtaams2_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    p("mtrtaams2_mgc_verify_handle_connect -> ok"
      "~n   CH: ~p", [CH]),
    {ok, CH, ok};
mtrtaams2_mgc_verify_handle_connect(Else) ->
    p("mtrtaams2_mgc_verify_handle_connect -> unknown"
      "~n   Else: ~p", [Else]),
    {error, Else, ok}.

mtrtaams2_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtrtaams2_mgc_verify_service_change_req(Ev, Mid)
    end.

mtrtaams2_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    p("mtrtaams2_mgc_verify_service_change_req -> ok"
      "~n   AR: ~p", [AR]),
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
						 [mtrtaams2_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtrtaams2_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtrtaams2_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtrtaams2_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtrtaams2_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtrtaams2_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtrtaams2_mgc_verify_service_change_req(Else, _Min) ->
    p("mtrtaams2_mgc_verify_service_change_req -> unknown"
      "~n   Else: ~p", [Else]),
    ED = mtrtaams2_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaams2_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtrtaams2_mgc_verify_notify_request(Ev)
    end.

mtrtaams2_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    p("mtrtaams2_mgc_verify_notify_request:fun -> ok"
      "~n   AR: ~p", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    p("mtrtaams2_mgc_verify_notify_request:fun -> "
	      "single command", []),		    
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{requestId = Rid, 
					observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, {kalle, Rid}},
	    Reply = {HandleAck, 
		     [mtrtaams2_mgc_notify_reply_ar1(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = CRs} ->
	    p("mtrtaams2_mgc_verify_notify_request:fun -> "
	      "multi command (~w)", [length(CRs)]),		    
	    Tids = [Tid || 
		       #'CommandRequest'{command = 
					 {notifyReq, 
					  #'NotifyRequest'{
					    terminationID = [Tid]}}} 
			   <- CRs],
	    Reply = 
		{discard_ack, 
		 [mtrtaams2_mgc_notify_reply_ar2(Cid, Tids)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtrtaams2_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtrtaams2_mgc_verify_notify_request(Else) ->
    p("mtrtaams2_mgc_verify_notify_request:fun -> unknown"
      "~n   Else: ~p", [Else]),
    ED = mtrtaams2_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtrtaams2_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, 
			  {kalle, Rid}}) -> 
    p("mtrtaams2_mgc_verify_ack -> ok"
      "~n   CH:  ~p"
      "~n   Rid: ~p", [CH, Rid]),
    {ok, CH, ok};
mtrtaams2_mgc_verify_ack(Else) ->
    p("mtrtaams2_mgc_verify_ack -> Else: "
      "~n~p", [Else]),
    {error, Else, ok}.

mtrtaams2_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    p("mtrtaams2_mgc_verify_handle_disconnect -> ok"
      "~n   CH: ~p"
      "~n   R:  ~p", [CH, R]),
    {ok, CH, ok};
mtrtaams2_mgc_verify_handle_disconnect(Else) ->
    p("mtrtaams2_mgc_verify_handle_disconnect -> unknown"
      "~n   Else: ~p", [Else]),
    {error, Else, ok}.


mtrtaams2_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtrtaams2_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtrtaams2_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaams2_mgc_notify_reply_ar1(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtrtaams2_mgc_notify_reply_ar2(Cid, Tids) ->
    CRs = [cre_cmdReply(cre_notifyReply([Tid])) || Tid <- Tids],
    cre_actionReply(Cid, CRs).

mtrtaams2_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtrtaams2_mgc_notify_reply_ar1(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtrtaams2_mg_verify_handle_connect_fun(), 
        {?MODULE, mtrtaams2_mg_verify_handle_connect, []}).
-define(mtrtaams2_mg_verify_service_change_reply_fun(),
        {?MODULE, mtrtaams2_mg_verify_service_change_reply, []}).
-define(mtrtaams2_mg_verify_notify_reply_fun(),
        {?MODULE, mtrtaams2_mg_verify_notify_reply, []}).
-else.
-define(mtrtaams2_mg_verify_handle_connect_fun(), 
        fun mtrtaams2_mg_verify_handle_connect/1).
-define(mtrtaams2_mg_verify_service_change_reply_fun(),
        fun mtrtaams2_mg_verify_service_change_reply/1).
-define(mtrtaams2_mg_verify_notify_reply_fun(),
	fun mtrtaams2_mg_verify_notify_reply/1).
-endif.

mtrtaams2_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtrtaams2_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR1 = fun(Cid, Rid) ->
		  [mtrtaams2_mg_notify_request_ar1(10 + Rid, Tid, Cid)]
	  end,
    NR2 = fun(Cid, Rid) ->
		  [mtrtaams2_mg_notify_request_ar2(20 + Rid, Tid, Cid)]
	  end,
    ConnectVerify            = ?mtrtaams2_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtrtaams2_mg_verify_service_change_reply_fun(), 
    NotifyReplyVerify        = ?mtrtaams2_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtrtaams2_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtrtaams2_mg_verify_service_change_reply/1,
%%     NotifyReplyVerify        = fun mtrtaams2_mg_verify_notify_reply/1, 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     {megaco_trace, disable},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     {connect, [{serialize, true}]},
	     {megaco_callback, handle_connect, ConnectVerify},
	     megaco_connect,
	     {megaco_cast, ServiceChangeReq, []},
	     {megaco_callback, handle_connect, ConnectVerify}, 
	     {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {sleep, 1000},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     {sleep, 1000},
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxsize,  1024},
	     {megaco_update_conn_info, trans_timer,        5000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, [NR1(1,2), NR1(1,3), NR1(1,4)], []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_cast, [NR1(2,5), NR1(2,6), NR1(2,7)], []},
	     {megaco_cast, NR2(2,1), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtrtaams2_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    p("mtrtaams2_mg_verify_handle_connect -> ok"
      "~n   CH: ~p", [CH]),
    {ok, CH, ok};
mtrtaams2_mg_verify_handle_connect(Else) ->
    p("mtrtaams2_mg_verify_handle_connect -> unknown"
      "~n   Else: ~p", [Else]),
    {error, Else, ok}.

mtrtaams2_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					  {ok, [AR]}, _}) ->
    p("mtrtaams2_mg_verify_service_change_reply -> ok"
      "~n   AR: ~p", [AR]),
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
mtrtaams2_mg_verify_service_change_reply(Else) ->
    p("mtrtaams2_mg_verify_service_change_reply -> unknown"
      "~n   Else: ~p", [Else]),
    {error, Else, ok}.

mtrtaams2_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				  {ok, [AR]}, _}) ->
    p("mtrtaams2_mg_verify_notify_reply -> ok"
      "~n   AR: ~p", [AR]),
    {ok, AR, ok};
mtrtaams2_mg_verify_notify_reply(Else) ->
    p("mtrtaams2_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p", [Else]),
    {error, Else, ok}.

mtrtaams2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaams2_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtrtaams2_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtrtaams2_mg_notify_request_ar1(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtrtaams2_mg_notify_request_ar2(Rid, Tid, Cid) ->
    F = fun(N) ->
		T       = 22000000 + N,
		TS      = integer_to_list(T),
		TT      = cre_timeNotation("19990729", TS),
		Ev      = cre_obsEvent("al/of", TT),
		EvsDesc = cre_obsEvsDesc(Rid+N, [Ev]),
		NR      = cre_notifyReq([Tid], EvsDesc),
		CMD     = cre_command(NR),
		cre_cmdReq(CMD)
	end,
    Ns = [0,1,2,3,4,5,6,7,8,9],
    CRs = [F(N) || N <- Ns],
    cre_actionReq(Cid, CRs).

mtrtaams2_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = mtrtaams2_mg_notify_request_ar1(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtrtaams2_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_trans_req_and_pending(suite) ->
    [];
single_trans_req_and_pending(doc) ->
    [];
single_trans_req_and_pending(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_pending(suite) ->
    [];
multi_trans_req_and_pending(doc) ->
    [];
multi_trans_req_and_pending(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_ack_and_pending(suite) ->
    [];
multi_trans_req_and_ack_and_pending(doc) ->
    [];
multi_trans_req_and_ack_and_pending(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        mtraaap),
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
    MgcEvSeq = mtraaap_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtraaap_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtraaap_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtraaap_mgc_verify_handle_connect, []}).
-define(mtraaap_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtraaap_mgc_verify_service_change_req, [Mid]}).
-define(mtraaap_mgc_verify_notify_req_fun(),
        {?MODULE, mtraaap_mgc_verify_notify_request, []}).
-define(mtraaap_mgc_verify_notify_reply_fun(),
        {?MODULE, mtraaap_mgc_verify_notify_reply, []}).
-define(mtraaap_mgc_verify_ack_fun(),
        {?MODULE, mtraaap_mgc_verify_ack, []}).
-define(mtraaap_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtraaap_mgc_verify_handle_disconnect, []}).
-else.
-define(mtraaap_mgc_verify_handle_connect_fun(), 
        fun mtraaap_mgc_verify_handle_connect/1).
-define(mtraaap_mgc_verify_service_change_req_fun(Mid),
        mtraaap_mgc_verify_service_change_req_fun(Mid)).
-define(mtraaap_mgc_verify_notify_req_fun(),
	mtraaap_mgc_verify_notify_request_fun()).
-define(mtraaap_mgc_verify_notify_reply_fun(),
	fun mtraaap_mgc_verify_notify_reply/1).
-define(mtraaap_mgc_verify_ack_fun(),
	fun mtraaap_mgc_verify_ack/1).
-define(mtraaap_mgc_verify_handle_disconnect_fun(),
	fun mtraaap_mgc_verify_handle_disconnect/1).
-endif.

mtraaap_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtraaap_mgc_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ReqTmr = #megaco_incr_timer{wait_for    = 500,
				factor      = 1,
				max_retries = 1},
    ConnectVerify          = ?mtraaap_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtraaap_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtraaap_mgc_verify_notify_req_fun(),
    NotifyReplyVerify      = ?mtraaap_mgc_verify_notify_reply_fun(), 
    AckVerify              = ?mtraaap_mgc_verify_ack_fun(), 
    DiscoVerify            = ?mtraaap_mgc_verify_handle_disconnect_fun(),
%%     ConnectVerify          = fun mtraaap_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtraaap_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtraaap_mgc_verify_notify_request_fun(),
%%     NotifyReplyVerify      = fun mtraaap_mgc_verify_notify_reply/1, 
%%     AckVerify              = fun mtraaap_mgc_verify_ack/1, 
%%     DiscoVerify            = fun mtraaap_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,        ConnectVerify},
	     {megaco_callback, handle_trans_request,  ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request,  NotifyReqVerify},
	     {megaco_callback, handle_trans_request,  NotifyReqVerify},
	     {megaco_callback, handle_trans_request,  NotifyReqVerify},
	     {megaco_update_conn_info, request_timer, ReqTmr},
	     {megaco_cast, NR(1,1), []},
	     {megaco_callback, [{handle_trans_ack,     3, AckVerify},
				{handle_trans_request, 3, NotifyReqVerify},
				{handle_trans_reply,   1, NotifyReplyVerify}]},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtraaap_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtraaap_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtraaap_mgc_verify_handle_connect(Else) ->
    io:format("mtraaap_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaap_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtraaap_mgc_verify_service_change_req(Ev, Mid)
    end.

mtraaap_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtraaap_mgc_verify_service_change_req -> ok"
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
						 [mtraaap_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtraaap_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtraaap_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtraaap_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtraaap_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtraaap_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtraaap_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtraaap_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtraaap_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtraaap_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtraaap_mgc_verify_notify_request(Ev)
    end.

mtraaap_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtraaap_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, kalle},
	    Reply = {HandleAck, 
		     [mtraaap_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtraaap_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtraaap_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtraaap_mgc_verify_notify_request(Else) ->
    io:format("mtraaap_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtraaap_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtraaap_mgc_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				 {ok, [AR]}, _}) ->
    io:format("mtraaap_mgc_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtraaap_mgc_verify_notify_reply(Else) ->
    io:format("mtraaap_mgc_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaap_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, kalle}) -> 
    io:format("mtraaap_mgc_verify_ack -> ok"
	      "~n   CH: ~p"
	      "~n", [CH]),
    {ok, CH, ok};
mtraaap_mgc_verify_ack(Else) ->
    io:format("mtraaap_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaap_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtraaap_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtraaap_mgc_verify_handle_disconnect(Else) ->
    io:format("mtraaap_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtraaap_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtraaap_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtraaap_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtraaap_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "44000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtraaap_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtraaap_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtraaap_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtraaap_mg_verify_handle_connect_fun(), 
        {?MODULE, mtraaap_mg_verify_handle_connect, []}).
-define(mtraaap_mg_verify_service_change_reply_fun(),
        {?MODULE, mtraaap_mg_verify_service_change_reply, []}).
-define(mtraaap_mg_verify_notify_req_fun(),
        {?MODULE, mtraaap_mgc_verify_notify_request, []}).
-define(mtraaap_mg_verify_notify_reply_fun(),
        {?MODULE, mtraaap_mg_verify_notify_reply, []}).
-else.
-define(mtraaap_mg_verify_handle_connect_fun(), 
        fun mtraaap_mg_verify_handle_connect/1).
-define(mtraaap_mg_verify_service_change_reply_fun(),
        fun mtraaap_mg_verify_service_change_reply/1).
-define(mtraaap_mg_verify_notify_req_fun(),
	mtraaap_mgc_verify_notify_request_fun()).
-define(mtraaap_mg_verify_notify_reply_fun(),
	fun mtraaap_mg_verify_notify_reply/1).
-endif.

mtraaap_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtraaap_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtraaap_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?mtraaap_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtraaap_mg_verify_service_change_reply_fun(), 
    NotifyReqVerify          = ?mtraaap_mg_verify_notify_req_fun(),
    NotifyReplyVerify        = ?mtraaap_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtraaap_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtraaap_mg_verify_service_change_reply/1,
%%     NotifyReqVerify          = mtraaap_mg_verify_notify_request_fun(),
%%     NotifyReplyVerify        = fun mtraaap_mg_verify_notify_reply/1, 
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        1000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_update_conn_info, trans_timer,        120000},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtraaap_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtraaap_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtraaap_mg_verify_handle_connect(Else) ->
    io:format("mtraaap_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaap_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					{ok, [AR]}, _}) ->
    io:format("mtraaap_mg_verify_service_change_reply -> ok"
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
mtraaap_mg_verify_service_change_reply(Else) ->
    io:format("mtraaap_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaap_mg_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtraaap_mg_verify_notify_request(Ev)
    end.

mtraaap_mg_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtraaap_mg_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtraaap_mg_notify_reply_ar(Cid, Tid)]},
	    {ok, 3000, AR, Reply};
	_ ->
	    ED = mtraaap_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtraaap_mg_verify_notify_request(Else) ->
    io:format("mtraaap_mg_verify_notify_request:fun -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtraaap_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtraaap_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("mtraaap_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtraaap_mg_verify_notify_reply(Else) ->
    io:format("mtraaap_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaap_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtraaap_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtraaap_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtraaap_mg_notify_reply_ar(Cid, TermId) ->
    NR = cre_notifyReply([TermId]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtraaap_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtraaap_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR    = mtraaap_mg_notify_request_ar(Rid, TermId, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for this test case
%%

mtraaap_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_ack_and_pending(suite) ->
    [];
multi_ack_and_pending(doc) ->
    [];
multi_ack_and_pending(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_reply(suite) ->
    [];
multi_trans_req_and_reply(doc) ->
    [];
multi_trans_req_and_reply(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_trans_req_and_ack_and_reply(suite) ->
    [];
multi_trans_req_and_ack_and_reply(doc) ->
    [];
multi_trans_req_and_ack_and_reply(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_trans_req_and_ack_and_reply),
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
    MgcEvSeq = mtraaar_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = mtraaar_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

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
-define(mtraaar_mgc_verify_handle_connect_fun(), 
        {?MODULE, mtraaar_mgc_verify_handle_connect, []}).
-define(mtraaar_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, mtraaar_mgc_verify_service_change_req, [Mid]}).
-define(mtraaar_mgc_verify_notify_req_fun(),
        {?MODULE, mtraaar_mgc_verify_notify_request, []}).
-define(mtraaar_mgc_verify_notify_reply_fun(),
        {?MODULE, mtraaar_mgc_verify_notify_reply, []}).
-define(mtraaar_mgc_verify_ack_fun(),
        {?MODULE, mtraaar_mgc_verify_ack, []}).
-define(mtraaar_mgc_verify_handle_disconnect_fun(),
        {?MODULE, mtraaar_mgc_verify_handle_disconnect, []}).
-else.
-define(mtraaar_mgc_verify_handle_connect_fun(), 
        fun mtraaar_mgc_verify_handle_connect/1).
-define(mtraaar_mgc_verify_service_change_req_fun(Mid),
        mtraaar_mgc_verify_service_change_req_fun(Mid)).
-define(mtraaar_mgc_verify_notify_req_fun(),
	mtraaar_mgc_verify_notify_request_fun()).
-define(mtraaar_mgc_verify_notify_reply_fun(),
	fun mtraaar_mgc_verify_notify_reply/1).
-define(mtraaar_mgc_verify_ack_fun(),
	fun mtraaar_mgc_verify_ack/1).
-define(mtraaar_mgc_verify_handle_disconnect_fun(),
	fun mtraaar_mgc_verify_handle_disconnect/1).
-endif.

mtraaar_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtraaar_mgc_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify          = ?mtraaar_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?mtraaar_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?mtraaar_mgc_verify_notify_req_fun(),
    NotifyReplyVerify      = ?mtraaar_mgc_verify_notify_reply_fun(), 
    AckVerify              = ?mtraaar_mgc_verify_ack_fun(), 
    DiscoVerify            = ?mtraaar_mgc_verify_handle_disconnect_fun(), 
%%     ConnectVerify          = fun mtraaar_mgc_verify_handle_connect/1,
%%     ServiceChangeReqVerify = mtraaar_mgc_verify_service_change_req_fun(Mid),
%%     NotifyReqVerify        = mtraaar_mgc_verify_notify_request_fun(),
%%     NotifyReplyVerify      = fun mtraaar_mgc_verify_notify_reply/1, 
%%     AckVerify              = fun mtraaar_mgc_verify_ack/1, 
%%     DiscoVerify            = fun mtraaar_mgc_verify_handle_disconnect/1,
    EvSeq = [
	     {debug, true},
	     %% {megaco_trace, max}, 
	     {megaco_trace, disable}, 
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_update_conn_info, request_timer,      1000},
	     {megaco_cast, NR(1,1), []},

	     {megaco_callback, [{handle_trans_ack,     3, AckVerify},
				{handle_trans_request, 3, NotifyReqVerify},
				{handle_trans_reply,   1, NotifyReplyVerify}]},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


mtraaar_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtraaar_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtraaar_mgc_verify_handle_connect(Else) ->
    io:format("mtraaar_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaar_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    mtraaar_mgc_verify_service_change_req(Ev, Mid)
    end.

mtraaar_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("mtraaar_mgc_verify_service_change_req -> ok"
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
						 [mtraaar_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = mtraaar_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = mtraaar_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = mtraaar_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = mtraaar_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = mtraaar_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
mtraaar_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("mtraaar_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtraaar_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtraaar_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtraaar_mgc_verify_notify_request(Ev)
    end.

mtraaar_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtraaar_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, kalle},
	    Reply = {HandleAck, 
		     [mtraaar_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtraaar_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtraaar_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtraaar_mgc_verify_notify_request(Else) ->
    io:format("mtraaar_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtraaar_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtraaar_mgc_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				 {ok, [AR]}, _}) ->
    io:format("mtraaar_mgc_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtraaar_mgc_verify_notify_reply({handle_trans_reply, CH, ?VERSION, 
				 UnknownResult, _}) ->
    io:format("mtraaar_mgc_verify_notify_reply -> unknown result"
	      "~n   UnknownResult: ~p~n", [UnknownResult]),
    {error, {unknown_reply_result, UnknownResult, CH}, ok};
mtraaar_mgc_verify_notify_reply(Else) ->
    io:format("mtraaar_mgc_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, {unknown_reply, Else}, ok}.

mtraaar_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, kalle}) -> 
    io:format("mtraaar_mgc_verify_ack -> ok"
	      "~n   CH: ~p"
	      "~n", [CH]),
    {ok, CH, ok};
mtraaar_mgc_verify_ack(Else) ->
    io:format("mtraaar_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaar_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("mtraaar_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
mtraaar_mgc_verify_handle_disconnect(Else) ->
    io:format("mtraaar_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


mtraaar_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

mtraaar_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = mtraaar_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtraaar_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "44000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtraaar_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtraaar_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = mtraaar_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(mtraaar_mg_verify_handle_connect_fun(), 
        {?MODULE, mtraaar_mg_verify_handle_connect, []}).
-define(mtraaar_mg_verify_service_change_reply_fun(),
        {?MODULE, mtraaar_mg_verify_service_change_reply, []}).
-define(mtraaar_mg_verify_notify_req_fun(),
        {?MODULE, mtraaar_mgc_verify_notify_request, []}).
-define(mtraaar_mg_verify_notify_reply_fun(),
        {?MODULE, mtraaar_mg_verify_notify_reply, []}).
-else.
-define(mtraaar_mg_verify_handle_connect_fun(), 
        fun mtraaar_mg_verify_handle_connect/1).
-define(mtraaar_mg_verify_service_change_reply_fun(),
        fun mtraaar_mg_verify_service_change_reply/1).
-define(mtraaar_mg_verify_notify_req_fun(),
	mtraaar_mgc_verify_notify_request_fun()).
-define(mtraaar_mg_verify_notify_reply_fun(),
	fun mtraaar_mg_verify_notify_reply/1).
-endif.

mtraaar_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [mtraaar_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [mtraaar_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?mtraaar_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?mtraaar_mg_verify_service_change_reply_fun(), 
    NotifyReqVerify          = ?mtraaar_mg_verify_notify_req_fun(),
    NotifyReplyVerify        = ?mtraaar_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun mtraaar_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun mtraaar_mg_verify_service_change_reply/1,
%%     NotifyReqVerify          = mtraaar_mg_verify_notify_request_fun(),
%%     NotifyReplyVerify        = fun mtraaar_mg_verify_notify_reply/1, 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     %% {megaco_trace, max},
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        1000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_update_conn_info, trans_timer,        120000},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

mtraaar_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("mtraaar_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
mtraaar_mg_verify_handle_connect(Else) ->
    io:format("mtraaar_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaar_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					{ok, [AR]}, _}) ->
    io:format("mtraaar_mg_verify_service_change_reply -> ok"
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
mtraaar_mg_verify_service_change_reply(Else) ->
    io:format("mtraaar_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaar_mg_verify_notify_request_fun() ->
    fun(Ev) ->
	    mtraaar_mg_verify_notify_request(Ev)
    end.

mtraaar_mg_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("mtraaar_mg_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [mtraaar_mg_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = mtraaar_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
mtraaar_mg_verify_notify_request(Else) ->
    io:format("mtraaar_mg_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = mtraaar_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

mtraaar_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("mtraaar_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
mtraaar_mg_verify_notify_reply(Else) ->
    io:format("mtraaar_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

mtraaar_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtraaar_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = mtraaar_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

mtraaar_mg_notify_reply_ar(Cid, TermId) ->
    NR = cre_notifyReply([TermId]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

mtraaar_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

mtraaar_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR    = mtraaar_mg_notify_request_ar(Rid, TermId, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

mtraaar_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_ack_and_reply(suite) ->
    [];
multi_ack_and_reply(doc) ->
    [];
multi_ack_and_reply(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7192_1(suite) ->
    [];
otp_7192_1(doc) ->
    [""];
otp_7192_1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_7192_1),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),

    MgMid = {deviceName,"mg"},

    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),


    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp71921_mgc_event_sequence(text, tcp, MgMid),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = otp71921_mg_event_sequence(text, tcp, MgMid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator(s) completion"),
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
-define(otp71921_mgc_verify_handle_connect_fun(), 
        {?MODULE, otp71921_mgc_verify_handle_connect, []}).
-define(otp71921_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, otp71921_mgc_verify_service_change_req, [Mid]}).
-define(otp71921_mgc_verify_notify_req_fun(),
        {?MODULE, otp71921_mgc_verify_notify_request, []}).
-define(otp71921_mgc_verify_notify_reply_fun(),
        {?MODULE, otp71921_mgc_verify_notify_reply, []}).
-define(otp71921_mgc_verify_ack_fun(),
        {?MODULE, otp71921_mgc_verify_ack, []}).
-define(otp71921_mgc_verify_handle_disconnect_fun(),
        {?MODULE, otp71921_mgc_verify_handle_disconnect, []}).
-else.
-define(otp71921_mgc_verify_handle_connect_fun(), 
        fun otp71921_mgc_verify_handle_connect/1).
-define(otp71921_mgc_verify_service_change_req_fun(Mid),
        otp71921_mgc_verify_service_change_req_fun(Mid)).
-define(otp71921_mgc_verify_notify_req_fun(),
	otp71921_mgc_verify_notify_request_fun()).
-define(otp71921_mgc_verify_notify_reply_fun(),
	fun otp71921_mgc_verify_notify_reply/1).
-define(otp71921_mgc_verify_ack_fun(),
	fun otp71921_mgc_verify_ack/1).
-define(otp71921_mgc_verify_handle_disconnect_fun(),
	fun otp71921_mgc_verify_handle_disconnect/1).
-endif.

otp71921_mgc_event_sequence(text, tcp, MgMid) ->
    Mid = {deviceName, "ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [otp71921_mgc_notify_request_ar(Rid, Tid, Cid)]
	 end,
    LocalConnectVerify     = ?otp71921_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?otp71921_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?otp71921_mgc_verify_notify_req_fun(),
    NotifyReplyVerify      = ?otp71921_mgc_verify_notify_reply_fun(), 
    AckVerify              = ?otp71921_mgc_verify_ack_fun(), 
    DiscoVerify            = ?otp71921_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
	     {debug, true},
	     %% {megaco_trace, max}, 
	     {megaco_trace, disable}, 
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_connect,  MgMid}, 
	     {megaco_callback, handle_connect,       LocalConnectVerify},
	     %% {megaco_callback, handle_connect,       RemoteConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_update_conn_info, request_timer,      1000},
	     {megaco_cast, NR(1,1), []},

	     {megaco_callback, [{handle_trans_ack,     3, AckVerify},
				{handle_trans_request, 3, NotifyReqVerify},
				{handle_trans_reply,   1, NotifyReplyVerify}]},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


otp71921_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp71921_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp71921_mgc_verify_handle_connect(Else) ->
    io:format("otp71921_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71921_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    otp71921_mgc_verify_service_change_req(Ev, Mid)
    end.

otp71921_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("otp71921_mgc_verify_service_change_req -> ok"
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
						 [otp71921_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = otp71921_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = otp71921_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = otp71921_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = otp71921_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = otp71921_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
otp71921_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("otp71921_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp71921_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp71921_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    otp71921_mgc_verify_notify_request(Ev)
    end.

otp71921_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("otp71921_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, kalle},
	    Reply = {HandleAck, 
		     [otp71921_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [otp71921_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp71921_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp71921_mgc_verify_notify_request(Else) ->
    io:format("otp71921_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp71921_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp71921_mgc_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				 {ok, [AR]}, _}) ->
    io:format("otp71921_mgc_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp71921_mgc_verify_notify_reply({handle_trans_reply, CH, ?VERSION, 
				 UnknownResult, _}) ->
    io:format("otp71921_mgc_verify_notify_reply -> unknown result"
	      "~n   UnknownResult: ~p~n", [UnknownResult]),
    {error, {unknown_reply_result, UnknownResult, CH}, ok};
otp71921_mgc_verify_notify_reply(Else) ->
    io:format("otp71921_mgc_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, {unknown_reply, Else}, ok}.

otp71921_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, kalle}) -> 
    io:format("otp71921_mgc_verify_ack -> ok"
	      "~n   CH: ~p"
	      "~n", [CH]),
    {ok, CH, ok};
otp71921_mgc_verify_ack(Else) ->
    io:format("otp71921_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71921_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("otp71921_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
otp71921_mgc_verify_handle_disconnect(Else) ->
    io:format("otp71921_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp71921_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

otp71921_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = otp71921_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp71921_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "44000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp71921_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp71921_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = otp71921_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp71921_mg_verify_handle_connect_fun(), 
        {?MODULE, otp71921_mg_verify_handle_connect, []}).
-define(otp71921_mg_verify_service_change_reply_fun(),
        {?MODULE, otp71921_mg_verify_service_change_reply, []}).
-define(otp71921_mg_verify_notify_req_fun(),
        {?MODULE, otp71921_mgc_verify_notify_request, []}).
-define(otp71921_mg_verify_notify_reply_fun(),
        {?MODULE, otp71921_mg_verify_notify_reply, []}).
-else.
-define(otp71921_mg_verify_handle_connect_fun(), 
        fun otp71921_mg_verify_handle_connect/1).
-define(otp71921_mg_verify_service_change_reply_fun(),
        fun otp71921_mg_verify_service_change_reply/1).
-define(otp71921_mg_verify_notify_req_fun(),
	otp71921_mgc_verify_notify_request_fun()).
-define(otp71921_mg_verify_notify_reply_fun(),
	fun otp71921_mg_verify_notify_reply/1).
-endif.

otp71921_mg_event_sequence(text, tcp, Mid) ->
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [otp71921_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [otp71921_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?otp71921_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?otp71921_mg_verify_service_change_reply_fun(), 
    NotifyReqVerify          = ?otp71921_mg_verify_notify_req_fun(),
    NotifyReplyVerify        = ?otp71921_mg_verify_notify_reply_fun(), 
%%     ConnectVerify            = fun otp71921_mg_verify_handle_connect/1,
%%     ServiceChangeReplyVerify = fun otp71921_mg_verify_service_change_reply/1,
%%     NotifyReqVerify          = otp71921_mg_verify_notify_request_fun(),
%%     NotifyReplyVerify        = fun otp71921_mg_verify_notify_reply/1, 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     %% {megaco_trace, max},
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        1000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_update_conn_info, trans_timer,        120000},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

otp71921_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp71921_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp71921_mg_verify_handle_connect(Else) ->
    io:format("otp71921_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71921_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					{ok, [AR]}, _}) ->
    io:format("otp71921_mg_verify_service_change_reply -> ok"
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
otp71921_mg_verify_service_change_reply(Else) ->
    io:format("otp71921_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71921_mg_verify_notify_request_fun() ->
    fun(Ev) ->
	    otp71921_mg_verify_notify_request(Ev)
    end.

otp71921_mg_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("otp71921_mg_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [otp71921_mg_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp71921_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp71921_mg_verify_notify_request(Else) ->
    io:format("otp71921_mg_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp71921_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp71921_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("otp71921_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp71921_mg_verify_notify_reply(Else) ->
    io:format("otp71921_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71921_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp71921_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp71921_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp71921_mg_notify_reply_ar(Cid, TermId) ->
    NR = cre_notifyReply([TermId]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp71921_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp71921_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR    = otp71921_mg_notify_request_ar(Rid, TermId, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

otp71921_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7192_2(suite) ->
    [];
otp_7192_2(doc) ->
    [];
otp_7192_2(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_7192_2),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),

    MgMid = {deviceName,"mg"},

    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),


    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp71922_mgc_event_sequence(text, tcp, MgMid),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = otp71922_mg_event_sequence(text, tcp, MgMid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator(s) completion"),
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
-define(otp71922_mgc_verify_handle_connect_fun(), 
        {?MODULE, otp71922_mgc_verify_handle_connect, []}).
-define(otp71922_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, otp71922_mgc_verify_service_change_req, [Mid]}).
-define(otp71922_mgc_verify_notify_req_fun(),
        {?MODULE, otp71922_mgc_verify_notify_request, []}).
-define(otp71922_mgc_verify_notify_reply_fun(),
        {?MODULE, otp71922_mgc_verify_notify_reply, []}).
-define(otp71922_mgc_verify_ack_fun(),
        {?MODULE, otp71922_mgc_verify_ack, []}).
-define(otp71922_mgc_verify_handle_disconnect_fun(),
        {?MODULE, otp71922_mgc_verify_handle_disconnect, []}).
-else.
-define(otp71922_mgc_verify_handle_connect_fun(), 
        fun otp71922_mgc_verify_handle_connect/1).
-define(otp71922_mgc_verify_service_change_req_fun(Mid),
        otp71922_mgc_verify_service_change_req_fun(Mid)).
-define(otp71922_mgc_verify_notify_req_fun(),
	otp71922_mgc_verify_notify_request_fun()).
-define(otp71922_mgc_verify_notify_reply_fun(),
	fun otp71922_mgc_verify_notify_reply/1).
-define(otp71922_mgc_verify_ack_fun(),
	fun otp71922_mgc_verify_ack/1).
-define(otp71922_mgc_verify_handle_disconnect_fun(),
	fun otp71922_mgc_verify_handle_disconnect/1).
-endif.

otp71922_mgc_event_sequence(text, tcp, MgMid) ->
    Mid = {deviceName, "ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [otp71922_mgc_notify_request_ar(Rid, Tid, Cid)]
	 end,
    LocalConnectVerify     = ?otp71922_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?otp71922_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?otp71922_mgc_verify_notify_req_fun(),
    NotifyReplyVerify      = ?otp71922_mgc_verify_notify_reply_fun(), 
    AckVerify              = ?otp71922_mgc_verify_ack_fun(), 
    DiscoVerify            = ?otp71922_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
	     {debug, true},
	     %% {megaco_trace, max}, 
	     {megaco_trace, disable}, 
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_connect,  MgMid}, 
	     {megaco_callback, handle_connect,       LocalConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_update_conn_info, request_timer,      1000},
	     {megaco_cast, NR(1,1), []},

	     {megaco_callback, [{handle_trans_ack,     3, AckVerify},
				{handle_trans_request, 3, NotifyReqVerify},
				{handle_trans_reply,   1, NotifyReplyVerify}]},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


otp71922_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp71922_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, timer:seconds(2), CH, ok};
otp71922_mgc_verify_handle_connect(Else) ->
    io:format("otp71922_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71922_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    otp71922_mgc_verify_service_change_req(Ev, Mid)
    end.

otp71922_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("otp71922_mgc_verify_service_change_req -> ok"
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
						 [otp71922_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = otp71922_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = otp71922_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = otp71922_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = otp71922_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = otp71922_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
otp71922_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("otp71922_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp71922_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp71922_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    otp71922_mgc_verify_notify_request(Ev)
    end.

otp71922_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("otp71922_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, kalle},
	    Reply = {HandleAck, 
		     [otp71922_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [otp71922_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp71922_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp71922_mgc_verify_notify_request(Else) ->
    io:format("otp71922_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp71922_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp71922_mgc_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				 {ok, [AR]}, _}) ->
    io:format("otp71922_mgc_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp71922_mgc_verify_notify_reply({handle_trans_reply, CH, ?VERSION, 
				 UnknownResult, _}) ->
    io:format("otp71922_mgc_verify_notify_reply -> unknown result"
	      "~n   UnknownResult: ~p~n", [UnknownResult]),
    {error, {unknown_reply_result, UnknownResult, CH}, ok};
otp71922_mgc_verify_notify_reply(Else) ->
    io:format("otp71922_mgc_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, {unknown_reply, Else}, ok}.

otp71922_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, kalle}) -> 
    io:format("otp71922_mgc_verify_ack -> ok"
	      "~n   CH: ~p"
	      "~n", [CH]),
    {ok, CH, ok};
otp71922_mgc_verify_ack(Else) ->
    io:format("otp71922_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71922_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("otp71922_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
otp71922_mgc_verify_handle_disconnect(Else) ->
    io:format("otp71922_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


otp71922_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

otp71922_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = otp71922_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp71922_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "44000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp71922_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp71922_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = otp71922_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp71922_mg_verify_handle_connect_fun(), 
        {?MODULE, otp71922_mg_verify_handle_connect, []}).
-define(otp71922_mg_verify_service_change_reply_fun(),
        {?MODULE, otp71922_mg_verify_service_change_reply, []}).
-define(otp71922_mg_verify_notify_req_fun(),
        {?MODULE, otp71922_mgc_verify_notify_request, []}).
-define(otp71922_mg_verify_notify_reply_fun(),
        {?MODULE, otp71922_mg_verify_notify_reply, []}).
-else.
-define(otp71922_mg_verify_handle_connect_fun(), 
        fun otp71922_mg_verify_handle_connect/1).
-define(otp71922_mg_verify_service_change_reply_fun(),
        fun otp71922_mg_verify_service_change_reply/1).
-define(otp71922_mg_verify_notify_req_fun(),
	otp71922_mgc_verify_notify_request_fun()).
-define(otp71922_mg_verify_notify_reply_fun(),
	fun otp71922_mg_verify_notify_reply/1).
-endif.

otp71922_mg_event_sequence(text, tcp, Mid) ->
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [otp71922_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [otp71922_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?otp71922_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?otp71922_mg_verify_service_change_reply_fun(), 
    NotifyReqVerify          = ?otp71922_mg_verify_notify_req_fun(),
    NotifyReplyVerify        = ?otp71922_mg_verify_notify_reply_fun(), 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     %% {megaco_trace, max},
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        1000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_update_conn_info, trans_timer,        120000},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

otp71922_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp71922_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp71922_mg_verify_handle_connect(Else) ->
    io:format("otp71922_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71922_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					{ok, [AR]}, _}) ->
    io:format("otp71922_mg_verify_service_change_reply -> ok"
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
otp71922_mg_verify_service_change_reply(Else) ->
    io:format("otp71922_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71922_mg_verify_notify_request_fun() ->
    fun(Ev) ->
	    otp71922_mg_verify_notify_request(Ev)
    end.

otp71922_mg_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("otp71922_mg_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [otp71922_mg_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp71922_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp71922_mg_verify_notify_request(Else) ->
    io:format("otp71922_mg_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp71922_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp71922_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("otp71922_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp71922_mg_verify_notify_reply(Else) ->
    io:format("otp71922_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp71922_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp71922_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp71922_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp71922_mg_notify_reply_ar(Cid, TermId) ->
    NR = cre_notifyReply([TermId]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp71922_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp71922_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR    = otp71922_mg_notify_request_ar(Rid, TermId, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

otp71922_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7192_3(suite) ->
    [];
otp_7192_3(doc) ->
    ["Same as otp_7192_2 but transport is UDP instead of TCP"];
otp_7192_3(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_7192_3),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),

    MgMid = {deviceName,"mg"},

    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),


    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_megaco_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp72923_mgc_event_sequence(text, udp, MgMid),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_megaco_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = otp72923_mg_event_sequence(text, udp, MgMid),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator(s) completion"),
    await_completion([MgcId, MgId], 60000),

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
-define(otp72923_mgc_verify_handle_connect_fun(), 
        {?MODULE, otp72923_mgc_verify_handle_connect, []}).
-define(otp72923_mgc_verify_service_change_req_fun(Mid),
        {?MODULE, otp72923_mgc_verify_service_change_req, [Mid]}).
-define(otp72923_mgc_verify_notify_req_fun(),
        {?MODULE, otp72923_mgc_verify_notify_request, []}).
-define(otp72923_mgc_verify_notify_reply_fun(),
        {?MODULE, otp72923_mgc_verify_notify_reply, []}).
-define(otp72923_mgc_verify_ack_fun(),
        {?MODULE, otp72923_mgc_verify_ack, []}).
-define(otp72923_mgc_verify_handle_disconnect_fun(),
        {?MODULE, otp72923_mgc_verify_handle_disconnect, []}).
-else.
-define(otp72923_mgc_verify_handle_connect_fun(), 
        fun otp72923_mgc_verify_handle_connect/1).
-define(otp72923_mgc_verify_service_change_req_fun(Mid),
        otp72923_mgc_verify_service_change_req_fun(Mid)).
-define(otp72923_mgc_verify_notify_req_fun(),
	otp72923_mgc_verify_notify_request_fun()).
-define(otp72923_mgc_verify_notify_reply_fun(),
	fun otp72923_mgc_verify_notify_reply/1).
-define(otp72923_mgc_verify_ack_fun(),
	fun otp72923_mgc_verify_ack/1).
-define(otp72923_mgc_verify_handle_disconnect_fun(),
	fun otp72923_mgc_verify_handle_disconnect/1).
-endif.

otp72923_mgc_event_sequence(text, udp, MgMid) ->
    Mid = {deviceName, "ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_udp}
	 ],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [otp72923_mgc_notify_request_ar(Rid, Tid, Cid)]
	 end,
    LocalConnectVerify     = ?otp72923_mgc_verify_handle_connect_fun(), 
    ServiceChangeReqVerify = ?otp72923_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify        = ?otp72923_mgc_verify_notify_req_fun(),
    NotifyReplyVerify      = ?otp72923_mgc_verify_notify_reply_fun(), 
    AckVerify              = ?otp72923_mgc_verify_ack_fun(), 
    %% DiscoVerify            = ?otp72923_mgc_verify_handle_disconnect_fun(), 
    EvSeq = [
	     {debug, true},
	     {megaco_trace, max}, 
	     %% {megaco_trace, disable}, 
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_connect,  MgMid}, 
	     {megaco_callback, handle_connect,       LocalConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_update_conn_info, request_timer,      1000},
	     {megaco_cast, NR(1,1), []},

	     {megaco_callback, [{handle_trans_ack,     3, AckVerify},
				{handle_trans_request, 3, NotifyReqVerify},
				{handle_trans_reply,   1, NotifyReplyVerify}]},
	     %% {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


otp72923_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp72923_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, timer:seconds(2), CH, ok};
otp72923_mgc_verify_handle_connect(Else) ->
    io:format("otp72923_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp72923_mgc_verify_service_change_req_fun(Mid) ->
    fun(Ev) ->
	    otp72923_mgc_verify_service_change_req(Ev, Mid)
    end.

otp72923_mgc_verify_service_change_req(
  {handle_trans_request, _, ?VERSION, [AR]}, Mid) ->
    io:format("otp72923_mgc_verify_service_change_req -> ok"
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
						 [otp72923_mgc_service_change_reply_ar(Mid, 1)]},
					    {ok, AR, Reply};
					_ ->
					    Err = {invalid_SCP, Parms},
					    ED = otp72923_err_desc(Parms),
					    ErrReply = {discard_ack, 
							ED},
					    {error, Err, ErrReply}
				    end;
				_ ->
				    Err = {invalid_termination_id, Tid},
				    ED = otp72923_err_desc(Tid),
				    ErrReply = {discard_ack, ED},
				    {error, Err, ErrReply}
			    end;
			_ ->
			    Err = {invalid_command, Cmd},
			    ED = otp72923_err_desc(Cmd),
			    ErrReply = {discard_ack, ED},
			    {error, Err, ErrReply}
		    end;
		_ ->
		    Err = {invalid_command_request, CR},
		    ED = otp72923_err_desc(CR),
		    ErrReply = {discard_ack, ED},
		    {error, Err, ErrReply}
	    end;
	_ ->
	    Err = {invalid_action_request, AR},
	    ED = otp72923_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, Err, ErrReply}
    end;
otp72923_mgc_verify_service_change_req(Else, _Mid) ->
    io:format("otp72923_mgc_verify_service_change_req -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp72923_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp72923_mgc_verify_notify_request_fun() ->
    fun(Ev) ->
	    otp72923_mgc_verify_notify_request(Ev)
    end.

otp72923_mgc_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("otp72923_mgc_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    HandleAck = {handle_sloppy_ack, kalle},
	    Reply = {HandleAck, 
		     [otp72923_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	#'ActionRequest'{contextId = 2 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [otp72923_mgc_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp72923_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp72923_mgc_verify_notify_request(Else) ->
    io:format("otp72923_mgc_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp72923_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp72923_mgc_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				 {ok, [AR]}, _}) ->
    io:format("otp72923_mgc_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp72923_mgc_verify_notify_reply({handle_trans_reply, CH, ?VERSION, 
				 UnknownResult, _}) ->
    io:format("otp72923_mgc_verify_notify_reply -> unknown result"
	      "~n   UnknownResult: ~p~n", [UnknownResult]),
    {error, {unknown_reply_result, UnknownResult, CH}, ok};
otp72923_mgc_verify_notify_reply(Else) ->
    io:format("otp72923_mgc_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, {unknown_reply, Else}, ok}.

otp72923_mgc_verify_ack({handle_trans_ack, CH, ?VERSION, ok, kalle}) -> 
    io:format("otp72923_mgc_verify_ack -> ok"
	      "~n   CH: ~p"
	      "~n", [CH]),
    {ok, CH, ok};
otp72923_mgc_verify_ack(Else) ->
    io:format("otp72923_mgc_verify_ack -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

%% otp72923_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
%%     io:format("otp72923_mgc_verify_handle_disconnect -> ok"
%% 	      "~n   CH: ~p"
%% 	      "~n   R:  ~p"
%% 	      "~n", [CH, R]),
%%     {ok, CH, ok};
%% otp72923_mgc_verify_handle_disconnect(Else) ->
%%     io:format("otp72923_mgc_verify_handle_disconnect -> unknown"
%% 	      "~n   Else: ~p~n", [Else]),
%%     {error, Else, ok}.


otp72923_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

otp72923_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = otp72923_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp72923_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "44000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp72923_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp72923_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = otp72923_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
-ifdef(megaco_hipe_special).
-define(otp72923_mg_verify_handle_connect_fun(), 
        {?MODULE, otp72923_mg_verify_handle_connect, []}).
-define(otp72923_mg_verify_service_change_reply_fun(),
        {?MODULE, otp72923_mg_verify_service_change_reply, []}).
-define(otp72923_mg_verify_notify_req_fun(),
        {?MODULE, otp72923_mgc_verify_notify_request, []}).
-define(otp72923_mg_verify_notify_reply_fun(),
        {?MODULE, otp72923_mg_verify_notify_reply, []}).
-else.
-define(otp72923_mg_verify_handle_connect_fun(), 
        fun otp72923_mg_verify_handle_connect/1).
-define(otp72923_mg_verify_service_change_reply_fun(),
        fun otp72923_mg_verify_service_change_reply/1).
-define(otp72923_mg_verify_notify_req_fun(),
	otp72923_mgc_verify_notify_request_fun()).
-define(otp72923_mg_verify_notify_reply_fun(),
	fun otp72923_mg_verify_notify_reply/1).
-endif.

otp72923_mg_event_sequence(text, udp, Mid) ->
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_udp}
	 ],
    ServiceChangeReq = [otp72923_mg_service_change_request_ar(Mid, 1)],
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [otp72923_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    ConnectVerify            = ?otp72923_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = ?otp72923_mg_verify_service_change_reply_fun(), 
    NotifyReqVerify          = ?otp72923_mg_verify_notify_req_fun(),
    NotifyReplyVerify        = ?otp72923_mg_verify_notify_reply_fun(), 
    EvSeq = [
	     %% {debug, true},
	     {debug, false},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     %% {megaco_trace, max},
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
	     {megaco_update_conn_info, auto_ack,           true},
	     {megaco_update_conn_info, trans_ack_maxcount, 10},
	     {megaco_update_conn_info, trans_req_maxcount, 10},
	     {megaco_update_conn_info, trans_timer,        1000},
	     {megaco_update_conn_info, trans_ack,          true},
	     {megaco_update_conn_info, trans_req,          true},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_cast, NR(1,2), []},
	     {megaco_cast, NR(1,3), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_update_conn_info, trans_timer,        120000},
	     {megaco_cast, NR(2,1), []},
	     {megaco_cast, NR(2,2), []},
	     {megaco_cast, NR(2,3), []},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

otp72923_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("otp72923_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
otp72923_mg_verify_handle_connect(Else) ->
    io:format("otp72923_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp72923_mg_verify_service_change_reply({handle_trans_reply, _CH, ?VERSION, 
					{ok, [AR]}, _}) ->
    io:format("otp72923_mg_verify_service_change_reply -> ok"
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
otp72923_mg_verify_service_change_reply(Else) ->
    io:format("otp72923_mg_verify_service_change_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp72923_mg_verify_notify_request_fun() ->
    fun(Ev) ->
	    otp72923_mg_verify_notify_request(Ev)
    end.

otp72923_mg_verify_notify_request(
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("otp72923_mg_verify_notify_request -> ok"
	      "~n   AR: ~p~n", [AR]),
    case AR of
	#'ActionRequest'{contextId = 1 = Cid, 
			 commandRequests = [CR]} ->
	    #'CommandRequest'{command = Cmd} = CR,
	    {notifyReq, NR} = Cmd,
	    #'NotifyRequest'{terminationID = [Tid],
			     observedEventsDescriptor = OED,
			     errorDescriptor = asn1_NOVALUE} = NR,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
	    #'ObservedEvent'{eventName = "al/of"} = OE,
	    Reply = {discard_ack, [otp72923_mg_notify_reply_ar(Cid, Tid)]},
	    {ok, AR, Reply};
	_ ->
	    ED = otp72923_err_desc(AR),
	    ErrReply = {discard_ack, ED},
	    {error, AR, ErrReply}
    end;
otp72923_mg_verify_notify_request(Else) ->
    io:format("otp72923_mg_verify_notify_request -> unknown"
	      "~n   Else: ~p~n", [Else]),
    ED = otp72923_err_desc(Else),
    ErrReply = {discard_ack, ED},
    {error, Else, ErrReply}.

otp72923_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
				{ok, [AR]}, _}) ->
    io:format("otp72923_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
otp72923_mg_verify_notify_reply(Else) ->
    io:format("otp72923_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp72923_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp72923_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp72923_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp72923_mg_notify_reply_ar(Cid, TermId) ->
    NR = cre_notifyReply([TermId]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp72923_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp72923_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR    = otp72923_mg_notify_request_ar(Rid, TermId, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% Common functions for the multi_trans_req_timeout test case
%%

otp72923_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Common message creation functions
%%

cre_ErrDesc(T) ->
    cre_ErrDesc(?megaco_internal_gateway_error, T).

cre_ErrDesc(EC, T) ->
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.

cre_serviceChangeParm(M,R,P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M, 
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
cre_obsEvent(Name, Not, Par) ->
    #'ObservedEvent'{eventName    = Name, 
		     timeNotation = Not, 
		     eventParList = Par}.

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

%% --

cre_serviceChangeResParm(Mid) ->
    #'ServiceChangeResParm'{serviceChangeMgcId = Mid}.

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
    
cre_notifyReply(Tid) ->
    #'NotifyReply'{terminationID = Tid}.

cre_actionReply(CtxId, CmdRep) ->
    #'ActionReply'{contextId    = CtxId,
                   commandReply = CmdRep}.

cre_transResult(ED) when is_record(ED, 'ErrorDescriptor') ->
    {transactionError, ED};
cre_transResult([AR|_] = ARs) when is_record(AR, 'ActionReply') ->
    {actionReplies, ARs}.

cre_transReply(TransId, Res) ->
    #'TransactionReply'{transactionId     = TransId,
			transactionResult = Res}.


%% --

cre_serviceChangeProf(Name, Ver) when is_list(Name) andalso is_integer(Ver) ->
    #'ServiceChangeProfile'{profileName = Name, 
			    version     = Ver}.

cre_transaction(Trans) when is_record(Trans, 'TransactionRequest') ->
    {transactionRequest, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionPending') ->
    {transactionPending, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionReply') ->
    {transactionReply, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionAck') ->
    {transactionResponseAck, Trans}.

cre_transactions(Trans) when is_list(Trans) ->
    {transactions, Trans}.

cre_message(Version, Mid, Body) ->
    #'Message'{version     = Version,
	       mId         = Mid,
	       messageBody = Body}.

cre_megacoMessage(Mess) ->
    #'MegacoMessage'{mess = Mess}.
   
    
%%
%% Common functions
%%

encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            Mod:encode_message(Conf, M) 
    end.
encode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
            Mod:encode_message(Conf, Ver, M) 
    end.

decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            Mod:decode_message(Conf, M) 
    end.
decode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
            Mod:decode_message(Conf, Ver, M) 
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

await_ack(_User, 0, Timeout, _Expected) ->
    d("await_ack -> done when Timeout = ~p", [Timeout]),
    ok;
await_ack(User, N, Timeout, Expected) when (N > 0) andalso is_integer(Timeout) ->
    d("await_ack -> entry with N: ~p, Timeout: ~p", [N,Timeout]),
    T = tim(),
    receive
	{ack_received, User, Expected} ->
	    d("await_ack -> received another ack"),
	    await_ack(User, N-1, Timeout - (tim() - T), Expected);
	{ack_received, User, UnExpected} ->
	    d("await_ack -> unexpected ack result: ~p", [UnExpected]),
	    exit({unexpected_ack_result, UnExpected, Expected})
    after Timeout ->
	    exit({await_ack_timeout, N})
    end;
await_ack(User, N, infinity, Expected) when N > 0 ->
    d("await_ack -> entry with N: ~p", [N]),
    receive
	{ack_received, User, Expected} ->
	    d("await_ack -> received another ack"),
	    await_ack(User, N-1, infinity, Expected);
	{ack_received, User, UnExpected} ->
	    d("await_ack -> unexpected ack result: ~p", [UnExpected]),
	    exit({unexpected_ack_result, UnExpected, Expected})
    end.

await_req(_User, 0, Timeout) ->
    d("await_req -> done when Timeout = ~p", [Timeout]),
    ok;
await_req(User, N, Timeout) when (N > 0) andalso is_integer(Timeout) ->
    d("await_req -> entry with N: ~p, Timeout: ~p", [N,Timeout]),
    T = tim(),
    receive
	{req_received, User, ARs} ->
	    d("await_req -> received req(s) when N = ~w", [N]),
	    N1 = await_req1(N, ARs),
	    await_req(User, N1, Timeout - (tim() - T))
    after Timeout ->
	    exit({await_req_timeout, N})
    end;
await_req(User, N, infinity) when N > 0 ->
    d("await_req -> entry with N: ~p", [N]),
    receive
	{req_received, User, ARs} ->
	    d("await_req -> received req(s) when N = ~2",[N]),
	    N1 = await_req1(N, ARs),
	    await_req(User, N1, infinity)
    end.

await_req1(N, []) when N >= 0 ->
    N;
await_req1(N, [AR|ARs]) when (N > 0) andalso is_record(AR, 'ActionRequest') ->
    await_req1(N-1, ARs);
await_req1(N, ARs) ->
    exit({unexpected_req_result, N, ARs}).

% await_rep(_User, 0, Timeout) ->
%     d("await_rep -> done when Timeout = ~p", [Timeout]),
%     ok;
% await_rep(User, N, Timeout) when N > 0, integer(Timeout) ->
%     d("await_rep -> entry with N: ~p, Timeout: ~p", [N,Timeout]),
%     T = tim(),
%     receive
% 	{rep_received, User, ARs} ->
% 	    d("await_rep -> received rep(s)"),
% 	    N1 = await_rep1(N, ARs),
% 	    await_rep(User, N1, Timeout - (tim() - T))
%     after Timeout ->
% 	    exit({await_rep_timeout, N})
%     end;
% await_rep(User, N, infinity) when N > 0 ->
%     d("await_rep -> entry with N: ~p", [N]),
%     receive
% 	{rep_received, User, ARs} ->
% 	    d("await_rep -> received rep(s)"),
% 	    N1 = await_rep1(N, ARs),
% 	    await_rep(User, N1, infinity)
%     end.

% await_rep1(N, []) when N >= 0 ->
%     N;
% await_rep1(N, [AR|ARs]) when N > 0, record(AR, 'ActionReply') ->
%     await_rep1(N-1, ARs);
% await_rep1(N, ARs) ->
%     exit({unexpected_rep_result, N, ARs}).

tim() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).


make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
     end.


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

error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), now(), get(tc), "INF", F, A).


d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, get(verbosity), now(), get(tc), "DBG", F, A).


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, Ts, Tc, P, F, A) ->
    print(printable(Severity,Verbosity), Ts, Tc, P, F, A).

print(true, Ts, Tc, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s:~w ***"
	      "~n   " ++ F ++ "~n", 
	      [format_timestamp(Ts), P, self(), get(tc), Tc | A]);
print(_, _, _, _, _, _) ->
    ok.


p(F, A) ->
    io:format("*** [~s] ***"
	      "~n   " ++ F ++ "~n", 
	      [format_timestamp(now()) | A]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_init() ->
    {A,B,C} = now(),
    random:seed(A,B,C).

random() ->
    10 * random:uniform(50).

apply_load_timer() ->
    erlang:send_after(random(), self(), apply_load_timeout).


format_timestamp({_N1, _N2, N3}   = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
    lists:flatten(FormatDate).

