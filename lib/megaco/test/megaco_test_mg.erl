%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2019. All Rights Reserved.
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
%% Purpose: Implements an "MG" used by the test suite
%%----------------------------------------------------------------------
-module(megaco_test_mg).

-export([start/5, start/6, stop/1, 
	 get_stats/1, reset_stats/1, 
	 user_info/1, user_info/2, 
	 update_user_info/3, 
	 conn_info/1, conn_info/2, 
	 update_conn_info/3, 
	 service_change/1, 
	 ack_info/2, rep_info/2, 
	 group_requests/2, 
	 notify_request/1, 
	 await_notify_reply/1, 
	 notify_request_and_reply/1, 
	 cancel_request/2, 
	 apply_load/2, 
	 apply_multi_load/3, 
	 enable_test_code/4, 
	 encode_ar_first/2, 
	 verbosity/2]).

-export([mg/3, notify_request_handler_main/5]).
-export([loader_main/4]).

%% Megaco callback api
-export([
	 handle_connect/4,
	 handle_disconnect/5,
	 handle_syntax_error/5,
	 handle_message_error/5,
	 handle_trans_request/5,
	 handle_trans_long_request/5,
	 handle_trans_reply/6,
	 handle_trans_ack/6
	]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(A4444, tid(255*256*256)                 ).
-define(A4445, tid(255*256*256 + 255)           ).
-define(A5555, tid(255*256*256 + 255*256)       ).
-define(A5556, tid(255*256*256 + 255*256 + 255) ).

-record(mg, {mid             = undefined,
	     state           = initiated,
	     req_handler     = undefined,
	     call_mode       = async,
	     group_size      = 1,
	     encode_ar_first = false,
	     ack_info        = undefined,
	     rep_info        = undefined,
	     load_counter    = 0,
	     reply_counter   = 0,
	     mload_info      = undefined,
	     parent          = undefined,
	     dsi_timer,
             evs             = []}).

-define(EVS_MAX, 10).


%%% --------------------------------------------------------------------

start(Node, Mid, Encoding, Transport, Verbosity) ->
    %% Conf = [{megaco_trace, io}],
    %% Conf = [{megaco_trace, "megaco-mg.trace"}],
    Conf = [{megaco_trace, false}],
    start(Node, Mid, Encoding, Transport, Conf, Verbosity).

start(Node, Mid, Encoding, Transport, Conf, Verbosity) ->
    d("start mg[~p]: ~p"
      "~n   Encoding:  ~p"
      "~n   Transport: ~p"
      "~n   Conf:      ~p", [Node, Mid, Encoding, Transport, Conf]),
    RI1    = encoding_config(Encoding),
    RI2    = transport_config(Transport),
    {RI3, Conf1} = transport_opts(Conf), 
    RI     = {receive_info, RI1 ++ RI2 ++ RI3},
    Config = [{local_mid, Mid}, RI] ++ Conf1,
    Self   = self(),
    Fun    = 
	fun() ->
		io:format("LOADER(~p,~p) started~n", [self(),node()]),
		case (catch mg(Self, Verbosity, Config)) of
		    {'EXIT', Reason} ->
			io:format("LOADER(~p,~p) terminating with exit"
				  "~n~p"
				  "~n", [self(), node(), Reason]),
			exit(Reason);
		    Else ->
			io:format("LOADER(~p,~p) terminating with"
				  "~n~p"
				  "~n", [self(), node(), Else]),
			Else
		end
	end,
    true = erlang:monitor_node(Node, true),
    Pid = spawn_link(Node, Fun),
    %% Pid = spawn_link(Node, ?MODULE, mg, [self(), Verbosity, Config]),
    MonRef = (catch erlang:monitor(process, Pid)),
    NodePing = net_adm:ping(Node), 
    ProcInfo = (catch proc_info(Pid)), 
    i("start -> "
      "~n   self():           ~p"
      "~n   node():           ~p"
      "~n   net_adm:ping(~p): ~p" 
      "~n   Loader:           ~p"
      "~n   Monitor ref:      ~p"
      "~n   Process info:     ~p", 
      [self(), node(), 
       Node, NodePing, 
       Pid, 
       MonRef, ProcInfo]),
    await_started(Node, MonRef, Pid).

proc_info(Pid) ->
    rpc:call(node(Pid), erlang, process_info, [Pid]).

encoding_config({Encoding, EC}) when is_atom(Encoding) andalso is_list(EC) ->
    {Mod, Port} = select_encoding(Encoding),
    [{encoding_module, Mod},
     {encoding_config, EC},
     {port,            Port}];
encoding_config(Encoding) when is_atom(Encoding) ->
    {Mod, Port} = select_encoding(Encoding),
    [{encoding_module, Mod},
     {encoding_config, []},
     {port,            Port}];
encoding_config(Encoding) ->
    throw({error, {invalid_encoding, Encoding}}).

select_encoding(text) ->
    {megaco_pretty_text_encoder, 2944};
select_encoding(pretty_text) ->
    {megaco_pretty_text_encoder, 2944};
select_encoding(compact_text) ->
    {megaco_compact_text_encoder, 2944};
select_encoding(binary) ->
    {megaco_ber_encoder, 2945};
select_encoding(erl_dist) ->
    {megaco_erl_dist_encoder, 2946};
select_encoding(Encoding) ->
    throw({error, {invalid_encoding, Encoding}}).

transport_config(tcp) ->
    [{transport_module, megaco_tcp}];
transport_config(udp) ->
    [{transport_module, megaco_udp}];
transport_config(TransportConfig) when is_list(TransportConfig) ->
    {value, {transport, Trans}} = 
	lists:keysearch(transport, 1, TransportConfig),
    transport_config(Trans) ++ 
	case lists:keysearch(host, 1, TransportConfig) of
	    {value, Value} ->
		[Value];
	    false ->
		[]
	end.
    
transport_opts(Config) ->
    case lists:keysearch(transport_opts, 1, Config) of
	{value, TO} ->
	    Config1 = lists:keydelete(transport_opts, 1, Config),
	    {[TO], Config1};
	false ->
	    {[], Config}
    end.
									

await_started(Node, MonRef, Pid) ->
    i("await_started -> entry with"
      "~n   MonRef: ~p"
      "~n   Pid:    ~p", [MonRef, Pid]),
    receive
	{started, Pid} ->
	    d("await_started ~p - started"
	      "~n   Process info: ~p", [Pid, (catch proc_info(Pid))]),
	    true = erlang:monitor_node(Node, false),	    
	    erlang:demonitor(MonRef),
	    {ok, Pid};

	{nodedown, Node} ->
	    i("await_started ~p - received node down", [Pid]),
	    exit({node_down, Node}); 

	{'DOWN', MonRef, process, Pid, Info} ->
	    i("await_started ~p - received down signal: ~p", 
	      [Pid, Info]),
	    true = erlang:monitor_node(Node, false),	    
	    exit({failed_starting, Pid, Info});

	{'EXIT', Pid, Reason} ->
	    i("await_started ~p - received exit signal: ~p", [Pid, Reason]),
	    true = erlang:monitor_node(Node, false),	    
	    exit({failed_starting, Pid, Reason})

    %% This timeout was originally 10 secs, but on some debug compiled
    %% platforms, it was simply not long enough
    after 20000 ->
	    NodePing = net_adm:ping(Node), 
	    ProcInfo = (catch proc_info(Pid)),
	    FlushQ = megaco_test_lib:flush(), 
	    i("await_started ~p - timeout: "
	      "~n   net_adm:ping(~p):     ~p" 
	      "~n   Process info:         ~p"
	      "~n   Messages in my queue: ~p", 
	      [Pid, Node, NodePing, ProcInfo, FlushQ]),
	    true = erlang:monitor_node(Node, false),	    
	    exit({error, timeout})
    end.


verbosity(Pid, V) ->
    Pid ! {verbosity, V, self()}.


stop(Pid) ->
    server_request(Pid, stop, stopped).


get_stats(Pid) ->
    server_request(Pid, statistics, statistics_reply).

reset_stats(Pid) ->
    server_request(Pid, reset_stats, reset_stats_ack).


user_info(Pid) ->
    server_request(Pid, {user_info, all}, user_info_ack).

user_info(Pid, Tag) when is_atom(Tag) ->
    server_request(Pid, {user_info, Tag}, user_info_ack).


update_user_info(Pid, Tag, Val) ->
    server_request(Pid, {update_user_info, Tag, Val}, update_user_info_ack).


conn_info(Pid) ->
    server_request(Pid, {conn_info, all}, conn_info_ack).

conn_info(Pid, Tag) when is_atom(Tag) ->
    server_request(Pid, {conn_info, Tag}, conn_info_ack).


update_conn_info(Pid, Tag, Val) ->
    server_request(Pid, {update_conn_info, Tag, Val}, update_conn_info_ack).


enable_test_code(Pid, Module, Where, Fun) 
  when is_atom(Module) andalso is_atom(Where) andalso is_function(Fun) ->
    Tag = {Module, Where},
    server_request(Pid, {enable_test_code, Tag, Fun}, enable_test_code_reply).

encode_ar_first(Pid, New) when is_atom(New) ->
    server_request(Pid, {encode_ar_first, New}, encode_ar_first_reply).

service_change(Pid) ->
    server_request(Pid, service_change, service_change_reply).


group_requests(Pid, N) ->
    server_request(Pid, {group_requests, N}, group_requests_reply).


ack_info(Pid, InfoPid) ->
    Pid ! {{ack_info, InfoPid}, self()}.

rep_info(Pid, InfoPid) ->
    Pid ! {{rep_info, InfoPid}, self()}.


notify_request(Pid) ->
    Pid ! {notify_request, self()}.


await_notify_reply(Pid) ->
    await_reply(Pid, notify_request_reply).


notify_request_and_reply(Pid) ->
    notify_request(Pid),
    await_notify_reply(Pid).


cancel_request(Pid, Reason) ->
    server_request(Pid, cancel_request, Reason, cancel_request_reply).


apply_load(Pid, CounterStart) ->
    server_request(Pid, apply_load, CounterStart, apply_load_ack).


apply_multi_load(Pid, NumLoaders, NumReq) ->
    server_request(Pid, apply_multi_load, {NumLoaders, NumReq}, apply_multi_load_ack).


server_request(Pid, Req, ReplyTag) ->
    Pid ! {Req, self()},
    await_reply(Pid, ReplyTag).

server_request(Pid, Req, ReqData, ReplyTag) ->
    Pid ! {Req, ReqData, self()},
    await_reply(Pid, ReplyTag).

await_reply(Pid, ReplyTag) ->
    await_reply(Pid, ReplyTag, infinity).

await_reply(Pid, ReplyTag, Timeout) ->
    receive
	{ReplyTag, Reply, Pid} ->
	    Reply;
	{'EXIT', Pid, Reason} ->
	    exit({failed, ReplyTag, Pid, Reason})
    after Timeout ->
	    exit({timeout, ReplyTag, Pid})
    end.


server_reply(Pid, ReplyTag, Reply) ->
    Pid ! {ReplyTag, Reply, self()}.


%%% --------------------------------------------------------------------


mg(Parent, Verbosity, Config) ->
    process_flag(trap_exit, true),
    put(sname, "MG"),
    %% put(verbosity, Verbosity),
    put(verbosity, debug),  % Enable debug printouts during init
    i("mg -> starting"),
    %% megaco:enable_trace(max, io),
    case (catch init(Config)) of
	{error, _} = Error ->
	    exit(Error);
	{'EXIT', Reason} ->
	    exit({init_failed, Reason});
	{ok, Mid, DSITimer} ->
	    notify_started(Parent),
	    MG = #mg{parent = Parent, mid = Mid, dsi_timer = DSITimer},
	    i("mg -> started"),
	    put(verbosity, Verbosity),
	    case (catch loop(evs(MG, started))) of
		{'EXIT', normal} ->
		    exit(normal);
		{'EXIT', Reason} ->
		    i("mg failed with reason:~n   ~p", [Reason]),
		    exit(Reason);
		Else ->
		    i("mg terminated: ~n   ~p", [Else]),
		    exit({unexpected, Else})
	    end
    end.

init(Config) ->
    d("init -> entry with"
      "~n   Config: ~p", [Config]),
    random_init(),
    d("init -> random initiated", []),
    Mid = get_conf(local_mid, Config),
    d("init -> Mid: ~p", [Mid]),
    RI  = get_conf(receive_info, Config),
    d("init -> RI: ~p", [RI]),

    d("init -> maybe start the display system info timer"),
    DSITimer = 
	case get_conf(display_system_info, Config, undefined) of
	    Time when is_integer(Time) ->
		d("init -> creating display system info timer"),
		create_timer(Time, display_system_info);
	    _ ->
		undefined
	end,
    Conf0 = lists:keydelete(display_system_info, 1, Config),

    d("init -> start megaco"),
    application:start(megaco),


    d("init -> possibly enable megaco trace"),
    case lists:keysearch(megaco_trace, 1, Conf0) of
	{value, {megaco_trace, true}} ->
	    megaco:enable_trace(max, io);
	{value, {megaco_trace, io}} ->
	    megaco:enable_trace(max, io);
	{value, {megaco_trace, File}} when is_list(File) ->
	    megaco:enable_trace(max, File);
	_ ->
	    ok
    end,
    Conf1 = lists:keydelete(megaco_trace, 1, Conf0),
    
    d("init -> start megaco user"),
    Conf2 = lists:keydelete(local_mid,    1, Conf1),
    Conf3 = lists:keydelete(receive_info, 1, Conf2),
    ok = megaco:start_user(Mid, Conf3),
    d("init -> update user info (user_mod)"),
    ok = megaco:update_user_info(Mid, user_mod,  ?MODULE),
    d("init -> update user info (user_args)"),
    ok = megaco:update_user_info(Mid, user_args, [self(), Mid]),

    d("init -> get user info (receive_handle)"),
    RH = megaco:user_info(Mid, receive_handle),
    d("init -> parse receive info"),
    {MgcPort, MgcHost, RH1, TO} = parse_receive_info(RI, RH),
    d("init -> start transport (with ~p)", [TO]),
    {ok, _CH} = start_transport(MgcPort, MgcHost, RH1, TO),
    {ok, Mid, DSITimer}.


loop(#mg{parent = Parent, mid = Mid} = S) ->
    d("loop -> await request", []),
    receive
	{display_system_info, Time} ->
	    display_system_info(S#mg.mid),
	    NewTimer = create_timer(Time, display_system_info),
	    loop(evs(S#mg{dsi_timer = NewTimer}, {dsi, Time}));

	{verbosity, V, Parent} ->
	    i("loop -> received new verbosity: ~p", [V]),
	    put(verbosity,V),
	    loop(evs(S, {verb, V}));


	{stop, Parent} ->
	    i("loop -> stopping", []),
	    display_system_info(S#mg.mid, "at finish "),
	    cancel_timer(S#mg.dsi_timer),
	    Res = do_stop(Mid),
	    d("loop -> stop result: ~p", [Res]),
	    server_reply(Parent, stopped, {ok, Res}),
	    done(evs(S, stop), normal);

	{{enable_test_code, Tag, Fun}, Parent} ->
	    i("loop -> enable_test_code: ~p, ~p", [Tag, Fun]),
	    Reply = (catch ets:insert(megaco_test_data, {Tag, Fun})),
	    d("loop -> enable_test_code -> "
	      "~n   Reply:                          ~p"
	      "~n   ets:tab2list(megaco_test_data): ~p", 
	      [Reply,ets:tab2list(megaco_test_data)]),
	    server_reply(Parent, enable_test_code_reply, Reply),
	    loop(evs(S, {enable_test_code, Tag}));

	{{encode_ar_first, EAF}, Parent} ->
	    i("loop -> encode_ar_first: ~p", [EAF]),
	    {Reply, S1} = handle_encode_ar_first(S, EAF),
	    server_reply(Parent, encode_ar_first_reply, Reply),
	    loop(evs(S1#mg{encode_ar_first = EAF}, {enc_arf, EAF}));

	%% Give me statistics
	{statistics, Parent} ->
	    i("loop -> got request for statistics", []),
	    Stats = do_get_statistics(Mid),
	    server_reply(Parent, statistics_reply, {ok, Stats}),
	    loop(evs(S, stats)); 

	{reset_stats, Parent} ->
	    i("loop -> got request to reset stats counters", []),
	    do_reset_stats(Mid),
	    server_reply(Parent, reset_stats_ack, ok),
	    loop(evs(S, rst_stats));

	{{user_info, Tag}, Parent} ->
	    i("loop -> got user_info request for ~w", [Tag]),
	    Res = do_get_user_info(Mid, Tag),
	    d("loop -> Res: ~p", [Res]),
	    server_reply(Parent, user_info_ack, Res),
	    loop(evs(S, {ui, Tag}));

	{{update_user_info, Tag, Val}, Parent} ->
	    i("loop -> got update_user_info: ~w -> ~p", [Tag, Val]),
	    Res = do_update_user_info(Mid, Tag, Val),
	    d("loop -> Res: ~p", [Res]),
	    server_reply(Parent, update_user_info_ack, Res),
	    loop(evs(S, {uui, {Tag, Val}}));

	{{conn_info, Tag}, Parent} ->
	    i("loop -> got conn_info request for ~w", [Tag]),
	    Res = do_get_conn_info(Mid, Tag),
	    server_reply(Parent, conn_info_ack, Res),
	    loop(evs(S, {ci, Tag}));

	{{update_conn_info, Tag, Val}, Parent} ->
	    i("loop -> got update_conn_info: ~w -> ~p", [Tag, Val]),
	    Res = do_update_conn_info(Mid, Tag, Val),
	    server_reply(Parent, update_conn_info_ack, Res),
	    loop(evs(S, {uci, {Tag, Val}}));


	%% Do a service change
	%% No server-reply here. Since the service change is 
	%% async, the reply (from the MGC) will come later.
	{service_change, Parent} ->
	    i("loop -> received request to perform service change", []),
	    S1 = 
		case (catch do_service_change(S)) of
		    {ok, MG} ->
			d("loop -> service change initiated", []),
			MG;
		    Error ->
			d("loop -> service change failed: ~p", [Error]),
			server_reply(Parent, service_change_reply, Error),
			S
		end,
	    loop(evs(S1, svc_ch)); 

	{{group_requests, N}, Parent} when N > 0 ->
	    i("loop -> received group_requests ~p", [N]),
	    Reply = {ok, S#mg.group_size}, 
	    server_reply(Parent, group_requests_reply, Reply),
	    loop(evs(S#mg{group_size = N}, {grp_reqs, N})); 

	{{ack_info, To}, Parent} ->
	    i("loop -> received request to inform about received ack's ", []),
	    loop(evs(S#mg{ack_info = To}, {acki, To}));

	{{rep_info, To}, Parent} ->
	    i("loop -> received request to inform about received rep's ", []),
	    loop(evs(S#mg{rep_info = To}, {repi, To}));

	%% Make a sync-call
	{notify_request, Parent} ->
	    i("loop -> received request to send notify request ", []),
	    {Res, S1} = do_handle_notify_request(S),
	    d("loop -> notify request result: ~p", [Res]),
	    loop(evs(S1, not_req));

	%% sync-call complete
	{notify_request_complete, NotifyReply, Pid} ->
	    i("loop -> received notify request complete from "
	      "~n   ~p with"
	      "~n   NotifyReply: ~p", 
	      [Pid, NotifyReply]),
	    server_reply(Parent, notify_request_reply, NotifyReply),
	    loop(evs(S#mg{req_handler = undefined}, {not_reqc, NotifyReply}));


	%% cancel requests
	{cancel_request, Reason, Parent} ->
	    i("loop -> received request to cancel (all) megaco requests ", []),
	    Res = do_cancel_requests(Mid, Reason),
	    server_reply(Parent, cancel_request_reply, Res),
	    loop(evs(S, {creq, Reason}));


	%% Apply multi-load
	{apply_multi_load, {NL, NR}, Parent} -> 
	    i("loop -> received apply_multi_load request: ~w, ~w", [NL, NR]),
	    S1 = start_loaders(S, NL, NR),
	    loop(evs(S1, {apply_mload, {NL, NR}}));


	%% Apply some load
	{apply_load, Times, Parent} ->
	    i("loop -> received apply_load request: ~w", [Times]),
	    S1 = 
		case update_load_times(S, Times) of
		    {ok, MG} ->
			apply_load_timer(),
			server_reply(Parent, apply_load_ack, ok),
			MG;
		    Error ->
			server_reply(Parent, apply_load_ack, Error),
			S
		end,
	    loop(evs(S1, {apply_load, Times}));

	{apply_load_timeout, _} ->
	    d("loop -> received apply_load timeout", []),
	    S1 = do_apply_load(S),
	    loop(evs(S1, apply_loadto));


	%% Megaco callback messages
	{request, Request, Mid, From} ->
	    d("loop -> received megaco request: ~n   ~p"
	      "~n   Mid:  ~p"
	      "~n   From: ~p", 
	      [Request, Mid, From]),
	    {Reply, S1} = handle_megaco_request(S, Request),
	    d("loop -> send (megaco callback) request reply: ~n~p", [Reply]),
	    From ! {reply, Reply, self()},
	    loop(evs(S1, {req, {Request, Mid, From}}));


	{'EXIT', Pid, Reason} -> 
	    i("loop -> received exit signal from ~p: "
              "~n   ~p", [Pid, Reason]),
	    S1 = handle_exit(S, Pid, Reason),
	    loop(evs(S1, {exit, {Pid, Reason}}));


	Invalid ->
	    error_msg("received invalid request: ~n~p", [Invalid]),
	    loop(evs(S, {invalid, Invalid}))

    end.


evs(#mg{evs = EVS} = S, Ev) when (length(EVS) < ?EVS_MAX) ->
    S#mg{evs = [{?FTS(), Ev}|EVS]};
evs(#mg{evs = EVS} = S, Ev) ->
    S#mg{evs = [{?FTS(), Ev}|lists:droplast(EVS)]}.

done(#mg{evs = EVS}, Reason) ->
    info_msg("Exiting with latest event(s): "
             "~n   ~p"
             "~n", [EVS]),
    exit(Reason).


handle_encode_ar_first(#mg{encode_ar_first = Old} = MG, New) 
  when (New =:= true) orelse (New =:= false) ->
    {{ok, Old}, MG#mg{encode_ar_first = New}};
handle_encode_ar_first(MG, New) ->
    {{error, {invalid_value, New}}, MG}.
    

%%
%% Stop user
%%
do_stop(Mid) ->
    d("do_stop -> stopping user ~p", [Mid]),
    Disco = fun close_conn/1, 
    lists:map(Disco, megaco:user_info(Mid, connections)),
    megaco:stop_user(Mid).

close_conn(CH) ->
    d("do_stop -> closing connection ~p", [CH]),
    Reason     = {stopped_by_user,self()},
    Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    megaco:disconnect(CH, Reason),
    megaco:cancel(CH, Reason),
    case SendMod of
	megaco_tcp -> megaco_tcp:close(SendHandle);
	megaco_udp -> megaco_udp:close(SendHandle);
	SendMod    -> exit(Pid, Reason)
    end.



%% 
%% Get statistics 
%% 
do_get_statistics(Mid) ->
    case megaco:user_info(Mid, connections) of
	[CH] ->
	    do_get_conn_statistics(CH);
	[] ->
	    []
    end.

do_get_conn_statistics(CH) ->
    {ok, Gen}  = megaco:get_stats(),
    %% Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    {ok, Trans} = 
	case SendMod of
	    megaco_tcp -> megaco_tcp:get_stats(SendHandle);
	    megaco_udp -> megaco_udp:get_stats(SendHandle)
	end,
    [{gen, Gen}, {trans, Trans}].


%%
%% reset user stats
%%
do_reset_stats(Mid) ->
    %% We only have one connection
    [CH] = megaco:user_info(Mid, connections),
    do_reset_stats1(CH).

do_reset_stats1(CH) ->
    megaco:reset_stats(),
    case (catch megaco:conn_info(CH, send_mod)) of
	{error, Reason} ->
	    error_msg("unexpected result when retrieving send module for "
		      "own connection ~p: ~p. "
		      "~nexiting...", [CH, Reason]),
	    exit({invalid_connection, CH, Reason});
	{'EXIT', Reason} ->
	    error_msg("exit signal when retrieving send module for "
		      "own connection ~p: ~p. "
		      "~nexiting...", [CH, Reason]),
	    exit({invalid_connection, CH, Reason});
	SendMod when is_atom(SendMod) ->
	    SendMod:reset_stats()
    end.


%%
%% Get user info for user
%%
do_get_user_info(Mid, all = Tag) ->
    case (catch megaco:user_info(Mid, Tag)) of
	L when is_list(L) ->
	    lists:sort(L);
	Else ->
	    Else
    end;
do_get_user_info(Mid, Tag) ->
    (catch megaco:user_info(Mid, Tag)).


%%
%% Update user info for user
%%
do_update_user_info(Mid, Tag, Val) ->
    (catch megaco:update_user_info(Mid, Tag, Val)).


%%
%% Get conn info 
%%
do_get_conn_info(CH, all = Tag) when is_record(CH, megaco_conn_handle) ->
    case (catch megaco:conn_info(CH, Tag)) of
	L when is_list(L) ->
	    lists:sort(L);
	Else ->
	    Else
    end;
do_get_conn_info(CH, Tag) when is_record(CH, megaco_conn_handle) ->
    (catch megaco:conn_info(CH, Tag));
do_get_conn_info(Mid, Tag) ->
    case megaco:user_info(Mid, connections) of
	[CH|_] ->
	    do_get_conn_info(CH, Tag);
	[] ->
	    []
    end.

%%
%% Update conn info for user
%%
do_update_conn_info(Mid, Tag, Val) ->
    %% We only have one connection
    [CH] = megaco:user_info(Mid, connections),
    (catch megaco:update_conn_info(CH, Tag, Val)).




%%
%% Perform service change 
%%
do_service_change(#mg{mid             = Mid, 
		      state           = initiated, 
		      encode_ar_first = EAF} = MG) ->
    %% We only have one connection
    d("do service change for ~p", [Mid]),
    [CH]   = megaco:user_info(Mid, connections),
    Method = restart,
    Reason = ?megaco_cold_boot,
    case do_service_change(CH, Method, EAF, Reason) of
	ok ->
	    {ok, MG#mg{state = connecting}};
	Error ->
	    d("service change for ~p failed: ~n~p", [Mid, Error]),
	    Error
    end;
do_service_change(#mg{state = State} = MG) ->
    {{error, {invalid_state, State}}, MG}.

do_service_change(ConnHandle, Method, EAF, Reason) ->
    d("send service change using:"
      "~n   ConnHandle: ~p"
      "~n   Method:     ~p"
      "~n   EAF:        ~p"
      "~n   Reason:     ~p", [ConnHandle, Method, EAF, Reason]),
    SCP    = cre_serviceChangeParm(Method, [Reason]),
    TermId = [?megaco_root_termination_id],
    SCR    = cre_serviceChangeReq(TermId, SCP),
    CR     = cre_commandReq({serviceChangeReq, SCR}),
    AR     = cre_actionReq(?megaco_null_context_id,[CR]),
    send_async(EAF, ConnHandle, [AR], []).


%% Make a sync call 
do_handle_notify_request(#mg{mid             = Mid, 
			     group_size      = N, 
			     encode_ar_first = EAF, 
			     state           = connected} = MG) ->
    d("do_handle_notify_request -> entry"),
    [CH] = megaco:user_info(Mid, connections),
    Pid = start_notify_request_handler(EAF, CH, N),
    {ok, MG#mg{req_handler = Pid}};
do_handle_notify_request(#mg{state = State} = MG) ->
    d("do_handle_notify_request -> entry with"
      "~n   State: ~p", [State]),
    {{error, {invalid_state, State}}, MG}.



%%
%% Cancel requests 
%%
do_cancel_requests(Mid, Reason) ->
    [CH] = megaco:user_info(Mid, connections),
    megaco:cancel(CH, Reason).    


%% 
%% Apply multi load 
%% 
start_loaders(#mg{mid = Mid, encode_ar_first = EAF} = MG, NumLoaders, Times) ->
    [CH] = megaco:user_info(Mid, connections),
    Env  = get(),
    Loaders = start_loaders1(NumLoaders, [], [Env, EAF, Times, CH]),
    d("start_loaders -> Loaders: ~n~w", [Loaders]),
    MG#mg{mload_info = {Loaders, 0, 0}}.

start_loaders1(0, Acc, _) ->
    Acc;
start_loaders1(N, Acc, Args) ->
    Pid = spawn_link(?MODULE, loader_main, Args),
    start_loaders1(N-1, [Pid|Acc], Args).

loader_main(Env, EAF, N, CH) ->
    lists:foreach(fun({Tag,Val}) -> put(Tag,Val) end, Env),
    loader_main(EAF, N, CH).
			  
loader_main(_EAF, 0, _) ->
    d("loader_main -> done"),
    exit(loader_done);
loader_main(EAF, N, CH) ->
    d("loader_main -> entry with: ~w", [N]),
    {Act, _} = make_notify_request(),
    _Res     = send_sync(EAF, CH, Act, []),
    loader_main(EAF, N-1, CH).



handle_exit(#mg{parent = Pid} = S, Pid, Reason) ->
    error_msg("received exit from the parent:"
	      "~n   ~p", [Reason]),
    done(S, {parent_terminated, Reason});

handle_exit(#mg{parent = Parent, req_handler = Pid} = MG, Pid, Reason) ->
    error_msg("received unexpected exit from the request handler:"
	      "~n   ~p", [Reason]),
    server_reply(Parent, notify_request_reply, 
		 {error, {request_handler_exit, Reason}}),
    MG#mg{req_handler = undefined};

handle_exit(#mg{parent = Parent, mload_info = {Loaders0, Ok, Err}} = MG, 
	    Pid, loader_done) ->
    d("handle_exit(loader_done) -> entry when"
      "~n   Loaders0: ~p"
      "~n   Ok:       ~p"
      "~n   Err:      ~p", [Loaders0, Ok, Err]),
    Loaders = lists:delete(Pid, Loaders0),
    LoadInfo =
	case Loaders of
	    [] ->
		d("handle_exit -> multi load done", []),
		server_reply(Parent, apply_multi_load_ack, {ok, Ok+1, Err}),
		undefined;
	    _ ->
		{Loaders, Ok+1, Err}
	end,
    MG#mg{mload_info = LoadInfo};


handle_exit(#mg{parent = Parent, mload_info = {Loaders, Ok, Err}} = MG, 
	    Pid, Reason) 
  when length(Loaders) > 0 ->
    d("handle_exit -> entry when"
      "~n   Reason:  ~p"
      "~n   Loaders: ~p"
      "~n   Ok:      ~p"
      "~n   Err:     ~p", [Reason, Loaders, Ok, Err]),
    case lists:delete(Pid, Loaders) of
	[] -> 
	    %% since we cannot be empty prior the delete, 
	    %% the last one exited...
	    server_reply(Parent, apply_multi_load, {ok, Ok, Err+1}),
	    MG#mg{mload_info = undefined};
	Loaders ->
	    %% Could not be this MG, so go on to the next
	    error_msg("received unexpected exit signal from ~p:~n~p", 
		      [Pid, Reason]);
	Loaders1 ->
	    %% Not empty, but we removed one
	    MG#mg{mload_info = {Loaders1,Ok,Err+1}}
    end;
handle_exit(_MG, Pid, Reason) ->
    error_msg("received unexpected exit signal from ~p:~n~p", 
	      [Pid, Reason]).

		      
parse_receive_info(RI, RH) ->
    d("parse_receive_info -> get encoding module"),
    EM = get_encoding_module(RI),
    d("parse_receive_info -> get encoding config"),
    EC = get_encoding_config(RI, EM),
    d("parse_receive_info -> get transport module"),
    TM = get_transport_module(RI),
    d("parse_receive_info -> get transport port"),
    TP = get_transport_port(RI),
    d("parse_receive_info -> get transport host"),
    TH = get_transport_host(RI),
    d("parse_receive_info -> get transport opts"),
    TO = get_transport_opts(RI),
    RH1 = RH#megaco_receive_handle{send_mod        = TM,
				   encoding_mod    = EM,
				   encoding_config = EC},
    {TP, TH, RH1, TO}.


start_transport(MgcPort, MgcHost, 
		   #megaco_receive_handle{send_mod = megaco_tcp} = RH, TO) ->
    start_tcp(MgcPort, MgcHost, RH, TO);
start_transport(MgcPort, MgcHost, 
		   #megaco_receive_handle{send_mod = megaco_udp} = RH, TO) ->
    start_udp(MgcPort, MgcHost, RH, TO);
start_transport(_, _, #megaco_receive_handle{send_mod = Mod}, _TO) ->
    throw({error, {bad_send_mod, Mod}}).


start_tcp(MgcPort, MgcHost, RH, TO) ->
    d("start tcp transport: "
      "~n   MGC Port:          ~p"
      "~n   MGC Host:          ~p"
      "~n   Receive handle:    ~p"
      "~n   Transport options: ~p", [MgcPort, MgcHost, RH, TO]),
    case megaco_tcp:start_transport() of
	{ok, Sup} ->
	    d("tcp transport started: ~p", [Sup]),
	    start_tcp_connect(TO, RH, MgcPort, MgcHost, Sup);
        {error, Reason} ->
            throw({error, {megaco_tcp_start_transport, Reason}})
    end.

start_tcp_connect(TO, RH, Port, Host, Sup) ->
    d("try tcp connecting to: ~p:~p", [Host, Port]),
    Opts = [{host,           Host},
	    {port,           Port}, 
	    {receive_handle, RH},
	    {tcp_options,    [{nodelay, true}]}] ++ TO,
    try_start_tcp_connect(RH, Sup, Opts, 250, noError).

try_start_tcp_connect(RH, Sup, Opts, Timeout, Error0) when (Timeout < 5000) ->
    Sleep = random(Timeout) + 100,
    d("try tcp connect (~p,~p)", [Timeout, Sleep]),
    case megaco_tcp:connect(Sup, Opts) of
	{ok, SendHandle, ControlPid} ->
	    d("tcp connected: ~p, ~p", [SendHandle, ControlPid]),
	    megaco_tcp_connect(RH, SendHandle, ControlPid);
	Error1 when Error0 =:= noError -> % Keep the first error
	    d("failed connecting [1]: ~p", [Error1]),
	    sleep(Sleep),
	    try_start_tcp_connect(RH, Sup, Opts, Timeout*2, Error1);
	Error2 ->	
	    d("failed connecting [2]: ~p", [Error2]),
	    sleep(Sleep),
	    try_start_tcp_connect(RH, Sup, Opts, Timeout*2, Error0)
    end;
try_start_tcp_connect(_RH, Sup, _Opts, _Timeout, Error) ->
    megaco_tcp:stop_transport(Sup),
    throw({error, {megaco_tcp_connect, Error}}).
    
megaco_tcp_connect(RH, SendHandle, ControlPid) ->
    PrelMgcMid = preliminary_mid,
    d("megaco connect", []),    
    {ok, CH} = megaco:connect(RH, PrelMgcMid, SendHandle, ControlPid),
    d("megaco connected: ~p", [CH]),    
    {ok, CH}.

start_udp(MgcPort, MgcHost, RH, TO) ->
    d("start udp transport (~p)", [MgcPort]),
    case megaco_udp:start_transport() of
	{ok, Sup} ->
	    d("udp transport started: ~p", [Sup]),
	    Opts = [{port, 0}, {receive_handle, RH}] ++ TO,
	    d("udp open", []),
	    case megaco_udp:open(Sup, Opts) of
		{ok, Handle, ControlPid} ->
		    d("udp opened: ~p, ~p", [Handle, ControlPid]),
		    megaco_udp_connect(MgcPort, MgcHost, 
				       RH, Handle, ControlPid);
		{error, Reason} ->
                    throw({error, {megaco_udp_open, Reason}})
	    end;
        {error, Reason} ->
            throw({error, {megaco_udp_start_transport, Reason}})
    end.

megaco_udp_connect(MgcPort, MgcHost, RH, Handle, ControlPid) ->
    MgcMid     = preliminary_mid,
    SendHandle = megaco_udp:create_send_handle(Handle, MgcHost, MgcPort),
    d("megaco connect", []),    
    {ok, CH} = megaco:connect(RH, MgcMid, SendHandle, ControlPid),
    d("megaco connected: ~p", [CH]),    
    {ok, CH}.


update_load_times(#mg{load_counter = 0} = MG, Times) ->
    d("update_load_times(0) -> entry with"
      "~n   Times: ~p", [Times]),
    {ok, MG#mg{load_counter = Times}};
update_load_times(#mg{load_counter = N}, Times) ->
    d("update_load_times(~p) -> entry with"
      "~n   Times: ~p", [N, Times]),
    {error, {already_counting, N}}.


do_apply_load(#mg{mid = Mid} = MG) ->
    d("do_apply_load -> entry"),
    case megaco:user_info(Mid, connections) of
	[CH] ->
	    do_apply_load(MG, CH);
	[] ->
	    i("failed to apply load: no connections for ~p", [Mid]),
	    MG
    end.

do_apply_load(#mg{parent          = Parent,
		  encode_ar_first = EAF,
		  call_mode       = Mode, 
		  group_size      = Sz, 
		  load_counter    = N0} = MG, CH) ->
    d("do_apply_load -> entry with"
      "~n   Mode: ~p"
      "~n   Sz:   ~p"
      "~n   N0:   ~p", [Mode, Sz, N0]),
    {NofSent, Actions, ReplyData} = make_notify_request(N0, Sz),
    d("do_apply_load -> notifications constructed:"
      "~n   NofSent:   ~p"
      "~n   Actions:   ~p"
      "~n   ReplyData: ~p", [NofSent, Actions, ReplyData]),
    N = N0 - NofSent,
    case Mode of
	sync ->
	    Result = send_sync(EAF, CH, Actions, []),
	    d("do_apply_load -> call result when N = ~p: ~n~p", [N,Result]),
	    case N of
		0 ->
		    d("do_apply_load -> load complete"),
		    Parent ! {load_complete, self()},
		    MG#mg{call_mode = async, load_counter = 0};
		_ ->
		    d("do_apply_load -> make another round"),
		    apply_load_timer(),
		    MG#mg{call_mode = async, load_counter = N}
	    end;
	async ->
	    Result = send_async(EAF, CH, Actions, [{reply_data, ReplyData}]),
	    d("do_apply_load -> cast result:~n   ~p", [Result]),
	    MG#mg{call_mode     = sync, 
		  load_counter  = N, 
		  reply_counter = NofSent} % Outstanding replies
    end.


start_notify_request_handler(EAF, CH, N) ->
    d("start_notify_request_handler -> entry with"
      "~n   EAF: ~p"
      "~n   CH:  ~p"
      "~n   N:   ~p", [EAF, CH, N]),
    Env = get(),
    spawn_link(?MODULE, notify_request_handler_main, [self(), Env, EAF, CH, N]).

notify_request_handler_main(Parent, Env, EAF, CH, N) ->
    F = fun({Tag, Val}) -> put(Tag, Val) end,
    lists:foreach(F, Env),
    d("notify_request_handler_main -> entry with"
      "~n   Parent: ~p"
      "~n   EAF:    ~p"
      "~n   CH:     ~p"
      "~n   N:      ~p", [Parent, EAF, CH, N]),
    Res = do_notify_request(EAF, CH, N),
    d("notify_request_handler_main -> notify complete:"
      "~n   Res: ~p", [Res]),
    Parent ! {notify_request_complete, {ok, Res}, self()},
    unlink(Parent),
    exit(normal).

do_notify_request(_EAF, _CH, N) when N =< 0 ->
    d("do_notify_request(~p) -> ignoring", [N]),
    ignore;
do_notify_request(EAF, CH, 1) ->
    d("do_notify_request(1) -> entry with"),
    {Action, _} = make_notify_request(),
    send_sync(EAF, CH, Action, []);
do_notify_request(EAF, CH, N) ->
    d("do_notify_request(~p) -> entry with", [N]),
    {N, Actions, _} = make_notify_request(N,N),
    send_sync(EAF, CH, Actions, []).

make_notify_request(N, Sz) when (N >= Sz) andalso (Sz > 0) ->
    {Req, ReplyData} = make_notify_request(N, Sz, [], []),
    {Sz, Req, ReplyData};
make_notify_request(N, _Sz) when N > 0 ->
    {Req, ReplyData} = make_notify_request(N, N, [], []),
    {N, Req, ReplyData}.


    
make_notify_request(_Offset, 0, Actions, ReplyDatas) ->
    {lists:reverse(Actions), lists:reverse(ReplyDatas)};
make_notify_request(Offset, N, Actions, ReplyDatas) when N > 0 ->
    TimeStamp = cre_timeNotation(),
    Event     = cre_observedEvent("al/of", TimeStamp),
    Desc      = cre_observedEventsDesc(2000 + N, [Event]),
    TidNum    = 2#10000000 + Offset - N,
    NotifyReq = cre_notifyReq([#megaco_term_id{id = tid(TidNum)}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    ActReq    = cre_actionReq(?megaco_null_context_id, [CmdReq]),
    make_notify_request(Offset, N-1, [[ActReq]|Actions], [Desc|ReplyDatas]).
    
make_notify_request() ->
    TimeStamp  = cre_timeNotation("19990729", "22000000"),
    Event      = cre_observedEvent("al/of",   TimeStamp),
    Desc1      = cre_observedEventsDesc(2221, [Event]),
    Desc2      = cre_observedEventsDesc(2222, [Event]),
    Desc3      = cre_observedEventsDesc(2223, [Event]),
    Desc4      = cre_observedEventsDesc(2224, [Event]),
    NotifyReq1 = cre_notifyReq([#megaco_term_id{id = ?A4444}], Desc1),
    NotifyReq2 = cre_notifyReq([#megaco_term_id{id = ?A4445}], Desc2),
    CmdReq1    = cre_commandReq({notifyReq, NotifyReq1}),
    CmdReq2    = cre_commandReq({notifyReq, NotifyReq2}),
    ActReq     = cre_actionReq(?megaco_null_context_id, [CmdReq1,CmdReq2]),
    {[ActReq], [Desc3,Desc4]}.


cre_actionReq(Cid, Cmds) ->
    #'ActionRequest'{contextId       = Cid,
		     commandRequests = Cmds}.

cre_commandReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_serviceChangeReq(TermId, Parms) ->
    #'ServiceChangeRequest'{terminationID      = TermId,
			    serviceChangeParms = Parms}.

cre_serviceChangeParm(Method, Reason) ->
    #'ServiceChangeParm'{serviceChangeMethod = Method,
			 serviceChangeReason = Reason}.

cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID = Tid, observedEventsDescriptor = EvsDesc}.

% cre_notifyRep(Tid) ->
%     #'NotifyReply'{terminationID = [Tid]}.

% cre_notifyRep(Tid,Err) ->
%     #'NotifyReply'{terminationID = [Tid], errorDescriptor = Err}.

cre_observedEventsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId = Id, observedEventLst = EvList}.

cre_observedEvent(Name, Not) ->
    #'ObservedEvent'{eventName = Name, timeNotation = Not}.

cre_timeNotation() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:universal_time(),
    D = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year,Month,Day])),
    T = lists:flatten(io_lib:format("~2..0w~2..0w~4..0w", [Hour,Min,Sec])),
    cre_timeNotation(D, T).
    
cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

cre_error_descr(Code,Text) ->
    #'ErrorDescriptor'{errorCode = Code, errorText = Text}.

% cre_error_descr(Code,FormatString,Args) ->
%     Text = lists:flatten(io_lib:format(FormatString,Args)),
%     cre_error_descr(Code,Text).


%% -----------------------
%% Handle megaco callbacks
%%

handle_megaco_request(#mg{state = connecting} = MG,
		      {handle_connect, _CH, _PV}) ->
    d("handle_megaco_request(handle_connect,connecting) -> entry"),
    {ok, MG};
handle_megaco_request(#mg{state = S} = MG,
		      {handle_connect, _CH, _PV}) ->
    d("handle_megaco_request(handle_connect) -> entry"),
    Desc = 
	lists:flatten(io_lib:format("not ready for connect in state ~p", [S])),
    ED   =  cre_error_descr(?megaco_internal_gateway_error, Desc),
    {{discard_ack, ED}, MG};

handle_megaco_request(#mg{req_handler = Pid} = MG,
		      {handle_disconnect, _CH, _PV, R}) 
  when is_pid(Pid) ->
    d("handle_megaco_request(handle_disconnect) -> entry with"
      "~n   Pid: ~p", [Pid]),
    Error = {error, {disconnected, R}},
    self() ! {notify_request_complete, Error, Pid},
    unlink(Pid),
    exit(Pid, kill),
    {ok, MG#mg{req_handler = undefined, state = initiated}};
handle_megaco_request(MG, {handle_disconnect, _CH, _PV, _R}) ->
    d("handle_megaco_request(handle_disconnect) -> entry"),
    {ok, MG#mg{state = initiated}};

handle_megaco_request(MG, 
		      {handle_syntax_error, _RH, _PV, _ED}) ->
    {reply, MG};

handle_megaco_request(#mg{req_handler = Pid} = MG,
		      {handle_message_error, CH, PV, ED}) 
  when is_pid(Pid) ->
    d("handle_megaco_request(handle_message_error) -> entry with"
      "~n   Pid:    ~p"
      "~n   CH:     ~p"
      "~n   PV:     ~p"
      "~n   ED:     ~p", [Pid, CH, PV, ED]),    
    self() ! {notify_request_complete, ED, Pid},
    unlink(Pid),
    exit(Pid, kill),
    {no_reply, MG#mg{req_handler = undefined}};
handle_megaco_request(MG, {handle_message_error, CH, PV, ED}) ->
    d("handle_megaco_request(handle_message_error) -> entry with"
      "~n   CH:     ~p"
      "~n   PV:     ~p"
      "~n   ED:     ~p", [CH, PV, ED]),    
    {no_reply, MG};

handle_megaco_request(MG, {handle_trans_request, _CH, _PV, _AR}) ->
    ED =  cre_error_descr(?megaco_not_implemented,
			  "Transaction requests not handled"),
    {{discard_ack, ED}, MG};

handle_megaco_request(MG,
		      {handle_trans_long_request, _CH, _PV, _RD}) ->
    ED = cre_error_descr(?megaco_not_implemented,
			 "Long transaction requests not handled"),
    {{discard_ack, ED}, MG};

handle_megaco_request(#mg{rep_info = P} = MG, 
		      {handle_trans_reply, CH, PV, AR, RD}) when is_pid(P) ->
    P ! {rep_received, self(), AR},
    do_handle_trans_reply(MG, CH, PV, AR, RD);
handle_megaco_request(MG, {handle_trans_reply, CH, PV, AR, RD}) ->
    do_handle_trans_reply(MG, CH, PV, AR, RD);

handle_megaco_request(#mg{ack_info = P} = MG, 
		      {handle_trans_ack, _CH, _PV, AS, _AD}) when is_pid(P) ->
    d("handle_megaco_request(handle_trans_ack,~p) -> entry",[P]),
    P ! {ack_received, self(), AS},
    {ok, MG};
handle_megaco_request(MG, {handle_trans_ack, _CH, _PV, _AS, _AD}) ->
    d("handle_megaco_request(handle_trans_ack) -> entry"),
    {ok, MG}.

do_handle_trans_reply(#mg{parent = Parent, state = connecting} = MG,
		      CH, _PV, {ok, Rep}, _RD) ->
    d("do_handle_trans_reply(connecting) -> entry with"
      "~n   CH:     ~p"
      "~n   Rep:    ~p", [CH, Rep]),
    server_reply(Parent, service_change_reply, ok),
    {ok, MG#mg{state = connected}};
do_handle_trans_reply(#mg{parent = Parent, load_counter = 0} = MG,
		      CH, _PV, {ok, Rep}, _RD) ->
    d("do_handle_trans_reply(load_counter = 0) -> entry with"
      "~n   CH:     ~p"
      "~n   Rep:    ~p", [CH, Rep, Parent]),
    handle_trans_reply_verify_act(Rep),
    server_reply(Parent, load_complete, ok),
    {ok, MG#mg{reply_counter = 0}};
do_handle_trans_reply(#mg{reply_counter = 0} = MG,
		      CH, _PV, {ok, Rep}, _RD) ->
    d("do_handle_trans_reply(reply_counter = 0) -> entry with"
      "~n   CH:     ~p"
      "~n   Rep:    ~p", [CH, Rep]),
    handle_trans_reply_verify_act(Rep),
    apply_load_timer(),
    {ok, MG};
do_handle_trans_reply(#mg{reply_counter = N} = MG,
		      CH, _PV, {ok, Rep}, _RD) ->
    d("do_handle_trans_reply(reply_counter = ~p) -> entry with"
      "~n   CH:     ~p"
      "~n   Rep:    ~p", [N, CH, Rep]),
    handle_trans_reply_verify_act(Rep),
    apply_load_timer(),
    {ok, MG#mg{reply_counter = N-1}};
do_handle_trans_reply(MG, _CH, _PV, {error, ED}, _RD) ->
    i("unexpected error transaction: ~p", [ED]),
    {ok, MG}.


handle_trans_reply_verify_act([]) ->
    ok;
handle_trans_reply_verify_act([#'ActionReply'{commandReply = Rep}|Reps]) ->
    handle_trans_reply_verify_cmd(Rep),
    handle_trans_reply_verify_act(Reps);
handle_trans_reply_verify_act([Rep|Reps]) ->
    i("received 'propably' unexpected reply: ~n~p", [Rep]),
    handle_trans_reply_verify_act(Reps).

handle_trans_reply_verify_cmd([]) ->
    ok;
handle_trans_reply_verify_cmd([Cmd|Cmds]) ->
    case Cmd of
	{notifyReply, #'NotifyReply'{terminationID = [Tid]}} ->
	    d("received expected notification reply from ~n   ~p", [Tid]);
	Else ->
	    i("received unexpected notification reply ~n~p", [Else])
    end,
    handle_trans_reply_verify_cmd(Cmds).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

notify_started(Parent) ->
    Parent ! {started, self()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The megaco user callback interface 

handle_connect(CH, PV, Pid, Mid) ->
    case CH#megaco_conn_handle.remote_mid of
        preliminary_mid ->
	    %% Avoids deadlock
	    ok;
	_ ->
	    Reply = request(Pid, {handle_connect, CH, PV}, Mid),
	    Reply
    end.

handle_disconnect(_CH, _PV, 
		  {user_disconnect, {stopped_by_user, Pid}}, 
		  Pid, _Mid) ->
    ok;
handle_disconnect(CH, PV, R, Pid, Mid) ->
    request(Pid, {handle_disconnect, CH, PV, R}, Mid).

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor, Pid, Mid) ->
    Req = {handle_syntax_error, ReceiveHandle, ProtocolVersion, 
	   ErrorDescriptor},
    request(Pid, Req, Mid).
    
handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Pid, Mid) ->
    Req = {handle_message_error, ConnHandle, ProtocolVersion, ErrorDescriptor},
    request(Pid, Req, Mid).

handle_trans_request(CH, PV, AR, Pid, Mid) ->
    Reply = request(Pid, {handle_trans_request, CH, PV, AR}, Mid),
    Reply.

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData, Pid, Mid) ->
    Req = {handle_trans_long_request, ConnHandle, ProtocolVersion, ReqData},
    request(Pid, Req, Mid).

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData, Pid, Mid) ->
    Req = {handle_trans_reply, ConnHandle, ProtocolVersion, 
	   ActualReply, ReplyData},
    request(Pid, Req, Mid).

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData, Pid, Mid) ->
    Req = {handle_trans_ack, ConnHandle, ProtocolVersion, AckStatus, AckData},
    request(Pid, Req, Mid).


request(Pid, Request, Mid) ->
    Pid ! {request, Request, Mid, self()},
    receive
	{reply, {delay, To, ED}, Pid} ->
	    sleep(To),
	    {discard_ack, ED};
	{reply, Reply, Pid} ->
	    Reply
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_async(true, CH, Actions, Opts) ->
    d("send_async(true) -> encode actions first"),
    case megaco:encode_actions(CH, Actions, Opts) of
	{ok, BinOrBins} ->
	    d("send_async(true) -> send message"),
	    megaco:cast(CH, BinOrBins, Opts);
	Error ->
	    d("send_async(true) -> encode failed: ~n~p", [Error]),
	    Error
    end;
send_async(_, CH, Actions, Opts) ->
    d("send_async(true) -> send message"),
    megaco:cast(CH, Actions, Opts).

send_sync(true, CH, Actions, Opts) ->
    d("send_sync(true) -> encode actions first"),
    case megaco:encode_actions(CH, Actions, Opts) of
	{ok, BinOrBins} ->
	    d("send_sync(true) -> send message"),
	    megaco:call(CH, BinOrBins, Opts);
	Error ->
	    d("send_sync(true) -> encode failed: ~n~p", [Error]),
	    Error
    end;
send_sync(_, CH, Actions, Opts) ->
    d("send_sync(false) -> send message"),
    megaco:call(CH, Actions, Opts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) ->
    receive after X -> ok end.


info_msg(F,A)  -> error_logger:info_msg("MG: " ++ F ++ "~n",A).
error_msg(F,A) -> error_logger:error_msg("MG: " ++ F ++ "~n",A).


get_encoding_module(RI) ->
    case (catch get_conf(encoding_module, RI)) of
	{error, _} ->
	    undefined;
	Val ->
	    Val
    end.

get_encoding_config(RI, EM) ->
    case text_codec(EM) of
	true ->
	    case megaco:system_info(text_config) of
		[Conf] when is_list(Conf) ->
		    Conf;
		_ ->
		    []
	    end;

	false ->
	    get_conf(encoding_config, RI)
    end.

text_codec(megaco_compact_text_encoder) ->
    true;
text_codec(megaco_pretty_text_encoder) ->
    true;
text_codec(_) ->
    false.


get_transport_module(RI) ->
    get_conf(transport_module, RI).

get_transport_port(RI) ->
    get_conf(port, RI).

get_transport_host(RI) ->
    {ok, LocalHost} = inet:gethostname(),
    get_conf(host, RI, LocalHost).

get_transport_opts(RI) ->
    get_conf(transport_opts, RI, []).


get_conf(Key, Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value, {Key, Val}} ->
	    Val;
	_ ->
	    exit({error, {not_found, Key, Config}})
    end.

get_conf(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
	{value, {Key, Val}} ->
	    Val;
	_ ->
	    Default
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tid(N) when N >= 0 ->
    {Rem1, Val1} = num2str(N),
    {Rem2, Val2} = num2str(Rem1),
    {0,    Val3} = num2str(Rem2),
    [Val3, Val2, Val1].

num2str(N) when N >= 0 ->
    num2str(N, []).

num2str(Rem, Val) when length(Val) == 8 ->
    {Rem, Val};
num2str(N, Val) ->
    D = N div 2,
    case N rem 2 of
	1 ->
	    num2str(D, [$1|Val]);
	0 -> 
	    num2str(D, [$0|Val])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), "", F, A).


d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, get(verbosity), "DBG", F, A).


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, P, F, A) ->
    print(printable(Severity,Verbosity), P, F, A).

print(true, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s ***"
              "~n   " ++ F ++ "~n~n", 
              [?FTS(), P, self(), get(sname) | A]);
print(_, _, _, _) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_init() ->
    ok.

random() ->
    random(50).
random(N) ->
    rand:uniform(N).


display_system_info(Mid) ->
    display_system_info(Mid, "").

display_system_info(Mid, Pre) ->
    TimeStr = ?FTS(),
    MibStr  = lists:flatten(io_lib:format("~p ", [Mid])), 
    megaco_test_lib:display_system_info(MibStr ++ Pre ++ TimeStr).


create_timer(Time, Event) ->
    erlang:send_after(Time, self(), {Event, Time}).

cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    erlang:cancel_timer(Ref).


apply_load_timer() ->
    create_timer(random(), apply_load_timeout).


