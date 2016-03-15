%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% Purpose: Simple example of an MG
%%
%% Example usage:
%% 
%%   cd megaco/examples/simple
%%   erl -pa ../../../megaco/ebin -s megaco_filter -s megaco
%%   megaco_simple_mg:start().
%%----------------------------------------------------------------------

-module(megaco_simple_mg).

-behaviour(megaco_user).

-export([
	 start_batch/0, start_batch/1, init_batch/4,
	 start/0, start/3, 
	 start/4, %% ????????????????????????
	 stop/0, stop/1,
	 start_tcp_text/2, start_tcp_binary/2, 
	 start_udp_text/2, start_udp_binary/2
	 ]).

-export([
         handle_connect/2,
         handle_disconnect/3,
         handle_syntax_error/3,
         handle_message_error/3,
         handle_trans_request/3,
         handle_trans_long_request/3,
         handle_trans_reply/4,
         handle_trans_ack/4,
	 handle_unexpected_trans/3,
	 handle_trans_request_abort/4
        ]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").



%%----------------------------------------------------------------------
%% To be used at command line: erl -s ?MODULE start_batch
%%----------------------------------------------------------------------

start_batch() ->
    start_batch([]).

start_batch(Args0) when is_list(Args0) ->
    {ok, LocalHost} = inet:gethostname(),
    Defs    = [{mgc_host, LocalHost}, {trace,false}, {debug, false}],
    Args    = parse_args(Args0, Defs),
    MgcHost = get_arg(mgc_host, Args),
    Trace   = get_arg(trace, Args),
    Debug   = get_arg(debug, Args),
    Pid     = spawn(?MODULE, init_batch, [self(), MgcHost, Trace, Debug]),
    receive
	{init_batch, Pid, Res} ->
	    io:format("~p(~p): ~p~n", [?MODULE, ?LINE, Res]),
	    Res
    end.
	    
parse_args([], Acc) ->
    Acc;
parse_args([Arg|Args], Acc) when is_atom(Arg) ->
    case string:tokens(atom_to_list(Arg),"{},") of
	["mgc_host", Host] when is_list(Host) ->
	    parse_args(Args, parse_args(mgc_host, Host, Acc));
	["trace",Trace] ->
	    parse_args(Args, parse_args(trace, list_to_atom(Trace), Acc));
	["debug",Debug] ->
	    parse_args(Args, parse_args(debug, list_to_atom(Debug), Acc));
	_Invalid ->
	    parse_args(Args, Acc)
    end.

parse_args(Key, Val, Args) ->
    Entry = {Key, Val},
    case lists:keyreplace(Key, 1, Args, {Key, Val}) of
	Args ->
	    [Entry|Args];
	Args2 ->
	    Args2
    end.

get_arg(Key, Args) ->
    {value, {Key, Val}} = lists:keysearch(Key, 1, Args),
    Val.

init_batch(ReplyTo, MgcHost, Trace, Debug) ->
    register(?MODULE, self()),
    Res = start(MgcHost, Trace, Debug),
    ReplyTo ! {init_batch, self(), Res},
    receive
    after infinity -> Res
    end.


%%----------------------------------------------------------------------
%% Starting the MG
%%----------------------------------------------------------------------

%% -----------------------------------------------------------------------

init_inline_trace(true) ->
    megaco:enable_trace(max, io);
init_inline_trace(_) ->
    ok.

%% -----------------------------------------------------------------------


start() ->
    {ok, LocalHost} = inet:gethostname(),
    start(LocalHost, false, false).

%% Used when calling from the erlang shell:
start(MgcHost, Trace, Debug) 
  when is_atom(MgcHost) andalso is_atom(Trace) andalso is_atom(Debug) ->
    start(atom_to_list(MgcHost), Trace, Debug);

start(MgcHost, Trace, Debug) 
  when is_list(MgcHost) andalso is_atom(Trace) andalso is_atom(Debug) ->
    put(debug, Debug),
    d("start -> entry with"
      "~n   MgcHost: ~s"
      "~n   Trace:   ~p", [MgcHost, Trace]),
    init_inline_trace(Trace),
    Starters = [fun start_tcp_text/2,
		fun start_tcp_binary/2,
		fun start_udp_text/2,
		fun start_udp_binary/2],
    [Fun(MgcHost, []) || Fun <- Starters].

start_tcp_text(MgcHost, Default) ->
    d("start_tcp_text -> entry with"
      "~n   MgcHost: ~p", [MgcHost]),
    Config = [{encoding_mod, megaco_pretty_text_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_tcp} | Default],
    Mid = {deviceName, "gateway_tt"},
    {Mid, start(MgcHost, ?megaco_ip_port_text, Mid, Config)}.

start_tcp_binary(MgcHost, Default) ->
    d("start_tcp_binary -> entry with"
      "~n   MgcHost: ~p", [MgcHost]),
    Config = [{encoding_mod, megaco_binary_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_tcp} | Default],
    Mid = {deviceName, "gateway_tb"},
    {Mid, start(MgcHost, ?megaco_ip_port_binary, Mid, Config)}.

start_udp_text(MgcHost, Default) ->
    d("start_udp_text -> entry with"
      "~n   MgcHost: ~p", [MgcHost]),
    Config = [{encoding_mod, megaco_pretty_text_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_udp} | Default],
    Mid = {deviceName, "gateway_ut"},
    {Mid, start(MgcHost, ?megaco_ip_port_text, Mid, Config)}.

start_udp_binary(MgcHost, Default) ->
    d("start_udp_binary -> entry with"
      "~n   MgcHost: ~p", [MgcHost]),
    Config = [{encoding_mod, megaco_binary_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_udp} | Default],
    Mid = {deviceName, "gateway_ub"},
    {Mid, start(MgcHost, ?megaco_ip_port_binary, Mid, Config)}.

start(MgcHost, MgcPort, Mid, Config) ->
    case megaco:start_user(Mid, [{user_mod, ?MODULE} | Config]) of
	ok ->
	    case start_transport(MgcHost, MgcPort, Mid) of
		{ok, ConnHandle} ->
		    service_change(ConnHandle);
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, {start_user, Reason}}
    end.

start_transport(MgcHost, MgcPort, Mid) ->
    RecHandle = megaco:user_info(Mid, receive_handle),
    case RecHandle#megaco_receive_handle.send_mod of
	megaco_tcp -> start_tcp(MgcHost, MgcPort, RecHandle);
	megaco_udp -> start_udp(MgcHost, MgcPort, RecHandle);
	SendMod    -> {error, {bad_send_mod, SendMod}}
    end.

start_tcp(MgcHost, MgcPort, RecHandle) ->
    d("start_tcp -> start transport"),
    case megaco_tcp:start_transport() of
	{ok, Pid} ->
	    d("start_tcp -> transport started: ~p", [Pid]),
	    Options = [{host, MgcHost},
		       {port, MgcPort},
		       {receive_handle, RecHandle}],
	    case megaco_tcp:connect(Pid, Options) of
		{ok, SendHandle, ControlPid} ->
		    d("start_tcp -> connected: ~p", [ControlPid]),
		    MgcMid = preliminary_mid,
		    megaco:connect(RecHandle, MgcMid, SendHandle, ControlPid);
		{error, Reason} ->
		    d("start_tcp -> connection failed: ~p", [Reason]),
		    {error, {megaco_tcp_connect, Reason}}
	    end;
	{error, Reason} ->
	    d("start_tcp -> failed starting transport: ~p", [Reason]),
	    {error, {megaco_tcp_start_transport, Reason}}
    end.

start_udp(MgcHost, MgcPort, RecHandle) ->
    d("start_udp -> start transport"),
    case megaco_udp:start_transport() of
	{ok, SupPid} ->
	    d("start_udp -> transport started: ~p", [SupPid]),
	    Options = [{port, 0}, {receive_handle, RecHandle}],
	    case megaco_udp:open(SupPid, Options) of
		{ok, Handle, ControlPid} ->
		    d("start_udp -> port opened: ~p", [ControlPid]),
		    %% Socket = megaco_udp:socket(Handle),
		    %% MgPort = inet:port(Socket), BUGBUG BUGBUG
		    MgcMid = preliminary_mid,
		    SendHandle = megaco_udp:create_send_handle(Handle, 
							       MgcHost, % BUGBUG BUGBUG
							       MgcPort),
		    megaco:connect(RecHandle, MgcMid, SendHandle, ControlPid);
		{error, Reason} ->
		    d("start_udp -> failed open port: ~p", [Reason]),
		    {error, {megaco_udp_open, Reason}}
	    end;
	{error, Reason} ->
	    d("start_udp -> failed starting transport: ~p", [Reason]),
	    {error, {megaco_udp_start_transport, Reason}}
    end.

service_change(ConnHandle) ->
    service_change(ConnHandle, restart, ?megaco_cold_boot).

service_change(ConnHandle, Method, Reason) ->
    SCP = #'ServiceChangeParm'{serviceChangeMethod = Method,
			       serviceChangeReason = [Reason]},
    TermId = [?megaco_root_termination_id],
    SCR = #'ServiceChangeRequest'{terminationID = TermId,
				  serviceChangeParms = SCP},
    CR = #'CommandRequest'{command = {serviceChangeReq, SCR}},
    AR = #'ActionRequest'{contextId = ?megaco_null_context_id,
			  commandRequests = [CR]},
    megaco:call(ConnHandle, [AR], []).
    
%%----------------------------------------------------------------------
%% Stopping the MG
%%----------------------------------------------------------------------

stop() ->
    [{Mid, stop(Mid)} || Mid <- megaco:system_info(users)].

stop(Mid) ->
    Reason = stopped_by_user,
    Disco = fun(CH) ->
		    Pid = megaco:conn_info(CH, control_pid),
		    megaco:disconnect(CH, Reason),
		    megaco:cancel(CH, Reason),
		    exit(Pid, Reason)
	    end,
    lists:map(Disco, megaco:user_info(Mid, connections)),
    megaco:stop_user(Mid).

%%----------------------------------------------------------------------
%% Invoked when a new connection is established
%%----------------------------------------------------------------------

handle_connect(ConnHandle, ProtocolVersion) ->
    d("handle_connect -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p", [ConnHandle, ProtocolVersion]),
    ok.

%%----------------------------------------------------------------------
%% Invoked when a connection is teared down
%%----------------------------------------------------------------------

handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    d("handle_disconnect -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   Reason:          ~p", [ConnHandle, ProtocolVersion, Reason]),
    megaco:cancel(ConnHandle, Reason), % Cancel the outstanding messages
    d("handle_disconnect -> done", []),
    ok.

%%----------------------------------------------------------------------
%% Invoked when  a received message had syntax errors
%%----------------------------------------------------------------------

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor) ->
    d("handle_syntax_error -> entry with"
      "~n   ReceiveHandle:   ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ErrorDescriptor: ~p", 
      [ReceiveHandle, ProtocolVersion, ErrorDescriptor]),
    reply.

%%----------------------------------------------------------------------
%% Invoked when a received message contained no transactions
%%----------------------------------------------------------------------

handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor) ->
    d("handle_message_error -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ErrorDescriptor: ~p", 
      [ConnHandle, ProtocolVersion, ErrorDescriptor]),
    no_reply.

%%----------------------------------------------------------------------
%% Invoked for each transaction request
%%----------------------------------------------------------------------

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests) ->
    d("handle_trans_request -> entry with" 
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ActionRequests:  ~p", 
      [ConnHandle, ProtocolVersion, ActionRequests]),
    ED =  #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                             errorText = "Transaction requests not handled"},
    {discard_ack, ED}.

%%----------------------------------------------------------------------
%% Optionally invoked for a time consuming transaction request
%%----------------------------------------------------------------------

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData) ->
    d("handle_trans_long_request -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ReqData:         ~p", [ConnHandle, ProtocolVersion, ReqData]),
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                            errorText = "Long transaction requests not handled"},
    {discard_ack,  ED}.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction reply
%%----------------------------------------------------------------------

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData) ->
    d("handle_trans_reply -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ActualReply:     ~p"
      "~n   ReplyData:       ~p", 
      [ConnHandle, ProtocolVersion, ActualReply, ReplyData]),
    ok.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction acknowledgement
%%----------------------------------------------------------------------

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData) ->
    d("handle_trans_ack -> entry with"
       "~n   ConnHandle:      ~p"
       "~n   ProtocolVersion: ~p"
       "~n   AckStatus:       ~p"
       "~n   AckData:         ~p", 
      [ConnHandle, ProtocolVersion, AckStatus, AckData]),
    ok.


%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    d("handle_unexpected_trans -> entry with"
       "~n   ConnHandle:      ~p"
       "~n   ProtocolVersion: ~p"
       "~n   AckStatus:       ~p"
       "~n   AckData:         ~p", 
      [ConnHandle, ProtocolVersion, Trans]),
    ok.


%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_trans_request_abort(ConnHandle, ProtocolVersion, TransId, Pid) ->
    d("handle_trans_request_abort -> entry with"
       "~n   ConnHandle:      ~p"
       "~n   ProtocolVersion: ~p"
       "~n   TransId:         ~p"
       "~n   Pid:             ~p", 
      [ConnHandle, ProtocolVersion, TransId, Pid]),
    ok.


%%----------------------------------------------------------------------
%% DEBUGGING
%%----------------------------------------------------------------------

d(F) ->
    d(F, []).

d(F,A) ->
    d(get(debug),F,A).

d(true,F,A) ->
    io:format("SIMPLE_MG: " ++ F ++ "~n", A);
d(_, _F, _A) ->
    ok.




