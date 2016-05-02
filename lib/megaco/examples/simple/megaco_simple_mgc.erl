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
%% Purpose: Simple example of an MGC
%% 
%% Example usage:
%%
%%   cd megaco/examples/simple
%%   erl -pa ../../../megaco/ebin -s megaco_filter -s megaco
%%   megaco_simple_mgc:start().
%%----------------------------------------------------------------------

-module(megaco_simple_mgc).

-behaviour(megaco_user).

-export([
	 start_batch/0, start_batch/1, init_batch/3,
	 start/0, start/2, 
	 start/4,
	 stop/0, stop/1
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
%% Starting the MGC
%%----------------------------------------------------------------------

start() ->
    start(false, false).

start(Trace, Debug) ->
    start({deviceName, "controller"}, [], Trace, Debug).

start(Mid, Config, Trace, Debug) ->
    put(debug, Debug),
    d("start -> entry with"
      "~n   Mid:    ~p"
      "~n   Config: ~p"
      "~n   Trace:  ~p", [Mid, Config, Trace]),
    init_inline_trace(Trace),
    case megaco:start_user(Mid, [{user_mod, ?MODULE} | Config]) of
	ok ->
	    d("start -> user started"),
	    case catch do_start(Mid) of
		{'EXIT', Reason} ->
		    d("start -> exited: ~n~p",[Reason]),
		    {error, Reason};
		Other ->
		    d("start -> Other: ~n~p",[Other]),
		    Other
	    end;
	{error, Reason} ->
	    d("start -> user start failed: ~n~p", [Reason]),
	    {error, {start_user, Reason}}
    end.


%% -----------------------------------------------------------------------

init_inline_trace(true) ->
    megaco:enable_trace(max, io);
init_inline_trace(_) ->
    ok.

%% -----------------------------------------------------------------------

do_start(Mid) ->
    d("do_start -> entry"),
    RecHandle = megaco:user_info(Mid, receive_handle),
    d("do_start -> RecHandle: ~n~p",[RecHandle]),

    TextMod  = megaco_pretty_text_encoder,
    TextTcp  = RecHandle#megaco_receive_handle{encoding_mod    = TextMod,
					       encoding_config = [],
					       send_mod        = megaco_tcp},
    d("do_start -> TextTcp: ~n~p",[TextTcp]),
    TextUdp  = TextTcp#megaco_receive_handle{send_mod = megaco_udp},
    d("do_start -> TextUdp: ~n~p",[TextUdp]),

    BinMod  = megaco_binary_encoder,
    BinTcp  = RecHandle#megaco_receive_handle{encoding_mod    = BinMod,
					      encoding_config = [],
					      send_mod        = megaco_tcp},
    d("do_start -> BinTcp: ~n~p",[BinTcp]),
    BinUdp  = BinTcp#megaco_receive_handle{send_mod = megaco_udp},
    d("do_start -> BinUdp: ~n~p",[BinUdp]),

    ListenTo = [{?megaco_ip_port_text, TextTcp},
		{?megaco_ip_port_text, TextUdp},
		{?megaco_ip_port_binary,  BinTcp},
		{?megaco_ip_port_binary,  BinUdp}
	       ],
    
    d("do_start -> start transports"),
    Transports =
	[{start_transport(Port, RH), Port, RH} || {Port, RH} <- ListenTo],
    d("do_start -> Transports: ~n~p",[Transports]),
    
    {ok, Transports}.

start_transport(MgcPort, RecHandle) ->
    case RecHandle#megaco_receive_handle.send_mod of
	megaco_tcp -> start_tcp(MgcPort, RecHandle);
	megaco_udp -> start_udp(MgcPort, RecHandle);
	SendMod    -> {error, {bad_send_mod, SendMod}}
    end.

start_udp(MgcPort, RecHandle) ->
    d("start_udp -> entry with"
      "~n   MgcPort:   ~p"
      "~n   RecHandle: ~p", [MgcPort, RecHandle]),
    case megaco_udp:start_transport() of
	{ok, SupPid} ->
	    Options = [{port, MgcPort}, {receive_handle, RecHandle}],
	    case megaco_udp:open(SupPid, Options) of
		{ok, _SendHandle, _ControlPid} ->
		    ok;
		{error, Reason} ->
		    {error, {megaco_udp_open, Reason}}
	    end;
	{error, Reason} ->
	    {error, {megaco_udp_start_transport, Reason}}
    end.

start_tcp(MgcPort, RecHandle) ->
    d("start_tcp -> entry with"
      "~n   MgcPort:   ~p"
      "~n   RecHandle: ~p", [MgcPort, RecHandle]),
    case megaco_tcp:start_transport() of
	{ok, SupPid} ->
	    d("start_tcp -> transport started: "
	      "~n   SupPid: ~p", [SupPid]),
	    Options = [{port, MgcPort}, {receive_handle, RecHandle}],
	    case megaco_tcp:listen(SupPid, Options) of
		ok ->
		    d("start_tcp -> listen ok"),
		    ok;
		{error, Reason} ->
		    d("start_tcp -> listen failed: "
		      "~n   Reason: ~p", [Reason]),
		    {error, {megaco_tcp_listen, Reason}}
	    end;
	{error, Reason} ->
	    d("start_tcp -> transport start failed: "
	      "~n   Reason: ~p", [Reason]),
	    {error, {megaco_tcp_start_transport, Reason}}
    end.

%%----------------------------------------------------------------------
%% Stopping the MGC
%%----------------------------------------------------------------------

stop() ->
    [{Mid, stop(Mid)} || Mid <- megaco:system_info(users)].

stop(Mid) ->
    d("stop -> entry with~n   Mid: ~p", [Mid]),
    Disco = fun(CH) ->
		    d("stop -> CH: ~p", [CH]),
		    Reason = stopped_by_user, 
		    Pid = megaco:conn_info(CH, control_pid),
		    SendMod = megaco:conn_info(CH, send_mod),
		    SendHandle = megaco:conn_info(CH, send_handle),

		    d("stop -> disconnect", []),
		    megaco:disconnect(CH, Reason),
		    d("stop -> cancel", []),
		    megaco:cancel(CH, Reason),
		    d("stop -> close transport"
		      "~n   SendMod:    ~p"
		      "~n   SendHandle: ~p", [SendMod, SendHandle]),
		    case SendMod of
			megaco_tcp -> megaco_tcp:close(SendHandle);
			megaco_udp -> megaco_udp:close(SendHandle);
			SendMod    -> exit(Pid, Reason)
		    end
	    end,
    Conns = megaco:user_info(Mid, connections),
    d("stop -> Conns: ~p", [Conns]),
    Disconns = lists:map(Disco, Conns),
    d("stop -> Disconns: ~p", [Disconns]),
    megaco:stop_user(Mid),
    case whereis(?MODULE) of
	undefined ->
	    ignore;
	Pid ->
	    d("stop -> Pid: ~p", [Pid]),
	    unlink(Pid),
	    exit(Pid, shutdown)
    end,
    ok.

%%----------------------------------------------------------------------
%% Invoked when a new connection is established
%%----------------------------------------------------------------------

handle_connect(ConnHandle, ProtocolVersion) ->
    d("handle_connect -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "", [ConnHandle, ProtocolVersion]),
    ok.

%%----------------------------------------------------------------------
%% Invoked when a connection is teared down
%%----------------------------------------------------------------------

handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    d("handle_disconnect -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   Reason:          ~p"
      "", [ConnHandle, ProtocolVersion, Reason]),
    megaco:cancel(ConnHandle, Reason), % Cancel the outstanding messages
    ok.

%%----------------------------------------------------------------------
%% Invoked when  a received message had syntax errors
%%----------------------------------------------------------------------

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor) ->
    d("handle_syntax_error -> entry with"
      "~n   ReceiveHandle:   ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ErrorDescriptor: ~p"
      "", [ReceiveHandle, ProtocolVersion, ErrorDescriptor]),
    reply.

%%----------------------------------------------------------------------
%% Invoked when a received message contained no transactions
%%----------------------------------------------------------------------

handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor) ->
    d("handle_message_error -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ErrorDescriptor: ~p"
      "", [ConnHandle, ProtocolVersion, ErrorDescriptor]),
    no_reply.

%%----------------------------------------------------------------------
%% Invoked for each transaction request
%%----------------------------------------------------------------------

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests) ->
    d("handle_trans_request -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ActionRequests:  ~p"
      "", [ConnHandle, ProtocolVersion, ActionRequests]),
    ED =  #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                             errorText = "Only single service change on null context handled"},
    case ActionRequests of
	[AR] ->
	    ContextId = AR#'ActionRequest'.contextId,
	    case AR#'ActionRequest'.commandRequests of
		[CR] when ContextId =:= ?megaco_null_context_id ->
		    case CR#'CommandRequest'.command of
			{serviceChangeReq, Req} ->
			    Rep = service_change(ConnHandle, ProtocolVersion, Req),
			    {discard_ack,
			     [#'ActionReply'{contextId = ContextId,
					     commandReply = [{serviceChangeReply, Rep}]}]};
			_ ->
			    {discard_ack, ED}
		    end;
		_ ->
		    {discard_ack, ED}
	    end;
	_ ->
	    {discard_ack, ED}
    end.

service_change(ConnHandle, _ProtocolVersion, SCR) ->
    SCP = SCR#'ServiceChangeRequest'.serviceChangeParms,
    #'ServiceChangeParm'{serviceChangeAddress = Address,
			 serviceChangeProfile = Profile} = SCP,
    TermId = SCR#'ServiceChangeRequest'.terminationID,
    if
	TermId == [?megaco_root_termination_id] ->
	    MyMid = ConnHandle#megaco_conn_handle.local_mid,
	    Res = {serviceChangeResParms,
		   #'ServiceChangeResParm'{serviceChangeMgcId   = MyMid,
					   serviceChangeAddress = Address,
					   serviceChangeProfile = Profile}},
	    #'ServiceChangeReply'{terminationID       = TermId,
				  serviceChangeResult = Res};
	true ->
	    Res = {errorDescriptor,
		   #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
				      errorText = "Only handled for root"}},
	    
	     #'ServiceChangeReply'{terminationID       = TermId,
				   serviceChangeResult = Res}
    end.
	
%%----------------------------------------------------------------------
%% Optionally invoked for a time consuming transaction request
%%----------------------------------------------------------------------

handle_trans_long_request(_ConnHandle, _ProtocolVersion, _ReqData) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
			    errorText = "Long transaction requests not handled"},
    {discard_ack, ED}.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction reply
%%----------------------------------------------------------------------

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData) ->
    d("handle_trans_eply -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ActualReply:     ~p"
      "~n   ReplyData:       ~p"
      "", [ConnHandle, ProtocolVersion, ActualReply, ReplyData]),
    ok.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction acknowledgement
%%----------------------------------------------------------------------

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData) ->
    d("handle_trans_ack -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   ckStatus:        ~p"
      "~n   AckData:         ~p"
      "", [ConnHandle, ProtocolVersion, AckStatus, AckData]),
    ok.

%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    d("handle_unexpected_trans -> entry with"
      "~n   ConnHandle:      ~p"
      "~n   ProtocolVersion: ~p"
      "~n   Trans:           ~p"
      "", [ConnHandle, ProtocolVersion, Trans]),
    ok.


%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_trans_request_abort(_ConnHandle, _ProtocolVersion, _TransId, _Pid) ->
    ok.

%%----------------------------------------------------------------------
%% To be used at command line: erl -s ?MODULE start_batch
%%----------------------------------------------------------------------

start_batch() ->
    start_batch([false]).

start_batch(Args0) ->
    Defs  = [{trace,false}, {debug, false}],
    Args  = parse_args(Args0, Defs),
    Trace = get_arg(trace, Args),
    Debug = get_arg(debug, Args),
    Pid = spawn(?MODULE, init_batch, [self(), Trace, Debug]),
    receive
	{init_batch, Pid, Res} ->
	    io:format("~p(~p): ~p~n", [?MODULE, ?LINE, Res]),
	    Res
    end.
	    
init_batch(ReplyTo, Trace, Debug) ->
    register(?MODULE, self()),
    Res = start(Trace, Debug),
    ReplyTo ! {init_batch, self(), Res},
    receive
    after infinity -> Res
    end.


parse_args([], Acc) ->
    Acc;
parse_args([Arg|Args], Acc) when is_atom(Arg) ->
    case string:tokens(atom_to_list(Arg),"{},") of
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


%%----------------------------------------------------------------------
%% DEBUGGING
%%----------------------------------------------------------------------

d(F) ->
    d(F, []).

d(F,A) ->
    d(get(debug),F,A).

d(true,F,A) ->
    io:format("SIMPLE_MGC: " ++ F ++ "~n", A);
d(_, _F, _A) ->
    ok.

