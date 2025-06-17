%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% Purpose: Handles an ssh connection, e.i. both the
%% setup SSH Transport Layer Protocol (RFC 4253), Authentication
%% Protocol (RFC 4252) and SSH connection Protocol (RFC 4255)
%% Details of the different protocols are
%% implemented in ssh_transport.erl, ssh_auth.erl and ssh_connection.erl
%% ----------------------------------------------------------------------

-module(ssh_fsm_kexinit).
-moduledoc false.

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_auth.hrl").
-include("ssh_connect.hrl").

-include("ssh_fsm.hrl").

%%====================================================================
%%% Exports
%%====================================================================

%%% Behaviour callbacks
-export([callback_mode/0, handle_event/4, terminate/3,
	 format_status/2, code_change/4]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1,
         ssh_dbg_on/1, ssh_dbg_off/1,
         ssh_dbg_format/2]).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
    [handle_event_function,
     state_enter].

%%--------------------------------------------------------------------


handle_event(Type, Event = prepare_next_packet, StateName, D) ->
    ssh_connection_handler:handle_event(Type, Event, StateName, D);
handle_event(Type, Event = {send_disconnect, _, _, _, _}, StateName, D) ->
    ssh_connection_handler:handle_event(Type, Event, StateName, D);

%%% ######## {kexinit, client|server, init|renegotiate} ####
handle_event(internal, {#ssh_msg_kexinit{}=Kex, Payload}, {kexinit,Role,ReNeg},
	     D = #data{key_exchange_init_msg = OwnKex}) ->
    Ssh1 = ssh_transport:key_init(peer_role(Role), D#data.ssh_params, Payload),
    Ssh = case ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1, ReNeg) of
	      {ok, NextKexMsg, Ssh2} when Role==client ->
		  ssh_connection_handler:send_bytes(NextKexMsg, D),
		  Ssh2;
	      {ok, Ssh2} when Role==server ->
		  Ssh2
	  end,
    {next_state, {key_exchange,Role,ReNeg}, D#data{ssh_params=Ssh}};

%%% ######## {key_exchange, client|server, init|renegotiate} ####
%%%---- diffie-hellman
handle_event(internal, #ssh_msg_kexdh_init{} = Msg, {key_exchange,server,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, KexdhReply, Ssh1} = ssh_transport:handle_kexdh_init(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexdhReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kexdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kexdh_reply(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- diffie-hellman group exchange
handle_event(internal, #ssh_msg_kex_dh_gex_request{} = Msg, {key_exchange,server,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, GexGroup, Ssh1} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(GexGroup, D),
    Ssh = ssh_transport:parallell_gen_key(Ssh1),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kex_dh_gex_request_old{} = Msg, {key_exchange,server,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, GexGroup, Ssh1} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(GexGroup, D),
    Ssh = ssh_transport:parallell_gen_key(Ssh1),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kex_dh_gex_group{} = Msg, {key_exchange,client,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, KexGexInit, Ssh} = ssh_transport:handle_kex_dh_gex_group(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexGexInit, D),
    {next_state, {key_exchange_dh_gex_reply,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- elliptic curve diffie-hellman
handle_event(internal, #ssh_msg_kex_ecdh_init{} = Msg, {key_exchange,server,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, KexEcdhReply, Ssh1} = ssh_transport:handle_kex_ecdh_init(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexEcdhReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kex_ecdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_ecdh_reply(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};

%%% ######## handle KEX strict
handle_event(internal, _Event, {key_exchange,_Role,init},
             #data{ssh_params = #ssh{algorithms = #alg{kex_strict_negotiated = true},
                                     send_sequence = SendSeq,
                                     recv_sequence = RecvSeq}}) ->
    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                io_lib:format("KEX strict violation: send_sequence = ~p  recv_sequence = ~p",
                              [SendSeq, RecvSeq]));

%%% ######## {key_exchange_dh_gex_init, server, init|renegotiate} ####
handle_event(internal, #ssh_msg_kex_dh_gex_init{} = Msg, {key_exchange_dh_gex_init,server,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, KexGexReply, Ssh1} =  ssh_transport:handle_kex_dh_gex_init(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexGexReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};
%%% ######## handle KEX strict
handle_event(internal, _Event, {key_exchange_dh_gex_init,_Role,init},
             #data{ssh_params = #ssh{algorithms = #alg{kex_strict_negotiated = true},
                                     send_sequence = SendSeq,
                                     recv_sequence = RecvSeq}}) ->
    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                io_lib:format("KEX strict violation: send_sequence = ~p  recv_sequence = ~p",
                              [SendSeq, RecvSeq]));

%%% ######## {key_exchange_dh_gex_reply, client, init|renegotiate} ####
handle_event(internal, #ssh_msg_kex_dh_gex_reply{} = Msg, {key_exchange_dh_gex_reply,client,ReNeg}, D) ->
    ok = check_kex_strict(Msg, D),
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_dh_gex_reply(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};
%%% ######## handle KEX strict
handle_event(internal, _Event, {key_exchange_dh_gex_reply,_Role,init},
             #data{ssh_params = #ssh{algorithms = #alg{kex_strict_negotiated = true},
                                     send_sequence = SendSeq,
                                     recv_sequence = RecvSeq}}) ->
    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                io_lib:format("KEX strict violation: send_sequence = ~p  recv_sequence = ~p",
                              [SendSeq, RecvSeq]));

%%% ######## {new_keys, client|server} ####
%% First key exchange round:
handle_event(internal, #ssh_msg_newkeys{} = Msg, {new_keys,client,init}, D0) ->
    {ok, Ssh1} = ssh_transport:handle_new_keys(Msg, D0#data.ssh_params),
    %% {ok, ExtInfo, Ssh2} = ssh_transport:ext_info_message(Ssh1),
    %% ssh_connection_handler:send_bytes(ExtInfo, D0),
    {MsgReq, Ssh} = ssh_auth:service_request_msg(Ssh1),
    D = ssh_connection_handler:send_msg(MsgReq, D0#data{ssh_params = Ssh}),
    {next_state, {ext_info,client,init}, D};

handle_event(internal, #ssh_msg_newkeys{} = Msg, {new_keys,server,init}, D) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    %% {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    %% ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {ext_info,server,init}, D#data{ssh_params=Ssh}};

%%% ######## handle KEX strict
handle_event(internal, _Event, {new_keys,_Role,init},
             #data{ssh_params = #ssh{algorithms = #alg{kex_strict_negotiated = true},
                                     send_sequence = SendSeq,
                                     recv_sequence = RecvSeq}}) ->
    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                io_lib:format("KEX strict violation (send_sequence = ~p recv_sequence = ~p)",
                              [SendSeq, RecvSeq]));

%% Subsequent key exchange rounds (renegotiation):
handle_event(internal, #ssh_msg_newkeys{} = Msg, {new_keys,Role,renegotiate}, D) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    %% {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    %% ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {ext_info,Role,renegotiate}, D#data{ssh_params=Ssh}};


%%% ######## {ext_info, client|server, init|renegotiate} ####

handle_event(internal, #ssh_msg_ext_info{}=Msg, {ext_info,Role,init}, D0) ->
    D = ssh_connection_handler:handle_ssh_msg_ext_info(Msg, D0),
    {next_state, {service_request,Role}, D, {change_callback_module,ssh_connection_handler}};

handle_event(internal, #ssh_msg_ext_info{}=Msg, {ext_info,Role,renegotiate}, D0) ->
    D = ssh_connection_handler:handle_ssh_msg_ext_info(Msg, D0),
    {next_state, {connected,Role}, D, {change_callback_module,ssh_connection_handler}};

handle_event(internal, #ssh_msg_newkeys{}=Msg, {ext_info,_Role,renegotiate}, D) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    {keep_state, D#data{ssh_params = Ssh}};

handle_event(internal, Msg, {ext_info,Role,init}, D) when is_tuple(Msg) ->
    %% If something else arrives, goto next state and handle the event in that one
    {next_state, {service_request,Role}, D, [postpone, {change_callback_module,ssh_connection_handler}]};

handle_event(internal, Msg, {ext_info,Role,renegotiate}, D) when is_tuple(Msg) ->
    %% If something else arrives, goto next state and handle the event in that one
    {next_state, {connected,Role}, D, [postpone, {change_callback_module,ssh_connection_handler}]};

%%% ######## UNHANDLED EVENT!
handle_event(Type, Event, StateName, D) ->
    ssh_connection_handler:handle_event(Type, Event, StateName, D).

%%--------------------------------------------------------------------
format_status(A, B) ->
    ssh_connection_handler:format_status(A, B).

%%--------------------------------------------------------------------
terminate(Reason, StateName, D) ->
    ssh_connection_handler:terminate(Reason, StateName, D).

%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% "Invert" the Role
peer_role(client) -> server;
peer_role(server) -> client.

check_kex_strict(Msg,
                 #data{ssh_params =
                           #ssh{algorithms =
                                    #alg{
                                       kex = Kex,
                                       kex_strict_negotiated = KexStrictNegotiated},
                                send_sequence = SendSeq,
                                recv_sequence = RecvSeq}}) ->
    case check_msg_group(Msg, get_alg_group(Kex), KexStrictNegotiated) of
        ok ->
            ok;
        error ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("KEX strict violation: send_sequence = ~p  recv_sequence = ~p",
                                      [SendSeq, RecvSeq]))
    end.

get_alg_group(Kex) when Kex == 'diffie-hellman-group16-sha512';
                        Kex == 'diffie-hellman-group18-sha512';
                        Kex == 'diffie-hellman-group14-sha256';
                        Kex == 'diffie-hellman-group14-sha1';
                        Kex == 'diffie-hellman-group1-sha1' ->
    dh_alg;
get_alg_group(Kex) when Kex == 'diffie-hellman-group-exchange-sha256';
                        Kex == 'diffie-hellman-group-exchange-sha1' ->
    dh_gex_alg;
get_alg_group(Kex) when Kex == 'curve25519-sha256';
                        Kex == 'curve25519-sha256@libssh.org';
                        Kex == 'curve448-sha512';
                        Kex == 'ecdh-sha2-nistp521';
                        Kex == 'ecdh-sha2-nistp384';
                        Kex == 'ecdh-sha2-nistp256' ->
    ecdh_alg.

check_msg_group(_Msg, _AlgGroup, false) -> ok;
check_msg_group(#ssh_msg_kexdh_init{},  dh_alg, true) -> ok;
check_msg_group(#ssh_msg_kexdh_reply{}, dh_alg, true) -> ok;
check_msg_group(#ssh_msg_kex_dh_gex_request_old{}, dh_gex_alg, true) -> ok;
check_msg_group(#ssh_msg_kex_dh_gex_request{},     dh_gex_alg, true) -> ok;
check_msg_group(#ssh_msg_kex_dh_gex_group{},       dh_gex_alg, true) -> ok;
check_msg_group(#ssh_msg_kex_dh_gex_init{},        dh_gex_alg, true) -> ok;
check_msg_group(#ssh_msg_kex_dh_gex_reply{},       dh_gex_alg, true) -> ok;
check_msg_group(#ssh_msg_kex_ecdh_init{},  ecdh_alg, true) -> ok;
check_msg_group(#ssh_msg_kex_ecdh_reply{}, ecdh_alg, true) -> ok;
check_msg_group(_Msg, _AlgGroup, _) -> error.

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [connection_events].

ssh_dbg_flags(connection_events) -> [c].

ssh_dbg_on(connection_events) -> dbg:tp(?MODULE,   handle_event, 4, x).

ssh_dbg_off(connection_events) -> dbg:ctpg(?MODULE, handle_event, 4).

ssh_dbg_format(connection_events, {call, {?MODULE,handle_event, [EventType, EventContent, State, _Data]}}) ->
    ["Connection event\n",
     io_lib:format("[~w] EventType: ~p~nEventContent: ~p~nState: ~p~n", [?MODULE, EventType, EventContent, State])
    ];
ssh_dbg_format(connection_events, {return_from, {?MODULE,handle_event,4}, Ret}) ->
    ["Connection event result\n",
     io_lib:format("[~w] ~p~n", [?MODULE, ssh_dbg:reduce_state(Ret, #data{})])
    ].
