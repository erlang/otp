%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
    [handle_event_function,
     state_enter].

%%--------------------------------------------------------------------

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
    {ok, KexdhReply, Ssh1} = ssh_transport:handle_kexdh_init(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexdhReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kexdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kexdh_reply(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- diffie-hellman group exchange
handle_event(internal, #ssh_msg_kex_dh_gex_request{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, GexGroup, Ssh1} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(GexGroup, D),
    Ssh = ssh_transport:parallell_gen_key(Ssh1),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kex_dh_gex_request_old{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, GexGroup, Ssh1} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(GexGroup, D),
    Ssh = ssh_transport:parallell_gen_key(Ssh1),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kex_dh_gex_group{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, KexGexInit, Ssh} = ssh_transport:handle_kex_dh_gex_group(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexGexInit, D),
    {next_state, {key_exchange_dh_gex_reply,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- elliptic curve diffie-hellman
handle_event(internal, #ssh_msg_kex_ecdh_init{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, KexEcdhReply, Ssh1} = ssh_transport:handle_kex_ecdh_init(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexEcdhReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(internal, #ssh_msg_kex_ecdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_ecdh_reply(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange_dh_gex_init, server, init|renegotiate} ####

handle_event(internal, #ssh_msg_kex_dh_gex_init{} = Msg, {key_exchange_dh_gex_init,server,ReNeg}, D) ->
    {ok, KexGexReply, Ssh1} =  ssh_transport:handle_kex_dh_gex_init(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(KexGexReply, D),
    {ok, NewKeys, Ssh2} = ssh_transport:new_keys_message(Ssh1),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh2),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange_dh_gex_reply, client, init|renegotiate} ####

handle_event(internal, #ssh_msg_kex_dh_gex_reply{} = Msg, {key_exchange_dh_gex_reply,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_dh_gex_reply(Msg, D#data.ssh_params),
    ssh_connection_handler:send_bytes(NewKeys, D),
    {ok, ExtInfo, Ssh} = ssh_transport:ext_info_message(Ssh1),
    ssh_connection_handler:send_bytes(ExtInfo, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};


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

