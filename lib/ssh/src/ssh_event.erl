%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Support functionality for the ssh_event_funs option
%% ----------------------------------------------------------------------

-module(ssh_event).
-moduledoc false.

-export([message_received/2, connected/2, disconnected/2]).

-export([disconnectfun/1, connectfun/1]).

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include("ssh_auth.hrl").
-include("ssh_transport.hrl").

-include("ssh_fsm.hrl").

message_received(Msg, D) ->
    Context = #{msg_type => msg_type(Msg),
                msg_type_name => element(1, Msg)
               },
    process_event(?FUNCTION_NAME, Context, D).

connected(Method, D) ->
    Context = #{user_auth => Method},
    handle_legacy_fun(connectfun, ?FUNCTION_NAME, Context, D),
    process_event(?FUNCTION_NAME, Context, D).

disconnected(Context, D) ->
    handle_legacy_fun(disconnectfun, ?FUNCTION_NAME, Context, D),
    process_event(?FUNCTION_NAME, Context, D).

process_event(Event, EventSpecificContext, D) ->
    EventFuns = ?GET_OPT(event_funs, (D#data.ssh_params)#ssh.opts),
    case maps:get(Event, EventFuns, undefined) of
        undefined -> ok;
        EventFun -> process_event(Event, EventSpecificContext, D, EventFun)
    end.

process_event(Event, EventSpecificContext, D, EventFun) ->
    ConnInfo = ssh_connection_handler:connection_info_server(D),
    Role = (D#data.ssh_params)#ssh.role,
    Context = EventSpecificContext#{connection_ref => self(),
                                    connection_info => ConnInfo,
                                    role => Role},
    EventFun(Event, Context).

handle_legacy_fun(FunName, Event, Context, D) ->
    LegacyFun = ?GET_OPT(FunName, (D#data.ssh_params)#ssh.opts),
    EventFun = ?MODULE:FunName(LegacyFun),
    process_event(Event, Context, D, EventFun).

disconnectfun(Fun) ->
    fun(disconnected, #{description := Desc}) ->
            Fun(Desc)
    end.

connectfun(Fun) ->
    Fun3 =
        fun(connected, #{user_auth := Method, connection_info := ConnInfo}) ->
                User = proplists:get_value(user, ConnInfo),
                %% For connectFun this is how the peer is constructed
            {_,Peer} = proplists:get_value(peer, ConnInfo),
                Fun(User, Peer, Method)
        end,
    Fun4 =
        fun(connected, #{user_auth := Method, connection_info := ConnInfo}) ->
                User = proplists:get_value(user, ConnInfo),
                %% For connectFun this is how the peer is constructed
            {_,Peer} = proplists:get_value(peer, ConnInfo),
                Fun(User, Peer, Method, ConnInfo)
        end,
    case erlang:fun_info(Fun, arity) of
        {arity, 3} ->
            Fun3;
        {arity, 4} ->
            Fun4
    end.


%%%================================================================
%%%
%%% msg_type/1  – given a message record, return its SSH packet type byte
%%%
%%% msg_record_tag/1 – given a packet type byte, return the record
%%%                    tag atom (or a list of candidates for shared
%%%                    type bytes).
%%%
%%% Note: several KEX record types share type bytes 30/31/32/33/34
%%% because the KEX family is chosen by algorithm negotiation, not
%%% by the type byte alone.

%% Transport layer (RFC 4253)
msg_type(#ssh_msg_disconnect{})           -> ?SSH_MSG_DISCONNECT;        % 1
msg_type(#ssh_msg_ignore{})               -> ?SSH_MSG_IGNORE;             % 2
msg_type(#ssh_msg_unimplemented{})        -> ?SSH_MSG_UNIMPLEMENTED;      % 3
msg_type(#ssh_msg_debug{})                -> ?SSH_MSG_DEBUG;              % 4
msg_type(#ssh_msg_service_request{})      -> ?SSH_MSG_SERVICE_REQUEST;    % 5
msg_type(#ssh_msg_service_accept{})       -> ?SSH_MSG_SERVICE_ACCEPT;     % 6
msg_type(#ssh_msg_ext_info{})             -> ?SSH_MSG_EXT_INFO;           % 7
msg_type(#ssh_msg_kexinit{})              -> ?SSH_MSG_KEXINIT;            % 20
msg_type(#ssh_msg_newkeys{})              -> ?SSH_MSG_NEWKEYS;            % 21
%% KEX messages – all families reuse 30/31 (and 32/33/34 for GEX)
msg_type(#ssh_msg_kexdh_init{})           -> ?SSH_MSG_KEXDH_INIT;        % 30
msg_type(#ssh_msg_kexdh_reply{})          -> ?SSH_MSG_KEXDH_REPLY;       % 31
msg_type(#ssh_msg_kex_dh_gex_request_old{}) -> ?SSH_MSG_KEX_DH_GEX_REQUEST_OLD; % 30
msg_type(#ssh_msg_kex_dh_gex_group{})    -> ?SSH_MSG_KEX_DH_GEX_GROUP;  % 31
msg_type(#ssh_msg_kex_dh_gex_init{})     -> ?SSH_MSG_KEX_DH_GEX_INIT;   % 32
msg_type(#ssh_msg_kex_dh_gex_reply{})    -> ?SSH_MSG_KEX_DH_GEX_REPLY;  % 33
msg_type(#ssh_msg_kex_dh_gex_request{})  -> ?SSH_MSG_KEX_DH_GEX_REQUEST;% 34
msg_type(#ssh_msg_kex_ecdh_init{})       -> ?SSH_MSG_KEX_ECDH_INIT;     % 30
msg_type(#ssh_msg_kex_ecdh_reply{})      -> ?SSH_MSG_KEX_ECDH_REPLY;    % 31
msg_type(#ssh_msg_kex_hybrid_init{})     -> ?SSH_MSG_KEX_HYBRID_INIT;   % 30
msg_type(#ssh_msg_kex_hybrid_reply{})    -> ?SSH_MSG_KEX_HYBRID_REPLY;  % 31
%% User-authentication layer (RFC 4252)
msg_type(#ssh_msg_userauth_request{})    -> ?SSH_MSG_USERAUTH_REQUEST;   % 50
msg_type(#ssh_msg_userauth_failure{})    -> ?SSH_MSG_USERAUTH_FAILURE;   % 51
msg_type(#ssh_msg_userauth_success{})    -> ?SSH_MSG_USERAUTH_SUCCESS;   % 52
msg_type(#ssh_msg_userauth_banner{})     -> ?SSH_MSG_USERAUTH_BANNER;    % 53
msg_type(#ssh_msg_userauth_pk_ok{})      -> ?SSH_MSG_USERAUTH_PK_OK;     % 60
msg_type(#ssh_msg_userauth_passwd_changereq{}) -> ?SSH_MSG_USERAUTH_PASSWD_CHANGEREQ; % 60
msg_type(#ssh_msg_userauth_info_request{}) -> ?SSH_MSG_USERAUTH_INFO_REQUEST;  % 60
msg_type(#ssh_msg_userauth_info_response{}) -> ?SSH_MSG_USERAUTH_INFO_RESPONSE; % 61
%% Connection layer (RFC 4254)
msg_type(#ssh_msg_global_request{})      -> ?SSH_MSG_GLOBAL_REQUEST;     % 80
msg_type(#ssh_msg_request_success{})     -> ?SSH_MSG_REQUEST_SUCCESS;    % 81
msg_type(#ssh_msg_request_failure{})     -> ?SSH_MSG_REQUEST_FAILURE;    % 82
msg_type(#ssh_msg_channel_open{})        -> ?SSH_MSG_CHANNEL_OPEN;       % 90
msg_type(#ssh_msg_channel_open_confirmation{}) -> ?SSH_MSG_CHANNEL_OPEN_CONFIRMATION; % 91
msg_type(#ssh_msg_channel_open_failure{}) -> ?SSH_MSG_CHANNEL_OPEN_FAILURE; % 92
msg_type(#ssh_msg_channel_window_adjust{}) -> ?SSH_MSG_CHANNEL_WINDOW_ADJUST; % 93
msg_type(#ssh_msg_channel_data{})        -> ?SSH_MSG_CHANNEL_DATA;       % 94
msg_type(#ssh_msg_channel_extended_data{}) -> ?SSH_MSG_CHANNEL_EXTENDED_DATA; % 95
msg_type(#ssh_msg_channel_eof{})         -> ?SSH_MSG_CHANNEL_EOF;        % 96
msg_type(#ssh_msg_channel_close{})       -> ?SSH_MSG_CHANNEL_CLOSE;      % 97
msg_type(#ssh_msg_channel_request{})     -> ?SSH_MSG_CHANNEL_REQUEST;    % 98
msg_type(#ssh_msg_channel_success{})     -> ?SSH_MSG_CHANNEL_SUCCESS;    % 99
msg_type(#ssh_msg_channel_failure{})     -> ?SSH_MSG_CHANNEL_FAILURE.    % 100
