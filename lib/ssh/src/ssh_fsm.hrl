%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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

%%====================================================================
%% Types
%%====================================================================
-type connection_ref() :: ssh:connection_ref().
-type channel_id()     :: ssh:channel_id().

%%====================================================================
%% Internal process state
%%====================================================================
-record(data, {
	  starter                               :: pid()
						 | undefined,
	  auth_user                             :: string()
						 | undefined,
	  connection_state                      :: #connection{}
						 | undefined,
	  latest_channel_id         = 0         :: non_neg_integer()
                                                 | undefined,
	  transport_protocol                    :: atom()
                                                 | undefined,	% ex: tcp
	  transport_cb                          :: atom()
                                                 | undefined,	% ex: gen_tcp
	  transport_close_tag                   :: atom()
                                                 | undefined,	% ex: tcp_closed
	  ssh_params                            :: #ssh{}
                                                 | undefined,
	  socket                                :: gen_tcp:socket()
                                                 | undefined,
	  decrypted_data_buffer     = <<>>      :: binary()
                                                 | undefined,
	  encrypted_data_buffer     = <<>>      :: binary()
                                                 | undefined,
	  aead_data                 = <<>>      :: binary()
                                                 | undefined,
	  undecrypted_packet_length             :: undefined | non_neg_integer(),
	  key_exchange_init_msg                 :: #ssh_msg_kexinit{}
						 | undefined,
	  last_size_rekey           = 0         :: non_neg_integer(),
	  event_queue               = []        :: list(),
	  inet_initial_recbuf_size              :: pos_integer()
						 | undefined
	 }).


%%====================================================================
%% Macros
%%====================================================================
-define(send_disconnect(Code, DetailedText, StateName, State),
        ssh_connection_handler:send_disconnect(Code, DetailedText, ?MODULE, ?LINE, StateName, State)).

-define(send_disconnect(Code, Reason, DetailedText, StateName, State),
        ssh_connection_handler:send_disconnect(Code, Reason, DetailedText, ?MODULE, ?LINE, StateName, State)).


-define(CALL_FUN(Key,D), catch (?GET_OPT(Key, (D#data.ssh_params)#ssh.opts)) ).

-define(role(StateName), element(2,StateName)).
