%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%

-module(ssl_record).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").

-export([empty_connection_state/1, activate_pending_connection_state/2, is_correct_mac/2]).

empty_connection_state(ConnectionEnd) ->
    SecParams = empty_security_params(ConnectionEnd),
    #connection_state{security_parameters = SecParams}.

empty_security_params(ConnectionEnd = ?CLIENT) ->
    #security_parameters{connection_end = ConnectionEnd,
                         client_random = random()};
empty_security_params(ConnectionEnd = ?SERVER) ->
    #security_parameters{connection_end = ConnectionEnd,
                         server_random = random()}.
random() ->
    Secs_since_1970 = calendar:datetime_to_gregorian_seconds(
			calendar:universal_time()) - 62167219200,
    Random_28_bytes = crypto:rand_bytes(28),
    <<?UINT32(Secs_since_1970), Random_28_bytes/binary>>.

%%--------------------------------------------------------------------
-spec activate_pending_connection_state(#connection_states{}, read | write) ->
					       #connection_states{}.
%%
%% Description: Creates a new instance of the connection_states record
%% where the pending state of <Type> has been activated.
%%--------------------------------------------------------------------
activate_pending_connection_state(States =
                                  #connection_states{current_read = Current,
						     pending_read = Pending},
                                  read) ->
    %% Next epoch is a noop for SSL/TLS only uesed by DTLS
    NewCurrent = Pending#connection_state{epoch = connection_state_next_epoch(Current),
					  sequence_number = 0},
    SecParams = Pending#connection_state.security_parameters,
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = empty_connection_state(ConnectionEnd),
    SecureRenegotation = NewCurrent#connection_state.secure_renegotiation,
    NewPending = EmptyPending#connection_state{secure_renegotiation = SecureRenegotation},
    States#connection_states{current_read = NewCurrent,
                             pending_read = NewPending
                            };

activate_pending_connection_state(States =
                                  #connection_states{current_write = Current,
						     pending_write = Pending},
                                  write) ->
    %% Next epoch is a noop for SSL/TLS only uesed by DTLS
    NewCurrent = Pending#connection_state{epoch = connection_state_next_epoch(Current),
					  sequence_number = 0},
    SecParams = Pending#connection_state.security_parameters,
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = empty_connection_state(ConnectionEnd),
    SecureRenegotation = NewCurrent#connection_state.secure_renegotiation,
    NewPending = EmptyPending#connection_state{secure_renegotiation = SecureRenegotation},
    States#connection_states{current_write = NewCurrent,
                             pending_write = NewPending
                            }.

connection_state_next_epoch(#connection_state{epoch = undefined}) ->
    undefined;
connection_state_next_epoch(State) ->
    State#connection_state.epoch + 1.

is_correct_mac(Mac, Mac) ->
    true;
is_correct_mac(_M,_H) ->
    false.
