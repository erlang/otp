%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose:  Handle TLS/SSL/DTLS record protocol. Note epoch is only
%% used by DTLS but handled here so we can avoid code duplication.
%%----------------------------------------------------------------------

-module(ssl_record).

-include("ssl_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").

%% Connection state handling
-export([init_connection_states/2,
	 current_connection_state/2, pending_connection_state/2,
	 activate_pending_connection_state/2,
	 set_security_params/3,
         set_mac_secret/4,
	 set_master_secret/2,
         set_pending_cipher_state/4,
	 set_renegotiation_flag/2,
	 set_client_verify_data/3,
	 set_server_verify_data/3]).

%% Encoding records
-export([encode_handshake/3, encode_alert_record/3,
	 encode_change_cipher_spec/2, encode_data/3]).

%% Compression
-export([compress/3, uncompress/3, compressions/0]).

%% Payload encryption/decryption
-export([cipher/4, decipher/4, is_correct_mac/2,
	 cipher_aead/4, decipher_aead/4]).

-export_type([ssl_version/0, ssl_atom_version/0]).

-type ssl_version()       :: {integer(), integer()}.
-type ssl_atom_version() :: tls_record:tls_atom_version().

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec init_connection_states(client | server, one_n_minus_one | zero_n | disabled ) ->
				      #connection_states{}.
%%
%% Description: Creates a connection_states record with appropriate
%% values for the initial SSL connection setup.
%%--------------------------------------------------------------------
init_connection_states(Role, BeastMitigation) ->
    ConnectionEnd = record_protocol_role(Role),
    Current = initial_connection_state(ConnectionEnd, BeastMitigation),
    Pending = empty_connection_state(ConnectionEnd, BeastMitigation),
    #connection_states{dtls_write_msg_seq = 1, % only used by dtls
		       current_read = Current,
		       pending_read = Pending,
		       current_write = Current,
		       pending_write = Pending
                      }.

%%--------------------------------------------------------------------
-spec current_connection_state(#connection_states{}, read | write) ->
				      #connection_state{}.
%%
%% Description: Returns the instance of the connection_state record
%% that is currently defined as the current conection state.
%%--------------------------------------------------------------------
current_connection_state(#connection_states{current_read = Current},
			 read) ->
    Current;
current_connection_state(#connection_states{current_write = Current},
			 write) ->
    Current.

%%--------------------------------------------------------------------
-spec pending_connection_state(#connection_states{}, read | write) ->
				      term().
%%
%% Description: Returns the instance of the connection_state record
%% that is currently defined as the pending conection state.
%%--------------------------------------------------------------------
pending_connection_state(#connection_states{pending_read = Pending},
			 read) ->
    Pending;
pending_connection_state(#connection_states{pending_write = Pending},
			 write) ->
    Pending.


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
    NewCurrent = Pending#connection_state{epoch = dtls_next_epoch(Current),
					  sequence_number = 0},
    BeastMitigation = Pending#connection_state.beast_mitigation,
    SecParams = Pending#connection_state.security_parameters,
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = empty_connection_state(ConnectionEnd, BeastMitigation),
    SecureRenegotation = NewCurrent#connection_state.secure_renegotiation,
    NewPending = EmptyPending#connection_state{secure_renegotiation = SecureRenegotation},
    States#connection_states{current_read = NewCurrent,
                             pending_read = NewPending
                            };

activate_pending_connection_state(States =
                                  #connection_states{current_write = Current,
						     pending_write = Pending},
                                  write) ->
    NewCurrent = Pending#connection_state{epoch = dtls_next_epoch(Current),
					  sequence_number = 0},
    BeastMitigation = Pending#connection_state.beast_mitigation,
    SecParams = Pending#connection_state.security_parameters,
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = empty_connection_state(ConnectionEnd, BeastMitigation),
    SecureRenegotation = NewCurrent#connection_state.secure_renegotiation,
    NewPending = EmptyPending#connection_state{secure_renegotiation = SecureRenegotation},
    States#connection_states{current_write = NewCurrent,
                             pending_write = NewPending
                            }.


%%--------------------------------------------------------------------
-spec set_security_params(#security_parameters{}, #security_parameters{},
			     #connection_states{}) -> #connection_states{}.
%%
%% Description: Creates a new instance of the connection_states record
%% where the pending states gets its security parameters updated.
%%--------------------------------------------------------------------
set_security_params(ReadParams, WriteParams, States =
		       #connection_states{pending_read = Read,
					  pending_write = Write}) ->
    States#connection_states{pending_read =
                             Read#connection_state{security_parameters =
                                                   ReadParams},
                             pending_write =
                             Write#connection_state{security_parameters =
                                                    WriteParams}
                            }.
%%--------------------------------------------------------------------
-spec set_mac_secret(binary(), binary(), client | server,
			#connection_states{}) -> #connection_states{}.
%%
%% Description: update the mac_secret field in pending connection states
%%--------------------------------------------------------------------
set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, client, States) ->
    set_mac_secret(ServerWriteMacSecret, ClientWriteMacSecret, States);
set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, server, States) ->
    set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, States).

set_mac_secret(ReadMacSecret, WriteMacSecret,
	       States = #connection_states{pending_read = Read,
					   pending_write = Write}) ->
    States#connection_states{
      pending_read = Read#connection_state{mac_secret = ReadMacSecret},
      pending_write = Write#connection_state{mac_secret = WriteMacSecret}
     }.


%%--------------------------------------------------------------------
-spec set_master_secret(binary(), #connection_states{}) -> #connection_states{}.
%%
%% Description: Set master_secret in pending connection states
%%--------------------------------------------------------------------
set_master_secret(MasterSecret,
                  States = #connection_states{pending_read = Read,
                                              pending_write = Write}) ->
    ReadSecPar = Read#connection_state.security_parameters,
    Read1 = Read#connection_state{
              security_parameters = ReadSecPar#security_parameters{
                                      master_secret = MasterSecret}},
    WriteSecPar = Write#connection_state.security_parameters,
    Write1 = Write#connection_state{
               security_parameters = WriteSecPar#security_parameters{
                                       master_secret = MasterSecret}},
    States#connection_states{pending_read = Read1, pending_write = Write1}.

%%--------------------------------------------------------------------
-spec set_renegotiation_flag(boolean(), #connection_states{}) -> #connection_states{}.
%%
%% Description: Set secure_renegotiation in pending connection states
%%--------------------------------------------------------------------
set_renegotiation_flag(Flag, #connection_states{
			 current_read = CurrentRead0,
			 current_write = CurrentWrite0,
			 pending_read = PendingRead0,
			 pending_write = PendingWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{secure_renegotiation = Flag},
    CurrentWrite = CurrentWrite0#connection_state{secure_renegotiation = Flag},
    PendingRead = PendingRead0#connection_state{secure_renegotiation = Flag},
    PendingWrite = PendingWrite0#connection_state{secure_renegotiation = Flag},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       current_write = CurrentWrite,
				       pending_read = PendingRead,
				       pending_write = PendingWrite}.

%%--------------------------------------------------------------------
-spec set_client_verify_data(current_read | current_write | current_both,
			     binary(), #connection_states{})->
				    #connection_states{}.
%%
%% Description: Set verify data in connection states.
%%--------------------------------------------------------------------
set_client_verify_data(current_read, Data,
		       #connection_states{current_read = CurrentRead0,
					  pending_write = PendingWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{client_verify_data = Data},
    PendingWrite = PendingWrite0#connection_state{client_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       pending_write = PendingWrite};
set_client_verify_data(current_write, Data,
		       #connection_states{pending_read = PendingRead0,
					  current_write = CurrentWrite0}
		       = ConnectionStates) ->
    PendingRead = PendingRead0#connection_state{client_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{client_verify_data = Data},
    ConnectionStates#connection_states{pending_read = PendingRead,
				       current_write = CurrentWrite};
set_client_verify_data(current_both, Data,
		       #connection_states{current_read = CurrentRead0,
					  current_write = CurrentWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{client_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{client_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       current_write = CurrentWrite}.
%%--------------------------------------------------------------------
-spec set_server_verify_data(current_read | current_write | current_both,
			     binary(), #connection_states{})->
				    #connection_states{}.
%%
%% Description: Set verify data in pending connection states.
%%--------------------------------------------------------------------
set_server_verify_data(current_write, Data,
		       #connection_states{pending_read = PendingRead0,
					  current_write = CurrentWrite0}
		       = ConnectionStates) ->
    PendingRead = PendingRead0#connection_state{server_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{server_verify_data = Data},
    ConnectionStates#connection_states{pending_read = PendingRead,
				       current_write = CurrentWrite};

set_server_verify_data(current_read, Data,
		       #connection_states{current_read = CurrentRead0,
					  pending_write = PendingWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{server_verify_data = Data},
    PendingWrite = PendingWrite0#connection_state{server_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       pending_write = PendingWrite};

set_server_verify_data(current_both, Data,
		       #connection_states{current_read = CurrentRead0,
					  current_write = CurrentWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{server_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{server_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       current_write = CurrentWrite}.
%%--------------------------------------------------------------------
-spec set_pending_cipher_state(#connection_states{}, #cipher_state{},
			       #cipher_state{}, client | server) ->
				      #connection_states{}.
%%
%% Description: Set the cipher state in the specified pending connection state.
%%--------------------------------------------------------------------
set_pending_cipher_state(#connection_states{pending_read = Read,
                                            pending_write = Write} = States,
                         ClientState, ServerState, server) ->
    States#connection_states{
        pending_read = Read#connection_state{cipher_state = ClientState},
        pending_write = Write#connection_state{cipher_state = ServerState}};

set_pending_cipher_state(#connection_states{pending_read = Read,
                                            pending_write = Write} = States,
                         ClientState, ServerState, client) ->
    States#connection_states{
        pending_read = Read#connection_state{cipher_state = ServerState},
        pending_write = Write#connection_state{cipher_state = ClientState}}.


%%--------------------------------------------------------------------
-spec encode_handshake(iolist(), ssl_version(), #connection_states{}) ->
			      {iolist(), #connection_states{}}.
%%
%% Description: Encodes a handshake message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_handshake(Frag, Version, 
		 #connection_states{current_write = 
					#connection_state{
					   beast_mitigation = BeastMitigation,
					   security_parameters =
					       #security_parameters{bulk_cipher_algorithm = BCA}}} = 
		     ConnectionStates)
when is_list(Frag) ->
    case iolist_size(Frag) of
	N  when N > ?MAX_PLAIN_TEXT_LENGTH ->
	    Data = split_bin(iolist_to_binary(Frag), ?MAX_PLAIN_TEXT_LENGTH, Version, BCA, BeastMitigation),
	    encode_iolist(?HANDSHAKE, Data, Version, ConnectionStates);
	_  ->
	    encode_plain_text(?HANDSHAKE, Version, Frag, ConnectionStates)
    end;
%% TODO: this is a workarround for DTLS
%%
%% DTLS need to select the connection write state based on Epoch it wants to
%% send this fragment in. That Epoch does not nessarily has to be the same
%% as the current_write epoch.
%% The right solution might be to pass the WriteState instead of the ConnectionStates,
%% however, this will require substantion API changes.
encode_handshake(Frag, Version, ConnectionStates) ->
    encode_plain_text(?HANDSHAKE, Version, Frag, ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_alert_record(#alert{}, ssl_version(), #connection_states{}) ->
				 {iolist(), #connection_states{}}.
%%
%% Description: Encodes an alert message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_alert_record(#alert{level = Level, description = Description},
                    Version, ConnectionStates) ->
    encode_plain_text(?ALERT, Version, <<?BYTE(Level), ?BYTE(Description)>>,
		      ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_change_cipher_spec(ssl_version(), #connection_states{}) ->
				       {iolist(), #connection_states{}}.
%%
%% Description: Encodes a change_cipher_spec-message to send on the ssl socket.
%%--------------------------------------------------------------------
encode_change_cipher_spec(Version, ConnectionStates) ->
    encode_plain_text(?CHANGE_CIPHER_SPEC, Version, <<1:8>>, ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_data(binary(), ssl_version(), #connection_states{}) ->
			 {iolist(), #connection_states{}}.
%%
%% Description: Encodes data to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_data(Frag, Version,
	    #connection_states{current_write = #connection_state{
				 beast_mitigation = BeastMitigation,
				 security_parameters =
				     #security_parameters{bulk_cipher_algorithm = BCA}}} =
		ConnectionStates) ->
    Data = split_bin(Frag, ?MAX_PLAIN_TEXT_LENGTH, Version, BCA, BeastMitigation),
    encode_iolist(?APPLICATION_DATA, Data, Version, ConnectionStates).

uncompress(?NULL, Data, CS) ->
    {Data, CS}.

compress(?NULL, Data, CS) ->
    {Data, CS}.

%%--------------------------------------------------------------------
-spec compressions() -> [binary()].
%%
%% Description: return a list of compressions supported (currently none)
%%--------------------------------------------------------------------
compressions() ->
    [?byte(?NULL)].

%%--------------------------------------------------------------------
-spec cipher(ssl_version(), iodata(), #connection_state{}, MacHash::binary()) ->
		    {CipherFragment::binary(), #connection_state{}}.
%%
%% Description: Payload encryption
%%--------------------------------------------------------------------
cipher(Version, Fragment,
       #connection_state{cipher_state = CipherS0,
			 security_parameters=
			     #security_parameters{bulk_cipher_algorithm =
						      BulkCipherAlgo}
			} = WriteState0, MacHash) ->

    {CipherFragment, CipherS1} =
	ssl_cipher:cipher(BulkCipherAlgo, CipherS0, MacHash, Fragment, Version),
    {CipherFragment,  WriteState0#connection_state{cipher_state = CipherS1}}.
%%--------------------------------------------------------------------
-spec cipher_aead(ssl_version(), iodata(), #connection_state{}, MacHash::binary()) ->
			 {CipherFragment::binary(), #connection_state{}}.
%%
%% Description: Payload encryption
%%--------------------------------------------------------------------
cipher_aead(Version, Fragment,
       #connection_state{cipher_state = CipherS0,
			 sequence_number = SeqNo,
			 security_parameters=
			     #security_parameters{bulk_cipher_algorithm =
						      BulkCipherAlgo}
			} = WriteState0, AAD) ->

    {CipherFragment, CipherS1} =
	ssl_cipher:cipher_aead(BulkCipherAlgo, CipherS0, SeqNo, AAD, Fragment, Version),
    {CipherFragment,  WriteState0#connection_state{cipher_state = CipherS1}}.

%%--------------------------------------------------------------------
-spec decipher(ssl_version(), binary(), #connection_state{}, boolean()) -> {binary(), binary(), #connection_state{}} | #alert{}.
%%
%% Description: Payload decryption
%%--------------------------------------------------------------------
decipher(Version, CipherFragment,
	 #connection_state{security_parameters =
			       #security_parameters{bulk_cipher_algorithm =
							BulkCipherAlgo,
						    hash_size = HashSz},
			   cipher_state = CipherS0
			  } = ReadState, PaddingCheck) ->
    case ssl_cipher:decipher(BulkCipherAlgo, HashSz, CipherS0, CipherFragment, Version, PaddingCheck) of
	{PlainFragment, Mac, CipherS1} ->
	    CS1 = ReadState#connection_state{cipher_state = CipherS1},
	    {PlainFragment, Mac, CS1};
	#alert{} = Alert ->
	    Alert
    end.
%%--------------------------------------------------------------------
-spec decipher_aead(ssl_version(), binary(), #connection_state{}, binary()) -> {binary(), binary(), #connection_state{}} | #alert{}.
%%
%% Description: Payload decryption
%%--------------------------------------------------------------------
decipher_aead(Version, CipherFragment,
	 #connection_state{sequence_number = SeqNo,
			   security_parameters =
			       #security_parameters{bulk_cipher_algorithm =
							BulkCipherAlgo},
			   cipher_state = CipherS0
			  } = ReadState, AAD) ->
    case ssl_cipher:decipher_aead(BulkCipherAlgo, CipherS0, SeqNo, AAD, CipherFragment, Version) of
	{PlainFragment, CipherS1} ->
	    CS1 = ReadState#connection_state{cipher_state = CipherS1},
	    {PlainFragment, CS1};
	#alert{} = Alert ->
	    Alert
    end.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
empty_connection_state(ConnectionEnd, BeastMitigation) ->
    SecParams = empty_security_params(ConnectionEnd),
    #connection_state{security_parameters = SecParams,
                      beast_mitigation = BeastMitigation}.

empty_security_params(ConnectionEnd = ?CLIENT) ->
    #security_parameters{connection_end = ConnectionEnd,
                         client_random = random()};
empty_security_params(ConnectionEnd = ?SERVER) ->
    #security_parameters{connection_end = ConnectionEnd,
                         server_random = random()}.
random() ->
    Secs_since_1970 = calendar:datetime_to_gregorian_seconds(
			calendar:universal_time()) - 62167219200,
    Random_28_bytes = ssl_cipher:random_bytes(28),
    <<?UINT32(Secs_since_1970), Random_28_bytes/binary>>.

dtls_next_epoch(#connection_state{epoch = undefined}) -> %% SSL/TLS
    undefined;
dtls_next_epoch(#connection_state{epoch = Epoch}) -> %% DTLS
    Epoch + 1.

is_correct_mac(Mac, Mac) ->
    true;
is_correct_mac(_M,_H) ->
    false.

record_protocol_role(client) ->
    ?CLIENT;
record_protocol_role(server) ->
    ?SERVER.

initial_connection_state(ConnectionEnd, BeastMitigation) ->
    #connection_state{security_parameters =
			  initial_security_params(ConnectionEnd),
                      sequence_number = 0,
                      beast_mitigation = BeastMitigation
                     }.

initial_security_params(ConnectionEnd) ->
    SecParams = #security_parameters{connection_end = ConnectionEnd,
				     compression_algorithm = ?NULL},
    ssl_cipher:security_parameters(?TLS_NULL_WITH_NULL_NULL, SecParams).


encode_plain_text(Type, Version, Data, ConnectionStates) ->
    RecordCB = protocol_module(Version),
    RecordCB:encode_plain_text(Type, Version, Data, ConnectionStates).

encode_iolist(Type, Data, Version, ConnectionStates0) ->
    RecordCB = protocol_module(Version),
    {ConnectionStates, EncodedMsg} =
        lists:foldl(fun(Text, {CS0, Encoded}) ->
			    {Enc, CS1} =
				RecordCB:encode_plain_text(Type, Version, Text, CS0),
			    {CS1, [Enc | Encoded]}
		    end, {ConnectionStates0, []}, Data),
    {lists:reverse(EncodedMsg), ConnectionStates}.

%% 1/n-1 splitting countermeasure Rizzo/Duong-Beast, RC4 chiphers are
%% not vulnerable to this attack.
split_bin(<<FirstByte:8, Rest/binary>>, ChunkSize, Version, BCA, one_n_minus_one) when
      BCA =/= ?RC4 andalso ({3, 1} == Version orelse
			    {3, 0} == Version) ->
    do_split_bin(Rest, ChunkSize, [[FirstByte]]);
%% 0/n splitting countermeasure for clients that are incompatible with 1/n-1
%% splitting.
split_bin(Bin, ChunkSize, Version, BCA, zero_n) when
      BCA =/= ?RC4 andalso ({3, 1} == Version orelse
			    {3, 0} == Version) ->
    do_split_bin(Bin, ChunkSize, [[<<>>]]);
split_bin(Bin, ChunkSize, _, _, _) ->
    do_split_bin(Bin, ChunkSize, []).

do_split_bin(<<>>, _, Acc) ->
    lists:reverse(Acc);
do_split_bin(Bin, ChunkSize, Acc) ->
    case Bin of
        <<Chunk:ChunkSize/binary, Rest/binary>> ->
            do_split_bin(Rest, ChunkSize, [Chunk | Acc]);
        _ ->
            lists:reverse(Acc, [Bin])
    end.

protocol_module({3, _}) ->
    tls_record;
protocol_module({254, _}) ->
    dtls_record.
