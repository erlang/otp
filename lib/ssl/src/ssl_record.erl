%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2019. All Rights Reserved.
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
-include("ssl_connection.hrl").
-include("ssl_internal.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").

%% Connection state handling
-export([initial_security_params/1, current_connection_state/2, pending_connection_state/2,
	 activate_pending_connection_state/3,
	 set_security_params/3,
         set_mac_secret/4,
	 set_master_secret/2,
         set_pending_cipher_state/4,
	 set_renegotiation_flag/2,
	 set_client_verify_data/3,
	 set_server_verify_data/3,
	 empty_connection_state/2, initial_connection_state/2, record_protocol_role/1,
         step_encryption_state/1]).

%% Compression
-export([compress/3, uncompress/3, compressions/0]).

%% Payload encryption/decryption
-export([cipher/4, cipher/5, decipher/4,
         cipher_aead/4, cipher_aead/5, decipher_aead/5,
         is_correct_mac/2, nonce_seed/3]).

-export_type([ssl_version/0, ssl_atom_version/0, connection_states/0, connection_state/0]).

-type ssl_version()       :: {integer(), integer()}.
-type ssl_atom_version() :: tls_record:tls_atom_version().
-type connection_states() :: map(). %% Map
-type connection_state() :: map(). %% Map

%%====================================================================
%% Connection state handling
%%====================================================================

%%--------------------------------------------------------------------
-spec current_connection_state(connection_states(), read | write) ->
				      connection_state().
%%
%% Description: Returns the instance of the connection_state map
%% that is currently defined as the current connection state.
%%--------------------------------------------------------------------
current_connection_state(ConnectionStates, read) ->
    maps:get(current_read, ConnectionStates);
current_connection_state(ConnectionStates, write) ->
    maps:get(current_write, ConnectionStates).

%%--------------------------------------------------------------------
-spec pending_connection_state(connection_states(), read | write) ->
				      connection_state().
%%
%% Description: Returns the instance of the connection_state map
%% that is pendingly defined as the pending connection state.
%%--------------------------------------------------------------------
pending_connection_state(ConnectionStates, read) ->
    maps:get(pending_read, ConnectionStates);
pending_connection_state(ConnectionStates, write) ->
    maps:get(pending_write, ConnectionStates).

%%--------------------------------------------------------------------
-spec activate_pending_connection_state(connection_states(), read | write, tls_connection | dtls_connection) ->
					       connection_states().
%%
%% Description: Creates a new instance of the connection_states record
%% where the pending state of <Type> has been activated.
%%--------------------------------------------------------------------
activate_pending_connection_state(#{current_read := Current,
				    pending_read := Pending} = States,
                                  read, Connection) ->
    #{secure_renegotiation := SecureRenegotation} = Current,
    #{beast_mitigation := BeastMitigation,
      security_parameters := SecParams} = Pending,
    NewCurrent = Pending#{sequence_number => 0},
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = Connection:empty_connection_state(ConnectionEnd, BeastMitigation),
    NewPending = EmptyPending#{secure_renegotiation => SecureRenegotation},
    States#{current_read => NewCurrent,
	    pending_read => NewPending
	   };

activate_pending_connection_state(#{current_write := Current,
				    pending_write := Pending} = States,
                                  write, Connection) ->
    NewCurrent = Pending#{sequence_number => 0},
    #{secure_renegotiation := SecureRenegotation} = Current,
    #{beast_mitigation := BeastMitigation,
      security_parameters := SecParams} = Pending,
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = Connection:empty_connection_state(ConnectionEnd, BeastMitigation),
    NewPending = EmptyPending#{secure_renegotiation => SecureRenegotation},
    States#{current_write => NewCurrent,
	    pending_write => NewPending
	   }.

%%--------------------------------------------------------------------
-spec step_encryption_state(#state{}) -> #state{}.
%%
%% Description: Activates the next encyrption state (e.g. handshake
%% encryption).
%%--------------------------------------------------------------------
step_encryption_state(#state{connection_states =
                                 #{pending_read := PendingRead,
                                   pending_write := PendingWrite} = ConnStates} = State) ->
    NewRead = PendingRead#{sequence_number => 0},
    NewWrite = PendingWrite#{sequence_number => 0},
    State#state{connection_states =
                    ConnStates#{current_read => NewRead,
                                current_write => NewWrite}}.


%%--------------------------------------------------------------------
-spec set_security_params(#security_parameters{}, #security_parameters{},
			  connection_states()) -> connection_states().
%%
%% Description: Creates a new instance of the connection_states record
%% where the pending states gets its security parameters updated.
%%--------------------------------------------------------------------
set_security_params(ReadParams, WriteParams, 
		    #{pending_read := Read,
		      pending_write := Write} = States) ->
    States#{pending_read  => Read#{security_parameters => ReadParams},
	    pending_write => Write#{security_parameters => WriteParams}
	   }.
%%--------------------------------------------------------------------
-spec set_mac_secret(binary(), binary(), client | server,
		     connection_states()) -> connection_states().
%%
%% Description: update the mac_secret field in pending connection states
%%--------------------------------------------------------------------
set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, client, States) ->
    set_mac_secret(ServerWriteMacSecret, ClientWriteMacSecret, States);
set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, server, States) ->
    set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, States).

set_mac_secret(ReadMacSecret, WriteMacSecret,
	       States = #{pending_read := Read,
			  pending_write := Write}) ->
    States#{pending_read => Read#{mac_secret => ReadMacSecret},
	    pending_write => Write#{mac_secret => WriteMacSecret}
     }.


%%--------------------------------------------------------------------
-spec set_master_secret(binary(), connection_states()) -> connection_states().
%%
%% Description: Set master_secret in pending connection states
%%--------------------------------------------------------------------
set_master_secret(MasterSecret,
                  States = #{pending_read := Read = #{security_parameters := ReadSecPar},
			     pending_write := Write = #{security_parameters := WriteSecPar}}) ->
    Read1 = Read#{security_parameters => ReadSecPar#security_parameters{
					   master_secret = MasterSecret}},
    Write1 = Write#{security_parameters => WriteSecPar#security_parameters{
					     master_secret = MasterSecret}},
    States#{pending_read => Read1, pending_write => Write1}.

%%--------------------------------------------------------------------
-spec set_renegotiation_flag(boolean(), connection_states()) -> connection_states().
%%
%% Description: Set secure_renegotiation in pending connection states
%%--------------------------------------------------------------------
set_renegotiation_flag(Flag, #{current_read := CurrentRead0,
			       current_write := CurrentWrite0,
			       pending_read := PendingRead0,
			       pending_write := PendingWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#{secure_renegotiation => Flag},
    CurrentWrite = CurrentWrite0#{secure_renegotiation => Flag},
    PendingRead = PendingRead0#{secure_renegotiation => Flag},
    PendingWrite = PendingWrite0#{secure_renegotiation => Flag},
    ConnectionStates#{current_read => CurrentRead,
		      current_write => CurrentWrite,
		      pending_read => PendingRead,
		      pending_write => PendingWrite}.

%%--------------------------------------------------------------------
-spec set_client_verify_data(current_read | current_write | current_both,
			     binary(), connection_states())->
				    connection_states().
%%
%% Description: Set verify data in connection states.
%%--------------------------------------------------------------------
set_client_verify_data(current_read, Data,
		       #{current_read := CurrentRead0,
			 pending_write := PendingWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#{client_verify_data => Data},
    PendingWrite = PendingWrite0#{client_verify_data => Data},
    ConnectionStates#{current_read => CurrentRead,
		      pending_write => PendingWrite};
set_client_verify_data(current_write, Data,
		       #{pending_read := PendingRead0,
			 current_write := CurrentWrite0}
		       = ConnectionStates) ->
    PendingRead = PendingRead0#{client_verify_data => Data},
    CurrentWrite = CurrentWrite0#{client_verify_data => Data},
    ConnectionStates#{pending_read => PendingRead,
		      current_write => CurrentWrite};
set_client_verify_data(current_both, Data,
		       #{current_read := CurrentRead0,
			 current_write := CurrentWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#{client_verify_data => Data},
    CurrentWrite = CurrentWrite0#{client_verify_data => Data},
    ConnectionStates#{current_read => CurrentRead,
		      current_write => CurrentWrite}.
%%--------------------------------------------------------------------
-spec set_server_verify_data(current_read | current_write | current_both,
			     binary(), connection_states())->
				    connection_states().
%%
%% Description: Set verify data in pending connection states.
%%--------------------------------------------------------------------
set_server_verify_data(current_write, Data,
		       #{pending_read := PendingRead0,
			 current_write := CurrentWrite0}
		       = ConnectionStates) ->
    PendingRead = PendingRead0#{server_verify_data => Data},
    CurrentWrite = CurrentWrite0#{server_verify_data => Data},
    ConnectionStates#{pending_read => PendingRead,
		      current_write => CurrentWrite};

set_server_verify_data(current_read, Data,
		       #{current_read := CurrentRead0,
			 pending_write := PendingWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#{server_verify_data => Data},
    PendingWrite = PendingWrite0#{server_verify_data => Data},
    ConnectionStates#{current_read => CurrentRead,
				       pending_write => PendingWrite};

set_server_verify_data(current_both, Data,
		       #{current_read := CurrentRead0,
			 current_write := CurrentWrite0}
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#{server_verify_data => Data},
    CurrentWrite = CurrentWrite0#{server_verify_data => Data},
    ConnectionStates#{current_read => CurrentRead,
		      current_write => CurrentWrite}.
%%--------------------------------------------------------------------
-spec set_pending_cipher_state(connection_states(), #cipher_state{},
			       #cipher_state{}, client | server) ->
				      connection_states().
%%
%% Description: Set the cipher state in the specified pending connection state.
%%--------------------------------------------------------------------
set_pending_cipher_state(#{pending_read := Read,
			   pending_write := Write} = States,
                         ClientState, ServerState, server) ->
    States#{
      pending_read => Read#{cipher_state => ClientState},
      pending_write => Write#{cipher_state => ServerState}};

set_pending_cipher_state(#{pending_read := Read,
			   pending_write := Write} = States,
                         ClientState, ServerState, client) ->
    States#{
      pending_read => Read#{cipher_state => ServerState},
      pending_write => Write#{cipher_state => ClientState}}.

%%====================================================================
%% Compression
%%====================================================================

uncompress(?NULL, Data, CS) ->
    {Data, CS}.

compress(?NULL, Data, CS) ->
    {Data, CS}.

%%--------------------------------------------------------------------
-spec compressions() -> [integer()].
%%
%% Description: return a list of compressions supported (currently none)
%%--------------------------------------------------------------------
compressions() ->
    [?NULL].

%%====================================================================
%% Payload encryption/decryption
%%====================================================================

%%--------------------------------------------------------------------
-spec cipher(ssl_version(), iodata(), connection_state(), MacHash::binary()) ->
		    {CipherFragment::binary(), connection_state()}.
%%
%% Description: Payload encryption
%%--------------------------------------------------------------------
cipher(Version, Fragment,
       #{cipher_state := CipherS0,
	 security_parameters :=
	     #security_parameters{bulk_cipher_algorithm =
				      BulkCipherAlgo}
	} = WriteState0, MacHash) ->
    %%
    {CipherFragment, CipherS1} =
	ssl_cipher:cipher(BulkCipherAlgo, CipherS0, MacHash, Fragment, Version),
    {CipherFragment,  WriteState0#{cipher_state => CipherS1}}.

%%--------------------------------------------------------------------
-spec cipher(ssl_version(), iodata(), #cipher_state{}, MacHash::binary(), #security_parameters{}) ->
		    {CipherFragment::binary(), #cipher_state{}}.
%%
%% Description: Payload encryption
%%--------------------------------------------------------------------
cipher(Version, Fragment, CipherS0, MacHash,
       #security_parameters{bulk_cipher_algorithm = BulkCipherAlgo}) ->
    %%
    ssl_cipher:cipher(BulkCipherAlgo, CipherS0, MacHash, Fragment, Version).

%%--------------------------------------------------------------------
-spec cipher_aead(ssl_version(), iodata(), connection_state(), AAD::binary()) ->
 			 {CipherFragment::binary(), connection_state()}.

%% Description: Payload encryption
%% %%--------------------------------------------------------------------
cipher_aead(_Version, Fragment,
	    #{cipher_state := CipherS0,
	      security_parameters :=
		  #security_parameters{bulk_cipher_algorithm =
					   BulkCipherAlgo}
	     } = WriteState0, AAD) ->
    {CipherFragment, CipherS1} =
	do_cipher_aead(BulkCipherAlgo, Fragment, CipherS0, AAD),
    {CipherFragment,  WriteState0#{cipher_state => CipherS1}}.

%%--------------------------------------------------------------------
-spec cipher_aead(ssl_version(), iodata(), #cipher_state{}, AAD::binary(), #security_parameters{}) ->
                         {CipherFragment::binary(), #cipher_state{}}.

%% Description: Payload encryption
%% %%--------------------------------------------------------------------
cipher_aead(_Version, Fragment, CipherS, AAD,
            #security_parameters{bulk_cipher_algorithm = BulkCipherAlgo}) ->
    do_cipher_aead(BulkCipherAlgo, Fragment, CipherS, AAD).

%%--------------------------------------------------------------------
-spec decipher(ssl_version(), binary(), connection_state(), boolean()) ->
                      {binary(), binary(), connection_state()} | #alert{}.
%%
%% Description: Payload decryption
%%--------------------------------------------------------------------
decipher(Version, CipherFragment,
	 #{security_parameters :=
	       #security_parameters{bulk_cipher_algorithm =
					BulkCipherAlgo,
				    hash_size = HashSz},
	   cipher_state := CipherS0
	  } = ReadState, PaddingCheck) ->
    case ssl_cipher:decipher(BulkCipherAlgo, HashSz, CipherS0, CipherFragment, Version, PaddingCheck) of
	{PlainFragment, Mac, CipherS1} ->
	    CS1 = ReadState#{cipher_state => CipherS1},
	    {PlainFragment, Mac, CS1};
	#alert{} = Alert ->
	    Alert
    end.
%%--------------------------------------------------------------------
-spec decipher_aead(ssl_cipher:cipher_enum(),  #cipher_state{}, binary(), binary(), ssl_record:ssl_version()) ->
			   binary() | #alert{}.
%%
%% Description: Decrypts the data and checks the associated data (AAD) MAC using
%% cipher described by cipher_enum() and updating the cipher state.
%% Use for suites that use authenticated encryption with associated data (AEAD)
%%-------------------------------------------------------------------
decipher_aead(Type, #cipher_state{key = Key} = CipherState, AAD0, CipherFragment, _) ->
    try
        Nonce = decrypt_nonce(Type, CipherState, CipherFragment),
        {AAD, CipherText, CipherTag} = aead_ciphertext_split(Type, CipherState, CipherFragment, AAD0),
        case ssl_cipher:aead_decrypt(Type, Key, Nonce, CipherText, CipherTag, AAD) of
	    Content when is_binary(Content) ->
		Content;
	    _ ->
                ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
	end
    catch
	_:_ ->
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
    end.

nonce_seed(?CHACHA20_POLY1305, Seed, CipherState) ->
    ssl_cipher:nonce_seed(Seed, CipherState);
nonce_seed(_,_, CipherState) ->
    CipherState.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

empty_connection_state(ConnectionEnd, BeastMitigation) ->
    SecParams = empty_security_params(ConnectionEnd),
    #{security_parameters => SecParams,
      beast_mitigation => BeastMitigation,
      compression_state  => undefined,
      cipher_state  => undefined,
      mac_secret  => undefined,
      secure_renegotiation => undefined,
      client_verify_data => undefined,
      server_verify_data => undefined
     }.

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

-compile({inline, [is_correct_mac/2]}).
is_correct_mac(Mac, Mac) ->
    true;
is_correct_mac(_M,_H) ->
    false.

-compile({inline, [record_protocol_role/1]}).
record_protocol_role(client) ->
    ?CLIENT;
record_protocol_role(server) ->
    ?SERVER.

initial_connection_state(ConnectionEnd, BeastMitigation) ->
    #{security_parameters =>
	  initial_security_params(ConnectionEnd),
      sequence_number => 0,
      beast_mitigation => BeastMitigation,
      compression_state  => undefined,
      cipher_state  => undefined,
      mac_secret  => undefined,
      secure_renegotiation => undefined,
      client_verify_data => undefined,
      server_verify_data => undefined
     }.

initial_security_params(ConnectionEnd) ->
    SecParams = #security_parameters{connection_end = ConnectionEnd,
				     compression_algorithm = ?NULL},
    ssl_cipher:security_parameters(?TLS_NULL_WITH_NULL_NULL, SecParams).

-define(end_additional_data(AAD, Len), << (begin(AAD)end)/binary, ?UINT16(begin(Len)end) >>).

do_cipher_aead(?CHACHA20_POLY1305 = Type, Fragment, #cipher_state{key=Key, tag_len = TagLen} = CipherState, AAD0) ->
    AAD = ?end_additional_data(AAD0, erlang:iolist_size(Fragment)),
    Nonce = chacha_nonce(CipherState),
    {Content, CipherTag} = ssl_cipher:aead_encrypt(Type, Key, Nonce, Fragment, AAD, TagLen),
    {<<Content/binary, CipherTag/binary>>, CipherState};
do_cipher_aead(Type, Fragment, #cipher_state{key=Key, tag_len = TagLen, nonce = ExplicitNonce} = CipherState, AAD0) ->
    AAD = ?end_additional_data(AAD0, erlang:iolist_size(Fragment)),
    Nonce = encrypt_nonce(Type, CipherState),
    {Content, CipherTag} = ssl_cipher:aead_encrypt(Type, Key, Nonce, Fragment, AAD, TagLen),
    {<<ExplicitNonce:64/integer, Content/binary, CipherTag/binary>>, CipherState#cipher_state{nonce = ExplicitNonce + 1}}.


chacha_nonce(#cipher_state{nonce = Nonce, iv = IV}) ->
    crypto:exor(<<?UINT32(0), Nonce/binary>>, IV).

encrypt_nonce(Type, #cipher_state{iv = IV, nonce = ExplicitNonce}) when  Type == ?AES_GCM;
                                                                         Type == ?AES_CCM;
                                                                         Type == ?AES_CCM_8 ->
    <<Salt:4/bytes, _/binary>> = IV,
    <<Salt/binary, ExplicitNonce:64/integer>>.

decrypt_nonce(?CHACHA20_POLY1305, CipherState, _) ->
    chacha_nonce(CipherState);
decrypt_nonce(Type, #cipher_state{iv = <<Salt:4/bytes, _/binary>>}, <<ExplicitNonce:8/bytes, _/binary>>) when 
      Type == ?AES_GCM; 
      Type == ?AES_CCM; 
      Type == ?AES_CCM_8 ->
    <<Salt/binary, ExplicitNonce/binary>>.

-compile({inline, [aead_ciphertext_split/4]}).
aead_ciphertext_split(?CHACHA20_POLY1305, #cipher_state{tag_len = Len}, CipherTextFragment, AAD) ->
    CipherLen = byte_size(CipherTextFragment) - Len,
    <<CipherText:CipherLen/bytes, CipherTag:Len/bytes>> = CipherTextFragment,
    {?end_additional_data(AAD, CipherLen), CipherText, CipherTag};
aead_ciphertext_split(Type,  #cipher_state{tag_len = Len}, CipherTextFragment, AAD) when Type == ?AES_GCM;
                                                                                         Type == ?AES_CCM;
                                                                                         Type == ?AES_CCM_8 ->
    CipherLen = byte_size(CipherTextFragment) - (Len + 8), %% 8 is length of explicit Nonce
    << _:8/bytes, CipherText:CipherLen/bytes, CipherTag:Len/bytes>> = CipherTextFragment,
    {?end_additional_data(AAD, CipherLen), CipherText, CipherTag}.
