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

%%
%%----------------------------------------------------------------------
%% Purpose: Handle DTLS record protocol. (Parts that are not shared with SSL/TLS)
%%----------------------------------------------------------------------
-module(dtls_record).

-include("dtls_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("dtls_handshake.hrl").
-include("ssl_cipher.hrl").

%% Handling of incoming data
-export([get_dtls_records/4,  init_connection_states/2, empty_connection_state/1]).

-export([save_current_connection_state/2, next_epoch/2, get_connection_state_by_epoch/3, replay_detect/2,
         init_connection_state_seq/2, current_connection_state_epoch/2]).

%% Encoding
-export([encode_handshake/4, encode_alert_record/3,
	 encode_change_cipher_spec/3, encode_data/3, encode_plain_text/5]).

%% Decoding
-export([decode_cipher_text/2]).

%% Protocol version handling
-export([protocol_version/1, lowest_protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, highest_protocol_version/2,
	 is_higher/2, supported_protocol_versions/0,
	 is_acceptable_version/2, hello_version/2]).


-export_type([dtls_atom_version/0]).

-type dtls_atom_version()  :: dtlsv1 | 'dtlsv1.2'.

-define(REPLAY_WINDOW_SIZE, 64).

-compile(inline).

%%====================================================================
%%  Handling of incoming data
%%====================================================================
%%--------------------------------------------------------------------
-spec init_connection_states(client | server, one_n_minus_one | zero_n | disabled) ->
 				    ssl_record:connection_states().
%% %
						%
%% Description: Creates a connection_states record with appropriate
%% values for the initial SSL connection setup.
%%--------------------------------------------------------------------
init_connection_states(Role, BeastMitigation) ->
    ConnectionEnd = ssl_record:record_protocol_role(Role),
    Initial = initial_connection_state(ConnectionEnd, BeastMitigation),
    Current = Initial#{epoch := 0},
    InitialPending = ssl_record:empty_connection_state(ConnectionEnd, BeastMitigation),
    Pending = empty_connection_state(InitialPending),
    #{saved_read  => Current,
      current_read  => Current,
      pending_read  => Pending,
      saved_write => Current,
      current_write => Current,
      pending_write => Pending}.

empty_connection_state(Empty) ->    
    Empty#{epoch => undefined, replay_window => init_replay_window(?REPLAY_WINDOW_SIZE)}.

%%--------------------------------------------------------------------
-spec save_current_connection_state(ssl_record:connection_states(), read | write) ->
				      ssl_record:connection_states().
%%
%% Description: Returns the instance of the connection_state map
%% where the current read|write state has been copied to the save state.
%%--------------------------------------------------------------------
save_current_connection_state(#{current_read := Current} = States, read) ->
    States#{saved_read := Current};

save_current_connection_state(#{current_write := Current} = States, write) ->
    States#{saved_write := Current}.

next_epoch(#{pending_read := Pending,
	     current_read := #{epoch := Epoch}} = States, read) ->
    States#{pending_read := Pending#{epoch := Epoch + 1,
                                     replay_window := init_replay_window(?REPLAY_WINDOW_SIZE)}};

next_epoch(#{pending_write := Pending,
	     current_write := #{epoch := Epoch}} = States, write) ->
    States#{pending_write := Pending#{epoch := Epoch + 1,
                                      replay_window := init_replay_window(?REPLAY_WINDOW_SIZE)}}.

get_connection_state_by_epoch(Epoch, #{current_write := #{epoch := Epoch} = Current},
			      write) ->
    Current;
get_connection_state_by_epoch(Epoch, #{saved_write := #{epoch := Epoch} = Saved},
			      write) ->
    Saved;
get_connection_state_by_epoch(Epoch, #{current_read := #{epoch := Epoch} = Current},
			      read) ->
    Current;
get_connection_state_by_epoch(Epoch, #{saved_read := #{epoch := Epoch} = Saved},
			      read) ->
    Saved.

set_connection_state_by_epoch(WriteState, Epoch, #{current_write := #{epoch := Epoch}} = States,
			      write) ->
    States#{current_write := WriteState};
set_connection_state_by_epoch(WriteState, Epoch, #{saved_write := #{epoch := Epoch}} = States,
			      write) ->
    States#{saved_write := WriteState};
set_connection_state_by_epoch(ReadState, Epoch, #{current_read := #{epoch := Epoch}} = States,
			      read) ->
    States#{current_read := ReadState};
set_connection_state_by_epoch(ReadState, Epoch, #{saved_read := #{epoch := Epoch}} = States,
			      read) ->
    States#{saved_read := ReadState}.

%%--------------------------------------------------------------------
-spec init_connection_state_seq(ssl_record:ssl_version(), ssl_record:connection_states()) ->
				       ssl_record:connection_state().
%%
%% Description: Copy the read sequence number to the write sequence number
%% This is only valid for DTLS in the first client_hello
%%--------------------------------------------------------------------
init_connection_state_seq({254, _},
			  #{current_read := #{epoch := 0, sequence_number := Seq},
			    current_write := #{epoch := 0} = Write} = ConnnectionStates0) ->
    ConnnectionStates0#{current_write => Write#{sequence_number => Seq}};
init_connection_state_seq(_, ConnnectionStates) ->
    ConnnectionStates.

%%--------------------------------------------------------
-spec current_connection_state_epoch(ssl_record:connection_states(), read | write) ->
					    integer().
%%
%% Description: Returns the epoch the connection_state record
%% that is currently defined as the current connection state.
%%--------------------------------------------------------------------
current_connection_state_epoch(#{current_read := #{epoch := Epoch}},
			       read) ->
    Epoch;
current_connection_state_epoch(#{current_write := #{epoch := Epoch}},
			       write) ->
    Epoch.

%%--------------------------------------------------------------------
-spec get_dtls_records(binary(), {atom(), atom(), ssl_record:ssl_version(), [ssl_record:ssl_version()]}, binary(),
                       ssl_options()) -> {[binary()], binary()} | #alert{}.
%%
%% Description: Given old buffer and new data from UDP/SCTP, packs up a records
%% and returns it as a list of tls_compressed binaries also returns leftover
%% data
%%--------------------------------------------------------------------
get_dtls_records(Data, Vinfo, Buffer, SslOpts) ->
    BinData = list_to_binary([Buffer, Data]),
    get_dtls_records_aux(Vinfo, BinData, [], SslOpts).

%%====================================================================
%% Encoding DTLS records
%%====================================================================

%%--------------------------------------------------------------------
-spec encode_handshake(iolist(), ssl_record:ssl_version(), integer(), ssl_record:connection_states()) ->
			      {iolist(), ssl_record:connection_states()}.
%
%% Description: Encodes a handshake message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_handshake(Frag, Version, Epoch, ConnectionStates) ->
    encode_plain_text(?HANDSHAKE, Version, Epoch, Frag, ConnectionStates).


%%--------------------------------------------------------------------
-spec encode_alert_record(#alert{}, ssl_record:ssl_version(), ssl_record:connection_states()) ->
				 {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes an alert message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_alert_record(#alert{level = Level, description = Description},
                    Version, ConnectionStates) ->
    #{epoch := Epoch} = ssl_record:current_connection_state(ConnectionStates, write),
    encode_plain_text(?ALERT, Version, Epoch, <<?BYTE(Level), ?BYTE(Description)>>,
		      ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_change_cipher_spec(ssl_record:ssl_version(), integer(), ssl_record:connection_states()) ->
				       {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes a change_cipher_spec-message to send on the ssl socket.
%%--------------------------------------------------------------------
encode_change_cipher_spec(Version, Epoch, ConnectionStates) ->
    encode_plain_text(?CHANGE_CIPHER_SPEC, Version, Epoch, ?byte(?CHANGE_CIPHER_SPEC_PROTO), ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_data(binary(), ssl_record:ssl_version(), ssl_record:connection_states()) ->
			 {iolist(),ssl_record:connection_states()}.
%%
%% Description: Encodes data to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_data(Data, Version, ConnectionStates) ->
    #{epoch := Epoch} = ssl_record:current_connection_state(ConnectionStates, write),
    encode_plain_text(?APPLICATION_DATA, Version, Epoch, Data, ConnectionStates).

encode_plain_text(Type, Version, Epoch, Data, ConnectionStates) ->
    Write0 = get_connection_state_by_epoch(Epoch, ConnectionStates, write),
    {CipherFragment, Write1} = encode_plain_text(Type, Version, Data, Write0),
    {CipherText, Write} = encode_dtls_cipher_text(Type, Version, CipherFragment, Write1),
    {CipherText, set_connection_state_by_epoch(Write, Epoch, ConnectionStates, write)}.

%%====================================================================
%% Decoding 
%%====================================================================

decode_cipher_text(#ssl_tls{epoch = Epoch} = CipherText, ConnnectionStates0) ->
    ReadState = get_connection_state_by_epoch(Epoch, ConnnectionStates0, read),
    decode_cipher_text(CipherText, ReadState, ConnnectionStates0).


%%====================================================================
%% Protocol version handling
%%====================================================================

%%--------------------------------------------------------------------
-spec protocol_version(dtls_atom_version() | ssl_record:ssl_version()) ->
			      ssl_record:ssl_version() | dtls_atom_version().
%%
%% Description: Creates a protocol version record from a version atom
%% or vice versa.
%%--------------------------------------------------------------------
protocol_version('dtlsv1.2') ->
    {254, 253};
protocol_version(dtlsv1) ->
    {254, 255};
protocol_version({254, 253}) ->
    'dtlsv1.2';
protocol_version({254, 255}) ->
    dtlsv1.
%%--------------------------------------------------------------------
-spec lowest_protocol_version(ssl_record:ssl_version(), ssl_record:ssl_version()) -> ssl_record:ssl_version().
%%
%% Description: Lowes protocol version of two given versions
%%--------------------------------------------------------------------
lowest_protocol_version(Version = {M, N}, {M, O}) when N > O ->
    Version;
lowest_protocol_version({M, _}, Version = {M, _}) ->
    Version;
lowest_protocol_version(Version = {M,_}, {N, _}) when M > N ->
    Version;
lowest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec lowest_protocol_version([ssl_record:ssl_version()]) -> ssl_record:ssl_version().
%%     
%% Description: Lowest protocol version present in a list
%%--------------------------------------------------------------------
lowest_protocol_version([]) ->
    lowest_protocol_version();
lowest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    lowest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version([ssl_record:ssl_version()]) -> ssl_record:ssl_version().
%%
%% Description: Highest protocol version present in a list
%%--------------------------------------------------------------------
highest_protocol_version([]) ->
    highest_protocol_version();
highest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    highest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version(ssl_record:ssl_version(), ssl_record:ssl_version()) -> ssl_record:ssl_version().
%%
%% Description: Highest protocol version of two given versions
%%--------------------------------------------------------------------
highest_protocol_version(Version = {M, N}, {M, O})   when N < O ->
    Version;
highest_protocol_version({M, _},
			Version = {M, _}) ->
    Version;
highest_protocol_version(Version = {M,_},
			{N, _}) when M < N ->
    Version;
highest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec is_higher(V1 :: ssl_record:ssl_version(), V2::ssl_record:ssl_version()) -> boolean().
%%
%% Description: Is V1 > V2
%%--------------------------------------------------------------------
is_higher({M, N}, {M, O}) when N < O ->
    true;
is_higher({M, _}, {N, _}) when M < N ->
    true;
is_higher(_, _) ->
    false.

%%--------------------------------------------------------------------
-spec supported_protocol_versions() -> [ssl_record:ssl_version()].
%%
%% Description: Protocol versions supported
%%--------------------------------------------------------------------
supported_protocol_versions() ->
    Fun = fun(Version) ->
		  protocol_version(Version)
	  end,
    case application:get_env(ssl, dtls_protocol_version) of
	undefined ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, []} ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, Vsns} when is_list(Vsns) ->
	    supported_protocol_versions(lists:map(Fun, Vsns));
	{ok, Vsn} ->
	    supported_protocol_versions([Fun(Vsn)])
     end.

supported_protocol_versions([]) ->
    Vsns = case sufficient_dtlsv1_2_crypto_support() of
	       true ->
		   ?ALL_DATAGRAM_SUPPORTED_VERSIONS;
	       false ->
		   ?MIN_DATAGRAM_SUPPORTED_VERSIONS
	   end,
    application:set_env(ssl, dtls_protocol_version, Vsns),
    Vsns;

supported_protocol_versions([_|_] = Vsns) ->
    case sufficient_dtlsv1_2_crypto_support() of
	true ->
	    Vsns;
	false ->
	    case Vsns -- ['dtlsv1.2'] of
		[] ->
		    ?MIN_SUPPORTED_VERSIONS;
		NewVsns ->
		    NewVsns
	    end
    end.

%%--------------------------------------------------------------------
-spec is_acceptable_version(ssl_record:ssl_version(), Supported :: [ssl_record:ssl_version()]) -> boolean().
%%
%% Description: ssl version 2 is not acceptable security risks are too big.
%%
%%--------------------------------------------------------------------
is_acceptable_version(Version, Versions) ->
    lists:member(Version, Versions).

-spec hello_version(ssl_record:ssl_version(), [ssl_record:ssl_version()]) -> ssl_record:ssl_version().
hello_version(Version, Versions) ->
    case dtls_v1:corresponding_tls_version(Version) of
        TLSVersion when TLSVersion >= {3, 3} ->
            Version;
        _ ->
            lowest_protocol_version(Versions)
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_connection_state(ConnectionEnd, BeastMitigation) ->
    #{security_parameters =>
	  ssl_record:initial_security_params(ConnectionEnd),
      epoch => undefined,
      sequence_number => 0,
      replay_window => init_replay_window(?REPLAY_WINDOW_SIZE),
      beast_mitigation => BeastMitigation,
      compression_state  => undefined,
      cipher_state  => undefined,
      mac_secret  => undefined,
      secure_renegotiation => undefined,
      client_verify_data => undefined,
      server_verify_data => undefined
     }.

get_dtls_records_aux({DataTag, StateName, _, Versions} = Vinfo, <<?BYTE(Type),?BYTE(MajVer),?BYTE(MinVer),
                                                         ?UINT16(Epoch), ?UINT48(SequenceNumber),
                                                         ?UINT16(Length), Data:Length/binary, Rest/binary>> = RawDTLSRecord,
		     Acc, #{log_level := LogLevel} = SslOpts)
  when ((StateName == hello)
        orelse ((StateName == certify) andalso (DataTag == udp))
        orelse ((StateName == abbreviated) andalso (DataTag == udp))) andalso ((Type == ?HANDSHAKE)
                                                                               orelse
                                                                                 (Type == ?ALERT)) ->
    ssl_logger:debug(LogLevel, inbound, 'record', [RawDTLSRecord]),
    case is_acceptable_version({MajVer, MinVer}, Versions) of
        true ->
            get_dtls_records_aux(Vinfo, Rest, [#ssl_tls{type = Type,
                                                 version = {MajVer, MinVer},
                                                 epoch = Epoch, sequence_number = SequenceNumber,
                                                 fragment = Data} | Acc], SslOpts);
        false ->
              ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
        end;
get_dtls_records_aux({_, _, Version, _} = Vinfo, <<?BYTE(Type),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary, Rest/binary>> = RawDTLSRecord,
		     Acc, #{log_level := LogLevel} = SslOpts) when (Type == ?APPLICATION_DATA) orelse
                                        (Type == ?HANDSHAKE) orelse
                                        (Type == ?ALERT) orelse
                                        (Type == ?CHANGE_CIPHER_SPEC) ->
    ssl_logger:debug(LogLevel, inbound, 'record', [RawDTLSRecord]),
    case {MajVer, MinVer} of
        Version ->
            get_dtls_records_aux(Vinfo, Rest, [#ssl_tls{type = Type,
                                                 version = {MajVer, MinVer},
                                                 epoch = Epoch, sequence_number = SequenceNumber,
                                                 fragment = Data} | Acc], SslOpts);
        _ ->
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
    end;
get_dtls_records_aux(_, <<?BYTE(_), ?BYTE(_MajVer), ?BYTE(_MinVer),
		       ?UINT16(Length), _/binary>>,
		     _Acc, _) when Length > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_dtls_records_aux(_, Data, Acc, _) ->
    case size(Data) =< ?MAX_CIPHER_TEXT_LENGTH + ?INITIAL_BYTES of
	true ->
	    {lists:reverse(Acc), Data};
	false ->
	    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE)
    end.
%%--------------------------------------------------------------------

init_replay_window(Size) ->
    #{size => Size,
      top => Size,
      bottom => 0,
      mask => 0 bsl 64
     }.

replay_detect(#ssl_tls{sequence_number = SequenceNumber}, #{replay_window := Window}) ->
    is_replay(SequenceNumber, Window).


is_replay(SequenceNumber, #{bottom := Bottom}) when SequenceNumber < Bottom ->
    true;
is_replay(SequenceNumber, #{size := Size,
                            top := Top,
                            bottom := Bottom,
                            mask :=  Mask})  when (SequenceNumber >= Bottom) andalso (SequenceNumber =< Top) ->
    Index = (SequenceNumber rem Size),
    (Index band Mask) == 1;

is_replay(_, _) ->
    false.

update_replay_window(SequenceNumber,  #{replay_window := #{size := Size,
                                                           top := Top,
                                                           bottom := Bottom,
                                                           mask :=  Mask0} = Window0} = ConnectionStates) ->
    NoNewBits = SequenceNumber - Top,
    Index = SequenceNumber rem Size,
    Mask = (Mask0 bsl NoNewBits) bor Index,
    Window =  Window0#{top => SequenceNumber,
                       bottom => Bottom + NoNewBits,
                       mask => Mask},
    ConnectionStates#{replay_window := Window}.

%%--------------------------------------------------------------------

encode_dtls_cipher_text(Type, {MajVer, MinVer}, Fragment, 
		       #{epoch := Epoch, sequence_number := Seq} = WriteState) ->
    Length = erlang:iolist_size(Fragment),
    {[<<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Epoch),
	?UINT48(Seq), ?UINT16(Length)>>, Fragment], 
     WriteState#{sequence_number => Seq + 1}}.

encode_plain_text(Type, Version, Data, #{compression_state := CompS0,
                                         cipher_state := CipherS0,
					 epoch := Epoch,
					 sequence_number := Seq,
					 security_parameters :=
					     #security_parameters{
						cipher_type = ?AEAD,
                                                bulk_cipher_algorithm = BCAlg,
						compression_algorithm = CompAlg}
					} = WriteState0) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    AAD = start_additional_data(Type, Version, Epoch, Seq),
    CipherS = ssl_record:nonce_seed(BCAlg, <<?UINT16(Epoch), ?UINT48(Seq)>>, CipherS0),
    WriteState = WriteState0#{compression_state => CompS1,
                              cipher_state => CipherS},
    TLSVersion = dtls_v1:corresponding_tls_version(Version),
    ssl_record:cipher_aead(TLSVersion, Comp, WriteState, AAD);
encode_plain_text(Type, Version, Fragment, #{compression_state := CompS0,
					 epoch := Epoch,
					 sequence_number := Seq,
                                         cipher_state := CipherS0,
					 security_parameters :=
					     #security_parameters{compression_algorithm = CompAlg,
                                                                  bulk_cipher_algorithm =
                                                                      BulkCipherAlgo}
					}= WriteState0) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Fragment, CompS0),
    WriteState1 = WriteState0#{compression_state => CompS1},
    MAC = calc_mac_hash(Type, Version, WriteState1, Epoch, Seq, Comp),
    TLSVersion = dtls_v1:corresponding_tls_version(Version),
    {CipherFragment, CipherS1} =
	ssl_cipher:cipher(BulkCipherAlgo, CipherS0, MAC, Fragment, TLSVersion),
    {CipherFragment,  WriteState0#{cipher_state => CipherS1}}.

%%--------------------------------------------------------------------
decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    epoch = Epoch,
			    sequence_number = Seq,
			    fragment = CipherFragment} = CipherText,
		   #{compression_state := CompressionS0,
                     cipher_state := CipherS0,
		     security_parameters :=
			 #security_parameters{
			    cipher_type = ?AEAD,
                            bulk_cipher_algorithm =
                                BulkCipherAlgo,
			    compression_algorithm = CompAlg}} = ReadState0, 
		   ConnnectionStates0) ->
    AAD = start_additional_data(Type, Version, Epoch, Seq),
    CipherS = ssl_record:nonce_seed(BulkCipherAlgo, <<?UINT16(Epoch), ?UINT48(Seq)>>, CipherS0),
    TLSVersion = dtls_v1:corresponding_tls_version(Version),
    case ssl_record:decipher_aead(BulkCipherAlgo, CipherS, AAD, CipherFragment, TLSVersion) of
	PlainFragment when is_binary(PlainFragment) ->
	    {Plain, CompressionS} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    ReadState1 = ReadState0#{compression_state := CompressionS,
                                     cipher_state := CipherS},
            ReadState = update_replay_window(Seq, ReadState1),
	    ConnnectionStates = set_connection_state_by_epoch(ReadState, Epoch, ConnnectionStates0, read),
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	  #alert{} = Alert ->
	    Alert
    end;
decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    epoch = Epoch,
			    sequence_number = Seq,
			    fragment = CipherFragment} = CipherText,
		   #{compression_state := CompressionS0,
		     security_parameters :=
			 #security_parameters{
			    compression_algorithm = CompAlg}} = ReadState0,
		   ConnnectionStates0) ->
    {PlainFragment, Mac, ReadState1} = ssl_record:decipher(dtls_v1:corresponding_tls_version(Version),
							   CipherFragment, ReadState0, true),
    MacHash = calc_mac_hash(Type, Version, ReadState1, Epoch, Seq, PlainFragment),
    case ssl_record:is_correct_mac(Mac, MacHash) of
	true ->
	    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    
	    ReadState2 = ReadState1#{compression_state => CompressionS1},
            ReadState = update_replay_window(Seq, ReadState2),
	    ConnnectionStates = set_connection_state_by_epoch(ReadState, Epoch, ConnnectionStates0, read),
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	false ->
	    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
    end.
%%--------------------------------------------------------------------

calc_mac_hash(Type, Version, #{mac_secret := MacSecret,
			       security_parameters := #security_parameters{mac_algorithm = MacAlg}},
	      Epoch, SeqNo, Fragment) ->
    Length = erlang:iolist_size(Fragment),
    mac_hash(Version, MacAlg, MacSecret, Epoch, SeqNo, Type,
	     Length, Fragment).

mac_hash({Major, Minor}, MacAlg, MacSecret, Epoch, SeqNo, Type, Length, Fragment) ->
    Value = [<<?UINT16(Epoch), ?UINT48(SeqNo), ?BYTE(Type),
       ?BYTE(Major), ?BYTE(Minor), ?UINT16(Length)>>,
     Fragment],
    dtls_v1:hmac_hash(MacAlg, MacSecret, Value).
    
start_additional_data(Type, {MajVer, MinVer}, Epoch, SeqNo) ->
    <<?UINT16(Epoch), ?UINT48(SeqNo), ?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer)>>.

%%--------------------------------------------------------------------

lowest_list_protocol_version(Ver, []) ->
    Ver;
lowest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    lowest_list_protocol_version(lowest_protocol_version(Ver1, Ver2), Rest).

highest_list_protocol_version(Ver, []) ->
    Ver;
highest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    highest_list_protocol_version(highest_protocol_version(Ver1, Ver2), Rest).

highest_protocol_version() ->
    highest_protocol_version(supported_protocol_versions()).

lowest_protocol_version() ->
    lowest_protocol_version(supported_protocol_versions()).

sufficient_dtlsv1_2_crypto_support() ->
    CryptoSupport = crypto:supports(),
    proplists:get_bool(sha256, proplists:get_value(hashs, CryptoSupport)).

