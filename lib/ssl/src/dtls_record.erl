%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2015. All Rights Reserved.
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
-export([get_dtls_records/2]).

%% Decoding
-export([decode_cipher_text/2]).

%% Encoding
-export([encode_plain_text/4, encode_handshake/3, encode_change_cipher_spec/2]).

%% Protocol version handling
-export([protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, supported_protocol_versions/0,
	 is_acceptable_version/2]).

%% DTLS Epoch handling
-export([init_connection_state_seq/2, current_connection_state_epoch/2,
	 set_connection_state_by_epoch/3, connection_state_by_epoch/3]).

-export_type([dtls_version/0, dtls_atom_version/0]).

-type dtls_version()       :: ssl_record:ssl_version().
-type dtls_atom_version()  :: dtlsv1 | 'dtlsv1.2'.

-compile(inline).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec get_dtls_records(binary(), binary()) -> {[binary()], binary()} | #alert{}.
%%
%% Description: Given old buffer and new data from UDP/SCTP, packs up a records
%% and returns it as a list of tls_compressed binaries also returns leftover
%% data
%%--------------------------------------------------------------------
get_dtls_records(Data, <<>>) ->
    get_dtls_records_aux(Data, []);
get_dtls_records(Data, Buffer) ->
    get_dtls_records_aux(list_to_binary([Buffer, Data]), []).

get_dtls_records_aux(<<?BYTE(?APPLICATION_DATA),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary, Rest/binary>>,
		     Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?APPLICATION_DATA,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?HANDSHAKE),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length),
		       Data:Length/binary, Rest/binary>>, Acc) when MajVer >= 128 ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?HANDSHAKE,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?ALERT),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary,
		       Rest/binary>>, Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?ALERT,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?CHANGE_CIPHER_SPEC),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary, Rest/binary>>,
		     Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?CHANGE_CIPHER_SPEC,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);

get_dtls_records_aux(<<0:1, _CT:7, ?BYTE(_MajVer), ?BYTE(_MinVer),
		       ?UINT16(Length), _/binary>>,
		     _Acc) when Length > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_dtls_records_aux(<<1:1, Length0:15, _/binary>>,_Acc)
  when Length0 > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_dtls_records_aux(Data, Acc) ->
    case size(Data) =< ?MAX_CIPHER_TEXT_LENGTH + ?INITIAL_BYTES of
	true ->
	    {lists:reverse(Acc), Data};
	false ->
	    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE)
    end.

encode_plain_text(Type, Version, Data,
		  #connection_states{current_write =
					 #connection_state{
					    epoch = Epoch,
					    sequence_number = Seq,
					    compression_state=CompS0,
					    security_parameters=
						#security_parameters{
						   cipher_type = ?AEAD,
						   compression_algorithm=CompAlg}
					   }= WriteState0} = ConnectionStates) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    WriteState1 = WriteState0#connection_state{compression_state = CompS1},
    AAD = calc_aad(Type, Version, Epoch, Seq),
    {CipherFragment, WriteState} = ssl_record:cipher_aead(dtls_v1:corresponding_tls_version(Version),
							  Comp, WriteState1, AAD),
    CipherText = encode_tls_cipher_text(Type, Version, Epoch, Seq, CipherFragment),
    {CipherText, ConnectionStates#connection_states{current_write =
							WriteState#connection_state{sequence_number = Seq +1}}};

encode_plain_text(Type, Version, Data,
		  #connection_states{current_write=#connection_state{
						      epoch = Epoch,
						      sequence_number = Seq,
						      compression_state=CompS0,
						      security_parameters=
							  #security_parameters{compression_algorithm=CompAlg}
						     }= WriteState0} = ConnectionStates) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    WriteState1 = WriteState0#connection_state{compression_state = CompS1},
    MacHash = calc_mac_hash(WriteState1, Type, Version, Epoch, Seq, Comp),
    {CipherFragment, WriteState} = ssl_record:cipher(dtls_v1:corresponding_tls_version(Version), 
						     Comp, WriteState1, MacHash),
    CipherText = encode_tls_cipher_text(Type, Version, Epoch, Seq, CipherFragment),
    {CipherText, ConnectionStates#connection_states{current_write =
							WriteState#connection_state{sequence_number = Seq +1}}}.

decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    epoch = Epoch,
			    sequence_number = Seq,
			    fragment = CipherFragment} = CipherText,
		   #connection_states{current_read =
					  #connection_state{
					     compression_state = CompressionS0,
					     security_parameters=
						 #security_parameters{
						    cipher_type = ?AEAD,
						    compression_algorithm=CompAlg}
					    } = ReadState0}= ConnnectionStates0) ->
    AAD = calc_aad(Type, Version, Epoch, Seq),
    case ssl_record:decipher_aead(dtls_v1:corresponding_tls_version(Version),
				  CipherFragment, ReadState0, AAD) of
	{PlainFragment, ReadState1} ->
	    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    ConnnectionStates = ConnnectionStates0#connection_states{
				  current_read = ReadState1#connection_state{
						   compression_state = CompressionS1}},
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	#alert{} = Alert ->
	    Alert
    end;

decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    epoch = Epoch,
			    sequence_number = Seq,
			    fragment = CipherFragment} = CipherText,
		   #connection_states{current_read =
					  #connection_state{
					     compression_state = CompressionS0,
					     security_parameters=
						 #security_parameters{
						    compression_algorithm=CompAlg}
					    } = ReadState0}= ConnnectionStates0) ->
    {PlainFragment, Mac, ReadState1} = ssl_record:decipher(dtls_v1:corresponding_tls_version(Version),
							   CipherFragment, ReadState0, true),
    MacHash = calc_mac_hash(ReadState1, Type, Version, Epoch, Seq, PlainFragment),
    case ssl_record:is_correct_mac(Mac, MacHash) of
	true ->
	    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    ConnnectionStates = ConnnectionStates0#connection_states{
				  current_read = ReadState1#connection_state{
						   compression_state = CompressionS1}},
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	false ->
	    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
    end.
%%--------------------------------------------------------------------
-spec encode_handshake(iolist(), dtls_version(), #connection_states{}) ->
			      {iolist(), #connection_states{}}.
%%
%% Description: Encodes a handshake message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_handshake(Frag, Version, ConnectionStates) ->
    encode_plain_text(?HANDSHAKE, Version, Frag, ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_change_cipher_spec(dtls_version(), #connection_states{}) ->
				       {iolist(), #connection_states{}}.
%%
%% Description: Encodes a change_cipher_spec-message to send on the ssl socket.
%%--------------------------------------------------------------------
encode_change_cipher_spec(Version, ConnectionStates) ->
    encode_plain_text(?CHANGE_CIPHER_SPEC, Version, <<1:8>>, ConnectionStates).

%%--------------------------------------------------------------------
-spec protocol_version(dtls_atom_version() | dtls_version()) ->
			      dtls_version() | dtls_atom_version().
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
-spec lowest_protocol_version(dtls_version(), dtls_version()) -> dtls_version().
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
-spec highest_protocol_version([dtls_version()]) -> dtls_version().
%%
%% Description: Highest protocol version present in a list
%%--------------------------------------------------------------------
highest_protocol_version([Ver | Vers]) ->
    highest_protocol_version(Ver, Vers).

highest_protocol_version(Version, []) ->
    Version;
highest_protocol_version(Version = {N, M}, [{N, O} | Rest])   when M < O ->
    highest_protocol_version(Version, Rest);
highest_protocol_version({M, _}, [Version = {M, _} | Rest]) ->
    highest_protocol_version(Version, Rest);
highest_protocol_version(Version = {M,_}, [{N,_} | Rest])  when M < N ->
    highest_protocol_version(Version, Rest);
highest_protocol_version(_, [Version | Rest]) ->
    highest_protocol_version(Version, Rest).


%%--------------------------------------------------------------------
-spec supported_protocol_versions() -> [dtls_version()].
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
	    supported_protocol_versions(Vsns);
	{ok, Vsn} ->
	    supported_protocol_versions([Vsn])
     end.

supported_protocol_versions([]) ->
    Vsns = supported_connection_protocol_versions([]),
    application:set_env(ssl, dtls_protocol_version, Vsns),
    Vsns;

supported_protocol_versions([_|_] = Vsns) ->
    Vsns.

supported_connection_protocol_versions([]) ->
    ?ALL_DATAGRAM_SUPPORTED_VERSIONS.

%%--------------------------------------------------------------------
-spec is_acceptable_version(dtls_version(), Supported :: [dtls_version()]) -> boolean().
%%
%% Description: ssl version 2 is not acceptable security risks are too big.
%%
%%--------------------------------------------------------------------
is_acceptable_version(Version, Versions) ->
    lists:member(Version, Versions).


%%--------------------------------------------------------------------
-spec init_connection_state_seq(dtls_version(), #connection_states{}) ->
				       #connection_state{}.
%%
%% Description: Copy the read sequence number to the write sequence number
%% This is only valid for DTLS in the first client_hello
%%--------------------------------------------------------------------
init_connection_state_seq({254, _},
			  #connection_states{
			     current_read = Read = #connection_state{epoch = 0},
			     current_write = Write = #connection_state{epoch = 0}} = CS0) ->
    CS0#connection_states{current_write =
			      Write#connection_state{
				sequence_number = Read#connection_state.sequence_number}};
init_connection_state_seq(_, CS) ->
    CS.

%%--------------------------------------------------------
-spec current_connection_state_epoch(#connection_states{}, read | write) ->
					    integer().
%%
%% Description: Returns the epoch the connection_state record
%% that is currently defined as the current conection state.
%%--------------------------------------------------------------------
current_connection_state_epoch(#connection_states{current_read = Current},
			       read) ->
    Current#connection_state.epoch;
current_connection_state_epoch(#connection_states{current_write = Current},
			       write) ->
    Current#connection_state.epoch.

%%--------------------------------------------------------------------

-spec connection_state_by_epoch(#connection_states{}, integer(), read | write) ->
				      #connection_state{}.
%%
%% Description: Returns the instance of the connection_state record
%% that is defined by the Epoch.
%%--------------------------------------------------------------------
connection_state_by_epoch(#connection_states{current_read = CS}, Epoch, read)
  when CS#connection_state.epoch == Epoch ->
    CS;
connection_state_by_epoch(#connection_states{pending_read = CS}, Epoch, read)
  when CS#connection_state.epoch == Epoch ->
    CS;
connection_state_by_epoch(#connection_states{current_write = CS}, Epoch, write)
  when CS#connection_state.epoch == Epoch ->
    CS;
connection_state_by_epoch(#connection_states{pending_write = CS}, Epoch, write)
  when CS#connection_state.epoch == Epoch ->
    CS.
%%--------------------------------------------------------------------
-spec set_connection_state_by_epoch(#connection_states{},
				    #connection_state{}, read | write)
				   -> #connection_states{}.
%%
%% Description: Returns the instance of the connection_state record
%% that is defined by the Epoch.
%%--------------------------------------------------------------------
set_connection_state_by_epoch(ConnectionStates0 =
				  #connection_states{current_read = CS},
			      NewCS = #connection_state{epoch = Epoch}, read)
  when CS#connection_state.epoch == Epoch ->
    ConnectionStates0#connection_states{current_read = NewCS};

set_connection_state_by_epoch(ConnectionStates0 =
				  #connection_states{pending_read = CS},
			      NewCS = #connection_state{epoch = Epoch}, read)
  when CS#connection_state.epoch == Epoch ->
    ConnectionStates0#connection_states{pending_read = NewCS};

set_connection_state_by_epoch(ConnectionStates0 =
				  #connection_states{current_write = CS},
			      NewCS = #connection_state{epoch = Epoch}, write)
  when CS#connection_state.epoch == Epoch ->
    ConnectionStates0#connection_states{current_write = NewCS};

set_connection_state_by_epoch(ConnectionStates0 =
				  #connection_states{pending_write = CS},
			      NewCS = #connection_state{epoch = Epoch}, write)
  when CS#connection_state.epoch == Epoch ->
    ConnectionStates0#connection_states{pending_write = NewCS}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encode_tls_cipher_text(Type, {MajVer, MinVer}, Epoch, Seq, Fragment) ->
    Length = erlang:iolist_size(Fragment),
    [<<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Epoch),
       ?UINT48(Seq), ?UINT16(Length)>>, Fragment].

calc_mac_hash(#connection_state{mac_secret = MacSecret,
				security_parameters = #security_parameters{mac_algorithm = MacAlg}},
	      Type, Version, Epoch, SeqNo, Fragment) ->
    Length = erlang:iolist_size(Fragment),
    NewSeq = (Epoch bsl 48) + SeqNo,
    mac_hash(Version, MacAlg, MacSecret, NewSeq, Type,
	     Length, Fragment).

mac_hash(Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment) ->
    dtls_v1:mac_hash(Version, MacAlg, MacSecret, SeqNo, Type,
		     Length, Fragment).

calc_aad(Type, {MajVer, MinVer}, Epoch, SeqNo) ->
    NewSeq = (Epoch bsl 48) + SeqNo,
    <<NewSeq:64/integer, ?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer)>>.
