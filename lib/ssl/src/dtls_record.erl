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
-export([encode_plain_text/4]).

%% Protocol version handling
-export([protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, supported_protocol_versions/0,
	 is_acceptable_version/2, cipher/4, decipher/2]).

-export([init_connection_state_seq/2, current_connection_state_epoch/2,
	 set_connection_state_by_epoch/3, connection_state_by_epoch/3]).

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
					 epoch = Epoch, record_seq = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?HANDSHAKE),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length),
		       Data:Length/binary, Rest/binary>>, Acc) when MajVer >= 128 ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?HANDSHAKE,
					 version = {MajVer, MinVer},
					 epoch = Epoch, record_seq = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?ALERT),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary,
		       Rest/binary>>, Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?ALERT,
					 version = {MajVer, MinVer},
					 epoch = Epoch, record_seq = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?CHANGE_CIPHER_SPEC),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary, Rest/binary>>,
		     Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?CHANGE_CIPHER_SPEC,
					 version = {MajVer, MinVer},
					 epoch = Epoch, record_seq = SequenceNumber,
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
		  #connection_state{
		     compression_state = CompS0,
		     epoch = Epoch,
		     sequence_number = Seq,
		     security_parameters=
			 #security_parameters{compression_algorithm = CompAlg}
		    }= CS0) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    CS1 = CS0#connection_state{compression_state = CompS1},
    {CipherText, CS2} = cipher(Type, Version, Comp, CS1),
    CTBin = encode_tls_cipher_text(Type, Version, Epoch, Seq, CipherText),
    {CTBin, CS2}.

decode_cipher_text(CipherText, ConnnectionStates0) ->
    ReadState0 = ConnnectionStates0#connection_states.current_read,
    #connection_state{compression_state = CompressionS0,
		      security_parameters = SecParams} = ReadState0,
    CompressAlg = SecParams#security_parameters.compression_algorithm,
   case decipher(CipherText, ReadState0) of
       {Compressed, ReadState1} ->
	   {Plain, CompressionS1} = ssl_record:uncompress(CompressAlg,
					       Compressed, CompressionS0),
	   ConnnectionStates = ConnnectionStates0#connection_states{
				 current_read = ReadState1#connection_state{
						  compression_state = CompressionS1}},
	   {Plain, ConnnectionStates};
       #alert{} = Alert ->
	   Alert
   end.

%%--------------------------------------------------------------------
-spec protocol_version(tls_atom_version() | tls_version()) ->
			      tls_version() | tls_atom_version().
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
-spec lowest_protocol_version(tls_version(), tls_version()) -> tls_version().
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
-spec highest_protocol_version([tls_version()]) -> tls_version().
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
-spec supported_protocol_versions() -> [tls_version()].
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
-spec is_acceptable_version(tls_version(), Supported :: [tls_version()]) -> boolean().
%%
%% Description: ssl version 2 is not acceptable security risks are too big.
%%
%%--------------------------------------------------------------------
is_acceptable_version(Version, Versions) ->
    lists:member(Version, Versions).


%%--------------------------------------------------------------------
-spec init_connection_state_seq(tls_version(), #connection_states{}) ->
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
				    #connection_state{}, read | write) -> ok.
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

cipher(Type, Version, Fragment, CS0) ->
    Length = erlang:iolist_size(Fragment),
    {MacHash, CS1=#connection_state{cipher_state = CipherS0,
				    security_parameters=
					#security_parameters{bulk_cipher_algorithm =
								 BCA}
				   }} =
	hash_and_bump_seqno(CS0, Type, Version, Length, Fragment),
    {Ciphered, CipherS1} = ssl_cipher:cipher(BCA, CipherS0, MacHash, Fragment, Version),
    CS2 = CS1#connection_state{cipher_state=CipherS1},
    {Ciphered, CS2}.

decipher(TLS=#ssl_tls{type=Type, version=Version={254, _},
		      epoch = Epoch, record_seq = SeqNo,
		      fragment=Fragment}, CS0) ->
    SP = CS0#connection_state.security_parameters,
    BCA = SP#security_parameters.bulk_cipher_algorithm,
    HashSz = SP#security_parameters.hash_size,
    CipherS0 = CS0#connection_state.cipher_state,
    case ssl_cipher:decipher(BCA, HashSz, CipherS0, Fragment, Version) of
	{T, Mac, CipherS1} ->
	    CS1 = CS0#connection_state{cipher_state = CipherS1},
	    TLength = size(T),
	    MacHash = hash_with_seqno(CS1, Type, Version, Epoch, SeqNo, TLength, T),
	    case ssl_record:is_correct_mac(Mac, MacHash) of
		true ->
		    {TLS#ssl_tls{fragment = T}, CS1};
		false ->
		    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
	    end;
	#alert{} = Alert ->
	    Alert
    end.

hash_with_seqno(#connection_state{mac_secret = MacSecret,
				 security_parameters =
				     SecPars},
	       Type, Version = {254, _},
	       Epoch, SeqNo, Length, Fragment) ->
    mac_hash(Version,
	     SecPars#security_parameters.mac_algorithm,
	     MacSecret, (Epoch bsl 48) + SeqNo, Type,
	     Length, Fragment).

hash_and_bump_seqno(#connection_state{epoch = Epoch,
				      sequence_number = SeqNo,
				      mac_secret = MacSecret,
				      security_parameters =
				      SecPars} = CS0,
		    Type, Version = {254, _}, Length, Fragment) ->
    Hash = mac_hash(Version,
		    SecPars#security_parameters.mac_algorithm,
		    MacSecret, (Epoch bsl 48) + SeqNo, Type,
		    Length, Fragment),
    {Hash, CS0#connection_state{sequence_number = SeqNo+1}}.

mac_hash(Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment) ->
    dtls_v1:mac_hash(MacAlg, MacSecret, SeqNo, Type, Version,
		     Length, Fragment).
