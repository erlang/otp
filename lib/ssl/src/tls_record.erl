%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%% Purpose: Handle TLS/SSL record protocol. (Parts that are not shared with DTLS)
%%----------------------------------------------------------------------

-module(tls_record).

-include("tls_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("tls_handshake.hrl").
-include("ssl_cipher.hrl").

%% Handling of incoming data
-export([get_tls_records/2, init_connection_states/2]).

%% Encoding TLS records
-export([encode_handshake/3, encode_alert_record/3,
	 encode_change_cipher_spec/2, encode_data/3]).
-export([encode_plain_text/4]).

%% Protocol version handling
-export([protocol_version/1,  lowest_protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, highest_protocol_version/2,
	 is_higher/2, supported_protocol_versions/0,
	 is_acceptable_version/1, is_acceptable_version/2]).

%% Decoding
-export([decode_cipher_text/3]).

-export_type([tls_version/0, tls_atom_version/0]).

-type tls_version()       :: ssl_record:ssl_version().
-type tls_atom_version()  :: sslv3 | tlsv1 | 'tlsv1.1' | 'tlsv1.2'.

-compile(inline).

%%====================================================================
%% Internal application API
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
    Current = initial_connection_state(ConnectionEnd, BeastMitigation),
    Pending = ssl_record:empty_connection_state(ConnectionEnd, BeastMitigation),
    #{current_read  => Current,
      pending_read  => Pending,
      current_write => Current,
      pending_write => Pending}.

%%--------------------------------------------------------------------
-spec get_tls_records(binary(), binary()) -> {[binary()], binary()} | #alert{}.
%%			     
%% and returns it as a list of tls_compressed binaries also returns leftover
%% Description: Given old buffer and new data from TCP, packs up a records
%% data
%%--------------------------------------------------------------------
get_tls_records(Data, <<>>) ->
    get_tls_records_aux(Data, []);
get_tls_records(Data, Buffer) ->
    get_tls_records_aux(list_to_binary([Buffer, Data]), []).

%%--------------------------------------------------------------------
-spec encode_handshake(iolist(), tls_version(), ssl_record:connection_states()) ->
			      {iolist(), ssl_record:connection_states()}.
%
%% Description: Encodes a handshake message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_handshake(Frag, Version, 
		 #{current_write :=
		       #{beast_mitigation := BeastMitigation,
			  security_parameters :=
			     #security_parameters{bulk_cipher_algorithm = BCA}}} = 
		     ConnectionStates) ->
    case iolist_size(Frag) of
	N  when N > ?MAX_PLAIN_TEXT_LENGTH ->
	    Data = split_bin(iolist_to_binary(Frag), ?MAX_PLAIN_TEXT_LENGTH, Version, BCA, BeastMitigation),
	    encode_iolist(?HANDSHAKE, Data, Version, ConnectionStates);
	_  ->
	    encode_plain_text(?HANDSHAKE, Version, Frag, ConnectionStates)
    end.

%%--------------------------------------------------------------------
-spec encode_alert_record(#alert{}, tls_version(), ssl_record:connection_states()) ->
				 {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes an alert message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_alert_record(#alert{level = Level, description = Description},
                    Version, ConnectionStates) ->
    encode_plain_text(?ALERT, Version, <<?BYTE(Level), ?BYTE(Description)>>,
		      ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_change_cipher_spec(tls_version(), ssl_record:connection_states()) ->
				       {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes a change_cipher_spec-message to send on the ssl socket.
%%--------------------------------------------------------------------
encode_change_cipher_spec(Version, ConnectionStates) ->
    encode_plain_text(?CHANGE_CIPHER_SPEC, Version, ?byte(?CHANGE_CIPHER_SPEC_PROTO), ConnectionStates).

%%--------------------------------------------------------------------
-spec encode_data(binary(), tls_version(), ssl_record:connection_states()) ->
			 {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes data to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_data(Frag, Version,
	    #{current_write := #{beast_mitigation := BeastMitigation,
				 security_parameters :=
				     #security_parameters{bulk_cipher_algorithm = BCA}}} =
		ConnectionStates) ->
    Data = split_bin(Frag, ?MAX_PLAIN_TEXT_LENGTH, Version, BCA, BeastMitigation),
    encode_iolist(?APPLICATION_DATA, Data, Version, ConnectionStates).


%%--------------------------------------------------------------------
-spec protocol_version(tls_atom_version() | tls_version()) -> 
			      tls_version() | tls_atom_version().		      
%%     
%% Description: Creates a protocol version record from a version atom
%% or vice versa.
%%--------------------------------------------------------------------
protocol_version('tlsv1.2') ->
    {3, 3};
protocol_version('tlsv1.1') ->
    {3, 2};
protocol_version(tlsv1) ->
    {3, 1};
protocol_version(sslv3) ->
    {3, 0};
protocol_version(sslv2) -> %% Backwards compatibility
    {2, 0};
protocol_version({3, 3}) ->
    'tlsv1.2';
protocol_version({3, 2}) ->
    'tlsv1.1';
protocol_version({3, 1}) ->
    tlsv1;
protocol_version({3, 0}) ->
    sslv3.
%%--------------------------------------------------------------------
-spec lowest_protocol_version(tls_version(), tls_version()) -> tls_version().
%%     
%% Description: Lowes protocol version of two given versions 
%%--------------------------------------------------------------------
lowest_protocol_version(Version = {M, N}, {M, O})   when N < O ->
    Version;
lowest_protocol_version({M, _}, 
			Version = {M, _}) ->
    Version;
lowest_protocol_version(Version = {M,_}, 
			{N, _}) when M < N ->
    Version;
lowest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec lowest_protocol_version([tls_version()]) -> tls_version().
%%     
%% Description: Lowest protocol version present in a list
%%--------------------------------------------------------------------
lowest_protocol_version([]) ->
    lowest_protocol_version();
lowest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    lowest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version([tls_version()]) -> tls_version().
%%     
%% Description: Highest protocol version present in a list
%%--------------------------------------------------------------------
highest_protocol_version([]) ->
    highest_protocol_version();
highest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    highest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version(tls_version(), tls_version()) -> tls_version().
%%     
%% Description: Highest protocol version of two given versions 
%%--------------------------------------------------------------------
highest_protocol_version(Version = {M, N}, {M, O})   when N > O ->
    Version;
highest_protocol_version({M, _}, 
			Version = {M, _}) ->
    Version;
highest_protocol_version(Version = {M,_}, 
			{N, _}) when M > N ->
    Version;
highest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec is_higher(V1 :: tls_version(), V2::tls_version()) -> boolean().
%%     
%% Description: Is V1 > V2
%%--------------------------------------------------------------------
is_higher({M, N}, {M, O}) when N > O ->
    true;
is_higher({M, _}, {N, _}) when M > N ->
    true; 
is_higher(_, _) ->
    false.

%%--------------------------------------------------------------------
-spec supported_protocol_versions() -> [tls_version()].					 
%%
%% Description: Protocol versions supported
%%--------------------------------------------------------------------
supported_protocol_versions() ->
    Fun = fun(Version) ->
		  protocol_version(Version) 
	  end,
    case application:get_env(ssl, protocol_version) of
	undefined ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, []} ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, Vsns} when is_list(Vsns) ->
	    Versions = lists:filter(fun is_acceptable_version/1, lists:map(Fun, Vsns)),
	    supported_protocol_versions(Versions);
	{ok, Vsn} ->
	    Versions = lists:filter(fun is_acceptable_version/1, [Fun(Vsn)]),
	    supported_protocol_versions(Versions)
    end.

supported_protocol_versions([]) ->
    Vsns = case sufficient_tlsv1_2_crypto_support() of
	       true ->
		   ?ALL_SUPPORTED_VERSIONS;
	       false ->
		   ?MIN_SUPPORTED_VERSIONS
	   end,
    application:set_env(ssl, protocol_version, Vsns),
    Vsns;

supported_protocol_versions([_|_] = Vsns) ->
    case sufficient_tlsv1_2_crypto_support() of
	true -> 
	    Vsns;
	false ->
	    case Vsns -- ['tlsv1.2'] of
		[] ->
		    ?MIN_SUPPORTED_VERSIONS;
		NewVsns ->
		    NewVsns
	    end
    end.
%%--------------------------------------------------------------------
%%     
%% Description: ssl version 2 is not acceptable security risks are too big.
%% 
%%--------------------------------------------------------------------
-spec is_acceptable_version(tls_version()) -> boolean().
is_acceptable_version({N,_}) 
  when N >= ?LOWEST_MAJOR_SUPPORTED_VERSION ->
    true;
is_acceptable_version(_) ->
    false.

-spec is_acceptable_version(tls_version(), Supported :: [tls_version()]) -> boolean().
is_acceptable_version({N,_} = Version, Versions)   
  when N >= ?LOWEST_MAJOR_SUPPORTED_VERSION ->
    lists:member(Version, Versions);
is_acceptable_version(_,_) ->
    false.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_connection_state(ConnectionEnd, BeastMitigation) ->
    #{security_parameters =>
	  ssl_record:initial_security_params(ConnectionEnd),
      sequence_number => 0,
      beast_mitigation => BeastMitigation,
      compression_state  => undefined,
      cipher_state  => undefined,
      mac_secret  => undefined,
      secure_renegotiation => undefined,
      client_verify_data => undefined,
      server_verify_data => undefined
     }.

get_tls_records_aux(<<?BYTE(?APPLICATION_DATA),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), Data:Length/binary, Rest/binary>>, 
		    Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?APPLICATION_DATA,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
get_tls_records_aux(<<?BYTE(?HANDSHAKE),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), 
		     Data:Length/binary, Rest/binary>>, Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?HANDSHAKE,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
get_tls_records_aux(<<?BYTE(?ALERT),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), Data:Length/binary, 
		     Rest/binary>>, Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?ALERT,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
get_tls_records_aux(<<?BYTE(?CHANGE_CIPHER_SPEC),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), Data:Length/binary, Rest/binary>>, 
		    Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?CHANGE_CIPHER_SPEC,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
%% Matches an ssl v2 client hello message.
%% The server must be able to receive such messages, from clients that
%% are willing to use ssl v3 or higher, but have ssl v2 compatibility.
get_tls_records_aux(<<1:1, Length0:15, Data0:Length0/binary, Rest/binary>>,
		    Acc) ->
    case Data0 of
	<<?BYTE(?CLIENT_HELLO), ?BYTE(MajVer), ?BYTE(MinVer), _/binary>> ->
	    Length = Length0-1,
	    <<?BYTE(_), Data1:Length/binary>> = Data0,
	    Data = <<?BYTE(?CLIENT_HELLO), ?UINT24(Length), Data1/binary>>,
	    get_tls_records_aux(Rest, [#ssl_tls{type = ?HANDSHAKE,
						version = {MajVer, MinVer},
						fragment = Data} | Acc]);
	_ ->
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
	    
    end;

get_tls_records_aux(<<0:1, _CT:7, ?BYTE(_MajVer), ?BYTE(_MinVer),
                     ?UINT16(Length), _/binary>>,
                    _Acc) when Length > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_tls_records_aux(<<1:1, Length0:15, _/binary>>,_Acc) 
  when Length0 > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_tls_records_aux(Data, Acc) ->
    case size(Data) =< ?MAX_CIPHER_TEXT_LENGTH + ?INITIAL_BYTES of
	true ->
	    {lists:reverse(Acc), Data};
	false ->
	    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE)
	end.

encode_plain_text(Type, Version, Data, #{current_write := Write0} = ConnectionStates) ->
    {CipherFragment, Write1} = ssl_record:encode_plain_text(Type, Version, Data, Write0),
    {CipherText, Write} = encode_tls_cipher_text(Type, Version, CipherFragment, Write1),
    {CipherText, ConnectionStates#{current_write => Write}}.

lowest_list_protocol_version(Ver, []) ->
    Ver;
lowest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    lowest_list_protocol_version(lowest_protocol_version(Ver1, Ver2), Rest).

highest_list_protocol_version(Ver, []) ->
    Ver;
highest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    highest_list_protocol_version(highest_protocol_version(Ver1, Ver2), Rest).

encode_tls_cipher_text(Type, {MajVer, MinVer}, Fragment, #{sequence_number := Seq} = Write) ->
    Length = erlang:iolist_size(Fragment),
    {[<<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Length)>>, Fragment],
     Write#{sequence_number => Seq +1}}.

highest_protocol_version() ->
    highest_protocol_version(supported_protocol_versions()).

lowest_protocol_version() ->
    lowest_protocol_version(supported_protocol_versions()).

sufficient_tlsv1_2_crypto_support() ->
    CryptoSupport = crypto:supports(),
    proplists:get_bool(sha256, proplists:get_value(hashs, CryptoSupport)).

encode_iolist(Type, Data, Version, ConnectionStates0) ->
    {ConnectionStates, EncodedMsg} =
        lists:foldl(fun(Text, {CS0, Encoded}) ->
			    {Enc, CS1} =
				encode_plain_text(Type, Version, Text, CS0),
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

%%--------------------------------------------------------------------
-spec decode_cipher_text(#ssl_tls{}, ssl_record:connection_states(), boolean()) ->
				{#ssl_tls{}, ssl_record:connection_states()}| #alert{}.
%%
%% Description: Decode cipher text
%%--------------------------------------------------------------------
decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    fragment = CipherFragment} = CipherText,
		   #{current_read :=
			 #{compression_state := CompressionS0,
			   sequence_number := Seq,
			   security_parameters :=
			       #security_parameters{
				  cipher_type = ?AEAD,
				  compression_algorithm = CompAlg}
			  } = ReadState0} = ConnnectionStates0, _) ->
    AAD = ssl_cipher:calc_aad(Type, Version, ReadState0),
    case ssl_record:decipher_aead(Version, CipherFragment, ReadState0, AAD) of
	{PlainFragment, ReadState1} ->
	    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    ConnnectionStates = ConnnectionStates0#{
				  current_read => ReadState1#{sequence_number => Seq + 1,
							      compression_state => CompressionS1}},
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	#alert{} = Alert ->
	    Alert
    end;

decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    fragment = CipherFragment} = CipherText,
		   #{current_read :=
			 #{compression_state := CompressionS0,
			   sequence_number := Seq,
			   security_parameters :=
			       #security_parameters{compression_algorithm = CompAlg}
			  } = ReadState0} = ConnnectionStates0, PaddingCheck) ->
    case ssl_record:decipher(Version, CipherFragment, ReadState0, PaddingCheck) of
	{PlainFragment, Mac, ReadState1} ->
	    MacHash = ssl_cipher:calc_mac_hash(Type, Version, PlainFragment, ReadState1),
	    case ssl_record:is_correct_mac(Mac, MacHash) of
		true ->
		    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
								   PlainFragment, CompressionS0),
		    ConnnectionStates = ConnnectionStates0#{
					  current_read => ReadState1#{
							    sequence_number => Seq + 1,
							    compression_state => CompressionS1}},
		    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
		false ->
			?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
	    end;
	    #alert{} = Alert ->
	    Alert
    end. 
