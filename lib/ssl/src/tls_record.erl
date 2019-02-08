%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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
-export([get_tls_records/3, init_connection_states/2]).

%% Encoding TLS records
-export([encode_handshake/3, encode_alert_record/3,
	 encode_change_cipher_spec/2, encode_data/3]).
-export([encode_plain_text/4]).

%% Decoding
-export([decode_cipher_text/3]).

%% Protocol version handling
-export([protocol_version/1,  lowest_protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, highest_protocol_version/2,
	 is_higher/2, supported_protocol_versions/0,
	 is_acceptable_version/1, is_acceptable_version/2, hello_version/2]).

-export_type([tls_version/0, tls_atom_version/0]).

-type tls_version()       :: ssl_record:ssl_version().
-type tls_atom_version()  :: sslv3 | tlsv1 | 'tlsv1.1' | 'tlsv1.2'.

-compile(inline).

%%====================================================================
%% Handling of incoming data
%%====================================================================
%%--------------------------------------------------------------------
-spec init_connection_states(client | server, one_n_minus_one | zero_n | disabled) ->
 				    ssl_record:connection_states().
%% 
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
-spec get_tls_records(binary(), [tls_version()] | tls_version(), binary()) -> {[binary()], binary()} | #alert{}.
%%			     
%% and returns it as a list of tls_compressed binaries also returns leftover
%% Description: Given old buffer and new data from TCP, packs up a records
%% data
%%--------------------------------------------------------------------

get_tls_records(Data, Versions, Buffer) when is_binary(Buffer) ->
    parse_tls_records(Versions, {[Data],byte_size(Data),[]}, undefined);
get_tls_records(Data, Versions, {Hdr, {Front,Size,Rear}}) ->
    parse_tls_records(Versions, {Front,Size + byte_size(Data),[Data|Rear]}, Hdr).

%%====================================================================
%% Encoding
%%====================================================================

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
	    Data = split_bin(iolist_to_binary(Frag), Version, BCA, BeastMitigation),
	    encode_iolist(?HANDSHAKE, Version, Data, ConnectionStates);
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
    Data = split_bin(Frag, Version, BCA, BeastMitigation),
    encode_iolist(?APPLICATION_DATA, Version, Data, ConnectionStates).

%%====================================================================
%% Decoding
%%====================================================================

%%--------------------------------------------------------------------
-spec decode_cipher_text(#ssl_tls{}, ssl_record:connection_states(), boolean()) ->
				{#ssl_tls{}, ssl_record:connection_states()}| #alert{}.
%%
%% Description: Decode cipher text
%%--------------------------------------------------------------------
decode_cipher_text(CipherText,
		   #{current_read :=
			 #{sequence_number := Seq,
			   security_parameters :=
                               #security_parameters{cipher_type = ?AEAD,
                                                    bulk_cipher_algorithm = BulkCipherAlgo},
                           cipher_state := CipherS0
                          }
                    } = ConnectionStates0, _) ->
    SeqBin = <<?UINT64(Seq)>>,
    CipherS1 = ssl_record:nonce_seed(BulkCipherAlgo, SeqBin, CipherS0),
    #ssl_tls{type = Type, version = {MajVer,MinVer} = Version, fragment = Fragment} = CipherText,
    StartAdditionalData = <<SeqBin/binary, ?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer)>>,
    case ssl_record:decipher_aead(
           BulkCipherAlgo, CipherS1, StartAdditionalData, Fragment, Version)
    of
	{PlainFragment, CipherS} ->
            #{current_read :=
                  #{security_parameters := SecParams,
                    compression_state := CompressionS0} = ReadState0} = ConnectionStates0,
	    {Plain, CompressionS} = ssl_record:uncompress(SecParams#security_parameters.compression_algorithm,
                                                          PlainFragment, CompressionS0),
	    ConnectionStates = ConnectionStates0#{
				  current_read => ReadState0#{
                                                    cipher_state => CipherS,
                                                    sequence_number => Seq + 1,
                                                    compression_state => CompressionS}},
	    {CipherText#ssl_tls{fragment = Plain}, ConnectionStates};
	#alert{} = Alert ->
	    Alert
    end;

decode_cipher_text(#ssl_tls{version = Version,
			    fragment = CipherFragment} = CipherText,
		   #{current_read := ReadState0} = ConnnectionStates0, PaddingCheck) ->
    case ssl_record:decipher(Version, CipherFragment, ReadState0, PaddingCheck) of
	{PlainFragment, Mac, ReadState1} ->
	    MacHash = ssl_cipher:calc_mac_hash(CipherText#ssl_tls.type, Version, PlainFragment, ReadState1),
	    case ssl_record:is_correct_mac(Mac, MacHash) of
		true ->
                    #{sequence_number := Seq,
                      compression_state := CompressionS0,
                      security_parameters :=
                          #security_parameters{compression_algorithm = CompAlg}} = ReadState0,
		    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
								   PlainFragment, CompressionS0),
		    ConnnectionStates =
                        ConnnectionStates0#{current_read =>
                                                ReadState1#{sequence_number => Seq + 1,
                                                            compression_state => CompressionS1}},
		    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
		false ->
                    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
	    end;
	    #alert{} = Alert ->
	    Alert
    end.

%%====================================================================
%% Protocol version handling
%%====================================================================

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

-spec hello_version(tls_version(), [tls_version()]) -> tls_version().
hello_version(Version, _) when Version >= {3, 3} ->
    Version;
hello_version(_, Versions) ->
    lowest_protocol_version(Versions).

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


parse_tls_records(Versions, Q, undefined) ->
    decode_tls_records(Versions, Q, [], undefined, undefined, undefined);
parse_tls_records(Versions, Q, #ssl_tls{type = Type, version = Version, fragment = Length}) ->
    decode_tls_records(Versions, Q, [], Type, Version, Length).

decode_tls_records(Versions, {_,Size,_} = Q0, Acc, undefined, _Version, _Length) ->
    if
        5 =< Size ->
            {<<?BYTE(Type),?BYTE(MajVer),?BYTE(MinVer), ?UINT16(Length)>>, Q} = binary_from_front(5, Q0),
            validate_tls_records_type(Versions, Q, Acc, Type, {MajVer,MinVer}, Length);
        3 =< Size ->
            {<<?BYTE(Type),?BYTE(MajVer),?BYTE(MinVer)>>, Q} = binary_from_front(3, Q0),
            validate_tls_records_type(Versions, Q, Acc, Type, {MajVer,MinVer}, undefined);
        1 =< Size ->
            {<<?BYTE(Type)>>, Q} = binary_from_front(1, Q0),
            validate_tls_records_type(Versions, Q, Acc, Type, undefined, undefined);
        true ->
            validate_tls_records_type(Versions, Q0, Acc, undefined, undefined, undefined)
    end;
decode_tls_records(Versions, {_,Size,_} = Q0, Acc, Type, undefined, _Length) ->
    if
        4 =< Size ->
            {<<?BYTE(MajVer),?BYTE(MinVer), ?UINT16(Length)>>, Q} = binary_from_front(4, Q0),
            validate_tls_record_version(Versions, Q, Acc, Type, {MajVer,MinVer}, Length);
        2 =< Size ->
            {<<?BYTE(MajVer),?BYTE(MinVer)>>, Q} = binary_from_front(2, Q0),
            validate_tls_record_version(Versions, Q, Acc, Type, {MajVer,MinVer}, undefined);
        true ->
            validate_tls_record_version(Versions, Q0, Acc, Type, undefined, undefined)
    end;
decode_tls_records(Versions, {_,Size,_} = Q0, Acc, Type, Version, undefined) ->
    if
        2 =< Size ->
            {<<?UINT16(Length)>>, Q} = binary_from_front(2, Q0),
            validate_tls_record_length(Versions, Q, Acc, Type, Version, Length);
        true ->
            validate_tls_record_length(Versions, Q0, Acc, Type, Version, undefined)
    end;
decode_tls_records(Versions, Q, Acc, Type, Version, Length) ->
    validate_tls_record_length(Versions, Q, Acc, Type, Version, Length).

validate_tls_records_type(_Versions, Q, Acc, undefined, _Version, _Length) ->
    {lists:reverse(Acc),
     {undefined, Q}};
validate_tls_records_type(Versions, Q, Acc, Type, Version, Length) ->
    if
        ?KNOWN_RECORD_TYPE(Type) ->
            validate_tls_record_version(Versions, Q, Acc, Type, Version, Length);
        true ->
            %% Not ?KNOWN_RECORD_TYPE(Type)
            ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE)
    end.

validate_tls_record_version(_Versions, Q, Acc, Type, undefined, _Length) ->
    {lists:reverse(Acc),
     {#ssl_tls{type = Type, version = undefined, fragment = undefined}, Q}};
validate_tls_record_version(Versions, Q, Acc, Type, Version, Length) ->
    if
        is_list(Versions) ->
            case is_acceptable_version(Version, Versions) of
                true ->
                    validate_tls_record_length(Versions, Q, Acc, Type, Version, Length);
                false ->
                    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
            end;
        Version =:= Versions ->
            %% Exact version match
            validate_tls_record_length(Versions, Q, Acc, Type, Version, Length);
        true ->
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
    end.

validate_tls_record_length(_Versions, Q, Acc, Type, Version, undefined) ->
    {lists:reverse(Acc),
     {#ssl_tls{type = Type, version = Version, fragment = undefined}, Q}};
validate_tls_record_length(Versions, {_,Size0,_} = Q0, Acc, Type, Version, Length) ->
    if
        Length =< ?MAX_CIPHER_TEXT_LENGTH ->
            if
                Length =< Size0 ->
                    %% Complete record
                    {Fragment, Q} = binary_from_front(Length, Q0),
                    Record = #ssl_tls{type = Type, version = Version, fragment = Fragment},
                    decode_tls_records(Versions, Q, [Record|Acc], undefined, undefined, undefined);
                true ->
                    {lists:reverse(Acc),
                     {#ssl_tls{type = Type, version = Version, fragment = Length}, Q0}}
            end;
        true ->
            ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW)
    end.


binary_from_front(BinSize, {Front,Size,Rear}) ->
    binary_from_front(BinSize, Front, Size, Rear, []).
%%
binary_from_front(BinSize, [], Size, [_] = Rear, Acc) ->
    %% Optimize a simple case
    binary_from_front(BinSize, Rear, Size, [], Acc);
binary_from_front(BinSize, [], Size, Rear, Acc) ->
    binary_from_front(BinSize, lists:reverse(Rear), Size, [], Acc);
binary_from_front(BinSize, [Bin|Front], Size, Rear, []) ->
    %% Optimize a frequent case
    case Bin of
        <<RetBin:BinSize/binary, Rest/binary>> -> % More than enough - split here
            {RetBin, {case Rest of
                          <<>> ->
                              Front;
                          <<_/binary>> ->
                              [Rest|Front]
                      end,Size - BinSize,Rear}};
        <<_/binary>> -> % Not enough
            binary_from_front(BinSize - byte_size(Bin), Front, Size, Rear, [Bin])
    end;
binary_from_front(BinSize, [Bin|Front], Size, Rear, Acc) ->
    case Bin of
        <<Last:BinSize/binary, Rest/binary>> -> % More than enough - split here
            RetBin = iolist_to_binary(lists:reverse(Acc, [Last])),
            {RetBin,{case Rest of
                         <<>> ->
                             Front;
                         <<_/binary>> ->
                             [Rest|Front]
                     end,Size - byte_size(RetBin),Rear}};
        <<_/binary>> -> % Not enough
            binary_from_front(BinSize - byte_size(Bin), Front, Size, Rear, [Bin|Acc])
    end.

%%--------------------------------------------------------------------
encode_plain_text(Type, Version, Data, ConnectionStates0) ->
    {[CipherText],ConnectionStates} = encode_iolist(Type, Version, [Data], ConnectionStates0),
    {CipherText,ConnectionStates}.
%%--------------------------------------------------------------------
encode_iolist(Type, Version, Data,
              #{current_write := #{compression_state := CompS,
                                   cipher_state := CipherS,
                                   sequence_number := Seq}} = ConnectionStates) ->
    encode_iolist(Type, Version, Data, ConnectionStates, CompS, CipherS, Seq, []).
%%
encode_iolist(_Type, _Version, [], #{current_write := WriteS} = CS,
              CompS, CipherS, Seq, CipherFragments) ->
    {lists:reverse(CipherFragments),
     CS#{current_write := WriteS#{compression_state := CompS,
                                  cipher_state := CipherS,
                                  sequence_number := Seq}}};
encode_iolist(Type, Version, [Text|Data],
              #{current_write := #{security_parameters :=
                                       #security_parameters{cipher_type = ?AEAD,
                                                            bulk_cipher_algorithm = BCAlg,
                                                            compression_algorithm = CompAlg} = SecPars}} = CS,
              CompS0, CipherS0, Seq, CipherFragments) ->
    {CompText, CompS} = ssl_record:compress(CompAlg, Text, CompS0),
    SeqBin = <<?UINT64(Seq)>>,
    CipherS1 = ssl_record:nonce_seed(BCAlg, SeqBin, CipherS0),
    {MajVer, MinVer} = Version,
    VersionBin = <<?BYTE(MajVer), ?BYTE(MinVer)>>,
    StartAdditionalData = <<SeqBin/binary, ?BYTE(Type), VersionBin/binary>>,
    {CipherFragment,CipherS} = ssl_record:cipher_aead(Version, CompText, CipherS1, StartAdditionalData, SecPars),
    Length = byte_size(CipherFragment),
    CipherHeader = <<?BYTE(Type), VersionBin/binary, ?UINT16(Length)>>,
    encode_iolist(Type, Version, Data, CS, CompS, CipherS, Seq + 1,
                  [[CipherHeader, CipherFragment] | CipherFragments]);
encode_iolist(Type, Version, [Text|Data],
              #{current_write := #{security_parameters :=
                                       #security_parameters{compression_algorithm = CompAlg,
                                                            mac_algorithm = MacAlgorithm} = SecPars,
                                   mac_secret := MacSecret}} = CS,
              CompS0, CipherS0, Seq, CipherFragments) ->
    {CompText, CompS} = ssl_record:compress(CompAlg, Text, CompS0),
    MacHash = ssl_cipher:calc_mac_hash(Type, Version, CompText, MacAlgorithm, MacSecret, Seq),
    {CipherFragment,CipherS} = ssl_record:cipher(Version, CompText, CipherS0, MacHash, SecPars),
    Length = byte_size(CipherFragment),
    {MajVer, MinVer} = Version,
    CipherHeader = <<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Length)>>,
    encode_iolist(Type, Version, Data, CS, CompS, CipherS, Seq + 1,
                  [[CipherHeader, CipherFragment] | CipherFragments]);
encode_iolist(_Type, _Version, _Data, CS, _CompS, _CipherS, _Seq, _CipherFragments) ->
    exit({cs, CS}).
%%--------------------------------------------------------------------

%% 1/n-1 splitting countermeasure Rizzo/Duong-Beast, RC4 chiphers are
%% not vulnerable to this attack.
split_bin(<<FirstByte:8, Rest/binary>>, Version, BCA, one_n_minus_one) when
      BCA =/= ?RC4 andalso ({3, 1} == Version orelse
			    {3, 0} == Version) ->
    [[FirstByte]|do_split_bin(Rest)];
%% 0/n splitting countermeasure for clients that are incompatible with 1/n-1
%% splitting.
split_bin(Bin, Version, BCA, zero_n) when
      BCA =/= ?RC4 andalso ({3, 1} == Version orelse
			    {3, 0} == Version) ->
    [<<>>|do_split_bin(Bin)];
split_bin(Bin, _, _, _) ->
    do_split_bin(Bin).

do_split_bin(<<>>) -> [];
do_split_bin(Bin) ->
    case Bin of
        <<Chunk:?MAX_PLAIN_TEXT_LENGTH/binary, Rest/binary>> ->
            [Chunk|do_split_bin(Rest)];
        _ ->
            [Bin]
    end.
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

sufficient_tlsv1_2_crypto_support() ->
    CryptoSupport = crypto:supports(),
    proplists:get_bool(sha256, proplists:get_value(hashs, CryptoSupport)).


