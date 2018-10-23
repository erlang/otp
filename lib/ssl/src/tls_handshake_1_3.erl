%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
%% Purpose: Help funtions for handling the TLS 1.3 (specific parts of)
%%% TLS handshake protocol
%%----------------------------------------------------------------------

-module(tls_handshake_1_3).

-include("tls_handshake_1_3.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Encode
-export([encode_handshake/1, decode_handshake/2]).

%% Handshake
-export([handle_client_hello/5]).

encode_handshake(#certificate_request_1_3{
                    certificate_request_context = Context, 
                    extensions = Exts})->
    EncContext = encode_cert_req_context(Context),
    BinExts = encode_extensions(Exts),
    {?CERTIFICATE_REQUEST, <<EncContext/binary, BinExts/binary>>};
encode_handshake(#certificate_1_3{
                    certificate_request_context = Context, 
                    entries = Entries}) ->
    EncContext = encode_cert_req_context(Context),
    EncEntries = encode_cert_entries(Entries),
    {?CERTIFICATE, <<EncContext/binary, EncEntries/binary>>};
encode_handshake(#encrypted_extensions{extensions = Exts})->
    {?ENCRYPTED_EXTENSIONS, encode_extensions(Exts)};        
encode_handshake(#new_session_ticket{
                    ticket_lifetime = LifeTime,  
                    ticket_age_add = Age,   
                    ticket_nonce = Nonce,     
                    ticket = Ticket,           
                    extensions = Exts}) ->
    TicketSize = byte_size(Ticket),
    BinExts = encode_extensions(Exts),
    {?NEW_SESSION_TICKET, <<?UINT32(LifeTime), ?UINT32(Age),
                            ?BYTE(Nonce), ?UINT16(TicketSize), Ticket/binary,
                            BinExts/binary>>};
encode_handshake(#end_of_early_data{}) ->
    {?END_OF_EARLY_DATA, <<>>};
encode_handshake(#key_update{request_update = Update}) ->
    {?KEY_UPDATE, <<?BYTE(Update)>>};
encode_handshake(HandshakeMsg) ->
    ssl_handshake:encode_handshake(HandshakeMsg, {3,4}).

decode_handshake(?CERTIFICATE_REQUEST, <<?BYTE(0), ?UINT16(Size), EncExts:Size/binary>>) ->
    Exts = decode_extensions(EncExts),
    #certificate_request_1_3{
       certificate_request_context = <<>>,
       extensions = Exts};
decode_handshake(?CERTIFICATE_REQUEST, <<?BYTE(CSize), Context:CSize/binary,
                                         ?UINT16(Size), EncExts:Size/binary>>) ->
    Exts = decode_extensions(EncExts),
    #certificate_request_1_3{
       certificate_request_context = Context,
       extensions = Exts};
decode_handshake(?CERTIFICATE, <<?BYTE(0), ?UINT24(Size), Certs:Size/binary>>) ->
    CertList = decode_cert_entries(Certs),
    #certificate_1_3{ 
       certificate_request_context = <<>>,
       entries = CertList
      };
decode_handshake(?CERTIFICATE, <<?BYTE(CSize), Context:CSize/binary,
                                 ?UINT24(Size), Certs:Size/binary>>) ->
    CertList = decode_cert_entries(Certs),
    #certificate_1_3{ 
       certificate_request_context = Context,
       entries = CertList
      };
decode_handshake(?ENCRYPTED_EXTENSIONS, <<?UINT16(Size), EncExts:Size/binary>>) ->
    #encrypted_extensions{
       extensions = decode_extensions(EncExts)
      };
decode_handshake(?NEW_SESSION_TICKET, <<?UINT32(LifeTime), ?UINT32(Age),
                                        ?BYTE(Nonce), ?UINT16(TicketSize), Ticket:TicketSize/binary,
                                        BinExts/binary>>) ->
    Exts = decode_extensions(BinExts),
    #new_session_ticket{ticket_lifetime = LifeTime,  
                        ticket_age_add = Age,   
                        ticket_nonce = Nonce,     
                        ticket = Ticket,           
                        extensions = Exts};
decode_handshake(?END_OF_EARLY_DATA, _) ->
    #end_of_early_data{};
decode_handshake(?KEY_UPDATE, <<?BYTE(Update)>>) ->
    #key_update{request_update = Update};
decode_handshake(Tag, HandshakeMsg) ->
    ssl_handshake:decode_handshake({3,4}, Tag, HandshakeMsg).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encode_cert_req_context(<<>>) ->
    <<?BYTE(0)>>;
encode_cert_req_context(Bin) ->
    Size = byte_size(Bin),
    <<?BYTE(Size), Bin/binary>>.

encode_cert_entries(Entries) ->
    CertEntryList = encode_cert_entries(Entries, []),
    Size = byte_size(CertEntryList),
    <<?UINT24(Size), CertEntryList/binary>>.
 
encode_cert_entries([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
encode_cert_entries([#certificate_entry{data = Data,
                                        extensions = Exts} | Rest], Acc) ->
    DSize = byte_size(Data),
    BinExts = encode_extensions(Exts),
    ExtSize = byte_size(BinExts),
    encode_cert_entries(Rest, 
                        [<<?UINT24(DSize), Data/binary, ?UINT16(ExtSize), BinExts/binary>> | Acc]).

decode_cert_entries(Entries) ->
    decode_cert_entries(Entries, []).

decode_cert_entries(<<>>, Acc) ->
    lists:reverse(Acc);
decode_cert_entries(<<?UINT24(DSize), Data:DSize/binary, ?UINT16(Esize), BinExts:Esize/binary,
                      Rest/binary>>, Acc) ->
    Exts = decode_extensions(BinExts),
    decode_cert_entries(Rest, [#certificate_entry{data = Data,
                                                  extensions = Exts} | Acc]).

encode_extensions(Exts)->
    ssl_handshake:encode_extensions(extensions_list(Exts)).
decode_extensions(Exts) ->
    ssl_handshake:decode_extensions(Exts).

extensions_list(HelloExtensions) ->
    [Ext || {_, Ext} <- maps:to_list(HelloExtensions)].


handle_client_hello(Version,
                    #client_hello{session_id = SugesstedId,
                                  cipher_suites = CipherSuites,
                                  compression_methods = Compressions,
                                  random = Random,
                                  extensions = HelloExt},
		    #ssl_options{versions = Versions,
				 signature_algs = SupportedHashSigns,
				 eccs = SupportedECCs,
				 honor_ecc_order = ECCOrder} = SslOpts,
		    {Port, Session0, Cache, CacheCb, ConnectionStates0, Cert, _},
                    Renegotiation) ->
    case tls_record:is_acceptable_version(Version, Versions) of
	true ->
            %% Get supported_groups
            %% SupportedGroups = maps:get(elliptic_curves, HelloExt, undefined),
            %% Get KeyShareClientHello

            %% Validate supported_groups + KeyShareClientHello
            %% IF valid THEN
            %%   IF supported_groups IS empty send HelloRetryRequest
            %%   ELSE continue
            %% ELSE
            %%   send Alert
            %% ClientHashSigns = maps:get(signature_algs, HelloExt, undefined),
            %% ClientSignatureSchemes = maps:get(signature_algs_cert, HelloExt, undefined),

            %% Implement session handling.

            %% Select curve

            %% Sessions cannot be resumed by ClientHello

            %% Select cipher_suite
            %% Select hash_sign

            %% Handle extensions
	    ok;
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end.

