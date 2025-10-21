%% 
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
%% AES: RFC 3826
%% 

-module(snmp_usm).
-moduledoc false.

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([passwd2localized_key/3, localize_key/3]).
-export([auth_in/4, auth_out/4, set_msg_auth_params/3]).
-export([des_encrypt/3, des_decrypt/3]).
-export([aes_encrypt/5, aes_decrypt/5]).

-export_type([
              algorithm/0
             ]).


-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMP-USM-AES-MIB.hrl").
-include("SNMP-USM-HMAC-SHA2-MIB.hrl").
-include("snmp_usm.hrl").

-define(VMODULE,"USM").
-include("snmp_verbosity.hrl").


%%-----------------------------------------------------------------

-define(zeros12, [0,0,0,0,0,0,0,0,0,0,0,0]).
-define(zeros16, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
-define(zeros24, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0]).
-define(zeros32, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
-define(zeros48, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).

-define(i32(Int), (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8) band 255, Int band 255).

-define(BLOCK_CIPHER_AES(Key), case bit_size(iolist_to_binary(Key)) of
                                   128 -> aes_128_cfb128;
                                   192 -> aes_192_cfb128;
                                   256 -> aes_256_cfb128
                               end).

-define(BLOCK_CIPHER_DES, des_cbc).

-type algorithm() :: md5 | sha | sha224 | sha256 | sha384 | sha512.


%%-----------------------------------------------------------------
%% Func: passwd2localized_key/3
%% Types: Alg      = algorithm()
%%        Passwd   = string()
%%        EngineID = string()
%% Purpose: Generates a key that can be used as an authentication
%%          or privacy key using MD5 och SHA.  The key is
%%          localized for EngineID.
%%          The algorithm is described in appendix A.1 2) of
%%          rfc2274.
%%-----------------------------------------------------------------

-spec passwd2localized_key(Alg      :: algorithm(),
                           Passwd   :: string(),
                           EngineID :: string()) ->
          LocalizedKey :: list().

passwd2localized_key(Alg, Passwd, EngineID) when length(Passwd) > 0 ->
    Key = mk_digest(Alg, Passwd),
    localize_key(Alg, Key, EngineID).

    
%%-----------------------------------------------------------------
%% Func: localize_key/3
%% Types: Alg      = md5 | sha
%%        Key      = binary()
%%        EngineID = string()
%% Purpose: Localizes an unlocalized key for EngineID.  See rfc2274
%%          section 2.6 for a definition of localized keys.
%%-----------------------------------------------------------------

-spec localize_key(Alg      :: algorithm(),
                   Key      :: binary(),
                   EngineID :: string()) ->
          LocalizedKey :: list().

localize_key(Alg, Key, EngineID) ->
    Str = [Key, EngineID, Key],
    binary_to_list(crypto:hash(Alg, Str)).


mk_digest(Alg, Passwd)
  when (Alg =:= md5)    orelse
       (Alg =:= sha)    orelse
       (Alg =:= sha224) orelse
       (Alg =:= sha256) orelse 
       (Alg =:= sha384) orelse 
       (Alg =:= sha512) ->
    Ctx  = crypto:hash_init(Alg),
    Ctx2 = mk_digest_loop(0, [], Ctx, Passwd, length(Passwd)),
    crypto:hash_final(Ctx2).

mk_digest_loop(Count, Buf, Ctx, Passwd, PasswdLen) when Count < 1048576 ->
    {Buf64, NBuf} = mk_buf64(length(Buf), Buf, Passwd, PasswdLen),
    NCtx = crypto:hash_update(Ctx, Buf64),
    mk_digest_loop(Count+64, NBuf, NCtx, Passwd, PasswdLen);
mk_digest_loop(_Count, _Buf, Ctx, _Passwd, _PasswdLen) ->
    Ctx.

%% Create a 64 bytes long string, by repeating Passwd as many times 
%% as necessary. Output is the 64 byte string, and the rest of the 
%% last repetition of the Passwd. This is used as input in the next
%% invocation.
mk_buf64(BufLen, Buf, Passwd, PasswdLen) ->
    case BufLen + PasswdLen of
	TotLen when TotLen > 64 ->
	    {[Buf, lists:sublist(Passwd, 64-BufLen)],
	     lists:sublist(Passwd, 65-BufLen, PasswdLen)};
	TotLen ->
	    mk_buf64(TotLen, [Buf, Passwd], Passwd, PasswdLen)
    end.


%%-----------------------------------------------------------------
%% Auth and priv algorithms
%%-----------------------------------------------------------------

auth_in(usmHMACMD5AuthProtocol, AuthKey, AuthParams, Packet) ->
    md5_auth_in(AuthKey, AuthParams, Packet);
auth_in(?usmHMACMD5AuthProtocol, AuthKey, AuthParams, Packet) ->
    md5_auth_in(AuthKey, AuthParams, Packet);
auth_in(usmHMACSHAAuthProtocol, AuthKey, AuthParams, Packet) ->
    sha_auth_in(AuthKey, AuthParams, Packet);
auth_in(?usmHMACSHAAuthProtocol, AuthKey, AuthParams, Packet) ->
    sha_auth_in(AuthKey, AuthParams, Packet);
auth_in(usmHMAC128SHA224AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha224) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha224,
                 ?usmHMAC128SHA224AuthProtocol_secret_key_length,
                 ?usmHMAC128SHA224AuthProtocol_mac_length);
auth_in(?usmHMAC128SHA224AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha224) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha224,
                 ?usmHMAC128SHA224AuthProtocol_secret_key_length,
                 ?usmHMAC128SHA224AuthProtocol_mac_length);
auth_in(usmHMAC192SHA256AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha256) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha256,
                 ?usmHMAC192SHA256AuthProtocol_secret_key_length,
                 ?usmHMAC192SHA256AuthProtocol_mac_length);
auth_in(?usmHMAC192SHA256AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha256) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha256,
                 ?usmHMAC192SHA256AuthProtocol_secret_key_length,
                 ?usmHMAC192SHA256AuthProtocol_mac_length);
auth_in(usmHMAC256SHA384AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha384) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha384,
                 ?usmHMAC256SHA384AuthProtocol_secret_key_length,
                 ?usmHMAC256SHA384AuthProtocol_mac_length);
auth_in(?usmHMAC256SHA384AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha384) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha384,
                 ?usmHMAC256SHA384AuthProtocol_secret_key_length,
                 ?usmHMAC256SHA384AuthProtocol_mac_length);
auth_in(usmHMAC384SHA512AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha512) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha512,
                 ?usmHMAC384SHA512AuthProtocol_secret_key_length,
                 ?usmHMAC384SHA512AuthProtocol_mac_length);
auth_in(?usmHMAC384SHA512AuthProtocol, AuthKey, AuthParams, Packet) ->
    ?vtrace("auth_in(sha512) -> entry"),
    sha2_auth_in(AuthKey, AuthParams, Packet,
                 sha512,
                 ?usmHMAC384SHA512AuthProtocol_secret_key_length,
                 ?usmHMAC384SHA512AuthProtocol_mac_length).

auth_out(usmNoAuthProtocol, _AuthKey, _Message, _UsmSecParams) -> % 3.1.3
    %% ?vtrace("auth_out(noAuth) -> entry"),
    error(unSupportedSecurityLevel);
auth_out(?usmNoAuthProtocol, _AuthKey, _Message, _UsmSecParams) -> % 3.1.3
    %% ?vtrace("auth_out(noAuth) -> entry"),
    error(unSupportedSecurityLevel);
auth_out(usmHMACMD5AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(md5) -> entry"),
    md5_auth_out(AuthKey, Message, UsmSecParams);
auth_out(?usmHMACMD5AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(md5) -> entry"),
    md5_auth_out(AuthKey, Message, UsmSecParams);
auth_out(usmHMACSHAAuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha) -> entry"),
    sha_auth_out(AuthKey, Message, UsmSecParams);
auth_out(?usmHMACSHAAuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha) -> entry"),
    sha_auth_out(AuthKey, Message, UsmSecParams);
auth_out(usmHMAC128SHA224AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha224) -> entry"),
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha224,
                  ?usmHMAC128SHA224AuthProtocol_secret_key_length,
                  ?usmHMAC128SHA224AuthProtocol_mac_length);
auth_out(?usmHMAC128SHA224AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha224) -> entry"), 
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha224,
                  ?usmHMAC128SHA224AuthProtocol_secret_key_length,
                  ?usmHMAC128SHA224AuthProtocol_mac_length);
auth_out(usmHMAC192SHA256AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha256) -> entry"),
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha256,
                  ?usmHMAC192SHA256AuthProtocol_secret_key_length,
                  ?usmHMAC192SHA256AuthProtocol_mac_length);
auth_out(?usmHMAC192SHA256AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha256) -> entry"),
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha256,
                  ?usmHMAC192SHA256AuthProtocol_secret_key_length,
                  ?usmHMAC192SHA256AuthProtocol_mac_length);
auth_out(usmHMAC256SHA384AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha384) -> entry"),
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha384,
                  ?usmHMAC256SHA384AuthProtocol_secret_key_length,
                  ?usmHMAC256SHA384AuthProtocol_mac_length);
auth_out(?usmHMAC256SHA384AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha384) -> entry"),
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha384,
                  ?usmHMAC256SHA384AuthProtocol_secret_key_length,
                  ?usmHMAC256SHA384AuthProtocol_mac_length);
auth_out(usmHMAC384SHA512AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha512) -> entry"),
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha512,
                  ?usmHMAC384SHA512AuthProtocol_secret_key_length,
                  ?usmHMAC384SHA512AuthProtocol_mac_length);
auth_out(?usmHMAC384SHA512AuthProtocol, AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("auth_out(sha512) -> entry"),
    sha2_auth_out(AuthKey, Message, UsmSecParams,
                  sha512,
                  ?usmHMAC384SHA512AuthProtocol_secret_key_length,
                  ?usmHMAC384SHA512AuthProtocol_mac_length).

md5_auth_out(AuthKey, Message, UsmSecParams) ->
    %% ?vtrace("md5_auth_out -> entry with"
    %%  	    "~n   AuthKey:      ~w"
    %% 	    "~n   Message:      ~w"
    %%  	    "~n   UsmSecParams: ~w", [AuthKey, Message, UsmSecParams]),
    %% 6.3.1.1
    Message2 = set_msg_auth_params(Message, UsmSecParams, ?zeros12),
    Packet   = snmp_pdus:enc_message_only(Message2),
    %% 6.3.1.2-4 is done by the crypto function
    %% 6.3.1.4
    MAC = binary_to_list(crypto:macN(hmac, md5, AuthKey, Packet, 12)),
    %% ?vtrace("md5_auth_out -> crypto (md5) encoded"
    %%  	    "~n   MAC: ~w", [MAC]),
    %% 6.3.1.5
    set_msg_auth_params(Message, UsmSecParams, MAC).

md5_auth_in(AuthKey, AuthParams, Packet) when length(AuthParams) == 12 ->
    %% ?vtrace("md5_auth_in -> entry with"
    %%  	    "~n   AuthKey:    ~w"
    %%  	    "~n   AuthParams: ~w"
    %%  	    "~n   Packet:     ~w", [AuthKey, AuthParams, Packet]),
    %% 6.3.2.3
    Packet2 = patch_packet(binary_to_list(Packet), ?zeros12),
    %% 6.3.2.5
    MAC = binary_to_list(crypto:macN(hmac, md5, AuthKey, Packet2, 12)),
    %% 6.3.2.6
    %% ?vtrace("md5_auth_in -> crypto (md5) encoded"
    %%  	    "~n   MAC: ~w", [MAC]),
    MAC == AuthParams;
md5_auth_in(_AuthKey, _AuthParams, _Packet) ->
    %% 6.3.2.1
    ?vlog("md5_auth_in -> entry with"
          "~n   _AuthKey:    ~p"
          "~n   _AuthParams: ~p", [_AuthKey, _AuthParams]),
    false.


sha_auth_out(AuthKey, Message, UsmSecParams) ->
    %% 7.3.1.1
    Message2 = set_msg_auth_params(Message, UsmSecParams, ?zeros12),
    Packet = snmp_pdus:enc_message_only(Message2),
    %% 7.3.1.2-4 is done by the crypto function
    %% 7.3.1.4
    MAC = binary_to_list(crypto:macN(hmac, sha, AuthKey, Packet, 12)),
    %% 7.3.1.5
    set_msg_auth_params(Message, UsmSecParams, MAC).

sha_auth_in(AuthKey, AuthParams, Packet) when length(AuthParams) =:= 12 ->
    %% 7.3.2.3
    Packet2 = patch_packet(binary_to_list(Packet), ?zeros12),
    %% 7.3.2.5
    MAC = binary_to_list(crypto:macN(hmac, sha, AuthKey, Packet2, 12)),
    %% 7.3.2.6
    MAC == AuthParams;
sha_auth_in(_AuthKey, _AuthParams, _Packet) ->
    %% 7.3.2.1
    ?vlog("sha_auth_in -> entry with"
          "~n   _AuthKey:    ~p"
          "~n   _AuthParams: ~p", [_AuthKey, _AuthParams]),
    false.


%% RFC:4.2.1
sha2_auth_out(AuthKey, Message, UsmSecParams,
              HashAlg, M, N)
  when (length(AuthKey) =:= M) ->
    %% ?vtrace("sha2_auth_out -> entry with"
    %%         "~n      AuthKey:      ~p"
    %%         "~n      UsmSecParams: ~p"
    %%         "~n      HashAlg:      ~p"
    %%         "~n      M:            ~p"
    %%         "~n      N:            ~p",
    %%         [AuthKey, UsmSecParams, HashAlg, M, N]),
    %% 4.2.1:1
    Message2 = set_msg_auth_params(Message, UsmSecParams, zeros(N)),
    Packet   = snmp_pdus:enc_message_only(Message2),
    %% 7.3.1.2-4 is done by the crypto function
    %% 4.2.1:2-3
    MAC = binary_to_list(crypto:macN(hmac, HashAlg, AuthKey, Packet, N)),
    %% ?vtrace("sha2_auth_out -> "
    %%         "~n      MAC: ~p", [MAC]),
    %% 4.2.1:4-5
    set_msg_auth_params(Message, UsmSecParams, MAC).

%% RFC:4.2.2
sha2_auth_in(AuthKey, AuthParams, Packet,
             HashAlg, M, N)
  when (length(AuthKey) =:= M) andalso
       (length(AuthParams) =:= N) ->
    %% ?vtrace("sha2_auth_in -> entry with"
    %%         "~n      AuthKey:    ~p"
    %%         "~n      AuthParams: ~p"
    %%         "~n      HashAlg:    ~p"
    %%         "~n      M:          ~p"
    %%         "~n      N:          ~p", [AuthKey, AuthParams, HashAlg, M, N]),
    %% 4.2.2:3
    Packet2 = patch_packet(binary_to_list(Packet), zeros(N)),
    %% 4.2.2:4
    MAC = binary_to_list(crypto:macN(hmac, HashAlg, AuthKey, Packet2, N)),
    %% ?vtrace("sha2_auth_in -> "
    %%         "~n      MAC: ~p", [MAC]),
    %% 4.2.2:7
    %% true | {false, authenticationFailure}
    (MAC == AuthParams);
sha2_auth_in(_AuthKey, _AuthParams, _Packet,
             _HashAlg, _M, _N) ->
    %% 4.2.2
    ?vlog("sha2_auth_in -> entry with"
          "~n      _AuthKey:    ~p"
          "~n      _AuthParams: ~p"
          "~n      _HashAlg:    ~p"
          "~n      _M:          ~p"
          "~n      _N:          ~p", [_AuthKey, _AuthParams, _HashAlg, _M, _N]),
    %% authenticationError.
    false.


zeros(16) -> ?zeros16;
zeros(24) -> ?zeros24;
zeros(32) -> ?zeros32;
zeros(48) -> ?zeros48.


des_encrypt(PrivKey, Data, SaltFun) ->
    [A,B,C,D,E,F,G,H | PreIV] = PrivKey,
    DesKey = [A,B,C,D,E,F,G,H],
    Salt = SaltFun(),
    IV = list_to_binary(snmp_misc:str_xor(PreIV, Salt)),
    TailLen = (8 - (length(Data) rem 8)) rem 8,
    Tail = mk_tail(TailLen),
    EncData = crypto:crypto_one_time(?BLOCK_CIPHER_DES, 
                                     DesKey, IV, [Data,Tail], true),
    {ok, binary_to_list(EncData), Salt}.

des_decrypt(PrivKey, MsgPrivParams, EncData) 
  when length(MsgPrivParams) =:= 8 ->
    ?vtrace("des_decrypt -> entry with"
	    "~n   PrivKey:       ~p"
	    "~n   MsgPrivParams: ~p"
	    "~n   EncData:       ~p", [PrivKey, MsgPrivParams, EncData]),
    [A,B,C,D,E,F,G,H | PreIV] = PrivKey,
    DesKey = [A,B,C,D,E,F,G,H],
    Salt = MsgPrivParams,
    IV = list_to_binary(snmp_misc:str_xor(PreIV, Salt)),
    %% Whatabout errors here???  E.g. not a multiple of 8!
    Data = binary_to_list(crypto:crypto_one_time(?BLOCK_CIPHER_DES, 
                                                 DesKey, IV, EncData, false)),
    Data2 = snmp_pdus:strip_encrypted_scoped_pdu_data(Data),
    {ok, Data2};
des_decrypt(PrivKey, BadMsgPrivParams, EncData) ->
    ?vtrace("des_decrypt -> entry when bad MsgPrivParams"
	    "~n   PrivKey:          ~p"
	    "~n   BadMsgPrivParams: ~p"
	    "~n   EncData:          ~p", 
	    [PrivKey, BadMsgPrivParams, EncData]),
    throw({error, {bad_msgPrivParams, PrivKey, BadMsgPrivParams, EncData}}).
    

aes_encrypt(PrivKey, Data, SaltFun, EngineBoots, EngineTime) ->
    AesKey = PrivKey,
    Salt = SaltFun(),
    IV = list_to_binary([?i32(EngineBoots), ?i32(EngineTime) | Salt]),
    EncData = crypto:crypto_one_time(?BLOCK_CIPHER_AES(AesKey), 
                                     AesKey, IV, Data, true),
    {ok, binary_to_list(EncData), Salt}.

aes_decrypt(PrivKey, MsgPrivParams, EncData, EngineBoots, EngineTime)
  when length(MsgPrivParams) =:= 8 ->
    AesKey = PrivKey,
    Salt = MsgPrivParams,
    IV = list_to_binary([?i32(EngineBoots), ?i32(EngineTime) | Salt]),
    %% Whatabout errors here???  E.g. not a multiple of 8!
    Data = binary_to_list(crypto:crypto_one_time(?BLOCK_CIPHER_AES(AesKey),
                                                 AesKey, IV, EncData, false)),
    Data2 = snmp_pdus:strip_encrypted_scoped_pdu_data(Data),
    {ok, Data2}.


%%-----------------------------------------------------------------
%% Utility functions
%%-----------------------------------------------------------------
mk_tail(N) when N > 0 ->
    [0 | mk_tail(N-1)];
mk_tail(0) ->
    [].

set_msg_auth_params(Message, UsmSecParams, AuthParams) ->
    NUsmSecParams = 
	UsmSecParams#usmSecurityParameters{msgAuthenticationParameters =
					   AuthParams},
    SecBytes = snmp_pdus:enc_usm_security_parameters(NUsmSecParams),
    VsnHdr   = Message#message.vsn_hdr,
    NVsnHdr  = VsnHdr#v3_hdr{msgSecurityParameters = SecBytes},
    Message#message{vsn_hdr = NVsnHdr}.


%% Not very nice...
%% This function patches the asn.1 encoded message. It changes the
%% AuthenticationParameters to 12|16|24|32|48 zeros.
%% NOTE: returns a deep list of bytes
patch_packet([48 | T], Patch) ->
    %% Length for whole packet - 2 is tag for version
    {Len1, [2 | T1]} = split_len(T),
    %% Length for version - 48 is tag for header data
    {Len2, [Vsn,48|T2]} = split_len(T1),
    %% Length for header data
    {Len3, T3} = split_len(T2),
    [48,Len1,2,Len2,Vsn,48,Len3|pp2(dec_len(Len3),T3,Patch)].

%% Skip HeaderData - 4 is tag for SecurityParameters
pp2(0,[4|T],Patch) ->
    %% 48 is tag for UsmSecParams
    {Len1,[48|T1]} = split_len(T),
    %% 4 is tag for EngineID
    {Len2,[4|T2]} = split_len(T1),
    %% Len 3 is length for EngineID
    {Len3,T3} = split_len(T2),
    [4,Len1,48,Len2,4,Len3|pp3(dec_len(Len3),T3,Patch)];
pp2(N,[H|T],Patch) ->
    [H|pp2(N-1,T,Patch)].

%% Skip EngineID - 2 is tag for EngineBoots
pp3(0,[2|T],Patch) ->
    {Len1,T1} = split_len(T),
    [2,Len1|pp4(dec_len(Len1),T1,Patch)];
pp3(N,[H|T],Patch) ->
    [H|pp3(N-1,T,Patch)].

%% Skip EngineBoots - 2 is tag for EngineTime
pp4(0,[2|T],Patch) ->
    {Len1,T1} = split_len(T),
    [2,Len1|pp5(dec_len(Len1),T1,Patch)];
pp4(N,[H|T],Patch) ->
    [H|pp4(N-1,T,Patch)].

%% Skip EngineTime - 4 is tag for UserName 
pp5(0,[4|T],Patch) ->
    {Len1,T1} = split_len(T),
    [4,Len1|pp6(dec_len(Len1),T1,Patch)];
pp5(N,[H|T],Patch) ->
    [H|pp5(N-1,T,Patch)].

%% Skip UserName - 4 is tag for AuthenticationParameters
%% This is what we're looking for!
pp6(0, [4|T], Patch) ->
    PatchLen      = length(Patch),
    {Len1, Rest1} = split_len(T),
    PatchLen      = dec_len(Len1), % Force match
    {_Old, Rest2} = lists:split(PatchLen, Rest1),
    %% Patch: list of zeros of length: 12 | 16 | 24 | 32 | 48
    [4, Len1, Patch|Rest2];
pp6(N,[H|T],Patch) ->
    [H|pp6(N-1,T,Patch)].


%% Returns {LengthOctets, Rest}
split_len([Hd|Tl]) ->
    %% definite form
    case is8set(Hd) of
	0 -> % Short form
	    {Hd,Tl};
	1 -> % Long form - at least one more octet
	    No = clear(Hd, 8),
	    {DigList,Rest} = head(No,Tl),
	    {[Hd | DigList], Rest}
    end.

dec_len(D) when is_integer(D) ->
    D;
dec_len([_LongOctet|T]) ->
    dl(T).
dl([D]) ->
    D;
dl([A,B]) ->
    (A bsl 8) bor B;
dl([A,B,C]) ->
    (A bsl 16) bor (B bsl 8) bor C;
dl([0 | T]) ->
    dl(T).

head(L,List) when length(List) == L -> {List,[]};
head(L,List) ->
    head(L,List,[]).

head(0,L,Res) ->
    {lists:reverse(Res),L};

head(Int,[H|Tail],Res) ->
    head(Int-1,Tail,[H|Res]).

clear(Byte, 8) -> 
    Byte band 127.
%% clear(Byte,Pos) when Pos < 9 ->
%%     Mask = bnot bset(0,Pos),
%%     Mask band Byte.

%% bset(Byte, 8) -> 
%%     Byte bor 2#10000000;
%% bset(Byte, Pos) when (Pos < 9) ->
%%     Mask = 1 bsl (Pos-1),
%%     Byte bor Mask.

is8set(Byte) ->
    if
	Byte > 127 -> 1;
	true -> 0
    end.

error(Reason) ->
    throw({error, Reason}).

