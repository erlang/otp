%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2026. All Rights Reserved.
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

-module(ssh_eqc_encode_decode).

-compile(export_all).

-include_lib("common_test/include/ct_property_test.hrl").

%% Public key records:
-include_lib("public_key/include/public_key.hrl").

%% ML-KEM size constants (duplicated from ssh.hrl to avoid LAZY macro
%% conflict with PropEr's proper_common.hrl)
-define(MLKEM768_PUBLICKEY_SIZE, 1184).
-define(MLKEM768_CIPHERTEXT_SIZE, 1088).
-define(MLKEM1024_PUBLICKEY_SIZE, 1568).
-define(MLKEM1024_CIPHERTEXT_SIZE, 1568).
-define(X25519_PUBLICKEY_SIZE, 32).
-define(NISTP256_PUBLICKEY_SIZE, 65).
-define(NISTP256_COMPRESSED_PUBLICKEY_SIZE, 33).
-define(NISTP384_PUBLICKEY_SIZE, 97).
-define(NISTP384_COMPRESSED_PUBLICKEY_SIZE, 49).

%%% Properties:

prop_ssh_decode() ->
    ?FORALL({Msg,KexFam}, ?LET(KF, kex_family(), {ssh_msg(KF),KF} ),
            collect(KexFam,
                    try ssh_message:decode(decode_state(Msg,KexFam))
                    of
                        _ -> true
                    catch

                        C:E -> io:format('~p:~p~n',[C,E]),
                               false
                    end)).

%%% This fails because ssh_message is not symmetric in encode and decode regarding data types
prop_ssh_decode_encode() ->
    ?FORALL({Msg,KexFam}, ?LET(KF, kex_family(), {ssh_msg(KF),KF} ),
            collect(KexFam,
                    Msg == ssh_message:encode(
                             fix_asym(
                               ssh_message:decode(decode_state(Msg,KexFam)),
                               KexFam))
                   )).

%%%================================================================
%%%
%%% Generators
%%% 

ssh_msg(<<"dh">>) ->
    ?LET(M,oneof(
	     [
	      [msg_code('SSH_MSG_KEXDH_INIT'),gen_mpint()],  % 30
	      [msg_code('SSH_MSG_KEXDH_REPLY'),gen_pubkey_string(rsa),gen_mpint(),gen_signature_string(rsa)] % 31
	      | rest_ssh_msgs()
	     ]),
	 list_to_binary(M));

ssh_msg(<<"dh_gex">>) ->
    ?LET(M,oneof(
	     [
	      [msg_code('SSH_MSG_KEX_DH_GEX_REQUEST_OLD'),gen_uint32()],  % 30
	      [msg_code('SSH_MSG_KEX_DH_GEX_GROUP'),gen_mpint(),gen_mpint()]  % 31
	      | rest_ssh_msgs()
	     ]),
	 list_to_binary(M));

 ssh_msg(<<"ecdh">>) ->
     ?LET(M,oneof(
 	     [
 	      [msg_code('SSH_MSG_KEX_ECDH_INIT'),gen_mpint()],   % 30
	      [msg_code('SSH_MSG_KEX_ECDH_REPLY'),gen_pubkey_string(ecdsa),gen_mpint(),gen_signature_string(ecdsa)] % 31
	      | rest_ssh_msgs()
 	     ]),
          list_to_binary(M));

ssh_msg(<<"mlkem",_/binary>> = KexFam) ->
    ?LET(M,oneof(
             [
              [msg_code('SSH_MSG_KEX_HYBRID_INIT'),             % 30
               gen_hybrid_string(KexFam, init)],
              [msg_code('SSH_MSG_KEX_HYBRID_REPLY'),            % 31
               gen_pubkey_string(rsa),
               gen_hybrid_string(KexFam, reply),
               gen_signature_string(rsa)]
             | rest_ssh_msgs()
             ]),
         list_to_binary(M)).


rest_ssh_msgs() -> 
    [%%                    SSH_MSG_USERAUTH_INFO_RESPONSE
     %% hard args          SSH_MSG_USERAUTH_INFO_REQUEST
     %% rfc4252 p12 error  SSH_MSG_USERAUTH_REQUEST
     [msg_code('SSH_MSG_KEX_DH_GEX_REQUEST'),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_KEX_DH_GEX_INIT'),gen_mpint()],
     [msg_code('SSH_MSG_KEX_DH_GEX_REPLY'),gen_pubkey_string(rsa),gen_mpint(),gen_signature_string(rsa)],
     [msg_code('SSH_MSG_CHANNEL_CLOSE'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_DATA'),gen_uint32(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_EOF'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_EXTENDED_DATA'),gen_uint32(),gen_uint32(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_FAILURE'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("direct-tcpip"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("forwarded-tcpip"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("session"),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("x11"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string( ),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN_CONFIRMATION'),gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN_FAILURE'),gen_uint32(),gen_uint32(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("env"),gen_boolean(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exec"),gen_boolean(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exit-signal"),0,gen_string( ),gen_boolean(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exit-status"),0,gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("pty-req"),gen_boolean(),gen_string( ),gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("shell"),gen_boolean()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("signal"),0,gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("subsystem"),gen_boolean(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("window-change"),0,gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("x11-req"),gen_boolean(),gen_boolean(),gen_string( ),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("xon-xoff"),0,gen_boolean()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string( ),gen_boolean()],
     [msg_code('SSH_MSG_CHANNEL_SUCCESS'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_WINDOW_ADJUST'),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_DEBUG'),gen_boolean(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_DISCONNECT'),gen_uint32(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string("cancel-tcpip-forward"),gen_boolean(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string("tcpip-forward"),gen_boolean(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string( ),gen_boolean()],
     [msg_code('SSH_MSG_IGNORE'),gen_string( )],
     gen_ext_info(),
     [msg_code('SSH_MSG_KEXINIT'),gen_byte(16),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_boolean(),gen_uint32()],
     [msg_code('SSH_MSG_NEWKEYS')],
     [msg_code('SSH_MSG_REQUEST_FAILURE')],
     [msg_code('SSH_MSG_REQUEST_SUCCESS')],
     [msg_code('SSH_MSG_REQUEST_SUCCESS'),gen_uint32()],
     [msg_code('SSH_MSG_SERVICE_ACCEPT'),gen_string( )],
     [msg_code('SSH_MSG_SERVICE_REQUEST'),gen_string( )],
     [msg_code('SSH_MSG_UNIMPLEMENTED'),gen_uint32()],
     [msg_code('SSH_MSG_USERAUTH_BANNER'),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_USERAUTH_FAILURE'),gen_name_list(),gen_boolean()],
     [msg_code('SSH_MSG_USERAUTH_PASSWD_CHANGEREQ'),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_USERAUTH_PK_OK'),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_USERAUTH_SUCCESS')]
    ].

kex_family() -> oneof([<<"dh">>, <<"dh_gex">>, <<"ecdh">> | hybrid_families()]).

hybrid_families() ->
    [<<"mlkem768x25519">>, <<"mlkem768nistp256">>, <<"mlkem1024nistp384">>].

gen_boolean() -> choose(0,1).

gen_byte() -> choose(0,255).

gen_uint16() -> gen_byte(2).

gen_uint32() -> gen_byte(4).

gen_uint64() -> gen_byte(8).

gen_byte(N) when N>0 -> [gen_byte() || _ <- lists:seq(1,N)].
    
gen_char() -> choose($a,$z).

gen_mpint() -> ?LET(I, largeint(), ssh_bits:mpint(I)).

strip_0s([0|T]) -> strip_0s(T);
strip_0s(X) -> X.
    

gen_string() -> 
    ?LET(Size, choose(0,10), 
	 ?LET(Vector,vector(Size, gen_char()),
	      gen_string(Vector) 
	     )).

gen_string(S) when is_binary(S) -> gen_string(binary_to_list(S));
gen_string(S) when is_list(S) -> uint32_to_list(length(S)) ++ S.
    
gen_name_list() ->
    ?LET(NumNames, choose(0,10),
	 ?LET(L, [gen_name() || _ <- lists:seq(1,NumNames)],
	      gen_string( string:join(L,"," ) )
	)).

gen_name() -> gen_string().

uint32_to_list(I) ->  binary_to_list(<<I:32/unsigned-big-integer>>).
    
gen_pubkey_string(Type) ->
    PubKey = case Type of
		 rsa -> #'RSAPublicKey'{modulus = 12345,publicExponent = 2};
		 ecdsa -> {#'ECPoint'{point=[1,2,3,4,5]},
			   {namedCurve,{1,2,840,10045,3,1,7}}} % 'secp256r1' nistp256
	     end,
    gen_string(ssh_message:ssh2_pubkey_encode(PubKey)).

    
gen_signature_string(Type) ->
    Signature = <<"hejhopp">>,
    Id = case Type of
	     rsa -> "ssh-rsa";
	     ecdsa -> "ecdsa-sha2-nistp256"
	 end,
    gen_string(gen_string(Id) ++ gen_string(Signature)).

%% Hybrid KEX (RFC 10042) payload generators. The engine generates the
%% bytes so failing cases shrink and replay. Content is opaque to the
%% decoder; only the length decides acceptance.
gen_hybrid_string(KexFam, Msg) ->
    ?LET(Len, elements(hybrid_sizes(KexFam, Msg)),
         ?LET(Bytes, vector(Len, gen_byte()),
              gen_string(Bytes))).

%% RFC 10042 #3: allowed c_init (msg 30) and s_reply (msg 31) sizes.
%% NIST curves allow compressed and uncompressed points.
hybrid_sizes(<<"mlkem768x25519">>, init) ->
    [?MLKEM768_PUBLICKEY_SIZE + ?X25519_PUBLICKEY_SIZE];
hybrid_sizes(<<"mlkem768x25519">>, reply) ->
    [?MLKEM768_CIPHERTEXT_SIZE + ?X25519_PUBLICKEY_SIZE];
hybrid_sizes(<<"mlkem768nistp256">>, init) ->
    [?MLKEM768_PUBLICKEY_SIZE + ?NISTP256_COMPRESSED_PUBLICKEY_SIZE,
     ?MLKEM768_PUBLICKEY_SIZE + ?NISTP256_PUBLICKEY_SIZE];
hybrid_sizes(<<"mlkem768nistp256">>, reply) ->
    [?MLKEM768_CIPHERTEXT_SIZE + ?NISTP256_COMPRESSED_PUBLICKEY_SIZE,
     ?MLKEM768_CIPHERTEXT_SIZE + ?NISTP256_PUBLICKEY_SIZE];
hybrid_sizes(<<"mlkem1024nistp384">>, init) ->
    [?MLKEM1024_PUBLICKEY_SIZE + ?NISTP384_COMPRESSED_PUBLICKEY_SIZE,
     ?MLKEM1024_PUBLICKEY_SIZE + ?NISTP384_PUBLICKEY_SIZE];
hybrid_sizes(<<"mlkem1024nistp384">>, reply) ->
    [?MLKEM1024_CIPHERTEXT_SIZE + ?NISTP384_COMPRESSED_PUBLICKEY_SIZE,
     ?MLKEM1024_CIPHERTEXT_SIZE + ?NISTP384_PUBLICKEY_SIZE].

hybrid_kem_pk_size(<<"mlkem1024", _/binary>>) -> ?MLKEM1024_PUBLICKEY_SIZE;
hybrid_kem_pk_size(<<"mlkem768", _/binary>>) -> ?MLKEM768_PUBLICKEY_SIZE.

%% SSH_MSG_EXT_INFO: uint32 N followed by N name-value string pairs
gen_ext_info() ->
    ?LET(N, choose(0,5),
         ?LET(Pairs, vector(N, {gen_string(), gen_string()}),
              [msg_code('SSH_MSG_EXT_INFO')] ++
                  uint32_to_list(N) ++
                  lists:append([[Name, Val] || {Name, Val} <- Pairs]))).

-define(MSG_CODE(Name,Num),
msg_code(Name) -> Num;
msg_code(Num) -> Name
).

?MSG_CODE('SSH_MSG_USERAUTH_REQUEST',   50);
?MSG_CODE('SSH_MSG_USERAUTH_FAILURE',   51);
?MSG_CODE('SSH_MSG_USERAUTH_SUCCESS',   52);
?MSG_CODE('SSH_MSG_USERAUTH_BANNER',   53);
?MSG_CODE('SSH_MSG_USERAUTH_PK_OK',   60);
?MSG_CODE('SSH_MSG_USERAUTH_PASSWD_CHANGEREQ',   60);
?MSG_CODE('SSH_MSG_DISCONNECT',   1);
?MSG_CODE('SSH_MSG_IGNORE',   2);
?MSG_CODE('SSH_MSG_UNIMPLEMENTED',   3);
?MSG_CODE('SSH_MSG_DEBUG',   4);
?MSG_CODE('SSH_MSG_SERVICE_REQUEST',   5);
?MSG_CODE('SSH_MSG_SERVICE_ACCEPT',   6);
?MSG_CODE('SSH_MSG_EXT_INFO',   7);
?MSG_CODE('SSH_MSG_KEXINIT',   20);
?MSG_CODE('SSH_MSG_NEWKEYS',   21);
?MSG_CODE('SSH_MSG_GLOBAL_REQUEST',   80);
?MSG_CODE('SSH_MSG_REQUEST_SUCCESS',   81);
?MSG_CODE('SSH_MSG_REQUEST_FAILURE',   82);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN',   90);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN_CONFIRMATION',   91);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN_FAILURE',   92);
?MSG_CODE('SSH_MSG_CHANNEL_WINDOW_ADJUST',   93);
?MSG_CODE('SSH_MSG_CHANNEL_DATA',   94);
?MSG_CODE('SSH_MSG_CHANNEL_EXTENDED_DATA',   95);
?MSG_CODE('SSH_MSG_CHANNEL_EOF',   96);
?MSG_CODE('SSH_MSG_CHANNEL_CLOSE',   97);
?MSG_CODE('SSH_MSG_CHANNEL_REQUEST',   98);
?MSG_CODE('SSH_MSG_CHANNEL_SUCCESS',   99);
?MSG_CODE('SSH_MSG_CHANNEL_FAILURE',   100);
?MSG_CODE('SSH_MSG_USERAUTH_INFO_REQUEST',   60);
?MSG_CODE('SSH_MSG_USERAUTH_INFO_RESPONSE',   61);
?MSG_CODE('SSH_MSG_KEXDH_INIT', 30);
?MSG_CODE('SSH_MSG_KEXDH_REPLY', 31);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REQUEST_OLD',   30);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REQUEST',   34);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_GROUP',   31);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_INIT',   32);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REPLY', 33);
?MSG_CODE('SSH_MSG_KEX_ECDH_INIT', 30);
?MSG_CODE('SSH_MSG_KEX_ECDH_REPLY', 31);
?MSG_CODE('SSH_MSG_KEX_HYBRID_INIT', 30);
?MSG_CODE('SSH_MSG_KEX_HYBRID_REPLY', 31).

%%%====================================================
%%%=== WARNING: Knowledge of the test object ahead! ===
%%%====================================================

%% SSH message records:
-include_lib("ssh/src/ssh_connect.hrl").
-include_lib("ssh/src/ssh_transport.hrl").

%%% Encoding and decoding is asymmetric so out=binary in=string. Sometimes. :(
-define(fix_asym_Xdh_reply(S),
 fix_asym(#S{public_host_key = Key, h_sig = {Alg,Sig}} = M) ->
      M#S{public_host_key = {Key, list_to_atom(Alg)}, h_sig = Sig}
).


fix_asym(#ssh_msg_global_request{name=N} = M) -> M#ssh_msg_global_request{name = binary_to_list(N)};
fix_asym(#ssh_msg_debug{message=D,language=L} = M) -> M#ssh_msg_debug{message = binary_to_list(D),
								      language = binary_to_list(L)};
fix_asym(#ssh_msg_kexinit{cookie=C} = M) -> M#ssh_msg_kexinit{cookie = <<C:128>>};
?fix_asym_Xdh_reply(ssh_msg_kexdh_reply);
?fix_asym_Xdh_reply(ssh_msg_kex_dh_gex_reply);
?fix_asym_Xdh_reply(ssh_msg_kex_ecdh_reply);
?fix_asym_Xdh_reply(ssh_msg_kex_hybrid_reply);
fix_asym(M) -> M.

%% ML-KEM hybrid: decode returns single binary c_init, encode expects
%% {C_pk2, C_pk1}. The split point depends on the KEM of the family.
fix_asym(#ssh_msg_kex_hybrid_init{c_init = C} = M, KexFam) when is_binary(C) ->
    KemSize = hybrid_kem_pk_size(KexFam),
    <<C_pk2:KemSize/binary, C_pk1/binary>> = C,
    M#ssh_msg_kex_hybrid_init{c_init = {C_pk2, C_pk1}};
fix_asym(M, _KexFam) ->
    fix_asym(M).


%%% Message codes 30 and 31 are overloaded depending on kex family so arrange the decoder
%%% input as the test object does
decode_state(<<30,_/binary>>=Msg, KexFam) -> <<KexFam/binary, Msg/binary>>;
decode_state(<<31,_/binary>>=Msg, KexFam) -> <<KexFam/binary, Msg/binary>>;
decode_state(Msg, _) -> Msg.


%%%================================================================
%%%
%%% RFC 10042 hybrid KEX: size gating and point validation
%%%

%%% Size gating in ssh_message:decode/1 for messages 30/31.
%%%
%%% ORACLE SOURCE: RFC 10042 #3 (message sizes) and the hybrid
%%% ssh_message:decode/1 clauses. An allowed length gives the record; any
%%% other length gives the documented size error. A message with extra or
%%% missing bytes is rejected (size error or function_clause); the
%%% connection handler turns any decode exception into a disconnect.
prop_hybrid_decode_sizes() ->
    ?FORALL({KexFam, Msg, Variant, Len}, gen_decode_case(),
            collect({KexFam, Msg, Variant,
                     lists:member(Len, hybrid_sizes(KexFam, Msg))},
                    check_decode(KexFam, Msg, Variant, Len))).

gen_decode_case() ->
    ?LET({KexFam, Msg}, {elements(hybrid_families()), elements([init, reply])},
         ?LET({Variant, Len},
              {elements([exact, exact, trailing, truncated]),
               gen_hybrid_len(hybrid_sizes(KexFam, Msg))},
              {KexFam, Msg, Variant, Len})).

gen_hybrid_len(Allowed) ->
    oneof([elements(Allowed),
           elements([A + D || A <- Allowed, D <- [-1, 1]]),
           choose(0, lists:max(Allowed) + 64)]).

check_decode(KexFam, Msg, exact, Len) ->
    Allowed = hybrid_sizes(KexFam, Msg),
    IsAllowed = lists:member(Len, Allowed),
    SizeTag = hybrid_size_tag(Msg),
    try ssh_message:decode(hybrid_msg(KexFam, Msg, Len)) of
        #ssh_msg_kex_hybrid_init{c_init = C} when Msg == init ->
            IsAllowed andalso byte_size(C) == Len;
        #ssh_msg_kex_hybrid_reply{s_reply = S} when Msg == reply ->
            IsAllowed andalso byte_size(S) == Len;
        Other ->
            io:format("Unexpected decode result: ~p~n", [Other]),
            false
    catch
        throw:{error, {SizeTag, Len, Allowed}} ->
            not IsAllowed;
        C:E ->
            io:format("Unexpected exception: ~p:~p~n", [C, E]),
            false
    end;
check_decode(KexFam, Msg, Variant, Len) ->
    Bin0 = hybrid_msg(KexFam, Msg, Len),
    Bin = case Variant of
              trailing -> <<Bin0/binary, 0>>;
              truncated -> binary:part(Bin0, 0, byte_size(Bin0) - 1)
          end,
    try ssh_message:decode(Bin) of
        Decoded ->
            io:format("Accepted malformed message: ~p~n", [Decoded]),
            false
    catch
        throw:{error, _} -> true;
        error:function_clause -> true;
        C:E ->
            io:format("Unexpected exception: ~p:~p~n", [C, E]),
            false
    end.

hybrid_size_tag(init) -> mlkem_init_invalid_size;
hybrid_size_tag(reply) -> mlkem_reply_invalid_size.

%% Wire message as seen by ssh_message:decode/1 (with the kex prefix).
%% The payload content is opaque to the decoder, so zeros are enough.
hybrid_msg(KexFam, init, Len) ->
    Body = binary:copy(<<0>>, Len),
    <<KexFam/binary, 30, Len:32, Body/binary>>;
hybrid_msg(KexFam, reply, Len) ->
    Body = binary:copy(<<0>>, Len),
    HostKey = ssh_message:ssh2_pubkey_encode(
                #'RSAPublicKey'{modulus = 3233, publicExponent = 17}),
    Sig = <<12:32, "rsa-sha2-256", 3:32, 1, 2, 3>>,
    <<KexFam/binary, 31,
      (byte_size(HostKey)):32, HostKey/binary,
      Len:32, Body/binary,
      (byte_size(Sig)):32, Sig/binary>>.

%%% Point validation in ssh_transport:compute_key/4 (hybrid_server and
%%% hybrid_client), with real ML-KEM keys so the ECDH step is reached.
%%%
%%% ORACLE SOURCE: RFC 10042 #2.4 (K = HASH(K_PQ || K_CL)): both sides
%%% derive the same secret from valid points. SEC1 v2 #2.3.4
%%% (octet string to point): a NIST point must have tag 2, 3 (compressed)
%%% or 4 (uncompressed) and lie on the curve; anything else must be
%%% rejected, never yield a key. RFC 7748 #6.1 / RFC 8731 #3: an X25519
%%% exchange that yields the all-zero secret (low-order peer point) must
%%% be aborted.
%%%
%%% Needs ML-KEM-768/1024 and the NIST curves in crypto; the CT case
%%% skips otherwise (ssh_property_test_SUITE:init_per_testcase/2).
prop_hybrid_compute_key() ->
    ?FORALL({KexFam, Role, Form, Mut}, gen_compute_key_case(),
            collect({KexFam, Role, Form, mutation_class(Mut)},
                    check_compute_key(KexFam, Role, Form, Mut))).

gen_compute_key_case() ->
    ?LET(KexFam, elements(hybrid_families()),
         ?LET({Role, Form, Mut},
              {elements([server, client]),
               elements(point_forms(KexFam)),
               gen_mutation(KexFam)},
              {KexFam, Role, Form, Mut})).

point_forms(<<"mlkem768x25519">>) -> [raw];
point_forms(_) -> [uncompressed, compressed].

%% Every 32-byte string is a valid X25519 encoding, but low-order points
%% give the all-zero secret: u = 0 and u = 1 (RFC 7748 #7).
gen_mutation(<<"mlkem768x25519">>) ->
    oneof([valid,
           ?LET(U, elements([0, 1]), {loworder, U})]);
gen_mutation(_) ->
    oneof([valid,
           offcurve,
           ?LET(T, choose(0, 255), {badtag, bad_tag(T)})]).

bad_tag(T) when T >= 2, T =< 4 -> T + 3;
bad_tag(T) -> T.

mutation_class({badtag, _}) -> badtag;
mutation_class({loworder, _}) -> loworder;
mutation_class(M) -> M.

hybrid_kem_curve(<<"mlkem768x25519">>) -> {mlkem768, x25519};
hybrid_kem_curve(<<"mlkem768nistp256">>) -> {mlkem768, secp256r1};
hybrid_kem_curve(<<"mlkem1024nistp384">>) -> {mlkem1024, secp384r1}.

check_compute_key(KexFam, Role, Form, Mut) ->
    {Kem, Curve} = KC = hybrid_kem_curve(KexFam),
    {C_pk2, C_sk2} = crypto:generate_key(Kem, []),
    {C_pk1_raw, C_sk1} = crypto:generate_key(ecdh, Curve),
    {S_pk1_raw, S_sk1} = crypto:generate_key(ecdh, Curve),
    C_pk1 = point_form(Form, C_pk1_raw),
    S_pk1 = point_form(Form, S_pk1_raw),
    Bad = fun(Raw, P) -> mutate_point(Mut, Form, Curve, Raw, P) end,
    case Role of
        server ->
            C_init = <<C_pk2/binary, (Bad(C_pk1_raw, C_pk1))/binary>>,
            case hybrid_compute(hybrid_server, C_init, S_sk1, KC) of
                {ok, {S_ct, K_srv}} when Mut == valid ->
                    S_reply = <<S_ct/binary, S_pk1/binary>>,
                    {ok, K_srv} ==
                        hybrid_compute(hybrid_client, S_reply,
                                       {C_sk2, C_sk1}, KC);
                Res ->
                    check_rejected(server, Mut, Res)
            end;
        client ->
            {ok, {S_ct, K_srv}} =
                hybrid_compute(hybrid_server, <<C_pk2/binary, C_pk1/binary>>,
                               S_sk1, KC),
            S_reply = <<S_ct/binary, (Bad(S_pk1_raw, S_pk1))/binary>>,
            case hybrid_compute(hybrid_client, S_reply, {C_sk2, C_sk1}, KC) of
                {ok, K_cli} when Mut == valid ->
                    K_cli == K_srv;
                Res ->
                    check_rejected(client, Mut, Res)
            end
    end.

%% A mutated point must raise; a valid point must not.
check_rejected(Role, Mut, {ok, Result}) ->
    io:format("~p accepted ~p point: ~p~n", [Role, Mut, Result]),
    false;
check_rejected(_Role, valid, {exception, Reason}) ->
    io:format("Valid point rejected: ~p~n", [Reason]),
    false;
check_rejected(_Role, _Mut, {exception, _}) ->
    true.

hybrid_compute(Side, Peer, Own, KC) ->
    try ssh_transport:compute_key(Side, Peer, Own, KC) of
        R -> {ok, R}
    catch
        C:E -> {exception, {C, E}}
    end.

%% crypto:generate_key(ecdh, ...) returns uncompressed NIST points
%% (<<4, X, Y>>) and raw 32-byte X25519 keys.
point_form(raw, P) -> P;
point_form(uncompressed, P) -> P;
point_form(compressed, <<4, XY/binary>>) ->
    N = byte_size(XY) div 2,
    <<X:N/binary, Y:N/binary>> = XY,
    <<(2 + (binary:last(Y) band 1)), X/binary>>.

%% offcurve, uncompressed: flip the lowest bit of Y; the result is on
%% the curve only with negligible probability.
%% offcurve, compressed: replace X by the next X for which x^3 + ax + b
%% has no square root mod p, so decompression must fail.
%% badtag: keep the length of the chosen form, replace the tag byte.
%% loworder: X25519 u-coordinate of a low-order point, little-endian.
mutate_point(valid, _Form, _Curve, _Raw, P) -> P;
mutate_point(offcurve, uncompressed, _Curve, <<4, XY/binary>>, _P) ->
    Last = binary:last(XY),
    <<4, (binary:part(XY, 0, byte_size(XY) - 1))/binary, (Last bxor 1)>>;
mutate_point(offcurve, compressed, Curve, _Raw, <<Tag, X/binary>>) ->
    <<Tag, (non_residue_x(Curve, X))/binary>>;
mutate_point({badtag, T}, _Form, _Curve, _Raw, <<_, Rest/binary>>) ->
    <<T, Rest/binary>>;
mutate_point({loworder, U}, raw, x25519, _Raw, _P) ->
    <<U:256/little>>.

%% Smallest X' >= X (mod p) with x^3 + ax + b a quadratic non-residue
%% (Euler's criterion). About half of all X qualify.
non_residue_x(Curve, XBin) ->
    {{prime_field, PBin}, {ABin, BBin, _Seed}, _, _, _} =
        crypto:ec_curve(Curve),
    [P, A, B] = [binary:decode_unsigned(V) || V <- [PBin, ABin, BBin]],
    Size = byte_size(XBin),
    non_residue_x(binary:decode_unsigned(XBin) rem P, P, A, B, Size).

non_residue_x(X, P, A, B, Size) ->
    Rhs = (X * X * X + A * X + B) rem P,
    case Rhs =/= 0 andalso
        binary:decode_unsigned(
          crypto:mod_pow(Rhs, (P - 1) div 2, P)) == P - 1 of
        true -> <<X:Size/unit:8>>;
        false -> non_residue_x((X + 1) rem P, P, A, B, Size)
    end.
