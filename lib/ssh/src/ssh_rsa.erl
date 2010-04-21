%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
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

%%

%%% Description: rsa public-key sign and verify

-module(ssh_rsa).

-export([verify/3, sign/2]).
-export([alg_name/0]).

-include("ssh.hrl").
-include("PKCS-1.hrl").


-define(MGF(Seed,Len), mgf1((Seed),(Len))).
-define(HASH(X), crypto:sha((X))).
-define(HLen, 20).

%% start() ->
%%     crypto:start().

%% sign_file(File) ->
%%     start(),
%%     {ok,Bin} = file:read_file(File),
%%     {ok,Key} = ssh_file:private_host_rsa_key(user),
%%     sign(Key, Bin).

%% verify_file(File, Sig) ->
%%     start(),
%%     {ok,Bin} = file:read_file(File),
%%     {ok,Key} = ssh_file:public_host_rsa_key(user),
%%     verify(Key, Bin, Sig).

sign(Private,Mb) ->
    rsassa_pkcs1_v1_5_sign(Private,Mb).

verify(Public,Mb,Sb) ->
    rsassa_pkcs1_v1_5_verify(Public,Mb,Sb).



%% Integer to octet string
i2osp(X, XLen) ->
    ssh_bits:i2bin(X, XLen).

%% Octet string to Integer
os2ip(X) ->
    ssh_bits:bin2i(X).

%% decrypt1, M = message representative
%% rsaep(#ssh_key { public={N,E}}, M) ->
%%     ?ssh_assert(M >= 0 andalso M =< N-1, out_of_range),
%%     ssh_math:ipow(M, E, N).

%% encrypt1, C = cipher representative
%% rsadp(#ssh_key { public={N,_}, private={_,D}}, C) ->
%%     ?ssh_assert(C >= 0 andalso C =< N-1, out_of_range),
%%     ssh_math:ipow(C, D, N).

%% sign1, M = message representative
rsasp1(#ssh_key { public={N,_}, private={_,D}}, M) ->
    ?ssh_assert((M >= 0 andalso M =< N-1), out_of_range),
    ssh_math:ipow(M, D, N).

%% verify1,  S =signature representative
rsavp1(#ssh_key { public={N,E}}, S)  ->
    ?ssh_assert(S >= 0 andalso S =< N-1, out_of_range),
    ssh_math:ipow(S, E, N).


%% M messaage
%% rsaes_oaep_encrypt(Public, M) ->
%%     rsaes_oaep_encrypt(Public, M, <<>>).

%% rsaes_oaep_encrypt(Public=#ssh_key { public={N,_E}}, M, L) ->
%%     ?ssh_assert(size(L) =< 16#ffffffffffffffff, label_to_long),
%%     K = (ssh_bits:isize(N)+7) div 8,
%%     MLen = size(M),
%%     %% LLen  = size(L),
%%     ?ssh_assert(MLen =< K - 2*?HLen - 2, message_to_long),
%%     LHash = ?HASH(L),
%%     PS = ssh_bits:fill_bits(K - MLen - 2*?HLen - 2, 0),
%%     DB = <<LHash/binary, PS/binary, 16#01, M/binary>>,
%%     Seed = ssh_bits:random(?HLen),
%%     DbMask = ?MGF(Seed, K - ?HLen - 1),
%%     MaskedDB = ssh_bits:xor_bits(DB, DbMask),
%%     SeedMask = ?MGF(MaskedDB, ?HLen),
%%     MaskedSeed = ssh_bits:xor_bits(Seed, SeedMask),
%%     EM = <<16#00, MaskedSeed/binary, MaskedDB/binary>>,
%%     Mc = os2ip(EM),
%%     C = rsaep(Public, Mc),
%%     i2osp(C, K).

%% rsaes_oaep_decrypt(Key, C) ->
%%     rsaes_oaep_decrypt(Key, C, <<>>).

%% rsaes_oaep_decrypt(Private=#ssh_key { public={N,_},private={_,_}},Cb,L) ->
%%     ?ssh_assert(size(L) =< 16#ffffffffffffffff, label_to_long),
%%     K = (ssh_bits:isize(N)+7) div 8,
%%     ?ssh_assert(K == 2*?HLen + 2, decryption_error),
%%     C = os2ip(Cb),
%%     M = rsadp(Private, C),
%%     EM = i2osp(M, K),
%%     LHash = ?HASH(L),
%%     MLen = K - ?HLen -1,
%%     case EM of
%% 	<<16#00, MaskedSeed:?HLen/binary, MaskedDB:MLen>> ->
%% 	    SeedMask = ?MGF(MaskedDB, ?HLen),
%% 	    Seed = ssh_bits:xor_bits(MaskedSeed, SeedMask),
%% 	    DbMask = ?MGF(Seed, K - ?HLen - 1),
%% 	    DB = ssh_bits:xor_bits(MaskedDB, DbMask),
%% 	    PSLen = K - MLen - 2*?HLen - 2,
%% 	    case DB of
%% 		<<LHash:?HLen, _PS:PSLen/binary, 16#01, M/binary>> ->
%% 		    M;
%% 		_ ->
%% 		    exit(decryption_error)
%% 	    end;
%% 	_ ->
%% 	    exit(decryption_error)
%%     end.


%% rsaes_pkcs1_v1_5_encrypt(Public=#ssh_key { public={N,_}}, M) ->    
%%     K = (ssh_bits:isize(N)+7) div 8,
%%     MLen = size(M),
%%     ?ssh_assert(MLen =< K - 11, message_to_long),
%%     PS = ssh_bits:random(K - MLen - 3),
%%     EM = <<16#00,16#02,PS/binary,16#00,M/binary>>,
%%     Mc = os2ip(EM),
%%     C = rsaep(Public, Mc),
%%     i2osp(C, K).


%% rsaes_pkcs1_v1_5_decrypt(Private=#ssh_key { public={N,_},private={_,_}},
%% 			 Cb) ->
%%     K = (ssh_bits:isize(N)+7) div 8,
%%     CLen = size(Cb),
%%     ?ssh_assert(CLen == K andalso K >= 11, decryption_error),
%%     C = os2ip(Cb),
%%     M = rsadp(Private, C),
%%     EM = i2osp(M, K),
%%     PSLen = K - CLen - 3,
%%     case EM of
%% 	<<16#00, 16#02, _PS:PSLen/binary, 16#00, M>> ->
%% 	    M;
%% 	_ ->
%% 	    exit(decryption_error)
%%     end.

%% rsassa_pss_sign(Private=#ssh_key { public={N,_},private={_,_}},Mb) ->
%%     ModBits = ssh_bits:isize(N),
%%     K = (ModBits+7) div 8,
%%     EM = emsa_pss_encode(Mb, ModBits - 1),
%%     M = os2ip(EM),
%%     S = rsasp1(Private, M),
%%     i2osp(S, K).

%% rsassa_pss_verify(Public=#ssh_key { public={N,_E}},Mb,Sb) ->
%%     ModBits = ssh_bits:isize(N),
%%     K = (ModBits+7) div 8,
%%     ?ssh_assert(size(Sb) == K, invalid_signature),
%%     S = os2ip(Sb),
%%     M = rsavp1(Public,S),
%%     EMLen = (ModBits-1+7) div 8,
%%     EM = i2osp(M, EMLen),
%%     emsa_pss_verify(Mb, EM, ModBits-1).


rsassa_pkcs1_v1_5_sign(Private=#ssh_key { public={N,_},private={_,_D}},Mb) ->
    K = (ssh_bits:isize(N)+7) div 8,    
    EM = emsa_pkcs1_v1_5_encode(Mb, K),
    M = os2ip(EM),
    S = rsasp1(Private, M),
    i2osp(S, K).

rsassa_pkcs1_v1_5_verify(Public=#ssh_key { public={N,_E}}, Mb, Sb) ->
    K = (ssh_bits:isize(N)+7) div 8,
    ?ssh_assert(size(Sb) == K, invalid_signature),
    S = os2ip(Sb),
    M = rsavp1(Public, S),
    EM = i2osp(M, K),
    %?dbg(true, "verify K=~p S=~w ~n#M=~w~n#EM=~w~n", [K, S, M, EM]),
    case emsa_pkcs1_v1_5_encode(Mb, K) of
	EM -> ok;
	_S ->
	    io:format("S: ~p~n", [_S]),
	    {error, invalid_signature} % exit(invalid_signature)
    end.


emsa_pkcs1_v1_5_encode(M, EMLen) ->
    H = ?HASH(M),
    %% Must use speical xxNull types here!
    Alg = #'AlgorithmNull' { algorithm = ?'id-sha1',
			     parameters = <<>> },
    {ok,TCode} = 'PKCS-1':encode('DigestInfoNull',
				#'DigestInfoNull'{ digestAlgorithm = Alg,
						   digest = H }),
    T = list_to_binary(TCode),
    TLen = size(T),
    ?ssh_assert(EMLen >= TLen + 11, message_to_short),
    PS = ssh_bits:fill_bits(EMLen - TLen - 3, 16#ff),
    <<16#00, 16#01, PS/binary, 16#00, T/binary>>.


%% emsa_pss_encode(M, EMBits) ->
%%     emsa_pss_encode(M, EMBits, 0).

%% emsa_pss_encode(M, EMBits, SLen) ->
%%     ?ssh_assert(size(M) =< 16#ffffffffffffffff, message_to_long),
%%     EMLen = (EMBits + 7) div 8,
%%     MHash = ?HASH(M),
%%     ?ssh_assert(EMLen >= ?HLen + SLen + 2, encoding_error),
%%     Salt = ssh_bits:random(SLen),
%%     M1 = [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
%% 	  MHash, Salt],
%%     H = ?HASH(M1),
%%     PS = ssh_bits:fill_bits(EMLen-SLen-?HLen-2, 0),
%%     DB = <<PS/binary, 16#01, Salt/binary>>,
%%     DbMask = ?MGF(H, EMLen - ?HLen -1),
%%     MaskedDB = ssh_bits:xor_bits(DB, DbMask),
%%     ZLen = 8*EMLen - EMBits,
%%     NZLen = (8 - (ZLen rem 8)) rem 8,
%%     <<_:ZLen, NZ:NZLen, MaskedDB1/binary>> = MaskedDB,
%%     MaskedDB2 = <<0:ZLen, NZ:NZLen, MaskedDB1/binary>>,
%%     <<MaskedDB2/binary, H/binary, 16#BC>>.


%% emsa_pss_verify(M, EM, EMBits) ->
%%     emsa_pss_verify(M, EM, EMBits, 0).

%% emsa_pss_verify(M, EM, EMBits, SLen) ->
%%     ?ssh_assert(size(M) =< 16#ffffffffffffffff, message_to_long),
%%     EMLen = (EMBits + 7) div 8,
%%     MHash = ?HASH(M),
%%     ?ssh_assert(EMLen >= ?HLen + SLen + 2, inconsistent),
%%     MaskLen = (EMLen - ?HLen - 1)-1,
%%     ZLen = 8*EMLen - EMBits,
%%     NZLen = (8 - (ZLen rem 8)) rem 8,
%%     case EM of
%% 	<<0:ZLen,Nz:NZLen,MaskedDB1:MaskLen/binary, H:?HLen/binary, 16#BC>> ->
%% 	    MaskedDB = <<0:ZLen,Nz:NZLen,MaskedDB1/binary>>,
%% 	    DbMask = ?MGF(H, EMLen - ?HLen - 1),
%% 	    DB = ssh_bits:xor_bits(MaskedDB, DbMask),
%% 	    PSLen1 = (EMLen - SLen - ?HLen - 2) - 1,
%% 	    PS = ssh_bits:fill_bits(PSLen1, 0),
%% 	    case DB of
%% 		<<_:ZLen,0:NZLen,PS:PSLen1/binary,16#01,Salt:SLen/binary>> ->
%% 		    M1 = [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
%% 			  MHash, Salt],
%% 		    case ?HASH(M1) of
%% 			H -> ok;
%% 			_ -> exit(inconsistent)
%% 		    end;
%% 		_ ->
%% 		    exit(inconsistent)
%% 	    end;
%% 	_ ->
%% 	    exit(inconsistent)
%%     end.

    

%% Mask generating function MGF1
%% mgf1(MGFSeed, MaskLen) ->
%%     T = mgf1_loop(0, ((MaskLen + ?HLen -1) div ?HLen) - 1, MGFSeed, ""),
%%     <<R:MaskLen/binary, _/binary>> = T,
%%     R.

%% mgf1_loop(Counter, N, _, T) when Counter > N ->
%%     list_to_binary(T);
%% mgf1_loop(Counter, N, MGFSeed, T) ->
%%     C = i2osp(Counter, 4),
%%     mgf1_loop(Counter+1, N, MGFSeed, [T, ?HASH([MGFSeed, C])]).




alg_name() ->
    "ssh-rsa".
