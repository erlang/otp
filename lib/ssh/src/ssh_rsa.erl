%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2012. All Rights Reserved.
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

-include_lib("public_key/include/public_key.hrl").

-export([verify/3, sign/2]).
-export([alg_name/0]).

-include("ssh.hrl").

-define(MGF(Seed,Len), mgf1((Seed),(Len))).
-define(HASH(X), crypto:sha((X))).
-define(HLen, 20).

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

%% sign1, M = message representative
rsasp1(#'RSAPrivateKey'{modulus = N, privateExponent = D}, M) ->
    ?ssh_assert((M >= 0 andalso M =< N-1), out_of_range),
    ssh_math:ipow(M, D, N).

%% verify1,  S =signature representative
rsavp1(#'RSAPublicKey'{publicExponent = E, modulus = N}, S)  ->
    ?ssh_assert(S >= 0 andalso S =< N-1, out_of_range),
    ssh_math:ipow(S, E, N).


rsassa_pkcs1_v1_5_sign(#'RSAPrivateKey'{modulus = N} = Private, Mb) ->
    K = (ssh_bits:isize(N)+7) div 8,    
    EM = emsa_pkcs1_v1_5_encode(Mb, K),
    M = os2ip(EM),
    S = rsasp1(Private, M),
    i2osp(S, K);

rsassa_pkcs1_v1_5_sign(Private=#ssh_key { public={N,_},private={_,_D}},Mb) ->
    K = (ssh_bits:isize(N)+7) div 8,    
    EM = emsa_pkcs1_v1_5_encode(Mb, K),
    M = os2ip(EM),
    S = rsasp1(Private, M),
    i2osp(S, K).

rsassa_pkcs1_v1_5_verify(#'RSAPublicKey'{modulus = N} = Public, Mb, Sb) ->
    K = (ssh_bits:isize(N)+7) div 8,
    ?ssh_assert(size(Sb) == K, invalid_signature),
    S = os2ip(Sb),
    M = rsavp1(Public, S),
    EM = i2osp(M, K),
    %?dbg(true, "verify K=~p S=~w ~n#M=~w~n#EM=~w~n", [K, S, M, EM]),
    case emsa_pkcs1_v1_5_encode(Mb, K) of
	EM -> true;
	_S -> false
	    %%{error, invalid_signature}
    end.


emsa_pkcs1_v1_5_encode(M, EMLen) ->
    H = ?HASH(M),
    %% Must use speical xxNull types here!
    Alg = #'AlgorithmNull' { algorithm = ?'id-sha1',
			     parameters = <<>> },
    TCode = public_key:der_encode('DigestInfoNull',
				  #'DigestInfoNull'{ digestAlgorithm = Alg,
						     digest = H }),
    TLen = size(TCode),
    ?ssh_assert(EMLen >= TLen + 11, message_to_short),
    PS = ssh_bits:fill_bits(EMLen - TLen - 3, 16#ff),
    <<16#00, 16#01, PS/binary, 16#00, TCode/binary>>.


alg_name() ->
    "ssh-rsa".
