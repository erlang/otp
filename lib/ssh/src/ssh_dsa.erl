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

%%% Description: dsa public-key sign and verify

-module(ssh_dsa).

-export([verify/3, verify/4]).
-export([sign/2]).
-export([alg_name/0]).

-include("ssh.hrl").
-include_lib("public_key/include/public_key.hrl").

sign(_Private= #'DSAPrivateKey'{p = P, q = Q, g = G, x = X},Mb) ->
    K = ssh_bits:irandom(160) rem Q,
    R = ssh_math:ipow(G, K, P) rem Q,
    Ki = ssh_math:invert(K, Q),
    <<M:160/big-unsigned-integer>> = crypto:sha(Mb),
    S = (Ki * (M + X*R)) rem Q,
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>>.

verify(PlainText, sha, Sig, {Y,  {_, P, Q, G}}) ->
    verify(#ssh_key{type = dsa,
		    public = {P,Q,G,Y}}, PlainText, Sig).

verify(Public, Mb, Sb) ->
    case catch xverify(Public, Mb, Sb) of
	{'EXIT', _Reason} ->
	    false;
	ok ->
	    true
    end.

xverify(_Public=#ssh_key { public={P,Q,G,Y} },Mb,Sb) ->
    <<R0:160/big-unsigned-integer, S0:160/big-unsigned-integer>> = Sb,
    ?ssh_assert(R0 >= 0 andalso R0 < Q andalso
		S0 >= 0 andalso S0 < Q, out_of_range),
    W = ssh_math:invert(S0,Q),
    <<M0:160/big-unsigned-integer>> = crypto:sha(Mb),
    U1 = (M0*W) rem Q,
    U2 = (R0*W) rem Q,
    T1 = ssh_math:ipow(G,U1,P),
    T2 = ssh_math:ipow(Y,U2,P),
    V = ((T1*T2) rem P) rem Q,
    if V == R0 ->
	    ok;
       true ->
	    {error, inconsistent_key}
    end.

alg_name() ->
    "ssh-dss".
