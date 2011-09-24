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

%%% Description: SSH math utilities

-module(ssh_math).

-export([ilog2/1, ipow/3, invert/2, ipow2/3]).
	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% INTEGER utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% number of bits (used) in a integer = isize(N) = |log2(N)|+1
ilog2(N) ->
    ssh_bits:isize(N) - 1.


%% calculate A^B mod M
ipow(A, B, M) when M > 0, B >= 0 ->
    crypto:mod_exp(A, B, M).

ipow2(A, B, M) when M > 0, B >= 0 ->
    if A == 1 -> 
 	    1;
       true -> 
 	    ipow2(A, B, M, 1)
    end.

ipow2(A, 1, M, Prod) ->
    (A*Prod) rem M;
ipow2(_A, 0, _M, Prod) ->
    Prod;
ipow2(A, B, M, Prod)  ->
    B1 = B bsr 1,
    A1 = (A*A) rem M,
    if B - B1 == B1 ->
	    ipow2(A1, B1, M, Prod);
       true ->
	    ipow2(A1, B1, M, (A*Prod) rem M)
    end.

%% %%
%% %% Normal gcd
%% %%
%% gcd(R, Q) when abs(Q) < abs(R) -> gcd1(Q,R);
%% gcd(R, Q) -> gcd1(R,Q).

%% gcd1(0, Q) -> Q;
%% gcd1(R, Q) ->
%%     gcd1(Q rem R, R).


%% %%
%% %% Least common multiple of (R,Q)
%% %%
%% lcm(0, _Q) -> 0;
%% lcm(_R, 0) -> 0;
%% lcm(R, Q) ->
%%     (Q div gcd(R, Q)) * R.

%% %%
%% %% Extended gcd gcd(R,Q) -> {G, {A,B}} such that G == R*A + Q*B
%% %%
%% %% Here we could have use for a bif divrem(Q, R) -> {Quote, Remainder}
%% %%
%% egcd(R,Q) when abs(Q) < abs(R) -> egcd1(Q,R,1,0,0,1);
%% egcd(R,Q) -> egcd1(R,Q,0,1,1,0).

%% egcd1(0,Q,_,_,Q1,Q2) -> {Q, {Q2,Q1}};
%% egcd1(R,Q,R1,R2,Q1,Q2) ->
%%     D = Q div R,
%%     egcd1(Q rem R, R, Q1-D*R1, Q2-D*R2, R1, R2).

%%
%% Invert an element X mod P
%% Calculated as {1, {A,B}} = egcd(X,P),
%%   1 == P*A + X*B == X*B (mod P) i.e B is the inverse element
%%
%% X > 0, P > 0, X < P   (P should be prime)
%%
invert(X,P) when X > 0, P > 0, X < P ->
    I = inv(X,P,1,0),
    if 
        I < 0 -> P + I;
        true -> I
    end.

inv(0,_,_,Q) -> Q;
inv(X,P,R1,Q1) ->
    D = P div X,
    inv(P rem X, X, Q1 - D*R1, R1).


%% %%
%% %% Integer square root
%% %%

%% isqrt(0) -> 0;
%% isqrt(1) -> 1;
%% isqrt(X) when X >= 0 ->
%%     R = X div 2,
%%     isqrt(X div R, R, X).

%% isqrt(Q,R,X) when Q < R ->
%%     R1 = (R+Q) div 2,
%%     isqrt(X div R1, R1, X);
%% isqrt(_, R, _) -> R.


