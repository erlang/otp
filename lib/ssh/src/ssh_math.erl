%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2013. All Rights Reserved.
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

-export([ipow/3]).
-export([ilog2/1, invert/2, ipow2/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% INTEGER utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% calculate A^B mod M
ipow(A, B, M) when M > 0, B >= 0 ->
    crypto:mod_exp(A, B, M).
ilog2(N) ->
    ssh_bits:isize(N) - 1.

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




