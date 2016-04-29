%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

%%% Description : SSH 1/2 pdu elements encode/decode

-module(ssh_bits).

-include("ssh.hrl").

-export([mpint/1, name_list/1]).
-export([random/1]).

%%%----------------------------------------------------------------
name_list([Name]) -> to_bin(Name);
name_list([Name|Ns]) -> <<(to_bin(Name))/binary, ",", (name_list(Ns))/binary>>;
name_list([]) -> <<>>.

to_bin(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
to_bin(S) when is_list(S) -> list_to_binary(S);
to_bin(B) when is_binary(B) -> B.

%%%----------------------------------------------------------------
%%% Multi Precision Integer encoding
mpint(-1) -> <<0,0,0,1,16#ff>>;
mpint(0) -> <<0,0,0,0>>;
mpint(X) when X < 0 -> mpint_neg(X,0,[]);
mpint(X) -> mpint_pos(X,0,[]).
    
mpint_neg(-1,I,Ds=[MSB|_]) ->
    if MSB band 16#80 =/= 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([255|Ds]))/binary>>;
       true ->
	    <<?UINT32(I), (list_to_binary(Ds))/binary>>
    end;
mpint_neg(X,I,Ds)  ->
    mpint_neg(X bsr 8,I+1,[(X band 255)|Ds]).
    
mpint_pos(0,I,Ds=[MSB|_]) ->
    if MSB band 16#80 == 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([0|Ds]))/binary>>;
       true ->
	    <<?UINT32(I), (list_to_binary(Ds))/binary>>
    end;
mpint_pos(X,I,Ds) ->
    mpint_pos(X bsr 8,I+1,[(X band 255)|Ds]).


%%%----------------------------------------------------------------
%% random/1
%%   Generate N random bytes
%%
random(N) -> crypto:strong_rand_bytes(N).



