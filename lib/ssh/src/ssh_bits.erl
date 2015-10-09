%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2013. All Rights Reserved.
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

-export([encode/2]).
-export([mpint/1, string/1, name_list/1]).
-export([random/1]).

-define(name_list(X), 
	(fun(B) -> ?binary(B) end)(list_to_binary(name_concat(X)))).


name_concat([Name]) when is_atom(Name) -> atom_to_list(Name);
name_concat([Name]) when is_list(Name) -> Name;
name_concat([Name|Ns]) -> 
    if is_atom(Name) ->
	    [atom_to_list(Name),"," | name_concat(Ns)];
       is_list(Name) ->
	    [Name,"," | name_concat(Ns)]
    end;
name_concat([]) -> [].


name_list(Ns) ->
    ?name_list(Ns).
    

string(Str) ->
    ?string(Str).


%% MP representaion  (SSH2)
mpint(X) when X < 0 ->
    if X == -1 ->
	    <<0,0,0,1,16#ff>>;	    
       true ->
	    mpint_neg(X,0,[])
    end;
mpint(X) ->
    if X == 0 ->
	    <<0,0,0,0>>;
       true ->
	    mpint_pos(X,0,[])
    end.

mpint_neg(-1,I,Ds=[MSB|_]) ->
    if MSB band 16#80 =/= 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([255|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_neg(X,I,Ds)  ->
    mpint_neg(X bsr 8,I+1,[(X band 255)|Ds]).
    
mpint_pos(0,I,Ds=[MSB|_]) ->
    if MSB band 16#80 == 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([0|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_pos(X,I,Ds) ->
    mpint_pos(X bsr 8,I+1,[(X band 255)|Ds]).


encode(List, Types) ->
    list_to_binary(enc(List, Types)).

%%
%% Encode record element
%%
enc(Xs, Ts) ->
    enc(Xs, Ts, 0).

enc(Xs, [boolean|Ts], Offset) ->
    X = hd(Xs),
    [?boolean(X) | enc(tl(Xs), Ts, Offset+1)];
enc(Xs, [byte|Ts], Offset) ->
    X = hd(Xs),
    [?byte(X) | enc(tl(Xs), Ts,Offset+1)];
enc(Xs, [uint16|Ts], Offset) ->
    X = hd(Xs),
    [?uint16(X) | enc(tl(Xs), Ts,Offset+2)];
enc(Xs, [uint32 |Ts], Offset) ->
    X = hd(Xs),
    [?uint32(X) | enc(tl(Xs), Ts,Offset+4)];
enc(Xs, [uint64|Ts], Offset) ->
    X = hd(Xs),
    [?uint64(X) | enc(tl(Xs), Ts,Offset+8)];
enc(Xs, [mpint|Ts], Offset) ->
    Y = mpint(hd(Xs)),
    [Y | enc(tl(Xs), Ts,Offset+size(Y))];
enc(Xs, [string|Ts], Offset) ->
    X0 = hd(Xs),
    Y = ?string(X0),
    [Y | enc(tl(Xs),Ts,Offset+size(Y))];
enc(Xs, [string_utf8|Ts], Offset) ->
    X0 = hd(Xs),
    Y = ?string_utf8(X0),
    [Y | enc(tl(Xs),Ts,Offset+size(Y))];
enc(Xs, [binary|Ts], Offset) ->
     X0 = hd(Xs),
    Y = ?binary(X0),
    [Y | enc(tl(Xs), Ts,Offset+size(Y))];
enc(Xs, [name_list|Ts], Offset) ->
    X0 = hd(Xs),
    Y = ?name_list(X0),
    [Y | enc(tl(Xs), Ts, Offset+size(Y))];
enc(Xs, [cookie|Ts], Offset) ->
    [random(16) | enc(tl(Xs), Ts, Offset+16)];
enc(Xs, [{pad,N}|Ts], Offset) ->
    K = (N - (Offset rem N)) rem N,
    [fill_bits(K,0) | enc(Xs, Ts, Offset+K)];
enc(Xs, ['...'| []], _Offset) ->
    X = hd(Xs),
    if is_binary(X) ->
	    [X];
       is_list(X) ->
	    [list_to_binary(X)];
       X==undefined ->
	    []
    end;
enc([], [],_) ->
    [].


%%
%% Create a binary with constant bytes 
%%
fill_bits(N,C) ->
    list_to_binary(fill(N,C)).

fill(0,_C) -> [];
fill(1,C) -> [C];
fill(N,C) ->
    Cs = fill(N div 2, C),
    Cs1 = [Cs,Cs],
    if N band 1 == 0 ->
	    Cs1;
       true ->
	    [C,Cs,Cs]
    end.


%% random/1
%%   Generate N random bytes
%%
random(N) ->
    crypto:strong_rand_bytes(N).



