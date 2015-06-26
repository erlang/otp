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

%%% Description : SSH 1/2 pdu elements encode/decode

-module(ssh_bits).

-include("ssh.hrl").

-export([encode/2]).
-export([mpint/1, erlint/2, string/1, name_list/1]).
-export([random/1]).

-export([isize/1]).
-export([irandom/1, irandom/3]).
-export([fill_bits/2]).
-export([i2bin/2, bin2i/1]).


-define(name_list(X), 
	(fun(B) -> ?binary(B) end)(list_to_binary(name_concat(X)))).

-define(VERSION_MAGIC, 131).
-define(SMALL_INTEGER_EXT, $a).
-define(INTEGER_EXT,       $b).
-define(SMALL_BIG_EXT,     $n).
-define(LARGE_BIG_EXT,     $o).


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

erlint(Len, BinInt) ->
    Sz = Len*8,
    <<Int:Sz/big-signed-integer>> = BinInt,
    Int.
	
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



isize(N) when N > 0 ->
    case term_to_binary(N) of
	<<?VERSION_MAGIC, ?SMALL_INTEGER_EXT, X>> ->
	    isize_byte(X);
	<<?VERSION_MAGIC, ?INTEGER_EXT, X3,X2,X1,X0>> ->
	    isize_bytes([X3,X2,X1,X0]);
	<<?VERSION_MAGIC, ?SMALL_BIG_EXT, S:8/big-unsigned-integer, 0,
	 Ds:S/binary>> ->
	    K = S - 1,
	    <<_:K/binary, Top>> = Ds,
	    isize_byte(Top)+K*8;
	<<?VERSION_MAGIC, ?LARGE_BIG_EXT, S:32/big-unsigned-integer, 0,
	 Ds:S/binary>> ->
	    K = S - 1,
	    <<_:K/binary, Top>> = Ds,
	    isize_byte(Top)+K*8
    end;
isize(0) -> 0.

%% big endian byte list
isize_bytes([0|L]) ->
    isize_bytes(L);
isize_bytes([Top|L]) ->
    isize_byte(Top) + length(L)*8.

%% Well could be improved
isize_byte(X) ->
    if X >= 2#10000000 -> 8;
       X >= 2#1000000 -> 7;
       X >= 2#100000 -> 6;
       X >= 2#10000 -> 5;
       X >= 2#1000 -> 4;
       X >= 2#100 -> 3;
       X >= 2#10 -> 2;
       X >= 2#1 -> 1;
       true -> 0
    end.

%% Convert integer into binary 
%% When XLen is the wanted size in octets of the output
i2bin(X, XLen) ->
    XSz = isize(X),
    Sz = XLen*8,
    if Sz < XSz -> 
	    exit(integer_to_large);
       true ->
	    (<<X:Sz/big-unsigned-integer>>)
    end.

%% Convert a binary into an integer
%%
bin2i(X) ->
    Sz = size(X)*8,
    <<Y:Sz/big-unsigned-integer>> = X,
    Y.

%%
%% irandom(N)
%%
%%  Generate a N bits size random number
%%  note that the top most bit is always set
%%  to guarantee that the number is N bits
%%
irandom(Bits) ->
    irandom(Bits, 1, 0).

%%
%% irandom(N, Top, Bottom)
%%
%%  Generate a N bits size random number
%% Where Top = 0 - do not set top bit
%%           = 1 - set the most significant bit
%%           = 2 - set two most significant bits
%%       Bot = 0 - do not set the least signifcant bit
%%       Bot = 1 - set the least signifcant bit (i.e always odd)
%%
irandom(Bits, Top, Bottom) when is_integer(Top),
                                0 =< Top, Top =< 2 ->
    crypto:erlint(crypto:strong_rand_mpint(Bits, Top - 1, Bottom)).
