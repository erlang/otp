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

%%% Description : SSH 1/2 pdu elements encode/decode

-module(ssh_bits).

-include("ssh.hrl").

-export([encode/1, encode/2]).
-export([decode/1, decode/2, decode/3]).
-export([mpint/1,  bignum/1, string/1, name_list/1]).
-export([b64_encode/1, b64_decode/1]).
-export([install_messages/1, uninstall_messages/1]).

%% integer utils
-export([isize/1]).
-export([irandom/1, irandom/3]).
-export([random/1, random/3]).
-export([xor_bits/2, fill_bits/2]).
-export([i2bin/2, bin2i/1]).

-import(lists, [foreach/2, reverse/1]).

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


%% BIGNUM representation SSH1
bignum(X) ->
    XSz = isize(X),
    Pad = (8 - (XSz rem 8)) rem 8,
    <<?UINT16(XSz),0:Pad/unsigned-integer,X:XSz/big-unsigned-integer>>.


install_messages(Codes) ->
    foreach(fun({Name, Code, Ts}) ->
		   %%  ?dbg(true, "install msg: ~s = ~w ~w~n", 
%% 			 [Name,Code,Ts]),
		    put({msg_name,Code}, {Name,Ts}),
		    put({msg_code,Name}, {Code,Ts})
	    end, Codes).

uninstall_messages(Codes) ->
    foreach(fun({Name, Code, _Ts}) ->
		  %%   ?dbg(true, "uninstall msg: ~s = ~w ~w~n", 
%% 			 [Name,Code,_Ts]),
		    erase({msg_name,Code}),
		    erase({msg_code,Name})
	    end, Codes).

%%
%% Encode a record, the type spec is expected to be 
%% in process dictionary under the key {msg_code, RecodeName}
%%
encode(Record) ->
    case get({msg_code, element(1, Record)}) of
	undefined -> 
	    {error, unimplemented};
	{Code, Ts} ->
	    Data = enc(tl(tuple_to_list(Record)), Ts),
	    list_to_binary([Code, Data])
    end.

encode(List, Types) ->
    list_to_binary(enc(List, Types)).

%%
%% Encode record element
%%
enc(Xs, Ts) ->
    enc(Xs, Ts, 0).

enc(Xs, [Type|Ts], Offset) ->
    case Type of
	boolean ->
	    X=hd(Xs), 
	    [?boolean(X) | enc(tl(Xs), Ts, Offset+1)];
	byte ->
	    X=hd(Xs),
	    [?byte(X) | enc(tl(Xs), Ts,Offset+1)];
	uint16 ->  
	    X=hd(Xs),
	    [?uint16(X) | enc(tl(Xs), Ts,Offset+2)];
	uint32 ->
	    X=hd(Xs),
	    [?uint32(X) | enc(tl(Xs), Ts,Offset+4)];
	uint64 ->
	    X=hd(Xs),
	    [?uint64(X) | enc(tl(Xs), Ts,Offset+8)];
	mpint ->
	    Y=mpint(hd(Xs)),
	    [Y | enc(tl(Xs), Ts,Offset+size(Y))];
	bignum ->  
	    Y=bignum(hd(Xs)),
	    [Y | enc(tl(Xs),Ts,Offset+size(Y))];
	string ->
	    X0=hd(Xs),
	    Y=?string(X0),
	    [Y | enc(tl(Xs),Ts,Offset+size(Y))];
	binary ->
	    X0=hd(Xs),
	    Y=?binary(X0),
	    [Y | enc(tl(Xs), Ts,Offset+size(Y))];
	name_list -> 
	    X0=hd(Xs),
	    Y=?name_list(X0),
	    [Y | enc(tl(Xs), Ts, Offset+size(Y))];
	cookie -> 
	    [random(16) | enc(tl(Xs), Ts, Offset+16)];
	{pad,N} ->
	    K = (N - (Offset rem N)) rem N,
	    [fill_bits(K,0) | enc(Xs, Ts, Offset+K)];
	'...' when Ts==[] ->
	    X=hd(Xs),
	    if is_binary(X) -> 
		    [X];
	       is_list(X) ->
		    [list_to_binary(X)];
	       X==undefined ->
		    []
	    end
    end;
enc([], [],_) ->
    [].



%%
%% Decode a SSH record the type is encoded as the first byte
%% and the type spec MUST be installed in {msg_name, ID}
%%

decode(Binary = <<?BYTE(ID), _/binary>>) ->
    case get({msg_name, ID}) of
	undefined -> 
	    {unknown, Binary};
	{Name, Ts} ->
	    {_, Elems} = decode(Binary,1,Ts),
	    list_to_tuple([Name | Elems])
    end.

%%
%% Decode a binary form offset 0
%%

decode(Binary, Types) when is_binary(Binary) andalso is_list(Types) ->
    {_,Elems} = decode(Binary, 0, Types),
    Elems.


%%
%% Decode a binary from byte offset Offset
%% return {UpdatedOffset, DecodedElements}
%%
decode(Binary, Offset, Types) ->
    decode(Binary, Offset, Types, []).

decode(Binary, Offset, [Type|Ts], Acc) ->
    case Type of
	boolean ->
	    <<_:Offset/binary, ?BOOLEAN(X0), _/binary>> = Binary,
	    X = if X0 == 0 -> false; true -> true end,
	    decode(Binary, Offset+1, Ts, [X | Acc]);

	byte ->
	    <<_:Offset/binary, ?BYTE(X), _/binary>> = Binary,
	    decode(Binary, Offset+1, Ts, [X | Acc]);

	uint16 ->
	    <<_:Offset/binary, ?UINT16(X), _/binary>> = Binary,
	    decode(Binary, Offset+2, Ts, [X | Acc]);

	uint32 ->
	    <<_:Offset/binary, ?UINT32(X), _/binary>> = Binary,
	    decode(Binary, Offset+4, Ts, [X | Acc]);

	uint64 ->
	    <<_:Offset/binary, ?UINT64(X), _/binary>> = Binary,
	    decode(Binary, Offset+8, Ts, [X | Acc]);

	mpint ->
	    <<_:Offset/binary, ?UINT32(L), X0:L/binary,_/binary>> = Binary,
	    Sz = L*8,
	    <<X:Sz/big-signed-integer>> = X0,
	    decode(Binary, Offset+4+L, Ts, [X | Acc]);

	bignum ->
	    <<_:Offset/binary, ?UINT16(Bits),_/binary>> = Binary,
	    L = (Bits+7) div 8,
	    Pad = (8 - (Bits rem 8)) rem 8,
	    <<_:Offset/binary, _:16, _:Pad, X:Bits/big-unsigned-integer,
	     _/binary>> = Binary,
	    decode(Binary, Offset+2+L, Ts, [X | Acc]);

	string ->
	    Size = size(Binary),
	    if Size < Offset + 4  ->
		    %% empty string at end
		    {Size, reverse(["" | Acc])};
	       true ->
		    <<_:Offset/binary,?UINT32(L), X:L/binary,_/binary>> =
			Binary,
		    decode(Binary, Offset+4+L, Ts, [binary_to_list(X) | 
						    Acc])
	    end;

	binary ->
	    <<_:Offset/binary,?UINT32(L), X:L/binary,_/binary>> = Binary,
	    decode(Binary, Offset+4+L, Ts, [X | Acc]);

	name_list ->
	    <<_:Offset/binary,?UINT32(L), X:L/binary,_/binary>> = Binary,
	    List = string:tokens(binary_to_list(X), ","),
	    decode(Binary, Offset+4+L, Ts, [List | Acc]);

	cookie ->
	    <<_:Offset/binary, X:16/binary, _/binary>> = Binary,
	    decode(Binary, Offset+16, Ts, [X | Acc]);

	{pad,N} -> %% pad offset to a multiple of N
	    K = (N - (Offset rem N)) rem N,
	    decode(Binary, Offset+K, Ts, Acc);
	    
	
	'...' when Ts==[] ->
	    <<_:Offset/binary, X/binary>> = Binary,
	    {Offset+size(X), reverse([X | Acc])}
    end;
decode(_Binary, Offset, [], Acc) ->
    {Offset, reverse(Acc)}.



%% HACK WARNING :-)
-define(VERSION_MAGIC, 131).
-define(SMALL_INTEGER_EXT, $a).
-define(INTEGER_EXT,       $b).
-define(SMALL_BIG_EXT,     $n).
-define(LARGE_BIG_EXT,     $o).

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

%% xor 2 binaries
xor_bits(XBits, YBits) ->
    XSz = size(XBits)*8,
    YSz = size(YBits)*8,
    Sz = if XSz < YSz -> XSz; true -> YSz end, %% min
    <<X:Sz, _/binary>> = XBits,
    <<Y:Sz, _/binary>> = YBits,
    <<(X bxor Y):Sz>>.

%%
%% irandom(N)
%%
%%  Generate a N bits size random number
%%  note that the top most bit is always set
%%  to guarantee that the number is N bits
%%
irandom(Bits) ->
    irandom(Bits, 1, 0).

%% irandom_odd(Bits) ->
%%     irandom(Bits, 1, 1).

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
irandom(0, _Top, _Bottom) -> 
    0;
irandom(Bits, Top, Bottom) ->
    Bytes = (Bits+7) div 8,
    Skip  = (8-(Bits rem 8)) rem 8,
    TMask = case Top of
		  0 -> 0;
		  1 -> 16#80;
		  2 -> 16#c0
	      end,
    BMask = case Bottom of
		0 -> 0;
		1 -> (1 bsl Skip)
	    end,
    <<X:Bits/big-unsigned-integer, _:Skip>> = random(Bytes, TMask, BMask),
    X.

%%
%% random/1
%%   Generate N random bytes
%%
random(N) ->
    random(N, 0, 0).

random(N, TMask, BMask) ->
    list_to_binary(rnd(N, TMask, BMask)).

%% random/3
%%   random(Bytes, TopMask, BotMask)
%% where 
%% Bytes is the number of bytes to generate
%% TopMask is bitwised or'ed to the first byte
%% BotMask is bitwised or'ed to the last byte
%%
rnd(0, _TMask, _BMask) ->
    [];
rnd(1, TMask, BMask) ->
    [(rand8() bor TMask) bor BMask];
rnd(N, TMask, BMask) ->
    [(rand8() bor TMask) | rnd_n(N-1, BMask)].

rnd_n(1, BMask) ->
    [rand8() bor BMask];
rnd_n(I, BMask) ->
    [rand8() | rnd_n(I-1, BMask)].

rand8() ->
    (rand32() bsr 8) band 16#ff.

rand32() ->
    random:uniform(16#100000000) -1.

%%
%% Base 64 encode/decode
%%

b64_encode(Bs) when is_list(Bs) -> 
    base64:encode(Bs);    
b64_encode(Bin) when is_binary(Bin) ->
    base64:encode(Bin).

b64_decode(Bin) when is_binary(Bin) -> 
    base64:mime_decode(Bin);
b64_decode(Cs) when is_list(Cs) -> 
    base64:mime_decode(Cs).


