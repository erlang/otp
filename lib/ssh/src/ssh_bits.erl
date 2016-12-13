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
name_list(NamesList) -> list_to_binary(lists:join($,, NamesList)).

%%%----------------------------------------------------------------
%%% Multi Precision Integer encoding
mpint(-1) -> <<0,0,0,1,16#ff>>;
mpint(0) -> <<0,0,0,0>>;
mpint(I) when I>0 ->
    <<B1,V/binary>> = binary:encode_unsigned(I),
    case B1 band 16#80 of
	16#80 ->
	    <<(size(V)+2):32/unsigned-big-integer, 0,B1,V/binary >>;
	_ ->
	    <<(size(V)+1):32/unsigned-big-integer, B1,V/binary >>
    end;
mpint(N) when N<0 -> 
    Sxn =  8*size(binary:encode_unsigned(-N)),
    Sxn1 = Sxn+8,
    <<W:Sxn1>> = <<1, 0:Sxn>>,
    <<B1,V/binary>> = binary:encode_unsigned(W+N),
    case B1 band 16#80 of
	16#80 ->
	    <<(size(V)+1):32/unsigned-big-integer, B1,V/binary >>;
	_ ->
	    <<(size(V)+2):32/unsigned-big-integer, 255,B1,V/binary >>
    end.

%%%----------------------------------------------------------------
%% random/1
%%   Generate N random bytes
%%
random(N) -> crypto:strong_rand_bytes(N).



