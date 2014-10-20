%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2013. All Rights Reserved.
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
-module(ber_decode_error).

-export([run/1]).

run([]) ->
    {ok,B}  = 'Constructed':encode('S3', {'S3',17}),
    [T,L|V] = binary_to_list(B),
    Bytes = list_to_binary([T,L+3|V] ++ [2,1,3]),
    case 'Constructed':decode('S3', Bytes) of
	{error,{asn1,{unexpected,_}}} -> ok
    end,

    %% Unexpected bytes must be accepted if there is an extensionmark
    {ok,{'S3ext',17}} = 'Constructed':decode('S3ext', Bytes),

    %% Truncated tag.
    {error,{asn1,{invalid_tag,_}}} =
	(catch 'Constructed':decode('I', <<31,255,255>>)),

    %% Overlong tag.
    {error,{asn1,{invalid_tag,_}}} =
	(catch 'Constructed':decode('I', <<31,255,255,255,127>>)),

    %% Invalid length.
    {error,{asn1,{invalid_length,_}}} =
	(catch 'Constructed':decode('I', <<8,255>>)),

    %% Other errors.
    {error,{asn1,{invalid_value,_}}} =
	(catch 'Constructed':decode('I', <<>>)),

    {error,{asn1,{invalid_value,_}}} =
	(catch 'Constructed':decode('I', <<8,7>>)),

    %% Short indefinite length. Make sure that the decoder doesn't look
    %% beyond the end of binary when looking for a 0,0 terminator.
    {error,{asn1,{invalid_length,_}}} =
	(catch 'Constructed':decode('S', sub(<<8,16#80,0,0>>, 3))),
    {error,{asn1,{invalid_length,_}}} =
	(catch 'Constructed':decode('S', sub(<<8,16#80,0,0>>, 2))),
    {error,{asn1,{invalid_length,_}}} =
	(catch 'Constructed':decode('S', sub(<<40,16#80,1,1,255,0,0>>, 6))),
    {error,{asn1,{invalid_length,_}}} =
	(catch 'Constructed':decode('S', sub(<<40,16#80,1,1,255,0,0>>, 5))),

    %% A primitive must not be encoded with an indefinite length.
    {error,{asn1,{invalid_length,_}}} =
	(catch 'Constructed':decode('OS', <<4,128,4,3,97,98,99,0,0>>)),
    ok.

sub(Bin, Bytes) ->
    <<B:Bytes/binary,_/binary>> = Bin,
    B.
