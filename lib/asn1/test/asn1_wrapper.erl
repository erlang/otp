%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
-module(asn1_wrapper).
-author('kenneth@bilbo').

-compile(export_all).
%%-export([Function/Arity, ...]).


encode(Module,Type,Value) ->
    case asn1rt:encode(Module,Type,Value) of
	{ok,X} when is_binary(X) ->
	    {ok, binary_to_list(X)};
	{ok,X} ->
	    {ok, binary_to_list(list_to_binary(X))};
	Error ->
	    Error
    end.

decode(Module,Type,Bytes) ->
    case Module:encoding_rule() of
	ber ->
	    asn1rt:decode(Module,Type,Bytes);
	ber_bin when is_binary(Bytes) ->
	    asn1rt:decode(Module,Type,Bytes);
	ber_bin ->
	    asn1rt:decode(Module,Type,list_to_binary(Bytes));
	ber_bin_v2 when is_binary(Bytes) ->
	    asn1rt:decode(Module,Type,Bytes);
	ber_bin_v2 ->
	    asn1rt:decode(Module,Type,list_to_binary(Bytes));
	per ->
	    asn1rt:decode(Module,Type,Bytes);
	per_bin when is_binary(Bytes) ->
	    asn1rt:decode(Module,Type,Bytes);
	per_bin ->
	    asn1rt:decode(Module,Type,list_to_binary(Bytes));
	uper_bin when is_binary(Bytes) ->
	    asn1rt:decode(Module,Type,Bytes);
	uper_bin ->
	    asn1rt:decode(Module,Type,list_to_binary(Bytes))
    end.

erule(ber) ->
    ber;
erule(ber_bin) ->
    ber;
erule(ber_bin_v2) ->
    ber;
erule(per) ->
    per;
erule(per_bin) ->
    per;
erule(uper_bin) ->
    per.


