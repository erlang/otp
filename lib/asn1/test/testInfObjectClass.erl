%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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

-module(testInfObjectClass).


-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

main(Rule) ->
    %% this test is added for OTP-4591, to test that elements in decoded
    %% value has terms in right order.
    Val = {'Seq',12,13,2},
    roundtrip('Seq', Val),
    
    %% OTP-5783
    {error,{asn1,{'Type not compatible with table constraint',
		  {component,'ArgumentType'},
		  {value,_},_}}} = 'InfClass':encode('Seq', {'Seq',12,13,1}),
    Bytes2 = case Rule of
		 ber ->
		     <<48,9,2,1,12,2,1,11,2,1,1>>;
		 _ ->
		     <<1,12,1,11,1,1>>
	     end,
    {error,{asn1,{'Type not compatible with table constraint',
		  {{component,_},
		   {value,_B},_}}}} = 'InfClass':decode('Seq', Bytes2),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('InfClass', T, V).
