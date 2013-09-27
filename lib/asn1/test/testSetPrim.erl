%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
-module(testSetPrim).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Set',{bool, boolCon, boolPri, boolApp, boolExpCon, boolExpPri, boolExpApp}).
-record('Empty',{}).

main(_Rules) ->
    roundtrip('Set',
	      #'Set'{bool=true,boolCon=true,boolPri=true,boolApp=true,
		     boolExpCon=true,boolExpPri=true,boolExpApp=true}),
    roundtrip('Set',
	      #'Set'{bool=false,boolCon=false,boolPri=false,boolApp=false,
		     boolExpCon=false,boolExpPri=false,boolExpApp=false}),
    roundtrip('Set',
	      #'Set'{bool=false,boolCon=true,boolPri=false,boolApp=true,
		     boolExpCon=false,boolExpPri=true,boolExpApp=false}),
    roundtrip('Empty', #'Empty'{}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetPrim', T, V).
