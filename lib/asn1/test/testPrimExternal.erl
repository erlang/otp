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
-module(testPrimExternal).

-export([external/1]).

-include_lib("test_server/include/test_server.hrl").

external(_Rules) ->
    Types = ['NT',
	     'Imp',
	     'Exp',
	     'NTNT',
	     'NTImp',
	     'NTExp',
	     'ImpNT',
	     'ImpImp',
	     'ImpExp',
	     'ExpNT',
	     'ExpImp',
	     'ExpExp',
	     'XNTNT',
	     'XNTImp',
	     'XNTExp',
	     'XImpNT',
	     'XImpImp',
	     'XImpExp',
	     'XExpNT',
	     'XExpImp',
	     'XExpExp'],
    _ = [asn1_test_lib:roundtrip('PrimExternal', T, <<"kalle">>) ||
	    T <- Types],
    ok.
