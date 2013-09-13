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
-module(testSetOfCho).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SetChoDef',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetChoOpt',{bool1, int1, set1 = asn1_NOVALUE}).
-record('SetChoEmbDef',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetChoEmbOpt',{bool1, int1, set1 = asn1_NOVALUE}).
-record('SetOfChoEmbDef_SETOF',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetOfChoEmbOpt_SETOF',{bool1, int1, set1 = asn1_NOVALUE}).

main(_Rules) ->
    roundtrip('SetChoDef',
	      #'SetChoDef'{bool1=true,int1=17,set1=asn1_DEFAULT},
	      #'SetChoDef'{bool1=true,int1=17,set1=[]}),
    roundtrip('SetChoDef',
	      #'SetChoDef'{bool1=true,int1=17,set1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SetChoOpt',
	      #'SetChoOpt'{bool1=true,int1=17,set1=asn1_NOVALUE}),
    roundtrip('SetChoOpt',
	      #'SetChoOpt'{bool1=true,int1=17,set1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SetChoEmbDef',
	      #'SetChoEmbDef'{bool1=true,int1=17,set1=asn1_DEFAULT},
	      #'SetChoEmbDef'{bool1=true,int1=17,set1=[]}),
    roundtrip('SetChoEmbDef',
	      #'SetChoEmbDef'{bool1=true,int1=17,
			      set1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SetChoEmbOpt',
	      #'SetChoEmbOpt'{bool1=true,int1=17,set1=asn1_NOVALUE}),
    roundtrip('SetChoEmbOpt',
	      #'SetChoEmbOpt'{bool1=true,int1=17,
			      set1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SetOfChoEmbDef',
	      [#'SetOfChoEmbDef_SETOF'{bool1=true,int1=17,set1=asn1_DEFAULT}],
	      [#'SetOfChoEmbDef_SETOF'{bool1=true,int1=17,set1=[]}]),
    roundtrip('SetOfChoEmbDef',
	      [#'SetOfChoEmbDef_SETOF'{bool1=true,int1=17,
				       set1=[{boolIn,true},{intIn,25}]}]),

    roundtrip('SetOfChoEmbOpt',
	      [#'SetOfChoEmbOpt_SETOF'{bool1=true,int1=17,set1=asn1_NOVALUE}]),
    roundtrip('SetOfChoEmbOpt',
	      [#'SetOfChoEmbOpt_SETOF'{bool1=true,int1=17,
				       set1=[{boolIn,true},{intIn,25}]}]),

    ok.

roundtrip(T, V) ->
    roundtrip(T, V, V).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('SetOfCho', Type, Value, ExpectedValue).
