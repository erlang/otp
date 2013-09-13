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
-module(testSeqOfCho).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqChoDef',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqChoOpt',{bool1, int1, seq1 = asn1_NOVALUE}).
-record('SeqChoEmbDef',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqChoEmbOpt',{bool1, int1, seq1 = asn1_NOVALUE}).
-record('SeqOfChoEmbDef_SEQOF',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqOfChoEmbOpt_SEQOF',{bool1, int1, seq1 = asn1_NOVALUE}).

main(_Rules) ->
    roundtrip('SeqChoDef',
	      #'SeqChoDef'{bool1=true,int1=17,seq1=asn1_DEFAULT},
	      #'SeqChoDef'{bool1=true,int1=17,seq1=[]}),
    roundtrip('SeqChoDef',
	      #'SeqChoDef'{bool1=true,int1=17,
			   seq1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SeqChoOpt',
	      #'SeqChoOpt'{bool1=true,int1=17,seq1=asn1_NOVALUE}),
    roundtrip('SeqChoOpt',
	      #'SeqChoOpt'{bool1=true,int1=17,
			   seq1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SeqChoEmbDef',
	      #'SeqChoEmbDef'{bool1=true,int1=17,seq1=asn1_DEFAULT},
	      #'SeqChoEmbDef'{bool1=true,int1=17,seq1=[]}),
    roundtrip('SeqChoEmbDef',
	      #'SeqChoEmbDef'{bool1=true,int1=17,
			      seq1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SeqChoEmbOpt',
	      #'SeqChoEmbOpt'{bool1=true,int1=17,seq1=asn1_NOVALUE}),
    roundtrip('SeqChoEmbOpt',
	      #'SeqChoEmbOpt'{bool1=true,int1=17,
			      seq1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SeqOfChoEmbDef',
	      [#'SeqOfChoEmbDef_SEQOF'{bool1=true,int1=17,seq1=asn1_DEFAULT}],
	      [#'SeqOfChoEmbDef_SEQOF'{bool1=true,int1=17,seq1=[]}]),
    roundtrip('SeqOfChoEmbDef',
	      [#'SeqOfChoEmbDef_SEQOF'{bool1=true,int1=17,
				       seq1=[{boolIn,true},{intIn,25}]}]),
    roundtrip('SeqOfChoEmbOpt',
	      [#'SeqOfChoEmbOpt_SEQOF'{bool1=true,int1=17,seq1=asn1_NOVALUE}]),
    roundtrip('SeqOfChoEmbOpt',
	      [#'SeqOfChoEmbOpt_SEQOF'{bool1=true,int1=17,
				       seq1=[{boolIn,true},{intIn,25}]}]),
    ok.

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('SeqOfCho', Type, Value, ExpectedValue).
