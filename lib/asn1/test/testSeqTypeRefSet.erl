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
-module(testSeqTypeRefSet).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqTRset',{seqSet, seqSetI, seqSetE, 'seqSet-I', 'seqSetI-I', 'seqSetE-I', 'seqSet-E', 'seqSetI-E', 'seqSetE-E'}).
-record('SeqSet',{setInt, setOs}).
-record('SeqSetImp',{setInt, setOs}).
-record('SeqSetExp',{setInt, setOs}).



main(_Rules) ->
    roundtrip('SeqTRset',
	      #'SeqTRset'{seqSet=#'SeqSet'{setInt=2,setOs="A1"},
			  seqSetI=#'SeqSet'{setInt=2,setOs="A2"},
			  seqSetE=#'SeqSet'{setInt=2,setOs="A3"},
			  'seqSet-I'=#'SeqSetImp'{setInt=2,setOs="A4"},
			  'seqSetI-I'=#'SeqSetImp'{setInt=2,setOs="A5"},
			  'seqSetE-I'=#'SeqSetImp'{setInt=2,setOs="A6"},
			  'seqSet-E'=#'SeqSetExp'{setInt=2,setOs="A7"},
			  'seqSetI-E'=#'SeqSetExp'{setInt=2,setOs="A8"},
			  'seqSetE-E'=#'SeqSetExp'{setInt=2,setOs="A9"}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTypeRefSet', T, V).
