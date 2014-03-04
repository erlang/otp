%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
-module(testSeqTypeRefPrim).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqTR',{octStr, octStrI, octStrE, 'octStr-I', 'octStrI-I', 'octStrE-I', 'octStr-E', 'octStrI-E', 'octStrE-E'}).

main(_Rules) ->
    roundtrip('SeqTR',
	      #'SeqTR'{'octStr' = <<"A string 1">>,
		       'octStrI' = <<"A string 2">>,
		       'octStrE' = <<"A string 3">>,
		       'octStr-I' = <<"A string 4">>,
		       'octStrI-I' = <<"A string 5">>,
		       'octStrE-I' = <<"A string 6">>,
		       'octStr-E' = <<"A string 7">>,
		       'octStrI-E' = <<"A string 8">>,
		       'octStrE-E' = <<"A string 9">>}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTypeRefPrim', T, V).
