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
-module(testSeqExternal).

-include("External.hrl").
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqXSet1',{set, bool, int}).
-record('SeqXSet2',{bool, set, int}).
-record('SeqXSet3',{bool, int, set}).

main(_Rules) ->
    roundtrip('XNTNT', #'XSeqNT'{os="kalle",bool=true}),
    roundtrip('XImpNT', #'XSeqNT'{os="kalle",bool=true}),
    roundtrip('XExpNT', #'XSeqNT'{os="kalle",bool=true}),
    roundtrip('XNTImp', #'XSeqImp'{os="kalle",bool=true}),
    roundtrip('XImpImp', #'XSeqImp'{os="kalle",bool=true}),
    roundtrip('XExpImp', #'XSeqImp'{os="kalle",bool=true}),
    roundtrip('XNTExp', #'XSeqExp'{os="kalle",bool=true}),
    roundtrip('XImpExp', #'XSeqExp'{os="kalle",bool=true}),
    roundtrip('XExpExp', #'XSeqExp'{os="kalle",bool=true}),
    roundtrip('SeqXSet1',
	      #'SeqXSet1'{set=#'XSet1'{bool1=true,int1=77,
				       set1=#'XSetIn'{boolIn=false,intIn=88}},
			  bool=true,int=66}),
    roundtrip('SeqXSet2',
	      #'SeqXSet2'{bool=true,
			  set=#'XSet1'{bool1=true,int1=77,
				       set1=#'XSetIn'{boolIn=false,intIn=88}},
			  int=66}),
    roundtrip('SeqXSet3',
	      #'SeqXSet3'{bool=true,int=66,
			  set=#'XSet1'{bool1=true,int1=77,
				       set1=#'XSetIn'{boolIn=false,intIn=88}}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqExternal', T, V).
