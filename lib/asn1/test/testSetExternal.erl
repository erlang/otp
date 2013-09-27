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
-module(testSetExternal).
-export([main/1]).

-include("External.hrl").
-include_lib("test_server/include/test_server.hrl").

-record('SetXSeq1',{seq, bool, int}).
-record('SetXSeq2',{bool, seq, int}).
-record('SetXSeq3',{bool, int, seq}).

main(_Rules) ->
    roundtrip('XNTNT', #'XSetNT'{os="kalle",bool=true}),
    roundtrip('XImpNT', #'XSetNT'{os="kalle",bool=true}),
    roundtrip('XExpNT', #'XSetNT'{os="kalle",bool=true}),
    roundtrip('XNTImp', #'XSetImp'{os="kalle",bool=true}),
    roundtrip('XImpImp', #'XSetImp'{os="kalle",bool=true}),
    roundtrip('XExpImp', #'XSetImp'{os="kalle",bool=true}),
    roundtrip('XNTExp', #'XSetExp'{os="kalle",bool=true}),
    roundtrip('XImpExp', #'XSetExp'{os="kalle",bool=true}),
    roundtrip('XExpExp', #'XSetExp'{os="kalle",bool=true}),
    roundtrip('SetXSeq1', #'SetXSeq1'{seq=#'XSeq1'{bool1=true,int1=77,
						   seq1=#'XSeqIn'{boolIn=false,intIn=88}},
				      bool=true,int=66}),
    roundtrip('SetXSeq2', #'SetXSeq2'{bool=true,
				      seq=#'XSeq1'{bool1=true,int1=77,
						   seq1=#'XSeqIn'{boolIn=false,intIn=88}},
				      int=66}),
    roundtrip('SetXSeq3', #'SetXSeq3'{bool=true,int=66,
				      seq=#'XSeq1'{bool1=true,int1=77,
						   seq1=#'XSeqIn'{boolIn=false,intIn=88}}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetExternal', T, V).
