%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
-module(testDoubleEllipses).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq',{a, c}).
-record('SeqV2',{a, b ,c}).
-record('SeqAlt',{a,d,b,e,c,f,g}).
-record('SeqAltV2',{a,d,b,e,h,i,c,f,g}).

-record('Set',{a, c}).
-record('SetV2',{a, b ,c}).
-record('SetAlt',{a,d,b,e,c,f,g}).
-record('SetAltV2',{a,d,b,e,h,i,c,f,g}).

main(_Rules) ->
    roundtrip('Seq', #'Seq'{a=10,c=true}),
    roundtrip('SeqV2', #'SeqV2'{a=10,b=false,c=true}),
    roundtrip('SeqAlt',
	      #'SeqAlt'{a=10,d=12,b = <<2#1010:4>>,
			e=true,c=false,f=14,g=16}),
    roundtrip('SeqAltV2',
	      #'SeqAltV2'{a=10,d=12,
			  b = <<2#1010:4>>,
			  e=true,h="PS",i=13,c=false,f=14,g=16}),
    
    roundtrip('Set', #'Set'{a=10,c=true}),
    roundtrip('SetV2', #'SetV2'{a=10,b=false,c=true}),
    roundtrip('SetAlt',
	      #'SetAlt'{a=10,d=12,
			b = <<2#1010:4>>,
			e=true,c=false,f=14,g=16}),
    roundtrip('SetAltV2',
	      #'SetAltV2'{a=10,d=12,
			  b = <<2#1010:4>>,
			  e=true,h="PS",i=13,c=false,f=14,g=16}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('DoubleEllipses', T, V).
