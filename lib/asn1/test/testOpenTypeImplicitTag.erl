%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
-module(testOpenTypeImplicitTag).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

main(_Rules) ->
    roundtrip('Seq', {'Seq',<<1,1,255>>,<<1,1,255>>,12,<<1,1,255>>}),
    roundtrip('Seq', {'Seq',<<1,1,255>>,asn1_NOVALUE,12,<<1,1,255>>}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('OpenTypeImplicitTag', T, V).
