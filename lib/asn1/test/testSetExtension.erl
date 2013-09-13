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
-module(testSetExtension).
-include("External.hrl").
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SetExt1',{}).
-record('SetExt2',{bool, int}).
-record('SetExt3',{bool, int}).
-record('SetExt4',{bool, int}).

main(_Rules) ->
    roundtrip('SetExt1', #'SetExt1'{}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    roundtrip('SetExt3', #'SetExt3'{bool=true,int=99}),
    roundtrip('SetExt3', #'SetExt3'{bool=true,int=99}),
    roundtrip('SetExt4', #'SetExt4'{bool=true,int=99}),
    roundtrip('SetExt4', #'SetExt4'{bool=true,int=99}),
    roundtrip('SetExt1', #'SetExt1'{}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetExtension', T, V).


