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
-module(testSeqExtension).

-include("External.hrl").
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqExt1',{}).
-record('SeqExt2',{bool, int}).
-record('SeqExt3',{bool, int}).
-record('SeqExt4',{bool, int}).


main(_Rules) ->
    roundtrip('SeqExt1', #'SeqExt1'{}),

    roundtrip('SeqExt2', #'SeqExt2'{bool=true,int=99}),
    roundtrip('SeqExt2', #'SeqExt2'{bool=false,int=42}),

    roundtrip('SeqExt3', #'SeqExt3'{bool=true,int=-77777}),
    roundtrip('SeqExt3', #'SeqExt3'{bool=false,int=-42000}),

    roundtrip('SeqExt4', #'SeqExt4'{bool=true,int=12345}),
    roundtrip('SeqExt4', #'SeqExt4'{bool=false,int=123456}),

    ok.

roundtrip(Type, Value) ->
    {ok,Encoded} = 'SeqExtension':encode(Type, Value),
    {ok,Value} = 'SeqExtension':decode(Type, Encoded),
    ok.
