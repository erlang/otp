%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
-module(testSeqSetIndefinite).
-export([main/0]).

-include_lib("test_server/include/test_server.hrl").

main() ->
    seq_indefinite(),
    set_indefinite().

seq_indefinite() ->
    %% normal encoding
    B = <<48,20,1,1,255,48,9,1,1,255,2,4,251,35,238,194,2,4,251,55,236,161>>,
    %% indefinite length encoding
    Bi = <<48,22,1,1,255,48,128,1,1,255,2,4,251,35,238,194,0,0,2,4,251,55,236,161>>,
    %% the value which is encoded
    V = {'SeqS3',true,{'SeqS3_seqS3',true,-81531198},-80221023},
    {ok,V} = 'SeqSetIndefinite':decode('SeqS3', B),
    {ok,V} = 'SeqSetIndefinite':decode('SeqS3', Bi),

    ok.

set_indefinite() ->
    %% normal encoding
    B = <<49,20,1,1,255,49,9,1,1,255,2,4,251,35,238,194,2,4,251,55,236,161>>,
    %% indefinite length encoding
    Bi = <<49,22,1,1,255,49,128,1,1,255,2,4,251,35,238,194,0,0,2,4,251,55,236,161>>,

    %% the value which is encoded
    V = {'SetS3',true,{'SetS3_setS3',true,-81531198},-80221023},
    {ok,V} = 'SeqSetIndefinite':decode('SetS3', B),
    {ok,V} = 'SeqSetIndefinite':decode('SetS3', Bi),

    ok.
