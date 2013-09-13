%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2012. All Rights Reserved.
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
-module(testSeq2738).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqOptFake',{int, opt = asn1_NOVALUE}).
-record('OptSeqFake',{bool = false}).

main(_Rules) ->
    Enc = asn1_test_lib:roundtrip_enc('Seq2738',
				      'SeqOptFake',
				      #'SeqOptFake'{int=10,opt=#'OptSeqFake'{}}),
    {error,_} = 'Seq2738':decode('SeqOpt', Enc),
    ok.
