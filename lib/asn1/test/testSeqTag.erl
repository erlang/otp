%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(testSeqTag).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").
-include("External.hrl").

-record('SeqTag',{nt, imp, exp}).
-record('SeqTagImp',{nt, imp, exp}).
-record('SeqTagExp',{nt, imp, exp}).
-record('SeqTagX',{xnt, ximp, xexp}).
-record('SeqTagImpX',{xnt, ximp, xexp}).
-record('SeqTagExpX',{xnt, ximp, xexp}).
-record('NT',{os, bool}).
-record('Imp',{os, bool}).
-record('Exp',{os, bool}).

main(_Rules) ->
    roundtrip('SeqTag', #'SeqTag'{nt=#'NT'{os = <<"kalle">>,bool=true},
				  imp=#'Imp'{os = <<"kalle">>,bool=true},
				  exp=#'Exp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SeqTagImp', #'SeqTagImp'{nt=#'NT'{os = <<"kalle">>,bool=true},
					imp=#'Imp'{os = <<"kalle">>,bool=true},
					exp=#'Exp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SeqTagExp', #'SeqTagExp'{nt=#'NT'{os = <<"kalle">>,bool=true},
					imp=#'Imp'{os = <<"kalle">>,bool=true},
					exp=#'Exp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SeqTagX', #'SeqTagX'{xnt=#'XSeqNT'{os = <<"kalle">>,bool=true},
				    ximp=#'XSeqImp'{os = <<"kalle">>,bool=true},
				    xexp=#'XSeqExp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SeqTagImpX', #'SeqTagImpX'{xnt=#'XSeqNT'{os = <<"kalle">>,bool=true},
					  ximp=#'XSeqImp'{os = <<"kalle">>,bool=true},
					  xexp=#'XSeqExp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SeqTagExpX', #'SeqTagExpX'{xnt=#'XSeqNT'{os = <<"kalle">>,bool=true},
					  ximp=#'XSeqImp'{os = <<"kalle">>,bool=true},
					  xexp=#'XSeqExp'{os = <<"kalle">>,bool=true}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTag', T, V).
