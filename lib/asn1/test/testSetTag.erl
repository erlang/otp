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
-module(testSetTag).
-export([main/1]).

-include_lib("common_test/include/ct.hrl").
-include("External.hrl").

-record('SetTag',{nt, imp, exp}).
-record('SetTagImp',{nt, imp, exp}).
-record('SetTagExp',{nt, imp, exp}).
-record('SetTagX',{xnt, ximp, xexp}).
-record('SetTagImpX',{xnt, ximp, xexp}).
-record('SetTagExpX',{xnt, ximp, xexp}).
-record('NT',{os, bool}).
-record('Imp',{os, bool}).
-record('Exp',{os, bool}).

main(_Rules) ->
    roundtrip('SetTag', #'SetTag'{nt=#'NT'{os = <<"kalle">>,bool=true},
				  imp=#'Imp'{os = <<"kalle">>,bool=true},
				  exp=#'Exp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SetTagImp', #'SetTagImp'{nt=#'NT'{os = <<"kalle">>,bool=true},
					imp=#'Imp'{os = <<"kalle">>,bool=true},
					exp=#'Exp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SetTagExp', #'SetTagExp'{nt=#'NT'{os = <<"kalle">>,bool=true},
					imp=#'Imp'{os = <<"kalle">>,bool=true},
					exp=#'Exp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SetTagX', #'SetTagX'{xnt=#'XSetNT'{os = <<"kalle">>,bool=true},
				    ximp=#'XSetImp'{os = <<"kalle">>,bool=true},
				    xexp=#'XSetExp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SetTagImpX', #'SetTagImpX'{xnt=#'XSetNT'{os = <<"kalle">>,bool=true},
					  ximp=#'XSetImp'{os = <<"kalle">>,bool=true},
					  xexp=#'XSetExp'{os = <<"kalle">>,bool=true}}),
    roundtrip('SetTagExpX', #'SetTagExpX'{xnt=#'XSetNT'{os = <<"kalle">>,bool=true},
					  ximp=#'XSetImp'{os = <<"kalle">>,bool=true},
					  xexp=#'XSetExp'{os = <<"kalle">>,bool=true}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetTag', T, V).
