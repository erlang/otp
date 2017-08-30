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
-module(testSetOfExternal).
-export([main/1]).

-include_lib("common_test/include/ct.hrl").
-include("External.hrl").

-record('NT',{os, bool}).
-record('Imp',{os, bool}).
-record('Exp',{os, bool}).

main(_Rules) ->
    roundtrip('NTNT', [#'NT'{os = <<"kalle">>,bool=true},
		       #'NT'{os = <<"kalle">>,bool=true}]),
    roundtrip('ImpNT', [#'NT'{os = <<"kalle">>,bool=true},
			#'NT'{os = <<"kalle">>,bool=true}]),
    roundtrip('ExpNT', [#'NT'{os = <<"kalle">>,bool=true},
			#'NT'{os = <<"kalle">>,bool=true}]),
    roundtrip('NTImp', [#'Imp'{os = <<"kalle">>,bool=true},
			#'Imp'{os = <<"kalle">>,bool=true}]),
    roundtrip('ImpImp', [#'Imp'{os = <<"kalle">>,bool=true},
			 #'Imp'{os = <<"kalle">>,bool=true}]),
    roundtrip('ExpImp', [#'Imp'{os = <<"kalle">>,bool=true},
			 #'Imp'{os = <<"kalle">>,bool=true}]),
    roundtrip('NTExp', [#'Exp'{os = <<"kalle">>,bool=true},
			#'Exp'{os = <<"kalle">>,bool=true}]),
    roundtrip('ImpExp', [#'Exp'{os = <<"kalle">>,bool=true},
			 #'Exp'{os = <<"kalle">>,bool=true}]),
    roundtrip('ExpExp', [#'Exp'{os = <<"kalle">>,bool=true},
			 #'Exp'{os = <<"kalle">>,bool=true}]),
    roundtrip('XNTNT', [#'XSetNT'{os = <<"kalle">>,bool=true},
			#'XSetNT'{os = <<"kalle">>,bool=true}]),
    roundtrip('XImpNT', [#'XSetNT'{os = <<"kalle">>,bool=true},
			 #'XSetNT'{os = <<"kalle">>,bool=true}]),
    roundtrip('XExpNT', [#'XSetNT'{os = <<"kalle">>,bool=true},
			 #'XSetNT'{os = <<"kalle">>,bool=true}]),
    roundtrip('XNTImp', [#'XSetImp'{os = <<"kalle">>,bool=true},
			 #'XSetImp'{os = <<"kalle">>,bool=true}]),
    roundtrip('XImpImp', [#'XSetImp'{os = <<"kalle">>,bool=true},
			  #'XSetImp'{os = <<"kalle">>,bool=true}]),
    roundtrip('XExpImp', [#'XSetImp'{os = <<"kalle">>,bool=true},
			  #'XSetImp'{os = <<"kalle">>,bool=true}]),
    roundtrip('XNTExp', [#'XSetExp'{os = <<"kalle">>,bool=true},
			 #'XSetExp'{os = <<"kalle">>,bool=true}]),
    roundtrip('XImpExp', [#'XSetExp'{os = <<"kalle">>,bool=true},
			  #'XSetExp'{os = <<"kalle">>,bool=true}]),
    roundtrip('XExpExp', [#'XSetExp'{os = <<"kalle">>,bool=true},
			  #'XSetExp'{os = <<"kalle">>,bool=true}]),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetOfExternal', T, V).
