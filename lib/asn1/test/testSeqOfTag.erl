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
-module(testSeqOfTag).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").
-include("External.hrl").


-record('SeqTagNt',{nt}).
-record('SeqTagNtI',{imp}).
-record('SeqTagNtE',{exp}).
-record('SeqTagI',{nt}).
-record('SeqTagII',{imp}).
-record('SeqTagIE',{exp}).
-record('SeqTagE',{nt}).
-record('SeqTagEI',{imp}).
-record('SeqTagEE',{exp}).
-record('SeqTagXNt',{xnt}).
-record('SeqTagXI',{ximp}).
-record('SeqTagXE',{xexp}).
-record('SeqTagImpX',{xnt, ximp, xexp}).
-record('SeqTagExpX',{xnt, ximp, xexp}).
-record('NT',{os, bool}).
-record('Imp',{os, bool}).
-record('Exp',{os, bool}).

main(_Rules) ->
    roundtrip('SeqTagNt', #'SeqTagNt'{nt=[#'NT'{os = <<"kalle">>,bool=true},
					  #'NT'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagNtI', #'SeqTagNtI'{imp=[#'Imp'{os = <<"kalle">>,bool=true},
					     #'Imp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagNtE', #'SeqTagNtE'{exp=[#'Exp'{os = <<"kalle">>,bool=true},
					     #'Exp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagI', #'SeqTagI'{nt=[#'NT'{os = <<"kalle">>,bool=true},
					#'NT'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagII', #'SeqTagII'{imp=[#'Imp'{os = <<"kalle">>,bool=true},
					   #'Imp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagIE', #'SeqTagIE'{exp=[#'Exp'{os = <<"kalle">>,bool=true},
					   #'Exp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagE', #'SeqTagE'{nt=[#'NT'{os = <<"kalle">>,bool=true},
					#'NT'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagEI', #'SeqTagEI'{imp=[#'Imp'{os = <<"kalle">>,bool=true},
					   #'Imp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagEE', #'SeqTagEE'{exp=[#'Exp'{os = <<"kalle">>,bool=true},
					   #'Exp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagXNt',
	      #'SeqTagXNt'{xnt=[#'XSeqNT'{os = <<"kalle">>,bool=true},
				#'XSeqNT'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagXI',
	      #'SeqTagXI'{ximp=[#'XSeqImp'{os = <<"kalle">>,bool=true},
				#'XSeqImp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagXE',
	      #'SeqTagXE'{xexp=[#'XSeqExp'{os = <<"kalle">>,bool=true},
				#'XSeqExp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagImpX',
	      #'SeqTagImpX'{xnt=[#'XSeqNT'{os = <<"kalle">>,bool=true},
				 #'XSeqNT'{os = <<"kalle">>,bool=true}],
			    ximp=[#'XSeqImp'{os = <<"kalle">>,bool=true},
				  #'XSeqImp'{os = <<"kalle">>,bool=true}],
			    xexp=[#'XSeqExp'{os = <<"kalle">>,bool=true},
				  #'XSeqExp'{os = <<"kalle">>,bool=true}]}),
    roundtrip('SeqTagExpX',
	      #'SeqTagExpX'{xnt=[#'XSeqNT'{os = <<"kalle">>,bool=true},
				 #'XSeqNT'{os = <<"kalle">>,bool=true}],
			    ximp=[#'XSeqImp'{os = <<"kalle">>,bool=true},
				  #'XSeqImp'{os = <<"kalle">>,bool=true}],
			    xexp=[#'XSeqExp'{os = <<"kalle">>,bool=true},
				  #'XSeqExp'{os = <<"kalle">>,bool=true}]}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqOfTag', T, V).
