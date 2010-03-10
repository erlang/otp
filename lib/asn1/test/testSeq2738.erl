%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

%-record('SeqOpt',{int, opt = asn1_NOVALUE}).
-record('SeqOptFake',{int, opt = asn1_NOVALUE}).
%-record('OptSeq',{int=17}).
-record('OptSeqFake',{bool = false}).




compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "Seq2738",[Rules,{outdir,OutDir}]++Options).


main(_Rules) ->
    
    ?line {ok,Bytes} = 
	asn1_wrapper:encode('Seq2738','SeqOptFake',
		      #'SeqOptFake'{int = 10,
				opt = #'OptSeqFake'{}}),
    ?line {ok,#'SeqOptFake'{int=10,opt=#'OptSeqFake'{bool=false}}} = 
	asn1_wrapper:decode('Seq2738','SeqOptFake',lists:flatten(Bytes)),
    ?line {error,_} = 
	asn1_wrapper:decode('Seq2738','SeqOpt',lists:flatten(Bytes)),
    ok.
