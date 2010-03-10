%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(testSetTypeRefSeq).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SetTRseq',{setSeq, setSeqI, setSeqE, 'setSeq-I', 'setSeqI-I', 'setSeqE-I', 'setSeq-E', 'setSeqI-E', 'setSeqE-E'}).
-record('SetSeq',{seqInt, seqOs}).
-record('SetSeqImp',{seqInt, seqOs}).
-record('SetSeqExp',{seqInt, seqOs}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetTypeRefSeq",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SetTypeRefSeq','SetTRseq',
		      #'SetTRseq'{'setSeq' = #'SetSeq'{seqOs = "A1",
						       seqInt = 2},
				  'setSeqI' = #'SetSeq'{seqOs = "A2",
							seqInt = 2},
				  'setSeqE' = #'SetSeq'{seqOs = "A3",
							seqInt = 2},
				  'setSeq-I' = #'SetSeqImp'{seqOs = "A4",
							    seqInt = 2},
				  'setSeqI-I' = #'SetSeqImp'{seqOs = "A5",
							     seqInt = 2},
				  'setSeqE-I' = #'SetSeqImp'{seqOs = "A6",
							     seqInt = 2},
				  'setSeq-E' = #'SetSeqExp'{seqOs = "A7",
							    seqInt = 2},
				  'setSeqI-E' = #'SetSeqExp'{seqOs = "A8",
							     seqInt = 2},
				  'setSeqE-E' = #'SetSeqExp'{seqOs = "A9",
							     seqInt = 2}}),
    ?line {ok,{'SetTRseq',{'SetSeq',2,"A1"},
	       {'SetSeq',2,"A2"},
	       {'SetSeq',2,"A3"},
	       {'SetSeqImp',2,"A4"},
	       {'SetSeqImp',2,"A5"},
	       {'SetSeqImp',2,"A6"},
	       {'SetSeqExp',2,"A7"},
	       {'SetSeqExp',2,"A8"},
	       {'SetSeqExp',2,"A9"}}} = 
	asn1_wrapper:decode('SetTypeRefSeq','SetTRseq',lists:flatten(Bytes41)),
    
    ok.
