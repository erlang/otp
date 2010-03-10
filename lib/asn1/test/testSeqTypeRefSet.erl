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
-module(testSeqTypeRefSet).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqTRset',{seqSet, seqSetI, seqSetE, 'seqSet-I', 'seqSetI-I', 'seqSetE-I', 'seqSet-E', 'seqSetI-E', 'seqSetE-E'}).
-record('SeqSet',{setInt, setOs}).
-record('SeqSetImp',{setInt, setOs}).
-record('SeqSetExp',{setInt, setOs}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqTypeRefSet",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SeqTypeRefSet','SeqTRset',
		      #'SeqTRset'{'seqSet' = #'SeqSet'{setOs = "A1",
						       setInt = 2},
				  'seqSetI' = #'SeqSet'{setOs = "A2",
							setInt = 2},
				  'seqSetE' = #'SeqSet'{setOs = "A3",
							setInt = 2},
				  'seqSet-I' = #'SeqSetImp'{setOs = "A4",
							    setInt = 2},
				  'seqSetI-I' = #'SeqSetImp'{setOs = "A5",
							     setInt = 2},
				  'seqSetE-I' = #'SeqSetImp'{setOs = "A6",
							     setInt = 2},
				  'seqSet-E' = #'SeqSetExp'{setOs = "A7",
							    setInt = 2},
				  'seqSetI-E' = #'SeqSetExp'{setOs = "A8",
							     setInt = 2},
				  'seqSetE-E' = #'SeqSetExp'{setOs = "A9",
							     setInt = 2}}),
    ?line {ok,{'SeqTRset',{'SeqSet',2,"A1"},
	       {'SeqSet',2,"A2"},
	       {'SeqSet',2,"A3"},
	       {'SeqSetImp',2,"A4"},
	       {'SeqSetImp',2,"A5"},
	       {'SeqSetImp',2,"A6"},
	       {'SeqSetExp',2,"A7"},
	       {'SeqSetExp',2,"A8"},
	       {'SeqSetExp',2,"A9"}}} = 
	asn1_wrapper:decode('SeqTypeRefSet','SeqTRset',lists:flatten(Bytes41)),
    
    ok.
