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
-module(testSeqOfTag).


-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").
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



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqOfTag",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagNt',
		      #'SeqTagNt'{nt = [#'NT'{bool = true, os = "kalle"},
					#'NT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagNt',
	       [{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagNt',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagNtI',
		      #'SeqTagNtI'{imp = [#'Imp'{bool = true, os = "kalle"},
					  #'Imp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagNtI',
	       [{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagNtI',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagNtE',
		      #'SeqTagNtE'{exp = [#'Exp'{bool = true, os = "kalle"},
					  #'Exp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagNtE',
	       [{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagNtE',lists:flatten(Bytes13)),
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagI',
		      #'SeqTagI'{nt = [#'NT'{bool = true, os = "kalle"},
				       #'NT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagI',
	       [{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagI',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagII',
		      #'SeqTagII'{imp = [#'Imp'{bool = true, os = "kalle"},
					 #'Imp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagII',
	       [{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagII',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagIE',
		      #'SeqTagIE'{exp = [#'Exp'{bool = true, os = "kalle"},
					 #'Exp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagIE',
	       [{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagIE',lists:flatten(Bytes23)),
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagE',
		      #'SeqTagE'{nt = [#'NT'{bool = true, os = "kalle"},
				       #'NT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagE',
	       [{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagE',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagEI',
		      #'SeqTagEI'{imp = [#'Imp'{bool = true, os = "kalle"},
					 #'Imp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagEI',
	       [{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagEI',lists:flatten(Bytes32)),
    
    ?line {ok,Bytes33} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagEE',
		      #'SeqTagEE'{exp = [#'Exp'{bool = true, os = "kalle"},
					 #'Exp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagEE',
	       [{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagEE',lists:flatten(Bytes33)),
    
    
    
    



    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagXNt',
		      #'SeqTagXNt'{xnt = [#'XSeqNT'{bool = true, os = "kalle"},
					  #'XSeqNT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagXNt',
	       [{'XSeqNT',[107,97,108,108,101],true},{'XSeqNT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagXNt',lists:flatten(Bytes41)),
    
    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagXI',
		      #'SeqTagXI'{ximp = [#'XSeqImp'{bool = true, os = "kalle"},
					  #'XSeqImp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagXI',
	       [{'XSeqImp',[107,97,108,108,101],true},{'XSeqImp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagXI',lists:flatten(Bytes42)),
    
    ?line {ok,Bytes43} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagXE',
		      #'SeqTagXE'{xexp = [#'XSeqExp'{bool = true, os = "kalle"},
					  #'XSeqExp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagXE',
	       [{'XSeqExp',[107,97,108,108,101],true},{'XSeqExp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagXE',lists:flatten(Bytes43)),
        
    


    
    ?line {ok,Bytes51} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagImpX',
		      #'SeqTagImpX'{xnt = [#'XSeqNT'{bool = true, os = "kalle"},
					   #'XSeqNT'{bool = true, os = "kalle"}],
				    ximp = [#'XSeqImp'{bool = true, os = "kalle"},
					    #'XSeqImp'{bool = true, os = "kalle"}],
				    xexp = [#'XSeqExp'{bool = true, os = "kalle"},
					    #'XSeqExp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagImpX',
	       [{'XSeqNT',[107,97,108,108,101],true},{'XSeqNT',[107,97,108,108,101],true}], 
	       [{'XSeqImp',[107,97,108,108,101],true},{'XSeqImp',[107,97,108,108,101],true}],
	       [{'XSeqExp',[107,97,108,108,101],true},{'XSeqExp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagImpX',lists:flatten(Bytes51)),
    


    ?line {ok,Bytes52} = 
	asn1_wrapper:encode('SeqOfTag','SeqTagExpX',
		      #'SeqTagExpX'{xnt = [#'XSeqNT'{bool = true, os = "kalle"},
					   #'XSeqNT'{bool = true, os = "kalle"}],
				    ximp = [#'XSeqImp'{bool = true, os = "kalle"},
					    #'XSeqImp'{bool = true, os = "kalle"}],
				    xexp = [#'XSeqExp'{bool = true, os = "kalle"},
					    #'XSeqExp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SeqTagExpX',
	       [{'XSeqNT',[107,97,108,108,101],true},{'XSeqNT',[107,97,108,108,101],true}], 
	       [{'XSeqImp',[107,97,108,108,101],true},{'XSeqImp',[107,97,108,108,101],true}],
	       [{'XSeqExp',[107,97,108,108,101],true},{'XSeqExp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SeqOfTag','SeqTagExpX',lists:flatten(Bytes52)),
    
ok.
