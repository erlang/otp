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
-module(testSeqTag).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").
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


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqTag",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqTag','SeqTag',#'SeqTag'{nt = #'NT'{bool = true, os = "kalle"},
						  imp = #'Imp'{bool = true, os = "kalle"},
						  exp = #'Exp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SeqTag',{'NT',"kalle",true},{'Imp',"kalle",true},{'Exp',"kalle",true}}} = 
	asn1_wrapper:decode('SeqTag','SeqTag',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SeqTag','SeqTagImp',#'SeqTagImp'{nt = #'NT'{bool = true, os = "kalle"},
							imp = #'Imp'{bool = true, os = "kalle"},
							exp = #'Exp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SeqTagImp',{'NT',"kalle",true},{'Imp',"kalle",true},{'Exp',"kalle",true}}} = 
	asn1_wrapper:decode('SeqTag','SeqTagImp',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SeqTag','SeqTagExp',#'SeqTagExp'{nt = #'NT'{bool = true, os = "kalle"},
							imp = #'Imp'{bool = true, os = "kalle"},
							exp = #'Exp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SeqTagExp',{'NT',"kalle",true},{'Imp',"kalle",true},{'Exp',"kalle",true}}} = 
	asn1_wrapper:decode('SeqTag','SeqTagExp',lists:flatten(Bytes13)),
    
    
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqTag','SeqTagX',
		      #'SeqTagX'{xnt = #'XSeqNT'{bool = true, os = "kalle"},
				 ximp = #'XSeqImp'{bool = true, os = "kalle"},
				 xexp = #'XSeqExp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SeqTagX',{'XSeqNT',"kalle",true},
	       {'XSeqImp',"kalle",true},
	       {'XSeqExp',"kalle",true}}} = 
	asn1_wrapper:decode('SeqTag','SeqTagX',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SeqTag','SeqTagImpX',
		      #'SeqTagImpX'{xnt = #'XSeqNT'{bool = true, os = "kalle"},
				    ximp = #'XSeqImp'{bool = true, os = "kalle"},
				    xexp = #'XSeqExp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SeqTagImpX',{'XSeqNT',"kalle",true},
	       {'XSeqImp',"kalle",true},
	       {'XSeqExp',"kalle",true}}} = 
	asn1_wrapper:decode('SeqTag','SeqTagImpX',lists:flatten(Bytes22)),
    
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SeqTag','SeqTagExpX',
		      #'SeqTagExpX'{xnt = #'XSeqNT'{bool = true, os = "kalle"},
				    ximp = #'XSeqImp'{bool = true, os = "kalle"},
				    xexp = #'XSeqExp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SeqTagExpX',{'XSeqNT',"kalle",true},
	       {'XSeqImp',"kalle",true},
	       {'XSeqExp',"kalle",true}}} = 
	asn1_wrapper:decode('SeqTag','SeqTagExpX',lists:flatten(Bytes23)),
    
    
    
    
    
    ok.
