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
-module(testSetOfTag).


-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").
-include("External.hrl").


-record('SetTagNt',{nt}).
-record('SetTagNtI',{imp}).
-record('SetTagNtE',{exp}).
-record('SetTagI',{nt}).
-record('SetTagII',{imp}).
-record('SetTagIE',{exp}).
-record('SetTagE',{nt}).
-record('SetTagEI',{imp}).
-record('SetTagEE',{exp}).
-record('SetTagXNt',{xnt}).
-record('SetTagXI',{ximp}).
-record('SetTagXE',{xexp}).
-record('SetTagImpX',{xnt, ximp, xexp}).
-record('SetTagExpX',{xnt, ximp, xexp}).
-record('NT',{os, bool}).
-record('Imp',{os, bool}).
-record('Exp',{os, bool}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetOfTag",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetOfTag','SetTagNt',
		      #'SetTagNt'{nt = [#'NT'{bool = true, os = "kalle"},
					#'NT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagNt',
	       [{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagNt',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SetOfTag','SetTagNtI',
		      #'SetTagNtI'{imp = [#'Imp'{bool = true, os = "kalle"},
					  #'Imp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagNtI',
	       [{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagNtI',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SetOfTag','SetTagNtE',
		      #'SetTagNtE'{exp = [#'Exp'{bool = true, os = "kalle"},
					  #'Exp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagNtE',
	       [{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagNtE',lists:flatten(Bytes13)),
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetOfTag','SetTagI',
		      #'SetTagI'{nt = [#'NT'{bool = true, os = "kalle"},
				       #'NT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagI',
	       [{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagI',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SetOfTag','SetTagII',
		      #'SetTagII'{imp = [#'Imp'{bool = true, os = "kalle"},
					 #'Imp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagII',
	       [{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagII',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SetOfTag','SetTagIE',
		      #'SetTagIE'{exp = [#'Exp'{bool = true, os = "kalle"},
					 #'Exp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagIE',
	       [{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagIE',lists:flatten(Bytes23)),
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetOfTag','SetTagE',
		      #'SetTagE'{nt = [#'NT'{bool = true, os = "kalle"},
				       #'NT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagE',
	       [{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagE',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SetOfTag','SetTagEI',
		      #'SetTagEI'{imp = [#'Imp'{bool = true, os = "kalle"},
					 #'Imp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagEI',
	       [{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagEI',lists:flatten(Bytes32)),
    
    ?line {ok,Bytes33} = 
	asn1_wrapper:encode('SetOfTag','SetTagEE',
		      #'SetTagEE'{exp = [#'Exp'{bool = true, os = "kalle"},
					 #'Exp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagEE',
	       [{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagEE',lists:flatten(Bytes33)),
    
    
    
    



    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SetOfTag','SetTagXNt',
		      #'SetTagXNt'{xnt = [#'XSetNT'{bool = true, os = "kalle"},
					  #'XSetNT'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagXNt',
	       [{'XSetNT',[107,97,108,108,101],true},{'XSetNT',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagXNt',lists:flatten(Bytes41)),
    
    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SetOfTag','SetTagXI',
		      #'SetTagXI'{ximp = [#'XSetImp'{bool = true, os = "kalle"},
					  #'XSetImp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagXI',
	       [{'XSetImp',[107,97,108,108,101],true},{'XSetImp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagXI',lists:flatten(Bytes42)),
    
    ?line {ok,Bytes43} = 
	asn1_wrapper:encode('SetOfTag','SetTagXE',
		      #'SetTagXE'{xexp = [#'XSetExp'{bool = true, os = "kalle"},
					  #'XSetExp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagXE',
	       [{'XSetExp',[107,97,108,108,101],true},{'XSetExp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagXE',lists:flatten(Bytes43)),
        
    


    
    ?line {ok,Bytes51} = 
	asn1_wrapper:encode('SetOfTag','SetTagImpX',
		      #'SetTagImpX'{xnt = [#'XSetNT'{bool = true, os = "kalle"},
					   #'XSetNT'{bool = true, os = "kalle"}],
				    ximp = [#'XSetImp'{bool = true, os = "kalle"},
					    #'XSetImp'{bool = true, os = "kalle"}],
				    xexp = [#'XSetExp'{bool = true, os = "kalle"},
					    #'XSetExp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagImpX',
	       [{'XSetNT',[107,97,108,108,101],true},{'XSetNT',[107,97,108,108,101],true}], 
	       [{'XSetImp',[107,97,108,108,101],true},{'XSetImp',[107,97,108,108,101],true}],
	       [{'XSetExp',[107,97,108,108,101],true},{'XSetExp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagImpX',lists:flatten(Bytes51)),
    


    ?line {ok,Bytes52} = 
	asn1_wrapper:encode('SetOfTag','SetTagExpX',
		      #'SetTagExpX'{xnt = [#'XSetNT'{bool = true, os = "kalle"},
					   #'XSetNT'{bool = true, os = "kalle"}],
				    ximp = [#'XSetImp'{bool = true, os = "kalle"},
					    #'XSetImp'{bool = true, os = "kalle"}],
				    xexp = [#'XSetExp'{bool = true, os = "kalle"},
					    #'XSetExp'{bool = true, os = "kalle"}]}),
    ?line {ok,{'SetTagExpX',
	       [{'XSetNT',[107,97,108,108,101],true},{'XSetNT',[107,97,108,108,101],true}], 
	       [{'XSetImp',[107,97,108,108,101],true},{'XSetImp',[107,97,108,108,101],true}],
	       [{'XSetExp',[107,97,108,108,101],true},{'XSetExp',[107,97,108,108,101],true}]}} = 
	asn1_wrapper:decode('SetOfTag','SetTagExpX',lists:flatten(Bytes52)),
    
    ok.
