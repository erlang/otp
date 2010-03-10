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
-module(testSetTag).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").
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


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetTag",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetTag','SetTag',#'SetTag'{nt = #'NT'{bool = true, os = "kalle"},
						  imp = #'Imp'{bool = true, os = "kalle"},
						  exp = #'Exp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SetTag',{'NT',"kalle",true},{'Imp',"kalle",true},{'Exp',"kalle",true}}} = 
	asn1_wrapper:decode('SetTag','SetTag',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SetTag','SetTagImp',#'SetTagImp'{nt = #'NT'{bool = true, os = "kalle"},
							imp = #'Imp'{bool = true, os = "kalle"},
							exp = #'Exp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SetTagImp',{'NT',"kalle",true},{'Imp',"kalle",true},{'Exp',"kalle",true}}} = 
	asn1_wrapper:decode('SetTag','SetTagImp',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SetTag','SetTagExp',#'SetTagExp'{nt = #'NT'{bool = true, os = "kalle"},
							imp = #'Imp'{bool = true, os = "kalle"},
							exp = #'Exp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SetTagExp',{'NT',"kalle",true},{'Imp',"kalle",true},{'Exp',"kalle",true}}} = 
	asn1_wrapper:decode('SetTag','SetTagExp',lists:flatten(Bytes13)),
    
    
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetTag','SetTagX',
		      #'SetTagX'{xnt = #'XSetNT'{bool = true, os = "kalle"},
				 ximp = #'XSetImp'{bool = true, os = "kalle"},
				 xexp = #'XSetExp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SetTagX',{'XSetNT',"kalle",true},
	       {'XSetImp',"kalle",true},
	       {'XSetExp',"kalle",true}}} = 
	asn1_wrapper:decode('SetTag','SetTagX',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SetTag','SetTagImpX',
		      #'SetTagImpX'{xnt = #'XSetNT'{bool = true, os = "kalle"},
				    ximp = #'XSetImp'{bool = true, os = "kalle"},
				    xexp = #'XSetExp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SetTagImpX',{'XSetNT',"kalle",true},
	       {'XSetImp',"kalle",true},
	       {'XSetExp',"kalle",true}}} = 
	asn1_wrapper:decode('SetTag','SetTagImpX',lists:flatten(Bytes22)),
    
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SetTag','SetTagExpX',
		      #'SetTagExpX'{xnt = #'XSetNT'{bool = true, os = "kalle"},
				    ximp = #'XSetImp'{bool = true, os = "kalle"},
				    xexp = #'XSetExp'{bool = true, os = "kalle"}}),
    ?line {ok,{'SetTagExpX',{'XSetNT',"kalle",true},
	       {'XSetImp',"kalle",true},
	       {'XSetExp',"kalle",true}}} = 
	asn1_wrapper:decode('SetTag','SetTagExpX',lists:flatten(Bytes23)),
    
    
    
    
    
    ok.
