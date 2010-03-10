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
-module(testSetOfExternal).


-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").
-include("External.hrl").

-record('NT',{os, bool}).
-record('Imp',{os, bool}).
-record('Exp',{os, bool}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetOfExternal",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetOfExternal','NTNT',[#'NT'{bool = true, os = "kalle"},
					      #'NT'{bool = true, os = "kalle"}]),
    ?line {ok,[{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','NTNT',lists:flatten(Bytes11)),

    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SetOfExternal','ImpNT',[#'NT'{bool = true, os = "kalle"},
					       #'NT'{bool = true, os = "kalle"}]),
    ?line {ok,[{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','ImpNT',lists:flatten(Bytes12)),

    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SetOfExternal','ExpNT',[#'NT'{bool = true, os = "kalle"},
					       #'NT'{bool = true, os = "kalle"}]),
    ?line {ok,[{'NT',[107,97,108,108,101],true},{'NT',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','ExpNT',lists:flatten(Bytes13)),

    

    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetOfExternal','NTImp',[#'Imp'{bool = true, os = "kalle"},
					       #'Imp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','NTImp',lists:flatten(Bytes21)),

    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SetOfExternal','ImpImp',[#'Imp'{bool = true, os = "kalle"},
						#'Imp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','ImpImp',lists:flatten(Bytes22)),

    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SetOfExternal','ExpImp',[#'Imp'{bool = true, os = "kalle"},
						#'Imp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'Imp',[107,97,108,108,101],true},{'Imp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','ExpImp',lists:flatten(Bytes23)),


    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetOfExternal','NTExp',[#'Exp'{bool = true, os = "kalle"},
					       #'Exp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','NTExp',lists:flatten(Bytes31)),

    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SetOfExternal','ImpExp',[#'Exp'{bool = true, os = "kalle"},
						#'Exp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','ImpExp',lists:flatten(Bytes32)),

    ?line {ok,Bytes33} = 
	asn1_wrapper:encode('SetOfExternal','ExpExp',[#'Exp'{bool = true, os = "kalle"},
						#'Exp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'Exp',[107,97,108,108,101],true},{'Exp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','ExpExp',lists:flatten(Bytes33)),







    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SetOfExternal','XNTNT',[#'XSetNT'{bool = true, os = "kalle"},
					       #'XSetNT'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetNT',[107,97,108,108,101],true},{'XSetNT',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XNTNT',lists:flatten(Bytes41)),

    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SetOfExternal','XImpNT',[#'XSetNT'{bool = true, os = "kalle"},
						#'XSetNT'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetNT',[107,97,108,108,101],true},{'XSetNT',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XImpNT',lists:flatten(Bytes42)),

    ?line {ok,Bytes43} = 
	asn1_wrapper:encode('SetOfExternal','XExpNT',[#'XSetNT'{bool = true, os = "kalle"},
						#'XSetNT'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetNT',[107,97,108,108,101],true},{'XSetNT',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XExpNT',lists:flatten(Bytes43)),

    

    ?line {ok,Bytes51} = 
	asn1_wrapper:encode('SetOfExternal','XNTImp',[#'XSetImp'{bool = true, os = "kalle"},
						#'XSetImp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetImp',[107,97,108,108,101],true},{'XSetImp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XNTImp',lists:flatten(Bytes51)),

    ?line {ok,Bytes52} = 
	asn1_wrapper:encode('SetOfExternal','XImpImp',[#'XSetImp'{bool = true, os = "kalle"},
						 #'XSetImp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetImp',[107,97,108,108,101],true},{'XSetImp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XImpImp',lists:flatten(Bytes52)),

    ?line {ok,Bytes53} = 
	asn1_wrapper:encode('SetOfExternal','XExpImp',[#'XSetImp'{bool = true, os = "kalle"},
						 #'XSetImp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetImp',[107,97,108,108,101],true},{'XSetImp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XExpImp',lists:flatten(Bytes53)),


    
    ?line {ok,Bytes61} = 
	asn1_wrapper:encode('SetOfExternal','XNTExp',[#'XSetExp'{bool = true, os = "kalle"},
						#'XSetExp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetExp',[107,97,108,108,101],true},{'XSetExp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XNTExp',lists:flatten(Bytes61)),

    ?line {ok,Bytes62} = 
	asn1_wrapper:encode('SetOfExternal','XImpExp',[#'XSetExp'{bool = true, os = "kalle"},
						 #'XSetExp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetExp',[107,97,108,108,101],true},{'XSetExp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XImpExp',lists:flatten(Bytes62)),

    ?line {ok,Bytes63} = 
	asn1_wrapper:encode('SetOfExternal','XExpExp',[#'XSetExp'{bool = true, os = "kalle"},
						 #'XSetExp'{bool = true, os = "kalle"}]),
    ?line {ok,[{'XSetExp',[107,97,108,108,101],true},{'XSetExp',[107,97,108,108,101],true}]} = 
	asn1_wrapper:decode('SetOfExternal','XExpExp',lists:flatten(Bytes63)),


    
    
    ok.
