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
-module(testSeqExternal).

-include("External.hrl").
-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").


-record('SeqXSet1',{set, bool, int}).
-record('SeqXSet2',{bool, set, int}).
-record('SeqXSet3',{bool, int, set}).
%-record('NT',{os, bool}).
%-record('Imp',{os, bool}).
%-record('Exp',{os, bool}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqExternal",
			      [Rules,{outdir,OutDir}]++Options).



main(_Rules) ->

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqExternal','XNTNT',#'XSeqNT'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqNT',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XNTNT',lists:flatten(Bytes11)),

    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SeqExternal','XImpNT',#'XSeqNT'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqNT',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XImpNT',lists:flatten(Bytes12)),

    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SeqExternal','XExpNT',#'XSeqNT'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqNT',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XExpNT',lists:flatten(Bytes13)),

    

    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqExternal','XNTImp',#'XSeqImp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqImp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XNTImp',lists:flatten(Bytes21)),

    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SeqExternal','XImpImp',#'XSeqImp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqImp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XImpImp',lists:flatten(Bytes22)),

    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SeqExternal','XExpImp',#'XSeqImp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqImp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XExpImp',lists:flatten(Bytes23)),


    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqExternal','XNTExp',#'XSeqExp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqExp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XNTExp',lists:flatten(Bytes31)),

    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SeqExternal','XImpExp',#'XSeqExp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqExp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XImpExp',lists:flatten(Bytes32)),

    ?line {ok,Bytes33} = 
	asn1_wrapper:encode('SeqExternal','XExpExp',#'XSeqExp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSeqExp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SeqExternal','XExpExp',lists:flatten(Bytes33)),


    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SeqExternal','SeqXSet1',
		      #'SeqXSet1'{bool = true, 
				  int = 66,
				  set = #'XSet1'{bool1 = true, 
						 int1 = 77,
						 set1 = #'XSetIn'{boolIn = false, 
								  intIn = 88}}}),
    ?line {ok,{'SeqXSet1',{'XSet1',true,77,{'XSetIn',false,88}},true,66}} = 
	asn1_wrapper:decode('SeqExternal','SeqXSet1',lists:flatten(Bytes41)),



    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SeqExternal','SeqXSet2',
		      #'SeqXSet2'{bool = true, 
				  int = 66,
				  set = #'XSet1'{bool1 = true, 
						 int1 = 77,
						 set1 = #'XSetIn'{boolIn = false, 
								  intIn = 88}}}),
    ?line {ok,{'SeqXSet2',true,{'XSet1',true,77,{'XSetIn',false,88}},66}} = 
	asn1_wrapper:decode('SeqExternal','SeqXSet2',lists:flatten(Bytes42)),

    ?line {ok,Bytes43} = 
	asn1_wrapper:encode('SeqExternal','SeqXSet3',
		      #'SeqXSet3'{bool = true, 
				  int = 66,
				  set = #'XSet1'{bool1 = true, 
						 int1 = 77,
						 set1 = #'XSetIn'{boolIn = false, 
								  intIn = 88}}}),
    ?line {ok,{'SeqXSet3',true,66,{'XSet1',true,77,{'XSetIn',false,88}}}} = 
	asn1_wrapper:decode('SeqExternal','SeqXSet3',lists:flatten(Bytes43)),


    

    ok.


