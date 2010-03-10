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
-module(testSetExternal).

-include("External.hrl").
-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").


-record('SetXSeq1',{seq, bool, int}).
-record('SetXSeq2',{bool, seq, int}).
-record('SetXSeq3',{bool, int, seq}).
%-record('NT',{os, bool}).
%-record('Imp',{os, bool}).
%-record('Exp',{os, bool}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetExternal",
			      [Rules,{outdir,OutDir}]++Options).



main(_Rules) ->

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetExternal','XNTNT',#'XSetNT'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetNT',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XNTNT',lists:flatten(Bytes11)),

    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SetExternal','XImpNT',#'XSetNT'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetNT',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XImpNT',lists:flatten(Bytes12)),

    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SetExternal','XExpNT',#'XSetNT'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetNT',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XExpNT',lists:flatten(Bytes13)),

    

    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetExternal','XNTImp',#'XSetImp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetImp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XNTImp',lists:flatten(Bytes21)),

    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SetExternal','XImpImp',#'XSetImp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetImp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XImpImp',lists:flatten(Bytes22)),

    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SetExternal','XExpImp',#'XSetImp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetImp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XExpImp',lists:flatten(Bytes23)),


    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetExternal','XNTExp',#'XSetExp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetExp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XNTExp',lists:flatten(Bytes31)),

    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SetExternal','XImpExp',#'XSetExp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetExp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XImpExp',lists:flatten(Bytes32)),

    ?line {ok,Bytes33} = 
	asn1_wrapper:encode('SetExternal','XExpExp',#'XSetExp'{bool = true, os = "kalle"}),
    ?line {ok,{'XSetExp',[107,97,108,108,101],true}} = 
	asn1_wrapper:decode('SetExternal','XExpExp',lists:flatten(Bytes33)),


    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SetExternal','SetXSeq1',
		      #'SetXSeq1'{bool = true, 
				  int = 66,
				  seq = #'XSeq1'{bool1 = true, 
						 int1 = 77,
						 seq1 = #'XSeqIn'{boolIn = false, 
								  intIn = 88}}}),
    ?line {ok,{'SetXSeq1',{'XSeq1',true,77,{'XSeqIn',false,88}},true,66}} = 
	asn1_wrapper:decode('SetExternal','SetXSeq1',lists:flatten(Bytes41)),



    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SetExternal','SetXSeq2',
		      #'SetXSeq2'{bool = true, 
				  int = 66,
				  seq = #'XSeq1'{bool1 = true, 
						 int1 = 77,
						 seq1 = #'XSeqIn'{boolIn = false, 
								  intIn = 88}}}),
    ?line {ok,{'SetXSeq2',true,{'XSeq1',true,77,{'XSeqIn',false,88}},66}} = 
	asn1_wrapper:decode('SetExternal','SetXSeq2',lists:flatten(Bytes42)),

    ?line {ok,Bytes43} = 
	asn1_wrapper:encode('SetExternal','SetXSeq3',
		      #'SetXSeq3'{bool = true, 
				  int = 66,
				  seq = #'XSeq1'{bool1 = true, 
						 int1 = 77,
						 seq1 = #'XSeqIn'{boolIn = false, 
								  intIn = 88}}}),
    ?line {ok,{'SetXSeq3',true,66,{'XSeq1',true,77,{'XSeqIn',false,88}}}} = 
	asn1_wrapper:decode('SetExternal','SetXSeq3',lists:flatten(Bytes43)),


    

    ok.


