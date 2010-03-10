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
-module(testSetTypeRefSet).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Set1',{bool1, int1, set1}).
-record('Set2',{set2, bool2, int2}).
-record('Set3',{bool3, set3, int3}).
-record('Set4',{set41, set42, set43}).
-record('SetIn',{boolIn, intIn}).
-record('SetS1',{boolS1, intS1, setS1}).
-record('SetS1_setS1',{boolIn, intIn}).
-record('SetS2',{setS2, boolS2, intS2}).
-record('SetS2_setS2',{boolIn, intIn}).
-record('SetS3',{boolS3, setS3, intS3}).
-record('SetS3_setS3',{boolIn, intIn}).
-record('SetSTag',{setS1, setS2, setS3}).
-record('SetSTag_setS1',{b1, i1}).
-record('SetSTag_setS2',{b2, i2}).
-record('SetSTag_setS3',{b3, i3}).
-record('SetTRset',{setSet, setSetI, setSetE, 'setSet-I', 'setSetI-I', 'setSetE-I', 'setSet-E', 'setSetI-E', 'setSetE-E'}).
-record('SetSet',{setInt, setOs}).
-record('SetSetImp',{setInt, setOs}).
-record('SetSetExp',{setInt, setOs}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetTypeRefSet",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetTypeRefSet','Set1',#'Set1'{bool1 = true,
						     int1 = 15,
						     set1 = #'SetIn'{boolIn = true,
								     intIn = 66}}), 
    ?line {ok,{'Set1',true,15,{'SetIn',true,66}}} = 
	asn1_wrapper:decode('SetTypeRefSet','Set1',lists:flatten(Bytes11)),
    
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SetTypeRefSet','Set2',#'Set2'{set2 = #'SetIn'{boolIn = true,
								     intIn = 66},
						     bool2 = true,
						     int2 = 15}),    
    ?line {ok,{'Set2',{'SetIn',true,66},true,15}} = 
	asn1_wrapper:decode('SetTypeRefSet','Set2',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SetTypeRefSet','Set3',#'Set3'{bool3 = true,
						     set3 = #'SetIn'{boolIn = true,
								     intIn = 66},
						     int3 = 15}),    
    ?line {ok,{'Set3',true,{'SetIn',true,66},15}} = 
	asn1_wrapper:decode('SetTypeRefSet','Set3',lists:flatten(Bytes13)),
    
    
    
    ?line {ok,Bytes14} = 
	asn1_wrapper:encode('SetTypeRefSet','Set4',#'Set4'{set41 = #'SetIn'{boolIn = true,
								      intIn = 66},
						     set42 = #'SetIn'{boolIn = true,
								      intIn = 66},
						     set43 = #'SetIn'{boolIn = true,
								      intIn = 66}}),    
    ?line {ok,{'Set4',{'SetIn',true,66},{'SetIn',true,66},{'SetIn',true,66}}} = 
	asn1_wrapper:decode('SetTypeRefSet','Set4',lists:flatten(Bytes14)),
    
    
    
    
    
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetTypeRefSet','SetS1',#'SetS1'{boolS1 = true,
						       intS1 = 15,
						       setS1 = #'SetS1_setS1'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SetS1',true,15,{'SetS1_setS1',true,66}}} = 
	asn1_wrapper:decode('SetTypeRefSet','SetS1',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SetTypeRefSet','SetS2',#'SetS2'{setS2 = #'SetS2_setS2'{boolIn = true,
									      intIn = 66},
						       boolS2 = true,
						       intS2 = 15}),    
    ?line {ok,{'SetS2',{'SetS2_setS2',true,66},true,15}} = 
	asn1_wrapper:decode('SetTypeRefSet','SetS2',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SetTypeRefSet','SetS3',#'SetS3'{boolS3 = true,
						       setS3 = #'SetS3_setS3'{boolIn = true,
									      intIn = 66},
						       intS3 = 15}),    
    ?line {ok,{'SetS3',true,{'SetS3_setS3',true,66},15}} = 
	asn1_wrapper:decode('SetTypeRefSet','SetS3',lists:flatten(Bytes23)),
    
    
    
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetTypeRefSet','SetSTag',#'SetSTag'{setS1 = #'SetSTag_setS1'{b1 = true,
										    i1 = 11},
							   setS2 = #'SetSTag_setS2'{b2 = true,
										    i2 = 22},
							   setS3 = #'SetSTag_setS3'{b3 = true,
										    i3 = 33}}),    
    ?line {ok,{'SetSTag',{'SetSTag_setS1',true,11},
	       {'SetSTag_setS2',true,22},
	       {'SetSTag_setS3',true,33}}} = 
	asn1_wrapper:decode('SetTypeRefSet','SetSTag',lists:flatten(Bytes31)),
    
    
    
    
    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SetTypeRefSet','SetTRset',
		      #'SetTRset'{'setSet' = #'SetSet'{setOs = "A1",
						       setInt = 2},
				  'setSetI' = #'SetSet'{setOs = "A2",
							setInt = 2},
				  'setSetE' = #'SetSet'{setOs = "A3",
							setInt = 2},
				  'setSet-I' = #'SetSetImp'{setOs = "A4",
							    setInt = 2},
				  'setSetI-I' = #'SetSetImp'{setOs = "A5",
							     setInt = 2},
				  'setSetE-I' = #'SetSetImp'{setOs = "A6",
							     setInt = 2},
				  'setSet-E' = #'SetSetExp'{setOs = "A7",
							    setInt = 2},
				  'setSetI-E' = #'SetSetExp'{setOs = "A8",
							     setInt = 2},
				  'setSetE-E' = #'SetSetExp'{setOs = "A9",
							     setInt = 2}}),
    ?line {ok,{'SetTRset',{'SetSet',2,"A1"},
      {'SetSet',2,"A2"},
      {'SetSet',2,"A3"},
      {'SetSetImp',2,"A4"},
      {'SetSetImp',2,"A5"},
      {'SetSetImp',2,"A6"},
      {'SetSetExp',2,"A7"},
      {'SetSetExp',2,"A8"},
      {'SetSetExp',2,"A9"}}} = 
	asn1_wrapper:decode('SetTypeRefSet','SetTRset',lists:flatten(Bytes41)),
    
    ok.
