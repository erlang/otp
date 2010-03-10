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
-module(testSetOf).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Set1',{bool1, int1, set1 = asn1_DEFAULT}).
-record('Set2',{set2 = asn1_DEFAULT, bool2, int2}).
-record('Set3',{bool3, set3 = asn1_DEFAULT, int3}).
-record('Set4',{set41 = asn1_DEFAULT, set42 = asn1_DEFAULT, set43 = asn1_DEFAULT}).
-record('SetIn',{boolIn, intIn}).
%-record('SetCho',{bool1, int1, set1 = asn1_DEFAULT}).
%-record('SetChoInline',{bool1, int1, set1 = asn1_DEFAULT}).
%-record('SetChoOfInline_SETOF',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetEmp',{set1}).
-record('Empty',{}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetOf",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetOf','Set1',#'Set1'{bool1 = true,
					     int1 = 17}),
    ?line {ok,{'Set1',true,17,[]}} = 
	asn1_wrapper:decode('SetOf','Set1',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SetOf','Set1',#'Set1'{bool1 = true,
					     int1 = 17,
					     set1 = [#'SetIn'{boolIn = true,
							      intIn = 25}]}),
    ?line {ok,{'Set1',true,17,[{'SetIn',true,25}]}} = 
	asn1_wrapper:decode('SetOf','Set1',lists:flatten(Bytes12)),
    
    
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SetOf','Set1',#'Set1'{bool1 = true,
					     int1 = 17,
					     set1 = [#'SetIn'{boolIn = true,
							      intIn = 25},
						     #'SetIn'{boolIn = false,
							      intIn = 125},
						     #'SetIn'{boolIn = false,
							      intIn = 225}]}),
    ?line {ok,{'Set1',true,17,[{'SetIn',true,25},{'SetIn',false,125},{'SetIn',false,225}]}} = 
	asn1_wrapper:decode('SetOf','Set1',lists:flatten(Bytes13)),






    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetOf','Set2',#'Set2'{bool2 = true,
					     int2 = 17}),
    
    ?line {ok,{'Set2',[],true,17}} = 
	asn1_wrapper:decode('SetOf','Set2',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SetOf','Set2',#'Set2'{bool2 = true,
					     int2 = 17,
					     set2 = [#'SetIn'{boolIn = true,
							      intIn = 25}]}),
    ?line {ok,{'Set2',[{'SetIn',true,25}],true,17}} = 
	asn1_wrapper:decode('SetOf','Set2',lists:flatten(Bytes22)),
    
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SetOf','Set2',#'Set2'{bool2 = true,
					     int2 = 17,
					     set2 = [#'SetIn'{boolIn = true,
							      intIn = 25},
						     #'SetIn'{boolIn = false,
							      intIn = 125},
						     #'SetIn'{boolIn = false,
							      intIn = 225}]}),
    ?line {ok,{'Set2',[{'SetIn',true,25},{'SetIn',false,125},{'SetIn',false,225}],true,17}} = 
	asn1_wrapper:decode('SetOf','Set2',lists:flatten(Bytes23)),
    
    



    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetOf','Set3',#'Set3'{bool3 = true,
					     int3 = 17}),
    ?line {ok,{'Set3',true,[],17}} = 
	asn1_wrapper:decode('SetOf','Set3',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SetOf','Set3',#'Set3'{bool3 = true,
					     int3 = 17,
					     set3 = [#'SetIn'{boolIn = true,
							      intIn = 25}]}),
    ?line {ok,{'Set3',true,[{'SetIn',true,25}],17}} = 
	asn1_wrapper:decode('SetOf','Set3',lists:flatten(Bytes32)),
    
    
    ?line {ok,Bytes33} = 
	asn1_wrapper:encode('SetOf','Set3',#'Set3'{bool3 = true,
					     int3 = 17,
					     set3 = [#'SetIn'{boolIn = true,
							      intIn = 25},
						     #'SetIn'{boolIn = false,
							      intIn = 125},
						     #'SetIn'{boolIn = false,
							      intIn = 225}]}),
    ?line {ok,{'Set3',true,[{'SetIn',true,25},{'SetIn',false,125},{'SetIn',false,225}],17}} = 
	asn1_wrapper:decode('SetOf','Set3',lists:flatten(Bytes33)),
    
  



  
    
    ?line {ok,Bytes41} = asn1_wrapper:encode('SetOf','Set4',#'Set4'{}),
    ?line {ok,{'Set4',[],[],[]}} = asn1_wrapper:decode('SetOf','Set4',lists:flatten(Bytes41)),
        
    
    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SetOf','Set4',#'Set4'{set41 = [#'SetIn'{boolIn = true,
							       intIn = 25}]}),
    ?line {ok,{'Set4',[{'SetIn',true,25}],[],[]}} = 
	asn1_wrapper:decode('SetOf','Set4',lists:flatten(Bytes42)),
    
    
    ?line {ok,Bytes43} = 
	asn1_wrapper:encode('SetOf','Set4',#'Set4'{set41 = [#'SetIn'{boolIn = true,
							       intIn = 25},
						      #'SetIn'{boolIn = false,
							       intIn = 125},
						      #'SetIn'{boolIn = false,
							       intIn = 225}]}),
    ?line {ok,{'Set4',[{'SetIn',true,25},{'SetIn',false,125},{'SetIn',false,225}],[],[]}} = 
	asn1_wrapper:decode('SetOf','Set4',lists:flatten(Bytes43)),
    
    
    ?line {ok,Bytes44} = 
	asn1_wrapper:encode('SetOf','Set4',#'Set4'{set42 = [#'SetIn'{boolIn = true,
								       intIn = 25}]}),
    ?line {ok,{'Set4',[],[{'SetIn',true,25}],[]}} = 
	asn1_wrapper:decode('SetOf','Set4',lists:flatten(Bytes44)),
    
    
    ?line {ok,Bytes45} = 
	asn1_wrapper:encode('SetOf','Set4',#'Set4'{set42 = [#'SetIn'{boolIn = true,
							       intIn = 25},
						      #'SetIn'{boolIn = false,
							       intIn = 125},
						      #'SetIn'{boolIn = false,
							       intIn = 225}]}),
    ?line {ok,{'Set4',[],[{'SetIn',true,25},{'SetIn',false,125},{'SetIn',false,225}],[]}} = 
	asn1_wrapper:decode('SetOf','Set4',lists:flatten(Bytes45)),
    
    
    ?line {ok,Bytes46} = 
	asn1_wrapper:encode('SetOf','Set4',#'Set4'{set43 = [#'SetIn'{boolIn = true,
							       intIn = 25}]}),
    ?line {ok,{'Set4',[],[],[{'SetIn',true,25}]}} = 
	asn1_wrapper:decode('SetOf','Set4',lists:flatten(Bytes46)),
    
    
    ?line {ok,Bytes47} = 
	asn1_wrapper:encode('SetOf','Set4',#'Set4'{set43 = [#'SetIn'{boolIn = true,
							       intIn = 25},
						      #'SetIn'{boolIn = false,
							       intIn = 125},
						      #'SetIn'{boolIn = false,
							       intIn = 225}]}),
    ?line {ok,{'Set4',[],[],[{'SetIn',true,25},{'SetIn',false,125},{'SetIn',false,225}]}} = 
	asn1_wrapper:decode('SetOf','Set4',lists:flatten(Bytes47)),
    



    ?line {ok,Bytes51} = asn1_wrapper:encode('SetOf','SetOs',["First","Second","Third"]),
    ?line {ok,["First","Second","Third"]} = 
	asn1_wrapper:decode('SetOf','SetOs',lists:flatten(Bytes51)),
     
    ?line {ok,Bytes52} = asn1_wrapper:encode('SetOf','SetOsImp',["First","Second","Third"]),
    ?line {ok,["First","Second","Third"]} = 
	asn1_wrapper:decode('SetOf','SetOsImp',lists:flatten(Bytes52)),
     
    ?line {ok,Bytes53} = asn1_wrapper:encode('SetOf','SetOsExp',["First","Second","Third"]),
    ?line {ok,["First","Second","Third"]} = 
	asn1_wrapper:decode('SetOf','SetOsExp',lists:flatten(Bytes53)),
     





    
    ?line {ok,Bytes71} = asn1_wrapper:encode('SetOf','SetEmp',#'SetEmp'{set1 = [#'Empty'{}]}),
    ?line {ok,{'SetEmp',[{'Empty'}]}} = asn1_wrapper:decode('SetOf','SetEmp',lists:flatten(Bytes71)),
     
    ok.

