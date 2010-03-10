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
-module(testChoRecursive).


-export([compile/3]).
-export([recursive/1]).

-include_lib("test_server/include/test_server.hrl").

-record('ChoRec_something',{a, b, c}).
-record('ChoRec2_something',{a, b, c}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoRecursive",[Rules,{outdir,OutDir}]++Options).



recursive(_Rules) ->
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('ChoRecursive','ChoRec',{'ChoRec',{something,
				      #'ChoRec_something'{a = 77,
							  b = "some octets here",
							  c = {'ChoRec',{nothing,'NULL'}}}}}),
    ?line {ok,{something,{'ChoRec_something',77,"some octets here",{nothing,'NULL'}}}} = 
	   asn1_wrapper:decode('ChoRecursive','ChoRec',lists:flatten(Bytes11)),
	   
	   
    ?line {ok,Bytes12} = asn1_wrapper:encode('ChoRecursive','ChoRec',{'ChoRec',{nothing,'NULL'}}),
    ?line {ok,{nothing,'NULL'}} = 
	asn1_wrapper:decode('ChoRecursive','ChoRec',lists:flatten(Bytes12)),
	   
	   
	   
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('ChoRecursive','ChoRec2',{'ChoRec2',
					       {something,
						#'ChoRec2_something'{a = 77,
								     b = "some octets here",
								     c = {'ChoRec2',
									  {nothing,'NULL'}}}}}),
    ?line {ok,{something,{'ChoRec2_something',77,"some octets here",{nothing,'NULL'}}}} = 
	   asn1_wrapper:decode('ChoRecursive','ChoRec2',lists:flatten(Bytes21)),
		  
		  
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('ChoRecursive','ChoRec2',{'ChoRec2',{nothing,'NULL'}}),
    ?line {ok,{nothing,'NULL'}} = 
	asn1_wrapper:decode('ChoRecursive','ChoRec2',lists:flatten(Bytes22)),
		  
		  
		  
		  
    ok.
