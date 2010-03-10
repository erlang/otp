%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
-module(testParamBasic).

-export([compile/3]).
-export([compile_der/2]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('T11',{number, string=asn1_DEFAULT}).
-record('T12',{number, string=asn1_DEFAULT}).
-record('T21',{number, string}).
-record('T22',{number, string}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ParamBasic",
			      [Rules,{outdir,OutDir}]++Options).

compile_der(Config,Rules) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ParamBasic",
			      [der,Rules,{outdir,OutDir}]).

main(Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('ParamBasic','T11',
			    #'T11'{number = 11,
				   string = "hello"}),
    ?line {ok,{'T11',11,"hello"}} =
	asn1_wrapper:decode('ParamBasic','T11',Bytes11),
	    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('ParamBasic','T12',
			    #'T12'{number = 11,
				   string = [1,0,1,0,1]}),
    ?line {ok,{'T12',11,[1,0,1,0,1]}} =
	asn1_wrapper:decode('ParamBasic','T12',Bytes12),
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('ParamBasic','T21',
			    #'T21'{number = 11,
				   string = "hello"}),
    ?line {ok,{'T21',11,"hello"}} =
	asn1_wrapper:decode('ParamBasic','T21',Bytes13),
	    
    ?line {ok,Bytes14} = 
	asn1_wrapper:encode('ParamBasic','T22',
			    #'T22'{number = 11,
				   string = [1,0,1,0,1]}),
    ?line {ok,{'T22',11,[1,0,1,0,1]}} =
	asn1_wrapper:decode('ParamBasic','T22',Bytes14),

    case Rules of
	der ->

	    ?line {ok,[48,3,128,1,11]} = 
		asn1_wrapper:encode('ParamBasic','T11',
				    #'T11'{number = 11,
					   string = "hej"}),
	    ?line {ok,{'T11',11,"hej"}} =
		asn1_wrapper:decode('ParamBasic','T11',[48,3,128,1,11]),
	    
	    ?line {ok,[48,3,128,1,11]} = 
		asn1_wrapper:encode('ParamBasic','T12',
				    #'T12'{number = 11,
					   string = [1,0,1,0]}),
	    
	    ?line {ok,{'T12',11,[1,0,1,0]}} =
		asn1_wrapper:decode('ParamBasic','T12',[48,3,128,1,11]);
	_ -> ok
    end,
	    
    ok.
