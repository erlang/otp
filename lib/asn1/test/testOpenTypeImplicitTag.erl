%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
-module(testOpenTypeImplicitTag).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "OpenTypeImplicitTag",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->

    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('OpenTypeImplicitTag','Seq',
			    {'Seq',[1,1,255],[1,1,255],12,[1,1,255]}),
    ?line {ok,{'Seq',_,_,12,_}} = 
	asn1_wrapper:decode('OpenTypeImplicitTag','Seq',
			    lists:flatten(Bytes1)),

    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('OpenTypeImplicitTag','Seq',
			    {'Seq',[1,1,255],asn1_NOVALUE,12,[1,1,255]}),
    ?line {ok,{'Seq',_,asn1_NOVALUE,12,_}} = 
	asn1_wrapper:decode('OpenTypeImplicitTag','Seq',
			    lists:flatten(Bytes2)),
    ok.
