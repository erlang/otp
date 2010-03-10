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
-module(testSetTypeRefPrim).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SetTR',{octStr, octStrI, octStrE, 'octStr-I', 'octStrI-I', 'octStrE-I', 'octStr-E', 'octStrI-E', 'octStrE-E'}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetTypeRefPrim",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->


    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetTypeRefPrim','SetTR',#'SetTR'{'octStr' = "A string 1",
						       'octStrI' = "A string 2",
						       'octStrE' = "A string 3",
						       'octStr-I' = "A string 4",
						       'octStrI-I' = "A string 5",
						       'octStrE-I' = "A string 6",
						       'octStr-E' = "A string 7",
						       'octStrI-E' = "A string 8",
						       'octStrE-E' = "A string 9"}),
    ?line {ok,{'SetTR',"A string 1","A string 2","A string 3","A string 4","A string 5","A string 6","A string 7","A string 8","A string 9"}} = 
	asn1_wrapper:decode('SetTypeRefPrim','SetTR',lists:flatten(Bytes11)),



    ok.
