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
-module(testMvrasn6).

-export([compile/2]).
-export([main/0]).

-include_lib("test_server/include/test_server.hrl").

compile(Config,Rules) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line Options = [Rules,{outdir,OutDir}],
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-21-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-20-6",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-19-6",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-15-6",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-18-6",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-14-6",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-11-6",Options).

    
main() ->
    ok.

