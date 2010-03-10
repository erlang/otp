%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
-module(choice_extension).

-export([run/0, compile/3]).

-include_lib("test_server/include/test_server.hrl").

compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoExtension",[Rules,{outdir,OutDir}]++Options).

run() ->
    Val = {str,"abc"},
    ?line {ok,B} = asn1_wrapper:encode('ChoExtension','ChoExt4',Val),
    ?line {ok,Val} = asn1_wrapper:decode('ChoExtension','ChoExt4',lists:flatten(B)),
    ok.
