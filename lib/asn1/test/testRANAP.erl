%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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

-module(testRANAP).

-export([compile/3,testobj/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Erule,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),

    ?line ok = asn1ct:compile(filename:join(DataDir,"RANAP"),[Erule,{outdir,OutDir}]++Options),
    ?line {ok,testobj} = compile:file(filename:join(DataDir,"testobj"),[{i,OutDir},{outdir,OutDir}]++Options),
    ok.

testobj(_Erule) ->
    ?line ok = testobj:run_com_id(),
    ?line ok = testobj:run_dir_tsf_2cn(),
    ?line ok = testobj:run_dir_tsf_2rnc(),
    ?line ok = testobj:run_init_ue(),
    ?line ok = testobj:run_iu_rel_cmd(),
    ?line ok = testobj:run_iu_rel_cmp(),            
    ?line ok = testobj:run_rab_ass_rsp_delete(),
    ?line ok = testobj:run_rab_ass_rsp_setup(),
    ?line ok = testobj:run_rab_create(),            
    ?line ok = testobj:run_rab_rel(),
    ?line ok = testobj:run_reset(),
    ?line ok = testobj:run_reset_res(),             
    ?line ok = testobj:run_sm_cmd(),
    ?line ok = testobj:run_sm_cmp(),
    ?line ok = testobj:run_sm_rej().
