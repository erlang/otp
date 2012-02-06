%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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

-export([compile/2,testobj/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config, Options) ->
    asn1_test_lib:compile("RANAP", Config, Options),
    DataDir = ?config(data_dir,Config),
    CaseDir = ?config(case_dir,Config),
    {ok,testobj} = compile:file(filename:join(DataDir,"testobj"),[{i,CaseDir},{outdir,CaseDir}]++Options),
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
