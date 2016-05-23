%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(hipe_SUITE).
-export([all/0, t_copy_literals/1]).

all() ->
    case erlang:system_info(hipe_architecture) of
	undefined -> {skip, "HiPE is disabled"};
	_ -> [t_copy_literals]
    end.

t_copy_literals(doc) ->
    "Check that BEAM literals referenced from HiPE stack are copied by"
    " check_process_code";
t_copy_literals(Config) when is_list(Config) ->
    %% Compile the the ref_cell and literals modules.
    Data = proplists:get_value(data_dir, Config),
    Priv = proplists:get_value(priv_dir, Config),
    RefFile = filename:join(Data, "ref_cell"),
    {ok,ref_cell} = c:c(RefFile, [{outdir,Priv},native]),
    true = code:is_module_native(ref_cell),
    LitFile = filename:join(Data, "literals"),
    {ok,literals} = c:c(LitFile, [{outdir,Priv}]),

    %% store references to literals on HiPE stacks
    PA = ref_cell:start_link(),
    ref_cell:call(PA, {put_res_of, fun literals:a/0}),
    PB = ref_cell:start_link_deep(),
    ref_cell:call(PB, {put_res_of, fun literals:b/0}),

    %% purge the literals
    _ = (catch erlang:purge_module(literals)),
    true = erlang:delete_module(literals),
    true = erlang:purge_module(literals),

    %% check that the ex-literals are ok
    [a,b,c] = ref_cell:call(PA, get),
    {a,b,c} = ref_cell:call(PB, get),

    %% cleanup
    ref_cell:call(PA, done),
    ref_cell:call(PB, done),
    _ = (catch erlang:purge_module(ref_cell)),
    true = erlang:delete_module(ref_cell),
    true = erlang:purge_module(ref_cell),
    ok.
