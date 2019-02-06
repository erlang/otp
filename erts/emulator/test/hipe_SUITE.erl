%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2017. All Rights Reserved.
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
-export([all/0
	,t_copy_literals/1
	,t_purge/1
        ,t_trycatch/1
	]).

all() ->
    case erlang:system_info(hipe_architecture) of
	undefined -> {skip, "HiPE is disabled"};
	_ -> [t_copy_literals
	     ,t_purge
             ,t_trycatch
	     ]
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

    %% Give the literal collector some time to work...
    receive after 2000 -> ok end,

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

t_purge(doc) -> "Checks that native code is properly found and purged";
t_purge(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Priv = proplists:get_value(priv_dir, Config),
    SrcFile = filename:join(Data, "ref_cell"),
    BeamFile = filename:join(Priv, "ref_cell"),
    {ok,ref_cell} = c:c(SrcFile, [{outdir,Priv},native]),
    true = code:is_module_native(ref_cell),

    PA = ref_cell:start_link(),

    %% Unload, PA should still be running
    true = erlang:delete_module(ref_cell),
    %% Can't use ref_cel:call/2, it's in old code!
    call(PA, {put_res_of, fun()-> hej end}),
    hej = call(PA, get),

    %% Load same module again
    code:load_abs(BeamFile),
    true = code:is_module_native(ref_cell),
    PB = ref_cell:start_link(),

    %% Purge old code, PA should be killed, PB should survive
    unlink(PA),
    ARef = monitor(process, PA),
    true = erlang:purge_module(ref_cell),
    receive {'DOWN', ARef, process, PA, killed} -> ok
    after 1 -> ct:fail("PA was not killed")
    end,

    %% Unload, PB should still be running
    true = erlang:delete_module(ref_cell),
    call(PB, {put_res_of, fun()-> svejs end}),
    svejs = call(PB, get),

    unlink(PB),
    BRef = monitor(process, PB),
    true = erlang:purge_module(ref_cell),
    receive {'DOWN', BRef, process, PB, killed} -> ok
    after 1 -> ct:fail("PB was not killed")
    end,

    ok.

call(Pid, Call) ->
    Pid ! {Call, self()},
    receive {Pid, Res} -> Res end.

t_trycatch(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = ["trycatch_1.erl","trycatch_2.erl","trycatch_3.erl"],
    Sources0 = [filename:join(DataDir, Src) || Src <- Files],
    Sources = trycatch_combine(Sources0),
    t_trycatch_1(Sources).

t_trycatch_1([S|Ss]) ->
    io:format("~p", [S]),
    compile_and_load(S),
    call_trycatch(try_catch),
    call_trycatch(plain_catch),
    io:nl(),
    t_trycatch_1(Ss);
t_trycatch_1([]) ->
    ok.

trycatch_combine([N|Ns]) ->
    Combined = trycatch_combine(Ns),
    lists:append([[[{N,[]}|C],[{N,[native]},C]] || C <- Combined]);
trycatch_combine([]) ->
    [[]].

call_trycatch(Func) ->
    case do_call_trycatch(error, Func, {error,whatever}) of
        {error,whatever,[{trycatch_3,three,1,_}|_]} ->
            ok
    end,
    case do_call_trycatch(error, Func, fc) of
        {error,function_clause,[{trycatch_3,three,[fc],_}|_]} ->
            ok;
        {error,function_clause,[{trycatch_3,three,1,_}|_]} ->
            ok
    end,
    case do_call_trycatch(throw, Func, {throw,{a,b}}) of
        {throw,{a,b},[{trycatch_3,three,1,_}|_]} ->
            ok
    end,
    case do_call_trycatch(exit, Func, {exit,{a,b,c}}) of
        {exit,{a,b,c},[{trycatch_3,three,1,_}|_]} ->
            ok
    end,
    ok.

do_call_trycatch(_Class, try_catch, Argument) ->
    trycatch_1:one_try_catch(Argument);
do_call_trycatch(error, plain_catch, Argument) ->
    {{'EXIT',{Reason,Stk}},Stk} = trycatch_1:one_plain_catch(Argument),
    {error,Reason,Stk};
do_call_trycatch(throw, plain_catch, Argument) ->
    {Reason,Stk} = trycatch_1:one_plain_catch(Argument),
    {throw,Reason,Stk};
do_call_trycatch(exit, plain_catch, Argument) ->
    {{'EXIT',Reason},Stk} = trycatch_1:one_plain_catch(Argument),
    {exit,Reason,Stk}.

compile_and_load(Sources) ->
    _ = [begin
             {ok,Mod,Bin} = compile:file(Src, [binary,report|Opts]),
             code:purge(Mod),
             code:delete(Mod),
             code:purge(Mod),
             {module,Mod} = code:load_binary(Mod, atom_to_list(Mod), Bin)
         end || {Src,Opts} <- Sources],
    ok.
