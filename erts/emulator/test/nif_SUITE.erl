%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(nif_SUITE).

%%-define(line_trace,true).
-define(CHECK(Exp,Got), Exp = check(Exp,Got,?LINE)).
%%-define(CHECK(Exp,Got), Exp = Got).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0, groups/0,
         init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2,
         basic/1, reload_error/1, upgrade/1, heap_frag/1,
         t_on_load/1,
         select/1, select_steal/1,
         monitor_process_a/1,
         monitor_process_b/1,
         monitor_process_c/1,
         monitor_process_d/1,
         demonitor_process/1,
         monitor_frenzy/1,
         hipe/1,
	 types/1, many_args/1, binaries/1, get_string/1, get_atom/1,
	 maps/1,
	 api_macros/1,
	 from_array/1, iolist_as_binary/1, resource/1, resource_binary/1,
	 resource_takeover/1,
	 threading/1, send/1, send2/1, send3/1, send_threaded/1, neg/1,
	 is_checks/1,
	 get_length/1, make_atom/1, make_string/1, reverse_list_test/1,
	 otp_9828/1,
	 otp_9668/1, consume_timeslice/1, nif_schedule/1,
	 nif_exception/1, call_nif_exception/1,
	 nif_nan_and_inf/1, nif_atom_too_long/1,
	 nif_monotonic_time/1, nif_time_offset/1, nif_convert_time_unit/1,
         nif_now_time/1, nif_cpu_time/1, nif_unique_integer/1,
         nif_is_process_alive/1, nif_is_port_alive/1,
         nif_term_to_binary/1, nif_binary_to_term/1,
         nif_port_command/1,
         nif_snprintf/1,
         nif_internal_hash/1,
         nif_internal_hash_salted/1,
         nif_phash2/1,
         nif_whereis/1, nif_whereis_parallel/1,
         nif_whereis_threaded/1, nif_whereis_proxy/1,
         nif_ioq/1
	]).

-export([many_args_100/100]).

-define(nif_stub,nif_stub_error(?LINE)).

-define(is_resource, is_reference).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [basic]
        ++
    [{group, G} || G <- api_groups()]
        ++
    [reload_error, heap_frag, types, many_args,
     select, select_steal,
     {group, monitor},
     monitor_frenzy,
     hipe,
     binaries, get_string, get_atom, maps, api_macros, from_array,
     iolist_as_binary, resource, resource_binary,
     threading, send, send2, send3,
     send_threaded, neg, is_checks, get_length, make_atom,
     make_string,reverse_list_test,
     otp_9828,
     otp_9668, consume_timeslice,
     nif_schedule, nif_exception, nif_nan_and_inf, nif_atom_too_long,
     nif_monotonic_time, nif_time_offset, nif_convert_time_unit,
     nif_now_time, nif_cpu_time, nif_unique_integer,
     nif_is_process_alive, nif_is_port_alive,
     nif_term_to_binary, nif_binary_to_term,
     nif_port_command,
     nif_snprintf,
     nif_internal_hash,
     nif_internal_hash_salted,
     nif_phash2,
     nif_whereis, nif_whereis_parallel, nif_whereis_threaded,
     nif_ioq].

groups() ->
    [{G, [], api_repeaters()} || G <- api_groups()]
        ++
    [{monitor, [], [monitor_process_a,
                    monitor_process_b,
                    monitor_process_c,
                    monitor_process_d,
                    demonitor_process]}].


api_groups() -> [api_latest, api_2_4, api_2_0].

api_repeaters() -> [upgrade, resource_takeover, t_on_load].

init_per_group(api_2_4, Config) ->
    [{nif_api_version, ".2_4"} | Config];
init_per_group(api_2_0, Config) ->
    case {os:type(),erlang:system_info({wordsize, internal})} of
        {{win32,_}, 8} ->
            %% ERL_NIF_TERM was declared as 32-bit 'long' until 2.3
            {skip, "API 2.0 buggy on Windows 64-bit"};
        _ ->
            [{nif_api_version, ".2_0"} | Config]
    end;
init_per_group(_, Config) -> Config.

end_per_group(_,_) -> ok.

init_per_testcase(t_on_load, Config) ->
    ets:new(nif_SUITE, [named_table]),
    Config;
init_per_testcase(hipe, Config) ->
    case erlang:system_info(hipe_architecture) of
	undefined -> {skip, "HiPE is disabled"};
	_ -> Config
    end;
init_per_testcase(nif_whereis_threaded, Config) ->
    case erlang:system_info(threads) of
        true -> Config;
        false -> {skip, "No thread support"}
    end;
init_per_testcase(Select, Config) when Select =:= select;
                                       Select =:= select_steal ->
    case os:type() of
        {win32,_} ->
            {skip, "Test not yet implemented for windows"};
        _ ->
            Config
    end;
init_per_testcase(_Case, Config) ->
    %% Clear any resource dtor data before test starts in case another tc
    %% left it in a bad state
    catch last_resource_dtor_call(),
    Config.

end_per_testcase(t_on_load, _Config) ->
    ets:delete(nif_SUITE),
    testcase_cleanup();
end_per_testcase(_Func, _Config) ->
    testcase_cleanup().

testcase_cleanup() ->
    P1 = code:purge(nif_mod),
    Del = code:delete(nif_mod),
    P2 = code:purge(nif_mod),
    io:format("fin purged=~p, deleted=~p and then purged=~p\n",[P1,Del,P2]).

%% Basic smoke test of load_nif and a simple NIF call
basic(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    true = (lib_version() =/= undefined),
    [{load,1,1,101},{lib_version,1,2,102}] = call_history(),
    [] = call_history(),
    true = lists:member(?MODULE, erlang:system_info(taints)),
    ok.

%% Test old reload feature now always fails
reload_error(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),

    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "nif_mod"),
    {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ok = nif_mod:load_nif_lib(Config, 1),

    hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),    
        
    {error, {reload, _}} = nif_mod:load_nif_lib(Config, 2),
    1 = nif_mod:lib_version(),
    [{lib_version,1,3,103}] = nif_mod_call_history(),

    {error, {reload, _}} = nif_mod:load_nif_lib(Config, 1),
    1 = nif_mod:lib_version(),
    [{lib_version,1,4,104}] = nif_mod_call_history(),

    true = erlang:delete_module(nif_mod),
    [] = nif_mod_call_history(),    

    %%false= check_process_code(Pid, nif_mod),
    true = erlang:purge_module(nif_mod),
    [{unload,1,5,105}] = nif_mod_call_history(),

    true = lists:member(?MODULE, erlang:system_info(taints)),
    true = lists:member(nif_mod, erlang:system_info(taints)),
    verify_tmpmem(TmpMem),
    ok.

%% Test upgrade callback in nif lib
upgrade(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),

    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "nif_mod"),
    {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    ok = nif_mod:load_nif_lib(Config, 1),
    {Pid,MRef} = nif_mod:start(),
    1 = call(Pid,lib_version),

    hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    [{load,1,1,101},{lib_version,1,2,102},{get_priv_data_ptr,1,3,103}] = nif_mod_call_history(),    
        
    %% Module upgrade with same lib-version
    {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    undefined = nif_mod:lib_version(),
    1 = call(Pid,lib_version),
    [{lib_version,1,4,104}] = nif_mod_call_history(),

    ok = nif_mod:load_nif_lib(Config, 1),
    1 = nif_mod:lib_version(),
    [{upgrade,1,5,105},{lib_version,1,6,106}] = nif_mod_call_history(),

    upgraded = call(Pid,upgrade),
    false = check_process_code(Pid, nif_mod),
    true = erlang:purge_module(nif_mod),
    [{unload,1,7,107}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    [{lib_version,1,8,108}] = nif_mod_call_history(),

    true = erlang:delete_module(nif_mod),
    [] = nif_mod_call_history(),    

    %% Repeat upgrade again but from old (deleted) instance
    {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    undefined = nif_mod:lib_version(),
    1 = call(Pid,lib_version),
    [{lib_version,1,9,109}] = nif_mod_call_history(),

    ok = nif_mod:load_nif_lib(Config, 1),
    1 = nif_mod:lib_version(),
    [{upgrade,1,10,110},{lib_version,1,11,111}] = nif_mod_call_history(),

    upgraded = call(Pid,upgrade),
    false = check_process_code(Pid, nif_mod),
    true = erlang:purge_module(nif_mod),
    [{unload,1,12,112}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    [{lib_version,1,13,113}] = nif_mod_call_history(),

    true = erlang:delete_module(nif_mod),
    [] = nif_mod_call_history(),


    Pid ! die,
    {'DOWN', MRef, process, Pid, normal} = receive_any(),
    false = check_process_code(Pid, nif_mod),
    true = erlang:purge_module(nif_mod),
    [{unload,1,14,114}] = nif_mod_call_history(),

    %% Module upgrade with different lib version
    {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    undefined = nif_mod:lib_version(),
    {Pid2,MRef2} = nif_mod:start(),
    undefined = call(Pid2,lib_version),

    ok = nif_mod:load_nif_lib(Config, 1),
    hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    1 = call(Pid2,lib_version),
    [{load,1,1,101},{get_priv_data_ptr,1,2,102},{lib_version,1,3,103}] = nif_mod_call_history(),

    {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    undefined = nif_mod:lib_version(),
    [] = nif_mod_call_history(),
    1 = call(Pid2,lib_version),
    [{lib_version,1,4,104}] = nif_mod_call_history(),

    ok = nif_mod:load_nif_lib(Config, 2),
    2 = nif_mod:lib_version(),
    [{upgrade,2,1,201},{lib_version,2,2,202}] = nif_mod_call_history(),

    1 = call(Pid2,lib_version),
    [{lib_version,1,5,105}] = nif_mod_call_history(),

    upgraded = call(Pid2,upgrade),
    false = check_process_code(Pid2, nif_mod),
    true = erlang:purge_module(nif_mod),
    [{unload,1,6,106}] = nif_mod_call_history(),

    2 = nif_mod:lib_version(),
    [{lib_version,2,3,203}] = nif_mod_call_history(),

    true = erlang:delete_module(nif_mod),
    [] = nif_mod_call_history(),    


    %% Reverse upgrade but from old (deleted) instance
    {module,nif_mod} = erlang:load_module(nif_mod,Bin),
    undefined = nif_mod:lib_version(),
    [] = nif_mod_call_history(),
    2 = call(Pid2,lib_version),
    [{lib_version,2,4,204}] = nif_mod_call_history(),

    ok = nif_mod:load_nif_lib(Config, 1),
    1 = nif_mod:lib_version(),
    [{upgrade,1,1,101},{lib_version,1,2,102}] = nif_mod_call_history(),

    2 = call(Pid2,lib_version),
    [{lib_version,2,5,205}] = nif_mod_call_history(),

    upgraded = call(Pid2,upgrade),
    false = check_process_code(Pid2, nif_mod),
    true = erlang:purge_module(nif_mod),
    [{unload,2,6,206}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    [{lib_version,1,3,103}] = nif_mod_call_history(),

    true = erlang:delete_module(nif_mod),
    [] = nif_mod_call_history(),


    Pid2 ! die,
    {'DOWN', MRef2, process, Pid2, normal} = receive_any(),
    false= check_process_code(Pid2, nif_mod),
    true = erlang:purge_module(nif_mod),
    [{unload,1,4,104}] = nif_mod_call_history(),

    true = lists:member(?MODULE, erlang:system_info(taints)),
    true = lists:member(nif_mod, erlang:system_info(taints)),
    verify_tmpmem(TmpMem),
    ok.

%% Test loading/upgrade in on_load
t_on_load(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),

    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "nif_mod"),
    {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors,
                                           {d,'USE_ON_LOAD'}]),

    %% Use ETS to tell nif_mod:on_load what to do
    ets:insert(nif_SUITE, {data_dir, Data}),
    ets:insert(nif_SUITE, {lib_version, 1}),
    API = proplists:get_value(nif_api_version, Config, ""),
    ets:insert(nif_SUITE, {nif_api_version, API}),
    {module,nif_mod} = code:load_binary(nif_mod,File,Bin),
    hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),

    {Pid,MRef} = nif_mod:start(),
    1 = call(Pid,lib_version),
    [{lib_version,1,3,103}] = nif_mod_call_history(),

    %% Module upgrade with same lib-version
    {module,nif_mod} = code:load_binary(nif_mod,File,Bin),
    1 = nif_mod:lib_version(),
    1 = call(Pid,lib_version),
    [{upgrade,1,4,104},{lib_version,1,5,105},{lib_version,1,6,106}] = nif_mod_call_history(),

    upgraded = call(Pid,upgrade),
    false = check_process_code(Pid, nif_mod),
    true = code:soft_purge(nif_mod),
    [{unload,1,7,107}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    [{lib_version,1,8,108}] = nif_mod_call_history(),

    true = code:delete(nif_mod),
    [] = nif_mod_call_history(),

    %% Repeat upgrade again but from old (deleted) instance
    {module,nif_mod} = code:load_binary(nif_mod,File,Bin),
    [{upgrade,1,9,109}] = nif_mod_call_history(),
    1 = nif_mod:lib_version(),
    1 = call(Pid,lib_version),
    [{lib_version,1,10,110},{lib_version,1,11,111}] = nif_mod_call_history(),

    upgraded = call(Pid,upgrade),
    false = check_process_code(Pid, nif_mod),
    true = code:soft_purge(nif_mod),
    [{unload,1,12,112}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    [{lib_version,1,13,113}] = nif_mod_call_history(),

    true = code:delete(nif_mod),
    [] = nif_mod_call_history(),


    Pid ! die,
    {'DOWN', MRef, process, Pid, normal} = receive_any(),
    false = check_process_code(Pid, nif_mod),
    true = code:soft_purge(nif_mod),
    [{unload,1,14,114}] = nif_mod_call_history(),

    %% Module upgrade with different lib version
    {module,nif_mod} = code:load_binary(nif_mod,File,Bin),
    hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    {Pid2,MRef2} = nif_mod:start(),
    1 = call(Pid2,lib_version),
    [{lib_version,1,3,103},{lib_version,1,4,104}] = nif_mod_call_history(),

    true = ets:insert(nif_SUITE,{lib_version,2}),
    {module,nif_mod} = code:load_binary(nif_mod,File,Bin),
    [{upgrade,2,1,201}] = nif_mod_call_history(),

    2 = nif_mod:lib_version(),
    1 = call(Pid2,lib_version),
    [{lib_version,2,2,202},{lib_version,1,5,105}] = nif_mod_call_history(),

    upgraded = call(Pid2,upgrade),
    false = check_process_code(Pid2, nif_mod),
    true = code:soft_purge(nif_mod),
    [{unload,1,6,106}] = nif_mod_call_history(),

    2 = nif_mod:lib_version(),
    2 = call(Pid2,lib_version),
    [{lib_version,2,3,203},{lib_version,2,4,204}] = nif_mod_call_history(),

    true = code:delete(nif_mod),
    [] = nif_mod_call_history(),

    %% Reverse upgrade but from old (deleted) instance
    ets:insert(nif_SUITE,{lib_version,1}),
    {module,nif_mod} = code:load_binary(nif_mod,File,Bin),
    [{upgrade,1,1,101}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    2 = call(Pid2,lib_version),
    [{lib_version,1,2,102},{lib_version,2,5,205}] = nif_mod_call_history(),

    upgraded = call(Pid2,upgrade),
    false = check_process_code(Pid2, nif_mod),
    true = code:soft_purge(nif_mod),
    [{unload,2,6,206}] = nif_mod_call_history(),

    1 = nif_mod:lib_version(),
    [{lib_version,1,3,103}] = nif_mod_call_history(),

    true = code:delete(nif_mod),
    [] = nif_mod_call_history(),


    Pid2 ! die,
    {'DOWN', MRef2, process, Pid2, normal} = receive_any(),
    false= check_process_code(Pid2, nif_mod),
    true = code:soft_purge(nif_mod),
    [{unload,1,4,104}] = nif_mod_call_history(),

    true = lists:member(?MODULE, erlang:system_info(taints)),
    true = lists:member(nif_mod, erlang:system_info(taints)),
    verify_tmpmem(TmpMem),
    ok.

-define(ERL_NIF_SELECT_READ, (1 bsl 0)).
-define(ERL_NIF_SELECT_WRITE, (1 bsl 1)).
-define(ERL_NIF_SELECT_STOP, (1 bsl 2)).

-define(ERL_NIF_SELECT_STOP_CALLED, (1 bsl 0)).
-define(ERL_NIF_SELECT_STOP_SCHEDULED, (1 bsl 1)).
-define(ERL_NIF_SELECT_INVALID_EVENT, (1 bsl 2)).
-define(ERL_NIF_SELECT_FAILED, (1 bsl 3)).


select(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),

    Ref = make_ref(),
    Ref2 = make_ref(),
    {{R, R_ptr}, {W, W_ptr}} = pipe_nif(),
    ok = write_nif(W, <<"hej">>),
    <<"hej">> = read_nif(R, 3),

    %% Wait for read
    eagain = read_nif(R, 3),
    0 = select_nif(R,?ERL_NIF_SELECT_READ,R,null,Ref),
    [] = flush(0),
    ok = write_nif(W, <<"hej">>),
    [{select, R, Ref, ready_input}] = flush(),
    0 = select_nif(R,?ERL_NIF_SELECT_READ,R,self(),Ref2),
    [{select, R, Ref2, ready_input}] = flush(),
    Papa = self(),
    Pid = spawn_link(fun() ->
                             [{select, R, Ref, ready_input}] = flush(),
                             Papa ! {self(), done}
                     end),
    0 = select_nif(R,?ERL_NIF_SELECT_READ,R,Pid,Ref),
    {Pid, done} = receive_any(1000),
    <<"hej">> = read_nif(R, 3),

    %% Wait for write
    Written = write_full(W, $a),
    0 = select_nif(W,?ERL_NIF_SELECT_WRITE,W,self(),Ref),
    [] = flush(0),
    Written = read_nif(R,byte_size(Written)),
    [{select, W, Ref, ready_output}] = flush(),

    %% Close write and wait for EOF
    eagain = read_nif(R, 1),
    check_stop_ret(select_nif(W,?ERL_NIF_SELECT_STOP,W,null,Ref)),
    [{fd_resource_stop, W_ptr, _}] = flush(),
    {1, {W_ptr,_}} = last_fd_stop_call(),
    true = is_closed_nif(W),
    [] = flush(0),
    0 = select_nif(R,?ERL_NIF_SELECT_READ,R,self(),Ref),
    [{select, R, Ref, ready_input}] = flush(),
    eof = read_nif(R,1),

    check_stop_ret(select_nif(R,?ERL_NIF_SELECT_STOP,R,null,Ref)),
    [{fd_resource_stop, R_ptr, _}] = flush(),
    {1, {R_ptr,_}} = last_fd_stop_call(),
    true = is_closed_nif(R),

    select_2(Config).

select_2(Config) ->
    erlang:garbage_collect(),
    {_,_,2} = last_resource_dtor_call(),

    Ref1 = make_ref(),
    Ref2 = make_ref(),
    {{R, R_ptr}, {W, W_ptr}} = pipe_nif(),

    %% Change ref
    eagain = read_nif(R, 1),
    0 = select_nif(R,?ERL_NIF_SELECT_READ,R,null,Ref1),
    0 = select_nif(R,?ERL_NIF_SELECT_READ,R,self(),Ref2),

    [] = flush(0),
    ok = write_nif(W, <<"hej">>),
    [{select, R, Ref2, ready_input}] = flush(),
    <<"hej">> = read_nif(R, 3),

    %% Change pid
    eagain = read_nif(R, 1),
    0 = select_nif(R,?ERL_NIF_SELECT_READ,R,null,Ref1),
    Papa = self(),
    spawn_link(fun() ->
                       0 = select_nif(R,?ERL_NIF_SELECT_READ,R,null,Ref1),
                       [] = flush(0),
                       Papa ! sync,
                       [{select, R, Ref1, ready_input}] = flush(),
                       <<"hej">> = read_nif(R, 3),
                       Papa ! done
               end),
    sync = receive_any(),
    ok = write_nif(W, <<"hej">>),
    done = receive_any(),
    [] = flush(0),

    check_stop_ret(select_nif(R,?ERL_NIF_SELECT_STOP,R,null,Ref1)),
    [{fd_resource_stop, R_ptr, _}] = flush(),
    {1, {R_ptr,_}} = last_fd_stop_call(),
    true = is_closed_nif(R),

    %% Stop without previous read/write select
    ?ERL_NIF_SELECT_STOP_CALLED = select_nif(W,?ERL_NIF_SELECT_STOP,W,null,Ref1),
    [{fd_resource_stop, W_ptr, 1}] = flush(),
    {1, {W_ptr,1}} = last_fd_stop_call(),
    true = is_closed_nif(W),

    select_3(Config).

select_3(_Config) ->
    erlang:garbage_collect(),
    {_,_,2} = last_resource_dtor_call(),
    ok.

%% @doc The stealing child process for the select_steal test. Duplicates given
%% W/RFds and runs select on them to steal
select_steal_child_process(Parent, RFd) ->
    %% Duplicate the resource with the same FD
    {R2Fd, _R2Ptr} = dupe_resource_nif(RFd),
    Ref2 = make_ref(),

    %% Try to select from the child pid (steal from parent)
    ?assertEqual(0, select_nif(R2Fd, ?ERL_NIF_SELECT_READ, R2Fd, null, Ref2)),
    ?assertEqual([], flush(0)),
    ?assertEqual(eagain, read_nif(R2Fd, 1)),

    %% Check that now events arrive to this temporary process
    Parent ! {self(), stage1}, % signal parent to send the <<"stolen1">>

    %% Receive <<"stolen1">> via enif_select
    ?assertEqual(0, select_nif(R2Fd, ?ERL_NIF_SELECT_READ, R2Fd, null, Ref2)),
    ?assertMatch([{select, R2Fd, Ref2, ready_input}], flush()),
    ?assertEqual(<<"stolen1">>, read_nif(R2Fd, 7)),

    clear_select_nif(R2Fd),

    % do not do this here - stop_selecting(R2Fd, R2Rsrc, Ref2),
    Parent ! {self(), done}.

%% @doc Similar to select/1 test, make a double ended pipe. Then try to steal
%% the socket, see what happens.
select_steal(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),

    Ref = make_ref(),
    {{RFd, RPtr}, {WFd, WPtr}} = pipe_nif(),

    %% Bind the socket to current pid in enif_select
    ?assertEqual(0, select_nif(RFd, ?ERL_NIF_SELECT_READ, RFd, null, Ref)),
    ?assertEqual([], flush(0)),

    %% Spawn a process and do some stealing
    Parent = self(),
    Pid = spawn_link(fun() -> select_steal_child_process(Parent, RFd) end),

    %% Signal from the child to send the first message
    {Pid, stage1} = receive_any(),
    ?assertEqual(ok, write_nif(WFd, <<"stolen1">>)),

    ?assertMatch([{Pid, done}], flush(1)),  % synchronize with the child

    %% Try to select from the parent pid (steal back)
    ?assertEqual(0, select_nif(RFd, ?ERL_NIF_SELECT_READ, RFd, Pid, Ref)),

    %% Ensure that no data is hanging and close.
    %% Rfd is stolen at this point.
    check_stop_ret(select_nif(WFd, ?ERL_NIF_SELECT_STOP, WFd, null, Ref)),
    ?assertMatch([{fd_resource_stop, WPtr, _}], flush()),
    {1, {WPtr, 1}} = last_fd_stop_call(),

    check_stop_ret(select_nif(RFd, ?ERL_NIF_SELECT_STOP, RFd, null, Ref)),
    ?assertMatch([{fd_resource_stop, RPtr, _}], flush()),
    {1, {RPtr, 1}} = last_fd_stop_call(),

    ?assert(is_closed_nif(WFd)),

    ok.

check_stop_ret(?ERL_NIF_SELECT_STOP_CALLED) -> ok;
check_stop_ret(?ERL_NIF_SELECT_STOP_SCHEDULED) -> ok.

write_full(W, C) ->
    write_full(W, C, <<>>).
write_full(W, C, Acc) ->
    case write_nif(W, <<C>>) of
        ok ->
            write_full(W, (C+1) band 255, <<Acc/binary, C>>);
        {eagain,0} ->
            Acc
    end.

%% Basic monitoring of one process that terminates 
monitor_process_a(Config) ->
    ensure_lib_loaded(Config),

    F = fun(Terminator, UseMsgEnv) ->
                Pid = spawn(fun() ->
                                    receive
                                        {exit, Arg} -> exit(Arg);
                                        return -> ok;
                                        BadMatch -> goodmatch = BadMatch
                                    end
                            end),
                R_ptr = alloc_monitor_resource_nif(),
                {0, Mon1} = monitor_process_nif(R_ptr, Pid, UseMsgEnv, self()),
                [R_ptr] = monitored_by(Pid),
                Terminator(Pid),
                [{monitor_resource_down, R_ptr, Pid, Mon2}] = flush(),
                0 = compare_monitors_nif(Mon1, Mon2),
                [] = last_resource_dtor_call(),
                ok = release_resource(R_ptr),
                {R_ptr, _, 1} = last_resource_dtor_call()
        end,

    T1 = fun(Pid) -> Pid ! {exit, 17} end,
    T2 = fun(Pid) -> Pid ! return end,
    T3 = fun(Pid) -> Pid ! badmatch end,
    T4 = fun(Pid) -> exit(Pid, 18) end,

    [F(T, UME) || T <- [T1,T2,T3,T4], UME <- [true, false]],

    ok.

%% Test auto-demonitoring at resource destruction
monitor_process_b(Config) ->
    ensure_lib_loaded(Config),

    monitor_process_b_do(false),
    case erlang:system_info(threads) of
        true ->  monitor_process_b_do(true);
        false -> ok
    end,
    ok.


monitor_process_b_do(FromThread) ->
    Pid = spawn_link(fun() ->
                             receive
                                 return -> ok
                             end
                     end),
    R_ptr = alloc_monitor_resource_nif(),
    {0,_} = monitor_process_nif(R_ptr, Pid, true, self()),
    [R_ptr] = monitored_by(Pid),
    case FromThread of
        false -> ok = release_resource(R_ptr);
        true -> ok = release_resource_from_thread(R_ptr)
    end,
    [] = flush(0),
    {R_ptr, _, 1} = last_resource_dtor_call(),
    [] = monitored_by(Pid),
    Pid ! return,
    ok.

%% Test termination of monitored process holding last resource ref
monitor_process_c(Config) ->
    ensure_lib_loaded(Config),

    Papa = self(),
    Pid = spawn_link(fun() ->
                             R_ptr = alloc_monitor_resource_nif(),
                             {0,Mon} = monitor_process_nif(R_ptr, self(), true, Papa),
                             [R_ptr] = monitored_by(self()),
                             put(store, make_resource(R_ptr)),
                             ok = release_resource(R_ptr),
                             [] = last_resource_dtor_call(),
                             Papa ! {self(), done, R_ptr, Mon},
                             exit
                     end),
    [{Pid, done, R_ptr, Mon1},
     {monitor_resource_down, R_ptr, Pid, Mon2}] = flush(2),
    compare_monitors_nif(Mon1, Mon2),
    {R_ptr, _, 1} = last_resource_dtor_call(),
    ok.

%% Test race of resource dtor called when monitored process is exiting
monitor_process_d(Config) ->
    ensure_lib_loaded(Config),

    Papa = self(),
    {Target,TRef} = spawn_monitor(fun() ->
                                          nothing = receive_any()
                                  end),
    
    R_ptr = alloc_monitor_resource_nif(),
    {0,_} = monitor_process_nif(R_ptr, Target, true, self()),
    [Papa, R_ptr] = monitored_by(Target),
    
    exit(Target, die),
    ok = release_resource(R_ptr),

    [{'DOWN', TRef, process, Target, die}] = flush(),  %% no monitor_resource_down
    {R_ptr, _, 1} = last_resource_dtor_call(),

    ok.

%% Test basic demonitoring
demonitor_process(Config) ->
    ensure_lib_loaded(Config),

    Pid = spawn_link(fun() ->
                             receive
                                 return -> ok
                             end
                     end),
    R_ptr = alloc_monitor_resource_nif(),
    {0,MonBin1} = monitor_process_nif(R_ptr, Pid, true, self()),
    [R_ptr] = monitored_by(Pid),
    {0,MonBin2} = monitor_process_nif(R_ptr, Pid, true, self()),
    [R_ptr, R_ptr] = monitored_by(Pid),
    0 = demonitor_process_nif(R_ptr, MonBin1),
    [R_ptr] = monitored_by(Pid),
    1 = demonitor_process_nif(R_ptr, MonBin1),
    0 = demonitor_process_nif(R_ptr, MonBin2),
    [] = monitored_by(Pid),
    1 = demonitor_process_nif(R_ptr, MonBin2),

    ok = release_resource(R_ptr),
    [] = flush(0),
    {R_ptr, _, 1} = last_resource_dtor_call(),
    [] = monitored_by(Pid),
    Pid ! return,
    ok.


monitored_by(Pid) ->
    {monitored_by, List0} = process_info(Pid, monitored_by),
    List1 = lists:map(fun(E) when ?is_resource(E) ->
                              {Ptr, _} = get_resource(monitor_resource_type, E),
                              Ptr;
                         (E) -> E
                      end,
                      List0),
    erlang:garbage_collect(),
    lists:sort(List1).

-define(FRENZY_RAND_BITS, 25).

%% Exercise monitoring from NIF resources by randomly
%% create/destruct processes, resources and monitors.
monitor_frenzy(Config) ->
    ensure_lib_loaded(Config),

    Procs1 = processes(),
    io:format("~p processes before: ~p\n", [length(Procs1), Procs1]),

    %% Spawn first worker process
    Master = self(),
    spawn_link(fun() ->
                       SelfPix = monitor_frenzy_nif(init, ?FRENZY_RAND_BITS, 0, 0),
                       unlink(Master),
                       frenzy(SelfPix, {undefined, []})
          end),
    receive after 5*1000 -> ok end,

    io:format("stats = ~p\n", [monitor_frenzy_nif(stats, 0, 0, 0)]),

    Pids = monitor_frenzy_nif(stop, 0, 0, 0),
    io:format("stats = ~p\n", [monitor_frenzy_nif(stats, 0, 0, 0)]),

    lists:foreach(fun(P) ->
                          MRef = monitor(process, P),
                          exit(P, stop),
                          {'DOWN', MRef, process, P, _} = receive_any()
                  end,
                  Pids),

    io:format("stats = ~p\n", [monitor_frenzy_nif(stats, 0, 0, 0)]),

    Procs2 = processes(),
    io:format("~p processes after: ~p\n", [length(Procs2), Procs2]),
    ok.


frenzy(_SelfPix, done) ->
    ok;
frenzy(SelfPix, State0) ->
    Rnd = rand:uniform(1 bsl (?FRENZY_RAND_BITS+2)) - 1,
    Op = Rnd band 3,
    State1 = frenzy_do_op(SelfPix, Op, (Rnd bsr 2), State0),
    frenzy(SelfPix, State1).

frenzy_do_op(SelfPix, Op, Rnd, {Pid0,RBins}=State0) ->
    case Op of
        0 -> % add/remove process
            Papa = self(),
            NewPid = case Pid0 of
                         undefined -> % Prepare new process to be added
                             spawn(fun() ->
                                           MRef = monitor(process, Papa),
                                           case receive_any() of
                                               {go, MyPix, MyState} ->
                                                   demonitor(MRef, [flush]),
                                                   frenzy(MyPix, MyState);
                                               {'DOWN', MRef, process, Papa, _} ->
                                                   ok
                                           end
                                   end);
                         _ ->
                             Pid0
                     end,
            case monitor_frenzy_nif(Op, Rnd, SelfPix, NewPid) of
                NewPix when is_integer(NewPix) ->
                    NewPid ! {go, NewPix, {undefined, []}},
                    {undefined, RBins};
                ExitPid when is_pid(ExitPid) ->
                    false = (ExitPid =:= self()),
                    exit(ExitPid,die),
                    {NewPid, RBins};
                done ->
                    done
            end;

        3 ->
            %% Try provoke revival-race of resource from magic ref external format
            _ = [binary_to_term(B) || B <- RBins],
            {Pid0, []};
        _ ->
            case monitor_frenzy_nif(Op, Rnd, SelfPix, undefined) of
                Rsrc when ?is_resource(Rsrc) ->
                    %% Store resource in ext format only, for later revival
                    State1 = {Pid0, [term_to_binary(Rsrc) | RBins]},
                    gc_and_return(State1);
                ok -> State0;
                0 -> State0;
                1 -> State0;
                done -> done
            end
    end.

gc_and_return(RetVal) ->
    erlang:garbage_collect(),
    RetVal.

hipe(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Priv = proplists:get_value(priv_dir, Config),
    Src = filename:join(Data, "hipe_compiled"),
    {ok,hipe_compiled} = c:c(Src, [{outdir,Priv},native]),
    true = code:is_module_native(hipe_compiled),
    {error, {notsup,_}} = hipe_compiled:try_load_nif(),
    true = code:delete(hipe_compiled),
    false = code:purge(hipe_compiled),
    ok.


%% Test NIF building heap fragments
heap_frag(Config) when is_list(Config) ->    
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),
    
    heap_frag_do(1,1000000),
    verify_tmpmem(TmpMem),
    ok.

heap_frag_do(N, Max) when N > Max ->
    ok;
heap_frag_do(N, Max) ->
    io:format("Create list of length ~p\n",[N]),
    L = lists:seq(1,N),
    L = list_seq(N),
    heap_frag_do(((N*5) div 4) + 1, Max).

%% Type tests
types(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),
    ok = type_test(),
    lists:foreach(fun(Tpl) ->
                    Lst = erlang:tuple_to_list(Tpl),                 
                    Lst = tuple_2_list(Tpl)
                  end,
                  [{},{ok},{{}},{[],{}},{1,2,3,4,5}]),
    Stuff = [[],{},0,0.0,(1 bsl 100),(fun()-> ok end),make_ref(),self()],
    [eq_cmp(A,clone(B)) || A<-Stuff, B<-Stuff],

    {IntSz, LongSz} = type_sizes(),
    UintMax = (1 bsl (IntSz*8)) - 1,
    IntMax = UintMax bsr 1,
    IntMin = -(IntMax+1),
    UlongMax = (1 bsl (LongSz*8)) - 1,
    LongMax = UlongMax bsr 1,
    LongMin = -(LongMax+1),
    Uint64Max = (1 bsl 64) - 1,
    Int64Max = Uint64Max bsr 1,
    Int64Min = -(Int64Max+1),
    Limits = [{IntMin,IntMax},{0,UintMax},{LongMin,LongMax},{0,UlongMax},{Int64Min,Int64Max},{0,Uint64Max}],
    io:format("Limits = ~p\n", [Limits]),
    lists:foreach(fun(I) ->
			  R1 = echo_int(I),
			  %%io:format("echo_int(~p) -> ~p\n", [I, R1]),
			  R2 = my_echo_int(I, Limits),
			  R1 = R2,
			  true = (R1 =:= R2),
			  true = (R1 == R2)
		  end, int_list()),

    verify_tmpmem(TmpMem),
    true = (compare(-1294536544000, -1178704800000) < 0),
    true = (compare(-1178704800000, -1294536544000) > 0),
    true = (compare(-295147905179352825856, -36893488147419103232) < 0),
    true = (compare(-36893488147419103232, -295147905179352825856) > 0),
    true = (compare(-29514790517935282585612345678, -36893488147419103232) < 0),
    true = (compare(-36893488147419103232, -29514790517935282585612345678) > 0),
    ok.

int_list() ->
    Start = 1 bsl 200,
    int_list([Start], -Start).
int_list([N | _]=List, End) when N<End ->
    List;
int_list([N | _]=List, End) ->
    int_list([N - (1 + (abs(N) div 3)) | List], End).
    
my_echo_int(I, Limits) ->
    lists:map(fun({Min,Max}) ->
		      if I < Min -> false;
			 I > Max -> false;
			 true -> I
		      end
	      end, Limits).

clone(X) ->
    binary_to_term(term_to_binary(X)).

eq_cmp(A,B) ->
    eq_cmp_do(A,B),
    eq_cmp_do([A,B],[A,B]),
    eq_cmp_do({A,B},{A,B}).

eq_cmp_do(A,B) ->
    %%io:format("compare ~p and ~p\n",[A,B]),
    Eq = (A =:= B),
    Eq = is_identical(A,B),
    Cmp = if
            A < B -> -1;
            A == B -> 0;
            A > B -> 1
        end,
    Cmp = case compare(A,B) of
                    C when is_integer(C), C < 0 -> -1;
                    0 -> 0;
                    C when is_integer(C) -> 1
                end,       
    ok. 


%% Test NIF with many arguments
many_args(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config ,1),
    ok = apply(?MODULE,many_args_100,lists:seq(1,100)),
    ok = many_args_100(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100),
    verify_tmpmem(TmpMem),
    ok.

%% Test NIF binary handling.
binaries(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config, 1),
    RefcBin = list_to_binary(lists:seq(1,255)),
    RefcBin = clone_bin(RefcBin),
    HeapBin = list_to_binary(lists:seq(1,20)),
    HeapBin = clone_bin(HeapBin),
    <<_:8,Sub1:6/binary,_/binary>> = RefcBin, 
    <<_:8,Sub2:6/binary,_/binary>> = HeapBin,
    Sub1 = Sub2,
    Sub1 = clone_bin(Sub1),
    Sub2 = clone_bin(Sub2),
    <<_:9,Sub3:6/binary,_/bitstring>> = RefcBin, 
    <<_:9,Sub4:6/binary,_/bitstring>> = HeapBin,
    Sub3 = Sub4,
    Sub3 = clone_bin(Sub3),
    Sub4 = clone_bin(Sub4),
    %% When NIFs get bitstring support
    %%<<_:8,Sub5:27/bitstring,_/bitstring>> = RefcBin, 
    %%<<_:8,Sub6:27/bitstring,_/bitstring>> = HeapBin,
    %%Sub5 = Sub6,
    %%Sub5 = clone_bin(Sub5),
    %%Sub6 = clone_bin(Sub6),
    %%<<_:9,Sub7:27/bitstring,_/bitstring>> = RefcBin, 
    %%<<_:9,Sub8:27/bitstring,_/bitstring>> = HeapBin,
    %%Sub7 = Sub8,
    %%Sub7 = clone_bin(Sub7),
    %%Sub8 = clone_bin(Sub8),
    %%<<>> = clone_bin(<<>>),

    <<_:8,SubBinA:200/binary,_/binary>> = RefcBin,
    <<_:9,SubBinB:200/binary,_/bitstring>> = RefcBin,
    <<_:8,SubBinC:17/binary,_/binary>> = HeapBin,
    <<_:9,SubBinD:17/binary,_/bitstring>> = HeapBin,
    test_make_sub_bin(RefcBin),
    test_make_sub_bin(HeapBin),
    test_make_sub_bin(SubBinA),
    test_make_sub_bin(SubBinB),
    test_make_sub_bin(SubBinC),
    test_make_sub_bin(SubBinD),
    
    verify_tmpmem(TmpMem),
    ok.

test_make_sub_bin(Bin) ->
    Size = byte_size(Bin),
    Rest10 = Size - 10,
    Rest1 = Size - 1,
    Bin = make_sub_bin(Bin, 0, Size),
    <<_:10/binary,Sub0:Rest10/binary>> = Bin,
    Sub0 = make_sub_bin(Bin, 10, Rest10),
    <<Sub1:10/binary,_/binary>> = Bin,
    Sub1 = make_sub_bin(Bin, 0, 10),
    <<_:7/binary,Sub2:10/binary,_/binary>> = Bin,
    Sub2 = make_sub_bin(Bin, 7, 10),
    <<>> = make_sub_bin(Bin, 0, 0),
    <<>> = make_sub_bin(Bin, 10, 0),
    <<>> = make_sub_bin(Bin, Rest1, 0),
    <<>> = make_sub_bin(Bin, Size, 0),
    ok.
    
%% Test enif_get_string
get_string(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    {7, <<"hejsan",0,_:3/binary>>} = string_to_bin("hejsan",10),
    {7, <<"hejsan",0,_>>} = string_to_bin("hejsan",8),
    {7, <<"hejsan",0>>} = string_to_bin("hejsan",7),
    {-6, <<"hejsa",0>>} = string_to_bin("hejsan",6),
    {-5, <<"hejs",0>>} = string_to_bin("hejsan",5),
    {-1, <<0>>} = string_to_bin("hejsan",1),
    {0, <<>>} = string_to_bin("hejsan",0),
    {1, <<0>>} = string_to_bin("",1),
    {0, <<>>} = string_to_bin("",0),
    ok.

%% Test enif_get_atom
get_atom(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    {7, <<"hejsan",0,_:3/binary>>} = atom_to_bin(hejsan,10),
    {7, <<"hejsan",0,_>>} = atom_to_bin(hejsan,8),
    {7, <<"hejsan",0>>} = atom_to_bin(hejsan,7),
    {0, <<_:6/binary>>} = atom_to_bin(hejsan,6),
    {0, <<>>} = atom_to_bin(hejsan,0),
    {1, <<0>>} = atom_to_bin('',1),
    {0, <<>>} = atom_to_bin('',0),
    ok.

%% Test NIF maps handling.
maps(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    Pairs = [{adam, "bert"}] ++
            [{I,I}||I <- lists:seq(1,10)] ++
	    [{a,value},{"a","value"},{<<"a">>,<<"value">>}],
    ok = ensure_lib_loaded(Config, 1),
    M  = maps_from_list_nif(Pairs),
    R = {RIs,Is} = sorted_list_from_maps_nif(M),
    io:format("Pairs: ~p~nMap: ~p~nReturned: ~p~n", [lists:sort(Pairs),M,R]),
    true = (lists:sort(Is) =:= lists:sort(Pairs)),
    Is = lists:reverse(RIs),

    #{} = maps_from_list_nif([]),
    {[],[]} = sorted_list_from_maps_nif(#{}),

    1 = is_map_nif(M),
    0 = is_map_nif("no map"),

    Msz = map_size(M),
    {1,Msz} = get_map_size_nif(M),
    {1,0} = get_map_size_nif(#{}),
    {0,-123} = get_map_size_nif({#{}}),

    #{} = M0 = make_new_map_nif(),

    {1, #{key := value}=M1} = make_map_put_nif(M0, key, value),
    {1, #{key := value, "key2" := "value2"}=M2} = make_map_put_nif(M1, "key2", "value2"),
    {1, #{key := "value", "key2" := "value2"}=M3} = make_map_put_nif(M2, key, "value"),
    {0, undefined} = make_map_put_nif(666, key, value),

    {1, "value2"} = get_map_value_nif(M3,"key2"),
    {0, undefined} = get_map_value_nif(M3,"key3"),
    {0, undefined} = get_map_value_nif(false,key),

    {0, undefined} = make_map_update_nif(M0, key, value),
    {0, undefined} = make_map_update_nif(M1, "key2", "value2"),
    {1, #{key := "value", "key2" := "value2"}} = make_map_update_nif(M2, key, "value"),
    {0, undefined} = make_map_update_nif(666, key, value),

    {1, #{}} = make_map_remove_nif(M1, key),
    {1, M1} = make_map_remove_nif(M2, "key2"),
    {1, M2} = make_map_remove_nif(M2, "key3"),
    {0, undefined} = make_map_remove_nif(self(), key),

    M1 = maps_from_list_nif(maps:to_list(M1)),
    M2 = maps_from_list_nif(maps:to_list(M2)),
    M3 = maps_from_list_nif(maps:to_list(M3)),

    has_duplicate_keys = maps_from_list_nif([{1,1},{1,1}]),

    verify_tmpmem(TmpMem),
    ok.
 
%% Test macros enif_make_list<N> and enif_make_tuple<N>
api_macros(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    Expected = {[lists:seq(1,N) || N <- lists:seq(1,9)],
		[list_to_tuple(lists:seq(1,N)) || N <- lists:seq(1,9)]
	       },
    Expected = macros(list_to_tuple(lists:seq(1,9))),
    ok.

%% enif_make_[tuple|list]_from_array
from_array(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    lists:foreach(fun(Tpl) ->
			  Lst = tuple_to_list(Tpl),
			  {Lst,Tpl} = tuple_2_list_and_tuple(Tpl)
		  end,
		  [{}, {1,2,3}, {[4,5],[],{},{6,7}}, {{}}, {[]}]),
    ok.

%% enif_inspect_iolist_as_binary
iolist_as_binary(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    TmpMem = tmpmem(),
    List = [<<"hejsan">>, <<>>, [], [17], [<<>>],
	    [127,128,255,0],
	    [1, 2, 3, <<"abc">>, [<<"def">>,4], 5, <<"ghi">>],
	    [1, 2, 3, <<"abc">>, [<<"def">>,4], 5 | <<"ghi">>]],
	    
    lists:foreach(fun(IoL) ->
			  B1 = erlang:iolist_to_binary(IoL),
			  B2 = iolist_2_bin(IoL),
			  B1 = B2
		  end,
		  List),
    verify_tmpmem(TmpMem),
    ok.

%% Test memory managed objects, aka 'resources'
resource(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    Type = get_resource_type(0),
    resource_hugo(Type),
    resource_otto(Type),
    resource_new(Type),
    resource_neg(Type),
    ok.

resource_hugo(Type) ->
    DtorCall = resource_hugo_do(Type),
    erlang:garbage_collect(),
    DtorCall = last_resource_dtor_call(),
    ok.

resource_hugo_do(Type) ->
    HugoBin = <<"Hugo Hacker">>,
    HugoPtr = alloc_resource(Type, HugoBin),
    Hugo = make_resource(HugoPtr),
    true = is_reference(Hugo),
    release_resource(HugoPtr),
    erlang:garbage_collect(),
    {HugoPtr,HugoBin} = get_resource(Type,Hugo),
    Pid = spawn_link(fun() -> 			     
                             receive {Pid, Type, Resource, Ptr, Bin} ->
                                         Pid ! {self(), got_it},
                                         receive {Pid, check_it} ->
                                                     {Ptr,Bin} = get_resource(Type,Resource),
                                                     Pid ! {self(), ok}
                                         end
                             end
                     end),
    Pid ! {self(), Type, Hugo, HugoPtr, HugoBin},
    {Pid, got_it} = receive_any(),
    erlang:garbage_collect(),   % just to make our ProcBin move in memory
    Pid ! {self(), check_it},
    {Pid, ok} = receive_any(),
    [] = last_resource_dtor_call(),
    {HugoPtr,HugoBin} = get_resource(Type,Hugo),
    {HugoPtr, HugoBin, 1}.

resource_otto(Type) ->
    {OttoPtr, OttoBin} = resource_otto_do(Type),
    erlang:garbage_collect(),
    [] = last_resource_dtor_call(),
    release_resource(OttoPtr),
    {OttoPtr,OttoBin,1} = last_resource_dtor_call(),
    ok.
    
resource_otto_do(Type) ->
    OttoBin = <<"Otto Ordonnans">>,
    OttoPtr = alloc_resource(Type, OttoBin),
    Otto = make_resource(OttoPtr),
    true = is_reference(Otto),
    %% forget resource term but keep referenced by NIF
    {OttoPtr, OttoBin}.    

resource_new(Type) ->
    {PtrB,BinB} = resource_new_do1(Type),
    erlang:garbage_collect(),
    {PtrB,BinB,1} = last_resource_dtor_call(),
    ok.
    
resource_new_do1(Type) ->
    {{PtrA,BinA}, {ResB,PtrB,BinB}} = resource_new_do2(Type),
    erlang:garbage_collect(),
    {PtrA,BinA,1} = last_resource_dtor_call(),
    {PtrB,BinB} = get_resource(Type, ResB),
    %% forget ResB and make it garbage
    {PtrB,BinB}.
    
resource_new_do2(Type) ->
    BinA = <<"NewA">>,
    BinB = <<"NewB">>,
    ResA = make_new_resource(Type, BinA),
    ResB = make_new_resource(Type, BinB),
    true = is_reference(ResA),
    true = is_reference(ResB),
    true = (ResA /= ResB),
    {PtrA,BinA} = get_resource(Type, ResA),
    {PtrB,BinB} = get_resource(Type, ResB),
    true = (PtrA =/= PtrB),
    %% forget ResA and make it garbage
    {{PtrA,BinA}, {ResB,PtrB,BinB}}.

resource_neg(TypeA) ->
    resource_neg_do(TypeA),

    catch exit(42), % dummy exception to purge saved stacktraces from earlier exception
    erlang:garbage_collect(),
    {_,_,2} = last_resource_dtor_call(),
    ok.

resource_neg_do(TypeA) ->
    TypeB = get_resource_type(1),
    ResA = make_new_resource(TypeA, <<"Arnold">>),
    ResB= make_new_resource(TypeB, <<"Bobo">>),
    {'EXIT',{badarg,_}} = (catch get_resource(TypeA, ResB)),
    {'EXIT',{badarg,_}} = (catch get_resource(TypeB, ResA)),
    ok.

%% Test enif_make_resource_binary
resource_binary(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    {Ptr,Bin} = resource_binary_do(),
    erlang:garbage_collect(),
    Last = last_resource_dtor_call(),
    ?CHECK({Ptr,Bin,1}, Last),
    ok.

resource_binary_do() ->
    Bin = <<"Hej Hopp i lingonskogen">>,
    {Ptr,ResBin1} = make_new_resource_binary(Bin),
    ResBin1 = Bin,          
    ResInfo = {Ptr,_} = get_resource(binary_resource_type,ResBin1),

    Papa = self(),
    Forwarder = spawn_link(fun() -> forwarder(Papa) end),
    io:format("sending to forwarder pid=~p\n",[Forwarder]),  
    Forwarder ! ResBin1,
    ResBin2 = receive_any(),
    ResBin2 = ResBin1,
    ResInfo = get_resource(binary_resource_type,ResBin2),
    Forwarder ! terminate,
    {Forwarder, 1} = receive_any(),
    erlang:garbage_collect(),
    ResInfo = get_resource(binary_resource_type,ResBin1),
    ResInfo = get_resource(binary_resource_type,ResBin2),
    ResInfo.

    
-define(RT_CREATE,1).
-define(RT_TAKEOVER,2).

%% Test resource takeover by module upgrade
resource_takeover(Config) when is_list(Config) ->    
    TmpMem = tmpmem(),
    ensure_lib_loaded(Config),

    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "nif_mod"),
    {ok,nif_mod,ModBin} = compile:file(File, [binary,return_errors]),
    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),

    ok = nif_mod:load_nif_lib(Config, 1, 
                              [{resource_type, 0, ?RT_CREATE, "resource_type_A",resource_dtor_A,
                                ?RT_CREATE},
                               {resource_type, 1, ?RT_CREATE, "resource_type_null_A",null,
                                ?RT_CREATE},
                               {resource_type, 2, ?RT_CREATE bor ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
                                ?RT_CREATE},
                               {resource_type, 3, ?RT_CREATE, "resource_type_B_goneX",resource_dtor_B,
                                ?RT_CREATE},
                               {resource_type, 4, ?RT_CREATE, "resource_type_null_goneX",null,
                                ?RT_CREATE},
                               {resource_type, null, ?RT_TAKEOVER, "Pink unicorn", resource_dtor_A,
                                ?RT_TAKEOVER}
                              ]),

    hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),

    {Holder, _MRef} = spawn_opt(fun resource_holder/0, [link, monitor]),

    {A1,BinA1} = make_resource(0,Holder,"A1"),
    {A2,BinA2} = make_resource(0,Holder,"A2"),
    {A3,BinA3} = make_resource(0,Holder,"A3"),

    {NA1,_BinNA1} = make_resource(1,Holder,"NA1"),
    {NA2,BinNA2} = make_resource(1,Holder,"NA2"),
    {NA3,_BinNA3} = make_resource(1,Holder,"NA3"),

    {AN1,BinAN1} = make_resource(2,Holder,"AN1"),
    {AN2,_BinAN2} = make_resource(2,Holder,"AN2"),
    {AN3,BinAN3} = make_resource(2,Holder,"AN3"),

    {BGX1,BinBGX1} = make_resource(3,Holder,"BGX1"),
    {BGX2,BinBGX2} = make_resource(3,Holder,"BGX2"),

    {NGX1,_BinNGX1} = make_resource(4,Holder,"NGX1"),
    {NGX2,_BinNGX2} = make_resource(4,Holder,"NGX2"),

    [] = nif_mod_call_history(),

    ok = forget_resource(A1),
    [{{resource_dtor_A_v1,BinA1},1,3,103}] = nif_mod_call_history(),    

    ok = forget_resource(NA1),
    [] = nif_mod_call_history(), % no dtor

    ok = forget_resource(AN1),
    ?CHECK([{{resource_dtor_A_v1,BinAN1},1,4,104}] , nif_mod_call_history()),

    ok = forget_resource(BGX1),
    ?CHECK([{{resource_dtor_B_v1,BinBGX1},1,5,105}], nif_mod_call_history()),

    ok = forget_resource(NGX1),
    ?CHECK([], nif_mod_call_history()), % no dtor

    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    ok = nif_mod:load_nif_lib(Config, 2,
                              [{resource_type, 0, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
                                ?RT_TAKEOVER},
                               {resource_type, 1, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",resource_dtor_A,
                                ?RT_TAKEOVER},
                               {resource_type, 2, ?RT_TAKEOVER, "resource_type_A_null",null,
                                ?RT_TAKEOVER},
                               {resource_type, null, ?RT_TAKEOVER, "Pink unicorn", resource_dtor_A,
                                ?RT_TAKEOVER},
                               {resource_type, null, ?RT_CREATE, "resource_type_B_goneX",resource_dtor_B,
                                ?RT_CREATE},
                               {resource_type, null, ?RT_CREATE, "resource_type_null_goneX",null,
                                ?RT_CREATE},
                               {resource_type, 3, ?RT_CREATE, "resource_type_B_goneY",resource_dtor_B,
                                ?RT_CREATE},
                               {resource_type, 4, ?RT_CREATE, "resource_type_null_goneY",null,
                                ?RT_CREATE}
                              ]),
    ?CHECK([{upgrade,2,1,201}], nif_mod_call_history()),
    true = erlang:purge_module(nif_mod),
    ?CHECK([], nif_mod_call_history()),  % BGX2 keeping lib loaded

    BinA2 = read_resource(0,A2),
    ok = forget_resource(A2),
    ?CHECK([{{resource_dtor_A_v2,BinA2},2,2,202}], nif_mod_call_history()),    

    ok = forget_resource(NA2),
    ?CHECK([{{resource_dtor_A_v2,BinNA2},2,3,203}], nif_mod_call_history()),    

    ok = forget_resource(AN2),
    ?CHECK([], nif_mod_call_history()),    % no dtor

    ok = forget_resource(BGX2),  % calling dtor in orphan library v1 still loaded
    ?CHECK([{{resource_dtor_B_v1,BinBGX2},1,6,106}, {unload,1,7,107}],
           nif_mod_call_history()),

    ok = forget_resource(NGX2),
    ?CHECK([], nif_mod_call_history()),  % no dtor

    {BGY1,BinBGY1} = make_resource(3,Holder,"BGY1"),
    {NGY1,_BinNGY1} = make_resource(4,Holder,"NGY1"),

    %% Module upgrade with same lib-version
    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    undefined = nif_mod:lib_version(),
    ok = nif_mod:load_nif_lib(Config, 2,
                              [{resource_type, 2, ?RT_TAKEOVER, "resource_type_A",resource_dtor_B,
                                ?RT_TAKEOVER},
                               {resource_type, 0, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
                                ?RT_TAKEOVER},
                               {resource_type, 1, ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
                                ?RT_TAKEOVER},
                               {resource_type, null, ?RT_TAKEOVER, "Pink elephant", resource_dtor_A,
                                ?RT_TAKEOVER},
                               {resource_type, 3, ?RT_CREATE, "resource_type_B_goneZ",resource_dtor_B,
                                ?RT_CREATE},
                               {resource_type, 4, ?RT_CREATE, "resource_type_null_goneZ",null,
                                ?RT_CREATE}
                              ]),

    2 = nif_mod:lib_version(),
    ?CHECK([{upgrade,2,4,204},{lib_version,2,5,205}], nif_mod_call_history()),

    ok = forget_resource(A3),
    ?CHECK([{{resource_dtor_B_v2,BinA3},2,6,206}], nif_mod_call_history()),    

    ok = forget_resource(NA3),
    ?CHECK([], nif_mod_call_history()),    

    ok = forget_resource(AN3),
    ?CHECK([{{resource_dtor_A_v2,BinAN3},2,7,207}], nif_mod_call_history()),

    {A4,BinA4} = make_resource(2,Holder, "A4"),
    {NA4,BinNA4} = make_resource(0,Holder, "NA4"),
    {AN4,_BinAN4} = make_resource(1,Holder, "AN4"),

    {BGZ1,BinBGZ1} = make_resource(3,Holder,"BGZ1"),
    {NGZ1,_BinNGZ1} = make_resource(4,Holder,"NGZ1"),

    false = code:purge(nif_mod),
    [] = nif_mod_call_history(),

    ok = forget_resource(NGY1),
    [] = nif_mod_call_history(),

    ok = forget_resource(BGY1),  % calling dtor in orphan library v2 still loaded
    [{{resource_dtor_B_v2,BinBGY1},2,8,208},{unload,2,9,209}] = nif_mod_call_history(),

    %% Module upgrade with other lib-version
    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    undefined = nif_mod:lib_version(),
    ok = nif_mod:load_nif_lib(Config, 1,
				    [{resource_type, 2, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, 0, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",resource_dtor_A,
				      ?RT_TAKEOVER},
				     {resource_type, 1, ?RT_TAKEOVER, "resource_type_A_null",null,
				      ?RT_TAKEOVER},
				     {resource_type, null, ?RT_TAKEOVER, "Mr Pink", resource_dtor_A,
				      ?RT_TAKEOVER}
				    ]),

    1 = nif_mod:lib_version(),
    [{upgrade,1,1,101},{lib_version,1,2,102}] = nif_mod_call_history(),

    %%false= check_process_code(Pid, nif_mod),
    false = code:purge(nif_mod),
    %% no unload here as we still have instances with destructors
    [] = nif_mod_call_history(),

    ok = forget_resource(BGZ1),  % calling dtor in orphan library v2 still loaded
    [{{resource_dtor_B_v2,BinBGZ1},2,10,210},{unload,2,11,211}] = nif_mod_call_history(),

    ok = forget_resource(NGZ1),
    [] = nif_mod_call_history(),

    ok = forget_resource(A4),
    [{{resource_dtor_A_v1,BinA4},1,3,103}] = nif_mod_call_history(),

    ok = forget_resource(NA4),
    [{{resource_dtor_A_v1,BinNA4},1,4,104}] = nif_mod_call_history(),

    ok = forget_resource(AN4),
    [] = nif_mod_call_history(),


    %%
    %% Test rollback after failed upgrade of same lib-version
    %%

    {A5,BinA5} = make_resource(2, Holder, "A5"),
    {NA5,BinNA5} = make_resource(0, Holder, "NA5"),
    {AN5,_BinAN5} = make_resource(1, Holder, "AN5"),

    {A6,BinA6} = make_resource(2, Holder, "A6"),
    {NA6,BinNA6} = make_resource(0, Holder, "NA6"),
    {AN6,_BinAN6} = make_resource(1, Holder, "AN6"),

    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    undefined = nif_mod:lib_version(),
    {error,{upgrade,_}} =
	nif_mod:load_nif_lib(Config, 1,
			     [{resource_type, 4, ?RT_TAKEOVER, "resource_type_A",resource_dtor_B,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_CREATE, "Mr Pink", resource_dtor_A,
			       ?RT_CREATE},

			      {return, 1}  % FAIL
			     ]),

    undefined = nif_mod:lib_version(),
    [{upgrade,1,5,105}] = nif_mod_call_history(),

    %% Make sure dtor was not changed (from A to B)
    ok = forget_resource(A5),
    [{{resource_dtor_A_v1,BinA5},1,6,106}] = nif_mod_call_history(),

    %% Make sure dtor was not nullified (from A to null)
    ok = forget_resource(NA5),
    [{{resource_dtor_A_v1,BinNA5},1,7,107}] = nif_mod_call_history(),

    %% Make sure dtor was not added (from null to A)
    ok = forget_resource(AN5),
    [] = nif_mod_call_history(),

    %%
    %% Test rollback after failed upgrade of other lib-version
    %%

    {error,{upgrade,_}} =
	nif_mod:load_nif_lib(Config, 2,
			     [{resource_type, 4, ?RT_TAKEOVER, "resource_type_A",resource_dtor_B,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER, "resource_type_A_null",resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, null, ?RT_TAKEOVER, "Mr Pink", resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_CREATE, "Mr Pink", resource_dtor_A,
			       ?RT_CREATE},

			      {return, 1}  % FAIL
			     ]),

    undefined = nif_mod:lib_version(),
    [{upgrade,2,_,_}] = nif_mod_call_history(),

    %% Make sure dtor was not changed (from A to B)
    ok = forget_resource(A6),
    [{{resource_dtor_A_v1,BinA6},1,_,_}] = nif_mod_call_history(),

    %% Make sure dtor was not nullified (from A to null)
    ok = forget_resource(NA6),
    [{{resource_dtor_A_v1,BinNA6},1,_,_}] = nif_mod_call_history(),

    %% Make sure dtor was not added (from null to A)
    ok = forget_resource(AN6),
    [] = nif_mod_call_history(),

    %%
    %% Test rolback after failed initial load
    %%
    false = code:purge(nif_mod),
    [{unload,1,_,_}] = nif_mod_call_history(),
    true = code:delete(nif_mod),
    false = code:purge(nif_mod),
    [] = nif_mod_call_history(),


    {module,nif_mod} = erlang:load_module(nif_mod,ModBin),
    undefined = nif_mod:lib_version(),
    {error,{load,_}} =
	nif_mod:load_nif_lib(Config, 1,
			     [{resource_type, null, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
			       ?RT_TAKEOVER},
			      {resource_type, 4, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",null,
			       ?RT_CREATE},
			      {resource_type, 4, ?RT_CREATE, "resource_type_A_null",resource_dtor_A,
			       ?RT_CREATE},
			      {resource_type, 4, ?RT_CREATE, "Mr Pink", resource_dtor_A,
			       ?RT_CREATE},

			      {return, 1}  % FAIL
			     ]),

    undefined = nif_mod:lib_version(),
    ok = nif_mod:load_nif_lib(Config, 1,
			      [{resource_type, null, ?RT_TAKEOVER, "resource_type_A",resource_dtor_A,
				?RT_TAKEOVER},
			       {resource_type, 0, ?RT_TAKEOVER bor ?RT_CREATE, "resource_type_null_A",
				resource_dtor_A, ?RT_CREATE},

			       {resource_type, 1, ?RT_CREATE, "resource_type_A_null", null,
				?RT_CREATE},
			       {resource_type, null, ?RT_TAKEOVER, "Mr Pink", resource_dtor_A,
				?RT_TAKEOVER},

			       {return, 0}  % SUCCESS
			      ]),

    hold_nif_mod_priv_data(nif_mod:get_priv_data_ptr()),
    [{load,1,1,101},{get_priv_data_ptr,1,2,102}] = nif_mod_call_history(),

    {NA7,BinNA7} = make_resource(0, Holder, "NA7"),
    {AN7,_BinAN7} = make_resource(1, Holder, "AN7"),

    ok = forget_resource(NA7),
    [{{resource_dtor_A_v1,BinNA7},1,_,_}] = nif_mod_call_history(),

    ok = forget_resource(AN7),
    [] = nif_mod_call_history(),

    true = erlang:delete_module(nif_mod),
    true = erlang:purge_module(nif_mod),

    true = lists:member(?MODULE, erlang:system_info(taints)),
    true = lists:member(nif_mod, erlang:system_info(taints)),
    verify_tmpmem(TmpMem),    
    ok.

make_resource(Type,Holder,Str) when is_list(Str) ->
    Bin = list_to_binary(Str),
    A1 = make_resource_do(Type,Holder,Bin),
    Bin = read_resource(Type,A1),
    {A1,Bin}.

make_resource_do(Type, Holder, Bin) ->
    Holder ! {self(), make, Type, Bin},
    {Holder, make_ok, Id} = receive_any(),
    {Holder,Id}.

read_resource(Type, {Holder,Id}) ->
    Holder ! {self(), get, Type, Id},
    {Holder, get_ok, Bin} = receive_any(),
    Bin.

forget_resource({Holder,Id}) ->
    Holder ! {self(), forget, Id},
    {Holder, forget_ok, Id} = receive_any(),
    ok.


resource_holder() ->
    resource_holder([]).
resource_holder(List) ->
    %%io:format("resource_holder waiting for msg\n", []),
    Msg = receive_any(),
    %%io:format("resource_holder got ~p with list = ~p\n", [Msg,List]),
    case Msg of
	{Pid, make, Type, Bin} ->	    
	    Resource = nif_mod:make_new_resource(Type, Bin),
	    Id = {make_ref(),Bin},
	    Pid ! {self(), make_ok, Id},
	    resource_holder([{Id,Resource} | List]);
	{Pid, get, Type, Id} ->
	    {Id,Resource} = lists:keyfind(Id, 1, List),
	    Pid ! {self(), get_ok, nif_mod:get_resource(Type, Resource)},
	    resource_holder(List);
	
	{Pid, forget, Id} ->
	    NewList = lists:keydelete(Id, 1, List),
	    %%io:format("resource_holder forget: NewList = ~p\n", [NewList]),
	    resource_holder(Pid, {self(),forget_ok,Id}, NewList)
    end.

resource_holder(Pid,Reply,List) ->
    erlang:garbage_collect(),
    %%io:format("resource_holder GC'ed, now send ~p to ~p\n", [Reply,Pid]),
    Pid ! Reply,
    resource_holder(List).


%% Test the threading API functions (reuse tests from driver API)
threading(Config) when is_list(Config) ->
    case erlang:system_info(threads) of
    	true -> threading_do(Config);
	false -> {skipped,"No thread support"}
    end.

threading_do(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "tester"),
    {ok,tester,ModBin} = compile:file(File, [binary,return_errors]),
    {module,tester} = erlang:load_module(tester,ModBin),

    ok = tester:load_nif_lib(Config, "basic"),   
    ok = tester:run(),

    erlang:load_module(tester,ModBin),
    erlang:purge_module(tester),
    ok = tester:load_nif_lib(Config, "rwlock"),
    ok = tester:run(),

    erlang:load_module(tester,ModBin),
    erlang:purge_module(tester),
    ok = tester:load_nif_lib(Config, "tsd"),
    ok = tester:run(),

    erlang:delete_module(tester),
    erlang:purge_module(tester).


%% Test NIF message sending
send(Config) when is_list(Config) ->    
    ensure_lib_loaded(Config),

    N = 1500,
    List = lists:seq(1,N),
    {ok,1} = send_list_seq(N, self),
    {ok,1} = send_list_seq(N, self()),
    List = receive_any(),
    List = receive_any(),
    Papa = self(),
    spawn_link(fun() -> {ok,1} = send_list_seq(N, Papa) end),
    List = receive_any(),

    {ok, 1, BlobS} = send_new_blob(self(), other_term()),
    BlobR = receive_any(),
    io:format("Sent ~p\nGot ~p\n", [BlobS, BlobR]),
    BlobR = BlobS,

    %% send to dead pid
    {DeadPid, DeadMon} = spawn_monitor(fun() -> void end),
    {'DOWN', DeadMon, process, DeadPid, normal} = receive_any(),
    {ok,0} = send_list_seq(7, DeadPid),
    ok.

%% More NIF message sending
send2(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),

    send2_do1(fun send_blob_dbg/2),
    ok.

%% Send msg from user thread
send_threaded(Config) when is_list(Config) ->
    send2_do1(fun(ME,To) -> send_blob_thread_dbg(ME,To,join) end),
    send2_do1(fun(ME,To) -> send_blob_thread_and_join(ME,To) end),
    ok.


send2_do1(SendBlobF) ->
    io:format("sending to self=~p\n",[self()]),
    send2_do2(SendBlobF, self()),

    Papa = self(),
    Forwarder = spawn_link(fun() -> forwarder(Papa) end),
    io:format("sending to forwarder pid=~p\n",[Forwarder]),
    send2_do2(SendBlobF, Forwarder),
    Forwarder ! terminate,
    {Forwarder, 4} = receive_any(),
    ok.

send2_do2(SendBlobF, To) ->   
    MsgEnv = alloc_msgenv(),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    {ok,1,Blob0} = SendBlobF(MsgEnv, To),
    Blob1 = receive_any(),
    Blob1 = Blob0,
    
    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    {ok,1,Blob2} = SendBlobF(MsgEnv, To),
    Blob3 = receive_any(),
    Blob3 = Blob2,

    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),

    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    {ok,1,Blob4} = SendBlobF(MsgEnv, To),
    Blob5 = receive_any(),
    Blob5 = Blob4,

    clear_msgenv(MsgEnv),
    clear_msgenv(MsgEnv),
    repeat(50, fun(_) -> grow_blob(MsgEnv,other_term()) end, []),
    {ok,1,Blob6} = SendBlobF(MsgEnv, To),
    Blob7 = receive_any(),
    Blob7 = Blob6,

    ok.


send_blob_thread_and_join(MsgEnv, To) ->
    {ok,Blob} = send_blob_thread_dbg(MsgEnv, To, no_join),
    {ok,SendRes} = join_send_thread(MsgEnv),
    {ok,SendRes,Blob}.

send_blob_dbg(MsgEnv, To) ->
    Ret = send_blob(MsgEnv, To),
    %%io:format("send_blob to ~p returned ~p\n",[To,Ret]),
    Ret.

send_blob_thread_dbg(MsgEnv, To, Join) ->
    Ret = send_blob_thread(MsgEnv, To, Join),
    %%io:format("send_blob_thread to ~p Join=~p returned ~p\n",[To,Join,Ret]),
    Ret.


forwarder(To) ->
    forwarder(To, 0).
forwarder(To, N) ->
    case receive_any() of
	terminate ->
	    To ! {self(), N};
	Msg ->	    
	    To ! Msg,	   
	    forwarder(To, N+1)
    end.

other_term() ->
    {fun(X,Y) -> X*Y end, make_ref()}.

%% Message sending stress test
send3(Config) when is_list(Config) ->
    %% Let a number of processes send random message blobs between each other
    %% using enif_send. Kill and spawn new ones randomly to keep a ~constant
    %% number of workers running.
    rand:seed(exsplus),
    io:format("seed: ~p\n",[rand:export_seed()]),
    ets:new(nif_SUITE,[named_table,public]),
    true = ets:insert(nif_SUITE,{send3,0,0,0,0}),
    timer:send_after(10000, timeout), % Run for 10 seconds
    SpawnCnt = send3_controller(0, [], [], 20),
    [{_,Rcv,SndOk,SndFail,Balance}] = ets:lookup(nif_SUITE,send3),
    io:format("spawns=~p received=~p, sent=~p send-failure=~p balance=~p\n",
              [SpawnCnt,Rcv,SndOk,SndFail,Balance]),
    ets:delete(nif_SUITE).

send3_controller(SpawnCnt, [], _, infinity) ->
    SpawnCnt;
send3_controller(SpawnCnt0, Mons0, Pids0, Tick) ->
    receive 
        timeout ->
            io:format("Timeout. Sending 'halt' to ~p\n",[Pids0]),            
            lists:foreach(fun(P) -> P ! {halt,self()} end, Pids0),
            lists:foreach(fun(P) -> receive {halted,P} -> ok end end, Pids0),
            QTot = lists:foldl(fun(P,QSum) ->
                                 {message_queue_len,QLen} = 
                                    erlang:process_info(P,message_queue_len),
                                 QSum + QLen
                               end, 0, Pids0),
            io:format("Total queue length ~p\n",[QTot]),            
            lists:foreach(fun(P) -> P ! die end, Pids0),
            send3_controller(SpawnCnt0, Mons0, [], infinity);
        {'DOWN', MonRef, process, _Pid, _} ->
            Mons1 = lists:delete(MonRef, Mons0),
            %%io:format("Got DOWN from ~p. Monitors left: ~p\n",[Pid,Mons1]),            
            send3_controller(SpawnCnt0, Mons1, Pids0, Tick)
    after Tick -> 
        Max = 20,
        N = length(Pids0),
        PidN = rand:uniform(Max),
        %%io:format("N=~p PidN=~p Pids0=~p\n", [N,PidN,Pids0]), 
        case PidN > N of
            true ->
                {NewPid,Mon} = spawn_opt(fun send3_proc/0, [link,monitor]),                
                lists:foreach(fun(P) -> P ! {is_born,NewPid} end, Pids0),
                Balance = ets:lookup_element(nif_SUITE,send3,5),
                Inject = (Balance =< 0),
                case Inject of
                    true ->  ok;
                    false -> ets:update_element(nif_SUITE,send3,{5,-1})
                end,
                NewPid ! {pids,Pids0,Inject},
                send3_controller(SpawnCnt0+1, [Mon|Mons0], [NewPid|Pids0], Tick);
            false ->
                KillPid = lists:nth(PidN,Pids0),
                KillPid ! die,
                Pids1 = lists:delete(KillPid, Pids0),
                lists:foreach(fun(P) -> P ! {is_dead,KillPid} end, Pids1),
                send3_controller(SpawnCnt0, Mons0, Pids1, Tick)
        end        
   end.

send3_proc() ->
    %%io:format("Process ~p spawned\n",[self()]),
    send3_proc([self()], {0,0,0}, {1,2,3,4,5}).
send3_proc(Pids0, Counters={Rcv,SndOk,SndFail}, State0) ->
    %%io:format("~p: Pids0=~p", [self(), Pids0]),
    %%timer:sleep(10),
    receive 
        {pids, Pids1, Inject} ->
            %%io:format("~p: got ~p Inject=~p\n", [self(), Pids1, Inject]), 
            Pids0 = [self()],
            Pids2 = [self() | Pids1],
            case Inject of
                true -> send3_proc_send(Pids2, Counters, State0);
                false -> send3_proc(Pids2, Counters, State0)
            end;
        {is_born, Pid} ->
            %%io:format("~p: is_born ~p, got ~p\n", [self(), Pid, Pids0]), 
            send3_proc([Pid | Pids0], Counters, State0);
        {is_dead, Pid} ->            
            Pids1 = lists:delete(Pid,Pids0),
            %%io:format("~p: is_dead ~p, got ~p\n", [self(), Pid, Pids1]),
            send3_proc(Pids1, Counters, State0);
        {blob, Blob0} ->
            %%io:format("~p: blob ~p\n", [self(), Blob0]),
            State1 = send3_new_state(State0, Blob0),
            send3_proc_send(Pids0, {Rcv+1,SndOk,SndFail}, State1);
        die ->
            %%io:format("Process ~p terminating, stats = ~p\n",[self(),Counters]),
            {message_queue_len,Dropped} = erlang:process_info(self(),message_queue_len),
            _R = ets:update_counter(nif_SUITE,send3,
                               [{2,Rcv},{3,SndOk},{4,SndFail},{5,1-Dropped}]),
            %%io:format("~p: dies R=~p\n", [self(), R]),
            ok;
        {halt,Papa} ->
            Papa ! {halted,self()},
            io:format("~p halted\n",[self()]),
            receive die -> ok end,
            io:format("~p dying\n",[self()])
    end.

send3_proc_send(Pids, {Rcv,SndOk,SndFail}, State0) ->
    To = lists:nth(rand:uniform(length(Pids)),Pids),
    Blob = send3_make_blob(),
    State1 = send3_new_state(State0,Blob), 
    case send3_send(To, Blob) of
        true ->
            send3_proc(Pids, {Rcv,SndOk+1,SndFail}, State1);
        false ->
            send3_proc(Pids, {Rcv,SndOk,SndFail+1}, State1)
    end.


send3_make_blob() ->    
    case rand:uniform(20)-1 of
        0 -> {term,[]};
        N ->
            MsgEnv = alloc_msgenv(), 
            repeat(N bsr 1,
                   fun(_) -> grow_blob(MsgEnv,other_term(),rand:uniform(1 bsl 20))
                   end, void),
            case (N band 3) of
                0 -> {term,copy_blob(MsgEnv)};
                1 -> {copy,copy_blob(MsgEnv)};
                _ -> {msgenv,MsgEnv}
            end
    end.

send3_send(Pid, Msg) ->
    %% 90% enif_send and 10% normal bang
    case rand:uniform(10) of
        1 -> send3_send_bang(Pid,Msg);
        _ -> send3_send_nif(Pid,Msg)
    end.
send3_send_nif(Pid, {term,Blob}) ->
    %%io:format("~p send term nif\n",[self()]),
    send_term(Pid, {blob, Blob}) =:= 1;
send3_send_nif(Pid, {copy,Blob}) ->
    %%io:format("~p send term nif\n",[self()]),
    send_copy_term(Pid, {blob, Blob}) =:= 1;
send3_send_nif(Pid, {msgenv,MsgEnv}) ->
    %%io:format("~p send blob nif\n",[self()]),   
    send3_blob(MsgEnv, Pid, blob) =:= 1.

send3_send_bang(Pid, {term,Blob}) ->
    %%io:format("~p send term bang\n",[self()]),   
    Pid ! {blob, Blob},
    true;
send3_send_bang(Pid, {copy,Blob}) ->
    %%io:format("~p send term bang\n",[self()]),
    Pid ! {blob, Blob},
    true;
send3_send_bang(Pid, {msgenv,MsgEnv}) ->
    %%io:format("~p send blob bang\n",[self()]),   
    Pid ! {blob, copy_blob(MsgEnv)},
    true.

send3_new_state(State, Blob) ->
    case rand:uniform(5+2) of
        N when N =< 5-> setelement(N, State, Blob);
        _ -> State  % Don't store blob
    end.

%% Negative testing of load_nif
neg(Config) when is_list(Config) ->
    TmpMem = tmpmem(),
    {'EXIT',{badarg,_}} = (catch erlang:load_nif(badarg, 0)),
    
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "nif_mod"),
    {ok,nif_mod,Bin} = compile:file(File, [binary,return_errors]),
    {module,nif_mod} = erlang:load_module(nif_mod,Bin),

    {error,{load_failed,_}} = nif_mod:load_nif_lib(Config, 0),
    {error,{bad_lib,_}} = nif_mod:load_nif_lib(Config, no_init),    
    verify_tmpmem(TmpMem),
    ok.

%% Test all enif_is functions
is_checks(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 12),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -12),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 18446744073709551617),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -18446744073709551617),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 99.146),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -99.146),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, 18446744073709551616.2e2),
    ok = check_is(hejsan, <<19,98>>, make_ref(), ok, fun() -> ok end,
			self(), hd(erlang:ports()), [], [1,9,9,8],
			{hejsan, "hejsan", [$h,"ejs",<<"an">>]}, -18446744073709551616.2e2),
    try
        check_is_exception(),
        throw(expected_badarg)
    catch
        error:badarg ->
            ok
    end.

%% Test all enif_get_length functions
get_length(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    ok = length_test(hejsan, "hejsan", [], [], not_a_list, [1,2|3]).

ensure_lib_loaded(Config) ->
    ensure_lib_loaded(Config, 1).
ensure_lib_loaded(Config, Ver) ->
    Path = ?config(data_dir, Config),
    case lib_version() of
        undefined ->
            Lib = "nif_SUITE." ++ integer_to_list(Ver),
            ok = erlang:load_nif(filename:join(Path,Lib), []);
        Ver when is_integer(Ver) ->
            ok
    end,
    erl_ddll:try_load(Path, echo_drv, []),
    ok.

make_atom(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    An0Atom = an0atom,
    An0Atom0 = 'an\000atom\000',
    Atoms = make_atoms(),
    7 = size(Atoms),
    Atoms = {An0Atom,An0Atom,An0Atom,An0Atom0,An0Atom,An0Atom,An0Atom0}.

make_string(Config) when is_list(Config) ->
    ensure_lib_loaded(Config, 1),
    Strings = make_strings(),
    5 = size(Strings),
    A0String = "a0string",
    A0String0 = [$a,0,$s,$t,$r,$i,$n,$g,0],
    AStringWithAccents = [$E,$r,$l,$a,$n,$g,$ ,16#e4,$r,$ ,$e,$t,$t,$ ,$g,$e,$n,$e,$r,$e,$l,$l,$t,$ ,$p,$r,$o,$g,$r,$a,$m,$s,$p,$r,16#e5,$k],
    Strings = {A0String,A0String,A0String,A0String0, AStringWithAccents}.

reverse_list_test(Config) ->
    ensure_lib_loaded(Config, 1),
    List = lists:seq(1,100),
    RevList = lists:reverse(List),
    RevList = reverse_list(List),
    badarg = reverse_list(foo).

%% Memory leak of tmp-buffer when inspecting iolist or unaligned binary in unbound environment
otp_9668(Config) ->
    ensure_lib_loaded(Config, 1),
    TmpMem = tmpmem(),
    IOList = ["This",' ',<<"is">>,' ',[<<"an iolist">>,'.']],    
    otp_9668_nif(IOList),

    <<_:5/bitstring,UnalignedBin:10/binary,_/bitstring>> = <<"Abuse me as unaligned">>,
    otp_9668_nif(UnalignedBin),

    verify_tmpmem(TmpMem),
    ok.

%% Copy of writable binary
otp_9828(Config) ->
    ensure_lib_loaded(Config, 1),
    otp_9828_loop(<<"I'm alive!">>, 1000).

otp_9828_loop(_Bin, 0) ->
    ok;
otp_9828_loop(Bin, Val) ->
    WrtBin = <<Bin/binary, Val:32>>,
    ok = otp_9828_nif(WrtBin),
    otp_9828_loop(WrtBin, Val-1).


consume_timeslice(Config) when is_list(Config) ->
    case {erlang:system_info(debug_compiled),
	  erlang:system_info(lock_checking)} of
	{false, false} ->
	    consume_timeslice_test(Config);
	{false, true} ->
	    {skipped, "Lock checking enabled"};
	_ ->
	    {skipped, "Debug compiled"}
    end.

consume_timeslice_test(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    CONTEXT_REDS = 4000,
    Me = self(),
    Go = make_ref(),
    RedDiff = make_ref(),
    Done = make_ref(),
    DummyMFA = {?MODULE,dummy_call,1},
    P = spawn(fun () ->
                      receive Go -> ok end,
                      {reductions, R1} = process_info(self(), reductions),
                      1 = consume_timeslice_nif(100, false),
                      dummy_call(111),
                      0 = consume_timeslice_nif(90, false),
                      dummy_call(222),
                      1 = consume_timeslice_nif(10, false),
                      dummy_call(333),
                      0 = consume_timeslice_nif(25, false),
                      0 = consume_timeslice_nif(25, false),
                      0 = consume_timeslice_nif(25, false),
                      1 = consume_timeslice_nif(25, false),
                      0 = consume_timeslice_nif(25, false),

                      ok = case consume_timeslice_nif(1, true) of
                               Cnt when Cnt > 70, Cnt < 80 -> ok;
                               Other -> Other
                           end,
                      dummy_call(444),

                      {reductions, R2} = process_info(self(), reductions),
                      Me ! {RedDiff, R2 - R1},
                      exit(Done)
              end),
    erlang:yield(),

    erlang:trace_pattern(DummyMFA, [], [local]),
    1 = erlang:trace(P, true, [call, running, procs, {tracer, self()}]),

    P ! Go,

    %% receive Go -> ok end,
    {trace, P, in, _} = next_tmsg(P),
    
    %% consume_timeslice_nif(100),
    %% dummy_call(111)
    {trace, P, out, _} = next_tmsg(P),
    {trace, P, in, _} = next_tmsg(P),
    {trace, P, call, {?MODULE,dummy_call,[111]}} = next_tmsg(P),

    %% consume_timeslice_nif(90),
    %% dummy_call(222)
    {trace, P, call, {?MODULE,dummy_call,[222]}} = next_tmsg(P),

    %% consume_timeslice_nif(10),
    %% dummy_call(333)
    {trace, P, out, _} = next_tmsg(P),
    {trace, P, in, _} = next_tmsg(P),
    {trace, P, call, {?MODULE,dummy_call,[333]}} = next_tmsg(P),

    %% 25,25,25,25, 25
    {trace, P, out, {?MODULE,consume_timeslice_nif,2}} = next_tmsg(P),
    {trace, P, in, {?MODULE,consume_timeslice_nif,2}} = next_tmsg(P),

    %% consume_timeslice(1,true)
    %% dummy_call(444)
    {trace, P, out, DummyMFA} = next_tmsg(P),
    {trace, P, in, DummyMFA} = next_tmsg(P),
    {trace, P, call, {?MODULE,dummy_call,[444]}} = next_tmsg(P),

    %% exit(Done)
    {trace, P, exit, Done} = next_tmsg(P),

    ExpReds = (100 + 90 + 10 + 25*5 + 75) * CONTEXT_REDS div 100,
    receive
	{RedDiff, Reductions} when Reductions < (ExpReds + 10), Reductions > (ExpReds - 10) ->
	    io:format("Reductions = ~p~n", [Reductions]),
	    ok;
	{RedDiff, Reductions} ->
	    ct:fail({unexpected_reduction_count, Reductions, ExpReds})
    end,
    
    none = next_msg(P),

    ok.

nif_schedule(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    A = "this is a string",
    B = {this,is,a,tuple},
    {B,A} = call_nif_schedule(A, B),
    ok = try call_nif_schedule(1, 2)
	 catch
	     error:badarg:Stk ->
		 [{?MODULE,call_nif_schedule,[1,2],_}|_] = Stk,
		 ok
	 end,
    ok.

nif_exception(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    try
	%% this checks that the expected exception occurs when the NIF
	%% calls enif_make_badarg at some point but then tries to return a
	%% value that isn't an exception
	call_nif_exception(0),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    %% this checks that a NIF can raise various terms as exceptions
    ok = nif_raise_exceptions(call_nif_exception),
    ok.

nif_nan_and_inf(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    try
	call_nif_nan_or_inf(nan),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    try
	call_nif_nan_or_inf(inf),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    try
	call_nif_nan_or_inf(tuple),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end.

nif_atom_too_long(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),
    try
	call_nif_atom_too_long(all),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end,
    try
	call_nif_atom_too_long(len),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    ok
    end.

next_msg(_Pid) ->
    receive
        M -> M
    after 100 ->
              none
    end.

next_tmsg(Pid) ->    
    receive TMsg when is_tuple(TMsg),
                      element(1, TMsg) == trace,
                      element(2, TMsg) == Pid ->
                TMsg
    after 100 ->
              none
    end.

dummy_call(_) ->    
    ok.

tmpmem() ->
    case erlang:system_info({allocator,temp_alloc}) of
	false -> undefined;
	MemInfo ->
	    MSBCS = lists:foldl(
		      fun ({instance, 0, _}, Acc) ->
			      Acc; % Ignore instance 0
			  ({instance, _, L}, Acc) ->
			      {value,{_,MBCS}} = lists:keysearch(mbcs, 1, L),
			      {value,{_,SBCS}} = lists:keysearch(sbcs, 1, L),
			      [MBCS,SBCS | Acc]
		      end,
		      [],
		      MemInfo),
	    lists:foldl(
	      fun(L, {Bl0,BlSz0}) ->
		      {value,{_,Bl,_,_}} = lists:keysearch(blocks, 1, L),
		      {value,{_,BlSz,_,_}} = lists:keysearch(blocks_size, 1, L),
		      {Bl0+Bl,BlSz0+BlSz}
	      end, {0,0}, MSBCS)
    end.

verify_tmpmem(MemInfo) ->
    %%wait_for_test_procs(),
    case tmpmem() of
	MemInfo ->
	    io:format("Tmp mem info: ~p", [MemInfo]),
	    case MemInfo of
		{notsup,undefined} ->
		    %% Use 'erl +Mea max' to do more complete memory leak testing.
		    {comment,"Incomplete or no mem leak testing"};
		_ ->
		    ok
	    end;
	Other ->
            ct:fail("Expected: ~p\nActual:   ~p", [MemInfo, Other])
    end.

call(Pid,Cmd) ->
    %%io:format("~p calling ~p with ~p\n",[self(), Pid, Cmd]),
    Pid ! {self(), Cmd},
    receive
	{Pid,Reply} -> Reply
    end.

receive_any() ->
    receive M -> M end.	     

receive_any(Timeout) ->
    receive M -> M
    after Timeout -> timeout end.

flush() ->
    flush(1).

flush(0) ->
    flush(0, 10);  % don't waste too much time waiting for nothing
flush(N) ->
    flush(N, 1000).

flush(N, Timeout) ->
    receive M ->
            [M | flush(N-1)]
    after Timeout ->
            []
    end.

repeat(0, _, Arg) ->
    Arg;
repeat(N, Fun, Arg0) ->
    repeat(N-1, Fun, Fun(Arg0)).

check(Exp,Got,Line) ->
    case Got of
 	Exp -> Exp;	    
  	_ ->
	    io:format("CHECK at line ~p\nExpected: ~p\nGot     : ~p\n",
                      [Line,Exp,Got]),
 	    Got
    end.

nif_raise_exceptions(NifFunc) ->
    ExcTerms = [{error, test}, "a string", <<"a binary">>,
                42, [1,2,3,4,5], [{p,1},{p,2},{p,3}]],
    lists:foldl(fun(Term, ok) ->
                        try
                            erlang:apply(?MODULE,NifFunc,[Term]),
                            ct:fail({expected,Term})
                        catch
                            error:Term:Stk ->
                                [{?MODULE,NifFunc,[Term],_}|_] = Stk,
                                ok
                        end
                end, ok, ExcTerms).

-define(ERL_NIF_TIME_ERROR, -9223372036854775808).
-define(TIME_UNITS, [second, millisecond, microsecond, nanosecond]).

nif_monotonic_time(_Config) ->
    ?ERL_NIF_TIME_ERROR = monotonic_time(invalid_time_unit),
    mtime_loop(1000000).

mtime_loop(0) ->
    ok;
mtime_loop(N) ->
    chk_mtime(?TIME_UNITS),
    mtime_loop(N-1).

chk_mtime([]) ->
    ok;
chk_mtime([TU|TUs]) ->
    A = erlang:monotonic_time(TU),
    B = monotonic_time(TU),
    C = erlang:monotonic_time(TU),
    try
	true = A =< B,
	true = B =< C
    catch
	_ : _ ->
	    ct:fail({monotonic_time_missmatch, TU, A, B, C})
    end,
    chk_mtime(TUs).

nif_time_offset(_Config) ->
    ?ERL_NIF_TIME_ERROR = time_offset(invalid_time_unit),
    toffs_loop(1000000).

toffs_loop(0) ->
    ok;
toffs_loop(N) ->
    chk_toffs(?TIME_UNITS),
    toffs_loop(N-1).

chk_toffs([]) ->
    ok;
chk_toffs([TU|TUs]) ->
    TO = erlang:time_offset(TU),
    NifTO = time_offset(TU),
    case TO =:= NifTO of
        true ->
            ok;
        false ->
            case erlang:system_info(time_warp_mode) of
                no_time_warp ->
                    ct:fail({time_offset_mismatch, TU, TO, NifTO});
                _ ->
                    %% Most frequent time offset change
                    %% is currently only every 15:th
                    %% second so this should currently
                    %% work...
                    NTO = erlang:time_offset(TU),
                    case NifTO =:= NTO of
                        true ->
                            ok;
                        false ->
                            ct:fail({time_offset_mismatch, TU, TO, NifTO, NTO})
                    end
            end
    end,
    chk_toffs(TUs).

nif_convert_time_unit(_Config) ->
    ?ERL_NIF_TIME_ERROR = convert_time_unit(0, second, invalid_time_unit),
    ?ERL_NIF_TIME_ERROR = convert_time_unit(0, invalid_time_unit, second),
    ?ERL_NIF_TIME_ERROR = convert_time_unit(0, invalid_time_unit, invalid_time_unit),
    lists:foreach(fun (Offset) ->
                          lists:foreach(fun (Diff) ->
                                                chk_ctu(Diff+(Offset*1000*1000*1000))
                                        end,
                                        [999999999999,
                                         99999999999,
                                         9999999999,
                                         999999999,
                                         99999999,
                                         9999999,
                                         999999,
                                         99999,
                                         999,
                                         99,
                                         9,
                                         1,
                                         11,
                                         101,
                                         1001,
                                         10001,
                                         100001,
                                         1000001,
                                         10000001,
                                         100000001,
                                         1000000001,
                                         100000000001,
                                         1000000000001,
                                         5,
                                         50,
                                         500,
                                         5000,
                                         50000,
                                         500000,
                                         5000000,
                                         50000000,
                                         500000000,
                                         5000000000,
                                         50000000000,
                                         500000000000])
                  end,
                  [-4711, -1000, -475, -5, -4, -3, -2, -1, 0,
                   1, 2, 3, 4, 5, 475, 1000, 4711]),
    ctu_loop(1000000).

ctu_loop(0) ->
    ok;
ctu_loop(N) ->
    chk_ctu(erlang:monotonic_time(nanosecond)),
    ctu_loop(N-1).

chk_ctu(Time) ->
    chk_ctu(Time, ?TIME_UNITS).

chk_ctu(_Time, []) ->
    ok;
chk_ctu(Time, [FromTU|FromTUs]) ->
    chk_ctu(Time, FromTU, ?TIME_UNITS),
    chk_ctu(Time, FromTUs).

chk_ctu(_Time, _FromTU, []) ->
    ok;
chk_ctu(Time, FromTU, [ToTU|ToTUs]) ->
    T = erlang:convert_time_unit(Time, nanosecond, FromTU),
    TE = erlang:convert_time_unit(T, FromTU, ToTU),
    TN = convert_time_unit(T, FromTU, ToTU),
    case TE =:= TN of
	false ->
	    ct:fail({conversion_mismatch, FromTU, T, ToTU, TE, TN});
	true ->
	    chk_ctu(Time, FromTU, ToTUs)
    end.

nif_now_time(Config) ->
    ensure_lib_loaded(Config),

    N1 = now(),
    NifN1 = now_time(),
    NifN2 = now_time(),
    N2 = now(),
    true = N1 < NifN1,
    true = NifN1 < NifN2,
    true = NifN2 < N2.

nif_cpu_time(Config) ->
    ensure_lib_loaded(Config),

    try cpu_time() of
        {_, _, _} ->
            ok
    catch error:badarg ->
            {comment, "cpu_time not supported"}
    end.

nif_unique_integer(Config) ->
    ensure_lib_loaded(Config),

    UM1 = erlang:unique_integer([monotonic]),
    UM2 = unique_integer_nif([monotonic]),
    UM3 = erlang:unique_integer([monotonic]),

    true = UM1 < UM2,
    true = UM2 < UM3,

    UMP1 = erlang:unique_integer([monotonic, positive]),
    UMP2 = unique_integer_nif([monotonic, positive]),
    UMP3 = erlang:unique_integer([monotonic, positive]),

    true = 0 =< UMP1,
    true = UMP1 < UMP2,
    true = UMP2 < UMP3,

    UP1 = erlang:unique_integer([positive]),
    UP2 = unique_integer_nif([positive]),
    UP3 = erlang:unique_integer([positive]),

    true = 0 =< UP1,
    true = 0 =< UP2,
    true = 0 =< UP3,

    true = is_integer(unique_integer_nif([])),
    true = is_integer(unique_integer_nif([])),
    true = is_integer(unique_integer_nif([])).

nif_is_process_alive(Config) ->
    ensure_lib_loaded(Config),

    {Pid,_} = spawn_monitor(fun() -> receive ok -> nok end end),
    true = is_process_alive_nif(Pid),
    exit(Pid, die),
    receive _ -> ok end, %% Clear monitor
    false = is_process_alive_nif(Pid).

nif_is_port_alive(Config) ->
    ensure_lib_loaded(Config),

    Port = open_port({spawn,echo_drv},[eof]),
    true = is_port_alive_nif(Port),
    port_close(Port),
    false = is_port_alive_nif(Port).

nif_term_to_binary(Config) ->
    ensure_lib_loaded(Config),
    T = {#{ok => nok}, <<0:8096>>, lists:seq(1,100)},
    Bin = term_to_binary(T),
    ct:log("~p",[Bin]),
    Bin = term_to_binary_nif(T, undefined),
    true = term_to_binary_nif(T, self()),
    receive Bin -> ok end.

-define(ERL_NIF_BIN2TERM_SAFE, 16#20000000).

nif_binary_to_term(Config) ->
    ensure_lib_loaded(Config),
    T = {#{ok => nok}, <<0:8096>>, lists:seq(1,100)},
    Bin = term_to_binary(T),
    Len = byte_size(Bin),
    {Len,T} = binary_to_term_nif(Bin, undefined, 0),
    Len = binary_to_term_nif(Bin, self(), 0),
    T = receive M -> M after 1000 -> timeout end,

    {Len, T} = binary_to_term_nif(Bin, undefined, ?ERL_NIF_BIN2TERM_SAFE),
    false = binary_to_term_nif(<<131,100,0,14,"undefined_atom">>,
			   undefined, ?ERL_NIF_BIN2TERM_SAFE),
    false = binary_to_term_nif(Bin, undefined, 1),
    ok.

nif_port_command(Config) ->
    ensure_lib_loaded(Config),

    Port = open_port({spawn,echo_drv},[eof]),
    true = port_command_nif(Port, "hello\n"),
    receive {Port,{data,"hello\n"}} -> ok
    after 1000 -> ct:fail(timeout) end,

    RefcBin = lists:flatten([lists:duplicate(100, "hello"),"\n"]),
    true = port_command_nif(Port, iolist_to_binary(RefcBin)),
    receive {Port,{data,RefcBin}} -> ok
    after 1000 -> ct:fail(timeout) end,

    %% Test that invalid arguments correctly returns
    %% badarg and that the port survives.
    {'EXIT', {badarg, _}} = (catch port_command_nif(Port, [ok])),

    IoList = [lists:duplicate(100,<<"hello">>),"\n"],
    true = port_command_nif(Port, [IoList]),
    FlatIoList = binary_to_list(iolist_to_binary(IoList)),
    receive {Port,{data,FlatIoList}} -> ok
    after 1000 -> ct:fail(timeout) end,

    port_close(Port),

    {'EXIT', {badarg, _}} = (catch port_command_nif(Port, "hello\n")),
    ok.

nif_snprintf(Config) ->
    ensure_lib_loaded(Config),
    <<"ok",0>> = format_term_nif(3,ok),
    <<"o",0>>  = format_term_nif(2,ok),
    <<"\"hello world\"",0>> = format_term_nif(14,"hello world"),
    <<"{{hello,world,-33},3.14",_/binary>> = format_term_nif(50,{{hello,world, -33}, 3.14, self()}),
    <<"{{hello,world,-33},",0>> = format_term_nif(20,{{hello,world, -33}, 3.14, self()}),
    ok.

nif_internal_hash(Config) ->
    ensure_lib_loaded(Config),
    HashValueBitSize = nif_hash_result_bitsize(internal),
    Terms = unique([random_term() || _ <- lists:seq(1, 500)]),
    HashValues = [hash_nif(internal, Term, 0) || Term <- Terms],
    test_bit_distribution_fitness(HashValues, HashValueBitSize).

nif_internal_hash_salted(Config) ->
    ensure_lib_loaded(Config),
    test_salted_nif_hash(internal).

nif_phash2(Config) ->
    ensure_lib_loaded(Config),
    HashValueBitSize = nif_hash_result_bitsize(phash2),
    Terms = unique([random_term() || _ <- lists:seq(1, 500)]),
    HashValues =
        lists:map(
          fun (Term) ->
                  HashValue = erlang:phash2(Term),
                  Salt = random_uint32(), % phash2 should ignore salt
                  NifHashValue = hash_nif(phash2, Term, Salt),
                  (HashValue =:= NifHashValue
                   orelse ct:fail("Expected: ~p\nActual:   ~p",
                                  [HashValue, NifHashValue])),
                  HashValue
          end,
          Terms),
    test_bit_distribution_fitness(HashValues, HashValueBitSize).

test_salted_nif_hash(HashType) ->
    HashValueBitSize = nif_hash_result_bitsize(HashType),
    Terms = unique([random_term() || _ <- lists:seq(1, 500)]),
    Salts = unique([random_uint32() || _ <- lists:seq(1, 50)]),
    {HashValuesPerSalt, HashValuesPerTerm} =
        lists:mapfoldl(
          fun (Salt, Acc) ->
                  {HashValues, NewAcc} =
                    lists:mapfoldl(
                      fun (Term, AccB) ->
                              HashValue = hash_nif(HashType, Term, Salt),
                              NewAccB = dict:append(Term, HashValue, AccB),
                              {HashValue, NewAccB}
                      end,
                      Acc,
                      Terms),
                  {{Salt, HashValues}, NewAcc}
          end,
          dict:new(),
          Salts),

    % Test per-salt hash distribution of different terms
    lists:foreach(
      fun ({_Salt, HashValues}) ->
              test_bit_distribution_fitness(HashValues, HashValueBitSize)
      end,
      HashValuesPerSalt),

    % Test per-term hash distribution of different salts
    dict:fold(
      fun (_Term, HashValues, Acc) ->
              test_bit_distribution_fitness(HashValues, HashValueBitSize),
              Acc
      end,
      ok,
      HashValuesPerTerm).

test_bit_distribution_fitness(Integers, BitSize) ->
    MaxInteger = (1 bsl BitSize) - 1,
    OnesPerBit =
        lists:foldl(
          fun (Integer, Acc) when Integer >= 0, Integer =< MaxInteger ->
                  lists:foldl(
                    fun (BitIndex, AccB) ->
                            BitValue = (Integer band (1 bsl BitIndex)) bsr BitIndex,
                            orddict:update_counter(BitIndex, BitValue, AccB)
                    end,
                    Acc,
                    lists:seq(0, BitSize - 1))
          end,
          orddict:new(),
          Integers),

    N = length(Integers),
    ExpectedNrOfOnes = N div 2,
    %% ExpectedNrOfOnes should have a binomial distribution
    %% with a standard deviation as:
    ExpectedStdDev = math:sqrt(N) / 2,
    %% which can be approximated as a normal distribution
    %% where we allow a deviation of 6 std.devs
    %% for a fail probability of 0.000000002:
    MaxStdDevs = 6,

    FailureText =
        orddict:fold(
          fun (BitIndex, NrOfOnes, Acc) ->
                  Deviation = abs(NrOfOnes - ExpectedNrOfOnes) / ExpectedStdDev,
                  case Deviation >= MaxStdDevs of
                      false ->
                          Acc;
                      true ->
                          [Acc,
                           io_lib:format(
                             "Unreasonable deviation on number of set bits (i=~p): "
                             "expected ~p, got ~p (# std.dev ~.3f > ~p)~n",
                             [BitIndex, ExpectedNrOfOnes, NrOfOnes, Deviation, MaxStdDevs])]
                  end
          end,
          [],
          OnesPerBit),

    (FailureText =:= [] orelse ct:fail(FailureText)).

nif_hash_result_bitsize(internal) -> 32;
nif_hash_result_bitsize(phash2) -> 27.

unique(List) ->
    lists:usort(List).

random_uint32() ->
    rand:uniform(1 bsl 32) - 1.

random_term() ->
    case rand:uniform(6) of
        1 -> rand:uniform(1 bsl 27) - 1; % small
        2 -> (1 bsl 27) + rand:uniform(1 bsl 128); % big
        3 -> random_sign() * (rand:uniform() * (1 bsl 53)); % float
        4 -> random_binary();
        5 -> random_pid();
        6 ->
            Length = rand:uniform(10),
            List = [random_term() || _ <- lists:seq(1, Length)],
            case rand:uniform(2) of
                1 ->
                   List;
                2 ->
                   list_to_tuple(List)
            end
    end.

random_sign() ->
    case rand:uniform(2) of
        1 -> -1.0;
        2 -> 1.0
    end.

random_binary() ->
    list_to_binary(random_bytes(rand:uniform(32) - 1)).

random_bytes(0) ->
    [];
random_bytes(N) when N > 0 ->
    [rand:uniform(256) - 1 | random_bytes(N - 1)].

random_pid() ->
    Processes = erlang:processes(),
    lists:nth(rand:uniform(length(Processes)), Processes).

%% Test enif_whereis_...

nif_whereis(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),

    RegName = nif_whereis_test_thing,
    undefined = erlang:whereis(RegName),
    false = whereis_term(pid, RegName),

    Mgr = self(),
    Ref = make_ref(),
    ProcMsg = {Ref, ?LINE},
    PortMsg = ?MODULE_STRING " whereis hello\n",

    {Pid, Mon} = spawn_monitor(?MODULE, nif_whereis_proxy, [Ref]),
    true = register(RegName, Pid),
    Pid = erlang:whereis(RegName),
    Pid = whereis_term(pid, RegName),
    false = whereis_term(port, RegName),
    false = whereis_term(pid, [RegName]),

    ok = whereis_send(pid, RegName, {forward, Mgr, ProcMsg}),
    ok = receive ProcMsg -> ok end,

    Pid ! {Ref, quit},
    ok = receive {'DOWN', Mon, process, Pid, normal} -> ok end,
    undefined = erlang:whereis(RegName),
    false = whereis_term(pid, RegName),

    Port = open_port({spawn, echo_drv}, [eof]),
    true = register(RegName, Port),
    Port = erlang:whereis(RegName),
    Port = whereis_term(port, RegName),
    false = whereis_term(pid, RegName),
    false = whereis_term(port, [RegName]),

    ok = whereis_send(port, RegName, PortMsg),
    ok = receive {Port, {data, PortMsg}} -> ok end,

    port_close(Port),
    undefined = erlang:whereis(RegName),
    false = whereis_term(port, RegName),
    ok.

nif_whereis_parallel(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),

    %% try to be at least a little asymetric
    NProcs = trunc(3.7 * erlang:system_info(schedulers)),
    NSeq = lists:seq(1, NProcs),
    Names = [list_to_atom("nif_whereis_proc_" ++ integer_to_list(N))
            || N <- NSeq],
    Mgr = self(),
    Ref = make_ref(),

    NotReg = fun(Name) ->
        erlang:whereis(Name) == undefined
    end,
    PidReg = fun({Name, Pid, _Mon}) ->
        erlang:whereis(Name) == Pid andalso whereis_term(pid, Name) == Pid
    end,
    RecvDown = fun({_Name, Pid, Mon}) ->
        receive {'DOWN', Mon, process, Pid, normal} -> true
        after   1500 -> false end
    end,
    RecvNum = fun(N) ->
        receive {N, Ref} -> true
        after   1500 -> false end
    end,

    true = lists:all(NotReg, Names),

    %% {Name, Pid, Mon}
    Procs = lists:map(
        fun(N) ->
            Name = lists:nth(N, Names),
            Prev = lists:nth((if N == 1 -> NProcs; true -> (N - 1) end), Names),
            Next = lists:nth((if N == NProcs -> 1; true -> (N + 1) end), Names),
            {Pid, Mon} = spawn_monitor(
                ?MODULE, nif_whereis_proxy, [{N, Ref, Mgr, [Prev, Next]}]),
            true = register(Name, Pid),
            {Name, Pid, Mon}
        end, NSeq),

    true = lists:all(PidReg, Procs),

    %% tell them all to 'fire' as fast as we can
    repeat(10, fun(_) ->
                       [P ! {Ref, send_proc} || {_, P, _} <- Procs]
               end, void),

    %% each gets forwarded through two processes
    repeat(10, fun(_) ->
                       true = lists:all(RecvNum, NSeq),
                       true = lists:all(RecvNum, NSeq)
               end, void),

    %% tell them all to 'quit' by name
    [N ! {Ref, quit} || {N, _, _} <- Procs],
    true = lists:all(RecvDown, Procs),
    true = lists:all(NotReg, Names),
    ok.

nif_whereis_threaded(Config) when is_list(Config) ->
    ensure_lib_loaded(Config),

    RegName = nif_whereis_test_threaded,
    undefined = erlang:whereis(RegName),

    Ref = make_ref(),
    {Pid, Mon} = spawn_monitor(?MODULE, nif_whereis_proxy, [Ref]),
    true = register(RegName, Pid),

    {ok, ProcThr} = whereis_thd_lookup(pid, RegName),
    {ok, Pid} = whereis_thd_result(ProcThr),

    Pid ! {Ref, quit},
    ok = receive {'DOWN', Mon, process, Pid, normal} -> ok end,

    Port = open_port({spawn, echo_drv}, [eof]),
    true = register(RegName, Port),

    {ok, PortThr} = whereis_thd_lookup(port, RegName),
    {ok, Port} = whereis_thd_result(PortThr),

    port_close(Port),
    ok.

%% exported to be spawned by MFA by whereis tests
nif_whereis_proxy({N, Ref, Mgr, Targets} = Args) ->
    receive
        {forward, To, Data} ->
            To ! Data,
            nif_whereis_proxy(Args);
        {Ref, quit} ->
            ok;
        {Ref, send_port} ->
            Msg = ?MODULE_STRING " whereis " ++ integer_to_list(N) ++ "\n",
            lists:foreach(
                fun(T) ->
                    ok = whereis_send(port, T, Msg)
                end, Targets),
            nif_whereis_proxy(Args);
        {Ref, send_proc} ->
            lists:foreach(
                fun(T) ->
                    ok = whereis_send(pid, T, {forward, Mgr, {N, Ref}})
                end, Targets),
            nif_whereis_proxy(Args)
    end;
nif_whereis_proxy(Ref) ->
    receive
        {forward, To, Data} ->
            To ! Data,
            nif_whereis_proxy(Ref);
        {Ref, quit} ->
            ok
    end.
nif_ioq(Config) ->
    ensure_lib_loaded(Config),

    Script =
        [{create, a},

         %% Test enq of erlang term binary
         {enqb,   a},
         {enqb,   a, 3},

         %% Test enq of non-erlang term binary
         {enqbraw,a},
         {enqbraw,a, 5},
         {peek,   a},
         {peek_head,  a},
         {deq,    a, 42},

         %% Test enqv
         {enqv,   a, 2, 100},
         {peek_head,  a},
         {deq,    a, all},

         %% This skips all elements but one in the iolist
         {enqv,   a, 5, iolist_size(nif_ioq_payload(5)) - 1},
         {peek_head,  a},
         {peek,   a},

         %% Ensure that enqueued refc binaries are intact after a roundtrip.
         %%
         %% This test and the ones immediately following it does not go through
         %% erlang:iolist_to_iovec/1
         {enqv,   a, [nif_ioq_payload(refcbin) || _ <- lists:seq(1,20)], 0},
         {peek,   a},

         %% ... heap binaries
         {enqv,   a, [nif_ioq_payload(heapbin) || _ <- lists:seq(1,20)], 0},
         {peek,   a},

         %% ... plain sub-binaries
         {enqv,   a, [nif_ioq_payload(subbin) || _ <- lists:seq(1,20)], 0},
         {peek,   a},

         %% ... unaligned binaries
         {enqv,   a, [nif_ioq_payload(unaligned_bin) || _ <- lists:seq(1,20)], 0},
         {peek,   a},

         %% Enq stuff to destroy with data in queue
         {enqv,   a, 2, 100},
         {destroy,a},

         %% Test destroy of new queue
         {create, a},
         {destroy,a}
        ],

    nif_ioq_run(Script),

    %% Test that only enif_inspect_as_vec works
    Payload = nif_ioq_payload(5),
    PayloadBin = iolist_to_binary(Payload),

    [begin
         PayloadBin = iolist_to_binary(ioq_nif(inspect,Payload,Stack,Env)),
         <<>>       = iolist_to_binary(ioq_nif(inspect,[],Stack,Env))
     end || Stack <- [no_stack, use_stack], Env <- [use_env, no_env]],

    %% Test error cases

    Q = ioq_nif(create),

    false = ioq_nif(peek_head, Q),

    {'EXIT', {badarg, _}} = (catch ioq_nif(deq, Q, 1)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, 1, 1234)),

    false = ioq_nif(peek_head, Q),

    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, [atom_in_list], 0)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, [make_ref()], 0)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, [256], 0)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, [-1], 0)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, [#{}], 0)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, [1 bsl 64], 0)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(enqv, Q, [{tuple}], 0)),

    false = ioq_nif(peek_head, Q),

    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  [atom_in_list], use_stack)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  [make_ref()], no_stack)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  [256], use_stack)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  [-1], no_stack)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  [#{}], use_stack)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  [1 bsl 64], no_stack)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  [{tuple}], use_stack)),
    {'EXIT', {badarg, _}} = (catch ioq_nif(inspect,  <<"binary">>, use_stack)),

    ioq_nif(destroy, Q),

    %% Test that the example in the docs works
    ExampleQ = ioq_nif(create),
    true = ioq_nif(example, ExampleQ, nif_ioq_payload(5)),
    ioq_nif(destroy, ExampleQ),

    ok.


nif_ioq_run(Script) ->
    nif_ioq_run(Script, #{}).

nif_ioq_run([{Action, Name}|T], State)
  when Action =:= enqb; Action =:= enqbraw ->
    nif_ioq_run([{Action, Name, heapbin}|T], State);
nif_ioq_run([{Action, Name, Skip}|T], State)
  when Action =:= enqb, is_integer(Skip);
       Action =:= enqbraw, is_integer(Skip) ->
    nif_ioq_run([{Action, Name, heapbin, Skip}|T], State);
nif_ioq_run([{Action, Name, N}|T], State)
  when Action =:= enqv; Action =:= enqb; Action =:= enqbraw ->
    nif_ioq_run([{Action, Name, N, 0}|T], State);
nif_ioq_run([{Action, Name, N, Skip}|T], State)
  when Action =:= enqv; Action =:= enqb; Action =:= enqbraw ->

    #{ q := IOQ, b := B } = Q = maps:get(Name, State),
    true = ioq_nif(size, IOQ) == iolist_size(B),

    %% Sanitize the log output a bit so that it doesn't become too large.
    H = {Action, Name, try iolist_size(N) of Sz -> Sz catch _:_ -> N end, Skip},
    ct:log("~p", [H]),

    Data = nif_ioq_payload(N),
    ioq_nif(Action, IOQ, Data, Skip),

    <<_:Skip/binary, SkippedData/binary>> = iolist_to_binary(Data),

    true = ioq_nif(size, IOQ) == (iolist_size([B|SkippedData])),

    nif_ioq_run(T, State#{ Name := Q#{ b := [B|SkippedData]}});
nif_ioq_run([{peek, Name} = H|T], State) ->
    #{ q := IOQ, b := B } = maps:get(Name, State),
    true = ioq_nif(size, IOQ) == iolist_size(B),

    ct:log("~p", [H]),

    Data = ioq_nif(peek, IOQ, ioq_nif(size, IOQ)),

    true = iolist_to_binary(B) == iolist_to_binary(Data),
    nif_ioq_run(T, State);
nif_ioq_run([{peek_head, Name} = H|T], State) ->
    #{ q := IOQ, b := B } = maps:get(Name, State),
    RefData = iolist_to_binary(B),

    ct:log("~p", [H]),

    {true, QueueHead} = ioq_nif(peek_head, IOQ),
    true = byte_size(QueueHead) > 0,

    {RefHead, _Tail} = split_binary(RefData, byte_size(QueueHead)),

    true = QueueHead =:= RefHead,

    nif_ioq_run(T, State);
nif_ioq_run([{deq, Name, all}|T], State) ->
    #{ q := IOQ, b := B } = maps:get(Name, State),
    Size = ioq_nif(size, IOQ),
    true = Size == iolist_size(B),
    nif_ioq_run([{deq, Name, Size}|T], State);
nif_ioq_run([{deq, Name, N} = H|T], State) ->
    #{ q := IOQ, b := B } = Q = maps:get(Name, State),
    true = ioq_nif(size, IOQ) == iolist_size(B),

    ct:log("~p", [H]),

    <<_:N/binary,Remain/binary>> = iolist_to_binary(B),
    NewQ = Q#{ b := Remain },

    Sz = ioq_nif(deq, IOQ, N),

    true = Sz == iolist_size(Remain),
    true = ioq_nif(size, IOQ) == iolist_size(Remain),

    nif_ioq_run(T, State#{ Name := NewQ });
nif_ioq_run([{create, Name} = H|T], State) ->
    ct:log("~p", [H]),
    nif_ioq_run(T, State#{ Name => #{ q => ioq_nif(create), b => [] } });
nif_ioq_run([{destroy, Name} = H|T], State) ->
    #{ q := IOQ, b := B } = maps:get(Name, State),
    true = ioq_nif(size, IOQ) == iolist_size(B),

    ct:log("~p", [H]),

    ioq_nif(destroy, IOQ),

    nif_ioq_run(T, maps:remove(Name, State));
nif_ioq_run([], State) ->
    State.

nif_ioq_payload(N) when is_integer(N) ->
    Tail = if N > 3 -> nif_ioq_payload(N-3); true -> [] end,
    Head = element(1, lists:split(N,[nif_ioq_payload(subbin),
                                     nif_ioq_payload(heapbin),
                                     nif_ioq_payload(refcbin),
                                     nif_ioq_payload(unaligned_bin) | Tail])),
    erlang:iolist_to_iovec(Head);
nif_ioq_payload(subbin) ->
    Bin = nif_ioq_payload(refcbin),
    Sz = size(Bin) - 1,
    <<_:8,SubBin:Sz/binary,_/bits>> = Bin,
    SubBin;
nif_ioq_payload(unaligned_bin) ->
    make_unaligned_binary(<< <<I>> || I <- lists:seq(1, 255) >>);
nif_ioq_payload(heapbin) ->
    <<"a literal heap binary">>;
nif_ioq_payload(refcbin) ->
    iolist_to_binary([lists:seq(1,255) || _ <- lists:seq(1,255)]);
nif_ioq_payload(Else) ->
    Else.

make_unaligned_binary(Bin0) ->
    Size = byte_size(Bin0),
    <<0:3,Bin:Size/binary,31:5>> = id(<<0:3,Bin0/binary,31:5>>),
    Bin.

id(I) -> I.

%% The NIFs:
lib_version() -> undefined.
call_history() -> ?nif_stub.
hold_nif_mod_priv_data(_Ptr) -> ?nif_stub.
nif_mod_call_history() -> ?nif_stub.
list_seq(_To) -> ?nif_stub.
type_test() -> ?nif_stub.
tuple_2_list(_) -> ?nif_stub.    
is_identical(_,_) -> ?nif_stub.
compare(_,_) -> ?nif_stub.
hash_nif(_Type, _Term, _Salt) -> ?nif_stub.
many_args_100(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> ?nif_stub.
clone_bin(_) -> ?nif_stub.
make_sub_bin(_,_,_) -> ?nif_stub.
string_to_bin(_,_) -> ?nif_stub.
atom_to_bin(_,_) -> ?nif_stub.    
macros(_) -> ?nif_stub.
tuple_2_list_and_tuple(_) -> ?nif_stub.
iolist_2_bin(_) -> ?nif_stub.
get_resource_type(_) -> ?nif_stub.
alloc_resource(_,_) -> ?nif_stub.
make_resource(_) -> ?nif_stub.
get_resource(_,_) -> ?nif_stub.
release_resource(_) -> ?nif_stub.
release_resource_from_thread(_) -> ?nif_stub.
last_resource_dtor_call() -> ?nif_stub.
make_new_resource(_,_) -> ?nif_stub.
check_is(_,_,_,_,_,_,_,_,_,_,_) -> ?nif_stub.
check_is_exception() -> ?nif_stub.
length_test(_,_,_,_,_,_) -> ?nif_stub.
make_atoms() -> ?nif_stub.
make_strings() -> ?nif_stub.
make_new_resource_binary(_) -> ?nif_stub.
send_list_seq(_,_) -> ?nif_stub.     
send_new_blob(_,_) -> ?nif_stub.     
alloc_msgenv() -> ?nif_stub.
clear_msgenv(_) -> ?nif_stub.
grow_blob(_,_) -> ?nif_stub.
grow_blob(_,_,_) -> ?nif_stub.
send_blob(_,_) -> ?nif_stub.
send3_blob(_,_,_) -> ?nif_stub.
send_blob_thread(_,_,_) -> ?nif_stub.
join_send_thread(_) -> ?nif_stub.
copy_blob(_) -> ?nif_stub.
send_term(_,_) -> ?nif_stub.
send_copy_term(_,_) -> ?nif_stub.
reverse_list(_) -> ?nif_stub.
echo_int(_) -> ?nif_stub.
type_sizes() -> ?nif_stub.
otp_9668_nif(_) -> ?nif_stub.
otp_9828_nif(_) -> ?nif_stub.
consume_timeslice_nif(_,_) -> ?nif_stub.
call_nif_schedule(_,_) -> ?nif_stub.
call_nif_exception(_) -> ?nif_stub.
call_nif_nan_or_inf(_) -> ?nif_stub.
call_nif_atom_too_long(_) -> ?nif_stub.
unique_integer_nif(_) -> ?nif_stub.
is_process_alive_nif(_) -> ?nif_stub.
is_port_alive_nif(_) -> ?nif_stub.
term_to_binary_nif(_, _) -> ?nif_stub.
binary_to_term_nif(_, _, _) -> ?nif_stub.
port_command_nif(_, _) -> ?nif_stub.
format_term_nif(_,_) -> ?nif_stub.
select_nif(_,_,_,_,_) -> ?nif_stub.
dupe_resource_nif(_) -> ?nif_stub.
pipe_nif() -> ?nif_stub.
write_nif(_,_) -> ?nif_stub.
read_nif(_,_) -> ?nif_stub.
is_closed_nif(_) -> ?nif_stub.
clear_select_nif(_) -> ?nif_stub.
last_fd_stop_call() -> ?nif_stub.
alloc_monitor_resource_nif() -> ?nif_stub.
monitor_process_nif(_,_,_,_) -> ?nif_stub.
demonitor_process_nif(_,_) -> ?nif_stub.
compare_monitors_nif(_,_) -> ?nif_stub.
monitor_frenzy_nif(_,_,_,_) -> ?nif_stub.
ioq_nif(_) -> ?nif_stub.
ioq_nif(_,_) -> ?nif_stub.
ioq_nif(_,_,_) -> ?nif_stub.
ioq_nif(_,_,_,_) -> ?nif_stub.

%% whereis
whereis_send(_Type,_Name,_Msg) -> ?nif_stub.
whereis_term(_Type,_Name) -> ?nif_stub.
whereis_thd_lookup(_Type,_Name) -> ?nif_stub.
whereis_thd_result(_Thd) -> ?nif_stub.

%% maps
is_map_nif(_) -> ?nif_stub.
get_map_size_nif(_) -> ?nif_stub.
make_new_map_nif() -> ?nif_stub.
make_map_put_nif(_,_,_) -> ?nif_stub.
get_map_value_nif(_,_) -> ?nif_stub.
make_map_update_nif(_,_,_) -> ?nif_stub.
make_map_remove_nif(_,_) -> ?nif_stub.
maps_from_list_nif(_) -> ?nif_stub.
sorted_list_from_maps_nif(_) -> ?nif_stub.

%% Time
monotonic_time(_) -> ?nif_stub.
time_offset(_) -> ?nif_stub.
convert_time_unit(_,_,_) -> ?nif_stub.
now_time() -> ?nif_stub.
cpu_time() -> ?nif_stub.

nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).
