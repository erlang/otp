%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2025. All Rights Reserved.
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
-module(test_lib).

-include_lib("common_test/include/ct.hrl").
-compile({no_auto_import,[binary_part/2]}).
-export([id/1,recompile/1,recompile_core/1,parallel/0,
         uniq/0,opt_opts/1,get_data_dir/1,
         smoke_disasm/1,
         p_run/2,
         highest_opcode/1,
         get_unique_files/1,get_unique_files/2]).

%% Used by test case that override BIFs.
-export([binary_part/2,binary/1]).

id(I) -> I.

recompile(Mod) when is_atom(Mod) ->
    case whereis(cover_server) of
 	undefined -> ok;
	_ ->
	    %% Re-compile the test suite if the cover server is running.
	    Beam = code:which(Mod),
	    Src = filename:rootname(Beam, ".beam") ++ ".erl",
	    Opts = [bin_opt_info,recv_opt_info|opt_opts(Mod)],
	    io:format("Recompiling ~p (~p)\n", [Mod,Opts]),
	    c:c(Src, [{outdir,filename:dirname(Src)}|Opts])
    end,

    %% Smoke-test of beam disassembler.
    smoke_disasm(Mod).

recompile_core(Mod) when is_atom(Mod) ->
    case whereis(cover_server) of
        undefined -> ok;
        _ ->
            %% Re-compile the test suite if the cover server is running.
            Beam = code:which(Mod),
            Src = filename:rootname(Beam, ".beam"),
            Opts = [bin_opt_info,recv_opt_info|opt_opts(Mod)],
            io:format("Recompiling ~p (~p)\n", [Mod,Opts]),
            c:c(Src, [from_core,{outdir,filename:dirname(Src)}|Opts])
    end,

    %% Smoke-test of beam disassembler.
    smoke_disasm(Mod).

smoke_disasm(Mod) when is_atom(Mod) ->
    smoke_disasm(code:which(Mod));
smoke_disasm(File) when is_list(File) ->
    Res = beam_disasm:file(File),
    {beam_file,_Mod} = {element(1, Res),element(2, Res)}.

parallel() ->
    case erlang:system_info(schedulers) =:= 1 of
	true -> [];
	false -> [parallel]
    end.

uniq() ->
    U = erlang:unique_integer([positive]),
    "_" ++ integer_to_list(U).

%% Retrieve the "interesting" compiler options (options for optimization
%% and compatibility) for the given module.

opt_opts(Mod) ->
    Comp = Mod:module_info(compile),
    %% `options` may not be set at all if +deterministic is enabled.
    Opts = proplists:get_value(options, Comp, []),
    lists:filter(fun
                     (beam_debug_info) -> true;
                     (debug_info) -> true;
                     (dialyzer) -> true;
                     ({feature,_,enable}) -> true;
                     ({feature,_,disable}) -> true;
                     (inline) -> true;
                     (line_coverage) -> true;
                     (no_badrecord) -> true;
                     (no_bool_opt) -> true;
                     (no_bs_create_bin) -> true;
                     (no_bsm_opt) -> true;
                     (no_bs_match) -> true;
                     (no_copt) -> true;
                     (no_fun_opt) -> true;
                     (no_min_max_bifs) -> true;
                     (no_module_opt) -> true;
                     (no_postopt) -> true;
                     (no_recv_opt) -> true;
                     (no_share_opt) -> true;
                     (no_ssa_opt_float) -> true;
                     (no_ssa_opt_ranges) -> true;
                     (no_ssa_opt) -> true;
                     (no_stack_trimming) -> true;
                     (no_type_opt) -> true;
                     (_) -> false
                end, Opts).

%% Some test suites gets cloned (e.g. to "record_SUITE" to
%% "record_no_opt_SUITE"), but the data directory is not cloned.
%% This function retrieves the path to the original data directory.

get_data_dir(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Opts = [{return,list}],
    Suffixes = ["_no_opt_SUITE",
                "_no_bool_opt_SUITE",
                "_no_copt_SUITE",
                "_no_copt_ssa_SUITE",
                "_post_opt_SUITE",
                "_inline_SUITE",
                "_no_module_opt_SUITE",
                "_no_type_opt_SUITE",
                "_no_ssa_opt_SUITE",
                "_cover_SUITE"],
    lists:foldl(fun(Suffix, Acc) ->
                        Opts = [{return,list}],
                        re:replace(Acc, Suffix, "_SUITE", Opts)
                end, Data, Suffixes).

%% Test whether the module is cloned. We don't consider modules
%% compiled with compatibility for an older release cloned (that
%% will improve coverage).

is_cloned("_no_opt_SUITE") -> true;
is_cloned("_no_bool_opt_SUITE") -> true;
is_cloned("_no_copt_SUITE") -> true;
is_cloned("_no_copt_ssa_SUITE") -> true;
is_cloned("_no_ssa_opt_SUITE") -> true;
is_cloned("_no_type_opt_SUITE") -> true;
is_cloned("_post_opt_SUITE") -> true;
is_cloned("_inline_SUITE") -> true;
is_cloned("_no_module_opt_SUITE") -> true;
is_cloned("_cover_SUITE") -> true;
is_cloned([_|T]) -> is_cloned(T);
is_cloned([]) -> false.

%% Return the highest opcode use in the BEAM module.

highest_opcode(Beam) ->
    {ok,{_Mod,[{"Code",Code}]}} = beam_lib:chunks(Beam, ["Code"]),
    FormatNumber = 0,
    <<16:32,FormatNumber:32,HighestOpcode:32,_/binary>> = Code,
    HighestOpcode.

%% Get all unique files in the test case directory.
get_unique_files(Ext) ->
    get_unique_files(Ext, fun(_ModString) -> false end).

get_unique_files(Ext, IsCloned) when is_function(IsCloned, 1) ->
    Wc = filename:join(filename:dirname(code:which(?MODULE)), "*"++Ext),
    [F || F <- filelib:wildcard(Wc),
	  not is_cloned(F, Ext, IsCloned), not is_lfe_module(F, Ext)].

is_cloned(File, Ext, IsCloned) ->
    ModString = filename:basename(File, Ext),
    is_cloned(ModString) orelse IsCloned(ModString).

is_lfe_module(File, Ext) ->
    case filename:basename(File, Ext) of
	"lfe_" ++ _ -> true;
	_ -> false
    end.

%% p_run(fun(Data) -> ok|error, List) -> ok
%%  Will fail the test case if there were any errors.

p_run(Test, List) ->
    case erlang:system_info(wordsize) of
        4 ->
            %% A 32-bit system. If this is Windows, only 2 GiB of
            %% virtual address space is available. In an attempt to
            %% avoid fragmenting the memory, we don't spawn any new
            %% processes, and instead run the tests sequentially in
            %% the same process.
            s_run(Test, List);
        8 ->
            %% Limit the number of parallel processes to avoid running
            %% out of virtual address space or memory.
            N = min(erlang:system_info(schedulers), 8),
            p_run(Test, List, N)
    end.

p_run(Test, List, N) ->
    io:format("p_run: ~p parallel processes; ~p jobs\n",
              [N,length(List)]),
    p_run_loop(Test, List, N, [], 0, 0).

p_run_loop(_, [], _, [], Errors, Ws) ->
    run_result(Errors, Ws);
p_run_loop(Test, [H|T], N, Refs, Errors, Ws) when length(Refs) < N ->
    {_,Ref} = erlang:spawn_monitor(fun() -> exit(Test(H)) end),
    p_run_loop(Test, T, N, [Ref|Refs], Errors, Ws);
p_run_loop(Test, List, N, Refs0, Errors0, Ws0) ->
    receive
	{'DOWN',Ref,process,_,Res} ->
	    {Errors,Ws} = case Res of
			      ok -> {Errors0,Ws0};
			      error -> {Errors0+1,Ws0};
			      warning -> {Errors0,Ws0+1}
			  end,
	    Refs = Refs0 -- [Ref],
	    p_run_loop(Test, List, N, Refs, Errors, Ws)
    end.

s_run(Test, List) ->
    io:format("p_run: a single process; ~p jobs\n",
              [length(List)]),
    s_run_loop(Test, List, 0, 0).

s_run_loop(_Test, [], Errors, Ws) ->
    run_result(Errors, Ws);
s_run_loop(Test, [H|T], Errors, Ws)  ->
    case Test(H) of
        ok ->
            s_run_loop(Test, T, Errors, Ws);
        error ->
            s_run_loop(Test, T, Errors + 1, Ws);
        warning ->
            s_run_loop(Test, T, Errors, Ws + 1)
    end.

run_result(Errors, Ws) ->
    case Errors of
	0 ->
	    case Ws of
		0 -> ok;
		1 -> {comment,"1 warning"};
		N -> {comment,integer_to_list(N)++" warnings"}
	    end;
	N ->
	    ct:fail({N,errors})
    end.

%% This is for the misc_SUITE:override_bif testcase
binary_part(_A,_B) ->
    dummy.

%% This is for overridden_bif_SUITE.
binary(N) ->
    N rem 10 =:= 0.
