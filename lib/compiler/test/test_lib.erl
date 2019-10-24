%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
         is_cloned_mod/1,smoke_disasm/1,p_run/2,
         highest_opcode/1]).

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
	    Opts = [bin_opt_info|opt_opts(Mod)],
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
            Opts = [bin_opt_info|opt_opts(Mod)],
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
    {options,Opts} = lists:keyfind(options, 1, Comp),
    lists:filter(fun
                     (debug_info) -> true;
                     (dialyzer) -> true;
                     (inline) -> true;
                     (no_bsm3) -> true;
                     (no_bsm_opt) -> true;
                     (no_copt) -> true;
                     (no_fun_opt) -> true;
                     (no_module_opt) -> true;
                     (no_postopt) -> true;
                     (no_put_tuple2) -> true;
                     (no_recv_opt) -> true;
                     (no_share_opt) -> true;
                     (no_shared_fun_wrappers) -> true;
                     (no_ssa_float) -> true;
                     (no_ssa_opt) -> true;
                     (no_stack_trimming) -> true;
                     (no_swap) -> true;
                     (no_type_opt) -> true;
                     (_) -> false
                end, Opts).

%% Some test suites gets cloned (e.g. to "record_SUITE" to
%% "record_no_opt_SUITE"), but the data directory is not cloned.
%% This function retrieves the path to the original data directory.

get_data_dir(Config) ->
    Data0 = proplists:get_value(data_dir, Config),
    Opts = [{return,list}],
    Data1 = re:replace(Data0, "_no_opt_SUITE", "_SUITE", Opts),
    Data2 = re:replace(Data1, "_post_opt_SUITE", "_SUITE", Opts),
    Data3 = re:replace(Data2, "_inline_SUITE", "_SUITE", Opts),
    Data4 = re:replace(Data3, "_r21_SUITE", "_SUITE", Opts),
    Data5 = re:replace(Data4, "_no_module_opt_SUITE", "_SUITE", Opts),
    Data = re:replace(Data5, "_no_type_opt_SUITE", "_SUITE", Opts),
    re:replace(Data, "_no_ssa_opt_SUITE", "_SUITE", Opts).

is_cloned_mod(Mod) ->
    is_cloned_mod_1(atom_to_list(Mod)).

%% Test whether Mod is a cloned module.

is_cloned_mod_1("_no_opt_SUITE") -> true;
is_cloned_mod_1("_no_ssa_opt_SUITE") -> true;
is_cloned_mod_1("_post_opt_SUITE") -> true;
is_cloned_mod_1("_inline_SUITE") -> true;
is_cloned_mod_1("_21_SUITE") -> true;
is_cloned_mod_1("_no_module_opt_SUITE") -> true;
is_cloned_mod_1([_|T]) -> is_cloned_mod_1(T);
is_cloned_mod_1([]) -> false.

%% Return the highest opcode use in the BEAM module.

highest_opcode(Beam) ->
    {ok,{_Mod,[{"Code",Code}]}} = beam_lib:chunks(Beam, ["Code"]),
    FormatNumber = 0,
    <<16:32,FormatNumber:32,HighestOpcode:32,_/binary>> = Code,
    HighestOpcode.

%% p_run(fun(Data) -> ok|error, List) -> ok
%%  Will fail the test case if there were any errors.

p_run(Test, List) ->
    S = erlang:system_info(schedulers),
    N = S + 1,
    io:format("p_run: ~p parallel processes\n", [N]),
    p_run_loop(Test, List, N, [], 0, 0).

p_run_loop(_, [], _, [], Errors, Ws) ->
    case Errors of
	0 ->
	    case Ws of
		0 -> ok;
		1 -> {comment,"1 warning"};
		N -> {comment,integer_to_list(N)++" warnings"}
	    end;
	N ->
	    ct:fail({N,errors})
    end;
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

%% This is for the misc_SUITE:override_bif testcase
binary_part(_A,_B) ->
    dummy.

%% This is for overridden_bif_SUITE.
binary(N) ->
    N rem 10 =:= 0.
