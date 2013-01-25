%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2013. All Rights Reserved.
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
-module(test_lib).

-include("test_server.hrl").
-compile({no_auto_import,[binary_part/2]}).
-export([recompile/1,parallel/0,uniq/0,opt_opts/1,get_data_dir/1,
	 smoke_disasm/1,p_run/2,binary_part/2]).

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

smoke_disasm(Mod) when is_atom(Mod) ->
    smoke_disasm(code:which(Mod));
smoke_disasm(File) when is_list(File) ->
    Res = beam_disasm:file(File),
    {beam_file,_Mod} = {element(1, Res),element(2, Res)}.

parallel() ->
    case ?t:is_cover() orelse erlang:system_info(schedulers) =:= 1 of
	true -> [];
	false -> [parallel]
    end.

uniq() ->
    U0 = erlang:ref_to_list(make_ref()),
    U1 = re:replace(U0, "^#Ref", ""),
    U = re:replace(U1, "[^[A-Za-z0-9_]+", "_", [global]),
    re:replace(U, "_*$", "", [{return,list}]).

%% Retrieve the "interesting" compiler options (options for optimization
%% and compatibility) for the given module.

opt_opts(Mod) ->
    Comp = Mod:module_info(compile),
    {value,{options,Opts}} = lists:keysearch(options, 1, Comp),
    lists:filter(fun(no_copt) -> true;
		    (no_postopt) -> true;
		    (no_float_opt) -> true;
		    (no_new_funs) -> true;
		    (no_new_binaries) -> true;
		    (no_new_apply) -> true;
		    (no_gc_bifs) -> true;
		    (no_stack_trimming) -> true;
		    (debug_info) -> true;
		    (inline) -> true;
		    (_) -> false
		 end, Opts).

%% Some test suites gets cloned (e.g. to "record_SUITE" to
%% "record_no_opt_SUITE"), but the data directory is not cloned.
%% This function retrieves the path to the original data directory.

get_data_dir(Config) ->
    Data0 = ?config(data_dir, Config),
    Opts = [{return,list}],
    Data1 = re:replace(Data0, "_no_opt_SUITE", "_SUITE", Opts),
    Data = re:replace(Data1, "_post_opt_SUITE", "_SUITE", Opts),
    re:replace(Data, "_inline_SUITE", "_SUITE", Opts).

%% p_run(fun(Data) -> ok|error, List) -> ok
%%  Will fail the test case if there were any errors.

p_run(Test, List) ->
    N = case ?t:is_cover() of
	    false ->
		erlang:system_info(schedulers);
	    true ->
		%% Cover is running. Using more than one process
		%% will probably only slow down compilation.
		1
	end,
    p_run_loop(Test, List, N, [], 0, 0).

p_run_loop(_, [], _, [], Errors, Ws) ->
    case Errors of
	0 ->
	    case Ws of
		0 -> ok;
		1 -> {comment,"1 warning"};
		N -> {comment,integer_to_list(N)++" warnings"}
	    end;
	N -> ?t:fail({N,errors})
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
