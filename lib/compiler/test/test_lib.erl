%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
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

-export([recompile/1,opt_opts/1,get_data_dir/1,smoke_disasm/1]).

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
    {beam_file,Mod} = {element(1, Res),element(2, Res)}.

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

%% Some test suites gets cloned (e.g. to "record_SUITE" to "record_no_opt_SUITE"),
%% but the data directory is not cloned. This function retrieves the path to
%% the original data directory.

get_data_dir(Config) ->
    Data0 = ?config(data_dir, Config),
    {ok,Data1,_} = regexp:sub(Data0, "_no_opt_SUITE", "_SUITE"),
    {ok,Data2,_} = regexp:sub(Data1, "_post_opt_SUITE", "_SUITE"),
    {ok,Data,_} = regexp:sub(Data2, "_inline_SUITE", "_SUITE"),
    Data.
