%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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
-module(misc_SUITE).

-export([all/1,init_per_testcase/2,fin_per_testcase/2,
	 tobias/1,empty_string/1,md5/1,silly_coverage/1,
	 confused_literals/1,integer_encoding/1]).
	 
-include("test_server.hrl").

%% Include an opaque declaration to cover the stripping of
%% opaque types from attributes in v3_kernel.
-opaque misc_SUITE_test_cases() :: [atom()].

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = test_server:timetrap(?t:minutes(10)),
    [{watchdog,Dog}|Config].

fin_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

-spec all(any()) -> misc_SUITE_test_cases().

all(suite) ->
    test_lib:recompile(?MODULE),
    [tobias,empty_string,md5,silly_coverage,confused_literals,
     integer_encoding].

%% A bug reported by Tobias Lindahl for a development version of R11B.

tobias(Config) when is_list(Config) ->
    ?line 1 = tobias_1([1,2,3]),
    ok.

tobias_1([H|_T]) ->
    %% In an R11B compiler, the move optimizer in beam_block would
    %% confuse H and _T.
    tobias_2(0, 0),
    H.

tobias_2(_, _) ->
    2.


%% A bug reported by Richard Carlsson. Used to crash beam_asm
%% because of a put_string instruction with an empty string.
%% The real problem was in sys_core_fold (empty strings should
%% be replaced by []).

-record(r, {s = ""}).

empty_string(Config) when is_list(Config) ->
    ?line #r{s="x"} = empty_string_1(#r{}),
    ok.

empty_string_1(T) ->
    case T of
	#r{s = ""} -> T #r{s = "x"}
    end.

md5(Config) when is_list(Config) ->
    case ?MODULE of
	misc_SUITE -> md5();
	_ -> {skip,"Enough to run this case once."}
    end.

md5() ->
    ?line Dir = filename:dirname(code:which(?MODULE)),
    ?line Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
    ?line io:format("Found ~w beam files", [length(Beams)]),
    ?line lists:foreach(fun md5_1/1, Beams).

md5_1(Beam) ->
    ?line {ok,{Mod,[Vsn]}} = beam_lib:version(Beam),
    ?line {ok,Code} = file:read_file(Beam),
    ?line {Mod,<<Vsn:128>>} = {Mod,code:module_md5(Code)}.

%% Cover some code that handles internal errors.

silly_coverage(Config) when is_list(Config) ->
    %% sys_core_fold, sys_core_setel, v3_kernel
    BadCoreErlang = {c_module,[],
		     name,[],[],
		     [{{c_var,[],{foo,2}},seriously_bad_body}]},
    ?line expect_error(fun() -> sys_core_fold:module(BadCoreErlang, []) end),
    ?line expect_error(fun() -> sys_core_dsetel:module(BadCoreErlang, []) end),
    ?line expect_error(fun() -> v3_kernel:module(BadCoreErlang, []) end),

    %% v3_life
    BadKernel = {k_mdef,[],?MODULE,
		 [{foo,0}],
		 [],
		 [{k_fdef,
		   {k,[],[],[]},
		   f,0,[],
		   seriously_bad_body}]},
    ?line expect_error(fun() -> v3_life:module(BadKernel, []) end),

    %% v3_codegen
    CodegenInput = {?MODULE,[{foo,0}],[],[{function,foo,0,[a|b],a,b}]},
    ?line expect_error(fun() -> v3_codegen:module(CodegenInput, []) end),

    %% beam_block
    BlockInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list],99}]},
    ?line expect_error(fun() -> beam_block:module(BlockInput, []) end),

    %% beam_bool
    BoolInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    ?line expect_error(fun() -> beam_bool:module(BoolInput, []) end),

    %% beam_dead
    DeadInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2},
		     {jump,bad}]}],99},
    ?line expect_error(fun() -> beam_block:module(DeadInput, []) end),

    %% beam_clean
    CleanInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2},
		     {jump,{f,42}}]}],99},
    ?line expect_error(fun() -> beam_clean:module(CleanInput, []) end),

    %% beam_peep
    PeepInput = {?MODULE,[{foo,0}],[],
		 [{function,foo,0,2,
		   [{label,1},
		    {func_info,{atom,?MODULE},{atom,foo},0},
		    {label,2}|non_proper_list]}],99},
    ?line expect_error(fun() -> beam_peep:module(PeepInput, []) end),

    %% beam_bsm. This is tricky. Our function must be sane enough to not crash
    %% btb_index/1, but must crash the main optimization pass.
    BsmInput = {?MODULE,[{foo,0}],[],
		[{function,foo,0,2,
		  [{label,1},
		   {func_info,{atom,?MODULE},{atom,foo},0},
		   {label,2},
		   {test,bs_get_binary2,{f,99},0,[{x,0},{atom,all},1,[]],{x,0}},
		   {block,[a|b]}]}],0},
    ?line expect_error(fun() -> beam_bsm:module(BsmInput, []) end),

    %% beam_receive.
    ReceiveInput = {?MODULE,[{foo,0}],[],
		    [{function,foo,0,2,
		      [{label,1},
		       {func_info,{atom,?MODULE},{atom,foo},0},
		       {label,2},
		       {call_ext,0,{extfunc,erlang,make_ref,0}},
		       {block,[a|b]}]}],0},
    ?line expect_error(fun() -> beam_receive:module(ReceiveInput, []) end),

    ok.

expect_error(Fun) ->
    try	Fun() of
	Any ->
	    io:format("~p", [Any]),
	    ?t:fail(call_was_supposed_to_fail)
    catch
	_:_ ->
	    io:format("~p\n", [erlang:get_stacktrace()])
    end.

confused_literals(Config) when is_list(Config) ->
    ?line {0,infinity} = confused_literals_1(int),
    ?line {0.0,infinity} = confused_literals_1(float),
    ok.

confused_literals_1(int) -> {0,infinity};
confused_literals_1(float) -> {0.0,infinity}.

integer_encoding(Config) when is_list(Config) ->
    case ?MODULE of
	misc_SUITE -> integer_encoding_1(Config);
	_ -> {skip,"Enough to run this case once."}
    end.

integer_encoding_1(Config) ->
    Dog = test_server:timetrap(?t:minutes(4)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line SrcFile = filename:join(PrivDir, "misc_SUITE_integer_encoding.erl"),
    ?line DataFile = filename:join(PrivDir, "integer_encoding.data"),
    Mod = misc_SUITE_integer_encoding,

    %% Create files.
    ?line {ok,Src} = file:open(SrcFile, [write]),
    ?line {ok,Data} = file:open(DataFile, [write]),
    io:format(Src, "-module(~s).\n", [Mod]),
    io:put_chars(Src, "-export([t/1]).\n"),
    io:put_chars(Src, "t(Last) ->[\n"),
    io:put_chars(Data, "[\n"),

    ?line do_integer_encoding(-(id(1) bsl 10000), Src, Data),
    ?line do_integer_encoding(id(1) bsl 10000, Src, Data),
    ?line do_integer_encoding(2048, 0, Src, Data),

    io:put_chars(Src, "Last].\n\n"),
    ?line ok = file:close(Src),
    io:put_chars(Data, "0].\n\n"),
    ?line ok = file:close(Data),

    %% Compile and load Erlang module.
    ?line SrcRoot = filename:rootname(SrcFile),
    ?line {ok,Mod,Binary} = compile:file(SrcRoot, [binary,report]),
    ?line {module,Mod} = code:load_binary(Mod, SrcRoot, Binary),

    %% Compare lists.
    ?line List = Mod:t(0),
    ?line {ok,[List]} = file:consult(DataFile),
    OneBsl10000 = id(1) bsl 10000,
    ?line [-(1 bsl 10000),OneBsl10000|_] = List,

    %% Cleanup.
    ?line file:delete(SrcFile),
    ?line file:delete(DataFile),
    ?t:timetrap_cancel(Dog),
    ok.

do_integer_encoding(0, _, _, _) -> ok;
do_integer_encoding(N, I0, Src, Data) ->
    I1 = (I0 bsl 5) bor (random:uniform(32) - 1),
    do_integer_encoding(I1, Src, Data),
    I2 = -(I1 bxor (random:uniform(32) - 1)),
    do_integer_encoding(I2, Src, Data),
    do_integer_encoding(N-1, I1, Src, Data).

do_integer_encoding(I, Src, Data) ->
    Str = integer_to_list(I),
    io:put_chars(Src, Str),
    io:put_chars(Src, ", \n"),
    io:put_chars(Data, Str),
    io:put_chars(Data, ", \n").
    
    
id(I) -> I.
    
