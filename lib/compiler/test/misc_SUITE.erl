%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
-module(misc_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 tobias/1,empty_string/1,md5/1,silly_coverage/1,
	 confused_literals/1,integer_encoding/0,integer_encoding/1,
	 override_bif/1]).
	 
-include_lib("common_test/include/ct.hrl").

%% For the override_bif testcase.
%% NB, no other testcases in this testsuite can use these without erlang:prefix!
-compile({no_auto_import,[abs/1]}).
-compile({no_auto_import,[binary_part/3]}).
-compile({no_auto_import,[binary_part/2]}).
-import(test_lib,[binary_part/2]).

%% This should do no harm (except for fun byte_size/1 which does not, by design, work with import
-compile({no_auto_import,[byte_size/1]}).
-import(erlang,[byte_size/1]).

%% Cover the code for callback handling.
-callback must_define_this_one() -> 'ok'.
-callback do_something_strange(atom()) -> 'ok'.
-optional_callbacks([do_something_strange/1]).
-optional_callbacks([ignore_me]).		%Invalid; ignored.

%% Include an opaque declaration to cover the stripping of
%% opaque types from attributes in v3_kernel.
-opaque misc_SUITE_test_cases() :: [atom()].

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Config.

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

-spec all() -> misc_SUITE_test_cases().
all() -> 
    slow_group() ++ [{group,p}].

groups() -> 
    [{p,[parallel],
      [tobias,empty_string,silly_coverage,
       confused_literals,override_bif]},
     {slow,[parallel],[integer_encoding,md5]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

slow_group() ->
    case ?MODULE of
	misc_SUITE ->
            %% Canononical module name. Run slow cases.
            [{group,slow}];
        _ ->
            %% Cloned module. Don't run.
            []
    end.
    
%%
%% Functions that override new and old bif's
%%
abs(_N) ->
    dummy_abs.

binary_part(_,_,_) ->
    dummy_bp.

%% Test that local functions and imports override auto-imported BIFs.
override_bif(Config) when is_list(Config) ->
    dummy_abs = abs(1),
    dummy_bp = binary_part(<<"hello">>,1,1),
    dummy = binary_part(<<"hello">>,{1,1}),
    1 = erlang:abs(1),
    <<"e">> = erlang:binary_part(<<"hello">>,1,1),
    <<"e">> = erlang:binary_part(<<"hello">>,{1,1}),
    F = fun(X) when byte_size(X) =:= 4 ->
		four;
	   (X) ->
		byte_size(X)
	end,
    four = F(<<1,2,3,4>>),
    5 = F(<<1,2,3,4,5>>),
    ok.

%% A bug reported by Tobias Lindahl for a development version of R11B.

tobias(Config) when is_list(Config) ->
    1 = tobias_1([1,2,3]),
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
    #r{s="x"} = empty_string_1(#r{}),
    ok.

empty_string_1(T) ->
    case T of
	#r{s = ""} -> T #r{s = "x"}
    end.

md5(Config) when is_list(Config) ->
    Dir = filename:dirname(code:which(?MODULE)),
    Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
    io:format("Found ~w beam files", [length(Beams)]),
    lists:foreach(fun md5_1/1, Beams).

md5_1(Beam) ->
    {ok,{Mod,[Vsn]}} = beam_lib:version(Beam),
    {ok,Code} = file:read_file(Beam),
    {Mod,<<Vsn:128>>} = {Mod,code:module_md5(Code)}.

%% Cover some code that handles internal errors.

silly_coverage(Config) when is_list(Config) ->
    %% sys_core_fold, sys_core_alias, sys_core_bsm, v3_kernel
    BadCoreErlang = {c_module,[],
		     name,[],[],
		     [{{c_var,[],{foo,2}},seriously_bad_body}]},
    expect_error(fun() -> sys_core_fold:module(BadCoreErlang, []) end),
    expect_error(fun() -> sys_core_alias:module(BadCoreErlang, []) end),
    expect_error(fun() -> sys_core_bsm:module(BadCoreErlang, []) end),
    expect_error(fun() -> v3_kernel:module(BadCoreErlang, []) end),

    %% beam_kernel_to_ssa
    BadKernel = {k_mdef,[],?MODULE,
		 [{foo,0}],
		 [],
		 [{k_fdef,
		   {k,[],[],[]},
		   f,0,[],
		   seriously_bad_body}]},
    expect_error(fun() -> beam_kernel_to_ssa:module(BadKernel, []) end),

    %% beam_ssa_lint
    %% beam_ssa_recv
    %% beam_ssa_share
    %% beam_ssa_pre_codegen
    %% beam_ssa_codegen
    BadSSA = {b_module,#{},a,b,c,
              [{b_function,#{func_info=>{mod,foo,0}},args,bad_blocks,0}]},
    expect_error(fun() -> beam_ssa_lint:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_recv:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_share:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_pre_codegen:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_codegen:module(BadSSA, []) end),

    %% beam_ssa_opt
    BadSSABlocks = #{0 => {b_blk,#{},[bad_code],{b_ret,#{},arg}}},
    BadSSAOpt = {b_module,#{},a,[],c,
                 [{b_function,#{func_info=>{mod,foo,0}},[],
                   BadSSABlocks,0}]},
    expect_error(fun() -> beam_ssa_opt:module(BadSSAOpt, []) end),

    %% beam_ssa_lint, beam_ssa_pp
    {error,[{_,Errors}]} = beam_ssa_lint:module(bad_ssa_lint_input(), []),
    _ = [io:put_chars(Mod:format_error(Reason)) ||
            {Mod,Reason} <- Errors],

    %% Cover printing of annotations in beam_ssa_pp
    PPAnno = #{func_info=>{mod,foo,0},other_anno=>value,map_anno=>#{k=>v}},
    PPBlocks = #{0=>{b_blk,#{},[],{b_ret,#{},{b_literal,42}}}},
    PP = {b_function,PPAnno,[],PPBlocks,0},
    io:put_chars(beam_ssa_pp:format_function(PP)),

    %% beam_a
    BeamAInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_a:module(BeamAInput, []) end),

    %% beam_block
    BlockInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_block:module(BlockInput, []) end),

    %% beam_except
    ExceptInput = {?MODULE,[{foo,0}],[],
		   [{function,foo,0,2,
		     [{label,1},
		      {line,loc},
		      {func_info,{atom,?MODULE},{atom,foo},0},
		      {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_except:module(ExceptInput, []) end),

    %% beam_jump
    JumpInput = BlockInput,
    expect_error(fun() -> beam_jump:module(JumpInput, []) end),

    %% beam_clean
    CleanInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2},
		     {jump,{f,42}}]}],99},
    expect_error(fun() -> beam_clean:module(CleanInput, []) end),

    %% beam_jump
    TrimInput = BlockInput,
    expect_error(fun() -> beam_trim:module(TrimInput, []) end),

    %% beam_peep. This is tricky. Use a select instruction with
    %% an odd number of elements in the list to crash
    %% prune_redundant_values/2 but not beam_clean:clean_labels/1.
    PeepInput = {?MODULE,[{foo,0}],[],
		 [{function,foo,0,2,
		   [{label,1},
		    {func_info,{atom,?MODULE},{atom,foo},0},
		    {label,2},{select,select_val,r,{f,2},[{f,2}]}]}],
		 2},
    expect_error(fun() -> beam_peep:module(PeepInput, []) end),

    BeamZInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_z:module(BeamZInput, []) end),

    %% beam_validator.
    BeamValInput = {?MODULE,[{foo,0}],[],
		    [{function,foo,0,2,
		      [{label,1},
		       {func_info,{atom,?MODULE},{atom,foo},0},
		       {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_validator:module(BeamValInput, []) end),

    ok.

bad_ssa_lint_input() ->
    {b_module,#{},t,
     [{foobar,1},{module_info,0},{module_info,1}],
     [],
     [{b_function,
       #{func_info => {t,foobar,1},location => {"t.erl",4}},
       [{b_var,0}],
       #{0 => {b_blk,#{},[],{b_ret,#{},{b_var,'@undefined_var'}}}},
       3},
      {b_function,
       #{func_info => {t,module_info,0}},
       [],
       #{0 =>
             {b_blk,#{},
              [{b_set,#{},
                {b_var,{'@ssa_ret',3}},
                call,
                [{b_remote,
                  {b_literal,erlang},
                  {b_literal,get_module_info},
                  1},
                 {b_var,'@unknown_variable'}]}],
              {b_ret,#{},{b_var,{'@ssa_ret',3}}}}},
       4}]}.

expect_error(Fun) ->
    try	Fun() of
	Any ->
	    io:format("~p", [Any]),
	    ct:fail(call_was_supposed_to_fail)
    catch
	Class:Reason:Stk ->
	    io:format("~p:~p\n~p\n", [Class,Reason,Stk]),
	    case {Class,Reason} of
		{error,undef} ->
		    ct:fail(not_supposed_to_fail_with_undef);
		{_,_} ->
		    ok
	    end
    end.

confused_literals(Config) when is_list(Config) ->
    {0,infinity} = confused_literals_1(int),
    {0.0,infinity} = confused_literals_1(float),
    ok.

confused_literals_1(int) -> {0,infinity};
confused_literals_1(float) -> {0.0,infinity}.

integer_encoding() ->
    [{timetrap,{minutes,4}}].

integer_encoding(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    SrcFile = filename:join(PrivDir, "misc_SUITE_integer_encoding.erl"),
    DataFile = filename:join(PrivDir, "integer_encoding.data"),
    Mod = misc_SUITE_integer_encoding,

    %% Create files.
    {ok,Src} = file:open(SrcFile, [write]),
    {ok,Data} = file:open(DataFile, [write]),
    io:format(Src, "-module(~s).\n", [Mod]),
    io:put_chars(Src, "-export([t/1]).\n"),
    io:put_chars(Src, "t(Last) ->[\n"),
    io:put_chars(Data, "[\n"),

    do_integer_encoding(137, 0, Src, Data),
    _ = [begin
	     B = 1 bsl I,
	     do_integer_encoding(-B-1, Src, Data),
	     do_integer_encoding(-B, Src, Data),
	     do_integer_encoding(-B+1, Src, Data),
	     do_integer_encoding(B-1, Src, Data),
	     do_integer_encoding(B, Src, Data),
	     do_integer_encoding(B+1, Src, Data)
	 end || I <- lists:seq(1, 130)],
    io:put_chars(Src, "Last].\n\n"),
    ok = file:close(Src),
    io:put_chars(Data, "0].\n\n"),
    ok = file:close(Data),

    %% Compile and load Erlang module.
    SrcRoot = filename:rootname(SrcFile),
    {ok,Mod,Binary} = compile:file(SrcRoot, [binary,report]),
    {module,Mod} = code:load_binary(Mod, SrcRoot, Binary),

    %% Compare lists.
    List = Mod:t(0),
    {ok,[List]} = file:consult(DataFile),

    %% Cleanup.
    file:delete(SrcFile),
    file:delete(DataFile),
    ok.

do_integer_encoding(0, _, _, _) -> ok;
do_integer_encoding(N, I0, Src, Data) ->
    I1 = (I0 bsl 5) bor (rand:uniform(32) - 1),
    do_integer_encoding(I1, Src, Data),
    I2 = -(I1 bxor (rand:uniform(32) - 1)),
    do_integer_encoding(I2, Src, Data),
    do_integer_encoding(N-1, I1, Src, Data).

do_integer_encoding(I, Src, Data) ->
    Str = integer_to_list(I),
    io:put_chars(Src, [Str,",\n"]),
    io:put_chars(Data, [Str,",\n"]).
