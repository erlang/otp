%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
	 override_bif/1,catch_precedence/1]).
	 
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

%% Cover handling of the `nifs` attribute.
-nifs([all/0]).

%% Import SSA records.
-import_record(beam_ssa, [b_module, b_blk, b_function, b_set,
                          b_var, b_literal, b_remote, b_local,
                          b_ret]).

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
       confused_literals,override_bif,catch_precedence]},
     {slow,[parallel],[integer_encoding,md5]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    if
        is_atom(Config) ->
            %% Cover handling of load_nif. Will never actually be called.
            _ = erlang:load_nif("no_real_nif", 42);
        true ->
            ok
    end,
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

catch_precedence(Config) when is_list(Config) ->
        %% lower than addition
        3 = begin (catch throw(2)) + 1 end,
        2 = begin catch throw(2) + 1 end,

        %% lower than comparison
        true = begin (catch throw(false)) =/= true end,
        false = begin catch throw(false) =/= true end,

        %% lower than send (which has the same precedence as =)
        Pid = spawn_link(fun () -> receive stop -> ok end end),
        false = is_pid(begin (catch throw(Pid)) ! stop end),
        true = is_pid(begin catch throw(Pid) ! stop end).

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
    %% v3_core
    BadAbstr = [{attribute,0,module,bad_module},
                {function,0,foo,2,[bad_clauses]}],
    expect_error(fun() -> v3_core:module(BadAbstr, []) end),

    %% sys_core_fold, sys_core_alias, sys_core_bsm, beam_core_to_ssa
    BadCoreErlang = {c_module,[],
		     {c_literal,[],name},[],[],
		     [{{c_var,[],{foo,2}},seriously_bad_body}]},
    expect_error(fun() -> sys_core_fold:module(BadCoreErlang, []) end),
    expect_error(fun() -> sys_core_alias:module(BadCoreErlang, []) end),
    expect_error(fun() -> sys_core_bsm:module(BadCoreErlang, []) end),
    expect_error(fun() -> beam_core_to_ssa:module(BadCoreErlang, []) end),

    %% beam_ssa_lint
    %% beam_ssa_bool
    %% beam_ssa_recv
    %% beam_ssa_share
    %% beam_ssa_pre_codegen
    %% beam_ssa_codegen
    BadSSA = #b_module{anno=#{},name=a,exports=b,attributes=c,
                       body=[#b_function{anno=#{func_info=>{mod,foo,0}},
                                         args=args,
                                         bs=bad_blocks,
                                         cnt=0}]},
    expect_error(fun() -> beam_ssa_lint:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_bool:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_recv:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_share:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_pre_codegen:module(BadSSA, []) end),
    expect_error(fun() -> beam_ssa_codegen:module(BadSSA, []) end),

    %% beam_ssa_opt
    BadSSABlocks = #{0 => #b_blk{anno=#{},
                                 is=[bad_code],
                                 last=#b_ret{anno=#{},arg=arg}}},
    BadSSAFunc = #b_function{anno=#{func_info => {mod,foo,0}},
                             args=[],
                             bs=BadSSABlocks,
                             cnt=0},
    BadSSAOpt = #b_module{anno=#{},name=a,
                          exports=[],attributes=[],
                          body=[BadSSAFunc]},
    expect_error(fun() -> beam_ssa_opt:module(BadSSAOpt, []) end),

    %% beam_ssa_bc_size
    cover_beam_ssa_bc_size(1),

    %% beam_ssa_lint, beam_ssa_pp
    {error,[{_,Errors}]} = beam_ssa_lint:module(bad_ssa_lint_input(), []),
    _ = [io:put_chars(Mod:format_error(Reason)) ||
            {Mod,Reason} <- Errors],

    %% Cover printing of annotations in beam_ssa_pp
    PPAnno = #{func_info=>{mod,foo,0},other_anno=>value,map_anno=>#{k=>v}},
    PPBlocks = #{0 =>
                     #b_blk{anno=#{},
                            is=[],
                            last=#b_ret{anno=#{},
                                        arg=#b_literal{val=42}}}},
    PP = #b_function{anno=PPAnno,args=[],bs=PPBlocks,cnt=0},
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
    expect_error(fun() -> beam_validator:validate(BeamValInput, strong) end),

    ok.

cover_beam_ssa_bc_size(20) ->
    ok;
cover_beam_ssa_bc_size(N) ->
    BcSizeKey = #b_local{name=#b_literal{val=name},arity=1},
    %% Try different sizes for the opt_st record.
    OptSt = erlang:make_tuple(N, #{}, [{1,opt_st}]),
    expect_error(fun() -> beam_ssa_bc_size:opt(#{BcSizeKey => OptSt}) end),
    cover_beam_ssa_bc_size(N + 1).

bad_ssa_lint_input() ->
    Ret = #b_var{name=100},
    Funcs = [#b_function{anno=#{func_info => {t,a,1},location => {"t.erl",4}},
                         args=[#b_var{name=0}],
                         bs=#{0 =>
                                  #b_blk{anno=#{},
                                         is=[],
                                         last=#b_ret{anno=#{},
                                                     arg=#b_var{name='@undefined_var'}}}},
                         cnt=3},
             #b_function{anno=#{func_info => {t,b,1},location => {"t.erl",5}},
                         args=[#b_var{name=0}],
                         bs=#{0 =>
                               #b_blk{anno=#{},
                                      is=[#b_set{anno=#{},
                                                 dst=#b_var{name='@first_var'},
                                                 op=first_op,
                                                 args=[]},
                                          #b_set{anno=#{},
                                                 dst=#b_var{name='@second_var'},
                                                 op=second_op,
                                                 args=[]},
                                          #b_set{anno=#{},
                                                 dst=#b_var{name='@ret'},
                                                 op=succeeded,
                                                 args=[#b_var{name='@first_var'}]}],
                                      last=#b_ret{anno=#{},
                                                  arg=#b_var{name='@ret'}}}},
                         cnt=3},
             #b_function{anno=#{func_info => {t,c,1},location => {"t.erl",6}},
                         args=[#b_var{name=0}],
                         bs=#{0 =>
                                  #b_blk{anno=#{},
                                         is=[#b_set{anno=#{},
                                                    dst=#b_var{name='@first_var'},
                                                    op=first_op,
                                                    args=[]},
                                             #b_set{anno=#{},
                                                    dst=#b_var{name='@ret'},
                                                    op=succeeded,
                                                    args=[#b_var{name='@first_var'}]},
                                             #b_set{anno=#{},
                                                    dst=#b_var{name='@second_var'},
                                                    op=second_op,
                                                    args=[]}],
                                         last=#b_ret{anno=#{},
                                                     arg=#b_var{name='@ret'}}}},
                         cnt=3},
             #b_function{anno=#{func_info => {t,module_info,0}},
                         args=[],
                         bs=#{0 =>
                                  #b_blk{anno=#{},
                                         is=[#b_set{anno=#{},
                                                    dst=Ret,
                                                    op=call,
                                                    args=[#b_remote{mod=#b_literal{val=erlang},
                                                                    name=#b_literal{val=get_module_info},
                                                                    arity=1},
                                                          #b_var{name='@unknown_variable'}]}],
                                         last=#b_ret{anno=#{},arg=Ret}}},
                         cnt=4}],
    #b_module{anno=#{},name=t,
              exports=[{a,1},{b,1},{c,1},{module_info,0},{module_info,1}],
              attributes=[],
              body=Funcs}.

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
