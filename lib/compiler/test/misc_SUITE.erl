%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
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
	 confused_literals/1,integer_encoding/1,override_bif/1]).
	 
-include_lib("test_server/include/test_server.hrl").

%% For the override_bif testcase.
%% NB, no other testcases in this testsuite can use these without erlang:prefix!
-compile({no_auto_import,[abs/1]}).
-compile({no_auto_import,[binary_part/3]}).
-compile({no_auto_import,[binary_part/2]}).
-import(test_lib,[binary_part/2]).

%% This should do no harm (except for fun byte_size/1 which does not, by design, work with import
-compile({no_auto_import,[byte_size/1]}).
-import(erlang,[byte_size/1]).



%% Include an opaque declaration to cover the stripping of
%% opaque types from attributes in v3_kernel.
-opaque misc_SUITE_test_cases() :: [atom()].

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = test_server:timetrap(?t:minutes(10)),
    [{watchdog,Dog}|Config].

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

-spec all() -> misc_SUITE_test_cases().
all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,[],
      [tobias,empty_string,md5,silly_coverage,
       confused_literals,integer_encoding,override_bif]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%
%% Functions that override new and old bif's
%%
abs(_N) ->
    dummy_abs.

binary_part(_,_,_) ->
    dummy_bp.

% Make sure that auto-imported BIF's are overridden correctly

override_bif(suite) ->
    [];
override_bif(doc) ->
    ["Test dat local functions and imports override auto-imported BIFs."];
override_bif(Config) when is_list(Config) ->
    ?line dummy_abs = abs(1),
    ?line dummy_bp = binary_part(<<"hello">>,1,1),
    ?line dummy = binary_part(<<"hello">>,{1,1}),
    ?line 1 = erlang:abs(1),
    ?line <<"e">> = erlang:binary_part(<<"hello">>,1,1),
    ?line <<"e">> = erlang:binary_part(<<"hello">>,{1,1}),
    F = fun(X) when byte_size(X) =:= 4 ->
		four;
	   (X) ->
		byte_size(X)
	end,
    ?line four = F(<<1,2,3,4>>),
    ?line 5 = F(<<1,2,3,4,5>>),
    ok.

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
    CodegenInput = {?MODULE,[{foo,0}],[],[{function,foo,0,[a|b],a,b,[]}]},
    ?line expect_error(fun() -> v3_codegen:module(CodegenInput, []) end),

    %% beam_a
    BeamAInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_a:module(BeamAInput, []) end),

    %% beam_reorder
    BlockInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_reorder:module(BlockInput, []) end),

    %% beam_block
    BlockInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    ?line expect_error(fun() -> beam_block:module(BlockInput, []) end),

    %% beam_bs
    BsInput = BlockInput,
    expect_error(fun() -> beam_bs:module(BsInput, []) end),

    %% beam_type
    TypeInput = {?MODULE,[{foo,0}],[],
		   [{function,foo,0,2,
		     [{label,1},
		      {line,loc},
		      {func_info,{atom,?MODULE},{atom,foo},0},
		      {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_type:module(TypeInput, []) end),

    %% beam_except
    ExceptInput = {?MODULE,[{foo,0}],[],
		   [{function,foo,0,2,
		     [{label,1},
		      {line,loc},
		      {func_info,{atom,?MODULE},{atom,foo},0},
		      {label,2}|non_proper_list]}],99},
    expect_error(fun() -> beam_except:module(ExceptInput, []) end),

    %% beam_bool
    BoolInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2}|non_proper_list]}],99},
    ?line expect_error(fun() -> beam_bool:module(BoolInput, []) end),

    %% beam_dead. This is tricky. Our function must look OK to
    %% beam_utils:clean_labels/1, but must crash beam_dead.
    DeadInput = {?MODULE,[{foo,0}],[],
		  [{function,foo,0,2,
		    [{label,1},
		     {func_info,{atom,?MODULE},{atom,foo},0},
		     {label,2},
		     {test,is_eq_exact,{f,1},[bad,operands]}]}],99},
    expect_error(fun() -> beam_dead:module(DeadInput, []) end),

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

expect_error(Fun) ->
    try	Fun() of
	Any ->
	    io:format("~p", [Any]),
	    ?t:fail(call_was_supposed_to_fail)
    catch
	Class:Reason ->
	    Stk = erlang:get_stacktrace(),
	    io:format("~p:~p\n~p\n", [Class,Reason,Stk]),
	    case {Class,Reason} of
		{error,undef} ->
		    ?t:fail(not_supposed_to_fail_with_undef);
		{_,_} ->
		    ok
	    end
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
    do_integer_encoding(1024, 0, Src, Data),
    _ = [begin
	     B = 1 bsl I,
	     do_integer_encoding(-B-1, Src, Data),
	     do_integer_encoding(-B, Src, Data),
	     do_integer_encoding(-B+1, Src, Data),
	     do_integer_encoding(B-1, Src, Data),
	     do_integer_encoding(B, Src, Data),
	     do_integer_encoding(B+1, Src, Data)
	 end || I <- lists:seq(1, 128)],
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
    io:put_chars(Src, [Str,",\n"]),
    io:put_chars(Data, [Str,",\n"]).

    
id(I) -> I.
    
