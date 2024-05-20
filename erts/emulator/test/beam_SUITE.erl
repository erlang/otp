%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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

-module(beam_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 packed_registers/1, apply_last/1, apply_last_bif/1,
	 heap_sizes/1, big_lists/1, fconv/1,
         select_val/1, select_tuple_arity/1,
         swap_temp_apply/1, beam_init_yregs/1,
         beam_register_cache/1]).

-export([applied/2,swap_temp_applied/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("syntax_tools/include/merl.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [packed_registers, apply_last, apply_last_bif,
     heap_sizes, big_lists, fconv,
     select_val, select_tuple_arity,
     swap_temp_apply, beam_init_yregs,
     beam_register_cache].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%% Verify that apply(M, F, A) is really tail recursive.
apply_last(Config) when is_list(Config) ->
    Pid  = spawn(?MODULE, applied, [self(), 10000]),
    Size =
	receive
	    {Pid, finished} ->
		stack_size(Pid)
	after 30000 ->
		ct:fail("applied/2 timed out.")
	end,
    Pid ! die,
    io:format("Size: ~p~n", [Size]),
    if
	Size < 700 ->
	    ok;
	true ->
	    ct:fail("10000 apply() grew stack too much.")
    end,
    ok.

stack_size(Pid) ->
    {heap_size, HS}=process_info(Pid, heap_size),
    {stack_size,SS}=process_info(Pid, stack_size),
    HS+SS.

applied(Starter, 0) ->
    Starter ! {self(), finished},
    receive
	die ->
	    ok
    end,
    ok;
applied(Starter, N) ->
    apply(?MODULE, applied, [Starter, N-1]).

%% Verify that tail-recursive use of apply(M,F,A) on a Bif works."
apply_last_bif(Config) when is_list(Config) ->
    apply(erlang, abs, [1]).

%% Test whether packing works properly.
packed_registers(Config) when is_list(Config) ->
    Mod = ?FUNCTION_NAME,

    %% Generate scrambled sequence.
    Seq0 = [{erlang:phash2(I),I} || I <- lists:seq(0, 260)],
    Seq = [I || {_,I} <- lists:sort(Seq0)],

    %% Generate a test modules that uses get_list/3 instructions
    %% with high register numbers.
    S0 = [begin
	      VarName = list_to_atom("V"++integer_to_list(V)),
	      {merl:var(VarName),V}
	  end || V <- Seq],
    Vars = [V || {V,_} <- S0],
    NewVars = [begin
		   VarName = list_to_atom("M"++integer_to_list(V)),
		   merl:var(VarName)
	       end || V <- Seq],
    MoreNewVars = [begin
                       VarName = list_to_atom("MM"++integer_to_list(V)),
                       merl:var(VarName)
                   end || V <- Seq],
    TupleEls = [?Q("id(_@Value@)") || {_,Value} <- S0],
    S = [?Q("_@Var = id(_@Value@)") || {Var,Value} <- S0],
    Code = ?Q(["-module('@Mod@').\n"
	       "-export([f/0]).\n"
	       "f() ->\n"
               "Tuple = id({_@TupleEls}),\n"
               "{_@MoreNewVars} = Tuple,\n"
	       "_@S,\n"
	       "_ = id(0),\n"
	       "L = [_@Vars],\n"
	       "_ = id(1),\n"
	       "[_@NewVars] = L,\n"		%Test get_list/3.
	       "_ = id(2),\n"
	       "id([_@Vars,_@NewVars,_@MoreNewVars]).\n"
	       "id(I) -> I.\n"]),

    merl:compile_and_load(Code, []),

    %% Optionally print the generated code.
    PrintCode = false,                          %Change to true to print code.

    case PrintCode of
        false ->
            ok;
        true ->
            merl:print(Code),
            erts_debug:df(Mod),
            {ok,Dis} = file:read_file(atom_to_list(Mod)++".dis"),
            io:put_chars(Dis)
    end,

    %% Executing the generated code in freshly spawned process makes it much
    %% more likely to crash if there is a bug in init_yregs.
    CombinedSeq = Seq ++ Seq ++ Seq,
    CombinedSeq = spawn_exec(fun Mod:f/0),

    %% Clean up.
    true = code:delete(Mod),
    false = code:purge(Mod),
    ok.

heap_sizes(Config) when is_list(Config) ->
    Sizes = erlang:system_info(heap_sizes),
    io:format("~p heap sizes\n", [length(Sizes)]),
    io:format("~p\n", [Sizes]),

    %% Verify that heap sizes increase monotonically.
    Largest = lists:foldl(fun(E, P) when is_integer(P), E > P -> E;
				   (E, []) -> E
				end, [], Sizes),

    %% Verify that the largest heap size consists of
    %%  - 31 bits of bytes on 32 bits arch
    %%  - at least 52 bits of bytes (48 is the maximum virtual address)
    %%    and at the most 63 bits on 64 bit archs
    %% heap sizes are in words
    case erlang:system_info(wordsize) of
	8 ->
	    0    = (Largest*8) bsr 63,
	    true = (Largest*8) > (1 bsl 52);
	4 ->
	    1 = (Largest*4) bsr 31
    end,
    ok.

%% Thanks to Igor Goryachev.

big_lists(Config) when is_list(Config) ->
    b(),
    ok.

a() ->
    {selected,
     ["uid",
      "nickname",
      "n_family",
      "n_given",
      "email_pref",
      "tel_home_number",
      "tel_cellular_number",
      "adr_home_country",
      "adr_home_locality",
      "adr_home_region",
      "url",
      "gender",
      "bday",
      "constitution",
      "height",
      "weight",
      "hair",
      "routine",
      "smoke",
      "maritalstatus",
      "children",
      "independence",
      "school_number",
      "school_locality",
      "school_title",
      "school_period",
      "org_orgname",
      "title",
      "adr_work_locality",
      "photo_type",
      "photo_binval"],
     [{"test"}]}.

b() ->
    case a() of
        {selected,
         ["uid",
          "nickname",
          "n_family",
          "n_given",
          "email_pref",
          "tel_home_number",
          "tel_cellular_number",
          "adr_home_country",
          "adr_home_locality",
          "adr_home_region",
          "url",
          "gender",
          "bday",
          "constitution",
          "height",
          "weight",
          "hair",
          "routine",
          "smoke",
          "maritalstatus",
          "children",
          "independence",
          "school_number",
          "school_locality",
          "school_title",
          "school_period",
          "org_orgname",
          "title",
          "adr_work_locality",
          "photo_type",
          "photo_binval"],
         _} ->
	    ok
    end.

fconv(Config) when is_list(Config) ->
    do_fconv(atom),
    do_fconv(nil),
    do_fconv(tuple_literal),
    3.0 = do_fconv(1.0, 2.0),
    ok.

do_fconv(Type) ->
    try
	do_fconv(Type, 1.0),
	ct:fail(no_badarith)
    catch
	error:badarith ->
	    ok
    end.

do_fconv(atom, Float) when is_float(Float) ->
    Float + a;
do_fconv(nil, Float) when is_float(Float) ->
    Float + [];
do_fconv(tuple_literal, Float) when is_float(Float) ->
    Float + {a,b};
do_fconv(A, B) when is_float(A), is_float(B) ->
    A + B.


select_val(Config) when is_list(Config) ->
    Mod = ?FUNCTION_NAME,

    %% Test select_val instructions around interesting powers of 2.
    Powers = lists:seq(0, 33) ++
        lists:seq(47, 49) ++
        lists:seq(58, 65) ++
        lists:seq(8192 - 5, 8192 + 5),
    L = make_tests(Powers, 0, []),
    Code = make_module(Mod, L),
    merl:compile_and_load(Code, []),

    %% Uncomment the following line to print the generated code.
    %%merl:print(Code),

    verify(L, Mod),

    %% Clean up.
    true = code:delete(Mod),
    false = code:purge(Mod),

    ok.

verify([{F,L}|T], Mod) ->
    verify_1(L, F, Mod),

    Sum = lists:sum([abs(V) || {V,_} <- L]),
    expect_error(Mod, F, -Sum),
    expect_error(Mod, F, Sum),
    expect_error(Mod, F, not_an_integer),

    verify(T, Mod);
verify([], _Mod) -> ok.

verify_1([{V,Exp}|T], F, Mod) ->
    case Mod:F(V) of
        Exp ->
            verify_1(T, F, Mod);
        Other ->
            io:format("~p:~p(~p) evaluates to ~p; expected ~p",
                      [Mod,F,V,Other,Exp]),
            ct:fail(unexpected_value)
    end;
verify_1([], _, _) -> ok.

expect_error(M, F, V) ->
    try M:F(V) of
        UnexpectedSuccess ->
            io:format("~p:~p(~p) evaluates to ~p; expected it to fail",
                      [M,F,V,UnexpectedSuccess]),
            ct:fail(unexpected_success)
    catch error:function_clause:Stk ->
            [{M,F,[V],_}|_] = Stk
    end.

make_tests([P|Ps], N0, Acc0) ->
    {Acc,N} = make_tests_1(1 bsl P, N0, Acc0),
    make_tests(Ps, N, Acc);
make_tests([], N0, Acc0) ->
    %% Finally, generate some functions with a huge number of clauses.

    {Acc1, N1} = make_huge_bsearch(N0, Acc0),
    {Acc, _N} = make_huge_jump_tab(N1, Acc1),

    lists:reverse(Acc).

make_tests_1(V, N0, Acc0) ->
    {Acc,N} = make_tests_2(V, N0, Acc0),
    make_tests_2(-V, N, Acc).

make_tests_2(V, N0, Acc0) ->
    %% Because the values are not consecutive, a binary search will be
    %% generated.
    NumClauses = 14,
    {Acc1,N1} = make_tests_3(V - NumClauses*5, V, NumClauses, 5, N0, Acc0),

    %% Test a binary search with 107 values.
    {Acc,N} = make_test("bsearch", V - 5*50, V + 5*56, 5, N1, Acc1),

    %% Use consecutive values to allow a jump table to be used.
    make_test("jump_tab", V - 10, V + 10, 1, N, Acc).

make_tests_3(_First, _Last, 0, _Step, N, Acc) ->
    {Acc,N};
make_tests_3(First, Last, NumClauses, Step, N0, Acc0) ->
    {Acc,N} = make_test("f", First, Last, Step, N0, Acc0),
    make_tests_3(First+Step, Last+Step, NumClauses-1, Step, N, Acc).

make_huge_jump_tab(N, Acc) ->
    NC = 16384 + 1,
    make_test("huge_jump_tab", 0, NC, 1, N, Acc).

make_huge_bsearch(N0, Acc) ->
    NC = 2000,

    %% Let the last number be a bignum.
    Last = 1 bsl (erlang:system_info(wordsize)*8 - 5),
    Step = 5,

    make_test("huge_bsearch", Last - NC*Step, Last, Step, N0, Acc).

make_test(Prefix, First, Last, Step, N, Acc) ->
    Name = list_to_atom(Prefix ++ integer_to_list(N)),
    Seq = lists:seq(First, Last, Step),
    L = [{I,erlang:phash2({I,N})} || I <- Seq],
    {[{Name,L}|Acc],N+1}.

make_module(Mod, L) ->
    ModDef = ?Q(["-module('@Mod@')."]),
    Fs = make_funcs(L),
    Xs = [{F,1} || {F,_} <- L],
    Exported = [erl_syntax:arity_qualifier(merl:term(N), merl:term(A)) ||
                   {N,A} <- Xs],
    Export = ?Q("-export(['@_Exported'/1])."),
    [ModDef, Export | Fs].

make_funcs([{Name,List}|T]) ->
    [make_func(Name, List) | make_funcs(T)];
make_funcs([]) -> [].

make_func(Name, List) ->
    Cs = [?Q(["(_@I@) -> _@Body@"]) || {I,Body} <- List],
    erl_syntax:function(erl_syntax:atom(Name), Cs).

select_tuple_arity(_Config) ->
    Mod = ?FUNCTION_NAME,

    {Vs,Cs} = make_tuple_tests(300, [], []),
    Name = erl_syntax:atom(match_tuple),
    F = erl_syntax:function(Name, Cs),
    Code = ?Q(["-module('@Mod@').\n"
               "-export([match_tuple/1]).\n"]) ++ [F],

    merl:compile_and_load(Code, []),

    %% %% Uncomment the following line to print the generated code.
    %% merl:print(Code),

    verify_tuple_match(Vs, Mod),

    %% Clean up.
    true = code:delete(Mod),
    false = code:purge(Mod),

    ok.

make_tuple_tests(0, Clauses, Acc) ->
    {Acc,Clauses};
make_tuple_tests(Size, Clauses, Acc) ->
    V = erlang:phash2(Size),
    Es = lists:duplicate(Size, erl_syntax:underscore()),
    Tuple = erl_syntax:tuple(Es),
    Value = erl_syntax:integer(V),
    Clause = erl_syntax:clause([Tuple], [], [Value]),
    make_tuple_tests(Size-1, [Clause|Clauses], [{Size,V}|Acc]).

verify_tuple_match([{Size,Result}|T], Mod) ->
    Tuple = erlang:make_tuple(Size, a),
    Result = Mod:match_tuple(Tuple),
    verify_tuple_match(T, Mod);
verify_tuple_match([], _) ->
    ok.

swap_temp_apply(_Config) ->
    {swap_temp_applied,42} = do_swap_temp_apply(41),
    not_an_integer = do_swap_temp_apply(not_an_integer),
    ok.

do_swap_temp_apply(Msg) ->
    case swap_temp_apply_function(Msg) of
	undefined -> Msg;
	Type ->
	    %% The following sequence:
	    %%   move {x,0} {x,2}
	    %%   move {y,0} {x,0}
	    %%   move {x,2} {y,0}
	    %%   apply 1
	    %%
	    %% Would be incorrectly transformed to:
	    %%   swap {x,0} {y,0}
	    %%   apply 1
	    %%
	    %% ({x,1} is the module, {x,2} the function to be applied).
	    %%
	    %% If the instructions are to be transformed, the correct
	    %% transformation is:
	    %%
	    %%   swap_temp {x,0} {y,0} {x,2}
	    %%   apply 1
	    Fields = ?MODULE:Type(Msg),
	    {Type,Fields}
    end.

swap_temp_apply_function(Int) when is_integer(Int) ->
    swap_temp_applied;
swap_temp_apply_function(_) ->
    undefined.

swap_temp_applied(Int) ->
    Int+1.

beam_init_yregs(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Mod = ?FUNCTION_NAME,
    File = filename:join(DataDir, Mod),
    {ok,Mod,Code} = compile:file(File, [from_asm,no_postopt,binary]),
    {module,Mod} = code:load_binary(Mod, Mod, Code),

    _ = [ok = spawn_exec(fun Mod:Mod/0) || _ <- lists:seq(1, 10)],

    %% Clean up.
    true = code:delete(Mod),
    false = code:purge(Mod),

    ok.

%% GH-8433: The register cache wasn't properly maintained for certain helper
%% functions in the ARM JIT.
beam_register_cache(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Mod = ?FUNCTION_NAME,

    File = filename:join(DataDir, Mod),
    {ok,Mod,Code} = compile:file(File, [from_asm,no_postopt,binary]),
    {module,Mod} = code:load_binary(Mod, Mod, Code),

    try
        ok = Mod:Mod()
    after
        %% Clean up.
        true = code:delete(Mod),
        false = code:purge(Mod)
    end,

    ok.

%%% Common utilities.
spawn_exec(F) ->
    {Pid,Ref} = spawn_monitor(fun() ->
                                      exit(F())
                              end),
    receive
        {'DOWN',Ref,process,Pid,Result} -> Result
    end.
