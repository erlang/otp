%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
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
	 buildo_mucho/1, heap_sizes/1, big_lists/1, fconv/1,
	 select_val/1]).

-export([applied/2]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [packed_registers, apply_last, apply_last_bif,
     buildo_mucho, heap_sizes, big_lists, select_val].

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
		?t:fail("applied/2 timed out.")
	end,
    Pid ! die,
    ?t:format("Size: ~p~n", [Size]),
    if
	Size < 700 ->
	    ok;
	true ->
	    ?t:fail("10000 apply() grew stack too much.")
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

%% Test three high register numbers in a put_list instruction
%% (to test whether packing works properly).
packed_registers(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Mod = packed_regs,
    Name = filename:join(PrivDir, atom_to_list(Mod) ++ ".erl"),

    %% Generate a module which generates a list of tuples.
    %% put_list(A) -> [{A, 600}, {A, 999}, ... {A, 0}].
    Code = gen_packed_regs(600, ["-module("++atom_to_list(Mod)++").\n",
				       "-export([put_list/1]).\n",
				       "put_list(A) ->\n["]),
    ok = file:write_file(Name, Code),

    %% Compile the module.
    io:format("Compiling: ~s\n", [Name]),
    CompRc = compile:file(Name, [{outdir, PrivDir}, report]),
    io:format("Result: ~p\n",[CompRc]),
    {ok, Mod} = CompRc,

    %% Load it.
    io:format("Loading...\n",[]),
    LoadRc = code:load_abs(filename:join(PrivDir, atom_to_list(Mod))),
    {module,_Module} = LoadRc,

    %% Call it and verify result.
    Term = {a, b},
    L = Mod:put_list(Term),
    verify_packed_regs(L, Term, 600),
    ok.

gen_packed_regs(0, Acc) ->
    [Acc|"{A,0}].\n"];
gen_packed_regs(N, Acc) ->
    gen_packed_regs(N-1, [Acc,"{A,",integer_to_list(N)|"},\n"]).

verify_packed_regs([], _, -1) -> ok;
verify_packed_regs([{Term, N}| T], Term, N) ->
    verify_packed_regs(T, Term, N-1);
verify_packed_regs(L, Term, N) ->
    ok = io:format("Expected [{~p, ~p}|T]; got\n~p\n", [Term, N, L]),
    test_server:fail().

buildo_mucho(Config) when is_list(Config) ->
    buildo_mucho_1(),
    ok.

buildo_mucho_1() ->
    %% Thanks to Per Gustafsson, HiPE.
    [{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},
     {<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1},{<<>>,1}].

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
    %%  - atleast 52 bits of bytes (48 is the maximum virtual address)
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
	test_server:fail()
    catch
	error:badarith ->
	    ok
    end.

do_fconv(atom, Float) when is_float(Float) ->
    Float + a;
do_fconv(nil, Float) when is_float(Float) ->
    Float + [];
do_fconv(tuple_literal, Float) when is_float(Float) ->
    Float + {a,b}.

select_val(Config) when is_list(Config) ->
    zero = do_select_val(0),
    big = do_select_val(1 bsl 64),
    integer = do_select_val(42),
    ok.

do_select_val(X) ->
    case X of
	0 ->
	    zero;
	1 bsl 64 ->
	    big;
	Int when is_integer(Int) ->
	    integer
    end.
