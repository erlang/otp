%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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

-module(beam_SUITE).

-export([all/1, packed_registers/1, apply_last/1, apply_last_bif/1,
	 buildo_mucho/1, heap_sizes/1, big_lists/1, fconv/1]).

-export([applied/2]).

-include("test_server.hrl").

all(suite) ->
    [packed_registers, apply_last, apply_last_bif, buildo_mucho,
     heap_sizes, big_lists].


%% Verify that apply(M, F, A) is really tail recursive.
apply_last(Config) when is_list(Config) ->
    Pid=spawn(?MODULE, applied, [self(), 10000]),
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
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Mod = packed_regs,
    ?line Name = filename:join(PrivDir, atom_to_list(Mod) ++ ".erl"),

    %% Generate a module which generates a list of tuples.
    %% put_list(A) -> [{A, 600}, {A, 999}, ... {A, 0}].
    ?line Code = gen_packed_regs(600, ["-module("++atom_to_list(Mod)++").\n",
				       "-export([put_list/1]).\n",
				       "put_list(A) ->\n["]),
    ?line ok = file:write_file(Name, Code),

    %% Compile the module.
    ?line io:format("Compiling: ~s\n", [Name]),
    ?line CompRc = compile:file(Name, [{outdir, PrivDir}, report]),
    ?line io:format("Result: ~p\n",[CompRc]),
    ?line {ok, Mod} = CompRc,

    %% Load it.
    ?line io:format("Loading...\n",[]),
    ?line LoadRc = code:load_abs(filename:join(PrivDir, atom_to_list(Mod))),
    ?line {module,_Module} = LoadRc,

    %% Call it and verify result.
    ?line Term = {a, b},
    ?line L = Mod:put_list(Term),
    ?line verify_packed_regs(L, Term, 600),
    ok.

gen_packed_regs(0, Acc) ->
    [Acc|"{A,0}].\n"];
gen_packed_regs(N, Acc) ->
    gen_packed_regs(N-1, [Acc,"{A,",integer_to_list(N)|"},\n"]).

verify_packed_regs([], _, -1) -> ok;
verify_packed_regs([{Term, N}| T], Term, N) ->
    verify_packed_regs(T, Term, N-1);
verify_packed_regs(L, Term, N) ->
    ?line ok = io:format("Expected [{~p, ~p}|T]; got\n~p\n", [Term, N, L]),
    ?line test_server:fail().

buildo_mucho(Config) when is_list(Config) ->
    ?line buildo_mucho_1(),
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
    ?line Sizes = erlang:system_info(heap_sizes),
    ?line io:format("~p heap sizes\n", [length(Sizes)]),
    ?line io:format("~p\n", [Sizes]),

    %% Verify that heap sizes increase monotonically.
    ?line Largest = lists:foldl(fun(E, P) when is_integer(P), E > P -> E;
				   (E, []) -> E
				end, [], Sizes),

    %% Verify that the largest heap size consists of 31 or 63 bits.
    ?line
	case Largest bsr (erlang:system_info(wordsize)*8-2) of
	    R when R > 0 -> ok
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
    ?line do_fconv(atom),
    ?line do_fconv(nil),
    ?line do_fconv(tuple_literal),
    ?line 3.0 = do_fconv(1.0, 2.0),
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
