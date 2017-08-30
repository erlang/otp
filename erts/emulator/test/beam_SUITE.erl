%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
	 select_val/1, swap_temp_apply/1]).

-export([applied/2,swap_temp_applied/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("syntax_tools/include/merl.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [packed_registers, apply_last, apply_last_bif,
     buildo_mucho, heap_sizes, big_lists, select_val,
     swap_temp_apply].

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
    S = [?Q("_@Var = id(_@Value@)") || {Var,Value} <- S0],
    Code = ?Q(["-module('@Mod@').\n"
	       "-export([f/0]).\n"
	       "f() ->\n"
	       "_@S,\n"
	       "_ = id(0),\n"
	       "L = [_@Vars],\n"
	       "_ = id(1),\n"
	       "[_@NewVars] = L,\n"		%Test get_list/3.
	       "_ = id(2),\n"
	       "id([_@Vars,_@NewVars]).\n"
	       "id(I) -> I.\n"]),
    merl:compile_and_load(Code),
    CombinedSeq = Seq ++ Seq,
    CombinedSeq = Mod:f(),

    %% Clean up.
    true = code:delete(Mod),
    false = code:purge(Mod),
    ok.

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
