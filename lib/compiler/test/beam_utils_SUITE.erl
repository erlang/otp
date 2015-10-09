%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
-module(beam_utils_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 apply_fun/1,apply_mf/1,bs_init/1,bs_save/1,
	 is_not_killed/1,is_not_used_at/1,
	 select/1,y_catch/1]).
-export([id/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [apply_fun,
       apply_mf,
       bs_init,
       bs_save,
       is_not_killed,
       is_not_used_at,
       select,
       y_catch
      ]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

apply_fun(_Config) ->
    3 = do_apply_fun(false, false),
    3 = do_apply_fun(false, true),
    3 = do_apply_fun(true, false),
    2 = do_apply_fun(true, true),
    ok.

do_apply_fun(X, Y) ->
    F = fun(I) -> I+1 end,
    Arg = case X andalso id(Y) of
	      true -> 1;
	      false -> 2
	  end,
    F(Arg).

apply_mf(_Config) ->
    ok = do_apply_mf_used({a,b}, ?MODULE, id),
    error = do_apply_mf_used([a], ?MODULE, id),
    {'EXIT',{{case_clause,{[],b}},_}} = (catch do_apply_mf_used({[],b}, ?MODULE, id)),

    error = do_apply_mf_killed({error,[a]}, ?MODULE, id),
    ok = do_apply_mf_killed([b], ?MODULE, id),
    {'EXIT',{{case_clause,{a,[b]}},_}} = (catch do_apply_mf_killed({a,[b]}, ?MODULE, id)),
    {'EXIT',{{case_clause,{error,[]}},_}} = (catch do_apply_mf_killed({error,[]}, ?MODULE, id)),

    ok.

do_apply_mf_used(Arg, Mod, Func) ->
    Res = case id(Arg) of
	      {Decoded,_} when Decoded =/= [] ->
		  ok;
	      List when is_list(List) ->
		  error
	  end,
    Mod:Func(Res).

do_apply_mf_killed(Arg, Mod, Func) ->
    Res = case id(Arg) of
	      {Tag,Decoded} when Decoded =/= [], Tag =:= error ->
		  error;
	      List when is_list(List) ->
		  ok
	  end,
    Mod:Func(Res).

bs_init(_Config) ->
    <<7>> = do_bs_init_1([?MODULE], 7),
    error = do_bs_init_1([?MODULE], 0.0),
    error = do_bs_init_1([?MODULE], -43),
    error = do_bs_init_1([?MODULE], 42),

    <<>> = do_bs_init_2([]),
    <<0:32,((1 bsl 32)-1):32>> = do_bs_init_2([0,(1 bsl 32)-1]),
    {'EXIT',{badarg,_}} = (catch do_bs_init_2([0.5])),
    {'EXIT',{badarg,_}} = (catch do_bs_init_2([-1])),
    {'EXIT',{badarg,_}} = (catch do_bs_init_2([1 bsl 32])),
    ok.

do_bs_init_1([?MODULE], Sz) ->
    if
	is_integer(Sz), Sz >= -42, Sz < 42 ->
	    id(<<Sz:8>>);
	true ->
	    error
    end.

do_bs_init_2(SigNos) ->
    << <<SigNo:32>> ||
	SigNo <- SigNos,
	(is_integer(SigNo) andalso SigNo >= 0 andalso SigNo < (1 bsl 32)) orelse
	    erlang:error(badarg)
    >>.


bs_save(_Config) ->
    {a,30,<<>>} = do_bs_save(<<1:1,30:5>>),
    {b,127,<<>>} = do_bs_save(<<1:1,31:5,0:1,127:7>>),
    {c,127,<<>>} = do_bs_save(<<1:1,31:5,1:1,127:7>>),
    {c,127,<<>>} = do_bs_save(<<0:1,31:5,1:1,127:7>>),
    {d,1024,<<>>} = do_bs_save(<<0:1,31:5>>),
    ok.

do_bs_save(<<_:1, Tag:5, T/binary>>) when Tag < 31 ->
    {a,Tag,T};
do_bs_save(<<1:1, 31:5, 0:1, Tag:7, T/binary>>)  ->
    {b,Tag,T};
do_bs_save(<<_:1, 31:5, 1:1, Tag:7, T/binary>>) ->
    {c,Tag,T};
do_bs_save(<<_:1, 31:5, T/binary>>) ->
    {d,1024,T}.

is_not_killed(_Config) ->
    {Pid,Ref} = spawn_monitor(fun() -> exit(banan) end),
    receive
	{'DOWN', Ref, process, Pid, banan} ->
	    ok
    end,
    receive after 0 -> ok end.

is_not_used_at(_Config) ->
    {a,b} = do_is_not_used_at(a, [{a,b}]),
    {a,b} = do_is_not_used_at(a, [x,{a,b}]),
    {a,b} = do_is_not_used_at(a, [{x,y},{a,b}]),
    none = do_is_not_used_at(z, [{a,b}]),
    none = do_is_not_used_at(a, [x]),
    none = do_is_not_used_at(a, [{x,y}]),
    ok.

do_is_not_used_at(Key, [P|Ps]) ->
    if
	tuple_size(P) >= 1, element(1, P) =:= Key ->
	    P;
	true ->
	    do_is_not_used_at(Key, Ps)
    end;
do_is_not_used_at(_Key, []) -> none.

-record(select, {fixed=false}).

select(_Config) ->
    a = do_select(#select{}, 0, 0),
    b = do_select(#select{}, 0, 1),
    c = do_select(#select{fixed=true}, 0, 0),
    c = do_select(#select{fixed=true}, 0, 1),
    ok.

do_select(Head, OldSize, BSize) ->
    Overwrite0 =
	if
	    OldSize =:= BSize -> same;
	    true -> true
	end,
    Overwrite =
	if
	    Head#select.fixed =/= false ->
		false;
	    true ->
		Overwrite0
	end,
    if
	Overwrite =:= same ->
	    a;
	Overwrite ->
	    b;
	true ->
	    c
    end.

y_catch(_Config) ->
    ok = try
	     do_y_catch(<<"<?xmlX">>, {state}),
	     failed
	 catch
	     throw:{<<"<?xmlX">>,{state}} ->
		 ok
	 end.

do_y_catch(<<"<?xml",Rest0/binary>> = Bytes, State0) ->
    {Rest1,State1} =
	case do_y_catch_1(Rest0, State0) of
	    false ->
		{Bytes,State0};
	    true ->
		{_XmlAttributes, R, S} = do_y_catch_2(Rest0),
		{R,S}
	end,
    case catch id({Rest1,State1}) of
	Other ->
	    throw(Other)
    end.

do_y_catch_1(<<_,_/binary>>, _) ->
    false.

do_y_catch_2(_) -> {a,b,c}.


%% The identity function.
id(I) -> I.
