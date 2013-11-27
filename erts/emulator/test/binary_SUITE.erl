%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

-module(binary_SUITE).
-compile({nowarn_deprecated_function, {erlang,hash,2}}).

%% Tests binaries and the BIFs:
%%	list_to_binary/1
%%      iolist_to_binary/1
%%      list_to_bitstring/1
%%	binary_to_list/1
%%	binary_to_list/3
%%	binary_to_term/1
%%  	binary_to_term/2
%%      bitstring_to_list/1
%%	term_to_binary/1
%%      erlang:external_size/1
%%	size(Binary)
%%      iolist_size/1
%%	split_binary/2
%%      hash(Binary, N)
%%      phash(Binary, N)
%%      phash2(Binary, N)
%%

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,
	 copy_terms/1, conversions/1, deep_lists/1, deep_bitstr_lists/1,
	 bad_list_to_binary/1, bad_binary_to_list/1,
	 t_split_binary/1, bad_split/1,
	 terms/1, terms_float/1, float_middle_endian/1,
	 external_size/1, t_iolist_size/1,
	 t_hash/1,
	 bad_size/1,
	 bad_term_to_binary/1,
	 bad_binary_to_term_2/1,safe_binary_to_term2/1,
	 bad_binary_to_term/1, bad_terms/1, more_bad_terms/1,
	 otp_5484/1,otp_5933/1,
	 ordering/1,unaligned_order/1,gc_test/1,
	 bit_sized_binary_sizes/1,
	 otp_6817/1,deep/1,obsolete_funs/1,robustness/1,otp_8117/1,
	 otp_8180/1, ttb_trap/1]).

%% Internal exports.
-export([sleeper/0,ttb_loop/2]).

suite() -> [{ct_hooks,[ts_install_cth]},
	    {timetrap,{minutes,2}}].

all() -> 
    [copy_terms, conversions, deep_lists, deep_bitstr_lists,
     t_split_binary, bad_split,
     bad_list_to_binary, bad_binary_to_list, terms,
     terms_float, float_middle_endian, external_size, t_iolist_size,
     bad_binary_to_term_2, safe_binary_to_term2,
     bad_binary_to_term, bad_terms, t_hash, bad_size,
     bad_term_to_binary, more_bad_terms, otp_5484, otp_5933,
     ordering, unaligned_order, gc_test,
     bit_sized_binary_sizes, otp_6817, otp_8117, deep,
     obsolete_funs, robustness, otp_8180, ttb_trap].

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

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

-define(heap_binary_size, 64).

copy_terms(Config) when is_list(Config) ->
    Self = self(),
    ?line Pid = spawn_link(fun() -> copy_server(Self) end),
    F = fun(Term) ->
		Pid ! Term,
		receive
		    Term -> ok;
		    Other ->
			io:format("Sent: ~P\nGot back:~P", [Term,12,Other,12]),
			?t:fail(bad_term)
		end
	end,
    ?line test_terms(F),
    ok.

copy_server(Parent) ->
    receive
	Term ->
	    Parent ! Term,
	    copy_server(Parent)
    end.

%% Tests list_to_binary/1, binary_to_list/1 and size/1,
%% using flat lists.

conversions(suite) -> [];
conversions(Config) when is_list(Config) ->
    ?line test_bin([]),
    ?line test_bin([1]),
    ?line test_bin([1, 2]),
    ?line test_bin([1, 2, 3]),
    ?line test_bin(lists:seq(0, ?heap_binary_size)),
    ?line test_bin(lists:seq(0, ?heap_binary_size+1)),
    ?line test_bin(lists:seq(0, 255)),
    ?line test_bin(lists:duplicate(50000, $@)),

    %% Binary in list.
    List = [1,2,3,4,5],
    ?line B1 = make_sub_binary(list_to_binary(List)),
    ?line 5 = size(B1),
    ?line 5 = size(make_unaligned_sub_binary(B1)),
    ?line 40 = bit_size(B1),
    ?line 40 = bit_size(make_unaligned_sub_binary(B1)),
    ?line B2 = list_to_binary([42,B1,19]),
    ?line B2 = list_to_binary([42,make_unaligned_sub_binary(B1),19]),
    ?line B2 = iolist_to_binary(B2),
    ?line B2 = iolist_to_binary(make_unaligned_sub_binary(B2)),
    ?line 7 = size(B2),
    ?line 7 = size(make_sub_binary(B2)),
    ?line 56 = bit_size(B2),
    ?line 56 = bit_size(make_sub_binary(B2)),
    ?line [42,1,2,3,4,5,19] = binary_to_list(B2),
    ?line [42,1,2,3,4,5,19] = binary_to_list(make_sub_binary(B2)),
    ?line [42,1,2,3,4,5,19] = binary_to_list(make_unaligned_sub_binary(B2)),
    ?line [42,1,2,3,4,5,19] = bitstring_to_list(B2),
    ?line [42,1,2,3,4,5,19] = bitstring_to_list(make_sub_binary(B2)),
    ?line [42,1,2,3,4,5,19] = bitstring_to_list(make_unaligned_sub_binary(B2)),

    ok.

test_bin(List) ->
    ?line Size = length(List),
    ?line Bin = list_to_binary(List),
    ?line Bin = iolist_to_binary(List),
    ?line Bin = list_to_bitstring(List),
    ?line Size = iolist_size(List),
    ?line Size = iolist_size(Bin),
    ?line Size = iolist_size(make_unaligned_sub_binary(Bin)),
    ?line Size = size(Bin),
    ?line Size = size(make_sub_binary(Bin)),
    ?line Size = size(make_unaligned_sub_binary(Bin)),
    ?line List = binary_to_list(Bin),
    ?line List = binary_to_list(make_sub_binary(Bin)),
    ?line List = binary_to_list(make_unaligned_sub_binary(Bin)),
    ?line List = bitstring_to_list(Bin),
    ?line List = bitstring_to_list(make_unaligned_sub_binary(Bin)).

%% Tests list_to_binary/1, iolist_to_binary/1, list_to_bitstr/1, binary_to_list/1,3,
%% bitstr_to_list/1, and size/1, using deep lists.

deep_lists(Config) when is_list(Config) ->
    ?line test_deep_list(["abc"]),
    ?line test_deep_list([[12,13,[123,15]]]),
    ?line test_deep_list([[12,13,[lists:seq(0, 255), []]]]),
    ok.

test_deep_list(List) ->
    ?line FlatList = lists:flatten(List),
    ?line Size = length(FlatList),
    ?line Bin = list_to_binary(List),
    ?line Bin = iolist_to_binary(List),
    ?line Bin = iolist_to_binary(Bin),
    ?line Bin = list_to_bitstring(List),
    ?line Size = size(Bin),
    ?line Size = iolist_size(List),
    ?line Size = iolist_size(FlatList),
    ?line Size = iolist_size(Bin),
    ?line Bitsize = bit_size(Bin),
    ?line Bitsize = 8*Size,
    ?line FlatList = binary_to_list(Bin),
    ?line FlatList = bitstring_to_list(Bin),
    io:format("testing plain binary..."),
    ?line t_binary_to_list_3(FlatList, Bin, 1, Size),
    io:format("testing unaligned sub binary..."),
    ?line t_binary_to_list_3(FlatList, make_unaligned_sub_binary(Bin), 1, Size).

t_binary_to_list_3(List, Bin, From, To) ->
    ?line going_up(List, Bin, From, To),
    ?line going_down(List, Bin, From, To),
    ?line going_center(List, Bin, From, To).

going_up(List, Bin, From, To) when From =< To ->
    ?line List = binary_to_list(Bin, From, To),
    ?line going_up(tl(List), Bin, From+1, To);
going_up(_List, _Bin, From, To) when From > To ->
    ok.
    
going_down(List, Bin, From, To) when To > 0->
    ?line compare(List, binary_to_list(Bin, From, To), To-From+1),
    ?line going_down(List, Bin, From, To-1);
going_down(_List, _Bin, _From, _To) ->
    ok.

going_center(List, Bin, From, To) when From >= To ->
    ?line compare(List, binary_to_list(Bin, From, To), To-From+1),
    ?line going_center(tl(List), Bin, From+1, To-1);
going_center(_List, _Bin, _From, _To) ->
    ok.

compare([X|Rest1], [X|Rest2], Left) when Left > 0 ->
    ?line compare(Rest1, Rest2, Left-1);
compare([_X|_], [_Y|_], _Left) ->
    ?line test_server:fail();
compare(_List, [], 0) ->
    ok.

deep_bitstr_lists(Config) when is_list(Config) ->
    ?line {<<7:3>>,[<<7:3>>]} = test_deep_bitstr([<<7:3>>]),
    ?line {<<42,5:3>>=Bin,[42,<<5:3>>]=List} = test_deep_bitstr([42,<<5:3>>]),
    ?line {Bin,List} = test_deep_bitstr([42|<<5:3>>]),
    ?line {Bin,List} = test_deep_bitstr([<<42,5:3>>]),
    ?line {Bin,List} = test_deep_bitstr([<<1:3>>,<<10:5>>|<<5:3>>]),
    ?line {Bin,List} = test_deep_bitstr([<<1:3>>,<<10:5>>,<<5:3>>]),
    ?line {Bin,List} = test_deep_bitstr([[<<1:3>>,<<10:5>>],[],<<5:3>>]),
    ?line {Bin,List} = test_deep_bitstr([[[<<1:3>>]|<<10:5>>],[],<<5:3>>]),
    ?line {Bin,List} = test_deep_bitstr([[<<0:1>>,<<0:1>>,[],<<1:1>>,<<10:5>>],
					 <<1:1>>,<<0:1>>,<<1:1>>]),
    ok.

test_deep_bitstr(List) ->
    %%?line {'EXIT',{badarg,_}} = list_to_binary(List),
    Bin = list_to_bitstring(List),
    {Bin,bitstring_to_list(Bin)}.

bad_list_to_binary(suite) -> [];
bad_list_to_binary(Config) when is_list(Config) ->
    ?line test_bad_bin(atom),
    ?line test_bad_bin(42),
    ?line test_bad_bin([1|2]),
    ?line test_bad_bin([256]),
    ?line test_bad_bin([255, [256]]),
    ?line test_bad_bin([-1]),
    ?line test_bad_bin([atom_in_list]),
    ?line test_bad_bin([[<<8>>]|bad_tail]),

    {'EXIT',{badarg,_}} = (catch list_to_binary(id(<<1,2,3>>))),
    {'EXIT',{badarg,_}} = (catch list_to_binary(id([<<42:7>>]))),
    {'EXIT',{badarg,_}} = (catch list_to_bitstring(id(<<1,2,3>>))),
    
    %% Funs used to be implemented as a type of binary internally.
    ?line test_bad_bin(fun(X, Y) -> X*Y end),
    ?line test_bad_bin([1,fun(X) -> X + 1 end,2|fun() -> 0 end]),
    ?line test_bad_bin([fun(X) -> X + 1 end]),

    %% Test iolists that do not fit in the address space.
    %% Unfortunately, it would be too slow to test in a 64-bit emulator.
    case erlang:system_info(wordsize) of
	4 -> huge_iolists();
	_ -> ok
    end.

huge_iolists() ->
    FourGigs = 1 bsl 32,
    ?line Sizes = [FourGigs+N || N <- lists:seq(0, 64)] ++
	[1 bsl N || N <- lists:seq(33, 37)],
    ?line Base = <<0:(1 bsl 20)/unit:8>>,
    [begin
	 L = build_iolist(Sz, Base),
	 ?line {'EXIT',{system_limit,_}} = (catch list_to_binary([L])),
	 ?line {'EXIT',{system_limit,_}} = (catch list_to_bitstring([L])),
	 ?line {'EXIT',{system_limit,_}} = (catch binary:list_to_bin([L])),
	 ?line {'EXIT',{system_limit,_}} = (catch iolist_to_binary(L))
	 end || Sz <- Sizes],
    ok.

test_bad_bin(List) ->
    {'EXIT',{badarg,_}} = (catch list_to_binary(List)),
    {'EXIT',{badarg,_}} = (catch iolist_to_binary(List)),
    {'EXIT',{badarg,_}} = (catch list_to_bitstring(List)),
    {'EXIT',{badarg,_}} = (catch iolist_size(List)).

bad_binary_to_list(doc) -> "Tries binary_to_list/1,3 with bad arguments.";
bad_binary_to_list(Config) when is_list(Config) ->
    ?line bad_bin_to_list(fun(X) -> X * 42 end),

    GoodBin = list_to_binary(lists:seq(1, 10)),
    ?line bad_bin_to_list(fun(X) -> X * 44 end, 1, 2),
    ?line bad_bin_to_list(GoodBin, 0, 1),
    ?line bad_bin_to_list(GoodBin, 2, 1),
    ?line bad_bin_to_list(GoodBin, 11, 11),
    {'EXIT',{badarg,_}} = (catch binary_to_list(id(<<42:7>>))),
    ok.

bad_bin_to_list(BadBin) ->
    {'EXIT',{badarg,_}} = (catch binary_to_list(BadBin)),
    {'EXIT',{badarg,_}} = (catch bitstring_to_list(BadBin)).

bad_bin_to_list(Bin, First, Last) ->
    {'EXIT',{badarg,_}} = (catch binary_to_list(Bin, First, Last)).
    
    
%% Tries to split a binary at all possible positions.

t_split_binary(suite) -> [];
t_split_binary(Config) when is_list(Config) ->
    ?line L = lists:seq(0, ?heap_binary_size-5), %Heap binary.
    ?line B = list_to_binary(L),
    ?line split(L, B, size(B)),

    %% Sub binary of heap binary.
    ?line split(L, make_sub_binary(B), size(B)),
    {X,_Y} = split_binary(B, size(B) div 2),
    ?line split(binary_to_list(X), X, size(X)),

    %% Unaligned sub binary of heap binary.
    ?line split(L, make_unaligned_sub_binary(B), size(B)),
    {X,_Y} = split_binary(B, size(B) div 2),
    ?line split(binary_to_list(X), X, size(X)),
    
    %% Reference-counted binary.
    ?line L2 = lists:seq(0, ?heap_binary_size+1),
    ?line B2 = list_to_binary(L2),
    ?line split(L2, B2, size(B2)),

    %% Sub binary of reference-counted binary.
    ?line split(L2, make_sub_binary(B2), size(B2)),
    {X2,_Y2} = split_binary(B2, size(B2) div 2),
    ?line split(binary_to_list(X2), X2, size(X2)),

    %% Unaligned sub binary of reference-counted binary.
    ?line split(L2, make_unaligned_sub_binary(B2), size(B2)),
    {X2,_Y2} = split_binary(B2, size(B2) div 2),
    ?line split(binary_to_list(X2), X2, size(X2)),

    ok.

split(L, B, Pos) when Pos > 0 ->
    ?line {B1, B2} = split_binary(B, Pos),
    ?line B1 = list_to_binary(lists:sublist(L, 1, Pos)),
    ?line B2 = list_to_binary(lists:nthtail(Pos, L)),
    ?line split(L, B, Pos-1);
split(_L, _B, 0) ->
    ok.

bad_split(doc) -> "Tries split_binary/2 with bad arguments.";
bad_split(suite) -> [];
bad_split(Config) when is_list(Config) ->
    GoodBin = list_to_binary([1,2,3]),
    ?line bad_split(GoodBin, -1),
    ?line bad_split(GoodBin, 4),
    ?line bad_split(GoodBin, a),

    %% Funs are a kind of binaries.
    ?line bad_split(fun(_X) -> 1 end, 1),
    ok.
    
bad_split(Bin, Pos) ->
    {'EXIT',{badarg,_}} = (catch split_binary(Bin, Pos)).

t_hash(doc) -> "Test hash/2 with different type of binaries.";
t_hash(Config) when is_list(Config) ->
    test_hash([]),
    test_hash([253]),
    test_hash(lists:seq(1, ?heap_binary_size)),
    test_hash(lists:seq(1, ?heap_binary_size+1)),
    test_hash([X rem 256 || X <- lists:seq(1, 312)]),
    ok.

test_hash(List) ->
    Bin = list_to_binary(List),
    Sbin = make_sub_binary(List),
    Unaligned = make_unaligned_sub_binary(Sbin),
    ?line test_hash_1(Bin, Sbin, Unaligned, fun erlang:hash/2),
    ?line test_hash_1(Bin, Sbin, Unaligned, fun erlang:phash/2),
    ?line test_hash_1(Bin, Sbin, Unaligned, fun erlang:phash2/2).

test_hash_1(Bin, Sbin, Unaligned, Hash) when is_function(Hash, 2) ->
    N = 65535,
    case {Hash(Bin, N),Hash(Sbin, N),Hash(Unaligned, N)} of
	{H,H,H} -> ok;
	{H1,H2,H3} ->
	    io:format("Different hash values: ~p, ~p, ~p\n", [H1,H2,H3]),
	    ?t:fail()
    end.

bad_size(doc) -> "Try bad arguments to size/1.";
bad_size(suite) -> [];
bad_size(Config) when is_list(Config) ->
    ?line {'EXIT',{badarg,_}} = (catch size(fun(X) -> X + 33 end)),
    ok.

bad_term_to_binary(Config) when is_list(Config) ->
    T = id({a,b,c}),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, not_a_list)),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [blurf])),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [{compressed,-1}])),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [{compressed,10}])),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [{compressed,cucumber}])),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [{compressed}])),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [{version,1}|bad_tail])),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [{minor_version,-1}])),
    ?line {'EXIT',{badarg,_}} = (catch term_to_binary(T, [{minor_version,x}])),

    ok.

%% Tests binary_to_term/1 and term_to_binary/1.

terms(Config) when is_list(Config) ->
    TestFun = fun(Term) ->
		      try
			  S = io_lib:format("~p", [Term]),
			  io:put_chars(S)
		      catch
			  error:badarg ->
			      io:put_chars("bit sized binary")
		      end,
		      Bin = term_to_binary(Term),
		      case erlang:external_size(Bin) of
			  Sz when is_integer(Sz), size(Bin) =< Sz ->
			      ok
		      end,
              Bin1 = term_to_binary(Term, [{minor_version, 1}]),
              case erlang:external_size(Bin1, [{minor_version, 1}]) of
              Sz1 when is_integer(Sz1), size(Bin1) =< Sz1 ->
                  ok
              end,
		      Term = binary_to_term(Bin),
		      Term = binary_to_term(Bin, [safe]),
		      Unaligned = make_unaligned_sub_binary(Bin),
		      Term = binary_to_term(Unaligned),
		      Term = binary_to_term(Unaligned, []),
		      Term = binary_to_term(Bin, [safe]),
		      BinC = erlang:term_to_binary(Term, [compressed]),
		      Term = binary_to_term(BinC),
		      true = size(BinC) =< size(Bin),
		      Bin = term_to_binary(Term, [{compressed,0}]),
		      terms_compression_levels(Term, size(Bin), 1),
		      UnalignedC = make_unaligned_sub_binary(BinC),
		      Term = binary_to_term(UnalignedC)
	      end,
    ?line test_terms(TestFun),
    ok.

terms_compression_levels(Term, UncompressedSz, Level) when Level < 10 ->
    BinC = erlang:term_to_binary(Term, [{compressed,Level}]),
    Term = binary_to_term(BinC),
    Sz = byte_size(BinC),
    true = Sz =< UncompressedSz,
    terms_compression_levels(Term, UncompressedSz, Level+1);
terms_compression_levels(_, _, _) -> ok.

terms_float(Config) when is_list(Config) ->
    ?line test_floats(fun(Term) ->
			      Bin0 = term_to_binary(Term),
			      Bin0 = term_to_binary(Term, [{minor_version,0}]),
			      Term = binary_to_term(Bin0),
			      Bin1 = term_to_binary(Term, [{minor_version,1}]),
			      Term = binary_to_term(Bin1),
			      true = size(Bin1) < size(Bin0),
                  Size0 = erlang:external_size(Term),
                  Size00 = erlang:external_size(Term, [{minor_version, 0}]),
                  Size1 = erlang:external_size(Term, [{minor_version, 1}]),
                  true = (Size0 =:= Size00),
                  true = Size1 < Size0
		      end).

float_middle_endian(Config) when is_list(Config) ->
    %% Testing for roundtrip is not enough.
    ?line <<131,70,63,240,0,0,0,0,0,0>> = term_to_binary(1.0, [{minor_version,1}]),
    ?line 1.0 = binary_to_term(<<131,70,63,240,0,0,0,0,0,0>>).

external_size(Config) when is_list(Config) ->
    %% Build a term whose external size only fits in a big num (on 32-bit CPU).
    ?line external_size_1(16#11111111111111117777777777777777888889999, 0, 16#FFFFFFF),

    %% Test that the same binary aligned and unaligned has the same external size.
    ?line Bin = iolist_to_binary([1,2,3,96]),
    ?line Unaligned = make_unaligned_sub_binary(Bin),
    case {erlang:external_size(Bin),erlang:external_size(Unaligned)} of
	{X,X} -> ok;
	{Sz1,Sz2} ->
	    io:format("  Aligned size: ~p\n", [Sz1]),
	    io:format("Unaligned size: ~p\n", [Sz2]),
	    ?line ?t:fail()
    end,
    ?line erlang:external_size(Bin) =:= erlang:external_size(Bin, [{minor_version, 1}]),
    ?line erlang:external_size(Unaligned) =:= erlang:external_size(Unaligned, [{minor_version, 1}]).

external_size_1(Term, Size0, Limit) when Size0 < Limit ->
    case erlang:external_size(Term) of
	Size when is_integer(Size), Size0 < Size ->
	    io:format("~p", [Size]),
	    external_size_1([Term|Term], Size, Limit)
    end;
external_size_1(_, _, _) -> ok.

t_iolist_size(Config) when is_list(Config) ->
    ?line Seed = now(),
    ?line io:format("Seed: ~p", [Seed]),
    ?line random:seed(Seed),
    ?line Base = <<0:(1 bsl 20)/unit:8>>,
    ?line Powers = [1 bsl N || N <- lists:seq(2, 37)],
    ?line Sizes0 = [[N - random:uniform(N div 2),
		     lists:seq(N-2, N+2),
		     N+N div 2,
		     N + random:uniform(N div 2)] ||
		       N <- Powers],
    %% Test sizes around 1^32 more thoroughly.
    FourGigs = 1 bsl 32,
    ?line Sizes1 = [FourGigs+N || N <- lists:seq(-8, 40)] ++ Sizes0,
    ?line Sizes2 = lists:flatten(Sizes1),
    ?line Sizes = lists:usort(Sizes2),
    io:format("~p sizes:", [length(Sizes)]),
    io:format("~p\n", [Sizes]),
    ?line [Sz = iolist_size(build_iolist(Sz, Base)) || Sz <- Sizes],
    ok.

build_iolist(N, Base) when N < 16 ->
    case random:uniform(3) of
	1 ->
	    <<Bin:N/binary,_/binary>> = Base,
	    Bin;
	_ ->
	    lists:seq(1, N)
    end;
build_iolist(N, Base) when N =< byte_size(Base) ->
    case random:uniform(3) of
	1 ->
	    <<Bin:N/binary,_/binary>> = Base,
	    Bin;
	2 ->
	    <<Bin:N/binary,_/binary>> = Base,
	    [Bin];
	3 ->
	    case N rem 2 of
		0 ->
		    L = build_iolist(N div 2, Base),
		    [L,L];
		1 ->
		    L = build_iolist(N div 2, Base),
		    [L,L,45]
	    end
    end;
build_iolist(N0, Base) ->
    Small = random:uniform(15),
    Seq = lists:seq(1, Small),
    N = N0 - Small,
    case N rem 2 of
	0 ->
	    L = build_iolist(N div 2, Base),
	    [L,L|Seq];
	1 ->
	    L = build_iolist(N div 2, Base),
	    [47,L,L|Seq]
    end.


bad_binary_to_term_2(doc) -> "OTP-4053.";
bad_binary_to_term_2(suite) -> [];
bad_binary_to_term_2(Config) when is_list(Config) ->
    ?line {ok, N} = test_server:start_node(plopp, slave, []),
    ?line R = rpc:call(N, erlang, binary_to_term, [<<131,111,255,255,255,0>>]),
    ?line case R of
	      {badrpc, {'EXIT', _}} ->
		  ok;
	      _Other ->
		  test_server:fail({rpcresult, R})
	  end,
    ?line test_server:stop_node(N),
    ok.

bad_binary_to_term(doc) -> "Try bad input to binary_to_term/1.";
bad_binary_to_term(Config) when is_list(Config) ->
    ?line bad_bin_to_term(an_atom),
    ?line bad_bin_to_term({an,tuple}),
    ?line bad_bin_to_term({a,list}),
    ?line bad_bin_to_term(fun() -> self() end),
    ?line bad_bin_to_term(fun(X) -> 42*X end),
    ?line bad_bin_to_term(fun(X, Y) -> {X,Y} end),
    ?line bad_bin_to_term(fun(X, Y, Z) -> {X,Y,Z} end),
    ?line bad_bin_to_term(bit_sized_binary(term_to_binary({you,should,'not',see,this,term}))),

    %% Bad float.
    ?line bad_bin_to_term(<<131,70,-1:64>>),
    ok.

bad_bin_to_term(BadBin) ->
    {'EXIT',{badarg,_}} = (catch binary_to_term(BadBin)).

bad_bin_to_term(BadBin,Opts) ->
    {'EXIT',{badarg,_}} = (catch binary_to_term(BadBin,Opts)).

safe_binary_to_term2(doc) -> "Test safety options for binary_to_term/2";
safe_binary_to_term2(Config) when is_list(Config) ->
    ?line bad_bin_to_term(<<131,100,0,14,"undefined_atom">>, [safe]),
    ?line bad_bin_to_term(<<131,100,0,14,"other_bad_atom">>, [safe]),
    BadHostAtom = <<100,0,14,"badguy@badhost">>,
    Empty = <<0,0,0,0>>,
    BadRef = <<131,114,0,3,BadHostAtom/binary,0,<<0,0,0,255>>/binary,
	      Empty/binary,Empty/binary>>,
    ?line bad_bin_to_term(BadRef, [safe]), % good ref, with a bad atom
    ?line fullsweep_after = binary_to_term(<<131,100,0,15,"fullsweep_after">>, [safe]), % should be a good atom
    BadExtFun = <<131,113,100,0,4,98,108,117,101,100,0,4,109,111,111,110,97,3>>,
    ?line bad_bin_to_term(BadExtFun, [safe]),
    ok.

%% Tests bad input to binary_to_term/1.

bad_terms(suite) -> [];
bad_terms(Config) when is_list(Config) ->
    ?line test_terms(fun corrupter/1),
    {'EXIT',{badarg,_}} = (catch binary_to_term(<<131,$M,3:32,0,11,22,33>>)),
    {'EXIT',{badarg,_}} = (catch binary_to_term(<<131,$M,3:32,9,11,22,33>>)),
    {'EXIT',{badarg,_}} = (catch binary_to_term(<<131,$M,0:32,1,11,22,33>>)),
    {'EXIT',{badarg,_}} = (catch binary_to_term(<<131,$M,-1:32,1,11,22,33>>)),
    ok.


corrupter(Term) when is_function(Term);
		     is_function(hd(Term));
		     is_function(element(2,element(2,element(2,Term)))) ->
    %% Check if lists is native compiled. If it is, we do not try to
    %% corrupt funs as this can create some very strange behaviour.
    %% To show the error print `Byte` in the foreach fun in corrupter/2.
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    corrupter0(Term);
	Architecture ->
	    {lists, ListsBinary, _ListsFilename} = code:get_object_code(lists),
	    ChunkName = hipe_unified_loader:chunk_name(Architecture),
	    NativeChunk = beam_lib:chunks(ListsBinary, [ChunkName]),
	    case NativeChunk of
		{ok,{_,[{_,Bin}]}} when is_binary(Bin) ->
		    S = io_lib:format("Skipping corruption of: ~P", [Term,12]),
		    io:put_chars(S);
		{error, beam_lib, _} ->
		    corrupter0(Term)
	    end
    end;
corrupter(Term) ->
    corrupter0(Term).

corrupter0(Term) ->
    ?line try
	      S = io_lib:format("About to corrupt: ~P", [Term,12]),
	      io:put_chars(S)
	  catch
	      error:badarg ->
		  io:format("About to corrupt: <<bit-level-binary:~p",
			    [bit_size(Term)])
	  end,
    ?line Bin = term_to_binary(Term),
    ?line corrupter(Bin, size(Bin)-1),
    ?line CompressedBin = term_to_binary(Term, [compressed]),
    ?line corrupter(CompressedBin, size(CompressedBin)-1).

corrupter(Bin, Pos) when Pos >= 0 ->
    ?line {ShorterBin, Rest} = split_binary(Bin, Pos),
    ?line catch binary_to_term(ShorterBin), %% emulator shouldn't crash
    ?line MovedBin = list_to_binary([ShorterBin]),
    ?line catch binary_to_term(MovedBin), %% emulator shouldn't crash

    %% Bit faults, shouldn't crash
    <<Byte,Tail/binary>> = Rest,
    Fun = fun(M) -> FaultyByte = Byte bxor M,                    
		    catch binary_to_term(<<ShorterBin/binary,
					  FaultyByte, Tail/binary>>) end,
    ?line lists:foreach(Fun,[1,2,4,8,16,32,64,128,255]),    
    ?line corrupter(Bin, Pos-1);
corrupter(_Bin, _) ->
    ok.

more_bad_terms(suite) -> [];
more_bad_terms(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line BadFile = filename:join(Data, "bad_binary"),
    ?line ok = io:format("File: ~s\n", [BadFile]),
    ?line case file:read_file(BadFile) of
	      {ok,Bin} ->
		  ?line {'EXIT',{badarg,_}} = (catch binary_to_term(Bin)),
		  ok;
	      Other ->
		  ?line ?t:fail(Other)
	  end.

otp_5484(Config) when is_list(Config) ->
    ?line {'EXIT',_} =
	(catch
	     binary_to_term(
	       <<131,
		104,2,				%Tuple, 2 elements
		103,				%Pid
		100,0,20,"wslin1427198@wslin14",
		%% Obviously bad values follow.
		255,255,255,255,
		255,255,255,255,
		255,
		106>>)),

    ?line {'EXIT',_} =
	(catch
	     binary_to_term(
	       <<131,
		104,2,				%Tuple, 2 elements
		103,				%Pid
		106,				%[] instead of atom.
		0,0,0,17,
		0,0,0,135,
		2,
		106>>)),

    ?line {'EXIT',_} =
	(catch
	     binary_to_term(
	       %% A old-type fun in a list containing a bad creator pid.
	       <<131,108,0,0,0,1,117,0,0,0,0,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,115,116,255,255,0,25,255,0,0,0,0,100,0,1,116,97,0,98,6,142,121,72,106>>)),

    ?line {'EXIT',_} =
	(catch
	     binary_to_term(
	       %% A new-type fun in a list containing a bad creator pid.
	       %% 
	       <<131,
		108,0,0,0,1,			%List, 1 element
		112,0,0,0,66,0,52,216,81,158,148,250,237,109,185,9,208,60,202,156,244,218,0,0,0,0,0,0,0,0,100,0,1,116,97,0,98,6,142,121,72,
		103,				%Pid.
		106,				%[] instead of an atom.
		0,0,0,27,0,0,0,0,0,106>>)),

    ?line {'EXIT',_} =
	(catch
	     binary_to_term(
	       %% A new-type fun in a list containing a bad module.
	       <<131,
		108,0,0,0,1,			%List, 1 element
		112,0,0,0,70,0,224,90,4,101,48,28,110,228,153,48,239,169,232,77,108,145,0,0,0,0,0,0,0,2,
		%%100,0,1,116,
		107,0,1,64,			%String instead of atom (same length).
		97,0,98,6,64,82,230,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,115,116,0,0,0,48,0,0,0,0,0,97,42,97,7,106>>)),

    ?line {'EXIT',_} =
	(catch
	     binary_to_term(
	       %% A new-type fun in a list containing a bad index.
	       <<131,
		108,0,0,0,1,			%List, 1 element
		112,0,0,0,70,0,224,90,4,101,48,28,110,228,153,48,239,169,232,77,108,145,0,0,0,0,0,0,0,2,
		100,0,1,116,
		%%97,0,				%Integer: 0.
		104,0,				%Tuple {} instead of integer.
		98,6,64,82,230,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,115,116,0,0,0,48,0,0,0,0,0,97,42,97,7,106>>)),

    ?line {'EXIT',_} =
	(catch
	     binary_to_term(
	       %% A new-type fun in a list containing a bad unique value.
	       <<131,
		108,0,0,0,1,			%List, 1 element
		112,0,0,0,70,0,224,90,4,101,48,28,110,228,153,48,239,169,232,77,108,145,0,0,0,0,0,0,0,2,
		100,0,1,116,
		97,0,				%Integer: 0.
		%%98,6,64,82,230,		%Integer.
		100,0,2,64,65,			%Atom instead of integer.
		103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,115,116,0,0,0,48,0,0,0,0,0,97,42,97,7,106>>)),

    %% An absurdly large atom.
    ?line {'EXIT',_} = 
	(catch binary_to_term(iolist_to_binary([<<131,100,65000:16>>|
						lists:duplicate(65000, 42)]))),

    %% Longer than 255 characters.
    ?line {'EXIT',_} = 
	(catch binary_to_term(iolist_to_binary([<<131,100,256:16>>|
						lists:duplicate(256, 42)]))),

    %% OTP-7218. Thanks to Matthew Dempsky. Also make sure that we
    %% cover the other error cases for external funs (EXPORT_EXT).
    ?line {'EXIT',_} = 
	(catch binary_to_term(
		 <<131,
		  113,				%EXPORT_EXP
		  97,13,			%Integer: 13
		  97,13,			%Integer: 13
		  97,13>>)),			%Integer: 13
    ?line {'EXIT',_} = 
	(catch binary_to_term(
		 <<131,
		  113,				%EXPORT_EXP
		  100,0,1,64,			%Atom: '@'
		  97,13,			%Integer: 13
		  97,13>>)),			%Integer: 13
    ?line {'EXIT',_} = 
	(catch binary_to_term(
		 <<131,
		  113,				%EXPORT_EXP
		  100,0,1,64,			%Atom: '@'
		  100,0,1,64,			%Atom: '@'
		  106>>)),			%NIL
    ?line {'EXIT',_} = 
	(catch binary_to_term(
		 <<131,
		  113,				%EXPORT_EXP
		  100,0,1,64,			%Atom: '@'
		  100,0,1,64,			%Atom: '@'
		  98,255,255,255,255>>)),	%Integer: -1
    ?line {'EXIT',_} = 
	(catch binary_to_term(
		 <<131,
		  113,				%EXPORT_EXP
		  100,0,1,64,			%Atom: '@'
		  100,0,1,64,			%Atom: '@'
		  113,97,13,97,13,97,13>>)),	%fun 13:13/13

    %% Bad funs.
    ?line {'EXIT',_} = (catch binary_to_term(fake_fun(0, lists:seq(0, 256)))),
    ok.

fake_fun(Arity, Env0) ->
    Uniq = erlang:md5([]),
    Index = 0,
    NumFree = length(Env0),
    Mod = list_to_binary(?MODULE_STRING),
    OldIndex = 0,
    OldUniq = 16#123456,
    <<131,Pid/binary>> = term_to_binary(self()),
    Env1 = [term_to_binary(Term) || Term <- Env0],
    Env = << <<Bin/binary>> || <<131,Bin/binary>> <- Env1 >>,
    B = <<Arity,Uniq/binary,Index:32,NumFree:32,
	 $d,(byte_size(Mod)):16,Mod/binary,	%Module.
	 $a,OldIndex:8,
	 $b,OldUniq:32,
	 Pid/binary,Env/binary>>,
    <<131,$p,(byte_size(B)+4):32,B/binary>>.


%% More bad terms submitted by Matthias Lang.
otp_5933(Config) when is_list(Config) ->
    ?line try_bad_lengths(<<131,$m>>),		%binary
    ?line try_bad_lengths(<<131,$n>>),		%bignum
    ?line try_bad_lengths(<<131,$o>>),		%huge bignum
    ok.

try_bad_lengths(B) ->
    try_bad_lengths(B, 16#FFFFFFFF).

try_bad_lengths(B, L) when L > 16#FFFFFFF0 ->
    Bin = <<B/binary,L:32>>,
    io:format("~p\n", [Bin]),
    {'EXIT',_} = (catch binary_to_term(Bin)),
    try_bad_lengths(B, L-1);
try_bad_lengths(_, _) -> ok.


otp_6817(Config) when is_list(Config) ->
    process_flag(min_heap_size, 20000),		%Use the heap, not heap fragments.

    %% Floats are only validated when the heap fragment has been allocated.
    BadFloat = <<131,99,53,46,48,$X,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,101,45,48,49,0,0,0,0,0>>,
    ?line otp_6817_try_bin(BadFloat),

    %% {Binary,BadFloat}: When the error in float is discovered, a refc-binary
    %% has been allocated and the list of refc-binaries goes through the
    %% limbo area between the heap top and stack.
    BinAndFloat = 
	<<131,104,2,109,0,0,1,0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
	 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
	 46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,
	 71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,
	 96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
	 116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,
	 135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,
	 154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,
	 173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
	 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,
	 211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,
	 230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,
	 249,250,251,252,253,254,255,99,51,46,49,52,$B,$l,$u,$r,$f,48,48,48,48,48,48,
	 48,48,49,50,52,51,52,101,43,48,48,0,0,0,0,0>>,
    ?line otp_6817_try_bin(BinAndFloat),

    %% {Fun,BadFloat}
    FunAndFloat =
	<<131,104,2,112,0,0,0,66,0,238,239,135,138,137,216,89,57,22,111,52,126,16,84,
	 71,8,0,0,0,0,0,0,0,0,100,0,1,116,97,0,98,5,175,169,123,103,100,0,13,110,111,
	 110,111,100,101,64,110,111,104,111,115,116,0,0,0,41,0,0,0,0,0,99,50,46,55,48,
	 $Y,57,57,57,57,57,57,57,57,57,57,57,57,57,54,52,52,55,101,43,48,48,0,0,0,0,0>>,
    ?line otp_6817_try_bin(FunAndFloat),

    %% [ExternalPid|BadFloat]
    ExtPidAndFloat =
	<<131,108,0,0,0,1,103,100,0,13,107,97,108,108,101,64,115,116,114,105,100,101,
	 114,0,0,0,36,0,0,0,0,2,99,48,46,$@,48,48,48,48,48,48,48,48,48,48,48,48,48,48,
	 48,48,48,48,48,101,43,48,48,0,0,0,0,0>>,
    ?line otp_6817_try_bin(ExtPidAndFloat),
    ok.

otp_6817_try_bin(Bin) ->
    erlang:garbage_collect(),

    %% If the bug is present, the heap pointer will moved when the invalid term
    %% is found and we will have a linked list passing through the limbo area
    %% between the heap top and the stack pointer.
    catch binary_to_term(Bin),

    %% If the bug is present, we will overwrite the pointers in the limbo area.
    Filler = erlang:make_tuple(1024, 16#3FA),
    id(Filler),

    %% Will crash if the bug is present.
    erlang:garbage_collect().

otp_8117(doc) -> "Some bugs in binary_to_term when 32-bit integers are negative.";
otp_8117(suite) -> [];
otp_8117(Config) when is_list(Config) ->
    [otp_8117_do(Op,-(1 bsl N)) || Op <- ['fun',list,tuple],
				   N <- lists:seq(0,31)],
    ok.

otp_8117_do('fun',Neg) ->
    % Fun with negative num_free
    FunBin = term_to_binary(fun() -> ok end),
    ?line <<B1:27/binary,_NumFree:32,Rest/binary>> = FunBin,   
    ?line bad_bin_to_term(<<B1/binary,Neg:32,Rest/binary>>);
otp_8117_do(list,Neg) ->
    %% List with negative length
    ?line bad_bin_to_term(<<131,104,2,108,Neg:32,97,11,104,1,97,12,97,13,106,97,14>>);
otp_8117_do(tuple,Neg) ->    
    %% Tuple with negative arity
    ?line bad_bin_to_term(<<131,104,2,105,Neg:32,97,11,97,12,97,13,97,14>>).
    

ordering(doc) -> "Tests ordering of binaries.";
ordering(suite) -> [];
ordering(Config) when is_list(Config) ->
    B1 = list_to_binary([7,8,9]),
    B2 = make_sub_binary([1,2,3,4]),
    B3 = list_to_binary([1,2,3,5]),
    Unaligned = make_unaligned_sub_binary(B2),

    %% From R8 binaries are compared as strings.

    ?line false = B1 == B2,
    ?line false = B1 =:= B2,
    ?line true = B1 /= B2,
    ?line true = B1 =/= B2,

    ?line true = B1 > B2,
    ?line true = B2 < B3,
    ?line true = B2 =< B1,
    ?line true = B2 =< B3,

    ?line true = B2 =:= Unaligned,
    ?line true = B2 == Unaligned,
    ?line true = Unaligned < B3,
    ?line true = Unaligned =< B3,

    %% Binaries are greater than all other terms.

    ?line true = B1 > 0,
    ?line true = B1 > 39827491247298471289473333333333333333333333333333,
    ?line true = B1 > -3489274937438742190467869234328742398347,
    ?line true = B1 > 3.14,
    ?line true = B1 > [],
    ?line true = B1 > [a],
    ?line true = B1 > {a},
    ?line true = B1 > self(),
    ?line true = B1 > make_ref(),
    ?line true = B1 > xxx,
    ?line true = B1 > fun() -> 1 end,
    ?line true = B1 > fun erlang:send/2,

    ?line Path = ?config(priv_dir, Config),
    ?line AFile = filename:join(Path, "vanilla_file"),
    ?line Port = open_port(AFile, [out]),
    ?line true = B1 > Port,

    ?line true = B1 >= 0,
    ?line true = B1 >= 39827491247298471289473333333333333333333333333333,
    ?line true = B1 >= -3489274937438742190467869234328742398347,
    ?line true = B1 >= 3.14,
    ?line true = B1 >= [],
    ?line true = B1 >= [a],
    ?line true = B1 >= {a},
    ?line true = B1 >= self(),
    ?line true = B1 >= make_ref(),
    ?line true = B1 >= xxx,
    ?line true = B1 >= fun() -> 1 end,
    ?line true = B1 >= fun erlang:send/2,
    ?line true = B1 >= Port,

    ok.

%% Test that comparisions between binaries with different alignment work.
unaligned_order(Config) when is_list(Config) ->
    L = lists:seq(0, 7),
    [test_unaligned_order(I, J) || I <- L, J <- L], 
    ok.

test_unaligned_order(I, J) ->
    Align = {I,J},
    io:format("~p ~p", [I,J]),
    ?line true = test_unaligned_order_1('=:=', <<1,2,3,16#AA,16#7C,4,16#5F,5,16#5A>>,
					<<1,2,3,16#AA,16#7C,4,16#5F,5,16#5A>>,
					Align),
    ?line false = test_unaligned_order_1('=/=', <<1,2,3>>, <<1,2,3>>, Align),
    ?line true = test_unaligned_order_1('==', <<4,5,6>>, <<4,5,6>>, Align),
    ?line false = test_unaligned_order_1('/=', <<1,2,3>>, <<1,2,3>>, Align),

    ?line true = test_unaligned_order_1('<', <<1,2>>, <<1,2,3>>, Align),
    ?line true = test_unaligned_order_1('=<', <<1,2>>, <<1,2,3>>, Align),
    ?line true = test_unaligned_order_1('=<', <<1,2,7,8>>, <<1,2,7,8>>, Align),
    ok.

test_unaligned_order_1(Op, A, B, {Aa,Ba}) ->
    erlang:Op(unaligned_sub_bin(A, Aa), unaligned_sub_bin(B, Ba)).
    
test_terms(Test_Func) ->
    ?line Test_Func(atom),
    ?line Test_Func(''),
    ?line Test_Func('a'),
    ?line Test_Func('ab'),
    ?line Test_Func('abc'),
    ?line Test_Func('abcd'),
    ?line Test_Func('abcde'),
    ?line Test_Func('abcdef'),
    ?line Test_Func('abcdefg'),
    ?line Test_Func('abcdefgh'),

    ?line Test_Func(fun() -> ok end),
    X = id([a,{b,c},c]),
    Y = id({x,y,z}),
    Z = id(1 bsl 8*257),
    ?line Test_Func(fun() -> X end),
    ?line Test_Func(fun() -> {X,Y} end),
    ?line Test_Func([fun() -> {X,Y,Z} end,
		     fun() -> {Z,X,Y} end,
		     fun() -> {Y,Z,X} end]),

    ?line Test_Func({trace_ts,{even_bigger,{some_data,fun() -> ok end}},{1,2,3}}),
    ?line Test_Func({trace_ts,{even_bigger,{some_data,<<1,2,3,4,5,6,7,8,9,10>>}},
		     {1,2,3}}),

    ?line Test_Func(1),
    ?line Test_Func(42),
    ?line Test_Func(-23),
    ?line Test_Func(256),
    ?line Test_Func(25555),
    ?line Test_Func(-3333),

    ?line Test_Func(1.0),

    ?line Test_Func(183749783987483978498378478393874),
    ?line Test_Func(-37894183749783987483978498378478393874),
    Very_Big = very_big_num(),
    ?line Test_Func(Very_Big),
    ?line Test_Func(-Very_Big+1),

    ?line Test_Func([]),
    ?line Test_Func("abcdef"),
    ?line Test_Func([a, b, 1, 2]),
    ?line Test_Func([a|b]),

    ?line Test_Func({}),
    ?line Test_Func({1}),
    ?line Test_Func({a, b}),
    ?line Test_Func({a, b, c}),
    ?line Test_Func(list_to_tuple(lists:seq(0, 255))),
    ?line Test_Func(list_to_tuple(lists:seq(0, 256))),

    ?line Test_Func(make_ref()),
    ?line Test_Func([make_ref(), make_ref()]),

    ?line Test_Func(make_port()),

    ?line Test_Func(make_pid()),

    ?line Test_Func(Bin0 = list_to_binary(lists:seq(0, 14))),
    ?line Test_Func(Bin1 = list_to_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(Bin2 = list_to_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(Bin3 = list_to_binary(lists:seq(0, 255))),

    ?line Test_Func(make_unaligned_sub_binary(Bin0)),
    ?line Test_Func(make_unaligned_sub_binary(Bin1)),
    ?line Test_Func(make_unaligned_sub_binary(Bin2)),
    ?line Test_Func(make_unaligned_sub_binary(Bin3)),

    ?line Test_Func(make_sub_binary(lists:seq(42, 43))),
    ?line Test_Func(make_sub_binary([42,43,44])),
    ?line Test_Func(make_sub_binary([42,43,44,45])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46,47])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46,47,48])),
    ?line Test_Func(make_sub_binary(lists:seq(42, 49))),
    ?line Test_Func(make_sub_binary(lists:seq(0, 14))),
    ?line Test_Func(make_sub_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(make_sub_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(make_sub_binary(lists:seq(0, 255))),

    ?line Test_Func(make_unaligned_sub_binary(lists:seq(42, 43))),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46,47])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46,47,48])),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(42, 49))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, 14))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, 255))),

    %% Bit level binaries.
    ?line Test_Func(<<1:1>>),
    ?line Test_Func(<<2:2>>),
    ?line Test_Func(<<42:10>>),
    ?line Test_Func(list_to_bitstring([<<5:6>>|lists:seq(0, 255)])),

    ?line Test_Func(F = fun(A) -> 42*A end),
    ?line Test_Func(lists:duplicate(32, F)),

    ?line Test_Func(FF = fun binary_SUITE:all/0),
    ?line Test_Func(lists:duplicate(32, FF)),

    ok.

test_floats(Test_Func) ->
    ?line Test_Func(5.5),
    ?line Test_Func(-15.32),
    ?line Test_Func(1.2435e25),
    ?line Test_Func(1.2333e-20),
    ?line Test_Func(199.0e+15),
    ok.

very_big_num() ->
    very_big_num(33, 1).

very_big_num(Left, Result) when Left > 0 ->
    ?line very_big_num(Left-1, Result*256);
very_big_num(0, Result) ->
    ?line Result.

make_port() ->
    ?line open_port({spawn, efile}, [eof]).

make_pid() ->
    ?line spawn_link(?MODULE, sleeper, []).

sleeper() ->
    ?line receive after infinity -> ok end.


%% Test that binaries are garbage collected properly.
gc_test(Config) when is_list(Config) ->
    %% Note: This test is only relevant for REFC binaries.
    %% Therefore, we take care that all binaries are REFC binaries.
    B = list_to_binary(lists:seq(0, ?heap_binary_size)),
    Self = self(),
    F1 = fun() ->
		 gc(),
		 {binary,[]} = process_info(self(), binary),
		 Self ! {self(),done}
	 end,
    F = fun() ->
		receive go -> ok end,
		{binary,[{_,65,1}]} = process_info(self(), binary),
		gc(),
		{B1,B2} = my_split_binary(B, 4),
		gc(),
		gc(),
		{binary,L1} = process_info(self(), binary),
		[Binfo1,Binfo2,Binfo3] = L1,
		{_,65,3} = Binfo1 = Binfo2 = Binfo3,
		65 = size(B),
		4 = size(B1),
		61 = size(B2),
		F1()
	end,
    gc(),
    gc(),
    65 = size(B),
    gc_test1(spawn_opt(erlang, apply, [F,[]], [link,{fullsweep_after,0}])).

gc_test1(Pid) ->
    gc(),
    Pid ! go,
    receive
	{Pid,done} -> ok
    after 10000 ->
	    ?line ?t:fail()
    end.

%% Like split binary, but returns REFC binaries. Only useful for gc_test/1.

my_split_binary(B, Pos) ->
    Self = self(),
    Ref = make_ref(),
    spawn(fun() -> Self ! {Ref,split_binary(B, Pos)} end),
    receive
	{Ref,Result} -> Result
    end.

gc() ->
    erlang:garbage_collect(),
    gc1().
gc1() -> ok.

bit_sized_binary_sizes(Config) when is_list(Config) ->
    ?line [bsbs_1(A) || A <- lists:seq(1, 8)],
    ok.

bsbs_1(A) ->
    BinSize = 32+A,
    io:format("A: ~p BinSize: ~p", [A,BinSize]),
    Bin = binary_to_term(<<131,$M,5:32,A,0,0,0,0,0>>),
    BinSize = bit_size(Bin).

deep(Config) when is_list(Config) ->
    ?line deep_roundtrip(lists:foldl(fun(E, A) ->
					     [E,A]
				     end, [], lists:seq(1, 1000000))),
    ?line deep_roundtrip(lists:foldl(fun(E, A) ->
					     {E,A}
				     end, [], lists:seq(1, 1000000))),
    ?line deep_roundtrip(lists:foldl(fun(E, A) ->
					     fun() -> {E,A} end
				     end, [], lists:seq(1, 1000000))),
    ok.

deep_roundtrip(T) ->
    B = term_to_binary(T),
    T = binary_to_term(B).

obsolete_funs(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),

    X = id({1,2,3}),
    Y = id([a,b,c,d]),
    Z = id({x,y,z}),
    ?line obsolete_fun(fun() -> ok end),
    ?line obsolete_fun(fun() -> X end),
    ?line obsolete_fun(fun(A) -> {A,X} end),
    ?line obsolete_fun(fun() -> {X,Y} end),
    ?line obsolete_fun(fun() -> {X,Y,Z} end),

    ?line obsolete_fun(fun ?MODULE:all/1),

    erts_debug:set_internal_state(available_internal_state, false),
    ok.

obsolete_fun(Fun) ->
    Tuple = case erlang:fun_info(Fun, type) of
		{type,external} ->
		    {module,M} = erlang:fun_info(Fun, module),
		    {name,F} = erlang:fun_info(Fun, name),
		    {M,F};
		{type,local} ->
		    {module,M} = erlang:fun_info(Fun, module),
		    {index,I} = erlang:fun_info(Fun, index),
		    {uniq,U} = erlang:fun_info(Fun, uniq),
		    {env,E} = erlang:fun_info(Fun, env),
		    {'fun',M,I,U,list_to_tuple(E)}
	    end,
    Tuple = no_fun_roundtrip(Fun).

no_fun_roundtrip(Term) ->
    binary_to_term(erts_debug:get_internal_state({term_to_binary_no_funs,Term})).

%% Test non-standard encodings never generated by term_to_binary/1
%% but recognized by binary_to_term/1.

robustness(Config) when is_list(Config) ->
    ?line [] = binary_to_term(<<131,107,0,0>>),	%Empty string.
    ?line [] = binary_to_term(<<131,108,0,0,0,0,106>>),	%Zero-length list.

    %% {[],a} where [] is a zero-length list.
    ?line {[],a} = binary_to_term(<<131,104,2,108,0,0,0,0,106,100,0,1,97>>),

    %% {42,a} where 42 is a zero-length list with 42 in the tail.
    ?line {42,a} = binary_to_term(<<131,104,2,108,0,0,0,0,97,42,100,0,1,97>>),

    %% {{x,y},a} where {x,y} is a zero-length list with {x,y} in the tail.
    ?line {{x,y},a} = binary_to_term(<<131,104,2,108,0,0,0,0,
				      104,2,100,0,1,120,100,0,1,
				      121,100,0,1,97>>),

    %% Bignums fitting in 32 bits.
    ?line 16#7FFFFFFF = binary_to_term(<<131,98,127,255,255,255>>),
    ?line -1 = binary_to_term(<<131,98,255,255,255,255>>),
    
    ok.

%% OTP-8180: Test several terms that have been known to crash the emulator.
%% (Thanks to Scott Lystig Fritchie.)
otp_8180(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line Wc = filename:join(Data, "zzz.*"),
    Files = filelib:wildcard(Wc),
    [run_otp_8180(F) || F <- Files],
    ok.

run_otp_8180(Name) ->
    io:format("~s", [Name]),
    ?line {ok,Bins} = file:consult(Name),
    [begin
	 io:format("~p\n", [Bin]),
	 ?line {'EXIT',{badarg,_}} = (catch binary_to_term(Bin))
     end || Bin <- Bins],
    ok.

%% Test that exit and GC during term_to_binary trap does not crash.
ttb_trap(Config) when is_list(Config)->
    case erlang:system_info(wordsize) of
	N when N < 8 ->
	    {skipped, "Only on 64bit machines"};
	_ ->
	    do_ttb_trap(5)
    end.

do_ttb_trap(0) ->
    ok;
do_ttb_trap(N) ->
    Pid = spawn(?MODULE,ttb_loop,[1000,self()]),
    receive ok -> ok end,
    receive after 100 -> ok end,
    erlang:garbage_collect(Pid),
    receive after 100 -> ok end,
    exit(Pid,kill),
    receive after 1 -> ok end,
    do_ttb_trap(N-1).

ttb_loop(N,Pid) ->
    Term = lists:duplicate(2000000,2000000),
    Pid ! ok,
    ttb_loop2(N,Term).
ttb_loop2(0,_T) ->
    ok;
ttb_loop2(N,T) ->
    apply(erlang,term_to_binary,[T]),
    ttb_loop2(N-1,T).


%% Utilities.

make_sub_binary(Bin) when is_binary(Bin) ->
    {_,B} = split_binary(list_to_binary([0,1,3,Bin]), 3),
    B;
make_sub_binary(List) ->
    make_sub_binary(list_to_binary(List)).

make_unaligned_sub_binary(Bin0) when is_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin;
make_unaligned_sub_binary(List) ->
    make_unaligned_sub_binary(list_to_binary(List)).

%% Add 1 bit to the size of the binary.
bit_sized_binary(Bin0) ->
    Bin = <<Bin0/binary,1:1>>,
    BitSize = bit_size(Bin),
    BitSize = 8*size(Bin) + 1,
    Bin.

unaligned_sub_bin(Bin, 0) -> Bin;
unaligned_sub_bin(Bin0, Offs) ->
    F = random:uniform(256),
    Roffs = 8-Offs,
    Bin1 = <<F:Offs,Bin0/binary,F:Roffs>>,
    Sz = size(Bin0),
    <<_:Offs,Bin:Sz/binary,_:Roffs>> = id(Bin1),
    Bin.

id(I) -> I.
