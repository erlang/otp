%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

%%
%% Verifying erlang:phash/2. And now also phash2/2, to some extent.
%% Test the hashing algorithm for integer numbers in 2 ways:
%% 1 Test that numbers in diferent sequences get sufficiently spread
%%   in a "bit pattern" way (modulo 256 etc).
%% 2 Test that numbers are correctly hashed compared to a reference implementation,
%%   regardless of their internal representation. The hashing algorithm should never 
%%   change. 
%% The hashing of other datatypes is tested with a few samples, so that we are sure 
%% it does not change across versions.
%% Also tests that the limit can be between 0 and 16#FFFFFFFF.
%%
-module(hash_SUITE).
-export([basic_test/0,cmp_test/1,range_test/0,spread_test/1,
	 phash2_test/0, otp_5292_test/0,
         otp_7127_test/0, 
         run_phash2_benchmarks/0,
         test_phash2_binary_aligned_and_unaligned_equal/1,
         test_phash2_4GB_plus_bin/1,
         test_phash2_10MB_plus_bin/1,
         test_phash2_large_map/1,
         test_phash2_shallow_long_list/1,
         test_phash2_deep_list/1,
         test_phash2_deep_tuple/1,
         test_phash2_deep_tiny/1,
         test_phash2_with_42/1,
         test_phash2_with_short_tuple/1,
         test_phash2_with_short_list/1,
         test_phash2_with_tiny_bin/1,
         test_phash2_with_tiny_unaligned_sub_binary/1,
         test_phash2_with_small_unaligned_sub_binary/1,
         test_phash2_with_large_bin/1,
         test_phash2_with_large_unaligned_sub_binary/1,
         test_phash2_with_super_large_unaligned_sub_binary/1]).

%%
%% Define to run outside of test server
%%
%-define(STANDALONE,1).

%%
%% Define for debug output
%%
-define(debug,1).

-ifdef(STANDALONE).
-define(config(A,B),config(A,B)).
-record(event, {name, data}).
-export([config/2]).
-else.
-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-endif.

-ifdef(debug).
-ifdef(STANDALONE).
-define(line, erlang:display({?MODULE,?LINE}), ).
-endif.
-define(dbgformat(A,B),io:format(A,B)).
-else.
-ifdef(STANDALONE).
-define(line, noop, ).
-endif.
-define(dbgformat(A,B),noop).
-endif.

-ifdef(STANDALONE).
config(priv_dir,_) ->
    ".".
notify(X) -> 
    erlang:display(X).
-else.
%% When run in test server.
-export([groups/0, all/0, suite/0,
	 test_basic/1,test_cmp/1,test_range/1,test_spread/1,
	 test_phash2/1,otp_5292/1,bit_level_binaries/1,otp_7127/1,
         test_hash_zero/1, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() -> 
    [test_basic, test_cmp, test_range, test_spread,
     test_phash2, otp_5292, bit_level_binaries, otp_7127,
     test_hash_zero, test_phash2_binary_aligned_and_unaligned_equal,
     test_phash2_4GB_plus_bin,
     test_phash2_10MB_plus_bin,
     {group, phash2_benchmark_tests},
     {group, phash2_benchmark}].

get_phash2_benchmarks() ->
    [
     test_phash2_large_map,
     test_phash2_shallow_long_list,
     test_phash2_deep_list,
     test_phash2_deep_tuple,
     test_phash2_deep_tiny,
     test_phash2_with_42,
     test_phash2_with_short_tuple,
     test_phash2_with_short_list,
     test_phash2_with_tiny_bin,
     test_phash2_with_tiny_unaligned_sub_binary,
     test_phash2_with_small_unaligned_sub_binary,
     test_phash2_with_large_bin,
     test_phash2_with_large_unaligned_sub_binary,
     test_phash2_with_super_large_unaligned_sub_binary
    ].

groups() -> 
    [
     {
      phash2_benchmark_tests,
      [],
      get_phash2_benchmarks()
     },
     {
      phash2_benchmark,
      [],
      get_phash2_benchmarks()
     }
    ].


init_per_suite(Config) ->
    io:format("START APPS~n"),
    A0 = case application:start(sasl) of
	     ok -> [sasl];
	     _ -> []
	 end,
    A = case application:start(os_mon) of
	     ok -> [os_mon|A0];
	     _ -> A0
	 end,
    io:format("APPS STARTED~n"),
    [{started_apps, A}|Config].

end_per_suite(Config) ->
    As = proplists:get_value(started_apps, Config),
    lists:foreach(fun (A) -> application:stop(A) end, As),
    Config.

init_per_group(phash2_benchmark_tests, Config) ->
    [phash2_benchmark_tests |Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.


%% Tests basic functionality of erlang:phash and that the
%% hashes has not changed (neither hash nor phash)
test_basic(Config) when is_list(Config) ->
    basic_test().


%% Compares integer hashes made by erlang:phash with those of a reference implementation
test_cmp(Config) when is_list(Config) ->
    cmp_test(10000).

%% Tests ranges on erlang:phash from 1 to 2^32
test_range(Config) when is_list(Config) ->
    range_test().

%% Tests that the hashes are spread ok
test_spread(Config) when is_list(Config) ->
    spread_test(10).

%% Tests phash2
test_phash2(Config) when is_list(Config) ->
    phash2_test().

%% Tests hash, phash and phash2 regarding integers.
otp_5292(Config) when is_list(Config) ->
    otp_5292_test().

%% Test hashing bit-level binaries.
bit_level_binaries(Config) when is_list(Config) ->
    bit_level_binaries_do().

%% Tests phash2/1.
otp_7127(Config) when is_list(Config) ->
    otp_7127_test().

test_hash_zero(Config) when is_list(Config) ->
    hash_zero_test().

notify(X) ->
    ct_event:notify(X).
-endif.



%%
%% Here are the real tests, they can be run without test_server, 
%% define -DSTANDALONE when compiling.
%%
basic_test() ->
    685556714 = erlang:phash({a,b,c},16#FFFFFFFF),
    37442646 =  erlang:phash([a,b,c,{1,2,3},c:pid(0,2,3),
				    16#77777777777777],16#FFFFFFFF),
    ExternalReference = <<131,114,0,3,100,0,13,110,111,110,111,100,101,64,
			 110,111,104,111,115,116,0,0,0,0,122,0,0,0,0,0,0,0,0>>,
    ExternalReference = <<131,114,0,3,100,0,13,110,111,110,111,100,101,64,
			 110,111,104,111,115,116,0,0,0,0,122,0,0,0,0,0,0,0,0>>,
    1113403635 = phash_from_external(ExternalReference),

    ExternalFun = <<131,112,0,0,0,70,1,212,190,220,28,179,144,194,131,
                    19,215,105,97,77,251,125,93,0,0,0,0,0,0,0,2,100,0,1,
                    116,97,0,98,6,165,246,224,103,100,0,13,110,111,
                    110,111,100,101,64,110,111,104,111,115,116,0,0,0,91,
                    0,0,0,0,0,97,2,97,1>>,
    25769064 = phash_from_external(ExternalFun),

    case (catch erlang:phash(1,0)) of
	{'EXIT',{badarg, _}} ->
	    ok;
	_ ->
	    exit(phash_accepted_zero_as_range)
    end.

phash_from_external(Ext) ->
    erlang:phash(binary_to_term(Ext), 16#FFFFFFFF).

range_test() ->
    F = fun(From,From,_FF) ->
		ok;
	   (From,To,FF) ->
		R = rand:uniform(16#FFFFFFFFFFFFFFFF),
		X = erlang:phash(R, From),
		Y = erlang:phash(R, 16#100000000) - 1,
		Z = (Y rem From) + 1,
		case X =:= Z of
		    true ->
			FF(From*2,To,FF);
		    _ ->
			exit({range_test_failed, hash_on, R, range, From})
		end
	end,
    F(1,16#100000000,F).


spread_test(N) ->
    test_fun(N,{erlang,phash},16#50000000000,fun(X) ->
						     X
					     end),
    test_fun(N,{erlang,phash},0,fun(X) ->
					X
				end),
    test_fun(N,{erlang,phash},16#123456789ABCDEF123456789ABCDEF,fun(X) ->
									X
								end),
    test_fun(N,{erlang,phash},16#50000000000,fun(X) ->
						     integer_to_list(X)
					     end),
    test_fun(N,{erlang,phash},16#50000000000,fun(X) ->
						     integer_to_bytelist(X,[])
					     end),
    test_fun(N,{erlang,phash},16#50000000000,fun(X) ->
						     integer_to_binary_value(X)
					     end).
    


cmp_test(N) ->
    do_cmp_hashes(N,8).

do_cmp_hashes(0,_) ->
    ok;
do_cmp_hashes(N,Steps) ->
    R0 = rand:uniform(1 bsl Steps - 1) + rand:uniform(16#FFFFFFFF),
    R = case rand:uniform(2) of
	    1 ->
		R0;
	    _ ->
		-R0
	end,
    NSteps = case N rem 10 of
		 0 ->
		     case (Steps + 8) rem 1024 of
			 0 ->
			     8;
			 OK ->
			     OK
		     end;
		 _ ->
		     Steps
	     end,
    X = erlang:phash(R,16#FFFFFFFF),
    Y = make_hash(R,16#FFFFFFFF),
    case X =:= Y of
	true ->
	    do_cmp_hashes(N - 1, NSteps);
	_ ->
	    exit({missmatch_on_input, R, phash, X, make_hash, Y})
    end.

phash2_test() ->
    Max = 1 bsl 32,
    BPort = <<131,102,100,0,13,110,111,110,111,100,101,64,110,111,104,
              111,115,116,0,0,0,1,0>>,
    Port = binary_to_term(BPort),

    BXPort = <<131,102,100,0,11,97,112,97,64,108,101,103,111,108,97,115,
               0,0,0,24,3>>,
    XPort = binary_to_term(BXPort),

    BRef = <<131,114,0,3,100,0,13,110,111,110,111,100,101,64,110,111,104,
             111,115,116,0,0,0,1,255,0,0,0,0,0,0,0,0>>,
    Ref = binary_to_term(BRef),

    BXRef = <<131,114,0,3,100,0,11,97,112,97,64,108,101,103,111,108,97,115,
              2,0,0,0,155,0,0,0,0,0,0,0,0>>,
    XRef = binary_to_term(BXRef),

    BXPid = <<131,103,100,0,11,97,112,97,64,108,101,103,111,108,97,115,
              0,0,0,36,0,0,0,0,1>>,
    XPid = binary_to_term(BXPid),


    %% X = f1(), Y = f2(), Z = f3(X, Y),

    %% F1 = fun f1/0, % -> abc
    B1 = <<131,112,0,0,0,66,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,1,0,0,0,0,100,0,1,116,97,1,98,2,195,126,
           58,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F1 = binary_to_term(B1),

    %% F2 = fun f2/0, % -> abd
    B2 = <<131,112,0,0,0,66,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,2,0,0,0,0,100,0,1,116,97,2,98,3,130,152,
           185,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F2 = binary_to_term(B2),

    %% F3 = fun f3/2, % -> {abc, abd}
    B3 = <<131,112,0,0,0,66,2,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,3,0,0,0,0,100,0,1,116,97,3,98,7,168,160,
           93,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F3 = binary_to_term(B3),

    %% F4 = fun () -> 123456789012345678901234567 end,
    B4 = <<131,112,0,0,0,66,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,4,0,0,0,0,100,0,1,116,97,4,98,2,230,21,
           171,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F4 = binary_to_term(B4),

    %% F5 = fun() -> {X,Y,Z} end,
    B5 = <<131,112,0,0,0,92,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,5,0,0,0,3,100,0,1,116,97,5,98,0,99,101,
           130,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0,100,0,3,97,98,99,100,0,3,97,98,
           100,104,2,100,0,3,97,98,99,100,0,3,97,98,100>>,
    F5 = binary_to_term(B5),

    Chars = lists:seq(32,127),
    NotAHeapBin = list_to_binary(lists:flatten(lists:duplicate(500,Chars))),
    <<_:128,SubBin/binary>> = NotAHeapBin,
    L = [%% nil
	 {[], 3468870702},

	 %% atom :( not very good ):
         %% (cannot use block_hash due to compatibility issues...)
	 {abc,26499},
	 {abd,26500},
	 {'åäö', 62518}, 
	 %% 81 runes as an atom, 'ᚠᚡᚢᚣᚤᚥᚦᚧᚨᚩᚪᚫᚬᚭᚮᚯᚰᚱᚲᚳᚴᚵᚶᚷᚸᚹᚺᚻᚼᚽᚾᚿᛀᛁᛂᛃᛄᛅᛆᛇᛈᛉᛊᛋᛌᛍᛎᛏᛐᛑᛒᛓᛔᛕᛖᛗᛘᛙᛚᛛᛜᛝᛞᛟᛠᛡᛢᛣᛤᛥᛦᛧᛨᛩᛪ᛫᛬᛭ᛮᛯᛰ'
	 {erlang:binary_to_term(<<131, 118, 0, 243, (unicode:characters_to_binary(lists:seq(5792, 5872)))/binary >>), 241561024},
	 %% åäö dynamic
	 {erlang:binary_to_term(<<131, 118, 0, 6, 195, 165, 195, 164, 195, 182>>),62518},
	 %% the atom '゙゚゛゜ゝゞゟ゠ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズ'
	 {erlang:binary_to_term(<<131, 118, 0, 102, (unicode:characters_to_binary(lists:seq(12441, 12542)))/binary>>), 246053818},
	 %% the atom, '😃'
	 {erlang:binary_to_term(<<131, 118, 0, 4, 240, 159, 152, 131>>), 1026307},

	 %% small
	 {0,3175731469},
	 {1, 539485162},
	 {-1, 1117813597},
	 {1 bsl 20, 1477815345},
	 {-(1 bsl 20), 3076904293},

	 %% bignum
	 {4294967296, 2108323275},
	 {-4294967296, 2586067094},
	 {981494972710656, 1622082818},
	 {-981494972710656, 3367191372},
	 {36893488147419103232, 2545846594},
	 {-36893488147419103232, 1649047068},
	 {1606938044258990275541962092341162602522202993782792835301376,
	  2573322433},
	 {-1606938044258990275541962092341162602522202993782792835301376,
	  2288753377},

	 %% binary
	 {<<>>, 147926629},
	 {<<0:8>>, 2914887855},
	 {<<0:32>>, 2014511533},
	 {<<"abc">>, 1306188027},
	 {<<"12345678901234567890">>, 3021061640},
	 {NotAHeapBin,2644086993},
	 {SubBin,3575839236},

	 %% unaligned sub binaries
	 {unaligned_sub_bin(<<>>), 147926629},
	 {unaligned_sub_bin(<<0:8>>), 2914887855},
	 {unaligned_sub_bin(<<0:32>>), 2014511533},
	 {unaligned_sub_bin(<<"abc">>), 1306188027},
	 {unaligned_sub_bin(<<"12345678901234567890">>), 3021061640},
	 {unaligned_sub_bin(NotAHeapBin),2644086993},
	 {unaligned_sub_bin(SubBin),3575839236},

	 %% bit-level binaries
	 {<<0:7>>, 1055790816},
	 {(fun()-> B = <<255,7:3>>, <<_:4,D/bitstring>> = B, D end)(), 911751529},
	 {<<"abc",13:4>>, 670412287},
	 {<<5:3,"12345678901234567890">>, 289973273},

	 %% fun
	 {F1, 3826013332},
	 {F2, 126009152},
	 {F3, 3482452479},
	 {F4, 633704783},
	 {F5, 1241537408},

	 %% module fun
	 {fun lists:map/2, 840287883},
 	 {fun lists:map/3, 2318478565},
 	 {fun lists:filter/2, 635165125},
 	 {fun lists:filter/3, 3824649396},
 	 {fun xxx:map/2, 2630071865},
 	 {fun xxx:map/3, 4237970519},

	 %% pid
	 {c:pid(0,0,0), 2858162279},
	 {c:pid(0,1,0), 2870503209},
	 {c:pid(0,2,0), 1707788908},
	 {XPid, 1290188489},

	 %% port
	 {Port,1954394636},
	 {XPort,274735},

	 %% ref
	 {Ref, 1675501484},
	 {XRef, 3845846926},

	 %% float
	 {0.0, 423528920},
	 {3.14, 3731709215},
	 {-3.14, 1827518724},

	 %% list
	 {[0.0], 167906877},
	 {[{}], 4050867804},
	 {[<<>>], 440873397},
	 {[[]], 499070068},
	 {[abc], 3112446404},
         {[a,b,c], 1505666924},
	 {[a,b|c], 433753489},
	 {"abc", 519996486},
	 {"abc"++[1009], 290369864},
	 {"abc"++[1009]++"de", 4134369195},
	 {"1234567890123456", 963649519},

	 %% tuple
	 {{}, 221703996},
	 {{{}}, 2165044361},
	 {{<<>>}, 682464809},
	 {{0.0}, 688441152},
	 {{[]}, 1775079505},
	 {{abc}, 2032039329},
	 {{a,1,{},-3.14}, 1364396939},
	 {{c:pid(0,2,0)}, 686997880},
	 {{F4}, 2279632930},
	 {{a,<<>>}, 2724468891},
	 {{b,<<>>}, 2702508511}
	],
    SpecFun = fun(S) -> sofs:no_elements(S) > 1 end,
    F = sofs:relation_to_family(sofs:converse(sofs:relation(L))),
    D = sofs:to_external(sofs:family_specification(SpecFun, F)),
    [] = D,
    [] = [{E,H,H2} || {E,H} <- L, (H2 = erlang:phash2(E, Max)) =/= H],
    ok.

test_phash2_binary_aligned_and_unaligned_equal(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    test_aligned_and_unaligned_equal_up_to(256*12+255),
    erts_debug:set_internal_state(available_internal_state, false).

test_aligned_and_unaligned_equal_up_to(BinSize) ->
    Results =
        lists:map(fun(Size) ->
                          test_aligned_and_unaligned_equal(Size)
                  end, lists:seq(1, BinSize)),
    %% DataDir = filename:join(filename:dirname(code:which(?MODULE)), "hash_SUITE_data"),
    %% ExpResFile = filename:join(DataDir, "phash2_bin_expected_results.txt"),
    %% {ok, [ExpRes]} = file:consult(ExpResFile),
    %% %% ok = file:write_file(ExpResFile, io_lib:format("~w.~n", [Results])),
    %% Results = ExpRes,
    110469206 = erlang:phash2(Results).

test_aligned_and_unaligned_equal(BinSize) ->
    Bin = make_random_bin(BinSize),
    LastByte = last_byte(Bin),
    LastInBitstring = LastByte rem 11,
    Bitstring = << Bin/binary, <<LastInBitstring:5>>/bitstring >>,
    UnalignedBin = make_unaligned_sub_bitstring(Bin),
    UnalignedBitstring = make_unaligned_sub_bitstring(Bitstring),
    case erts_debug:get_internal_state(available_internal_state) of
        false -> erts_debug:set_internal_state(available_internal_state, true);
        _ -> ok
    end,
    erts_debug:set_internal_state(reds_left, 3),
    BinHash = erlang:phash2(Bin),
    BinHash = erlang:phash2(Bin),
    erts_debug:set_internal_state(reds_left, 3),
    UnalignedBinHash = erlang:phash2(UnalignedBin),
    UnalignedBinHash = erlang:phash2(UnalignedBin),
    BinHash = UnalignedBinHash,
    erts_debug:set_internal_state(reds_left, 3),
    BitstringHash = erlang:phash2(Bitstring),
    BitstringHash = erlang:phash2(Bitstring),
    erts_debug:set_internal_state(reds_left, 3),
    UnalignedBitstringHash = erlang:phash2(UnalignedBitstring),
    UnalignedBitstringHash = erlang:phash2(UnalignedBitstring),
    BitstringHash = UnalignedBitstringHash,
    {BinHash, BitstringHash}.

last_byte(Bin) ->
    NotLastByteSize = (erlang:bit_size(Bin)) - 8,
    <<_:NotLastByteSize/bitstring, LastByte:8>> = Bin,
    LastByte.

test_phash2_4GB_plus_bin(Config) when is_list(Config) ->
    run_when_enough_resources(
      fun() ->
              erts_debug:set_internal_state(available_internal_state, true),
              %% Created Bin4GB here so it only needs to be created once
              erts_debug:set_internal_state(force_gc, self()),
              Bin4GB = get_4GB_bin(),
              test_phash2_plus_bin_helper1(Bin4GB, <<>>, <<>>, 13708901),
              erts_debug:set_internal_state(force_gc, self()),
              test_phash2_plus_bin_helper1(Bin4GB, <<>>, <<3:5>>, 66617678),
              erts_debug:set_internal_state(force_gc, self()),
              test_phash2_plus_bin_helper1(Bin4GB, <<13>>, <<>>, 31308392),
              erts_debug:set_internal_state(force_gc, self()),
              erts_debug:set_internal_state(available_internal_state, false)
      end).


test_phash2_10MB_plus_bin(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(force_gc, self()),
    Bin10MB = get_10MB_bin(),
    test_phash2_plus_bin_helper1(Bin10MB, <<>>, <<>>, 22776267),
    erts_debug:set_internal_state(force_gc, self()),
    test_phash2_plus_bin_helper1(Bin10MB, <<>>, <<3:5>>, 124488972),
    erts_debug:set_internal_state(force_gc, self()),
    test_phash2_plus_bin_helper1(Bin10MB, <<13>>, <<>>, 72958346),
    erts_debug:set_internal_state(force_gc, self()),
    erts_debug:set_internal_state(available_internal_state, false).

get_10MB_bin() ->
    TmpBin = make_random_bin(10239),
    Bin = erlang:iolist_to_binary([0, TmpBin]),
    IOList10MB = duplicate_iolist(Bin, 10),
    Bin10MB = erlang:iolist_to_binary(IOList10MB),
    10485760 = size(Bin10MB),
    Bin10MB.

get_4GB_bin() ->
    TmpBin = make_random_bin(65535),
    Bin = erlang:iolist_to_binary([0, TmpBin]),
    IOList4GB = duplicate_iolist(Bin, 16),
    Bin4GB = erlang:iolist_to_binary(IOList4GB),
    4294967296 = size(Bin4GB),
    Bin4GB.

duplicate_iolist(IOList, 0) ->
    IOList;
duplicate_iolist(IOList, NrOfTimes) ->
    duplicate_iolist([IOList, IOList], NrOfTimes - 1).

test_phash2_plus_bin_helper1(Bin4GB, ExtraBytes, ExtraBits, ExpectedHash) ->
    test_phash2_plus_bin_helper2(Bin4GB, fun id/1, ExtraBytes, ExtraBits, ExpectedHash),
    test_phash2_plus_bin_helper2(Bin4GB, fun make_unaligned_sub_bitstring/1, ExtraBytes, ExtraBits, ExpectedHash).

test_phash2_plus_bin_helper2(Bin, TransformerFun, ExtraBytes, ExtraBits, ExpectedHash) ->
    ExtraBitstring = << ExtraBytes/binary, ExtraBits/bitstring >>,
    LargerBitstring = << ExtraBytes/binary,
                         ExtraBits/bitstring,
                         Bin/bitstring >>,
    LargerTransformedBitstring = TransformerFun(LargerBitstring),
    ExtraBitstringHash = erlang:phash2(ExtraBitstring),
    ExpectedHash =
        case size(LargerTransformedBitstring) < 4294967296 of
            true ->
                erts_debug:set_internal_state(force_gc, self()),
                erts_debug:set_internal_state(reds_left, 1),
                Hash = erlang:phash2(LargerTransformedBitstring),
                Hash = erlang:phash2(LargerTransformedBitstring),
                Hash;
            false ->
                erts_debug:set_internal_state(force_gc, self()),
                erts_debug:set_internal_state(reds_left, 1),
                ExtraBitstringHash = erlang:phash2(LargerTransformedBitstring),
                ExtraBitstringHash = erlang:phash2(LargerTransformedBitstring),
                ExtraBitstringHash
        end.

run_when_enough_resources(Fun) ->
    case {total_memory(), erlang:system_info(wordsize)} of
        {Mem, 8} when is_integer(Mem) andalso Mem >= 31 ->
            Fun();
        {Mem, WordSize} ->
            {skipped,
             io_lib:format("Not enough resources (System Memory >= ~p, Word Size = ~p)",
                           [Mem, WordSize])}
    end.

%% Total memory in GB
total_memory() ->
    try
        MemoryData = memsup:get_system_memory_data(),
        case lists:keysearch(total_memory, 1, MemoryData) of
            {value, {total_memory, TM}} ->
        	TM div (1024*1024*1024);
            false ->
        	{value, {system_total_memory, STM}} =
        	    lists:keysearch(system_total_memory, 1, MemoryData),
        	STM div (1024*1024*1024)
        end
    catch
        _ : _ ->
            undefined
    end.

-ifdef(FALSE).
f1() ->
    abc.

f2() ->
    abd.

f3(X, Y) ->
    {X, Y}.
-endif.

otp_5292_test() ->
    PH = fun(E) ->
                 EInList = [1, 2, 3, E],
                 EInList2 = [E, 1, 2, 3],
                 NegEInList = [1, 2, 3, -E],
                 NegEInList2 = [-E, 1, 2, 3],
                 [erlang:phash(E, 1 bsl 32),
                  erlang:phash(-E, 1 bsl 32),
                  erlang:phash2(E, 1 bsl 32),
                  erlang:phash2(-E, 1 bsl 32),
                  erlang:phash2(EInList, 1 bsl 32),
                  erlang:phash2(EInList2, 1 bsl 32),
                  erlang:phash2(NegEInList, 1 bsl 32),
                  erlang:phash2(NegEInList2, 1 bsl 32)]
         end,
    S2 = md5([md5(hash_int(S, E, PH)) || {Start, N, Sz} <- d(),
                                         {S, E} <- int(Start, N, Sz)]),
    <<234,63,192,76,253,57,250,32,44,11,73,1,161,102,14,238>> = S2,
    ok.

d() ->
    [%% Start,          NumOfIntervals, SizeOfInterval
        {(1 bsl I)-100, 2,              100} || I <- lists:seq(1, 1000)].

int(Start, N, Sz) ->
    {_, R} = lists:mapfoldl(fun(S, Acc) ->
                                    {S + Sz, [{S,S+Sz-1} | Acc]}
                            end, [], lists:seq(Start, Start+(N-1)*Sz, Sz)),
    lists:reverse(R).

hash_int(Start, End, F) ->
    HL = lists:flatmap(fun(E) -> F(E) end, lists:seq(Start, End)),
    {Start, End, md5(HL)}.

md5(T) ->
    erlang:md5(term_to_binary(T)).   

bit_level_binaries_do() ->
    [3511317,7022633,14044578,28087749,56173436,112344123,90467083|_] =
	bit_level_all_different(fun erlang:phash/2),
    [102233154,19716,102133857,4532024,123369135,24565730,109558721|_] =
	bit_level_all_different(fun erlang:phash2/2),

    13233341 = test_hash_phash(<<42:7>>, 16#7FFFFFF),
    79121243 = test_hash_phash(<<99:7>>, 16#7FFFFFF),
    95517726 = test_hash_phash(<<16#378ABF73:31>>, 16#7FFFFFF),

    64409098 = test_phash2(<<99:7>>, 16#7FFFFFF),
    55555814 = test_phash2(<<123,19:2>>, 16#7FFFFFF),
    83868582 = test_phash2(<<123,45,6:3>>, 16#7FFFFFF),
    2123204 = test_phash2(<<123,45,7:3>>, 16#7FFFFFF),

    ok.

bit_level_all_different(Hash) ->
    {name,Name} = erlang:fun_info(Hash, name),
    Seq = lists:seq(1, 32),
    Hashes0 = [Hash(<<1:Sz>>, 16#7FFFFFF) || Sz <- Seq],
    io:format("~p/2 ~p", [Name,Hashes0]),
    Hashes0 = [Hash(unaligned_sub_bitstr(<<1:Sz>>), 16#7FFFFFF) || Sz <- Seq],
    32 = length(lists:usort(Hashes0)),

    Hashes1 = [Hash(<<(1 bsl (Sz-1)):Sz>>, 16#7FFFFFF) || Sz <- Seq],
    io:format("~p/2 ~p", [Name,Hashes1]),
    Hashes1 = [Hash(unaligned_sub_bitstr(<<(1 bsl (Sz-1)):Sz>>), 16#7FFFFFF) ||
		  Sz <- Seq],
    32 = length(lists:usort(Hashes1)),

    Hashes2 = [Hash(<<0:Sz>>, 16#7FFFFFF) || Sz <- Seq],
    io:format("~p/2 ~p", [Name,Hashes2]),
    Hashes2 = [Hash(unaligned_sub_bitstr(<<0:Sz>>), 16#7FFFFFF) || Sz <- Seq],
    32 = length(lists:usort(Hashes2)),

    Hashes1.

test_hash_phash(Bitstr, Rem) ->
    Hash = erlang:phash(Bitstr, Rem),
    Hash = erlang:phash(unaligned_sub_bitstr(Bitstr), Rem).

test_phash2(Bitstr, Rem) ->
    Hash = erlang:phash2(Bitstr, Rem),
    Hash = erlang:phash2(unaligned_sub_bitstr(Bitstr), Rem).

otp_7127_test() ->
    %% Used to return 2589127136.
    38990304 = erlang:phash2(<<"Scott9">>),
    ok.

hash_zero_test() ->
    Zs = [0.0, -0.0, 0/-1, 0.0/-1, 0/-(1 bsl 65),
          binary_to_term(<<131,70,0,0,0,0,0,0,0,0>>),    %% +0.0
          binary_to_term(<<131,70,128,0,0,0,0,0,0,0>>)], %% -0.0
    ok = hash_zero_test(Zs,fun(T) -> erlang:phash2(T, 1 bsl 32) end),
    ok = hash_zero_test(Zs,fun(T) -> erlang:phash(T, 1 bsl 32) end),
    ok.

hash_zero_test([Z|Zs],F) ->
    hash_zero_test(Zs,Z,F(Z),F).
hash_zero_test([Z|Zs],Z0,V,F) ->
    true = Z0 =:= Z, %% assert exact equal
    Z0   = Z,        %% assert matching
    V    = F(Z),     %% assert hash
    hash_zero_test(Zs,Z0,V,F);
hash_zero_test([],_,_,_) ->
    ok.


%%
%% Reference implementation of integer hashing
%%

%%
%% These are primes just above 2^28 that will never be changed, they are also in
%% utils.c.
%%
-define(FN2,268439161).
-define(FN3,268435459).
-define(FN4,268436141).

make_hash(N,M) ->
    Prime1 = ?FN2,
    {Prime2, BL0} = to_bytes(N),
    BL = pad(BL0),
    (integer_hash(BL, Prime1, Prime2) rem M) + 1.

to_bytes(N) when N < 0 ->
    {?FN4,to_bytes(-N,[])};
to_bytes(N) ->
    {?FN3,to_bytes(N,[])}.
to_bytes(0,Acc) ->
    Acc;
to_bytes(N,Acc) ->
    to_bytes(N bsr 8, [N band 16#FF | Acc]).

pad([]) ->
    [0,0,0,0];
pad(L) ->
    case 4 - (length(L) rem 4) of
	4 ->
	    L;
	N ->
	    lists:duplicate(N,0) ++ L
    end.

integer_hash(BL,P1,P2) ->
    (do_ihash(0,lists:reverse(BL),P1) * P2) band 16#FFFFFFFF.

do_ihash(Hash,[],_) ->
    Hash;
do_ihash(Hash, [H|T], P) ->
    do_ihash((((Hash * P) band 16#FFFFFFFF) + H) band 16#FFFFFFFF, T, P).
    
    


%%
%% Utilities for the test of "spreading"
%%
-ifdef(debug).
hex(N) ->
    hex(0,N,[]).
hex(X,0,Acc) when X >= 8 ->
    [$0, $x | Acc];
hex(X,N,Acc) ->
    hex(X+1,N bsr 4, [trans(N band 16#F) | Acc]).

trans(N) when N < 10 ->
    N + $0;
trans(10) ->
    $A;
trans(11) ->
    $B;
trans(12) ->
    $C;
trans(13) ->
    $D;
trans(14) ->
    $E;
trans(15) ->
    $F.
-endif.

gen_keys(N, Template, BP,Fun) ->
    Ratio = (1 bsl (BP * 8)),
    Low = Template + Ratio,
    High = Template + (N*Ratio),
    ?dbgformat("N = ~p, BP = ~p, Template = ~p, Low = ~s, High = ~s~n",
	      [hex(N),hex(BP),hex(Template),hex(Low),hex(High-1)]),
    Fun(Template),
    gen_keys2(Low, High,Ratio,Fun).

gen_keys2(High,High2,_,_) when High >= High2 -> 
    [];
gen_keys2(Low,High,R,Fun) ->
    Fun(Low),
    gen_keys2(Low + R,High,R,Fun).

test_fun(N,{HM,HF}, Template, Fun) ->
    init_table(),
    test_fun_1(0,1,N+1,{HM,HF},Template,Fun).

test_fun_1(_,To,To,_,_,_) ->
    ok;
test_fun_1(A,X,To,Y,Z,W) when A > To ->
    ?dbgformat("~p:~p(~p,~p,~p,~p,~p,~p)~n",[?MODULE,test_fun_1,To,X,To,Y,Z,W]),
    test_fun_1(0,X+1,To,Y,Z,W);
test_fun_1(Pos,Siz,To,{HM,HF},Template,Fun) when 1 bsl (Siz*8) =< 65536 ->
    io:format("Byte: ~p, Size: ~p~n",[Pos,Siz]),
    N = 1 bsl (Siz*8),
    gen_keys(N,Template,Pos,fun (X) ->     
				    P = HM:HF(Fun(X),N),
				    ets:insert(?MODULE,{P})
			    end
	    ),
    Hits = collect_hits(),
    io:format(
      "Hashing of ~p values spread over ~p buckets~n",
      [N,Hits]),
    case (N div Hits) > 2 of
	true ->
	    exit({not_spread_enough, Hits, on, N});
	_ ->
	    test_fun_1(Pos + Siz, Siz, To,{HM,HF},Template,Fun)
    end;
test_fun_1(_,_,_,_,_,_) ->
    ok.

init_table() ->
    (catch ets:delete(?MODULE)),
    ets:new(?MODULE,[ordered_set,named_table]).
   
collect_hits() ->
    N = ets:info(?MODULE,size),
    init_table(),
    N.

integer_to_binary_value(N) ->
    list_to_binary(lists:reverse(integer_to_bytelist(N,[]))).

integer_to_bytelist(0,Acc) ->
    Acc;
integer_to_bytelist(N,Acc) ->
    integer_to_bytelist(N bsr 8, [N band 16#FF | Acc]).

unaligned_sub_bin(Bin0) when is_binary(Bin0) ->
    Bin1 = <<42:6,Bin0/binary,3:2>>,
    Sz = size(Bin0),
    <<42:6,Bin:Sz/binary,3:2>> = id(Bin1),
    Bin.

unaligned_sub_bitstr(Bin0) when is_bitstring(Bin0) ->
    Bin1 = <<(-1):4,Bin0/bits,(-1):64>>,
    Bits = bit_size(Bin0),
    <<_:4,Bin:Bits/bits,_:64>> = id(Bin1),
    Bin.

id(I) -> I.
    

%% Benchmarks for phash2

run_phash2_benchmarks() ->
    Benchmarks = [
                  test_phash2_large_map,
                  test_phash2_shallow_long_list,
                  test_phash2_deep_list,
                  test_phash2_deep_tuple,
                  test_phash2_deep_tiny,
                  test_phash2_with_42,
                  test_phash2_with_short_tuple,
                  test_phash2_with_short_list,
                  test_phash2_with_tiny_bin,
                  test_phash2_with_tiny_unaligned_sub_binary,
                  test_phash2_with_small_unaligned_sub_binary,
                  test_phash2_with_large_bin,
                  test_phash2_with_large_unaligned_sub_binary,
                  test_phash2_with_super_large_unaligned_sub_binary
                 ],
    [print_comment(B) || B <- Benchmarks].


print_comment(FunctionName) ->
    io:format("~p~n", [FunctionName]),
    io:format("~s~n", [element(2, erlang:apply(?MODULE, FunctionName, [[]]))]).

nr_of_iters(BenchmarkNumberOfIterations, Config) ->
    case lists:member(phash2_benchmark_tests, Config) of
        true -> 1;
        false -> BenchmarkNumberOfIterations
    end.


test_phash2_large_map(Config) when is_list(Config) ->
    {Size, ExpectedHash} =
        case {total_memory(), erlang:system_info(wordsize)} of
            {Mem, 8} when is_integer(Mem) andalso Mem > 2 ->
                {1000000, 121857429};
            _ ->
                {1000, 66609305}
        end,
    run_phash2_test_and_benchmark(nr_of_iters(45, Config),
                                  get_map(Size),
                                  ExpectedHash).

test_phash2_shallow_long_list(Config) when is_list(Config) ->
    {Size, ExpectedHash} =
        case {total_memory(), erlang:system_info(wordsize)} of
            {Mem, 8} when is_integer(Mem) andalso Mem > 2 ->
                {1000000, 78700388};
            _ ->
                {1000, 54749638}
        end,
    run_phash2_test_and_benchmark(nr_of_iters(1, Config),
                                  lists:duplicate(Size, get_complex_tuple()),
                                  ExpectedHash).

test_phash2_deep_list(Config) when is_list(Config) ->
    {Size, ExpectedHash} =
        case {total_memory(), erlang:system_info(wordsize)} of
            {Mem, 8} when is_integer(Mem) andalso Mem > 2 ->
                {500000, 17986444};
            _ ->
                {1000, 81794308}
        end,
    run_phash2_test_and_benchmark(nr_of_iters(1, Config),
                                  make_deep_list(Size, get_complex_tuple()),
                                  ExpectedHash).

test_phash2_deep_tuple(Config) when is_list(Config) ->
    {Size, ExpectedHash} =
        case {total_memory(), erlang:system_info(wordsize)} of
            {Mem, 8} when is_integer(Mem) andalso Mem > 2 ->
                {500000, 116594715};
            _ ->
                {500, 109057352}
        end,
    run_phash2_test_and_benchmark(nr_of_iters(1, Config),
                                  make_deep_tuple(Size, get_complex_tuple()),
                                  ExpectedHash).

test_phash2_deep_tiny(Config) when is_list(Config) ->
    run_phash2_test_and_benchmark(nr_of_iters(1000000, Config),
                                  make_deep_list(19, 42),
                                  111589624).

test_phash2_with_42(Config) when is_list(Config) ->
    run_phash2_test_and_benchmark(nr_of_iters(20000000, Config),
                                  42,
                                  30328728).

test_phash2_with_short_tuple(Config) when is_list(Config) ->
    run_phash2_test_and_benchmark(nr_of_iters(10000000, Config),
                                  {a,b,<<"hej">>, "hej"},
                                  50727199).

test_phash2_with_short_list(Config) when is_list(Config) ->
    run_phash2_test_and_benchmark(nr_of_iters(10000000, Config),
                                  [a,b,"hej", "hello"],
                                  117108642).

test_phash2_with_tiny_bin(Config) when is_list(Config) ->
    run_phash2_test_and_benchmark(nr_of_iters(20000000, Config),
                                  make_random_bin(10),
                                  129616602).

test_phash2_with_tiny_unaligned_sub_binary(Config) when is_list(Config) ->
    run_phash2_test_and_benchmark(nr_of_iters(10000000, Config),
                                  make_unaligned_sub_binary(make_random_bin(11)),
                                  59364725).

test_phash2_with_small_unaligned_sub_binary(Config) when is_list(Config) ->
    run_phash2_test_and_benchmark(nr_of_iters(400000, Config),
                                  make_unaligned_sub_binary(make_random_bin(1001)),
                                  130388119).

test_phash2_with_large_bin(Config) when is_list(Config) ->
    {Size, ExpectedHash} =
        case {total_memory(), erlang:system_info(wordsize)} of
            {Mem, 8} when is_integer(Mem) andalso Mem > 2 ->
                {10000000, 48249379};
            _ ->
                {1042, 14679520}
        end,
    run_phash2_test_and_benchmark(nr_of_iters(150, Config),
                                  make_random_bin(Size),
                                  ExpectedHash).

test_phash2_with_large_unaligned_sub_binary(Config) when is_list(Config) ->
    {Size, ExpectedHash} =
        case {total_memory(), erlang:system_info(wordsize)} of
            {Mem, 8} when is_integer(Mem) andalso Mem > 2 ->
                {10000001, 122836437};
            _ ->
                {10042, 127144287}
        end,
    run_phash2_test_and_benchmark(nr_of_iters(50, Config),
                                  make_unaligned_sub_binary(make_random_bin(Size)),
                                  ExpectedHash).

test_phash2_with_super_large_unaligned_sub_binary(Config) when is_list(Config) ->
    {Size, ExpectedHash} =
        case {total_memory(), erlang:system_info(wordsize)} of
            {Mem, 8} when is_integer(Mem) andalso Mem > 2 ->
                {20000001, 112086727};
            _ ->
                {20042, 91996619}
        end,
    run_phash2_test_and_benchmark(nr_of_iters(20, Config),
                                  make_unaligned_sub_binary(make_random_bin(Size)),
                                  ExpectedHash).

make_deep_list(1, Item) ->
    {Item, Item};
make_deep_list(Depth, Item) ->
    [{Item, Item}, make_deep_list(Depth - 1, Item)].

make_deep_tuple(1, Item) ->
    [Item, Item];
make_deep_tuple(Depth, Item) ->
    {[Item, Item], make_deep_tuple(Depth - 1, Item)}.

% Helper functions for benchmarking

loop(0, _) -> ok;
loop(Iterations, Fun) ->
    Fun(),
    loop(Iterations - 1, Fun).

run_phash2_test_and_benchmark(Iterations, Term, ExpectedHash) ->
    Parent = self(),
    Test =
        fun() ->
                Hash = erlang:phash2(Term),
                case ExpectedHash =:= Hash of
                    false ->
                        Parent ! {got_bad_hash, Hash},
                        ExpectedHash = Hash;
                    _ -> ok
                end
        end,
    Benchmark =
        fun() ->
                garbage_collect(),
                {Time, _} =timer:tc(fun() -> loop(Iterations, Test) end),
                Parent ! Time
        end,
    spawn(Benchmark),
    receive
        {got_bad_hash, Hash} ->
            ExpectedHash = Hash;
        Time ->
            TimeInS = case (Time/1000000) of
                          0.0 -> 0.0000000001;
                          T -> T
                      end,
            IterationsPerSecond = Iterations / TimeInS,
            notify(#event{ name = benchmark_data, data = [{value, IterationsPerSecond}]}),
            {comment, io_lib:format("Iterations per second: ~p, Iterations ~p, Benchmark time: ~p seconds)",
                                    [IterationsPerSecond, Iterations, Time/1000000])}
    end.

get_complex_tuple() ->
    BPort = <<131,102,100,0,13,110,111,110,111,100,101,64,110,111,104,
              111,115,116,0,0,0,1,0>>,
    Port = binary_to_term(BPort),

    BXPort = <<131,102,100,0,11,97,112,97,64,108,101,103,111,108,97,115,
               0,0,0,24,3>>,
    XPort = binary_to_term(BXPort),

    BRef = <<131,114,0,3,100,0,13,110,111,110,111,100,101,64,110,111,104,
             111,115,116,0,0,0,1,255,0,0,0,0,0,0,0,0>>,
    Ref = binary_to_term(BRef),

    BXRef = <<131,114,0,3,100,0,11,97,112,97,64,108,101,103,111,108,97,115,
              2,0,0,0,155,0,0,0,0,0,0,0,0>>,
    XRef = binary_to_term(BXRef),

    BXPid = <<131,103,100,0,11,97,112,97,64,108,101,103,111,108,97,115,
              0,0,0,36,0,0,0,0,1>>,
    XPid = binary_to_term(BXPid),


    %% X = f1(), Y = f2(), Z = f3(X, Y),

    %% F1 = fun f1/0, % -> abc
    B1 = <<131,112,0,0,0,66,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,1,0,0,0,0,100,0,1,116,97,1,98,2,195,126,
           58,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F1 = binary_to_term(B1),

    %% F2 = fun f2/0, % -> abd
    B2 = <<131,112,0,0,0,66,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,2,0,0,0,0,100,0,1,116,97,2,98,3,130,152,
           185,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F2 = binary_to_term(B2),

    %% F3 = fun f3/2, % -> {abc, abd}
    B3 = <<131,112,0,0,0,66,2,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,3,0,0,0,0,100,0,1,116,97,3,98,7,168,160,
           93,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F3 = binary_to_term(B3),

    %% F4 = fun () -> 123456789012345678901234567 end,
    B4 = <<131,112,0,0,0,66,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,4,0,0,0,0,100,0,1,116,97,4,98,2,230,21,
           171,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0>>,
    F4 = binary_to_term(B4),

    %% F5 = fun() -> {X,Y,Z} end,
    B5 = <<131,112,0,0,0,92,0,215,206,77,69,249,50,170,17,129,47,21,98,
           13,196,76,242,0,0,0,5,0,0,0,3,100,0,1,116,97,5,98,0,99,101,
           130,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,
           115,116,0,0,0,112,0,0,0,0,0,100,0,3,97,98,99,100,0,3,97,98,
           100,104,2,100,0,3,97,98,99,100,0,3,97,98,100>>,
    F5 = binary_to_term(B5),
    {{1,{2}},an_atom, 1, 3434.923942394,<<"this is a binary">>,
     make_unaligned_sub_binary(<<"this is also a binary">>),c,d,e,f,g,h,i,j,k,l,[f],
     999999999999999999666666662123123123123324234999999999999999, 234234234,
     BPort, Port, BXPort, XPort, BRef, Ref, BXRef, XRef, BXPid, XPid, F1, F2, F3, F4, F5,
     #{a => 1, b => 2, c => 3, d => 4, e => 5, f => 6, g => 7, h => 8, i => 9,
       j => 1, k => 1, l => 123123123123213, m => [1,2,3,4,5,6,7,8], o => 5, p => 6,
       q => 7, r => 8, s => 9}}.

get_map_helper(MapSoFar, 0) ->
    MapSoFar;
get_map_helper(MapSoFar, NumOfItemsToAdd) ->
    NewMapSoFar = maps:put(NumOfItemsToAdd, NumOfItemsToAdd, MapSoFar),
    get_map_helper(NewMapSoFar, NumOfItemsToAdd -1).

get_map(Size) ->
    get_map_helper(#{}, Size).


%% Copied from binary_SUITE
make_unaligned_sub_binary(Bin0) when is_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

make_unaligned_sub_bitstring(Bin0) ->
    Bin1 = <<0:3,Bin0/bitstring,31:5>>,
    Sz = erlang:bit_size(Bin0),
    <<0:3,Bin:Sz/bitstring,31:5>> = id(Bin1),
    Bin.

make_random_bin(Size) ->
    make_random_bin(Size, []).

make_random_bin(0, Acc) ->
    iolist_to_binary(Acc);
make_random_bin(Size, []) ->
    make_random_bin(Size - 1, [simple_rand() rem 256]);
make_random_bin(Size, [N | Tail]) ->
    make_random_bin(Size - 1, [simple_rand(N) rem 256, N |Tail]).

simple_rand() ->
    123456789.
simple_rand(Seed) ->
    A = 1103515245,
    C = 12345,
    M = (1 bsl 31),
    (A * Seed + C) rem M.
