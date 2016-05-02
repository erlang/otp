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
         otp_7127_test/0]).
-compile({nowarn_deprecated_function, {erlang,hash,2}}).

%%
%% Define to run outside of test server
%%
%-define(STANDALONE,1).

%%
%% Define for debug output
%%
%-define(debug,1).

-ifdef(STANDALONE).
-define(config(A,B),config(A,B)).
-export([config/2]).
-else.
-include_lib("common_test/include/ct.hrl").
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
-else.
%% When run in test server.
-export([all/0, suite/0,
	 test_basic/1,test_cmp/1,test_range/1,test_spread/1,
	 test_phash2/1,otp_5292/1,bit_level_binaries/1,otp_7127/1,
         test_hash_zero/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() -> 
    [test_basic, test_cmp, test_range, test_spread,
     test_phash2, otp_5292, bit_level_binaries, otp_7127,
     test_hash_zero].

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
-endif.



%%
%% Here are the real tests, they can be run without test_server, 
%% define -DSTANDALONE when compiling.
%%
basic_test() ->
    685556714 = erlang:phash({a,b,c},16#FFFFFFFF),
    14468079 = erlang:hash({a,b,c},16#7FFFFFF),
    37442646 =  erlang:phash([a,b,c,{1,2,3},c:pid(0,2,3),
				    16#77777777777777],16#FFFFFFFF),
    Comment = case erlang:hash([a,b,c,{1,2,3},c:pid(0,2,3),
				      16#77777777777777],16#7FFFFFF) of
			102727602 ->
			    big = erlang:system_info(endian),
			    "Big endian machine";
			105818829 ->
			    little = erlang:system_info(endian),
			    "Little endian machine"
		    end,
    ExternalReference = <<131,114,0,3,100,0,13,110,111,110,111,100,101,64,
			 110,111,104,111,115,116,0,0,0,0,122,0,0,0,0,0,0,0,0>>,
    1113403635 = erlang:phash(binary_to_term(ExternalReference),
				    16#FFFFFFFF),
    123 = erlang:hash(binary_to_term(ExternalReference),
			    16#7FFFFFF),
    ExternalFun = <<131,117,0,0,0,3,103,100,0,13,110,111,110,111,100,101,64,
		   110,111,104,111,115,116,0,0,0,38,0,0,0,0,0,100,0,8,101,
		   114,108,95,101,118,97,108,97,20,98,5,182,139,98,108,0,0,
		   0,3,104,2,100,0,1,66,109,0,0,0,33,131,114,0,3,100,0,13,
		   110,111,110,111,100,101,64,110,111,104,111,115,116,0,0,
		   0,0,122,0,0,0,0,0,0,0,0,104,2,100,0,1,76,107,0,33,131,
		   114,0,3,100,0,13,110,111,110,111,100,101,64,110,111,104,
		   111,115,116,0,0,0,0,122,0,0,0,0,0,0,0,0,104,2,100,0,1,82,
		   114,0,3,100,0,13,110,111,110,111,100,101,64,110,111,104,
		   111,115,116,0,0,0,0,122,0,0,0,0,0,0,0,0,106,108,0,0,0,1,
		   104,5,100,0,6,99,108,97,117,115,101,97,1,106,106,108,0,0,
		   0,1,104,3,100,0,7,105,110,116,101,103,101,114,97,1,97,1,
		   106,106,104,3,100,0,4,101,118,97,108,104,2,100,0,5,115,
		   104,101,108,108,100,0,10,108,111,99,97,108,95,102,117,
		   110,99,108,0,0,0,1,103,100,0,13,110,111,110,111,100,101,
		   64,110,111,104,111,115,116,0,0,0,22,0,0,0,0,0,106>>,
    170987488 = erlang:phash(binary_to_term(ExternalFun),
				   16#FFFFFFFF),
    124460689 = erlang:hash(binary_to_term(ExternalFun),
				  16#7FFFFFF),
    case (catch erlang:phash(1,0)) of
	{'EXIT',{badarg, _}} ->
	    {comment, Comment};
	_ ->
	    exit(phash_accepted_zero_as_range)
    end.


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

-ifdef(FALSE).
f1() ->
    abc.

f2() ->
    abd.

f3(X, Y) ->
    {X, Y}.
-endif.

otp_5292_test() ->
    H = fun(E) -> [erlang:hash(E, 16#7FFFFFF),
                   erlang:hash(-E, 16#7FFFFFF)]
        end,
    S1 = md5([md5(hash_int(S, E, H)) || {Start, N, Sz} <- d(), 
                                        {S, E} <- int(Start, N, Sz)]),
    PH = fun(E) -> [erlang:phash(E, 1 bsl 32),
                    erlang:phash(-E, 1 bsl 32),
                    erlang:phash2(E, 1 bsl 32),
                    erlang:phash2(-E, 1 bsl 32)]
            end,
    S2 = md5([md5(hash_int(S, E, PH)) || {Start, N, Sz} <- d(), 
                                         {S, E} <- int(Start, N, Sz)]),
    Comment = case S1 of 
			<<4,248,208,156,200,131,7,1,173,13,239,173,112,81,16,174>> ->
			    big = erlang:system_info(endian),
                            "Big endian machine";
                        <<180,28,33,231,239,184,71,125,76,47,227,241,78,184,176,233>> ->
			    little = erlang:system_info(endian),
                            "Little endian machine"
                    end,
    <<124,81,198,121,174,233,19,137,10,83,33,80,226,111,238,99>> = S2,
    2 = erlang:hash(1, (1 bsl 27) -1),
    {'EXIT', _} = (catch erlang:hash(1, (1 bsl 27))),
    {comment, Comment}.

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
	bit_level_all_different(fun erlang:hash/2),
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
    Hash = erlang:hash(Bitstr, Rem),
    Hash = erlang:phash(Bitstr, Rem),
    Hash = erlang:hash(unaligned_sub_bitstr(Bitstr), Rem),
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
    ok = hash_zero_test(Zs,fun(T) -> erlang:hash(T, (1 bsl 27) - 1) end),
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
    
