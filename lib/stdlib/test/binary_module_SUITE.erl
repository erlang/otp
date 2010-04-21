-module(binary_module_SUITE).

-export([all/1, interesting/1,random_ref_comp/1]).

-define(STANDALONE,1).

-ifdef(STANDALONE).

-define(line,erlang:display({?MODULE,?LINE}),).

-else.

-include("test_server.hrl").

-endif.



-ifdef(STANDALONE).
-export([run/0]).

run() ->
    [ apply(?MODULE,X,[[]]) || X <- all(suite) ].

-endif.

all(suite) -> [interesting,random_ref_comp].


interesting(doc) ->
    ["Try some interesting patterns"];
interesting(Config) when is_list(Config) ->
    X = do_interesting(binary),
    X = do_interesting(binref).

do_interesting(Module) ->
    ?line {0,4} = Module:match(<<"123456">>,
			 Module:compile_pattern([<<"12">>,<<"1234">>,
						 <<"23">>,<<"3">>,
						 <<"34">>,<<"456">>,
						 <<"45">>,<<"6">>])),
    ?line [{0,4},{5,1}] = Module:matches(<<"123456">>,
				   Module:compile_pattern([<<"12">>,<<"1234">>,
							   <<"23">>,<<"3">>,
							   <<"34">>,<<"456">>,
							   <<"45">>,<<"6">>])),
    ?line [{0,4}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"12">>,<<"1234">>,
						     <<"23">>,<<"3">>,
						     <<"34">>,<<"456">>,
						     <<"45">>])),
    ?line [{0,2},{2,2}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"12">>,
						     <<"23">>,<<"3">>,
						     <<"34">>,<<"456">>,
						     <<"45">>])),
    ?line {1,4} = Module:match(<<"123456">>,
			 Module:compile_pattern([<<"34">>,<<"34">>,
						 <<"12347">>,<<"2345">>])),
    ?line [{1,4}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"34">>,<<"34">>,
						     <<"12347">>,<<"2345">>])),
    ?line [{2,2}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"34">>,<<"34">>,
						     <<"12347">>,<<"2346">>])),

    ?line {0,4} = Module:match(<<"123456">>,
			 [<<"12">>,<<"1234">>,
			  <<"23">>,<<"3">>,
			  <<"34">>,<<"456">>,
			  <<"45">>,<<"6">>]),
    ?line [{0,4},{5,1}] = Module:matches(<<"123456">>,
				   [<<"12">>,<<"1234">>,
				    <<"23">>,<<"3">>,
				    <<"34">>,<<"456">>,
				    <<"45">>,<<"6">>]),
    ?line [{0,4}] = Module:matches(<<"123456">>,
			     [<<"12">>,<<"1234">>,
			      <<"23">>,<<"3">>,
			      <<"34">>,<<"456">>,
			      <<"45">>]),
    ?line [{0,2},{2,2}] = Module:matches(<<"123456">>,
					 [<<"12">>,
					  <<"23">>,<<"3">>,
					  <<"34">>,<<"456">>,
					  <<"45">>]),
    ?line {1,4} = Module:match(<<"123456">>,
			       [<<"34">>,<<"34">>,
				<<"12347">>,<<"2345">>]),
    ?line [{1,4}] = Module:matches(<<"123456">>,
				   [<<"34">>,<<"34">>,
				    <<"12347">>,<<"2345">>]),
    ?line [{2,2}] = Module:matches(<<"123456">>,
				   [<<"34">>,<<"34">>,
				    <<"12347">>,<<"2346">>]),
    ok.

random_ref_comp(doc) ->
    ["Test pseudorandomly generated cases against reference imlementation"];
random_ref_comp(Config) when is_list(Config) ->
    ?line put(success_counter,0),
    ?line random:seed({1271,769940,559934}),
    ?line do_random_match_comp(5000,{1,40},{30,1000}),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    ?line do_random_match_comp2(5000,{1,40},{30,1000}),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    ?line do_random_match_comp3(5000,{1,40},{30,1000}),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    ?line do_random_matches_comp(5000,{1,40},{30,1000}),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    ?line do_random_matches_comp2(5000,{1,40},{30,1000}),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    ?line do_random_matches_comp3(5,{1,40},{30,1000}),
    ?line erts_debug:set_internal_state(available_internal_state,true),
    ?line io:format("oldlimit: ~p~n",[ erts_debug:set_internal_state(binary_loop_limit,100)]),
    ?line do_random_matches_comp3(5,{1,40},{30,1000}),
    ?line io:format("limit was: ~p~n",[ erts_debug:set_internal_state(binary_loop_limit,default)]),
    ?line erts_debug:set_internal_state(available_internal_state,false),
    ok.

do_random_matches_comp(0,_,_) ->
    ok;
do_random_matches_comp(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Needles = [random_string(NeedleRange) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    Haystack = random_string(HaystackRange),
    true = do_matches_comp(Needles,Haystack),
    do_random_matches_comp(N-1,NeedleRange,HaystackRange).

do_random_matches_comp2(0,_,_) ->
    ok;
do_random_matches_comp2(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    true = do_matches_comp(Needles,Haystack),
    do_random_matches_comp2(N-1,NeedleRange,HaystackRange).

do_random_matches_comp3(0,_,_) ->
    ok;
do_random_matches_comp3(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    RefRes = binref:matches(Haystack,Needles),
    true = do_matches_comp_loop(10000,Needles,Haystack, RefRes),
    do_random_matches_comp3(N-1,NeedleRange,HaystackRange).

do_matches_comp_loop(0,_,_,_) ->
    true;
do_matches_comp_loop(N, Needles, Haystack0,RR) ->
    DummySize=N*8,
    Haystack1 = <<0:DummySize,Haystack0/binary>>,
    RR1=[{X+N,Y} || {X,Y} <- RR],
    true = do_matches_comp2(Needles,Haystack1,RR1),
    Haystack2 = <<Haystack0/binary,Haystack1/binary>>,
    RR2 = RR ++ [{X2+N+byte_size(Haystack0),Y2} || {X2,Y2} <- RR],
    true = do_matches_comp2(Needles,Haystack2,RR2),
    do_matches_comp_loop(N-1, Needles, Haystack0,RR).


do_matches_comp2(N,H,A) ->
    C = (catch binary:matches(H,N)),
    case (A =:= C) of
	true ->
	    true;
	_ ->
	    io:format("Failed to match ~p (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~n,C:~p.~n",
		      [A,C]),
	    exit(mismatch)
    end.
do_matches_comp(N,H) ->
    A = (catch binref:matches(H,N)),
    B = (catch binref:matches(H,binref:compile_pattern(N))),
    C = (catch binary:matches(H,N)),
    D = (catch binary:matches(H,binary:compile_pattern(N))),
    if
	A =/= nomatch ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case {(A =:= B), (B =:= C),(C =:= D)} of
	{true,true,true} ->
	    true;
	_ ->
	    io:format("Failed to match ~p (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~nB:~p,~n,C:~p,~n,D:~p.~n",
		      [A,B,C,D]),
	    exit(mismatch)
    end.

do_random_match_comp(0,_,_) ->
    ok;
do_random_match_comp(N,NeedleRange,HaystackRange) ->
    Needle = random_string(NeedleRange),
    Haystack = random_string(HaystackRange),
    true = do_match_comp(Needle,Haystack),
    do_random_match_comp(N-1,NeedleRange,HaystackRange).

do_random_match_comp2(0,_,_) ->
    ok;
do_random_match_comp2(N,NeedleRange,HaystackRange) ->
    Haystack = random_string(HaystackRange),
    Needle = random_substring(NeedleRange,Haystack),
    true = do_match_comp(Needle,Haystack),
    do_random_match_comp2(N-1,NeedleRange,HaystackRange).

do_random_match_comp3(0,_,_) ->
    ok;
do_random_match_comp3(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    true = do_match_comp3(Needles,Haystack),
    do_random_match_comp3(N-1,NeedleRange,HaystackRange).

do_match_comp(N,H) ->
    A = (catch binref:match(H,N)),
    B = (catch binref:match(H,binref:compile_pattern([N]))),
    C = (catch binary:match(H,N)),
    D = (catch binary:match(H,binary:compile_pattern([N]))),
    if
	A =/= nomatch ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case {(A =:= B), (B =:= C),(C =:= D)} of
	{true,true,true} ->
	    true;
	_ ->
	    io:format("Failed to match ~s (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~nB:~p,~n,C:~p,~n,D:~p.~n",
		      [A,B,C,D]),
	    exit(mismatch)
    end.

do_match_comp3(N,H) ->
    A = (catch binref:match(H,N)),
    B = (catch binref:match(H,binref:compile_pattern(N))),
    C = (catch binary:match(H,N)),
    D = (catch binary:match(H,binary:compile_pattern(N))),
    if
	A =/= nomatch ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case {(A =:= B), (B =:= C),(C =:= D)} of
	{true,true,true} ->
	    true;
	_ ->
	    io:format("Failed to match ~s (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~nB:~p,~n,C:~p,~n,D:~p.~n",
		      [A,B,C,D]),
	    exit(mismatch)
    end.

one_random(N) ->
    M = ((N - 1) rem 68) + 1,
    element(M,{$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z,$å,$ä,$ö,$A,$B,$C,$D,$E,$F,$G,$H,$I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,$W,$X,$Y,$Z,$Å,$Ä,$Ö,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9}).

random_string({Min,Max}) ->
    X = random:uniform(Max - Min + 1) + Min - 1,
    list_to_binary([one_random(random:uniform(68)) || _ <- lists:seq(1,X)]).
random_substring({Min,Max},Hay) ->
    X = random:uniform(Max - Min + 1) + Min - 1,
    Y = byte_size(Hay),
    Z = if
	    X > Y -> Y;
	    true -> X
	end,
    PMax = Y - Z,
    Pos = random:uniform(PMax + 1) - 1,
    <<_:Pos/binary,Res:Z/binary,_/binary>> = Hay,
    Res.
