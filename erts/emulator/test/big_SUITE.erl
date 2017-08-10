%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(big_SUITE).


-export([all/0, suite/0, groups/0]).

-export([t_div/1, eq_28/1, eq_32/1, eq_big/1, eq_math/1, big_literals/1,
	 borders/1, negative/1, big_float_1/1, big_float_2/1,
         bxor_2pow/1,
	 shift_limit_1/1, powmod/1, system_limit/1, toobig/1, otp_6692/1]).

%% Internal exports.
-export([eval/1]).
-export([init/3]).

-export([fac/1, fib/1, pow/2, gcd/2, lcm/2]).


-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() -> 
    [t_div, eq_28, eq_32, eq_big, eq_math, big_literals,
     borders, negative, {group, big_float}, shift_limit_1,
     bxor_2pow,
     powmod, system_limit, toobig, otp_6692].

groups() -> 
    [{big_float, [], [big_float_1, big_float_2]}].

%%
%% Syntax of data files:
%% Expr1 = Expr2.
%% ...
%% built in functions are:
%% fac(N).
%% fib(N).
%% pow(X, N)  == X ^ N
%% gcd(Q, R) 
%% lcm(Q, R)
%%
eq_28(Config) when is_list(Config) ->
    TestFile = test_file(Config, "eq_28.dat"),
    test(TestFile).

eq_32(Config) when is_list(Config) ->
    TestFile = test_file(Config, "eq_32.dat"),
    test(TestFile).

eq_big(Config) when is_list(Config) ->
    TestFile = test_file(Config, "eq_big.dat"),
    test(TestFile).

eq_math(Config) when is_list(Config) ->
    TestFile = test_file(Config, "eq_math.dat"),
    test(TestFile).


%% Tests border cases between small/big.
borders(Config) when is_list(Config) ->
    TestFile = test_file(Config, "borders.dat"),
    test(TestFile).

negative(Config) when is_list(Config) ->
    TestFile = test_file(Config, "negative.dat"),
    test(TestFile).
    

%% Find test file
test_file(Config, Name) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join(DataDir, Name).

%%
%%
%% Run test on file test_big_seq.erl
%%
%%
test(File) ->
    test(File, [node()]).

test(File, Nodes) ->
    {ok,Fd} = file:open(File, [read]),
    Res = test(File, Fd, Nodes),
    file:close(Fd),
    case Res of
	{0,Cases} -> {comment, integer_to_list(Cases) ++ " cases"};
	{_,_} -> ct:fail("failed")
    end.

test(File, Fd, Ns) ->
    test(File, Fd, Ns, 0, 0, 0).

test(File, Fd, Ns, L, Cases, Err) ->
    case io:parse_erl_exprs(Fd, '') of
	{eof,_} -> {Err, Cases};
	{error, {Line,_Mod,Message}, _} ->
	    Fmt = erl_parse:format_error(Message),
	    io:format("~s:~w: error ~s~n", [File, Line+L, Fmt]),
	    {Err+1, Cases};
	{ok, [{match,ThisLine,Expr1,Expr2}], Line} ->
	    case multi_match(Ns, {op,0,'-',Expr1,Expr2}) of
		[] ->
		    test(File, Fd, Ns, Line+L-1,Cases+1, Err);
		[_|_] ->
		    PP = erl_pp:expr({op,0,'=/=',Expr1,Expr2}),
		    io:format("~s:~w : error ~s~n", [File,ThisLine+L, PP]),
		    test(File, Fd, Ns, Line+L-1,Cases+1, Err+1)
	    end;
	{ok, Exprs, Line} ->
	    PP = erl_pp:exprs(Exprs),
	    io:format("~s: ~w: equation expected not ~s~n", [File,Line+L,PP]),
	    test(File, Fd, Ns, Line+L-1,Cases+1, Err+1)
    end.

multi_match(Ns, Expr) ->
    multi_match(Ns, Expr, []).

multi_match([Node|Ns], Expr, Rs) ->
    X = rpc:call(Node, big_SUITE, eval, [Expr]),
    if X == 0 -> multi_match(Ns, Expr, Rs);
       true -> multi_match(Ns, Expr, [{Node,X}|Rs])
    end;
multi_match([], _, Rs) -> Rs.

eval(Expr) ->
    LFH = fun(Name, As) -> apply(?MODULE, Name, As) end,

    %% Applied arithmetic BIFs.
    {value,V,_} = erl_eval:expr(Expr, [], {value,LFH}),

    %% Real arithmetic instructions.
    V = eval(Expr, LFH),

    V.

%% Like a subset of erl_eval:expr/3, but uses real arithmetic instructions instead of
%% applying them (it does make a difference).

eval({op,_,Op,A0}, LFH) ->
    A = eval(A0, LFH),
    Res = eval_op(Op, A),
    erlang:garbage_collect(),
    Res;
eval({op,_,Op,A0,B0}, LFH) ->
    [A,B] = eval_list([A0,B0], LFH),
    Res = eval_op(Op, A, B),
    erlang:garbage_collect(),
    Res;
eval({integer,_,I}, _) -> I;
eval({call,_,{atom,_,Local},Args0}, LFH) ->
    Args = eval_list(Args0, LFH),
    LFH(Local, Args).

eval_list([E|Es], LFH) ->
    [eval(E, LFH)|eval_list(Es, LFH)];
eval_list([], _) -> [].

eval_op('-', A) -> -A;
eval_op('+', A) -> +A;
eval_op('bnot', A) -> bnot A.

eval_op('-', A, B) -> A - B;
eval_op('+', A, B) -> A + B;
eval_op('*', A, B) -> A * B;
eval_op('div', A, B) -> A div B;
eval_op('rem', A, B) -> A rem B;
eval_op('band', A, B) -> A band B;
eval_op('bor', A, B) -> A bor B;
eval_op('bxor', A, B) -> A bxor B;
eval_op('bsl', A, B) -> A bsl B;
eval_op('bsr', A, B) -> A bsr B.

%% Built in test functions

fac(0) -> 1;
fac(1) -> 1;
fac(N) -> N * fac(N-1).

%%
%% X ^ N
%%
pow(_, 0) -> 1;
pow(X, 1) -> X;
pow(X, N) when (N band 1) == 1 ->
    X2 = pow(X, N bsr 1),
    X*X2*X2;
pow(X, N) ->
    X2 = pow(X, N bsr 1),
    X2*X2.

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

%%
%% Gcd 
%%
gcd(Q, 0) -> Q;
gcd(Q, R) -> gcd(R, Q rem R).

%%
%% Least common multiple
%%
lcm(Q, R) ->
    Q*R div gcd(Q, R).


%% Test case t_div cut in from R2D test suite.

t_div(Config) when is_list(Config) ->
    'try'(fun() -> 98765432101234 div 98765432101235 end, 0),

    % Big remainder, small quotient.
    'try'(fun() -> 339254531512 div 68719476736 end, 4),
    ok.

'try'(Fun, Result) ->
    'try'(89, Fun, Result, []).

'try'(0, _, _, _) ->
    ok;
'try'(Iter, Fun, Result, Filler) ->
    spawn(?MODULE, init, [self(), Fun, list_to_tuple(Filler)]),
    receive
	{result, Result} ->
	    'try'(Iter-1, Fun, Result, [0|Filler]);
	{result, Other} ->
	    ct:fail("Expected ~p; got ~p~n", [Result, Other])
    end.

init(ReplyTo, Fun, _Filler) ->
    ReplyTo ! {result, Fun()}.

%% Tests that big-number literals work correctly.
big_literals(Config) when is_list(Config) ->
    %% Note: The literal test cannot be compiler on a pre-R4 Beam emulator,
    %% so we compile it now.
    DataDir = proplists:get_value(data_dir, Config),
    Test = filename:join(DataDir, "literal_test"),
    {ok, Mod, Bin} = compile:file(Test, [binary]),
    {module, Mod} = code:load_binary(Mod, Mod, Bin),
    ok = Mod:t(),
    ok.


%% OTP-2436, part 1
big_float_1(Config) when is_list(Config) ->
    %% F is a number very close to a maximum float.
    F = id(1.7e308),
    I = trunc(F),
    true = (I == F),
    false = (I /= F),
    true = (I > F/2),
    false = (I =< F/2),
    true = (I*2 >= F),
    false = (I*2 < F),
    true = (I*I > F),
    false = (I*I =< F),

    true = (F == I),
    false = (F /= I),
    false = (F/2 > I),
    true = (F/2 =< I),
    false = (F >= I*2),
    true = (F < I*2),
    false = (F > I*I),
    true = (F =< I*I),
    ok.

%% "OTP-2436, part 2
big_float_2(Config) when is_list(Config) ->
    F = id(1.7e308),
    I = trunc(F),
    {'EXIT', _} = (catch 1/(2*I)),
    _Ignore = 2/I,
    {'EXIT', _} = (catch 4/(2*I)),
    ok.

%% OTP-3256
shift_limit_1(Config) when is_list(Config) ->
    case catch (id(1) bsl 100000000) of
	      {'EXIT', {system_limit, _}} ->
		  ok
	  end,
    ok.

powmod(Config) when is_list(Config) ->
    A = 1696192905348584855517250509684275447603964214606878827319923580493120589769459602596313014087329389174229999430092223701630077631205171572331191216670754029016160388576759960413039261647653627052707047,
    B = 43581177444506616087519351724629421082877485633442736512567383077022781906420535744195118099822189576169114064491200598595995538299156626345938812352676950427869649947439032133573270227067833308153431095,
    C = 52751775381034251994634567029696659541685100826881826508158083211003576763074162948462801435204697796532659535818017760528684167216110865807581759669824808936751316879636014972704885388116861127856231,
    42092892863788727404752752803608028634538446791189806757622214958680350350975318060071308251566643822307995215323107194784213893808887471095918905937046217646432382915847269148913963434734284563536888 = powmod(A, B, C),
    ok.

powmod(A, 1, C) ->
    A rem C;
powmod(A, 2, C) ->
    A*A rem C;
powmod(A, B, C) ->
    B1 = B div 2,
    B2 = B - B1,
    P = powmod(A, B1, C),
    case B2 of
	B1 ->
	    (P*P) rem C;
	_  -> 
	    (P*P*A) rem C
    end.

system_limit(Config) when is_list(Config) ->
    Maxbig = maxbig(),
    {'EXIT',{system_limit,_}} = (catch Maxbig+1),
    {'EXIT',{system_limit,_}} = (catch -Maxbig-1),
    {'EXIT',{system_limit,_}} = (catch 2*Maxbig),
    {'EXIT',{system_limit,_}} = (catch bnot Maxbig),
    {'EXIT',{system_limit,_}} = (catch apply(erlang, id('bnot'), [Maxbig])),
    {'EXIT',{system_limit,_}} = (catch Maxbig bsl 2),
    {'EXIT',{system_limit,_}} = (catch apply(erlang, id('bsl'), [Maxbig,2])),
    {'EXIT',{system_limit,_}} = (catch id(1) bsl (1 bsl 45)),
    {'EXIT',{system_limit,_}} = (catch id(1) bsl (1 bsl 69)),

    %% There should be no system_limit exception when shifting a zero.
    0 = id(0) bsl (1 bsl 128),
    0 = id(0) bsr -(1 bsl 128),
    Erlang = id(erlang),
    0 = Erlang:'bsl'(id(0), 1 bsl 128),
    0 = Erlang:'bsr'(id(0), -(1 bsl 128)),
    ok.

maxbig() ->
    %% We assume that the maximum arity is (1 bsl 19) - 1.
    Ws = erlang:system_info(wordsize),
    (((1 bsl ((16777184 * (Ws div 4))-1)) - 1) bsl 1) + 1.

id(I) -> I.

toobig(Config) when is_list(Config) ->
    {'EXIT',{{badmatch,_},_}} = (catch toobig()),
    ok.

toobig() ->
    A = erlang:term_to_binary(lists:seq(1000000, 2200000)),
    ASize = erlang:bit_size(A),
    <<ANr:ASize>> = A, % should fail
    ANr band ANr.

%% Tests for DIV/REM bug reported in OTP-6692
otp_6692(Config) when is_list(Config)->
    loop1(1,1000).

fact(N) ->
     fact(N,1).

fact(0,P) -> P;
fact(N,P) -> fact(N-1,P*N).

raised(X,1) ->
    X;
raised(X,N) ->
    X*raised(X,N-1).

loop1(M,M) ->
    ok;
loop1(N,M) ->
    loop2(fact(N),raised(7,7),1,8),
    loop1(N+1,M).

loop2(_,_,M,M) ->
    ok;
loop2(X,Y,N,M) ->
    Z = raised(Y,N),
    case X rem Z of
	Z ->
	    exit({failed,X,'REM',Z,'=',Z});
	0 ->
	    case (X div Z) * Z of
		X ->
		    ok;
		Wrong ->
		    exit({failed,X,'DIV',Z,'*',Z,'=',Wrong})
	    end;
	_ ->
	    ok
    end,
    loop2(X,Y,N+1,M).
    

%% ERL-450
bxor_2pow(_Config) ->
    IL = lists:seq(8*3, 8*16, 4),
    JL = lists:seq(0, 64),
    [bxor_2pow_1((1 bsl I), (1 bsl J))
     || I <- IL, J <- JL],
    ok.

bxor_2pow_1(A, B) ->
    for(-1,1, fun(Ad) ->
                      for(-1,1, fun(Bd) ->
                                        bxor_2pow_2(A+Ad, B+Bd),
                                        bxor_2pow_2(-A+Ad, B+Bd),
                                        bxor_2pow_2(A+Ad, -B+Bd),
                                        bxor_2pow_2(-A+Ad, -B+Bd)
                                end)
              end).

for(From, To, _Fun) when From > To ->
    ok;
for(From, To, Fun) ->
    Fun(From),
    for(From+1, To, Fun).

bxor_2pow_2(A, B) ->
    Correct = my_bxor(A, B),
    case A bxor B of
        Correct -> ok;
        Wrong ->
            io:format("~.16b bxor ~.16b\n", [A,B]),
            io:format("Expected ~.16b\n", [Correct]),
            io:format("Got      ~.16b\n", [Wrong]),
            ct:fail({failed, 'bxor'})

    end.

%% Implement bxor without bxor
my_bxor(A, B) ->
    my_bxor(A, B, 0, 0).

my_bxor(0, 0, _, Acc) -> Acc;
my_bxor(-1, -1, _, Acc) -> Acc;
my_bxor(-1, 0, N, Acc) -> (-1 bsl N) bor Acc; % sign extension
my_bxor(0, -1, N, Acc) -> (-1 bsl N) bor Acc; % sign extension
my_bxor(A, B, N, Acc0) ->
    Acc1 = case (A band 1) =:= (B band 1) of
               true -> Acc0;
               false -> Acc0 bor (1 bsl N)
          end,
    my_bxor(A bsr 1, B bsr 1, N+1, Acc1).
