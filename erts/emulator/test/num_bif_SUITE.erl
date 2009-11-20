%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

-module(num_bif_SUITE).

-include("test_server.hrl").

%% Tests the BIFs:
%% 	abs/1
%%	float/1
%%	float_to_list/1
%%	integer_to_list/1
%%	list_to_float/1
%%	list_to_integer/1
%%	round/1
%%	trunc/1

-export([all/1, t_abs/1, t_float/1,
	 t_float_to_list/1, t_integer_to_list/1,
	 t_list_to_integer/1,
	 t_list_to_float/1, t_list_to_float_safe/1, t_list_to_float_risky/1,
	 t_round/1, t_trunc/1]).

all(suite) -> [t_abs, t_float, t_float_to_list, t_integer_to_list,
	       t_list_to_float, t_list_to_integer,
	       t_round, t_trunc].

t_abs(Config) when is_list(Config) ->
    %% Floats.
    ?line 5.5 = abs(id(5.5)),
    ?line 0.0 = abs(id(0.0)),
    ?line 100.0 = abs(id(-100.0)),
    
    %% Integers.
    ?line 5 = abs(id(5)),
    ?line 0 = abs(id(0)),
    ?line 100 = abs(id(-100)),

    %% The largest smallnum. OTP-3190.
    ?line X = id((1 bsl 27) - 1),
    ?line X = abs(X),
    ?line X = abs(X-1)+1,
    ?line X = abs(X+1)-1,
    ?line X = abs(-X),
    ?line X = abs(-X-1)-1,
    ?line X = abs(-X+1)+1,

    %% Bignums.
    BigNum = id(13984792374983749),
    ?line BigNum = abs(BigNum),
    ?line BigNum = abs(-BigNum),
    ok.
    
t_float(Config) when is_list(Config) ->
    ?line 0.0 = float(id(0)),
    ?line 2.5 = float(id(2.5)),
    ?line 0.0 = float(id(0.0)),
    ?line -100.55 = float(id(-100.55)),
    ?line 42.0 = float(id(42)),
    ?line -100.0 = float(id(-100)),

    %% Bignums.
    ?line 4294967305.0 = float(id(4294967305)),
    ?line -4294967305.0 = float(id(-4294967305)),

    %% Extremly big bignums.
    ?line Big = id(list_to_integer(id(lists:duplicate(2000, $1)))),
    ?line {'EXIT', {badarg, _}} = (catch float(Big)),

    %% Invalid types and lists.
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(id(atom))),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(id(123))),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(id([$1,[$2]]))),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(id("1.2"))),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(id("a"))),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(id(""))),
    ok.


%% Tests float_to_list/1.

t_float_to_list(Config) when is_list(Config) ->
    ?line test_ftl("0.0e+0", 0.0),
    ?line test_ftl("2.5e+1", 25.0),
    ?line test_ftl("2.5e+0", 2.5),
    ?line test_ftl("2.5e-1", 0.25),
    ?line test_ftl("-3.5e+17", -350.0e15),
    ok.
    
test_ftl(Expect, Float) ->
    %% No ?line on the next line -- we want the line number from t_float_to_list.
    Expect = remove_zeros(lists:reverse(float_to_list(Float)), []).

%% Removes any non-significant zeros in a floating point number.
%% Example: 2.500000e+01 -> 2.5e+1

remove_zeros([$+, $e|Rest], [$0, X|Result]) ->
    remove_zeros([$+, $e|Rest], [X|Result]);
remove_zeros([$-, $e|Rest], [$0, X|Result]) ->
    remove_zeros([$-, $e|Rest], [X|Result]);
remove_zeros([$0, $.|Rest], [$e|Result]) ->
    remove_zeros(Rest, [$., $0, $e|Result]);
remove_zeros([$0|Rest], [$e|Result]) ->
    remove_zeros(Rest, [$e|Result]);
remove_zeros([Char|Rest], Result) ->
    remove_zeros(Rest, [Char|Result]);
remove_zeros([], Result) ->
    Result.

%% Tests integer_to_list/1.

t_integer_to_list(Config) when is_list(Config) ->
    ?line "0" = integer_to_list(id(0)),
    ?line "42" = integer_to_list(id(42)),
    ?line "-42" = integer_to_list(id(-42)),
    ?line "32768" = integer_to_list(id(32768)),
    ?line "268435455" = integer_to_list(id(268435455)),
    ?line "-268435455" = integer_to_list(id(-268435455)),
    ?line "123456932798748738738" = integer_to_list(id(123456932798748738738)),
    ?line Big_List = id(lists:duplicate(2000, id($1))),
    ?line Big = list_to_integer(Big_List),
    ?line Big_List = integer_to_list(Big),
    ok.

%% Tests list_to_float/1.

t_list_to_float(suite) -> [t_list_to_float_safe, t_list_to_float_risky].

t_list_to_float_safe(Config) when is_list(Config) ->
    ?line 0.0 = list_to_float(id("0.0")),
    ?line 0.0 = list_to_float(id("-0.0")),
    ?line 0.5 = list_to_float(id("0.5")),
    ?line -0.5 = list_to_float(id("-0.5")),
    ?line 100.0 = list_to_float(id("1.0e2")),
    ?line 127.5 = list_to_float(id("127.5")),
    ?line -199.5 = list_to_float(id("-199.5")),

    ?line {'EXIT',{badarg,_}} = (catch list_to_float(id("0"))),
    ?line {'EXIT',{badarg,_}} = (catch list_to_float(id("0..0"))),
    ?line {'EXIT',{badarg,_}} = (catch list_to_float(id("0e12"))),
    ?line {'EXIT',{badarg,_}} = (catch list_to_float(id("--0.0"))),

    ok.

%% This might crash the emulator...
%% (Known to crash the Unix version of Erlang 4.4.1)

t_list_to_float_risky(Config) when is_list(Config) ->
    ?line Many_Ones = lists:duplicate(25000, id($1)),
    ?line id(list_to_float("2."++Many_Ones)),
    ?line {'EXIT', {badarg, _}} = (catch list_to_float("2"++Many_Ones)),
    ok.

%% Tests list_to_integer/1.

t_list_to_integer(Config) when is_list(Config) ->
    ?line 0 = list_to_integer(id("0")),
    ?line 0 = list_to_integer(id("00")),
    ?line 0 = list_to_integer(id("-0")),
    ?line 1 = list_to_integer(id("1")),
    ?line -1 = list_to_integer(id("-1")),
    ?line 42 = list_to_integer(id("42")),
    ?line -12 = list_to_integer(id("-12")),
    ?line 32768 = list_to_integer(id("32768")),
    ?line 268435455 = list_to_integer(id("268435455")),
    ?line -268435455 = list_to_integer(id("-268435455")),

    %% Bignums.
    ?line 123456932798748738738 = list_to_integer(id("123456932798748738738")),
    ?line id(list_to_integer(lists:duplicate(2000, id($1)))),
    ok.

%% Tests round/1.

t_round(Config) when is_list(Config) ->
    ?line 0 = round(id(0.0)),
    ?line 0 = round(id(0.4)),
    ?line 1 = round(id(0.5)),
    ?line 0 = round(id(-0.4)),
    ?line -1 = round(id(-0.5)),
    ?line 255 = round(id(255.3)),
    ?line 256 = round(id(255.6)),
    ?line -1033 = round(id(-1033.3)),
    ?line -1034 = round(id(-1033.6)),
    
    % OTP-3722:
    ?line X = id((1 bsl 27) - 1),
    ?line MX = -X,
    ?line MXm1 = -X-1,
    ?line MXp1 = -X+1,
    ?line F = id(X + 0.0),
    ?line X = round(F),
    ?line X = round(F+1)-1,
    ?line X = round(F-1)+1,
    ?line MX = round(-F),
    ?line MXm1 = round(-F-1),
    ?line MXp1 = round(-F+1),

    ?line X = round(F+0.1),
    ?line X = round(F+1+0.1)-1,
    ?line X = round(F-1+0.1)+1,
    ?line MX = round(-F+0.1),
    ?line MXm1 = round(-F-1+0.1),
    ?line MXp1 = round(-F+1+0.1),

    ?line X = round(F-0.1),
    ?line X = round(F+1-0.1)-1,
    ?line X = round(F-1-0.1)+1,
    ?line MX = round(-F-0.1),
    ?line MXm1 = round(-F-1-0.1),
    ?line MXp1 = round(-F+1-0.1),

    ?line 0.5 = abs(round(F+0.5)-(F+0.5)),
    ?line 0.5 = abs(round(F-0.5)-(F-0.5)),
    ?line 0.5 = abs(round(-F-0.5)-(-F-0.5)),
    ?line 0.5 = abs(round(-F+0.5)-(-F+0.5)),

    %% Bignums.
    ?line 4294967296 = round(id(4294967296.1)),
    ?line 4294967297 = round(id(4294967296.9)),
    ?line -4294967296 = -round(id(4294967296.1)),
    ?line -4294967297 = -round(id(4294967296.9)),
    ok.

t_trunc(Config) when is_list(Config) ->
    ?line 0 = trunc(id(0.0)),
    ?line 5 = trunc(id(5.3333)),
    ?line -10 = trunc(id(-10.978987)),

    % The largest smallnum, converted to float (OTP-3722):
    ?line X = id((1 bsl 27) - 1),
    ?line F = id(X + 0.0),
    io:format("X = ~p/~w/~w, F = ~p/~w/~w, trunc(F) = ~p/~w/~w~n",
	      [X, X, binary_to_list(term_to_binary(X)),
	       F, F, binary_to_list(term_to_binary(F)),
	       trunc(F), trunc(F), binary_to_list(term_to_binary(trunc(F)))]),
    ?line X = trunc(F),
    ?line X = trunc(F+1)-1,
    ?line X = trunc(F-1)+1,
    ?line X = -trunc(-F),
    ?line X = -trunc(-F-1)-1,
    ?line X = -trunc(-F+1)+1,

    %% Bignums.
    ?line 4294967305 = trunc(id(4294967305.7)),
    ?line -4294967305 = trunc(id(-4294967305.7)),
    ok.

% Calling this function (which is not supposed to be inlined) prevents
% the compiler from calculating the answer, so we don't test the compiler
% instead of the newest runtime system.
id(X) -> X.
