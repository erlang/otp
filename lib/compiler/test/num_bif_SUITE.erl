%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").

%% Tests optimization of the BIFs:
%% 	abs/1
%%	float/1
%%	float_to_list/1
%%	integer_to_list/1
%%	list_to_float/1
%%	list_to_integer/1
%%	round/1
%%	trunc/1

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, t_abs/1, t_float/1,
	 t_float_to_list/1, t_integer_to_list/1,
	 t_list_to_integer/1,
	 t_list_to_float_safe/1, t_list_to_float_risky/1,
	 t_round/1, t_trunc/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [t_abs, t_float, t_float_to_list, t_integer_to_list,
     {group, t_list_to_float}, t_list_to_integer, t_round,
     t_trunc].

groups() -> 
    [{t_list_to_float, [],
      [t_list_to_float_safe, t_list_to_float_risky]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


t_abs(Config) when is_list(Config) ->
    %% Floats.
    ?line 5.5 = abs(5.5),
    ?line 0.0 = abs(0.0),
    ?line 100.0 = abs(-100.0),
    
    %% Integers.
    ?line 5 = abs(5),
    ?line 0 = abs(0),
    ?line 100 = abs(-100),

    %% The largest smallnum. OTP-3190.
    ?line X = (1 bsl 27) - 1,
    ?line X = abs(X),
    ?line X = abs(X-1)+1,
    ?line X = abs(X+1)-1,
    ?line X = abs(-X),
    ?line X = abs(-X-1)-1,
    ?line X = abs(-X+1)+1,

    %% Bignums.
    BigNum = 13984792374983749,
    ?line BigNum = abs(BigNum),
    ?line BigNum = abs(-BigNum),
    ok.
    
t_float(Config) when is_list(Config) ->
    ?line 0.0 = float(0),
    ?line 2.5 = float(2.5),
    ?line 0.0 = float(0.0),
    ?line -100.55 = float(-100.55),
    ?line 42.0 = float(42),
    ?line -100.0 = float(-100),

    %% Bignums.
    ?line 4294967305.0 = float(4294967305),
    ?line -4294967305.0 = float(-4294967305),

    %% Extremly big bignums.
    ?line Big = list_to_integer(lists:duplicate(2000, $1)),
    ?line {'EXIT', {badarg, _}} = (catch float(Big)),

    %% Invalid types and lists.
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(atom)),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer(123)),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer([$1, [$2]])),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer("1.2")),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer("a")),
    ?line {'EXIT', {badarg, _}} = (catch list_to_integer("")),
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
    ?line "0" = integer_to_list(0),
    ?line "42" = integer_to_list(42),
    ?line "-42" = integer_to_list(-42),
    ?line "-42" = integer_to_list(-42),
    ?line "32768" = integer_to_list(32768),
    ?line "268435455" = integer_to_list(268435455),
    ?line "-268435455" = integer_to_list(-268435455),
    ?line "123456932798748738738" = integer_to_list(123456932798748738738),
    ?line Big_List = lists:duplicate(2000, $1),
    ?line Big = list_to_integer(Big_List),
    ?line Big_List = integer_to_list(Big),
    ok.

%% Tests list_to_float/1.


t_list_to_float_safe(Config) when is_list(Config) ->
    ?line 0.0 = list_to_float("0.0"),
    ?line 0.0 = list_to_float("-0.0"),
    ?line 0.5 = list_to_float("0.5"),
    ?line -0.5 = list_to_float("-0.5"),
    ?line 100.0 = list_to_float("1.0e2"),
    ?line 127.5 = list_to_float("127.5"),
    ?line -199.5 = list_to_float("-199.5"),

    ?line {'EXIT', {badarg, _}} = (catch list_to_float("0")),
    ?line {'EXIT', {badarg, _}} = (catch list_to_float("0..0")),
    ?line {'EXIT', {badarg, _}} = (catch list_to_float("0e12")),
    ?line {'EXIT', {badarg, _}} = (catch list_to_float("--0.0")),
%%    ?line {'EXIT', {badarg, _}} = (catch list_to_float("0.0e+99999999")),

    ok.

%% This might crash the emulator...
%% (Known to crash the Unix version of Erlang 4.4.1)

t_list_to_float_risky(Config) when is_list(Config) ->
    ?line Many_Ones = lists:duplicate(25000, $1),
    ?line _ = list_to_float("2."++Many_Ones),
    ?line {'EXIT', {badarg, _}} = (catch list_to_float("2"++Many_Ones)),
    ok.

%% Tests list_to_integer/1.

t_list_to_integer(Config) when is_list(Config) ->
    ?line 0 = list_to_integer("0"),
    ?line 0 = list_to_integer("00"),
    ?line 0 = list_to_integer("-0"),
    ?line 1 = list_to_integer("1"),
    ?line -1 = list_to_integer("-1"),
    ?line 42 = list_to_integer("42"),
    ?line -12 = list_to_integer("-12"),
    ?line 32768 = list_to_integer("32768"),
    ?line 268435455 = list_to_integer("268435455"),
    ?line -268435455 = list_to_integer("-268435455"),

    %% Bignums.
    ?line 123456932798748738738 = list_to_integer("123456932798748738738"),
    ?line _ = list_to_integer(lists:duplicate(2000, $1)),
    ok.

%% Tests round/1.

t_round(Config) when is_list(Config) ->
    ?line 0 = round(0.0),
    ?line 0 = round(0.4),
    ?line 1 = round(0.5),
    ?line 0 = round(-0.4),
    ?line -1 = round(-0.5),
    ?line 255 = round(255.3),
    ?line 256 = round(255.6),
    ?line -1033 = round(-1033.3),
    ?line -1034 = round(-1033.6),
    
    % OTP-3722:
    ?line X = (1 bsl 27) - 1,
    ?line MX = -X,
    ?line MXm1 = -X-1,
    ?line MXp1 = -X+1,
    ?line F = X + 0.0,
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
    ?line 4294967296 = round(4294967296.1),
    ?line 4294967297 = round(4294967296.9),
    ?line -4294967296 = -round(4294967296.1),
    ?line -4294967297 = -round(4294967296.9),
    ok.

t_trunc(Config) when is_list(Config) ->
    ?line 0 = trunc(0.0),
    ?line 5 = trunc(5.3333),
    ?line -10 = trunc(-10.978987),
    % The largest smallnum, converted to float (OTP-3722):
    ?line X = (1 bsl 27) - 1,
    ?line F = X + 0.0,
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
    ?line 4294967305 = trunc(4294967305.7),
    ?line -4294967305 = trunc(-4294967305.7),
    ok.
